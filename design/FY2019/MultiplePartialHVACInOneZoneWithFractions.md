Multiple/Partial HVAC Control, now with user specified fractions!
=================================================================

**Noel Merket, NREL**

 - 4 January 2019 - Original NFP 

## Justification for New Feature ##

During the implementation of Mike Witte's [Improve Control of Multiple HVAC in One Zone](https://github.com/NREL/EnergyPlus/blob/8d7fcb46b41e083a30a872f2b9fc23ca65912c2e/design/FY2017/MultiplePartialHVACInOneZone.md) NFP it was decided to not specify the fraction of the heating or cooling load that each zone equipment should meet and instead make a `UniformLoad` option available which evenly spreads the load between equipment. In some applications, particularly in residential buildings, the load is not evenly distributed between HVAC equipment. An example would be a central dx coil unit for 80% of the house and a window unit for the addition. Specifying HVAC load fractions is required for Home Energy Score, Energy Rating Index, and Weatherization Assistant modeling. 

## E-mail and  Conference Call Conclusions ##

none yet.

## Overview and Approach ##

A new enumeration will be available on the `ZoneHVAC:Equipment` Load Distribution Scheme input. It will be called something like, `FractionalLoad` (open to suggestions). When that option is selected, it will read the fractions from the following pair of fields that will be added to the `ZoneHVAC:EquipmentList` for each equipment:

```
Zone Equipment <x> Cooling Fraction
Zone Equipment <x> Heating or No-Load Fraction
```

Similar to (and uapologetically lifted from) Mike Witte's ["Option A" from the previous NFP](https://github.com/NREL/EnergyPlus/blob/8d7fcb46b41e083a30a872f2b9fc23ca65912c2e/design/FY2017/MultiplePartialHVACInOneZone.md#option-a---initial-load), the fractions will be applied as follows:

The zone equipment manager multiplies the initial load by the applicable schedule fraction before calling a given piece of equipment.  Here is an example where the radiant cooling panels meet 25% of the cooling load and the air system meets the remaining load.

```
  ZoneHVAC:EquipmentList,
    SPACE2-1 Eq,             !- Name
    FractionalLoad,          !- Load Distribution Scheme
    ZoneHVAC:CoolingPanel:RadiantConvective:Water,  !- Zone Equipment 1 Object Type
    SPACE2 Cooling Panel,    !- Zone Equipment 1 Name
    1,                       !- Zone Equipment 1 Cooling Sequence
    1,                       !- Zone Equipment 1 Heating or No-Load Sequence
    0.25,                    !- Zone Equipment 1 Cooling Fraction
    ,                        !- Zone Equipment 1 Heating or No-Load Fraction
    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type
    SPACE2-1 ATU,            !- Zone Equipment 2 Name
    2,                       !- Zone Equipment 2 Cooling Sequence
    2,                       !- Zone Equipment 2 Heating or No-Load Sequence
    0.75,                    !- Zone Equipment 2 Cooling Fraction
    ;                        !- Zone Equipment 2 Heating or No-Load Fraction
```

If the current cooling load is 1000W, the cooling panel will be passed a load of 0.25 \* 1000=250W. The air distribution unit will be passed a load of 0.75 \* 1000W=750W. 

*Pros* - The schedule fractions are intuitive, 0.25 + 0.75 = 1.0

*Cons* - If the first piece of equipment cannot provide the requested load, the next one will not try to meet to meet the unmet portion. and the zone setpoint will not be met.

If effect this will work exactly like the `UniformLoad` load distribution scheme, but will allow the user to specify the fractions instead of having it split evenly among the equipment.


## Testing/Validation/Data Sources ##

Build example files and check results.

## Input Output Reference Documentation ##

See docs source changes on this branch. (Not done yet, but not hard to imagine).

## Engineering Reference ##

I don't think changes will be required for this document.

## Example File and Transition Changes ##

Proposed example file:

 - Create a copy of the `House-2FurnaceAC-UniformLoad.idf` example file but with this new load distribution scheme.

Transition will be required for `ZoneHVAC:EquipmentList`.


## References ##

- Mike Witte - [Improve Control of Multiple HVAC in One Zone](https://github.com/NREL/EnergyPlus/blob/8d7fcb46b41e083a30a872f2b9fc23ca65912c2e/design/FY2017/MultiplePartialHVACInOneZone.md) NFP.



