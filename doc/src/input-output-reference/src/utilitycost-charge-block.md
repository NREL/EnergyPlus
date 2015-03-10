# UtilityCost:Charge:Block

The UtilityCost:Charge:Block objects is also used extensively for tariff calculation. It is used to compute energy and demand charges that are structured in blocks of charges. It may also be used for taxes, surcharges and any other charges that occur on a utility bill but those are more commonly simple flat charges so UtilityCost:Charge:Simple  is more commonly used for those functions. As many UtilityCost:Charge:Block objects as needed may be defined for a single tariff and they will be added together. Blocks are a structure used by almost all utilities for calculating energy and demand charges and they allow the utility to charge more or less per unit of energy or demand if more units are used.

## Inputs

#### Field: Charge Variable Name

This is the name associated with the UtilityCost:Charge:Block object and will appear in the report. In addition, the results of the UtilityCost:Charge:Block are stored in a variable with the same name. That way, the results may be used for further calculation.

#### Field: Tariff Name

The name of the tariff that this UtilityCost:Charge:Block is associated with.

#### Field: Source Variable

The name of the source used by the UtilityCost:Charge:Block. This is usually the name of the variable holding the energy or demand but may also be the name of any variable including the subtotal or basis if other charges are based on those.

#### Field: Season

This is the name (or number) of a season for which the  UtilityCost:Charge:Block is calculated. If this is set to annual, the calculations are performed for the UtilityCost:Charge:Block for the entire year (all months) otherwise it is calculated only for those months in the season defined. The season is defined by the Season Schedule in the UtilityCost:Tariff object. The choices for seasons are the following names or numbers may be used:

- Annual
- Summer
- Winter
- Spring
- Fall

#### Field: Category Variable Name

All charges get added somewhere in the hierarchy described in the introduction to EnergyPlus economics above. This field shows where the charge should be added. The reason to enter this field appropriately is so that the charge gets reported in a reasonable category. The charge automatically gets added to the variable that is the category. The choices for this field are limited to:

- EnergyCharges
- DemandCharges
- ServiceCharges
- Basis
- Adjustment
- Surcharge
- Subtotal
- Taxes
- Total
- NotIncluded

#### Field: Remaining Into Variable

If the blocks do not use all of the energy or demand from the source, some energy and demand remains, then the remaining amount should be assigned to a variable. If no variable is assigned and some amount of energy or demand is not used in the block structure a warning will be issued.

#### Field: Block Size Multiplier Value or Variable Name

The sizes of the blocks are usually used directly but if a value or a variable is entered here, the block sizes entered in the rest of the charge are first multiplied by the entered value prior to being used. This is common for rates that are kWh/kW rates and in that case the variable that holds the monthly total electric demand would be entered. If no value is entered, a default value of one is assumed so that the block sizes remain exactly as entered. This field is unusual for the EnergyPlus syntax because it can be either a number or a name of a variable.

#### Field: Block Size 1 Value or Variable Name

The size of the first block of the charges is entered here. For most rates that use multiple blocks, this will be the value for the first block size. Additional block sizes are entered with every other field. A default variable named "remaining" contains a large value and this may be used when the remaining amount should be included in that block. This field and the next field may be repeated in groups of two for as many blocks as are defined. This field is unusual for the EnergyPlus syntax because it can be either a number or a name of a variable.

#### Field: Block 1 Cost per Unit Value or Variable Name

The cost of the first block. This value is also repeated for every other field in the charge. As many blocks may be entered as are needed. This field and the previous field may be repeated in groups of two for as many blocks as are defined. This field is unusual for the EnergyPlus syntax because it can be either a number or a name of a variable. For SellToUtility tariffs, the values in this field are usually expressed as negative numbers.

These last two fields may be repeated fifteen times. If more blocks are defined by the utility, the computation can be continued using another UtilityCost:Charge:Block and using the variable from Remaining Into Variable as the source variable.