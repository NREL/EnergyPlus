# UtilityCost:Charge:Simple

The UtilityCost:Charge:Simple is one of the most often used objects for tariff calculation. It is used to compute energy and demand charges that are very simple. It may also be used for taxes, surcharges and any other charges that occur on a utility bill. As many UtilityCost:Charge:Simple objects as needed may be defined for a single tariff and they will be added together.

## Inputs

#### Field: Charge Variable Name

This is the name associated with the UtilityCost:Charge:Simple object and will appear in the report. In addition, the results of the UtilityCost:Charge:Simple calculation are stored in a variable with the same name. That way, the results may be used for further calculation. Spaces are not significant in Charge Variable Names. They are removed during the utility bill calculation process.

#### Field: Tariff Name

The name of the tariff that this UtilityCost:Charge:Simple is associated with.

#### Field: Source Variable

The name of the source used by the UtilityCost:Charge:Simple. This is usually the name of the variable holding the energy or demand but may also be the name of any variable including the subtotal or basis if other charges are based on those.

#### Field: Season

This is the name (or number) of a season for which the UtilityCost:Charge:Simple is calculated. If this is set to Annual, the calculations are performed for the UtilityCost:Charge:Simple for the entire year (all months) otherwise it is calculated only for those months in the season defined. The season is defined by the Season Schedule in the UtilityCost:Tariff object. The choices for seasons are the following names or numbers may be used:

- Annual
- Winter
- Spring
- Summer
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

#### Field: Cost per Unit Value or Variable Name

This field contains either a single number or the name of a variable. The number is multiplied with all of the energy or demand or other source that is specified in the source field. If a variable is used, then the monthly values of the variable are multiplied against the variable specified in the source field. This field makes it easy to include a simple charge without specifying block sizes. It is added to any of the block calculations (see later fields) but is most often used by itself. If no value or variable is entered, a zero is assumed. This is a good way to include a tax or cost adjustment. For SellToUtility tariffs, the values in this field are usually expressed as negative numbers.