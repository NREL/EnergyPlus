# Group – Economics

EnergyPlus contains a number of objects to model construction cost and utility bill calculations.

The objects used by EnergyPlus for construction cost calculations are as follows:

- ComponentCost:Adjustments
- ComponentCost:Reference
- ComponentCost:LineItem

The objects used by EnergyPlus for utility bill calculations are as follows:

- UtilityCost:Tariff
- UtilityCost:Qualify
- UtilityCost:Charge:Simple
- UtilityCost:Charge:Block
- UtilityCost:Ratchet
- UtilityCost:Variable
- UtilityCost:Computation

The objects used for performing a life-cycle cost analysis are:

- LifeCycleCost:Parameters
- LifeCycleCost:RecurringCosts 
- LifeCycleCost:NonrecurringCost
- LifeCycleCost:UsePriceEscalation
- LifeCycleCost:UseAdjustment 

The object used for specifying the currency symbol of the monetary unit used is:

- CurrencyType