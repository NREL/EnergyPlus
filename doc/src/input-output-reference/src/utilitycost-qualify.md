# UtilityCost:Qualify

Many utilities provide a large number of tariffs and separate their customers by the amount of energy use they have or by the amount of demand. The qualify object allows only certain tariffs that are modeled to be used. If the results of the simulation fall outside of the range of qualifications, that tariff is still calculated but the "Qualified" entry will say "No" and the UtilityCost:Qualify that caused its exclusion is shown. When multiple tariffs from the same utility are modeled and only one of them should be used, make sure that the qualifiers are consistent especially within the same GroupName (see UtilityCost:Tariff). Multiple UtilityCost:Qualify objects can appear for a specific rate and they can be based on any variable.

The energy used and demands for each month vary. Since it is possible that they vary across the minimum and maximum values defined by this object, a choice in how to treat the thresholds is necessary. Say that a utility has two tariffs, a small tariff that has a maximum demand requirement of 50kW and a large tariff that has a minimum demand requirement of 50kW. If the use for a building goes above the 50kW threshold during some summer months and the rest of the time is below 50kW, then how does the utility choose which rate would qualify. The utility probably has very specific requirements about which months or consecutive months can be above or below a threshold to qualify for the rate.

## Inputs

#### Field: Name

The name used for the UtilityCost:Qualify and displayed if the tariff does not qualify. It is also treated as a variable and contains:

- 1 for each month that does qualify, 
- 0 for months that do not qualify, and 
- -1 for months that are not part of the season selected.

#### Field: Tariff Name

The name of the UtilityCost:Tariff object that is associated with this UtilityCost:Qualify object.

#### Field: Variable Name

The name of the variable used. For energy and demand, the automatically created variables, totalEnergy and totalDemand should be used, respectively.

#### Field: Qualify Type

Enter either "Minimum" or "Maximum".

#### Field: Threshold Value or Variable Name

The minimum or maximum value for the qualify. If the variable has values that are less than this value when the qualify type is minimum then the tariff may be disqualified. If the variable has values that are greater than this value when the qualify type is maximum then the tariff may be disqualified. For demand, it is still the minimum of the demands set each month even though the demands are, by definition, the peak value for the month. Depending on the threshold test entry (see below) this minimum applies to all months, some of the months, or just a single month.

#### Field: Season

If the UtilityCost:Qualify only applies to a season, enter the season name from the following list:

- Annual
- Winter
- Spring
- Summer
- Fall

If this field is left blank, it defaults to Annual.

#### Field: Threshold Test

Enter either "Count" or "Consecutive". Uses the number in Number of Months in one of two different ways depending on the Threshold Test. If the Threshold Test is set to "Count" then the qualification is based on the count of the total number of months per year. If the Threshold Test is set to "Consecutive" then the qualification is based on a consecutive number of months. If left blank this field defaults to count and it should be allowed to default for most cases.

#### Field: Number of Months 

A number from 1 to 12. If no value entered, 12 is assumed when the qualify type is minimum and 1 when the qualify type is maximum. This is the number of months that the threshold test applies to determine if the rate qualifies or not. If the season is less than 12 months (if it is not annual) then the value is automatically reduced to the number of months of the season.