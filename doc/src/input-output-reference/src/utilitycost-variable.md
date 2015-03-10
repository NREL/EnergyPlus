# UtilityCost:Variable

The UtilityCost:Variable object allows for the direct entry of monthly values into a variable.

## Inputs

#### Field: Name

The name of the variable.

#### Field: Tariff Name

The name of the tariff that is using the variable.

#### Field: Variable Type

This field is used to indicate the kind of units that may be associated with the variable. It is used by IDF Editor to display the appropriate SI and IP units for the January through December values. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Energy 
- Demand
- Currency
- Dimensionless

#### Field: January through December Values

An entry (up to 24 are available) of the monthly values of the variable. Normally 12 months are used for most utility rates and so 12 values should be entered. If any values are not entered, the value of the previous entry is used. This allows a variable that is constant for an entire year to be entered using only the first value.