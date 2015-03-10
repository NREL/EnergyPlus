# Complex Tariff Modeling

Much can be accomplished with the UtilityCost:Charge:Simple and UtilityCost:Charge:Block objects. In almost all cases, these structures are flexible enough to model the tariff. At times the built in approach is not sufficient to model a particular rate. In those cases, the rate may be modeled by using the UtilityCost:Computation object. For most rates this object can be omitted but for difficult to model rates, they may be used to modify the normal way the rate computation is performed.

Each ComputeStep is performed in order and can be either a variable followed by an expression or simply a variable.

- demandMod SUM demand1 demand2
- chargeSummer1

The normal calculations associated with the UtilityCost:Qualify, UtilityCost:Charge:Simple, UtilityCost:Charge:Block, or UtilityCost:Ratchet are performed the first time that object is called. Subsequent times are not recomputed but use the value stored in the appropriate variable. The result of the computation for those objects is available in the expression for further computation. The expressions allow the manipulation of variables directly using simple mathematical functions. Only variable names may be entered along with operators. Numbers may not be entered and should be entered with UtilityCost:Variable. Multiple operators are allowed per step. Variables names do not include spaces. If spaces are found in a variable name then an error is shown. Unused variables are flagged with a warning. All expressions are in the form:

- assignedToVariable OPERATOR variable1 variable2

Each step is performed for each of the months before the next step is taken. The same operation is performed for each step as specified.

The actual evaluation of the expression is performed from the end of the line to the beginning using stack based operations. Some operators pull all values from the stack (SUM, MAXIMUM, MINIMUM) while most use just one or two values from the stack. All operations return a single value to the stack. This stack based approach to expressions is sometimes called Polish Notation. While multiple operators are allowed per line, it reduces confusion if each line has only a single operator.

When the UtilityCost:Computation object is used, it must contain references to all objects involved in the rate in the order that they should be computed. No checking is performed for objects that should or should not be included; instead, the computational process is driven completely by the input of this object.

A typical rate will have a simple series of steps. For example F the following steps would be created automatically by the software:

~~~~~~~~~~~~~~~~~~~~

    UtilityCost:Computation,
      SpecialStepsForF,             !- Name
      ExampleF,                     !- Tariff Name
      EnergyCharges SUM SummerOnPeak SummerOffPeak WinterOnPeak WinterOffPeak, Compute Step 1
      Basis SUM EnergyCharges DemandCharges ServiceCharges, Compute Step 2
      SubTotal SUM Basis Adjustments Surcharges, Compute Step 3
      Total SUM SubTotal Taxes, Compute Step 4
      MinDemand; Compute Step 5
~~~~~~~~~~~~~~~~~~~~

This example does not have any dependency of different charges that are summed on the same line but if it did the charges could appear by themselves on lines and this would indicate the order that they should be computed. Here is the same example again showing that form.

~~~~~~~~~~~~~~~~~~~~

    UtilityCost:Computation,
      SpecialStepsForF,            !- Name
      ExampleF,                    !- Tariff Name
      SummerOnPeak,                !- Compute Step 1
      SummerOffPeak,               !- Compute Step 2
      WinterOnPeak,                !- Compute Step 3
      WinterOffPeak,               !- Compute Step 4
      EnergyCharges SUM SummerOnPeak SummerOffPeak WinterOnPeak WinterOffPeak,  !- Compute Step 5
      Basis SUM EnergyCharges DemandCharges ServiceCharges,                     !- Compute Step 6
      SubTotal SUM Basis Adjustments Surcharges,   !- Compute Step 7
      Total SUM SubTotal Taxes,                    !- Compute Step 8
      MinDemand;                   !- Compute Step 9
~~~~~~~~~~~~~~~~~~~~

This second expanded format is generated automatically when no UtilityCost:Computation object is provided.

The functions built in are (number of arguments in parentheses):

### Operator:   SUM (unlimited)

Adds together all of the variable values for each month for as many variables that are used.

### Operator:   ADD (2)

Adds together the first variable value with the second variable value.

### Operator:   MULTIPLY (2)

Multiplies the first variable value with the second variable value. MULT may also be used.

### Operator:  SUBTRACT (2)

Subtracts the second variable value from the first variable value. SUBT may also be used.

### Operator:  DIVIDE (2)

Divides the first variable value by the second variable value. DIV may also be used.

### Operator:  ABSOLUTE (1)

Takes the absolute value of the variable value. This turns negative values into positive ones. ABS may also be used.

### Operator:  INTEGER (1)

Removes non-integer portions of the variable values. INT may also be used.

### Operator:  SIGN (1)

Returns a 1 for positive values, -1 for negative values and 0 for zero values.

### Operator:  MAXIMUM (unlimited)

Selects the maximum value across variables for each of the months. Two or more variables may be used. MAX may also be used.

### Operator:  MINIMUM (unlimited)

Selects the minimum value across variables for each of the months. Two or more variables may be used. MIN may also be used.

### Operator:  EXCEEDS (2)

Returns the difference between the first and second variable if the first variable is greater than the second. If the second variable is greater or equal to the first variable returns a zero.

### Operator:  ANNUALMINIMUM (1)

Returns the minimum value of the twelve months in the variable. Zero values are ignored (see also ANNUALMINZERO). If all values are zero then zero is returned. ANMIN may also be used.

### Operator:  ANNUALMAXIMUM (1)

Returns the maximum value of the twelve months in the variable. Zero values are ignored (see also ANNUALMAXZERO). If all values are zero then zero is returned. ANMAX may also be used.

### Operator:  ANNUALSUM (1)

Returns the sum of the twelve months in the variable. ANSUM may also be used.

### Operator:  ANNUALAVERAGE (1)

Returns the sum of the twelve monthly values divided by the number of non-zero monthly values. If all values are zero than zero is returned. If an average is desired that includes zero valued elements the SUM() function should be used divided by 12. ANAVG may also be used.

### Operator:  ANNUALOR (1)

Returns 1 for all monthly values if any of the monthly values are non-zero. ANOR may also be used.

### Operator:  ANNUALAND (1)

Returns 1 for all monthly values if all of the monthly values are non-zero. ANAND may also be used.

### Operator:  ANNUALMAXIMUMZERO (1)

Returns the maximum value of the twelve monthly values in x. Zeros are counted so if a zero is the largest value (the rest would be negative) a zero may be returned. ANMAXZ may also be used.

### Operator:  ANNUALMINIMUMZERO (1)

Returns the minimum value of the twelve monthly values in x. Zeros are counted. ANMINZ may also be used.

### Operator:  IF (3)

Returns the value of the second variable if the first variable is non-zero otherwise returns the value of the third variable. Usually used with GT, GE, LT, LE, EQ, NE operators.

### Operator:  GREATERTHAN (2)

1 if the first variable value is greater than the second variable value otherwise 0. GT may also be used.

### Operator:  GREATEREQUAL (2)

1 if the first variable value is greater than or equal to the second variable value otherwise 0. GE may also be used.

### Operator:  LESSTHAN (2)

1 if the first variable value is less than the second variable value otherwise 0. LT may also be used.

### Operator:  LESSEQUAL (2)

1 if the first variable value is less than or equal to the second variable value otherwise 0. LE may also be used.

### Operator:  EQUAL (2)

1 if the first variable value is equal to the second variable value otherwise 0. EQ may also be used.

### Operator:  NOTEQUAL (2)

1 if the first variable value is not equal to the second variable value otherwise 0. NE may also be used.

### Operator:  AND (2)

1 if the first variable value and second variable values are both not zero otherwise 0.

### Operator:  OR (2)

1 if the first variable value and second variable values are either not zero, otherwise 0 if both the first and second variable value are both zero.

### Operator:  NOT (2)

1 if the first variable value and only variable value is zero otherwise 0 if the first variable value is not zero.

### Operator:  FROM (Unlimited)

Indicates what variables a variable is dependant on (or derived from). No computation is performed. The algorithm that creates the automatic sequence of equations uses these to indicate when variables are dependant on other variables so that they can be sorted into the correct order.