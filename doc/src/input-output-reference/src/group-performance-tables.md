# Group – Performance Tables

This group of objects consists of tabular data which are used to characterize the performance of HVAC equipment. The use of performance tables eliminate the need to perform a regression analysis to calculate the performance curve equation coefficients. Two performance tables are available to represent any performance curve having one or two independent variables. The tabular data is entered as data pairs and represent each independent variable followed by the table output at that condition. When using these two performance table objects, a regression analysis may be performed during the simulation to create the associated performance curve. The regression analysis is performed when the user requests to evaluate the curve to the limits specified (see Interpolation Method input field). The performance curve that is created is used throughout the simulation and is also written to the eio file when the user sets the diagnostics output flag DisplayAdvancedVariables (ref. [Output:Diagnostics](#outputdiagnostics), DisplayAdvancedVariables)  This performance curve object may be directly used in future input files. The following descriptions define the input requirements for the performance table objects.

## Table:OneIndependentVariable

Input for the data table consists of a table name, the curve type, the interpolation method, and the data pairs representing the polynomial equation. Optional inputs for minimum and maximum values for the independent variable and curve minimum and maximum may be used to limit the output of the table when evaluating the regression curve (e.g., limit extrapolation). The existing performance curves represented by this table object are:

![](media/image579.png)\


![](media/image580.png)\


![](media/image581.png)\


![](media/image582.png)\


![](media/image583.png)\


### Inputs

#### Field: Name

A unique user assigned name for an instance of a performance table. When a table is used, it is referenced by this name. The name of this table object may be used anywhere a valid performance curve object is allowed.

#### Field: Curve Type

The form of the polynomial equation used to represent the tabular data. Choices are *Linear*, *Quadratic*, *Cubic*, *Quartic* and *Exponent*. This field is used to determine if this table object is representative of the type of performance table (curve) allowed for a particular application (i.e., if this type of polynomial is allowed to be used as the curve type for other objects). This input infers the polynomial order (or degree). This input also specifies the number of interpolation points if the next field, Interpolation Method is set to LagrangeInterpolationLinearExtrapolation. The number of interpolation points for Linear, Quadratic, Cubic, Quartic, and Exponent are set at 2, 3, 4, 5, and 4, respectively.

#### Field: Interpolation Method

The method used to evaluate the tabular data. Choices are *LinearInterpolationOfTable, LagrangeInterpolationLinearExtrapolation* and *EvaluateCurveToLimits*. When *LinearInterpolationOfTable* is selected, the tabular data is interpolated within the limits of the data set and the following fields are used only within the boundary of the table limits (i.e., are only used to limit interpolation). Extrapolation of the data set is not allowed. When *LagrangeInterpolationLinearExtrapolation* is selected, a polynomial interpolation routine is used within the table boundaries along with linear extrapolation. Given the valid one independent variable equations above, the polynomial order is up to 5^th^ order. When *EvaluateCurveToLimits* is selected, the coefficients (i.e., C in the equations above) of the polynomial equation are calculated and used to determine the table output within the limits specified in the following fields. If these fields are not entered, the limits of the tabular data are used. The minimum number of data pairs required for this choice are 2, 3, 4, 5, and 4 data pairs for linear, quadratic, cubic, quartic, and exponent curves, respectively. If insufficient data is provided, the regression analysis is not performed and the simulation reverts to interpolating the tabular data. The performance curve is written to the eio file when the output diagnotics flag DisplayAdvancedVariables is used (ref. [Output:Diagnostics](#outputdiagnostics), DisplayAdvancedVariables).

#### Field: Minimum Value of X

The minimum allowable value of X. Values of X less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method*. If this field is left blank, the data set determines the limit.

#### Field: Maximum Value of X

The maximum allowable value of X. Values of X greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method*. If this field is left blank, the data set determines the limit.

#### Field: Minimum Table Output

The minimum allowable value of the table output. Values less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method*. If this field is left blank, the data set determines the limit.

#### Field: Maximum Table Output

The maximum allowable value of the table output. Values greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method*. If this field is left blank, the data set determines the limit.

#### Field: Input Unit Type for X1

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of X. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Distance
- Power

#### Field: Output Unit Type

This field is used to indicate the kind of units that may be associated with the output values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Curve Output and Maximum Curve Output. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Capacity
- Power

#### Field: Normalization Reference

The normalization point for the data set. This field provides a method for automatically normalizing a set of data. Equipment performance data is typically normalized based on a specific operating condition. Both the output values and minimum/maximum curve limits are normalized. This normalization can be performed simply by entering the data at the reference point. If this field is left blank or a 1 is entered, the data set is assumed to be in the correct format.

#### Data Pairs

The following inputs describe the data set. Data pairs are entered as X and Output pairs where X is the independent variable (i.e., x in the equation above) and Output is the dependent variable (i.e., Output in the equation above). Any number of data pairs may be entered, however, a minimum number of data pairs must be entered and depends on the polynomial equation. Linear (1^st^ order), Quadratic (2^nd^ order), Cubic (3^rd^ order), Quartic (4^th^ order), and Exponent (nth order) polynomials require a minimum of 2, 3, 4, 5, and 4 data pairs, respectively. This minimum requirement is for performing a regression analysis, not interpolating the tabular data. If a regression analysis is not performed (i.e., only table interpolation is used), these minimum requirements are not enforced. If a regression analysis is performed and these minimum requirements are not met, the simulation will revert to table interpolation. These fields may be repeated as many times as necessary to define the tabular data.

#### Field: X Value #n

The value of the independent variable n (i.e., independent variable values for data 1 to number of data pairs).

#### Field: Output Value #n

The value of the dependent variable n (i.e., dependent variable values for data 1 to number of data pairs).

Following is an example input for a [Table:OneIndependentVariable](#tableoneindependentvariable) object:

~~~~~~~~~~~~~~~~~~~~

    Table:OneIndependentVariable,
    WindACCoolCapFFF,       !- Name
    Linear,                 !- Curve Type
    EvaluateCurveToLimits,  !- Interpolation Type
    0,                      !- Minimum Value of X1,
    1.5,                    !- Maximum Value of X1,
    0.8,                    !- Minimum Table Output
    1.1,                    !- Maximum Table Output
    Dimensionless,          !- Input Unit Type for X1
    Dimensionless,          !- Output Unit Type
    ,                       !- Normalization Point
    0,0.8,                  !- X Value #1, Output Value #1
    1.5,1.1;                !- X Value #2, Output Value #2
~~~~~~~~~~~~~~~~~~~~

### Outputs

#### Performance Curve Output Value []

The current value of the performance table. Performance curves and tables use the same output variable. This value is averaged over the time step being reported. Inactive or unused performance curves will show a value of -999 (e.g., equipment is off, a specific performance curve is not required for this aspect of the equipment model at this time step, etc.). This value means that the performance curve was not called during the simulation and, therefore, not evaluated. This inactive state value is only set at the beginning of each environment. When averaging over long periods of time, this inactive state value may skew results. In this case, use a detaled reporting frequency (ref. [Output:Variable](#outputvariable) object) to view results at each HVAC time step.

#### Performance Curve Input Variable 1 Value []

The current value of the independent variable passed to the performance curve. This value is averaged over the time step being reported.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Performance Curve Output Value []
    HVAC,Average,Performance Curve Input Variable 1 Value []
~~~~~~~~~~~~~~~~~~~~

## Table:TwoIndependentVariables

Input for the data table consists of a table name, the curve type, the interpolation method, and the data pairs representing the polynomial equation. Optional inputs for minimum and maximum values for the independent variable, and curve minimum and maximum may be used to limit the output of the table (e.g., limit extrapolation) when evaluating the regression curve instead of the tabular data. The polynomial equations currently represented by this table object are:

![](media/image584.png)\


![](media/image585.png)\


### Inputs

#### Field: Name

A unique user assigned name for an instance of a performance table. When a table is used, it is referenced by this name. The name of this table object may be used anywhere a valid performance curve object is allowed.

#### Field: Curve Type

The form of the polynomial equation represented by the entered data. The only valid choices are *BiQuadratic* and *QuadraticLinear*. This field is used to determine if this table object is representative of the type of performance table (curve) allowed for a particular equipment model (i.e., if this type of polynomial is allowed to be used as the curve type for specific input fields). A minimum of 6 data pairs are required when selecting *EvaluateCurveToLimits* in the following field. This input also specifies the number of interpolation points if the next field, Interpolation Method is set to LagrangeInterpolationLinearExtrapolation. The number of interpolation points for BiQuadratic and QuadraticLinear are set at 6.

#### Field: Interpolation Method

The method used to evaluate the tabular data. Choices are *LinearInterpolationOfTable, LagrangeInterpolationLinearExtrapolation,* and *EvaluateCurveToLimits*. When *LinearInterpolationOfTable* is selected, the data entered is evaluated within the limits of the data set and the following fields are used only within the boundary of the table limits (i.e., are only used to limit interpolation). A complete data set is required. For each X variable, entries must include all Y variables and associated curve output (i.e., a complete table, no gaps or missing data). Extrapolation of the data set is not allowed. When Lagrange*InterpolationLinearExtrapolation* is selected, a polynomial interpolation routine is used within the table boundaries along with linear extrapolation. Given the valid equations above, the polynomial order is fixed at 2 which infers that 3 data points will be used for interpolation. When *EvaluateCurveToLimits* is selected, the coefficients (i.e., C in the equations above) of the polynomial equation are calculated and used to determine the table output within the limits specified in the following fields (or the tabular data, whichever provide the larger range). If the following fields are not entered, the limits of the data set are used. A complete table is not required when using this method, however, a minimum number of data is required to perform the regression analysis. The minimum number of data pairs required for this choice is 6 data pairs for both bi-quadratic and quadratic-linear curve types. If insufficient data is provided, the simulation reverts to interpolating the tabular data. The performance curve is written to the eio file when the output diagnotics flag DisplayAdvancedVariables is used (ref. [Output:Diagnostics](#outputdiagnostics), DisplayAdvancedVariables).

#### Field: Minimum Value of X

The minimum allowable value of X. Values of X less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method*. If this field is left blank, the data set determines the limit.

#### Field: Maximum Value of X

The maximum allowable value of X. Values of X greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method*. If this field is left blank, the data set determines the limit.

#### Field: Minimum Value of Y

The minimum allowable value of Y. Values of Y less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method*. If this field is left blank, the data set determines the limit.

#### Field: Maximum Value of Y

The maximum allowable value of Y. Values of Y greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method*. If this field is left blank, the data set determines the limit.

#### Field: Minimum Table Output

The minimum allowable value of the table output. Values less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method*. If this field is left blank, the data set determines the limit.

#### Field: Maximum Table Output

The maximum allowable value of the table output. Values greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method*. If this field is left blank, the data set determines the limit.

#### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the X values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of X. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Distance
- Power

#### Field: Input Unit Type for Y

This field is used to indicate the kind of units that may be associated with the Y values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of Y and Maximum Value of Y. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Distance
- Power

#### Field: Output Unit Type

This field is used to indicate the kind of units that may be associated with the output values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Curve Output and Maximum Curve Output. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Capacity
- Power

#### Field: Normalization Reference

The normalization point for the data set. This field provides a method for automatically normalizing a set of data. Equipment performance data is typically normalized based on a specific operating condition. Both the output values and minimum/maximum curve limits are normalized as applicable. This normalization can be performed simply by entering the data at the reference point. If this field is left blank or a 1 is entered, the data set is assumed to be in the correct format.

#### Data Pairs

The following inputs describe the data set. Data pairs are entered as X, Y and Output pairs where X and Y are the independent variables (i.e., x and y in the equations above) and Output is the dependent variable (i.e., Output in the equation above). Any number of data pairs may be entered, however, a minimum number of data pairs must be entered and depends on the polynomial equation. BiQuadratic and QuadraticLinear (2^nd^ order with 2 independent variables) require a minimum of 6 data pairs. This minimum requirement is for performing a regression analysis, not interpolating the tabular data. If a regression analysis is not performed (i.e., only table interpolation is used), these minimum requirements are not enforced. If a regression analysis is performed and these minimum requirements are not met, the simulation will revert to table interpolation. These fields may be repeated as many times as necessary to define the tabular data.

#### Field: X Value #n

The value of the first independent variable n (i.e., first independent variable values for data pair 1 to number of data pairs).

#### Field: Y Value #n

The value of the second independent variable n (i.e., second independent variable values for data pair 1 to number of data pairs).

#### Field: Output Value #n

The value of the dependent variable n (i.e., dependent variable values for data pair 1 to number of data pairs).

Following is an example input for a Table:TwoIndependentVariable object:

~~~~~~~~~~~~~~~~~~~~

    Table:TwoIndependentVariables,
    CCOOLEIRFT,             !- Name
    BiQuadratic,            !- Curve Type
    EvaluateCurveToLimits,  !- Interpolation Type
    5.0,                    !- Minimum Value of x,
    60.0,                   !- Maximum Value of x,
    5.0,                    !- Minimum Value of y,
    70.0,                   !- Maximum Value of y,
    0.01,                   !- Minimum Table Output
    0.5,                    !- Maximum Table Output
    Temperature,            !- Input Unit Type for x
    Temperature,            !- Input Unit Type for y
    Dimensionless,          !- Output Unit Type
    0.258266502,            !- Normalization Point
    17.2222222,18.3333333,0.18275919, !-X Value #1,Y Value #1,Output #1
    17.2222222,23.8888889,0.207383768,!-X Value #2,Y Value #2,Output #2
    17.2222222,29.4444444,0.237525541,!-X Value #3,Y Value #3,Output #3
    17.2222222,35,0.274217751,        !-X Value #4,Y Value #4,Output #4
    17.2222222,40.5555556,0.318548624,!-X Value #5,Y Value #5,Output #5
    17.2222222,46.1111111,0.374560389,!-X Value #6,Y Value #6,Output #6
    19.4444444,18.3333333,0.172982816,!-X Value #7,Y Value #7,Output #7
    19.4444444,23.8888889,0.19637691, !-X Value #8,Y Value #8,Output #8
    19.4444444,29.4444444,0.224789822,!-X Value #9,Y Value #9,Output #9
    19.4444444,35,0.258266502,        !-X Value #10,Y Value #10,Output #10
    19.4444444,40.5555556,0.299896794,!-X Value #11,Y Value #11,Output #11
    19.4444444,46.1111111,0.351242021,!-X Value #12,Y Value #12,Output #12
    21.6666667,18.3333333,0.164081122,!-X Value #13,Y Value #13,Output #13
    21.6666667,23.8888889,0.185603303,!-X Value #14,Y Value #14,Output #14
    21.6666667,29.4444444,0.211709812,!-X Value #15,Y Value #15,Output #15
    21.6666667,35,0.243492052,        !-X Value #16,Y Value #16,Output #16
    21.6666667,40.5555556,0.281757875,!-X Value #17,Y Value #17,Output #17
    21.6666667,46.1111111,0.331185659;!-X Value #18,Y Value #18,Output #18
~~~~~~~~~~~~~~~~~~~~

### Outputs

#### Performance Curve Output Value []

The current value of the performance table. Performance curves and tables use the same output variable. This value is averaged over the time step being reported. Inactive or unused performance curves will show a value of -999 (e.g., equipment is off, a specific performance curve is not required for this aspect of the equipment model at this time step, etc.). This value means that the performance curve was not called during the simulation and, therefore, not evaluated. This inactive state value is only set at the beginning of each environment. When averaging over long periods of time, this inactive state value may skew results. In this case, use a detaled reporting frequency (ref. [Output:Variable](#outputvariable) object) to view results at each HVAC time step.

#### Performance Curve Input Variable 1 Value []

#### Performance Curve Input Variable 2 Value []

The current value of the first and second independent variable passed to the performance curve. The order of the independent variables is in the same order as the model equation represented by this performance curve object. This value is averaged over the time step being reported.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Performance Curve Output Value []
    HVAC,Average,Performance Curve Input Variable 1 Value []
    HVAC,Average,Performance Curve Input Variable 2 Value []
~~~~~~~~~~~~~~~~~~~~

## Lookup Tables

Lookup tables are useful for determining the results of a nth order polynomial without the need to calculate the polynomial coefficients. The Lagrange method for interpolation is currently the only method available for polynomial interpolation. LinearInterpolationOfTable and EvaluateCurveToLimits may also be used. The multi-variable lookup table can represent from 1 to 5 independent variables and can interpolate these independent variables up to a 4^th^ order polynomial. The polynomial order is assumed to be the number of interpolation points (n) minus 1. Polynomial interpolation of tabular data takes place when the values of the independent variables are within the upper table boundary and at least n data points are available when approaching the lower table boundary for any independent variable. When any independent variable value is outside the table limits, linear extrapolation is used to predict the table result and is based on the two nearest data points in the table for that particular independent variable.

## Table:MultiVariableLookup

Input for the multi-variable data table consists of a table name, the interpolation method, the number of points to be used for interpolation, an optional external file name, and, if an external file name is not entered, the tabular data. Optional inputs for minimum and maximum values for the independent variable, and curve minimum and maximum may be used to limit the output of the table (e.g., limit extrapolation).

### Inputs

#### Field: Name

A unique user assigned name for an instance of a lookup table. When a table is used, it is referenced by this name. The name of this table object may be used anywhere a valid performance curve object is allowed.

#### Field: Curve Type

The order of the polynomial used to represent the tabular data. Choices are *Linear*, *Quadratic*, *Cubic*, *Quartic, BiQuadratic, QaudraticLinear, BuCibic*. This field is used to determine if this table object is representative of the type of performance table (curve) allowed for a particular application (i.e., if this type of polynomial is allowed to be used as the curve type for other objects). This input also infers the polynomial order (or degree).

#### Field: Interpolation Method

The method used to evaluate the lookup table. The choices are *LinearInterpolationOfTable, LagrangeInterpolationLinearExtrapolation, and EvaluateCurveToLimits*. . When *LinearInterpolationOfTable* is selected, the data entered is evaluated within the limits of the data set and the minimum/maximum value of x and output inputs are used only within the boundary of the table limits (i.e., are only used to limit interpolation). A complete data set is required. For each X variable, entries must include all Y variables and associated curve output (i.e., a complete table, no gaps or missing data). Extrapolation of the data set is not allowed. When *LagrangeInterpolationLinearExtrapolation* is selected, the lookup table is evaluated using Lagrange's form of the interpolating polynomial. The order of the polynomial is inferred to be one less than the input in the Number of Interpolation Points field below. When *EvaluateCurveToLimits* is selected, the coefficients (i.e., C in the equation above) of the polynomial equation are calculated and used to determine the table output within the limits specified in the following fields (or the tabular data, whichever provide the larger range). If the minimum/maximum fields below are not entered, the limits of the data set are used. A complete table is not required when using this method, however, a minimum number of data is required to perform the regression analysis. The minimum number of data pairs required for this choice is 6 data pairs for both bi-quadratic and quadratic-linear curve types. If insufficient data is provided, the simulation reverts to interpolating the tabular data. The performance curve is written to the eio file when the output diagnotics flag DisplayAdvancedVariables is used (ref. [Output:Diagnostics](#outputdiagnostics), DisplayAdvancedVariables). A regression analysis may only be performed on lookup tables with one or two independent variables.

#### Field: Table Data Format

The format specifier which determines the method used to read the lookup table. The only valid choice at this time is *SingleLineIndependentVariableWithMatrix*. When *SingleLineIndependentVariableWithMatrix* is selected, a free format is used as shown below. The format shown below assumes the output values are in ascending order for both independent variables 1 and 2. Tabular data are entered in 3 groups of data, the header, the independent variable values, and the tabular data matrix. A summary of the external file format is shown below.

~~~~~~~~~~~~~~~~~~~~

    Key:
    IV = independent variable (i.e., the X1, or X2, etc)
    NIV = number of independent variables (i.e., number of values for X1, or X2, etc.)
    IVV = independent variable value (i.e., the value for X1, or X2, etc.)
    NIV1 = number of values for independent variable #1 (i.e., X1)
    NIV2 = number of values for independent variable #2 (i.e., X2)
    NIV3 = number of values for independent variable #3 (i.e., X3)
    NIV4 = number of values for independent variable #4 (i.e., X4)
    NIV5 = number of values for independent variable #5 (i.e., X5)
    OV = output value

    Header:
    Row 1: <NIV>, <NIV1>, <NIV2> (only used if NIV>1), <NIV3> (only used if NIV>2),
    <NIV4> (only used if NIV>3), <NIV5> (only used if NIV>4)

    Independent Variable Values:
    Row 2: <IVV1 to IVVn for IV1> where n = NIV1 (max = 10)
    Row 2a (row included only when NIV > 1): IVV1 to IVVn for IV2 where n = NIV2 (max = 10)
    Row 2b (row included only when NIV > 2): IVV1 to IVVn for IV3 where n = NIV3 (max = 10)
    Row 2c (row included only when NIV > 3): IVV1 to IVVn for IV4 where n = NIV4 (max = 10)
    Row 2d (row included only when NIV > 4): IVV1 to IVVn for IV5 where n = NIV5 (max = 10)

    Tabular Data Matrix Format (repeat rows 3 and 4(1) – 4(m) for each occurrence of a unique data pair for independent variable #3, #4, and #5):

    Row 3 (row included only when NIV > 2): IVVn for IV3, IVVn for IV4 (included only when NIV > 3), IVVn for IV5 (included only when NIV > 4), (where n = a unique value for the independent variable)

    Row 4(1): OV1/1 to OVn/1 for each value of IV1 where n = NIV1 representing IV2(1)
    Row 4(2): OV1/2 to OVn/2 for each value of IV1 where n = NIV1 representing IV2(2)
    Row 4(3): OV1/3 to OVn/3 for each value of IV1 where n = NIV1 representing IV2(3)
    ...
    Row 4(m): OV1/m to OVn/m for each value of IV1 where n = NIV1 representing IV2(m) where m = NIV2
~~~~~~~~~~~~~~~~~~~~

#### Field: Number of Interpolation Points

This field specifies the number of data points to be used in the lookup table interpolation. The value specified in this field should be 1 greater than the order (or degree) of the polynomial used for interpolation. For example, if 2 is entered here, the polynomial is assumed to be first order where only 2 data points are required to calculate the table solution. If 3 is entered here, the polynomial is assumed to be second order and 3 data points would be required to calculate the table solution. Linear extrapolation is always used when the independent variable values are near the lower edge or outside the range of the table. The interpolation points selected are always the nearest data point greater than the independent variable with 1 or more values less than the independent variable. For example, if 3 interpolation points are used, the data point greater than the independent variable and 2 data points less than the indpenedent variable are used to determine the table solution. If the independent variable exactly equals a table reference point, that value is used as the table solution.

#### Field: External File Name

The name of an external file that represents the tabular data. This file would include all data after the Output Unit Type field below starting with the number of independent variables and continuing until all input and output values have been entered. The tabular data may use either comma, space or tab delimited format. The field should include a full path with file name, for best results. The field must be <= 100 characters. The file name must not include commas or an exclamation point. A relative path or a simple file name should work with version 7.0 or later when using EP-Launch even though EP-Launch uses temporary directories as part of the execution of EnergyPlus. If using RunEPlus.bat to run EnergyPlus from the command line, a relative path or a simple file name may work if RunEPlus.bat is run from the folder that contains EnergyPlus.exe.

#### Field: X1 Sort Order

The choice field specifies the order of the lookup table data for the 1^st^ independent variable. Valid choices are *Increasing* or *Decreasing*. If increasing is selected, then the order of the data is from lowest value of the 1^st^ independent variable to highest. If decreasing is selected, then the order of the data is from highest value of the 1^st^ independent variable to lowest.

#### Field: X2 Sort Order

The choice field specifies the order of the lookup table data for the 2^nd^ independent variable. Valid choices are *Increasing* or *Decreasing*. If increasing is selected, then the order of the data is from lowest value of the 2^nd^ independent variable to highest. If decreasing is selected, then the order of the data is from highest value of the 2^nd^ independent variable to lowest.

#### Field: Minimum Value of X1

The minimum allowable value of X1. Values of X1 less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit.

#### Field: Maximum Value of X1

The maximum allowable value of X1. Values of X1 greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit.

#### Field: Minimum Value of X2

The minimum allowable value of X2. Values of X2 less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit. This field is only used when the number of independent variables is greater than 1.

#### Field: Maximum Value of X2

The maximum allowable value of X2. Values of X2 greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit. This field is only used when the number of independent variables is greater than 1.

#### Field: Minimum Value of X3

The minimum allowable value of X3. Values of X3 less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit. This field is only used when the number of independent variables is greater than 2.

#### Field: Maximum Value of X3

The maximum allowable value of X3. Values of X3 greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit. This field is only used when the number of independent variables is greater than 2.

#### Field: Minimum Value of X4

The minimum allowable value of X4. Values of X4 less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit. This field is only used when the number of independent variables is greater than 3.

#### Field: Maximum Value of X4

The maximum allowable value of X4. Values of X4 greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit. This field is only used when the number of independent variables is greater than 3.

#### Field: Minimum Value of X5

The minimum allowable value of X5. Values of X5 less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit. This field is only used when the number of independent variables is greater than 4.

#### Field: Maximum Value of X5

The maximum allowable value of X5. Values of X5 greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit. This field is only used when the number of independent variables is greater than 4.

#### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit.

#### Field: Maximum Curve Output

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum. This field can only limit extrapolation when the *Interpolation Method* is selected as *EvaluateCurveToLimits or LagrangeInterpolationLinearExtrapolation*. This filed can be used to limit interpolation using any *Interpolation Method.* If this field is left blank, the data set determines the limit.

#### Field: Input Unit Type for X1

This field is used to indicate the kind of units that may be associated with the X1 values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X1 and Maximum Value of X1. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Distance
- Power

#### Field: Input Unit Type for X2

This field is used to indicate the kind of units that may be associated with the X2 values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X2 and Maximum Value of X2. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Distance
- Power

#### Field: Input Unit Type for X3

This field is used to indicate the kind of units that may be associated with the X3 values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X3 and Maximum Value of X3. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Distance
- Power

#### Field: Input Unit Type for X4

This field is used to indicate the kind of units that may be associated with the X4 values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X4 and Maximum Value of X4. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Distance
- Power

#### Field: Input Unit Type for X5

This field is used to indicate the kind of units that may be associated with the X5 values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X5 and Maximum Value of X5. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Distance
- Power

#### Field: Output Unit Type

This field is used to indicate the kind of units that may be associated with the output values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Curve Output and Maximum Curve Output. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Capacity
- Power

## Data Input Format

The following inputs describe the data set. These values may also be placed in an external file starting with Field #1. If an external file name is entered above, the following fields are not used. Data are entered in a specific order according to the number and quantity of independent variables. The data input format is described here in 3 groups as Header, Independent Variable Values, and the Tabular Data. The header is always the first line in the data set and represents the number of independent varibles followed by the number of values for each independent variable. The independent variable values are listed next and represent the values of each independent variable on a separate line. These values are only listed once at the top of the data set and are listed in ascending order. The number of independent variables, the first number in the header, specifies how many lines, or rows, make up this section of tabular data. The remaining lines are repeated as many times as necessary to completely describe the tabular data. The values of independent variables 3 – n are listed on a single line followed by a matrix of data that represent the table output corresponding to independent variables 1 and 2.

- **Header:** 

Line 1: The first number specified is the number of independent variables followed by the number of variables for each independent variable.

- **Independent Variable Values:**

Line 1: the n values for the first independent variable as specified in the header.

Line 2: the n values for the second independent variable as specified in the header if the number of independent variables is greater than 1. This line is not required if the number of independent variables is less than 2.

Line 3: the n values for the third independent variable as specified in the header if the number of independent variables is greater than 2. This line is not required if the number of independent variables is less than 3.

Line 4: the n values for the fourth independent variable as specified in the header if the number of independent variables is greater than 3. This line is not required if the number of independent variables is less than 4.

Line 5: the n values for the fifth independent variable as specified in the header if the number of independent variables is greater than 4. This line is not required if the number of independent variables is less than 5.

- **Tabular Data:**

Line 1: the values of the 3^rd^ through nth independent variables

Line 2 – m (where m-1 equals the number of variables for the second independent variable): the tabular data corresponding to the 1^st^ independent variable (the number of inputs on this line equals the number of values for 1^st^ independent variable), the mth values for the 2^nd^ independent variable and corresponding to the values for the 3^rd^ – nth independent variables (see Line 1 above for Tabular Data).

Three examples are provided here as this can be difficult to describe in text. This example assumes that the table data are listed in ascending order from lowest to highest.

- **One Independent Variable:**
- **Header:**

1, 5, (1 independent variable with 5 values for the single independent variable)

- **Independent Variable Values:**

70, 72, 74, 76, 78, (the five values for the single independent variable, these values are always listed in ascending order)

- **Tabular Data:**

0.1, 0.3, 0.5, 0.7, 0.9; (the table values associated with the independent variable values listed previously – Note: these values may be read in ascending or descending order based on the input in the X1 Sort Order field above). Example: If X1 Sort Order is ascending, 0.1 corresponds to the value of 70 for the first independent variable. If X1 Sort Order is descending, then 0.1 corresponds to the value of 78 for the first independent variable.  The use of ascending order provides a direct correlation to inputs for the Independent Variable Values group above.

- **Two Independent Variables:**
- **Header:**

2, 5, 6, (2 independent variables with 5 and 6 values for the 1^st^ and 2^nd^ independent variables, respectively)

- **Independent Variable Values:**

70, 72, 74, 76, 78, (the five values for the 1st independent variable, these values are always listed in ascending order)

32, 45, 68, 81, 94, 107, (the six values for the 2nd independent variable, these values are always listed in ascending order)

- **Tabular Data:**

0.1, 0.3, 0.5, 0.7, 0.9, (the first five table values associated with each value of the 1^st^ independent variable and the first value (32) of the second independent variable listed previously – Note: these values may be listed in ascending or descending order based on the input in the X1 Sort Order field above. Each of these rows may also be listed in ascending or descending order based on the input in the X2 Sort Order field above).

0.2, 0.4, 0.6, 0.8, 1.0, (the second five table values associated with each value of the 1^st^ independent variable and the second value (45) of the second independent variable listed previously).

0.3, 0.5, 0.7, 0.9, 1.1, (the third five table values associated with each value of the 1^st^ independent variable and the third value (68) of the second independent variable listed previously).

0.4, 0.6, 0.8, 1.0, 1.2, (the fourth five table values associated with each value of the 1^st^ independent variable and the fourth value (81) of the second independent variable listed previously).

0.5, 0.7, 0.9, 1.1, 1.3, (the fifth five table values associated with each value of the 1^st^ independent variable and the fifth value (94) of the second independent variable listed previously).

0.6, 0.8, 1.0, 1.2, 1.4; (the sixth five table values associated with each value of the 1^st^ independent variable and the sixth value (107) of the second independent variable listed previously).

- **Three Independent Variables:**
- **Header:**

3, 5, 6, 2, (3 independent variables with 5, 6 and 2 values for the 1^st^ , 2^nd^, and 3^rd^ independent variables)

- **Independent Variable Values:**

70, 72, 74, 76, 78, (the five values for the 1st independent variable, these values are always listed in ascending order)

32, 45, 68, 81, 94, 107, (the six values for the 2nd independent variable, these values are always listed in ascending order)

15.5, 45.5, (the two values for the 3^rd^ independent variable, these values are always listed in ascending order)

- **Tabular Data:**

15.5, (the first value for the 3^rd^ independent variable – values for the 4^th^ and 5^th^ independent variable are also listed on this line if they are used)

0.1, 0.3, 0.5, 0.7, 0.9, (the table values associated with each value of the 1^st^ independent variable, the first value (32) of the second independent variable, and the first value (15.5) of the 3^rd^ independent variable). Note: these values may be listed in ascending or descending order based on the input in the X1 Sort Order field above).

0.2, 0.4, 0.6, 0.8, 1.0, (the table values associated with each value of the 1^st^ independent variable, the second value (45) of the second independent variable, and the first value (15.5) of the 3^rd^ independent variable).

0.3, 0.5, 0.7, 0.9, 1.1, (the table values associated with each value of the 1^st^ independent variable, the third value (68) of the second independent variable, and the first value (15.5) of the 3^rd^ independent variable).

0.4, 0.6, 0.8, 1.0, 1.2, (the table values associated with each value of the 1^st^ independent variable, the fourth value (81) of the second independent variable, and the first value (15.5) of the 3^rd^ independent variable).

0.5, 0.7, 0.9, 1.1, 1.3, (the table values associated with each value of the 1^st^ independent variable, the fifth value (94) of the second independent variable, and the first value (15.5) of the 3^rd^ independent variable).

0.6, 0.8, 1.0, 1.2, 1.4. (the table values associated with each value of the 1^st^ independent variable, the sixth value (107) of the second independent variable, and the first value (15.5) of the 3^rd^ independent variable).

45.5, (the second value for the 3^rd^ independent variable – values for the 4^th^ and 5^th^ independent variable are also listed on this line if they are used)

1.1, 1.3, 1.5, 1.7, 1.9, (the table values associated with each value of the 1^st^ independent variable, the first value (32) of the second independent variable, and the second value (45.5) of the 3^rd^ independent variable).

1.2, 1.4, 1.6, 1.8, 2.0, (the table values associated with each value of the 1^st^ independent variable, the second value (45) of the second independent variable, and the second value (45.5) of the 3^rd^ independent variable).

1.3, 1.5, 1.7, 1.9, 2.1, (the table values associated with each value of the 1^st^ independent variable, the third value (68) of the second independent variable, and the second value (45.5) of the 3^rd^ independent variable).

1.4, 1.6, 1.8, 2.0, 2.2, (the table values associated with each value of the 1^st^ independent variable, the fourth value (81) of the second independent variable, and the second value (45.5) of the 3^rd^ independent variable).

1.5, 1.7, 1.9, 2.1, 2.3, (the table values associated with each value of the 1^st^ independent variable, the fifth value (94) of the second independent variable, and the second value (45.5) of the 3^rd^ independent variable).

1.6, 1.8, 2.0, 2.2, 2.4. (the table values associated with each value of the 1^st^ independent variable, the sixth value (107) of the second independent variable, and the second value (45.5) of the 3^rd^ independent variable).

> Note: The following fields define the tabular data as described in the previous examples. These field values depend on the number and quantity of independent variables used in the table. The tabular data can be entered in the following fields or entered using an external file. These fields are echoed to the eio file as a check when the output diganostics flag DisplayAdvancedVariables is used (ref. [Output:Diagnostics](#outputdiagnostics), DisplayAdvancedVariables). The normalization value is not included in the echoed values.

### Field: Field #1 (NumberOfIndependentVariables)

The number of independent variables used in the lookup table. This number (n) can range from 1 to 5. The number entered here will define the following n fields as the number of values for these n independent variables. This is the first number in the external file as shown in the previous examples.

### Field: Field #2 (NumberOfValuesForIndependentVariable1)

The number of values for the first independent variable. This is the second number in the tabular data and resides on the same line as Field #1.

### Field: Field #3 (DeterminedByPreviousFields)

The value specified in this field is dependent on the number entered in Field #1. If a 1 is entered in Field #1, this field represents the first value of the single independent variable. The number of values entered in Field #3-#n will be equal to the number entered in Field #2. Values entered in fields #3-#n are always entered in increasing value (ascending order). If a number greater than 1 is entered in Field #1, this field represents the number of values for the second independent variable.

### Field: Field #4

The value specified in this field is dependent on the number entered in Field #1. If a 1 is entered in Field #1, this field represents the second value of the single independent variable. The number of values entered in Field #3-#n will be equal to the number entered in Field #2. Values entered in fields #3-#n are always entered in increasing value (ascending order). If a 2 is entered in Field #1, this field represents the first value of the first independent variable. If a number greater than 2 is entered in Field #1, this field represents the number of values for the third independent variable.

<reduced for brevity>

Following is an example input for a [Table:MultiVariableLookup](#tablemultivariablelookup) object:

~~~~~~~~~~~~~~~~~~~~

    Table:MultiVariableLookup,
    HPACCoolCapFT,            !- Name
    LagrangeinterpolationLinearExtrapolation,  !- Interpolation Method
    3,                        !- Number of Interpolation Points
    BiQuadratic,              !- Interpolation Type
    SingleLineIndependentVariableWithMatrix, !- Table Data Format
    ,                         !- External File Name
    ASCENDING,                !- X1 Sort Order
    ASCENDING,                !- X2 Sort Order
    24999.9597049817,         !- Normalization reference
    12.77778,                 !- Minimum Value of X1,
    23.88889,                 !- Maximum Value of X1,
    18.0,                     !- Minimum Value of X2,
    46.11111,                 !- Maximum Value of X2,
    ,                         !- Minimum Value of X3
    ,                         !- Maximum Value of X3
    ,                         !- Minimum Value of X4
    ,                         !- Maximum Value of X4
    ,                         !- Minimum Value of X5
    ,                         !- Maximum Value of X5
    ,                         !- Minimum Table Output
    ,                         !- Maximum Table Output
    Temperature,              !- Input Unit Type for X1
    Temperature,              !- Input Unit Type for X2
    ,                         !- Input Unit Type for X3
    ,                         !- Input Unit Type for X4
    ,                         !- Input Unit Type for X5
    Dimensionless,            !- Output Unit Type
    2, 6, 7,
    12.77778, 15, 18, 19.44448943, 21, 23.88889,
    18, 24, 30, 35, 36, 41, 46.11111,
    24421.69383, 25997.3589, 28392.31868, 29655.22876, 31094.97495, 33988.3473,
    22779.73113, 24352.1562, 26742.74198, 28003.546, 29441.02425, 32330.1846,
    21147.21662, 22716.4017, 25102.61348, 26361.31143, 27796.52175, 30681.4701,
    19794.00525, 21360.49033, 23743.0571, 25000, 26433.32038, 29314.75872,
    19524.15032, 21090.0954, 23471.93318, 24728.52506, 26161.46745, 29042.2038,
    18178.81244, 19742.05753, 22120.2503, 23375.08713, 24806.13958, 27683.36592,
    16810.36004, 18370.84513, 20745.3119, 21998.35468, 23427.47518, 26301.11353;
~~~~~~~~~~~~~~~~~~~~

## Lookup Table Outputs

## Lookup Table Outputs

### Performance Curve Output Value []

The current value of the lookup table. Performance curves and tables use the same output variable. This value is averaged over the time step being reported. Inactive or unused performance curves will show a value of -999 (e.g., equipment is off, a specific performance curve is not required for this aspect of the equipment model at this time step, etc.). This value means that the performance curve was not called during the simulation and, therefore, not evaluated. This inactive state value is only set at the beginning of each environment. When averaging over long periods of time, this inactive state value may skew results. In this case, use a detaled reporting frequency (ref. [Output:Variable](#outputvariable) object) to view results at each HVAC time step.

### Performance Curve Input Variable 1(-N) Value []

The current value of the nth independent variable passed to the performance curve. The order of the independent variables is in the same order as the model equation represented by this performance curve object. This value is averaged over the time step being reported.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Performance Curve Output Value []
    HVAC,Average,Performance Curve Input Variable 1 Value []
    HVAC,Average,Performance Curve Input Variable 2 Value []
    HVAC,Average,Performance Curve Input Variable 3 Value []
    HVAC,Average,Performance Curve Input Variable 4 Value []
    HVAC,Average,Performance Curve Input Variable 5 Value []
~~~~~~~~~~~~~~~~~~~~