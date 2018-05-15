# Refactoring of Performance Tables

## Justification for New Feature

The input of performance tables into EnergyPlus is a crucial part of most building energy modeling efforts, particularly in the representation of the performance of HVAC equipment. Recent feedback indicates that the current facility is increasingly becoming a pain point for users. Among the challenges users face are:

- The EnergyPlus documentation does not always match the implemented capabilities;
- Incomplete inputs are not fully vetted before computations are made, and
- Some tables are untested.

ASHRAE Standard 205, which seeks to facilitate sharing of equipment characteristics for performance simulation, is nearing completion, so better support for the upcoming standard is a highly desirable feature and provides an opportunity to address some of the current table-related issues in EnergyPlus. In support of ASHRAE Standard 205 integration into EnergyPlus, a new N-dimensional linear interpolation routine for gridded data is required. This is a critical capability for any tool utilizing ASHRAE 205 equipment data and the improved functionality will remove a noted deficiency.

## Overview

This work will be accomplished through several tasks:

1. Develop an N-dimensional interpolation method for gridded data. Where appropriate, utilization of existing open source routines and/or libraries is permitted provided that prior approval has been explicitly obtained from NREL prior to implementation. The interpolation method is not required to fill in any missing input data, but must recognize missing input data.
2. Refactor existing EnergyPlus table procedures to utilize the new methods or initiate the deprecation process for capabilities that are not currently fully functional.
3. Clean up EnergyPlus table input objects to have full, as-advertised, functionality. Where possible, warning and/or error messages should be added to advise the user of incomplete inputs.

This New Feature Proposal focuses largely on tasks #2 and #3. The interpolation method will be further defined in the design document.

The general proposal is to collapse the existing three table objects:

- *Table:OneIndependentVariable*,
- *Table:TwoIndependentVariables*, and
- *Table:MultiVariableLookup*

into a single new object: *Table:Lookup*.

## Discussion

### Abandoning curve-fitting capability

The current capability to fit curves to tables is limited to only some two-dimensional curve types. This feature allowed curve coefficients to be generated and used for tables (when using the "EvaluateCurveToLimits" extrapolation method). As manufacturers are already uneasy about the use of curve fits to represent their equipment, and the limited implementation of this feature, we are proposing dropping this feature from the new *Table:Lookup* objects. This also means that tables can be used anywhere where the dimensionality of the curve input matches that of the table.

### Using CSV imports for independent variables

When performance data is provided in tabular form, often times it accompanies the values of independent variables used to generate the data. Although this data is often repetitive (as it repeats each value for all possible combinations of the other independent variables), it may be convenient to include a way to read these values from the same file. This will be supported in the design of this feature, though the extent of error checking will be limited (e.g., correct order and cycling of independent values will not be validated).

## Input Output Reference Documentation

### Group Performance Tables

This group of objects consists of tabular data which are used to characterize the performance of HVAC equipment. The use of performance tables eliminate the need to perform a regression analysis to calculate the performance curve equation coefficients.

<!--The tabular data is entered as data pairs and represent each independent variable followed by the table output at that condition. When using these two performance table objects, a regression analysis may be performed during the simulation to create the associated performance curve. The regression analysis is performed when the user requests to evaluate the curve to the limits specified (see Interpolation Method input field). The performance curve that is created is used throughout the simulation and is also written to the eio file when the user sets the diagnostics output flag DisplayAdvancedVariables (ref. Output:Diagnostics, DisplayAdvancedVariables) This performance curve object may be directly used in future input files. The following descriptions define the input requirements for the performance table objects. -->

### Table:Lookup

Input for tables representing data with one or more independent variables.

#### Inputs

##### Field: Name

A unique user-assigned name for an instance of a lookup table. When a table is used, it is referenced by this name. The name of this table object may be used anywhere a valid performance curve object is allowed.

##### Field: Independent Variable List Name

The name of a Table:IndependentVariableList object that defines the independent variables that comprise the dimensions of the tabular data.

##### Field: Normalize Output

Determines whether or not this output will be automatically normalized to 1.0 at the normalized reference conditions defined by each independent variable in the associated independent variable list. Normalization applies to the Output Value fields as well as the Minimum and Maximum Output fields Choices are: *Yes* or *No*.

##### Field: Minimum Output

The minimum allowable value of the evaluated table after interpolation and extrapolation. Values less than the minimum will be replaced by the minimum. If this field is left blank, no limit is imposed.

##### Field: Maximum Output

The maximum allowable value of the evaluated table after interpolation and extrapolation. Values greater than the maximum will be replaced by the maximum. If this field is left blank, no limit is imposed.

##### Field: Output Unit Type

This field is used to indicate the kind of units that may be associated with the output values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Output and Maximum Output. If none of these options are appropriate, select *Dimensionless* which will have no unit conversion. Options are:

- *Dimensionless*
- *Capacity*
- *Power*

##### Field: External File Name

The name of an external CSV file that represents the tabular data. This file should be formatted such that the data for this particular output is ordered according to the order of the corresponding independent variables. For example, for three independent variables (`iv1`, `iv2`, `iv3`) with 3, 2, and 4 values respectively. The output values (`out[iv1][iv2][iv3]`) should be ordered as:


`iv1` | `iv2` | `iv3` | `output`
--- | --- | --- | ---
`iv1[1]` | `iv2[1]` | `iv3[1]` | `out[1][1][1]`
`iv1[1]` | `iv2[1]` | `iv3[2]` | `out[1][1][2]`
`iv1[1]` | `iv2[1]` | `iv3[3]` | `out[1][1][3]`
`iv1[1]` | `iv2[1]` | `iv3[4]` | `out[1][1][4]`
`iv1[1]` | `iv2[2]` | `iv3[1]` | `out[1][2][1]`
`iv1[1]` | `iv2[2]` | `iv3[2]` | `out[1][2][2]`
`iv1[1]` | `iv2[2]` | `iv3[3]` | `out[1][2][3]`
`iv1[1]` | `iv2[2]` | `iv3[4]` | `out[1][2][4]`
`iv1[2]` | `iv2[1]` | `iv3[1]` | `out[2][1][1]`
`iv1[2]` | `iv2[1]` | `iv3[2]` | `out[2][1][2]`
`iv1[2]` | `iv2[1]` | `iv3[3]` | `out[2][1][3]`
`iv1[2]` | `iv2[1]` | `iv3[4]` | `out[2][1][4]`
`iv1[2]` | `iv2[2]` | `iv3[1]` | `out[2][2][1]`
`iv1[2]` | `iv2[2]` | `iv3[2]` | `out[2][2][2]`
`iv1[2]` | `iv2[2]` | `iv3[3]` | `out[2][2][3]`
`iv1[2]` | `iv2[2]` | `iv3[4]` | `out[2][2][4]`
`iv1[3]` | `iv2[1]` | `iv3[1]` | `out[3][1][1]`
`iv1[3]` | `iv2[1]` | `iv3[2]` | `out[3][1][2]`
`iv1[3]` | `iv2[1]` | `iv3[3]` | `out[3][1][3]`
`iv1[3]` | `iv2[1]` | `iv3[4]` | `out[3][1][4]`
`iv1[3]` | `iv2[2]` | `iv3[1]` | `out[3][2][1]`
`iv1[3]` | `iv2[2]` | `iv3[2]` | `out[3][2][2]`
`iv1[3]` | `iv2[2]` | `iv3[3]` | `out[3][2][3]`
`iv1[3]` | `iv2[2]` | `iv3[4]` | `out[3][2][4]`

##### Field: External File Column Number

The column number (starting at 1) in the CSV file corresponding to this output.

##### Field: External File First Row Number

The row number (starting at 1) in the CSV file where the data for this output begins. If there are not enough rows of data to fill out the full grid of data an error will be presented to the user.

##### Output Value \<x\>

This field is repeated to capture the full set of output data in the table (if not otherwise defined in an external file). The data for this particular output is ordered according to the order of the corresponding independent variables. For example, for three independent variables (`iv1`, `iv2`, `iv3`) with 3, 2, and 4 values respectively. The output values (`out[iv1][iv2][iv3]`) should be ordered as:


`iv1` | `iv2` | `iv3` | `output`
--- | --- | --- | ---
`iv1[1]` | `iv2[1]` | `iv3[1]` | `out[1][1][1]`
`iv1[1]` | `iv2[1]` | `iv3[2]` | `out[1][1][2]`
`iv1[1]` | `iv2[1]` | `iv3[3]` | `out[1][1][3]`
`iv1[1]` | `iv2[1]` | `iv3[4]` | `out[1][1][4]`
`iv1[1]` | `iv2[2]` | `iv3[1]` | `out[1][2][1]`
`iv1[1]` | `iv2[2]` | `iv3[2]` | `out[1][2][2]`
`iv1[1]` | `iv2[2]` | `iv3[3]` | `out[1][2][3]`
`iv1[1]` | `iv2[2]` | `iv3[4]` | `out[1][2][4]`
`iv1[2]` | `iv2[1]` | `iv3[1]` | `out[2][1][1]`
`iv1[2]` | `iv2[1]` | `iv3[2]` | `out[2][1][2]`
`iv1[2]` | `iv2[1]` | `iv3[3]` | `out[2][1][3]`
`iv1[2]` | `iv2[1]` | `iv3[4]` | `out[2][1][4]`
`iv1[2]` | `iv2[2]` | `iv3[1]` | `out[2][2][1]`
`iv1[2]` | `iv2[2]` | `iv3[2]` | `out[2][2][2]`
`iv1[2]` | `iv2[2]` | `iv3[3]` | `out[2][2][3]`
`iv1[2]` | `iv2[2]` | `iv3[4]` | `out[2][2][4]`
`iv1[3]` | `iv2[1]` | `iv3[1]` | `out[3][1][1]`
`iv1[3]` | `iv2[1]` | `iv3[2]` | `out[3][1][2]`
`iv1[3]` | `iv2[1]` | `iv3[3]` | `out[3][1][3]`
`iv1[3]` | `iv2[1]` | `iv3[4]` | `out[3][1][4]`
`iv1[3]` | `iv2[2]` | `iv3[1]` | `out[3][2][1]`
`iv1[3]` | `iv2[2]` | `iv3[2]` | `out[3][2][2]`
`iv1[3]` | `iv2[2]` | `iv3[3]` | `out[3][2][3]`
`iv1[3]` | `iv2[2]` | `iv3[4]` | `out[3][2][4]`

#### Example IDF

```
Table:Lookup,
  !- Name
  !- Interpolation Method
  !- Extrapolation Method
  !- Curve Type
  !- Independent Variable List Name
  !- Normalize Output
  !- Minimum Output
  !- Maximum Output
  !- Output Unit Type
  !- External File Name
  !- External File Column Number
  !- External File First Row Number
  !- Output Value 1
  !- ...
  !- Output Value N
```

#### Outputs

##### Performance Curve Output Value []

The current value of the performance table. Performance curves and tables use the same output variable. This value is averaged over the time step being reported. Inactive or unused performance curves will show a value of -999 (e.g., equipment is off, a specific performance curve is not required for this aspect of the equipment model at this time step, etc.). This value means that the performance curve was not called during the simulation and, therefore, not evaluated. This inactive state value is only set at the beginning of each environment. When averaging over long periods of time, this inactive state value may skew results. In this case, use a detailed reporting frequency (ref. Output:Variable object) to view results at each HVAC time step.

##### Performance Curve Input Variable \<x\> Value []

### Table:IndependentVariableList

A list of Table:IndependentVariable references that define the size and dimensions of the data for one or more Table:Lookup objects. The order of this list defines the order that the tabular data must be defined. The output values in the Table:Lookup associated with this list will list the output in an order cycling through the last item in the list first, and then the second to last, and so on with the the first item cycling last. For example, for three independent variables (`iv1`, `iv2`, `iv3`) with 3, 2, and 4 values respectively. The output values (`out[iv1][iv2][iv3]`) should be ordered as:


`iv1` | `iv2` | `iv3` | `output`
--- | --- | --- | ---
`iv1[1]` | `iv2[1]` | `iv3[1]` | `out[1][1][1]`
`iv1[1]` | `iv2[1]` | `iv3[2]` | `out[1][1][2]`
`iv1[1]` | `iv2[1]` | `iv3[3]` | `out[1][1][3]`
`iv1[1]` | `iv2[1]` | `iv3[4]` | `out[1][1][4]`
`iv1[1]` | `iv2[2]` | `iv3[1]` | `out[1][2][1]`
`iv1[1]` | `iv2[2]` | `iv3[2]` | `out[1][2][2]`
`iv1[1]` | `iv2[2]` | `iv3[3]` | `out[1][2][3]`
`iv1[1]` | `iv2[2]` | `iv3[4]` | `out[1][2][4]`
`iv1[2]` | `iv2[1]` | `iv3[1]` | `out[2][1][1]`
`iv1[2]` | `iv2[1]` | `iv3[2]` | `out[2][1][2]`
`iv1[2]` | `iv2[1]` | `iv3[3]` | `out[2][1][3]`
`iv1[2]` | `iv2[1]` | `iv3[4]` | `out[2][1][4]`
`iv1[2]` | `iv2[2]` | `iv3[1]` | `out[2][2][1]`
`iv1[2]` | `iv2[2]` | `iv3[2]` | `out[2][2][2]`
`iv1[2]` | `iv2[2]` | `iv3[3]` | `out[2][2][3]`
`iv1[2]` | `iv2[2]` | `iv3[4]` | `out[2][2][4]`
`iv1[3]` | `iv2[1]` | `iv3[1]` | `out[3][1][1]`
`iv1[3]` | `iv2[1]` | `iv3[2]` | `out[3][1][2]`
`iv1[3]` | `iv2[1]` | `iv3[3]` | `out[3][1][3]`
`iv1[3]` | `iv2[1]` | `iv3[4]` | `out[3][1][4]`
`iv1[3]` | `iv2[2]` | `iv3[1]` | `out[3][2][1]`
`iv1[3]` | `iv2[2]` | `iv3[2]` | `out[3][2][2]`
`iv1[3]` | `iv2[2]` | `iv3[3]` | `out[3][2][3]`
`iv1[3]` | `iv2[2]` | `iv3[4]` | `out[3][2][4]`

#### Inputs

##### Field: Name

A unique user-assigned name for a list of independent variables. This name is referenced by Table:Lookup objects. The name of this list object may be referenced by any number of Table:Lookup objects.

##### Field: Independent Variable \<x\> Name

This field is repeated for the number of independent variables that define the dimensions of any corresponding Table:Lookup objects that refer to this list. Each instance provides the name of a Table:IndependentVariable object that defines the values and properties of an independent variable.

#### Example IDF

```
Table:IndependentVariableList,
  !- Name
  !- Independent Variable 1 Name
  !- ...
  !- Independent Variable N Name
```

### Table:IndependentVariable

Independent variables are used to define the size and dimensions of a Table:Lookup object.

#### Inputs

##### Field: Name

A unique user-assigned name for an independent variables. This name is referenced by Table:IndependentVariableList objects. The name of this object may be referenced by any number of Table:IndependentVariableList objects.

##### Field: Interpolation Method

Method used to determine the value of the table within the bounds of its independent variables. The choices are:

- *Linear*
- *Cubic*

##### Field: Extrapolation Method

Method used to determine the value of the table beyond the bounds of its independent variables. The choices are:

- *Constant*: Value is the same as the interpolated value at the closest point along the table's boundary.
- *Linear*: Value is linearly extrapolated in all dimensions from the interpolated value at the closest point along the table's boundary.
- *Unavailable*: The object using this curve is assumed to be not operable outside of the provided range of values. This will override any corresponding availability schedules of applicable equipment.


##### Field: Minimum Value

The minimum allowable value. Table:Lookup output for values between this value and the lowest value provided for this independent variable will be extrapolated according to the extrapolation method. Below this value, extrapolation is held constant and a warning will be issued. If extrapolation method is "Unavailable", the corresponding equipment will be disabled for all values less than the lowest independent variable value, regardless of the minimum value set here.

##### Field: Maximum Value

The maximum allowable value. Table:Lookup output for values between this value and the highest value provided for this independent variable will be extrapolated according to the extrapolation method. Above this value, extrapolation is held constant and a warning will be issued. If extrapolation method is "Unavailable", the corresponding equipment will be disabled for all values greater than the highest independent variable value, regardless of the maximum value set here.

##### Field: Unit Type

This field is used to indicate the kind of units that may be associated with this independent variable. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value and Maximum Value. The available options are shown below. If none of these options are appropriate, select *Dimensionless* which will have no unit conversion.

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Distance
- Power

##### Field: Normalization Value

The value of this independent variable where nominal or rated output is defined. This will be used to normalize the data so that the outputs of any Table:Lookup at this value (and the corresponding Normalization Values of the other independent variables described in the same Table:IndependentVariableList object) are equal to 1.0.

##### Field: External File Name

The name of an external CSV file that represents the tabular data. This file should be formatted such that the data for any output is ordered according to the order of the corresponding independent variables. For example, for three independent variables (`iv1`, `iv2`, `iv3`) with 3, 2, and 4 values respectively. The output values (`out[iv1][iv2][iv3]`) should be ordered as:


`iv1` | `iv2` | `iv3` | `output`
--- | --- | --- | ---
`iv1[1]` | `iv2[1]` | `iv3[1]` | `out[1][1][1]`
`iv1[1]` | `iv2[1]` | `iv3[2]` | `out[1][1][2]`
`iv1[1]` | `iv2[1]` | `iv3[3]` | `out[1][1][3]`
`iv1[1]` | `iv2[1]` | `iv3[4]` | `out[1][1][4]`
`iv1[1]` | `iv2[2]` | `iv3[1]` | `out[1][2][1]`
`iv1[1]` | `iv2[2]` | `iv3[2]` | `out[1][2][2]`
`iv1[1]` | `iv2[2]` | `iv3[3]` | `out[1][2][3]`
`iv1[1]` | `iv2[2]` | `iv3[4]` | `out[1][2][4]`
`iv1[2]` | `iv2[1]` | `iv3[1]` | `out[2][1][1]`
`iv1[2]` | `iv2[1]` | `iv3[2]` | `out[2][1][2]`
`iv1[2]` | `iv2[1]` | `iv3[3]` | `out[2][1][3]`
`iv1[2]` | `iv2[1]` | `iv3[4]` | `out[2][1][4]`
`iv1[2]` | `iv2[2]` | `iv3[1]` | `out[2][2][1]`
`iv1[2]` | `iv2[2]` | `iv3[2]` | `out[2][2][2]`
`iv1[2]` | `iv2[2]` | `iv3[3]` | `out[2][2][3]`
`iv1[2]` | `iv2[2]` | `iv3[4]` | `out[2][2][4]`
`iv1[3]` | `iv2[1]` | `iv3[1]` | `out[3][1][1]`
`iv1[3]` | `iv2[1]` | `iv3[2]` | `out[3][1][2]`
`iv1[3]` | `iv2[1]` | `iv3[3]` | `out[3][1][3]`
`iv1[3]` | `iv2[1]` | `iv3[4]` | `out[3][1][4]`
`iv1[3]` | `iv2[2]` | `iv3[1]` | `out[3][2][1]`
`iv1[3]` | `iv2[2]` | `iv3[2]` | `out[3][2][2]`
`iv1[3]` | `iv2[2]` | `iv3[3]` | `out[3][2][3]`
`iv1[3]` | `iv2[2]` | `iv3[4]` | `out[3][2][4]`

Independent variable values must appear in **ascending** order (an error will be issued if this is not the case).

##### Field: External File Column Number

The column number (starting at 1) in the CSV file corresponding to this independent variable. As the values of the independent variables each repeat over a defined cycle, EnergyPlus will only read unique values from this column. EnergyPlus does not validate that the cycles are repeating correctly. In fact, the same data can be read by only defining each value once as it is first encountered:

`iv1` | `iv2` | `iv3` | `output`
--- | --- | --- | ---
`iv1[1]` | `iv2[1]` | `iv3[1]` | `out[1][1][1]`
| |  | `iv3[2]` | `out[1][1][2]`
| |  | `iv3[3]` | `out[1][1][3]`
| |  | `iv3[4]` | `out[1][1][4]`
| | `iv2[2]` | | `out[1][2][1]`
| | | | `out[1][2][2]`
| | | | `out[1][2][3]`
| | | | `out[1][2][4]`
`iv1[2]` | | | `out[2][1][1]`
| | | | `out[2][1][2]`
| | | | `out[2][1][3]`
| | | | `out[2][1][4]`
| | | | `out[2][2][1]`
| | | | `out[2][2][2]`
| | | | `out[2][2][3]`
| | | | `out[2][2][4]`
`iv1[3]` | | | `out[3][1][1]`
| | | | `out[3][1][2]`
| | | | `out[3][1][3]`
| | | | `out[3][1][4]`
| | | | `out[3][2][1]`
| | | | `out[3][2][2]`
| | | | `out[3][2][3]`
| | | | `out[3][2][4]`

##### Field: External File First Row Number

The row number (starting at 1) in the CSV file where the data for this independent variable begins. Any values in the same column below this row are considered part of the range.

##### Field: Value \<x\>

This field is repeated to capture the full set of values for this independent variable. These values must be defined in **ascending** order (an error will be issued if this is not the case).

#### Example IDF

```
Table:IndependentVariable,
  !- Name
  !- Minimum Value
  !- Maximum Value
  !- Unit Type
  !- Normalization Value
  !- Value 1
  !- ...
  !- Value N
```

## Engineering Reference: Performance Tables

### Lookup Tables

Lookup tables provide a method to evaluate a group of data that may or may not conform to a fundamental equation. Only tabular data which conform to existing performance curve equations are currently allowed (i.e., that lookup table name is entered where any valid performance curve is allowed therefore the table data must conform to the model's fundamental equation for that specific curve). Efforts to include this lookup table in the energy management system would allow custom use of lookup tables, however, calling lookup tables from the EMS system is not currently possible. Lookup tables can interpolate the actual data using a linear interpolation or piecewise cubic spline. As with other table or curve objects, the lookup table can be used anywhere a valid curve object name is allowed. Care must be taken to ensure the table data format is consistent with the associate model that is using the performance curve (e.g., DX cooling coil capacity as a function of temperature where independent variable X1 = indoor wet-bulb temperature and independent variable X2 = outdoor dry-bulb temperature).

An interpolative lookup can be specified to be either linear or cubic independently for each dimension (input variable). For performance points outside the defined grid space, an extrapolation method--constant or linear--can be set independently for each dimension. Finally, each axis can have specified boundaries beyond which extrapolation is not permitted.

#### Linear Interpolation

For linear interpolation in 1 dimension, given known values of $f$ at $x_0$ and $x_1$, and a point $x$ between $x_0$ and $x_1$, the value at $x$ is estimated by:

$$f\left(x\right) = \left(1-\mu\right) \cdot f\left(x_0\right) + \mu \cdot f\left(x_1\right)$$

where $\mu$ is the location of $x$ expressed as a fraction of the distance between $x_0$ and $x_1$:

$$\mu = \left(x-x_0\right) / \left(x_1 - x_0\right)$$


#### Cubic Spline Interpolation

The general formula for a 1-dimensional piecewise cubic spline (for $x$ between known values $f\left(x_0\right)$ and $f\left(x_1\right)$) is:

$$\begin{array}{rll}
f\left(x\right) &= \left(2\mu^3 - 3\mu^2 + 1\right) \cdot f\left(x_0\right)
&+ \left(-2\mu^3 + 3\mu^2\right) \cdot f\left(x_1\right) \\
&+ \left(\mu^3 - 2\mu^2 + \mu\right) \cdot f^\prime \left(x_0\right)
&+ \left(\mu^3 - \mu^2\right) \cdot f^\prime \left(x_1\right)
\end{array}$$

The Catmull-Rom cubic spline interpolation defines the derivatives $f^\prime \left(x_0\right)$ and $f^\prime\left(x_1\right)$ as the slope between the previous and following grid points on the axis:

$$\begin{array}{rl}
f^\prime(x_0) &= ( f(x_1) - f(x_{-1}) ) / ( x_1 - x_{-1}) \\
f^\prime(x_1) &= ( f(x_2) - f(x_0) ) / ( x_2 - x_0)
\end{array}$$

When the hypercube is at the edge of the grid, Catmull-Rom simply extends the slope of the final segment for defining the slope terms: i.e., if there is no $x_{-1}$, we substitute $x_0$ into the $f^\prime(x_0)$ formula.

If a mix of interpolation methods are specified among the included dimensions, the library will perform that mix as requested.

If the lookup point is beyond the grid edge on any axis, the library will perform the requested extrapolation method (linear or constant) on that dimension while proceeding with interpolation along any in-bounds dimension. If a "do-not-extrapolate-beyond" boundary is specified, the library will perform a constant extrapolation from that boundary outward (in that dimension), and it will return the resulting numerical answer along with a warning that the request was outside the boundaries.


## Source Code Design

The main component of the source code design is the addition of a new tabular data interpolation library (currently called "Btwxt" [betwixt]). The purpose of this library is to isolate the interpolation computation and data structures from the rest of EnergyPlus in a fashion that is easily unit tested and can be easily ported to other C++ programs.

The other component is the integration of Btwxt in EnergyPlus. This will involve a modest refactoring of how EnergyPlus handles table data structures.

### Btwxt

Btwxt is a general-purpose N-dimensional interpolation library for gridded data. After reviewing existing open-source libraries, we believe it is sensible to write a new library.

The library will accept:
*   a collection of N vectors representing the input variables,
*   attributes of each of the input vectors describing preferred interpolation and extrapolation methods, and
*   an array of (or collection of arrays of) values that map onto the grid defined by the input vectors.

Together, the input vectors and array(s) of output values will define a performance space. The library will allow querying of a defined performance space for a well-defined point within the grid.

*   A point fully inside the grid boundaries will receive an interpolated value.
    *   The library will support (a) linear and (b) cubic interpolation.
    *   The library structure will allow for alternative interpolation methods to be added later.
*   A point outside the grid boundaries will receive an extrapolated value.
    *   The library will support (a) linear and (b) constant extrapolation
    *   The library will allow each input dimension to define its extrapolation method and an extrapolation limit.
    *   Each input dimension will have the option of specifying outer boundaries beyond which the library will not interpolate.
    *   Each input dimension will have the option of disallowing any extrapolation.

#### Glossary

*   *GridAxis* - The collection of values that define points along a single grid dimension (often thought of as an independent variable)
*   *GridSpace* - The collection of multiple GridAxis objects that fully define the performance space.
*   *ValueTables* - A collection of result/output values that all map to the same GridSpace.
*   *GriddedData* - A unification of a GridSpace with its associated ValueTables.
*   *GridPoint* - A coordinate set (one value along each axis) that defines a single point within a GridSpace.
*   *ValuePoint* - the set of result/output values at a single coordinate location. For example, the output values for a GridPoint.
*   *Floor* - along one GridAxis, the index of the grid-defined point less-than-or-equal-to a specified GridPoint. For example, for `GridAxis = {5, 10, 15, 20}`, `Floor(12, GridAxis) = 1`. (1 being the index of the value "10" in the array.)
*   *Weight* - the fraction defined by how far between the floor and the next point on a GridAxis a GridPoint falls. Referenced from the floor. For example, for `GridAxis = {5, 10, 15, 20}`, `Weight(12, GridAxis) = 0.4`.


#### Approach

The GriddedData constructor is being designed to work smoothly with the PerformanceData FlatBuffer file format being defined for ASHRAE Section 205. It will also be easy to instantiate a GriddedData object from a well-defined set of input vectors and value arrays.

##### Summaries of important classes:
**GridAxis**

```c++
class GridAxis{
  GridAxis(std::vector<double> grid_vector);

  // attributes
  std::vector<double> grid;
  int interpolation_method;
  int extrapolation_method;
  std::pair<double, double> extrapolation_limits;
  // bool is_regular;     (to add later)

  // methods
  std::size_t get_length();
  void set_interp_method();
  void set_extrap_method();
  void set_extrap_limits();
}
```
The `GridAxis` constructor ensures that the `grid_vector` is sorted strictly ascending.

Each axis (input vector) is able to specify its own preferred:
*   interpolation method
*   extrapolation method
*   extrapolation limit, outside of which Btwxt will return a warning.

The idea behind a future `is_regular` attribute is that there are steps in the interpolation math that become much quicker if a grid axis is regularly spaced.

**GridSpace**
```c++
class GridSpace{
public:
  GridSpace(std::vector<GridAxis> grid_axes);

  std::vector<GridAxis> axes;
};
```
The `GridSpace` is a container for the set of N GridAxis objects that delineate the performance space.


**GriddedData**
```c++
class GriddedData{
  GriddedData(
    std::vector< std::vector<double> > grid,
    std::vector< std::vector<double> > values
  );

  // attributes
  GridSpace grid_axes;
  Eigen::ArrayXXd value_tables;
  std::size_t num_values;
  std::vector<std::size_t> dimension_lengths;

  // methods
  void check_inputs();
  std::vector<double> get_grid_vector(const std::size_t& grid_index);
  std::vector<double> get_values(std::vector<std::size_t> coords);
  Eigen::ArrayXd get_column(const std::vector<std::size_t>& coords);
}
```
`GriddedData` holds both a `GridSpace` object and an array of the provided gridded values. The constructor ensures the size of the value array matches with the dimensions of the input grid.

The `value_tables` array comprises a single long row for each output. That is, if this `GriddedData` object has N output variables, the `value_tables` array will be NxM where M is the number of points defined by the grid.

The `GriddedData.get_column()` method returns an Mx1 column array of the output values for a provided grid index (i.e., by index, not by value). `GriddedData.get_values()` returns the same output values in a std::vector rather than an Eigen Array.


**GridPoint**
```c++
class GridPoint{
  GridPoint(std::vector<double> &target_vector);

  // attributes
  std::vector<double> target;
}
```

A `GridPoint` describes the coordinates (by value on each GridAxis) of a performance point we are about to interpolate about.



**WhereInTheGridIsThisPoint**
```c++
class WhereInTheGridIsThisPoint{
public:
  WhereInTheGridIsThisPoint(GridPoint&, GriddedData&);

  // attributes
  std::vector<std::size_t> point_floor;
  std::vector<double> weights;
  std::vector<bool> is_inbounds;

  // methods
  std::vector<std::size_t> get_floor();
  std::vector<double> get_weights();
  std::vector<bool> get_is_inbounds();
  void find_floor(GridPoint&, GriddedData&);
  void calculate_weights(GridPoint&, GriddedData&);
};
```
This elegantly named class is the locator that describes how a specific `GridPoint` fits into the `GriddedData` performance space.
*   `point_floor` is the defined grid point less than or equal to `GridPoint` in each dimension.
*   `weights` define the where GridPoint falls between two GridAxis elements, for each dimension.
*   `is_inbounds` is used to decide interpolation or extrapolation for each GridAxis.


**RegularGridInterpolator**
```c++
class RegularGridInterpolator{
public:
  RegularGridInterpolator(GriddedData &the_blob);
  RegularGridInterpolator(
    std::vector< std::vector<double> > grid,
    std::vector< std::vector<double> > values
  );

  double calculate_value_at_target(std::vector<double> target, std::size_t table_index);
  double operator()(std::vector<double> target, std::size_t table_index)
  {
    return calculate_value_at_target(target, table_index);
  }

  double calculate_value_at_target(std::size_t table_index);
  double operator()(std::size_t table_index)
  {
    return calculate_value_at_target(table_index);
  }

  std::vector<double> calculate_all_values_at_target(std::vector<double> target);
  std::vector<double> operator()(std::vector<double> target)
  {
    return calculate_all_values_at_target(target);
  }

  std::vector<double> calculate_all_values_at_target();
  std::vector<double> operator()()
  {
    return calculate_all_values_at_target();
  }

  void set_new_grid_point(std::vector<double> target);
  std::vector<double> get_current_grid_point();
  void clear_current_grid_point();
  std::size_t get_ndims();

private:
  GriddedData the_blob;
  GridPoint current_grid_point;

  void check_target_dimensions(std::vector<double> target);
  std::vector<double> the_calculator();

```


Note that the "()" operator is overloaded, allowing a function-like shortcut to interpolation. `my_interpolator.calculate_all_values_at_target(target)` can be shortened to `my_interpolator(target)`.

The library will store a `current_grid_point`, meaning that `my_interpolator()` implies interpolating to that saved `GridPoint`. The floors and weights are saved as attributes of `current_grid_point`, so reusing a point will save those search and algebraic steps.

Accordingly, the `RegularGridInterpolator` API has methods to set, get, and clear `current_grid_point`. If the developer attempts to interpolate to `current_grid_point` before one has been specified, the library will return a warning.

the private `the_calculator` method is wrapped by the various public `calculate...` methods and houses the interpolation algorithm itself. The algorithm will deal with the possible interpolation and extrapolation methods on the fly, for each axis.


#### Algorithmic Overview

The interpolation algorithm is designed around the N-dimensional grid cell containing the target point. Btwxt refers to that grid cell as the hypercube and bases the interpolation math around the hypercube's values.

**Linear Interpolation:**

Linear interpolation in 1 dimension: Given known values of f at x<sub>0</sub> and x<sub>1</sub>, and a point x between x<sub>0</sub> and x<sub>1</sub>, the value at x is estimated by f(x) = (1-&mu;) &times; f(x<sub>0</sub>) + &mu; &times; f(x<sub>1</sub>) where &mu; is the location of x expressed as a fraction of the distance between x<sub>0 </sub> and x<sub>1</sub>: &mu; = (x-x<sub>0</sub>)/(x<sub>1</sub>-x<sub>0</sub>).

Generalizing to N-dimensions: For each dimension d of the hypercube containing a target GridPoint, let &mu;<sub>d</sub> be the fraction of the distance along that hypercube edge where the GridPoint lies. The vertex on low side on that edge gets a weight of (1-&mu;<sub>d</sub>) and the high side gets a &mu;<sub>d</sub> multiplier.

So for a 2-D linear interpolation, for a hypercube of (x<sub>0</sub>,y<sub>0</sub>), (x<sub>0</sub>,y<sub>1</sub>), (x<sub>1</sub>,y<sub>0</sub>), (x<sub>1</sub>,y<sub>1</sub>), and a GridPoint of (x<sub>0</sub>+&mu;<sub>x</sub>, y<sub>0</sub>+&mu;<sub>y</sub>):

f(x<sub>0</sub>+&mu;<sub>x</sub>, y<sub>0</sub>+&mu;<sub>y</sub>) = f(x<sub>0</sub>, y<sub>0</sub>) (1-&mu;<sub>x</sub>) (1-&mu;<sub>y</sub>) <br>
&nbsp;&nbsp;&plus; f(x<sub>0</sub>, y<sub>1</sub>) (1-&mu;<sub>x</sub>) &mu;<sub>y</sub> <br>
&nbsp;&nbsp;&plus; f(x<sub>1</sub>, y<sub>0</sub>) &mu;<sub>x</sub> (1-&mu;<sub>y</sub>) <br>
&nbsp;&nbsp;&plus; f(x<sub>1</sub>, y<sub>1</sub>) &mu;<sub>x</sub> &mu;<sub>y</sub>

This extends to arbitrary dimensions. For N dimensions, a linear interpolation will have 2<sup>N</sup> terms, each with N multiplicative factors.


**Cubic Interpolation:**

The general formula for a 1-dimensional piecewise cubic spline (for x between known values x<sub>0</sub> and x<sub>1</sub>) is:

f(x) = (2&mu;3 - 3&mu;2 + 1) f(x<sub>0</sub>) + (-2&mu;3 + 3&mu;2) f(x<sub>1</sub>) <br>
&nbsp;&nbsp;&plus; (&mu;3 - 2&mu;2 + &mu;) f’(x<sub>0</sub>)
&plus; (&mu;3 - &mu;2) f’(x<sub>1</sub>)

We need to define what we will use as the derivative at x<sub>0</sub> and x<sub>1</sub>. Btwxt implements a Catmull-Rom cubic spline, which defines the slope at x as the slope between the point before and the point after x:

f’(x<sub>0</sub>) = ( f(x<sub>1</sub>) - f(x<sub>-1</sub>) ) / ( x<sub>1</sub> - x<sub>-1</sub>) <br>
f’(x<sub>1</sub>) = ( f(x<sub>2</sub>) - f(x<sub>0</sub>) ) / ( x<sub>2</sub> - x<sub>0</sub>)

When the hypercube is at the edge of the grid, Catmull-Rom simply extends the slope of the final segment.

As with the linear interpolation, the cubic can be rewritten as a sum of weights on each vertex. Because of the slope calculations, there are four points involved on each cubic axis. For one dimension, reorganized, the formula looks like this:

f(x) = f(x<sub>0</sub>) (2&mu;3 - 3&mu;2 + 1)  <br>
&nbsp;&nbsp;&plus; f(x<sub>1</sub>) (-2&mu;3 + 3&mu;2) <br>
&nbsp;&nbsp;&plus;  f(x<sub>1</sub>) (&mu;3 - 2&mu;2 + &mu;) / ( x<sub>1</sub> - x<sub>-1</sub>)  <br>
&nbsp;&nbsp;&minus; f(x<sub>-1</sub>) (&mu;3 - 2&mu;2 + &mu;) / ( x<sub>1</sub> - x<sub>-1</sub>) <br>
&nbsp;&nbsp;&plus; f(x<sub>2</sub>) (&mu;3 - &mu;2) / ( x<sub>2</sub> - x<sub>0</sub>) <br>
&nbsp;&nbsp;&minus; f(x<sub>0</sub>) (&mu;3 - &mu;2) / ( x<sub>2</sub> - x<sub>0</sub>)


The cubic formula extends to multiple dimensions. More points are involved (4<sup>N</sup> for N dimensions, rather than 2<sup>N</sup> for linear), but the formula remains a sum of products of independent weights on each point in the hypercube. The algorithm can also mix and match interpolation methods from one axis to another. The cubic interpolation, being a more complex calculation, takes substantially longer than linear, especially as the number of cubic axes increases.

#### Performance Benchmarking

The Btwxt library will include a CPU/memory performance benchmark mechanism.
Six-variable function, ten grid steps on each axis.
Select 1000 GridPoints in the defined region. Interpolate and time.

### EnergyPlus Refactor

Of the structures and methods/functions existing in EnergyPlus, only two will be used for the refactor:

1. `PerfCurve`: The global array (of `PerfomanceCurveData`) of "curve" data.

    - The `PerfomanceCurveData` will be modified to point to new RectangularGridInterpolator class object.

2. `CurveValue`: The global method to evaluate "curves".

    - This will be modified to call methods on the RectangularGridInterpolator in order to return the necessary value.

Many of the other structures and methods/functions will be kept only for legacy use of old table objects (until the completion of the deprecation policy):

  - `TableData`: The global array (of `TableDataStruct`) of table data for 1 and 2 dimensional table objects.
  - `PerfCurveTableData`: A slightly modified version of the former.
  - `TableLookup`: The global array (of `TableDataStruct`) of table data for the multi-dimensional table object.
  - `ReadTableData`: Method to read old IDF/CSV format.
  - `InterpolateLagrange` and `DLAG`: Old Lagrangian interpolation routines.
  - `PerformanceTableObject`: Method called by `CurveValue` to evaluate 1 and 2 dimensional table objects.
  - `TableLookupObject`: Method called by `CurveValue` to evaluate multi-dimensional table objects.
  - `SolveRegression`: Current curve fitting method. Only works for 1 and 2 dimensional table objects and a subset of curve types.

## IDD


```
Table:Lookup,
       \memo Lookup tables are used in place of curves and can represent any number
       \memo of independent variables (defined as Table:IndependentVariable objects
       \memo in a Table:IndependentVariableList). Output values are interpolated
       \memo within the bounds defined by each independent variable and extrapolated
       \memo beyond the bounds according to the interpolation/extrapolation methods
       \memo defined by each independent variable.
       \extensible:1
  A1,  \field Name
       \required-field
       \reference AllCurves
       \reference UniVariateTables
       \reference BiVariateTables
       \reference MultiVariateTables
       \type alpha
  A2,  \field Independent Variable List Name
       \required-field
       \type object-list
       \object-list IndependentVariableListName
  A3,  \field Normalize Output
       \type choice
       \key Yes
       \key No
       \default No
  N1,  \field Minimum Output
       \type real
       \unitsBasedOnField A4
  N2,  \field Maximum Output
       \type real
       \unitsBasedOnField A4
  A4,  \field Output Unit Type
       \type choice
       \key Dimensionless
       \key Capacity
       \key Power
       \default Dimensionless
  A5,  \field External File Name
       \type alpha
       \retaincase
  N3,  \field External File Column Number
       \type integer
       \minimum 1
  N4,  \field External File Row Number
       \type integer
       \minimum 1
  N5;  \field Output Value 1
       \type real
       \unitsBasedOnField A4
       \begin-extensible

Table:IndependentVariableList,
       \memo A sorted list of independent variables used by one or more Table:Lookup
       \memo objects.
       \extensible:1
  A1,  \field Name
       \required-field
       \reference IndependentVariableListName
       \type alpha
  A2;  \field Independent Variable 1 Name
       \required-field
       \type object-list
       \object-list IndependentVariableName
       \begin-extensible

Table:IndependentVariable,
       \memo An independent variable representing a single dimension of a Table:Lookup
       \memo object.
       \extensible:1
  A1,  \field Name
       \required-field
       \reference IndependentVariableName
       \type alpha
  A2,  \field Interpolation Method
       \type choice
       \key Linear
       \key Cubic
       \default Linear
  A3,  \field Extrapolation Method
       \type choice
       \key Constant
       \key Linear
       \key Unavailable
       \default Constant
  N1,  \field Minimum Value
       \type real
       \unitsBasedOnField A4
  N2,  \field Maximum Value
       \type real
       \unitsBasedOnField A4
  N3,  \field Normalization Value
       \type real
       \unitsBasedOnField A4
  A4,  \field Unit Type
       \type choice
       \key Dimensionless
       \key Temperature
       \key VolumetricFlow
       \key MassFlow
       \key Power
       \key Distance
       \key Angle
       \default Dimensionless
  A5,  \field External File Name
       \type alpha
       \retaincase
  N4,  \field External File Column Number
       \type integer
       \minimum 1
  N5,  \field External File Row Number
       \type integer
       \minimum 1
  N6;  \field Value 1
       \type real
       \begin-extensible
```
