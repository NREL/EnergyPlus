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

## Input Output Reference Documentation

### Group Performance Tables

This group of objects consists of tabular data which are used to characterize the performance of HVAC equipment. The use of performance tables eliminate the need to perform a regression analysis to calculate the performance curve equation coefficients.

<!--The tabular data is entered as data pairs and represent each independent variable followed by the table output at that condition. When using these two performance table objects, a regression analysis may be performed during the simulation to create the associated performance curve. The regression analysis is performed when the user requests to evaluate the curve to the limits specified (see Interpolation Method input field). The performance curve that is created is used throughout the simulation and is also written to the eio file when the user sets the diagnostics output flag DisplayAdvancedVariables (ref. Output:Diagnostics, DisplayAdvancedVariables) This performance curve object may be directly used in future input files. The following descriptions define the input requirements for the performance table objects. -->

***Questions for Reviewers:***

- Should we remove/deprecate other lookup tables? It's not clear that there will be a one-to-one correspondence with the new structure, but mostly related to some features that we'd want to deprecate anyway.
- I'd recommend removing the ability to use the table to generate regression curves. It seems like this would be less accurate to the data.

### Table:Lookup

Input for tables representing data with one or more independent variables.

#### Inputs

##### Field: Name

A unique user-assigned name for an instance of a lookup table. When a table is used, it is referenced by this name. The name of this table object may be used anywhere a valid performance curve object is allowed.

##### Field: Interpolation Method

Method used to determine the value of the table within the bounds of its independent variables. The choices are:

- *Linear*

***Questions for Reviewers:***

- Do we want/need higher order interpolation than linear? Lagrange interpolation can be dangerous?
- Maybe this should be replaced by an interpolation order (an integer between 1 and N - 1)?
- Do we want different methods depending on independent variable (e.g., quadratic-linear curve types)?

##### Field: Extrapolation Method

Method used to determine the value of the table beyond the bounds of its independent variables. The choices are:

- *Constant*: Value is the same as the interpolated value at the closest point along the table's boundary.
- *Linear*: Value is linearly extrapolated in all dimensions from the interpolated value at the closest point along the table's boundary.
- *Unavailable*: The object using this curve is assumed to be not operable. This will override any corresponding availability schedules of applicable equipment.

***Questions for Reviewers:***

- Do we want different methods depending on independent variable?

##### Field: Curve Type

The type of curve this table is representing. Choices are:

- *Linear*
- *Quadratic*
- *Cubic*
- *Quartic*
- *BiQuadratic*
- *QuadraticLinear*
- *Bicubic*

This field is used to determine if this table object is representative of the type of curve allowed for a particular application (i.e., if this type of polynomial is allowed to be used as the curve type for other objects).

##### Field: Independent Variable List Name

The name of a Table:IndependentVariableList object that defines the independent variables that comprise the dimensions of the tabular data.

##### Field: Normalize Output

Determines whether or not this output will be automatically normalized to 1.0 at the normalized reference conditions defined by each independent variable in the associated independent variable list. Normalization applies to the Output Value fields as well as the Minimum and Maximum Output fields Choices are: *Yes* or *No*.

##### Field: Minimum Output

The minimum allowable value of the evaluated table after interpolation and extrapolation. Values less than the minimum will be replaced by the minimum. If this field is left blank, no limit is imposed.

***Questions for Reviewers:***

- Used to be that default minimum was defined by the data set. Is that better?

##### Field: Maximum Output

The maximum allowable value of the evaluated table after interpolation and extrapolation. Values greater than the maximum will be replaced by the maximum. If this field is left blank, no limit is imposed.

***Questions for Reviewers:***

- Used to be that default maximum was defined by the data set. Is that better?

##### Field: Output Unit Type

This field is used to indicate the kind of units that may be associated with the output values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Output and Maximum Output. If none of these options are appropriate, select *Dimensionless* which will have no unit conversion. Options are:

- *Dimensionless*
- *Capacity*
- *Power*

***Questions for Reviewers:***

- Should we expand this to use any output unit defined in the recent unit refactoring scheme?

##### Field: External File Name

The name of an external CSV file that represents the tabular data. This file should be formatted such that the data for this particular output is ordered according to the order of the corresponding independent variables. For example, for three independent variables (iv1, iv2, iv3) with 3, 2, and 4 values respectively. The output values (out[iv1][iv2][iv3]) should be ordered as:

```
out[1][1][1]
out[1][1][2]
out[1][1][3]
out[1][1][4]
out[1][2][1]
out[1][2][2]
out[1][2][3]
out[1][2][4]
out[2][1][1]
out[2][1][2]
out[2][1][3]
out[2][1][4]
out[2][2][1]
out[2][2][2]
out[2][2][3]
out[2][2][4]
out[3][1][1]
out[3][1][2]
out[3][1][3]
out[3][1][4]
out[3][2][1]
out[3][2][2]
out[3][2][3]
out[3][2][4]
```

##### Field: External File Column Number

The column number in the CSV file corresponding to this output.

##### Field: External File First Row Number

The row number in the CSV file where the data for this output begins. If there are not enough rows of data to fill out the full grid of data an error will be presented to the user.

##### Output Value \<x\>

This field is repeated to capture the full set of output data in the table (if not otherwise defined in an external file). The data for this particular output is ordered according to the order of the corresponding independent variables. For example, for three independent variables (iv1, iv2, iv3) with 3, 2, and 4 values respectively. The output values (out[iv1][iv2][iv3]) should be ordered as:

```
out[1][1][1]
out[1][1][2]
out[1][1][3]
out[1][1][4]
out[1][2][1]
out[1][2][2]
out[1][2][3]
out[1][2][4]
out[2][1][1]
out[2][1][2]
out[2][1][3]
out[2][1][4]
out[2][2][1]
out[2][2][2]
out[2][2][3]
out[2][2][4]
out[3][1][1]
out[3][1][2]
out[3][1][3]
out[3][1][4]
out[3][2][1]
out[3][2][2]
out[3][2][3]
out[3][2][4]
```

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

A list of Table:IndependentVariable references that define the size and dimensions of the data for one or more Table:Lookup objects. The order of this list defines the order that the tabular data must be defined. The output values in the Table:Lookup associated with this list will list the output in an order cycling through the last item in the list first, and then the second to last, and so on with the the first item cycling last. For example, for three independent variables (iv1, iv2, iv3) with 3, 2, and 4 values respectively. The output values (out[iv1][iv2][iv3]) should be ordered as:

```
out[1][1][1]
out[1][1][2]
out[1][1][3]
out[1][1][4]
out[1][2][1]
out[1][2][2]
out[1][2][3]
out[1][2][4]
out[2][1][1]
out[2][1][2]
out[2][1][3]
out[2][1][4]
out[2][2][1]
out[2][2][2]
out[2][2][3]
out[2][2][4]
out[3][1][1]
out[3][1][2]
out[3][1][3]
out[3][1][4]
out[3][2][1]
out[3][2][2]
out[3][2][3]
out[3][2][4]
```

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

##### Field: Minimum Value

***Questions for Reviewers:***

- Can this field be removed? It is only here because it was used in previous tables. Can't the minimum be inferred by the smallest value in the list of values?

##### Field: Maximum Value

***Questions for Reviewers:***

- Can this field be removed? It is only here because it was used in previous tables. Can't the maximum be inferred by the largest value in the list of values?

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

To be rewritten to capture new method for design document.


<!-- ## New Input Objects TODO: Fill with design doc -->

### IDD

To finalize after NFP review

<!--
```
Table:Lookup,
       \memo Lookup table can represent any number of independent variables and
       \memo can interpolate these independent variables up to a 4th order
       \memo polynomial. The polynomial order is assumed to be the number of
       \memo interpolation points (n) minus 1. When any independent variable
       \memo value is outside the table limits, extrapolation behavior is
       \memo defined by the user.
       \extensible:???
       \min-fields ???
  A1 , \field Name
       \required-field
       \reference AllCurves
       \reference UniVariateTables
       \reference BiVariateTables
       \reference MultiVariateTables
       \type alpha
  A2 , \field Interpolation Method
       \type choice
       \key Linear
       \key EvaluateCurve
       \key Lagrange
       \default Linear
  N1 , \field Number of Interpolation Points
       \type integer
       \minimum> 1
       \maximum 4
       \default 3
  A3 , \field Curve Type
       \type choice
       \key Linear
       \key Quadratic
       \key Cubic
       \key Quartic
       \key Exponent
       \key BiQuadratic
       \key QuadraticLinear
       \key BiCubic
       \key TriQuadratic
       \key Other
       \note The curve types BiCubic and TriQuadratic may not
       \note be used with Interpolation Method = EvaluateCurveToLimits
```
-->
