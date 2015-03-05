# Performance Curves

Performance curves define a polynomial equation which represents a conditional response to a model input. A polynomial is a mathematical expression which uses a sum of powers in one or more variables. The equation below represents an n^th^ order univariate polynomial. The highest power in the equation represents its order or degree (n).

![](media/image7342.png)\


A polynomial in two independent variables (multi-variate) is given by:

![](media/image7343.png)\


In Energyplus, the result of an equation, the dependent variable, represents the response a system or component has given an input (the independent variable) to the system or component model. This response represents the equipment performance related to the mechanism that causes this change (e.g., the change in capacity or power based on a conditional change in temperature, part-load ratio, or other phenomenon). A variety of performance curves are available to describe the most common forms of engineering equations. The coefficients (a-z) in the following equations are entered in the associated curve object to define a specific phenomenon. Minimum and maximum limits may be applied to both the independent and the dependent variables as necessary.

## Curves based on a single independent variable

A polynomial having only a single independent variable represents a one-dimensional relationship between a condition (the model input) and response (the model output). The previously described n^th^ order univariate polynomial is representative of the following performance curves.

### Linear Curves

A performance curve having an order or degree of 1.

![](media/image7344.png)\


### Quadratic Curves

A performance curve having an order or degree of 2.

![](media/image7345.png)\


### Cubic Curves

A performance curve having an order or degree of 3.

![](media/image7346.png)\


### Quartic Curves

A performance curve having an order or degree of 4.

![](media/image7347.png)\


### Exponent Curves

A performance curve having an order or degree of c.

![](media/image7348.png)\


## Curves based on two independent variables

A polynomial having two independent variables represents a two-dimensional relationship between a condition (the model inputs) and response (the model output). The previously described n^th^ order multi-variate polynomial is representative of the following performance curves.

### QuadraticLinear Curves

A performance curve having an order or degree of 2 in two independent variables.

![](media/image7349.png)\


### BiQuadratic Curves

A performance curve having an order or degree of 2 in two independent variables.

![](media/image7350.png)\


### BiCubic Curves

A performance curve having an order or degree of 3 in two independent variables.

![](media/image7351.png)\


Calulating performance curve coefficients in a spreadsheet is a simple matter of finding the data required to perform the regression analysis. For example, the biquadratic equation shown above is representative of the cooling capacity as a function of temperature performance curve for DX cooling coils. The fundamental equation for DX cooling coil capacity is:

![](media/image7352.png)\


where

Twb,i = wet-bulb temperature of the air entering the cooling coil, °C

Tc,i (or Tdb,i)= dry-bulb temperature of the air entering an air-cooled condenser, °C

Given the data set shown in the table below, each of the independent variables would be calculated according to the fundamental equation above (i.e., the T, T^2^, and cross-product terms would be multiplied out). The data would be converted to degrees celcius and the cooling capacity would be converted to Watts. The data would also be normalized using the ARI rating point shown as highlighted in the table.

![](media/image7353.png)\


The fundamental equation would be used to determine the number of independent variables and also the form of the equation. Given the example described here, the spreadsheet would be set up to look like the equation as shown in the following table. A regression analysis could then be performed on the data set. The first five columns are the independent variables and the last column is the dependent variable. A spreadsheet tool is selected to perform the regression analysis, and the coefficients are calculated and displayed in the spreadsheet.

![](media/image7354.png)\


The regression analysis and summary statistical output is shown below. The equation coefficients are shown highlighted. In this example, the equation coefficents are: a = 0.757382, b = 0.014666, c= 0.000459, d = -0.00095, e = -6.7E-05, and f = -0.00015. These coefficients would be entered in a Curve:BiQuadratic object and used to describe the cooling capacity as a function of temperature for the DX cooling coil model. Minimum and Maximum values from the tabular data are entered as Min/Max values for the curve object. The values may be relaxed slightly with care to allow extrapolation as needed. A performance table may be used to automatically perform the regression analysis as described in the following section.

![](media/image7355.png)\


~~~~~~~~~~~~~~~~~~~~

      Curve:Biquadratic,
        CoolCapFTExample,        !- Name
        0.757382,                !- Coefficient1 Constant
        0.014666,                !- Coefficient2 x
        0.000459,                !- Coefficient3 x**2
        -0.00095,                !- Coefficient4 y
        -0.000067,               !- Coefficient5 y**2
        -0.00015,                !- Coefficient6 x*y
        17.22222,                !- Minimum Value of x
        21.66667,                !- Maximum Value of x
        18.33333,                !- Minimum Value of y
        46.11111,                !- Maximum Value of y
        ,                        !- Minimum Curve Output
        ,                        !- Maximum Curve Output
        Temperature,             !- Input Unit Type for X
        Temperature,             !- Input Unit Type for Y
        Dimensionless;           !- Output Unit Type
~~~~~~~~~~~~~~~~~~~~

## Curves based on three independent variables

A polynomial having three independent variables represents a three-dimensional relationship between a condition (the model inputs) and response (the model output).

### TriQuadratic Curves

A performance curve having an order or degree of 2 in three independent variables.

![](media/image7356.png)\


## Pressure drop curve

### Functional Pressure Drop Curve

A performance curve representing the minor loss and/or friction calculations in plant pressure simulations.

![](media/image7357.png)\
