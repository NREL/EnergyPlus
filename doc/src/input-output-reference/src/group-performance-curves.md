# Group – Performance Curves

This group of objects primarily consists of polynomial curves that are used to characterize the performance of HVAC equipment. Several other non-polynomial curves are also included to characterize the performance of pumps and fans. All of the curves are input, stored, and evaluated entirely within the CurveManager module. The curves are usually derived from fits or regressions to data covering a limited range. Results for independent variable values outside this range are likely to be invalid, so curve input always contains a range of validity (maximum and minimum permitted values) for each independent variable and can optionally have limits on the curve ouput. No error or warning message is issued if an independent variable is outside the range. Instead, the curve manager uses the minimum value if an independent variable is less than the minimum, and the maximum if a variable exceeds the maximum. Similarly, no error or warning message is issued if the curve output is outside the range of the optional minimum and maximum curve output limits. Instead, the curve manager uses the minimum and maximum curve limits to cap the output of the performance curve.

Curve names must be unique across all curve types.

## Curve:Linear

Input for the linear curve consists of a curve name, the two coefficients, and the maximum and minimum valid independent variable values. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation represented by the linear curve is:

![](media/image565.png)\


### Inputs

#### Field: Name

A user assigned unique name for an instance of a linear curve. When a curve is used, it is referenced by this name.

#### Field: Coefficient1 Constant

The constant coefficient (C~1~) in the equation.

#### Field: Coefficient2 x

The linear coefficient (C~2~) in the equation.

#### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of x. Values of x greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of x. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

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

Following is an example input:

~~~~~~~~~~~~~~~~~~~~

    Curve:Linear,
           Curve-Linear, ! name
           -1,       ! Coefficient1 Constant
           2,       ! Coefficient2 x
           0.0,          ! min curve output
           1.0;          ! max curve output
~~~~~~~~~~~~~~~~~~~~

The following is another example, as might be applied in the [Fan:ComponentModel](#fancomponentmodel) to characterize duct static pressure reset (using a constant duct static pressure set point of 248.84 Pa in this case):

~~~~~~~~~~~~~~~~~~~~

    Curve:Linear,
        DiagnosticSPR,                   ! Curve Name f = C1 + C2*x
        248.84,                          ! Coefficient1 Constant [Pa]
        0.,                              ! Coefficent 2 Press/Flow [Pa-s/m3]
        0.,                              ! Minimum Value of x (Qfan) [m3/s]
        100.,                            ! Maximum Value of x (Qfan) [m3/s]
        62.5,                            ! Minimum Curve Output [Pa]
        248.84;                          ! Maximum Curve Output [Pa]
~~~~~~~~~~~~~~~~~~~~

## Curve:QuadLinear

Input consists of the curve name, the five coefficients, and maximum and minimum values for each of the independent variables. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation is represented by this curve:

### Inputs

#### **y = C1 + C2 \* w + C3 \* x + C4 \* y + C5 \* z**

#### Field: Name

A user assigned unique name for an instance of this curve. When a curve of this type is used, it is referenced by this name.

#### Field: Coefficient1 Constant

The constant coefficient (C1) in the equation.

#### Field: Coefficient2 w

The coefficient (C2) in the equation.

#### Field: Coefficient3 x

The coefficient (C3) in the equation.

#### Field: Coefficient4 y

The coefficient (C4) in the equation.

#### Field: Coefficient5 z

The coefficient (C5) in the equation.

#### Field: Minimum Value of w

The minimum allowable value of w. Values of w less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of w

The maximum allowable value of w. Values of z greater than the maximum will be replaced by the maximum.

#### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of x. Values of z greater than the maximum will be replaced by the maximum.

#### Field: Minimum Value of y

The minimum allowable value of y. Values of y less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of y

The maximum allowable value of y. Values of z greater than the maximum will be replaced by the maximum.

#### Field: Minimum Value of z

The minimum allowable value of z. Values of z less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of z

The maximum allowable value of z. Values of z greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for w

This field is used to indicate the kind of units that may be associated with the w values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of w and Maximum Value of w. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select Dimensionless which will have no unit conversion.

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance
- VolumetricFlowPerPower 

#### Field: Input Unit Type for x

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of x and Maximum Value of x. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select Dimensionless which will have no unit conversion.

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance
- VolumetricFlowPerPower 

#### Field: Input Unit Type for y

This field is used to indicate the kind of units that may be associated with the y values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of y and Maximum Value of y. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select Dimensionless which will have no unit conversion.

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance 
- VolumetricFlowPerPower 

#### Field: Input Unit Type for z

This field is used to indicate the kind of units that may be associated with the z values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of z and Maximum Value of z. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select Dimensionless which will have no unit conversion.

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance
- VolumetricFlowPerPower 

Below are an example inputs for QuadLinear Curves.

~~~~~~~~~~~~~~~~~~~~

    CURVE:QUADLINEAR,
        MinDsnWBCurveName, ! Curve Name
        -3.3333,          ! CoefficientC1
        0.0,              ! CoefficientC2
        38.9,             ! CoefficientC3
        0.,               ! CoefficientC4
        0.,               ! CoefficientC5
        -30.,             ! Minimum Value of w
        40.,              ! Maximum Value of w
        0.,               ! Minimum Value of x
        1.,               ! Maximum Value of x
        10.,              ! Minimum Value of y
        38.,              ! Maximum Value of y
        1. E-8,           ! Minimum Value of z
        8. E-8,           ! Maximum Value of z
        0.,               ! Minimum Curve Output
        38.;              ! Maximum Curve Output

    Curve:QuadLinear,
        MinActWBCurveName, ! Curve Name
        -8.3333,          ! CoefficientC1
        2.0,              ! CoefficientC2
        5.5556.,          ! CoefficientC3
        -1.0,             ! CoefficientC4
        0.,               ! CoefficientC5
        0.,               ! Minimum Value of w
        38.,              ! Maximum Value of w
        0.,               ! Minimum Value of x
        1.,               ! Maximum Value of x
        10.,              ! Minimum Value of y
        38.,              ! Maximum Value of y
        1. E-8,           ! Minimum Value of z
        8. E-8,           ! Maximum Value of z
        0.,               ! Minimum Curve Output
        43.;              ! Maximum Curve Output

    Curve:QuadLinear,
        OptCondEntCurveName, ! Curve Name
        12.2711,          ! CoefficientC1
        0.80,             ! CoefficientC2
        6.6667,           ! CoefficientC3
        0-0.266,          ! CoefficientC4
        -6193484.,        ! CoefficientC5
        0.,               ! Minimum Value of w
        38.,              ! Maximum Value of w
        0.,               ! Minimum Value of x
        1.,               ! Maximum Value of x
        10.,              ! Minimum Value of y
        38.,              ! Maximum Value of y
        1. E-8,           ! Minimum Value of z
        8. E-8,           ! Maximum Value of z
        0.,               ! Minimum Curve Output
        32.;              ! Maximum Curve Output

~~~~~~~~~~~~~~~~~~~~

## Curve:Quadratic

Input for a quadratic curve consists of the curve name, the three coefficients, and the maximum and minimum valid independent variable values. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation represented by the quadratic curve is:

![](media/image566.png)\


### Inputs

#### Field: Name

A user assigned unique name for an instance of a quadratic curve. When a curve is used, it is referenced by this name.

#### Field: Coefficient1 Constant

The constant coefficient (C~1~) in the equation.

#### **Field: Coefficient2 x**

The linear coefficient (C~2~) in the equation.

#### Field: Coefficient3 x\*\*2

The quadratic coefficient (C~3~) in the equation.

#### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of x. Values of x greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output 

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of x. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

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

Following is an example input.

~~~~~~~~~~~~~~~~~~~~

    Curve:Quadratic,
           WindACCBFFFF, ! name
           -2.277,       ! Coefficient1 Constant
           5.2114,       ! Coefficient2 x
           -1.9344,      ! Coefficient3 x**2
           0.0,          ! Minimum Value of x
           1.0;          ! Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

## Curve:Cubic

Input for a cubic curve consists of the curve name, the 4 coefficients, and the maximum and minimum valid independent variable values. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation represented by the cubic curve is:

![](media/image567.png)\


### Inputs

#### Field: Name

A user assigned unique name for an instance of a cubic curve. When a curve is used, it is referenced by this name.

#### Field: Coefficient1 Constant

The constant coefficient (C~1~) in the equation.

#### Field: Coefficient2 x

The linear coefficient (C~2~) in the equation.

#### Field: Coefficient3 x\*\*2

The quadratic coefficient (C~3~) in the equation.

#### Field: Coefficient4 x\*\*3

The cubic coefficient (C~4~) in the equation.

#### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of x. Values of x greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of x. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
-  VolumetricFlow
- MassFlow
- Distance
- Power

#### Field: Output Unit Type

This field is used to indicate the kind of units that may be associated with the output values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Curve Output and Maximum Curve Output. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Capacity
- Power

Following is an input example.

~~~~~~~~~~~~~~~~~~~~

    Curve:Cubic,
           WindACEIRFPLF, ! name
           .00000273404,  ! Coefficient1 Constant
           1.05259,       ! Coefficient2 x
           -.0552087,     ! Coefficient3 x**2
           .00262236,     ! coeff4
           0.0, ! min
           1.1; ! max
~~~~~~~~~~~~~~~~~~~~

## Curve:Quartic

Input for a Quartic (fourth order polynomial) curve consists of the curve name, the five coefficients, and the maximum and minimum valid independent variable values. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation represented by the quartic curve is:

![](media/image568.png)\


### Inputs

#### Field: Name

A user assigned unique name for an instance of a Quartic curve. When a curve is used, it is referenced by this name.

#### Field: Coefficient1 Constant

The constant coefficient (C~1~) in the equation.

#### Field: Coefficient2 x

The linear coefficient (C~2~) in the equation.

#### Field: Coefficient3 x\*\*2

The quadratic coefficient (C~3~) in the equation.

#### Field: Coefficient4 x\*\*3

The cubic coefficient (C~4~) in the equation.

#### Field: Coefficient5 x\*\*4

The fourth-order coefficient (C~5~) in the equation.

#### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of x. Values of x greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of x. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

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

Following is an input example.

~~~~~~~~~~~~~~~~~~~~

      Curve:Quartic,
        BGSeries60,                 !- y = -611.41x4 + 192.68x3 - 88.843x2 + 4.7634x + 5.5656
        5.5656,                     !- Constant
        4.7634,                     !- 1st coefficient
        -88.843,                    !- 2nd coefficient
        192.68,                     !- 3rd coefficient
        -611.41,                    !- 4th coefficient
        0.0,                        !- Min Phi Value
        0.2412;                     !- Max Phi Value
~~~~~~~~~~~~~~~~~~~~

The following is another example, as might be applied in the [Fan:ComponentModel](#fancomponentmodel) to characterize belt maximum efficiency (using a medium efficiency belt in this case):

~~~~~~~~~~~~~~~~~~~~

      Curve:Quartic,
        BeltMaxEffMedium,                ! Curve Name
        -0.09504,                        ! CoefficientC1
        0.03415,                         ! CoefficientC2
        -0.008897,                       ! CoefficientC3
        0.001159,                        ! CoefficientC4
        -0.00006132,                     ! CoefficientC5
        -1.2,                            ! Minimum Value of x
        6.2,                             ! Maximum Value of x
        -4.6,                            ! Minimum Curve Output
        0.;                              ! Maximum Curve Output
~~~~~~~~~~~~~~~~~~~~

## Curve:Exponent

Input for a exponent curve consists of the curve name, the 3 coefficients, and the maximum and minimum valid independent variable values. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation represented by the exponent curve is:

![](media/image569.png)\


### Inputs

#### Field: Name

A user assigned unique name for an instance of an exponent curve. When a curve is used, it is referenced by this name.

#### Field: Coefficient1 Constant

The constant coefficient (C~1~) in the equation.

#### Field: Coefficient2 Constant

The linear coefficient (C~2~) in the equation.

#### Field: Coefficient3 Constant

The exponent coefficient (C~3~) in the equation.

#### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of x. Values of x greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of x. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

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

Following is an input example.

~~~~~~~~~~~~~~~~~~~~

      Curve:Exponent,
    !   Curve = C1 + C2*x**C3, x = fan speed ratio
        FanPowerExponentCurve, !- Name
        0.0,                   !- Coefficient1 Constant
        1.0,                   !- Coefficient2 Constant
        3.0,                   !- Coefficient3 Constant
        0.0,                   !- Minimum Value of x
        1.5,                   !- Maximum Value of x
        0.1,                   !- Minimum Curve Output
        1.5;                   !- Maximum Curve Output
~~~~~~~~~~~~~~~~~~~~

## Curve:Bicubic

This curve type is a function of two independent variables. Input consists of the curve name, the ten coefficients, and the minimum and maximum values for each of the independent variables. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation represented by the bicubic curve is:

![](media/image570.png)\


### Inputs

#### Field: Name

A user assigned unique name for an instance of a bicubic curve. When a curve is used by another object, it is referenced by this name.

#### Field: Coefficient1 Constant

The constant coefficient (C~1~) in the equation.

#### Field: Coefficient2 x

The coefficient C~2~ in the equation.

#### Field: Coefficient3 x\*\*2

The coefficient C~3~ in the equation.

#### Field: Coefficient4 y

The coefficient C~4~ in the equation.

#### Field: Coefficient5 y\*\*2

The coefficient C~5~ in the equation.

#### Field: Coefficient6 x\*y

The coefficient C~6~ in the equation.

#### Field: Coefficient7 x\*\*3

The coefficient C~7~ in the equation.

#### Field: Coefficient8 y\*\*3

The coefficient C~8~ in the equation.

#### Field: Coefficient9 x\*\*2\*y

The coefficient C~9~ in the equation.

#### Field: Coefficient10 x\*y\*\*2

The coefficient C~10~ in the equation.

#### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than this minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of x. Values of x greater than this maximum will be replaced by the maximum.

#### Field: Minimum Value of y

The minimum allowable value of y. Values of y less than this minimum will be replaced by the minimum.

#### Field: Maximum Value of y

The maximum allowable value of y. Values of y greater than this maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of x. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance

#### Field: Input Unit Type for Y

This field is used to indicate the kind of units that may be associated with the y values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of Y and Maximum Value of Y. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance

#### Field: Output Unit Type

This field is used to indicate the kind of units that may be associated with the output values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Curve Output and Maximum Curve Output. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Capacity
- Power

Below is an example input.

~~~~~~~~~~~~~~~~~~~~

      Curve:Bicubic,
        Main Chiller EIRFPLR,       !- Name
        7.086284E-02,               !- Coefficient1 Constant
        2.787561E-03,               !- Coefficient2 x
       -8.917038E-06,               !- Coefficient3 x**2
        2.309734E-01,               !- Coefficient4 y
        1.250442E+00,               !- Coefficient5 y**2
       -2.161029E-03,               !- Coefficient6 x*y
        0.000000E+00,               !- Coefficient7 x**3
       -5.630094E-01,               !- Coefficient8 y**3
        0.000000E+00,               !- Coefficient9 x**2*y
        0.000000E+00,               !- Coefficient10 x*y**2
        20.33,                      !- Minimum Value of x
        35.00,                      !- Maximum Value of x
        0.25,                       !- Minimum Value of y
        1.01;                       !- Maximum Value of y
~~~~~~~~~~~~~~~~~~~~

## Curve:Biquadratic

This curve is a function of two independent variables. Input consists of the curve name, the six coefficients, and min and max values for each of the independent variables. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation represented by the bicubic curve is:

![](media/image571.png)\


### Inputs

#### Field: Name

A user assigned unique name for an instance of a biquadratic curve. When a curve is used, it is referenced by this name.

#### Field: Coefficient1 Constant

The constant coefficient (C~1~) in the equation.

#### Field: Coefficient2 x

The coefficient C~2~ in the equation.

#### Field: Coefficient3 x\*\*2

The coefficient C~3~ in the equation.

#### Field: Coefficient4 y

The coefficient C~4~ in the equation.

#### Field: Coefficient5 y\*\*2

The coefficient C~5~ in the equation.

#### Field: Coefficient6 x\*y

The coefficient C~6~ in the equation.

#### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of x. Values of x greater than the maximum will be replaced by the maximum.

#### Field: Minimum Value of y

The minimum allowable value of y. Values of y less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of y

The maximum allowable value of y. Values of y greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of x. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance

#### Field: Input Unit Type for Y

This field is used to indicate the kind of units that may be associated with the y values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of Y and Maximum Value of Y. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance

#### Field: Output Unit Type

This field is used to indicate the kind of units that may be associated with the output values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Curve Output and Maximum Curve Output. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Capacity
- Power

Below is an example input.

~~~~~~~~~~~~~~~~~~~~

    Curve:Biquadratic,
           WindACCoolCapFT, ! name
           0.942587793,     ! Coefficient1 Constant
           0.009543347,     ! Coefficient2 x
           0.000683770,     ! Coefficient3 x**2
           -0.011042676,    ! Coefficient4 y
           0.000005249,     ! Coefficient5 y**2
           -0.000009720,    ! Coefficient6 x*y
           15., 22., ! min and max of first independent variable
           29., 47.; ! min and max of second independent variable
~~~~~~~~~~~~~~~~~~~~

## Curve:QuadraticLinear

This curve is a function of two independent variables. Input consists of the curve name, the six coefficients, and min and max values for each of the independent variables. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation represented by the quadratic linear curve:

![](media/image572.png)\


### Inputs

#### Field: Name

A user assigned unique name for an instance of a quadratic-linear curve. When a curve is used, it is referenced by this name.

#### Field: Coefficient1 Constant

The constant coefficient (C~1~) in the equation.

#### Field: Coefficient2 x

The coefficient C~2~ in the equation.

#### Field: Coefficient3 x\*\*2

The coefficient C~3~ in the equation.

#### Field: Coefficient4 y

The coefficient C~4~ in the equation.

#### Field: Coefficient5 x\*y

The coefficient C~5~ in the equation.

#### Field: Coefficient6 x\*\*2\*y

The coefficient C~6~ in the equation.

#### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of x. Values of x greater than the maximum will be replaced by the maximum.

#### Field: Minimum Value of y

The minimum allowable value of y. Values of y less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of y

The maximum allowable value of y. Values of y greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of x. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance

#### Field: Input Unit Type for Y

This field is used to indicate the kind of units that may be associated with the y values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of Y and Maximum Value of Y. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance

#### Field: Output Unit Type

This field is used to indicate the kind of units that may be associated with the output values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Curve Output and Maximum Curve Output. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Capacity
- Power

Below is an example input.

~~~~~~~~~~~~~~~~~~~~

    Curve:QuadraticLinear,
        DischargeCurve,          !- Curve Name
        0.0,                     !- Coefficient1 Constant
        0.09,                    !- Coefficient2 x
        -0.15,                   !- Coefficient3 x**2
        0.612,                   !- Coefficient4 y
        -0.324,                  !- Coefficient5 x*y
        -0.216,                  !- Coefficient6 x**2*y
        0.0,                     !- Minimum Value of x
        1.0,                     !- Maximum Value of x
        0.0,                     !- Minimum Value of y
        9.9;                     !- Maximum Value of y
~~~~~~~~~~~~~~~~~~~~

## Curve:Triquadratic

This curve is a 2^nd^ order polynomial function of three variable polynomial independent variables. Input consists of the curve name, the twenty seven coefficients, and min and max values for each of the independent variables. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation represented by the triquadratic curve:

![](media/image573.png)\


### Inputs

#### Field: Coefficient1 Constant

The constant coefficient (A~0~) in the equation.

#### Field: Coefficient2 x\*\*2

The coefficient (A~1~) in the equation.

#### Field: Coefficient3 x

The coefficient (A~2~) in the equation.

#### Field: Coefficient4 y\*\*2

The coefficient (A~3~) in the equation.

#### Field: Coefficient5 y

The coefficient (A~4~) in the equation.

#### Field: Coefficient6 z\*\*2

The coefficient (A~5~) in the equation.

#### Field: Coefficient7 z

The coefficient (A~6~) in the equation.

#### Field: Coefficient8 x\*\*2\*y\*\*2

The coefficient (A~7~) in the equation.

#### Field: Coefficient9 x\*y

The coefficient (A~8~) in the equation.

#### Field: Coefficient10 x\*y\*\*2

The coefficient (A~9~) in the equation.

#### Field: Coefficient11 x\*\*2\*y

The coefficient (A~10~) in the equation.

#### Field: Coefficient12 x\*\*2\*z\*\*2

The coefficient (A~11~) in the equation.

#### Field: Coefficient13 x\*z

The coefficient (A~12~) in the equation.

#### Field: Coefficient14 x\*z\*\*2

The coefficient (A~13~) in the equation.

#### Field: Coefficient15 x\*\*2\*z

The coefficient (A~14~) in the equation.

#### Field: Coefficient16 y\*\*2\*z\*\*2

The coefficient (A~15~) in the equation.

#### Field: Coefficient17 y\*z

The coefficient (A~16~) in the equation.

#### Field: Coefficient18 y\*z\*\*2

The coefficient (A~17~) in the equation.

#### Field: Coefficient19 y\*\*2\*z

The coefficient (A~18~) in the equation.

#### Field: Coefficient20 x\*\*2\*y\*\*2\*z\*\*2

The coefficient (A~19~) in the equation.

#### Field: Coefficient21 x\*\*2\*y\*\*2\*z

The coefficient (A~20~) in the equation.

#### Field: Coefficient22 x\*\*2\*y\*z\*\*2

The coefficient (A~21~) in the equation.

#### Field: Coefficient23 x\*y\*\*2\*z\*\*2

The coefficient (A~22~) in the equation.

#### Field: Coefficient24 x\*\*2\*y\*z

The coefficient (A~23~) in the equation.

#### Field: Coefficient25 x\*y\*\*2\*z

The coefficient (A~24~) in the equation.

#### Field: Coefficient26 x\*y\*z\*\*2

The coefficient (A~25~) in the equation.

#### Field: Coefficient27 x\*y\*z

The coefficient (A~26~) in the equation.

#### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The maximum allowable value of x. Values of x greater than the maximum will be replaced by the maximum.

#### Field: Minimum Value of y

The minimum allowable value of y. Values of y less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of y

The maximum allowable value of y. Values of y greater than the maximum will be replaced by the maximum.

#### Field: Minimum Value of z

The minimum allowable value of z. Values of z less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of z

The maximum allowable value of z. Values of z greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the x values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of X and Maximum Value of x. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance

#### Field: Input Unit Type for Y

This field is used to indicate the kind of units that may be associated with the y values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of Y and Maximum Value of Y. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance

#### Field: Input Unit Type for Z

This field is used to indicate the kind of units that may be associated with the z values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Value of Z and Maximum Value of Z. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Temperature
- VolumetricFlow
- MassFlow
- Power
- Distance

#### Field: Output Unit Type

This field is used to indicate the kind of units that may be associated with the output values. It is used by IDF Editor to display the appropriate SI and IP units for the Minimum Curve Output and Maximum Curve Output. The unit conversion is not applied to the coefficients. The available options are shown below. If none of these options are appropriate, select **Dimensionless** which will have no unit conversion**.**

- Dimensionless
- Capacity
- Power

## Curve:Functional:PressureDrop

Input for a pressure drop curve consists of the curve name, a varying number of parameters, and the maximum and minimum valid independent variable values. The equation represented by the pressure drop curve is:

![](media/image574.png)\


### Inputs

#### Field: Name

A user assigned unique name for an instance of a pressure drop curve. When a curve is used, it is referenced by this name.

#### Field: Diameter

This diameter represents an equivalent diameter for the given branch. This is parameter (D) in the equation, and has units of {m}. Since varying components may be found on the same branch, this value must be selected along with the other inputs in order to provide a proper value of pressure drop. This is used to calculate the velocity in addition to being used directly in the frictional pressure drop calculation.

#### Field: Minor Loss Coefficient

This is the pressure drop coefficient typically applied to components such as fittings and occasionally heat pumps. This is parameter (K) in the equation and is dimensionless. This coefficient is used to describe the amount of dynamic pressure lost during the process. This value may be left blank if the user only wants to account for frictional losses in this branch.

#### Field: Length

This is the length of a pressure drop process in which friction is applied. This is parameter (L) in the equation and has units of {m}. In a pipe, this would be the length of the pipe, however in many cases, pressure drop in other components are applied as an "equivalent length."  This is only required if the user is wanting to perform frictional pressure drop calculations.

#### Field: Roughness

This field represents the first method to simulate frictional losses. This parameter does not appear directly in the equation above, as it is only used to develop the friction factor (f), but the roughness will have units of {m} if entered. If the user enters this roughness value, the pressure system will use it along with an approximation to the Moody chart to estimate the friction factor given the current flow conditions. This allows the friction calculate to be dynamic throughout the simulation.

#### Field: Fixed Friction Factor

This field represents the second method to simulate frictional losses. This is parameter (f) in the equation and is dimensionless. This parameter is a fixed value of friction factor which would override any calculations performed based on roughness, etc. If the user has a known friction factor for a given component, this is where it should be entered.

In the curve outputs for this object:

- Curve Input 1: MassFlow
- Curve Input 2: Density
- Curve Input 3: Velocity
- Curve Output: the resultant value

Following is an input example.

~~~~~~~~~~~~~~~~~~~~

    Curve:Functional:PressureDrop,
      PressureMinorAndFriction,!- Name
      0.05,                    !- Diameter
      53.8,                    !- Minor Loss Coefficient
      200,                     !- Length
      ,                        !- Roughness
      0.008;                   !- Fixed Friction Factor
~~~~~~~~~~~~~~~~~~~~

## Curve:FanPressureRise

Input for the fan total pressure rise curve consists of the curve name, the four coefficients, and the maximum and minimum valid independent variable values. Optional inputs for the curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation is:

![](media/image575.png)\


where Δ*P~fan,tot~* is the fan total pressure rise (Pa) as a function of volumetric flow through the fan (*Q~fan~*, m^3^/s), duct static pressure set point (*P~sm~*, Pa), and static pressure surrounding the ducts (*P~o~*, Pa). *P~o~* is assumed to be zero.

The first term of the curve looks like the common system curve in which the fan pressure rise is proportional to the square of the fan flow, but here it also depends implicitly on supply and return pressure losses, and in part on the fraction of the fan flow that is outdoor air (essentially "leaks" into and out of the return side of the system). Very often it is the only term considered, but that would only be correct with fixed-position dampers, no distribution system leakage, no linear resistance components, and no duct static pressure control.

The second term accounts for significant flow resistances in the system where the pressure difference is linearly proportional to the flow. Some filters and coils in the return may need this term to be adequately described. This term could be ignored if there are no linear components or if their pressure drops are very small compared to the other terms.

The third term, which depends on the fan flow and square root of the supply duct pressure *P~sm~*, accounts in part for leakage from the supply system when damper positions are fixed or are changed independently of static pressure or fan flow. In this case, reducing or eliminating supply leakage results in a different system curve. This, however, might be only a minor "correction" to the simple system curves generally used. The third term is zero when the VAV box dampers are modulated to control flow. Consequently, with variable-position supply dampers, reducing or eliminating supply leakage does not change the system curve.

The last term also accounts in part for leakage from the supply system when damper positions are fixed or are changed independently of static pressure or fan flow. This term indicates that the same fan pressure rise can be achieved by raising the duct pressure and closing dampers. The only change in the system in such a case is that the leakage flow may increase. The coefficient for this term is one when the VAV box dampers are modulated to control flow. In both cases, this term may be the most important "correction" to the simple system curves generally used, especially at low flows.

### Inputs

#### Field: Name

The required user-assigned unique alpha name for an instance of this curve. When a curve of this type is used, it is referenced by this name.

#### Field: Fan Pressure Rise Coefficient1 C1

The required numeric constant coefficient C*~1~* (Pa s^2^/m^6^) in the curve. Must be greater than zero.

#### Field: Fan Pressure Rise Coefficient2 C2

The required numeric constant coefficient C*~2~* (Pa s/m^3^) in the curve*.*

#### Field: Fan Pressure Rise Coefficient3 C3

The required numeric constant coefficient *C~3~* (Pa^0.5^ s/m^3^) in the curve*.*

#### Field: Fan Pressure Rise Coefficient4 C4

The required numeric constant coefficient C*~4~* (dimensionless) in the curve*.*

#### Field: Minimum Value of Qfan

The required numeric minimum allowable value of *Q~fan~* (m^3^/s). Values of *Q~fan~* less than the minimum will be replaced within this curve by the minimum.

#### Field: Maximum Value of Qfan

The required numeric maximum allowable value of *Q~fan~* (m^3^/s). Values of *Q~fan~* greater than the maximum will be replaced within this curve by the maximum.

#### Field: Minimum Value of Psm

The required numeric minimum allowable value of *P~sm~* (Pa). Values of *P~sm~* less than the minimum will be replaced within this curve by the minimum.

#### Field: Maximum Value of Psm

The required numeric maximum allowable value of *P~sm~* (Pa). Values of *P~sm~* greater than the maximum will be replaced within this curve by the maximum.

#### Field: Minimum Curve Output

The optional numeric minimum allowable value of the evaluated curve (Pa). Values less than the minimum will be replaced within this curve by the minimum.

#### Field: Maximum Curve Output

The optional numeric maximum allowable value of the evaluated curve (Pa). Values greater than the maximum will be replaced within this curve by the maximum.

The following is an input example describing the fan total pressure rise for a VAV system with a constant non-zero duct static pressure set point (the set point is described separately; see the [Curve:Linear](#curvelinear) object for a related example):

~~~~~~~~~~~~~~~~~~~~

      Curve:FanPressureRise,
        VSD Example,          ! Curve Name f = C1*Qfan**2+C2*Qfan+C3*Qfan*(Psm-Po)**0.5+C4*(Psm-Po) with Po=0
        1446.75833497653,     ! CoefficientC1 Alpha [Pa s2/m6]
        0.,                   ! CoefficientC2 Beta [Pa s/m3]
        0.,                   ! CoefficientC3 Gamma [Pa0.5 s/m3]
        1.,                   ! CoefficientC4 Delta [-]
        0.,                   ! Minimum Value of Qfan [m3/s]
        100.,                 ! Maximum Value of Qfan [m3/s]
        62.5,                 ! Minimum Value of Psm [Pa]
        300.,                 ! Maximum Value of Psm [Pa]
        0.;                   ! Minimum Curve Output [Pa]
        5000.;                ! Maximum Curve Output [Pa]
~~~~~~~~~~~~~~~~~~~~

## Curve:ExponentialSkewNormal

Input for the exponential-modified skew normal curve consists of the curve name, the four coefficients, and the maximum and minimum valid independent variable values. Optional inputs for the curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation is:

![](media/image576.png)\


where *Z~1~ =* (*x – C~1~*) */ C~2~*; *Z~2~ =* [(*exp*^(^*^C^3^\*x^*^)^ \* *C~4~\*x*) *- C~1~*] */ C~2~*; *Z~3~ = -C~1~ / C~2~*

### Inputs

#### Field: Name

The required user-assigned unique alpha name for an instance of this curve. When a curve of this type is used, it is referenced by this name.

#### Field: Coefficient1 C1

The required numeric constant coefficient *C~1~* in the curve.

#### Field: Coefficient2 C2

The required numeric constant coefficient *C~2~* in the curve. Must be non-zero.

#### Field: Coefficient3 C3

The required numeric constant coefficient *C~3~* in the curve.

#### Field: Coefficient4 C4

The required numeric constant coefficient *C~4~* in the curve.

#### Field: Minimum Value of x

The required numeric minimum allowable value of *x*. Values of *x* less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The required numeric maximum allowable value of *x*. Values of *x* greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The optional numeric minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The optional numeric maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for x

This optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum Value of *x* and Maximum Value of *x* (currently dimensionless). At this time, only the **Dimensionless** option is provided so that no unit conversion is used for this curve.

#### Field: Output Unit Type

This optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum Curve Output and Maximum Curve Output (currently dimensionless). At this time, only the **Dimensionless** option is provided so that no unit conversion is used for this curve.

The following are input examples describing the normalized fan efficiency for the normal (non-stall) and stall operating regions of a backward-curved airfoil centrifugal fan:

~~~~~~~~~~~~~~~~~~~~

      Curve:ExponentialSkewNormal,
        FanEff120CPLANormal,             ! Curve Name
        0.072613,                        ! CoefficientC1 Afan
        0.833213,                        ! CoefficientC2 Bfan
        0.,                              ! CoefficientC3 Cfan
        0.013911,                        ! CoefficientC4 Dfan
        -4.,                             ! Minimum Value of x
        5.,                              ! Maximum Value of x
        0.1,                             ! Minimum Curve Output
        1.;                              ! Maximum Curve Output

      Curve:ExponentialSkewNormal,
        FanEff120CPLAStall,              ! Curve Name
        -1.674931,                       ! CoefficientC1 Afan
        1.980182,                        ! CoefficientC2 Bfan
        0.,                              ! CoefficientC3 Cfan
        1.844950,                        ! CoefficientC4 Dfan
        -4.,                             ! Minimum Value of x
        5.,                              ! Maximum Value of x
        0.1,                             ! Minimum Curve Output
        1.;                              ! Maximum Curve Output
~~~~~~~~~~~~~~~~~~~~

## Curve:Sigmoid

Input for the sigmoid curve consists of the curve name, the five coefficients, and the maximum and minimum valid independent variable values. Optional inputs for the curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation is:

![](media/image577.png)\


### Inputs

#### Field: Name

The required user-assigned unique alpha name for an instance of this curve. When a curve of this type is used, it is referenced by this name.

#### Field: Coefficient1 C1

The required numeric constant coefficient *C~1~* in the equation.

#### Field: Coefficient2 C2

The required numeric constant coefficient *C~2~* in the equation.

#### Field: Coefficient3 C3

The required numeric constant coefficient *C~3~* in the equation.

#### Field: Coefficient4 C4

The required numeric constant coefficient *C~4~* in the equation.

#### Field: Coefficient5 C5

The required numeric constant coefficient *C~5~* in the equation.

#### Field: Minimum Value of x

The required numeric minimum allowable value of *x*. Values of *x* less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The required numeric maximum allowable value of *x*. Values of *x* greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The optional numeric minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The optional numeric maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for x

This optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum Value of *x* and Maximum Value of *x* (currently dimensionless). At this time, only the **Dimensionless** option is provided so that no unit conversion is used for this curve.

#### Field: Output Unit Type

This optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum Curve Output and Maximum Curve Output (currently dimensionless). At this time, only the **Dimensionless** option is provided so that no unit conversion is used for this curve.

The following are input examples describing the normalized dimensionless flow for normal (non-stall) and stall operating regions of a backward-curved airfoil centrifugal fan:

~~~~~~~~~~~~~~~~~~~~

      Curve:Sigmoid,
        FanDimFlowNormal,                ! Curve Name
        0.,                              ! CoefficientC1 Aspd
        1.001423,                        ! CoefficientC2 Bspd
        0.123935,                        ! CoefficientC3 Cspd
        -0.476026,                       ! CoefficientC4 Dspd
        1.,                              ! CoefficientC5 Espd
        -4.,                             ! Minimum Value of x
        5.,                              ! Maximum Value of x
        0.05,                            ! Minimum Curve Output
        1.;                              ! Maximum Curve Output

      Curve:Sigmoid,
        FanDimFlowStall,                 ! Curve Name
        0.,                              ! CoefficientC1 Aspd
        5.924993,                        ! CoefficientC2 Bspd
        -1.91636,                        ! CoefficientC3 Cspd
        -0.851779,                       ! CoefficientC4 Dspd
        1.,                              ! CoefficientC5 Espd
        -4.,                             ! Minimum Value of x
        5.,                              ! Maximum Value of x
        0.05,                            ! Minimum Curve Output
        1.;                              ! Maximum Curve Output
~~~~~~~~~~~~~~~~~~~~

## Curve:RectangularHyperbola1

Input for the single rectangular hyperbola type 1 curve consists of the curve name, the three coefficients, and the maximum and minimum valid independent variable values. Optional inputs for the curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation is:

*y =* (*C~1~·\* x*) */* (*C~2~+x*) *+ C~3~*

### Inputs

#### Field: Name

The required user-assigned unique alpha name for an instance of this curve. When a curve of this type is used, it is referenced by this name.

#### Field: Coefficient1 C1

The required numeric constant coefficient *C~1~* in the equation.

#### Field: Coefficient2 C2

The required numeric constant coefficient *C~2~* in the equation.

#### Field: Coefficient3 C3

The required numeric constant coefficient *C~3~* in the equation.

#### Field: Minimum Value of x

The required numeric minimum allowable value of *x*. Values of *x* less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The required numeric maximum allowable value of *x*. Values of *x* greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The optional numeric minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The optional numeric maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for x

This optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum Value of *x* and Maximum Value of *x* (currently dimensionless). At this time, only the **Dimensionless** option is provided so that no unit conversion is used for this curve.

#### Field: Output Unit Type

This optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum Curve Output and Maximum Curve Output (currently dimensionless). At this time, only the **Dimensionless** option is provided so that no unit conversion is used for this curve.

The following is an input example describing the maximum efficiency variation for a "mid- or average-efficiency" type of motor:

~~~~~~~~~~~~~~~~~~~~

        Curve:RectangularHyperbola1,
        MotorMaxEffAvg,                  ! Curve Name
        0.29228,                         ! CoefficientC1
        3.368739,                        ! CoefficientC2
        0.762471,                        ! CoefficientC3
        0.,                              ! Minimum Value of x
        7.6,                             ! Maximum Value of x
        0.01,                            ! Minimum Curve Output
        1.;                              ! Maximum Curve Output
~~~~~~~~~~~~~~~~~~~~

## Curve:RectangularHyperbola2

Input for the single rectangular hyperbola type 2 curve consists of the curve name, the three coefficients, and the maximum and minimum valid independent variable values. Optional inputs for the curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation is:

*y =* (*C~1~·\* x*) */* (*C~2~+x*) *+ C~3~·\* x*

### Inputs

#### Field: Name

The required user-assigned unique alpha name for an instance of this curve. When a curve of this type is used, it is referenced by this name.

#### Field: Coefficient1 C1

The required numeric constant coefficient *C~1~* in the equation.

#### Field: Coefficient2 C2

The required numeric constant coefficient *C~2~* in the equation.

#### Field: Coefficient3 C3

The required numeric constant coefficient *C~3~* in the equation.

#### Field: Minimum Value of x

The required numeric minimum allowable value of *x*. Values of *x* less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The required numeric maximum allowable value of *x*. Values of *x* greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The optional numeric minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The optional numeric maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for x

This optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum Value of *x* and Maximum Value of *x* (currently dimensionless). At this time, only the **Dimensionless** option is provided so that no unit conversion is used for this curve.

#### Field: Output Unit Type

This optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum Curve Output and Maximum Curve Output (currently dimensionless). At this time, only the **Dimensionless** option is provided so that no unit conversion is used for this curve.

The following are input examples describing part-load efficiency variations for a "medium efficiency" type of V-belt (Regions 1 and 3), for a "mid- or average-efficiency" nominal 25 hp 4-pole motor, and a nominal 30 hp VFD:

~~~~~~~~~~~~~~~~~~~~

    Curve:RectangularHyperbola2,
        BeltPartLoadRegion1,             ! Curve Name
        0.920797,                        ! CoefficientC1
        0.0262686,                       ! CoefficientC2
        0.151594,                        ! CoefficientC3
        0.,                              ! Minimum Value of x
        1.,                              ! Maximum Value of x
        0.01,                            ! Minimum Curve Output
        1.;                              ! Maximum Curve Output

      Curve:RectangularHyperbola2,
        BeltPartLoadRegion3,             ! Curve Name
        1.037778,                        ! CoefficientC1
        0.0103068,                       ! CoefficientC2
        -0.0268146,                      ! CoefficientC3
        0.,                              ! Minimum Value of x
        1.,                              ! Maximum Value of x
        0.01,                            ! Minimum Curve Output
        1.;                              ! Maximum Curve Output

      Curve:RectangularHyperbola2,
        MotorPartLoad,                   ! Curve Name
        1.137209,                        ! CoefficientC1
        0.0502359,                       ! CoefficientC2
        -0.0891503,                      ! CoefficientC3
        0.,                              ! Minimum Value of x
        1.,                              ! Maximum Value of x
        0.01,                            ! Minimum Curve Output
        1.;                              ! Maximum Curve Output

      Curve:RectangularHyperbola2,
        VFDPartLoad,                     ! Curve Name
        0.987405,                        ! CoefficientC1
        0.0155361,                       ! CoefficientC2
        -0.0059365,                      ! CoefficientC3
        0.,                              ! Minimum Value of x
        1.,                              ! Maximum Value of x
        0.01,                            ! Minimum Curve Output
        1.;                              ! Maximum Curve Output
~~~~~~~~~~~~~~~~~~~~

## Curve:ExponentialDecay

Input for the exponential decay curve consists of the curve name, the three coefficients, and the maximum and minimum valid independent variable values. Optional inputs for the curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation is:

*y = C~1~ + C~2~ \* exp*^(^*^C^3^\*^^x^*^)^

### Inputs

#### Field: Name

The required user-assigned unique alpha name for an instance of this curve. When a curve of this type is used, it is referenced by this name.

#### Field: Coefficient1 C1

The required numeric constant coefficient *C~1~* in the equation.

#### Field: Coefficient2 C2

The required numeric constant coefficient *C~2~* in the equation.

#### Field: Coefficient3 C3

The required numeric constant coefficient *C~3~* in the equation.

#### Field: Minimum Value of x

The required numeric minimum allowable value of *x*. Values of *x* less than the minimum will be replaced by the minimum.

#### Field: Maximum Value of x

The required numeric maximum allowable value of *x*. Values of *x* greater than the maximum will be replaced by the maximum.

#### Field: Minimum Curve Output

The optional numeric minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

#### Field: Maximum Curve Output

The optional numeric maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

#### Field: Input Unit Type for x

This optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum Value of *x* and Maximum Value of *x* (currently dimensionless). At this time, only the **Dimensionless** option is provided so that no unit conversion is used for this curve.

#### Field: Output Unit Type

This optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum Curve Output and Maximum Curve Output (currently dimensionless). At this time, only the **Dimensionless** option is provided so that no unit conversion is used for this curve.

The following is an input example describing the part-load efficiency variation for a "medium efficiency" type of V-belt (Region 2):

~~~~~~~~~~~~~~~~~~~~

      Curve:ExponentialDecay,
        BeltPartLoadRegion2,             ! Curve Name
        1.011965,                        ! CoefficientC1
        -0.339038,                       ! CoefficientC2
        -3.43626,                        ! CoefficientC3
        0.,                              ! Minimum Value of x
        1.,                              ! Maximum Value of x
        0.01,                            ! Minimum Curve Output
        1.;                              ! Maximum Curve Output
~~~~~~~~~~~~~~~~~~~~

## Curve:DoubleExponentialDecay

Input for the double exponential decay curve consists of the curve name, the five coefficients, and the maximum and minimum valid independent variable values. Optional inputs for the curve include the minimum and maximum output of the performance curve. The equation is:

![](media/image578.png)\


### Inputs

#### Field: Name

This field indicates the required user-assigned name for an instance of this curve. When a curve of this type is used, it is referenced by this name.

#### Field: Coefficient1 C1

The required numeric constant coefficient C~1~ in the equation.

#### Field: Coefficient2 C2

The required numeric constant coefficient C~2~ in the equation.

#### Field: Coefficient3 C3

The required numeric constant coefficient C~3~ in the equation.

#### Field: Coefficient4 C4

The required numeric constant coefficient C~4~ in the equation.

#### Field: Coefficient5 C5

The required numeric constant coefficient C~5~ in the equation.

#### Field: Minimum Value of x

The required numeric minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum

#### Field: Maximum Value of x

The required numeric maximum allowable value of x. Values of x greater than the maximum will be replaced by the maximum

#### Field: Minimum Curve Output

The optional numeric minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum

#### Field: Maximum Curve Output

The optional numeric maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum

#### Field: Input Unit Type for x

The optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum and Maximum Value of x. Currently, only Dimensionless option is provided so that no unit conversion is used for this curve

#### Field: Output Unit Type

The optional field is provided for future purposes so that the IDF Editor could display the appropriate SI or IP units for the Minimum and Maximum Curve Ouput. Currently, only Dimensionless option is provided so that no unit conversion is used for this curve.

An example input of the [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay) input is:

~~~~~~~~~~~~~~~~~~~~

      Curve:DoubleExponentialDecay,
        BatteryCycleCurve,     !- Name
        1380,                  !- Coefficient1 C1
        6834,                  !- Coefficient2 C2
        -8.75,                 !- Coefficient3 C3
        6747,                  !- Coefficient4 C4
        -6.22,                 !- Coefficient5 C5
        0,                     !- Minimum Value of x
        1.0;                   !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

## Curve:CubicLinear

This curve is a function of two independent variables. Input consists of the curve name, the six coefficients, and min and max values for each of the independent variables. Optional inputs for curve minimum and maximum may be used to limit the output of the performance curve (e.g., limit extrapolation). The equation represented by the cubic linear curve:

y=*C*1+*C*2\*x+*C*3\**x*2+*C*4\**x*3+*C*5+*C*6\*x\*y

### Field: Name

A user assigned unique name for an instance of a quadratic-linear curve. When a curve is used, it is referenced by this name.

### Field: Coefficient1 Constant

The constant coefficient (C~1~) in the equation.

### Field: Coefficient2 x

The coefficient C~2~ in the equation.

### Field: Coefficient3 x\*\*2

The coefficient C~3~ in the equation.

### Field: Coefficient4 x\*\*3

The coefficient C~4~ in the equation.

### Field: Coefficient5 y

The coefficient C~5~ in the equation.

### Field: Coefficient6 x\*y

The coefficient C~6~ in the equation.

### Field: Minimum Value of x

The minimum allowable value of x. Values of x less than the minimum will be replaced by the minimum.

### Field: Maximum Value of x

The maximum allowable value of x. Values of x greater than the maximum will be replaced by the maximum.

### Field: Minimum Value of y

The minimum allowable value of y. Values of y less than the minimum will be replaced by the minimum.

### Field: Maximum Value of y

The maximum allowable value of y. Values of y greater than the maximum will be replaced by the maximum.

### Field: Minimum Curve Output

The minimum allowable value of the evaluated curve. Values less than the minimum will be replaced by the minimum.

### Field: Maximum Curve Output

The maximum allowable value of the evaluated curve. Values greater than the maximum will be replaced by the maximum.

### Field: Input Unit Type for X

This field is used to indicate the kind of units that may be associated with the x values. The only option at this time is **Dimensionless.**

### Field: Input Unit Type for Y

This field is used to indicate the kind of units that may be associated with the x values. The only option at this time is **Dimensionless.**

### Field: Output Unit Type

**This field is used to indicate the kind of units that may be associated with the output values. The only option at this time is Dimensionless.**

In addition, the detailed ice storage input will be modified to accept either QuadraticLinear or CubicLinear curve input.

An example input for the new CubicLinear equation form is shown below.

Curve:CubicLinear,

InsideMeltIceDischarging, !- Name

0.108734675,              !- Coefficient1 Constant

-0.989874286,             !- Coefficient2 x

0.696303562,              !- Coefficient3 x\*\*2

-0.134945307,             !- Coefficient4 x\*\*3

1.724007415,              !- Coefficient5 y

-1.094020457,             !- Coefficient6 y\*x

0.25,                     !- Minimum Value of x

1,                        !- Maximum Value of x

0.69,                     !- Minimum Value of y

1.26,                     !- Maximum Value of y

0.0926,                   !- Minimum Curve Output

0.4938,                   !- Maximum Curve Output

Dimensionless,            !- Input Unit Type for X

Dimensionless,            !- Input Unit Type for Y

Dimensionless,            !- Output Unit Type

Curve:CubicLinear,

InsideMeltIceDischarging, !- Name

0.108734675,              !- Coefficient1 Constant

-0.989874286,             !- Coefficient2 x

0.696303562,              !- Coefficient3 x\*\*2

-0.134945307,             !- Coefficient4 x\*\*3

1.724007415,              !- Coefficient5 y

-1.094020457,             !- Coefficient6 y\*x

0.25,                     !- Minimum Value of x

1,                        !- Maximum Value of x

0.69,                     !- Minimum Value of y

1.26,                     !- Maximum Value of y

0.0926,                   !- Minimum Curve Output

0.4938,                   !- Maximum Curve Output

Dimensionless,            !- Input Unit Type for X

Dimensionless,            !- Input Unit Type for Y

Dimensionless,            !- Output Unit Type

## Performance Curve Outputs

### Performance Curve Output Value

The current value of the performance curve. This value is averaged over the time step being reported. Inactive or unused performance curves will show a value of -999 (e.g., equipment is off, a specific performance curve is not required for this aspect of the equipment model at this time step, etc.). This value means that the performance curve was not called during the simulation and, therefore, not evaluated. This inactive state value is only set at the beginning of each environment. When averaging over long periods of time, this inactive state value may skew results. In this case, use a detaled reporting frequency (ref. [Output:Variable](#outputvariable) object) to view results at each HVAC time step.

### Performance Curve Input Variable 1(-N) Value []

The current value of the nth independent variable passed to the performance curve. The order of the independent variables is in the same order as the model equation represented by this performance curve object. This value is averaged over the time step being reported.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Performance Curve Output Value []
    HVAC,Average,Performance Curve Input Variable 1 Value []
    HVAC,Average,Performance Curve Input Variable 2 Value []
    HVAC,Average,Performance Curve Input Variable 3 Value []
~~~~~~~~~~~~~~~~~~~~