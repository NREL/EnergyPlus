# Built-In Functions

Several useful, built-in functions are available for use in Erl programs. You cannot configure these; they are internal to the language processor inside EnergyPlus. They provide access to a subset of general service routines that are useful inside the main EnergyPlus program or are intrinsic functions available in the underlying Fortran language. The "@" character is used to signal to the language processor that the following character string defines a built-in function that is used to assign a result to an Erl variable. The characters appended to the "@" operator must be one of the predefined names listed in Table 4, Table 5, Table 6, or Table 7. The syntax of the function call will vary depending on the arguments required by the function, but the general structure is:

SET \<variable\> = @\<function name\>  \<argument1\>  \<argument2\>   …   \<argumentN\>

Where "argument" can be either an Erl variable or a numeric constant.

For example, the following two statements can be used to set the value of an Erl variable called mySupplyRH to have percent relative humidity.

SET mySupplyRH = @RhFnTdbWPb mySupplyDryblub mySupplyHumRat mySupplyPress

SET mySupplyRH = mySupplyRH \* 100

## Built-in Math Functions

Table 4 lists the built-in functions for common mathematical functions. The numerical model for these functions is provided by the underlying Fortran language and the compiler.

Table: Built-in Math Functions for Erl

----------------------------------------------------------------------
Function Name   Description                       Number of Arguments
--------------- -------------------------------- ---------------------
@Round          Decreases precision of real                1
                number argument to nearest
                whole number, remains a real
                number.

@Mod            Returns remainder after                    2
                dividing the first argument by
                the second.

@Sin            Sine, returns sine of angle                1
                given in radians.

@Cos            Cosine, returns cosine of angle            1
                given in radians.

@ArcSin         Arcsine, returns angle in                  1
                radians from sine of angle.

@ArcCos         ArcCosine, returns angle in                1
                radians from cosine of angle.

@DegToRad       Degrees to radians, returns                1
                radians from degrees.

@RadToDeg       Radians to degrees, returns                1
                degrees from radians.

@Exp            Exponential, $e^x$, returns                1
                result.

@Ln             Natural log, $log_e(x)$,                   1
                returns result.

@Max            Maximum, returns largest value             2
                of two arguments.

@Min            Minimum, returns smallest value            2
                of two arguments.

@Abs            Absolute value, returns                    1
                positive magnitude of argument.

@RandomUniform  Pseudo-Random Number Generator,            2
                returns random number with
                uniform probability
                distribution across the range
                of values passed as the
                arguments, inclusive. Argument
                1 is the lower limit. Argument
                2 is the upper limit.

@RandomNormal   Pseudo-Random Number Generator,            4
                returns random number with
                normal (Gaussian) probability
                distribution as a function of
                the mean, standard deviation,
                and limits. Argument 1 is the
                mean. Argument 2 is the
                standard deviation. Argument 3
                is the lower limit. Argument 4
                is the upper limit.

@SeedRandom     Random Seed, controls the seed             1
                used in the random number
                generator for calls to @
                RandomUniform and @
                RandomNormal.  Use is optional
                and provided for repeatable
                series of random numbers. The
                argument is rounded to the
                nearest whole number and then
                used to set the size and values
                of the seed for the number
                generator.
----------------------------------------------------------------------

## Built-In Simulation Management Functions

Table: Built-in EnergyPlus Simulation Management Functions for Erl

----------------------------------------------------------------------
Function Name  Description                        Number of Arguments
-------------- --------------------------------- ---------------------
@FatalHaltEp   Throws fatal error with time of             1
               occurrence and stops execution
               of current model. Argument
               passes a number that can be used
               as an error code.

@SevereWarnEp  Throws severe error with time of            1
               occurrence and continues
               execution. Argument passes a
               number that can be used as an
               error code.

@WarnEp        Throws warning error and                    1
               continues execution. Argument
               passes a number that can be used
               as an error code.
----------------------------------------------------------------------

## Built-in Trend Variable Functions

For control algorithms, you often need to be able put a sensor reading into some historical context. The trend variables are provided in Erl as a way to log the time history of data to use in control decisions. To use the trend variables in Erl programs, their values must be extracted and placed into normal Erl variables. Setting up an Erl variable as a trend variable requires an EnergyManagementSystem:TrendVariable input object. The access functions listed in Table 6 are used to obtain data from a trend variable during the execution of an Erl program. These functions act on trend variables and return values into the user's Erl variables for subsequent use in calculations. Each trend function takes the name of the trend variable and an index that identifies how far back in time the function should be applied. Trend variable names are also Erl variables but with special pointers to another data structure with the time series data storage. The trend logs have a first-in, first-out storage array where only the most recent data are retained. Each element in the history corresponds to the result for that value over a zone timestep. The time difference between trend log items is the zone timestep in hours, so that the slope returned by @TrendDirection is in per-hour units.

Table: Built-in Functions for Trend Variables in Erl

-----------------------------------------------------------------------
Function Name    Description                       Number of Arguments
---------------- -------------------------------- ---------------------
@TrendValue      Returns history value for a                2
                 particular number of timesteps
                 into the past. Dereferences
                 data stored in trend into
                 another Erl variable. Takes
                 trend variable name and the
                 specific timestep into the past
                 to return.

@TrendAverage    Returns historical average (               2
                 mean) for values in trend
                 variable. Takes trend variable
                 name and number of steps into
                 the past to analyze

@TrendMax        Returns historical maximum for             2
                 values in trend variable. Takes
                 trend variable name and number
                 of steps into the past to
                 analyze.

@TrendMin        Returns historical minimum for             2
                 values in trend variable within
                 the index. Takes trend variable
                 name and number of steps into
                 the past to analyze.

@TrendDirection  Returns slope of a linear least            2
                 squares fit of trend data
                 within the index. Positive if
                 trend is increasing, negative
                 if decreasing. Takes trend
                 variable name and number of
                 steps into the past to analyze.

@TrendSum        Returns sum of elements stored             2
                 in trend. Takes trend variable
                 name and number of steps into
                 the past to analyze.
-----------------------------------------------------------------------

The trend functions all take as their second argument an array index. This number should be considered an integer for locating an array position. (It will be rounded down to the nearest integer using Fortran's FLOOR intrinsic.) This index argument tells the trend functions how far back in time they should reach into the history log when they evaluate the function call. This enables you to compare long- and short-range trends.

## Built-in Psychrometric Functions

Building modeling often involves calculations related to moist air. A comprehensive set of built-in functions is available for psychrometric calculations. Table 7 lists the functions available for use in Erl programs that are related to moist air properties and some physical properties related to water. More discussion of the psychrometric functions is provided in the section "Pyschrometric services" in the Module Developer Guide.

Table: Built-in Psychrometric Functions for Erl

Function Name|Arguments|Description|Units
-------------|---------|-----------|-----
@RhoAirFnPbTdbW|Result|Density of moist air|kg/m^3^
-- |Input 1|Barometric pressure|Pa
-- |Input 2|Drybulb temperature|ºC
-- |Input 3|Humidity ratio|kgWater/kgDryAir
@CpAirFnWTdb|Result|Heat capacity of moist air|J/kg-°C
-- |Input 1|Humidity ratio|kgWater/kgDryAir
-- |Input 2|Drybulb temperature|ºC
@HfgAirFnWTdb|Result|Heat of vaporization for vapor|J/kg
-- |Input 1|Humidity ratio|kgWater/kgDryAir
-- |Input 2|Drybulb temperature|ºC
@HgAirFnWTdb|Result|Enthalpy of the gas|
-- |Input 1|Humidity ratio|kgWater/kgDryAir
-- |Input 2|Drybulb temperature|ºC
@TdpFnTdbTwbPb|Result|Dew-point temperature|ºC
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Wetbulb temperature|ºC
-- |Input 3|Barometric pressure|Pa
@TdpFnWPb|Result|Dew-point temperature|ºC
-- |Input 1|Humidity ratio|kgWater/kgDryAir
-- |Input 2|Barometric pressure|Pa
@HFnTdbW|Result|Enthalpy of moist air|J/kg
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Humidity ratio|kgWater/kgDryAir
@HFnTdbRhPb|Result|Enthalpy of moist air|J/kg
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Relative humidity |Fraction (0.0..1)
-- |Input 3|Barometric pressure|Pa
@TdbFnHW|Result|Drybulb temperature|ºC
-- |Input 1|Enthalpy of moist air|J/kg
-- |Input 2|Humidity ratio|kgWater/kgDryAir
@RhovFnTdbRh|Result|Vapor density in air|kg/m^3^
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Humidity ratio|kgWater/kgDryAir
@RhovFnTdbWPb|Result|Vapor density in air|kg/m^3^
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Humidity ratio|kgWater/kgDryAir
-- |Input 3|Barometric pressure|Pa
@RhFnTdbRhov|Result|Relative humidity |Fraction (0.0..1)
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Vapor density in air|kg/m^3^
@RhFnTdbWPb|Result|Relative humidity |Fraction [0.0..1.0]
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Humidity ratio|kgWater/kgDryAir
-- |Input 3|Barometric pressure|Pa
@TwbFnTdbWPb|Result|Wetbulb temperature|ºC
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Humidity ratio|kgWater/kgDryAir
-- |Input 3|Barometric pressure|Pa
@VFnTdbWPb|Result|Specific volume |m^3^/kg
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Humidity ratio|kgWater/kgDryAir
-- |Input 3|Barometric pressure|Pa
@WFnTdpPb|Result|Humidity ratio|kgWater/kgDryAir
-- |Input 1|Dew-point temperature|ºC
-- |Input 2|Barometric pressure|Pa
@WFnTdbH|Result|Humidity ratio|kgWater/kgDryAir
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Enthalpy of moist air|J/kg
@WFnTdbTwbPb|Result|Humidity ratio|kgWater/kgDryAir
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Wetbulb temperature|ºC
-- |Input 3|Barometric pressure|Pa
@WFnTdbRhPb|Result|Humidity ratio|kgWater/kgDryAir
-- |Input 1|Drybulb temperature|ºC
-- |Input 2|Relative humidity |Fraction [0.0..1.0]
-- |Input 3|Barometric pressure|Pa
@PsatFnTemp|Result|Saturation pressure|Pa
-- |Input 1|Drybulb temperature|ºC
@TsatFnHPb|Result|Saturation temperature|ºC
-- |Input 1|Enthalpy of moist air|J/kg
-- |Input 2|Barometric pressure|Pa
@CpCW|Result|Heat capacity of water|J/kg-K
-- |Input 1|Temperature|ºC
@CpHW|Result|Heat capacity of water|J/kg-K
-- |Input 1|Temperature|ºC
@RhoH2O|Result|Density of water|kg/m^3^
-- |Input 1|Temperature|ºC

## Built-in Curve and Table Functions

EnergyPlus has a number of different generic curve and table input objects that are used to describe the performance characteristics for various component models.  Table 8 describes a built-in function called @CurveValue that is available for reusing those curve and table input objects in your Erl programs.  Although the Erl language could be used to replicate the functionality, reusing those input objects can have advantages because the input may have already been developed for use in traditional component models or the limiting and interpolation methods are helpful.  The @CurveValue function expects six arguments, although usually only a subset of them will be used depending on the number of independent variables involved with the curve or table.  Because Erl does not support passing optional arguments, dummy variables do need to be included in the function call for all unused independent variables.  For example, the Curve:Biquadratric object has only x and y independent variables, so input arguments 4, 5, and 6 will not be used when @CurveValue is evaluated:

Set MyCurveResult = @CurveValue myCurveIndex X1 Y1 dummy dummy dummy;

The first input argument is always an Erl variable that has been declared using an EnergyManagementSystem:CurveOrTableIndexVariable input object.  This variable identifies the location of a specific curve or table in the program's internal data structures.  It is important that you do not inadvertently reassign the value held in this variable because it is only filled once at the beginning of the simulation.

Table: Built-in Function for Accessing Curves and Tables

-----------------------------------------------------------------------------
Function Name  Arguments  Description           Notes
-------------- ---------- --------------------- -----------------------------
@CurveValue    Result     Result from           --
                          evaluating the curve
                          or table as a
                          function of the
                          input arguments

--             Input 1    Index variable        This variable needs to be
                          that "points" to a    declared and filled using an
                          specific curve or     EnergyManagementSystem:
                          table object defined  CurveOrTableIndexVariable
                          elsewhere in the      object.
                          IDF.

--             Input 2    First independent     Typically the "X" input
                          variable              value, always used

--             Input 3    Second independent    Typically the "Y" value,
                          variable              only used if curve/table has
                                                two or more independent
                                                variables

--             Input 4    Third independent     Typically the "Z" value,
                          variable              only used if curve/table has
                                                three or more independent
                                                variables.

--             Input 5    Fourth independent    Only used if table has four
                          variable              or more independent
                                                variables

--             Input 6    Fifth independent     Only used if table has five
                          variable              independent variables
-----------------------------------------------------------------------------
