# Variables

Variables are important to any programming language. All Erl variables, except trend variables, are treated the same way in Erl and can be used interchangeably with any instruction.

The rules for selecting names of EMS variables are:

No spaces are allowed in user-defined variable names.

Underscore ("_") is the only special character allowed in variable names.

Variable names are **not** case sensitive.

Names of variables with global scope must be unique.

Variables cannot be declared with the same name as a built-in variable.

Variables can be alpha numeric strings but should NOT start with a numeric.

The rules for using EMS variables are:

All numeric variables are treated as floating point numbers. You use the underlying Fortran language features to handle these as double precision real numbers.

You can use the SET statement to reassign sensor variables and built-in variables.

Actuator variables use SET to perform control actions.

EMS variables can have either local or global scope. Global scope means that the variables can be used across Erl programs and always refer to the same instance of a particular variable. Global variables must have unique names. Local scope means that variables can be used only within a given Erl program.

Erl programs have eight types of variables:

Sensor. Each EnergyManagementSystem:Sensor input object declares a user-defined variable and maps it to a variable elsewhere in EnergyPlus (via output variables). Variables so declared have global scope and are used to *get* time-varying input data from elsewhere in the EnergyPlus model.

Actuator. Each EnergyManagementSystem:Actuator input object declares a user-defined Erl variable and maps it to a variable elsewhere in EnergyPlus. Variables so declared have global scope and are used to *set* control results elsewhere in the EnergyPlus model.

Local. Local variables do not need to be explicitly declared. These undeclared variables will be automatically registered as EMS variables with local scope. This allows you to create new variables "on the fly."  Local variables can be used for temporary storage of intermediate results.

Global. EnergyManagementSystem:GlobalVariable input objects are used to declare variables with a user-defined name and global scope. Global variables can be used to store intermediate results that span across Erl programs. Because the Erl compiler does not support functions or argument passing, global variables have an important role in using subroutines. These variables are global only within Erl and not with respect to code elsewhere in EnergyPlus.

Built-in. The EMS system automatically declares a set of built-in variables with predefined names. These variables have global scope. The built-in variables are always created and cannot be eliminated.

Internal. Each EnergyManagementSystem:InternalVariable input object declares a user-defined Erl variable and maps it to a variable elsewhere in EnergyPlus. Variables so declared have global scope and are used to *get* static input data from elsewhere in EnergyPlus.

Trend. Each EnergyManagementSystem:TrendVariable input object declares a user-defined Erl trend variable and maps it to a global Erl variable for logging. Trend variables are used to store the history of Erl variables. Trend variables differ from other Erl variables in that they can be put to use only through the built-in trend functions (see Table 6).

Index. Each EnergyManagementSystem:CurveOrTableIndexVariable and/or EnergyManagementSystem:ConstructionIndexVariable declares a user defined Erl variable and maps it to a specific item in the IDF of indicated type.

Built-in variables, internal variables, and sensor variables are primarily used to *get* information about the state of the simulation. However, because all variables are treated alike, built-in variables and sensor variables can also be overwritten by the SET instruction. (This applies within the EMS only; the variables in the rest of EnergyPlus program will not be overwritten.)  This allows you to reassign constants such as "on" and "off" as well as the values of the other built-in variables and sensor variables. There is really no problem with this, as long as you know what you are doing. One possible application is to reset a sensor variable that references the outdoor drybulb temperature so it contains the temperature in degrees Fahrenheit instead of Celsius. Regardless of reassignment, sensor variables and the dynamic built-in variables (e.g., date and time) will be cleared and updated with the latest values from EnergyPlus at the next timestep. Resetting a sensor variable does not have a retroactive effect on the report variable to which it is mapped.

The actuator variable is the counterpart of the sensor variable. Sensor variables are used to *get* the state of building systems; actuator variables are used to *set* the state of building systems. When used with actuator variables, the SET instruction performs control actions on the object to which it maps.

## Built-In Variables

A set of built-in variables provides date, time, and weather information that is not available via standard report variables, as well as several handy constant variables such as "true," "false," "on," and "off." Several variables provide access to the time and date information during the course of a simulation.  The CurrentTime, Hour and Minute variables represent the point in time at end of the timestep currently being simulated. EnergyPlus primarily operates in standard time and these clock values are not adjusted for daylight savings time.  The built-in variable DaylightSavings time is available for use with the daylight savings time adjustment feature (see RunPeriodControl:DaylightSavingTime). The duration of simulation timesteps are available in the variables ZoneTimeStep and SystemTimeStep.

Table 2 lists the built-in variables that are always available for use in Erl programs.

Table: Built-In Unique Variables for Erl

-----------------------------------------------------------------------
   Variable Name     Value
-------------------- --------------------------------------------------
        Year         1900–2100

       Month         1–12

     DayOfMonth      1–31

     DayOfWeek       1–7 (1 = Sun, 2 = Mon, …)

     DayOfYear       1–365

      Holiday        0 if not. 1.0, 2.0, etc., for each type of
                     holiday in model

  DaylightSavings    0 or 1, 0 if not daylight savings time, 1 if
                     daylight savings time

    CurrentTime      0.0-24.0, (fractional hours)

        Hour         0–23 (whole hours only)

       Minute        1.0–60.0 (fractional minutes)

        True         1.0

       False         0.0

         On          1.0

        Off          0.0

         PI          3.14159265358979

      SunIsUp        0 (= no) or 1 (= yes)

     IsRaining       0 (= no) or 1 (= yes)

    ZoneTimeStep     Durating of the zone timestep in hours

   SystemTimeStep    Current duration of the system timestep in hours

 CurrentEnvironment  Integer index for the current environment
                     period (sizing periods and run periods accumulate
                     throughout the run)

        Null         Special structure that stops an actuator from
                     overriding control

 ActualDateAndTime   A simple sum of the values of the date/time
                     function. Could be used in random seeding.

     ActualTime      A simple sum of the values of the time part of
                     the date/time function. Could be used in random
                     seeding.
-----------------------------------------------------------------------

## Trend Variables

Trend variables are used to log data over time. They hold the history of Erl variables from previous zone timesteps. You create trend variables with an EnergyManagementSystem:TrendVariable input object,  each of which declares a user-defined Erl trend variable and maps it to a global Erl variable for logging.

Trend variables are also used to store the history of Erl variables. The trend input includes the number of timesteps that are to be logged. The logging of data is first-in-first-out, meaning that the oldest data are pushed out the back of the log to make room for the most recent data. The zone timestep is used as the (constant) increment of time between different elements in the log and this value, in fractional hours, is available in the ZoneTimeStep built-in variable.

Trend variables differ from other Erl variables in that they can be used only through the built-in trend functions (see Table 6). Trend functions provide a number of ways to analyze trend data and extract data from the log. There are functions to obtain the maximum, minimum, average, and sum of logged data. There is a function to obtain the rate of change in the data. All the trend functions include an argument for a log index. This index tells the function how far back in time to go when evaluating.
