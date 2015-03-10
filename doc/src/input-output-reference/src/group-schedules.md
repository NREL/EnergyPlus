# Group -- Schedules

This group of objects allows the user to influence scheduling of many items (such as occupancy density, lighting, thermostatic controls, occupancy activity). In addition, schedules are used to control shading element density on the building.

EnergyPlus schedules consist of three pieces:  a day description, a week description, and an annual description. An optional element is the schedule type. Each description level builds off the previous sub-level. The day description is simply a name and the values that span the 24 hours in a day to be associated with that name. The week description also has an identifier (name) and twelve additional names corresponding to previously defined day descriptions. There are names for each individual day of the week plus holiday, summer design day, winter design day and two more custom day designations. Finally, the annual schedule contains an identifier and the names and FROM-THROUGH dates of the week schedules associate with this annual schedule. The annual schedule can have several FROM-THROUGH date pairs.  One type of schedule reads the values from an external file to facilitate the incorporation of monitored data or factors that change throughout the year.

Schedules are processed by the EnergyPlus Schedule Manager, stored within the Schedule Manager and are accessed through module routines to get the basic values (timestep, hourly, etc). Values are resolved at the [Zone](#zone) [Timestep](#timestep) frequency and carry through any HVAC timesteps.

## Day Type

A brief description of "Day Type" which is used in the SizingPeriod objects, [RunPeriodControl:SpecialDays](#runperiodcontrolspecialdays) object, the Sizing objects and also used by reference in the [Schedule:Week:Daily](#scheduleweekdaily) object (discussed later in this section).

Schedules work on days of the week as well as certain specially designated days. Days of the week are the normal – Sunday, Monday, Tuesday, Wednesday, Thursday, Friday and Saturday. Special day types that can be designated are: Holiday, SummerDesignDay, WinterDesignDay, CustomDay1, CustomDay2. These day types can be used at the user's convenience to designate special scheduling (e.g. lights, electric equipment, set point temperatures) using these days as reference.

For example, a normal office building may have normal "occupancy" rules during the weekdays but significantly different use on weekend. For this, you would set up rules/schedules based on the weekdays (Monday through Friday, in the US) and different rules/schedules for the weekend (Saturday and Sunday, in the US). However, you could also specially designate SummerDesignDay and WinterDesignDay schedules for sizing calculations. These schedules can be activated by setting the Day Type field in the Design Day object to the appropriate season (**SummerDesignDay** for cooling design calculations; **WinterDesignDay** for heating design calculations).

In a different building, such as a theater/playhouse, the building may only have occupancy during certain weeks of the year and/or certain hours of certain days. If it was every week, you could designate the appropriate values during the "regular" days (Sunday through Saturday). But this would also be an ideal application for the "CustomDay1" and/or "CustomDay2". Here you would set the significant occupancy, lighting, and other schedules for the custom days and use unoccupied values for the normal weekdays. Then, using a weather file and setting special day periods as appropriate, you will get the "picture" of the building usage during the appropriate periods.

## ScheduleTypeLimits

Schedule types can be used to validate portions of the other schedules. Hourly day schedules, for example, are validated by range -- minimum/maximum (if entered) -- as well as numeric type (continuous or discrete). Annual schedules, on the other hand, are only validated for range – as the numeric type validation has already been done.

### Inputs

#### Field: Name

This alpha field should contain a unique (within the schedule types) designator. It is referenced wherever Schedule Type Limits Names can be referenced.

#### Field: Lower Limit Value

In this field, the lower (minimum) limit value for the schedule type should be entered. If this field is left blank, then the schedule type is not limited to a minimum/maximum value range.

#### Field: Upper Limit Value

In this field, the upper (maximum) limit value for the schedule type should be entered. If this field is left blank, then the schedule type is not limited to a minimum/maximum value range.

#### Field: Numeric Type

This field designates how the range values are validated. Using **Continuous** in this field allows for all numbers, including fractional amounts, within the range to be valid. Using **Discrete** in this field allows only integer values between the minimum and maximum range values to be valid.

#### Field: Unit Type

This field is used to indicate the kind of units that may be associated with the schedule that references the [ScheduleTypeLimits](#scheduletypelimits) object. It is used by IDF Editor to display the appropriate SI and IP units. This field is not used by EnergyPlus. The available options are shown below. If none of these options are appropriate, select **Dimensionless.**

- Dimensionless
- Temperature
- DeltaTemperature
- PrecipitationRate
- Angle
- Convection Coefficient
- Activity Level
- Velocity
- Capacity
- Power 
- Availability
- Percent
- Control 
- Mode

Several IDF Examples will illustrate the use:

~~~~~~~~~~~~~~~~~~~~

    ScheduleTypeLimits,Any Number;  ! Not limited
    ScheduleTypeLimits,Fraction, 0.0 , 1.0 ,CONTINUOUS;
    ScheduleTypeLimits,Temperature,-60,200,CONTINUOUS;
    ScheduleTypeLimits,Control Type,0,4,DISCRETE;
    ScheduleTypeLimits,On/Off,0,1,DISCRETE;
~~~~~~~~~~~~~~~~~~~~

## Day Schedules

The day schedules perform the assignment of pieces of information across a 24 hour day. This can occur in various fashions including a 1-per hour assignment, a user specified interval scheme or a list of values that represent an hour or portion of an hour.

## Schedule:Day:Hourly

The [Schedule:Day:Hourly](#scheduledayhourly) contains an hour-by-hour profile for a single simulation day.

### Inputs

#### Field: Name

This field should contain a unique (within all DaySchedules) designation for this schedule. It is referenced by WeekSchedules to define the appropriate schedule values.

#### Field: Schedule Type Limits Name

This field contains a reference to the Schedule Type Limits object. If found in a list of Schedule Type Limits (see [ScheduleTypeLimits](#scheduletypelimits) object above), then the restrictions from the referenced object will be used to validate the hourly field values (below).

#### Field: Hour Values (1-24)

These fields contain the hourly values for each of the 24 hours in a day. (Hour field 1 represents clock time 00:00:01 AM to 1:00:00 AM, hour field 2 is 1:00:01 AM to 2:00:00 AM, etc.)  The values in these fields will be passed to the simulation as indicated for "scheduled" items.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Schedule:Day:Hourly, Day On Peak, Fraction,
      0.,0.,0.,0.,0.,0.,0.,0.,0.,1.,1.,1.,1.,1.,1.,1.,1.,1.,0.,0.,0.,0.,0.,0.;
~~~~~~~~~~~~~~~~~~~~

## Schedule:Day:Interval

The [Schedule:Day:Interval](#scheduledayinterval) introduces a slightly different way of entering the schedule values for a day. Using the intervals, you can shorten the "hourly" input of the "[Schedule:Day:Hourly](#scheduledayhourly)" object to 2 fields. And, more importantly, you can enter an interval that represents only a portion of an hour. Schedule values are "given" to the simulation at the zone timestep, so there is also a possibility of "interpolation" from the entries used in this object to the value used in the simulation.

### Inputs

#### Field: Name

This field should contain a unique (within all DaySchedules) designation for this schedule. It is referenced by WeekSchedules to define the appropriate schedule values.

#### Field: Schedule Type Limits Name

This field contains a reference to the Schedule Type Limits object. If found in a list of Schedule Type Limits (see [ScheduleTypeLimits](#scheduletypelimits) object above), then the restrictions from the referenced object will be used to validate the hourly field values (below).

#### Field: Interpolate to Timestep

The value contained in this field directs how to apply values that aren't coincident with the given timestep (ref: [Timestep](#timestep)) intervals. If "Yes" is entered, then any intervals entered here will be interpolated/averaged and that value will be used at the appropriate minute in the hour. If "No" is entered, then the value that occurs on the appropriate minute in the hour will be used as the schedule value.

For example, if "yes" is entered and the interval is every 15 minutes (say a value of 0 for the first 15 minutes, then .5 for the second 15 minutes) AND there is a 10 minute timestep for the simulation: the value at 10 minutes will be 0 and the value at 20 minutes will be .25. For the same input entries but "no" for this field, the value at 10 minutes will be 0 and the value at 20 minutes will be .5.

#### Field-Set: Time and Value (extensible object)

To specify each interval, both an "until" time (which includes the designated time) and the value must be given.

#### Field: Time

The value of each field should represent clock time (standard) in the format "Until: HH:MM". 24 hour clock format (i.e. 1PM is 13:00) is used. Note that Until: 7:00 includes all times up through 07:00 or 7am.

#### Field: Value

This represents the actual value to be passed to the simulation at the appropriate timestep. (Using interpolation value as shown above). Limits on the values are indicated by the Schedule Type Limits Name field of this object.

And an example of use:

~~~~~~~~~~~~~~~~~~~~

    Schedule:Day:Interval,
        dd winter rel humidity,  !- Name
        Percent,                 !- Schedule Type Limits Name
        No,                      !- Interpolate to Timestep
        until: 24:00,             !- Time 1
        74;                      !- Value Until Time 1
~~~~~~~~~~~~~~~~~~~~

## Schedule:Day:List

To facilitate possible matches to externally generated data intervals, this object has been included. In similar fashion to the [Schedule:Day:Interval](#scheduledayinterval) object, this object can also include "sub-hourly" values but must represent a complete day in its list of values.

### Inputs

#### Field: Name

This field should contain a unique (within all day schedules) designation for this schedule. It is referenced by week schedules to define the appropriate schedule values.

#### Field: Schedule Type Limits Name

This field contains a reference to the Schedule Type Limits object. If found in a list of Schedule Type Limits (see [ScheduleTypeLimits](#scheduletypelimits) object above), then the restrictions from the referenced object will be used to validate the hourly field values (below).

#### Field: Interpolate to Timestep

The value contained in this field directs how to apply values that aren't coincident with the given timestep (ref: [Timestep](#timestep)) intervals. If "Yes" is entered, then any intervals entered here will be interpolated/averaged and that value will be used at the appropriate minute in the hour. If "No" is entered, then the value that occurs on the appropriate minute in the hour will be used as the schedule value.

For example, if "yes" is entered and the minutes per item is 15 minutes (say a value of 0 for the first 15 minutes, then .5 for the second 15 minutes) AND there is a 10 minute timestep for the simulation: the value at 10 minutes will be 0 and the value at 20 minutes will be .25. For the same input entries but "no" for this field, the value at 10 minutes will be 0 and the value at 20 minutes will be .5.

#### Field: Minutes Per Item

This field allows the "list" interval to be specified in the number of minutes for each item. The value here must be <= 60 and evenly divisible into 60 (same as the timestep limits).

#### Field Value 1 (same definition for each value – up to 1440 (24\*60) allowed)

This is the value to be used for the specified number of minutes.

For example:

~~~~~~~~~~~~~~~~~~~~

    Schedule:Day:List,
      Myschedule,  ! name
      Fraction,   ! Schedule type
      No,   ! Interpolate value
      30,   ! Minutes per item
      0.0,  ! from 00:01 to 00:30
      0.5,  ! from 00:31 to 01:00
      <snipped>
~~~~~~~~~~~~~~~~~~~~

## Week Schedule(s)

The week schedule object(s) perform the task of assigning the day schedule to day types in the simulation. The basic week schedule is shown next.

## Schedule:Week:Daily

### Inputs

#### Field: Name

This field should contain a unique (within all WeekSchedules) designation for this schedule. It is referenced by Schedules to define the appropriate schedule values.

#### Field: Schedule Day Name Fields (12 day types – Sunday, Monday, … )

These fields contain day schedule names for the appropriate day types. Days of the week (or special days as described earlier) will then use the indicated hourly profile as the actual schedule value.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Schedule:Week:Daily, Week on Peak,
            Day On Peak,Day On Peak,Day On Peak,
            Day On Peak,Day On Peak,Day On Peak,
            Day On Peak,Day On Peak,Day On Peak,
            Day On Peak,Day On Peak,Day On Peak;
~~~~~~~~~~~~~~~~~~~~

## Schedule:Week:Compact

Further flexibility can be realized by using the [Schedule:Week:Compact](#scheduleweekcompact) object. In this the fields, after the name is given, a "for" field is given for the days to be assigned and then a dayschedule name is used.

### Inputs

#### Field:Name

This field should contain a unique (within all WeekSchedules) designation for this schedule. It is referenced by Schedules to define the appropriate schedule values.

#### Field-Set – DayType List#, Schedule:Day Name #

Each assignment is made in a pair-wise fashion. First the "days" assignment and then the dayschedule name to be assigned. The entire set of day types must be assigned or an error will result.

#### Field: DayType List #

This field can optionally contain the prefix "For" for clarity. Multiple choices may then be combined on the line. Choices are: Weekdays, Weekends, Holidays, Alldays, SummerDesignDay, WinterDesignDay, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, CustomDay1, CustomDay2. In fields after the first "for", AllOtherDays may also be used. Note that the colon (:) after the For is optional but is suggested for readability.

#### Field: Schedule:Day Name #

This field contains the name of the day schedule (any of the Schedule:Day object names) that is to be applied for the days referenced in the prior field.

Some IDF examples:

~~~~~~~~~~~~~~~~~~~~

    Schedule:Week:Compact, Week on Peak,
            For: AllDays,
            Day On Peak;
    Schedule:Week:Compact, WeekDays on Peak,
            WeekDays,
            Day On Peak,
            AllOtherDays
            Day Off Peak;
~~~~~~~~~~~~~~~~~~~~

## Schedule:Year

The yearly schedule is used to cover the entire year using references to week schedules (which in turn reference day schedules). If the entered schedule does not cover the entire year, a fatal error will result.

### Inputs

#### Field: Name

This field should contain a unique (between [Schedule:Year](#scheduleyear), [Schedule:Compact](#schedulecompact), and [Schedule:File](#schedulefile)) designation for the schedule. It is referenced by various "scheduled" items (e.g. [Lights](#lights), [People](#people), Infiltration) to define the appropriate schedule values.

#### Field: Schedule Type Limits Name

This field contains a reference to the Schedule Type Limits object. If found in a list of Schedule Type Limits (see [ScheduleTypeLimits](#scheduletypelimits) object above), then the restrictions from the referenced object will be used to validate the hourly field values (below).

#### Field Set (WeekSchedule, Start Month and Day, End Month and Day)

Each of the designated fields is used to fully define the schedule values for the indicated time period). Up to 53 sets can be used. An error will be noted and EnergyPlus will be terminated if an incomplete set is entered. Missing time periods will also be noted as warning errors; for these time periods a zero (0.0) value will be returned when a schedule value is requested. Each of the sets has the following 5 fields:

#### Field: Schedule Week Name #

This field contains the appropriate WeekSchedule name for the designated time period.

#### Field: Start Month #

This numeric field is the starting month for the schedule time period.

#### Field: Start Day #

This numeric field is the starting day for the schedule time period.

#### Field: End Month #

This numeric field is the ending month for the schedule time period.

#### Field: End Day #

This numeric field is the ending day for the schedule time period.

Note that there are many possible periods to be described. An IDF example with a single period:

~~~~~~~~~~~~~~~~~~~~

    Schedule:Year, On Peak, Fraction,
            Week On Peak, 1,1, 12,31;
~~~~~~~~~~~~~~~~~~~~

And a multiple period illustration:

~~~~~~~~~~~~~~~~~~~~

    Schedule:Year,CoolingCoilAvailSched,Fraction,
            FanAndCoilAllOffWeekSched, 1,1, 3,31,
            FanAndCoilSummerWeekSched, 4,1, 9,30,
            FanAndCoilAllOffWeekSched, 10,1, 12,31;
~~~~~~~~~~~~~~~~~~~~

The following definition will generate an error (if any scheduled items are used in the simulation):

~~~~~~~~~~~~~~~~~~~~

    Schedule:Year,MySchedule,Fraction,4,1,9,30;
~~~~~~~~~~~~~~~~~~~~

## Schedule:Compact

For flexibility, a schedule can be entered in "one fell swoop". Using the [Schedule:Compact](#schedulecompact) object, all the features of the schedule components are accessed in a single command. Like the "regular" schedule object, each schedule:compact entry must cover all the days for a year. Additionally, the validations for DaySchedule (i.e. must have values for all 24 hours) and WeekSchedule (i.e. must have values for all day types) will apply. Schedule values are "given" to the simulation at the zone timestep, so there is also a possibility of "interpolation" from the entries used in this object to the value used in the simulation.

This object is an unusual object for description. For the data the number of fields and position are not set, they cannot really be described in the usual Field # manner. Thus, the following description will list the fields and order in which they must be used in the object. The name and schedule type are the exceptions:

### Inputs

#### Field: Name

This field should contain a unique (between [Schedule:Year](#scheduleyear), [Schedule:Compact](#schedulecompact), and [Schedule:File](#schedulefile)) designation for the schedule. It is referenced by various "scheduled" items (e.g. [Lights](#lights), [People](#people), Infiltration) to define the appropriate schedule values.

#### Field: Schedule Type Limits Name

This field contains a reference to the Schedule Type Limits object. If found in a list of Schedule Type Limits (see [ScheduleTypeLimits](#scheduletypelimits) object above), then the restrictions from the referenced object will be used to validate the hourly field values (below).

#### Field-Set (Through, For, Interpolate, Until, Value)

Each compact schedule must contain the elements Through (date), For (days), Interpolate (optional), Until (time of day) and Value. In general, each of the "titled" fields must include the "title". Note that the colon (:) after these elements (Through, For, Until) is optional but is suggested for readability.

#### Field: Through

This field starts with "Through:" and contains the ending date for the schedule period (may be more than one). Refer to Table 3. Date Field Interpretation for information on date entry – note that only Month-Day combinations are allowed for this field. Each "through" field generates a new WeekSchedule named "Schedule Name"_wk_# where # is the sequential number for this compact schedule.

#### Field: For

This field starts with "For:" and contains the applicable days (reference the compact week schedule object above for complete description) for the 24 hour period that must be described. Each "for" field generates a new DaySchedule named "Schedule Name"_dy_# where # is the sequential number for this compact schedule.

#### Field: Interpolate (optional)

This field, if used, starts with "Interpolate:" and contains the word "Yes" or "No". If this field is not used, it should not be blank – rather just have the following field appear in this slot. The definition of "Interpolate" in this context is shown in the interval day schedule above.

#### Field: Until

This field contains the ending time (again, reference the interval day schedule discussion above) for the current days and day schedule being defined.

#### Field: Value

Finally, the value field is the schedule value for the specified time interval.

And, some IDF examples:

~~~~~~~~~~~~~~~~~~~~

    Schedule:Compact,
      POFF,    !- Name
      Fraction,  !- Schedule Type Limits Name
      Through: 4/30,
      For: AllDays,
      Until: 24:00, 1.0,
      Through: 12/31,
      For: Weekdays,
       Until: 7:00,   .1,
       Until: 17:00, 1.0,
       Until: 24:00,  .1,
      For: Weekends Holidays,
       Until: 24:00,  .1,
      For: AllOtherDays,
       Until: 24:00,  .1;
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    ! Schedule Continuous
    Schedule:Compact,
      Continuous,
      on/off,
      Through: 12/31,
      For: AllDays,
      Until: 24:00, 1.0;
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    ! Schedule Daytime Ventilation
    Schedule:Compact,
      Daytime Ventilation,
      Fraction,
      Through: 12/31,
      For: Weekdays SummerDesignDay,
      Until: 08:00, 0.0,
      Until: 18:00, 1.0,
      Until: 24:00, 0.0,
      For: Weekends WinterDesignDay,
      Until: 10:00, 0.0,
      Until: 16:00, 1.0,
      Until: 24:00, 0.0,
      For: Holidays AllOtherDays,
      Until: 24:00, 0.0;
~~~~~~~~~~~~~~~~~~~~

## Schedule:Constant

The constant schedule is used to assign a constant hourly value. This schedule is created when a fixed hourly value is desired to represent a period of interest (e.g., always on operation mode for supply air fan).

### Inputs

#### Field: Name

This field should contain a unique name designation for this schedule. It is referenced by Schedules to define the appropriate schedule value.

#### Field: Schedule Type Limits Name

This field contains a reference to the Schedule Type Limits object. If found in a list of Schedule Type Limits (see [ScheduleTypeLimits](#scheduletypelimits) object above), then the restrictions from the referenced object will be used to validate the hourly field values (below).

#### Field: Hourly Value

This field contains a constant real value. A fixed value is assigned as an hourly value.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Schedule:Constant,
      AlwaysOn,                  !- Name
      On/Off,                    !- Schedule Type Limits Name
      1.0;                       !- Hourly Value

    ScheduleTypeLimits,
        On/Off,                  !- Name
        0,                       !- Lower Limit Value
        1,                       !- Upper Limit Value
        DISCRETE,                !- Numeric Type
        Availability;            !- Unit Type
~~~~~~~~~~~~~~~~~~~~

## Schedule:File

At times, data is available from a building being monitored or for factors that change throughout the year. The [Schedule:File](#schedulefile) object allows this type of data to be used in EnergyPlus as a schedule. [Schedule:File](#schedulefile) can also be used to read in hourly or sub-hourly schedules computed by other software or developed in a spreadsheet or other utility.

The format for the data file referenced is a text file with values separated by commas (or other optional delimiters) with one line per hour. The file may contain header lines that are skipped. The file should contain values for an entire year (8760 or 8784 hours of data) or a warning message will be issued. Multiple schedules may be created using a single external data file or multiple external data files may be used.  The first row of data must be for January 1, hour 1 (or timestep 1 for subhourly files).

[Schedule:File](#schedulefile) may be used along with the [FuelFactors](#fuelfactors) object and TDV files in the DataSets directory to compute Time Dependent Valuation based source energy as used by the California Energy Commission's Title 24 Energy Code. See Fuel Factor for more discussion on Time Dependent Valuation.

Two optional fields: **Interpolate to Timestep** and **Minutes per Item** allow for the input of sub-hourly schedules (similar to the [Schedule:Day:List](#scheduledaylist) object).

### Inputs

#### Field: Name

This field should contain a unique (between [Schedule:Year](#scheduleyear), [Schedule:Compact](#schedulecompact), and [Schedule:File](#schedulefile)) designation for the schedule. It is referenced by various "scheduled" items (e.g. [Lights](#lights), [People](#people), Infiltration, [FuelFactors](#fuelfactors)) to define the appropriate schedule values.

#### Field: Schedule Type Limits Name

This field contains a reference to the Schedule Type Limits object. If found in a list of Schedule Type Limits (see [ScheduleTypeLimits](#scheduletypelimits) object above), then the restrictions from the referenced object will be used to validate the hourly field values (below).

#### Field: File Name

This field contains the name of the file that contains the data for the schedule. The field should include a full path with file name, for best results. The field must be <= 100 characters. The file name must not include commas or an exclamation point. A relative path or a simple file name should work with version 7.0 or later when using EP-Launch even though EP-Launch uses temporary directories as part of the execution of EnergyPlus. If using RunEPlus.bat to run EnergyPlus from the command line, a relative path or a simple file name may work if RunEPlus.bat is run from the folder that contains EnergyPlus.exe.

#### Field: Column Number

The column that contains the value to be used in the schedule. The first column is column one. If no data for a row appears for a referenced column the value of zero is used for the schedule value for that hour.

#### Field: Rows to Skip at Top

Many times the data in a file contains rows (lines) that are describing the files or contain the names of each column. These rows need to be skipped and the number of skipped rows should be entered for this field. The next row after the skipped rows must contain data for January 1, hour 1.

#### Field: Number of Hours of Data

The value entered in this field should be either 8760 or 8784 as the number of hours of data. 8760 does not include the extra 24 hours for a leap year (if needed).  8784 will include the possibility of leap year data which can be processed according to leap year indicators in the weather file or specified elsewhere. Note if the simulation does not have a leap year specified but the schedule file contains 8784 hours of data, the first 8760 hours of data will be used. The schedule manager will not know to skip the 24 hours representing Feburary 29.

#### Field: Column Separator

This field specifies the character used to separate columns of data if the file has more than one column. The choices are: Comma, Tab, Fixed (spaces fill each column to a fixed width), or Semicolon. The default is Comma.

#### Field: Interpolate to Timestep

The value contained in this field directs how to apply values that aren't coincident with the given timestep (ref: TimeStep object) intervals. If "Yes" is entered, then any intervals entered here will be interpolated/averaged and that value will be used at the appropriate minute in the hour. If "No" is entered, then the value that occurs on the appropriate minute in the hour will be used as the schedule value.

For example, if "yes" is entered and the minutes per item is 15 minutes (say a value of 0 for the first 15 minutes, then .5 for the second 15 minutes) AND there is a 10 minute timestep for the simulation: the value at 10 minutes will be 0 and the value at 20 minutes will be .25. For the same input entries but "no" for this field, the value at 10 minutes will be 0 and the value at 20 minutes will be .5.

#### Field: Minutes Per Item

This field represents the number of minutes for each item – in this case each line of the file. The value here must be <= 60 and evenly divisible into 60 (same as the timestep limits).

Here is an IDF example:

~~~~~~~~~~~~~~~~~~~~

    Schedule:File,
       elecTDVfromCZ01res,   !- Name
       Any Number,           !- ScheduleType
       TDV_kBtu_CTZ01.csv,   !- Name of File
       2,                    !- Column Number
       4,                    !- Rows to Skip at Top
       8760,                 !- Number of Hours of Data
       Comma;                !- Column Separator
~~~~~~~~~~~~~~~~~~~~

or with a relational path:

~~~~~~~~~~~~~~~~~~~~

    Schedule:File,
       elecTDVfromCZ01res,   !- Name
       Any Number,           !- ScheduleType
       DataSets\TDV\TDV_kBtu_CTZ01.csv,   !- Name of File
       2,                    !- Column Number
       4;                    !- Rows to Skip at Top
~~~~~~~~~~~~~~~~~~~~

A sub-hourly indication. Note that this is identical to an hourly file because there are 60 minutes per item – the number of hours defaults to 8760 and the column separator defaults to a comma. If the number of minutes per item had been, say, 15, then the file would need to contain 8760\*4 or 35,040 rows for this item.

~~~~~~~~~~~~~~~~~~~~

      Schedule:File,
        elecTDVfromCZ06com,      !- Name
        Any Number,              !- Schedule Type Limits Name
        DataSets\TDV\TDV_2008_kBtu_CTZ06.csv,  !- File Name
        1,                       !- Column Number
        4,                       !- Rows to Skip at Top
        ,                        !- Number of Hours of Data
        ,                        !- Column Separator
        ,                        !- Interpolate to Timestep
        60;                      !- Minutes per Item
~~~~~~~~~~~~~~~~~~~~

### Outputs

An optional report can be used to gain the values described in the previous Schedule objects. This is a condensed reporting that illustrates the full range of schedule values – in the style of input: DaySchedule, WeekSchedule, Annual Schedule.

~~~~~~~~~~~~~~~~~~~~

    ! will give them on hourly increments (day schedule resolution)
    Output:Reports, Schedules, Hourly;
    ! will give them at the timestep of the simulation
    Output:Reports, Schedules, Timestep;
~~~~~~~~~~~~~~~~~~~~

This report is placed on the eplusout.eio file. Details of this reporting are shown in the Output Details and Examples document.

#### Schedule Value Output

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Schedule Value []
~~~~~~~~~~~~~~~~~~~~

#### Schedule Value

This is the schedule value (as given to whatever entity that uses it). It has no units in this context because values may be many different units (i.e. temperatures, fractions, watts). For best results, you may want to apply the schedule name when you use this output variable to avoid output proliferation. For example, the following reporting should yield the values shown above, depending on day of week and day type:

~~~~~~~~~~~~~~~~~~~~

    Output:Variable,People_Shopping_Sch,Schedule Value,hourly;
    Output:Variable,Activity_Shopping_Sch,Schedule Value,hourly;
~~~~~~~~~~~~~~~~~~~~