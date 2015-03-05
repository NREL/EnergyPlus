# Output:Table:TimeBins

The Output:Table:TimeBins report shows the amount of time in hours that occurs in different bins for the single specific variable or meter specified. Two different types of binning occur: by month and by hour of the day.

The IntervalStart, IntervalSize and IntervalCount determine the size of the bins for many of the types of reports that can be generated. The IntervalStart value is the lowest value of the first bin. The first bin is for all values greater or equal to IntervalStart and less than IntervalStart+IntervalSize. This gets repeated IntervalCount times. A bin for below the first interval and above the last interval is automatically created. Each table also has totals for the column and the rows. The report produced by Report:Table:TimeBins also indicates the total time simulated (for an annual simulation this would be 8760 hours).

NOTE:  For summed variables, such as meters, the values are divided by the length of the timestep in seconds before binning. For example, the bins for a variable in Joules are reported in Joules per second (Watts). The interval values must then be entered accordingly.

## Inputs

#### Field: Key Value

The Key Value field indicates which instance of a variable is reported. An asterisk indicates that a table would be produced for every key.

#### Field: Variable Name

The variable name to be reported. Reference the eplusout.rdd file for names that are specific to your simulation input file. You do not need to include the units [ ] field in your names here.

#### Field: Interval Start

The lower value of the first bin. The first bin is for all values greater or equal to the IntervalStart and less than IntervalStart + IntervalSize. See note above regarding summed variables.

#### Field: Interval Size

The amount by which each bin range is to increase, i.e. the difference between the upper and lower limits of each bin. See note above regarding summed variables.

#### Field: Interval Count

The number of bins desired.

#### Field: Schedule Name

Optional schedule name. If left blank, binning is performed for all hours simulated. If a schedule is specified, binning is performed for non-zero hours in the schedule.

#### Field: Variable Type

This field is used to indicate the kind of units that may be associated with the variable. It is used by IDF Editor to display the appropriate SI and IP units for the Interval Start and Interval Size values. The available options are shown below.

- Energy 
- Temperature
- Flowrate
- Demand

An example of a time bins input object follows.

~~~~~~~~~~~~~~~~~~~~

    Output:Table:TimeBins,
      *,                     ! Key Value
      Electricity:HVAC,      ! Variable Name
      4000000,               ! Interval Start
      500000,                ! Interval Size
      20;                    ! Interval Count
~~~~~~~~~~~~~~~~~~~~