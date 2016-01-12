Standard Output Reports
=======================

The following objects allow standard reports to be defined and utilized in EnergyPlus:

- Output:Table:TimeBins

- Output:Table:Monthly

- Output:Table:Annual

- Output:Table:SummaryReports

- OutputControl:Table:Style

No **Output:Meter** or **Output:Variable** objects need to be specified to use the standard reports. A good set of example reports are available in the StandardReports.idf file in the DataSets directory of EnergyPlus.

Output:Table:TimeBins
---------------------

The Output:Table:TimeBins report shows the amount of time in hours that occurs in different bins for the single specific variable or meter specified. Two different types of binning occur: by month and by hour of the day.

The IntervalStart, IntervalSize and IntervalCount determine the size of the bins for many of the types of reports that can be generated. The IntervalStart value is the lowest value of the first bin. The first bin is for all values greater or equal to IntervalStart and less than IntervalStart+IntervalSize. This gets repeated IntervalCount times. A bin for below the first interval and above the last interval is automatically created. Each table also has totals for the column and the rows. The report produced by Report:Table:TimeBins also indicates the total time simulated (for an annual simulation this would be 8760 hours).

NOTE:  For summed variables, such as meters, the values are divided by the length of the timestep in seconds before binning. For example, the bins for a variable in Joules are reported in Joules per second (Watts). The interval values must then be entered accordingly.

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

```idf
Output:Table:TimeBins,
  *,                     ! Key Value
  Electricity:HVAC,      ! Variable Name
  4000000,               ! Interval Start
  500000,                ! Interval Size
  20;                    ! Interval Count
```

Output:Table:Monthly
--------------------

The Output:Table:Monthly object provides a generic method of setting up tables of monthly results. These tables are good for creating reports that broadly summarize the building performance. At the bottom of the tables, three rows are added for the sum or average depending on the type of variable, the maximum and the minimum of each column. The report produced has multiple columns that are each defined using a repeated group of fields. The fields for the variable name and the aggregation type can be repeated for any number of columns. A single Output:Table:Monthly object often produces multiple tables in the output. A table is produced for every “key” for a particular output variable. A single table of results shows values only for a specific “key”. The exception to this are environment variables, such as outdoor air temperature, that are included in each table, no matter what the key.

#### Field: Name

The Name field allows the user to give the output table a label that is shown in the output file.

#### Field: Digits After Decimal

The Digits After Decimal field is a numeric value that indicates how many digits after the decimal point should be shown on the table of values.

#### Field Set: Variable or Meter and Aggregation Type

The VariableOrMeterName field and the AggregationType field may be repeated for each column desired in the monthly report. These two fields define each column of the report. The IDD object has been set up to allow up to 25 columns. This object is extensible, so the number of columns may be increased by editing the IDD file and adding the desired number of fields at the end of the object.

#### Field: Variable or Meter &lt;\#&gt; Name

The remainder of this input object consists of pairs of fields that can be repeatedVariable Or Meter Name fields contain the name of a variable (see Output:Variable and eplusout.rdd), meter (see Output:Meter and eplusout.mdd), or schedule. This value is shown on a monthly basis using the aggregation method specified. If the selected name is used for both a variable or a meter and a schedule name, the variable or meter will be used.

#### Field: Aggregation Type for Variable or Meter &lt;\#&gt;

The aggregation type determines how the variable is aggregated in the table. The choices for aggregation type are described below:

Simple Aggregation Types

**SumOrAverage** – For “sum” type variables adds up the values for each timestep and reports the sum of the value monthly. For “average” type variables, the value shown will be the average for the month. SumOrAverage is probably the most common choice for aggregation type.

**Maximum** – Reports the maximum value and the time that the maximum value occurred. When the maximum option is used with a summed variable the value is divided by the length of the timestep in seconds. For example, the maximum for a variable in Joules is reported in Watts.

**Minimum** – Reports the minimum value and the time that the minimum value occurred. When the minimum option is used with a summed variable the value is divided by the length of the timestep in seconds. For example, the minimum for a variable in Joules is reported in Watts.

**HoursNonZero** – The HoursNonZero option adds up the elapsed time during the month that this variable is non-zero and would be appropriate to determine the number of hour that a fan operates.

**HoursZero** – The HoursZero option adds up the elapsed time during the month that this variable has a zero value and would be appropriate to determine the number of hour that a fan does not operate.

**HoursPositive** – The HoursPositive option adds up the elapsed time during the month that this variable has a positive value. Hours with a zero value are not included.

**HoursNonPositive** – The HoursNonPositive option adds up the elapsed time during the month that this variable has non-positive value. Hours with a negative value and hours with a zero value are all included.

**HoursNegative** – The HoursNegative option adds up the elapsed time during the month that this variable has a negative value. Hours with a zero value are not included.

**HoursNonNegative** – The HoursNonNegative option adds up the elapsed time during the month that this variable has non-negative value. Hours with a positive value and hours with a zero value are all included.

*Advanced Aggregation Types*

The advanced aggregation types are described below. These aggregation types rely on doing an operation on the current variable but only during the time or times defined by a previously defined field. The ValueWhenMaxOrMin displays the value of one variable at the same time as the maximum or minimum is set on a previous field that is defined as either a maximum or minimum. The “Hours—“ aggregation types display then number of hours that a variable meets a condition. The “—DuringHoursShown” aggregation types perform those aggretations but instead of for all the hours in the month, only for the hours that the previous “Hours—“ entry applies. Multiple “—DuringHoursShown” after an “Hours—“ will all be based on that single “Hours—“ entry, in fact the “—DuringHoursShown” is based on the next column to the left that contains an “Hours—“ entry even if other types of aggregations are used in intermediate columns. Order of the variables in the report is important when using the advanced aggregation types since they often depend on a previous entry.

**ValueWhenMaximumOrMinimum** – The ValueWhenMaximumOrMinimum option looks at the previous variable in the report that sets a maximum or minimum and displays the value of the current variable at that same timestep. Order of the variables in the report is important when using ValueWhenMaxMin. This can be used, for example, when an outdoor temperature should be reported for the time of the maximum cooling load.

**SumOrAverageDuringHoursShown** – Provides the sum or average of the named variable when during the hours that the previous variable displayed with any of the aggregation types starting with “Hours”. For “sum” type variables adds up the values for each timestep and reports the sum of the value monthly during the hours reported on the previous variable. For “average” type variables, the value shown will be the average for the month during the hours reported on the previous variable. Order of the variables in the report is important when using this aggregation type.

**MaximumDuringHoursShown** – Reports the maximum value and the time that the maximum value occurred but only during the hours reported with the previous “hours-“ aggregation type. When the maximum option is used with a summed variable the value is divided by the length of the timestep in seconds. For example, the maximum for a variable in Joules is reported in Watts. Order of the variables in the report is important when using this aggregation type.

**MinimumDuringHoursShown** - Reports the minimum value and the time that the minimum value occurred but only during the hours reported with the previous “hours-“ aggregation type. When the minimum option is used with a summed variable the value is divided by the length of the timestep in seconds. For example, the minimum for a variable in Joules is reported in Watts. Order of the variables in the report is important when using this aggregation type.

An example of this object follows.

```idf
Output:Table:Monthly,
  Building Monthly Cooling Load Report,               ! Name
  3,                                                  ! Digits After Decimal
  Zone Air System Sensible Cooling Energy,                   ! Variable or Meter 1 Name
  SumOrAverage,                                       ! Aggregation Type for Variable or Meter 1
  Zone Air System Sensible Cooling Energy,                   ! Variable or Meter 2 Name
  Maximum,                                            ! Aggregation Type for Variable or Meter 2
  Site Outdoor Air Drybulb Temperature,                                   ! Variable or Meter 3 Name
  ValueWhenMaxMin;                                    ! Aggregation Type for Variable or Meter 3
```

Output:Table:Annual
-------------------

The Output:Table:Annual object provides a generic method of setting up tables of annual results with each row corresponding to an instance of an object. These tables are good for creating reports that broadly summarize the building performance. At the bottom of the tables, three rows are added for the sum or average depending on the type of variable, the maximum and the minimum of each column. The report produced has multiple columns that are each defined using a repeated group of fields. The fields for the variable name and the aggregation type can be repeated for any number of columns. If the variables from different type of objects are specified in a single Output:Table:Annual, all the object names for each type of objects will be shown as rows. Variables that don't apply for certain objects will be left blank for that cell.  

#### Field: Name

The Name field allows the user to give the output table a label that is shown in the output file.

#### Field: Filter

An optional text string that is compared to the names of the objects referenced by the variables and if they match are included in the table. A footnote will appear that indicates that the objects shown may not be all the objects that of that type that occur in the file. 

#### Field: Schedule Name

Optional schedule name. If left blank, aggregation is performed for all hours simulated. If a schedule is specified, aggregation is performed for non-zero hours in the schedule.

#### Field Set: Variable and Aggregation Type

The Variable Name field and the Aggregation Type field may be repeated for each column desired in the report. These two fields define each column of the report. The IDD object has been set up to allow up to 25 columns. This object is extensible, so the number of columns may be increased by editing the IDD file and adding the desired number of fields at the end of the object.

#### Field: Variable or Meter &lt;\#&gt; Name

The remainder of this input object consists of pairs of fields that can be repeatedVariable Or Meter Name fields contain the name of a variable (see Output:Variable and eplusout.rdd), meter (see Output:Meter and eplusout.mdd), or schedule. This value is shown using the aggregation method specified. If the selected name is used for both a variable or a meter and a schedule name, the variable or meter will be used.

#### Field: Aggregation Type for Variable or Meter &lt;\#&gt;

The aggregation type determines how the variable is aggregated in the table. The choices for aggregation type are described below:

*Simple Aggregation Types*

**SumOrAverage** – For “sum” type variables adds up the values for each timestep and reports the sum of the value. For “average” type variables, the value shown will be the average for the month. SumOrAverage is probably the most common choice for aggregation type.

**Maximum** – Reports the maximum value and the time that the maximum value occurred. When the maximum option is used with a summed variable the value is divided by the length of the timestep in seconds. For example, the maximum for a variable in Joules is reported in Watts.

**Minimum** – Reports the minimum value and the time that the minimum value occurred. When the minimum option is used with a summed variable the value is divided by the length of the timestep in seconds. For example, the minimum for a variable in Joules is reported in Watts.

**HoursNonZero** – The HoursNonZero option adds up the elapsed time during the month that this variable is non-zero and would be appropriate to determine the number of hour that a fan operates.

**HoursZero** – The HoursZero option adds up the elapsed time during the month that this variable has a zero value and would be appropriate to determine the number of hour that a fan does not operate.

**HoursPositive** – The HoursPositive option adds up the elapsed time during the month that this variable has a positive value. Hours with a zero value are not included.

**HoursNonPositive** – The HoursNonPositive option adds up the elapsed time during the month that this variable has non-positive value. Hours with a negative value and hours with a zero value are all included.

**HoursNegative** – The HoursNegative option adds up the elapsed time during the month that this variable has a negative value. Hours with a zero value are not included.

**HoursNonNegative** – The HoursNonNegative option adds up the elapsed time during the month that this variable has non-negative value. Hours with a positive value and hours with a zero value are all included.

**HourInTenBinsMinToMax** - Creates 10 columns for the specified variable and shows the number of hours in each of 10 bins based on the minimum and maximum value. The bin sizes will be rounded up to the next most signficant digit value (if the min is 0 and the max is 28786.3, the bin size would be 3000 not 2878.63). A table of the bin ranges would be generated below the main table when this option is selected.

**HourInTenBinsZeroToMax** - Creates 11 columns for the specified variable and shows the number of hours in each of 10 bins from zero to the maximum value and a bin for hours below zero. The bin sizes will be rounded up to the next most significant digit value (if the min is 0 and the max is 28786.3, the bin size would be 3000 not 2878.63). A table of the bin ranges would be generated below the main table when this option is selected. 

**HourInTenBinsMinToZero** - Creates 11 columns for the specified variable and shows the number of hours in each of 10 bins from zero to the minimum value and a bin for hours above zero. The bin sizes will be rounded up to the next most significant digit value (if the min is 0 and the min is -28786.3, the bin size would be 3000 not 2878.63). A table of the bin ranges would be generated below the main table when this option is selected.

*Advanced Aggregation Types*

The advanced aggregation types are described below. These aggregation types rely on doing an operation on the current variable but only during the time or times defined by a previously defined field. The ValueWhenMaxOrMin displays the value of one variable at the same time as the maximum or minimum is set on a previous field that is defined as either a maximum or minimum. The “Hours—“ aggregation types (not including binning) display then number of hours that a variable meets a condition. The “—DuringHoursShown” aggregation types perform those aggregations but instead of for all the hours in the month, only for the hours that the previous “Hours—“ entry applies. Multiple “—DuringHoursShown” after an “Hours—“ will all be based on that single “Hours—“ entry, in fact the “—DuringHoursShown” is based on the next column to the left that contains an “Hours—“ entry even if other types of aggregations are used in intermediate columns. Order of the variables in the report is important when using the advanced aggregation types since they often depend on a previous entry.

**ValueWhenMaximumOrMinimum** – The ValueWhenMaximumOrMinimum option looks at the previous variable in the report that sets a maximum or minimum and displays the value of the current variable at that same timestep. Order of the variables in the report is important when using ValueWhenMaxMin. This can be used, for example, when an outdoor temperature should be reported for the time of the maximum cooling load.

**SumOrAverageDuringHoursShown** – Provides the sum or average of the named variable when during the hours that the previous variable displayed with any of the aggregation types starting with “Hours”. For “sum” type variables adds up the values for each timestep and reports the sum of the value during the hours reported on the previous variable. For “average” type variables, the value shown will be the average for the month during the hours reported on the previous variable. Order of the variables in the report is important when using this aggregation type.

**MaximumDuringHoursShown** – Reports the maximum value and the time that the maximum value occurred but only during the hours reported with the previous “hours-“ aggregation type. When the maximum option is used with a summed variable the value is divided by the length of the timestep in seconds. For example, the maximum for a variable in Joules is reported in Watts. Order of the variables in the report is important when using this aggregation type.

**MinimumDuringHoursShown** - Reports the minimum value and the time that the minimum value occurred but only during the hours reported with the previous “hours-“ aggregation type. When the minimum option is used with a summed variable the value is divided by the length of the timestep in seconds. For example, the minimum for a variable in Joules is reported in Watts. Order of the variables in the report is important when using this aggregation type.


#### Field: Digits After Decimal for Variable or Meter &lt;\#&gt;

The Digits After Decimal field is a numeric value that indicates how many digits after the decimal point should be shown for that column.

An example of this object follows.

```idf
    Output:Table:Annual,
    Custom Annual Window Report,  !- Name
    , !- Report Group Name
    , !- Filter
    , !- Schedule Name
    SURFACE WINDOW TRANSMITTED SOLAR RADIATION RATE,  !- Variable, Meter, EMS Int Var, IDF field Name 1
    SumOrAverage,  !- Aggregation Type for 1
    2,  !- Digits After Decimal for 1
    SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION RATE,  !- Variable, Meter, EMS Int Var, IDF field Name 2
    SumOrAverage,  !- Aggregation Type for 2
    2,  !- Digits After Decimal for 2
    SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION RATE,  !- Variable, Meter, EMS Int Var, IDF field Name 3
    SumOrAverage,  !- Aggregation Type for 3
    2,  !- Digits After Decimal for 3
    SURFACE WINDOW HEAT GAIN RATE,  !- Variable, Meter, EMS Int Var, IDF field Name 4
    SumOrAverage,  !- Aggregation Type for 4
    2,  !- Digits After Decimal for 4
    ACE WINDOW HEAT LOSS RATE,  !- Variable, Meter, EMS Int Var, IDF field Name 5
    SumOrAverage,  !- Aggregation Type for 5
    2;  !- Digits After Decimal for 5
```



Output:Table:SummaryReports
---------------------------

The Output:Table:SummaryReports object controls which predefined tabular reports are produced.  The easiest option is to specify “AllSummary” which will produce all of the summary reports described in this section and does not include the Zone Component Load report. In addition, “AllMonthly” will produce all of the monthly reports described. If all predefined reports are needed. “AllSummaryAndMonthly” shows all of the summary and monthly predefined reports and does not include the Zone Component Load report. The “AllSummaryAndSizingPeriod” option and “AllSummaryMonthlyAndSizingPeriod” option are similar and add the Zone Component Load report. Including the Zone Component Load Summary report may increase the simulation run time.

#### Field: Report &lt;\#&gt; Name

All of the fields in the Output:Table:SummaryReports are the same. A long list of predefined summary reports is available by entering one of the available key choices described below. Each one indicates the name of the predefined reports that should be output. The input AllSummary will cause all the reports described below to be created.  The Report &lt;\#&gt; Name field can be repeated.

### Predefined Annual Summary Reports

#### Annual Building Utility Performance Summary (sometimes called ABUPS)

The Annual Building Utility Performance Summary report often called ABUPS – (key: AnnualBuildingUtilityPerformanceSummary) produces a report that is an overall summary of the utility consumption of the building. It contains a number of subtables that are each described below.

- Site and Source Energy – Indicates the total site and source energy use. For electricity the net electricity from the utility is used for the electric contribution. The site to source conversion factors are based on those entered by the user. These are entered in the EnvironmentalImpactFactors object and FuelFactors objects.

- Building Area – Shows the total floorspace of the building and the conditioned floorspace.

- End Uses – This shows the total use of electricity, natural gas, other fuels, purchased cooling, purchased heating and water for each major end-use category. The end-use categories are Heating, Cooling, Interior Lighting, Exterior Lighting, Interior Equipment, Exterior Equipment, Fans, Pumps, Heat Rejection, Humidification, Heat Recovery, Hot Water, Refrigeration, and Generators. Not all fuels have corresponding end uses. The values in this sub-table are from output meters. To determine which components are attached to each end-use meter, consult the meter details output file (\*.mtd). The source of the resource does not affect this table – the amount of electricity used for lights does not change if the electricity is from the utility or from an on-site generator. The Other Fuel column includes fuel oil\#1, fuel oil\#2, gasoline, coal, propane, diesel, otherfuel1 and otherfuel2. The district heating column also  includes steam.

- End Uses By Subcategory – Shows a breakdown of the major end uses by user-defined category. If an end-use subcategory was not input for an object, it is automatically added to the General subcategory for that major end-use category.

- Utility Use Per Floor Area – These two sub-tables show the results from the end-uses table divided by the total floor area defined for the building and for the total conditioned floor area. Only three categories for end-uses are used for these sub-tables, lighting, HVAC and other. HVAC includes fans, pumps, heating, cooling, heat rejection, humidification, and domestic hot water heating. The Other Fuel column includes fuel oil\#1, fuel oil\#2, gasoline, coal, propane, diesel, otherfuel1 and otherfuel2. The district heating column also  includes steam.

- Electric Loads Satisfied – Shows the different methods that electric loads are satisfied in the building. The values shown for on site power generation are: Fuel-Fired Power Generation, High Temperature Geothermal, Photovoltaic Power, and Wind Power. The flows to and from the electric utility are shown next and finally the total electricity used at the site is compared to the total generated on site plus the net amount from the electric utility. The percentages shown are based on the total electricity used by the end-uses. Note that High Temperature Geothermal and Wind Power are not yet implemented in EnergyPlus.

- On-Site Thermal Sources – Shows the on-site thermal sources of energy such as Water-Side Heat Recovery, Air to Air Heat Recovery for Cooling, Air to Air Heat Recovery for Heating, High-Temperature Geothermal, Solar Water Thermal, Solar Air Thermal. Note that High-Temperature Geothermal Solar Water Thermal, and Solar Air Thermal are not yet implemented in EnergyPlus.

- Water Loads Summary – Shows the different methods the water loads were satisfied. This table shows all zeros because water use is not yet implemented in EnergyPlus.

#### Input Verification and Results Summary (or IVRS)

The Input Verification and Results Summary report (key: InputVerificationandResultsSummary) produces a report with several tables including:

- General which includes general information like the Program Version and Build, Weather, Latitude, Longitude, Elevation, Time Zone, North Axis Angle, and Hours Simulated.

- Window-Wall Ratio table for envelope which includes the wall area, the window area and the ratio of the two. These are computed for all walls and for walls that are oriented generally north, south, east and west. All walls are categorized into one of these four cardinal directions. This is computed for walls that have a tilt of 60 to 120 degrees.

- Skylight-Roof Ratio table for envelope which includes the roof area and the skylight area and the ratio of the two. This includes all surfaces with a tilt of less than 60 degrees.

- Zone Summary includes internal load summary for each zone including area, if conditioned, volume, multipliers, gross wall area, window area, design lighting, design people, design plug and process.

#### Source Energy End Use Components Summary

The Source Energy End Use Components Summary report produces a report (key: SourceEnergyEndUseComponentsSummary) that includes three tables. These tables display source energy by fuel type that is calculated based on site to source energy factors specified by the user in the EnvironmentalImpactFactors and FuelFactors objects. The last two tables display the source energy in terms of area normalized metrics. Following is a description of each table:

- Source Energy End Use Components – This shows the total use of source electricity, source natural gas, source values of other fuels, source purchased cooling   and purchased heating for each major end-use category. The end-use categories are Heating, Cooling, Interior Lighting, Exterior Lighting, Interior Equipment, Exterior Equipment, Fans, Pumps, Heat Rejection, Humidification, Heat Recovery, Hot Water, Refrigeration, and Generators. Not all fuels have corresponding end uses. The values in this sub-table are from output meters. To determine which components are attached to each end-use meter, consult the meter details output file (\*.mtd). The source of the resource will affect this table – the amount of electricity used for lights will change if the electricity is from the utility or from an on-site generator. The Other Fuel column includes FuelOil\#1, FuelOil\#2, Gasoline, Coal, Propane, Diesel, OtherFuel1 and OtherFuel2. The district heating column also  includes steam.

- Source Energy End Use Components normalized by Conditioned Floor Area – This table shows the total end uses in source energy normalized by conditioned floor area.

- Source Energy End Use Components normalized by Total Floor Area – This table shows the total end uses in source energy normalized by total floor area.

#### Climatic Data Summary

The Climate Summary or Climatic Data Summary report (key: ClimaticDataSummary) produces a report that includes some of the details on each of the design days including: maximum dry-bulb, daily temperature range, humidity value, humidity type, wind speed, and wind direction.

#### Envelope Summary

The Envelope Summary report (key: EnvelopeSummary) produces a report that includes the following tables:

- Opaque which includes all opaque surfaces and includes the name of the construction, reflectance, U-Factor, gross area, azimuth, tilt, cardinal direction.

- Fenestration which includes all non-opaque surfaces and includes the name of the construction, areas (glass, frame, divider, single opening, multiplied openings), U-Factor, SHGC (the solar heat gain coefficient based on summer conditions), visible transmittance, conductance (frame, divider), indication of shade control, the name of the parent surface, azimuth, tilt, cardinal direction.

#### Surface Shadowing Summary

The Surface Shadowing Summary report (key: SurfaceShadowingSummary) produces a report that includes two tables. Note that surfaces starting with “Mir-“ are automatically generated by EnergyPlus and are the mirror images of user entered surfaces.

- Surfaces (Walls, Roofs, etc) that may be Shadowed by Other Surfaces and includes the name of the surface and a list of surfaces that may possibly cast shadows on that named surface. The list of possible shadow casters does not necessarily mean that they do cast shadows during the simulation, only that their relative position makes it possible that shadows from a surface in the list may fall on the named surface.

- Subsurfaces (Windows and Doors) that may be Shadowed by Surfaces, includes the name of the subsurface such as a window or a door and a corresponding list of surfaces that may be casting shadows on the windows and doors.

#### Shading Summary

The Shading Summary report (key: ShadingSummary) produces a report that includes the following tables:

- Sunlit Fraction which shows a list of windows and the fraction of the window that is sunlit for nine specific times of the year. The nine specific times include 9am, noon and 3pm on March 21, June 21, and December 21. These nine times were chosen to represent the range of sun angles. The simulation must include those times for the value to be included in the report.

- Window control includes the names of all windows that have a window shading control (see WindowProperty:ShadingControl) and includes the name of the control, the type of shading, the shaded construction, the kind of control, and if glare control is used.

#### Lighting Summary

The Lighting Summary report (key: LightingSummary) produces a report that includes the following tables:

- Interior Lighting which includes the name of the lights object, the zone that it is used in, the lighting power density, zone area, total power, end use subcategory, schedule name, average hours per week, return air fraction, and whether the zone is conditioned.

- Daylighting which includes the names of the daylighting objects, the zone they are used in, the type of daylighting being used, the control type, the fraction of the lighting controlled, the total power of the lighting installed in the zone and the lighting power that is controlled by the daylighting object.

- Exterior Lighting which includes the name of the ExteriorLights object, the total watts described by the object, if the ExteriorLights uses an astronomical clock or just a schedule, the name of the schedule used, and the average hours per week for the named schedule for the year. The effect of the astronomical clock does not get included in the averaged hours per week shown.

#### Equipment Summary

The Equipment Summary report (key: EquipmentSummary) produces a report that includes some details on the major HVAC equipment present. The report has seven parts.

- Central Plant includes details on chillers, boilers, and cooling towers including the capacity and efficiency. For Chiller:Electric:EIR and Chiller:Electric:ReformulatedEIR, IPLV at AHRI standard test conditions is reported.

- Cooling Coils includes the nominal total, sensible and latent capacities, the nominal sensible heat ratio, the nominal efficiency, nominal UA value, and nominal  surface area for each cooling coil. These values are calculated by calling the cooling coil simulation routine with the rated inlet conditions: inlet air dry bulb temperature = 26.67C, inlet air wet bulb temperature = 19.44C, inlet chilled water temperature = 6.67C.

- DX Cooling Coils summarizes the Standard Rating (Net) Cooling Capacity, SEER, EER and IEER values at AHRI standard test. Currently, these values are only reported for coil type = Coil:Cooling:DX:SingleSpeed with condenser type = AirCooled.

- DX Heating Coils summarizes the High Temperature Heating Standard (Net) Rating Capacity, Low Temperature Heating Standard (Net) Rating Capacity and Heating Seasonal Performance Factor (HSPF) values at AHRI standard test. Currently, these values are only reported for coil type = Coil:Heating:DX:SingleSpeed.

- Heating Coils includes the nominal capacity and efficiency for each heating coil. The capacity is calculated by calling the heating coil simulation routine at the rated inlet conditions: inlet air dry bulb temperature = 16.6C, inlet relative humidity = 50%, inlet hot water temperature = 82.2C.

- Fan includes the type of fan, the total efficiency, delta pressure, max flow rate, motor heat in air fraction, and end use.

- Pumps includes the type of pump, control type, head pressure, electric power, and motor efficiency for each pump.

- Service Water Heating includes the type of water heater, the storage volume, input, thermal efficiency, recovery efficiency, and energy factor.

#### HVAC Sizing Summary

The HVAC Sizing Summary report (key: HVACSizingSummary) produces a report that includes the following tables:

- Zone Cooling which includes the following columns for each zone: the calculated design load, the user specified design load, the calculated design air flow, the user specified design air flow, the name of the sizing period, the time of the peak load during the sizing period, the temperature at the time of the peak load during the sizing period, and the humidity ratio at the time of the peak load during the sizing period used.

- Zone Heating which includes the following columns for each zone: the calculated design load, the user specified design load, the calculated design air flow, the user specified design air flow, the name of the sizing period, the time of the peak load during the sizing period, the temperature at the time of the peak load during the sizing period, and the humidity ratio at the time of the peak load during the sizing period used.

- System Design Air Flow Rates which includes the following columns for each air loop: the calculated cooling air flow rate, the user specified air flow rate for cooling, the calculated heating air flow rate, the user specified air flow rate for heating.

- *Note:* values listed as "calculated" are the unaltered result of the zone or system sizing calculations, using the design sizing period weather and schedules specified in the input. Values listed as "user specified" are either the calculated values modified by global or zone sizing factors or values specified with the *flow/zone* or *flow/system* design air flow method.

#### Component Sizing Summary

The Component Sizing Summary report (key: ComponentSizingSummary) produces a report that includes different tables depending on the kinds of HVAC components that exist in the input file. A table is shown for each type of HVAC component that is sized. The table lists the objects of that type of component that appear in the input file and one or more parameters related to that component.  For example, the AirTerminal:SingleDuct:VAV:Reheat component creates a table showing the maximum air flow rate from the sizing calculations and the maximum reheat water flow rate. Another example is the Fan:VariableVolume object which shows a table with both the maximum and minimum flow rates for each fan based on the results from the sizing calculations.

#### Outdoor Air Summary

The Outdoor Air Summary report (key: OutdoorAirSummary) produces a report that includes the following tables:

- Average Outside Air During Occupied Hours table shows for each zone the average and nominal number of occupants, the zone volume, the average air change rate based on mechanical ventilation, infiltration and simple ventilation during occupied hours.

- Minimum Outside Air During Occupied Hours table shows for each zone the average and nominal number of occupants, the zone volume, the minimum air change rate based on mechanical ventilation, infiltration and simple ventilation during occupied hours.

#### System Summary

The System Summary Report (key: SystemSummary) produces a report that includes the following tables:

- Economizer which includes the following columns for each Controller:OutdoorAir object: the high limit shutoff control, the minimum outdoor air flow, the maximum outdoor air flow, if the return air temperature has a control limit, if the return air has an enthalpy limit, the outdoor air temperature limit, and the outdoor air enthalpy limit.

- Demand Controlled Ventilation table is for each Controller:MechanicalVentilation object and shows the name, the nominal outdoor air per person and the nominal outdoor air per zone area.

- Time Not Comfortable Based on Simple ASHRAE 55-2004 table shows how many hours that the space is not comfortable for each zone under the criteria of assuming winter clothes, summer clothes or both summer and winter clothes.  See the People object for more information about this thermal comfort calculation.

- Time Setpoint is Not Met table shows how many hours the space is more than 0.2C from the setpoint during heating and during cooling.  The last two columns indicate those hours that the setpoint is not met while the space is occupied.

#### Adaptive Comfort Summary

The Adaptive Comfort Summary report (key: AdaptiveComfortSummary) produces a report tabulating the sum of occupied hours not meeting adaptive comfort acceptability limits for each relevant People object (People objects for which adaptive comfort calculations are requested). These acceptability limits include ASHRAE Std. 55 80%, ASHRAE Std. 55 90%, CEN-15251 Category I, CEN-15251 Category II, and CEN-15251 Category III.

#### Sensible Heat Gain Summary

The Sensible Heat Gain Summary (key: SensibleHeatGainSummary) provides results for each zone and the overall building for some of the major heat gain components. The first four columns show the loads satisfied by sensible air heating and cooling as well as radiant heating and cooling surfaces in the zone. The heat gains from people, lighting, equipment, windows, interzone air flow, and infiltration are shown when adding heat to the zone and separately when removing heat from the zone (for applicable components). Finally the balance is shown as “Opaque Surface Conduction and Other Heat Addition” and “Opaque Surface Conduction and Other Heat Removal” which is a term indicating the affect of the walls, floors and ceilings/roof to the zone as well as the impact of the delay between heat gains/losses and loads on the HVAC equipment serving the zone.  The following shows each output variable that is used for each column. For each timestep in the simulation, positive values are shown as additions and negative values are shown as removal for most variables.

- HVAC Input Sensible Air Heating

Zone Air Heat Balance System Air Transfer Rate

Zone Air Heat Balance System Convective Heat Gain Rate

- HVAC Input Sensible Air Cooling

Zone Air Heat Balance System Air Transfer Rate

Zone Air Heat Balance System Convective Heat Gain Rate

- HVAC Input Heated Surface Heating

Zone Radiant HVAC Heating Energy

Zone Ventilated Slab Radiant Heating Energy

- HVAC Input Cooled Surface Cooling

Zone Radiant HVAC Cooling Energy

Zone Ventilated Slab Radiant Cooling Energy

- People Sensible Heat Addition

Zone People Sensible Heating Energy

- Lights Sensible Heat Addition

Zone Lights Total Heating Energy

- Equipment Sensible Heat Addition & Equipment Sensible Heat Removal

Zone Electric Equipment Radiant Heating Energy

Zone Gas Equipment Radiant Heating Energy

Zone Steam Equipment Radiant Heating Energy

Zone Hot Water Equipment Radiant Heating Energy

Zone Other Equipment Radiant Heating Energy

Zone Electric Equipment Convective Heating Energy

Zone Gas Equipment Convective Heating Energy

Zone Steam Equipment Convective Heating Energy

Zone Hot Water Equipment Convective Heating Energy

Zone Other Equipment Convective Heating Energy

- Window Heat Addition & Window Heat Removal

Zone Windows Total Heat Gain Energy

- Interzone Air Transfer Heat Addition & Interzone Air Transfer Heat Removal

Zone Air Heat Balance Interzone Air Transfer Rate

- Infiltration Heat Addition & Infiltration Heat Removal

Zone Air Heat Balance Outdoor Air Transfer Rate

The Opaque Surface Conduction and Other Heat Addition and Opaque Surface Conduction and Other Heat Removal columns are also calculated on an timestep basis as the negative value of the other removal and gain columns so that the total for the timestep sums to zero. These columns are derived strictly from the other columns.

#### Zone Component Load Summary

The Zone Component Loads Summary provides an estimate of the heating and cooling peak loads for each zone broken down into various components. This report may help determine which components of the load have the largest impact for the heating and cooling peak design conditions. When specified, the Zone Component Loads Summary report is created for each zone that is conditioned. The difference between the peak design sensible load and the estimated instant + delayed sensible load (as shown in the *Peak Conditions* subtable) is an indication of how consistent the overall total estimate may be to the computed total peak loads for the zone. When the report is called the zone sizing calculations are repeated twice so this may result in longer simulation times.  The key used to obtain this report is ZoneComponentLoadSummary. Since including this report may increase the simulation time, new key options have been added that will display all reports but the Zone Component Load Summary those keys used are AllSummaryButZoneComponentLoad and AllSummaryAndMonthlyButZoneComponentLoad.

The report has four parts:

- Estimated Cooling Peak Load Components

Contains the sensible-instant, sensible-delay, sensible-return air, latent, total and %grand total for people, lights, equipment, refrigeration, water use equipment, HVAC equipment loads, power generation equipment, infiltration, zone ventilation, interzone mixing, roof, interzone ceiling, other roof, exterior wall, interzone wall, ground contact wall, other wall, exterior floor, interzone floor, ground contact floor, other floor, fenestration conduction, fenestration solar, opaque door. The values in the sensible-delay column are estimated using a procedure shown in the Engineering Reference.

- Cooling Peak Conditions

Contains the time of the peak load and the outside dry bulb and wet bulb temperatures as well as the outside humidity ratio for that time. It also shows the zone temperature and relative humidity and humidity ratio for that time.

- Estimated Heating Peak Load Components

Contains the sensible-instant, sensible-delay, sensible-return air, latent, total and %grand total for people, lights, equipment, refrigeration, water use equipment, HVAC equipment loads, power generation equipment, infiltration, zone ventilation, interzone mixing, roof, interzone ceiling, other roof, exterior wall, interzone wall, ground contact wall, other wall, exterior floor, interzone floor, ground contact floor, other floor, fenestration conduction, fenestration solar, opaque door. The values in the sensible-day column are estimated using a procedure shown in the Engineering Reference.

- Heating Peak Conditions

Contains the time of the peak load and the outside dry bulb and wet bulb temperatures as well as the outside humidity ratio for that time. It also shows the zone temperature and the relative humidity and humidity ratio for that time.

#### Standard 62.1 Summary

The Standard 62.1 Summary (key: Standard62.1Summary) produces a report that is consistent with many of the outputs needed when doing calculations consistent with ASHRAE Standard 62.1-2010. The report is generated when sizing calculations are specified. The abbreviations used in the report are consistent with the abbreviations used in Appendix A4 of the Standard. The following tables are part of the report:

- System Ventilation Requirements for Cooling containing: Sum of Zone Primary Air Flow - Vpz-sum, System Population – Ps, Sum of Zone Population - Pz-sum, Occupant Diversity – D, Uncorrected Outdoor Air Intake Airflow – Vou, System Primary Airflow – Vps, Average Outdoor Air Fraction – Xs, System Ventilation Efficiency – Ev, Outdoor Air Intake Flow – Vot, Percent Outdoor Air - %OA.

- System Ventilation Requirements for Heating containing: Sum of Zone Primary Air Flow - Vpz-sum, System Population – Ps, Sum of Zone Population - Pz-sum, Occupant Diversity – D, Uncorrected Outdoor Air Intake Airflow – Vou, System Primary Airflow – Vps, Average Outdoor Air Fraction – Xs, System Ventilation Efficiency – Ev, Outdoor Air Intake Flow – Vot, Percent Outdoor Air - %OA.

- Zone Ventilation Parameters containing: AirLoop Name, People Outdoor Air Rate – Rp,  Zone Population – Pz, Area Outdoor Air Rate – Ra, Zone Floor Area – Az, Breathing Zone Outdoor Airflow – Vbz, Cooling Zone Air Distribution Effectiveness - Ez-clg, Cooling Zone Outdoor Airflow - Voz-clg, Heating Zone Air Distribution Effectiveness - Ez-htg, Heating Zone Outdoor Airflow - Voz-htg.

- System Ventilation Parameters containing: People Outdoor Air Rate – Rp, Sum of Zone Population - Pz-sum, Area Outdoor Air Rate – Ra, Sum of Zone Floor Area - Az-sum, Breathing Zone Outdoor Airflow – Vbz, Cooling Zone Outdoor Airflow - Voz-clg, Heating Zone Outdoor Airflow - Voz-htg.

- Zone Ventilation Calculations for Cooling Design containing: AirLoop Name, Box Type, Zone Primary Airflow – Vpz, Zone Discharge Airflow – Vdz, Minimum Zone Primary Airflow - Vpz-min, Zone Outdoor Airflow Cooling - Voz-clg, Primary Outdoor Air Fraction – Zpz, Primary Air Fraction – Ep, Secondary Recirculation Fraction- Er, Supply Air Fraction- Fa, Mixed Air Fraction – Fb, Outdoor Air Fraction – Fc, Zone Ventilation Efficiency – Evz.

- System Ventilation Calculations for Cooling Design containing: Sum of Zone Primary Airflow - Vpz-sum, System Primary Airflow – Vps, Sum of Zone Discharge Airflow - Vdz-sum, Minimum Zone Primary Airflow - Vpz-min, Zone Outdoor Airflow Cooling - Voz-clg, Zone Ventilation Efficiency - Evz-min.

- Zone Ventilation Calculations for Heating Design containing: AirLoop Name, Box Type, Zone Primary Airflow – Vpz, Zone Discharge Airflow – Vdz, Minimum Zone Primary Airflow - Vpz-min, Zone Outdoor Airflow Cooling - Voz-clg, Primary Outdoor Air Fraction – Zpz, Primary Air Fraction – Ep, Secondary Recirculation Fraction- Er, Supply Air Fraction- Fa, Mixed Air Fraction – Fb, Outdoor Air Fraction – Fc, Zone Ventilation Efficiency – Evz.

- System Ventilation Calculations for Heating Design containing: Sum of Zone Primary Airflow - Vpz-sum, System Primary Airflow – Vps, Sum of Zone Discharge Airflow - Vdz-sum, Minimum Zone Primary Airflow - Vpz-min, Zone Outdoor Airflow Cooling - Voz-clg, Zone Ventilation Efficiency - Evz-min.

#### Energy Meters Summary

The Energy Meters Summary (key: EnergyMeters) (which is a slight misnomer as some meters may not be strictly energy) provides the annual period (runperiod) results for each meter (reference the meter data dictionary file (.mdd) and/or the meter details file (.mtd). The results are broken out by fuel type (resource type) in this report.

#### LEED Summary

The LEED Summary report provides many of the simulation results required for certification of Energy and Atmosphere Credit 1 Optimized Energy Performance according to the LEED Green Building Rating System™. The report can be produced by specifying LEEDSummary in Output:Table:SummaryReports which is also part of the AllSummary option. Directly following is an example of this report.

#### Object Count Summary

The Object Count Summary provides the count on the number of specific objects in the file. The key used to obtain this report is ObjectCountSummary.

#### Component Cost Economics Summary

The Component Cost Economics Summary provides the construction cost estimate summary for the project. The costs are broken into eight catagories and the reference building costs are provided as a comparison. A second table is also produced that provides line item details with one line for every line item object. The key used to obtain this report is ComponentCostEconomicsSummary.

### Predefined Monthly Summary Reports

The predefined monthly report options are shown below. The key name of the predefined monthly report is all that is needed to have that report appear in the tabular output file. After each report name below are the output variables and aggregation types used. These cannot be modified when using the predefined reports but if changes are desired, a Output:Table:Monthly can be used instead. The StandardReports.idf file in the DataSets directory includes a Output:Table:Monthly that exactly corresponds to the predefined monthly reports shown below. They can be copied into an IDF file and extended if additional variables are desired. A listing of each available key for predefined monthly summary reports follows with a discrption of the variables included.

#### ZoneCoolingSummaryMonthly

- Zone Air System Sensible Cooling Energy (SumOrAverage)

- Zone Air System Sensible Cooling Rate (Maximum)

- Site Outdoor Air Drybulb Temperature (ValueWhenMaxMin)

- Site Outdoor Air Wetbulb Temperature (ValueWhenMaxMin)

- Zone Total Internal Latent Gain Energy (SumOrAverage)

- Zone Total Internal Latent Gain Energy (Maximum)

- Site Outdoor Air Drybulb Temperature (ValueWhenMaxMin)

- Site Outdoor Air Wetbulb Temperature (ValueWhenMaxMin)

#### ZoneHeatingSummaryMonthly

- Zone Air System Sensible Heating Energy (SumOrAverage)

- Zone Air System Sensible Heating Rate (Maximum)

- Site Outdoor Air Drybulb Temperature (ValueWhenMaxMin)

#### ZoneElectricSummaryMonthly

- Zone Lights Electric Energy (SumOrAverage)

- Zone Lights Electric Energy (Maximum)

- Zone Electric Equipment Electric Energy (SumOrAverage)

- Zone Electric Equipment Electric Energy (Maximum)

#### SpaceGainsMonthly

- Zone People Total Heating Energy (SumOrAverage)

- Zone Lights Total Heating Energy (SumOrAverage)

- Zone Electric Equipment Total Heating Energy (SumOrAverage)

- Zone Gas Equipment Total Heating Energy (SumOrAverage)

- Zone Hot Water Equipment Total Heating Energy (SumOrAverage)

- Zone Steam Equipment Total Heating Energy (SumOrAverage)

- Zone Other Equipment Total Heating Energy (SumOrAverage)

- Zone Infiltration Sensible Heat Gain Energy (SumOrAverage)

- Zone Infiltration Sensible Heat Loss Energy (SumOrAverage)

#### PeakSpaceGainsMonthly

- Zone People Total Heating Energy (Maximum)

- Zone Lights Total Heating Energy (Maximum)

- Zone Electric Equipment Total Heating Energy (Maximum)

- Zone Gas Equipment Total Heating Energy (Maximum)

- Zone Hot Water Equipment Total Heating Energy (Maximum)

- Zone Steam Equipment Total Heating Energy (Maximum)

- Zone Other Equipment Total Heating Energy (Maximum)

- Zone Infiltration Sensible Heat Gain Energy (Maximum)

- Zone Infiltration Sensible Heat Loss Energy (Maximum)

#### SpaceGainComponentsAtCoolingPeakMonthly

- Zone Air System Sensible Cooling Rate (Maximum)

- Zone People Total Heating Energy (ValueWhenMaxMin)

- Zone Lights Total Heating Energy (ValueWhenMaxMin)

- Zone Electric Equipment Total Heating Energy (ValueWhenMaxMin)

- Zone Gas Equipment Total Heating Energy (ValueWhenMaxMin)

- Zone Hot Water Equipment Total Heating Energy (ValueWhenMaxMin)

- Zone Steam Equipment Total Heating Energy (ValueWhenMaxMin)

- Zone Other Equipment Total Heating Energy (ValueWhenMaxMin)

- Zone Infiltration Sensible Heat Gain Energy (ValueWhenMaxMin)

- Zone Infiltration Sensible Heat Loss Energy (ValueWhenMaxMin)

#### EnergyConsumptionElectricityNaturalGasMonthly

- Electricity:Facility (SumOrAverage)

- Electricity:Facility (Maximum)

- Gas:Facility (SumOrAverage)

- Gas:Facility (Maximum)

#### EnergyConsumptionElectricityGeneratedPropaneMonthly

- ElectricityProduced:Facility (SumOrAverage)

- ElectricityProduced:Facility (Maximum)

- Propane:Facility (SumOrAverage)

- Propane:Facility (Maximum)

#### EnergyConsumptionDieselFuel OilMonthly

- Diesel:Facility (SumOrAverage)

- Diesel:Facility (Maximum)

- FuelOil\#1:Facility (SumOrAverage)

- FuelOil\#1:Facility (Maximum)

- FuelOil\#2:Facility (SumOrAverage)

- FuelOil\#2:Facility (Maximum)

#### EnergyConsumptionDisctrictHeatingCoolingMonthly

- DistrictCooling:Facility (SumOrAverage)

- DistrictCooling:Facility (Maximum)

- DistrictHeating:Facility (SumOrAverage)

- DistrictHeating:Facility (Maximum)

#### EnergyConsumptionCoalGasolineMonthly

- Coal:Facility (SumOrAverage)

- Coal:Facility (Maximum)

- Gasoline:Facility (SumOrAverage)

- Gasoline:Facility (Maximum)

#### EnergyConsumptionOtherFuelsMonthly

- OtherFuel1:Facility (SumOrAverage)

- OtherFuel1:Facility (Maximum)

- OtherFuel2:Facility (SumOrAverage)

- OtherFuel2:Facility (Maximum)

#### EndUseEnergyConsumptionElectricityMonthly

- InteriorLights:Electricity (SumOrAverage)

- ExteriorLights:Electricity (SumOrAverage)

- InteriorEquipment:Electricity (SumOrAverage)

- ExteriorEquipment:Electricity (SumOrAverage)

- Fans:Electricity (SumOrAverage)

- Pumps:Electricity (SumOrAverage)

- Heating:Electricity (SumOrAverage)

- Cooling:Electricity (SumOrAverage)

- HeatRejection:Electricity (SumOrAverage)

- Humidifier:Electricity (SumOrAverage)

- HeatRecovery:Electricity (SumOrAverage)

- WaterSystems:Electricity (SumOrAverage)

- Cogeneration:Electricity (SumOrAverage)

#### EndUseEnergyConsumptionNaturalGasMonthly

- InteriorEquipment:Gas (SumOrAverage)

- ExteriorEquipment:Gas (SumOrAverage)

- Heating:Gas (SumOrAverage)

- Cooling:Gas (SumOrAverage)

- WaterSystems:Gas (SumOrAverage)

- Cogeneration:Gas (SumOrAverage)

#### EndUseEnergyConsumptionDieselMonthly

- ExteriorEquipment:Diesel (SumOrAverage)

- Cooling:Diesel (SumOrAverage)

- Heating:Diesel (SumOrAverage)

- WaterSystems:Diesel (SumOrAverage)

- Cogeneration:Diesel (SumOrAverage)

#### EndUseEnergyConsumptionFuelOilMonthly

- ExteriorEquipment:FuelOil\#1 (SumOrAverage)

- Cooling:FuelOil\#1 (SumOrAverage)

- Heating:FuelOil\#1 (SumOrAverage)

- WaterSystems:FuelOil\#1 (SumOrAverage)

- Cogeneration:FuelOil\#1 (SumOrAverage)

- ExteriorEquipment:FuelOil\#2 (SumOrAverage)

- Cooling:FuelOil\#2 (SumOrAverage)

- Heating:FuelOil\#2 (SumOrAverage)

- WaterSystems:FuelOil\#2 (SumOrAverage)

- Cogeneration:FuelOil\#2 (SumOrAverage)

#### EndUseEnergyConsumptionCoalMonthly

- ExteriorEquipment:Coal (SumOrAverage)

- Heating:Coal (SumOrAverage)

- WaterSystems:Coal (SumOrAverage)

#### EndUseEnergyConsumptionPropaneMonthly

- ExteriorEquipment:Propane (SumOrAverage)

- Cooling:Propane (SumOrAverage)

- Heating:Propane (SumOrAverage)

- WaterSystems:Propane (SumOrAverage)

- Cogeneration:Propane (SumOrAverage)

#### EndUseEnergyConsumptionGasolineMonthly

- ExteriorEquipment:Gasoline (SumOrAverage)

- Cooling:Gasoline (SumOrAverage)

- Heating:Gasoline (SumOrAverage)

- WaterSystems:Gasoline (SumOrAverage)

- Cogeneration:Gasoline (SumOrAverage)

#### EndUseEnergyConsumptionOtherFuelsMonthly

- ExteriorEquipment:OtherFuel1 (SumOrAverage)

- Cooling:OtherFuel1 (SumOrAverage)

- Heating:OtherFuel1 (SumOrAverage)

- WaterSystems:OtherFuel1 (SumOrAverage)

- Cogeneration:OtherFuel1 (SumOrAverage)

- ExteriorEquipment:OtherFuel1 (SumOrAverage)

- Cooling:OtherFuel2 (SumOrAverage)

- Heating:OtherFuel2 (SumOrAverage)

- WaterSystems:OtherFuel2 (SumOrAverage)

- Cogeneration:OtherFuel2 (SumOrAverage)

#### PeakEnergyEndUseElectricityPart1Monthly

- InteriorLights:Electricity (Maximum)

- ExteriorLights:Electricity (Maximum)

- InteriorEquipment:Electricity (Maximum)

- ExteriorEquipment:Electricity (Maximum)

- Fans:Electricity (Maximum)

- Pumps:Electricity (Maximum)

- Heating:Electricity (Maximum)

#### PeakEnergyEndUseElectricityPart2Monthly

- Cooling:Electricity (Maximum)

- HeatRejection:Electricity (Maximum)

- Humidifier:Electricity (Maximum)

- HeatRecovery:Electricity (Maximum)

- WaterSystems:Electricity (Maximum)

- Cogeneration:Electricity (Maximum)

#### ElectricComponentsOfPeakDemandMonthly

- Electricity:Facility (Maximum)

- InteriorLights:Electricity (ValueWhenMaxMin)

- InteriorEquipment:Electricity (ValueWhenMaxMin)

- ExteriorLights:Electricity (ValueWhenMaxMin)

- ExteriorEquipment:Electricity (ValueWhenMaxMin)

- Fans:Electricity (ValueWhenMaxMin)

- Pumps:Electricity (ValueWhenMaxMin)

- Heating:Electricity (ValueWhenMaxMin)

- Cooling:Electricity (ValueWhenMaxMin)

- HeatRejection:Electricity (ValueWhenMaxMin)

#### PeakEnergyEndUseNaturalGasMonthly

- InteriorEquipment:Gas (Maximum)

- ExteriorEquipment:Gas (Maximum)

- Heating:Gas (Maximum)

- Cooling:Gas (Maximum)

- WaterSystems:Gas (Maximum)

- Cogeneration:Gas (Maximum)

#### PeakEnergyEndUseDieselMonthly

- ExteriorEquipment:Diesel (Maximum)

- Cooling:Diesel (Maximum)

- Heating:Diesel (Maximum)

- WaterSystems:Diesel (Maximum)

- Cogeneration:Diesel (Maximum)

#### PeakEnergyEndUseFuelOilMonthly

- ExteriorEquipment:FuelOil\#1 (Maximum)

- Cooling:FuelOil\#1 (Maximum)

- Heating:FuelOil\#1 (Maximum)

- WaterSystems:FuelOil\#1 (Maximum)

- Cogeneration:FuelOil\#1 (Maximum)

- ExteriorEquipment:FuelOil\#2 (Maximum)

- Cooling:FuelOil\#2 (Maximum)

- Heating:FuelOil\#2 (Maximum)

- WaterSystems:FuelOil\#2 (Maximum)

- Cogeneration:FuelOil\#2 (Maximum)

#### PeakEnergyEndUseCoalMonthly

- ExteriorEquipment:Coal (Maximum)

- Heating:Coal (Maximum)

- WaterSystems:Coal (Maximum)

#### PeakEnergyEndUsePropaneMonthly

- ExteriorEquipment:Propane (Maximum)

- Cooling:Propane (Maximum)

- Heating:Propane (Maximum)

- WaterSystems:Propane (Maximum)

- Cogeneration:Propane (Maximum)

#### PeakEnergyEndUseGasolineMonthly

- ExteriorEquipment:Gasoline (Maximum)

- Cooling:Gasoline (Maximum)

- Heating:Gasoline (Maximum)

- WaterSystems:Gasoline (Maximum)

- Cogeneration:Gasoline (Maximum)

#### PeakEnergyEndUseOtherFuelsMonthly

- ExteriorEquipment:OtherFuel1 (Maximum)

- Cooling:OtherFuel1 (Maximum)

- Heating:OtherFuel1 (Maximum)

- WaterSystems:OtherFuel1 (Maximum)

- Cogeneration:OtherFuel1 (Maximum)

- ExteriorEquipment:OtherFuel1 (Maximum)

- Cooling:OtherFuel1 (Maximum)

- Heating:OtherFuel1 (Maximum)

- WaterSystems:OtherFuel1 (Maximum)

- Cogeneration:OtherFuel1 (Maximum)

#### SetpointsNotMetWithTemperaturesMonthly

- Zone Heating Setpoint Not Met Time (HoursNonZero)

- Zone Mean Air Temperature (SumOrAverageDuringHoursShown)

- Zone Heating Setpoint Not Met While Occupied Time (HoursNonZero)

- Zone Mean Air Temperature (SumOrAverageDuringHoursShown)

- Zone Cooling Setpoint Not Met Time (HoursNonZero)

- Zone Mean Air Temperature (SumOrAverageDuringHoursShown)

- Zone Cooling Setpoint Not Met While Occupied Time (HoursNonZero)

- Zone Mean Air Temperature (SumOrAverageDuringHoursShown)

#### ComfortReportSimple55Monthly

- Zone Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time (HoursNonZero)

- Zone Mean Air Temperature (SumOrAverageDuringHoursShown)

- Zone Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time (HoursNonZero)

- Zone Mean Air Temperature (SumOrAverageDuringHoursShown)

- Zone Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time (HoursNonZero)

- Zone Mean Air Temperature (SumOrAverageDuringHoursShown)

#### UnglazedTranspiredSolarCollectorSummaryMonthly

- Solar Collector System Efficiency (HoursNonZero)

- Solar Collector System Efficiency (SumOrAverageDuringHoursShown)

- Solar Collector Outside Face Suction Velocity (SumOrAverageDuringHoursShown)

- Solar Collector Sensible Heating Rate (SumOrAverageDuringHoursShown)

#### OccupantComfortDataSummaryMonthly

- Zone People Occupant Count (HoursNonZero)

- Zone Air Temperature (SumOrAverageDuringHoursShown)

- Zone Air Relative Humidity (SumOrAverageDuringHoursShown)

- Zone Thermal Comfort Fanger Model PMV (SumOrAverageDuringHoursShown)

- Zone Thermal Comfort Fanger Model PPD (SumOrAverageDuringHoursShown)

#### ChillerReportMonthly

- Chiller Electric Energy (SumOrAverage)

- Chiller Electric Power (Maximum)

- Chiller Electric Energy (HoursNonZero)

- Chiller Evaporator Cooling Energy (SumOrAverage)

- Chiller Condenser Heat Transfer Energy (SumOrAverage)

- Chiller COP (SumOrAverage)

- Chiller COP (Maximum)

#### TowerReportMonthly

- Tower Fan Electric Consumption (SumOrAverage)

- Tower Fan Electric Consumption (HoursNonZero)

- Cooling Tower Fan Electric Power (Maximum)

- Cooling Tower Heat Transfer Rate (Maximum)

- Cooling Tower Inlet Temperature (SumOrAverage)

- Cooling Tower Outlet Temperature (SumOrAverage)

- Cooling Tower Mass Flow Rate (SumOrAverage)

#### BoilerReportMonthly

- Boiler Heating Energy (SumOrAverage)

- Boiler Gas Consumption (SumOrAverage)

- Boiler Heating Energy (HoursNonZero)

- Boiler Heating Rate (Maximum)

- Boiler Gas Consumption Rate (Maximum)

- Boiler Inlet Temperature (SumOrAverage)

- Boiler Outlet Temperature (SumOrAverage)

- Boiler Mass Flow Rate (SumOrAverage)

- Boiler Ancillary Electric Power (SumOrAverage)

#### DXReportMonthly

- Cooling Coil Total Cooling Energy (SumOrAverage)

- Cooling Coil Electric Energy (SumOrAverage)

- Cooling Coil Total Cooling Energy (HoursNonZero)

- Cooling Coil Sensible Cooling Energy (SumOrAverage)

- Cooling Coil Latent Cooling Energy (SumOrAverage)

- Cooling Coil Crankcase Heater Electric Energy (SumOrAverage)

- Cooling Coil Runtime Fraction (Maximum)

- Cooling Coil Runtime Fraction (Minimum)

- DX Coil Total Cooling Rate (Maximum)

- Cooling Coil Sensible Cooling Rate (Maximum)

- Cooling Coil Latent Cooling Rate (Maximum)

- Cooling Coil Electric Power (Maximum)

- Cooling Coil Crankcase Heater Electric Power (Maximum)

#### WindowReportMonthly

- Surface Window Transmitted Solar Radiation Rate (SumOrAverage)

- Surface Window Transmitted Beam Solar Radiation Rate (SumOrAverage)

- Surface Window Transmitted Diffuse Solar Radiation Rate (SumOrAverage)

- Surface Window Heat Gain Rate (SumOrAverage)

- Surface Window Heat Loss Rate (SumOrAverage)

- Surface Window Inside Face Glazing Condensation Status (HoursNonZero)

- Surface Shading Device Is On Time Fraction (HoursNonZero)

- Surface Storm Window On Off Status (HoursNonZero)

#### WindowEnergyReportMonthly

- Surface Window Transmitted Solar Radiation Energy (SumOrAverage)

- Surface Window Transmitted Beam Solar Radiation Rate Energy (SumOrAverage)

- Surface Window Transmitted Diffuse Solar Radiation Energy (SumOrAverage)

- Surface Window Heat Gain Energy (SumOrAverage)

- Surface Window Heat Loss Energy (SumOrAverage)

#### WindowZoneSummaryMonthly

- Zone Windows Total Heat Gain Rate (SumOrAverage)

- Zone Windows Total Heat Loss Rate (SumOrAverage)

- Zone Windows Total Transmitted Solar Radiation Rate (SumOrAverage)

- Zone Exterior Windows Total Transmitted Beam Solar Radiation Rate (SumOrAverage)

- Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Rate (SumOrAverage)

- Zone Interior Windows Total Transmitted Diffuse Solar Radiation Rate (SumOrAverage)

- Zone Interior Windows Total Transmitted Beam Solar Radiation Rate (SumOrAverage)

#### WindowEnergyZoneSummaryMonthly

- Zone Windows Total Heat Gain Energy (SumOrAverage)

- Zone Windows Total Heat Loss Energy (SumOrAverage)

- Zone Windows Total Transmitted Solar Radiation Energy (SumOrAverage)

- Zone Exterior Windows Total Transmitted Beam Solar Radiation Energy (SumOrAverage)

- Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Energy (SumOrAverage)

- Zone Interior Windows Total Transmitted Diffuse Solar Radiation Energy (SumOrAverage)

- Zone Interior Windows Total Transmitted Beam Solar Radiation Energy (SumOrAverage)

#### AverageOutdoorConditionsMonthly

- Site Outdoor Air Drybulb Temperature (SumOrAverage)

- Site Outdoor Air Wetbulb Temperature (SumOrAverage)

- Site Outdoor Air Dewpoint Temperature (SumOrAverage)

- Wind Speed (SumOrAverage)

- Site Sky Temperature (SumOrAverage)

- Site Diffuse Solar Radiation Rate per Area (SumOrAverage)

- Site Direct Solar Radiation Rate per Area (SumOrAverage)

- Raining (SumOrAverage)

#### OutdoorConditionsMaximumDryBulbMonthly

- Site Outdoor Air Drybulb Temperature (Maximum)

- Site Outdoor Air Wetbulb Temperature (ValueWhenMaxMin)

- Site Outdoor Air Dewpoint Temperature (ValueWhenMaxMin)

- Wind Speed (ValueWhenMaxMin)

- Site Sky Temperature (ValueWhenMaxMin)

- Site Diffuse Solar Radiation Rate per Area (ValueWhenMaxMin)

- Site Direct Solar Radiation Rate per Area (ValueWhenMaxMin)

#### OutdoorConditionsMinimumDryBulbMonthly

- Site Outdoor Air Wetbulb Temperature (ValueWhenMaxMin)

- Site Outdoor Air Dewpoint Temperature (ValueWhenMaxMin)

- Wind Speed (ValueWhenMaxMin)

- Site Sky Temperature (ValueWhenMaxMin)

- Site Diffuse Solar Radiation Rate per Area (ValueWhenMaxMin)

- Site Direct Solar Radiation Rate per Area (ValueWhenMaxMin)

#### OutdoorConditionsMaximumWetBulbMonthly

- Site Outdoor Air Wetbulb Temperature (Maximum)

- Site Outdoor Air Drybulb Temperature (ValueWhenMaxMin)

- Site Outdoor Air Dewpoint Temperature (ValueWhenMaxMin)

- Wind Speed (ValueWhenMaxMin)

- Site Sky Temperature (ValueWhenMaxMin)

- Site Diffuse Solar Radiation Rate per Area (ValueWhenMaxMin)

- Site Direct Solar Radiation Rate per Area (ValueWhenMaxMin)

#### OutdoorConditionsMaximumDewPointMonthly

- Site Outdoor Air Dewpoint Temperature (Maximum)

- Site Outdoor Air Drybulb Temperature (ValueWhenMaxMin)

- Site Outdoor Air Wetbulb Temperature (ValueWhenMaxMin)

- Wind Speed (ValueWhenMaxMin)

- Site Sky Temperature (ValueWhenMaxMin)

- Site Diffuse Solar Radiation Rate per Area (ValueWhenMaxMin)

- Site Direct Solar Radiation Rate per Area (ValueWhenMaxMin)

#### OutdoorGroundConditionsMonthly

- Site Ground Temperature (SumOrAverage)

- Site Surface Ground Temperature (SumOrAverage)

- Site Deep Ground Temperature (SumOrAverage)

- Site Mains Water Temperature (SumOrAverage)

- Site Ground Reflected Solar Radiation Rate per Area (SumOrAverage)

- Snow On Ground (SumOrAverage)

#### WindowACReportMonthly

- Zone Window Air Conditioner Total Cooling Energy (SumOrAverage)

- Zone Window Air Conditioner Electric Energy (SumOrAverage)

- Zone Window Air Conditioner Total Cooling Energy (HoursNonZero)

- Zone Window Air Conditioner Sensible Cooling Energy (SumOrAverage)

- Zone Window Air Conditioner Latent Cooling Energy (SumOrAverage)

- Zone Window Air Conditioner Total Cooling Rate (Maximum)

- Zone Window Air Conditioner Sensible Cooling Rate (ValueWhenMaxMin)

- Zone Window Air Conditioner Latent Cooling Rate (ValueWhenMaxMin)

- Zone Window Air Conditioner Electric Power (ValueWhenMaxMin)

#### WaterHeaterReportMonthly

- Water Heater Total Demand Energy (SumOrAverage)

- Water Heater Use Side Heat Transfer Energy (SumOrAverage)

- Water Heater Burner Heating Energy (SumOrAverage)

- Water Heater Gas Consumption (SumOrAverage)

- Water Heater Total Demand Energy (HoursNonZero)

- Water Heater Loss Demand Energy (SumOrAverage)

- Water Heater Heat Loss Energy (SumOrAverage)

- Water Heater Tank Temperature (SumOrAverage)

- Water Heater Heat Recovery Supply Energy (SumOrAverage)

- Water Heater Source Side Heat Transfer Energy (SumOrAverage)

#### GeneratorReportMonthly

- Generator Produced Electric Energy (SumOrAverage)

- Generator Diesel Energy (SumOrAverage)

- Generator Gas Consumption (SumOrAverage)

- Generator Produced Electric Energy (HoursNonZero)

- Generator Total Heat Recovery (SumOrAverage)

- Generator Jacket Heat Recovery Energy (SumOrAverage)

- Generator Lube Heat Recovery Energy (SumOrAverage)

- Generator Exhaust Heat Recovery Energy (SumOrAverage)

- Generator Exhaust Air Temperature (SumOrAverage)

#### DaylightingReportMonthly

- Site Exterior Beam Normal Illuminance (HoursNonZero)

- Daylighting Lighting Power Multiplier (SumOrAverageDuringHoursShown)

- Daylighting Lighting Power Multiplier (MinimumDuringHoursShown)

- Daylighting Reference Point 1 Illuminance (SumOrAverageDuringHoursShown)

- Daylighting Reference Point 1 Glare Index (SumOrAverageDuringHoursShown)

- Daylighting Reference Point 2 Illuminance (SumOrAverageDuringHoursShown)

- Daylighting Reference Point 2 Glare Index (SumOrAverageDuringHoursShown)

- Daylighting Reference Point 1 Glare Index Setpoint Exceeded Time

- Daylighting Reference Point 2 Glare Index Setpoint Exceeded Time

- Daylighting Reference Point 1 Daylight Illuminance Setpoint Exceeded Time

- Daylighting Reference Point 2 Daylight Illuminance Setpoint Exceeded Time

#### CoilReportMonthly

- Heating Coil Heating Energy (SumOrAverage)

- Heating Coil Heating Rate (Maximum)

- Cooling Coil Total Cooling Energy (SumOrAverage)

- Cooling Coil Sensible Cooling Energy (SumOrAverage)

- Cooling Coil Total Cooling Rate (Maximum)

- Cooling Coil Sensible Cooling Rate (ValueWhenMaxMin)

- Cooling Coil Wetted Area Fraction (SumOrAverage)

#### PlantLoopDemandReportMonthly

- Plant Supply Side Cooling Demand Rate (SumOrAverage)

- Plant Supply Side Cooling Demand Rate (Maximum)

- Plant Supply Side Heating Demand Rate (SumOrAverage)

- Plant Supply Side Heating Demand Rate (Maximum)

#### FanReportMonthly

- Fan Electric Consumption (SumOrAverage)

- Fan Rise in Air Temperature (SumOrAverage)

- Fan Electric Power (Maximum)

- Fan Rise in Air Temperature (ValueWhenMaxMin)

#### PumpReportMonthly

- Pump Electric Energy (SumOrAverage)

- Pump Fluid Heat Gain Energy (SumOrAverage)

- Pump Electric Power (Maximum)

- Pump Shaft Power (ValueWhenMaxMin)

- Pump Fluid Heat Gain Rate (ValueWhenMaxMin)

- Pump Outlet Temperature (ValueWhenMaxMin)

- Pump Mass Flow Rate (ValueWhenMaxMin)

#### CondLoopDemandReportMonthly

- Plant Supply Side Cooling Demand Rate (SumOrAverage)

- Plant Supply Side Cooling Demand Rate (Maximum)

- Plant Supply Side Inlet Temperature (ValueWhenMaxMin)

- Plant Supply Side Outlet Temperature (ValueWhenMaxMin)

- Plant Supply Side Heating Demand Rate (SumOrAverage)

- Plant Supply Side Heating Demand Rate (Maximum)

#### ZoneTemperatureOscillationReportMonthly

- Zone Oscillating Temperatures Time (HoursNonZero)

- Zone People Occupant Count (SumOrAverageDuringHoursShown)

#### AirLoopSystemEnergyAndWaterUseMonthly

- Air System Hot Water Energy (SumOrAverage)

- Air System Steam Energy (SumOrAverage)

- Air System Chilled Water Energy (SumOrAverage)

- Air System Electric Energy (SumOrAverage)

- Air System Gas Energy (SumOrAverage)

- Air System Water Volume (SumOrAverage)

#### AirLoopSystemComponentLoadsMonthly

- Air System Fan Air Heating Energy (SumOrAverage)

- Air System Cooling Coil Total Cooling Energy (SumOrAverage)

- Air System Heating Coil Total Heating Energy (SumOrAverage)

- Air System Heat Exchanger Total Heating Energy (SumOrAverage)

- Air System Heat Exchanger Total Cooling Energy (SumOrAverage)

- Air System Humidifier Total Heating Energy (SumOrAverage)

- Air System Evaporative Cooler Total Cooling Energy (SumOrAverage)

- Air System Desiccant Dehumidifier Total Cooling Energy (SumOrAverage)

#### AirLoopSystemComponentEnergyUseMonthly

- Air System Fan Electric Energy (SumOrAverage)

- Air System Heating Coil Hot Water Energy (SumOrAverage)

- Air System Cooling Coil Chilled Water Energy (SumOrAverage)

- Air System DX Heating Coil Electric Energy (SumOrAverage)

- Air System DX Cooling Coil Electric Energy (SumOrAverage)

- Air System Heating Coil Electric Energy (SumOrAverage)

- Air System Heating Coil Gas Energy (SumOrAverage)

- Air System Heating Coil Steam Energy (SumOrAverage)

- Air System Humidifier Electric Energy (SumOrAverage)

- Air System Evaporative Cooler Electric Energy (SumOrAverage)

- Air System Desiccant Dehumidifier Electric Energy (SumOrAverage)

#### MechanicalVentilationLoadsMonthly

- Zone Mechanical Ventilation No Load Heat Removal Energy (SumOrAverage)

- Zone Mechanical Ventilation Cooling Load Increase Energy (SumOrAverage)

- Zone Mechanical Ventilation Cooling Load Increase Due to Overheating Energy (SumOrAverage)

- Zone Mechanical Ventilation Cooling Load Decrease Energy (SumOrAverage)

- Zone Mechanical Ventilation No Load Heat Addition Energy (SumOrAverage)

- Zone Mechanical Ventilation Heating Load Increase Energy (SumOrAverage)

- Zone Mechanical Ventilation Heating Load Increase Due to Overcooling Energy (SumOrAverage)

- Zone Mechanical Ventilation Heating Load Decrease Energy (SumOrAverage)

- Zone Mechanical Ventilation Air Changes per Hour (SumOrAverage)



Sample IDF Input – Output:Table:SummaryReports

```idf
Output:Table:SummaryReports,
        AllSummary,  !- Report 1 Name
        AllMonthly;  !- Report 2 Name
```

OutputControl:Table:Style
-------------------------

The OutputControl:Table:Style object controls how all standardized reports are produced.

#### Field: Column Separator

You have nine different options:

- Comma

- Tab

- Fixed

- HTML

- XML

- CommaAndHTML

- TabAndHTML

- XMLAndHTML

- All

The Comma style produces a text file (eplustbl.csv) with the values of the table separated by commas. This is a good format for importing the results into a spreadsheet. If you do open the file with a spreadsheet, make sure you close the file prior to rerunning EnergyPlus otherwise the file will not be updated.

The Tab style produces a text file (eplustbl.tab) with the values separated by tabs. It is also a good format for using with a spreadsheet program but has the advantage of being more readable in a text editor.

The Fixed style produces a text file (eplustbl.txt) with the values at specific columns. It is the easiest to view using a text editor.

The HTML style produces a file (eplustbl.htm) that can be opened with an internet browser program. The values are shown in a tabular format that is easy to view. One advantage of the HTML style is that the results can be viewed in an internet browser program and EnergyPlus can be rerun and the “refresh” button pressed in the internet browser program to see the new results. . When the HTML style is specified, a Table of Contents for the file is generated and placed near the top of the file after the Annual Building Utility Performance Summary (aka ABUPS) report. The Table of Contents includes the names of each report and links for each table in the file.

The XML style produces a file (eplustbl.xml) that can be opened by any XML editor and many internet browser programs. The XML output is specifically intended for use by other software programs to make it simple to extract specific results.

The last four options combine the previous styles and allow multiple reports with different styles to be produced during a single simulation.

#### Field: Unit Conversion

This field is optional and if not specified defaults to no unit conversions. Four different input options are available:

- None – no conversions performed

- JtoKWH – Joules converted into kWh (1 / 3,600,000)

- JtoMJ – Joules converted into Megajoules (1 / 1,000,000)

- JtoGJ – Joules converted into Gigajoules (1 / 1,000,000,000)

- InchPound – convert all annual, monthly, economics and timebins tabular values to common Inch-Pound equivalent

The current options are limited to just how the energy is being reported. The default is to report energy in the form of Joules but since the magnitude of those numbers for many buildings is very large, these other conversion factors are available.

The JtoKWH, JtoMJ and JtoGJ unit conversion input option applies only to the Output:Table:Monthly reports and partially to the ABUPS report. For ABUPS, the JtoKWH option changes the report but the JtoMJ and JtoGJ options do not change the report since they it is already in MJ/m2 and GJ.. The InchPound unit conversion input option applies to all annual, monthly, timebins and economic reports that appear in the tabular output file.  In addition, this option does not effect the standard eplusout.eso file. The eplusout.eso file may be converted by using the ConvertESOMTR utility.

An example IDF input object follows.

```idf
OutputControl:Table:Style,
  Comma,                     ! Column Separator
   InchPound;                 ! Unit Conversion
```



Examples of the tabular reports and descriptions are contained in the Output Details and Examples document.



