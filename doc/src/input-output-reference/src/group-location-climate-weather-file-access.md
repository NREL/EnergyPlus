# Group -- Location – Climate – Weather File Access

This group of objects describes the ambient conditions for the simulation.

## Site:Location

The location class describes the parameters for the building's location. Only one location is allowed. Weather data file location, if it exists, will override any location data in the IDF. Thus, for an annual simulation, a Location does not need to be entered.

### Inputs

#### Field: Name

This alpha field is used as an identifying field in output reports.

#### Field: Latitude

This field represents the latitude (in degrees) of the facility. By convention, North Latitude is represented as positive; South Latitude as negative. Minutes should be represented in decimal fractions of 60. (15' is 15/60 or .25)

#### Field: Longitude

This field represents the longitude (in degrees) of the facility. By convention, East Longitude is represented as positive; West Longitude as negative. Minutes should be represented in decimal fractions of 60. (15' is 15/60 or .25)

#### Field: Time Zone

This field represents the time zone of the facility (relative to Greenwich Mean Time or the 0^th^ meridian). Time zones west of GMT (e.g. North America) are represented as negative; east of GMT as positive. Non-whole hours can be represented in decimal (e.g. 6:30 is 6.5).

#### Field: Elevation

This field represents the elevation of the facility in meters (relative to sea level).

Units with their abbreviations are shown in Appendix A.  And, as shown in an IDF:

~~~~~~~~~~~~~~~~~~~~

    Site:Location, DENVER COLORADO,     ! Name
       39.75000    ,     ! Latitude {N+ S-}
      -104.8700    ,     ! Longitude {W- E+}
      -7.000000    ,     ! TimeZoneNumber {GMT+/-}
      1610.26;           ! Elevation {m}
~~~~~~~~~~~~~~~~~~~~

Most examples in this document include the comment lines that illustrate each data field's value. However, this is not necessary (though it makes the IDF more readable). The previous example could also be:

~~~~~~~~~~~~~~~~~~~~

    Site:Location, DENVER COLORADO,39.75,-104.87,-7,1610.26;
~~~~~~~~~~~~~~~~~~~~

## SizingPeriod:DesignDay

The design day input describes the parameters to effect a "design day" simulation, often used for load calculations or sizing equipment. Using the values in these fields, EnergyPlus "creates" a complete days' worth of weather data (air temperatures, solar radiation, etc.)  Normal operation uses the default range multipliers as shown in Figure 6 though users may choose to input their own multiplier schedule. Likewise, normal operation specifies one "humidity indicating condition" which is used to calculate the humidity ratio at maximum temperature – this is used as the constant humidity ratio for the entire day. Again, this can be overridden by specifying a relative humidity schedule or requesting generation of an hourly wet-bulb temperature profile.  Multiple design days may be specified.

We refer you to the ASHRAE Handbook of Fundamentals for philosophy of what parameters are important for use as "design conditions" in sizing equipment.

> In the install, the "design day files" are included for the weather file locations that are included (weatherdata folder). All the design day definitions from the ASHRAE design conditions (latest publication date) are included, grouped by WMO region, on the main web site with the weather data. http://www.energyplus.gov/cfm/weather_data.cfm  These files are in "macro" form but it is easy to cut and paste the appropriate definition segments. These files include the location information as well as some locations have **RunPeriodControl:DaylightSavingTime** objects.

### Inputs

#### Field: Name

This field, like the location name, is used simply for reporting and identification. This name must be unique among the SizingPeriod names entered.

#### Field: Month

This numeric field specifies the month. That, in conjunction with the day of the month and location information, determines the current solar position and solar radiation values for each hour of the day.

#### Field: Day of Month

This numeric field specifies the day of the month. That, in conjunction with the month and location information, determines the current solar position and solar radiation values for each hour of the day.

#### Field: Day Type

This alpha field specifies the day type for the design day. This value indicates which day profile to use in schedules. For further information, see the Schedule discussion later in this document. (ref: Schedule)  Note that two of the possible day types are SummerDesignDay (for cooling) and WinterDesignDay (for heating) – allowing the user to customize schedules for the design conditions. That is, for design days tagged with a SummerDesignDay type, you can set schedules to be worst or typical schedules for a cooling season.

#### Field: Maximum Dry-Bulb Temperature

This numeric field should contain the day's maximum dry-bulb temperature in degrees Celsius. (Reference Appendix A of this document for EnergyPlus standard units and abbreviations). The MacroDataSets design day files show extreme temperature for locations as indicated in the ASHRAE HOF design condition tables.

#### Field: Daily Dry-bulb Temperature Range

A design day can have a "high" temperature and a "low" temperature (or can be a constant temperature for each hour of the day). If there is a difference between high and low temperatures, this field should contain the difference from the high to the low. EnergyPlus, by default, distributes this range over the 24 hours in the day as shown in the figure below:

![Default Daily range Multiplier for Design Days](media/default-daily-range-multiplier-for-design.png)


The multipliers are taken from the ASHRAE 2009 HOF. More explicitly, EnergyPlus creates an air temperature for each timestep by using the entered maximum dry-bulb temperature in conjunction with the entered daily range and the above multiplier values. The actual equation used is shown below:

![](media/image11.png)\


where

T~current~= Air temperature of current Hour of Day

T~Max~= User supplied Max Dry-bulb Temperature

T~range~= User supplied Daily Temperature Range

T~Multiplier~= Range multiplier as shown on the above graph

The range multiplier values represent typical conditions of diurnal temperatures (i.e. the low temperature for the day occurring about 5:00 AM and the maximum temperature for the day occurring about 3:00 PM.  Note that EnergyPlus does not shift the profile based on the time of solar noon as is optionally allowed in ASHRAE procedures.

ASHRAE research indicates that dry-bulb and wet-bulb temperatures typically follow the same profile, so EnergyPlus can use the default profile to generate humidity conditions (see Humidity Indicating Type = WetBulbProfileDefaultMultipliers below).

#### Field: Dry-Bulb Temperature Range Modifier Type 

If you are happy with lows at 5am and highs at 3pm, you can ignore this field. If you want to specify your own temperature range multipliers (see earlier discussion at the Temperature Range field description), you can specify a type here and create a day schedule which you reference in the next field.

If you specify **MultiplierSchedule** in this field, then you need to create a day schedule that specifies a multiplier applied to the temperature range field (above) to create the proper dry-bulb temperature range profile for your design day.

If you specify **DifferenceSchedule** in this field, then you need to create a day schedule that specifies a number to be **subtracted** from dry-bulb maximum temperature for each timestep in the day. Note that numbers in the delta schedules cannot be negative as that would result in a higher maximum than the maximum previously specified.

If you specify **TemperatureProfileSchedule** in this field, then you need to create a day schedule that specifies the actual dry-bulb temperatures throughout the day. You will not need to include a Maximum Dry-Bulb Temperature in that field.

If you leave this field blank or enter **DefaultMultipliers**, then the default multipliers will be used as shown in the "temperature range" field above.

#### Field: Dry-Bulb Temperature Range Modifier Day Schedule Name

This field is the name of a **day schedule** (ref. [Schedule:Day:Hourly](#scheduledayhourly), [Schedule:Day:Interval](#scheduledayinterval), [Schedule:Day:List](#scheduledaylist) objects) with the values as specified in the Dry-Bulb Temperature Range Modifier Type field above.

#### Field: Humidity Condition Type

The values/schedules indicated here and in subsequent fields create the humidity values in the 24 hour design day conditions profile. Valid choices here are: **WetBulb**, **Dewpoint**, **WetBulbProfileDefaultMultipliers, WetBulbProfileDifferenceSchedule,** **WetBulbProfileMultiplierSchedule**, **HumidityRatio**, **Enthalpy**, and **Schedule.**

The Humidity Condition Type fields have interacting uses and units, summarized as follows:

Table: Humidity Indicating Field Interactions - Design Day

**Humidity Condition Type**|**Primary Humidity Indicating Field**|**Humidity Indicating Day Schedule**
----------------------------------------|--------------------------------------------------|-------------------------------------------------
WetBulb|Wetbulb or DewPoint at Maximum Dry-Bulb|N/A (unused)
DewPoint|Wetbulb or DewPoint at Maximum Dry-Bulb|N/A (unused)
HumidityRatio|Humidity Ratio at Maximum Dry-Bulb |N/A (unused)
Enthalpy|Enthalpy at Maximum Dry-Bulb|N/A (unused)
WetBulbProfileDefaultMultipliers|Wetbulb or DewPoint at Maximum Dry-Bulb|N/A (unused)
WetBulbProfileMultiplierSchedule|Wetbulb or DewPoint at Maximum Dry-Bulb|Fractions of wet-bulb daily range (0 – 1)
WetBulbProfileDifferenceSchedule|Wetbulb or DewPoint at Maximum Dry-Bulb|Difference between maximum and hour/timestep wet-bulb temperature, C
RelativeHumiditySchedule|N/A|Relative humidity (%, 0 – 100)

Humidity conditions over the day are derived as follows:

**WetBulb, DewPoint, HumidityRatio, or Enthalpy**:

These four methods assume constant absolute humidity over the day.  Calculate W = humidity ratio at Maximum Dry-Bulb Temperature and Humidity Indicating Conditions.  Derive hourly/timestep humidity conditions from W and hour/timestep dry-bulb temperature.

**WetBulbProfileDefaultMultipliers**

Generate the wet-bulb temperature profile using Default Daily Range Multiplier for Design Days (shown in Figure 6 above) and Daily Wet-Bulb Temperature Range (below).  This method is analogous to DefaultMultiplier generation of the dry-bulb temperature profile and is the procedure recommended in Chapter 14 of ASHRAE 2009 HOF.

**WetBulbProfileMultiplierSchedule**

Generate the wet-bulb profile using multipliers from the Humidity Indicating Day Schedule and Daily Wet-Bulb Temperature Range (below).  Analogous to dry-bulb MultiplierSchedule.

**WetBulbProfileDifferenceSchedule**

Generate the wet-bulb profile by subtracting Humidity Indicating Day Schedule values from the daily maximum wet-bulb temperature (specified in Humidity Indicating Conditions at Maximum Dry-Bulb).  Analogous to dry-bulb DifferenceSchedule.

**RelativeHumiditySchedule**

Hourly relative humidity is specified in Humidity Indicating Day Schedule.

In all cases, the humidity ratio is limited to saturation at the hour/timestep dry-bulb (that is, the dry-bulb temperature is used as specified, but the humidity ratio is modified as needed to be physically possible).  Once a valid air state is determined, a complete set of consistent hour/timestep psychrometric values (dewpoint, wet-bulb, and enthalpy) is derived.

#### Field: Wetbulb or DewPoint at Maximum Dry-Bulb

If you choose **Wetbulb** or **Dewpoint** in the Humidity Condition Type field, then this numeric field should contain that value. Note that this field is unnecessary when you put in a humidity indicating day schedule (described later in this section).

#### Field: Humidity Condition Day Schedule Name

Allows specification a day schedule (ref. [Schedule:Day:Hourly](#scheduledayhourly), [Schedule:Day:Interval](#scheduledayinterval), [Schedule:Day:List](#scheduledaylist) objects) of values for relative humidity or wet-bulb profile per Humidity Indicating Type field.

#### Field: Humidity Ratio at Maximum Dry-Bulb

If **HumidityRatio** is chosen for the Humidity Condition Type field, then this numeric field should contain the desired humidity ratio at maximum dry-bulb temperature (units kg Water / kg Dry Air).

#### Field: Enthalpy at Maximum Dry-Bulb

If **Enthalpy** is chosen for the Humidity Condition Type field, then this numeric field should contain the desired enthalpy at maximum dry-bulb temperature (units Joules /kg).

#### Field: Daily Wet-Bulb Temperature Range

The difference between the maximum and minimum wet-bulb temperatures on the design day (Celsius).  Used for generating daily profiles of humidity conditions when Humidity Condition Type field (above) is **WetBulbProfileDefaultMultipliers** or **WetBulbProfileMultiplierSchedule**.  Values for wet-bulb temperature range are tabulated by month for 5564 locations worldwide on the CD that accompanies the ASHRAE 2009 HOF.

#### Field: Barometric Pressure

This numeric field is the constant barometric pressure (Pascals) for the entire day.

#### Field: Wind Speed

This numeric field is the wind speed in meters/second (constant throughout the day) for the day. The MacroDataSets design day files includes wind speed values (99.6% - heating, 1% cooling) as indicated in ASHRAE HOF design condition tables. But, you should be aware that traditional values for these are 6.7056 m/s (15 mph) for heating and 3.3528 m/s (7 mph) for cooling. A reminder is shown in the comments for the wind speed values. The comments also note the extreme wind speeds from the ASHRAE tables.

#### Field: Wind Direction

This numeric field is the source wind direction in degrees. By convention, winds from the North would have a value of 0., from the East a value of 90.

#### Field: Rain Indicator

This field indicates whether or not the building surfaces are wet. If the value is Yes, then it is assumed that the building surfaces are wet. Wet surfaces may change the conduction of heat through the surface.

#### Field: Snow Indicator

This field indicates whether or not there is snow on the ground. If the value is Yes, then it is assumed there is snow on the ground. Snow on the ground changes the ground reflectance properties.

#### Field: Daylight Saving Time Indicator

This field specifies whether to consider this day to be a Daylight Saving Day. Yes in the field indicates that daylight saving time is operational for this day. Yes essentially adds 1 hour to the scheduling times used in items with schedules.

#### Field: Solar Model Indicator

This field allows the user to select **ASHRAEClearSky, ASHRAETau, ZhangHuang, or Schedule** for solar modeling in the calculation of the solar radiation in the design day.  ASHRAEClearSky and ZhangHuang use the Clearness value as part of their calculations.  ASHRAETau invokes the revised model specified in Chapter 14 of the ASHRAE 2009 HOF and uses Taub and Taud values (below).  Technical details of the models are described in the Engineering Reference.  The **Schedule** choice allows you to enter schedule values for the day's profile (use the next two fields for the names).

#### Field: Beam Solar Day Schedule Name

This field allows the option for you to put in your own day profile of beam solar values (wh/m2). These values will replace the calculated values during design day processing. Only day schedules (ref. [Schedule:Day:Hourly](#scheduledayhourly), [Schedule:Day:Interval](#scheduledayinterval), [Schedule:Day:List](#scheduledaylist) objects) are used here.

#### Field: Diffuse Solar Day Schedule Name

This field allows the option for you to put in your own day profile of diffuse solar values (wh/m2).  These values will replace the calculated values during design day processing. Only day schedules (ref. [Schedule:Day:Hourly](#scheduledayhourly), [Schedule:Day:Interval](#scheduledayinterval), [Schedule:Day:List](#scheduledaylist) objects) are used here.

#### Field: ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub)

Optical depth for beam radiation, used only when Solar Model Indicator is ASHRAETau.  See next field.

#### Field: ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud)

Optical depth for diffuse radiation, used only when Solar Model Indicator is ASHRAETau.  Taub and Taud values are tabulated by month for 5564 locations worldwide on the CD that accompanies the ASHRAE 2009 HOF.

#### Field: Sky Clearness

If the choice in the Solar Model Indicator field is ASHRAEClearSky or ZhangHuang, then this numeric field should be entered. This value represents the "clearness" value for the day. This value, along with the solar position as defined by the Location information and the date entered for the design day, help define the solar radiation values for each hour of the day. Clearness may range from 0.0 to 1.2, where 1.0 represents a clear sky at sea level.  Values greated than 1.0 may be used for high altitude locations. Traditionally, one uses 0.0 clearness for Winter Design Days. Note that this "sky clearness" does not have the same meaning as output variable "Site Daylighting Model Sky Clearness".

IDF Examples:

~~~~~~~~~~~~~~~~~~~~

    ! Phoenix Sky Harbor Intl Ap_AZ_USA Annual Cooling (WB=>MDB)
    !   .4%, MDB=35.8°C WB=24.5°C
     SizingPeriod:DesignDay,
      Phoenix Sky Harbor Intl Ap Ann Clg .4% Condns WB=>MDB,     !- Name
              7,      !- Month
             21,      !- Day of Month
      SummerDesignDay,!- Day Type
           35.8,      !- Maximum Dry-Bulb Temperature {C}
             12,      !- Daily Dry-Bulb Temperature Range {C}
     DefaultMultipliers, !- Dry-Bulb Temperature Range Modifier Type
               ,      !- Dry-Bulb Temperature Range Modifier Schedule Name
        Wetbulb,      !- Humidity Condition Type
           24.5,      !- Wetbulb at Maximum Dry-Bulb {C}
               ,      !- Humidity Indicating Day Schedule Name
               ,      !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}
               ,      !- Enthalpy at Maximum Dry-Bulb {J/kg}
               ,      !- Daily Wet-Bulb Temperature Range {deltaC}
         97342.,      !- Barometric Pressure {Pa}
            4.1,      ! Wind Speed {m/s}
            260,      !- Wind Direction {Degrees; N=0, S=180}
             No,      !- Rain {Yes/No}
             No,      !- Snow on ground {Yes/No}
             No,      !- Daylight Savings Time Indicator
           ASHRAETau, !- Solar Model Indicator
               ,      !- Beam Solar Day Schedule Name
               ,      !- Diffuse Solar Day Schedule Name
          0.588,     !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub)
          1.653;  !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud)
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      SizingPeriod:DesignDay,
        Denver Centennial Golden Ann Htg 99% Condns DB - sched solar,  !- Name
        1,                       !- Month
        13,                      !- Day of Month
        WinterDesignDay,         !- Day Type
        -16,                     !- Maximum Dry-Bulb Temperature {C}
        0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}
        ,                        !- Dry-Bulb Temperature Range Modifier Type
        ,                    !- Dry-Bulb Temperature Range Modifier Schedule Name
        Wetbulb,                 !- Humidity Condition Type
        -16,                     !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}
        ,                        !- Humidity Indicating Day Schedule Name
        ,               !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}
        ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}
        ,                        !- Daily Wet-Bulb Temperature Range {deltaC}
        83411.,                  !- Barometric Pressure {Pa}
        2.3,                     !- Wind Speed {m/s}
        180,                     !- Wind Direction {deg}
        No,                      !- Rain Indicator
        No,                      !- Snow Indicator
        No,                      !- Daylight Saving Time Indicator
        Schedule,                !- Solar Model Indicator
        Winter (1/13) Beam Solar,!- Beam Solar Day Schedule Name
        Winter (1/13) Diffuse Solar;  !- Diffuse Solar Day Schedule Name

      Schedule:Day:Hourly,
        Winter (1/13) Beam Solar,
        Any Number,
        0,0,0,0,0,0,0,190,698,852,892,919,957,953,856,700,213,0,0,0,0,0,0,0;

      Schedule:Day:Hourly,
        Winter (1/13) Diffuse Solar,
        Any Number,
        0,0,0,0,0,0,0,35,118,116,92,65,40,14,0,0,5,0,0,0,0,0,0,0;
~~~~~~~~~~~~~~~~~~~~

Look at the example files 1ZoneUncontrolled_DDChanges.idf and 1ZoneUncontrolled_DD2009 for several examples of specifying Design Day inputs.

### Outputs

For the schedule fields in the object, several output variables can be used:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Sizing Period Site Beam Solar Schedule Value [W/m2]
    Zone,Average,Sizing Period Site Diffuse Solar Schedule Value [W/m2]
    Zone,Average,Sizing Period Site Humidity Condition Schedule Value [%]
    Zone,Average,Sizing Period Site Humidity Condition Schedule Value []
    Zone,Average,Sizing Period Site Drybulb Temperature Range Modifier Schedule Value []
    Zone,Average,Sizing Period Site Drybulb Temperature Range Modifier Schedule Value [deltaC]
    Zone,Average,Sizing Period Site Drybulb Temperature Range Modifier Schedule Value [C]
    Zone,Average,Sizing Period Site Sky Temperature Schedule Value [deltaC]
    Zone,Average,Sizing Period Site Sky Temperature Schedule Value [C]
~~~~~~~~~~~~~~~~~~~~

#### Sizing Period Site Beam Solar Schedule Value [W/m2]

#### Sizing Period Site Diffuse Solar Schedule Value [W/m2]

This schedule value is active when any Design Day objects have / use the solar schedule option. For those objects that don't have this option, the value will be displayed as -999.

#### Sizing Period Site Humidity Condition Schedule Value [%]

#### Sizing Period Site Humidity Condition Schedule Value []

This schedule value is active when any Design Day objects have / use the humidity schedule option. For those objects that don't have this option, the value will be displayed as -999.

#### Sizing Period Site Drybulb Temperature Range Modifier Schedule Value []

#### Sizing Period Site Drybulb Temperature Range Modifier Schedule Value [deltaC]

#### Sizing Period Site Drybulb Temperature Range Modifier Schedule Value [C]

This schedule value is active when any Design Day objects have / use the drybulb temperature range modifier chedule option. For those objects that don't have this option, the value will be displayed as -999.

#### Sizing Period Site Sky Temperature Schedule Value [deltaC]

#### Sizing Period Site Sky Temperature Schedule Value [C]

This schedule value is active when any Design Day objects have / use the WeatherProperties:SkyTemperature schedule option. For those objects that don't have this option, the value will be displayed as -999.

## Longer Design Periods

**Some features may benefit by using a longer design weather period for sizing or loads calculations.  Longer design periods may be created by using the [SizingPeriod:WeatherFileDays](#sizingperiodweatherfiledays)** or **[SizingPeriod:WeatherFileConditionType](#sizingperiodweatherfileconditiontype)** objects.  These two objects allow for selections from an attached weather file to be used in the sizing calculations.

## SizingPeriod:WeatherFileDays

The [SizingPeriod:WeatherFileDays](#sizingperiodweatherfiledays) object describes using a selected period from the "attached" weather file to be used in load calculations or sizing equipment.  The period selected can be as small as a single day or larger.  Multiple periods may be input. While this object may be used for sizing calculations, you should also consider using design days that represent more long term extremes or conditions.

### Inputs

#### Field: Name

This field allows for an assigned name for this run period so it can be tracked easily in sizing and other outputs.

#### Field: Begin Month

This numeric field should contain the starting month number (1=January, 2=February, etc.) for the annual run period desired.

#### Field: Begin Day of Month

This numeric field should contain the starting day of the starting month (must be valid for month) for the annual run period desired.

#### Field: End Month

This numeric field should contain the ending month number (1=January, 2=February, etc.) for the annual run period desired.

#### Field: End Day of Month

This numeric field should contain the ending day of the ending month (must be valid for month) for the annual run period desired.

#### Field: Day of Week for Start Day

For flexibility, the day of week indicated on the weather file can be overridden by this field's value. Valid days of the week (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday) or special days (SummerDesignDay, WinterDesignDay, CustomDay1, CustomDay2) must be entered in this field. To clarify, this value will be used as the Start Day (type) for this sizing period. When weekdays are used, each subsequent day in the period will be incremented.  When SummerDesignDay, WinterDesignDay, CustomDay1, CustomDay2 – are used, the day type will be applied for the entire period.

#### Field: Use Weather File Daylight Saving Period

Weather files can contain indicators of Daylight Saving Period days. For flexibility, you may want to ignore these designations on the weather file. This field should contain the word **Yes** if you will accept daylight saving period days as contained on the weather file (note: not all weather files may actually have this period) or **No** if you wish to ignore Daylight Saving Period days that may be on the weather file.

Note that a blank or null field in this field will indicate **Yes**.

#### Field: Use Weather File Rain and Snow Indicators

Weather files can contain "rain" and "snow" indicators. (EPW field "Present Weather Codes" – described in the AuxiliaryPrograms document). In turn, rain indicates wet surfaces which changes the film convection coefficient for the surface. Other models may use rain as well (Ground Heat Exchangers). Snow indicators can change the ground reflectance if there is snow on the ground. Entering "**Yes**" in this field allows the weather file conditions to represent "Rain" and "Snow";  entering "**No**" in the field "turns off" the rain/snow indicators for this period. You might use this to be able to compare two "same location" weather files of different years, origins, etc.

IDF Example:

~~~~~~~~~~~~~~~~~~~~

      SizingPeriod:WeatherFileDays,
        Summer including Extreme Summer days,  !- Name
        7,18,7,25,                             !- Begin/end Day/Month
        SummerDesignDay,                       !- Day type
        No,                          !- Use Weather File Daylight Saving Period
        No;                          !- Use Weather File Rain and Snow Indicators

      SizingPeriod:WeatherFileDays,
        Winter including Extreme Winter days,  !- Name
        1,25,2,1,                              !- Begin/end Day/Month
        WinterDesignDay,                       !- Day type
        No,                          !- Use Weather File Daylight Saving Period
        No;                          !- Use Weather File Rain and Snow Indicators
~~~~~~~~~~~~~~~~~~~~

## SizingPeriod:WeatherFileConditionType

When the EPW files are created, a heuristic procedure identifies extreme and typical periods in the actual weather file.  This object will allow one of those periods to be selected for sizing or load calculations (typically). Multiple objects may be input. While this object may be used for sizing calculations, you should also consider using design days that represent more long term extremes or conditions.

### Inputs

#### Field: Name

This field allows for an assigned name for this run period so it can be tracked easily in sizing and other outputs.

#### Field: Period Selection

This field allows the generic period calculated from the weather file to be selected for this run period.  It is not completely generic as there may be extreme cold periods in some weather files but extreme wet periods (tropical) in others. Not all weather files have all of the valid choices. The choices for this field are:

- SummerExtreme
- SummerTypical
- WinterExtreme
- WinterTypical
- AutumnTypical
- SpringTypical
- WetSeason
- DrySeason
- NoDrySeason
- NoWetSeason
- TropicalHot
- TropicalCold

#### Field: Day of Week for Start Day

For flexibility, the day of week indicated on the weather file can be overridden by this field's value. Valid days of the week (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday) or special days (SummerDesignDay, WinterDesignDay, CustomDay1, CustomDay2) must be entered in this field. To clarify, this value will be used as the Start Day (type) for this sizing period. When weekdays are used, each subsequent day in the period will be incremented.  When SummerDesignDay, WinterDesignDay, CustomDay1, CustomDay2 – are used, the day type will be applied for the entire period.

#### Field: Use Weather File Daylight Saving Period

Weather files can contain indicators of Daylight Saving Period days. For flexibility, you may want to ignore these designations on the weather file. This field should contain the word **Yes** if you will accept daylight saving period days as contained on the weather file (note: not all weather files may actually have this period) or **No** if you wish to ignore Daylight Saving Period days that may be on the weather file.

Note that a blank or null field in this field will indicate **Yes**.

#### Field: Use Weather File Rain and Snow Indicators

Weather files can contain "rain" and "snow" indicators. (EPW field "Present Weather Codes" – described in the AuxiliaryPrograms document). In turn, rain indicates wet surfaces which changes the film convection coefficient for the surface. Other models may use rain as well (Ground Heat Exchangers). Snow indicators can change the ground reflectance if there is snow on the ground. Entering "**Yes**" in this field allows the weather file conditions to represent "Rain" and "Snow";  entering "**No**" in the field "turns off" the rain/snow indicators for this period. You might use this to be able to compare two "same location" weather files of different years, origins, etc.

IDF Example:

~~~~~~~~~~~~~~~~~~~~

      SizingPeriod:WeatherFileConditionType,
        Extreme Summer Weather Period for Design,  !- Name
        SummerExtreme,                             !- Period Selection
        SummerDesignDay,                           !- Day Type
        No,                          !- Use Weather File Daylight Saving Period
        No;                          !- Use Weather File Rain and Snow Indicators

      SizingPeriod:WeatherFileConditionType,
        Extreme Winter Weather Period for Design,  !- Name
        WinterExtreme,                             !- Period Selection
        WinterDesignDay,                           !- Day Type
        No,                          !- Use Weather File Daylight Saving Period
        No;                          !- Use Weather File Rain and Snow Indicators
~~~~~~~~~~~~~~~~~~~~

## RunPeriod

The [RunPeriod](#runperiod) object describes the elements necessary to create a weather file simulation. Multiple run periods may be input. EnergyPlus accepts weather files in the special EnergyPlus weather format (described briefly below this document and in more detail in the Auxiliary Programs document). These files can describe Daylight Saving Time periods as well as holidays within their definitions. The [RunPeriod](#runperiod) object allows the user to override the use of both the Daylight Saving Period (i.e. use or ignore) and holidays that are embedded within the weather file. Note that the weather file also may contain design condition information, typical and extreme period information, ground temperatures based on air temperature calculations.

### Inputs

#### Field: Name

This optional field allows the [RunPeriod](#runperiod) to be named for output reporting. When left blank, the weather file location name is used. Note that the weather file location name will be appened to this name in tabular/summary reports.

#### Field: Begin Month

This numeric field should contain the starting month number (1=January, 2=February, etc.) for the annual run period desired.

#### Field: Begin Day of Month

This numeric field should contain the starting day of the starting month (must be valid for month) for the annual run period desired.

#### Field: End Month

This numeric field should contain the ending month number (1=January, 2=February, etc.) for the annual run period desired.

#### Field: End Day of Month

This numeric field should contain the ending day of the ending month (must be valid for month) for the annual run period desired.

#### Field: Day of Week for Start Day

For flexibility, the day of week indicated on the weather file can be overridden by this field's value. Valid days of the week (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday) must be entered in this field. To clarify, this value will be used as the Start Day (type) for this run period and subsequent days will be implemented. If a blank or UseWeatherFile is entered here, then the starting day type will be calculated from the weather file (ref: Auxiliary Programs document about Data Periods).

#### Field: Use Weather File Holidays and Special Days

Weather files can contain holiday designations or other kinds of special days. These day types cause a corresponding day's schedule (see SCHEDULE definitions below) to be used during that day. This field should contain the word **Yes** if holidays or other special days indicated directly on the weather file should retain their "day type" or **No** if holidays or other special days on the weather file should be ignored. Reference the [RunPeriodControl:SpecialDays](#runperiodcontrolspecialdays) object below to enter your own special days and holidays.

Note that a blank or null field in this field will indicate **Yes**.

#### Field: Use Weather File Daylight Saving Period

Weather files can contain indicators of Daylight Saving period days. For flexibility, you may want to ignore these designations on the weather file. This field should contain the word **Yes** if you will accept daylight saving period days as contained on the weather file (note: not all weather files may actually have this period) or **No** if you wish to ignore Daylight Saving period days that may be on the weather file.

Note that a blank or null field in this field will indicate **Yes**.

#### Field: Apply Weekend Holiday Rule

In some countries (notably the US), when holidays fall on weekends, they are often observed on a weekday close to the holiday day. (Usually if the specific day falls on a Saturday, the observed day is Friday; if on a Sunday, the observed day is Monday). EnergyPlus will represent this custom using the value in this field. If the field is **Yes**, then specific date holidays that have a duration of one day, will be "observed" on the Monday after the day. (Specific day holidays are such as January 1 – a day-month combination).  If the field is blank or **No**, then the holiday will be shown on the day-month as entered. As this option is listed at the [RunPeriod](#runperiod), all applicable special days for the run will use the rule – there is no override for individual special days.

Note that a blank or null field in this field will indicate **No**.

> Note: EnergyPlus processed weather files available on the EnergyPlus web site: http://www.energyplus.gov/cfm/weather_data.cfm have neither special days specified nor daylight saving period. However, DDY (Design Day) files produced from the ASHRAE Design Conditions that accompany the EPW files may include a DaylightSavingPeriod object for certain locations.

#### Field: Use Weather File Rain Indicators

Weather files can contain "rain" indicators. (EPW field "Present Weather Codes" – described in the AuxiliaryPrograms document). In turn, rain indicates wet surfaces which changes the film convection coefficient for the surface. Other models may use rain as well (Ground Heat Exchangers). Entering "**Yes**" in this field allows the weather file conditions to represent "Rain";  entering "**No**" in the field "turns off" the rain indicator for this period. You might use this to be able to compare two "same location" weather files of different years, origins, etc.

#### Field: Use Weather File Snow Indicators

Weather files can contain "snow" indicators. (EPW field "Snow Depth" > 0 indicates "Snow on the ground"). In turn, snow changes the reflectivity of the ground and cascades changes of this reflectivity. Entering "**Yes**" in this field allows the weather file conditions to represent "Snow";  entering "**No**" in the field "turns off" the snow indicator for this period. You might use this to be able to compare two "same location" weather files of different years, origins, etc.

#### Field: Number of Times Runperiod to be Repeated

This numeric field represents the number of times (usually years) the simulation has to be carried out in a multi runperiod simulation. The default value is set to 1. The number of years of simulation, in case of a Ground Loop Heat Exchanger (GLHE) simulation, should be equal to the length of simulation field in GLHE object. Note that you can specify a number of simulation years with a shorter run period (e.g. 1 week) and EnergyPlus will repeat the simulation of the shorter run period for that many times. Note that repeating will work with any weather file that contains the start date and end date in a "Data Period" (refer to Auxiliary Programs document for more documentation on weather file contents).

#### Field: Increment Day of Week on Repeat

When repeating RunPeriods (see previous field), you can choose to increment the day of week at the end of the period to have a more continuous day type. Enter **Yes** to increment the day of the week (default). An entry of **No** will keep the same day of week values as in the first time through.

#### Field: Start Year

When repeating a period, you might (for whatever reason), choose to have a specific year as a starting year. Enter an appropriate 4 digit year in this field. Refer to Auxiliary Programs document for more documentation on weather file contents, particularly in using leap year data (or not) within weather files.

And, as shown in an IDF:

~~~~~~~~~~~~~~~~~~~~

     RunPeriod,  ! Winter Simulation
        Winter Simulation,  !- Name
        12,  !- Begin Month
        1,  !- Begin Day of Month
        3,  !- End Month
        31,  !- End Day of Month
        UseWeatherFile,  !- Day of Week for Start Day
        Yes,  !- Use Weather File Holidays and Special Days
        Yes,  !- Use Weather File Daylight Saving Period
        No,  !- Apply Weekend Holiday Rule
        Yes,  !- Use Weather File Rain Indicators
        Yes;  !- Use Weather File Snow Indicators

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

     ! Multiple year simulation example
     RunPeriod,
        Multiple Years,  !- Name
        1,    !- Begin Month
        1,    !- Begin Day of Month
        12,   !- End Month
        31,   !- End Day of Month
        UseWeatherFile,  !- Day of Week for Start Day
        Yes,  !- Use Weather File Holidays and Special Days
        Yes,  !- Use Weather File Daylight Saving Period
        No,   !- Apply Weekend Holiday Rule
        Yes,  !- Use Weather File Rain Indicators
        Yes,  !- Use Weather File Snow Indicators
         3;   !- Number of Times Runperiod to be Repeated
~~~~~~~~~~~~~~~~~~~~

## RunPeriod:CustomRange

The [RunPeriod:CustomRange](#runperiodcustomrange) object describes the elements necessary to use a specially crafted (likely multiple year) weather file in a simulation. These kinds of weather files and simulations might be useful for matching utility periods or simulating several years of differing weather data. Multiple run periods may be input. EnergyPlus accepts weather files in the special EnergyPlus weather format (described briefly below this document and in more detail in the Auxiliary Programs document). In order to effectively use this object, however, you will need to use a text editor (EnergyPlus weather files area simple text files) or possibly a spreadsheet program and then save to a csv (comma separated variable) file. Weather files can describe Daylight Saving Time periods as well as holidays within their definitions. The [RunPeriod:CustomRange](#runperiodcustomrange) object allows the user to override the use of both the Daylight Saving Period (i.e. use or ignore) and holidays that are embedded within the weather file. Note that the weather file also may contain design condition information, typical and extreme period information, ground temperatures based on air temperature calculations.

### Inputs

#### Field: Name

This optional field allows the [RunPeriod](#runperiod) to be named for output reporting. When left blank, the weather file location name is used. Note that the weather file location name will be appened to this name in tabular/summary reports.

#### Field: Begin Month

This numeric field should contain the starting month number (1=January, 2=February, etc.) for the annual run period desired.

#### Field: Begin Day of Month

This numeric field should contain the starting day of the starting month (must be valid for month) for the annual run period desired.

#### Field: Begin Year

This numeric field should contain the beginning year for the custom range. Though all EnergyPlus (EPW) weather files contain a year/year field, the [RunPeriod](#runperiod) object does not use this field (previous object). The **RunPeriod:CustomRange** object causes the program to look specifically for the begin date specified by these three fields (i.e. Begin Month, Begin Day of Month, Begin Year).

#### Field: End Month

This numeric field should contain the ending month number (1=January, 2=February, etc.) for the annual run period desired.

#### Field: End Day of Month

This numeric field should contain the ending day of the ending month (must be valid for month) for the annual run period desired.

#### Field: End Year

This numeric field should contain the end year for the custom range. Though all EnergyPlus (EPW) weather files contain a year/year field, the [RunPeriod](#runperiod) object does not use this field (previous object). The **RunPeriod:CustomRange** object causes the program to look specifically for the end date specified by these three fields (i.e. End Month, End Day of Month, End Year).

#### Field: Day of Week for Start Day

For flexibility, the day of week indicated on the weather file can be overridden by this field's value. Valid days of the week (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday) must be entered in this field. To clarify, this value will be used as the Start Day (type) for this run period and subsequent days will be implemented. If a blank or UseWeatherFile is entered here, then the starting day type will be calculated from the weather file (ref: Auxiliary Programs document about Data Periods).

#### Field: Use Weather File Holidays and Special Days

Weather files can contain holiday designations or other kinds of special days. These day types cause a corresponding day's schedule (see SCHEDULE definitions below) to be used during that day. This field should contain the word **Yes** if holidays or other special days indicated directly on the weather file should retain their "day type" or **No** if holidays or other special days on the weather file should be ignored. Reference the [RunPeriodControl:SpecialDays](#runperiodcontrolspecialdays) object below to enter your own special days and holidays.

Note that a blank or null field in this field will indicate **Yes**.

#### Field: Use Weather File Daylight Saving Period

Weather files can contain indicators of Daylight Saving period days. For flexibility, you may want to ignore these designations on the weather file. This field should contain the word **Yes** if you will accept daylight saving period days as contained on the weather file (note: not all weather files may actually have this period) or **No** if you wish to ignore Daylight Saving period days that may be on the weather file.

Note that a blank or null field in this field will indicate **Yes**.

#### Field: Apply Weekend Holiday Rule

In some countries (notably the US), when holidays fall on weekends, they are often observed on a weekday close to the holiday day. (Usually if the specific day falls on a Saturday, the observed day is Friday; if on a Sunday, the observed day is Monday). EnergyPlus will represent this custom using the value in this field. If the field is **Yes**, then specific date holidays that have a duration of one day, will be "observed" on the Monday after the day. (Specific day holidays are such as January 1 – a day-month combination).  If the field is blank or **No**, then the holiday will be shown on the day-month as entered. As this option is listed at the [RunPeriod](#runperiod), all applicable special days for the run will use the rule – there is no override for individual special days.

Note that a blank or null field in this field will indicate **No**.

> Note: EnergyPlus processed weather files available on the EnergyPlus web site: http://www.energyplus.gov/cfm/weather_data.cfm have neither special days specified nor daylight saving period. However, DDY (Design Day) files produced from the ASHRAE Design Conditions that accompany the EPW files may include a DaylightSavingPeriod object for certain locations.

#### Field: Use Weather File Rain Indicators

Weather files can contain "rain" indicators. (EPW field "Present Weather Codes" – described in the AuxiliaryPrograms document). In turn, rain indicates wet surfaces which changes the film convection coefficient for the surface. Other models may use rain as well (Ground Heat Exchangers). Entering "**Yes**" in this field allows the weather file conditions to represent "Rain";  entering "**No**" in the field "turns off" the rain indicator for this period. You might use this to be able to compare two "same location" weather files of different years, origins, etc.

#### Field: Use Weather File Snow Indicators

Weather files can contain "snow" indicators. (EPW field "Snow Depth" > 0 indicates "Snow on the ground"). In turn, snow changes the reflectivity of the ground and cascades changes of this reflectivity. Entering "**Yes**" in this field allows the weather file conditions to represent "Snow";  entering "**No**" in the field "turns off" the snow indicator for this period. You might use this to be able to compare two "same location" weather files of different years, origins, etc.

## RunPeriodControl:SpecialDays

For weather file run periods, special day run periods can be described. These will always be in effect for the selected days in the run period. Depending on the Use Special Days value in the RunPeriod:\* object(s), these can augment any special days included on the weather file.

> Note: EnergyPlus processed weather files available on the EnergyPlus web site: http://www.energyplus.gov/cfm/weather_data.cfm have neither special days specified nor daylight saving period. However, DDY (Design Day) files produced from the ASHRAE Design Conditions that accompany the EPW files may include a DaylightSavingPeriod object for certain locations.

### Inputs

#### Field: Name

This alpha field is the title for the special day period. It must be unique among all the special day period objects entered.

#### Field: Start Date

This field is the starting date for the special day period. Dates in this field can be entered in several ways as shown in the accompanying table:

Table: Date Field Interpretation

Field Contents|Interpretation
--------------|--------------
<number> / <number>|Month / Day
<number> Month|Day and Month
Month <number>|Day and Month
<number> Weekday in Month|Numbered weekday of month
Last Weekday In Month|Last weekday of month

In the table, Month can be one of (January, February, March, April, May, June, July, August, September, October, November, December). Abbreviations of the first three characters are also valid.

In the table, Weekday can be one of (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday). Abbreviations of the first three characters are also valid.

#### Field: Duration

This numeric field specifies how long (number of days) the special day period lasts.

#### Field: Special Day Type

This alpha field designates the "day type" for schedule use during the special period. It must be one of (Holiday, SummerDesignDay, WinterDesignDay, CustomDay1, CustomDay2).

An example in the IDF would be:

~~~~~~~~~~~~~~~~~~~~

    RunPeriodControl:SpecialDays, President's Day, 3rd Monday in February,1,Holiday;
    RunPeriodControl:SpecialDays, Thanksgiving, 4th Thursday in November, 1,Holiday;
    RunPeriodControl:SpecialDays, Halloween, 10/31, 1, Holiday;
    RunPeriodControl:SpecialDays, Vacation, 5/1, 14, CustomDay1;
~~~~~~~~~~~~~~~~~~~~

## RunPeriodControl:DaylightSavingTime

Similar to a special day period, a daylight saving period may be entered to be applied to weather file run periods. These will always be in effect, regardless of the value entered on the [RunPeriod](#runperiod) object. Note that this period will always override any daylight saving period specified in a weather file.

> Note: EnergyPlus processed weather files available on the EnergyPlus web site: http://www.energyplus.gov/cfm/weather_data.cfm have neither special days specified nor daylight saving period.

> **Note:  For EnergyPlus [Output:Variable](#outputvariable) and [Output:Meter](#outputmeter-and-outputmetermeterfileonly) reporting, the time stamps are always in standard time. When daylight saving time is active, scheduled loads and controls will shift one hour relative to standard time.**

### Inputs

#### Field: Start Date

This is the starting date of the daylight saving period. Note that it can be entered in several formats as shown in Table 3. Date Field Interpretation.

#### Field: End Date

This is the ending date of the daylight saving period. Note that it can be entered in several formats as shown in Table 3. Date Field Interpretation.

And in the IDF:

~~~~~~~~~~~~~~~~~~~~

    ! U.S. Standard for Daylight Saving
    RunPeriodControl:DaylightSavingTime,2nd Sunday in March, 1st Sunday in November; !2007
    ! Brazil standard
    RunPeriodControl:DaylightSavingTime,1st Sunday in October, Last Sunday in February;
    ! European Standard
    RunPeriodControl:DaylightSavingTime, Last Sunday in March, Last Sunday in October;
    ! Syria Standard
    RunPeriodControl:DaylightSavingTime, 4/1, 10/1;
~~~~~~~~~~~~~~~~~~~~

Of course, these could not all appear in the same IDF as only one DaylightSavingPeriod object per input file is allowed. More information on Daylight Saving Periods can be seen on the web at: http://www.webexhibits.org/daylightsaving/  The ASHRAE Handbook of Fundamentals [ASHRAE 2005] also contains information about daylight saving periods and their climatic information now includes start and end dates for many locations.

## WeatherProperty:SkyTemperature

Sky Temperature, or radiative sky temperature, is internally calculated by EnergyPlus using an algorithm using horizontal infrared radiation from sky, cloudiness factors and current temperaure. The algorithm is fully described in the Engineering Reference document. For flexibility, the following object can be entered to override the internal calculations. Much of the literature describes the sky temperature as relative to either drybulb or dewpoint temperature.

### Inputs

#### Field: Name

This name references an existing design period (i.e., [SizingPeriod:DesignDay](#sizingperioddesignday), [SizingPeriod:WeatherFileDays](#sizingperiodweatherfiledays), or [SizingPeriod:WeatherFileConditionType](#sizingperiodweatherfileconditiontype)) or run period (by name or blank for all run periods).

#### Field: Calculation Type

Allowable entries here are: **ScheduleValue**, **DifferenceScheduleDryBulbValue**, or **DifferenceScheduleDewPointValue**. In each case the following field must specify a valid schedule name. **ScheduleValue** – the values in the schedule are used as the sky temperature. **DifferenceScheduleDryBulbValue** – the values in the schedule are used as a difference to the drybulb temperature value (+values would then be greater than the drybulb temperature, -values would then be less than the drybulb temperature) for the resulting sky temperature value. **DifferenceScheduleDewPointValue** – the values in the schedule are used as a difference to the dewpoint temperature value (+values would then be greater than the dewpoint temperature, -values would then be less than the dewpoint temperature) for the resulting sky temperature value.

#### Field: ScheduleName

This field specifies a schedule name to accomplish the sky temperature calculation from the previous field. A Schedule:Day:\* (i.e., [Schedule:Day:Hourly](#scheduledayhourly), [Schedule:Day:Interval](#scheduledayinterval), [Schedule:Day:List](#scheduledaylist)) should be specified if the name in the name field matches a [SizingPeriod:DesignDay](#sizingperioddesignday) object. If the name is one of the weather file period specifications (i.e. matches a [SizingPeriod:WeatherFileDays](#sizingperiodweatherfiledays), [SizingPeriod:WeatherFileConditionType](#sizingperiodweatherfileconditiontype) or [RunPeriod](#runperiod)), then the schedule name must match a full year schedule (i.e. [Schedule:Year](#scheduleyear), [Schedule:Compact](#schedulecompact), [Schedule:File](#schedulefile), or [Schedule:Constant](#scheduleconstant)).

An example of IDF usage (with DesignDay):

~~~~~~~~~~~~~~~~~~~~

      SizingPeriod:DesignDay,
        DENVER_STAPLETON Ann Clg 1% Sky Temperature modfier,  !- Name
        32.6,               !- Maximum Dry-Bulb Temperature {C}
        15.2,               !- Daily Temperature Range {deltaC}
        15.5,               !- Humidity Indicating Conditions at Maximum Dry-Bulb
        83411.,                  !- Barometric Pressure {Pa}
        4,                       !- Wind Speed {m/s}
        120,                     !- Wind Direction {deg}
        1.00,                    !- Sky Clearness
        0,                       !- Rain Indicator
        0,                       !- Snow Indicator
        25,                      !- Day of Month
        7,                       !- Month
        SummerDesignDay,         !- Day Type
        0,                       !- Daylight Saving Time Indicator
        WetBulb,                 !- Humidity Indicating Type
        ,                        !- Relative Humidity Day Schedule Name
        deltaschedule,           !- Dry-Bulb Temperature Range Modifier Type
        temp range deltas; !-   Dry-Bulb Temperature Range Modifier Schedule Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      WeatherProperty:SkyTemperature,
        DENVER_STAPLETON Ann Clg 1% Sky Temperature modfier,  !- Name
        ScheduleValue,           !- Calculation Type
        DaySchedule5;            !- Schedule Name

      Schedule:Day:Interval,
        DaySchedule5,            !- Name
        Temperature,             !- Schedule Type Limits Name
        Yes,                     !- Interpolate to Timestep
        until: 24:00,            !- Time 1
        5;                       !- Value Until Time 1
~~~~~~~~~~~~~~~~~~~~

## Site:WeatherStation

The [Site:WeatherStation](#siteweatherstation) object is used to specify the measurement conditions for the climatic data listed in the weather file. These conditions indicate the height above ground of the air temperature sensor, the height above ground of the wind speed sensor, as well as coefficients that describe the wind speed profile due to the terrain surrounding the weather station. There are necessary correlations between the entries for this object and some entries in the [Building](#building) object, specifically the **Terrain** field.

Weather stations throughout the world (ref: WMO – World Meteorological Organization) take their measurements at standard conditions:

Air temperature is measured at approximately 1.5 m above ground

Wind speed is measured at 10 m above ground

Weather station is in a flat, open field with little protection from the wind.

When using weather data from standard sources (e.g., TMY2, IWEC, TMY, or ASHRAE design day data), it is not necessary to use the **Site:WeatherStation** object. However, if you are using custom weather data or real-time weather data, you may need to read and understand the concepts in the **Site:WeatherStation** object.

The measurement conditions at the weather station (i.e., the weather file) are used by EnergyPlus in conjunction with the *Terrain* field of the **Building** object, or optionally with the **Site:HeightVariation** object (see below), to calculate the local variation in atmospheric properties as a function of height above ground. Outdoor air temperature decreases with height, while wind speed increases with height. The algorithms for this calculation are in the Engineering Reference.

The **Site:WeatherStation** object is useful when working with a custom weather file that includes data that were not measured at the WMO standard conditions. For example, the weather data could be measured on site, or on the roof top of a nearby building. The wind speed profile coefficients can be estimated from the table below or calculated beforehand using more sophisticated techniques such as CFD modeling of the weather station terrain.

Table: Wind Speed Profile Coefficients (ASHRAE Fundamentals 2005).

**Terrain Description**|**Exponent**|**Boundary Layer Thickness (m)**
------------------------------------|-------------------------|---------------------------------------------
Flat, open country|0.14|270
Rough, wooded country|0.22|370
Towns and cities|0.33|460
Ocean|0.10|210
Urban, industrial, forest|0.22|370

If the **Site:WeatherStation** object is omitted from the input file, the WMO standard measurement conditions are assumed.

### Inputs

#### Field: Wind Sensor Height Above Ground

The height [m] above ground for the wind speed sensor.

#### Field: Wind Speed Profile Exponent

The wind speed profile exponent for the terrain surrounding the weather station. The exponent can be estimated from the table above or calculated beforehand using more sophisticated techniques, such as CFD modeling of the weather station terrain.

#### Field: Wind Speed Profile Boundary Layer Thickness

The wind speed profile boundary layer thickness [m] for the terrain surrounding the weather station. The boundary layer can be estimated from the table above or calculated beforehand using more sophisticated techniques, such as CFD modeling of the weather station terrain.

#### Field: Air Temperature Sensor Height Above Ground

The height [m] above ground for the air temperature sensor.

For example, if you are using weather data measured on the top of your building, you should set the *Wind Sensor Height Above Ground* and the *Air Temperature Sensor Height Above Ground* to equal the height of your building (say 30 m). The *Wind Speed Profile Exponent* and *Wind Speed Profile Boundary Layer Thickness* should be set to match the values associated with the *Terrain* field of the **Building** object, or the equivalent fields of the **Site:HeightVariation** object.

Or, in IDF terms, with a building in a town or city:

~~~~~~~~~~~~~~~~~~~~

      Site:WeatherStation,
        30,  !- Wind Sensor Height Above Ground {m}
        0.33,  !- Wind Speed Profile Exponent {}
        460,  !- Wind Speed Profile Boundary Layer Thickness {m}
        30;  !- Air Temperature Sensor Height Above Ground {m}
~~~~~~~~~~~~~~~~~~~~

This would change if you had a different wind speed profile exponent or wind speed profile boundary layer thickness at your site.

## Site:HeightVariation

The [Site:HeightVariation](#siteheightvariation) object is used to specify the local variation in atmospheric properties at the site and should be used only if you require advanced control over the height-dependent variations for wind speed and temperature. The coefficients set by this object are used by EnergyPlus, in conjunction with the [Site:WeatherStation](#siteweatherstation) object (see above), to calculate the local variation in atmospheric properties as a function of height above ground. Outdoor air temperature decreases with height, while wind speed increases with height. The local outdoor air temperature and wind speed are calculated separately for all zones and surfaces, and optionally for outdoor air nodes for which a height has been specified (see [OutdoorAir:Node](#outdoorairnode) object). With the default inputs, wind speed falls significantly at heights lower than the weather station measurement height, and temperature increases slightly. The algorithms for this calculation are in the Engineering Reference. There are necessary correlations between the entries for this object and some entries in the [Building](#building) object, specifically the **Terrain** field.

Table: Atmospheric Variables at Two Different Heights Above Ground Level.

Variable|1.5 m|284 m|Absolute Diff|Percent Diff
--------|-----|-----|-------------|------------
Air Temperature|15°C|13.15°C|1.85°C|12.3%
Barometric Pressure|101,325 Pa|97,960 Pa|3,365 Pa|3.3%
Wind Speed|2.46 m/s|7.75 m/s|5.29 m/s|215%

Note that using this object overrides the wind speed profile coefficients implied by the *Terrain* field of the **Building** object even if the wind speed profile fields are left blank. The wind speed profile coefficients can be estimated from the table above (see **Site:WeatherStation**) or calculated beforehand using more sophisticated techniques such as CFD modeling of the site terrain.

### Inputs

#### Field: Wind Speed Profile Exponent

The wind speed profile exponent for the terrain surrounding the site. The exponent can be estimated from the table above (see **Site:WeatherStation**) or calculated beforehand using more sophisticated techniques, such as CFD modeling of the site terrain. Note that using this object overrides the wind speed profile coefficients implied by the *Terrain* field of the **Building** object even if this field is left blank.

#### Field: Wind Speed Profile Boundary Layer Thickness

The wind speed profile boundary layer thickness [m] for the terrain surrounding the site. The boundary layer can be estimated from the table above (see **Site:WeatherStation**) or calculated beforehand using more sophisticated techniques, such as CFD modeling of the site terrain. Note that using this object overrides the wind speed profile coefficients implied by the *Terrain* field of the **Building** object even if this field is left blank. This field can be set to zero to turn off all wind dependence on height.

#### Field: Air Temperature Gradient Coefficient

The air temperature gradient coefficient [K/m] is a research option that allows the user to control the variation in outdoor air temperature as a function of height above ground. The real physical value is 0.0065 K/m. This field can be set to zero to turn off all temperature dependence on height. Note that the *Air Temperature Sensor Height* in the **Site:WeatherStation** object should also be set to zero in order to force the local outdoor air temperatures to match the weather file outdoor air temperature. This change is required because the **Site:WeatherStation** object assumes an air temperature gradient of 0.0065 K/m.

~~~~~~~~~~~~~~~~~~~~

      Site:HeightVariation,
        0.22,  !- Wind Speed Profile Exponent
        370,  !- Wind Speed Profile Boundary Layer Thickness {m}
        0.0065;  !- Air Temperature Gradient Coefficient {deltaC/m}
~~~~~~~~~~~~~~~~~~~~

## Site:GroundTemperature:BuildingSurface

Ground temperatures are used for the ground heat transfer model. There can be only one ground temperature object included, and it is used as the outside surface temperature for all surfaces with Outside Boundary Condition=Ground. The object is options if you have no surfaces with ground contact. The outside surface temperature for individual surfaces can be specified using the OtherSideCoefficients (ref: [SurfaceProperty:OtherSideCoefficients](#surfacepropertyothersidecoefficients)) object that allows Toutside to be set with a schedule. This permits using any number of different outside face temperatures in addition to the ground temperature.

> Caution: The "undisturbed" ground temperatures calculated by the weather converter should not be used in building losses but are appropriate to be used in the [Site:GroundTemperature:Shallow](#sitegroundtemperatureshallow) and [Site:GroundTemperature:Deep](#sitegroundtemperaturedeep) objects. The reasoning (for building losses) is that these values are too extreme for the soil under a conditioned building. For best results, use the Slab or Basement program described in this document to calculate custom monthly average ground temperatures (see the Ground Heat Transfer section). This is especially important for residential applications and very small buildings. If one of these ground temperature preprocessors is not used, for typical commercial buildings in the USA, a reasonable default value is 2C less than the average indoor space temperature.

More information about determining appropriate ground temperatures is given in the Auxiliary Programs document.

### Inputs

#### Field: Month Temperature(s) – 12 fields in all

Each numeric field is the monthly ground temperature (degrees Celsius) used for the indicated month (January=1^st^ field, February=2^nd^ field, etc.)

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Site:GroundTemperature:BuildingSurface,19,20,20,20,20,20,20,20,20,20,20,20;
~~~~~~~~~~~~~~~~~~~~

## Site:GroundTemperature:Shallow

[Site:GroundTemperature:Shallow](#sitegroundtemperatureshallow) are used by the Surface Ground Heat Exchanger (i.e. object: [GroundHeatExchanger:Surface](#groundheatexchangersurface)). Only one shallow ground temperature object can be included.

> Note that the ground temperatures included in full year weather files may be suitable of being used for the values in these fields – namely, the .5 m depth temperatures that are calculated for "undisturbed" soil of "typical" conditions. However, you may wish to use some other change effect – based on the weather conditions of the building location.

### Inputs

#### Field: Month Temperature(s) – 12 fields in all

Each numeric field is the monthly surface ground temperature (degrees Celsius) used for the indicated month (January=1^st^ field, February=2^nd^ field, etc.)

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Site:GroundTemperature:Shallow,  4,4,6,6,10,10,15,15,14,14,8,8;
~~~~~~~~~~~~~~~~~~~~

## Site:GroundTemperature:Deep

[Site:GroundTemperature:Deep](#sitegroundtemperaturedeep) are used by the Pond Ground Heat Exchanger object (i.e. object: [GroundHeatExchanger:Pond](#groundheatexchangerpond)). Only one deep ground temperature object can be included.

> Note that the ground temperatures included in full year weather files may be suitable of being used for the values in these fields – namely, the 4 m depth temperatures that are calculated for "undisturbed" soil of "typical" conditions. However, you may wish to use some other change effect – based on the weather conditions or special knowledge of the building location.

### Inputs

#### Field: Month Temperature(s) – 12 fields in all

Each numeric field is the monthly deep ground temperature (degrees Celsius) used for the indicated month (January=1^st^ field, February=2^nd^ field, etc.)

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Site:GroundTemperature:Deep,  16,16,16,16,16,16,16,16,16,16,16,16;
~~~~~~~~~~~~~~~~~~~~

## Site:GroundDomain

This section documents the input object used to simulate ground coupled heat transfer with horizontal building surfaces within EnergyPlus. Horizontal ground surfaces within EnergyPlus interact with the [Site:GroundDomain](#sitegrounddomain) object by utilizing the [SurfaceProperty:OtherSideConditionsModel](#surfacepropertyothersideconditionsmodel) object. By utilizing this object, multiple horizontal surfaces can be coupled to the same [Site:GroundDomain](#sitegrounddomain) object. Each horizontal surface may also have its unique ground domain, however, runtime will be adversely affected.

Generally, there are two scenarios which [Site:GroundDomain](#sitegrounddomain) is equipped to model: in-grade slabs, and on-grade slabs.

![](media/image12.png)\


Figure 7In-grade configuration.

The in-grade slab option can be used to simulate situations when the upper slab surface is near the ground surface level. For this situation, slab's upper surface must interact with the zone via an OSCM boundary. Due to this, the FloorConstruction object for the zone floor must include a thin layer of the upper floor material. Horizontal and vertical insulation are modeled by the GroundDomain in this scenario. Horizontal insulation can be modeled as covering the full horizontal surface, or it can be limited to the perimeter regions only. In the latter case, the perimeter insulation width must be specified.

**![](media/image13.png)**

Figure 8 On-grade configuration

The on-grade slab option can be used to simulate situations when the lower slab surface is near the ground surface level. In this situation, the entire floor must be included within the floor construction object. Vertical insulation is modeled by the GroundDomain in this scenario.  Horizontal insulation can only be modeled as covering the full horizontal surface.

### Inputs

#### Field: Name

Alpha field used as a unique identifier for each ground domain.

#### Field: Ground Domain Depth

Numeric field used to determine the depth of the simulation domain, in meters.

#### Field: Aspect Ratio

Numeric field used to define the height to width ratio of the slab.

#### Field: Perimeter Offset

Numeric field used to determine the distance from the slab perimeter to the domain perimeter, in meters.

#### Field: Soil Thermal Conductivity

The thermal conductivity of the soil, in W/m-K.

#### Field: Soil Density

The bulk density of the soil, in kg/m3.

#### Field: Soil Specific Heat

The specific heat of dry soil, in J/kg-K. If moisture is defined in this object, moisture and freezing effects are accounted for by varying the specific heat value.

#### Field: Soil Moisture Content Volume Fraction

A nominal value of soil moisture content to be used when evaluating soil thermal properties.

#### Field: Soil Moisture Content Volume Fraction at Saturation

A nominal value of soil moisture content when the soil is saturated, this is used in evaluating thermal properties of freezing soil.

#### Field: Kusuda-Achebach Average Surface Temperature

The annual average surface temperature to be applied to the Kusuda-Achenbach farfield boundary temperature correlation, in °C

#### Field: Kusuda-Achebach Average Amplitude of Surface Temperature

The annual mean surface temperature variation from average used in determining the farfield boundary conditions.

#### Field: Kusuda-Achenbach Phase Shift of Minimum Surface Temperature

The phase shift of minimum surface temperature, or the day of the year when the minimum surface temperature occurs.

#### Field: Evapotranspiration Ground Cover Parameter

Numeric field specifies the ground cover effects used in the evapotranspiration model at the ground surface heat balance. The values range from 0 (solid, non-permeable ground surface) to 1.5 (wild growth).

#### Field: Slab Boundary Condition Model Name

This is the name of the other side boundary condition model used.

#### Field: Slab Location

Alpha field indicates whether the slab is in-grade (top surface level with ground surface) or on-grade (bottoms surface level with ground surface). Options include "ONGRADE" and "INGRADE".

#### Field: Slab Material Name

Name of the material object representing the slab material. Only applicable to in-grade situations.

#### Field: Horizontal Insulation

Alpha field indicates whether horizontal insulation is present. Options include "YES" and "NO". Only applicable to in-grade situations.

#### Field: Horizontal Insulation Material Name

Name of material object representing the horizontal slab insulation. Optional argument only required if horizontal insulation is present.

#### Field: Horizontal Insulation Extents

Alpha field indicates whether the horizontal slab insulation extends to cover the full horizontal area of the slab, or only covers the slab perimeter. Optional argument only required if horizontal insulation is present. Options include "FULL" and "PERIMETER".

#### Field: Perimeter Insulation Width

Numeric field indicating the width of the perimeter insulation measured from the slab edge. Valid range from > 0 to < half of smallest slab width.

#### Field: Vertical Insulation

Alpha field indicates whether vertical insulation is present. Options include "YES" and "NO".

#### Field: Vertical Insulation Name

Name of material object representing the vertical slab insulation. Optional argument only required if vertical insulation is present.

#### Field: Vertical Insulation Depth

Numeric field indicates the depth measured in meters from the ground surface to which the vertical perimeter insulation extends. Valid range from > Slab Thickness to < Domain Depth.

#### Field: Simulation Timestep

Alpha field indicating whether the domain will update temperatures at each zone timestep, or at hourly intervals. Options include "timestep" and "hourly".

An IDF example of an in-grade slab.

[Site:GroundDomain](#sitegrounddomain),
    IngradeCoupledSlab, !- Name
    5,                  !- Ground Domain Depth
    1,                  !- Aspect Ratio
    5,                  !- Domain Perimeter Offset
    1.8,                !- Soil Thermal Conductivity
    3200,               !- Soil Density
    836,                !- Soil Specific Heat
    30,   !- Soil Moisture Content Volume Fraction
    50,   !- Soil Moisture Content Volume Fraction at Saturation
    15.5, !- Kusuda-Achenbach Average Surface Temperature
    12.8, !- Kusuda-Achenbach Average Amplitude of Surface Temperature
    17.3, !- Kusuda-Achenbach Phase Shift of Minimum Surface Temperature
    1,    !- Evapotranspiration Ground Cover Parameter
    GroundCoupledOSCM,      !- Name of Floor Boundary Condition Model
    InGrade,                !- Slab Location (InGrade/OnGrade)
    Slab [Material](#material-and-material-properties)-In-grade, !- Slab [Material](#material-and-material-properties) Name
    Yes,                    !- Horizontal Insulation
    Slab Insulation,    !- Horizontal Insulation [Material](#material-and-material-properties) Name
    Perimeter,          !- Full Horizontal or Perimeter Only
    1,                  !- Perimeter insulation width
    Yes,                !- Vertical Insulation
    Slab Insulation,    !- Vertical Insulation Name
    2,                  !- Vertical perimeter insulation depth from surface
    Hourly;             !- Simulation timestep
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

And IDF example of an on-grade slab

[Site:GroundDomain](#sitegrounddomain),
    OngradeCoupledSlab, !- Name
    5,                  !- Ground Domain Depth {m}
    1,                  !- Aspect Ratio
    5,                  !- Domain Perimeter Offset {m}
    1.8,                !- Soil Thermal Conductivity {W/m-K}
    3200,               !- Soil Density {kg/m3}
    836,                !- Soil Specific Heat {J/kg-K}
    30,          !- Soil Moisture Content Volume Fraction
    50,          !- Soil Moisture Content Volume Fraction at Saturation
    15.5,        !- Kusuda-Achenbach Average Surface Temperature
    12.8,   !- Kusuda-Achenbach Average Amplitude of Surface Temperature
    17.3,   !- Kusuda-Achenbach Phase Shift of Minimum Surface Temperature
    1,           !- Evapotranspiration Ground Cover Parameter
    GroundCoupledOSCM,  !- Name of Floor Boundary Condition Model
    OnGrade,            !- Slab Location (InGrade/OnGrade)
    ,                   !- Slab [Material](#material-and-material-properties) Name
    ,                   !- Horizontal Insulation (Yes/No)
    ,                   !- Horizontal Insulation [Material](#material-and-material-properties) Name
    ,                   !- Full Horizontal or Perimeter Only
    ,                   !- Perimeter insulation width (m)
    Yes,                !- Vertical Insulation (Yes/No)
    Slab Insulation,    !- Vertical Insulation Name
    2,                  !- Vertical perimeter insulation depth from surface
    Hourly;             !- Simulation timestep. (Timestep/Hourly)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### Outputs

The following output variables are available.

[Zone](#zone), Average, [Zone](#zone) Coupled Surface Heat Flux [W/m2]
[Zone](#zone), Average, [Zone](#zone) Coupled Surface Temperature [C]
-------------------------------------------------------------------------------------------------------

#### Zone Coupled Surface Heat Flux [W/m2]

This is the value of the heat flux provided to the GroundDomain as a boundary condition which is determined by taking the average of all surfaces coupled to the domains OtherSideBoudaryCondition model.

#### Zone Coupled Surface Temperature [C]

This is the value of the OthersideConditionModel surface temperature. This is the temperature provided to the ground coupled surfaces as an outside boundary condition.

## Site:GroundTemperature:FCfactorMethod

[Site:GroundTemperature:FCfactorMethod](#sitegroundtemperaturefcfactormethod) is used only by the underground walls or slabs-on-grade or underground floors defined with C-factor ([Construction:CfactorUndergroundWall](#constructioncfactorundergroundwall)) and F-factor ([Construction:FfactorGroundFloor](#constructionffactorgroundfloor)) method for code compliance calculations where detailed construction layers are unknown. Only one such ground temperature object can be included. The monthly ground temperatures for this object are close to the monthly outside air temperatures delayed by three months. If user does not input this object in the IDF file, it will be defaulted to the 0.5m set of monthly ground temperatures from the weather file if they are available. Entering these will also overwrite any ground temperatures from the weather file in the F and C factor usage. If neither is available, an error will result.

### Inputs

#### Field: Month Temperature(s) – 12 fields in all

Each numeric field is the monthly ground temperature (degrees Celsius) used for the indicated month (January=1^st^ field, February=2^nd^ field, etc.)

And, the IDF example:

~~~~~~~~~~~~~~~~~~~~

    Site:GroundTemperature:FCfactorMethod,  9.5, 3.5, -0.7, -1.7, -0.6, 3.6, 9.3, 14, 18.2, 22.7, 21.2, 16.8;
~~~~~~~~~~~~~~~~~~~~

## Site:GroundReflectance

Ground reflectance values are used to calculate the ground reflected solar amount. This fractional amount (entered monthly) is used in this equation:

![](media/image14.png)\


Of course, the Ground Reflected Solar is never allowed to be negative. The ground reflectance can be further modified when snow is on the ground by the Snow Ground Reflectance Modifier. To use no ground reflected solar in your simulation, enter 0.0 for each month.

### Inputs

#### Field: Month Average Ground Reflectance(s) – 12 fields in all

Each numeric field is the monthly average reflectivity of the ground used for the indicated month (January=1^st^ field, February=2^nd^ field, etc.)

And use in an IDF:

~~~~~~~~~~~~~~~~~~~~

      Site:GroundReflectance,
         0.600,     !January Ground Reflectance
         0.600,     !February Ground Reflectance
         0.400,     !March Ground Reflectance
         0.300,     !April Ground Reflectance
         0.200,     !May Ground Reflectance
         0.200,     !June Ground Reflectance
         0.200,     !July Ground Reflectance
         0.200,     !August Ground Reflectance
         0.200,     !September Ground Reflectance
         0.200,     !October Ground Reflectance
         0.300,     !November Ground Reflectance
         0.400;     !December Ground Reflectance
~~~~~~~~~~~~~~~~~~~~

## Site:GroundReflectance:SnowModifier

It is generally accepted that snow resident on the ground increases the basic ground reflectance. EnergyPlus allows the user control over the snow ground reflectance for both "normal ground reflected solar" calculations (see above) and snow ground reflected solar modified for daylighting. These are entered under this object and both default to 1 (same as normal ground reflectance – no special case for snow which is a conservative approach).

### Inputs

#### Field: Ground Reflected Solar Modifier

This field is a decimal number which is used to modified the basic monthly ground reflectance when snow is on the ground (from design day input or weather data values).

![](media/image15.png)\


The actual Ground Reflectance is limited to [0.0,1.0].

#### Field: Daylighting Ground Reflected Solar Modifier

This field is a decimal number which is used to modified the basic monthly ground reflectance when snow is on the ground (from design day input or weather data values).

![](media/image16.png)\


The actual Ground Reflectance is limited to [0.0,1.0].

An IDF example:

~~~~~~~~~~~~~~~~~~~~

      Site:GroundReflectance:SnowModifier,
        1.0;                     !- Ground Reflected Solar Modifier
~~~~~~~~~~~~~~~~~~~~

Outputs will show both the inputs from the above object as well as monthly values for both Snow Ground Reflectance and Snow Ground Reflectance for Daylighting.

## Site:WaterMainsTemperature

The [Site:WaterMainsTemperature](#sitewatermainstemperature) object is used to calculate water temperatures delivered  by underground water main pipes. The mains temperatures are used as default, make-up water temperature inputs for several plant objects, including:  **WaterUse:Equipment, [WaterUse:Connections](#wateruseconnections), [WaterHeater:Mixed](#waterheatermixed)** and **WaterHeater:Stratified**. The mains temperatures are also used in the water systems objects to model the temperature of cold water supplies.

Water mains temperatures are a function of outdoor climate conditions and vary with time of year. A correlation has been formulated to predict water mains temperatures based on two weather inputs:

average annual outdoor air temperature (dry-bulb)

maximum difference in monthly average outdoor air temperatures

These values can be easily calculated from annual weather data using a spreadsheet or from the ".stat" file available with the EnergyPlus weather files at www.energyplus.gov. Monthly statistics for dry-bulb temperatures are shown with daily averages. The daily averages are averaged to obtain the annual average. The maximum and minimum daily average are subtracted to obtain the maximum difference. For more information on the water mains temperatures correlation, see the *EnergyPlus Engineering Document*.

Alternatively, the [Site:WaterMainsTemperature](#sitewatermainstemperature) object can read values from a schedule. This is useful for measured data or when water comes from a source other than buried pipes, e.g., a river or lake.

If there is no [Site:WaterMainsTemperature](#sitewatermainstemperature) object in the input file, a default constant value of 10 C is assumed.

### Inputs

#### Field: Calculation Method

This field selects the calculation method and must have the keyword Schedule or Correlation.

#### Field: Schedule Name

If the calculation method is Schedule, the water mains temperatures are read from the schedule referenced by this field. If the calculation method is Correlation, this field is ignored.

#### Field: Annual Average Outdoor Air Temperature

If the calculation method is Correlation, this field is used in the calculation as the annual average outdoor air temperature (dry-bulb) [C]. If the calculation method is Schedule, this field is ignored.

#### Field: Maximum Difference In Monthly Average Outdoor Air Temperatures 

If the calculation method is Correlation, this field is used in the calculation as the maximum difference in monthly average outdoor air temperatures [∆C]. If the calculation method is Schedule, this field is ignored.

~~~~~~~~~~~~~~~~~~~~

      Site:WaterMainsTemperature,
        Correlation,  !- Calculation Method {SCHEDULE | CORRELATION}
        ,  !- Schedule Name
        9.69,  !- Annual Average Outdoor Air Temperature {C}
        28.1;  !- Maximum Difference In Monthly Average Outdoor Air Temperatures           !- {deltaC}
~~~~~~~~~~~~~~~~~~~~

## Site:Precipitation

The [Site:Precipitation](#siteprecipitation) object is used to describe the amount of water precipitation at the building site over the course of the simulation run period. Precipitation includes both rain and the equivalent water content of snow. Precipitation is not yet described well enough in the many building weather data files. So this object can be used to provide the data using Schedule objects that define rates of precipitation in meters per hour.

A set of schedules for site precipitation have been developed for USA weather locations and are provided with EnergyPlus in the data set called PrecipitationSchedulesUSA.idf.  The user can develop schedules however they want. The schedules in the data set were developed using EnergyPlus' weather file (EPW) observations and the average monthly precipitation for the closest weather site provided by NOAA. EPW files for the USA that were based on TMY or TMY2 include weather observations for Light/Moderate/Heavy rainfall, however most international locations do not include these observations. The values were modeled by taking the middle of the ranges quoted in the EPW data dictionary. The assumed piecewise function is shown below.

![](media/image17.png)\


The values were inserted on hour by hour basis for the month based on the observations. Then each month was rescaled to meet the average precipitation for the month based on the 30-year average (1971-2000) provided by the NOAA/NCDC. Therefore, the flags in the EPW file match the precipitation schedules for the USA. Note that summing the average monthly precipitation values will not give you the average yearly precipitiation. The resulting value may be lower or higher than the average yearly value.

Once the typical rainfall pattern and rates are scheduled, the [Site:Precipitation](#siteprecipitation) object provides a method of shifting the total rainfall up or down for design purposes. Wetter or drier conditions can be modeled by changing the Design Annual Precipitation although the timing of precipitation throughout the year will not be changed.

### Inputs

#### Field: Precipitation Model Type

Choose rainfall modeling options. Only available option is ScheduleAndDesignLevel.

#### Field: Design Level for Total Annual Precipitation

Magnitude of total precipitation for an annual period to be used in the model. Value selected by the user to correspond with the amount of precipitation expected or being assumed for design purposes. The units are in meters. This field works with the following two fields to allow easily shifting the amounts without having to generate new schedules.

#### Field: Precipitation Rate Schedule Name

Name of a schedule defined elsewhere that describes the rate of precipitation. The precipitation rate schedule is analogous to weather file data. However, weather files for building simulation do not currently contain adequate data for such calculations. Therefore, EnergyPlus schedules are used to enter the pattern of precipitation events. The values in this schedule are the average rate of precipitation in meters per hour. The integration of these values over an annual schedule should equal the nominal annual precipitation.

#### Field: Average Total Annual Precipitation

Magnitude of annual precipitation associated with the rate schedule. This value is used to normalize the precipitation.

IDF example:

~~~~~~~~~~~~~~~~~~~~

    Site:Precipitation,
      ScheduledAndDesignLevel, !- Precipitation Model Type
      0.75,                    !- Design Level Total Annual Precipitation {m/yr}
      PrecipitationSchd,       !- Schedule Name for Precipitation Rates
      0.80771;                 !- Average Total Annual Precipitation {m/yr}
~~~~~~~~~~~~~~~~~~~~

## RoofIrrigation

The [RoofIrrigation](#roofirrigation) object is used to describe the amount of irrigation on the ecoroof surface over the course of the simulation runperiod. This object is used to provide irrigation data using Schedule objects that define rates of irrigation in meters per hour. These schedules can be one of two types: Schedule, or SmartSchedule.

### Inputs

#### Field: Irrigation Model Type

Choose irrigation modeling options. Available options are **Schedule** and **SmartSchedule**. The **Schedule** type is used to force an irrigation schedule regardless of the current moisture state of the soil. The **SmartSchedule** type allows the precipitation schedule to be overridden if the current moisture state of the soil is greater than 40% saturated.

#### Field: Irrigation Rate Schedule Name

Name of a schedule defined elsewhere that describes the rate of irrigation. The values in this schedule are the average rate of irrigation in meters per hour.

#### Field: Irrigation Maximum Saturation Threshold

Used with the SmartSchedule option in the Irrigation Model Type field to override the default 40% saturation limit for turning off the irrigation: values of 0 to 100 (percent) can be entered with 40% being the default.

IDF example:

~~~~~~~~~~~~~~~~~~~~

    RoofIrrigation,
      Schedule, !- Irrigation Model Type
      IrrigationSchd; !- Schedule Name for Irrigation Rates
~~~~~~~~~~~~~~~~~~~~

## Solar and Visible Spectrum Objects

The next two objects enable users to enter solar and visible spectrum which is used to calculate the thermal and visual performance of windows if their glazings are defined with full spectral data. EnergyPlus versions 8.0 and older hard-wired the solar and visible spectrum. The solar spectrum assumes air mass 1.5 terrestrial solar global spectral irradiance values (W/m2-micron) on a 37o tilted surface, based on ISO 9845-1 and ASTM E 892; derived from Optics5 data file ISO-9845GlobalNorm.std, 10-14-99. The visible/photopic spectrum is based on CIE 1931 observer; ISO/CIE 10527, CIE Standard Calorimetric Observers; derived from Optics5 data file "CIE 1931 Color Match from E308.txt", which is the same as WINDOW4 file Cie31t.dat.

## Site:SolarAndVisibleSpectrum

The SolarAndVisibleSpectrum object is used to specify the solar and visible spectrum data which is used as spectral weighting function to calculate the window performance (transmittance and absorptance) in EnergyPlus. This is a unique object, if it is missing from an IDF file, the default (same as EnergyPlus version 8.0) solar and visible spectrum data will be used.

### Inputs

#### Field: Name

This field specifies the name of the SolarAndVisibleSpectrum object.

#### Field: Spectrum Data Method

This field specifies the method used to enter the spectrum data. Two choices are available: Default and UserDefined. The choice Default will continue to use the hard-wired spectrum data in EnergyPlus (for backward compatibility). The choice UserDefined allows users to specify custom solar and visible spectrum data. The default choice is Default.

#### Field: Solar Spectrum Data Name

This field is required if the Spectrum Data Method is set to UserDefined. This field references a spectrum dataset for solar.

#### Field: Visible Spectrum Data Name

This field is required if the Spectrum Data Method is set to UserDefined. This field references a spectrum dataset for visible.

IDF example:

~~~~~~~~~~~~~~~~~~~~

    Site:SolarAndVisibleSpectrum,
      LocalSpectrum,             !- Name
      UserDefined,               !- Spectrum Data Method: Default, UserDefined
      SolarSpectrum,             !- Solar Spectrum Data Object Name
      VisibleSpectrum;           !- Visible Spectrum Data Object
~~~~~~~~~~~~~~~~~~~~

## Site:SpectrumData

The [Site:SpectrumData](#sitespectrumdata) object holds the user defined solar or visible spectrum data. For solar spectrum, up to 107 pairs of (wavelength, spectrum) can be entered. For visible spectrum, up to 81 pairs can be entered.

### Inputs

#### Field: Name

This field specifies the name of the SpectrumData object. The name must be unique across all SpectrumData objects.

#### Field: Spectrum Data Type

This field specifies the type of spectrum data. Choices are Solar and Visible.

#### Field: Wavelength <n>

This field specifies the nth wavelength in micron.

#### Field: Spectrum <n>

This field specifies the nth spectrum corresponding to the nth wavelength.

IDF example:

~~~~~~~~~~~~~~~~~~~~

    Site:SpectrumData,
      SolarSpectrum,             !- Name
      Solar,                     !- Spectrum Data Type
      0.3,0,                     !- up to 107 pair of (wavelength, spectrum)
      0.305,3.4,
      0.31,15.6,
      0.315,41.1,
      0.32,71.2,
      0.325,100.2,
      0.33,152.4,
      0.335,155.6,
      0.34,179.4,
      0.345,186.7,
      0.35,212,
      0.36,240.5,
      0.37,324,
      0.38,362.4,
      …;
~~~~~~~~~~~~~~~~~~~~

### Outputs

Climate related variables appear in two places for EnergyPlus outputs. Certain objects that are invariant throughout a simulation period have lines appear in the eplusout.eio file. For descriptions of this reporting, please see the Output Details and Examples document.

### Outputs

Variables related to ambient environment data are available at timestep and higher resolutions. Below is a variable dictionary of these variables and subsequent definitions:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Site Outdoor Air Drybulb Temperature [C]
    Zone,Average,Site Outdoor Air Dewpoint Temperature [C]
    Zone,Average,Site Outdoor Air Wetbulb Temperature [C]
    Zone,Average,Site Outdoor Air Humidity Ratio [kgWater/kgAir]
    Zone,Average,Site Outdoor Air Relative Humidity [%]
    Zone,Average,Site Outdoor Air Barometric Pressure [Pa]
    Zone,Average,Site Wind Speed [m/s]
    Zone,Average,Site Wind Direction [deg]
    Zone,Average,Site Sky Temperature [C]
    Zone,Average,Site Horizontal Infrared Radiation Rate per Area [W/m2]
    Zone,Average,Site Diffuse Solar Radiation Rate per Area [W/m2]
    Zone,Average,Site Direct Solar Radiation Rate per Area [W/m2]
    Zone,Sum,Site Precipitation Depth [m]
    Zone,Average,Site Ground Reflected Solar Radiation Rate per Area [W/m2]
    Zone,Average,Site Ground Temperature [C]
    Zone,Average,Site Surface Ground Temperature [C]
    Zone,Average,Site Deep Ground Temperature [C]
    Zone,Average,Site Simple Factor Model Ground Temperature [C]
    Zone,Average,Site Outdoor Air Enthalpy [J/kg]
    Zone,Average,Site Outdoor Air Density [kg/m3]
    Zone,Average,Site Solar Azimuth Angle [deg]
    Zone,Average,Site Solar Altitude Angle [deg]
    Zone,Average,Site Solar Hour Angle [deg]
    Zone,Average,Site Rain Status []
    Zone,Average,Site Snow on Ground Status []
    Zone,Average,Site Exterior Horizontal Sky Illuminance [lux]
    Zone,Average,Site Exterior Horizontal Beam Illuminance [lux]
    Zone,Average,Site Exterior Beam Normal Illuminance [lux]
    Zone,Average,Site Sky Diffuse Solar Radiation Luminous Efficacy [lum/W]
    Zone,Average,Site Beam Solar Radiation Luminous Efficacy [lum/W]
    Zone,Average,Site Daylighting Model Sky Clearness []
    Zone,Average,Sky Brightness for Daylighting Calculation []
    Zone,Average,Site Daylight Saving Time Status []
    Zone,Average,Site Day Type Index []
    Zone,Average,Site Mains Water Temperature [C]
    HVAC,Average,Site Precipitation Rate [m/s]
    HVAC,Sum,Site Precipitation Depth [m]
    HVAC,Sum,Water System Roof Irrigation Scheduled Depth[m]
    HVAC,Sum,Water System Roof Irrigation Actual Depth[m]
~~~~~~~~~~~~~~~~~~~~

> Note that these data values may be interpolated from "hour" points (ref: Weather Data Hourly Interpolation). Most of the data values represent the "average" over the reporting resolution period.

#### Site Outdoor Air Drybulb Temperature [C]

This is the outdoor dry-bulb temperature in degrees C.

#### Site Outdoor Air Dewpoint Temperature [C]

This is the outdoor dewpoint temperature in degrees C.

#### Site Outdoor Air Wetbulb Temperature [C]

The outdoor wet-bulb temperature is derived (at the timestep) from the values for dry-bulb temperature, humidity ratio and barometric pressure.

#### Site Outdoor Air Humidity Ratio [kgWater/kgAir]

The outdoor humidity ratio is derived (at the timestep) from the dry-bulb temperature, relative humidity and barometric pressure.

#### Site Outdoor Air Relative Humidity [%]

This is the outdoor relative humidity expressed in percent.

#### Site Outdoor Air Barometric Pressure [Pa]

This is the atmospheric/barometric pressure in Pa.

#### Site Wind Speed [m/s]

This is the outdoor wind speed in m/s.

#### Site Wind Direction [deg]

This is the wind direction (N=0, E=90, S=180, W=270).

#### Site Sky Temperature [C]

The sky temperature is derived from horizontal infrared radiation intensity. It is expressed in degrees C. The default calculation is shown below but this value may be modified by the [WeatherProperty:SkyTemperature](#weatherpropertyskytemperature) object. Note that Sigma is the Stefan-Boltzmann constant in the following equation:

![](media/image18.png)\


#### Site Horizontal Infrared Radiation Rate per Area [W/m2]

The horizontal infrared radiation intensity is expressed in W/m^2^ based, if missing from the weather file, on opaque sky cover, sky emissivity, temperature and other factors. The general calculation of Site Horizontal Infrared Radiation Rate per Area is discussed in both the Engineering Reference and the Auxiliary Programs document.

#### Site Diffuse Solar Radiation Rate per Area [W/m2]

Diffuse solar is the amount of solar radiation in W/m^2^ received from the sky (excluding the

solar disk) on a horizontal surface.

#### Site Direct Solar Radiation Rate per Area [W/m2]

Site Direct Solar Radiation Rate per Area is amount of solar radiation in W/m^2^ received within a 5.7° field of view centered on the sun. This is also known as Beam Solar.

#### Site Precipitation Depth [m]

This is the amount of liquid precipitation (m). The source of this field may be the weather file or the [Site:Precipitation](#siteprecipitation) object. The weather file values are in millimeters, but the report here is in meters. Two separate entities are displayed – from the weather file the key value is "Environment"; from the [Site:Precipitation](#siteprecipitation) object the key value is [Site:Precipitation](#siteprecipitation).

If the weather file does not have a liquid precipitation field but does have the present weather fields, then a heuristic calculation is attempted for both flagging when it is raining and the amount of precipitation. If the weather file does have liquid precipitation fields, then the output (e.g. monthly) can be compared with the weather file stat file.

#### Site Ground Reflected Solar Radiation Rate per Area [W/m2]

The ground reflected solar amount (W/m^2^) is derived from the Beam Solar, Diffuse Solar, User specified Ground Reflectance (for month) and Solar Altitude Angle:

![](media/image19.png)\


where if the calculation returns a value < 0.0, then 0.0 will be reported.

#### Site Ground Temperature [C]

The ground temperature is reported in degrees C – this is a user-specified input by month.

#### Site Surface Ground Temperature [C]

The ground temperature is reported in degrees C – this is a user-specified input (object: [Site:GroundTemperature:Shallow](#sitegroundtemperatureshallow)) by month.

#### Site Deep Ground Temperature [C]

The ground temperature is reported in degrees C – this is a user-specified input (object: [Site:GroundTemperature:Deep](#sitegroundtemperaturedeep)) by month.

#### Site Simple Factor Model Ground Temperature [C]

The Site Simple Factor Model Ground Temperature is reported in degrees C – this is a user-specified input (object: [Site:GroundTemperature:FCfactorMethod](#sitegroundtemperaturefcfactormethod)) by month or gleaned from the weather file as noted in the description of the object.

#### Site Outdoor Air Enthalpy [J/kg]

Outdoor enthalpy is derived at each timestep from the Site Outdoor Air Drybulb Temperatureand the Site Outdoor Air Humidity Ratio. It is reported in J/kg.

#### Site Outdoor Air Density [kg/m3]

Outdoor air density is derived at each timestep from the Site Outdoor Air Barometric Pressure, the Outdoor Dry-bulb temperature and the Outdoor Humidity Ratio. It is reported in units kg/m^3^.

#### Site Solar Azimuth Angle [deg]

The Solar Azimuth Angle () is measured from the North (clockwise) and is expressed in degrees. This is shown more clearly in the following figure.

![Solar Position Illustration](media/solar-position-illustration.jpeg)


#### Site Solar Altitude Angle [deg]

The Solar Altitude Angle () is the angle of the sun above the horizontal (degrees).

#### Site Solar Hour Angle [deg]

The Solar Hour Angle (*H*) gives the apparent solar time for the current time period (degrees). It is common astronomical practice to express the hour angle in hours, minutes and seconds of time rather than in degrees. You can convert the hour angle displayed from EnergyPlus to time by dividing by 15. (Note that 1 hour is equivalent to 15 degrees;  360 of the Earth's rotation takes place every 24 hours.)  The relationship of angles in degrees to time is shown in the following table:

Table: Relationship of Angles (degrees) to Time

**Unit of Angle**|**Equivalent time**
------------------------------|--------------------------------
1 radian|3.819719 hours
1 degree|4 minutes
1 arcmin|4 seconds
1 arcsec|0.066667 seconds

#### Site Rain Status []

This field shows whether or not (1=yes, 0=no) the weather shows "raining". For a Design Day, one can denote rain for the entire day but not timestep by timestep. Weather files may indicate rain for a single time interval. This is an "averaged" field – thus a 1 shown for a time period (e.g. daily reporting) means that it was raining during each timestep of that period.

#### Site Snow on Ground Status []

This field shows whether or not (1=yes, 0=no) the weather shows "snow on the ground". For a Design Day, one can denote snow for the entire day but not timestep by timestep. Weather files may indicate snow (snow depth) for a single time interval. This is an "averaged" field – thus a 1 shown for a time period (e.g. daily reporting) means that there was snow on the ground during each timestep of that period.

#### Site Daylight Saving Time Status []

This field shows when daylight saving time (1=yes, 0=no) is in effect. Though shown as an average variable, this value is only set on a daily basis.

#### Site Day Type Index []

This field shows what "day type" the current day is. Daytypes are (1=Sunday, 2=Monday, etc.) with Holiday=8, SummerDesignDay=9, WinterDesignDay=10, CustomDay1=11, CustomDay2=12. Though shown as an average variable, this value is only set on a daily basis.

#### Site Mains Water Temperature [C]

The value of the Water Mains Temperature is reported in C following the calculation shown in the Water Mains Temperature object.

#### Site Precipitation Rate [m/s]

#### Site Precipitation Depth [m]

The rate and quantity of precipitation at the site. These outputs are only available if a Site Precipication object is used. Precipitation is measured in meters.

#### Water System Roof Irrigation Scheduled Depth[m]

This is the scheduled amount of irrigation for the green roof (ecoroof) based on the user input. Amount is measured in meters.

#### Water System Roof Irrigation Actual Depth[m]

This is the actual amount of irrigation for the green roof (ecoroof) based on the scheduled user input and moisture state/saturation of the soil. Amount is measured in meters.

## Outputs for local temperature/wind speed calculations

Local atmospheric properties for outdoor air temperature and wind speed are separately calculated and reported for all zones, surfaces, and outdoor air nodes. The output variables are associated with their respective objects:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Zone Outdoor Air Drybulb Temperature [C]
    Zone,Average,Zone Outdoor Air Wetbulb Temperature [C]
    Zone,Average,Zone Outdoor Air Wind Speed [m/s]
    Zone,Average,Surface Ext Outdoor Dry Bulb [C]
    Zone,Average,Surface Ext Outdoor Wet Bulb [C]
    Zone,Average,Surface Ext Wind Speed [m/s]
    HVAC,Average,System Node Temperature [C]        (for OUTDOOR AIR NODE object)
~~~~~~~~~~~~~~~~~~~~

### Zone Outdoor Air Drybulb Temperature [C]

The outdoor air dry-bulb temperature calculated at the height above ground of the zone centroid.

### Zone Outdoor Air Wetbulb Temperature [C]

The outdoor air wet-bulb temperature calculated at the height above ground of the zone centroid.

### Zone Outdoor Air Wind Speed [m/s]

The outdoor wind speed calculated at the height above ground of the zone centroid.

### Surface Ext Outdoor Dry Bulb [C]

The outdoor air dry-bulb temperature calculated at the height above ground of the surface centroid.

### Surface Ext Outdoor Wet Bulb [C]

The outdoor air wet-bulb temperature calculated at the height above ground of the surface centroid.

### Surface Ext Wind Speed [m/s]

The outdoor wind speed calculated at the height above ground of the surface centroid.

### System Node Temperature [C]

When reporting for the **OutdoorAir:Node** object, this is the outdoor air dry-bulb temperature calculated at the height above ground of the node, if height is specified in the input.