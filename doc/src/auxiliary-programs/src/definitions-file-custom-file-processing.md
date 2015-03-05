# Definitions File & Custom File Processing

## Description of "Def" input file

Some of the data formats have inherent omissions (e.g. TMY does not have location data, BLAST ASCII does not have elevations). In order to overcome this limitation and to provide further flexibility, a definitions file (extension must be .def) is implemented. By naming this with the same "file name" as your input file (in the same folder), the weather converter will read the format and use that data, as appropriate, in the file conversions. The .def file uses Fortran "Namelist" input fields as shown in the example below. For flexibility, you can also define a "presets.def" file (such as when you have a list of files to process and the format or some portion is all the same between the group of files. The two def files (one named the same as the file name for the raw data and one named presets.def) will both be processed. Conflicts between the two will be shown in the .audit file. The set of namelist groups is:

- &location – Location data
- &miscdata – Comments to be applied to "COMMENT2" in the EPW file and "Source Data"
- &wthdata –  weather data specifications including file type, custom formats
- &datacontrol – user specified control over "missing" data (Custom format only)

> **Note that the "Def" formats are entirely different from the usual IDF formats of EnergyPlus.  No commas separate fields.  No semicolon terminates the entry.**

~~~~~~~~~~~~~~~~~~~~
    &location
    City='Hong Kong'
    StateProv=' '
    Country='CHN'
    InLat=22.75
    InLong=115
    InTime=8
    InElev=0
    InWMO=450040
    /

    &miscdata
    Comments1='This file was given to us by....'
    SourceData='Original xyz data'
    /
~~~~~~~~~~~~~~~~~~~~

The "slash" (/) character terminating each block is very important – omissions results in incorrect reading of data.

Definitions File Details are shown in the following table. You may leave out a field if you wish – the program will use whatever default is applicable (or usable) from the data format. All data formats accept this additional file. Only Custom format currently uses the &datacontrol element. And only Custom format input type uses the Data Elements, Format and Conversion factors from the &wthdata element.

Note that strings in the "def" should be enclosed in single quotes if there is more than one word in the string – if only one word, quotes do not need to be used.

Table: Definitions File &location description

&locationField Description|Field Name|Type
--------------------------|----------|----
Name of City|City|String
State or Province|StateProv|String
Country Code|Country|String (3 characters)
Latitude (N+/S-)|InLat|Numeric
Longitude (W-/E+)|InLong|Numeric
Time Zone (GMT +/-)|InTime|Numeric
Elevation (meters)|InElev|Numeric
WMO #|InWMO|Numeric or String (6 characters)

## Expected Formats for &location

### Fields: City, StateProv, Country

These fields are string variables. If Country is *not* included, an attempt to use the State/Prov entry may be used to determine country. Otherwise, these fields are not validated and are used to create part of the "location" header record in the EPW file. City can be up to 30 characters in length; StateProv up to 15 characters; Country up to 10 characters (standard 3 character abbreviation preferred).

### Fields: InLat, InLong

These fields are decimal equivalent for Latitude and Longitude. The convention is North Latitude is positive; South is negative. Likewise, East Longitude is positive; West Longitude is negative. That is, if your latitude is N 30 15' (North 30 degrees, 15 minutes) then your input is +30.25.

### Field: InTime

This field is the decimal equivalent for the Time Zone value. The convention is GMT +/-. That is, if your time zone is "behind" GMT time by 6 hours, your input would be –6.

### Field: InElev

This field is the location elevation in meters. Range can be from –300 to 6096. (These are the values from EnergyPlus – there is no validation of these in the weather converter.)

### Field: InWMO

This field is the WMO (World Meterological Organization) number for the location. Though not validated per se, if found in the "design conditions" auxiliary files, the Design Day information can be generated.

Table: Definitions File - &miscdata description

**&miscdata**Field Description|Field Name|Type
-------------------------------------------|----------|----
String for Comments 1 header|Comments1|String
String for Comments 2 header|Comments2|String
String for Source Data in Location header|SourceData|String
URL for output|OutputURL|String

## Expected Formats for &miscdata

### Fields: Comments1, Comments2

These are strings. After concatenation, they become part of the comment header lines in the EPW headers. Up to 150 characters each is allowed.

### Field: SourceData

This string is applied to the "Source Data" field in the Location Header. Up to 60 characters is allowed.

### Field: OutputURL

When a list of files is being processed, one of the outputs that results from the processing is a KML (Keyhole Markup Language) file that can be used with Google Earth to pinpoint the locations of the weather site. This field can be used to set this URL for later output. The list file format also includes a URL as its third (optional) parameter. If included, this input would overwrite other URL designations.

Table: Definitions file - &wthdata description

**&wthdata**Field Description|Field Name|Type
------------------------------------------|----------|----
Input File Type|InputFileType|String
Number of records per hour|NumInHour|Integer
Data Element Names|DataElements|Strings
Data Units|DataUnits|Strings
Multiplicative Conversion Factors for Data|DataConversionFactors|Numeric
Special Missing Values|DataMissingValues|Numeric
Format for input|InFormat|Format String or "delimited"
Delimiter Character|DelimiterChar|
Decimal Delimiter Character|DecimalSymbolChar|String
Date Separator|DateSeparator|String (single character)

## Expected Formats for &wthdata

### Field: InputFileType

You can always use this field and def file to "override" the default input format type that depends on the extension of your file (see Table 2. Input File Extensions with implied Data types). A complete set of valid values for Input File types is shown in the following table. Data Files are described more fully in the section Source Weather Data Formats that occurs later in this document.

Table: Input File Type Values

**Value**|**File Type Description**
----------------------|--------------------------------------
Tmy or ,tm2|TMY2 Data File
Iwec or iwc|IWEC Data File
Samson or dat|SAMSON Data File
wyec2 or wy2|WYEC2 Data File
Fmt or txt|DOE-2 FMT File
Clm or esp-r|ESP-r Formatted (CLM) data file
Blast or asc|BLAST ASCII Data File
Tmy|TMY Data File
Epw|EPW Data File
Csv|EPW - CSV Data File
Wea|Ecotect wea Data File
Swera or swe|SWERA Data File
Custom or User|Custom Data File

### Field: NumInHour

This field can be used to specify multi-interval (per hour) files. Without this field, the only formats that can have multiple intervals per hour are the EPW and CSV file formats – using the header record DataPeriods value for that field.

### Fields below only used in "Custom" format processing

### Field: DataElements

For custom files, you will need to indicate which data elements are in which positions of the raw data file. The fields must come from a standardized list of names see following tables that include internal names (short and long – as shown in Table 8) as well as the EnergyPlus CSV format names (short and long – shown in Table 9) plus some further elements that can be specified when the standard data elements are not part of the raw data (as shown in Table 10). "Ignore" is used to skip a raw data field that is not applicable to the weather converter formats. Note that variables listed in the following table (in italics) are allowed for flexibility – i.e. wetbulb temperature can be used to determine relative humidity and/or dewpoint temperature. The following three tables illustrate the names for data elements.

Table: Internal Data Element Names (directly applicable to EPW)

------------------------------------------------------------------------------------------------
**Short Name**     **Long Name**                                     **Default     **Used by
                                                                     EPW Units**   EnergyPlus**
----------------   ------------------------------------------------- ------------- -------------
year               Year                                              --            N

month              Month                                             --            Y

day                Day                                               --            Y

hour               hour                                              --            Y

minute             minute                                            --            N

datasource         datasource                                        --            N

drybulb            dry_bulb_temperature                              C             Y

dewpoint           dew_point_temperature                             C             Y

relhum             relative_humidity                                 %             Y

atmos_pressure     atmospheric_pressure                              Pa            Y

exthorrad          extraterrestrial_horizontal_radiation             Wh/m^2^       N

extdirrad          extraterrestrial_direct_normal_radiation          Wh/m^2^       N

horirsky           horizontal_infrared_radiation_                    Wh/m^2^       Y
                   intensity_from_sky

glohorrad          global_horizontal_radiation                       Wh/m^2^       N

dirnorrad          direct_normal_radiation                           Wh/m^2^       Y

difhorrad          diffuse_horizontal_radiation                      Wh/m^2^       Y

glohorillum        global_horizontal_illuminance                     lux           N

dirnorillum        direct_normal_illuminance                         lux           N

difhorillum        diffuse_horizontal_illuminance                    lux           N

zenlum             zenith_luminance                                  lux           N

winddir            wind_direction                                    degrees       Y

windspd            wind_speed                                        m/s           Y

totskycvr          total_sky_cover                                   tenths        N

opaqskycvr         opaque_sky_cover                                  tenths        N

visibility         visibility                                        km            N

ceiling_hgt        ceiling_height                                    m             N

presweathobs       present_weather_observation                       -             Y

presweathcodes     present_weather_codes                             -             Y

precip_wtr         precipitable_water                                mm            N

aerosol_opt_depth  aerosol_optical_depth                             thousandths   N

snowdepth          snow_depth                                        cm            Y

days_last_snow     days_since_last_snow                              -             N

Albedo             albedo                                            -             N

liq_precip_depth   liquid_precip_depth                               mm            Y

liq_precip_rate    liquid_precip_rate                                Hour          N
------------------------------------------------------------------------------------------------

The following table illustrates that the EnergyPlus CSV header names can be used for data elements in DEF files, if desired.

Table: Names from the EnergyPlus CSV files

--------------------------------------------------------------------------------------------------
**Short Name**              **Long Name**                              **Default     **Used by
                                                                       EPW Units**   EnergyPlus**
--------------------------- -----------------------------------------  ------------- --------------
*Date*                      *Date (used to derive Month/Day)*          -             N

*hh:mm*                     *HH:MM (used to derive hour/minute)*       -             N

datasource                  datasource                                 -             N

Drybulb                     dry bulb temperature                       C             Y

dewpoint                    dew point temperature                      C             Y

Relhum                      relative humidity                          %             Y

atmos pressure              atmospheric pressure                       Pa            Y

exthorzrad                  extraterrestrial horizontal radiation      Wh/m^2^       N

extdirrad                   extraterrestrial direct normal radiation   Wh/m^2^       N

horzirsky                   horizontal infrared radiation              Wh/m^2^       Y
                            intensity from sky

glohorzrad                  global horizontal radiation                Wh/m^2^       N

dirnorzrad                  direct normal radiation                    Wh/m^2^       Y

difhorzrad                  diffuse horizontal radiation               Wh/m^2^       Y

glohorzillum                global horizontal illuminance              lux           N

dirnorzillum                direct normal illuminance                  lux           N

difhorzillum                diffuse horizontal illuminance             lux           N

Zenlum                      zenith luminance                           lux           N

winddir                     wind direction                             degrees       Y

windspd                     wind speed                                 m/s           Y

totskycvr                   total sky cover                            tenths        N

opaqskycvr                  opaque sky cover                           tenths        N

visibility                  visibility                                 km            N

ceiling hgt                 ceiling height                             m             N

presweathobs                present weather observation                -             Y

presweathcodes              present weather codes                      -             Y

precip wtr                  precipitable water                         mm            N

aerosol opt depth           aerosol optical depth                      thousandths   N

snowdepth                   snow depth                                 cm            Y

days last snow              days since last snow                       -             N

Albedo                      albedo                                     -             N

rain                        liquid precipitation depth                 mm            Y

rain quantity               liquid precipitation rate                  Hour          N
--------------------------------------------------------------------------------------------------

## Custom Files – Auxiliary Data

Often raw data files will not have the preceding elements but similar elements that can be used to derive the values used in the EPW files and in EnergyPlus. (For example, dew point temperature and relative humidity are needed and can be derived from dry builb temperature and a humidity indicating element such as wet bulb temperature or humidity ratio). The following table contains the data element names that can be used in the Weather Converter program to derive other data which will then be placed into the EPW data fields.

Table: Auxiliary Data for Custom Files

**Short Name**|**Long Name**|**Units**|**Used by EnergyPlus**
---------------------------|--------------------------|----------------------|-----------------------------------
*wetbulb*|*wet_bulb_temperature*|*C*|*N*
*humratio*|*humidity_ratio*|*g/kg*|*N*
*dirhorrad*|*direct_horizontal_radiation*|*Wh/m^2^*|*N*
*interval*|*Interval*|*unit*|*N*
*hour_yr*|*hour_of_year*|*hour*|*N*
*time*|*Time*|*hh:mm*|*N*
*hh:mm*|*HH:MM*|*hh:mm*|*N*
*Date*|*Date*|*mm/dd/yyyy*|*N*

Explanation of these data elements follows:

### Wetbulb (Wet Bulb Temperature)

If you have the wet bulb temperature, this data element can be used to derive the dew point temperature and relative humidity.

### HumRatio (Humidity Ratio)

If you have the humidity ratio, this data element can be used to derive the dew point temperature and relative humidity.

### Dirhorrad (Direct Horizontal Radiation)

If you have direct horizontal radiation (and at least one other solar element from global horizontal radiation or diffuse horizontal radaition), this data element will be used to derive the direct normal radiation.

### Interval

If your "number of records per hour" is >1, then you can designate each interval of that hour with this field.

### Hour_Of_Year

If you wish, you can just put in the hour of the year for each record.  Note that if no date element is entered, then the default is that the data is in hour of the year (including possible number of records per hour).

### Time (or HH:MM)

Time can be entered (rather than hour) and the units must be hh:mm; this is then decoded on each record to the appropriate hour.

### Date

Dates can be entered as month, day, and year.  The units field must be entered and should designate the format for the date decoding. Date separator characters for this field are entered in the DateSeparator item. Default date separator is "/" and that is what is used in the table that shows the allowable units:

Table: Allowable date formats for Custom Data entries.

**Units Format**|**Interpretation**|**Example**
-----------------------------|-------------------------------|------------------------
mm/dd/yyyymm/dd/yym/d/y|Month, day, year|12/13/2009
yyyy/mm/ddyy/mm/ddy/m/d|Year, month, day|2009/12/13
dd/mm/yyyydd/mm/yyd/m/y|Day, month, year|13/12/2009

### Field: DataUnits

There should be as many DataUnits entries as DataElement entries. These are not generally used but may be used in the future for automatic conversions. The exception to this is "temperature" fields. Use "f" for Fahrenheit, "k" for Kelvin temperatures. Note that the DataConversionFactor for this field will be applied prior to conversion. (Many formats use integer numbers to represent values that are in tenths, for example.)

### Field: DataConversionFactors

There should be as many DataConversionFactors entries as DataElement entries. These factors are multiplicative factors (i.e. the input value is multiplied by this factor) and can be used to process input data into the values used in the EPW weather files.

### Field: DataMissingValues

There should be as many entries (though some can be blank) as DataElement entries. The values entered will override the default "missing" values (from the EPW data dictionary) and, whereas the defaults may be interpreted as a >= missing value (i.e. >= 999), these values will be exact (i.e. = -999.)

### Field: InFormat

The value in this field should be "delimited" if you are using a free format data file or specify a "Fortran style" format statement.

### Field: DelimiterChar

If you use a "delimited" format file, you need to specify a delimiter character. Only a single character may be specified.

### Field: DecimalSymbolChar

A single character can be used to specify the decimal "point" character. Default is the US Standard ".". With use of DelimiterChar and this field, one can essentially use the fields to specify European Standard Excel export formats.

### Field: DateSeparator

If you are entering the aforementiond "date" Data Element and your date separator is a character other than slash ("/"), then you need to enter a single character so the program can interpret your date entries.

Table: Definitions file - &datacontrol description

---------------------------------------------------------------------------------------------------------
**&datacontrol**Field Description              Field Name                       Type
--------------------------------------------   ------------------------------   -------------------------
Records to Skip                                NumRecordsToSkip                 Integer

Records to Read                                MaxNumRecordsToRead              Integer

Missing Data Action                            MissingDataAction                --

Missing Wind Direction Action                  MissingWindDirAction             --

Missing Wind Direction Value                   MissingWindDirValue              Real

Missing Opaque Sky Cover Action                MissingOpaqueSkyCoverAction      --

Missing Opaque Sky Cover Value                 MissingOpaqueSkyCoverValue       Real (Value 0.0 to 10.0)
                                                                                -- tenths of sky cover

Maximum Wind Speed                             MaxWindSpeed                     Real

Maximum Direct Solar                           MaxDirectSolar                   Real

Maximum Diffuse Solar                          MaxDiffuseSolar                  Real

Maximum Illuminance Value                      MaxIlluminanceValue              Real

Generate Solar Radiation Warnings              GenerateSolarRadiationWarnings   --

Generate Illuminance Warnings                  GenerateIlluminanceWarnings      --
---------------------------------------------------------------------------------------------------------

## Expected Formats for &datacontrol

Most of the items in this element are particularly applicable to custom format input files. Currently, they are only used in custom files, but may be more generally applicable in future releases.

### Field: NumRecordsToSkip

This is an integer number of records to skip during processing. You might use this if your input file has some information at the top of the file.

### Field: MaxNumRecordsToRead

This is an integer number of records to read (typically 8760 for a full year). You might use this if your input file has some information after the data records.

### Fields: MissingDataAction, MissingWindDirAction, MissingOpaqueSkyCoverAction

These fields tell the converter program what to do with "missing" data. Missing data can be found in two forms:  totally not included in the DataElements or a missing value (as defined in the EPW format). Valid values for these fields are:

- DEFAULT – use the default processing that the weather converter already uses – starts off with a specific value and updates if data is found.
- CONSTANT – use a constant value to replace all missing data
- RANDOM – use a random number to generate the missing data
- An additional value for MissingOpaqueSkyCoverAction is:
- TOTALSKY – use the value for Total Sky Cover

### Fields: MissingWindDirValue, MissingOpaqueSkyCoverValue

The values specified in this field are used with the action fields previously mentioned.

### Field: MaxWindSpeed

The default maximum wind speed (40m/s) may not be enough for some locations – this allows the override capability.

### Field: MaxDirectSolar, MaxDiffuseSolar, MaxIlluminanceValue

Default maximum solar values may not be enough for some locations – this allows the override capability.

### Field: GenerateSolarRadiationWarnings, GenerateIlluminanceWarnings

If you don't want to see extra warnings when input values are greater than max values (default or as specified in previous fields), use NO as the keyword. Use YES to make sure you see the warnings. Default is YES.

## Def File Examples

In the following examples, every attempt has been made to make sure that these work with the Weather Converter program. However, we cannot foresee all possible combinations. **Caveat emptor – user beware.**

Here's an example where the delimiter between fields is a semi-colon (;) and the decimal symbol character is a comma (,) – typical of some non-USA regional settings:

~~~~~~~~~~~~~~~~~~~~{caption="DEF file for with non-standard field delimiter and decimal symbol"}
    &location
    City=<cityname>
    StateProv=<state/province>
    Country=<country>
    InWMO=<wmo>
    InLat=<latitude>
    InLong=<longitude>
    InElev=<elevation>
    InTime=<timezone>
    /

    &wthdata
    NumInHour=1
    InputFileType='CUSTOM'
    InFormat='DELIMITED'
    DataElements=Date,HH:MM,Datasource,Dry Bulb Temperature,Dew Point Temperature,Relative Humidity,Atmospheric Pressure,Extraterrestrial Horizontal Radiation,Extraterrestrial Direct Normal Radiation,Horizontal Infrared Radiation Intensity from Sky,Global Horizontal Radiation,Direct Normal Radiation,Diffuse Horizontal Radiation,Global Horizontal Illuminance,Direct Normal Illuminance,Diffuse Horizontal Illuminance,Zenith Luminance,Wind Direction,Wind Speed,Total Sky Cover,Opaque Sky Cover,Visibility,Ceiling Height,Present Weather Observation,Present Weather Codes,Precipitable Water,Aerosol Optical Depth,Snow Depth,Days Since Last Snow,Albedo,Liquid Precipitation Depth,Liquid Precipitation Quantity
    DataUnits='mm.dd.yyyy','hh:mm','x','x','x','x','C','C','%','Pa','Wh/m2','Wh/m2','Wh/m2','Wh/m2','Wh/m2','Wh/m2','lux','lux','lux','Cd/m2','deg','m/s','tenths','tenths','km','m','x','x','mm','{.001}','cm','x','{.01}','mm','hr'
    DataConversionFactors=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    DelimiterChar=';'
    DateSeparator='.'
    DecimalSymbolChar=','
    /

    &datacontrol
    NumRecordsToSkip=19
    MaxNumRecordsToRead=8784
    MissingWindDirAction=RANDOM
    /
~~~~~~~~~~~~~~~~~~~~


Here's an example of a file used to "enhance" a DOE-2 FMT file:

~~~~~~~~~~~~~~~~~~~~{caption="DEF file for DOE-2 FMT file"}
    &location
    City='Kelburn'
    StateProv='Wellington'
    Country='NZL'
    InWMO=934360
    InLat=-42.3333
    InLong=174.8
    InElev=8
    InTime=1
    /

    &wthdata
    NumInHour=1
    InputFileType='FMT'
    /

    &miscdata
    Comments1='Standard Data Files for Computer Thermal Simulation of Solar Low Energy Non-residential Buildings; ven der Werff, Amor, and Donn 1990'
    Comments2='Full Actual year of dataSource data is TRY format converted to DOE-2 format;'
    /
~~~~~~~~~~~~~~~~~~~~


Here's an example of a fixed format used for custom file processing. Note that random sky cover is used, to facilitate calculating Horizontal IR from Sky that is used in EnergyPlus. Also, random wind direction is used because the data set does not contain wind direction.

~~~~~~~~~~~~~~~~~~~~{caption="DEF file for formatted custom file."}
    &location
    City='Torino-Caselle'
    StateProv=' '
    Country='ITA'
    InWMO=160590
    InLat=45.18333
    InLong=7.65
    InElev=282
    InTime=1
    /

    &wthdata
    NumInHour=1
    InputFileType='CUSTOM'
    InFormat='(I2, I2, I2, F7.2, F7.2, F5.1, F5.1, F5.1)'
    DataElements=Month,Day,Hour,DirNorRad,DifHorRad,DryBulb,Wind_Speed,Relative_Humidity
    DataUnits=,,,'kJ/M2','kJ/M2','C','m/s','%'
    DataConversionFactors=1,1,1,.2777778,.2777778,1,1,1
    /

    &miscdata
    Comments1='Italian Climate Data Set Gianni de Giorgio'
    Comments2='Period of record 1951-1970'
    SourceData='IGDG Data Set'
    /
    &datacontrol
    MissingOpaqueSkyCoverAction=RANDOM
    MissingWindDirAction=RANDOM
    /
~~~~~~~~~~~~~~~~~~~~


An example of a free format custom file. Here, there were several lines of text after the numeric data at the end of the file – thus we used the number of records to read parameter rather than hand editing each input file.

~~~~~~~~~~~~~~~~~~~~{caption="DEF File for delimited custom file."}
    &location
    City='Beijing'
    StateProv='Beijing'
    Country='CHN'
    InWMO='545110'
    InLat=39.92
    InLong=116.27
    InElev=55
    InTime=8
    /

    &miscdata
    Comments1='China Data Set - Zhang/Huang'
    /

    &wthdata
    NumInHour=1
    InputFileType='CUSTOM'
    InFormat='DELIMITED'
    DataElements=Ignore,Year,Month,Day,Hour,Ignore,DryBulb,DewPoint,Ignore,Relative_Humidity,Ignore,DirNorRad,DifHorRad,WindDir,Wind_Speed,OpaqSkyCvr,Atmos_Pressure
    DataUnits=x,x,x,x,x,x,'k','k',x,'%',x,'wh/m2','wh/m2','deg','m/s',x,'Pa'
    DataConversionFactors=1,1,1,1,1,1,.1,.1,1,1,1,1,1,1,.1,.1,10
    DelimiterChar=' '
    /

    &datacontrol
    NumRecordsToSkip=0
    MaxNumRecordsToRead=8760
    /
~~~~~~~~~~~~~~~~~~~~


Suppose you have a file that is "almost" TMY2 format. You can easily specify a Def file to treat it as a custom file rather than a TMY2 file (which, by standards, will have the data filled).

~~~~~~~~~~~~~~~~~~~~{caption="DEF File for almost TMY2 files."}
    &location
    City=<cityname>
    StateProv=<state/province>
    Country=<country>
    InWMO=<wmo>
    InLat=<latitude>
    InLong=<longitude>
    InElev=<elevation>
    InTime=<timezone>
    /

    &wthdata
    NumInHour=1
    InputFileType='CUSTOM'
    InFormat='(1X,I2,I2,I2,I2,I4,I4,I4,A2,I4,A2,I4,A2,I4,A2,I4,A2,I4,A2,I4,A2,I2,A2,I2,A2,I4,A2,I4,A2,I3,A2,I4,A2,I3,A2,I3,A2,I4,A2,I5,A2,I1,A9,I3,A2,I3,A2,I3,A2,I2,A2)'
    DataElements=ignore,year,month,day,hour,ExtHorzRad,ExtDirNormRad,GloHorzRad,ignore,DirNormRad,ignore,DifHorzRad,ignore,GloHorzIllum,ignore,DirNormIllum,ignore,DifHorzIllum,ignore,ZenithLum,ignore,ignore,ignore,ignore,ignore,DryBulb,ignore,DewPoint,ignore,RelHumid,ignore,Pressure,ignore,WindDir,ignore,WindSpd,ignore,Visibility,ignore,CeilHgt,ignore,ObsIndicator,WeatherCodes,PrecWtr,ignore,AerOptDepth,ignore,SnowDepth,ignore,DaysSnow,ignore
    DataUnits='x','x','x','x','x','x','Wh/m2','Wh/m2','Wh/m2','x','Wh/m2','x','Wh/m2','x','lux','x','lux','x','lux','x','Cd/m2','x','x','x','x','x','C','x','C','x','%','x','x','x','deg','x','m/s','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x'
    DataConversionFactors=1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.1, 1, 0.1, 1, 1, 1, 100, 1, 1, 1, 0.1, 1, 1, 1, 1,  1,  1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    /

    &miscdata
    Comments1='Custom DEF format for TMY2 formatted files.'
    SourceData='TMY2'
    /

    &datacontrol
    NumRecordsToSkip=1
    MaxNumRecordsToRead=8784
    MissingWindDirAction=RANDOM
    MissingDataAction=DEFAULT
    MissingOpaqueSkyCoverAction=RANDOM
    /
~~~~~~~~~~~~~~~~~~~~


Finally, an example of using an EPW file as a custom file with a DEF format. Note that the specially formatted CSV files from EnergyPlus can be automatically read in and this format is provided as an extra bonus.

~~~~~~~~~~~~~~~~~~~~{caption="DEF File for EPW files."}
    &location
    City=<cityname>
    StateProv=<state/province>
    Country=<country>
    InWMO=<wmo>
    InLat=<latitude>
    InLong=<longitude>
    InElev=<elevation>
    InTime=<timezone>
    /

    &wthdata
    NumInHour=1
    InputFileType='CUSTOM'
    InFormat='DELIMITED'
    DataElements=year,month,day,hour,minute,datasource,Dry_Bulb_Temperature,Dew_Point_Temperature,Relative_Humidity,Atmospheric_Pressure,Extraterrestrial_Horizontal_Radiation,Extraterrestrial_Direct_Normal_Radiation,Horizontal_Infrared_Radiation_Intensity_from_Sky,Global_Horizontal_Radiation,Direct_Normal_Radiation,Diffuse_Horizontal_Radiation,Global_Horizontal_Illuminance,Direct_Normal_Illuminance,Diffuse_Horizontal_Illuminance,Zenith_Luminance,Wind_Direction,Wind_Speed,Total_Sky_Cover,Opaque_Sky_Cover,Visibility,Ceiling_Height,Present_Weather_Observation,Present_Weather_Codes,Precipitable_Water,Aerosol_Optical_Depth,Snow_Depth,Days_Since_Last_Snow,Albedo,Liquid_Precipitation_Depth,Liquid_Precipitation_Quantity
    DataUnits='x','x','x','x','x','x','C','C','%','Pa','Wh/m2','Wh/m2','Wh/m2','Wh/m2','Wh/m2','Wh/m2','lux','lux','lux','Cd/m2','deg','m/s','tenths','tenths','km','m','x','x','mm','{.001}','cm','x','{.01}','mm','hr'
    DataConversionFactors=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    DelimiterChar=','
    /

    &miscdata
    Comments1='Standard EPW Custom def format for reading EPW files in EnergyPlus Weather Converter'
    SourceData='EPW'
    /

    &datacontrol
    NumRecordsToSkip=8
    MaxNumRecordsToRead=8784
    MissingWindDirAction=RANDOM
    /
~~~~~~~~~~~~~~~~~~~~


## Custom File Processing

In "normal" file processing, conversion from the input data elements to the EPW data elements is automatic. In "custom" file processing, there is limited flexibility in this regard. For example, the user may use "wet bulb" temperature in their inputs – this will allow the weather converter to calculate appropriate values for dew point temperature (if it is missing) and/or relative humidity. Again, limited calculations/derivations are done – should one input wet bulb temperature along with dew point temperature and relative humidity. Likewise, if only values for global horizontal radiation and diffuse horizontal radiation are given, the program will calculate a value for direct normal radiation using commonly recognized relationships between these values.

## Custom File Processing – Solar Radiation Value Calculation

EnergyPlus only uses the solar radiation data for Direct Normal and Diffuse Horizontal radation in its calculations. But many data sources have only Global Horizontal (sometimes called Total) or none of the solar radiation elements.

With any two of the solar components, it is reasonable to use the simple relationship of Global, Diffuse and Direct, such as:

$$\text{Global}_{\text{horizontal radiation}} = \text{Direct}_{\text{horizontal radiation}} + \text{Diffuse}_{\text{horizontal radiation}}$$

Using known solar position (calculated internally by the Weather converter from latitude, longitude, date and hour), one has:

$$\text{Direct}_{\text{normal radiation}} = \frac{\text{Direct}_{\text{horizontal radiation}}}{sin(\text{Solar}_{\text{height}})}$$

Thus, having two of the solar radiation components makes it relatively simple to derive the third.

However, many data sources may not have any solar radiation components. A study was undertaken to find an appropriate solar model to fill in missing solar data for weather files. The goal was to determine one or more possible solar models to use in the weather conversion/creation process. One model seemed better overall with the usual given data from the sources than others. The model, termed Zhang-Huang, has been used in a variety of locations and data creations, including the upcoming IWEC2 data. The model uses a simplistic approach of recent drybulb temperatures, sky cover, global solar constant and solar position. This model is only used when all solar values are missing from the incoming data. When global radiation is available, then a different model (Perez split) is used to split the global into direct normal and diffuse horizontal values.

Results, of course, can vary depending on locations.

For example, in Brisbane AUS, comparing the solar creation with the original IWEC data looks very good:

![Solar radiation comparison - IWEC vs Weather Solar Model (Brisbane AUS)](media/solar-radiation-comparison-iwec-vs-weather.png)

Of course, there are other locations that don't compare quite as well:

![Comparison of IWEC vs Weather program Solar Model (Singapore)](media/comparison-of-iwec-vs-weather-program-solar.png)
