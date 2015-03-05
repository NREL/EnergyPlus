# Data Sources/Uncertainty

More recent weather data source files have introduced the concept of data sources and uncertainty flags for many of the fields. The EnergyPlus weather format faithfully reproduces these fields as appropriate for the input source data types. By and large, most of the data sources and uncertainties have used the TMY2 established fields and values (See following table). As noted earlier, to enhance readability and reduce obfuscation, the EnergyPlus format for the data source and uncertainty flags collates them into one large field. Each data element still has its data source and uncertainty: it is positionally embodied depending on its place in the EPW data record.

Table: Key to Data Source and Uncertainty Flags

**Data Flag**|**Flag Values**
--------------------------|----------------------------
Dry Bulb Temperature Data Source|A-F
Dry Bulb Temperature Data Uncertainty|0-9
Dew Point Temperature Data Source|A-F
Dew Point Temperature Data Uncertainty|0-9
Relative Humidity Data Source|A-F
Relative Humidity Data Uncertainty|0-9
Atmospheric Station Pressure Data Source|A-F
Atmospheric Station Pressure Data Uncertainty|0-9
Horizontal Infrared Radiation Data Source|A-H, ?
Horizontal Infrared Radiation Data Uncertainty|0-9
Global Horizontal Radiation Data Source|A-H, ?
Global Horizontal Radiation Data Uncertainty|0-9
Direct Normal Radiation Data Source|A-H, ?
Direct Normal Radiation Data Uncertainty|0-9
Diffuse Horizontal Radiation Data Source|A-H, ?
Diffuse Horizontal Radiation Data Uncertainty|0-9
Global Horizontal Illuminance Data Source|I, ?
Global Horizontal Illuminance Data Uncertainty|0-9
Direct Normal Illuminance Data Source|I, ?
Direct Normal Illuminance Data Uncertainty|0-9
Diffuse Horizontal Illuminance Data Source|I, ?
Diffuse Horizontal Illuminance Data Uncertainty|0-9
Zenith Luminance Data Source|I, ?
Zenith Luminance Data Uncertainty|0-9
Wind Direction Data Source|A-F
Wind Direction Data Uncertainty|0-9
Wind Speed Data Source|A-F
Wind Speed Data Uncertainty|0-9
Total Sky Cover Data Source|A-F
Total Sky Cover Data Uncertainty|0-9
Opaque Sky Cover Data Source|A-F
Opaque Sky Cover Data Uncertainty|0-9
Visibility Data Source|A-F, ?
Visibility Data Uncertainty|0-9
Ceiling Height Data Source|A-F, ?
Ceiling Height Data Uncertainty|0-9
Precipitable Water Data Source|A-F
Precipitable Water Data Uncertainty|0-9
Broadband Aerosol Optical Depth Data Source|A-F
Broadband Aerosol Optical Depth Data Uncertainty|0-9
Snow Depth Data Source|A-F, ?
Snow Cover Data Uncertainty|0-9
Days Since Last Snowfall Data Source|A-F, ?
Days Since Last Snowfall Data Uncertainty|0-9

The definition of the solar radiation source flags and solar radiation uncertainty flags are shown in the following two tables:

Table: Solar Radiation and Illuminance Data Source Flag Codes

------------------------------------------------------------------------
 Flag Code  Definition
----------- ------------------------------------------------------------
     A      Post-1976 measured solar radiation data as received from
            NCDC or other sources

     B      Same as "A" except the global horizontal data underwent a
            calibration correction

     C      Pre-1976 measured global horizontal data (direct and
            diffuse were not measured before 1976), adjusted from solar
            to local time, usually with a calibration correction

     D      Data derived from the other two elements of solar radiation
            using the relationship, global = diffuse + direct ´
            cosine (zenith)

     E      Modeled solar radiation data using inputs of observed sky
            cover (cloud amount) and aerosol optical depths derived
            from direct normal data collected at the same location

     F      Modeled solar radiation data using interpolated sky cover
            and aerosol optical depths derived from direct normal data
            collected at the same location

     G      Modeled solar radiation data using observed sky cover and
            aerosol optical depths estimated from geographical
            relationships

     H      Modeled solar radiation data using interpolated sky cover
            and estimated aerosol optical depths

     I      Modeled illuminance or luminance data derived from measured
            or modeled solar radiation data

     ?      Source does not fit any of the above categories. Used for
            nighttime values and missing data
------------------------------------------------------------------------

Table: Solar Radiation and Illuminance Data Uncertainty Flag Codes

Flag|Uncertainty Range (%)
----|---------------------
1|Not used
2|2 - 4
3|4 - 6
4|6 - 9
5|9 - 13
6|13 - 18
7|18 - 25
8|25 - 35
9|35 - 50
0|Not applicable

Finally, the Meteorological data source and uncertainty flag/codes are shown in the following two tables:

Table: Meteorological Data Source Flag Codes

--------------------------------------------------------------------------
 Flag  Definition
------ -------------------------------------------------------------------
  A    Data as received from NCDC, converted to SI units

  B    Linearly interpolated

  C    Non-linearly interpolated to fill data gaps from 6 to 47 hours in
       length

  D    Not used

  E    Modeled or estimated, except: precipitable water, calculated from
       radiosonde data; dew point temperature calculated from dry bulb
       temperature and relative humidity; and relative humidity
       calculated from dry bulb temperature and dew point temperature

  F    Precipitable water, calculated from surface vapor pressure;
       aerosol optical depth, estimated from geographic correlation

  ?    Source does not fit any of the above. Used mostly for missing data
--------------------------------------------------------------------------

Table: Meteorological Uncertainty Flag Codes

Flag|Definition
----|----------
1- 6|Not used
7|Uncertainty consistent with NWS practices and the instrument or observation used to obtain the data
8|Greater uncertainty than 7 because values were interpolated or estimated
9|Greater uncertainty than 8 or unknown.
0|Not definable.