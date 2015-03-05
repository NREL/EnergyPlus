# Missing Weather File Data

The following data contains "missing" descriptors; a new concept not introduced previously in our IDD conventions. In this case, it will be processed as though those values are "missing" in the weather conversions. This may not always be desirable though the weather processor will fill in "missing" value with something "appropriate". Eventually, these missing values will be available through the weather processor. Until then, the following are used for initial missing conditions. When a valid value is encountered from weather data, however, it will become the new "missing" replacement value:

Table: Missing weather replacement values

**Data item**|**Supplied Value**|**Units**
--------------------------|-------------------------------|----------------------
Dry Bulb Temperature|6|C
Dew Point Temperature|3|C
Relative Humidity|50|%
Atmospheric Pressure|Standard\*\* Barometric Pressure (altitude based)|Pa
Wind Speed|2.5|m/s
Wind Direction|180|Deg
Total Sky Cover|5|(tenths)
Opaque Sky Cover|5|(tenths)
Visibility|777.7|Km
Ceiling Height|77777|m
Precipitable Water|0|Mm
Aerosol Optical Depth|0|Broadband turbidity
Snow Depth|0|Cm
Days Since Last Snow|88|Days
Radiation Values (Direct/Diffuse)|0.0|Wh/m2
Illuminance Values|0.0|lux (or CD/m2)

\*\*Standard Barometric pressure based on location elevation is taken from ASHRAE 2001 Handbook of Fundamentals, pages 6.1 & 6.2.