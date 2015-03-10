# Missing Weather File Data

The weather description of data contains "missing" descriptors, a new concept not introduced previously in our IDD conventions. In this case, it will be processed as though those values are "missing" in the weather conversions and/or EnergyPlus weather processing. This may not always be desirable though EnergyPlus will fill in "missing" value with something "appropriate". Eventually, these missing values will be available through the IDD and users will be able to supply their own values or EnergyPlus will calculate those values (such as radiation and illuminance) that are not a simple value replacement. Until then, the following are used:

Table: Missing weather replacement values

**Data item**|**Supplied Value**|**Units**
--------------------------|-------------------------------|----------------------
Dry-bulb Temperature|6|C
Dewpoint Temperature|3|C
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

\*\*Standard Barometric pressure based on location elevation is taken from ASHRAE 2001 Handbook of Fundamentals, Page 6.1 & 2.