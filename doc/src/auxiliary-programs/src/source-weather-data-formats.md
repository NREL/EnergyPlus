# Source Weather Data Formats

Source weather data for building energy simulation programs can be broken into two major classes: *historical data* and *typical weather years*. Historical data is just "real" data: usually measured (but sometimes modeled) data from a particular location for a given period of record. Typical years are ersatz years assembled to match the long term data from a particular location using a particular statistical measure. Typical data may also be "real" data but may not be a contiguous year – the data may be comprised of months from multiple years.

The primary source for historical weather data is the U.S. National Climatic Data Center (NCDC) in Asheville, NC: http://www.ncdc.noaa.gov/. NCDC can provide hourly historical data for thousands of locations around the world. This data may not always be complete; data items or periods of record may be missing.

## Data Set vs. Data Format

In this document as well in many others, you will read about a certain "data set" and you will also read about data in a "certain" format – e.g., the TMY2 data set and the TMY2 data format. Simply stated, a data set refers to a set of data files developed around a set of procedures for selection and usually with a specific purpose for use. A data format is merely the identification of data elements in a data file. In the TMY2 example, the TMY2 data set was developed as described below and the format of the data is usually called a TMY2 format.

Any data *could* be put into a TMY2 format, but it wouldn't necessarily be selected using the same procedures as the TMY2 data set.

## SAMSON Data Set/Format

A highly reliable source of historical data for U.S. locations is the Solar and Meteorological Surface Observational Network (SAMSON) data set assembled by the National Renewable Energy Laboratory (NREL) in Golden, CO: http://www.nrel.gov/. The SAMSON data set contains a 30 year (1961 to 1990) period of record for 239 locations and are available from the NCDC.

SAMSON also describes a file format. Though no data from the SAMSON data set are available for download in EPW format, the weather conversion utility can read and process this file format.

## TMY2 Data Set/Format

The TMY2 are data sets of hourly values of solar radiation and meteorological elements for a 1-year period. Their intended use is for computer simulations of solar energy conversion systems and building systems to facilitate performance comparisons of different system types, configurations, and locations in the United States and its territories. Because they represent typical rather than extreme conditions, they are not suited for designing systems to meet the worst-case conditions occurring at a location. The data are available from the National Renewable Energy Laboratory for download or on CD. 

TMY3 files have somewhat replaced TMY2 files but all TMY2 files are available for download from the website.

## Solar and Wind Energy Resource Assessment (SWERA) Format

The Solar and Wind Energy Resource Assessment (SWERA) project, funded by the United Nations Environment Program, is developing high quality information on solar and wind energy resources in 14 developing countries. Currently typical year hourly data are available for 156 locations in Belize, Brazil, China, Cuba, El Salvador, Ethiopia, Ghana, Guatemala, Honduras, Kenya, Maldives, Nicaragua, and Sri Lanka. The data are available from the SWERA project web site. http://swera.unep.net/

The SWERA format is very similar to the TMY2 format except:  WMO stations are used (6 character) in the header and 4 digit years have been adopted in the data lines.

## WYEC2 Data Set/Format

Culminating in the early 1980s, ASHRAE published their "Weather Year for Energy Calculations" (WYEC) hourly weather files for 51 US and Canadian locations. These files were intended to support the then growing interest in computer simulation of energy use in buildings. In the late 1980s, a major revision was initiated – this included the addition of 26 Typical Meteorological Year (TMY) hourly weather files to the original WYEC data set and a number of improvements.

The work of revising and improving the WYEC data base was performed by at the National Renewable Energy Laboratory (NREL) as part of the Solar Radiation Resource Assessment Program (SRRAP), during the period 1989 through 1993. Richard Perez, at the State University of New York at Albany -- Atmospheric Sciences Research Center provided a substantial contribution to this work. The resulting set of 77 revised and corrected hourly weather files are referred to as WYEC Version 2 or "WYEC2" data set.

The WYEC2 format is used as the format for several other data sets. The WYEC2 data set, however, was not made available for download though, of course, those files can be read if desired with the EnergyPlus Weather Converter utility.

## Canadian Weather for Energy Calculations (CWEC) 

Produced by Numerical Logics in collaboration with Environment Canada and the National Research Council of Canada, the CWEC were derived using a methodology similar to the TMY2 and TMY. CWEC hourly files represent weather conditions that result in approximately average heating and cooling loads in buildings. The National Energy Code of Canada requires the use of a CWEC file representative of a location when the performance path and customized design calculations are chosen as the means of building energy consumption compliance. The CWEC follow the ASHRAE WYEC2 format and were derived from the Canadian Energy and Engineering Data Sets (CWEEDS) of hourly weather information for Canada from the 1953-1995 period of record.

The 80 CWEC files contain hourly weather observations representing an artificial one-year period specifically designed for building energy calculations. All 80 locations in the CWEC data set are available for download in EnergyPlus weather format.

## International Weather for Energy Calculations (IWEC)

The IWEC are the result of ASHRAE Research Project 1015 conducted by Numerical Logics and Bodycote Materials Testing Canada for ASHRAE Technical Committee 4.2 Weather Information. The IWEC data files are 'typical' weather files suitable for use with building energy simulation programs for 227 locations outside the USA and Canada.

The files are derived from up to 18 years of DATSAV3 hourly weather data originally archived at the U S National Climatic Data Center. The weather data is supplemented by solar radiation estimated on an hourly basis from earth-sun geometry and hourly weather elements, particularly cloud amount information. The IWEC CD-ROM is available from ASHRAE.

The Department of Energy has licensed the IWEC data from ASHRAE. Our license with ASHRAE allows us to: "Distribute versions of the individual IWEC files in converted format suitable for EnergyPlus (EPW). Make the EnergyPlus versions of the IWEC files available to users at no cost via this EnergyPlus web site."  All 227 locations in the IWEC data set are available for download in EnergyPlus weather format.

The IWEC source data is © 2001 American Society of Heating, Refrigerating and Air-Conditioning Engineers (ASHRAE), Inc., Atlanta, GA, USA. http://www.ashrae.org/ All rights reserved as noted in the License Agreement and Additional Conditions.

IWEC also describes a data format, very similar to the TMY2 data format.

## Typical Meteorological Year (TMY)

Data for 230 locations in the USA plus four locations in Cuba, Marshall Islands, Palau, and Puerto Rico, derived from a 1948-1980 period of record. Many of the locations in the TMY data set were subsequently updated by the TMY2.

Similar to the TMY2, the TMY are data sets of hourly values of solar radiation and meteorological elements for a 1-year period. Their intended use is for computer simulations of solar energy conversion systems and building systems to facilitate performance comparisons of different system types, configurations, and locations in the United States and its territories. Because they represent typical rather than extreme conditions, they are not suited for designing systems to meet the worst-case conditions occurring at a location. The data are available for purchase from the National Climatic Data Center.

All TMY locations are available for download in EnergyPlus weather format.

TMY also describes a data format.

## California Climate Zones 2 (CTZ2)

Updated weather data for 16 California climate zones for use to demonstrate compliance with Title 24 with approved building energy simulation programs. All 16 CTZ2 weather files are available for download in EnergyPlus weather format. The original source data is available from the California Energy Commission. These source data files are described using the WYEC2 format.

## Solar and Wind Energy Resource Assessment (SWERA)

The Solar and Wind Energy Resource Assessment (SWERA) project, funded by the United Nations Environment Program, is developing high quality information on solar and wind energy resources in 13 developing countries. Currently typical year hourly data are available for 48 locations in Belize, Cuba, El Salvador, Guatemala, Honduras, Maldives, Nicaragua, and Sri Lanka. The data are available from the SWERA project web site. SWERA plans to release new data for Brazil, Ethiopia, Ghana, Kenya, and Nepal over the coming few months. These source data files are described using the SWERA format.

## Spanish Weather for Energy Calculations (SWEC)

Originally developed for use with Calener, a new program for building energy labelling in Spain, these weather files cover all 52 Spanish provincial capitals. Calener was developed by the Grupo de Termotecnia of the Escuela Superior de Ingeneiros in Seville for the Spanish Government. The weather files were synthetically generated using Climed (Portuguese software developed by Ricardo Aguiar) from mean monthly data coming from the Spanish Meteorological National Institute. These weather files were converted from the DOE-2 binary to EnergyPlus format and include constant wind speeds of 6.7 m/s.

For more information on these weather files, contact: Profesor Luis Pérez-Lombard (lpl@tmt.us.es) Escuela Superior de Ingenieros

## Italian Climatic data collection "Gianni De Giorgio" (IGDG)

Developed for use in simulating renewable energy technologies, this set of 66 weather files is based on a 1951-1970 period of record. The data were created by Professor Livio Mazzarella, Politecnico di Milano, and is named in honor of Gianni de Giorgio.

## Chinese Typical Year Weather (CTYW)

Developed for use in simulating building heating and air conditioning loads and energy use, and for calculating renewable energy utilization, this set of 57 weather files is based on a 1982-1997 period of record with data obtained from the U. S. National Climatic Data Center. The data were created by Prof. ZHANG Qingyuan of Tsukuba University Japan, in collaboration with Joe Huang of Lawrence Berkeley National Laboratory. The original typical year weather files are contained in: Zhang Qingyuan and Joe Huang. 2004. Chinese Typical Year Weather Data for Architectural Use (in Chinese). ISBN 7-111-14810-X. Beijing: China Machine Press. Available from: China Machine Press; No. 22 Baiwanzhuang Dajie; Beijing, CHINA 100037.

The CTYW files are no longer available for download from the web site.

## INETI Synthetic data for Portugal

Two weather files for Portugal developed by Ricardo Aguiar of Instituto Nacional de Engenharia, Tecnologia e Inovação (INETI). Synthetic data set based on spatially interpolation of public climatic data published by Instituto de Meteorologia 1951-80 combined with INETI owned data and other freely available data sources. INETI has granted DOE permission to distribute versions of the individual INETI files in converted format suitable for EnergyPlus (EPW) and make those files available to users at no cost via this EnergyPlus web site.

The INETI synthetic data are © 2005 Instituto Nacional de Engenharia, Tecnologia e Inovação, Lisboa, Portugal. http://www.ineti.pt

## ISHRAE India Weather Data Files (ISHRAE)

Developed for use in simulating building heating and air conditioning loads and energy use, and for calculating renewable energy utilization, this set of 58 weather files was developed by the Indian Society of Heating, Refrigerating, and Air-Conditioning Engineers (ISHRAE). The source data are Copyright 2005 ISHRAE. ISHRAE has made these data available to DOE under license for use at no cost to the EnergyPlus user community.

## City University of Hong Kong (CUHK) Data Set

CityUHK-45007 -- WMO#450070   Typical year file Hong Kong originally in IWEC format spreadsheet jointly developed by Dr TT Chow and ALS Chan of the City University of Hong Kong supported by a CERG grant from the Research Grants Council of the Hong Kong Special Administrative Region of China. Solar radiation measured from observatory station at 22.32N  114.17'E  65m above mean sea level.

## Chinese Standard Weather Data (CSWD)

Developed for use in simulating building heating and air conditioning loads and energy use, and for calculating renewable energy utilization, this set of 270 typical hourly data weather files. These data were developed by Dr. Jiang Yi, Department of Building Science and Technology at Tsinghua University and China Meteorological Bureau. The source data include annual design data, typical year data,  and extreme years for maximum enthalpy, and maximum and minimum temperature and solar radiation.

China Meteorological Bureau, Climate Information Center, Climate Data Office and Tsinghua University, Department of Building Science and Technology. 2005. China Standard Weather Data for Analyzing Building Thermal Conditions, April 2005. Beijing: China Building Industry Publishing House, ISBN 7-112-07273-3 (13228). http://www.china-building.com.cn.

## Kuwait Weather Data from Kuwait Institute for Scientific Research (KISR)

Two weather files for Kuwait based on measured meteorological data for Kuwait International Airport and KISR's coastal weather station. Provided by KISR in spreadsheet format.

## Egyptian Weather for Energy Calculations (EWEC)

Developed for standards development and energy simulation by Joe Huang from data provided by National Climatic Data Center for periods of record from 12 to 21 years, all ending in 2003. Joe Huang and Associates, Moraga, California, USA.

## Israel weather data (MSI)

Weather data for Israel locations developed by Faculty of Civil and Environmental Engineering, Technion - Israel Institute of Technology, Haifa, Israel, from data provided by the Israel Meteorological Service.

## Representative Meteorological Year (RMY) Australia Climatic Data

RMY Australia Representative Meteorological Year Climate Files Developed for the Australia Greenhouse Office for use in complying with Building Code of Australia. These data are licensed through ACADS BSG Ltd for use by EnergyPlus users. For use in any other formats, users must contact ACADS BSG Ltd for licensing information.

The RMY data are © 2006 Commonwealth of Australia, Department of the Environment and Water Resources, Australia Greenhouse Office, Canberra, ACT, Australia. www.greenhouse.gov.au/buildings/code.html All intellectual property rights reserved.

## Iranian Typical Meteorological Year (ITMY)

Typical year weather files have been created for Tehran from weather data for 1992 through 2003 published by the Islamic Republic of Iran Meteorological Organization (IRIMO) for Tehran Mehrabad. Developed for standards development and energy simulation by Joe Huang, White Box Technologies.

## New Zealand National Institute of Water & Atmospheric Research Ltd (NIWA)

The New Zealand Energy Efficiency and Conservation Authority (EECA) has developed a Home Energy Rating Scheme (HERS) for New Zealand households based on software simulation of energy loss and demand. The software requires hourly data to represent the different climates zones around New Zealand, especially for larger population centres. These climate data consist of hourly records for an artificial year created from twelve representative months.

Liley, J Ben, Hisako Shiona, James Sturman, David S Wratt. 2007. Typical Meteorological Years for the New Zealand Home Energy Rating Scheme. Prepared for the Energy Efficiency and Conservation Authority. NIWA Client Report: LAU2007- 02-JBL. NIWA, Omakau, New Zealand.

## Chartered Institution of Building Services Engineers (CIBSE)

The CIBSE, in association with the (UK) Met Office has produced 'Test Reference Years' and 'Design Summer Years' for 14 UK locations for use with building energy simulation software. The data sets are available in various formats, including EnergyPlus/ESP-r. These data are NOT available on the EnergyPlus web site. For further details, see:

www.cibse.org/index.cfm?go=publications.view&PubID=332&S1=y&L1=0&L2=0

## Real Time Data

Real-Time weather data is available from the EnergyPlus web site. From the web site: "Hourly weather data from stations across the world is continuously collected and stored into a local database. The data is available through this web interface. Most stations have information for dry bulb temperature, wet bulb temperature, wind speed/direction, atmospheric pressure, visibility, cloud conditions, and precipitation type. Data may not be available for all stations and may not be contiguous for time period selected."  The data is available in two output formats: CSV and IWEC. Note that their CSV is not the same as EnergyPlus CSV format. If you wish to get weather data from the real time sources, it may be easier to use the IWEC format with the EnergyPlus WeatherConverter program; HOWEVER, they do not collect solar data and, currently, the WeatherConverter cannot generate solar data for these data files.

## Meteonorm Data

Meteonorm software can be used when there is no measured data near the location for the simulation. Meteonorm extrapolates hourly data from statistical data for a location. Where statistical data aren't available, Meteonorm interpolates from other nearby sites. Generally a statistical approach is a last resort -- weather files generated from statistics will not demonstrate the normal hour-to-hour and day-to-day variability seen in measured data. Meteonorm version 6 will directly write EPW files.

## Other Formats

The data sets and formats described above are some of the newest formats available for use with building simulation programs. Source data comes in various formats. Typically the files are ASCII, but the data items, units, item location, and record length vary from format to format. NCDC can provide historical data in a variety of formats: TD-3280, TD-3510, TD-9950 (DATSAV2), TD-9956 (DATSAV3) and TD-1440 (CD144). The EnergyPlus weather processor cannot process any of the types directly.

Table: Summary of Downloadable Weather Data by Type

|**WMO Region**|
|---------------------------|
|**North and Central America**||||
|**USA**|**Canada**|**Central America**|**Africa**|**Asia**|**South America**|**Southwest Pacific**|**Europe**|**Total**
**CityUHK**|||||1||||1
**CSWD**|||||270||||270
**CTZ2**|16||||||||16
**CWEC**||80|||||||80
**ETMY**||||11|||||11
**IGDG**||||||||66|66
**IMGW**||||||||61|61
**INETI**||||||||2|2
**ISHRAE**|||||58||||58
**ITMY**|||||1||||1
**IWEC**|||5|12|49|19|20|122|227
**KISR**|||||2||||2
**Los Alamos TMY2**|1||||||||1
**MSI**||||||||4|4
**NIWA**|||||||16||16
**RMY**|||||||80||80
**SWEC**||||2||||50|156
**SWERA**|||37|33|66|20|||156
**TMY**|229||1||||4||234
**TMY2**|235||1||||1||237
**TMY3**|1011||7||||2||1020
**Totals**|1492|80|51|58|447|39|123|305|2699

One other format worth mentioning is TRY. TRY is "test reference year" data that did not include solar radiation data. "Test Reference Year" is a term that usually denotes selection of a specific year of "real" data based on some statistical procedure. The original TRY data (TD-9706) was based on an ASHRAE procedure to select the data from a "period of record". "The principle of the selection is to eliminate years in the period of record containing months with extremely high or low mean temperatures until only one year remains."  The weather conversion utility cannot process data in "TRY" format. However, many organizations develop data for reference year data (e.g. European TRY, Moisture Reference Data).

## Custom Format

Using a "definitions" file (see Description of "Def" input file), the weather converter can process a wide range of data formats. In the table above, both the GDG and CTYW weather data was processed by a custom format approach.

## Solar Data

Source weather data files may or may not contain solar data. All of the files that can be processed by the EnergyPlus Weather conversion utility contain solar data. The weather processor will transfer this data to the EPW weather file and the EnergyPlus simulation program will use it.

Historical weather data files in CD144 format do not contain solar data nor is such data generally available for a specific location and time period. In this case, ersatz solar data must be generated from cloud cover and other data using sky models and regression formulas. Currently, neither the Weather Conversion utility nor the EnergyPlus program synthesizes this data.  However, the weather conversion utility can use any two of the commonly recorded data (i.e. two of Global Horizontal Radiation, Horizontal Diffuse Radiation and Direct Normal (or Direct Horizontal) Radiation to calculate the EnergyPlus primary solar data of Direct Normal and Horizontal Diffuse Radiation values).