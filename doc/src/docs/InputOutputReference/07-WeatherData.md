Weather Data
============

The description of the weather data format (a simple text-based format) is well described in the Auxiliary Programs document and won’t be repeated here. Descriptions here will be pertinent to running the EnergyPlus program but you may wish to peruse the other descriptions as the statistics reports from the weather files may be useful during design and simulation.

Missing Weather File Data
-------------------------

The weather description of data contains “missing” descriptors, a new concept not introduced previously in our IDD conventions. In this case, it will be processed as though those values are “missing” in the weather conversions and/or EnergyPlus weather processing. This may not always be desirable though EnergyPlus will fill in “missing” value with something “appropriate”. Eventually, these missing values will be available through the IDD and users will be able to supply their own values or EnergyPlus will calculate those values (such as radiation and illuminance) that are not a simple value replacement. Until then, the following are used:

Table 44. Missing weather replacement values

<table class="table table-striped">
<tr>
<th>Data item</th>
<th>Supplied Value</th>
<th>Units</th>
</tr>
<tr>
<td>Dry-bulb Temperature</td>
<td>6</td>
<td>C</td>
</tr>
<tr>
<td>Dewpoint Temperature</td>
<td>3</td>
<td>C</td>
</tr>
<tr>
<td>Relative Humidity</td>
<td>50</td>
<td>%</td>
</tr>
<tr>
<td>Atmospheric Pressure</td>
<td>Standard** Barometric Pressure (altitude based)</td>
<td>Pa</td>
</tr>
<tr>
<td>Wind Speed</td>
<td>2.5</td>
<td>m/s</td>
</tr>
<tr>
<td>Wind Direction</td>
<td>180</td>
<td>Deg</td>
</tr>
<tr>
<td>Total Sky Cover</td>
<td>5</td>
<td>(tenths)</td>
</tr>
<tr>
<td>Opaque Sky Cover</td>
<td>5</td>
<td>(tenths)</td>
</tr>
<tr>
<td>Visibility</td>
<td>777.7</td>
<td>Km</td>
</tr>
<tr>
<td>Ceiling Height</td>
<td>77777</td>
<td>m</td>
</tr>
<tr>
<td>Precipitable Water</td>
<td>0</td>
<td>Mm</td>
</tr>
<tr>
<td>Aerosol Optical Depth</td>
<td>0</td>
<td>Broadband turbidity</td>
</tr>
<tr>
<td>Snow Depth</td>
<td>0</td>
<td>Cm</td>
</tr>
<tr>
<td>Days Since Last Snow</td>
<td>88</td>
<td>Days</td>
</tr>
<tr>
<td>Radiation Values (Direct/Diffuse)</td>
<td>0.0</td>
<td>Wh/m2</td>
</tr>
<tr>
<td>Illuminance Values</td>
<td>0.0</td>
<td>lux (or CD/m2)</td>
</tr>
</table>

\*\*Standard Barometric pressure based on location elevation is taken from ASHRAE 2001 Handbook of Fundamentals, Page 6.1 & 2.

Weather Data Hourly Interpolation
---------------------------------

Much of the existing weather data is produced in hourly format (refer to the discussion of data formats and weather conversion processing in the Auxiliary Programs Document). In order to match the hourly data in a continuous manner for EnergyPlus (where the timesteps might be less than hourly), simple interpolation between “last hour’s” values and “this hour’s” values is performed; we will refer to this as “Last Hour Interpolation”.

Note that in versions prior to V1.0.3, the interpolation was done between “this hour” and “next hour”. If required, we will refer to this as “Next Hour Interpolation”.

Remember that hour 1 for EnergyPlus is the time interval 00:00:01AM to 1:00:00AM. Hour 2 is 1:00:01AM to 2:00:00AM (hh:mm:ss). With the interpolation scheme (Last Hour Interpolation) and reporting weather data values at timestep resolution, the “hour” values reported should match with the hourly weather data values. (Note that reporting “hourly” resolution will not do this unless your “Number of Timesteps per Hour” is 1).

Note that this interpolation applies to outdoor data only – schedule values are not interpolated in this manner.

The weighting scheme / interpolation is simply:

<div>\[Valu{e_{TimeStep}} = LastHourValue\cdot Weigh{t_{LastHour}} + ThisHourValue\cdot Weigh{t_{ThisHour}}\]</div>

where

<div>\[Weigh{t_{ThisHour}} = \frac{{CurrentTimeStep}}{{Number\,of\,TimeSteps\,in\,Hour}}\]</div>

<div>\[Weigh{t_{LastHour}} = 1.0 - Weigh{t_{ThisHour}}\]</div>

To explain further, the weighting for four (4) timesteps in hour is:

Table 45. Illustration of Data Interpolation for 15 minute timesteps

<table class="table table-striped">
<tr>
<th>TimeStep</th>
<th>Time (mm:ss)</th>
<th>Weight<sub>LastHour</sub></th>
<th>Weight<sub>ThisHour</sub></th>
</tr>
<tr>
<td>1</td>
<td>00:01 to 15:00</td>
<td>.75</td>
<td>.25</td>
</tr>
<tr>
<td>2</td>
<td>15:01 to 30:00</td>
<td>.5</td>
<td>.5</td>
</tr>
<tr>
<td>3</td>
<td>30:01 to 45:00</td>
<td>.25</td>
<td>.75</td>
</tr>
<tr>
<td>4</td>
<td>45:01 to 60:00</td>
<td>0</td>
<td>1.0</td>
</tr>
</table>

And similarly for six (6) timesteps in hour:

Table 46. Illustration of Data Interpolation for 10 minute timesteps

<table class="table table-striped">
<tr>
<th>TimeStep</th>
<th>Time (mm:ss)</th>
<th>Weight<sub>LastHour</sub></th>
<th>Weight<sub>ThisHour</sub></th>
</tr>
<tr>
<td>1</td>
<td>00:01 to 10:00</td>
<td>0.833333333333333</td>
<td>0.166666666666667</td>
</tr>
<tr>
<td>2</td>
<td>10:01 to 20:00</td>
<td>0.666666666666667</td>
<td>0.333333333333333</td>
</tr>
<tr>
<td>3</td>
<td>20:01 to 30:00</td>
<td>.5</td>
<td>.5</td>
</tr>
<tr>
<td>4</td>
<td>30:01 to 40:00</td>
<td>0.333333333333333</td>
<td>0.666666666666667</td>
</tr>
<tr>
<td>5</td>
<td>40:01 to 50:00</td>
<td>0.166666666666667</td>
<td>0.833333333333333</td>
</tr>
<tr>
<td>6</td>
<td>50:01 to 60:00</td>
<td>0</td>
<td>1.0</td>
</tr>
</table>

This interpolation gives a smooth, continuous look to the outdoor data. For day boundaries (i.e. from hour 24 to hour 1), the values from the previous day’s last hour (23:00:01 to 24:00:00). For the a design day, this is the values for hour 24. For the first day of a weather simulation, it is likewise values for hour 24 of that day. For subsequent weather days, it will be the last hour of the previous day.

Weather File Data Reporting (errors) during Simulation
------------------------------------------------------

Missing data on the weather file used will be summarized on the **eplusout.err** file. In EnergyPlus, “missing data” is shown only for fields that EnergyPlus will use. For the “WeatherCodes”, an invalid field count (where the number of items in the field does not = 9) will be shown. The number of items count refers to the number of records on the weather file that are in error or missing – for an hourly weather file, this is the number of hours. Likewise out of range values (see specific fields in the previous definition) will be counted for each occurance and summarized. Note that the out of range values will not be changed by EnergyPlus and could affect your simulation results.



For example:

```
   ** Warning ** Missing Data Found on Weather Data File
   ************* Missing Atmospheric Pressure, Number of items=   48
   ************* Missing Dry Bulb Temperatures, Number of items=   4
   ** Warning ** Out of Range Data Found on Weather Data File
   ************* Out of Range Dry Bulb Temperature [&gt;-70,&lt;70], Number of items=   1
```
