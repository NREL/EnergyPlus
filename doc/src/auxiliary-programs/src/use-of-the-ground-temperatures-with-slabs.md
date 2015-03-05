# Use of the Ground Temperatures with Slabs

The Slab program produces temperature profiles for the outside surface at the core and at the perimeter of the slab. It also produces the average based on the perimeter and core areas used in the calculation. This allows the user to apply the Slab temperatures one of two ways in EnergyPlus:

*Option 1 – Core and Perimeter Temperatures*:  The EnergyPlus OtherSideCoefficients object can be used to provide two sets of twelve monthly average ground temperature values. In this way, both the perimeter and the core values from the Slab program can be used in the succeeding EnergyPlus run. This method assumes that the floor slab will be described using at least two different heat transfer surfaces. The use of OtherSideCoefficients to provide additional ground contact surfaces is described in detail in the "Multiple Ground Temperatures" section below.

*Option 2 – Average Temperatures:*  Use the monthly average temperatures produced by the Slab program in the EnergyPlus GroundTemperatures object. This will provide an average ground temperature at the outside face of any heat transfer surface whose OutsideFaceEnvironment field is set to "ground".

EnergyPlus accepts twelve separate monthly average inside temperatures. In addition, it is possible to add an hourly sinusoidal variation of the inside temperature with a 24 hour period sine function. This was included to show the effect of something such as night setback on the face temperature. Generally, the effect is quite small.

First the results for a monthly specified constant average inside temperature. The location is Minneapolis, and the slab is insulated.

~~~~~~~~~~~~~~~~~~~~
    Monthly Slab Outside Face Temperatures, C
    Perimeter Area: 304.00  Core Area: 1296.00
    Month   Average   Perimeter    Core      Inside
    1      17.67     16.11      18.03       18.0
    2      17.45     15.92      17.81       18.0
    3      17.43     16.07      17.74       18.0
    4      19.00     17.82      19.27       20.0
    5      19.24     18.23      19.48       20.0
    6      19.31     18.42      19.52       20.0
    7      20.92     20.14      21.11       22.0
    8      21.17     20.44      21.35       22.0
    9      21.22     20.45      21.40       22.0
    10      21.21     20.26      21.44       22.0
    11      19.62     18.54      19.88       20.0
    12      19.35     17.99      19.67       20.0
~~~~~~~~~~~~~~~~~~~~

The resulting heat flux is shown below. The inside heat transfer coefficient and slab thermal properties are specified in the input file. For this example the total thermal resistance from the inside air to the slab bottom surface was 0.27 (m^2^ C)/W. This value is controlled by the user with the inside heat transfer coefficient and slab thermal properties values in the slab program input file.

Month|Average|Perimeter|Core|Inside|Perimeter Heat Flux W/m^2^|Average Heat Flux W/m^2^
-----|-------|---------|----|------|--------------------------|------------------------
1|17.67|16.11|18.03|18|7.00|1.22
2|17.45|15.92|17.81|18|7.70|2.04
3|17.43|16.07|17.74|18|7.15|2.11
4|19|17.82|19.27|20|8.07|3.70
5|19.24|18.23|19.48|20|6.56|2.81
6|19.31|18.42|19.52|20|5.85|2.56
7|20.92|20.14|21.11|22|6.89|4.00
8|21.17|20.44|21.35|22|5.78|3.07
9|21.22|20.45|21.4|22|5.74|2.89
10|21.21|20.26|21.44|22|6.44|2.93
11|19.62|18.54|19.88|20|5.41|1.41
12|19.35|17.99|19.67|20|7.44|2.41

Then for the same conditions, the results with a 2 degree C amplitude 24-hour sine wave variation. Notice that the inside temperatures are the same since they are monthly averages and the daily variation oscillates about the mean. The core and perimeter slab temperatures are affected slightly.

~~~~~~~~~~~~~~~~~~~~
    Monthly Slab Outside Face Temperatures, C
    Perimeter Area: 304.00  Core Area: 1296.00
    Month   Average   Perimeter    Core      Inside
    1      17.51     16.03      17.86       18.0
    2      17.29     15.85      17.63       18.0
    3      17.27     16.00      17.57       18.0
    4      18.87     17.77      19.13       20.0
    5      19.11     18.16      19.34       20.0
    6      19.17     18.34      19.37       20.0
    7      20.81     20.07      20.98       22.0
    8      21.05     20.36      21.21       22.0
    9      21.09     20.38      21.26       22.0
    10      21.08     20.19      21.29       22.0
    11      19.47     18.45      19.71       20.0
    12      19.20     17.92      19.51       20.0
~~~~~~~~~~~~~~~~~~~~

An example of a 24-hour inside temperature profile for this case is shown below. The sine wave amplitude was 2 C.

~~~~~~~~~~~~~~~~~~~~
    Day       Hour   Perim Out Ts    Core Out Ts    Inside Temp
    1           1   17.30827       19.15832       18.51749
    1           2   17.29503       19.15274       18.99974
    1           3   17.30236       19.16732       19.41389
    1           4   17.32258       19.19376       19.73175
    1           5   17.34834       19.22526       19.93166
    1           6   17.37288       19.25529       20.00000
    1           7   17.39023       19.27798       19.93212
    1           8   17.39544       19.28838       19.73265
    1           9   17.38485       19.28117       19.41517
    1          10   17.35602       19.24733       19.00130
    1          11   17.30590       19.18686       18.51924
    1          12   17.23507       19.10210       18.00180
    1          13   17.14650       18.99703       17.48425
    1          14   17.04291       18.87713       17.00183
    1          15   16.92873       18.74895       16.58738
    1          16   16.81076       18.61963       16.26915
    1          17   16.69609       18.49656       16.06881
    1          18   16.59243       18.38671       16.00000
    1          19   16.50669       18.29626       16.06741
    1          20   16.44276       18.23010       16.26645
    1          21   16.40369       18.19161       16.58356
    1          22   16.38873       18.18218       16.99714
    1          23   16.39435       18.19834       17.47902
    1          24   16.41942       18.23298       17.99639
~~~~~~~~~~~~~~~~~~~~

A plot of the daily profiles is shown below. Note that the inside temperature change of 4 C produces only a small change in the slab lower face temperature.

![Daily Temperature Profiles (Slab)](media/daily-temperature-profiles-slab.jpeg)

The resulting heat fluxes are shown below. They can be compared with the fluxes shown above for the constant inside temperature run. The changes resulting from a fairly large 4 C daily temperature variation are probably not significant.

**Month**|**Average**|**Perimeter**|**Core**|**Inside**|**Perimeter Heat Flux W/m^2^**|**Average Heat Flux W/m^2^**
---------|-----------|-------------|--------|----------|------------------------------|----------------------------
1|17.51|16.03|17.86|18|7.30|1.81
2|17.29|15.85|17.63|18|7.96|2.63
3|17.27|16|17.57|18|7.41|2.70
4|18.87|17.77|19.13|20|8.26|4.19
5|19.11|18.16|19.34|20|6.81|3.30
6|19.17|18.34|19.37|20|6.15|3.07
7|20.81|20.07|20.98|22|7.15|4.41
8|21.05|20.36|21.21|22|6.07|3.52
9|21.09|20.38|21.26|22|6.00|3.37
10|21.08|20.19|21.29|22|6.70|3.41
11|19.47|18.45|19.71|20|5.74|1.96
12|19.2|17.92|19.51|20|7.70|2.96