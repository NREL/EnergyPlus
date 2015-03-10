# Program Inputs

In order to use the CalcSoilSurfTemp program, a weather data file is used. The entire year of weather data is used to calculate parameters of the soil surface temperature.

The CalcSoilSurfTemp program is simple and requires only two input fields: soil condition and soil surface condition. First, the user will see the four following options in the screen for the selection of the soil condition:

1. HEAVY AND SATURATED

2. HEAVY AND DAMP

3. HEAVY AND DRY

4. LIGHT AND DRY

Among them, the user should select the number corresponding to the particular soil condition. This determines the thermal diffusivity and thermal conductivity of the surrounding soil.

After the selection of soil condition, the user should also select the number corresponding to the condition of the ground surface above the earth tube from the eight following options:

1. BARE AND WET

2. BARE AND MOIST

3. BARE AND ARID

4. BARE AND DRY

5. COVERED AND WET

6. COVERED AND MOIST

7. COVERED AND ARID

8. COVERED AND DRY

This determines the absorption coefficient and the fraction of evaporation rate of the ground surface.

Note that both soil condition and soil surface condition are the average of the year -  not a particular time period within the year.

From this information and an analysis of the weather data, the CalcSoilSurfTemp program calculates the annual average soil surface temperature, the amplitude of soil surface temperature, and the phase constant of soil surface temperature. The user must then add these parameters as input parameter into earth tube model in EnergyPlus.