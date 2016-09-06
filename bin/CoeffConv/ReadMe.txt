CoeffConv is a program to convert DOE-2 temperature dependent curves in
Fahrenheit to EnergyPlus curves in Centigrade. The program converts
the Doe-2 coefficients of a biquadratic curve to the equivalent
EnergyPlus biquadratic curve coefficients.

Input is from file CoeffConvInput.txt (status=OLD). There are 6 lines of ascii input.
For example:

VarSpeedCoolCapFt
-0.29382,0.0222213,0.00006988,0.0040928,-0.00000226,-0.00013774
55.,75.
75.,115.
67.0,95.0
5.0

The 1st line is the user selected name of the curve.
The 2nd line contains the 6 biquadratic curve coefficients, comma separated. These are the Doe-2 coefficients.
The 3rd line contains the min and max values of the 1st independent variable, comma separated, deg F.
The 4th line contains the min and max values of the 2nd independent variable, comma separated, deg F.
The 5th line contains the rated values of the 1st & 2nd independent variables, comma separated, deg F.
The 6th line contains the delta T for the output performance maps.

All the input lines should start in column 1.
The above sequence can be repeated up to 100 times.

The output file is CoeffConvOutput.txt (status=NEW) (that means you need to delete any existing CoeffConvOutput.txt).

The output file will contain the EnergyPLus idf input for the curve, as well as any error messages;
Also the Doe-2 and EnergyPlus curve values at the rating point (where the value should be 1.0) and performance
maps for the curves - both Doe-2 and EnergyPlus.

There is an example input file & an example output file.

======================================================================================================================


CoeffCheck is a program to print out a performance map given a bi-quadratic performance
curve.

Input is from file CoeffCheckInput.txt (status=OLD). There are 6 lines of ascii input.
For example:

VarSpeedCoolCapFt
0.476428E+00,0.401147E-01,0.226411E-03,-0.827136E-03,-0.732240E-05,-0.446278E-03
12.777778,23.888889
23.888889,46.111111
19.444444,35.0
2.777778

The 1st line is the user selected name of the curve.
The 2nd line contains the 6 biquadratic curve coefficients, comma separated.
The 3rd line contains the min and max values of the 1st independent variable, comma separated
The 4th line contains the min and max values of the 2nd independent variable, comma separated
The 5th line contains the rated values of the 1st & 2nd independent variables, comma separated
The 6th line contains the delta T for the output performance map
The output file is CoeffCheckOutput.txt (status=NEW).

There is an example input file and an example output file.
