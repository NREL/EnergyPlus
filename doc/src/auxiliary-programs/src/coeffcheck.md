# CoeffCheck

CoeffCheck is a program to print out a performance map given a bi-quadratic performance curve.

EP-Launch can be used to run the CoeffCheck program. CoeffCheck is one of the options on the Utilities tab in EP-Launch. See the EP-Launch section in this document for more information on how to use EP-Launch with the CoeffCheck program. When using EP-Launch to run CoeffCheck the input file must have the file extension .cci and will create an output file with the file extension .cco.

You can also run the CoeffCheck program as a console application. The input file then must be from file CoeffCheckInput.txt (status=OLD). There are 6 lines of ascii input.

For example:

~~~~~~~~~~~~~~~~~~~~
    VarSpeedCoolCapFt
    0.476428E+00,0.401147E-01,0.226411E-03,-0.827136E-03,-0.732240E-05,-0.446278E-03
    12.777778,23.888889
    23.888889,46.111111
    19.444444,35.0
    2.777778
~~~~~~~~~~~~~~~~~~~~

The 1st line is the user selected name of the curve.

The 2nd line contains the 6 biquadratic curve coefficients, comma separated.

The 3rd line contains the min and max values of the 1st independent variable, comma separated

The 4th line contains the min and max values of the 2nd independent variable, comma separated

The 5th line contains the rated values of the 1st & 2nd independent variables, comma separated

The 6th line contains the delta T for the output performance map

The output file is CoeffCheckOutput.txt (status=NEW).

There is an example input file and an example output file installed with the program.
