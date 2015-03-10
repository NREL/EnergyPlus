# CSVproc

This simple post processing utility may be useful when doing parametric analyses. It takes a CSV (comma separated values file) and performs some simple statistics. It is a very small application with no interface. It is typically executed from the command line.

#. Open a DOS command prompt window (Start  Programs  Accessories  Command Prompt)
#. Change to the directory where EnergyPlus is installed (modify the commands below if you did not install EnergyPlus in the default install path):

~~~~~~~~~~~~~~~~~~~~
    C:
    CD \<root folder>\
~~~~~~~~~~~~~~~~~~~~

#. Change to the specific folder for the coefficient conversion applications:

~~~~~~~~~~~~~~~~~~~~
    CD PostProcess
~~~~~~~~~~~~~~~~~~~~

#. Run the program:

CSVproc <filename>

Where <filename> is the name of a CSV file, including extension. There is a simple readme.txt file in the folder. The program performs some simple statistics on each column and creates a new file with the same name without extension and -PROC.CSV added to the name.

The statistics performed on each column are:

SUM

MAX

MIN

AVERAGE

COUNT

COUNTIF > 0

COUNTIF > 5

COUNTIF > 10

COUNTIF > 15

COUNTIF > 20

COUNTIF > 25

COUNTIF > 30

COUNTIF > 35

COUNTIF > 40

COUNTIF > 45

COUNTIF > 50

COUNTIF > 55

COUNTIF > 60

COUNTIF > 65

COUNTIF > 70

COUNTIF > 75

COUNTIF > 80

COUNTIF > 85

COUNTIF > 90

COUNTIF > 95

COUNTIF > 100

COUNTIF = 1

COUNTIF < 19.9

COUNTIF > 24.0

Obviously, not all statistics are relevant for every output report variable. The average is based on the sum divided by the number of non-blank rows. The average is not based on the length of time for that timestep. Due to this, CSVproc is best suited for an hourly output file.

Source code is available upon request from jglazer@gard.com.
