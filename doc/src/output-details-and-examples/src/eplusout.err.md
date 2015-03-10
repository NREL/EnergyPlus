# eplusout.err

This file is *very important* to every simulation run. All of the warnings, errors, etc that occur during the run will show up in this file. They may show up in other files as well. The first line of the error file is also significant:

~~~~~~~~~~~~~~~~~~~~

    Program Version,EnergyPlus, <version>,IDD_Version <version>
~~~~~~~~~~~~~~~~~~~~

This shows not only the current version of the program but the "version" of the Energy+.idd file that was used.

Table: EnergyPlus Errors

Error Level|Action
-----------|------
Warning|Take note
Severe|Should Fix
Fatal|Program will abort

The previous table illustrates the three levels of errors that occur in the eplusout.err file. Several other message lines may be shown as well. For example:

~~~~~~~~~~~~~~~~~~~~

    ** Warning ** World Coordinate System selected.  Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored.
    **   ~~~   ** These may be used in daylighting reference point coordinate calculations  but not in normal geometry inputs.
~~~~~~~~~~~~~~~~~~~~

The line that includes the "~~~" is a "continue" error line. It continues from the previous line to help describe the context of the error.

Some common errors, their consequences, and what to do about them follows:

~~~~~~~~~~~~~~~~~~~~

    ** Severe  ** IP: Possible incorrect IDD File
    ** Severe  ** IP: Possible Invalid Numerics
    **  Fatal  ** IP: Errors occurred on processing IDF file. Preceding condition(s) cause termination.
~~~~~~~~~~~~~~~~~~~~

The previous errors cause program termination. The most likely cause is that you have an "old" IDF and have not converted it to work with the current version of the program. In this case, you will likely has some other hints such as alphas in numeric fields or too many fields in an object. Energyplus also has built in range checking:

~~~~~~~~~~~~~~~~~~~~

    ** Severe  ** Out of range value Numeric Field#7 (Sky Clearness), value=100.00000, range={>=0.0 and <=1.2}, in SIZINGPERIOD:DESIGNDAY=CHICAGO ANN CLG .4% CONDNS WB=>MDB
~~~~~~~~~~~~~~~~~~~~

If these occur during the initial processing of the IDF, you will see a termination clause:

~~~~~~~~~~~~~~~~~~~~

    ** Severe  ** IP: Out of "range" values found in input
    ** Severe  ** IP: Out of "range" values and/or blank required fields found in input
    **  Fatal  ** IP: Errors occurred on processing IDF file. Preceding condition(s) cause termination.
~~~~~~~~~~~~~~~~~~~~

The error message should supply you with enough information to find the line with the error. More details on error message formatting and some standard error messages are described in the Tips & Tricks document.