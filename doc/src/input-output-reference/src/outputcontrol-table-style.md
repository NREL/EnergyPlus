# OutputControl:Table:Style

The OutputControl:Table:Style object controls how all standardized reports are produced.

## Inputs

#### Field: Column Separator

You have nine different options:

- Comma
- Tab
- Fixed
- HTML
- XML
- CommaAndHTML
- TabAndHTML
- XMLAndHTML
- All

The Comma style produces a text file (eplustbl.csv) with the values of the table separated by commas. This is a good format for importing the results into a spreadsheet. If you do open the file with a spreadsheet, make sure you close the file prior to rerunning EnergyPlus otherwise the file will not be updated.

The Tab style produces a text file (eplustbl.tab) with the values separated by tabs. It is also a good format for using with a spreadsheet program but has the advantage of being more readable in a text editor.

The Fixed style produces a text file (eplustbl.txt) with the values at specific columns. It is the easiest to view using a text editor.

The HTML style produces a file (eplustbl.htm) that can be opened with an internet browser program. The values are shown in a tabular format that is easy to view. One advantage of the HTML style is that the results can be viewed in an internet browser program and EnergyPlus can be rerun and the "refresh" button pressed in the internet browser program to see the new results. . When the HTML style is specified, a Table of Contents for the file is generated and placed near the top of the file after the Annual [Building](#building) Utility Performance Summary (aka ABUPS) report. The Table of Contents includes the names of each report and links for each table in the file.

The XML style produces a file (eplustbl.xml) that can be opened by any XML editor and many internet browser programs. The XML output is specifically intended for use by other software programs to make it simple to extract specific results.

The last four options combine the previous styles and allow multiple reports with different styles to be produced during a single simulation.

#### Field: Unit Conversion

This field is optional and if not specified defaults to no unit conversions. Four different input options are available:

- None – no conversions performed
- JtoKWH – Joules converted into kWh (1 / 3,600,000)
- JtoMJ – Joules converted into Megajoules (1 / 1,000,000)
- JtoGJ – Joules converted into Gigajoules (1 / 1,000,000,000)
- InchPound – convert all annual, monthly, economics and timebins tabular values to common Inch-Pound equivalent

The current options are limited to just how the energy is being reported. The default is to report energy in the form of Joules but since the magnitude of those numbers for many buildings is very large, these other conversion factors are available.

The JtoKWH, JtoMJ and JtoGJ unit conversion input option applies only to the Output:Table:Monthly reports and partially to the ABUPS report. For ABUPS, the JtoKWH option changes the report but the JtoMJ and JtoGJ options do not change the report since they it is already in MJ/m2 and GJ.. The InchPound unit conversion input option applies to all annual, monthly, timebins and economic reports that appear in the tabular output file.  In addition, this option does not effect the standard eplusout.eso file. The eplusout.eso file may be converted by using the ConvertESOMTR utility.

An example IDF input object follows.

~~~~~~~~~~~~~~~~~~~~

    OutputControl:Table:Style,
      Comma,                     ! Column Separator
       InchPound;                 ! Unit Conversion
~~~~~~~~~~~~~~~~~~~~

Examples of the tabular reports and descriptions are contained in the Output Details and Examples document.