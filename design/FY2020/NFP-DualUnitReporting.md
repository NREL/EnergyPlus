UPDATE UNIT AND FILE CONTROL FOR TABULAR REPORTS
================

**Jason Glazer, GARD Analytics**

 - May 8, 2020
 

## Justification for New Feature ##

Users and user interfaces for EnergyPlus often need the tabular (HTML) reporting in both SI and IP units 
for the same simulation. For example, post processing may depend on the tabular reports being in SI units
while the user would like to see the results in IP units. The mixture of both units in output has been 
requested by user interface developers. 

See:

https://github.com/NREL/EnergyPlus/issues/5435


## E-mail and  Conference Call Conclusions ##

insert text

## Overview ##

Add the capability in EnergyPlus to provide the tabular reports in both SI and IP units while maintaining 
the SQL output file in SI units. Multiple approaches could be taken to accomplish this by providing 
additional inputs to control the units or by automatically producing outputs in various units. The 
expectation is that the SI tabular report would be in one HTML file and a second HTML file would contain the IP 
tabular reports. The SQL output file is only needed in SI but currently can be generated in either SI or IP.

## Approach ##

The current OutputControl:Table:Style, Output:JSON, and Output:SQLite objects all control aspects of the 
output for tabular and SQL outputs. The current form of these objects from the IDD is shown below: 

```

OutputControl:Table:Style,
       \memo default style for the OutputControl:Table:Style is comma -- this works well for
       \memo importing into spreadsheet programs such as Excel(tm) but not so well for word
       \memo processing programs -- there tab may be a better choice.  fixed puts spaces between
       \memo the "columns".  HTML produces tables in HTML. XML produces an XML file.
       \memo note - if no OutputControl:Table:Style is included, the defaults are comma and None.
       \unique-object
   A1, \field Column Separator
       \type choice
       \key Comma
       \key Tab
       \key Fixed
       \key HTML
       \key XML
       \key CommaAndHTML
       \key CommaAndXML
       \key TabAndHTML
       \key XMLandHTML
       \key All
       \default Comma
   A2; \field Unit Conversion
       \type choice
       \key None
       \key JtoKWH
       \key JtoMJ
       \key JtoGJ
       \key InchPound
       \default None
       
Output:JSON,
       \memo Output from EnergyPlus can be written to JSON format files.
       \unique-object
  A1 , \field Option Type
       \required-field
       \type choice
       \key TimeSeries
       \key TimeSeriesAndTabular
  A2 , \field Output JSON
       \type choice
       \key Yes
       \key No
       \default Yes
  A3 , \field Output CBOR
       \type choice
       \key Yes
       \key No
       \default No
  A4 ; \field Output MessagePack
       \type choice
       \key Yes
       \key No
       \default No


Output:SQLite,
       \memo Output from EnergyPlus can be written to an SQLite format file.
       \unique-object
  A1 ; \field Option Type
       \type choice
       \key Simple
       \key SimpleAndTabular
       
```

The existing choices for tabular reports already allowed for multiple output formats but the units for each of those formats
were all the same. 

The revised input objects are shown below with the tabular report control, centralized in one object and made 
extensible. As many files could be generated as wanted and each using any of the four different unit conversion formats 
desired: 

```

OutputControl:Table:Style,
 A1, \field Format for file-01.ext
       \type choice
       \key Comma
       \key Tab
       \key Fixed
       \key HTML
       \key XML
       \key SQL
       \key JSON
       \key CBOR
       \key MessagePack
       \default HTML
   A2, \field Unit Conversion for file-01.ext
       \type choice
       \key None
       \key JtoKWH
       \key JtoMJ
       \key JtoGJ
       \key InchPound
       \default None
   A3, \field Format for file-02.ext
       \type choice
       \key Comma
       \key Tab
       \key Fixed
       \key HTML
       \key XML
       \key SQL
       \key JSON
       \key CBOR
       \key MessagePack
       \default HTML
   A4, \field Unit Conversion for file-02.ext
       \type choice
       \key None
       \key JtoKWH
       \key JtoMJ
       \key JtoGJ
       \key InchPound
       \default None
   A5, \field Format for file-03.ext
       \type choice
       \key Comma
       \key Tab
       \key Fixed
       \key HTML
       \key XML
       \key SQL
       \key JSON
       \key CBOR
       \key MessagePack
       \default HTML
   A6, \field Unit Conversion for file-03.ext
       \type choice
       \key None
       \key JtoKWH
       \key JtoMJ
       \key JtoGJ
       \key InchPound
       \default None
    . . . (as many copies of these fields as desired)
       
OutputControl:Timeseries:Style,
       \memo Timeseries output from EnergyPlus can be written to SQLite and JSON format files.
       \unique-object
  A1 , \field Output SQL
       \type choice
       \key Yes
       \key No
       \default No
  A2 , \field Output JSON
       \type choice
       \key Yes
       \key No
       \default No
  A3 , \field Output CBOR
       \type choice
       \key Yes
       \key No
       \default No
  A4 ; \field Output MessagePack
       \type choice
       \key Yes
       \key No
       \default No

```

By providing file format and unit conversion choices for each output it provides as much flexiblity as needed. This approach 
would allows additional formats and unit conversion options to be added in the future.

The control for JSON and SQLite timeseries data has also been combined and changed to make it more consistent with the input 
object name controlling the format of the tabular outputs. If at some point explicit ESO and CSV options are implemented for 
timeseries data, they could easily be added to this object.

The output files related to the tabular output currently are named:

	{file}Table.{ext} or {file}.{ext}

depending on the type of file. The exact format varies and depends on the 
--output-suffix command line option. The new approach will also use the same format 
but will extend it by including in the file name the specific instance for the file:

	{file}Table-01.{ext} or {file}-01.{ext}
	{file}Table-02.{ext} or {file}-02.{ext}
	{file}Table-03.{ext} or {file}-03.{ext}

We may choose fo the -01 to be dropped specifically to help with current compatibilty.

When timeseries data is also requested, they would appear in each instance of the file although 
currently no unit conversion is expected for timeseries data.

## Testing/Validation/Data Sources ##

insert text

## Input Output Reference Documentation ##

insert text

## Input Description ##

insert text

## Outputs Description ##

insert text

## Engineering Reference ##

insert text

## Example File and Transition Changes ##

insert text

## References ##

insert text



