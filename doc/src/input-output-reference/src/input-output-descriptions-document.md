# Input – Output Descriptions (Document)

## Input Descriptions

In the descriptions below, the fields for each input object will be described but the Energy+.idd descriptions will not be shown. Refer to the actual Energy+.idd file for complete specifications. Energy+.idd is a text file that can be viewed with many text editors or word processors. The [Site:Location](#sitelocation) object will serve as an example.

~~~~~~~~~~~~~~~~~~~~

    Site:Location,
           \unique-object
           \min-fields 5
      A1 , \field Name
           \required-field
           \type  alpha
      N1 , \field Latitude
           \units deg
           \minimum -90.0
           \maximum +90.0
           \default 0.0
           \note + is North, - is South, degree minutes represented in decimal (i.e. 30 minutes is .5)
           \type real
      N2 , \field Longitude
           \units deg
           \minimum -180.0
           \maximum +180.0
           \default 0.0
           \note - is West, + is East, degree minutes represented in decimal (i.e. 30 minutes is .5)
           \type real
      N3 , \field Time Zone
           \note basic these limits on the WorldTimeZone Map (2003)
           \units hr
           \minimum -12.0
           \maximum +14.0
           \default 0.0
           \note  Time relative to GMT. Decimal hours.
           \type real
      N4 ; \field Elevation
           \units m
           \minimum -300.0
           \maximum< 8900.0
           \default 0.0
           \type real
~~~~~~~~~~~~~~~~~~~~

The IDD excerpt above is the complete definition as seen in the IDD file.

First, the object name is given. ([Site:Location](#sitelocation))  This is followed by a comma in both the definition (IDD) and in an input file (IDF). In fact, all fields except the terminating field of an IDD class object and IDF object are followed by commas. The final field in an IDD class object or in an IDF object is terminated by a semi-colon.

Next is an alpha field, the location name. As noted above, for input, spaces are significant in this field. The main inputs for [Site:Location](#sitelocation) are numeric fields. These are numbered (as is the alpha field) for convenience. The \\ designations will show various information about the objects as described above in the IDD conventions discussion. Of importance for reading this document are the units and possible minimum and maximum values for a  field.

There is automatic processing of the \\minimum, \\maximum and \\default data for numeric fields. Any infractions of the \\minimum, \\maximum fields are automatically detected and messages will appear in the standard error file. After all the input is checked, infractions will cause program termination (before the bulk of the simulation is completed). Defaults are also enforced if you leave the numeric field blank.

Some objects need all the parameters listed by the definition; some do not. In the descriptions that follow, we will try to indicate which parts are optional. Usually, these will be the last fields in the object input or definition. Even if items are not used for a particular object (e.g. Multiplier in the [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) and type=Door), the field must be included unless it is the *last field in the object*. So, for this instance, one must include a multiplier field (must be numeric and would need to obey any \\minimum, \\maximum rules) for doors.

Two spreadsheet files are included with the installation:

- ExampleFiles.xls – shows many details about the included example files including highlights of features.
- ExampleFiles-ObjectsLink.xls – shows, for each object, the first three occurrences of that object in an example file.

## Output Descriptions

In the descriptions below, we will endeavor to have each object's output displayed as well as each of the outputs described. The output variables for a run are selected by choosing the Output:VariableDictionary object. This object displays the available output variables for a run on the **eplusout.rdd** (regular variables) and **eplusout.mdd** (meter variables) files. Two significant styles are available for these displays and the descriptions below may have one for some objects and another for other objects. The variables are the same but will look a bit different.  For clarity, the two displays are shown below:

Note that the IDF-Editor can interpret both sets of files and assist you in getting output variables into your input files. But you will have to successfully run your input file first.

The Simple (or regular) display looks like the following figure and is interpreted:

**[Zone](#zone)/HVAC** – when the output is produced at the "[Zone](#zone)" timestep (ref: number of timesteps in each hour) or at the "HVAC" aka System timestep (which can vary for each hour).

**Average/Sum** – whether this is a averaged value over the reporting period (such as a temperature or rate) or whether this is a summed value over the reporting period. Reporting periods are specified in the [Output:Variable](#outputvariable) or [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects.

**<Variable Name>** -- The variable name one uses for reporting is displayed (e.g., Site Outdoor Drybulb Temperature) along with the units (e.g., [C]).

Example from the **eplusout.rdd** file:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Site Outdoor Air Drybulb Temperature [C]
    Zone,Average,Site Outdoor Air Dewpoint Temperature [C]
    Zone,Average,Site Outdoor Air Wetbulb Temperature [C]
    Zone,Average,Site Outdoor Air Humidity Ratio [kgWater/kgAir]
    Zone,Average,Site Outdoor Air Relative Humidity [%]
    Zone,Average,Site Outdoor Air Barometric Pressure [Pa]
    Zone,Average,Wind Speed [m/s]
    Zone,Average,Site Wind Direction [deg]
    Zone,Average,Site Sky Temperature [C]
    HVAC,Sum,Zone Air System Sensible Heating Energy [J]
    HVAC,Sum,Zone Air System Sensible Cooling Energy [J]
    HVAC,Average,Zone Air System Sensible Heating Rate [W]
    HVAC,Average,Zone Air System Sensible Cooling Rate [W]
    HVAC,Average,Zone Air Temperature [C]
    HVAC,Average,Zone Thermostat Air Temperature [C]
~~~~~~~~~~~~~~~~~~~~

Note that the **eplusout.mdd** file is similar, but meters are only available at the [Zone](#zone) timestep.

~~~~~~~~~~~~~~~~~~~~

    Zone,Meter,Electricity:Facility [J]
    Zone,Meter,ExteriorLights:Electricity [J]
    Zone,Meter,Grounds Lights:ExteriorLights:Electricity [J]
    Zone,Meter,EnergyTransfer:Facility [J]
    Zone,Meter,EnergyTransfer:Building [J]
    Zone,Meter,EnergyTransfer:Zone:R13WALL WALLS [J]
~~~~~~~~~~~~~~~~~~~~

The IDF display has all the same information in an IDF-ready form (i.e., you could copy and paste it into your input file using a text editor).

Example from the **eplusout.rdd** file:

~~~~~~~~~~~~~~~~~~~~

    Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly; !- Zone Average [C]
    Output:Variable,*,Site Outdoor Air Dewpoint Temperature,hourly; !- Zone Average [C]
    Output:Variable,*,Site Outdoor Air Wetbulb Temperature,hourly; !- Zone Average [C]
    Output:Variable,*,Site Outdoor Air Humidity Ratio,hourly; !- Zone Average [kgWater/kgAir]
    Output:Variable,*,Site Outdoor Air Relative Humidity,hourly; !- Zone Average [%]
    Output:Variable,*,Site Outdoor Air Barometric Pressure,hourly; !- Zone Average [Pa]
    Output:Variable,*,Zone Air System Sensible Heating Energy,hourly; !- HVAC Sum [J]
    Output:Variable,*,Zone Air System Sensible Cooling Energy,hourly; !- HVAC Sum [J]
    Output:Variable,*,Zone Air System Sensible Heating Rate,hourly; !- HVAC Average [W]
    Output:Variable,*,Zone Air System Sensible Cooling Rate,hourly; !- HVAC Average [W]
    Output:Variable,*,Zone Air Temperature,hourly; !- HVAC Average [C]
    Output:Variable,*,Zone Thermostat Air Temperature,hourly; !- HVAC Average [C]
~~~~~~~~~~~~~~~~~~~~

All of the same information appears in a slightly different form and defaults to "hourly" reporting frequency (which, of course, can be changed when you put it into your input file). The "\*" is preselected so that you would be reporting for all those items.