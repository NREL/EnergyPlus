# Instructions

## Exercise 1A. Run Pre-Defined Building with no Windows

Objective:  Learn to use EP-Launch to run an EnergyPlus input file and view output files.

#. Open EP-Launch.
#. Under "Input File", browse for input file Exercise1A.idf.  This input file contains the 1-zone model described above without the windows and lights. This is located under the install folder <root>\\ExampleFiles\\BasicsFiles,
#. Under "Weather File", select "No Weather File" (at the top of the pull-down list).
#. Press "Simulate".
#. When the simulation is complete, review output files:

- Press "Text Output Files" to see all text output.  Look especially at the eio and err output files.
- Press "Drawing Files" to see a dxf drawing of the building envelope.  (If using Voloview Express, right-click to switch between wireframe and shaded orbit view.  In DWG True View, use "View"  "Visual Styles" to switch between wireframe and solid views. In both programs, use "View" "Named Views" to select isometric views.)
- An empty svg drawing file will also open (this will show HVAC system components in later exercises).  Note that the Adobe SVG viewer is a "plug-in" for Internet Explorer (IE), so IE will open when viewing an SVG file.  Depending on the security settings in IE, you may be prompted with a warning about "active" content.
- Press "Spreadsheets" to open the numeric csv output files.  In Exercise1a.csv, review the pattern of outdoor conditions and loads.  (To make it easier to read the column headings, select Row 1, format cells, and turn on wrap text; then select cell B2 and select "freeze panes".)  In Exercise1aMeter.csv, review the facility district heating and cooling meters.
- Zone/Sys Air Temperature – the zone air temperatures are already being reported.
- Outdoor Dry Bulb – is being reported (so you can compare to outside temperature)
- The meter for the heating in the facility - DistrictHeating:Facility – is being reported. Facility is the entire building.
- The meter for the cooling in the facility - DistrictCooling:Facility – is being reported.

## Exercise 1B. Add Windows

Objective:  Learn how to add materials, constructions, and a surface using 3-D coordinates.

#. In EP-Launch, with input file Exercise1A.idf still selected, press "Edit – IDF Editor".  This will open Exercise1A.idf in the IDF Editor, a tool that assists in editing EnergyPlus input files (idf).
#. In IDF Editor, select File  Save Options . . . and set "Saved Order" to "Original with New at Top", and "Special Format for Some Objects" to "Yes."  Check the "Set as Default" box.
#. In IDF Editor, Select File  Save As . . . and save this file as Exercise1B.idf.
#. Create the construction definition for the windows which are double-pane clear gas with an air space:

- Using File  Open Dataset, open the window glass materials dataset file, WindowGlassMaterials.idf
- Scroll down the Class list and select "**WindowMaterial:Glazing**".Hint:  In IDF Editor, View  Show Classes with Objects Only (or ctl-L) will hide all empty object types from the class list.
- Locate the object which defines the material properties for "CLEAR 6MM".  Select this object (by clicking on the column heading).
- Using Edit  Copy Object (or the toolbar button, or ctl-C), copy this object.
- Switch windows to file Exercise1B.idf and paste the window material into this file.  (Verify that is had been added by going to **WindowMaterial:Glazing** to view the object.)
- Open dataset file WindowGasMaterials.idf.
- Locate "AIR 3MM", copy it and paste it into Exercise1B.idf.
- In Exercise1B.idf, select the "**Construction**" class.  There are three constructions pre-defined for the walls, roof, and floor.
- Press "New Obj" to create a new blank **Construction** object.
- Name this new construction "DOUBLE PANE WINDOW".
- Use the pulldown list to select "CLEAR 6MM" for the outside layer, then press "Enter" or "Return" to save this entry and move to the next field.
- Select "AIR 3MM" for Layer 2, and "CLEAR 6MM" for Layer 3.

#. Add the east window (3m wide by 2m high, centered on wall, *see the drawing in Figure 11 to determine coordinates):*

- Select "**FenestrationSurface:Detailed**" class.
- Add a new object named "EAST WINDOW".
- Set the remaining fields as listed:

- Surface Type = Window
- Construction Name = DOUBLE PANE WINDOW
- Base Surface Name = ZONE SURFACE EAST
- OutsideFaceEnvironment Object = <blank>
- View Factor to Ground = autocalculate
- Name of shading control = <blank>
- WindowFrameAndDivider Name = <blank>
- Multiplier = 1
- Number of Surface Vertex Groups = 4
- Vertex coordinates = *as determined from the drawing Figure 11.  Coordinates in this input are in World Coordinates (all relative to the global origin of 0,0,0).  Coordinates are specified as viewed from the outside of the surface, using the rules specified in the SurfaceGeometry object.*

#. Add the west window, similar to the east window.
#. Add a new **Output:Surfaces:List** object, type= Details.  This report produces a list of all surfaces in the eio output summarizing area, azimuth, tilt, etc.
#. Save and close the IDF file, select Exercise1B.idf in EP-Launch, run the simulation and view outputs.

- Always review the err file for errors and warnings.  Fix problems if needed and re-run.
- Are the windows in the right place in the dxf drawing file. (Use the Drawing File button or select the DXF file from View  Single File or from the Quick-Open panel).
- Review the surface details report in the eio file, search for "Zone/Shading Surfaces" to find this report. (Use the Text Output button, Quick Open "eio" button, or select from the single file menu, or use F7).  This report is easier to read by pasting this section into a spreadsheet and using the text to columns function with comma as a delimiter).  
- Open the csv output file and compare the heating and cooling loads with the results from Exercise1A.csv.

## Exercise 1C. Add Internal Loads

Objective:  Learn how to add schedules, internal loads, and report variables.

#. Save Exercise1B.idf as Exercise1C.idf.
#. Open the dataset file Schedules.idf:

- Copy the **Schedule:Compact** object named "Office Lighting", and paste it into Exercise1C.idf.
- Copy the **ScheduleTypeLimits** object named "Fraction", and paste it into Exercise1C.idf.

#. In Exercise1C.idf, add a LIGHTS object named ZONE ONE Lights, using the Office Lighting schedule, peak input is 1000W.  Consult the EnergyPlus Input Output Reference section on **Lights** for values for the return, radiant, and visible fractions.  Assume the lights are surface mounted fluorescents.
#. Save and close the IDF file, select Exercise1C.idf in EP-Launch, run the simulation and review outputs.
#. Open the rdd file (the report variable data dictionary) and find report variable names related to **Lights**.  Add a new **Output:Variable** object to report the lighting electric consumption.
#. Run the simulation and review outputs.

- Check the err file.
- Find the lighting electric consumption in the csv output file.

#. Compare heating and cooling loads with Exercise1A and Exercise1B.
#. Add more **Output:Variable** objects as desired.

## Exercise 1D. Annual Simulation and Predefined Reports

Objective:  Learn how to run an annual simulation using a weather data file and add table reports.

#. Save Exercise1C.idf as Exercise1D.idf.
#. Edit the **SimulationControl** object to turn off the design day simulations by setting "Run Simulation for Sizing Periods" to **No** and turn on the weather file (annual) simulation by setting "Run Simulation for Weather File Run Periods" to **Yes**..
#. Add a RunPeriod object to run a full annual simulation, let other fields default or remain blank.
#. Add a **Output:Table:SummaryReports** object, and select the following reports:  "Annual Building Performance Summary" (ABUPS), "Input Verification and Results Summary" (IVRS), "Climate Summary", and "Envelope Summary".
#. Add a **OutputControl:Table:Style** object, and select HTML format (ColumnSeparator).
#. Edit existing **Output:Variable** and **Output:Meter** objects and change the reporting frequency from Hourly to Monthly.
#. Save and close the IDF file, select Exercise1D.idf in EP-Launch.
#. Select Chicago TMY2 weather file (or the weather file of your choice) and run the simulation.
#. Review outputs.

- Check the err file.
- Look at the monthly results in the csv output.
- Press the Table output button to view the predefined reports.

## Solution: Exercise 1

*Try not to look at this section until you have completed the Exercise.*

### List of New Objects 

This is a listing of new and modified objects created in this Exercise.

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Glazing,
        CLEAR 6MM,               !- Name
        SpectralAverage,         !- Optical Data Type
        ,                        !- Name of Window Glass Spectral Data Set
        0.006,                   !- Thickness {m}
        0.775,                   !- Solar Transmittance at Normal Incidence
        0.071,                   !- Solar Reflectance at Normal Incidence: Front Side
        0.071,                   !- Solar Reflectance at Normal Incidence: Back Side
        0.881,                   !- Visible Transmittance at Normal Incidence
        0.080,                   !- Visible Reflectance at Normal Incidence: Front Side
        0.080,                   !- Visible Reflectance at Normal Incidence: Back Side
        0.0,                     !- IR Transmittance at Normal Incidence
        0.84,                    !- IR Hemispherical Emissivity: Front Side
        0.84,                    !- IR Hemispherical Emissivity: Back Side
        0.9;                     !- Conductivity {W/m-K}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Gas,
        AIR 3MM,                 !- Name
        Air    ,                 !- Gas Type
        0.0032;                  !- Thickness {m}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Construction,
        DOUBLE PANE WINDOW,      !- Name
        CLEAR 6MM,               !- Outside Layer
        AIR 3MM,                 !- Layer #2
        CLEAR 6MM;               !- Layer #3
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    FenestrationSurface:Detailed,
        EAST WINDOW,             !- User Supplied Surface Name
        WINDOW,                  !- Surface Type
        DOUBLE PANE WINDOW,      !- Construction Name of the Surface
        ZONE SURFACE EAST,       !- Base Surface Name
        ,                        !- OutsideFaceEnvironment Object
        autocalculate,           !- View Factor to Ground
        ,                        !- Name of shading control
        ,                        !- WindowFrameAndDivider Name
        1,                       !- Multiplier
        4,                       !- Number of vertices
        8, 1.5, 2.35,                       !- X,Y,Z  1 {m}
        8, 1.5, 0.35,                       !- X,Y,Z  2 {m}
        8, 4.5, 0.35,                       !- X,Y,Z  3 {m}
        8, 4.5, 2.35;                       !- X,Y,Z  4 {m}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    FenestrationSurface:Detailed,
        WEST WINDOW,             !- User Supplied Surface Name
        WINDOW,                  !- Surface Type
        DOUBLE PANE WINDOW,      !- Construction Name of the Surface
        ZONE SURFACE WEST,       !- Base Surface Name
        ,                        !- OutsideFaceEnvironment Object
        autocalculate,           !- View Factor to Ground
        ,                        !- Name of shading control
        ,                        !- WindowFrameAndDivider Name
        1,                       !- Multiplier
        4,                       !- Number of Vertices
        0, 4.5, 2.35,                       !- X,Y,Z  1 {m}
        0, 4.5, 0.35,                       !- X,Y,Z  2 {m}
        0, 1.5, 0.35,                       !- X,Y,Z  3 {m}
        0, 1.5, 2.35;                       !- X,Y,Z  4 {m}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Output:Surfaces:List,Details;
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Schedule:Compact,
        Office Lighting,         !- Name
        Fraction,                !- ScheduleType
        Through: 12/31,          !- Complex Field #1
        For: Weekdays SummerDesignDay,  !- Complex Field #2
        Until: 05:00, 0.05,      !- Complex Field #4
        Until: 07:00, 0.1,       !- Complex Field #6
        Until: 08:00, 0.3,       !- Complex Field #8
        Until: 17:00, 0.9,       !- Complex Field #10
        Until: 18:00, 0.5,       !- Complex Field #12
        Until: 20:00, 0.3,       !- Complex Field #14
        Until: 22:00, 0.2,       !- Complex Field #16
        Until: 23:00, 0.1,       !- Complex Field #18
        Until: 24:00, 0.05,      !- Complex Field #20
        For: Saturday WinterDesignDay,  !- Complex Field #21
        Until: 06:00, 0.05,      !- Complex Field #23
        Until: 08:00, 0.1,       !- Complex Field #25
        Until: 12:00, 0.3,       !- Complex Field #27
        Until: 17:00, 0.15,      !- Complex Field #29
        Until: 24:00, 0.05,      !- Complex Field #31
        For: Sunday Holidays AllOtherDays,  !- Complex Field #32
        Until: 24:00, 0.05;      !- Complex Field #34
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    ScheduleTypeLimits,
        Fraction,                !- ScheduleType Name
        0.0,                     !- Lower Limit Value
        1.0,                     !- Upper Limit Value
        CONTINUOUS;              !- Numeric Type
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Lights,
      ZONE ONE Lights,         !- Name
      ZONE ONE,                !- Zone Name
      Office Lighting,         !- Schedule Name
      LightingLevel,           !- Design Level Calculation Method
      1000,                    !- Lighting Level {W}
      ,                        !- Watts per Zone Floor Area {W/m2}
      ,                        !- Watts per Person {W/person}
      0,                       !- Return Air Fraction
      0.72,                    !- Fraction Radiant
      0.18,                    !- Fraction Visible
      1,                       !- Fraction Replaceable
      General,                 !- End-Use Subcategory
      No;                      !- Return Air Fraction Calculated from Plenum Temperature
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Output:Variable,*,Lights Electric Consumption ,hourly;
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    RunPeriod,
        1,                       !- Begin Month
        1,                       !- Begin Day Of Month
        12,                      !- End Month
        31,                      !- End Day Of Month
        UseWeatherFile,          !- Day Of Week For Start Day
        Yes,                     !- Use WeatherFile Holidays/Special Days
        Yes,                     !- Use WeatherFile DaylightSavingPeriod
        No,                      !- Apply Weekend Holiday Rule
        Yes,                     !- Use WeatherFile Rain Indicators
        Yes,                     !- Use WeatherFile Snow Indicators
        1;                       !- Number of years of simulation
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Output:Table:SummaryReports,
        Annual Building Utility Performance Summary,  !- ReportName1
        Input Verification and Results Summary,  !- ReportName2
        Climate Summary,         !- ReportName3
        Envelope Summary;        !- ReportName4
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    OutputControl:Table,
        HTML;                    !- ColumnSeparator
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    SimulationControl,
        No,                      !- Do the zone sizing calculation
        No,                      !- Do the system sizing calculation
        No,                      !- Do the plant sizing calculation
        No,                      !- Do the design day simulations
        Yes;                     !- Do the weather file simulation
~~~~~~~~~~~~~~~~~~~~