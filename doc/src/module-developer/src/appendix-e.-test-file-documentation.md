# Appendix E.  Test File Documentation

Each test file, whether released to the public or not, should be a best practice model and documented (comments at the top of the file) following the guidelines below.  The document template file is also included with each installation in the "ExampleFiles" folder – ExampleFilesDoc.txt

~~~~~~~~~~~~~~~~~~~~

    ! <name of file>
    ! Basic file description: <specify number of zones, stories in building, etc>
    ! Highlights: <Purpose of this example file>
    ! Simulation Location/Run: <location information, design days, run periods>
    ! Location:
    ! Design Days (should have SummerDesignDay,WinterDesignDay designations):
    ! Run Period (Weather File):
    ! Run Control (should include this):
    !
    ! Building: <more details about building.  metric units, if also english enclose in []{} or ()>
    ! Floor Area:
    ! Number of Stories:
    !
    ! Zone Description Details:
    ! Internal gains description: <lighting level, equipment, number of occupants, infiltration, daylighting, etc>
    ! Interzone Surfaces:
    ! Internal Mass:
    ! People:
    ! Lights:
    ! Windows:
    ! Detached Shading:
    ! Daylight:
    ! Natural Ventilation          :
    ! Compact Schedules (preferred):
    ! Solar Distribution:

    !
    ! HVAC: <HVAC description and plant supply, as appropriate>
    ! Purchased Air:
    ! Zonal Equipment:
    ! Central Air Handling Equipment:
    ! System Equipment Autosize:
    ! Purchased Cooling:
    ! Purchased Heating:
    ! Coils:
    ! Pumps:
    ! Boilers:
    ! Chillers:
    ! Towers:
    !
    ! Results: <how are results reported>
    ! Standard Reports:
    ! Timestep or Hourly Variables:
    ! Time bins Report:
    ! HTML Report:
    ! Environmental Emissions:
    ! Utility Tariffs:
~~~~~~~~~~~~~~~~~~~~

Most of the example files have completed their documentation requirements and include plan views of the building.  Our naming convention uses an underscore (_) as the first character of an input file "not for publication".