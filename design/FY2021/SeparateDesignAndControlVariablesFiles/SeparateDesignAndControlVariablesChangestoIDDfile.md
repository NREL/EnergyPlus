
Separate Design and Control Variables - Changes to IDD file
================

The metadata to remain same, so it was taken out to improve readability.

## LowTemperatureRadiant:VariableFlow Objects ##


**ZoneHVAC:LowTemperatureRadiant:VariableFlow:Design,**

    A1 , \field Name
    A2 , \field Fluid to Radiant Surface Heat Transfer Model
    N1 , \field Hydronic Tubing Inside Diameter
    N2 , \field Hydronic Tubing Outside Diameter
    N3 , \field Hydronic Tubing Conductivity
    A3 , \field Temperature Control Type
    A4 , \field Setpoint Control Type
    A5 , \field Heating Design Capacity Method
    N4 , \field Heating Design Capacity Per Floor Area
    N5 , \field Fraction of Autosized Heating Design Capacity
    N6 , \field Heating Control Throttling Range
    A6 , \field Heating Control Temperature Schedule Name
    A7 , \field Cooling Design Capacity Method
    N7 , \field Cooling Design Capacity Per Floor Area
    N8 , \field Fraction of Autosized Cooling Design Capacity
    N9 , \field Cooling Control Throttling Range
    A8 , \field Cooling Control Temperature Schedule Name
    A9 , \field Condensation Control Type
    N10, \field Condensation Control Dewpoint Offset
    A10; \field Changeover Delay Time Period Schedule

**ZoneHVAC:LowTemperatureRadiant:VariableFlow,**

    A1 , \field Name
    A2 , \field Design Object Name
    A3 , \field Availability Schedule Name
    A4 , \field Zone Name
    A5 , \field Surface Name or Radiant Surface Group Name
    N1 , \field Hydronic Tubing Length
    N2 , \field Heating Design Capacity
    N3 , \field Maximum Hot Water Flow
    A6 , \field Heating Water Inlet Node Name
    A7 , \field Heating Water Outlet Node Name
    N4 , \field Cooling Design Capacity
    N5 , \field Maximum Cold Water Flow
    A8 , \field Cooling Water Inlet Node Name
    A9 , \field Cooling Water Outlet Node Name
    A10, \field Number of Circuits
    N6 ; \field Circuit Length

## LowTemperatureRadiant:ConstantFlow Objects ####

**ZoneHVAC:LowTemperatureRadiant:ConstantFlow:Design,**

       A1 , \field Name
       A2 , \field Fluid to Radiant Surface Heat Transfer Model
       A3 , \field Temperature Control Type
       N1 , \field Running Mean Outdoor Dry-Bulb Temperature Weighting Factor
       N2 , \field Hydronic Tubing Inside Diameter
       N3 , \field Hydronic Tubing Outside Diameter
       N4 , \field Hydronic Tubing Conductivity
       N5 , \field Motor Efficiency
       N6 , \field Fraction of Motor Inefficiencies to Fluid Stream
       A4 , \field Condensation Control Type
       N7 , \field Condensation Control Dewpoint Offset
       A5 ; \field Changeover Delay Time Period Schedule

**ZoneHVAC:LowTemperatureRadiant:ConstantFlow,**

       A1 , \field Name
       A2 , \field Design Object Name
       A3 , \field Availability Schedule Name
       A4 , \field Zone Name
       A5 , \field Surface Name or Radiant Surface Group Name
       N1 , \field Hydronic Tubing Length
       N2 , \field Rated Flow Rate
       A6 , \field Pump Flow Rate Schedule Name
       N3 , \field Rated Pump Head
       N4 , \field Rated Power Consumption
       A7 , \field Heating Water Inlet Node Name
       A8 , \field Heating Water Outlet Node Name
       A9 , \field Heating High Water Temperature Schedule Name
       A10, \field Heating Low Water Temperature Schedule Name
       A11, \field Heating High Control Temperature Schedule Name
       A12, \field Heating Low Control Temperature Schedule Name
       A13, \field Cooling Water Inlet Node Name
       A14, \field Cooling Water Outlet Node Name
       A15, \field Cooling High Water Temperature Schedule Name
       A16, \field Cooling Low Water Temperature Schedule Name
       A17, \field Cooling High Control Temperature Schedule Name
       A18, \field Cooling Low Control Temperature Schedule Name
       A19, \field Number of Circuits
       N5 ; \field Circuit Length


## Baseboard:RadiantConvective:Water Objects ####

**ZoneHVAC:Baseboard:RadiantConvective:Water,**

           \memo Design parameters for ZoneHVAC:Baseboard:RadiantConvective objects
      A1 ,  \field Name
      A2 ,  \field Heating Design Capacity Method
      N1 ,  \field Heating Design Capacity Per Floor Area
      N2 ,  \field Fraction of Autosized Heating Design Capacity
      N3 ,  \field Convergence Tolerance
      N4 ,  \field Fraction Radiant
      N5 ;  \field Fraction of Radiant Energy Incident on People

**ZoneHVAC:Baseboard:RadiantConvective:Water,**

          \min-fields 8
      A1, \field Name
      A2, \field Availability Schedule Name
      A3, \field Inlet Node Name
      A4, \field Outlet Node Name
      N1, \field Rated Average Water Temperature
      N2, \field Rated Water Mass Flow Rate
      N3, \field Heating Design Capacity
      N4, \field Maximum Water Flow Rate
      A5, \field Design Object Name
      A6, \field Surface 1 Name
      N5, \field Fraction of Radiant Energy to Surface 1
      A7, \field Surface 2 Name
      N6, \field Fraction of Radiant Energy to Surface 2
    .
    .
    .
     A105, \field Surface 100 Name
     N104; \field Fraction of Radiant Energy to Surface 100

## Baseboard:RadiantConvective:Steam Objects ####

### Proposed

**ZoneHVAC:Baseboard:RadiantConvective:Steam,**

           \memo Design parameters for ZoneHVAC:Baseboard:RadiantConvective objects
      A1 ,  \field Name
      A2 ,  \field Heating Design Capacity Method
      N1 ,  \field Heating Design Capacity Per Floor Area
      N2 ,  \field Fraction of Autosized Heating Design Capacity
      N3 ,  \field Convergence Tolerance
      N4 ,  \field Fraction Radiant
      N5 ;  \field Fraction of Radiant Energy Incident on People

**ZoneHVAC:Baseboard:RadiantConvective:Steam,**

           \min-fields 7
      A1,  \field Name
      A2,  \field Availability Schedule Name
      A3,  \field Inlet Node Name
      A4,  \field Outlet Node Name
      N1,  \field Heating Design Capacity
      N2,  \field Degree of SubCooling
      N3,  \field Maximum Steam Flow Rate
      A5,  \field Design Object Name
      A6,  \field Surface 1 Name
      N4,  \field Fraction of Radiant Energy to Surface 1
      A7,  \field Surface 2 Name
      N5,  \field Fraction of Radiant Energy to Surface 2
    .
    .
    .
     A105, \field Surface 100 Name
     N103; \field Fraction of Radiant Energy to Surface 100
