# Fluid Property Services

Fluid property routines have been implemented within EnergyPlus with the goal of making the specification of new fluids relatively easy for the user and not require the user to specify data for the most common loop fluids.  Common refrigerants are listed within an extensive Reference Data Set (RDS) that is provided with the EnergyPlus program.

Fluids in EnergyPlus are broken into two categories: **refrigerants** and **glycols**.  This relates back to the amount of information needed to determine the properties of the various fluid types inside the program.  The decision to define or use one class of fluids or another relates back to whether or not one expects the fluid to change phase (liquid and/or vapor) or remain a liquid.  When a developer feels that a fluid may change phase, all code should access the Refrigerant class of fluid property routines.  When the developer is certain that the fluid will remain a liquid and wishes to abide by that assumption (generally, this is the case for most larger loops), all code from such modules should access the Glycol class of fluid property routines.  Each of these classes will be described in separate sections below since each class has different subroutine access to the main module.

Internally, both the refrigerant and glycol classes of fluids use "table lookup" and interpolation to find the appropriate value of a fluid property.  No curve fits are done internally and the interpolation search routines are currently not optimized (no interval halving or special fast searching techniques are used to find the values).

HOWEVER, if values out of range (too low or too high) are passed to the routines, the value returned is a valid value at the lowest or highest (respectively) input parameter (that was passed in out of range).

You will also note in the succeeding descriptions that IP units can be entered by some editors (IDF Editor) using predefined unit conversions.

## Using Fluid Property Routines in EnergyPlus Modules

The routines are contained within a single module: **FluidProperties.f90**

Developers can use the routines anywhere inside EnergyPlus through the following USE statement:

**USE FluidProperties**

Access to this module may be limited by expanding this line of code with the ONLY designator.

## Fluid Properties Functions for Refrigerant Class Fluids

In EnergyPlus, a refrigerant fluid is capable of being either in the liquid or vapor phase.  Due to this definition, data must be available for both of these regions in order for the program to accurately calculate the various fluid properties.  There are eight possible functions that may be used to obtain refrigerant data using the Fluid Properties module.  They include:

~~~~~~~~~~~~~~~~~~~~

    GetSatPressureRefrig(Refrigerant,Temperature,RefrigIndex,calledfrom)
    GetSatTemperatureRefrig (Refrigerant,Pressure,RefrigIndex,calledfrom)
    GetSatEnthalpyRefrig (Refrigerant,Temperature,Quality,RefrigIndex,calledfrom)
    GetSatDensityRefrig (Refrigerant,Temperature,Quality,RefrigIndex,calledfrom)
    GetSatSpecificHeatRefrig (Refrigerant,Temperature,Quality,RefrigIndex,calledfrom)
    GetSupHeatEnthalpyRefrig (Refrigerant,Temperature,Pressure,RefrigIndex,calledfrom)
    GetSupHeatPressureRefrig (Refrigerant,Temperature,Enthalpy,RefrigIndex,calledfrom)
    GetSupHeatDensityRefrig (Refrigerant,Temperature,Pressure,RefrigIndex,calledfrom)
~~~~~~~~~~~~~~~~~~~~

While most of the variables passed into the routine are self-explanatory, the three variables that are common to each of these functions are Refrigerant, RefrigIndex and calledfrom.  **Refrigerant** in this case is the character string name of the refrigerant in question as listed in the input file using the FluidNames object.  This must be passed into the function to identify the fluid being referenced.  **RefrigIndex** is an internal variable.  On the first call to the fluid property routine, it is zero.  All of the fluid property routines are set-up to find a non-zero index in the local fluid property data structure that corresponds to this refrigerant name.  Since finding the proper fluid from the fluid name each and every time is computationally slow, the index allows the code to quickly find the right data without doing an inordinate number of string comparisons.  Thus, module developers should store the RefrigIndex in their own local data structure in addition to the refrigerant name. calledfrom is a string variable passed to the routine so that error messages coming from the above functions can give a better context for errors when they happen.

Units for these other variables in these function calls are: Joules per kilogram for enthalpy, degrees Celsius for temperature, Pascals for pressure, kilograms per cubic meter for density, and Joules per kilogram-degree Celsius for specific heat.  Quality and concentration are dimensionless fractions.  All variables are considered input variables.

Module developers should use the functions listed above to first determine whether they are in the saturated region or the superheated region.  The GetSatPressureRefrig and GetSatTemperatureRefrig functions should assist the users in determining whether they are in or beyond the saturated region.  Once this is determined, the developer can call the appropriate function to obtain the quantity of interest: in the saturated region this includes the enthalpy, density, or specific heat; in the superheated region this includes the enthalpy, pressure, or density.

## Reference Data Set (RDS) Values for Refrigerant Class Fluids

The data for refrigerants that are included in the reference data set that comes with EnergyPlus are as follows (temperatures in Celsius, pressure in MegaPascals):

Table: Regions for Fluid Properties

Refrigerant|Sat. Temp range {C}|Super Temp range\* {C}|Super Pressure range\* {Pa}
-----------|-------------------|----------------------|---------------------------
R11|-110 to 198|-110 to 255|6.8 to 1.6E6
R11(specheat)|-110 to 190||
R12|-157 to 112|-156 to 169|.3 to 1.6E7
R12(specheat)|-157 to 104||
R22|-157 to 96|-157 to 153|0.4 to 1.7E7
R22(specheat)|-157 to 88||
R123|-107 to 184|-106 to 240|4.9 to 1.5E7
R134a|-103 to 101|-103 to 158|400 to 1.6E7
R404a|-72 to 72|-72 to 72|2.3E4 to 3.7E6
R410a|-72 to 69|-72 to 69|3.1E4 to 4.7E6
R507a|-72 to 69|-72 to 69|2.5E4 to 3.6E6
NH3|-77 to 132|-77 to 189|6.3E3 to 2.2E7
NH3(specheat)|-73 to 124||
Steam|0 to 370|0 to 500|610 to 4.0E8
Steam(specheat)|0 to 370||

\*Obviously data for all temperatures at all pressures isn't loaded.  The entire range of pressures given above will work, but the temperature range for a given pressure will be some subset of the Super Temp range shown above.

Subcooled region actually only returns h(f) or the saturated liquid value at the temperature you input.

## Fluid Property Data and Expanding the Refrigerants Available to EnergyPlus

The Fluid Property routines have been reengineered to allow other users to add refrigerants to the input file without having to make any changes to the program code.  The only requirement on input is that in order to add a new refrigerant, a user must enter a full set of data.  The exact definition of a full set of data is given below.

As with all EnergyPlus input, the fluid properties data has both an input data description and a reference data set that must show up in the input file.  All of the "standard" refrigerants listed above must show up in the in.idf file for it to be available to the rest of the simulation.  Below is the description of the input data description syntax for the fluid properties entries.

The first syntax item lists all of the fluids present in an input file and categorizes them as either a refrigerant (such as R11, R12, etc.) or a glycol (such as ethylene glycol, propylene glycol, etc.).  A refrigerant or glycol must be in this list in order to be used as a valid fluid in other loops in the input file.

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Names,
           \unique-object
           \extensible:2 repeat last two fields
           \memo list of potential fluid names/types in the input file, maximum of ten
      A1,  \field Fluid 1 Name
           \type alpha
      A2,  \field Fluid 1 Type
           \type choice
           \key Refrigerant
           \key Glycol
      A3,  \field Fluid 2 Name
           \type alpha
      A4,  \field Fluid 2 Type
           \type choice
           \key Refrigerant
           \key Glycol
      A5,  \field Fluid 3 Name
           \type alpha
      A6,  \field Fluid 3 Type
           \type choice
           \key Refrigerant
           \key Glycol
~~~~~~~~~~~~~~~~~~~~

An example of this statement in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Names,
      R11, REFRIGERANT,
      R12, REFRIGERANT,
      R22, REFRIGERANT,
      NH3, REFRIGERANT,
      Steam, REFRIGERANT,
      NewGlycol, GLYCOL,
      SuperGlycol, GLYCOL;
~~~~~~~~~~~~~~~~~~~~

All fluid properties vary with temperature.  As a result, the following syntax allows the user to list the temperatures at which the data points are valid.  Since in many cases, the temperatures will be similar, this provides a more compact input structure and avoids listing the temperatures multiple times.  The name associated with the temperature list is the piece of information that will allow the actual fluid property data statements to refer back to or link to the temperatures.  Up to 250 points may be entered with this syntax and temperatures must be entered in ascending order.  Units for the temperatures are degrees Celsius.  The same temperature list may be used by more than one refrigerant.

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Temperatures,
          \memo property values for fluid properties
          \memo list of up to 250 temperatures, note that number of property values must match the number of properties
          \memo in other words, there must be a one-to-one correspondence between the property values in this list and
          \memo the actual properties list in other syntax
          \memo degrees C (for all temperature inputs)
          \format FluidProperty
      A1, \field Name
          \type alpha
      N1, \field Temperature 1
          \type real
          \units C
    < same thing repeated over and over again>
      N250; \field Temperature 250
            \type real
~~~~~~~~~~~~~~~~~~~~

An example of this statement in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Temperatures,
          R11Temperatures,
          -70,-65,-60,-55,-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,2,4,6,8,10,12,14,16,18,
          20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,55,60,65,70,75,80,85,90,95,100,
          105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,198;
~~~~~~~~~~~~~~~~~~~~

Property data for the saturated region is entered with the following syntax.  Before the actual data is entered, this line of input must identify the refrigerant the data is to be associated with, what the data represents (choice of one of three keywords), the phase of the data (either fluid or gas), and the temperature list reference that links each data point with a temperature.

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Saturated,
          \memo fluid properties for the saturated region
          \format FluidProperty
      A1, \field Name
          \type object-list
          \object-list FluidNames
      A2, \field Fluid Property Type
          \note Enthalpy Units are J/kg
          \note Density Units are kg/m3
          \note SpecificHeat Units are J/kg-K
          \note Pressure Units are Pa
          \type choice
          \key Enthalpy     ! Units are J/kg
          \key Density      ! Units are kg/m3
          \key SpecificHeat ! Units are J/kg-K
          \key Pressure     ! Units are Pa
      A3, \field Fluid Phase
          \note Fluid=saturated fluid
          \note FluidGas=saturated vapor
          \type choice
          \key Fluid        ! saturated fluid
          \key FluidGas     ! saturated vapor
      A4, \field Temperature Values Name
          \note Enter the name of a FluidProperties:Temperatures object.
          \type object-list
          \object-list FluidPropertyTemperatures
      N1, \field Property Value 1
          \type real
          \unitsBasedOnField A2
      N2, \field Property Value 2
          \type real
          \unitsBasedOnField A2
    < same thing repeated over and over again>
      N250; \field Property Value 250
            \type real
            \unitsBasedOnField A2
~~~~~~~~~~~~~~~~~~~~

An example of this statement in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Saturated,
          R11,ENTHALPY,FLUID,R11Temperatures, ! Enthalpy in J/kg
          153580,154600,156310,158580,161300,164380,167740,171330,175100,179020,183060,
          187190,191400,195680,200000,201740,203490,205240,207000,208770,210530,212310,
          214080,215870,217650,219860,221230,223030,224830,226630,228860,230250,232060,
          233860,235700,237520,239350,241180,243010,246350,249450,254080,258730,263480,
          268110,272860,277000,282410,287240,292120,297030,302000,307090,312080,317210,
          322400,327670,333020,338460,344010,349680,355500,361480,367690,374100,381060,
          388850,397280,426300;
~~~~~~~~~~~~~~~~~~~~

The format of the data for the superheated region is almost identical to that of the saturated region with one addition -- a pressure.  The pressure is listed before the rest of the data and has units of Pa.

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Superheated,
          \memo fluid properties for the superheated region
          \format FluidProperty
      A1, \field Fluid Name
          \type object-list
          \object-list FluidNames
      A2, \field Fluid Property Type
          \note Enthalpy Units are J/kg
          \note Density Units are kg/m3
          \type choice
          \key Enthalpy     ! Units are J/kg
          \key Density      ! Units are kg/m3
      A3, \field Temperature Values Name
          \note Enter the name of a FluidProperties:Temperatures object.
          \type object-list
          \object-list FluidPropertyTemperatures
      N1, \field Pressure
          \note pressure for this list of properties
          \type real
          \units Pa
          \minimum> 0.0
      N2, \field Property Value 1
          \type real
          \unitsBasedOnField A2
      N3, \field Property Value 2
          \type real
          \unitsBasedOnField A2
    < same thing repeated over and over again>
      N250; \field Property Value 250
            \type real
           \unitsBasedOnField A2
~~~~~~~~~~~~~~~~~~~~

An example of this statement in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Superheated,
        R11,DENSITY,SuperR11Temperatures, ! Density in kg/m^3
        62000., !Pressure = 62000Pa
        0,0,0,0,0,0,0,0.0139,0.0134,0.0129,0.0124,0.012,0.0116,0.0112,0.0109,0.0105,
        0.0102,0.0099,0.0097,0.0094,0.0092,0.0089,0,0,0,0,0,0,0,0,0,0;
~~~~~~~~~~~~~~~~~~~~

## Fluid Properties Functions for Glycol Class Fluids

In EnergyPlus, a glycol fluid is assumed to remain in the liquid phase.  As a result, data is only required for fluids in the liquid state.  There are four possible functions that may be used to obtain glycol data using the Fluid Properties module.  These correspond to the fluid property of interest and include:

~~~~~~~~~~~~~~~~~~~~

    GetSpecificHeatGlycol (Glycol,Temperature,GlycolIndex,calledfrom)
    GetConductivityGlycol (Glycol,Temperature,GlycolIndex,calledfrom)
    GetDensityGlycol (Glycol,Temperature,GlycolIndex,calledfrom)
    GetViscosityGlycol (Glycol,Temperature,GlycolIndex,calledfrom)
~~~~~~~~~~~~~~~~~~~~

All of these functions are used in exactly the same way.  The module developer should send the glycol name (as listed in the GlycolConcentrations object in the input file) to the routine and the GlycolIndex (sent as 0 the first time and then set by the fluid property routine; see RefrigIndex discussion above).  The calledfrom parameter is also used as discussed in the Refrigerant parameter discussion above. In addition, the functions require the temperature of the glycol in degrees Celsius.

## Default Values for Glycol Class Fluids

There are default values for specific heat, density, conductivity, and viscosity for Water, Ethylene Glycol, and Propylene Glycol.  This means that if users accept the values as published in the ASHRAE Handbook of Fundamentals, then the only information the user must include in their input file is a description of the concentration of glycol used (via the GlycolConcentrations object).  If water is used in a loop, the user does not need to enter anything other than WATER as the fluid type in the appropriate input syntax.  Data for various concentrations of these three default fluids encompasses the range over with these fluids and their combinations are in the liquid phase (-35 to 125 degrees Celsius).  When the glycol combination in question is indeed a fluid, the functions will return the appropriate value for the parameter in question.  If the glycol is either a solid or vapor, the routine will return a zero value.  Units for the different properties are: Joules per kilogram-Kelvin for specific heat, Pascal-seconds for viscosity, Watts per meter-Kelvin for conductivity, and kilograms per cubic meter for density.  In contrast to the refrigerant data which is included in the RDS and must be copied into the user input file if it is to be used, the glycol default data has been hardwired into EnergyPlus and does not need to be entered into the input file.

## Fluid Property Data and Expanding the Glycols Available to EnergyPlus

The format of the data for the glycols is almost identical to that of the superheated region for refrigerants with one exception -- concentration replaces pressure.  The concentration is listed before the rest of the data and is dimensionless.

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Concentration,
          \memo fluid properties for water/other fluid mixtures
          \format FluidProperty
      A1, \field Fluid Name
          \type object-list
          \object-list FluidNames
          \note should not be any of the defaults (Water, EthyleneGlycol, or PropyleneGlycol)
      A2, \field Fluid Property Type
          \note Density Units are kg/m3
          \note SpecificHeat Units are J/kg-K
          \note Conductivity Units are W/m-K
          \note Viscosity Units are N-s/m2
          \type choice
          \key Density      ! Units are kg/m3
          \key SpecificHeat ! Units are J/kg-K
          \key Conductivity ! Units are W/m-K
          \key Viscosity    ! Units are N-s/m2
      A3, \field Temperature Values Name
          \note Enter the name of a FluidProperties:Temperatures object.
          \type object-list
          \object-list FluidPropertyTemperatures
      N1, \field Concentration
          \note Glycol concentration for this list of properties entered as a fraction
          \type real
          \units dimensionless
          \minimum 0.0
          \maximum 1.0
      N2, \field Property Value 1
          \type real
          \unitsBasedOnField A2
      N3, \field Property Value 2
          \type real
          \unitsBasedOnField A2
    < same thing repeated over and over again>
      N250; \field Property Value 250
            \type real
           \unitsBasedOnField A2
~~~~~~~~~~~~~~~~~~~~

An example of this statement in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Concentration,
         MyPropyleneGlycol,SPECIFICHEAT ,GlycolTemperatures, ! Specific heat in J/kg-K
         0.8, ! Concentration
         2572,2600,2627,2655,2683,2710,2738,2766,2793,2821,2849,2876,2904,2931,2959,
         2987,3014,3042,3070,3097,3125,3153,3180,3208,3236,3263,3291,3319,3346,3374,
         3402,3429,3457;
~~~~~~~~~~~~~~~~~~~~

The above input syntax is used to define data for a particular new fluid beyond the default glycol fluids.  It would be repeated at other appropriate concentration values, if necessary, to define the fluid.  It should be noted that in order to enter a fluid, the user must specify all four of the properties: conductivity, specific heat, viscosity, and density.

In addition to specifying the raw data for a new glycol, the user must list the fluid in the FluidNames object and then specify the concentration in the GlycolConcentrations object as shown below:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Names,
      MyPropyleneGlycol, GLYCOL;
    GlycolConcentrations,
      MyPropyleneGlycol, GLYCOL;
~~~~~~~~~~~~~~~~~~~~

The IDD description for the FluidProperties:GlycolConcentrations object is given below:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:GlycolConcentrations,
           \unique-object
           \memo list of glycols and what concentration they are, maximum of ten
      A1,  \field Fluid 1 Name
           \type alpha
           \reference GlycolConcentrations
      A2,  \field Glycol 1 Name
           \type choice
           \key EthyleneGlycol
           \key PropyleneGlycol
           \memo or UserDefined Fluid (must show up as a glycol in FluidProperties:Names list)
      N1,  \field Glycol 1 Concentration
           \type real
           \minimum 0.0
           \maximum 1.0
      A3,  \field Fluid 2 Name
           \type alpha
           \reference GlycolConcentrations
    < . . . repeated up to 10 times . . .>
      A19, \field Fluid 10 Name
           \type alpha
      A20, \field Glycol 10 Name
           \type choice
           \key EthyleneGlycol
           \key PropyleneGlycol
           \memo or UserDefined Fluid (must show up as a glycol in FluidProperties:Names list)
      N10; \field Glycol 10 Concentration
           \type real
           \minimum 0.0
           \maximum 1.0
~~~~~~~~~~~~~~~~~~~~

An example of how this would be used in an actual IDF is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:GlycolConcentrations,
      MyProGly80Percent,  !- fluid name 1
      MyPropyleneGlycol,  !- glycol name 1
      0.8,
      EthGly30Percent,  !- fluid name 2
      EthyleneGlycol,  !- glycol name 2
      0.3;  !- concentration 2
~~~~~~~~~~~~~~~~~~~~

The key relationship in this syntax is how FluidNames relates to GlycolConcentrations and how to have modules access through the proper name.  FluidNames are used to define raw data, whether for refrigerants or glycols.  With a glycol, it is not enough to define raw data since this does not necessarily define the actual concentration of glycol being used.  Thus, the GlycolConcentrations object is needed.  It defines a name for the actual glycol and then refers back to the FluidNames (first fluid listed in the above example) or to one of the default glycol fluids (second fluid listed in the above example).  It is critical that module developers refer to the "fluid name" listed in the GlycolConcentrations object.  This is the name used inside the fluid property module to access the proper data.  Note that when the GlycolConcentrations object is read in during execution that the module will interpolate down from a two-dimensional array of data (variation on temperature and concentration) to a one-dimensional array of data (with temperature as the only independent variable, concentration of a glycol fluid on any loop is assumed to be constant).  This means that only the temperature (along with the glycol fluid name and index) must be passed into the fluid property module and also saves execution time since only a one-dimensional interpolation is needed.