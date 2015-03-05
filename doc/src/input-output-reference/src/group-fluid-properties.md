# Group – Fluid Properties

The Fluid Property routines in EnergyPlus have been designed for ease of use and to allow users to add refrigerant and glycol data to the input file without having to make any changes to the program code. Fluid properties are calculated by interpolating the tabulated data in the input file. The main input requirement is that in order to add a new fluid, a user must enter a full set of data. The user may determine the interval between data points according to the desired accuracy and range required. Data for a number of common fluids are available in the required format in a reference dataset so that the user may append this data to their input file. In addition, data for Water, Ethylene Glycol (in various concentrations from 0 to 90%), and Propylene Glycol (in various concentrations from 0 to 90%) are available as default values within EnergyPlus.

Each fluid is categorized as either a ‘Refrigerant' or ‘Glycol'. Refrigerant data is further categorized as either saturated or superheated. Saturated refrigerant data is indexed, in the input data, according to pressure and temperature i.e. the specific heat, density, enthalpy in the input data are given for a given combination of pressure and temperature. Saturated refrigerant data is indexed according to pressure and quality. In the case of saturated pressure data only a temperature index is used. Glycol data is indexed in the input data according to temperature and concentration. The input data structure for each property is essentially two-dimensional (a matrix). However, since all fluid loops are assumed to be of constant concentration, EnergyPlus internally will interpolate from input data to data for the actual concentration being used in a particular loop. This reduces internally to a one-dimensional array of data for various properties where temperature is the only variable. This saves time since it reduces all interpolations for glycols to a single dimension.

In most cases there will be combinations of index variable (e.g. some combination of pressure and temperature) at which either no data is available, or the property data is invalid/irrelevant. For example, in the superheated refrigerant data, some combinations of temperature and pressure may correspond to a state in the saturated rather than superheated region. Similarly, in the case of glycols there may be some combinations of temperature and concentration that would correspond to the frozen state. In these cases, and where data is just not available, a zero should be present in the data at these points to indicate an invalid condition. EnergyPlus uses the presence of zeros to detect invalid conditions and give warning messages.

## FluidProperties:Name

The names of each fluid for which property data is given in the user input have to be specified in [FluidProperties:Name](#fluidpropertiesname) data objects. If data is to be included for a given fluid, either a refrigerant or a glycol, then its name must be included in the [FluidProperties:Name](#fluidpropertiesname) objects.

### Inputs

#### Field: Fluid Name

Each fluid name is a unique identifier that will be used in conjunction with the data statements described later in this section. The fluid name is either referred to by a component (in the case of a Refrigerant) or by another input line (in the case of a Glycol). This list is simply a list of user defined fluids present in the input data file.

#### Field: Fluid Type

Each fluid name must be identified by type - either **Refrigerant** or **Glycol**. This designation determines what type of data EnergyPlus expects for the fluid name. If the fluid name is defined as a Refrigerant, the user must supply valid data for pressure, enthalpy, specific heat, and density in the saturated region and enthalpy and density in the superheated region. If the fluid name is defined as a Glycol, the user must supply valid data for specific heat, conductivity, viscosity, and density at various concentrations (percentage of non-water liquid) and temperatures.

An example of these statements in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Name,
      R11, Refrigerant;

    FluidProperties:Name,
      R12, Refrigerant;

    FluidProperties:Name,
      R22, Refrigerant;

    FluidProperties:Name,
      NH3, Refrigerant;

    FluidProperties:Name,
      Steam, Refrigerant;

    FluidProperties:Name,
      SaltBrine, Glycol;

    FluidProperties:Name,
      NewGlycol, Glycol;
~~~~~~~~~~~~~~~~~~~~

## FluidProperties:GlycolConcentration

This input syntax is used to specify fluids that will be used with HVAC loops such as plant and condenser loops. It allows the user to define a mixture of water and some other substance and the particular concentration being used. The fluid name will be referred to by the loops or components and thus used to identify which value is valid for a particular fluid at a particular temperature. A series of three inputs is requires for each fluid concentration definition.

### Inputs

#### Field: Name

Each glycol concentration name is a unique identifier that will be used to define a particular combination of water and some other substance (usually another fluid). Other input (such as loop definitions) will refer to this name, linking the other input to the proper fluid definition.

#### Field: Glycol Type

This field refers to the substance that is being mixed with water to create the glycol. The user may either select from the two default fluids (**EthyleneGlycol** or **PropyleneGlycol**) or specify **UserDefinedGlycolType** and use a name in the subsequent field for fluids that have been entered elsewhere in the input file. It should be noted that users must enter a complete definition of new glycols—including values for conductivity, specific heat, density, and viscosity at various concentrations and temperatures—in order for them to be used in conjunction with a user defined glycol type.

#### Field: User Defined Glycol Name

This field is the name used elsewhere in a [FluidProperties:Name](#fluidpropertiesname) object for defining your own glycol properties. This field is left blank unless you enter **UserDefinedGlycolType** in the previous field.

#### Field: Glycol Concentration

This field allows the user to specify the actual concentration of glycol being used for this particular fluid. A concentration of 0.0 refers to pure water. A concentration of 1.0 would denote 100% of the fluid referenced in the Glycol Name input discussed above. Users should enter this value as a decimal fraction since EnergyPlus will limit the range of valid input values to between 0.0 and 1.0.

An example of this statement in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:GlycolConcentration,
      EthGly25Percent, EthyleneGlycol, ,0.25;
    FluidProperties:GlycolConcentration,
      ProGly40Percent, PropyleneGlycol, ,0.40;
    FluidProperties:GlycolConcentration,
      HalfAndHalf, UserDefinedGlycolType, NewBrine, 0.5;
~~~~~~~~~~~~~~~~~~~~

## FluidProperties:Temperatures

It is assumed that all fluid properties vary with temperature. A common set of temperatures is required for each set of fluid property data. Fluid property data supplied by the user must correspondingly be given at these temperatures. Since in most cases, refrigerant data can be specified at a common set of temperatures, a single set of ‘[FluidProperties:Temperatures](#fluidpropertiestemperatures)' can be referenced by a number of sets of property data. This provides a more compact input structure and avoids listing the temperatures multiple times. In the case of glycol data the same set of temperatures have to be used for each set of data at a given concentration. Units for the temperatures are degrees Celsius.

### Inputs

#### Field: Name

This field is simply an unique identifier name that is used to cross reference the list of temperatures with a specific list of fluid property data. A single temperature list can be used for more than one thermal property or fluid. For example, the same temperature list can be used for the superheated enthalpy of a refrigerant and the specific heat of a glycol if appropriate.

#### Field(s) 1-250: Temperature

These fields (up to 250) are the temperatures at which the data for the thermal properties are to be expected. Temperatures are assumed to be in ascending order in units of Celsius.

An example of this statement in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Temperatures,
          R11Temperatures,
          -70,-65,-60,-55,-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,2,4,6,8,10,12,
          14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,55,60,65,70,
          75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160,
          165,170,175,180,185,190,198;
~~~~~~~~~~~~~~~~~~~~

## FluidProperties:Saturated

Refrigerant property data for the saturated region is entered with the following syntax. For each set of property data a unique name, the type of property, phase and the corresponding list of temperatures have to be specified along with the property values.

### Inputs

#### Field: Name

This name identifies the fluid for which data follows in the remaining syntax. This fluid name must also be given in the fluid names list (ref: FluidProperties:Names object) as a **Refrigerant**.

#### Field: Fluid Property Type

This field identifies the fluid property for which data is being entered with this statement. The options for the saturated region are **Enthalpy, Density, SpecificHeat**, and **Pressure**. The units for these properties are J/kg, kg/m^3^, J/kg-K and Pa, respectively.

#### Field: Fluid Phase

In the saturated region, each property has two values: one for the saturated liquid state and one for the saturated gaseous state. Accordingly, the phase for which the saturated property data applies is specified as either **Fluid** or **FluidGas**. Data for each phase are required and are interpolated according to quality. However, in the case of saturated pressure data values for only one phase are required (i.e. can be specified with phase specified as either Fluid or FluidGas, as this is irrelevant).

#### Field: Temperatures Values Name

This name refers to the corresponding list of temperatures (ref: [FluidProperties:Temperatures](#fluidpropertiestemperatures) object). Each temperature in the temperature list corresponds to a data item found in the remaining fields of this syntax. The first temperature in the temperature list corresponds to the first fluid property value, the second temperature in the temperature list corresponds to the second fluid property value, etc.

#### Field(s) 1-250: Property Value

These fields (up to 250) are the values of the thermal property specified in the fluid property type field for the phase specified in the fluid phase field. The temperature list specified in the temperature list name field links the data with the corresponding temperatures.

An example of this statement in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Saturated,
          R11,Enthalpy,Fluid,R11Temperatures, ! Enthalpy in J/kg
          153580,154600,156310,158580,161300,164380,167740,171330,175100,179020,183060,
          187190,191400,195680,200000,201740,203490,205240,207000,208770,210530,212310,
          214080,215870,217650,219860,221230,223030,224830,226630,228860,230250,232060,
          233860,235700,237520,239350,241180,243010,246350,249450,254080,258730,263480,
          268110,272860,277000,282410,287240,292120,297030,302000,307090,312080,317210,
          322400,327670,333020,338460,344010,349680,355500,361480,367690,374100,381060,
          388850,397280,426300;
~~~~~~~~~~~~~~~~~~~~

## FluidProperties:Superheated

The format of the data for the superheated region is similar to that of the saturated region but with the addition of the pressure for which each set of property values applies. A single temperature specification is required for each set of property data at a number of pressures.

### Inputs

#### Field: Fluid Name

This name identifies the fluid for which data follows in the remaining syntax. This fluid name must show up in the fluid names list (ref: FluidProperties:Names object) as a **Refrigerant**.

#### Field: Fluid Property Type

This field identifies the fluid property for which data is being entered with this statement. The options for the superheated region are **Enthalpy** and **Density**. The units for these properties are J/kg and kg/m^3^, respectively.

#### Field: Temperatures Values Name

This name refers to the corresponding list of temperatures (ref: [FluidProperties:Temperatures](#fluidpropertiestemperatures) object). Each temperature in the temperature list corresponds to a given property data item at each pressure. The first temperature in the temperature list corresponds to the first fluid property value, the second temperature in the temperature list corresponds to the second fluid property value, etc.

#### Field: Pressure

In the superheated region, each fluid property is indexed according to temperature and pressure. Thus, a pressure must be listed for each set of property values. The units for pressure are Pascals (Pa). Lists of properties are various pressures and temperatures form a two-dimensional table from which EnergyPlus interpolates to find the property value at a specified temperature and pressure.

#### Field(s) 1-250: Property Value

These fields (up to 250) are the values of the thermal property specified in the fluid property type field for the phase specified in the fluid phase field. The temperature list specified in the temperature list name field links the data with the corresponding temperatures.

An example of this statement in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Superheated,
        R11,Density,SuperR11Temperatures, ! Density in kg/m^3
        62000., !Pressure = 62000Pa
        0,0,0,0,0,0,0,0.0139,0.0134,0.0129,0.0124,0.012,0.0116,0.0112,0.0109,0.0105,
        0.0102,0.0099,0.0097,0.0094,0.0092,0.0089,0,0,0,0,0,0,0,0,0,0;
~~~~~~~~~~~~~~~~~~~~

## Glycol Fluid Types

A full data set for a glycol fluid type in EnergyPlus includes: a temperature list and lists of values for specific heat at a given concentration. Data have to be given for at least two different concentrations. The syntax lines for the property value lists are described below.

## FluidProperties:Concentration

The format of the data for the glycols is similar to that of superheated refrigerants except that data at a number of concentrations is given, rather than pressures. The concentration is listed before the rest of the data and is normalized (i.e. in the range 0.0 to 1.0, rather than a percentage). Note that glycol solutions are assumed to exist in liquid form only in EnergyPlus.

### Inputs

#### Field: Fluid Name

This name identifies the fluid for which data follows in the remaining syntax. This fluid name must show up in the fluid names list (ref: FluidProperties:Names object) as a **Glycol**.. Note that users should not use Water, EthyleneGlycol, or PropyleneGlycol as names since these are default fluids.

#### Field: Fluid Property Type

This field identifies the fluid property for which data is being entered with this statement. The required data for glycols includes **SpecificHeat** with units of J/kg-K, **Density** with units of kg/m^3^, **Conductivity** with units of W/m-K, and **Viscosity** with units of N-s/m^2^.

#### Field: Temperatures Values Name

This name refers to the corresponding list of temperatures (ref: [FluidProperties:Temperatures](#fluidpropertiestemperatures) object). Each temperature in the temperature list corresponds to a data item found in the remaining fields of this syntax. The first temperature in the temperature list corresponds to the first fluid property value, the second temperature in the temperature list corresponds to the second fluid property value, etc.

#### Field: Concentration

A concentration must be listed for the values of specific heat that follow. Lists of properties are various concentrations and temperatures form a two-dimensional table from which EnergyPlus interpolates to find the property value at a specified temperature and concentration. Concentrations are specified in a normalized form (i.e. in the range 0.0 to 1.0, rather than a percentage)

#### Field(s) 1-250: Property Value

These fields (up to 250) are the values of the thermal property specified in the fluid property type field for the phase specified in the fluid phase field. The temperature list specified in the temperature list name field links the data with the corresponding temperatures.

An example of this statement in an input data file is:

~~~~~~~~~~~~~~~~~~~~

    FluidProperties:Concentration,
         NewGlycol,SpecificHeat ,GlycolTemperatures, ! Spec. heat in J/kg-K
         0.8, ! Concentration
         2572,2600,2627,2655,2683,2710,2738,2766,2793,2821,2849,2876,2904,2931,2959,
         2987,3014,3042,3070,3097,3125,3153,3180,3208,3236,3263,3291,3319,3346,3374,
         3402,3429,3457;
~~~~~~~~~~~~~~~~~~~~