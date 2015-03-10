# Group – Surface Construction Elements

This group of objects describes the physical properties and configuration for the building envelope and interior elements. That is, the walls, roofs, floors, windows, doors for the building.

## Specifying the Building Envelope

[Building](#building) element constructions in EnergyPlus are built from the basic thermal and other material property parameters in physical constructions. Materials are specified by types  and named. Constructions are defined by the composition of materials. Finally, surfaces are specified for the building with geometric coordinates as well as referenced constructions.

## Material and Material Properties

There are several material "types" which may be used to describe layers within opaque construction elements. The choice of which of these types to use is left up to the user. However, some guidance as to which material type to use is appropriate before describing each in detail. The opaque types are:

- Material
- Material:NoMass
- Material:AirGap
- Material:RoofVegetation
- Material:InfraredTransparent

[Material](#material-and-material-properties) is the "preferred" type of material. This requires knowledge of many of the thermal properties of the material, but it allows EnergyPlus to take into account the thermal mass of the material and thus allows the evaluation of transient conduction effects. [Material:NoMass](#materialnomass) is similar in nature but only requires the thermal resistance (R-value) rather than the thickness, thermal conductivity, density, and specific heat. Note that using a simple R-value only material forces EnergyPlus to assume steady state heat conduction through this material layer. Finally, [Material:AirGap](#materialairgap) should only be used for an air gap between other layers in a construction. This type assumes that air is sufficiently lightweight to require only an R-value. In addition, since it is not exposed to any external environment, surface properties such as absorptance are not necessary. [Material:RoofVegetation](#materialroofvegetation) is used to help model "green roofs". [Material:InfraredTransparent](#materialinfraredtransparent) is used similarly to the NoMass materials. Each of these materials is described in more detail below.

There are several material additions that can be made to the basic material properties. These additional material types are:

- MaterialProperty:MoisturePenetrationDepth:Settings
- MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm
- MaterialProperty:HeatAndMoistureTransfer:Diffusion
- MaterialProperty:HeatAndMoistureTransfer:Settings
- MaterialProperty:HeatAndMoistureTransfer:Redistribution
- MaterialProperty:HeatAndMoistureTransfer:Suction
- MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity
- MaterialProperty:PhaseChange

These material property objects are used in conjunction with the basic material specification and reference back to the name of the basic material type. Without the basic material type specified the program, will give a severe error and terminate. For example, specifying the moisture materials and changing the [HeatBalanceAlgorithm](#heatbalancealgorithm) to a moisture simulation will allow the moisture simulation to take place.

## Material

This definition should be used when the four main thermal properties (thickness, conductivity, density, and specific heat) of the material are known. This syntax is used to describe opaque construction elements only.

When a [Material](#material-and-material-properties) is used for the [Construction](#construction) of a building surface, care should be taken to not attempt to model assemblies that were not included in the intended scope of applicability for the underlying heat transfer models.  The building surface models are for normal applications to building energy efficiency where the main focus is on assemblies with some thermal resistance. Extremely thin and/or highly conductive material layers should be neglected from the [Construction](#construction) rather than included because they will not contribute to the assembly's overall thermal resistance or heat capacity. For some cases, thin and/or highly conductive materials are a serious problem for the heat transfer modeling and the values for thickness, conductivity, density and specific heat are checked for appropriateness. This check calculates the [Material](#material-and-material-properties)'s thermal diffusivity from the inputs for conductivity, density, and specific heat and compares it to a maximum threshold of 1.E-5 (m^2^/s). If the diffusivity is above this threshold, then the program checks if the layer is sufficiently thick and may issue a warning if it is too thin and highly conductive.

The absorptance values in this object impart surface properties to the construction and should be applied to the thermally significant inner and outer layers in the overall assembly.  Attempting to trick the program by modeling thin "paint" layers to apply surface properties is not a good idea; the models were not intended to support such strategies.

### Inputs

#### Field: Name

This field is a unique reference name that the user assigns to a particular material. This name can then be referred to by other input data (ref: [Construction](#construction) object).

#### Field: Roughness

This alpha field defines the relative roughness of a particular material layer. This parameter only influences the convection coefficients, more specifically the exterior convection coefficient. A special keyword is expected in this field with the options being "VeryRough", "Rough", "MediumRough", "MediumSmooth", "Smooth", and "VerySmooth" in order of roughest to smoothest options.

#### Field: Thickness

This field characterizes the thickness of the material layer in meters. This should be the dimension of the layer in the direction perpendicular to the main path of heat conduction. This value must be a positive. **Modeling layers thinner (less) than 0.003 m is not recommended; rather, add those properties to one of the adjacent layers.**

#### Field: Conductivity

This field is used to enter the thermal conductivity of the material layer. Units for this parameter are W/(m-K). Thermal conductivity must be greater than zero. **Modeling layers with conductivity higher than 5.0 W/(m-K) is not recommended; however, this may be appropriate for non-surfaces such as pipes and TDDs (ref. [DaylightingDevice:Tubular](#daylightingdevicetubular) object).**

#### Field: Density

This field is used to enter the density of the material layer in units of kg/m^3^. Density must be a positive quantity. **In some cases textbooks and references may use g/m^3^: be careful to not confuse units.**

#### Field: Specific Heat

This field represents the specific heat of the material layer in units of J/(kg-K). Note that these units are most likely different than those reported in textbooks and references which tend to use kJ/(kg-K) or J/(g-K). They were chosen for internal consistency within EnergyPlus. **Only values of specific heat of 100 or larger are allowed. Typical ranges are from 800 to 2000 J/(kg-K).**

#### Field: Thermal Absorptance

The thermal absorptance field in the [Material](#material-and-material-properties) input syntax represents the fraction of incident long wavelength radiation that is absorbed by the material. This parameter is used when calculating the long wavelength radiant exchange between various surfaces and affects the surface heat balances (both inside and outside as appropriate). For long wavelength radiant exchange, thermal emissivity and thermal emittance are equal to thermal absorptance. Values for this field must be between 0.0 and 1.0 (with 1.0 representing "black body" conditions).

#### Field: Solar Absorptance

The solar absorptance field in the [Material](#material-and-material-properties) input syntax represents the fraction of incident  solar radiation that is absorbed by the material. Solar radiation includes the visible spectrum as well as infrared and ultraviolet wavelengths. This parameter is used when calculating the amount of incident solar radiation absorbed by various surfaces and affects the surface heat balances (both inside and outside as appropriate). If solar reflectance (or reflectivity) data is available, then absorptance is equal to 1.0 minus reflectance (for opaque materials). Values for this field must be between 0.0 and 1.0.

#### Field: Visible Absorptance

The visible absorptance field in the [Material](#material-and-material-properties) input syntax represents the fraction of incident visible wavelength radiation that is absorbed by the material. Visible wavelength radiation is slightly different than solar radiation in that the visible band of wavelengths is much more narrow while solar radiation includes the visible spectrum as well as infrared and ultraviolet wavelengths. This parameter is used when calculating the amount of incident visible radiation absorbed by various surfaces and affects the surface heat balances (both inside and outside as appropriate) as well as the daylighting calculations. If visible reflectance (or reflectivity) data is available, then absorptance is equal to 1.0 minus reflectance (for opaque materials). Values for this field must be between 0.0 and 1.0.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Material,A2 - 4 IN DENSE FACE BRICK,  ! Material Name
     Rough,  ! Roughness
      0.1014984    ,  ! Thickness {m}
       1.245296    ,   ! Conductivity {W/M*K}
       2082.400    ,   ! Density {Kg/M**3}
      920.4800    ,   ! Specific Heat {J/Kg*K}
      0.9000000    ,   ! Thermal Absorptance
      0.9300000    ,   ! Solar Absorptance
      0.9300000    ;   ! Visible Absorptance
~~~~~~~~~~~~~~~~~~~~

## Material:NoMass

Use this definition when only the thermal resistance (R value) of the material is known. This object is used to describe opaque construction elements.

### Inputs

#### Field: Name

This field is a unique reference name that the user assigns to a particular material. This name can then be referred to by other input data (ref: [Construction](#construction) object).

#### Field: Roughness

This alpha field defines the relative roughness of a particular material layer. This parameter only influences the convection coefficients, more specifically the exterior convection coefficient. A  keyword is expected in this field with the options being "**VeryRough**", "**Rough**", "**MediumRough**", "**MediumSmooth**", "**Smooth**", and "**VerySmooth**" in order of roughest to smoothest options.

#### Field: Thermal Resistance

This field is used to enter the thermal resistance (R-value) of the material layer. Units for this parameter are (m^2^-K)/W. Thermal resistance must be greater than zero. Note that most R-values in the USA are calculated in Inch-Pound units and must be converted to the SI equivalent.

#### Field: Thermal Absorptance

The thermal absorptance field in the [Material](#material-and-material-properties) input syntax represents the fraction of incident long wavelength radiation that is absorbed by the material. This parameter is used when calculating the long wavelength radiant exchange between various surfaces and affects the surface heat balances (both inside and outside as appropriate). For long wavelength radiant exchange, thermal emissivity and thermal emittance are equal to thermal absorptance. Values for this field must be between 0.0 and 1.0 (with 1.0 representing "black body" conditions).

#### Field: Solar Absorptance

The solar absorptance field in the [Material](#material-and-material-properties) input syntax represents the fraction of incident  solar radiation that is absorbed by the material. Solar radiation includes the visible spectrum as well as infrared and ultraviolet wavelengths. This parameter is used when calculating the amount of incident solar radiation absorbed by various surfaces and affects the surface heat balances (both inside and outside as appropriate). If solar reflectance (or reflectivity) data is available, then absorptance is equal to 1.0 minus reflectance (for opaque materials). Values for this field must be between 0.0 and 1.0.

#### Field: Visible Absorptance

The visible absorptance field in the [Material](#material-and-material-properties) input syntax represents the fraction of incident visible wavelength radiation that is absorbed by the material. Visible wavelength radiation is slightly different than solar radiation in that the visible band of wavelengths is much more narrow while solar radiation includes the visible spectrum as well as infrared and ultraviolet wavelengths. This parameter is used when calculating the amount of incident visible radiation absorbed by various surfaces and affects the surface heat balances (both inside and outside as appropriate) as well as the daylighting calculations. If visible reflectance (or reflectivity) data is available, then absorptance is equal to 1.0 minus reflectance (for opaque materials). Values for this field must be between 0.0 and 1.0.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Material:NoMass,R13LAYER,  ! Material Name
     Rough,  ! Roughness
       2.290965    ,  ! Resistance {M**2K/W}
      0.9000000    ,   ! Thermal Absorptance
      0.7500000    ,   ! Solar Absorptance
      0.7500000    ;   ! Visible Absorptance
~~~~~~~~~~~~~~~~~~~~

## Material:InfraredTransparent

A Infrared Transparent surface is similar to a resistance-only surface.  The idd object for this type of surface is shown below.  The surface will actually participate in the transfer of visible and solar radiation by doing a wavelength transformation and making all short wave length radiation that is incident on the surface into long wave length radiation and having it participate in the long wavelength radiant exchange.  **Note the ConvectionCoefficient instructions that follow the Infrared Transparent construction object below**.

### Inputs

#### Field: Name

This field contains the unique name (across all [Material](#material-and-material-properties) objects) for the Infrared Transparent material.

A Infrared Transparent surface should not participate in a convective/conductive exchange between the zones it separates.  In order to minimize this effect, the ConvectionCoefficients object must be used for the surfaces referencing the Infrared Transparent (IRT) construction.

An example idf object specification for use with the IRT surface is shown below. Note that surfaces are not described in this example

~~~~~~~~~~~~~~~~~~~~

    Material:InfraredTransparent,
        IRTMaterial1;            !- Name

    Construction,
        IRTSurface,              !- Name
        IRTMaterial1;            !- Outside Layer

    SurfaceProperty:ConvectionCoefficients,
        Bottom:Top,              !- SurfaceName
        Outside,                 !- Convection Type 1
        value,                   !- Convection Value Type 1
        0.1,                     !- Convection value 1 {W/m2-K}
        ,                        !- Convection Schedule 1
        Inside,                  !- Convection Type 2
        value,                   !- Convection Value Type 2
        0.1;                     !- Convection value 2 {W/m2-K}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    SurfaceProperty:ConvectionCoefficients,
        SecondLevel:Bottom,      !- SurfaceName
        Outside,                 !- Convection Type 1
        value,                   !- Convection Value Type 1
        0.1,                     !- Convection value 1 {W/m2-K}
        ,                        !- Convection Schedule 1
        Inside,                  !- Convection Type 2
        value,                   !- Convection Value Type 2
        0.1;                     !- Convection value 2 {W/m2-K}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    SurfaceProperty:ConvectionCoefficients,
        SecondLevel:Top,         !- SurfaceName
        Outside,                 !- Convection Type 1
        value,                   !- Convection Value Type 1
        0.1,                     !- Convection value 1 {W/m2-K}
        ,                        !- Convection Schedule 1
        Inside,                  !- Convection Type 2
        value,                   !- Convection Value Type 2
        0.1;                     !- Convection value 2 {W/m2-K}

    SurfaceProperty:ConvectionCoefficients,
        ThirdLevel:Bottom,       !- SurfaceName
        Outside,                 !- Convection Type 1
        value,                   !- Convection Value Type 1
        0.1,                     !- Convection value 1 {W/m2-K}
        ,                        !- Convection Schedule 1
        Inside,                  !- Convection Type 2
        value,                   !- Convection Value Type 2
        0.1;                     !- Convection value 2 {W/m2-K}
~~~~~~~~~~~~~~~~~~~~

## Material:AirGap

This material is used to describe the air gap in an opaque construction element. Glass elements use a different property (WindowGas) to describe the air between two glass layers.

### Inputs

#### Field: Name

This field is a unique reference name that the user assigns to a particular material. This name can then be referred to by other input data (ref: [Construction](#construction) object).

#### Field: Thermal Resistance

This field is used to enter the thermal resistance (R-value) of the material layer. Units for this parameter are (m^2^-K)/W. Thermal resistance must be greater than zero. Note that most R-values in the USA are calculated in Inch-Pound units and must be converted to the SI equivalent.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Material:AirGap,B1 - AIRSPACE RESISTANCE,  ! Material Name
      0.1603675    ;  ! Resistance {M**2K/W}
~~~~~~~~~~~~~~~~~~~~

## MaterialProperty:MoisturePenetrationDepth:Settings

This material is used to describe the five moisture material properties that are used in the EMPD (Effective Moisture Penetration Depth) heat balance solution algorithm (known there as MoisturePenetrationDepthConductionTransferFunction). The EMPD algorithm is a simplified, lumped moisture model that simulates moisture storage and release from interior surfaces. The model uses "actual" convective mass transfer coefficients that are determined by existing heat and mass transfer relationships, e.g. the Lewis relation. An effective moisture penetration depth may be determined from either experimental or detailed simulation data by using actual surface areas and moisture sorption isotherms.

This moisture model will be used when the appropriate EMPD moisture materials are specified and the Solution Algorithm parameter is set to EMPD.

### Inputs

#### Field: Name

This field is a unique reference name that the user assigns to a particular material. This name can then be referred to by other input data (ref: [Construction](#construction) object).

#### Field: Moisture Penetration Depth

This field is used to enter the effective moisture penetration depth of the material layer. Units for this parameter are (m).

#### Field: Constants to Define Moisture Equilibrium Equation

The next four fields, coefficients "a", "b", "c", and "d", help define the sorption isotherm curve used for building materials under equilibrium conditions. They are used to define the relationship between the material's moisture content and the surface air relative humidity (ref: Effective Moisture Penetration Depth (EMPD) Model in the Engineering Reference):

![](media/image21.png)\


where

a,b,c,d= Coefficients to define the relationship between the material's moisture content and the surface air relative humidity

U= Moisture content defined as the mass fraction of water contained in a material [kg/kg]

![](media/image22.png) = Surface air relative humidity [0 to 1]

#### The next four fields are dimensionless coefficients:

#### Field: Moisture Equation Coefficient a

#### Field: Moisture Equation Coefficient b

#### Field: Moisture Equation Coefficient c

#### Field: Moisture Equation Coefficient d

Ann IDF example showing how it is used in conjunction with [Material](#material-and-material-properties) in synchronous pairs:

~~~~~~~~~~~~~~~~~~~~

      Material,
        E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name
        Smooth,                  !- Roughness
        1.9050000E-02,           !- Thickness {m}
        0.7264224,               !- Conductivity {W/m-K}
        1601.846,                !- Density {kg/m3}
        836.8000,                !- Specific Heat {J/kg-K}
        0.9000000,               !- Thermal Absorptance
        0.9200000,               !- Solar Absorptance
        0.9200000;               !- Visible Absorptance

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      MaterialProperty:MoisturePenetrationDepth:Settings,
        E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name
        0.004,                   !- Effective Moisture Penetration Depth {m}
        0.072549,           !- Moisture Equation Coefficient a {dimensionless}
        0.397173,           !- Moisture Equation Coefficient b {dimensionless}
        0.007774,           !- Moisture Equation Coefficient c {dimensionless}
        11.7057;            !- Moisture Equation Coefficient d {dimensionless}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Material,
        C10 - 8 IN HW CONCRETE,  !- Name
        MediumRough,             !- Roughness
        0.2033016,               !- Thickness {m}
        1.729577,                !- Conductivity {W/m-K}
        2242.585,                !- Density {kg/m3}
        836.8000,                !- Specific Heat {J/kg-K}
        0.9000000,               !- Thermal Absorptance
        0.6500000,               !- Solar Absorptance
        0.6500000;               !- Visible Absorptance

      MaterialProperty:MoisturePenetrationDepth:Settings,
        C10 - 8 IN HW CONCRETE,  !- Name
        0.004,                   !- Effective Moisture Penetration Depth {m}
        0.018062,             !- Moisture Equation Coefficient a {dimensionless}
        0.451879,             !- Moisture Equation Coefficient b {dimensionless}
        0.026178,             !- Moisture Equation Coefficient c {dimensionless}
        10.8356;              !- Moisture Equation Coefficient d {dimensionless}
~~~~~~~~~~~~~~~~~~~~

### Outputs

Output variables applicable to heat transfer surfaces using EMPD model:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,EMPD Surface Inside Face Water Vapor Density [kg/m3]
    Zone,Average,EMPD Surface Inside Face Humidity Ratio [kgWater/kgDryAir]
    Zone,Average,EMPD Surface Inside Face Relative Humidity [%]
~~~~~~~~~~~~~~~~~~~~

The following variables apply only to surfaces, where the material assigned to the inside layers is [MaterialProperty:MoisturePenetrationDepth:Settings](#materialpropertymoisturepenetrationdepthsettings). The EMPD (Effective Moisture Penetration Depth) moisture balance solution algorithm is used to calculate the inside surface moisture levels.

#### EMPD Surface Inside Face Water Vapor Density [kg/m3]

The vapor density at the inside surface, where the EMPD moisture balance solution algorithm is applied.

#### EMPD Surface Inside Face Humidity Ratio [kgWater/kgDryAir]

The humidity ratio at the inside surface, where the EMPD moisture balance solution algorithm is applied.

#### EMPD Surface Inside Face Relative Humidity [%]

The relative humidity at the inside surface, where the EMPD moisture balance solution algorithm is applied.

## MaterialProperty:PhaseChange

**Advanced/Research Usage:** This material is used to describe the temperature dependent material properties that are used in the Conduction Finite Difference solution algorithm. This conduction model is done when the appropriate materials are specified and the Solution Algorithm parameter is set to ConductionFiniteDifference. This permits simulating temperature dependent thermal conductivity and phase change materials (PCM) in EnergyPlus.

### Inputs

#### Field: Name

This field is a regular material name specifying the material with which this additional temperature dependent property information will be associated.

#### Field: Temperature Coefficient for Thermal Conductivity

This field is used to enter the temperature dependent coefficient for thermal conductivity of the material.  Units for this parameter are (W/(m-K^2^). This is the thermal conductivity change per unit temperature excursion from 20 C. The conductivity value at 20 C is the one specified with the basic material properties of the regular material specified in the name field. The thermal conductivity is obtained from:

![](media/image23.png)\


where:

k~o~ is the 20C value of thermal conductivity(normal idf  input)

k~1~ is the change in conductivity per degree temperature difference from 20C

(this field).

#### Field Set: Temperature-Enthalpy

The temperature – enthalpy set of inputs specify a two column tabular temperature-enthalpy function for the basic material. Sixteen pairs can be specified. Specify only the number of pairs necessary. The tabular function must cover the entire temperature range that will be seen by the material in the simulation. It is suggested that the function start at a low temperature, and extend to 100C. Note that the function has no negative slopes and the lowest slope that will occur is the base material specific heat. Temperature values should be strictly increasing. Enthalpy contributions of the phase change are always added to the enthalpy that would result from a constant specific heat base material.  An example of a simple enthalpy temperature function is shown below.

![](media/image24.png)\


#### Field: Temperature x

This field is used to specify the temperature of the temperature-enthalpy function for the basic material. Units are C.

#### Field: Enthalpy x

This field specifies the enthalpy that corresponds to the previous temperature of the temperature-enthalpy function. Units are J/kg.

And, an IDF example showing how it is used in conjunction with the Material:

Note, the following Heat Balance Algorithm is necessary (only specified once). Also, when using ConductionFiniteDifference, it is more efficient to set the zone timestep shorter than those used for the ConductionTransferFunction solution algorithm. It should be set to 12 timesteps per hour or greater, and can range up to 60.

~~~~~~~~~~~~~~~~~~~~

    HeatBalanceAlgorithm,
    ConductionFiniteDifference;

    Timestep,
    12;
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Material,
        E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name
        Smooth,                  !- Roughness
        1.9050000E-02,           !- Thickness {m}
        0.7264224,               !- Conductivity {W/m-K}
        1601.846,                !- Density {kg/m3}
        836.8000,                !- Specific Heat {J/kg-K}
        0.9000000,               !- Thermal Absorptance
        0.9200000,               !- Solar Absorptance
        0.9200000;               !- Visible Absorptance

    MaterialProperty:PhaseChange,
        E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name
        0.0,             !- Temperature coefficient,thermal conductivity(W/m K2)
        -20.,            !- Temperature 1, C
        0.01,            !- Enthalpy 1 at –20C, (J/kg)
        20.,             !- Temperature 2, C
        33400,           !- Enthalpy 2, (J/kg)
        20.5,            !- temperature 3, C
        70000,           !- Ethalpy 3, (J/kg)
        100.,            !- Temperature 4, C
        137000;          !- Enthalpy 4, (J/kg)
~~~~~~~~~~~~~~~~~~~~

## MaterialProperty:VariableThermalConductivity

This object is used to describe the temperature dependent material properties that are used in the CondFD (Conduction Finite Difference) solution algorithm. This conduction model is used when the appropriate CondFD materials are specified and the Solution Algorithm parameter is set to condFD.

### Inputs

#### Field: Name

This field is a regular material name specifying the material with which this additional temperature dependent property information will be associated.

#### Field Set: Temperature-Thermal Conductivity

The temperature – conductivity set of inputs specify a two column tabular temperature-thermal conductivity function for the basic material. Ten pairs can be specified. Specify only the number of pairs necessary. Temperature values should be strictly increasing.

#### Field: Temperature x

This field is used to specify the temperature of the temperature-conductivity function for the basic material. Units are C.

#### Field: Thermal Conductivity x

This field specifies the conductivity that corresponds to the temperature (previous field) of the temperature-conductivity function. Units are W/m-K.

And, an IDF example showing how it is used in conjunction with the Materials:

Note, the following Heat Balance Algorithm is necessary (only specified once). Also, when using Conduction Finite Difference, it is more efficient to set the zone time step shorter than those used for the Conduction Transfer Function solution algorithm. It should be set to 12 time steps per hour or greater, and can range up to 60.

~~~~~~~~~~~~~~~~~~~~

    HeatBalanceAlgorithm,
    ConductionFiniteDifference;

    Timestep,
    12;

    Material,
        PCMPlasterBoard ,      !- Name
        Smooth,                  !- Roughness
        1.9050000E-02,           !- Thickness {m}
        4.2,                     !- Conductivity {W/m-K}
        1601.846,                !- Density {kg/m3}
        836.8000,                !- Specific Heat {J/kg-K}
        0.9000000,               !- Thermal Absorptance
        0.9200000,               !- Solar Absorptance
        0.9200000;               !- Visible Absorptance

    MaterialProperty:VariableThermalConductivity,
        PCMPlasterBoard,         !- Name
        0,                       !- Temperature 1 {C}
        4.2,                     !- Thermal Conductivity 1 {W/m-K}
        22,                      !- Temperature 2 {C}
        4.2,                     !- Thermal Conductivity 2 {W/m-K}
        22.1,                    !- Temperature 3 {C}
        2.5,                     !- Thermal Conductivity 3 {W/m-K}
        100,                     !- Temperature 4 {C}
        2.5;                     !- Thermal Conductivity 4 {W/m-K}
~~~~~~~~~~~~~~~~~~~~

### Outputs

The Conduction Finite Difference solution algorithm uses a finite difference solution technique, the surfaces are divided into a nodal arrangement. The only output specific to Conduction Finite Difference solution (that is not include in other surface outputs) is node temperatures.

The following output variables are applicable to all opaque heat transfer surfaces when using Solution Algorithms ConductionFiniteDifference:

~~~~~~~~~~~~~~~~~~~~

    Zone,Sum,CondFD Inner Solver Loop Iteration Count []
    Zone,Average,CondFD Surface Temperature Node <1 – N> [C]
~~~~~~~~~~~~~~~~~~~~

#### CondFD Inner Solver Loop Iteration Count []

This outputs the count of iterations on the inner solver loop of CondFD for each surface.

#### CondFD Surface Temperature Node <X> [C]

This will output temperatures for a node in the surfaces being simulated with ConductionFiniteDifference. The key values for this output variable are the surface name. The nodes are numbered from outside to inside of the surface. The full listing will appear in the RDD file

## MaterialProperty:HeatAndMoistureTransfer:Settings

**Advanced/Research Usage:** This object is used to describe two of the seven additional material properties needed for the CombinedHeatAndMoistureFiniteElement heat balance solution algorithm. The settings object is used when the solutions algorithm is set to CombinedHeatAndMoistureFiniteElement and the appropriate material properties are assigned to each material. This permits the simulation of the moisture dependant thermal properties of the material as well as the transfer of moisture through, into and out of the material into the zone or exterior.

In addition to the Porosity and Initial Water content properties described here, five additional properties, described by tabulated relationships between variables, are required. These properties are;

- MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm
- MaterialProperty:HeatAndMoistureTransfer:Suction
- MaterialProperty:HeatAndMoistureTransfer:Redistribution
- MaterialProperty:HeatAndMoistureTransfer:Diffusion
- MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity

All materials in a construction are required to have all material properties defined for HAMT to work.

Within the [MaterialProperty:HeatAndMoistureTransfer:Settings](#materialpropertyheatandmoisturetransfersettings) object the following fields are defined.

### Inputs

#### Field: Material Name

This field is a unique reference name that the user assigns to a particular material. This name can then be referred to by other input data.

#### Field: Porosity

The porosity of a material is the maximum fraction, by volume, of a material that can be taken up with water. The units are [m3/m3].

#### Field: Initial Water Content Ratio

For this solution algorithm, the initial water content is assumed to be distributed evenly through the depth of the material. The units are [kg/kg].

Below is an example input for the porosity and initial water content of a material.

~~~~~~~~~~~~~~~~~~~~

    MaterialProperty:HeatAndMoistureTransfer:Settings,
          Concrete,     !- Name
          0.76,     !- Porosity
          0.2;     !- Initial (or typical) Water content
~~~~~~~~~~~~~~~~~~~~

## MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm

**Advanced/Research Usage:** This material property is used in conjunction with the CombinedHeatAndMoistureFiniteElement heat balance solution algorithm.

The Isotherm data relates the moisture, or water content [kg/m3] of a material with the relative humidity (RH). The water content is expected to increase as relative humidity increases, starting at zero content at 0.0relative humidity fraction and reaching a maximum, defined by the porosity, at 1.0 relative humidity fraction, which corresponds to 100% relatve humidity. Relative humidities are entered as fraction for this object ranging from 0.0 to 1.0. These two extremes (0.0 and 1.0) are automatically set by the HAMT solution. However, if they are entered they will be used as extra data points. Data should be provided with increasing RH and moisture content up to as high an RH as possible to provide a stable solution. One possible reason for the following error message may be that a material has a very rapid increase in water content for a small change in RH, which can happen if the last entered water content point is at a low RH and the material has a very high porosity.

~~~~~~~~~~~~~~~~~~~~

      ** Warning ** HeatAndMoistureTransfer: Large Latent Heat for Surface ROOF
~~~~~~~~~~~~~~~~~~~~

Another potential reason for this error being generated is the use of inappropriate values for Vapor Transfer Coefficients. See the [SurfaceProperties:VaporCoefficients](#surfacepropertiesvaporcoefficients) object in the Advanced Surface Concepts group.

### Inputs

#### Field: Material Name

This field is a unique reference name that the user assigns to a particular material. This name can then be referred to by other input data.

#### Field: Number of data  Coordinates

A maximum of 25 coordinates can be specified.

#### Field Set: Relative Humidity-Moisture Content

#### Field: Relative Humidity Fraction x

The relative humidity of the x^th^ coordinate. The relative humidity is entered as fraction, not in percent.

#### Field: Moisture Content x

The Moisture Content of the x^th^ coordinate. The units are [kg/m3]

Below is an example input for a material isotherm

~~~~~~~~~~~~~~~~~~~~

    MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm,
          Concrete,     !- Name
          10,     !- Number of data Coordinates
          0.2205,    !- Relative Humidity fraction #1
          22.31,     !- Moisture content #1
          0.202,     !- Relative Humidity fraction #2
          19.665,    !- Moisture content #2
          0.449,     !- Relative Humidity fraction #3
          40.02,     !- Moisture content #3
          0.454,     !- Relative Humidity fraction #4
          36.915,    !- Moisture content #4
          0.6506,    !- Relative Humidity fraction #5
          56.005,    !- Moisture content #5
          0.655,     !- Relative Humidity fraction #6
          52.325,    !- Moisture content #6
          0.824,     !- Relative Humidity fraction #7
          72.565,    !- Moisture content #7
          0.8725,    !- Relative Humidity fraction #8
          85.1,      !- Moisture content #8
          0.924,     !- Relative Humidity fraction #9
          91.08,     !- Moisture content #9
          0.964,     !- Relative Humidity fraction #10
          100.28;    !- Moisture content #10
~~~~~~~~~~~~~~~~~~~~

## MaterialProperty:HeatAndMoistureTransfer:Suction

**Advanced/Research Usage:** This material property is used in conjunction with the CombinedHeatAndMoistureFiniteElement heat balance solution algorithm.

The suction data relates the liquid transport coefficient, under suction, to the water content of a material. A data point at zero water content is required. The liquid transport coefficient at the highest entered water content value is used for all liquid transport coefficient values above this water content. These coefficients are used by HAMT when the rain flag is set in the weather file.

### Inputs

#### Field: Material Name

This field is a unique reference name that the user assigns to a particular material. This name can then be referred to by other input data.

#### Field: Number of Suction points

A maximum of 25 points can be specified.

#### Field Set: Moisture Content-Liquid Transport Coefficient

#### Field: Moisture Content x

The moisture content of the x^th^ point. The units are [kg/m3].

#### Field: Liquid Transport Coefficient x

The Liquid Transport Coefficient of the x^th^ point. The units are [m2/s].

Below is an example input for a material liquid transport coefficient under suction.

~~~~~~~~~~~~~~~~~~~~

    MaterialProperty:HeatAndMoistureTransfer:Suction,
          Concrete,     !- Name
          5,     !- Number of Suction points
          0,     !- Moisture content 1
          0,     !- Liquid Transport Coefficient 1
          72,     !- Moisture content 2
          0.0000000000741,     !- Liquid Transport Coefficient 2
          85,     !- Moisture content 3
          0.000000000253,     !- Liquid Transport Coefficient 3
          100,     !- Moisture content 4
          0.00000000101,     !- Liquid Transport Coefficient 4
          118,     !- Moisture content 5
          0.00000000128;     !- Liquid Transport Coefficient 5
~~~~~~~~~~~~~~~~~~~~

## MaterialProperty:HeatAndMoistureTransfer:Redistribution

**Advanced/Research Usage:** This material property is used in conjunction with the CombinedHeatAndMoistureFiniteElement heat balance solution algorithm.

The redistribution data relates the liquid transport coefficient to the water content of a material under normal conditions. A data point at zero water content is required. The liquid transport coefficient at the highest entered water content value is used for all liquid transport coefficient values above this water content. These coefficients are used by the Heat and Moisture Transfer algorithm when the rain flag is NOT set in the weather file.

### Inputs

#### Field: Material Name

This field is a unique reference name that the user assigns to a particular material. This name can then be referred to by other input data.

#### Field: Number of Redistribution points

A maximum of 25 points can be specified.

#### Field Set: Moisture Content-- Liquid Transport Coefficient

#### Field: Moisture Content x

The moisture content of the x^th^ point. The units are [kg/m3].

#### Field: Liquid Transport Coefficient x

The Liquid Transport Coefficient of the x^th^ point. The units are [m2/s].

Below is an example input for the object.

~~~~~~~~~~~~~~~~~~~~

    MaterialProperty:HeatAndMoistureTransfer:Redistribution,
          Concrete,     !- Name
          5,     !- Number of Redistribution points
          0,     !- Moisture content 1
          0,     !- Liquid Transport Coefficient 1
          72,     !- Moisture content 2
          0.00000000000741,     !- Liquid Transport Coefficient 2
          85,     !- Moisture content 3
          0.0000000000253,     !- Liquid Transport Coefficient 3
          100,     !- Moisture content 4
          0.000000000101,     !- Liquid Transport Coefficient 4
          118,     !- Moisture content 5
          0.000000000128;     !- Liquid Transport Coefficient 5
~~~~~~~~~~~~~~~~~~~~

## MaterialProperty:HeatAndMoistureTransfer:Diffusion

**Advanced/Research Usage:** This material property is used in conjunction with the CombinedHeatAndMoistureFiniteElement heat balance solution algorithm.

The MU data relates the vapor diffusion resistance factor (dimensionless) to the relative humidity as fraction(RH). A data point at zero RH is required. The vapor diffusion resistance factor at the highest entered relative humidity (RH) value is used for all vapor diffusion resistance factor values above this RH.  The relative humidity maximum value in fraction is 1.0.

### Inputs

#### Field: Material Name

This field is a unique reference name that the user assigns to a particular material. This name can then be referred to by other input data.

#### Field: Number of Data Pairs

A maximum of 25 pairs can be specified.

#### Field Set: Relative Humidity-Vapor Diffusion Resistance Factor

#### Field: Relative Humidity Fraction #x

The moisture content of the x^th^ pair. The relative humidity is entered as fraction, not in percent.

#### Field: Vapor Diffusion Resistance Factor #x

The Liquid Transport Coefficient of the x^th^ pair.

Below are some examples of the values for materials.

~~~~~~~~~~~~~~~~~~~~

    MaterialProperty:HeatAndMoistureTransfer:Diffusion,
          Plywood,     !- Name
          3,     !- Number of data Points
          0,     !- Relative Humidity Fraction 1
          700,     !- Water Vapor Diffusion Resistance Factor 1
          0.5,     !- Relative Humidity Fraction 2
          200,     !- Water Vapor Diffusion Resistance Factor 2
          1,     !- Relative Humidity Fraction 3
          20;     !- Water Vapor Diffusion Resistance Factor 3
    MaterialProperty:HeatAndMoistureTransfer:Diffusion,
          Concrete,     !- Name
          1,     !- Number of Mu Points
          0,     !- Relative Humidity Fraction 1
          180;     !- Water Vapor Diffusion Resistance Factor 1
~~~~~~~~~~~~~~~~~~~~

## MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity

**Advanced/Research Usage:** This material property is used in conjunction with the CombinedHeatAndMoistureFiniteElement heat balance solution algorithm.

The thermal data relates the thermal conductivity [W/m-K] of a material to the moisture or water content [kg/m3]. A data point at zero water content is required. The thermal conductivity at the highest entered water content value is used for all thermal conductivity values above this water content. If this object is not defined for a material then the algorithm will use a constant value entered in the [Material](#material-and-material-properties) object for all water contents.

### Inputs

#### Field: Material Name

This field is a unique reference name that the user assigns to a particular material. This name can then be referred to by other input data.

#### Field: Number of Thermal Coordinates

A maximum of 25 coordinates can be specified.

#### Field Set: Moisture Content- Thermal Conductivity

#### Field: Moisture Content x

The moisture content of the x^th^ coordinate. The units are [kg/m3]

#### Field: Thermal Conductivity x

The Thermal Conductivity of the x^th^ coordinate. The units are [W/m-K]

Below is an example of values for a material.

~~~~~~~~~~~~~~~~~~~~

    MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity
          Concrete,     !- Name
          2,     !- Number of Thermal Coordinates
          0,     !- Moisture content #1
          1.6,     !- Thermal Conductivity #1
          180,     !- Moisture content #2
          2.602;     !- Thermal Conductivity #2
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,HAMT Surface Average Water Content Ratio [kg/kg]
    Zone,Average,HAMT Surface Inside Face Temperature [C]
    Zone,Average,HAMT Surface Inside Face Relative Humidity [%]
    Zone,Average,HAMT Surface Inside Face Vapor Pressure [Pa]
    Zone,Average,HAMT Surface Outside Face Temperature [C]
    Zone,Average,HAMT Surface Outside Face Relative Humidity [%]
    Zone,Average,HAMT Surface Inside Face Relative Humidity [%]
~~~~~~~~~~~~~~~~~~~~

#### HAMT Surface Average Water Content Ratio [kg/kg]

This output is the summed water content [kg/kg] of all cells in a surface expressed as a fraction of the mass of the water to the material mass.

#### HAMT Surface Inside Face Temperature [C]

This output is the temperature [C] on the internal "surface" of the surface.

#### HAMT Surface Inside Face Relative Humidity [%]

#### HAMT Surface Inside Face Relative Humidity [%]

This output is the relative humidity on the internal "surface" of the surface expressed as a percentage.

#### HAMT Surface Inside Face Vapor Pressure [Pa]

This output is the vapor pressure [Pa] on the internal "surface" of the surface.

#### HAMT Surface Outside Face Temperature [C]

This output is the temperature on the external "surface" of the surface.

#### HAMT Surface Outside Face Relative Humidity [%]

This output is the relative humidity on the external "surface" of the surface.

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,HAMT Surface Temperature Cell N [C]
    Zone,Average,HAMT Surface Water Content Cell N [kg/kg]
    Zone,Average,HAMT Surface Relative Humidity Cell N [%]
~~~~~~~~~~~~~~~~~~~~

Detailed profile data for the variables Temperature [C], Relative Humidity [%] and Water Content [kg/kg] within each surface can also be reported. To calculate the heat and moisture transfer through surfaces HAMT splits up surfaces into discrete cells. Each cell is composed of a single material and has a position within the surface. HAMT automatically assigns cells to construction objects so that there are more cells closer to boundaries between materials and also at the "surfaces" of the surface. It is not possible for users to define their own cells.

#### HAMT Surface Relative Humidity Cell <N> [%]

This is the relative humidity of the cell in the surface.

#### HAMT Surface Temperature Cell <N> [C]

This is the temperature of the cell in the surface.

#### HAMT Surface Water Content Cell <N> [kg/kg]

This is the relative water content of the cell in the surface.

Each surface is made from a particular construction. The construction-surface relationship is output by HAMT to the eplusout.eio file with the following format.

! <HAMT cells>, Surface Name, [Construction](#construction) Name, Cell Numbers

! <HAMT origins>, Surface Name, [Construction](#construction) Name, Cell origins (m)

The output also contains the HAMT cell origins and cell number for each construction – surface combination. The coordinate system origin is defined as the exterior surface of the construction. Users can select any one of the Temperature, Relative Humidity or Water Content variables for any cell to be reported, using the following naming scheme for the output variable.

~~~~~~~~~~~~~~~~~~~~

    HAMT Profile Construction <Variable> Cell <Cell#>
~~~~~~~~~~~~~~~~~~~~

It is better to specify the "key" or Surface Name in this output.

So for example to output the temperature of the 10^th^ cell in a surface, eg "East Wall" would require the following output variable.

~~~~~~~~~~~~~~~~~~~~

    Output:Variable,
        East Wall,               !- Key Value
        HAMT Profile Construction Temperature Cell 10,  !- Variable Name
        Hourly;                  !- Reporting Frequency
~~~~~~~~~~~~~~~~~~~~

By selecting a whole range of these reports and using the information in the eplusout.eio file it is possible to build up a temperature profile of the surface.

## Materials for Glass Windows and Doors

All the materials for glass windows and doors have the prefix "WindowMaterial". The following  WindowMaterial descriptions (Glazing, Glazing:RefractionExtinctionMethod, Gas, GasMixture, Shade, Screen and Blind) apply to glass windows and doors. The property definitions described herein for Glazing, Gas and GasMixture are supported by the National Fenestration Rating Council as standard.

"Front side" is the side of the layer opposite the zone in which the window is defined. "Back side" is the side closest to the zone in which the window is defined. Therefore, for exterior windows, "front side" is the side closest to the outdoors. For interior windows, "front side" is the side closest to the zone adjacent to the zone in which the window is defined.

The solar radiation transmitted by the window layers enters the zone and is a component of the zone load. The solar radiation absorbed in each solid layer (glass, shade, screen or blind) participates in the window layer heat balance calculation. The visible transmittance and reflectance properties of the window are used in the daylighting calculation.

## WindowMaterial:Glazing

In the following, for exterior windows, "front side" is the side of the glass closest to the outside air and "back side" is the side closest to the zone the window is defined in. For interzone windows, "front side" is the side closest to the zone adjacent to the zone the window is defined in and "back side" is the side closest to the zone the window is defined in.

### Inputs

#### Field: Name

The name of the glass layer. It corresponds to a layer in a window construction.

#### Field: Optical Data Type

Valid values for this field are SpectralAverage, Spectral, BSDF.

If Optical Data Type = SpectralAverage, the values you enter for solar transmittance and reflectance are assumed to be averaged over the solar spectrum, and the values you enter for visible transmittance and reflectance are assumed to be averaged over the solar spectrum and weighted by the response of  the human eye. There is an EnergyPlus Reference Data Set for [WindowMaterial:Glazing](#windowmaterialglazing) that contains spectral average properties for many different types of glass.

If Optical Data Type = Spectral, then, in the following field, you must enter the name of a spectral data set defined with the WindowGlassSpectralData object. In this case, the values of  solar and visible transmittance and reflectance in the fields below should be blank.

If Optical Data Type = BSDF, the [Construction:ComplexFenestrationState](#constructioncomplexfenestrationstate) object must be used to define the window construction layers. The [Construction:ComplexFenestrationState](#constructioncomplexfenestrationstate) object contains references to the BSDF files which contain the optical properties of the Complex Fenestration layers. In this case,

#### Field: Window Glass Spectral Data Set Name

If Optical Data Type = Spectral, this is the name of a spectral data set defined with a WindowGlassSpectralData object.

#### Field: Thickness

The surface-to-surface thickness of the glass (m).

#### Field: Solar Transmittance at Normal Incidence

Transmittance at normal incidence averaged over the solar spectrum. Used only when Optical Data Type = SpectralAverage.

For uncoated glass, when alternative optical properties are available—such as thickness, solar index of refraction, and solar extinction coefficient—they can be converted to equivalent solar transmittance and reflectance values using the equations given in "Conversion from Alternative Specification of Glass Optical Properties.")

#### Field: Front Side Solar Reflectance at Normal Incidence

Front-side reflectance at normal incidence averaged over the solar spectrum. Used only when Optical Data Type = SpectralAverage.

#### Field: Back Side Solar Reflectance at Normal Incidence

Back-side reflectance at normal incidence averaged over the solar spectrum. Used only when Optical Data Type = SpectralAverage.

#### Field: Visible Transmittance at Normal Incidence

Transmittance at normal incidence averaged over the solar spectrum and weighted by the response of the human eye. Used only when Optical Data Type = SpectralAverage.

For uncoated glass, when alternative optical properties are available—such as thickness, visible index of refraction, and visible extinction coefficient—they can be converted to equivalent visible transmittance and reflectance values using the equations given in "Conversion from Alternative Specification of Glass Optical Properties.")

#### Field: Front Side Visible Reflectance at Normal Incidence

Front-side reflectance at normal incidence averaged over the solar spectrum and weighted by the response of the human eye. Used only when Optical Data Type = SpectralAverage.

#### Field: Back Side Visible Reflectance at Normal Incidence

Back-side reflectance at normal incidence averaged over the solar spectrum and weighted by the response of the human eye. Used only when Optical Data Type = SpectralAverage.

#### Field: Infrared Transmittance at Normal Incidence

Long-wave transmittance at normal incidence.

#### Field: Front Side Infrared Hemispherical Emissivity

Front-side long-wave emissivity.

#### Field: Back Side Infrared Hemispherical Emissivity

Back-side long-wave emissivity.

#### Field: Conductivity

Thermal conductivity (W/m-K).

#### Field: Dirt Correction Factor for Solar and Visible Transmittance

This is a factor that corrects for the presence of dirt on the glass. The program multiplies the fields "Solar Transmittance at Normal Incidence" and "Visible Transmittance at Normal Incidence" by this factor if the material is used as the outer glass layer of an exterior window or glass door. If the material is used as an inner glass layer (in double glazing, for example), the dirt correction factor is not applied because inner glass layers are assumed to be clean. Using a material with dirt correction factor < 1.0 in the construction for an interior window will result in an error message.

Representative values of the dirt correction factor are shown in Table 7.

     Table 7. Dirt Correction Factors

Type of Location|Angle of Glazing
----------------|----------------
|Vertical|45^O^|Horizontal
Non-industrial|0.9|0.8|0.7
Industrial|0.7|0.6|0.5
Very Dirty|0.6|0.5|0.4

From Appendix A, "Daylighting in Sports Halls, Report 2," SportScotland, Nov. 2002(www.sportscotland.org.uk)

The default value of the dirt correction factor is 1.0, which means the glass is clean.

It is assumed that dirt, if present, has no effect on the IR properties of the glass.

#### Field: Solar Diffusing

Takes values No (the default) and Yes. If No, the glass is transparent and beam solar radiation incident on the glass is transmitted as beam radiation with no diffuse component. If Yes, the glass is translucent and beam solar radiation incident on the glass is transmitted as hemispherically diffuse radiation with no beam component. See Figure 10. Solar Diffusing = Yes should only be used on the *innermost* pane of glass in an exterior window; it does not apply to interior windows.

For both Solar Diffusing = No and Yes, beam is reflected as beam with no diffuse component (see Figure 10). Solar Diffusing cannot be used with [Window](#window) Shading Control devices (except Switchable Glazing). When attempted, the window property will be set to No for Solar Diffusing. The Surface Details report will reflect the override.

If, in the [Building](#building) object, Solar Distribution = FullInteriorAndExterior, use of Solar Diffusing = Yes for glass in an exterior window will change the distribution of interior solar radiation from the window. The result is that beam solar radiation that would be transmitted by a transparent window and get absorbed by particular interior surfaces will be diffused by a translucent window and be spread over more interior surfaces. This can change the time dependence of heating and cooling loads in the zone.

In a zone with Daylighting:Detailed, translucent glazing---which is often used in skylights---will provide a more uniform daylight illuminance over the zone and will avoid patches of sunlight on the floor.

![Comparison between transmittance properties of transparent glass (Solar Diffusing = No) and translucent glass (Solar Diffusing = Yes).](media/comparison-between-transmittance-properties.png)


#### Field: Young's modulus

A measure of the stiffness of an elastic material.  It is defined as the ratio of the unaxial stress over the uniaxial strain in the range of stress in which Hooke's Law holds. It is used only with complex fenestration systems defined through the [Construction:ComplexFenestrationState](#constructioncomplexfenestrationstate) object. The default value for glass is 7.2e10 Pa.

#### Field: Poisson's ratio

The ratio, when a sample object is stretched, of the contraction or transverse strain (perpendicular to the applied load), to the extension or axial strain (in the direction of the applied load). This value is used only with complex fenestration systems defined through the [Construction:ComplexFenestrationState](#constructioncomplexfenestrationstate) object. The default value for glass is 0.22.

IDF examples of Spectral average and using a Spectral data set:

~~~~~~~~~~~~~~~~~~~~

    MATERIAL:WINDOWGLASS, GLASS - CLEAR SHEET 1 / 8 IN,
      SpectralAverage, ! Optical data type
      0.003, ! Thickness {m} 1/8"
      0.850, ! Solar transmittance at normal incidence
      0.075, ! Solar reflectance at normal incidence: front side
      0.075, ! Solar reflectance at normal incidence: back side
      0.901, ! Visible transmittance at normal incidence
      0.081, ! Visible reflectance at normal incidence: front side
      0.081, ! Visible reflectance at normal incidence: back side
      0.0,   ! IR transmittance at normal incidence
      0.84,  ! IR hemispherical emissivity: front side
      0.84,  ! IR hemispherical emissivity: back side
      0.9;   ! Conductivity {W/m-K}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Glazing ,SPECTRAL GLASS INNER PANE, ! Material name
        Spectral, ! Optical data type {SpectralAverage or Spectral}
        TestSpectralDataSet, ! Name of spectral data set
        0.0099, ! Thickness {m}
        ,  ! Solar transmittance at normal incidence
        ,  ! Solar reflectance at normal incidence: front side
        ,  ! Solar reflectance at normal incidence: back side
        ,  ! Visible transmittance at normal incidence
        ,  ! Visible reflectance at normal incidence: front side
        ,  ! Visible reflectance at normal incidence: back side
        0.0,   ! IR transmittance at normal incidence
        0.84,  ! IR emissivity: front side
        0.84,  ! IR emissivity: back side
        0.798; ! Conductivity {W/m-K}
~~~~~~~~~~~~~~~~~~~~

IDF example of Spectral Data Type = BSDF

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Glazing,
      Glass_5012_Layer,        !- Layer name : CLEAR_6.PPG
      BSDF,                    !- Optical Data Type
      ,                        !- Spectral Data name
      0.005664,                !- Thickness
      ,                        !- Solar Transmittance
      ,                        !- Solar Front Reflectance
      ,                        !- Solar Back Reflectance
      ,                        !- Visible Transmittance
      ,                        !- Visible Front Reflectance
      ,                        !- Visible Back reflectance
      0.000000,                !- IR Transmittance
      0.840000,                !-Front Emissivity
      0.840000,                !-Back Emissivity
      1.000000,                !-Conductivity
      ,                        !-Dirt Correction Factor for Sol/Vis Transmittance
      ,                        !-Solar Diffusing
      7.2e10,                  !-Young's modulus
      0.22;                    !-Poisson's ratio
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Glazing:RefractionExtinctionMethod

This is an alternative way of specifying glass properties. Index of refraction and extinction coefficient are given instead of the transmittance and reflectance values used in [WindowMaterial:Glazing](#windowmaterialglazing). However, unlike [WindowMaterial:Glazing](#windowmaterialglazing), [WindowMaterial:Glazing:RefractionExtinctionMethod](#windowmaterialglazingrefractionextinctionmethod) is restricted to cases where the front and back optical properties of the glass are the same. This means it cannot be used for glass with a coating on one side. In that case [WindowMaterial:Glazing](#windowmaterialglazing) should be used. Also, unlike [WindowMaterial:Glazing](#windowmaterialglazing), [WindowMaterial:Glazing:RefractionExtinctionMethod](#windowmaterialglazingrefractionextinctionmethod) does not allow input of glass wavelength-by-wavelength (spectral) properties.

### Inputs

#### Field: Name

The name of the glass layer. It corresponds to a layer in a window construction.

#### Field: Thickness

The surface-to-surface thickness of the glass (m).

#### Field: Solar Index of Refraction

Index of refraction averaged over the solar spectrum.

#### Field: Solar Extinction Coefficient

Extinction coefficient averaged over the solar spectrum (m^-1^).

#### Field: Visible Index of Refraction

Index of refraction averaged over the solar spectrum and weighted by the response of the human eye.

#### Field: Visible Extinction Coefficient

Extinction coefficient averaged over the solar spectrum and weighted by the response of the human eye (m^-1^).

#### Field: Infrared Transmittance at Normal Incidence

Long-wave transmittance at normal incidence.

#### Field: Infrared Hemispherical Emissivity

Long-wave hemispherical emissivity, assumed the same on both sides of the glass.

#### Field: Conductivity

Thermal conductivity (W/m-K).

#### Field: Dirt Correction Factor for Solar and Visible Transmittance

This is a factor that corrects for the presence of dirt on the glass. It multiplies the solar and visible transmittance at normal Incidence (which the program calculates from the input values of thickness, solar index of refraction, solar extinction coefficient, etc.) if the material is used as the outer glass layer of an exterior window or glass door. If the material is used as an inner glass layer (in double glazing, for example), the dirt correction factor is not applied because inner glass layers are assumed to be clean. Using a material with dirt correction factor < 1.0 in the construction for an interior window will result in an error message.

Representative values of the direct correction factor are shown in Table 7.

The default value of the dirt correction factor is 1.0, which means the glass is clean. It is assumed that dirt, if present, has no effect on the IR properties of the glass.

#### Field: Solar Diffusing

Takes values No (the default) and Yes. If No, the glass is transparent. If Yes, the glass is translucent and beam solar radiation incident on the glass is transmitted as hemispherically diffuse radiation with no beam component. Solar Diffusing = Yes should only be used on the innermost pane of glass in an exterior window; it does not apply to interior windows.

If, in the [Building](#building) object, Solar Distribution = FullInteriorAndExterior, use of Solar Diffusing = Yes for glass in an exterior window will change the distribution of interior solar radiation from the window. The result is that beam solar radiation that would be transmitted by a transparent window and get absorbed by particular interior surfaces will be diffused by a translucent window and be spread over more interior surfaces. This can change the time dependence of heating and cooling loads in the zone.

In a zone with Daylighting:Detailed, translucent glazing, which is often used in skylights, will provide a more uniform daylight illuminance over the zone and will avoid patches of sunlight on the floor.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Glazing:RefractionExtinctionMethod,
    4MM CLEAR GLASS, !- Material name
        0.004, !- Thickness {m}
        1.526, !- Solar index of refraction
        30.0 , !- Solar extinction coefficient (1/m)
        1.526, !- Visible index of refraction
        30.0 , !- Visible extinction coefficient (1/m)
        0.0,   !- IR transmittance at normal incidence
        0.84,  !- IR emissivity
        0.9;   !- Conductivity {W/m-K}
~~~~~~~~~~~~~~~~~~~~

## Glass Optical Properties Conversion

### Conversion from Glass Optical Properties Specified as Index of Refraction and Transmittance at Normal Incidence

The optical properties of uncoated glass are sometimes specified by index of refraction, *n*,*and transmittance at normal incidence, T*.

The following equations show how to convert from this set of values to the transmittance and reflectance values required by [WindowMaterial:Glazing](#windowmaterialglazing). These equations apply only to uncoated glass, and can be used to convert either spectral-average solar properties or spectral-average visible properties (in general, *n* and *T* are different for the solar and visible). Note that since the glass is uncoated, the front and back reflectances are the same and equal to the *R* that is solved for in the following equations.

Given *n* and *T*, find *R*:

![](media/image26.png)\


**Example:**

*T* = 0.86156

*n* = 1.526

![](media/image27.png)\


## WindowMaterial:GlazingGroup:Thermochromic

Thermochromic (TC) materials have active, reversible optical properties that vary with temperature. Thermochromic windows are adaptive window systems for incorporation into building envelopes. Thermochromic windows respond by absorbing sunlight and turning the sunlight energy into heat. As the thermochromic film warms it changes its light transmission level from less absorbing to more absorbing. The more sunlight it absorbs the lower the light level going through it. By using the suns own energy the window adapts based solely on the directness and amount of sunlight. Thermochromic materials will normally reduce optical transparency by absorption and/or reflection, and are specular (maintaining vision).

A thermochromic window is defined with a [Construction](#construction) object which references a special layer defined with a [WindowMaterial:GlazingGroup:Thermochromic](#windowmaterialglazinggroupthermochromic) object. The [WindowMaterial:GlazingGroup:Thermochromic](#windowmaterialglazinggroupthermochromic) object further references a series of [WindowMaterial:Glazing](#windowmaterialglazing) objects corresponding to each specification temperature of the TC layer.

This object specifies a layer of thermochromic glass, part of a thermochromic window. An example file ThermochromicWindow.idf is included in the EnergyPlus installation.

### Inputs

#### Field: Name

A unique user assigned name for a particular thermochromic glass material.

#### Field Set (Optical Data Temperature, Window Material Glazing Name) is extensible.

#### Field: Optical Data Temperature <N>

The temperature of the TC glass layer corresponding to the optical data of the TC layer. Unit is °C.

#### Field: Window Material Glazing Name <N>

The window glazing (defined with [WindowMaterial:Glazing](#windowmaterialglazing)) name that provides the TC glass layer performance at the above specified temperature.

IDF Examples

~~~~~~~~~~~~~~~~~~~~

      Construction,
        window_const,             !- Name
        Usual Glass,              !- Layer 1
        AIR 6MM,                  !- Layer 2
        TCGlazings,               !- Layer 3
        AIR 6MM,                  !- Layer 4
        Usual Glass;              !- Layer 5

      WindowMaterial:Gas,
        AIR 6MM,                 !- Name
        Air,                     !- Gas Type
        0.0063;                  !- Thickness {m}

    ! Added for thermochromic glazings
      WindowMaterial:GlazingGroup:Thermochromic,
        TCGlazings,
        0 ,  TCGlazing0,
        20,  TCGlazing20,
        25,  TCGlazing25,
        30,  TCGlazing30,
        35,  TCGlazing35,
        40,  TCGlazing40,
        45,  TCGlazing45,
        50,  TCGlazing50,
        55,  TCGlazing55,
        60,  TCGlazing60,
        65,  TCGlazing65,
        75,  TCGlazing75,
        85,  TCGlazing85;

    WindowMaterial:Glazing,
      TCGlazing0,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing20,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing25,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing30,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing35,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing40,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing45,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing50,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing55,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing60,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing65,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing75,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

    WindowMaterial:Glazing,
      TCGlazing85,  !- Name
      SpectralAverage,  !- Optical Data Type
      ,        !- Window Glass Spectral Data Set Name
      0.0030,  !- Thickness
      0.2442,  !- Solar Transmittance at Normal Incidence
      0.7058,  !- Front Side Solar Reflectance at Normal Incidence
      0.7058,  !- Back Side Solar Reflectance at Normal Incidence
      0.3192,  !- Visible Transmittance at Normal Incidence
      0.6308,  !- Front Side Visible Reflectance at Normal Incidence
      0.6308,  !- Back Side Visible Reflectance at Normal Incidence
      0.0000,  !- Infrared Transmittance at Normal Incidence
      0.9000,  !- Front Side Infrared Hemispherical Emissivity
      0.9000,  !- Back Side Infrared Hemispherical Emissivity
      0.0199,  !- Conductivity
      1.0000,  !- Dirt Correction Factor for Solar and Visible Transmittance
      No;      !- Solar Diffusing

~~~~~~~~~~~~~~~~~~~~

### Outputs

#### Surface Window Thermochromic Layer Temperature [C]

The temperature of the TC glass layer of a TC window at each time step.

#### Surface Window Thermochromic Layer Property Specification Temperature [C]

The temperature under which the optical data of the TC glass layer are specified.

The overall properties (U-factor/SHGC/VT) of the thermochromic windows at different specification temperatures are reported in the EIO file. These window constructions are created by EnergyPlus during run time. They have similar names with suffix "_TC_XX" where XX represents a specification temperature.

## WindowMaterial:Gas

This object specifies the properties of the gas between the panes of a multi-pane window. Gas Type = Custom allows you to specify the properties of gases other than air, Argon, Krypton or Xenon. There is an EnergyPlus Reference Data Set for Material:WindowGas that contains several types of gas of different thicknesses. See Material:WindowGasMixture for the case that the gas fill is a mixture of different gases.

### Inputs

#### Field: Name

The name of the gas fill. It refers to a layer in a window construction.

#### Field: Gas Type

The type of gas. The choices are Air, Argon, Krypton, or Xenon. If Gas Type = Custom you can use Conductivity Coefficient A, etc. to specify the properties of a different type of gas.

#### Field: Thickness

The thickness (m) of the gas layer.

#### Properties for Custom Gas Types

The following entries are used only if Gas Type = Custom. The A and B coefficients are those in the following expression that gives a property value as a function of temperature in degrees K:

![](media/image28.png)\


#### Field: Conductivity Coefficient A

The A coefficient for gas conductivity (W/m-K). Used only if Gas Type = Custom.

#### Field: Conductivity Coefficient B

The B coefficient for gas conductivity (W/m-K^2^). Used only if Gas Type = Custom.

#### Field: Conductivity Coefficient C

The C coefficient for gas conductivity (W/m-K^3^).  Used only if Gas Type = Custom.

#### Field: Viscosity Coefficient A

The A coefficient for gas viscosity (kg/m-s). Used only if Gas Type = Custom.

#### Field: Viscosity Coefficient B

The B coefficient for gas viscosity (kg/m-s-K). Used only if Gas Type = Custom.

#### Field: Viscosity Coefficient C

The C coefficient for gas viscosity (kg/m-s-K^2^).  Used only if Gas Type = Custom.

#### Field: Specific Heat Coefficient A

The A coefficient for gas specific heat (J/kg-K). Used only if Gas Type = Custom.

#### Field: Specific Heat Coefficient B

The B coefficient for gas specific heat (J/kg-K^2^). Used only if Gas Type = Custom.

#### Field: Specific Heat Coefficient C

The C coefficient for gas specific heat (J/kg-K^3^).  Used only if Gas Type = Custom.

#### Field: Specific Heat Ratio

The specific heat ratio for gas.  Used only if Gas Type = Custom.

#### Field: Molecular Weight

The molecular weight for gas.  The molecular weight is the mass of 1 mol of the substance.  This has a numerical value which is the average molecular mass of the molecules in the substance multiplied by Avogadro's constant. (kg/kmol) (Shown in the IDD as g/mol for consistency)

#### Field: Specific Heat Ratio

The specific heat ratio for gas.  The specific heat ratio of a gas is the ratio of the specific heat at contant pressure, to the specific heat at constant volume.  Used only if Gas Type = Custom.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Gas,AIRGAP,
      AIR,      ! Gas type (Air - Argon - Krypton - Xenon - Custom)]
      0.0125;   ! Thickness {m} 1/2 inch
~~~~~~~~~~~~~~~~~~~~

An IDF example to be used with a [WindowMaterial:Gap](#windowmaterialgap) definition (see below)

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Gas,
    Gas_1_W_0_0100,                                     !- gap name - Air
    Air,                                                !- type
    0.0100;                                             !- thickness
~~~~~~~~~~~~~~~~~~~~

An IDF example for a Custom Gas

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Gas,
    Gas_16_W_0_0003,                   !- gap name
    Custom,                            !- type
    0.0003,                            !- thickness
    2.873000e-003,                     !- Conductivity Coefficient A
    7.760000e-005,                     !- Conductivity Coefficient B
    0.000000e+000,                     !- Conductivity Coefficient C
    3.723000e-006,                     !- Conductivity Viscosity A
    4.940000e-008,                     !- Conductivity Viscosity B
    0.000000e+000,                     !- Conductivity Viscosity C
    1002.737000,                       !- Specific Heat Coefficient A
    0.012324,                          !- Specific Heat Coefficient B
    0.000000,                          !- Specific Heat Coefficient C
    28.969999,                         !- Molecular Weight
    1.400000;                          !- Specific Heat Ratio
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:GasMixture

This object allows you to specify the fill between the panes of a multi-pane window to be a mixture of two, three or four different gases chosen from air, argon, krypton and xenon. It can also be used if only one type of gas in the fill. In this case you can also use [WindowMaterial:Gas](#windowmaterialgas). Note that the fractions of gas types in the mixture should add up to 1.0.

### Inputs

#### Field: Name

The name of the gas mixture. It refers to a layer in a window construction.

#### Field: Thickness

The thickness (m) of the gas mixture layer.

#### Field: Number of Gases in Mixture

The number of different types of gas in the mixture ( a value from 1 to 4)

#### Set: Gas Type-Fraction (up to 4)

#### Field: Gas 1 Type

The type of  the first gas in the mixture. Choices are Air, Argon, Krypton and Xenon.

#### Field: Gas 1 Fraction

The fraction of the first gas in the mixture.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:GasMixture,ArgonKryptonMix,
    0.0125,   ! Thickness {m} 1/2 inch
    2,        ! Number of Gases in Mixture
    Argon,    ! Gas 1 Type
    0.6,      ! Gas 1 Fraction
    Krypton,  ! Gas 2 Type
    0.4;      ! Gas 2 Fraction
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Gap

This input object is used to define the gap between two layers in a complex fenestration system, where the [Construction:ComplexFenestrationState](#constructioncomplexfenestrationstate) object is used. It references the gas or gas mixtures defined in the [WindowMaterial:Gas](#windowmaterialgas) and [WindowMaterial:GasMixture](#windowmaterialgasmixture) objects. It is referenced as a layer in the [Construction:ComplexFenestrationState](#constructioncomplexfenestrationstate) object ;it cannot be referenced as a layer from the [Construction](#construction) object.

### Inputs

#### Field: Name

Unique name of the gap.

#### Field: Thickness

The thickness (m) of the gap layer.

#### Field: Pressure

The pressure (Pa) of the gas in the gap layer, used to calculate the gas properties of the glazing system gap. Default is atmospheric pressure, 101325 Pa.When modeling vacuum glazing, this value should represent the pressure in the evacuated glazing system. If this pressure is less that the ThermalModelParams:PressureLimit value, the the glazing system will be modeled as a vacuum glazing.

#### Field: Deflection State

This field is used when modeling the deflection of the glass layers in a window if the WinodowThermalModel:Params value for "deflection model" is "MeasuredDeflection".

#### Field: Support Pillar

References the support pillar of the gap layer if vacuum glazing is being modeled.  If left empty, then it is considered that gap layer does not have support pillars.

#### Field: Gas (or GasMixture)

References gas ([WindowMaterial:Gas](#windowmaterialgas)) or gas mixture ([WindowMaterial:GasMixture](#windowmaterialgasmixture)) of the gap layer.

An IDF example for simple glazing:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Gas,
    Gas_1_W_0_0120,                                !- gap name - Air
    Air,                                           !- type
    0.0120;                                        !- thickness

    WindowMaterial:Gap,
    Gap_1_Layer,                                   !- gap name: Air
    0.0120,                                        !- thickness
    Gas_1_W_0_0120,                                !- Gas (or Gas Mixture) name
    101325.0000;                                   !- pressure
~~~~~~~~~~~~~~~~~~~~

An IDF example for vacuum glazing:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Gap,
    Gap_16_Layer,                     !- gap name: Vacuum_0.001_pr-0.5_ps-50.8
    0.0003,                                             !- thicknessGas_16_W_0_0003,                                    !- Gas (or Gas Mixture) name
    0.1333,                                             !- pressure
    ,                                                   !- deflection state
    SupportPillar_16_Gap_1;                             !- SupportPillar

    WindowGap:SupportPillar,
    SupportPillar_16_Gap_1,                             !- Name
    0.0508,                                             !- spacing
    0.0005;                                             !- radius
~~~~~~~~~~~~~~~~~~~~

## WindowGap:DeflectionState

This input object is used to enter data describing deflection state of the gap.  It is referenced from [WindowMaterial:Gap](#windowmaterialgap) object only and it is used only when deflection model is set to MeasuredDeflection (see [WindowThermalModel:Params](#windowthermalmodelparams)), otherwise it is ignored.

### Inputs

#### Field: Name

Unique name of the deflection state.

#### Field: Deflected Thickness

The thickness (m) of the gap in deflected state.  It represents value of deflection at point of maximum which is usualy at the center poing of glazing system.  It is used only with tarcog algorithm set to Measured Deflection ([WindowThermalModel:Params](#windowthermalmodelparams)), otherwise this field will be ignored.

An IDF example where [WindowThermalModel:Params](#windowthermalmodelparams) Deflection Model = MeasuredDeflection:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Gap,
    Gap_1_Layer,                                  !- gap name: Air
    0.0120,                                       !- thickness
    Gas_1_W_0_0120,                               !- Gas (or Gas Mixture) name
    101325.0000,                                  !- pressure
    Gap_1_Deflection;                      !- deflection state

    WindowGap:DeflectionState,       !- deflection state of gap
      Gap_1_Deflection,              !- name
      0.011;                         !- gap thickness in deflected state
~~~~~~~~~~~~~~~~~~~~

## WindowGap:SupportPillar

This input object is used to enter data describing support pillar of the gap.  Support pillars are used in vacuum glazing in order to prevent deflection of glass layers.

![Support Pillar](media/support-pillar.png)


### Inputs

#### Field: Name

Unique name of the support pillar.

#### Field: Spacing

Distance (m) between support pillar centers (see the Engineering reference document for more information).

#### Field: Radius

The radius (m) of the support pillar (see Engineering reference document for more information).

An IDF example for vacuum glazing (see Vacuum Glazing example in [WindowMaterial:Gap](#windowmaterialgap) above)

~~~~~~~~~~~~~~~~~~~~

    WindowGap:SupportPillar,       !- gap support pillar
      SupportPillar_16_Gap_1,      !- basis matrix name
      0.05,                        !- pillar spacing
      0.005;                       !- pillar radius
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:SimpleGlazingSystem

This model should be used with caution. There may be significant differences in performance between the simple window system and the usual more detailed model.

This input object differs from the other WindowMaterial objects in that it describes an entire glazing system rather than individual layers. This object is used when only very limited information is available on the glazing layers or when specific performance levels are being targeted. The layer by layer description offers superior method of modeling windows that should be used instead of this object when sufficient data are available. This object accesses a model that turns simple performance indices into a fuller model of the glazing system.

The performance indices are U-factor and Solar Heat Gain Coefficient, and optionally Visible Transmittance. The values for these performance indices can be selected by the user to represent either glazing-only windows (with no frame) or an average window performance that includes the frame. Inside the program the model produces an equivalent window glazing layer with no frame. The properties of the modeled glazing layer are reported to the EIO file using the IDF input object syntax for the [WindowMaterial:Glazing](#windowmaterialglazing) input object. This equivalent layer could be reused in subsequent models if desired, however there will be important differences in the modeled window performance because the simple glazing system model includes its own special model for angular dependence when incident beam solar is not normal to the plane of the window.

When this object is referenced in a [Construction](#construction) object, it cannot be used with other glazing or gas material layers. Shades or blinds cannot be located between the glass, but these can be used on the inside or the outside of the glazing system.   If the glazing system does have between-the-glass shades or blinds, then the U and SHGC values entered in this object should include the impacts of those layers. Adding window treatment layers such as shades or screens will alter the overall performance to be different than the performance levels prescribed in this object.

### Inputs

#### Field: Name

The name of the glazing system.  This value is unique across all constructions.

#### Field: U-Factor

This field describes the value for window system U-Factor, or overall heat transfer coefficient. Units are in W/m^2^·K. This is the rated (NFRC) value for U-factor under winter heating conditions. The U-factor is assumed to be for vertically mounted products. Although the maximum allowable input is U-7.0 W/m^2^·K, the effective upper limit of the glazings generated by the underlying model is around U-5.8 W/m^2^·K.

#### Field: Solar Heat Gain Coefficient

This field describes the value for SHGC, or solar heat gain coefficient. There are no units. This is the rated (NFRC) value for SHGC under summer cooling conditions and represents SHGC for normal incidence and vertical orientation.

#### Field: Visible Transmittance

This field is optional. If it is omitted, then the visible transmittance properties are taken from the solar properties. If it is included then the model includes it when developing properties for the glazing system. This is the rated (NFRC) value for visible transmittance at normal incidence.

An example of this object.

~~~~~~~~~~~~~~~~~~~~

      WindowMaterial:SimpleGlazingSystem,
        SimpleWindow:DOUBLE PANE WINDOW , !- Name
        2.716 , !-  U-Factor
        0.763 , !-  Solar Heat Gain Coefficient
        0.812 ; !-  Visible Transmittance
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Shade

This object specifies the properties of window shade materials. Reflectance and emissivity properties are assumed to be the same on both sides of the shade. Shades are considered to be perfect diffusers (all transmitted and reflected radiation is hemispherically-diffuse) with transmittance and reflectance independent of angle of incidence. There is an EnergyPlus Reference Data Set for [WindowMaterial:Shade](#windowmaterialshade) that contains properties of generic window shades.

[Window](#window) shades can be on the inside of the window ("interior shades"), on the outside of the window ("exterior shades"), or between glass layers ("between-glass shades"). When in place, the shade is assumed to cover all of the glazed part of the window, including dividers; it does not cover any of the window frame, if present. The plane of the shade is assumed to be parallel to the glazing.

[WindowMaterial:Shade](#windowmaterialshade) can be used for diffusing materials such as drapery and translucent roller shades. For slat-type shading devices, like Venetian blinds, that have a strong angular dependence of transmission, absorption and reflection, it is better to use [WindowMaterial:Blind](#windowmaterialblind). [WindowMaterial:Screen](#windowmaterialscreen) should be used to model wire mesh insect screens where the solar and visible transmission and reflection properties vary with the angle of incidence of solar radiation.

Transmittance and reflectance values for drapery material with different color and openness of weave can be obtained from manufacturers or determined from 2001 ASHRAE Fundamentals, Chapter 30, Fig. 31.

There are two methods of assigning a shade to a window:

### Inputs

#### Method 1:

#. Define the construction of the window without the shade, the so-called "bare" construction.
#. Reference the bare construction in the [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) for the window.
#. Define the [WindowMaterial:Shade](#windowmaterialshade).
#. Define a [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) for the window in which you (a) specify that this [WindowMaterial:Shade](#windowmaterialshade) is the window's shading device and (b) specify how the shade is controlled.

#### Method 2:

#. Define the [Construction](#construction) of the window without the shade, the so-called "bare" construction.
#. Reference the bare construction in the [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) for the window.
#. Define the [WindowMaterial:Shade](#windowmaterialshade).
#. Define another [Construction](#construction), called the "shaded construction," that includes the [WindowMaterial:Shade](#windowmaterialshade).
#. Define a [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) for the window in which you (a) reference the shaded construction and (b) specify how the shade is controlled.

Note that [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) has to be used with either method, even if the shade is in place at all times. You will get an error message if you try to reference a shaded construction directly from [FenestrationSurface:Detailed](#fenestrationsurfacedetailed).

#### Field: Name

Name of the shade. It is referenced as an inside or outside layer in a window construction.

#### Field: Solar Transmittance

Transmittance averaged over the solar spectrum. Assumed independent of incidence angle.

#### Field: Solar Reflectance

Reflectance averaged over the solar spectrum. Assumed same on both sides of shade and independent of incidence angle.

#### Field: Visible Transmittance

Transmittance averaged over the solar spectrum and weighted by the response of the human eye. Assumed independent of incidence angle.

#### Field: Visible Reflectance

Reflectance averaged over the solar spectrum and weighted by the response of the human eye. Assumed same on both side of shade and independent of incidence angle.

#### Field: Thermal Hemispherical Emissivity

Effective long-wave emissivity. Assumed same on both sides of shade. We can approximate this  effective emissivity, *ε~eff~,*  as follows. Let *η* be the "openness" the shade, i.e.,  the ratio of the area of openings in the shade to the overall shade area (see Field: Air-Flow Permeability, below). Let the emissivity of  the shade material be ε. Then

![](media/image30.png)\


For most non-metallic materials *ε* is about 0.9.

#### Field: Thermal Transmittance

Effective long-wave transmittance. Assumed independent of incidence angle. We can approximate this  effective long-wave transmittance, *τ~eff~,*  as follows. Let *η* be the "openness" the shade, i.e.,  the ratio of the area of openings in the shade to the overall shade area. Let the long-wave transmittance of the shade material be *τ*. Then

![](media/image31.png)\


For most materials *τ* is very close to zero, which gives

![](media/image32.png)\


#### Field: Thickness

Thickness of the shade material (m). If the shade is not flat, such as for pleated pull-down shades or folded drapery, the average thickness normal to the plane of the shade should be used.

#### Field: Conductivity

Shade material conductivity (W/m-K).

#### Field: Shade to Glass Distance

Distance from shade to adjacent glass (m). This is denoted by *s* in Figure 12 and Figure 13, below. If the shade is not flat, such as for pleated pull-down shades or folded drapery, the average shade-to-glass distance should be used. (The shade-to-glass distance is used in calculating the natural convective air flow between glass and shade produced by buoyancy effects.). Note used for between-glass shades.

In the following, *H* is the glazing height and *W* is the glazing width.

#### Field: Top Opening Multiplier

Effective area for air flow at the top of the shade divided by *sW*, the horizontal area between glass and shade (see Figures below).

#### Field: Bottom Opening Multiplier

Effective area for air flow at the bottom of the shade divided by *sW*, the horizontal area between glass and shade (see Figures below).

#### Field: Left-Side Opening Multiplier

Effective area for air flow at the left side of the shade divided by *sH*, the vertical area between glass and shade (see Figures below).

#### Field: Right-Side Opening Multiplier

Effective area for air flow at the right side of the shade divided by *sH*, the vertical area between glass and shade (see Figures below).

#### Field: Field: Air-Flow Permeability

The fraction of the shade surface that is open to air flow, i.e., the total area of openings ("holes") in the shade surface divided by the shade area, *HW*. If air cannot pass through the shade material, Air-Flow Permeability = 0. For drapery fabric and screens the Air-Flow Permeability can be taken as the "openness" of the fabric (see 2001 ASHRAE Fundamentals, Chapter 30, Fig. 31), which is 0.0 to 0.07 for closed weave, 0.07 to 0.25 for semi-open weave, and 0.25 and higher for open weave.

![Vertical section (a) and perspective view (b) of glass  and interior shade layers  showing variables used in the gap air flow analysis. In (b), the air-flow opening areas A~bot~, A~top~, A~l~, A~r~ and A~h~ are shown schematically. See Engineering Manual for definition of thermal variables.](media/vertical-section-a-and-perspective-view-b-of.png)


![Examples of air-flow openings for an interior shade covering glass of height H and width W. Not to scale. (a) Horizontal section through shade with openings on the left and right sides (top view). (b) Vertical section through shade with openings at the top and bottom (side view). In (a) Left-Side Opening Multiplier = A~l~/sH = min(l/s,1) and Right-Side Opening Multiplier = A~r~ /sH = min(r/s,1). In (b) Top Opening Multiplier = A~top~/sW = t/s and Bottom Opening Multiplier = A~bot~ /sW = b/s.](media/examples-of-air-flow-openings-for-an-interior.png)


An IDF example:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Shade,
        DRAPES - CLOSE WEAVE MEDIUM,  !- Name
        0.05,                    !- Solar transmittance
        0.3000000,               !- Solar Reflectance
        .05,                     !- Visible transmittance
        0.3000000,               !- Visible reflectance
        0.9000000,               !- Thermal Hemispherical Emissivity
        0.0,                     !- Thermal Transmittance
        0.003,                   !- Thickness {m}
        0.1,                     !- Conductivity {W/m-K}
        0.050,                   !- Shade to glass distance {m}
        1.0,                     !- Top opening multiplier
        1.0,                     !- Bottom opening multiplier
        0.0,                     !- Left-side opening multiplier
        0.0,                     !- Right-side opening multiplier
        0.0;                     !- Air flow permeability
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Blind

This object specifies the properties of a window blind consisting of flat, equally-spaced slats. Unlike window shades, which are modeled as perfect diffusers, window blinds have solar and visible transmission and reflection properties that strongly depend on slat angle and angle of incidence of solar radiation. There is an EnergyPlus Reference Data Set for [WindowMaterial:Blind](#windowmaterialblind) that contains properties of generic window blinds.

Blinds can be located on the inside of the window ("interior blinds"), on the outside of the window ("exterior blinds"), or between two layers of glass ("between-glass blinds"). When in place, the blind is assumed to cover all of the glazed part of the window, including dividers; it does not cover any of the window frame, if present. The plane of the blind is assumed to be parallel to the glazing. When the blind is retracted it is assumed to cover none of the window. The solar and thermal effects of the blind's support strings, tapes or rods are ignored. Slat curvature, if present, is ignored.

There are two methods of assigning a blind to a window:

### Inputs

#### Method 1:

#. Define the construction of the window without the blind, the so-called "bare" construction.
#. Reference the bare construction in the [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) for the window.
#. Define the [WindowMaterial:Blind](#windowmaterialblind).
#. Define a [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) for the window in which you (a) specify that this [WindowMaterial:Blind](#windowmaterialblind) is the window's shading device and (b) specify how the blind is controlled.

#### Method 2:

#. Define the [Construction](#construction) of the window without the blind, the so-called "bare" construction.
#. Reference the bare construction in the [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) for the window.
#. Define the [WindowMaterial:Blind](#windowmaterialblind).
#. Define another [Construction](#construction), called the "shaded construction," that includes the [WindowMaterial:Blind](#windowmaterialblind).
#. Define a [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) for the window in which you (a) reference the shaded construction and (b) specify how the blind is controlled.

Note that [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) has to be used with either method, even if the blind is in place at all times. You will get an error message if you try to reference a construction with a blind directly from [Window](#window) objects ([FenestrationSurface:Detailed](#fenestrationsurfacedetailed) or [Window](#window)).

Note also that [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) is used to determine not only when the blind is in place, but how its slat angle is controlled.

#### Field: Name

Name of the blind. It is referenced as a layer in a window construction (ref: [Construction](#construction) object) or as a "[Material](#material-and-material-properties) Name of Shading Device" in a [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) object.

#### Field: Slat Orientation

The choices are Horizontal and Vertical. "Horizontal" means the slats are parallel to the bottom of the window; this is the same as saying that the slats are parallel to the X-axis of the window. "Vertical" means the slats are parallel to Y-axis of the window.

#### Field: Slat Width

The width of the slat measured from edge to edge (m). See Figure 14.

#### Field: Slat Separation

The distance between the front of a slat and the back of the adjacent slat (m). See Figure 14.

#### Field: Slat Thickness

The distance between the faces of a slat (m). See Figure 14.

#### Field: Slat Angle

The angle (degrees) between the glazing outward normal and the slat outward normal, where the outward normal points away from the front face of the slat (degrees). See Figure 14.

If the [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) for the blind has Type of Slat Angle Control for Blinds = FixedSlatAngle, the slat angle is fixed at "Slat Angle."

If Type of Slat Angle Control for Blinds = BlockBeamSolar, the program automatically adjusts the slat angle so as just block beam solar radiation. In this case the value of "Slat Angle" is used only when the blind is in place and there is no beam solar radiation incident on the blind.

If Type of Slat Angle Control for Blinds = ScheduledSlatAngle, the slat angle is variable. In this case "Slat Angle" is not applicable and the field should be blank.

If Type of Slat Angle Control for Blinds = FixedSlatAngle and "Slat Angle" is less than the minimum or greater than the maximum allowed by Slat Width, Slat Separation and Slat Thickness, the slat angle will be reset to the corresponding minimum or maximum and a warning will be issued.

#### Field: Slat Conductivity

The thermal conductivity of the slat (W/m-K).

#### Field: Slat Beam Solar Transmittance

The beam solar transmittance of the slat, assumed to be independent of angle of incidence on the slat. Any transmitted beam radiation is assumed to be 100% diffuse (i.e., slats are translucent).

#### Field: Front Side Slat Beam Solar Reflectance

The beam solar reflectance of the front side of the slat, assumed to be independent of angle of incidence (matte finish). This means that slats with a large specularly-reflective component (shiny slats) are not well modeled.

#### Field: Back Side Slat Beam Solar Reflectance

The beam solar reflectance of the back side of the slat, assumed to be independent of angle of incidence (matte finish). This means that slats with a large specularly-reflective component (shiny slats) are not well modeled.

#### Field: Slat Diffuse Solar Transmittance

The slat transmittance for hemispherically diffuse solar radiation. This value should equal "Slat Beam Solar Transmittance."

#### Field: Front Side Slat Diffuse Solar Reflectance

The front-side slat reflectance for hemispherically diffuse solar radiation. This value should equal "Front Side Slat Beam Solar Reflectance."

#### Field: Back Side Slat Diffuse Solar Reflectance

The back-side slat reflectance for hemispherically diffuse solar radiation. This value should equal "Back Side Slat Beam Solar Reflectance."

#### Field: Slat Beam Visible Transmittance

The beam visible transmittance of the slat, assumed to be independent of angle of incidence on the slat. Any transmitted visible radiation is assumed to be 100% diffuse (i.e., slats are translucent).

#### Field: Front Side Slat Beam Visible Reflectance

The beam visible reflectance on the front side of the slat, assumed to be independent of angle of incidence (matte finish). This means that slats with a large specularly-reflective component (shiny slats) are not well modeled.

#### Field: Back Side Slat Beam Visible Reflectance

The beam visible reflectance on the front side of the slat, assumed to be independent of angle of incidence (matte finish). This means that slats with a large specularly-reflective component (shiny slats) are not well modeled.

#### Field: Slat Diffuse Visible Transmittance

The slat transmittance for hemispherically diffuse visible radiation. This value should equal "Slat Beam Visible Transmittance."

#### Field: Front Side Slat Diffuse Visible Reflectance

The front-side slat reflectance for hemispherically diffuse visible radiation. This value should equal "Front Side Slat Beam Visible Reflectance."

#### Field: Back Side Slat Diffuse Visible Reflectance

The back-side slat reflectance for hemispherically diffuse visible radiation. This value should equal "Back Side Slat Beam Visible Reflectance.."

#### Field: Slat Infrared Hemispherical Transmittance

The slat Infrared transmittance. It is zero for solid metallic, wooden or glass slats, but may be non-zero in some cases (e.g., thin plastic slats).

#### Field: Front Side Slat Infrared  Hemispherical Emissivity

Front-side hemispherical emissivity of the slat. Approximately 0.9 for most materials. The most common exception is bare (unpainted) metal slats or slats finished with a metallic paint.

#### Field: Back Side Slat Infrared  Hemispherical Emissivity

Back-side hemispherical emissivity of the slat. Approximately 0.9 for most materials. The most common exception is bare (unpainted) metal slats or slats finished with a metallic paint.

#### Field: Blind to Glass Distance

For interior and exterior blinds, the distance from the mid-plane of the blind to the adjacent glass (m). See **Figure** 14**. Not used for between-glass blinds. As for window shades (ref:** WindowMaterial:Shade) this distance is used in calculating the natural convective air flow between glass and blind that is produced by buoyancy effects.

#### Opening Multipliers

The following opening multipliers are defined in the same way as for window shades (see [WindowMaterial:Shade](#windowmaterialshade), Figure 12 and Figure 13). Note that, unlike window shades, there is no input for Air-Flow Permeability; this is automatically calculated by the program from slat angle, width and separation.

#### Field: Blind Top Opening Multiplier

Defined as for Material:WindowShade.

#### Field: Blind Bottom Opening Multiplier

Defined as for Material:WindowShade.

#### Field: Blind Left-Side Opening Multiplier

Defined as for Material:WindowShade.

#### Field: Blind Right-Side Opening Multiplier

Defined as for Material:WindowShade.

#### Field: Minimum Slat Angle

The minimum allowed slat angle (degrees). Used only if [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) (for the window that incorporates this blind) varies the slat angle (i.e., the [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) has Type of Slat Angle Control for Blinds = ScheduledSlatAngle or BlockBeamSolar). In this case, if the program tries to select a slat angle less than Minimum Slat Angle it will be reset to Minimum Slat Angle. (Note that if the Minimum Slat Angle itself is less than the minimum allowed by Slat Width, Slat Separation and Slat Thickness, it will be reset to that minimum.)

#### Field: Maximum Slat Angle

The maximum allowed slat angle (degrees). Used only if [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) (for the window that incorporates this blind) varies the slat angle (i.e., the [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) has Type of Slat Angle Control for Blinds = ScheduledSlatAngle or BlockBeamSolar). In this case, if the program tries to select a slat angle greater than Maximum Slat Angle the slat angle will be reset to Maximum Slat Angle. (Note that if the Maximum Slat Angle itself is greater than the maximum allowed by Slat Width, Slat Separation and Slat Thickness, it will be reset to that maximum.)

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Blind,
     White Painted Metal Blind,   !- Name
     HORIZONTAL, !- Slat orientation
     0.025   , !- Slat width (m)
     0.01875 , !- Slat separation (m)
     0.001   , !- Slat thickness (m)
     45.0    , !- Slat angle (deg)
     44.9    , !- Slat conductivity (W/m-K)
     0.0     , !- Slat beam solar transmittance
     0.8     , !- Front Side Slat beam solar reflectance
     0.8     , !- Back Side Slat beam solar reflectance
     0.0     , !- Slat diffuse solar transmittance
     0.8     , !- Front Side Slat diffuse solar reflectance
     0.8     , !- Back Side Slat diffuse solar reflectance
     0.0     , !- Slat beam visible transmittance
     0.7     , !- Front Side Slat beam visible reflectance
     0.7     , !- Back Side Slat beam visible reflectance
     0.0     , !- Slat diffuse visible transmittance
     0.7     , !- Front Side Slat diffuse visible reflectance
     0.7     , !- Back Side Slat diffuse visible reflectance
     0.0     , !- Slat Infrared hemispherical transmittance
     0.9     , !- Front Side Slat Infrared hemispherical emissivity
     0.9     , !- Back Side Slat Infrared hemispherical emissivity
     0.050   , !- Blind-to-glass distance
     0.0     , !- Blind top opening multiplier
     0.0     , !- Blind bottom opening multiplier
     0.5     , !- Blind left-side opening multiplier
     0.5     , !- Blind right-side opening multiplier
     ,         !- Minimum slat angle (deg)
     ;         !- Maximum slat angle (deg)
~~~~~~~~~~~~~~~~~~~~

![(a) Side view of a window blind with horizontal slats  (or top view of blind with vertical slats) showing slat geometry. The front face of a slat is shown by a heavy line. The slat angle is defined as the angle between the glazing outward normal and the slat outward normal, where the outward normal points away from the front face of the slat. (b) Slat orientations for representative slat angles. The slat angle varies from 0^O^, when the front of the slat is parallel to the glazing and faces toward the outdoors,  to 90^O^, when the slat is perpendicular to the glazing, to 180^O^, when the front of the slat is parallel to the glazing and faces toward the indoors. The minimum and maximum slat angles are determined by the slat thickness, width and separation.](media/a-side-view-of-a-window-blind-with-horizontal.png)


## WindowMaterial:ComplexShade

This input object is used to define shade layers used in the [Construction:ComplexFenestrationState](#constructioncomplexfenestrationstate) object.

### Inputs

#### Field: Name

Unique name of the shading layer.

#### Field: Shading Layer Type

The type of shading layer.  The options are:

- Venetian – for modeling venetian blinds
- Woven – for modeling shading systems with a regular weave
- Perforated – for modeling perforated screens
- BSDF – for modeling shades whose properties are represented by a BSDF file
- OtherShadingType – for modeling shading systems which do not belong to the any of the previous group

#### Field: Thickness

The thickness (m) of the shading layer. This value is ignored for ShadingLayerType = Venetian, because the program will calculate the thickness based on the slat angle. This value is needed for ShadingLayerType = Woven and Perforated

#### Field: Conductivity

The conductivity (W/mK) of the shading layer material. Default: 1.0

- Venetian – the conductivity of the venetian blind slat material
- Woven – the conductivity of the woven shade material (such as the thread for a fabric shade)
- Perforated – for modeling perforated screens
- BSDF – for modeling shades whose properties are represented by a BSDF file
- OtherShadingType – for modeling shading systems which do not belong to the any of the previous group

#### Field: IR Transmittance

The IR transmittance of the shading layer. Minimum value: 0. Maximum value: 1. Default: 0.

#### Field: Front Emissivity

The front emissivity of the shading layer. Minimum value: 0. Maximum value: 1. Default: 0.90.

#### Field: Back Emissivity

The back emissivity of the shading layer. Minimum value: 0. Maximum value: 1. Default: 0.90.

#### Field: Top Opening Multiplier

The top opening multiplier value will depend on the location of the shading device within the glazing system. There are several possible scenarios which can occur and they can be divided into two groups:

**Shading device on the indoor/outdoor side of the window**

In this case the opening multiplier is calculated as the smallest distance between the shading device and the frame (d~top~), divided by the gap width (S).  There are three possible cases for the position of a shading device the on indoor/outdoor side (see Figure 15).

![Three cases for the D~top~ calculation for an indoor/outdoor shade: Case a) A shading device between the frame; Case b) A shading device outside the frame, covering the frame; Case c) a shading device outside the frame, not covering the frame.](media/three-cases-for-the-dtop-calculation-for-an.png)      ![Three cases for the D~top~ calculation for an indoor/outdoor shade: Case a) A shading device between the frame; Case b) A shading device outside the frame, covering the frame; Case c) a shading device outside the frame, not covering the frame.](media/three-cases-for-the-dtop-calculation-for-an-001.png)      ![Three cases for the D~top~ calculation for an indoor/outdoor shade: Case a) A shading device between the frame; Case b) A shading device outside the frame, covering the frame; Case c) a shading device outside the frame, not covering the frame.](media/three-cases-for-the-dtop-calculation-for-an-002.png)


In the case where the distance between the frame and the shading device is bigger than the gap width, the d~top~ multiplier is equal to one. Therefore, the calculation of the D~top~ opening multiplier is:

A~top~ = min(d~top~/S, 1)

**Shading device between glass layers**

In this case the opening multiplier is calculated as the smallest distance between the shading device and the frame or spacer (d~top~), divided by the smaller gap width (the minimum of (S~1~ andS~2~)).

![Calculation of Dtop for a shading device between glass layers](media/calculation-of-dtop-for-a-shading-device.png)           ![Calculation of Dtop for a shading device between glass layers](media/calculation-of-dtop-for-a-shading-device-001.png)


The D~top~ opening multiplier for a between glass shade is calculated as:

A~top~ = min(d~top~/S~min~, 1)

Where

S~min~ = min(S~1~, S~2~)

#### Field: Bottom Opening Multiplier

The bottom opening multiplier (d~bot~) is calculated in the same way as the top opening multiplier, with the rules applied to the bottom of the shading device.

#### Field: Left Side Opening Multiplier

The left side opening multiplier (d~left~) is calcuated in the same way as the top opening multiplier, with the rules applied to the left side of the shading device.

#### Field: Right Side Opening Multiplier

The right side opening multiplier (d~right~)  is calcuated in the same way as the top opening multiplier, with the rules applied to the right side of the shading device.

#### Field: Front Opening Multiplier

The fraction of glazing system area that is open on the front of the shading layer (see Figure 17). This fraction is calculated as follows: Afront / (W \* H), where Afront = Area of the front of the glazing system that is not covered by the shading system, W = the width of the glazing system (IGU) and H is height of the glazing system (IGU).

![Front view of shading layer openings.](media/front-view-of-shading-layer-openings..png)


#### Field: Slat Width

The width (m) of the venetian slats.  Used only for ShadingLayerType = Venetian.

#### Field: Slat Spacing

The distance (m) between front sides of the venetian slats.  Used only for ShadingLayerType = Venetian.

#### Field: Slat Thickness

The thickness (m) of the venetian slats.  Used only for ShadingLayerType = Venetian.

#### Field: Slat Angle

The slat tilt angle (degrees) of the venetian slats.  Used only for ShadingLayerType = Venetian.  Range of slat angle is from -90 to 90 degrees.

#### Field: Slat Conductivity

The conductivity (W/mK) of the venetian slats.  Used only for ShadingLayerType = Venetian.

#### Field: Slat Curve

The curvature radius (m) of the venetian slats.  Setting this value to zero means there is no curvature in the slat (it is flat), while a non-zero value is the radius of the slat curve.  This value cannot be smaller than Slat Width / 2.  Used only for ShadingLayerType = Venetian.

![Side view of horizontal venetian blind slats or top view of blinds with vertical slats.  Front face of slats is marked with red line.](media/side-view-of-horizontal-venetian-blind-slats.png)


An IDF example for ShadingLayerType = Venetian

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:ComplexShade,       !- venetian blind layer
      Shade_30001_Layer,               !- name
      Venetian,                        !- shading layer type
      0.005,                           !- thickness
      160,                             !- layer conductivity
      0.0,                             !- IR transmittance
      0.9,                             !- front emissivity
      0.9,                             !- back emissivity
      0.0,                             !- top opening multiplier
      0.0,                             !- bottom opening multiplier
      0.0,                             !- left side opening multiplier
      0.0,                             !- right side opening multiplier
      0.05,                            !- front opening multiplier
      0.016,                           !- venetian slat width
      0.012,                           !- venetian slat spacing
      0.0006,                          !- venetian slat thickness
      -45.00,                          !- venetian slat angle
      160.00,                          !- venetian slat conductivity
      0.0000;                          !- venetian slat curve
~~~~~~~~~~~~~~~~~~~~

An IDF example for ShadingLayerType = Woven

(Note that it is not necessary to include "blank" lines for the venetian blind input for a Woven shade definition).

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:ComplexShade,       !- woven shade layer
      Shade_30002_Layer,               !- name
      Woven,                           !- shading layer type
      0.011,                           !- thickness
      1,                               !- layer conductivity
      0.0,                             !- IR transmittance
      0.9,                             !- front emissivity
      0.9,                             !- back emissivity
      0.0,                             !- top opening multiplier
      0.0,                             !- bottom opening multiplier
      0.0,                             !- left side opening multiplier
      0.0,                             !- right side opening multiplier
      0.17;                            !- front opening multiplier
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Screen

This object specifies the properties of exterior window screen materials. The window screen model assumes the screen is made up of intersecting orthogonally-crossed cylinders. The surface of the cylinders is assumed to be diffusely reflecting, having the optical properties of a Lambertian surface.

The beam solar radiation transmitted through a window screen varies with sun angle and is made up of two distinct elements: a direct beam component and a reflected beam component. The direct beam transmittance component is modeled using the geometry of the screen material and the incident angle of the sun to account for shadowing of the window by the screen material. The reflected beam component is an empirical model that accounts for the inward reflection of solar beam off the screen material surface. This component is both highly directional and small in magnitude compared to the direct beam transmittance component (except at higher incident angles, for which case the magnitude of the direct beam component is small or zero and the reflected beam component, though small in absolute terms can be many times larger than the direct beam component). For this reason, the reflected beam transmittance component calculated by the model can be a. disregarded, b. treated as an additive component to direct beam transmittance (and in the same direction), or c. treated as hemispherically-diffuse transmittance based on a user input to the model.

![Direct beam and reflected beam transmittance components](media/direct-beam-and-reflected-beam-transmittance.jpeg)


The window screen "assembly" properties of overall beam solar reflectance and absorptance (including the screen material ‘cylinders' and open area) also change with sun angle and are calculated based on the values of the beam solar transmittance components (direct and reflected components described above) and the physical properties of the screen material (i.e., screen material diameter, spacing, and reflectance).

Transmittance, reflectance, and absorptance of diffuse solar radiation are considered constant values and apply to both the front and back surfaces of the screen. These properties are calculated by the model as an average value by integrating the screen's beam solar properties over a quarter hemisphere of incident radiation. Long-wave emissivity is also assumed to be the same for both sides of the screen.

There is an EnergyPlus Reference Data Set for [WindowMaterial:Screen](#windowmaterialscreen) that contains properties for generic window screens. [Window](#window) screens of this type can only be used on the outside surface of the window ("exterior screens"). When in place, the screen is assumed to cover all of the glazed part of the window, including dividers; it does not cover any of the window frame, if present. The plane of the screen is assumed to be parallel to the glazing.

[WindowMaterial:Screen](#windowmaterialscreen) can be used to model wire mesh insect screens where the solar and visible transmission and reflection properties vary with the angle of incidence of solar radiation. For diffusing materials such as drapery and translucent roller shades it is better to use the [WindowMaterial:Shade](#windowmaterialshade) object. For slat-type shading devices like Venetian blinds, which have solar and visible transmission and reflection properties that strongly depend on slat angle and angle of incidence of solar radiation, it is better to use [WindowMaterial:Blind](#windowmaterialblind).

There are two methods of assigning a screen to a window:

### Inputs

#### Method 1:

#. Define the construction of the window without the screen, the so-called "bare" construction.
#. Reference the bare construction in the [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) for the window.
#. Define the [WindowMaterial:Screen](#windowmaterialscreen) object.
#. Define a [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) for the window in which you (a) specify that this Material:WindowScreen is the window's shading device, and (b) specify how the screen is controlled.

#### Method 2:

#. Define the [Construction](#construction) of the window without the screen, the so-called "bare" construction.
#. Reference the bare construction in the [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) for the window.
#. Define the [WindowMaterial:Screen](#windowmaterialscreen) object.
#. Define another [Construction](#construction), called the "shaded construction," that includes the [WindowMaterial:Screen](#windowmaterialscreen).
#. Define a [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) for the window in which you (a) reference the shaded construction, and (b) specify how the screen is controlled.

Note that [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) has to be used with either method, even if the screen is in place at all times. You will get an error message if you try to reference a shaded construction directly from a [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) object.

#### Field: Name

Enter a unique name for the screen. This name is referenced as an outside layer in a window construction.

#### Field: Reflected Beam Transmittance Accounting Method

This input specifies the method used to account for screen-reflected beam solar radiation that is transmitted through the window screen (as opposed to being reflected back outside the building). Since this inward reflecting beam solar is highly directional and is not modeled in the direction of the actual reflection, the user is given the option of how to account for the directionality of this component of beam solar transmittance. Valid choices are DoNotModel, ModelAsDirectBeam (i.e., model as an additive component to direct solar beam and in the same direction), or ModelAsDiffuse (i.e., model as hemispherically-diffuse radiation). The default value is ModelAsDiffuse.

#### Field: Diffuse Solar Reflectance

This input specifies the solar reflectance (beam-to-diffuse) of the screen material itself (not the effective value for the overall screen "assembly" including open spaces between the screen material). The outgoing diffuse radiation is assumed to be Lambertian (distributed angularly according to Lambert's cosine law). The solar reflectance is assumed to be the same for both sides of the screen. This value must be from 0 to less than 1.0. In the absence of better information, the input value for diffuse solar reflectance should match the input value for diffuse visible reflectance.

#### Field: Diffuse Visible Reflectance

This input specifies the visible reflectance (beam-to-diffuse) of the screen material itself (not the effective value for the overall screen "assembly" including open spaces between the screen material) averaged over the solar spectrum and weighted by the response of the human eye. The outgoing diffuse radiation is assumed to be Lambertian (distributed angularly according to Lambert's cosine law). The visible reflectance is assumed to be the same for both sides of the screen. This value must be from 0 to less than 1.0.

If diffuse visible reflectance for the screen material is not available, then the following guidelines can be used to estimate this value:

Dark-colored screen (e.g., charcoal):0.08 – 0.10

Medium-colored screen (e.g., gray):0.20 – 0.25

Light-colored screen (e.g., bright aluminum):0.60 – 0.65

Commercially-available gray scale or grayscale reflecting chart references can be purchased for improved accuracy in estimating visible reflectance (by visual comparison of screen reflected brightness with that of various known-reflectance portions of the grayscale).

#### Field: Thermal Hemispherical Emissivity

*Long-wave emissivity ε* of the screen material itself (not the effective value for the overall screen "assembly" including open spaces between the screen material). The emissivity is assumed to be the same for both sides of the screen.

For most non-metallic materials, *ε* is about 0.9. For metallic materials, *ε* is dependent on material, its surface condition, and temperature. Typical values for metallic materials range from 0.05 – 0.1 with lower values representing a more finished surface (e.g. low oxidation, polished surface). [Material](#material-and-material-properties) emissivities may be found in Table 5 from the 2005 ASHRAE Handbook of Fundamentals, page 3.9. The value for this input field must be between 0 and 1, with a default value of 0.9 if this field is left blank.

#### Field: Conductivity

Screen material conductivity (W/m-K). This input value must be greater than 0. The default value is 221 W/m-K (aluminum).

#### Field: Screen Material Spacing

The spacing, S, of the screen material (m) is the distance from the center of one strand of screen to the center of the adjacent one. The spacing of the screen material is assumed to be the same in both directions (e.g., vertical and horizontal). This input value must be greater than the non-zero screen material diameter. If the spacing is different in the two directions, use the average of the two values.

![Screen Material Spacing and Diameter](media/screen-material-spacing-and-diameter.jpeg)


#### Field: Screen Material Diameter

The diameter, D, of individual strands or wires of the screen material (m). The screen material diameter is assumed to be the same in both directions (e.g., vertical and horizontal). This input value must be greater than 0 and less than the screen material spacing. If the diameter is different in the two directions, use the average of the two values.

#### Field: Screen to Glass Distance

Distance from the window screen to the adjacent glass surface (m). If the screen is not flat, the average screen to glass distance should be used. The screen-to-glass distance is used in calculating the natural convective air flow between the glass and the screen produced by buoyancy effects. This input value must be from 0.001 m to 1 m, with a default value of 0.025 m if this field is left blank.

#### Field: Top Opening Multiplier

Effective area for air flow at the top of the screen divided by the horizontal area between the glass and screen (see the same field for the Material:WindowShade object for additional description). The opening multiplier fields can be used to simulate a shading material that is offset from the window frame. Since window screens are typically installed against the window frame, the default value is equal to 0.This input value can range from 0 to 1.

#### Field: Bottom Opening Multiplier

Effective area for air flow at the bottom of the screen divided the horizontal area between the glass and screen (see the same field for the Material:WindowShade object for additional description). The opening multiplier fields can be used to simulate a shading material that is offset from the window frame. Since window screens are typically installed against the window frame, the default value is equal to 0. This input value can range from 0 to 1.

#### Field: Left-Side Opening Multiplier

Effective area for air flow at the left side of the screen divided the vertical area between the glass and screen (see the same field for the Material:WindowShade object for additional description). The opening multiplier fields can be used to simulate a shading material that is offset from the window frame. Since window screens are typically installed against the window frame, the default value is equal to 0. This input value can range from 0 to 1.

#### Field: Right-Side Opening Multiplier

Effective area for air flow at the right side of the screen divided the vertical area between the glass and screen (see the same field for the Material:WindowShade object for additional description). The opening multiplier fields can be used to simulate a shading material that is offset from the window frame. Since window screens are typically installed against the window frame, the default value is equal to 0. This input value can range from 0 to 1.

#### Field: Angle of Resolution for Screen Transmittance Output Map

Angle of resolution, in degrees, for the overall screen beam transmittance (direct and reflected) output map. The comma-separated variable file eplusscreen.csv (Ref. OutputDetailsandExamples.pdf) will contain the direct beam and reflected beam solar radiation that is transmitted through the window screen as a function of incident sun angle (0 to 90 degrees relative solar azimuth and 0 to 90 degrees relative solar altitude) in sun angle increments specified by this input field. The default value is 0, which means no transmittance map is generated. Other valid choice inputs are 1, 2, 3 and 5 degrees.

An IDF example for this object, along with [Construction](#construction) and [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) objects, is shown below:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Screen,
        EXTERIOR SCREEN,         !- Name
        ModelAsDiffuse,        !- Reflected Beam Transmittance Accounting Method
        0.6,                     !- Diffuse Solar Reflectance
        0.6,                     !- Diffuse Visible Reflectance
        0.9,                     !- Thermal Hemispherical Emissivity
        221.0,                   !- Conductivity {W/m-K}
        0.00154,                 !- Screen Material Spacing (m)
        0.000254,                !- Screen Material Diameter (m)
        0.025,                   !- Screen-to-Glass Distance {m}
        0.0,                     !- Top Opening Multiplier
        0.0,                     !- Bottom Opening Multiplier
        0.0,                     !- Left-Side Opening Multiplier
        0.0,                     !- Right-Side Opening Multiplier
        0;                   !- Angle of Resolution for Output Map {deg}

    Construction,
        DOUBLE PANE WITHOUT SCREEN,     !- Name
        GLASS - CLEAR SHEET 1 / 8 IN,   !- Outside Layer
        WinAirB1 - AIRSPACE RESISTANCE, !- Layer #2
        GLASS - CLEAR SHEET 1 / 8 IN;   !- Layer #3

    WindowProperty:ShadingControl,
        DOUBLE PANE WITH SCREEN, !- User Supplied Shading Control Name
        ExteriorScreen,          !- Shading Type
        ,                        !- Name of construction with shading
        AlwaysOn,                !- Shading Control Type
        ScreenSchedule,          !- Schedule Name
        20.0,                    !- SetPoint {W/m2, W or deg C}
        YES,                     !- Shading Control Is Scheduled
        NO,                      !- Glare Control Is Active
        EXTERIOR SCREEN,         !- Material Name of Shading Device
        ,                        !- Type of Slat Angle Control for Blinds
        ;                        !- Slat Angle Schedule Name
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Shade:EquivalentLayer

This object specifies the properties of Equivalent Layer window shade (roller blind) materials. Shades are considered to be thin, flat and perfect diffusers (all transmitted and reflected radiation is hemispherically-diffuse). However, shades can have beam-beam transmittance by virtue of their material openness.  The beam-beam transmittnec is assumed to be the same for both sides of the shade and is the same as the openness area fraction. Beam-dffuse transmittance and reflectance, and emissivity properties can be different for front and back side of the shade. [Window](#window) shades can be placed on the inside of the window, on the outside of the window, or between glass layers. [WindowMaterial:Shade:EquivalentLayer](#windowmaterialshadeequivalentlayer) is used for roller blinds. The off-normal solar property calculation of shades (roller blind) is based on a set of correlations developed from measurement of samples of commercially produced roller blind material with openness fraction less than 0.14. The model is not intended for materials with unusually high values of openness and should be limited to a mximum openness fraction of 0.20. The visible spectrum solar properties input fields are not used currently hence can be left blank.

### Inputs

#### Field: Name

Name of the shade. It is referenced as an inside, inbetween or outside layer in an equivalent layer  window construction.

#### Field: Shade Beam-Beam Solar Transmittance

This value is the beam-beam transmittance of the shade at normal incidence and it is the same as the openness area fraction of the shade material. Assumed to be the same for front and back sides of the roller blinds.  The minimum value is 0.0 and maximum value is less than 1.0.  The default value is 0.0. For most common shade materials (e.g. Roller Blinds) the material oppeness fraction doesn't exceed 0.20.

#### Field: Front Side Shade Beam-Diffuse Solar Transmittance

This value is the front side beam-diffuse transmittance of the shade material at normal incidence averaged over the entire spectrum of solar radiation. The minimum value is 0.0 and maximum value is less than 1.0.  The default value is 0.0.

#### Field: Back Side Shade Beam-Diffuse Solar Transmittance

This value is the back side beam-diffuse transmittance of the shade material at normal incidence averaged over the entire spectrum of solar radiation. The minimum value is 0.0 and maximum value is less than 1.0.  The default value is 0.0.

#### Field: Front Side Shade Beam-Diffuse Solar Reflectance

This value is the front side beam-diffuse reflectance of the shade material at normal incidence averaged over the entire spectrum of solar radiation. The minimum value is 0.0 and maximum value is less than 1.0.

#### Field: Back Side Shade Beam-Diffuse Solar Reflectance

This value is the back side beam-diffuse reflectance of the shade material at normal incidence averaged over the entire spectrum of solar radiation. The minimum value is 0.0 and maximum value is less than 1.0.

#### Field: Shade Beam-Beam Visible Transmittance

This value is the beam-beam transmittance at normal incidence averaged over the visible spectrum of solar radiation. Assumed to be the same for front and back sides. The minimum value is 0.0 and maximum value is less than 1.0.  Currently this input field is not used.

#### Field: Shade Beam-Diffuse Visible Transmittance

This value is the beam-diffuse transmittance at normal incidence averaged over the visible spectrum of solar radiation. Assumed to be the same for front and back sides. The minimum value is 0.0 and maximum value is less than 1.0.  Currently this input field is not used.

#### Field: Shade Beam-Diffuse Visible Reflectance

This value is the beam-diffuse reflectance at normal incidence averaged over the visible spectrum of solar radiation. Assumed to be the same for front and back sides. The minimum value is 0.0 and maximum value is less than 1.0.  Currently this input field is not used.

#### Field: Shade Material Infrared Transmittance

This value is the long-wave transmittance of the shade material and assumed to be the same for front and back sides of the shade. The minimum value is 0.0 and maximum value is less than 1.0. Default value is 0.05.

#### Field: Front Side Shade Material Infrared Emissivity

This value is the front side long-wave hemispherical emissivity of shade material. The minimum value is 0.0 and maximum value is less than 1.0. Default value is 0.91. The front side effective emissivity of the shade layer is calculated using this value and the material openness specified above.

#### Field: Back Side Shade Material Infrared Emissivity

This value is the back side long-wave hemispherical emissivity of shade material. The minimum value is 0.0 and maximum value is less than 1.0. Default value is 0.91. The back side effective emissivity of the shade is calculated using this value and the material openness specified above.

An IDF example for this object is shown below:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Shade:EquivalentLayer,
      Shade1,        !- Name
      0.190,         !- Shade Beam-Beam Solar Transmittance
      0.206,         !- Front Side Shade Beam-Diffuse Solar Transmittance
      0.206,         !- Back Side Shade Beam-Diffuse Solar Transmittance
      0.499,         !- Front Side Shade Beam-Diffuse Solar Reflectance
      0.499,         !- Back Side Shade Beam-Diffuse Solar Reflectance
      0.0,           !- Shade Beam-Beam Visible Transmittance
      0.0,           !- Shade Beam-Diffuse Vissible Transmittance
      0.0,           !- Shade Vissible Reflectance
      0.0,           !- Shade Material Infrared Transmittance
      0.84,          !- Front Side Shade Material Infrared Emissivity
      0.84;          !- Back Side Shade Material Infrared Emissivity
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Drape:EquivalentLayer

Specifies the optical and thermal properties of equivalent layer window drape fabric materials.

Drapery fabric shades are commonly placed on the the inside of the window. The long-wave (infrared) properties for commonly used drapery fabrics are assumed to be the same on both sides but different values can be specified when required. Drape fabric shade layers are considered to be perfect diffusers (reflected radiation is hemispherically-diffuse independent of angle of incidence). Unpleated drape fabric is treated as thin and flat layer. The off-normal optical properties of drapery fabric is determined from user specified optical properties at normal incidence using empirical correlations. Pleated drape fabric requires entering the pleated section average width and length as showsn in Figure 21. For pleated drapes the effective beam-beam and beam-diffuse solar properties are determined by tracking both radiation components, for a given incident angle solar radiation, through various interactions with a fabric pleated in a rectangular geometry shown in Figure 21.  The solar properties of the two different pleat facets are evaluated on the basis of the local solar incidence angle.  Therefore, the effective layer properties are influenced not just by horizontal solar profile angle, but also by incidence angle. The correlations used for drape fabrics optical property calculations reqiure that the solar absorptance of the fabric, at normal incidence, is not less than 1%.

![Geometry used for Pleated Drape Analysis](media/geometry-used-for-pleated-drape-analysis.png)


### Inputs

#### Field: Name

Name of the drape fabric shade layer. It is referenced as an inside, in between or outside layer in an equivalent layer window construction.

#### Field: Drape Beam-Beam Solar Transmittance

This value is the drape fabric beam-beam transmittance at normal incidence, and it is the same as the drape fabric openness area fraction.  Assumed to be the same for front and back sides of the drape fabric layer.  The minimum value is 0.0 and maximum value is less than 1.0. For most drape fabric materials the maximum frabric openness fraction do not exceed 0.2.  The default value is 0.0.

#### Field: Front Side Drape Beam-Diffuse Solar Transmittance

This value is the front side beam-diffuse solar transmittance of the drape fabric material at normal incidence averaged over the entire spectrum of solar radiation. The minimum value is 0.0 and maximum value is less than 1.0.

#### Field: Back Side Drape Beam-Diffuse Solar Transmittance

This value is the back side beam-diffuse solar transmittance of the drape fabric material at normal incidence averaged over the entire spectrum of solar radiation. The minimum value is 0.0 and maximum value is less than 1.0.

#### Field: Front Side Drape Beam-Diffuse Solar Reflectance

This value is the front side beam-diffuse solar reflectance of the drape fabric material at normal incidence averaged over the entire spectrum of solar radiation. The minimum value is 0.0 and maximum value is less than 1.0.

#### Field: Back Side Drape Beam-Diffuse Solar Reflectance

This value is the back side beam-diffuse solar reflectance of the drape fabric material at normal incidence averaged over the entire spectrum of solar radiation. The minimum value is 0.0 and maximum value is less than 1.0.

#### Field: Drape Beam-Beam Visible Transmittance

This value is the drape fabric beam-beam visible transmittance at normal incidence averaged over the visible spectrum range of solar radiation.  Assumed to be the same for front and back sides of the drape fabric layer.  The minimum value is 0.0 and maximum value is less than 1.0.  The default value is 0.0.  This input field is not used currently.

#### Field: Front Side Drape Beam-Diffuse Visible Reflectance

This value is the front side drape fabric beam-diffuse visible reflectance at normal incidence averaged over the visible spectrum range of solar radiation.  Assumed to be the same for front and back sides of the drape.  The minimum value is 0.0 and maximum value is less than 1.0.  The default value is 0.0.  This input field is not used currently.

#### Field: Back Side Drape Diffuse-Diffuse Visible Reflectance

This value is the back side drape fabric diffuse-diffuse visible reflectance at normal incidence averaged over the visible spectrum range of solar radiation.  Assumed to be the same for front and back sides of the drape.  The minimum value is 0.0 and maximum value is less than 1.0.  The default value is 0.0.  This input field is not used currently.

#### Field: Drape Material Infrared Transmittance

This value is the long-wave hemispherical transmittance of the fabric material at zero fabric openness fraction.  Assumed to be the same for front and back sides of the drape fabric material layer. The minimum value is 0.0 and maximum value is less than 1.0.  The default value is 0.05.

#### Field: Front Side Drape Material Infrared Emissivity

This value is the front side long-wave hemispherical emissivity of fabric material at zero shade opennes. The minimum value is 0.0 and maximum value is less than 1.0. the default value is 0.87. The front side effective emissivity of the drape fabric layer is calculated using this value and the fabric openness area fraction specified above.

#### Field: Back Side Drape Material Infrared Emissivity

This value is the back side long-wave hemispherical emissivity of fabric material at zero fabric openness fraction. The minimum value is 0.0 and maximum value is less than 1.0. The default value is 0.87. The back side effective emissivity of the drape fabric layer is calculated using this value and the fabric openness area fraction specified above.

#### Field: Width of Pleated Fabric

This value is the width of the pleated section of the draped fabric, w(m). If the drape fabric is flat (unpleated), then the pleated section width is set to zero. The default value is 0.0, i.e., assumes flat drape fabric.

#### Field: Length of Pleated Fabric

This value is the length of the pleated section of the draped fabric, s(m). If the drape fabric is flat (unpleated), then the pleated section length is set to zero.  The default value is 0.0, i.e., assumes flat drape fabric.

An IDF example for this object is shown below:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Drape:EquivalentLayer,
      Drape02,            !- Name
      0.14,               !- Shade Beam-Beam Solar Transmittance
      0.10,               !- Front Side Shade Beam-Diffuse Solar Transmittance
      0.10,               !- Back Side Shade Beam-Diffuse Solar Transmittance
      0.40,               !- Front Side Shade Beam-Diffuse Solar Reflectance
      0.50,               !- Back Side Shade Beam-Diffuse Solar Reflectance
      0.0,                !- Shade Beam-Beam Visible Transmittance
      0.0,                !- Shade Beam-Diffuse Visible Transmittance
      0.0,                !- Shade Beam-Diffuse Visible Reflectance
      0.10,               !- Shade Material Infrared Transmittance
      0.90,               !- Front Side Shade Material Infrared Emissivity
      0.80,               !- Back Side Shade Material Infrared Emissivity
      0.01,               !- Width of Pleated Fabric
      0.025;              !- Length of Pleated Fabric
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Blind:EquivalentLayer

This object specifies the properties of an Equivalent Layer window blind consisting of thin and equally-spaced slats. The the model assumes that slats are flat and thin, and applies correction for the slat curvature effect based on the user specified slat crwon.  Slats are assumed to transmit and reflect diffusely. The effective shortwave optical and longwave optical properties of venetian blind layer is estimated analytically. The Equivalent Layer blind model requires optical properties and geometry of the slats shown in the Figure "Geometry and Properties used for venetian blind analysis". Likewise, effective longwave properties are obtained for the layer knowing longwave properties of the slats.

![Geometry and Properties used for venetian blind analysis](media/geometry-and-properties-used-for-venetian.png)


The input data required to characterize a venetian blind are: front and back side reflectance and transmittance of the slat, geometry (Slat width, w, slat spacing, s, slat crown, c, and slat angle, , and long wave emittance and transmittance of the slat. Blinds can be located on the inside of the window, on the outside of the window, or between two layers of glass. The blind is assumed to cover all of the glazed part of the window.

### Inputs

#### Field: Name

Name of the venetian blind. It is referenced as an inside, outside or in between layers in an equivalent layer window construction.

#### Field: Slat Orientation

The choices are Horizontal and Vertical. "Horizontal" means the slats are parallel to the bottom of the window; this is the same as saying that the slats are parallel to the X-axis of the window. "Vertical" means the slats are parallel to Y-axis of the window.  The default is "Horizontal".

#### Field: Slat Width

This value is the width of the slat measured from edge to edge (m). The default value is 0.0254.

#### Field: Slat Separation

The distance between the front of a slat and the back of the adjacent slat (m). The default value is 0.025. The slat separation should not be greater than the slat width.

#### Field: Slat Crown

The perpendicular length between the slat cord  and the curve (m). Crown=0.0625x"Slat width". Slat is assumed to be rectangular in cross section and flat. The crown accounts for curvature of the slat.  The minimum value is 0.0, and the default value is 0.0015m.

#### Field: Slat Angle

The angle (degrees) between the glazing outward normal and the slat outward normal, where the outward normal points away from the front face of the slat (degrees). If the "Slat Angle Control" input field below specified is "FixedSlatAngle", then the slat angle is fixed at "Slat Angle" value entered.  Minimum value is zero, and the maximum value allowed is 180.0 degrees.  The default value is 45 degrees.

#### Field: Front Side Slat Beam-Diffuse Solar Transmittance

This value is the slat front side beam-diffuse solar transmittance at normal incidence averaged over the entire spectrum of solar radiation. Any transmitted beam radiation is assumed to be 100% diffuse (i.e., slats are translucent). Minimum value is 0.0, and the maximum value is less than 1.0. The default value is 0.0.

#### Field: Back Side Slat Beam-Diffuse Solar Transmittance

This value is the slat back side beam-diffuse solar transmittance at normal incidence averaged over the entire spectrum of solar radiation. Any transmitted beam radiation is assumed to be 100% diffuse (i.e., slats are translucent). Minimum value is 0.0,  and the maximum value is less than 1.0. The default value is 0.0.

#### Field: Front Side Slat Beam-Diffuse Solar Reflectance

This value is slat front side beam-diffuse solar reflectance at normal incidence averaged over the entire spectrum of solar radiation. All the reflected component is assumed to be diffuse. Minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Back Side Slat Beam-Diffuse Solar Reflectance

This value is the slat back side beam-diffuse solar reflectance at normal incidence averaged over the entire spectrum of solar radiation. All the reflected component is assumed to be diffuse. Minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Front Side Slat Beam-Diffuse Visible Solar Transmittance

This value is the slat front side beam-diffuse visible transmittance at normal incidence averaged over the visible spectrum range of solar radiation. Any transmitted beam radiation is assumed to be 100% diffuse (i.e., slats are translucent). Minimum value is 0.0, and the maximum value is less than 1.0. The default value is 0.0.

#### Field: Back Side Slat Beam-Diffuse Visible Solar Transmittance

This value is the slat back side beam-diffuse visible transmittance at normal incidence averaged the visible spectrum range of solar radiation. Any transmitted beam radiation is assumed to be 100% diffuse (i.e., slats are translucent). Minimum value is 0.0, and the maximum value is less than 1.0. The default value is 0.0.

#### Field: Front Side Slat Beam-Diffuse Visible Solar Reflectance

This value is the slat front side beam-diffuse visible reflectance at normal incidence averaged over the visible spectrum range of solar radiation. All the reflected component is assumed to be diffuse. Minimum value is 0.0, and the maximum value is less than 1.0

#### Field: Back Side Slat Beam-Diffuse Visible Solar Reflectance

This value is the slat back side beam-diffuse visible reflectance at normal incidence averaged over the visible spectrum range of solar radiation. All the reflected component is assumed to be diffuse. Minimum value is 0.0,  and the maximum value is less than 1.0

#### Field: Slat Diffuse-Diffuse Solar Transmittance

This value is the slat diffuse-diffuse solar transmittance for hemispherically diffuse solar radiation. This value is the same for front and back side of the slat.  Minimum value is 0.0,  and the maximum value is less than 1.0.

#### Field: Front Side Slat Diffuse-Diffuse Solar Reflectance

This value is the slat front side diffuse-diffuse solar reflectance for hemispherically diffuse solar radiation. Minimum value is 0.0,  and the maximum value is less than 1.0.

#### Field: Back Side Slat Diffuse-Diffuse Solar Reflectance

This value is the slat back side diffuse-diffuse solar reflectance for hemispherically diffuse solar radiation. Minimum value is 0.0,  and the maximum value is less than 1.0.

#### Field: Slat Diffuse-Diffuse Visible Transmittance

This value is the slat diffuse-diffuse visible transmittance for hemispherically diffuse visible spectrum range of solar radiation. This value is the same for front and back side of the slat.  Minimum value is 0.0,  and the maximumvalue is less than 1.0. This input field is not used currently.

#### Field: Front Side Slat Diffuse-Diffuse Visible Reflectance

This value is the slat front side diffuse-diffuse visible reflectance for hemispherically diffuse visible spectrum range of solar radiation. Minimum value is 0.0, and the maximum value is less than 1.0.  This input field is not used currently.

#### Field: Back Side Slat Diffuse-Diffuse Visible Reflectance

This value is the slat back side diffuse-diffuse visible reflectance for hemispherically diffuse visible spectrum range of solar radiation. Minimum value is 0.0, and the maximum value is less than 1.0.  This input field is not used currently.

#### Field: Slat Infrared Transmittance

This value is the long-wave hemispherical transmittance of the slat material. Assumed to be the same for both sides of the slat. The minimum value is 0.0, the maximum value is less than 1.0.  The default value is 0.0.

#### Field: Front Side Slat Infrared Emissivity

This value is the front side long-wave hemispherical emissivity of the slat material. The minimum value is 0.0, the maximum value is less than 1.0. The default value is 0.9.

#### Field: Back Side Slat Infrared Emissivity

This value is the back side long-wave hemispherical emissivity of the slat material. The minimum value is 0.0, the maximum value is less than 1.0.  The default value is 0.9.

#### Field: Slat Angle Control

This input field is used only if slat angle control is desired.  The three key choice inputsallowed are "FixedSlatAngle", "MaximizeSolar", and "BlockBeamSolar".  The default value is "FixedSlatAngle". If Type of Slat Angle Control for Blinds = MaximizeSolar the slat angle is adjusted to maximize solar gain.  If Type of Slat Angle Control for Blinds = BlockBeamSolar, the slat angle is adjusted to maximize visibiity while eliminating beam solar radiation.  If Type of Slat Angle Control for Blinds = FixedSlatAngle, then the model uses a fixed slat angle specified above.

An IDF example for this object, is shown below:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Blind:EquivalentLayer,
      VBU8D6+45SW1,      ! - Name
      Horizontal,        ! - Slat Orientation
      0.025,             ! - Slat Width
      0.025,             ! - Slat Separation
      0.0,               ! - Slat Crown
      45.0,              ! - Slat Angle
      0.0,               ! - Front Side Slat Beam-Diffuse Solar Transmittance
      0.0,               ! - Back Side Slat Beam-Diffuse Solar Transmittance
      0.0,               ! - Front Side Slat Beam-Diffuse Solar Reflectance
      0.0,               ! - Back Side Slat Beam-Diffuse Solar Reflectance
      0.0,               ! - Front Side Slat Beam-Diffuse Visible Transmittance
      0.0,               ! - Back Side Slat Beam-Diffuse Visible Transmittance
      0.0,               ! - Front Side Slat Beam-Diffuse Visible Reflectance
      0.0,               ! - Back Side Slat Beam-Diffuse Visible  Reflectance
      0.0,               ! - Slat Diffuse-Diffuse Solar Transmittance
      0.80,              ! - Front Side Slat Diffuse-Diffuse Solar Reflectance
      0.60,              ! - Back Side Slat Diffuse-Diffuse Solar Reflectance
      0.0,               ! - Slat Diffuse-Diffuse Visible Transmittance
      0.0,               ! - Front Side Slat Diffuse-Diffuse Visible Reflectance
      0.0,               ! - Back Side Slat Diffuse-Diffuse Visible Reflectance
      0.0,               ! - Slat Infrared Transmittance
      0.90,              ! - Front Side Slat Infrared Emissivity
      0.90,              ! - Back Side Slat Infrared Emissivity
      FixedSlatAngle;    ! - Slat Angle Control
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Screen:EquivalentLayer

This object specifies the optical and thermal properties of exterior screen materials for Equivalent Layer [Window](#window). Can only be placed on the exterior side of window construction. The window screen model assumes the screen is made up of intersecting orthogonally-crossed cylinders. The surface of the cylinders is assumed to be diffusely reflecting. The beam solar radiation transmitted through an equivalent Layer window screen varies with sun angle and is made up of two distinct elements: a beam-beam component and a beam-difuse component. The beam-beam transmittance component is calculated using screen openness area fraction determined from the geometry of the screen and the incident angle of the sun. Empirical correlations are used to obtain the effective off-normal solar and longwave properties of insect screens.  Insect screen geometry is shown in Figure 23.  The calculation of effective solar properties requires a set of properties measured at normal incidence.

![Geometry used for insect screen analysis](media/geometry-used-for-insect-screen-analysis.png)


The formulation of the model, assumption and correlations used to calculate effective solar and longwave properties of insect screens are described in the Engineering Reference.

### Inputs

#### Field: Name

Name of the insect screen. It is referenced as an outside layer in an equivalent layer window construction.

#### Field: Screen Beam-Beam Solar Transmittance

This value is the beam-beam transmittance of the screen material at normal incidence. This value is the same as the screen oppenness area fraction.  This value can be autocalculated from the wire spacing and wire diameter. It is the same for both sides of the screen. The minimum value is 0.0, and maximum value is less than 1.0.

#### Field: Screen Beam-Diffuse Solar Transmittance

This value is the beam-diffuse solar transmittance of the screen material at normal incidence averaged over the entire spectrum of solar radiation. Assumed to be the same for both sides of the screen. The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Screen Beam-Diffuse Solar Reflectance

This value is the beam-diffuse solar reflectance of the screen material at normal incidence averaged over the entire spectrum of solar radiation. Assumed to be the same for both sides of the screen. The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Screen Beam-Beam Visible Transmittance

This value is the beam-beam visible transmittance of the screen material at normal incidence averaged over the visible spectrum range of solar radiation.  Assumed to be the same for both sides of the screen. The minimum value is 0.0, and maximum value is less than 1.0. This input input field is not used currently.

#### Field: Screen Beam-Diffuse Visible Transmittance

This value is the beam-diffuse visible reflectance of the screen material at normal incidence averaged over the visible spectrum range of solar radiation. Assumed to be the same for both sides of the screen. The minimum value is 0.0, and the maximum value is less than 1.0. This input input field is not used currently.

#### Field: Screen Beam-Diffuse Visible Reflectance

This value is the beam-diffuse visible reflectance of the screen material at normal incidence averaged over the visible spectrum range of solar radiation. Assumed to be the same for both sides of the screen. The minimum value is 0.0, and the maximum value is less than 1.0. This input input field is not used currently.

#### Field: Screen Infrared Transmittance

This value is the long-wave hemispherical transmittance of the the screen material. Assumed to be the same for both sides of the screen material. The minimum value is 0.0, the maximum value is less than 1.0.  The default value is 0.02

#### Field: Screen Infrared Emissivity

This value is the long-wave hemispherical emissivity of the screen material. Assumed to be the same for both sides of the screen material. The minimum value is 0.0, the maximum value is less than 1.0. The default value is 0.93.

#### Field: Screen Wire Spacing

The spacing, S (m), of the screen material is the distance from the center of one strand of screen to the center of the adjacent one. The spacing of the screen material is assumed to be the same in both directions (e.g., vertical and horizontal). This input value must be greater than the non-zero screen material diameter. If the spacing is different in the two directions, use the average of the two values. Default value is 0.0025m.

#### Field: Screen Wire Diameter

The diameter, D (m), of individual strands or wires of the screen material. The screen material diameter is assumed to be the same in both directions (e.g., vertical and horizontal). This input value must be greater than 0 and less than the screen wire spacing. If the diameter is different in the two directions, use the average of the two values. Default value is 0.005m.

An IDF example for this object, is shown below:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Screen:EquivalentLayer,
      INSCRN,                !- Name
      0.763,                 !- Screen Beam-Beam Solar Transmittance
      0.052,                 !- Screen Beam-Diffuse Solar Transmittance
      0.076,                 !- Screen Beam-Diffuse Solar Reflectance
      0.0,                   !- Screen Beam-Beam Visible Transmittance
      0.0,                   !- Screen Beam-Diffuse Visible Transmittance
      0.0,                   !- Screen Beam-Diffuse Visible Reflectance
      0.0,                   !- Screen Infrared Transmittance
      0.84,                  !- Screen Infrared Emissivity
      0.025,                 !- Screen Wire Spacing
      0.005;                 !- Screen Wire Diameter
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Glazing:EquivalentLayer

Glass material properties for equivalent layer window model.  Uses transmittance/reflectance input method.  For exterior windows, "front side" is the side of the glass closest to the outside air and "back side" is the side closest to the zone the window is defined in. For interzone windows, "front side" is the side closest to the zone adjacent to the zone the window is defined in and "back side" is the side closest to the zone the window is defined in.

### Inputs

#### Field: Name

The name of the glass layer. It corresponds to a layer in an equivalent layer window construction.

#### Field: Optical Data Type

Valid values for this field are SpectralAverage, or Spectral. If Optical Data Type = SpectralAverage, the values you enter for solar transmittance and reflectance are assumed to be averaged over the solar spectrum, and the values you enter for visible transmittance and reflectance are assumed to be averaged over the solar spectrum and weighted by the response of  the human eye. The spectral data input is not supported now.

**Field: [Window](#window) Glass Spectral Data Set Name**

This input field is not used currently.

#### Field: Front Side Beam-Beam Solar Transmittance

This value is the front side beam-beam solar transmittance of the glazing at normal incidence averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Back Side Beam-Beam Solar Transmittance

This value is the back side beam-beam solar transmittance of the glazing at normal incidence averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Front Side Beam-Beam Solar Reflectance

This value is the front side beam-beam solar reflectance of the glazing at normal incidence averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Back Side Beam-Beam Solar Reflectance

This value is the back side beam-beam solar reflectance of the glazing at normal incidence averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Front Side Beam-Beam Visible Transmittance

This value is the front side beam-beam visible transmittance of the glazing at normal incidence averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Back Side Beam-Beam Visible Transmittance

This value is the back side beam-beam visible transmittance of the glazing at normal incidence averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Front Side Beam-Beam Visible Reflectance

This value is the front side beam-beam visible reflectance of the glazing at normal incidence averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Back Side Beam-Beam Visible Reflectance

This value is the back side beam-beam visible reflectance of the glazing at normal incidence averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Front Side Beam-Diffuse Solar Transmittance

This value is the front side beam-diffuse solar transmittance of the glazing at normal incidence averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage.  For clear glazing the beam-diffuse transmittance is zero. The minimum value is 0.0, and the maximum value is less than 1.0. Default value is 0.0.

#### Field: Back Side Beam-Diffuse Solar Transmittance

This value is the back side beam-diffuse solar transmittance of the glazing at normal incidence averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage.  For clear glazing the beam-diffuse solar transmittance is zero. The minimum value is 0.0, and the maximum value is less than 1.0. Default value is 0.0.

#### Field: Front Side Beam-Diffuse Solar Reflectance

This value is the front side beam-diffuse solar reflectance of the glazing at normal incidence averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0. Default value is 0.0.

#### Field: Back Side Beam-Diffuse Solar Reflectance

This value is the back side beam-diffuse solar reflectance of the glazing at normal incidence averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0. Default value is 0.0.

#### Field: Front Side Beam-Diffuse Visible Transmittance

This value is the front side beam-diffuse visible transmittance of the glazing at normal incidence averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage.  For clear glazing the beam-diffuse visible transmittance is zero. The minimum value is 0.0, and the maximum value is less than 1.0. Default value is 0.0.  This input field is not used currently.

#### Field: Back Side Beam-Diffuse Visible Transmittance

This value is the back side beam-diffuse visible transmittance of the glazing at normal incidence averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage.  For clear glazing the beam-diffuse visible transmittance is zero. The minimum value is 0.0, and the maximum value is less than 1.0. Default value is 0.0.  This input field is not used currently.

#### Field: Front Side Beam-Diffuse Visible Reflectance

This value is the front side beam-diffuse visible reflectance of the glazing at normal incidence averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0. Default value is 0.0. This input field is not used currently.

#### Field: Back Side Beam-Diffuse Visible Reflectance

This value is the back side beam-diffuse visible reflectance of the glazing at normal incidence averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage.  The minimum value is 0.0, and the maximum value is less than 1.0. Default value is 0.0. This input field is not used currently.

#### Field: Diffuse-Diffuse Solar Transmittance

This value is the diffuse-diffuse solar transmittance of the glazing averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage. The diffuse-diffuse transmittance is assumed to be the same for both sides of the glazing.  EnergyPlus automatically estimates the diffuse-diffuse solar transmittance from other inputs. If this input field is specified as "Autocalculate", then the calculated transmittance will be used. The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Front Side Diffuse-Diffuse Solar Reflectance

This value is the front side diffuse-diffuse solar reflectance of the glazing averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage. EnergyPlus automatically estimates the diffuse-diffuse reflectance from other inputs. If this input field is specified as "Autocalculate", then the calculated reflectance will be used. The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Back Side Diffuse-Diffuse Solar Reflectance

This value is the back side diffuse-diffuse solar reflectance of the glazing averaged over the entire spectrum of solar radiation.  Used only when Optical Data Type = SpectralAverage. EnergyPlus automatically estimates the diffuse-diffuse reflectance from other inputs. If this input field is specified as "Autocalculate", then the calculated reflectance will be used. The minimum value is 0.0, and the maximum value is less than 1.0.

#### Field: Diffuse-Diffuse Visible Solar Transmittance

This value is the diffuse-diffuse visible transmittance of the glazing averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage. The diffuse-diffuse visible transmittance is assumed to be the same for both sides of the glazing.  If this input field is specified as "Autocalculate", then the calculated transmittance will be used. The minimum value is 0.0, and the maximum value is less than 1.0. This input field is not used currently.

#### Field: Front Side Diffuse-Diffuse Visible Reflectance

This value is the front side diffuse-diffuse visible reflectance of the glazing averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage. EnergyPlus automatically estimates the front side diffuse-diffuse visible reflectance from front side beam-beam visible reflectance at normal incidence specified above. If this input field is specified as "Autocalculate", then the calculated reflectance will be used. The minimum value is 0.0, and the maximum value is less than 1.0. This input field is not used currently.

#### Field: Back Side Diffuse-Diffuse Visible Reflectance

This value is the back side diffuse-diffuse visible reflectance of the glazing averaged over the visible spectrum range of solar radiation.  Used only when Optical Data Type = SpectralAverage. EnergyPlus automatically estimates the back side diffuse-diffuse visible reflectance from back side beam-beam visible reflectance at normal incidence specified above. If this input field is specified as "Autocalculate", then the calculated reflectance will be used. The minimum value is 0.0, and the maximum value is less than 1.0. This input field is not used currently.

#### Field: Infrared Transmittance (applies to front and back)

This value is the long-wave hemispherical transmittance of the glazing. Assumed to be the same for both sides of the glazing. The minimum value is 0.0, the maximum value is less than 1.0.  The default value is 0.0.

#### Field: Front Side Infrared Emissivity

This value is the front side long-wave hemispherical emissivity of the glazing. The minimum value is 0.0, the maximum value is less than 1.0.  The default value is 0.84.

#### Field: Back Side Infrared Emissivity

This value is the back side long-wave hemispherical emissivity of the glazing. The minimum value is 0.0, the maximum value is less than 1.0.  The default value is 0.84.

An IDF example for this object, is shown below:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Glazing:EquivalentLayer,
      GLZCLR,                  !- Name
      SpectralAverage,         !- Optical Data Type
      ,                        !- Window Glass Spectral Data Set Name
      0.83,                    !- Front Side Beam-Beam Solar Transmittance
      0.83,                    !- Back Side Beam-Beam Solar Transmittance
      0.08,                    !- Front Side Beam-Beam Solar Reflectance
      0.08,                    !- Back Side Beam-Beam Solar Reflectance
      0.0,                     !- Front Side Beam-Beam Visible Transmittance
      0.0,                     !- Back Side Beam-Beam Visible Transmittance
      0.0,                     !- Front Side Beam-Beam Visible Reflectance
      0.0,                     !- Back Side Beam-Beam Visible Reflectance
      0.0,                     !- Front Side Beam-Diffuse Solar Transmittance
      0.0,                     !- Back Side Beam-Diffuse Solar Transmittance
      0.0,                     !- Front Side Beam-Diffuse Solar Reflectance
      0.0,                     !- Back Side Beam-Diffuse Solar Reflectance
      0.0,                     !- Front Side Beam-Diffuse Visible Transmittance
      0.0,                     !- Back Side Beam-Diffuse Visible Transmittance
      0.0,                     !- Front Side Beam-Diffuse Visible Reflectance
      0.0,                     !- Back Side Beam-Diffuse Visible Reflectance
      0.76,                    !- Diffuse-Diffuse Solar Transmittance
      0.14,                    !- Front Side Diffuse-Diffuse Solar Reflectance
      0.14,                    !- Back Side Diffuse-Diffuse Solar Reflectance
      0.0,                     !- Diffuse-Diffuse Visible Transmittance
      0.0,                     !- Front Side Diffuse-Diffuse Visible Reflectance
      0.0,                     !- Back Side Diffuse-Diffuse Visible Reflectance
      0.0,                     !- Infrared Transmittance
      0.84,                    !- Front Side Infrared Emissivity
      0.84;                    !- Back Side Infrared Emissivity
~~~~~~~~~~~~~~~~~~~~

## WindowMaterial:Gap:EquivalentLayer

This object is used in windows equivalent layer construction object and specifies the properties of the gap between the layers in multi-layer equivalent layer window object. There is an EnergyPlus Reference Data Set for Material:WindowGas that contains several types of gas. This object uses the gas types: Air, Argon, Xenon, Crypton, and Custom.  For Custom gas type users are required to entering the thermophicial properties.

### Inputs

#### Field: Name

The name of the gap. It refers to a layer in a window construction equivalent layer.

#### Field: Gas Type

The type of gas. The choices allowed are AIR, ARGON, XENON, KRYPTON, or CUSTOM.

#### Field: Thickness

The thickness (m) of the gap layer.

#### Field: Gap Vent Type

This inputfiled contains the valid key choice for gap vent type.  The valid vent types are: Sealed, VentedIndoor, and VentedOutdoor.  Sealed means the gap is enclosed and gas tight, i.e., no venting to indoor or outdoor environment. The gap types "VentedIndoor" and "VentedOutdoor" are used with gas type "Air" only. VentedIndoor means the air in the gap is naturally vented to indoor environment, and  VentedOutdoor means the air in the gap is naturally vented to the outdoor environment.

#### Properties for Custom Gas Types

The following entries are used only if Gas Type = Custom. The A, B and C coefficients are those in the following expression that gives a property value as a function of temperature in degrees K:

![](media/image48.png)\


#### Field: Conductivity Coefficient A

The A coefficient for gas conductivity (W/m-K). Used only if Gas Type = Custom.

#### Field: Conductivity Coefficient B

The B coefficient for gas conductivity (W/m-K^2^). Used only if Gas Type = Custom.

#### Field: Conductivity Coefficient C

The C coefficient for gas conductivity (W/m-K^3^).  Used only if Gas Type = Custom.

#### Field: Viscosity Coefficient A

The A coefficient for gas viscosity (kg/m-s). Used only if Gas Type = Custom.

#### Field: Viscosity Coefficient B

The B coefficient for gas viscosity (kg/m-s-K). Used only if Gas Type = Custom.

#### Field: Viscosity Coefficient C

The C coefficient for gas viscosity (kg/m-s-K^2^).  Used only if Gas Type = Custom.

#### Field: Specific Heat Coefficient A

The A coefficient for gas specific heat (J/kg-K). Used only if Gas Type = Custom.

#### Field: Specific Heat Coefficient B

The B coefficient for gas specific heat (J/kg-K^2^). Used only if Gas Type = Custom.

#### Field: Specific Heat Coefficient C

The C coefficient for gas specific heat (J/kg-K^2^).  Used only if Gas Type = Custom.

#### Field: Specific Heat Ratio

The specific heat ratio for gas.  Used only if Gas Type = Custom.

#### Field: Molecular Weight

The molecular weight for gas.  The molecular weight is the mass of 1 mol of the substance.  This has a numerical value which is the average molecular mass of the molecules in the substance multiplied by Avogadro's constant. (kg/kmol) (Shown in the IDD as g/mol for consistency)

#### Field: Specific Heat Ratio

The specific heat ratio for gas.  The specific heat ratio of a gas is the ratio of the specific heat at contant pressure, to the specific heat at constant volume.  Used only if Gas Type = Custom.

An IDF example for this object, is shown below:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Gap:EquivalentLayer,
      Custom CO2 Sealed 12mm,    !- Name
      CUSTOM,                    !- Gas Type
      0.0120,                    !- Thickness {m}
      Sealed,                    !- Gap Vent Type
     -5.8181E-3,                 !- Conductivity Coefficient A {W/m-K}
      7.4714E-5,                 !- Conductivity Coefficient B {W/m-K2}
      0.0,                       !- Conductivity Coefficient C {W/m-K3}
      8.5571E-7,                 !- Viscosity Coefficient A {kg/m-s}
      4.7143E-8,                 !- Viscosity Coefficient B {kg/m-s-K}
      0.0,                       !- Viscosity Coefficient C {kg/m-s-K2}
      5.76903E2,                 !- Specific Heat Coefficient A {J/kg-K}
      9.18088E-2,                !- Specific Heat Coefficient B {J/kg-K2}
      0.0,                       !- Specific Heat Coefficient C {J/kg-K3}
      44.01;                     !- Molecular Weight {g/mol}
~~~~~~~~~~~~~~~~~~~~

## Material:RoofVegetation

This definition must be used in order to simulate the green roof (ecoroof) model. The material becomes the outside layer in a green roof construction (see example below). In the initial release of the green roof model, only one material may be used as a green roof layer though, of course, several constructions using that material may be used. In addition, the model has only been tested with the ConductionTransferFunction solution algorithm – a warning will be issued for other solution algorithm choices. This model was developed for low-sloped exterior surfaces (roofs). It is not recommended for high-sloped exterior surfaces (e.g., walls).

### Inputs

#### Field: Name

This field is a unique reference name that the user assigns to a particular ecoroof material. This name can then be referred to by other input data.

#### Field: Height of Plants

This field defines the height of plants in units of meters. This field is limited to values in the range 0.005 < Height < 1.00 m. Default is .2 m.

#### Field: Leaf Area Index

This is the projected leaf area per unit area of soil surface. This field is dimensionless and is limited to values in the range of 0.001 < LAI < 5.0. Default is 1.0. At the present time the fraction vegetation cover is calculated directly from LAI (Leaf Area Index) using an empirical relation. The user may find it necessary to increase the specified value of LAI in order to represent high fractional coverage of the surface by vegetation.

#### Field: Leaf Reflectivity

This field represents the fraction of incident solar radiation that is reflected by the individual leaf surfaces (albedo). Solar radiation includes the visible spectrum as well as infrared and ultraviolet wavelengths. Values for this field must be between 0.05 and 0.5. Default is .22. Typical values are .18 to .25.

#### Field: Leaf Emissivity

This field is the ratio of thermal radiation emitted from leaf surfaces to that emitted by an ideal black body at the same temperature. This parameter is used when calculating the long wavelength radiant exchange at the leaf surfaces. Values for this field must be between 0.8 and 1.0 (with 1.0 representing "black body" conditions). Default is .95.

#### Field: Minimum Stomatal Resistance

This field represents the resistance of the plants to moisture transport. It has units of s/m. Plants with low values of stomatal resistance will result in higher evapotranspiration rates than plants with high resistance. Values for this field must be in the range of 50.0 to 300.0. Default is 180.

#### Field: Soil Layer Name

This field is a unique reference name that the user assigns to the soil layer for a particular ecoroof. This name can then be referred to by other input data. Default is **Green [Roof](#roof) Soil**.

#### Field: Roughness

This alpha field defines the relative roughness of a particular material layer. This parameter only influences the convection coefficients, more specifically the exterior convection coefficient. A keyword is expected in this field with the options being "VeryRough", "Rough", "MediumRough", "MediumSmooth", "Smooth", and "VerySmooth" in order of roughest to smoothest options. Default is MediumRough.

#### Field: Thickness

This field characterizes the thickness of the material layer in meters. This should be the dimension of the layer in the direction perpendicular to the main path of heat conduction. This value must be a positive number. Depths of .10m (4 inches) and .15m (6 inches) are common. Default if this field is left blank is .1. Maximum is .7m. Must be greater than .05 m.

#### Field: Conductivity of Dry Soil

This field is used to enter the thermal conductivity of the material layer. Units for this parameter are W/(m-K). Thermal conductivity must be greater than zero. Typical soils have values from .3 to .5. Minimum is .2 (specified in IDD). Default is .35 and maximum (in IDD) is 1.5.

#### Field: Density of Dry Soil

This field is used to enter the density of the material layer in units of kg/m^3^. Density must be a positive quantity. Typical soils range from 400 to 1000 (dry to wet). Minimum is 300, maximum is 2000 and default if field is left blank is 1100.

#### Field: Specific Heat of Dry Soil

This field represents the specific heat of the material layer in units of J/(kg-K). Note that these units are most likely different than those reported in textbooks and references which tend to use kJ/(kg-K) or J/(g-K). They were chosen for internal consistency within EnergyPlus. Only positive values of specific heat are allowed.

#### Field: Thermal Absorptance

The thermal absorptance field in the [Material](#material-and-material-properties) input syntax represents the fraction of incident long wavelength radiation that is absorbed by the material. This parameter is used when calculating the long wavelength radiant exchange between various surfaces and affects the surface heat balances (both inside and outside as appropriate). For long wavelength radiant exchange, thermal emissivity and thermal emittance are equal to thermal absorptance. Values for this field must be between 0.0 and 1.0 (with 1.0 representing "black body" conditions). Typical values are from .9 to .98.

#### Field: Solar Absorptance

The solar absorptance field in the [Material](#material-and-material-properties) input syntax represents the fraction of incident  solar radiation that is absorbed by the material. Solar radiation includes the visible spectrum as well as infrared and ultraviolet wavelengths. This parameter is used when calculating the amount of incident solar radiation absorbed by various surfaces and affects the surface heat balances (both inside and outside as appropriate). If solar reflectance (or reflectivity) data is available, then absorptance is equal to 1.0 minus reflectance (for opaque materials). Values for this field must be between 0.0 and 1.0. Typical values are from .6 to .85.

#### Field: Visible Absorptance

The visible absorptance field in the [Material](#material-and-material-properties) input syntax represents the fraction of incident visible wavelength radiation that is absorbed by the material. Visible wavelength radiation is slightly different than solar radiation in that the visible band of wavelengths is much more narrow while solar radiation includes the visible spectrum as well as infrared and ultraviolet wavelengths. This parameter is used when calculating the amount of incident visible radiation absorbed by various surfaces and affects the surface heat balances (both inside and outside as appropriate) as well as the daylighting calculations. If visible reflectance (or reflectivity) data is available, then absorptance is equal to 1.0 minus reflectance (for opaque materials). Values for this field must be between 0.5 and 1.0.

#### Field: Saturation Volumetric Moisture Content of the Soil Layer

The field allows for user input of the saturation moisture content of the soil layer. Maximum moisture content is typically less than .5. Range is [.1,.5] with the default being .3.

#### Field: Residual Volumetric Moisture Content of the Soil Layer

The field allows for user input of the residual moisture content of the soil layer. Default is .01, range is [.01,.1].

#### Field: Initial Volumetric Moisture Content of the Soil Layer

The field allows for user input of the initial moisture content of the soil layer. Range is (.05, .5] with the defaulte being .1.

#### Field: Moisture Diffusion Calculation Method

The field allows for two models to be selected: **Simple** or **Advanced**.

**Simple** is the original Ecoroof model - based on a constant diffusion of moisture through the soil.  This model starts with the soil in two layers.  Every time the soil properties update is called, it will look at the two soils moisture layers and asses which layer has more moisture in it. It then takes moisture from the higher moisture layer and redistributes it to the lower moisture layer at a constant rate.

**Advanced** is the later Ecoroof model. If you use it, you will need to increase your number of timesteps in hour for the simulation with a recommended value of 20. This moisture transport model is based on a project which looked at the way moisture transports through soil.  It uses a finite difference method to divide the soil into layers (nodes). It redistributes the soil moisture according the model described in:

Marcel G Schaap and Martinus Th. van Genuchten, 2006, 'A modified Maulem-van Genuchten Formulation for Improved Description of the Hydraulic Conductivity Near Saturation', Vadose [Zone](#zone) Journal 5 (1), p 27-34.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

      Material:RoofVegetation,
        BaseEco,                 !- Name
        0.5,                     !- Height of Plants {m}
        5,                       !- Leaf Area Index {dimensionless}
        0.2,                     !- Leaf Reflectivity {dimensionless}
        0.95,                    !- Leaf Emissivity
        180,                     !- Minimum Stomatal Resistance {s/m}
        EcoRoofSoil,             !- Soil Layer Name
        MediumSmooth,            !- Roughness
        0.18,                    !- Thickness {m}
        0.4,                     !- Conductivity of Dry Soil {W/m-K}
        641,                     !- Density of Dry Soil {kg/m3}
        1100,                    !- Specific Heat of Dry Soil {J/kg-K}
        0.95,                    !- Thermal Absorptance
        0.8,                     !- Solar Absorptance
        0.7,                     !- Visible Absorptance
        0.4,                     !- Saturation Volumetric Moisture Content of the Soil Layer
        0.01,                    !- Residual Volumetric Moisture Content of the Soil Layer
        0.2,                     !- Initial Volumetric Moisture Content of the Soil Layer
        Advanced;                !- Moisture Diffusion Calculation Method

      Material:RoofVegetation,
        LowLAI,                  !- Name
        0.5,                     !- Height of Plants {m}
        0.5,                     !- Leaf Area Index {dimensionless}
        0.2,                     !- Leaf Reflectivity {dimensionless}
        0.95,                    !- Leaf Emissivity
        180,                     !- Minimum Stomatal Resistance {s/m}
        EcoRoofSoil,             !- Soil Layer Name
        MediumSmooth,            !- Roughness
        0.18,                    !- Thickness {m}
        0.4,                     !- Conductivity of Dry Soil {W/m-K}
        641,                     !- Density of Dry Soil {kg/m3}
        1100,                    !- Specific Heat of Dry Soil {J/kg-K}
        0.95,                    !- Thermal Absorptance
        0.8,                     !- Solar Absorptance
        0.7,                     !- Visible Absorptance
        0.4,          !- Saturation Volumetric Moisture Content of the Soil Layer
        0.01,         !- Residual Volumetric Moisture Content of the Soil Layer
        0.2,          !- Initial Volumetric Moisture Content of the Soil Layer
        Advanced;                !- Moisture Diffusion Calculation Method
~~~~~~~~~~~~~~~~~~~~

And construction using the ecoroof material:

~~~~~~~~~~~~~~~~~~~~

    Construction,
        ASHRAE 90.1-2004_Sec 5.5-2_Roof,  !- Name
        BaseEco,                 !- Outside Layer
        ASHRAE 90.1-2004_Sec 5.5-2_Roof Insulation_1,  !- Layer #2
        ASHRAE 90.1-2004_Sec 5.5-2_MAT-METAL;  !- Layer #3
~~~~~~~~~~~~~~~~~~~~

## Ecoroof / RoofVegetation outputs

The following outputs are available for the [Roof](#roof) Vegetation surface.

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Green Roof Soil Temperature [C]
    Zone,Average,Green Roof Vegetation Temperature [C]
    Zone,Average,Green Roof Soil Root Moisture Ratio []
    Zone,Average,Green Roof Soil Near Surface Moisture Ratio []
    Zone,Average,Green Roof Soil Sensible Heat Transfer Rate per Area [W/m2]
    Zone,Average,Green Roof Vegetation Sensible Heat Transfer Rate per Area [W/m2]
    Zone,Average,Green Roof Vegetation Moisture Transfer Rate [m/s]
    Zone,Average,Green Roof Soil Moisture Transfer Rate [m/s]
    Zone,Average,Green Roof Vegetation Latent Heat Transfer Rate per Area [W/m2]
    Zone,Average,Green Roof Soil Latent Heat Transfer Rate per Area [W/m2]
    Zone,Sum,Green Roof Cumulative Precipitation Depth [m]
    Zone,Sum,Green Roof Cumulative Irrigation Depth [m]
    Zone,Sum,Green Roof Cumulative Runoff Depth [m]
    Zone,Sum,Green Roof Cumulative Evapotranspiration Depth [m]
    Zone,Sum,Green Roof Current Precipitation Depth [m]
    Zone,Sum,Green Roof Current Irrigation Depth [m]
    Zone,Sum,Green Roof Current Runoff Depth [m]
    Zone,Sum,Green Roof Current Evapotranspiration Depth [m]
~~~~~~~~~~~~~~~~~~~~

### Green Roof Soil Temperature [C]

Temperature of the Soil layer temperature in C.

### Green Roof Vegetation Temperature [C]

Temperature of the Vegetation layer temperature in C.

### Green Roof Soil Root Moisture Ratio []

Mean value of root moisture (m^3^/m^3^)

### Green Roof Soil Near Surface Moisture Ratio []

The moisture content in the soil near the surface (m^3^/m^3^)

### Green Roof Soil Sensible Heat Transfer Rate per Area [W/m2]

Sensible heat flux to ground (W/m^2^)

### Green Roof Vegetation Sensible Heat Transfer Rate per Area [W/m2]

Sensible heat transfer to foliage (W/m^2^)

### Green Roof Vegetation Moisture Transfer Rate [m/s]

Water evapotranspiration rate associated with latent heat from vegetation (m/s)

### Green Roof Soil Moisture Transfer Rate [m/s]

Water evapotranspiration rate associated with latent heat from ground surface (m/s)

### Green Roof Vegetation Latent Heat Transfer Rate per Area [W/m2]

Latent heat flux from vegetation (W/m^2^)

### Green Roof Soil Latent Heat Transfer Rate per Area [W/m2]

Latent heat flux from ground surface (W/m^2^)

### Green Roof Cumulative Precipitation Depth [m]

### Green Roof Current Precipitation Depth [m]

Cumulative or current precipitation (m)

### Green Roof Cumulative Irrigation Depth [m]

### Green Roof Current Irrigation Depth [m]

Cumulative or current irrigation (m)

### Green Roof Cumulative Runoff Depth [m]

### Green Roof Current Runoff Depth [m]

Cumulative or current runoff (m). Multiply by roof area to get volume.

### Green Roof Cumulative Evapotranspiration Depth [m]

### Green Roof Current Evapotranspiration Depth [m]

Cumulative or current evapotranspiration from soil and plants (m).

## MaterialProperty:GlazingSpectralData

With the [MaterialProperty:GlazingSpectralData](#materialpropertyglazingspectraldata) object, you can specify the wavelength-by-wavelength transmittance and reflectance properties of a glass material. To determine the  overall optical properties of a glazing system (solar and visible transmittance and solar absorptance vs. angle of incidence) EnergyPlus first calculates transmittance and absorptance vs. angle of incidence for each wavelength. This is then weighted by a standard solar spectrum to get the solar transmittance and absorptance vs. angle of incidence (for use in the solar heat gain calculations), and further weighted by the response of the human eye to get the visible transmittance vs. angle of incidence (for use in the daylighting calculation).

[MaterialProperty:GlazingSpectralData](#materialpropertyglazingspectraldata) should be used for multi-pane windows when one or more of the glass layers is *spectrally selective*, i.e., the transmittance depends strongly on wavelength. An example is glass with a coating that gives high transmittance in the daylight part of the solar spectrum (roughly 0.4 to 0.7 microns) and low transmittance at longer wavelengths, thus providing better solar heat gain control than uncoated glass. If spectral data is not used in case, the overall optical properties of the glazing system that EnergyPlus calculates will not be correct.

You can input up to 450 sets of values for wavelengths covering the solar spectrum. Each set consists of  {wavelength (microns), transmittance, front reflectance, back reflectance}

Spectral data of this kind are routinely measured by glass manufacturers. Data sets for over 800 commercially available products are contained in an Optical Data Library maintained by the Windows Group at Lawrence Berkeley National Laboratory. This library can be downloaded from http://windows.lbl.gov/. You will have to edit entries from this library to put them in the format required by the EnergyPlus WindowGlassSpectralData object.

An alternative to using the [MaterialProperty:GlazingSpectralData](#materialpropertyglazingspectraldata) object is to run the WINDOW window analysis program. This program has built-in access to the Optical Data Library and let's you easily create customized, multi-layer glazing systems that can be exported for use in EnergyPlus. For more details, see "StormWindow".

### Inputs

#### Field: Name

The name of the spectral data set. It is referenced by [WindowMaterial:Glazing](#windowmaterialglazing) when Optical Data Type = Spectral.

#### Fields 1-4 (repeated up to 450 times)

Sets of values for wavelengths covering the solar spectrum (from about 0.25 to 2.5 microns [10^-6^ m]). Each set consists of

**{wavelength (microns), transmittance, front reflectance, back reflectance}**

The wavelength values must be in ascending order. The transmittance and reflectance values are at normal incidence. "Front reflectance" is the reflectance for radiation striking the glass from the outside, i.e., from the side opposite the zone in which the window is defined. "Back reflectance" is the reflectance for radiation striking the glass from the inside, i.e., from the zone in which the window is defined. Therefore, for exterior windows, "front" is the side closest to the outdoors and "back" is the side closest to the zone in which the window is defined. For interior windows, "front" is the side closest to the adjacent zone and "back" is the side closest to the zone in which the window is defined.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    MaterialProperty:GlazingSpectralData,
          TestSpectralDataSet,
           ! { from WINDOW 4 library }
           ! { actual 9.91mm clear laminate: 15_mil PVB, ID:37966/50032-39-9 } 10.38
           ! { conductivity PVB adjusted, W/M/K  } 0.798
           ! { thermal IR transmittance, assumed } tir=0.00
           ! { thermal IR hemispherical emittance, assumed } emis= 0.84 0.84

           ! WL   T     Rfront Rback
           .300, 0.000, 0.045, 0.045,
           .310, 0.000, 0.044, 0.044,
           .320, 0.000, 0.044, 0.044,
           .330, 0.000, 0.042, 0.042,
           .340, 0.000, 0.041, 0.041,
           .350, 0.000, 0.040, 0.040,
    <snip>
          2.450, 0.200, 0.040, 0.040,
          2.500, 0.214, 0.039, 0.039;
~~~~~~~~~~~~~~~~~~~~

## Construction

For walls, roofs, floors, windows, and doors, constructions are "built" from the included materials. Each layer of the construction is a material name listed in order from "outside" to "inside". Up to ten layers (eight for windows) may be specified (one of the few limitations in EnergyPlus!). "Outside" is the layer furthest away from the [Zone](#zone) air (not necessarily the outside environment). "Inside" is the layer next to the [Zone](#zone) air. In the example floor below, for example, the outside layer is the acoustic tile below the floor, the next layer is the air space above the tile, and the inside layer is the concrete floor deck.

![Example Floor Construction illustration.](media/example-floor-construction-illustration..png)


[Window](#window) constructions are similarly built up from items in the [Window](#window) Materials set using similar layers.. See Figure 25. Illustration for material ordering in windows, which shows the case where an interior shading layer such as a blind is present. The gap between the inside glass layer (layer #3) and the interior shading layer is not entered. Similarly, for an exterior shading layer, the gap between the outside glass layer and the shading layer is not entered.

![Illustration for material ordering in windows.](media/illustration-for-material-ordering-in.png)


However, for a between-glass shading device the gaps on either side of the shading layer must be entered and they must have the same gas type. In addition, the gap widths with and without the between-glass shading layer must be consistent (see Figure 26).

A maximum of four glass layers and one shading layer is allowed. A gas layer must always separate adjacent glass layers in a multi-pane glazing without a between-glass shading layer.

![Window construction with and without a between-glass shading layer. Shown are gap widths g, g~1~ and g~2~, and shading layer width, w. An error will result if g~1~+g~2~+w is not equal to g, where w is zero for a blind and greater than zero for a shade.](media/window-construction-with-and-without-a.png)


Outside and inside air film resistances are never given as part of a construction definitions since they are calculated during the EnergyPlus simulation. Note also that constructions are assumed to be one-dimensional in a direction perpendicular to the surface.

### Inputs

#### Field: Name

This field is a user specified name that will be used as a reference by other input syntax. For example, a heat transfer surface (ref: [Building](#building) Surfaces) requires a construction name to define what the make-up of the wall is. This name must be identical to one of the [Construction](#construction) definitions in the input data file.

#### Field: Outside Layer

~~~~~~~~~~~~~~~~~~~~

    Each construction must have at least one layer. This field defines the material name associated with the layer on the outside of the construction—outside referring to the side that is not exposed to the zone but rather the opposite side environment, whether this is the outdoor environment or another zone. Material layers are defined based on their thermal properties elsewhere in the input file (ref: Material and Material Properties and Zone,Average,HAMT Surface Average Water Content Ratio [kg/kg]
    Zone,Average,HAMT Surface Inside Face Temperature [C]
    Zone,Average,HAMT Surface Inside Face Relative Humidity [%]
    Zone,Average,HAMT Surface Inside Face Vapor Pressure [Pa]
    Zone,Average,HAMT Surface Outside Face Temperature [C]
    Zone,Average,HAMT Surface Outside Face Relative Humidity [%]
    Zone,Average,HAMT Surface Inside Face Relative Humidity [%]
~~~~~~~~~~~~~~~~~~~~

#### HAMT Surface Average Water Content Ratio [kg/kg]

This output is the summed water content [kg/kg] of all cells in a surface expressed as a fraction of the mass of the water to the material mass.

#### HAMT Surface Inside Face Temperature [C]

This output is the temperature [C] on the internal "surface" of the surface.

#### HAMT Surface Inside Face Relative Humidity [%]

#### HAMT Surface Inside Face Relative Humidity [%]

This output is the relative humidity on the internal "surface" of the surface expressed as a percentage.

#### HAMT Surface Inside Face Vapor Pressure [Pa]

This output is the vapor pressure [Pa] on the internal "surface" of the surface.

#### HAMT Surface Outside Face Temperature [C]

This output is the temperature on the external "surface" of the surface.

#### HAMT Surface Outside Face Relative Humidity [%]

This output is the relative humidity on the external "surface" of the surface.

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,HAMT Surface Temperature Cell N [C]
    Zone,Average,HAMT Surface Water Content Cell N [kg/kg]
    Zone,Average,HAMT Surface Relative Humidity Cell N [%]
~~~~~~~~~~~~~~~~~~~~

Detailed profile data for the variables Temperature [C], Relative Humidity [%] and Water Content [kg/kg] within each surface can also be reported. To calculate the heat and moisture transfer through surfaces HAMT splits up surfaces into discrete cells. Each cell is composed of a single material and has a position within the surface. HAMT automatically assigns cells to construction objects so that there are more cells closer to boundaries between materials and also at the "surfaces" of the surface. It is not possible for users to define their own cells.

#### HAMT Surface Relative Humidity Cell <N> [%]

This is the relative humidity of the cell in the surface.

#### HAMT Surface Temperature Cell <N> [C]

This is the temperature of the cell in the surface.

#### HAMT Surface Water Content Cell <N> [kg/kg]

This is the relative water content of the cell in the surface.

Each surface is made from a particular construction. The construction-surface relationship is output by HAMT to the eplusout.eio file with the following format.

! <HAMT cells>, Surface Name, [Construction](#construction) Name, Cell Numbers

! <HAMT origins>, Surface Name, [Construction](#construction) Name, Cell origins (m)

The output also contains the HAMT cell origins and cell number for each construction – surface combination. The coordinate system origin is defined as the exterior surface of the construction. Users can select any one of the Temperature, Relative Humidity or Water Content variables for any cell to be reported, using the following naming scheme for the output variable.

~~~~~~~~~~~~~~~~~~~~

    HAMT Profile Construction <Variable> Cell <Cell#>
~~~~~~~~~~~~~~~~~~~~

It is better to specify the "key" or Surface Name in this output.

So for example to output the temperature of the 10^th^ cell in a surface, eg "East Wall" would require the following output variable.

~~~~~~~~~~~~~~~~~~~~

    Output:Variable,
        East Wall,               !- Key Value
        HAMT Profile Construction Temperature Cell 10,  !- Variable Name
        Hourly;                  !- Reporting Frequency
~~~~~~~~~~~~~~~~~~~~

By selecting a whole range of these reports and using the information in the eplusout.eio file it is possible to build up a temperature profile of the surface.

Materials for Glass Windows and Doors). As noted above, the outside layer should NOT be a film coefficient since EnergyPlus will calculate outside convection and radiation heat transfer more precisely.

#### Field(s) 2-10: Layers

The next fields are optional and the number of them showing up in a particular [Construction](#construction) definition depends solely on the number of material layers present in that construction. The data expected is identical to the outside layer field (see previous field description). The order of the remaining layers is important and should be listed in order of occurrence from the one just inside the outside layer until the inside layer is reached. As noted above, the inside layer should NOT be a film coefficient since EnergyPlus will calculate inside convection and radiation heat transfer more precisely.

IDF Example (floor construction):

~~~~~~~~~~~~~~~~~~~~

    Construction, FLOOR38,  ! Material layer names follow:
          E5 - ACOUSTIC TILE,
          E4 - CEILING AIRSPACE,
          C12 - 2 IN HW CONCRETE;
~~~~~~~~~~~~~~~~~~~~

IDF Example (window construction, no shade):

~~~~~~~~~~~~~~~~~~~~

    Construction, DOUBLE PANE WINDOW,  !- Material layer names follow:
          GLASS - CLEAR SHEET 1 / 8 IN,
          WinAirB1 - AIRSPACE RESISTANCE,
          GLASS - CLEAR SHEET 1 / 8 IN;
~~~~~~~~~~~~~~~~~~~~

IDF Example (window construction, with interior shade):

~~~~~~~~~~~~~~~~~~~~

    Construction, DOUBLE PANE WITH ROLL SHADE,  !- Material layer names follow:
          GLASS - CLEAR SHEET 1 / 8 IN,
          WinAirB1 - AIRSPACE RESISTANCE,
          GLASS - CLEAR SHEET 1 / 8 IN,
          ROLL SHADE - LIGHT
~~~~~~~~~~~~~~~~~~~~

## Site:GroundTemperature:FCfactorMethod

[Site:GroundTemperature:FCfactorMethod](#sitegroundtemperaturefcfactormethod) is used only by the underground walls or slabs-on-grade or underground floors defined with C-factor ([Construction:CfactorUndergroundWall](#constructioncfactorundergroundwall)) and F-factor ([Construction:FfactorGroundFloor](#constructionffactorgroundfloor)) method for code compliance calculations where detailed construction layers are unknown. Only one such ground temperature object can be included. The monthly ground temperatures for this object are close to the monthly outside air temperatures delayed by three months. If user does not input this object in the IDF file, it will be defaulted to the 0.5m set of monthly ground temperatures from the weather file if they are available.

### Inputs

#### Field: Month Temperature(s) – 12 fields in all

Each numeric field is the monthly ground temperature (degrees Celsius) used for the indicated month (January=1^st^ field, February=2^nd^ field, etc.)

And, the IDF example:

~~~~~~~~~~~~~~~~~~~~

    Site:GroundTemperature:FCfactorMethod,  9.5,3.5,-0.7,-1.7,-0.6,3.6,9.3,14,18.2,22.7,21.2,16.8;
~~~~~~~~~~~~~~~~~~~~

## Constructions - Modeling Underground Walls and Ground Floors Defined with C and F Factors for Building Energy Code Compliance

[Building](#building) energy code and standards like ASHRAE 90.1, 90.2 and California Title 24 require the underground wall constructions and slabs-on-grade or underground floors not to exceed certain maximum values of C-factor and F-factor, which do not specify detailed layer-by-layer materials for the constructions.

A simplified approach is introduced to create equivalent constructions and model the ground heat transfer through underground walls and ground floors for the building energy code compliance calculations. The approach is to create constructions based on the user defined C or F factor with two layers: one concrete layer (0.15 m thick) with thermal mass, and one fictitious insulation layer with no thermal mass. Three new objects were created for such purpose: **Construction:CfactorUndergroundWall**, **Construction:FfactorGroundFloor**, and **Site:GroundTemperature:FCfactorMethod**. Details of the approach are described in the Engineering Reference document. The wall and floor construction objects are described in this section; the ground temperature object is described with the other ground temperature objects.

When a underground wall or ground floor surface ([BuildingSurface:Detailed](#buildingsurfacedetailed), [Floor:Detailed](#floordetailed), and [Wall:Detailed](#walldetailed)) references one of the two construction objects, its field ‘Outside Boundary Condition' needs to be set to GroundFCfactorMethod. For simple (rectangular) wall and floor objects, the outside boundary condition is inferred from the construction type.

The [Site:GroundTemperature:FCfactorMethod](#sitegroundtemperaturefcfactormethod) is described in the section for ground temperatures, the following section describes the two new construction objects.

## Construction:CfactorUndergroundWall

This input object differs from the usual wall construction object in that it describes an entire construction rather than individual layers. This object is used when only the wall height (depth to the ground) and the C-factor are available.  This object accesses a model that creates an equivalent layer-by-layer construction for the underground wall to approximate the heat transfer through the wall considering the thermal mass of the earth soil.

This object is referenced by underground wall surfaces with their fields ‘Outside Boundary Condition' set to GroundFCfactorMethod.

### Inputs

#### Field: Name

The name of the underground wall construction.

#### Field: C-Factor

C-Factor is the time rate of steady-state heat flow through unit area of the construction, induced by a unit temperature difference between the body surfaces. The C-Factor unit is W/m^2^·K. The C-factor does not include soil or air films. ASHRAE Standard 90.1 and California Title 24 specify maximum C-factors for underground walls depending on space types and climate zones.

#### Field: Height

This field describes the height of the underground wall, i.e. the depth to the ground surface. The unit is meters.

IDF Example:

~~~~~~~~~~~~~~~~~~~~

    Construction:CfactorUndergroundWall,
        CfactorUGWall,
        0.436,           ! C-factor (W/m2K), does not include soil or air films
        4.57;            ! Height (m)

      BuildingSurface:Detailed,
        Zn001:Wall001,           !- Name
        Wall,                    !- Surface Type
        CfactorUGWall,           !- Construction Name
        ZONE ONE,                !- Zone Name
        GroundFCfactorMethod,    !- Outside Boundary Condition
        ,                        !- Outside Boundary Condition Object
        NoSun,                   !- Sun Exposure
        NoWind,                  !- Wind Exposure
        0.0,                     !- View Factor to Ground
        4,                       !- Number of Vertices
        0.0,0.0,4.572,           !- X,Y,Z ==> Vertex 1
        0.0,0.0,0.0,             !- X,Y,Z ==> Vertex 2
        15.24,0.0,0.0,           !- X,Y,Z ==> Vertex 3
        15.24,0.0,4.572;         !- X,Y,Z ==> Vertex 4
~~~~~~~~~~~~~~~~~~~~

## Construction:FfactorGroundFloor

This input object differs from the usual ground floor construction object in that it describes an entire construction rather than individual layers. This object is used when only the floor area, exposed perimeter, and the F-factor are available.  This object accesses a model that creates an equivalent layer-by-layer construction for the slab-on-grade or underground floor to approximate the heat transfer through the floor considering the thermal mass of the earth soil.

This object is referenced by slab-on-grade or underground floor surfaces with their fields ‘Outside Boundary Condition' set to GroundFCfactorMethod.

### Inputs

#### Field: Name

The name of the ground floor construction.

#### Field: F-Factor

F-Factor represents the heat transfer through the floor, induced by a unit temperature difference between the outside and inside air temperature, on the per linear length of the exposed perimeter of the floor. The unit for this input is W/m·K. ASHRAE Standard 90.1 and California Title 24 specify maximum F-factors for slab-on-grade or underground floors depending on space types and climate zones.

#### Field: Area

This field describes the area (in square meters) of the slab-on-grade or underground floor.

#### Field: PerimeterExposed

This field describes the exposed (direct contact with ambient air) perimeter (in meters) of the slab-on-grade or underground floor.

IDF Example:

~~~~~~~~~~~~~~~~~~~~

    Construction:FfactorGroundFloor,
        slabconst,
        0.12,     !F-factor in W/m-K
        232.26,   !Area in m2
        61.0;     !Exposed perimeter in m

      BuildingSurface:Detailed,
        Zn001:Flr001,            !- Name
        Floor,                   !- Surface Type
        slabconst,               !- Construction Name, FLOOR
        ZONE ONE,                !- Zone Name
        GroundFCfactorMethod,    !- Outside Boundary Condition, Surface
        ,                        !- Outside Boundary Condition Object, Zn001:Flr001
        NoSun,                   !- Sun Exposure
        NoWind,                  !- Wind Exposure
        0,                       !- View Factor to Ground
        4,                       !- Number of Vertices
        15.24,0.0,0.0,           !- X,Y,Z ==> Vertex 1
        0.0,0.0,0.0,             !- X,Y,Z ==> Vertex 2
        0.0,15.240,0.0,          !- X,Y,Z ==> Vertex 3
        15.24,15.24,0.0;         !- X,Y,Z ==> Vertex 4
~~~~~~~~~~~~~~~~~~~~

## Construction:InternalSource

In some cases such as radiant systems, a construction will actually have resistance wires or hydronic tubing embedded within the construction. Heat is then either added or removed from this building element to provide heating or cooling to the zone in question. In the case of building-integrated photovoltaics, the energy removed in the form of electricity will form a sink. It is possible to enter such constructions into EnergyPlus with the syntax described below. The definition is similar to the [Construction](#construction) definition with a few additions related to radiant or other systems that will lead to source/sink terms. The internal source capability is available with both the **ConductionTransferFunction** and **ConductionFiniteDifference** solution algorithms.  The only difference is that the two dimensional pipe arrangements are not available to ConductionFiniteDifference. Those fields are ignored in that implementation.

### Inputs

#### Field: Name

This field is a user specified name that will be used as a reference by other input syntax. For example, a heat transfer surface (ref: [Building](#building) Surfaces) requires a construction name to define what the make-up of the wall is.

#### Field: Source Present After Layer Number

This field is an integer that relates the location of the heat source or sink. The integer refers to the list of material layers that follow later in the syntax and determines the layer after which the source is present. If a source is embedded within a single homogenous layer (such as concrete), that layer should be split into two layers and the source added between them. For example, a value of "2" in this field tells EnergyPlus that the source is located between the second and third material layers listed later in the construction description (see layer fields below).

#### Field: Temperature Calculation Requested After Layer Number

The nature of this field is similar to the source interface parameter (see previous field) in that it is an integer, refers to the list of material layers that follow, and defines a location after the layer number identified by the user-defined number. In this case, the user is specifying the location for a separate temperature calculation rather than the location of the heat source/sink. This feature is intended to allow users to calculate a temperature within the construction. This might be important in a radiant cooling system where condensation could be a problem. This temperature calculation can assist users in making that determination in absence of a full heat and mass balance calculation.

#### Field: Dimensions for the CTF Calculation

This field is also an integer and refers to the detail level of the calculation. A value of "1" states that the user is only interested in a one-dimensional calculation. This is appropriate for electric resistance heating and for hydronic heating (when boiler/hot water heater performance is not affected by return and supply water temperatures). A value of "2" will trigger a two-dimensional solution for this surface only. This may be necessary for hydronic radiant cooling situations since chiller performance is affected by the water temperatures provided.

A few things should be noted about requesting two-dimensional solutions. First, the calculation of the conduction transfer functions (CTF) is fairly intensive and will require a significant amount of computing time. Second, the solution regime is two-dimensional internally but it has a one-dimensional boundary condition imposed at the inside and outside surface (i.e., surface temperatures are still isothermal is if the surface was one-dimensional).

#### Field: Tube Spacing

This field defines how far apart in meters the hydronic tubing or electrical resistance wires are spaced in the direction perpendicular to the main direction of heat transfer. Note that this parameter is only used for two-dimensional solutions (see previous field).

#### Field: Outside Layer

~~~~~~~~~~~~~~~~~~~~

    Each construction must have at least one layer. This field defines the material name associated with the layer on the outside of this construction—outside referring to the side that is not exposed to the zone but rather the opposite side environment, whether this is the outdoor environment or another zone. Material layers are defined based on their thermal properties elsewhere in the input file (ref: Material and Material Properties and Zone,Average,HAMT Surface Average Water Content Ratio [kg/kg]
    Zone,Average,HAMT Surface Inside Face Temperature [C]
    Zone,Average,HAMT Surface Inside Face Relative Humidity [%]
    Zone,Average,HAMT Surface Inside Face Vapor Pressure [Pa]
    Zone,Average,HAMT Surface Outside Face Temperature [C]
    Zone,Average,HAMT Surface Outside Face Relative Humidity [%]
    Zone,Average,HAMT Surface Inside Face Relative Humidity [%]
~~~~~~~~~~~~~~~~~~~~

#### HAMT Surface Average Water Content Ratio [kg/kg]

This output is the summed water content [kg/kg] of all cells in a surface expressed as a fraction of the mass of the water to the material mass.

#### HAMT Surface Inside Face Temperature [C]

This output is the temperature [C] on the internal "surface" of the surface.

#### HAMT Surface Inside Face Relative Humidity [%]

#### HAMT Surface Inside Face Relative Humidity [%]

This output is the relative humidity on the internal "surface" of the surface expressed as a percentage.

#### HAMT Surface Inside Face Vapor Pressure [Pa]

This output is the vapor pressure [Pa] on the internal "surface" of the surface.

#### HAMT Surface Outside Face Temperature [C]

This output is the temperature on the external "surface" of the surface.

#### HAMT Surface Outside Face Relative Humidity [%]

This output is the relative humidity on the external "surface" of the surface.

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,HAMT Surface Temperature Cell N [C]
    Zone,Average,HAMT Surface Water Content Cell N [kg/kg]
    Zone,Average,HAMT Surface Relative Humidity Cell N [%]
~~~~~~~~~~~~~~~~~~~~

Detailed profile data for the variables Temperature [C], Relative Humidity [%] and Water Content [kg/kg] within each surface can also be reported. To calculate the heat and moisture transfer through surfaces HAMT splits up surfaces into discrete cells. Each cell is composed of a single material and has a position within the surface. HAMT automatically assigns cells to construction objects so that there are more cells closer to boundaries between materials and also at the "surfaces" of the surface. It is not possible for users to define their own cells.

#### HAMT Surface Relative Humidity Cell <N> [%]

This is the relative humidity of the cell in the surface.

#### HAMT Surface Temperature Cell <N> [C]

This is the temperature of the cell in the surface.

#### HAMT Surface Water Content Cell <N> [kg/kg]

This is the relative water content of the cell in the surface.

Each surface is made from a particular construction. The construction-surface relationship is output by HAMT to the eplusout.eio file with the following format.

! <HAMT cells>, Surface Name, [Construction](#construction) Name, Cell Numbers

! <HAMT origins>, Surface Name, [Construction](#construction) Name, Cell origins (m)

The output also contains the HAMT cell origins and cell number for each construction – surface combination. The coordinate system origin is defined as the exterior surface of the construction. Users can select any one of the Temperature, Relative Humidity or Water Content variables for any cell to be reported, using the following naming scheme for the output variable.

~~~~~~~~~~~~~~~~~~~~

    HAMT Profile Construction <Variable> Cell <Cell#>
~~~~~~~~~~~~~~~~~~~~

It is better to specify the "key" or Surface Name in this output.

So for example to output the temperature of the 10^th^ cell in a surface, eg "East Wall" would require the following output variable.

~~~~~~~~~~~~~~~~~~~~

    Output:Variable,
        East Wall,               !- Key Value
        HAMT Profile Construction Temperature Cell 10,  !- Variable Name
        Hourly;                  !- Reporting Frequency
~~~~~~~~~~~~~~~~~~~~

By selecting a whole range of these reports and using the information in the eplusout.eio file it is possible to build up a temperature profile of the surface.

Materials for Glass Windows and Doors). As noted above, the outside layer should NOT be a film coefficient since EnergyPlus will calculate convection and radiation heat transfer more precisely.

#### Field(s) 2-10: Layers

The next fields are optional and the number of them showing up in a particular [Construction](#construction) definition depends solely on the number of material layers present in that particular construction. The data expected is identical to the outside layer field (see previous field description). The order of the remaining layers is important and should be listed in order of occurrence from the one just inside the outside layer until the inside layer is reached. As noted above, the inside layer should NOT be a film coefficient since EnergyPlus will calculate convection and radiation heat transfer more precisely.

## Composite Wall Constructions

Standard constructions in EnergyPlus are built with the materials and layers described earlier. However, some configurations will not be adequately represented by using this approach. The Reference Data Set CompositeWallConstructions.idf contains constructions and associated materials for a set of **composite** walls. These are walls—such as stud walls—that have complicated heat-flow paths so that the conduction is two- or three-dimensional. Thermal bridges are one of the common terms for these complicated heat-flow paths; this dataset will help you represent these in EnergyPlus.

The materials here are **not** real materials but are "equivalent" materials obtained from finite-difference modeling. (The thickness, conductivity, density and specific heat values of the material layers for the different constructions have been taken from the ASHRAE report "Modeling Two- and Three-Dimensional Heat Transfer through Composite Wall and [Roof](#roof) Assemblies in Hourly Energy Simulation Programs (1145-TRP)," by Enermodal Engineering Limited, Oak Ridge National Laboratory, and the Polish Academy of Sciences, January 2001.). EnergyPlus will calculate conduction transfer functions using these materials. The heat transfer based on these conduction transfer functions will then be very close to what would be calculated with a two- or three-dimensional heat transfer calculation.

For stud walls, using these composite constructions will give more accurate heat flow than you would get by manually dividing the wall into a stud section and a non-stud section.

**If your wall's exterior or interior roughness or thermal, solar or visible absorptances are different from those in the data set, you can make the appropriate changes to the first material (the outside layer) or the third material (the inside layer). None of the other values should be changed.**

> Complete description of the CompositeWallConstructions data set are found in the OutputDetailsAndExamples document.

## Construction:ComplexFenestrationState

This input object is used to describe the properties of a single state for complex fenestration.  There are two parts to the input, 1) layer-by-layer physical description of fenestration system and 2) a set of matrices that describe overall system optical performance.  Each layer also has associated with it two matrices that give the layer absorptance (for front and back incidence on the system.

The optical properties are given as a two-dimensional matrix describing the basis and four two-dimensional matrices of system bidirectional optical properties.

These input objects will generally be exported directly from the WINDOW program and it is expected that users usually will not develop the input themselves. However, this is an option for users who prefer to use a different method (e.g., Monte-Carlo ray-trace or measurement) of determining optical properties.

Multiple instances of this object are used to define the separate operating states of complex fenestration.  For example, blinds could be deployed or redirected to create a new state, or electrochromic glazings could change transmittance.  Each separate state defines the materials present and the overall optical performance.  If the glazing system has only one state, then only one of these objects is needed.

**If there is more than one complex fenestration state, it will be controlled using the EMS actuator called "Surface" with the control type "[Construction](#construction) State" and the EMS input object called [EnergyManagementSystem:ConstructionIndexVariable](#energymanagementsystemconstructionindexvariable).**

### Inputs

#### Field: Name

Unique name of this construction.  Used to identify type of window in surface objects.

#### Field: Basis Type keyword

Only value currently implemented is "LBNLWINDOW". More options may be added in the future.

#### Field: Basis Symmetry, keyword

Only value currently implemented is "None". More options will be added in the future.

#### Field: Thermal Parameters

This field gives the name of [WindowThermalModel:Params](#windowthermalmodelparams) object used to keep common data necessary for thermal simulation.

#### Field: Basis Matrix Name

This field gives the name of an 2 x N matrix object that defines the basis  For a fenestration basis, N would be the number of theta (polar angle) values, the first of the two elements for each of the i=1,..,N would be the theta value, and the second would be the number of phi (azimuthal angle) values that 360º is divided into for that theta.

#### Field: Solar Optical Complex Front Transmittance Matrix Name

This field contains the name of matrix object that describes the solar transmittance at different incident angles.  This is from the outside toward the inside.

#### Field: Solar Optical Complex Back Reflectance Matrix Name

This field contains the name of matrix object that describes the solar back reflectance at different incident angles.  This is from the inside toward the outside.

#### Field: Visible Optical Complex Front Transmittance Matrix Name

This field contains the name of matrix object that describes the visible transmittance at different incident angles.  This is from the outside toward the inside.

#### Field: Visible Optical Complex Back Reflectance Matrix Name

This field contains the name of vector object that describes the visible back reflectance at different incident angles.  This is from the inside toward the outside.

#### Field: Outside Layer <x=1>

Each construction must have at least one layer. The layer order is from outside to inside, with the first layer being either [WindowMaterial:Glazing](#windowmaterialglazing) or [WindowMaterial:ComplexShade](#windowmaterialcomplexshade). The next layer is a [WindowMaterial:Gap](#windowmaterialgap) layer, and the following layers then alternate between [WindowMaterial:Glazing](#windowmaterialglazing) or [WindowMaterial:ComplexShade](#windowmaterialcomplexshade) and [WindowMaterial:Gap](#windowmaterialgap). The last layer cannot be [WindowMaterial:Gap](#windowmaterialgap).

#### Field: Outside Layer Directional Front Absorptance Matrix Name

Points to an Nbasis x 1 matrix object.

#### Field: Outside Layer Directional Back Absorptance Matrix Name

Points to an Nbasis x 1 matrix object.

#### Above 3 fields are optionally repeated for layers 2-10

These layers include gaps, which do not need to have matrix data specified.

An IDF example of complex fenestration with single layer:

~~~~~~~~~~~~~~~~~~~~

    Construction:ComplexFenestrationState,       !- single layer example
      CFS_Glz_1,                 !- name
      LBNLWindow,                !- basis type
      None,                      !- basis symmetry type
      ThermParam_1,              !- window thermal model
      CFS_Glz_1_Basis,           !- basis matrix name
      CFS_Glz_1_TfSol,           !- Tfsol
      CFS_Glz_1_RbSol,           !- Rbsol
      CFS_Glz_1_Tfvis,           !- Tfvis
      CFS_Glz_1_Tbvis,           !- Tbvis
      Glass_102_Layer,           !- layer 1 name
      CFS_Glz_1_Layer_1_fAbs,    !- fAbs
      CFS_Glz_1_Layer_1_bAbs;    !- bAbs
~~~~~~~~~~~~~~~~~~~~

An complex fenestration IDF example with double layer (first layer is shading device):

~~~~~~~~~~~~~~~~~~~~

    Construction:ComplexFenestrationState,       !- double layer example
      CFS_Glz_59,                    !- name
      LBNLWindow,                    !- basis type
      None,                          !- basis symmetry type
      ThermParam_59,                 !- window thermal model
      CFS_Glz_59_Basis,              !- basis matrix name
      CFS_Glz_59_TfSol,              !- Tfsol
      CFS_Glz_59_RbSol,              !- Rbsol
      CFS_Glz_59_Tfvis,              !- Tfvis
      CFS_Glz_59_Tbvis,              !- Tbvis
      Shade_30001_Layer,             !- layer 1 name (shading device)
      CFS_Glz_59_Layer_1_fAbs,       !- fAbs
      CFS_Glz_59_Layer_1_bAbs,       !- bAbs
      Gap_1_Layer,                   !- layer 1 name
      ,                 !- absorptance matrices for gaps should be empty for now
      ,                              !- it is for future use
      Glass_3110_Layer,              !- layer 2 name
      CFS_Glz_59_Layer_3110_fAbs,    !- fAbs
      CFS_Glz_59_Layer_3110_bAbs;    !- bAbs
~~~~~~~~~~~~~~~~~~~~

## WindowThermalModel:Params

This input object is used with the Construction:ComplexFenestrationState

### Inputs

#### Field: Name

Unique name of the window thermal model parameters.

#### Field: Calculation Standard

The type of the calculation standard.  The choices are:

- ISO15099
- EN673Declared
- EN673Design

The default is ISO15099.

#### Field: Thermal Model

The type of thermal model.  The choices are:

- ISO15099
- ScaledCavityWidth
- ConvectiveScalarModel_NoSDThickness
- ConvectiveScalarModel_withSDThickness

The default is ISO15099.

#### Field: SD Scalar

Shading Device Scalar Factor. Only used for  Thermal Model = Convective Scalar Model.  Factor of venetian shading device layer contribution to convection. Real value between 0 (where the shading device contribution to convection is neglected) and 1 (where the shading device treated as "closed" – as if it is a glass layer with thermal properties of SD slat material). Default: 1.0

#### Field: Deflection Model

The type of deflection model used to model deflection in windows and glass.  The choices are:

- NoDeflection
- TemperatureAndPressureInput
- MeasuredDeflection

The default is NoDeflection.

#### Field: Vacuum Pressure Limit

The pressure (Pa) which will be considered to be the limit for vacuum glazing pressure.  All pressures less than or equal to this pressure will be considered to be vacuum. Default: 13.238 Pa.

#### Field: Initial Temperature

The temperature (^o^C) of the gap in the time of fabrication.  It is used only when [WindowThermalModel:Params](#windowthermalmodelparams) DeflectionModel =TemperatureAndPressureInput

#### Field: Initial Pressure

The pressure (Pa) of the gap at the time of fabrication of the sealed glazing system unitIt is used only when [WindowThermalModel:Params](#windowthermalmodelparams) DeflectionModel =TemperatureAndPressureInput.

An IDF example for [WindowThermalModel:Params](#windowthermalmodelparams) (without deflection):

~~~~~~~~~~~~~~~~~~~~

    WindowThermalModel:Params,
      ThermParam_59,                   !- name
      ISO15099,                        !- standard
      ISO15099,                        !- thermal model standard
      1.00,                            !- SD scalar
      NoDeflection;                    !- deflection model
~~~~~~~~~~~~~~~~~~~~

An IDF example for thermal paramters (with deflection):

~~~~~~~~~~~~~~~~~~~~

    WindowThermalModel:Params,
      ThermParam_59,                   !- name
      ISO15099,                        !- standard
      ISO15099,                        !- thermal model standard
      1.00,                            !- SD scalar
      TemperatureAndPressureInput,     !- deflection model
      ,                                !- vacuum pressure limit
      21.00,                           !- temperature at time of fabrication
      10000.00;                        !- pressure at time of fabrication
~~~~~~~~~~~~~~~~~~~~

An IDF example for [WindowThermalModel:Params](#windowthermalmodelparams) for modeling vacuum glazing

~~~~~~~~~~~~~~~~~~~~

    WindowThermalModel:Params,
    ThermParam_1006,                                    !- name
    ISO15099,                                           !- standard
    ISO15099,                                           !- thermal model
    1.0000,                                             !- SDScalar
    NoDeflection,                                       !- deflection model
    13.238;                                             !- vacuum pressure limit
~~~~~~~~~~~~~~~~~~~~

## Matix:TwoDimension

This is input object is only used with [Construction:ComplexFenestrationState](#constructioncomplexfenestrationstate) object to enter a two-dimensional matrix of values.

It is used to define the Basis Matrix for BSDF input data, and is also used to define the actual BSDF matrices data for the complete fenestration definition as well as the individual layers of the system.

The data are entered in row-major order: all the elements of row 1, followed by all the elements of row 2, etc.  The number of values to be entered depends on the number of rows and the number of columns.  Blank fields are treated as having been set to zero.

See example IDF file "SmOff_ CmplxGlz_IntExtShading.idf" for the definition of two complex shading layers with matrix data defined.

### Field: Name

Unique name of matrix input object.

### Field: Number of Rows

This field is the number of rows in the matrix.

### Field: Number of Columns

This field is the number of columns in the matrix

### < Field Set: Value # N >

Repeat entering value exactly the same number of times as the number of rows times the number of columns.

### Field: Value # 1

This is the value of the matrix at the first row and first column.

### Field: Value #2

This is the value of the matrix at the first row and the second column.

An IDF example of matrix for defining BSDF basis:

~~~~~~~~~~~~~~~~~~~~

    Matrix:TwoDimension,       !- matrix for basis definition
      CFS_Glz_1_Basis,         !- basis matrix name
      9,                       !- number of rows
      2,                       !- number of colums
      0.00000, 1.00000,
      10.00000, 8.00000,
      20.00000, 16.00000,
      30.00000, 20.00000,
      40.00000, 24.00000,
      50.00000, 24.00000,
      60.00000, 24.00000,
      70.00000, 16.00000,
      82.50000, 12.00000;
~~~~~~~~~~~~~~~~~~~~

## Construction:WindowEquivalentLayer

This object defines the construction for equivalent layer window (ASHWAT) model. This window can model various mix of glazing and shading layers combination. Shadings are defined as an integral part of the construction. The construction is defined by listing the layers name starting with outside layer and work your way to the inside Layer. Up to six solid layers (glazing and shade) and up to five gaps, i.e., a total of up to 11 layers maximum are allowed in equivalent layer window object. The solid layer types allowed are: Glazing, Insect Screen, Roller Blinds, Venetian Blind, and Drape Fabrics. This window model requires optical data of the individual glazing and shading layers to calculate the effective optical properties of the composite fenestration construction. Venetian blinds in equivalent layer window model can be in a fixed slat angle or has the option to control the slat angle in order to maximize visibility, or maximize solar gains. An equivalent-layer concept can simulate wide range of multiple glazing and shading layers combination and provides unlimited flexibility to combine different types of shading layers in a fenestration. For the gap layer object any one of the five different Gas types can be specified: AIR, ARGON, XENON, KRYPTON, or CUSTOM. This window object is referenced by fenestration surfaces. For details of the model description refer to Equivalent Layer Fenestration Model section in Engineering Reference. The various layer objects that can be referenced in Equivalent Layer window model are:

~~~~~~~~~~~~~~~~~~~~

    WindowMaterial:Glass:EquivalentLayer
    WindowMaterial:Shade:EquivalentLayer
    WindowMaterial:Drape:EquivalentLayer
    WindowMaterial:Blind:EquivalentLayer
    WindowMaterial:Screen:EquivalentLayer
    WindowMaterial:Gap:EquivalentLayer
~~~~~~~~~~~~~~~~~~~~

### Inputs

#### Field: Name

This field is a user specified name that will be used as a reference by other input syntax. For example, a heat transfer surface (ref: Fenestration) requires a construction name to define what the make-up of the fenestration is. This name must be identical to one of the [Window](#window) [Construction](#construction) Equivalent Layer definitions in the input data file.

#### Field: Outside Layer

Each equivalent layer window construction must have at least one layer. This field defines the material name associated with the layer on the outside of the construction—outside referring to the side that is exposed to the outdoor environment or another zone. [Material](#material-and-material-properties) layers for equivalent layer window model are defined based on their thermal properties elsewhere in the input file (ref: WindowEquivalentLayerMaterialNames)

#### Field: Layer 2 - Layer11

The next fields are optional and the number of them showing up in a particular equivalent layer window construction definition depends solely on the number of material layers present in that construction. The data expected is identical to the outside layer field (see previous field description). The order of the remaining layers is important and should be listed in order of occurrence from the one just inside the outside layer until the inside layer is reached. As noted above, the inside layer should NOT be a film coefficient since EnergyPlus will calculate inside convection and radiation heat transfer more precisely.

An IDF example for this object, is shown below:

~~~~~~~~~~~~~~~~~~~~

    Construction:WindowEquivalentLayer,
      Six Solid Layers Window,   !- Name
      INSCRN,                    !- Outside Layer
      Air GAP Outdoor 12.7mm,    !- Layer 2
      GLZGRY,                    !- Layer 3
      Argon GAP Sealed 12.7mm,   !- Layer 4
      FEP,                       !- Layer 5
      Xenon GAP Sealed 12.7mm,   !- Layer 6
      LOF1436,                   !- Layer 7
      Krypton GAP Sealed 12.7mm, !- Layer 8
      GLZCLR,                    !- Layer 9
      Air GAP Indoor 12.7mm,     !- Layer 10
      ShadeTrns;                 !- Layer 11
~~~~~~~~~~~~~~~~~~~~

## Construction:WindowDataFile

The WINDOW program, which does a thermal and optical analysis of a window under different design conditions, writes a data file ("[Window](#window) data file") containing a description of the window that was analyzed. The [Construction:WindowDataFile](#constructionwindowdatafile) object allows a window to be read in from the WINDOW data file—see "Importing Windows from WINDOW." For information on adding a shading device to the window see "[WindowProperty:ShadingControl](#windowpropertyshadingcontrol)."

### Inputs

#### Field: Name

This is the name of a window on the [Window](#window) data file. An error will result if EnergyPlus cannot find a window of this name on the file, or if the file, shown in the next field, is not present. The location of the data file should be specified in the File Name field. For details on what is done with the data if a matching window is found on the file see "Importing Windows from WINDOW."

#### Field: File Name

This is the file name of the [Window](#window) data file that contains the [Window](#window) referenced in the previous field. The field may include a full path with file name, for precise results. The field must be <= 100 characters. The file name must not include commas or an exclamation point. A relative path or a simple file name should work with version 7.0 or later when using EP-Launch even though EP-Launch uses temporary directories as part of the execution of EnergyPlus. If using RunEPlus.bat to run EnergyPlus from the command line, a relative path or a simple file name may work if RunEPlus.bat is run from the folder that contains EnergyPlus.exe.

If this field is left blank, the file name is defaulted to Window5DataFile.dat.

The IDD of the object:

Input Example

~~~~~~~~~~~~~~~~~~~~

    Construction:WindowDataFile,
      DoubleClear;          !- Name of a Window on the Window Data File
      ! Note – Window5DataFile.dat is presumed to be in the "run" folder where EnergyPlus.exe is

    FenestrationSurface:Detailed,
      Zn001:Wall001:Win001, !- Name
      Window,               !- Class
      DoubleClear,          !- Construction Name
      Zn001:Wall001,,       !- Base Surface Name, and Target (if applicable)
      0.5,                  !- View Factor to Ground
      ,                     !- Window Shading Control name
      ,                     !- Frame/Divider name
      1.0,                  !- Multiplier
      4,                    !- Number of vertices
      0.548, 0.0, 2.5000,   !- X,Y,Z of Vertices
      0.548, 0.0, 0.5000,
      5.548, 0.0, 0.5000,
      5.548, 0.0, 2.5000;
~~~~~~~~~~~~~~~~~~~~

An example showing use of specific data file name and complete path location follows:

~~~~~~~~~~~~~~~~~~~~

    Construction:WindowDataFile,
      DoubleClear,            !- Name of a Window on the Window Data File
      C:\EnergyPlusData\DataSets\MyWindow.dat;
~~~~~~~~~~~~~~~~~~~~

### Outputs

An optional report (contained in **eplusout.eio**) gives calculational elements for the materials and constructions used in the input. These reports are explained fully in the Output Details and Examples document.
