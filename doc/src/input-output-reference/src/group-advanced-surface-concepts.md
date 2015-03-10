# Group – Advanced Surface Concepts

This group of objects describe concepts applied to heat transfer surfaces that are of an advanced nature. Careful consideration must be given before using these.

## SurfaceProperty:HeatTransferAlgorithm

This object, and three other related objects, can be used to control which surface heat transfer model is used on specific surfaces.  The separate object called [HeatBalanceAlgorithm](#heatbalancealgorithm) is used to control the heat transfer model in an overall way while this object can be used to revise the algorithm selections for specific surfaces.  This object allows selectively overriding the global setting in [HeatBalanceAlgorithm](#heatbalancealgorithm) to choose one of the following models for a particular surface:

CTF (Conduction Transfer Functions),

EMPD (Effective Moisture Penetration Depth with Conduction Transfer Functions).

CondFD (Conduction Finite Difference)

HAMT (Combined Heat And Moisture Finite Element)

### Inputs

#### Field: Surface Name

This is the name of the surface that will be assigned to use the heat transfer algorithm selected in the next field.  This should be a name of a surface defined elsewhere.

#### Field: Algorithm

This field is used to determine the heat transfer algorithm that is to be applied to the surface named in the previous field.  The allowable choices are:

ConductionTransferFunction

MoisturePenetrationDepthConductionTransferFunction

ConductionFiniteDifference

CombinedHeatAndMoistureFiniteElement

## SurfaceProperty:HeatTransferAlgorithm:MultipleSurface

This object can be used to control the surface heat transfer model used for specific types of surfaces.  The separate object called [HeatBalanceAlgorithm](#heatbalancealgorithm) is used to control the heat transfer model in an overall way while this object can be used to revise the algorithm selections for specific types of surfaces.  This object allows selectively overriding the global setting in [HeatBalanceAlgorithm](#heatbalancealgorithm) to choose one of the following models for all surfaces of a particular type:

CTF (Conduction Transfer Functions),

EMPD (Effective Moisture Penetration Depth with Conduction Transfer Functions).

CondFD (Conduction Finite Difference)

HAMT (Combined Heat And Moisture Finite Element)

### Inputs

#### Field: Name

This is a unique, user-defined name for the object.

#### Field: Surface Type

This field is selects the type of surfaces that are all assigned to use the heat transfer algorithm selected in the next field.  This field is used with one of the following allowable keywords:

AllExteriorSurfaces—all surfaces that have "Outdoors" outside boundary condition

AllExteriorWalls—all walls that have "Outdoors" outside boundary condition

AllExteriorRoofs—all roofs that have "Outdoors" outside boundary condition

AllExteriorFloors—all floors that have "Outdoors" outside boundary condition

AllGroundContactSurfaces—all surfaces that have "Ground" outside boundary condition

AllInteriorSurfaces—all surfaces that are internal partition-type surfaces

AllInteriorWalls—all walls that are internal surfaces

AllInteriorCeilings—all ceilings that are internal surfaces

AllInteriorFloors—all floors that are are internal surfaces

#### Field: Algorithm

This field is used to determine the heat transfer algorithm that is to be applied to the surface types in the previous field.  The allowable choices are:

ConductionTransferFunction

MoisturePenetrationDepthConductionTransferFunction

ConductionFiniteDifference

CombinedHeatAndMoistureFiniteElement

~~~~~~~~~~~~~~~~~~~~

    SurfaceProperty:HeatTransferAlgorithm:MultipleSurface,
        my exterior wall override,
        AllExteriorWalls,
        ConductionFiniteDifference;
~~~~~~~~~~~~~~~~~~~~

## SurfaceProperty:HeatTransferAlgorithm:SurfaceList

This object can be used to control the surface heat transfer model used for a list of surfaces.  The separate object called [HeatBalanceAlgorithm](#heatbalancealgorithm) is used to control the heat transfer model in an overall way while this object can be used to revise the algorithm selections for a list of specific surfaces.  This object allows selectively overriding the global setting in [HeatBalanceAlgorithm](#heatbalancealgorithm) to choose one of the following models for listed:

CTF (Conduction Transfer Functions),

EMPD (Effective Moisture Penetration Depth with Conduction Transfer Functions).

CondFD (Conduction Finite Difference)

HAMT (Combined Heat And Moisture Finite Element)

### Inputs

#### Field: Name

This is a unique, user-defined name for the object.

#### Field: Algorithm

This field is used to determine the heat transfer algorithm that is to be applied to the surface listed in the remaining fields.  The allowable choices are:

ConductionTransferFunction

MoisturePenetrationDepthConductionTransferFunction

ConductionFiniteDifference

CombinedHeatAndMoistureFiniteElement

#### Field: Surface Name N

This is the name of the "Nth" surface that will be assigned to use the heat transfer algorithm selected in this object.  These should be the names of surfaces defined elsewhere. This object is extensible.  Additional surfaces can be added to extend the object.

An example IDF object follows.

~~~~~~~~~~~~~~~~~~~~

    SurfaceProperty:HeatTransferAlgorithm:SurfaceList,
        my wall construct override,   !- Name
        ConductionFiniteDifference,   !- Algorithm
        Zn001:Wall001,                !- Surface Name 1
        Zn001:Wall002,                !- Surface Name 2
        Zn001:Wall003,                !- Surface Name 3
        Zn001:Wall004;                !- Surface Name 4
~~~~~~~~~~~~~~~~~~~~

## SurfaceProperty:HeatTransferAlgorithm:Construction

This object can be used to control the surface heat transfer model used for surfaces that have a specific type of construction.  The separate object called [HeatBalanceAlgorithm](#heatbalancealgorithm) is used to control the heat transfer model in an overall way while this object can be used to revise the algorithm selections for specific constructions.  This object allows selectively overriding the global setting in [HeatBalanceAlgorithm](#heatbalancealgorithm) to choose one of the following models for all surfaces with particular type of construction:

CTF (Conduction Transfer Functions),

EMPD (Effective Moisture Penetration Depth with Conduction Transfer Functions).

CondFD (Conduction Finite Difference)

HAMT (Combined Heat And Moisture Finite Element)

### Inputs

#### Field: Name

This is a unique, user-defined name for the object.

#### Field: Algorithm

This field is used to determine the heat transfer algorithm that is to be applied to the surfaces with the type of of construction listed in the next field.  The allowable choices are:

ConductionTransferFunction

MoisturePenetrationDepthConductionTransferFunction

ConductionFiniteDifference

CombinedHeatAndMoistureFiniteElement

#### Field: Construction Name

This field is the name of a [Construction](#construction) object defined elsewhere.  All the surfaces in the model that are assigned this type of construction will be assigned to use the heat transfer algorithm slected in the previous field.

An example IDF object follows.

~~~~~~~~~~~~~~~~~~~~

    SurfaceProperty:HeatTransferAlgorithm:Construction,
        my wall construct override,  !- Name
        ConductionFiniteDifference,  !- Algorithm
        R13WALL;                     !- Construction Name
~~~~~~~~~~~~~~~~~~~~

## SurfaceControl:MoveableInsulation

Movable insulation can be used/scheduled on any surface regular surface (such as a wall, floor, roof, etc.) but not on a subsurface (such as a window, use [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) instead). With movable insulation, no reference is made in the surface that is using the insulation – rather the movable insulation statement references the surface to which it is applied.

Exterior and interior movable insulation have undergone some testing and appears to producing expected results. The underlying principle has been implemented in EnergyPlus for both interior and exterior movable insulation with the possibility for exterior movable insulation to be transparent (transparent insulation material or TIM).

TIM exterior layers can be used with the ConductionFiniteDifference (CondFD) solution algorithm. With this addition, TIM layers can be used in conjunction with wall layers that have phase change materials (PCM) included, or any other advanced capability of the CondFD algorithm such as variable conductivity. The input requirements are exactly the same as when used with the CTF algorithm. The Solution Algorithm needs to be changed to CondFD, and as with CTF, the "SurfaceControl:MovableInsulation" object must be completed to specify the insulated surface and the "[WindowMaterial:Glazing](#windowmaterialglazing)" object is needed to provide the TIM layer properties.

Basically, the addition of movable insulation allows the user to schedule an extra amount of insulation on either the inside or outside surface of a wall (or both). The insulation must be a simple, homogenous material layer (linked to a material definition within the input data file). Note that EnergyPlus allows the exterior movable insulation layer to be transparent to short wavelength radiation (solar). In this case, incident solar is split between the plane between the movable insulation and the surface and the plane between the movable insulation and the surrounding air. This calculation is fairly basic and based on the solar transmittance of the insulation layer (material properties). Using transparent layers for exterior movable insulation allows solar energy to penetrate deeper into a construction where it can be stored for later use in the building (similar in concept to a Trombe Wall).

### Field: Insulation Type

This field determines whether the movable insulation is applied to the inside or the outside of the surface by entering either "Inside" or "Outside", respectively.

### Field: Surface Name

This field refers the movable insulation back to a particular surface (ref: [Building](#building) Surfaces) via its user assigned name so that EnergyPlus knows where to apply this extra layer of insulation. This will affect either the inside or outside surface heat balance of this surface depending on the value in the insulation type field (see previous field).

### Field: Material Name

This field refers to a material layer (e.g., [Material](#material-and-material-properties), [Material:NoMass](#materialnomass), or [WindowMaterial:Glazing](#windowmaterialglazing); transparent layers are only valid for outside movable insulation) via its user assigned name. This provides the program with a full complement of material properties so that the effect of the insulation (R-value and solar transmittance) can be correctly taken into account by EnergyPlus.

### Field: Schedule Name

This field is a schedule that theoretically can be any positive real number but was originally intended to be a parameter between 0.0 and 1.0. Its purpose is to act as a fractional modifier on the resistance of the material layer. The actual thermal resistance of the movable insulation is equal to the resistance of the material layer times the current value in the movable insulation schedule. A value of 0.0 simply means that the movable insulation is not present.

An example of this syntax implemented in an input file is:

~~~~~~~~~~~~~~~~~~~~

    SurfaceControl:MoveableInsulation,
      Exterior,                      ! Insulation Type
      Zone001:Wall001,               ! Surface Name
      TransparentInsulationMaterial, ! Material Name
      PresentInWinterSchedule;       ! Schedule Name
~~~~~~~~~~~~~~~~~~~~

## SurfaceProperty:OtherSideCoefficients

By referencing the Other Side Coefficients statement in the surface statements (i.e. Outside Boundary Condition), the temperature of the outer plane of a surface (see Figure 41) can be directly controlled. Other side coefficients can also be used to control the exterior convective heat transfer coefficient of a surface and the corresponding exterior air temperature. It should be noted that solar effects are not accounted for when other side coefficients are used. In addition, if other side coefficients are specified for a surface, they also hold for subsurfaces of that surface (though subsurfaces can have their own coefficient set).

other side coefficients have the same effect on all types of heat transfer surfaces. In other words, an interior surface with other side coefficients specified and an exterior wall with identical other side coefficients specified are simulated exactly the same. A surface that uses other side coefficients should be thought of as a new or separate type of surface. All heat transfer surfaces are simulated in the same manner through conduction transfer functions. The only difference between the various types of heat transfer surfaces is the environment on the other side of the surface. For example, the other side environment of an exterior surface is the outdoor environment. For an interior surface, the temperature of the outer plane of the surface is set equal to the temperature of the inner plane of the surface. Similarly, a surface with other side coefficients specified will allow the user to control the other side environment.

Heat transfer through a surface is an extremely important component in the calculation of zone loads. The information to calculate this heat transfer is readily available if the surface is exposed to the outdoor environment or to another zone that is being simulated. Occasionally, a user will want to model the heat transfer through a surface that is adjacent to an area that is not included in the EnergyPlus model.  For example, an office area is attached to a warehouse and the user is only interested in simulating the office area. An interior surface with other side coefficients specified could be used to control the environment on the other side of the surface, thereby accounting for the heat transfer through the adjoining surface.

Other Side Coefficients affects the "other side" of a surface as described below. Each coefficient has a special meaning. You may enter a 0 or blank if you are not using a particular coefficient. Note that there are two potential ways to use other side coefficients. Either they are used to set the temperature of the exterior side surface directly (if the combined convective/radiative coefficient below is less than or equal to zero) or to set both the film coefficient (positive value for the combined convective/radiative coefficient below) and the outside air temperature.

### Inputs

#### Field: Name

This, of course, is the string referenced in the Surface statement that is using OtherSideCoefficients as the Outside Boundary Condition.

#### Field: Combined Convective/Radiative Film Coefficient

This is a trigger value. If the value is greater than zero, then it is taken to be the combined convective/radiative film coefficient. In this case (value > 0), the remaining fields are used first to calculate the outside air temperature for the surface and then to calculate the outside surface temperature based on the outside air temperature and the film coefficient. If this field is less than or equal to zero, then the remaining fields are used to calculate the surface temperature (not the outside air temperature). The units for this field are the same as for a convective heat transfer coefficient: W/(m^2^\*K). This is referred to as "C1" in the reference below.

#### Field: Constant Temperature

This field defines a temperature term that is a constant part of the calculation either of the surface or outside air temperature. This parameter is shown as "C2" in the equation below. The units for this parameter are degrees C. If a schedule name is included as the second parameter, the value of this parameter will be overridden by the value from the schedule. The default for this field is 0.0.

#### Field: Constant Temperature Coefficient

This field defines a constant coefficient that is applied to the constant temperature (see previous field). This parameter is shown as "C3" in the equation below. This parameter is dimensionless. The value of this parameter is usually 1.0 if a schedule is used to set C2. This field is ignored if *Sinusoidal Variation of Constant Temperature Coefficient* = Yes. The default for this field is 1.0.

#### Field: External Dry-Bulb Temperature Coefficient

This field defines a constant coefficient that is applied to the outside air dry-bulb temperature. This parameter is shown as "C4" in the equation below. This parameter is dimensionless. The default for this field is 0.0.

#### Field: Ground Temperature Coefficient

This field defines a constant coefficient that is applied to the ground temperature (ref. [Site:GroundTemperature:BuildingSurface](#sitegroundtemperaturebuildingsurface)). This parameter is shown as "C5" in the equation below. This parameter is dimensionless.

#### Field: Wind Speed Coefficient

This field defines a constant coefficient that is applied to the product of the outside air dry-bulb temperature and the wind speed. This parameter is shown as "C6" in the equation below. This parameter has dimensions of inverse velocity or s/m. The default for this field is 0.0.

#### Field: Zone Air Temperature Coefficient

This field defines a constant coefficient that is applied to the temperature of the zone to which this surface belongs. This parameter is shown as "C7" in the equation below. This parameter is dimensionless. The default for this field is 0.0.

#### Field: Constant Temperature Schedule Name

This field is used to supply a schedule name. That schedule will supply the "constant" temperature value C2. Note that the value of the C3 field should normally be 1.0 if a schedule is used for C2. If not blank, this field must be a valid schedule name.

#### Field: Sinusoidal Variation of Constant Temperature Coefficient

This field is optional and can be used to define an alternate method of prescribing the coefficient that is applied to the constant temperature (see the fields Constant Temperature and Constant Temperature Coefficient).  This parameter is shown as "C2" in the equation below.  If this field is omitted, left blank, or set to "No," then C2 is a constant (defined in the field Constant Temperature Coefficient).  However if this is set to "Yes," then the value of C2 varies with a unitary sine wave in the following way:

![](media/image66.png)\


The value for "period" is controlled in the following field.  The value for "time of day" is based on the zone timestep and is in units of hours.  The sine function here uses input as radians.  When using this option, the value for C2 will vary between -1.0 and 1.0 and the value put in the field Constant Temperature Coefficient is not used.  This option cannot be used at the same time as scheduling a constant temperature with the previous field.

#### Field: Period of Sinusoidal Variation 

This field is used to define the period of the sine wave when using the Sinusodial Variation of Constant Temperature Coefficient capability selected in the previous field.  This field is the time period of the sine wave in units of hours.  The default is 24 hours and provides a diurnal sine wave.  The value entered here is "period" in the equation in the previous field.

#### Field: Previous Other Side Temperature Coefficient

This field defines a constant coefficient that is applied to the other side temperature computed by this object from the previous zone time step. This parameter is shown as "C8" in the equation below. This parameter is dimensionless. The default for this field is 0.0.

#### Field: Minimum Other Side Temperature Limit

This field specifies a lower limit for the other side temperature result in degrees C. If blank, there is no lower limit.

#### Field: Maximum Other Side Temperature Limit

This field specifies an upper limit for the other side temperature result in degrees C. If blank, there is no upper limit.

The coefficients listed above are used in the following equation:

![](media/image67.png)\


where:

T = Outside Air Temperature when C1 (Combined convective/radiative film Coeff) > 0

T = Exterior Surface Temperature when C1 (Combined convective/radiative film Coeff) <= 0

Tzone  = Temperature of the zone being simulated (°C)

Toadb  = Dry-bulb temperature of the outdoor air (°C)

Tgrnd  = Temperature of the ground (°C) from Site:GroundTemperature:BuildingSurface

Wspd  = Outdoor wind speed (m/sec)

Tpast  = Other side temperature from previous zone timestep (°C)

![Illustration for Other Side Coefficients](media/illustration-for-other-side-coefficients.png)


~~~~~~~~~~~~~~~~~~~~

    !  Example input using temperature schedule
    SurfaceProperty:OtherSideCoefficients,
        OSCCoef:Zn005:Wall003,   !- Name
        0,                       !- Combined Convective/Radiative Film Coefficient {W/m2-K}
        0.000000,                !- Constant Temperature {C}
        1.000000,                !- Constant Temperature Coefficient
        0.000000,                !- External Dry-Bulb Temperature Coefficient
        0.000000,                !- Ground Temperature Coefficient
        0.000000,                !- Wind Speed Coefficient
        0.000000,                !- Zone Air Temperature Coefficient
        Zn005Wall003OtherSideTempSched;  !- Constant Temperature Schedule Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    !  Example input for outside heat transfer coefficient of 1.23, using Tosdb
    SurfaceProperty:OtherSideCoefficients,
        OSCCoef:Zn005:Wall004,   !- Name
        1.230000,                !- Combined Convective/Radiative Film Coefficient {W/m2-K}
        0.000000,                !- Constant Temperature {C}
        0.000000,                !- Constant Temperature Coefficient
        1.000000,                !- External Dry-Bulb Temperature Coefficient
        0.000000,                !- Ground Temperature Coefficient
        0.000000,                !- Wind Speed Coefficient
        0.000000,                !- Zone Air Temperature Coefficient
        ,                        !- Constant Temperature Schedule Name
        No,                      !- Sinusoidal Variation of Constant Temperature Coefficient
        24,                      !- Period of Sinusoidal Variation {hr}
        0.,                      !- Previous Other Side Temperature Coefficient
        ,                        !- Minimum Other Side Temperature Limit {C}
        ;                        !- Maximum Other Side Temperature Limit {C}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Surface Other Side Coefficients Exterior Air Drybulb Temperature
~~~~~~~~~~~~~~~~~~~~

#### Surface Other Side Coefficients Exterior Air Drybulb Temperature [C]

This is the air temperature applied to the other side of the surface.

## SurfaceProperty:OtherSideConditionsModel 

By referencing the Other Side Conditions Model statement in the surface statements (i.e. Outside Boundary Condition), the boundary conditions for the outer plane of the mass wall can be connected to the appropriate model for various multi-skin components. The types of multi-skin components that use this object include systems that are mounted to the outside surface using standoffs that create a small air gap – see Figure 42. This type of modeling allows using the usual heat transfer calculations for the underlying surface with other types of multi-skin  component models that are available including: unglazed transpired solar collectors, ventilated photovoltaic panels, and naturally ventilated facades.

The boundary condition values are determined dynamically by the program using internal component models. If you want to define other side surface temperatures or convection conditions, then use [SurfaceProperty:OtherSideCoefficients](#surfacepropertyothersidecoefficients) instead of this object.

It should be noted that when other side conditions models are used, solar effects are removed from the surface's outside face heat balance, but are used in modeling the component adjacent to that surface.

In addition, the other side conditions model has been modified to include underground piping system interaction.  The [PipingSystem:Underground:Domain](#pipingsystemundergrounddomain) object represents a mass of ground which may include interaction with, for example, basement surfaces.  In this case, the ground model will internally use the other side condition model hook to update boundary conditions for those surfaces which use that other side condition model name reference.

### Inputs

#### Field: Name

This is the string referenced in the Surface statement that is using OtherSideModel as the Exterior Environment.

#### Field: Type of Modeling

This is a string key selection used to identify the type of model that will be used to determine boundary conditions. The only available choices are "GapConvectionRadiation" or "UndergroundPipingSystemSurface."

![Illustration for Other Side Conditions Model](media/illustration-for-other-side-conditions-model.png)


An example specification is:

~~~~~~~~~~~~~~~~~~~~

    SurfaceProperty:OtherSideConditionsModel,
        UTSC OSCM ZN11,          ! OtherSideConditionsModel Name
        GapConvectionRadiation; ! Type of Modeling used to determine Boundary Conditions
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Surface Other Side Conditions Modeled Convection Air Temperature [C]
    Zone,Average,Surface Other Side Conditions Modeled Convection Heat Transfer Coefficient [W/m2-K]
    Zone,Average,Surface Other Side Conditions Modeled Radiation Temperature [C]
    Zone,Average,Surface Other Side Conditions Modeled Radiation Heat Transfer Coefficient [W/m2-K]
~~~~~~~~~~~~~~~~~~~~

#### Surface Other Side Conditions Modeled Convection Air Temperature [C]

This is the air temperature exposed to the other side of the surface by the model and used in convection heat transfer calculations.

#### Surface Other Side Conditions Modeled Convection Heat Transfer Coefficient [W/m2-K]

This is the surface convection heat transfer coefficient applied to the other side of the surface by the model.

#### Surface Other Side Conditions Modeled Radiation Temperature [C]

This is the effective temperature exposed to the other side of the surface for thermal radiation heat transfer calculations.

#### Surface Other Side Conditions Modeled Radiation Heat Transfer Coefficient [W/m2-K]

This is the effective (Iinearized) radiation heat transfer coefficient applied to the other side of the surface by the model.

## SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections

This object provides options to change the individual convection model equations for dynamic selection when using AdaptiveConvectionAlgorithm. This object is only needed to make changes to the default model selections for any or all of the surface categories. This object is for the inside face, the side of the surface facing a thermal zone.

### Inputs

#### Field: Name

A unique name for the object.

#### Field: Simple Bouyancy Vertical Wall Equation Source

Applies to zone with no HVAC or when HVAC is off.  This is for vertical walls.  The key choice options include:  FohannoPolidoriVerticalWall, ASHRAEVerticalWall, AlamdariHammondVerticalWall, KhalifaEq3WallAwayFromHeat, KhalifaEq6NonHeatedWalls FohannoPolidoriVerticalWall, ISO15099Windows, or UserCurve

#### Field: Simple Bouyancy Vertical Wall User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve

#### Field: Simple Bouyancy Stable Horizontal Equation Source

Applies to zone with no HVAC or when HVAC is off.  This is for horizontal surfaces with heat flow directed for stable thermal stratification.  The key choice options include: WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, or UserCurve

#### Field: Simple Bouyancy Stable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve

#### Field: Simple Bouyancy Unstable Horizontal Equation Source

Applies to zone with no HVAC or when HVAC is off. This is for passive horizontal surfaces with heat flow for unstable thermal stratification.  The key choice options include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, or UserCurve.

#### Field: Simple Bouyancy Unstable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Simple Bouyancy Stable Tilted Equation Source

Applies to zone with no HVAC or when HVAC is off.  This is for tilted surfaces with heat flow for stable thermal stratification.  The key choice options include:  WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, or UserCurve

#### Field: Simple Bouyancy Stable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Simple Bouyancy Unstable Tilted Equation Source

Applies to zone with no HVAC or when HVAC is off.  This is for tilted surfaces with heat flow for unstable thermal stratification.  The key choices include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, or UserCurve.

#### Field: Simple Bouyancy Unstable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Simple Bouyancy Windows Equation Source

Applies to zone with no HVAC or when HVAC is off.  This is for all window surfaces. The key choice options include: ASHRAEVerticalWall, AlamdariHammondVerticalWall, FohannoPolidoriVerticalWall, KaradagChilledCeiling, ISO15099Windows, or UserCurve.

#### Field: Simple Bouyancy Windows Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Floor Heat Ceiling Cool Vertical Wall Equation Source

Applies to zone with in-floor heating and/or in-ceiling cooling.  This is for vertical walls.  The key choice options include:  ASHRAEVerticalWall, AlamdariHammondVerticalWall, KhalifaEq3WallAwayFromHeat, FohannoPolidoriVerticalWall, ISO15099Windows, or UserCurve.

#### Field: Floor Heat Ceiling Cool Vertical Wall Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Floor Heat Ceiling Cool Stable Horizontal Equation Source

Applies to zone with in-floor heating and/or in-ceiling cooling. This is for passive horizontal surfaces with heat flow for stable thermal stratification.  The key choice options include:  WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, or UserCurve.

#### Field: Floor Heat Ceiling Cool Stable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Floor Heat Ceiling Cool Unstable Horizontal Equation Source

Applies to zone with in-floor heating and/or in-ceiling cooling. This is for passive horizontal surfaces with heat flow for unstable thermal stratification.  The key choice options include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, KhalifaEq4CeilingAwayFromHeat, or UserCurve.

#### Field: Floor Heat Ceiling Cool Unstable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Floor Heat Ceiling Cool Heated Floor Equation Source

Applies to zone with in-floor heating and/or in-ceiling cooling.  This is for a floor with active heating elements.  The key choice options include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, AwbiHattonHeatedFloor, or UserCurve

#### Field: Floor Heat Ceiling Cool Heated Floor Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Floor Heat Ceiling Cool Chilled Ceiling Equation Source

Applies to zone with in-floor heating and/or in-ceiling cooling. This is for a ceiling with active cooling elements. The key choice options include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, KaradagChilledCeiling, or UserCurve.

#### Field: Floor Heat Ceiling Cool Chilled Ceiling Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve

#### Field: Floor Heat Ceiling Cool Stable Tilted Equation Source

Applies to zone with in-floor heating and/or in-ceiling cooling. This is for tilted surfaces with heat flow for stable thermal stratification.  The key choice options include:  WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, ISO15099Windows, or UserCurve.

#### Field: Floor Heat Ceiling Cool Stable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Floor Heat Ceiling Cool Unstable Tilted Equation Source

Applies to zone with in-floor heating and/or in-ceiling cooling.  This is for tilted surfaces with heat flow for unstable thermal stratification.  The key choice options include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, ISO15099Windows, or UserCurve.

#### Field: Floor Heat Ceiling Cool Unstable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Floor Heat Ceiling Cool Window Equation Source

Applies to zone with in-floor heating and/or in-ceiling cooling. This is for all window surfaces.  The key choice options include:  ASHRAEVerticalWall, AlamdariHammondVerticalWall, ISO15099Windows, or UserCurve.

#### Field: Floor Heat Ceiling Cool Window Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Wall Panel Heating Vertical Wall Equation Source

Applies to zone with in-wall panel heating. This is for vertical walls that are not actively heated.  The key choice options include:  ASHRAEVerticalWall, AlamdariHammondVerticalWall, KhalifaEq6NonHeatedWalls, FohannoPolidoriVerticalWall, AlamdariHammondVerticalWall, ISO15099Windows, or UserCurve.

#### Field: Wall Panel Heating Vertical Wall Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Wall Panel Heating Heated Wall Equation Source

Applies to zone with in-wall panel heating. This is for vertical walls that are being actively heated.  The key choice options include:  ASHRAEVerticalWall, AlamdariHammondVerticalWall, KhalifaEq5WallNearHeat, AwbiHattonHeatedWall, FohannoPolidoriVerticalWall, AlamdariHammondVerticalWall, or UserCurve.

#### Field: Wall Panel Heating Heated Wall Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Wall Panel Heating Stable Horizontal Equation Source

Applies to zone with in-wall panel heating. This is for horizontal surfaces with heat flow directed for stable thermal stratification. The key choice options include: WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, or UserCurve.

#### Field: Wall Panel Heating Stable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Wall Panel Heating Unstable Horizontal Equation Source

Applies to zone with in-wall panel heating. This is for horizontal surfaces with heat flow directed for unstable thermal stratification. The key choice options include: ASHRAEVerticalWall, WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, KhalifaEq7Ceiling, or UserCurve

#### Field: Wall Panel Heating Unstable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Wall Panel Heating Stable Tilted Equation Source

Applies to zone with in-wall panel heating. This is for tilted surfaces with heat flow for stable thermal stratification.  The key choice options include:  WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, ISO15099Windows, or UserCurve.

#### Field: Wall Panel Heating Stable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve

#### Field: Wall Panel Heating Unstable Tilted Equation Source

Applies to zone with in-wall panel heating. This is for tilted surfaces with heat flow for unstable thermal stratification. The key choice options include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, ISO15099Windows, or UserCurve.

#### Field: Wall Panel Heating Unstable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Wall Panel Heating Window Equation Source

Applies to zone with in-wall panel heating. This is for all window surfaces.  The key choice options include: ASHRAEVerticalWall, AlamdariHammondVerticalWall, FohannoPolidoriVerticalWall, ISO15099Windows, or UserCurve.

#### Field: Wall Panel Heating Window Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Convective Zone Heater Vertical Wall Equation Source

Applies to zone with convective heater.  This is for vertical walls not directly affected by heater.  The key choice options include: ASHRAEVerticalWall, AlamdariHammondVerticalWall, KhalifaEq3WallAwayFromHeat, KhalifaEq6NonHeatedWalls, FohannoPolidoriVerticalWall, ISO15099Windows, or UserCurve

#### Field: Convective Zone Heater Vertical Wall Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Convective Zone Heater Vertical Walls Near Heater Equation Source

Applies to zone with convective heater. This is for vertical walls that are directly affected by heater. Walls are considered "near" when listed in field set for Fraction of Radiant Energy to Surface.  The key choice options include:  ASHRAEVerticalWall, AlamdariHammondVerticalWall, KhalifaEq5WallNearHeat, AwbiHattonHeatedWall, FohannoPolidoriVerticalWall, ISO15099Windows, or UserCurve.

#### Field: Convective Zone Heater Vertical Walls Near Heater Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Convective Zone Heater Stable Horizontal Equation Source

Applies to zone with convective heater.  This is for horizontal surfaces with heat flow directed for stable thermal stratification. The key choice options include:  WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, or UserCurve.

#### Field: Convective Zone Heater Stable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve

#### Field: Convective Zone Heater Unstable Horizontal Equation Source

Applies to zone with convective heater.  This is for horizontal surfaces with heat flow directed for unstable thermal stratification.  The key choice options include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, KhalifaEq4CeilingAwayFromHeat, KhalifaEq7Ceiling, or UserCurve.

#### Field: Convective Zone Heater Unstable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Convective Zone Heater Stable Tilted Equation Source

Applies to zone with convective heater.  This is for tilted surfaces with heat flow for stable thermal stratification. The key choice options include:  WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, or UserCurve.

#### Field: Convective Zone Heater Stable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Convective Zone Heater Unstable Tilted Equation Source

Applies to zone with convective heater.  This is for tilted surfaces with heat flow for unstable thermal stratification.  The key choice options include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, or UserCurve.

#### Field: Convective Zone Heater Unstable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Convective Zone Heater Windows Equation Source

Applies to zone with convective heater.  This is for all window surfaces. The key choice options include:  ASHRAEVerticalWall, AlamdariHammondVerticalWall, KhalifaEq3WallAwayFromHeat, FohannoPolidoriVerticalWall, ISO15099Windows, or UserCurve.

#### Field: Convective Zone Heater Windows Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Central Air Diffuser Wall Equation Source

Applies to zone with mechanical forced central air with diffusers. This is for all wall surfaces.  The key choice options include:  ASHRAEVerticalWall, FisherPedersenCeilingDiffuserWalls, AlamdariHammondVerticalWall, BeausoleilMorrisonMixedAssistedWall, BeausoleilMorrisonMixedOpposingWall, FohannoPolidoriVerticalWall, ISO15099Windows, GoldsteinNovoselacCeilingDiffuserWalls, or UserCurve

#### Field: Central Air Diffuser Wall Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Central Air Diffuser Ceiling Equation Source

Applies to zone with mechanical forced central air with diffusers.  This is for all ceiling surfaces.  The key choice options include:  FisherPedersenCeilingDiffuserCeiling, BeausoleilMorrisonMixedStableCeiling, BeausoleilMorrisonMixedUnstableCeiling, or UserCurve.

#### Field: Central Air Diffuser Ceiling Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Central Air Diffuser Floor Equation Source

Applies to zone with mechanical forced central air with diffusers.  This is for all floor surfaces. The key choice options include:  FisherPedersenCeilingDiffuserFloor, BeausoleilMorrisonMixedStableFloor, BeausoleilMorrisonMixedUnstableFloor, GoldsteinNovoselacCeilingDiffuserFloor, or UserCurve.

#### Field: Central Air Diffuser Floor Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Central Air Diffuser Window Equation Source

Applies to zone with mechanical forced central air with diffusers.  This is for all window surfaces.  The key choice options include:  ASHRAEVerticalWall, FisherPedersenCeilingDiffuserWalls, BeausoleilMorrisonMixedAssistedWall, BeausoleilMorrisonMixedOpposingWall, FohannoPolidoriVerticalWall, AlamdariHammondVerticalWall, ISO15099Windows, GoldsteinNovoselacCeilingDiffuserWindow, or UserCurve.

#### Field: Central Air Diffuser Window Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve

#### Field: Mechanical Zone Fan Circulation Vertical Wall Equation Source

The key choice options include: KhalifaEq3WallAwayFromHeat, ASHRAEVerticalWall, FisherPedersenCeilingDiffuserWalls, AlamdariHammondVerticalWall, BeausoleilMorrisonMixedAssistedWall, BeausoleilMorrisonMixedOpposingWall, FohannoPolidoriVerticalWall, ISO15099Windows, GoldsteinNovoselacCeilingDiffuserWalls, or UserCurve.

#### Field: Mechanical Zone Fan Circulation Vertical Wall Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve

#### Field: Mechanical Zone Fan Circulation Stable Horizontal Equation Source

The key choice options include: WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, or UserCurve.

#### Field: Mechanical Zone Fan Circulation Stable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Mechanical Zone Fan Circulation Unstable Horizontal Equation Source

The key choice options include:  KhalifaEq4CeilingAwayFromHeat, WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, or UserCurve.

#### Field: Mechanical Zone Fan Circulation Unstable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Mechanical Zone Fan Circulation Stable Tilted Equation Source

The key choice options include:  WaltonStableHorizontalOrTilt or UserCurve

#### Field Mechanical Zone Fan Circulation Stable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Mechanical Zone Fan Circulation Unstable Tilted Equation Source

The key choice options include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, or UserCurve.

#### Field: Mechanical Zone Fan Circulation Unstable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Mechanical Zone Fan Circulation Window Equation Source

The key choice options include:  ASHRAEVerticalWall, AlamdariHammondVerticalWall, FohannoPolidoriVerticalWall, ISO15099Windows, GoldsteinNovoselacCeilingDiffuserWindow, or UserCurve.

#### Field: Mechanical Zone Fan Circulation Unstable Tilted Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve

#### Field: Mixed Regime Bouyancy Assisting Flow on Walls Equation Source

The key choice options include:  BeausoleilMorrisonMixedAssistedWall, AlamdariHammondVerticalWall, FohannoPolidoriVerticalWall, ASHRAEVerticalWall, FisherPedersenCeilingDiffuserWalls, GoldsteinNovoselacCeilingDiffuserWalls, or UserCurve.

#### Field: Mixed Regime Bouyancy Assisting Flow on Walls Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Mixed Regime Bouyancy Oppossing Flow on Walls Equation Source

The key choice options include:  BeausoleilMorrisonMixedOpposingWall, AlamdariHammondVerticalWall, FohannoPolidoriVerticalWall, ASHRAEVerticalWall, FisherPedersenCeilingDiffuserWalls, GoldsteinNovoselacCeilingDiffuserWalls, or UserCurve

#### Field: Mixed Regime Bouyancy Oppossing Flow on Walls Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve

#### Field: Mixed Regime Stable Floor Equation Source

The key choice options include:  BeausoleilMorrisonMixedStableFloor, WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, or UserCurve

#### Field: Mixed Regime Stable Floor Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Mixed Regime Unstable Floor Equation Source

The key choice options include:  BeausoleilMorrisonMixedUnstableFloor, WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, or UserCurve.

#### Field: Mixed Regime Unstable Floor Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Mixed Regime Stable Ceiling Equation Source

The key choice options include:  BeausoleilMorrisonMixedStableCeiling, WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, or UserCurve.

#### Field: Mixed Regime Stable Ceiling Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Mixed Regime Unstable Ceiling Equation Source

The key choice options include:  BeausoleilMorrisonMixedUnstableCeiling, WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, or UserCurve.

#### Field: Mixed Regime Unstable Ceiling Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Mixed Regime Window Equation Source

The key choice options include:  GoldsteinNovoselacCeilingDiffuserWindow, ISO15099Windows, or UserCurve.

#### Field: Mixed Regime Window Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

## SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections

Options to change the individual convection model equations for dynamic selection when using AdaptiveConvectionAlgorithm. This object is only needed to make changes to the default model selections for any or all of the surface categories. This object is for the outside face, the side of the surface facing away from the thermal zone.

### Inputs

#### Field: Name

A unique name for the object

#### Field: Wind Convection Windward Vertical Wall Equation Source

This is for just the wind-driven component of the total convection coefficient. The key choice options include: SimpleCombined, TARPWindward, MoWiTTWindward, DOE2Windward, NusseltJurges, McAdams, Mitchell, Blocken, Emmel, or UserCurve.

#### Field: Wind Convection Windward Equation Vertical Wall User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Wind Convection Leeward Vertical Wall Equation Source

This is for just the wind-driven component of the total convection coefficient. The key choice options include:  SimpleCombined, TARPLeeward, MoWiTTLeeward, DOE2Leeward, Emmel, NusseltJurges, McAdams, Mitchell, or UserCurve.

#### Field: Wind Convection Leeward Vertical Wall Equation User Curve Name  

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Wind Convection Horizontal Roof Equation Source

This is for just the wind-driven component of the total convection coefficient. The key choice options include:  SimpleCombined, TARPWindward, MoWiTTWindward, DOE2Windward, NusseltJurges, McAdams, Mitchell, Blocken, Emmel, ClearRoof, or UserCurve.

#### Field: Wind Convection Horizontal Roof User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Natural Convection Vertical Wall Equation Source

This is for just the natural convection portion of the total film coefficient. This is for vertical walls. The key choice options include:  ASHRAEVerticalWall, AlamdariHammondVerticalWall, FohannoPolidoriVerticalWall, ISO15099Windows, UserCurve, or None.

#### Field: Natural Convection Vertical Wall Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Natural Convection Stable Horizontal Equation Source

This is for just the natural convection portion of the total film coefficient.  This is for horizontal surfaces with heat flow directed for stable thermal stratification.  The key choice options include:  WaltonStableHorizontalOrTilt, AlamdariHammondStableHorizontal, UserCurve, or None.

#### Field: Natural Convection Stable Horizontal Equation User Curve Name

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

#### Field: Natural Convection Unstable Horizontal Equation Source

This is for just the natural convection portion of the total film coefficient.  This is for horizontal surfaces with heat flow directed for unstable thermal stratification. The key choice optios include:  WaltonUnstableHorizontalOrTilt, AlamdariHammondUnstableHorizontal, UserCurve, or None

#### Field: Natural Convection Unstable Horizontal Equation User Curve Name 

The SurfaceConvectionAlgorithm:UserCurve named in this field is used when the previous field is set to UserCurve.

## SurfaceConvectionAlgorithm:Inside:UserCurve

This object is used to describe a custom model equation for surface convection heat transfer coefficients. If more than one curve is referenced, or non-blank, then they are all used and the result is the simple addition of all the curve results.

### Inputs

#### Field: Name

Unique name of input object.

#### Field: Reference Temperature for Convection Heat Transfer

This field controls the nature of the reference temperature used with convection coefficient when calculating the heat flow at the surface. Select one of the three choices:  MeanAirTemperature, AdjacentAirTemperature,  SupplyAirTemperature.  MeanAirTemperature is the typical application for the classic convection model used with the complete mixing of room air.  AdjacentAirTemperature applies when used with Roomair models that account for temperature variations within the zone air and directs the model to use the temperature near the surface rather than the average for the entire zone.  SupplyAirTemperature directs the model to use the supply air conditions for the heat transfer conditions.

#### Field: Hc Function of Temperature Difference Curve Name

This field contains the name of separate performance curve or table object that describes *h~c~*, the convection coefficient, as a function of temperature difference. The curve's "x" is absolute value of delta-T (Surface temperature minus air temperature, (C))

#### Field: Hc Function of Temperature Difference Divided by Height Curve Name

This field contains the name of separate performance curve or table object that describes *h~c~*, the convection coefficient, as a function of temperature difference divided by height. The curve's "x" is absolute value of delta-T/Height (Surface temp minus Air temp)/(vertical length scale), (C/m).  For an inside face, the vertical length scale is the zone's interior height.

#### Field: Hc Function of Air Change Rate Curve Name

This field contains the name of separate performance curve or table object that describes *h~c~*, the convection coefficient, as a function of air change rate. The curve's "x" is mechanical ACH (Air Changes per hour from mechanical air system), (1/hr)

#### Field: Hc Function of Air System Volume Flow Rate Divided by Zone Perimeter Length Curve Name

This field contains the name of separate performance curve or table object that describes *h~c~*, the convection coefficient, as a function of air change rate divided perimeter scale. Curve's "x" is mechanical system air flow rate (m^3^/s) divided by zone's length along exterior walls (m).

## SurfaceConvectionAlgorithm:Outside:UserCurve

This object is used to describe a custom model equation for surface convection heat transfer coefficients. If more than one curve is referenced, or non-blank, then they are all used and the result is the simple addition of all the curve results.

### Inputs

#### Field: Name

Unique name of input object.

#### Field: Wind Speed Type for Curve

This field specifies what sort of wind velocity data should be used in when evaluating the curve in the following field.  There are for key choice options. "WeatherFile" directs  using the unmodified value from the epw file or design weather data.  "HeightAdjust" uses the value from the epw file modified for height above ground, as determined by the z cooridinate, using the site terrain and weather station information.  "ParallelComponent" uses the value from the epw file modified to take just the velocity component that is parallel to the surface.  "ParallelComponentHeightAdjust" uses the height adjusted wind velocity and then computes the parallel component.

#### Field: Hf Function of Wind Speed Curve Name

This field contains the name of separate performance curve or table object that describes *h~f~*, the forced convection coefficient, as a function of wind speed. The curve's "x" is wind speed as defined by the method chosen in the previous field.

#### Field: Hn Function of Temperature Difference Curve Name

This field contains the name of separate performance curve or table object that describes *h~n~*, the natural convection coefficient, as a function of temperature difference. Curve's "x" is absolute value of delta-T (Surface temperature minus air temperature, (C))

#### Field: Hc Function of Temperature Difference Divided by Height Curve Name

This field contains the name of separate performance curve or table object that describes *h~n~*, the natural convection coefficient, as a function of temperature difference divided by height. Curve's "x" is absolute value of delta-T/Height (Surface temp minus Air temp)/(vertical length scale), (C/m).  For an outside face the vertical length scale is the exterior facade's overall height.

## SurfaceProperty:ConvectionCoefficients

The convection coefficients of each surface, both exterior and interior, are automatically calculated during EnergyPlus execution. These calculations are "governed" by other objects such as the [SurfaceConvectionAlgorithm:Inside](#surfaceconvectionalgorithminside) (overall default), the [Zone](#zone) object's field called [Zone](#zone) Inside Convection Algorithm ([Zone](#zone) Default), the  and the [SurfaceConvectionAlgorithm:Outside](#surfaceconvectionalgorithmoutside) (overall default), and/or the [Zone](#zone) object's field called [Zone](#zone) Outside Convection Algorithm ([Zone](#zone) Default). Usually, that will be enough flexibility for most users. However, if you need to match pre-existing convection coefficients (from another program) or are trying to match a test suite of results, you may wish to use the "override" convection coefficients in the following object. This object allows for a single surface to be given specific convection coefficients.

Note that using these in conjunction, in particular, the "Simple" option on either the Outside Convection Algorithm or the [Zone](#zone) Outside Convection Algorithm field will result in a combined coefficient regardless of choice chosen here.

> Note that surfaces with "[SurfaceProperty:OtherSideCoefficients](#surfacepropertyothersidecoefficients)" cannot use this object with the "outside" coefficient – attempting to do so will cause a severe error; [SurfaceProperty:OtherSideCoefficients](#surfacepropertyothersidecoefficients) surfaces can apply an "inside" coefficient. And, surfaces with "Ground" exposure do not use the "outside" coefficient that might be supplied here. Note, too, that some lower boundaries are used regardless by certain surface types (i.e. [Window](#window)) or certain algorithm types.

### Inputs

#### Field: Surface Name

This field is the applicable surface name for the user supplied convection coefficient.

#### Fields (Convection Location, Type, Coefficient & Schedule Name)

For simplicity, the descriptions of these field occur together – however, the fields are used sequentially when put into the IDF file (reference the IDF examples following the descriptions).

#### Field: Convection Coefficient 1 Location

#### Field: Convection Coefficient 2 Location

This field contains the word "Outside" or "Inside" depending on which location is being described.

#### Field: Convection Coefficient 1 Type

#### Field: Convection Coefficient 2 Type

The entries can be of several types: Value (simple numeric value), Schedule (name of schedule with the values), the usual key choices for overall models for Outside or Inside (Simple, SimpleCombined, TARP, AdaptiveConvectionAlgorithm etc.), the key choices for individual convection equations used for customizing the adaptive algorithm, or a custom user defined correlation. The field should contain one of the keys listed in the table below along with face they can be applied.  The definitions of the models and key choices are discussed under [SurfaceConvectionAlgorithm:Inside](#surfaceconvectionalgorithminside) , [SurfaceConvectionAlgorithm:Outside](#surfaceconvectionalgorithmoutside), [SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections](#surfaceconvectionalgorithminsideadaptivemodelselections), and [SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections](#surfaceconvectionalgorithmoutsideadaptivemodelselections).

Key choice|Applies to Inside or Outside
----------|----------------------------
Value|Both
Schedule|Both
Simple|Inside
SimpleCombined|Outside
TARP|Both
DOE-2|Outside
MoWitt|Outside
AdaptiveConvectionAlgorithm|Both
ASHRAEVerticalWall    |Both
WaltonUnstableHorizontalOrTilt|Both
WaltonStableHorizontalOrTilt|Both
FisherPedersenCeilingDiffuserWalls|Inside
FisherPedersenCeilingDiffuserCeiling|Inside
FisherPedersenCeilingDiffuserFloor|Inside
AlamdariHammondStableHorizontal|Both
AlamdariHammondUnstableHorizontal|Both
AlamdariHammondVerticalWall|Both
KhalifaEq3WallAwayFromHeat|Inside
KhalifaEq4CeilingAwayFromHeat|Inside
KhalifaEq5WallNearHeat|Inside
KhalifaEq6NonHeatedWalls|Inside
KhalifaEq7Ceiling|Inside
AwbiHattonHeatedFloor|Inside
AwbiHattonHeatedWall|Inside
BeausoleilMorrisonMixedAssistedWall|Inside
BeausoleilMorrisonMixedOpposingWall|Inside
BeausoleilMorrisonMixedStableFloor|Inside
BeausoleilMorrisonMixedUnstableFloor|Inside
BeausoleilMorrisonMixedStableCeiling|Inside
BeausoleilMorrisonMixedUnstableCeiling|Inside
FohannoPolidoriVerticalWall       |Both
KaradagChilledCeiling|Inside
ISO15099Windows|Inside
GoldsteinNovoselacCeilingDiffuserWindow|Inside
GoldsteinNovoselacCeilingDiffuserWalls|Inside
GoldsteinNovoselacCeilingDiffuserFloor|Inside
SimpleCombined      |Outside
NusseltJurges|Outside
McAdams|Outside
Mitchell|Outside
BlockenWindard|Outside
Emmel|Outside
ClearRoof|Outside
UserCurve|Both

#### Field: Convection Coefficient 1

#### Field: Convection Coefficient 2

If the Convection type was "Value", then this field is filled and contains the simple value to be used. Otherwise, this can be blank.

#### Field: Convection Coefficient 1 Schedule Name

#### Field: Convection Coefficient 2 Schedule Name

If the Convection type was "Schedule", then this field contains the name of a schedule describing the value to be used during the time intervals for the schedule.

The complete IDD definition for the ConvectionCoefficients object follows:

#### Field: Convection Coefficient 1 User Curve Name

#### Field: Convection Coefficient 2 User Curve Name

If the Convection type was "UserCurve", then this field contains the name of a SurfaceConvectionAlgorithm:UserCurve input objct describing the model equations to be used during the time intervals for the schedule.

In IDF usage:

~~~~~~~~~~~~~~~~~~~~

    SurfaceProperty:ConvectionCoefficients,
    Zn001:Wall001,  ! Surface Name
    Outside,        ! Convection Coefficient 1 Location
    Value,          ! Convection Coefficient 1 Type
    9.8,            ! Convection Coefficient 1
    ,               ! Convection Coefficient 1 Schedule Name
    ,               ! Convection Coefficient 1 User Curve Name
    Inside,         ! Convection Coefficient 2 Location
    Schedule,       ! Convection Coefficient 2 Type
    ,               ! Convection Coefficient 2 {blank because using schedule}
    MyInteriorCC,   ! Convection Coefficient 2 Schedule Name
    ;               ! Convection Coefficient 2 User Curve Name

    SurfaceProperty:ConvectionCoefficients,
    Zn001:Wall002,  ! Surface Name
    Inside,         ! Convection Coefficient 1 Location
    Value,          ! Convection Coefficient 1 Type
    .8,             ! Convection Coefficient 1
    ,               ! Convection Coefficient 1 Schedule Name
    ,               ! Convection Coefficient 1 User Curve Name
    Outside,        ! Convection Coefficient 2 Location
    Value,          ! Convection Coefficient 2 Type
    5.5,            ! Convection Coefficient 2
    ;               ! Convection Coefficient 2 User Curve Name

    SurfaceProperty:ConvectionCoefficients,
    Zn001:Wall003,  ! Surface Name
    Outside,        ! Convection Coefficient 1 Location
    Value,          ! Convection Coefficient 1 Type
    9.8;            ! Convection Coefficient 1
~~~~~~~~~~~~~~~~~~~~

## SurfaceProperty:ConvectionCoefficients:MultipleSurface

The convection coefficients of each surface, both outside and inside, are automatically calculated during EnergyPlus execution. These calculations are "governed" by other objects such as the Inside Convection Algorithm (overall default) and the [Zone](#zone) Inside Convection Algorithm ([Zone](#zone) Default) and the Outside Convection Algorithm (overall default) and/or the [Zone](#zone) Outside Convection Algorithm ([Zone](#zone) Default). Usually, that will be enough flexibility for most users. However, if you need to match pre-existing convection coefficients (from another program) or are trying to match a test suite of results, you may wish to use the "override" convection coefficients in the following object. This object is similar to the preceding "ConvectionCoefficients" object but allows multiple surfaces to be assigned a type with one object entry.

Note that using these in conjunction, in particular, the "Simple" option on either the Outside Convection Algorithm or the [Zone](#zone) Outside Convection Algorithm field will result in a combined coefficient regardless of choice chosen here.

> Note that surfaces with "[SurfaceProperty:OtherSideCoefficients](#surfacepropertyothersidecoefficients)" cannot use this object with the "outside" coefficient – attempting to do so will ignore OSC surfaces during a multiple surface "apply"; [SurfaceProperty:OtherSideCoefficients](#surfacepropertyothersidecoefficients) surfaces can apply an "inside" coefficient. And, surfaces with "Ground" exposure do not use the "outside" coefficient that might be supplied here. Note, too, that some lower boundaries are used regardless by certain surface types (i.e. [Window](#window)) or certain algorithm types.

### Inputs

#### Field: Surface Type

This field is the applicable surface name for the user supplied convection coefficient. The allowable surface types are:

- AllExteriorSurfaces  (all surfaces that are "external environment" surfaces)
- AllExteriorWindows (all windows that are "external environment" surfaces)
- AllExteriorWalls
- AllExteriorRoofs
- AllExteriorFloors
- AllInteriorSurface (all surfaces that are "internal" surfaces)
- AllInteriorWindows
- AllInteriorCeilings
- AllInteriorFloors

#### Fields (Convection Location, Type, Coefficient & Schedule Name)

For simplicity, the descriptions of these field occur together – however, the fields are used sequentially when put into the IDF file (reference the IDF examples following the descriptions).

#### Field: Convection Coefficient 1 Location

#### Field: Convection Coefficient 2 Location

This field contains the word "Outside" or "Inside" depending on which location is being described.

#### Field: Convection Coefficient 1 Type

#### Field: Convection Coefficient 2 Type

The entries can be of several types: Value (simple numeric value), Schedule (name of schedule with the values), the usual key choices for overall models for Outside or Inside (Simple, SimpleCombined, TARP, AdaptiveConvectionAlgorithm etc.), the key choices for individual convection equations used for customizing the adaptive algorithm, or a custom user defined correlation. The field should contain one of the keys listed in the table below along with face they can be applied.  The definitions of the models and key choices are discussed under [SurfaceConvectionAlgorithm:Inside](#surfaceconvectionalgorithminside) , [SurfaceConvectionAlgorithm:Outside](#surfaceconvectionalgorithmoutside), [SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections](#surfaceconvectionalgorithminsideadaptivemodelselections), and [SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections](#surfaceconvectionalgorithmoutsideadaptivemodelselections).

Key choice|Applies to Inside or Outside
----------|----------------------------
Value|Both
Schedule|Both
Simple|Inside
SimpleCombined|Outside
TARP|Both
DOE-2|Outside
MoWitt|Outside
AdaptiveConvectionAlgorithm|Both
ASHRAEVerticalWall    |Both
WaltonUnstableHorizontalOrTilt|Both
WaltonStableHorizontalOrTilt|Both
FisherPedersenCeilingDiffuserWalls|Inside
FisherPedersenCeilingDiffuserCeiling|Inside
FisherPedersenCeilingDiffuserFloor|Inside
AlamdariHammondStableHorizontal|Both
AlamdariHammondUnstableHorizontal|Both
AlamdariHammondVerticalWall|Both
KhalifaEq3WallAwayFromHeat|Inside
KhalifaEq4CeilingAwayFromHeat|Inside
KhalifaEq5WallNearHeat|Inside
KhalifaEq6NonHeatedWalls|Inside
KhalifaEq7Ceiling|Inside
AwbiHattonHeatedFloor|Inside
AwbiHattonHeatedWall|Inside
BeausoleilMorrisonMixedAssistedWall|Inside
BeausoleilMorrisonMixedOpposingWall|Inside
BeausoleilMorrisonMixedStableFloor|Inside
BeausoleilMorrisonMixedUnstableFloor|Inside
BeausoleilMorrisonMixedStableCeiling|Inside
BeausoleilMorrisonMixedUnstableCeiling|Inside
FohannoPolidoriVerticalWall       |Both
KaradagChilledCeiling|Inside
ISO15099Windows|Inside
GoldsteinNovoselacCeilingDiffuserWindow|Inside
GoldsteinNovoselacCeilingDiffuserWalls|Inside
GoldsteinNovoselacCeilingDiffuserFloor|Inside
SimpleCombined      |Outside
NusseltJurges|Outside
McAdams|Outside
Mitchell|Outside
BlockenWindard|Outside
Emmel|Outside
ClearRoof|Outside
UserCurve|Both

#### Field: Convection Coefficient 1

#### Field: Convection Coefficient 2

If the Convection type was "Value", then this field is filled and contains the simple value to be used. Otherwise, this can be blank.

#### Field: Convection Coefficient 1 Schedule Name

#### Field: Convection Coefficient 2 Schedule Name

If the Convection type was "Schedule", then this field contains the name of a schedule describing the value to be used during the time intervals for the schedule.

#### Field: Convection Coefficient 1 User Curve Name

#### Field: Convection Coefficient 2 User Curve Name

If the Convection type was "UserCurve", then this field contains the name of a SurfaceConvectionAlgorithm:UserCurve input objct describing the model equations to be used during the time intervals for the schedule.

In IDF usage:

~~~~~~~~~~~~~~~~~~~~

    SurfaceProperty:ConvectionCoefficients:MultipleSurface,
    AllExteriorWindows,  ! Surface Types
    Outside,             ! Convection Coefficient 1 Location
    MoWitt;              ! Convection Coefficient 1 Type
~~~~~~~~~~~~~~~~~~~~

## Convection Coefficient Application Hierarchy

Ultimate flexibility and possibly ultimate user confusion can result from the convection coefficient possibilties in EnergyPlus. The objects that control how convection models are assigned to individual surfaces  are:

- SurfaceConvectionAlgorithm:Inside
- SurfaceConvectionAlgorithm:Outside
- [Zone](#zone), Field: [Zone](#zone) Inside Convection Algorithm
- [Zone](#zone), Field: [Zone](#zone) Outside Convection Algorithm
- SurfaceProperty:ConvectionCoefficients
- SurfaceProperty:ConvectionCoefficients:MultipleSurface

General to Specific, they are assigned in this order:

Table: Convection Coefficient Assignment Hierarchy

Objects/Description|Action
-------------------|------
SurfaceConvectionAlgorithm:\* |Assigns for all surfaces in the run.
[Zone](#zone), Fields: [Zone](#zone) \* Convection Algorithm|Trumps global assignment and assigns for all surfaces in [Zone](#zone).
SurfaceProperty:...:MultipleSurfaces|Trumps above and assigns for specific types of surfaces.
Two Multiple Surface Assignments overlapping (such as AllExteriorSurfaces vs AllExteriorWalls)|Order in IDF is now important. Whichever is first gets the cookie and the second gets a warning.
Specific Surface Assignment|Trumps all Above.

There are additional objects that provide fine control over the models that get assigned.

- SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections
- SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections
- SurfaceConvectionAlgorithm:Inside:UserCurve
- SurfaceConvectionAlgorithm:Outside:UserCurve

## Convection Coefficients Outputs

Outputs for the User Supplied Convection Coefficients appear as values in the Surface Output Variables (*Surface Inside Face Convection Heat Transfer Coefficient* and *Surface Outside Face Convection Heat Transfer Coefficient*). What the program expects to use is shown in the Surface Details report (*Output:Surfaces:List, Details;*)

When EnergyPlus is set to Display Advanced Variables ([Output:Diagnostics](#outputdiagnostics), DisplayAdvancedVariables;), then additional output variables are available that indicate the status of convection modeling by identifying which models are in effect at a given time.  The adaptive convection algorithms may switch between models over time and the following output variables provide a way to monitor this behavior.  These outputs are integer codes and the integer values are explained in tables.

### Surface Inside Face Convection Classification Index 

This variable reports how the surface was classified as part of the adaptive convection algorithm for the inside face.  The algorithm examines probable flow regimes, heat flow directions, orientations, HVAC equipment connections, and current operating status to assign each surface a category.  The numbers in this report are integer codes that correspond to surface categories as described in the following table.

Code|Zone Airflow Regime|Type of surface|Heat Flow
----|-------------------|---------------|---------
1|A1 Radiant Heated Floor or Chilled Ceiling|Vertical Wall|Any
2|A1 Radiant Heated Floor or Chilled Ceiling|Horizontal|Stable
3|A1 Radiant Heated Floor or Chilled Ceiling|Horizontal|Unstable
4|A1 Radiant Heated Floor or Chilled Ceiling|Heated Floor|Unstable
5|A1 Radiant Heated Floor or Chilled Ceiling|Chilled Ceiling|Unstable
6|A1 Radiant Heated Floor or Chilled Ceiling|Tilted|Stable
7|A1 Radiant Heated Floor or Chilled Ceiling|Tilted|Unstable
8|A1 Radiant Heated Floor or Chilled Ceiling|Window|Any
9|A2 Radiant Wall Heat|Non-heated Vertical Wall|Any
10|A2 Radiant Wall Heat|Heated wall|Any
11|A2 Radiant Wall Heat|Horizontal|Stable
12|A2 Radiant Wall Heat|Horizontal|Unstable
13|A2 Radiant Wall Heat|Tilted|Stable
14|A2 Radiant Wall Heat|Tilted|Unstable
15|A2 Radiant Wall Heat|Windows|Any
16|A3 Simple Bouyancy|Vertical Walls|Any
17|A3 Simple Bouyancy|Horizontal|Stable
18|A3 Simple Bouyancy|Horizontal|Unstable
19|A3 Simple Bouyancy|Tilted|Stable
20|A3 Simple Bouyancy|Tilted|Unstable
21|A3 Simple Bouyancy|Windows|Any
22|B Convective [Zone](#zone) Heat|Vertical Walls|Any
23|B Convective [Zone](#zone) Heat|Vertical Walls near heater|Any
24|B Convective [Zone](#zone) Heat|Horizontal|Stable
25|B Convective [Zone](#zone) Heat|Horizontal|Unstable
26|B Convective [Zone](#zone) Heat|Tilted|Stable
27|B Convective [Zone](#zone) Heat|Tilted|Unstable
28|B Convective [Zone](#zone) Heat|Windows|Any
29|C Central Air Diffuser|Walls|Any
30|C Central Air Diffuser|Ceiling|Any
31|C Central Air Diffuser|Floor|Any
32|C Central Air Diffuser|Windows|Any
33|D [Zone](#zone) Fan Unit|Walls|Any
34|D [Zone](#zone) Fan Unit|Horizontal|Stable
35|D [Zone](#zone) Fan Unit|Horizontal|Unstable
36|D [Zone](#zone) Fan Unit|Tilted|Stable
37|D [Zone](#zone) Fan Unit|Tilted|Unstable
38|D [Zone](#zone) Fan Unit|Windows|Any
39|E Mixed Forced and Bouyancy|Walls|Assisting
40|E Mixed Forced and Bouyancy|Walls|Opposing
41|E Mixed Forced and Bouyancy|Floor|Stable
42|E Mixed Forced and Bouyancy|Floor|Unstable
43|E Mixed Forced and Bouyancy|Ceiling|Stable
44|E Mixed Forced and Bouyancy|Ceiling|Unstable
45|E Mixed Forced and Bouyancy|Windows|Any

### Surface Inside Face Convection Model Equation Index

This variable reports the specific model equation used to calculate the inside face convection coefficient.  This can vary when using the adaptive convection algorithm and so the result of that selection algorithm is reported here.  The following table lists the models associated with specific interger codes reported here.  The models correspond to key words used in input objects.

------------|---------------------
Report Code |Related key and model
200|UserValue
201|UserSchedule
202|UserCurve
203|ASHRAEVerticalWall
204|WaltonUnstableHorizontalOrTilt
205|WaltonStableHorizontalOrTilt
206|FisherPedersenCeilDiffuserFloor
207|FisherPedersenCeilDiffuserCeiling
208|FisherPedersenCeilDiffuserWalls
209|AlamdariHammondStableHorizontal
210|AlamdariHammondVerticalWall
211|AlamdariHammondUnstableHorizontal
212|KhalifaEq3WallAwayFromHeat
213|KhalifaEq4CeilingAwayFromHeat
214|KhalifaEq5WallNearHeat
215|KhalifaEq6NonHeatedWalls
216|KhalifaEq7Ceiling
217|AwbiHattonHeatedFloor
218|AwbiHattonHeatedWall
219|BeausoleilMorrisonMixedAssistingWall
220|BeausoleilMorrisonMixedOppossingWall
221|BeausoleilMorrisonMixedStableCeiling
222|BeausoleilMorrisonMixedUnstableCeiling
223|BeausoleilMorrisonMixedStableFloor
224|BeausoleilMorrisonMixedUnstableFloor
225|FohannoPolidoriVerticalWall
226|KaradagChilledCeiling
227|ISO15099Windows
228|GoldsteinNovoselacCeilingDiffuserWindow
229|GoldsteinNovoselacCeilingDiffuserWalls
230|GoldsteinNovoselacCeilingDiffuserFloor

### Surface Inside Face Convection Reference Air Index

The inside face convection heat transfer calculations can be based on different reference air temperatures.  This reference air temperature can vary during the simulation when the adaptive algorithm selects models that use different references.  The following table lists the meaning of the integer codes.

Report Code|Reference Air Temperature Method
-----------|--------------------------------
1|Zone mean air temperature
2|Surface adjacent air temperature
3|Supply air temperature

### Surface Outside Face Convection Classification Index

This variable reports how the surface was classified as part of the adaptive convection algorithm for the outside face.  The algorithm examines the wind direction, heat flow directions and orientations to assign each surface a category.  The numbers in this report are integer codes that correspond to surface categories as described in the following table.

Report Code|Surface Classification
-----------|----------------------
101|Vertical Wall, Windward
102|Vertical Wall, Leeward
103|Roof, stable heat flow direction
104|Roof, unstable heat flow direction

### Surface Outside Face Forced Convection Model Equation Index

### Surface Outside Face Natural Convection Model Equation Index

These variables report the specific model equation used to calculate the outside face's convection coefficient.  They can vary when using the adaptive convection algorithm and so the results of that selection algorithm are reported in these variables.  The following table lists the models associated with specific integer codes that might be reported here.  The models correspond to key words used in input objects.

-----------|---------------------
Report Code|Related key and model
300 |None
301|UserValue
302|UserSchedule
303|UserCurve
304|ASHRAESimpleCombined
305|NaturalASHRAEVerticalWall
306|NaturalWaltonUnstableHorizontalOrTilt
307|NaturalWaltonStableHorizontalOrTilt
308|SparrowWindward
309|SparrowLeeward
310|MoWiTTWindward
311|MoWiTTLeeward
312|DOE2Windward
313|DOE2Leeward
314|NusseltJurges
315|McAdams
316|Mitchell
317|ClearRoof
318|BlockenWindward
319|EmmelVertical
320|EmmelRoof
321|AlamdariHammondVerticalWall
322|FohannoPolidoriVerticalWall
323|ISO15099Windows
324|AlamdariHammondStableHorizontal
325|AlamdariHammondUnstableHorizontal

## SurfaceProperties:VaporCoefficients

Advanced/Research Usage:. The internal and external vapor transfer coefficients that are used by the CombinedHeatAndMoistureFiniteElement model are automatically calculated during EnergyPlus execution using information on the convection coefficients. However it is sometimes useful to be able to "override" the calculation and set fixed values of vapor transfer coefficient for a single surface. These coefficients are only used by the CombinedHeatAndMoistureFiniteElement model and will be ignored by other solution algorithms.

### Inputs

#### Field: Surface Name

This field is the applicable surface name for the user supplied vapor transfer coefficient.

#### Field: Constant External Vapor Transfer Coefficient

Select ‘yes' to use the value supplied as the external vapor transfer coefficient

#### Field: External Vapor Coefficient Value

HAMT will use this value to calculate the vapour transfer into and out off the external surface of this surface. Units are kg/Pa.s.m2.

#### Field: Constant Internal vapor Transfer Coefficient

Select ‘yes' to use the value supplied as the internal vapor transfer coefficient

#### Field: Internal Vapor Coefficient Value

The CombinedHeatAndMoistureFiniteElement algorithm will use this value to calculate the vapor transfer into and out of the internal surface of this surface. Units are kg/Pa.s.m2.

Below is an example input of vapor transfer coefficients for a surface.

~~~~~~~~~~~~~~~~~~~~

    SurfaceProperties:VaporCoefficients,
          South wall,     !- Surface Name
          Yes,     !- Constant External Vapor Transfer Coefficient
          0.0000000625,     !- External Vapor Coefficient Value
          Yes,     !- Constant Internal vapor Transfer Coefficient
          0.00000002;     !- Internal Vapor Coefficient Value
~~~~~~~~~~~~~~~~~~~~

## SurfaceProperty:ExteriorNaturalVentedCavity

This object is used to model a multi-skin exterior heat transfer surface. This is a special case where the outside face is a slightly detached layer forming a naturally ventilated cavity. The actual outer surface is referred to as the baffle. The modeling here assumes that the heat capacity in the outer baffle can be neglected since it is much lower than the underlying mass surface. This object is used with the [BuildingSurface:Detailed](#buildingsurfacedetailed) object where the Heat Transfer surfaces are referred to as the underlying surfaces. The constructions and materials for the [BuildingSurface:Detailed](#buildingsurfacedetailed) object should reflect the construction of just the underlying surface. The [SurfaceProperty:ExteriorNaturalVentedCavity](#surfacepropertyexteriornaturalventedcavity) object is used to describe the decoupled layer, or baffle, and the characteristics of the cavity and openings for natural ventilation. This object is also used in conjunction with the OtherSideConditionsModel.

The area and orientation are obtained from **BuildingSurface:Detailed** objects, which are referenced by name. This object can be used to model certain types of photovoltaic mounting configurations such as interlocking roof pavers. If the baffle covers only part of a surface, then that surface should be split into separate **BuildingSurface:Detailed** objects where one matches the size of the baffle. A single baffle can be associated with as many **BuildingSurface:Detailed** objects as desired (although if you need to use more than 10 surfaces, then the IDD will need to be extended). The base heat transfer surfaces need not be contiguous nor have the same orientation, but the program will issue warnings if surfaces have widely ranging tilts and azimuths.

Note that the model involves predicting the rates that ambient air moves in and out of the cavity. Accurate modeling of these air flows would be extremely challenging and so the models provided through this object are simplistic engineering models based on discharge coefficients that are sensitive to wind and bouancy effects. The accuracy depends on the values for, and applicability of, the discharge coefficients and unfortunately little research is available to help characterize these. The models should be considered rudimentary and the user is encouraged to explore different values for the coefficients in attempts to bound the importance of natural ventilation for the cavities. See the Engineering Reference for more details.

### Inputs

#### Field: Name

This field contains a unique name for the ventilated cavity.

#### Field: Boundary Conditions Model Name

This field contains the name of an [SurfaceProperty:OtherSideConditionsModel](#surfacepropertyothersideconditionsmodel) object declared elsewhere in the input file. This will connect the baffle and ventilated cavity to the exterior boundary conditions for the underlying heat transfer surface.

#### Field: Area Fraction of Openings

This field is used to enter an area fraction for what part of the baffle consists of openings. The area of the openings will set to the product of this field and the sum of the area of the underlying surfaces.

#### Field: Thermal Emissivity of Exterior Baffle Material

This field is used to enter the thermal emissivity of the baffler. This surface property is for longwave infrared radiation. The property is used for both sides of collector. Most painted materials have an emissivity of 0.9.

#### Field: Solar Absorptivity of Exterior Baffle

This field is used to enter the solar absorbtivity of the baffle. This surface property is for shortwave, solar radiation. The property is used for the front side of the baffle that faces the environment. Darker colors have a higher absorptivity. While black is the highest performance, other colors might be used to match the color scheme of the rest of the façade.

#### Field: Height Scale for Buoyancy-Driven Ventilation

This field is used to enter a nominal height scale (m) for prediction of ventilation induced by bouancy. This value (![](media/image70.png) ) is defined as the height from the midpoint of the lower opening to the neutral pressure level. Increasing the value will increase the ventilation rate due to buoyancy.

#### Field: Effective Thickness of Cavity Behind Exterior Baffle

This field is used to enter a nominal gap thickness (m) for the collector. If the baffle is corrugated, use the average depth. This distance value is only used when the collector is near horizontal to determine a length scale in the vertical direction for buoyancy calculations. For example, if the collector is mounted on a flat roof, its tilt-adjusted height is zero and the program will use this gap thickness as a length scale rather than the height from the previous field.

#### Field: Ratio of Actual Surface Area to Projected Surface Area

This field is used to enter a factor that accounts for the extra surface area resulting from and uneven baffle surface. Corrugations may be present to help stiffen the baffle or ventilated roofing tiles may have more surface are for convection heat transfer than the underlying surface. The projected surface area is obtained by the program from the (flat) underlying surfaces. If the baffle is flat then this ratio is 1.0. If the baffle is corrugated, then this ratio will be greater than one with a typical value might be 1.165.

#### Field: Roughness of Exterior Surface

This field is used to describe the relative roughness of the baffle material. This field is similar to one in the **Material** object. This parameter only influences the convection coefficients, more specifically the exterior convection coefficient. A special keyword is expected in this field with the options being "VeryRough", "Rough", "MediumRough", "MediumSmooth", "Smooth", and "VerySmooth" in order of roughest to smoothest options.

#### Field: Effectiveness for Perforations with Respect to Wind

This field is used to enter a value for the coefficient used to determine natural air exchanges from wind. Wind will cause exterior air to move in and out of the cavity. Cv is an arbitrary coefficient used to model the effectiveness of openings and depends on opening geometry and the orientation with respect to the wind. Cv should probably be in the range 0.05 to 0.65. Increasing Cv will increase the amount of natural ventilation. The following equation shows how Cv is used in the program to predict the volumetric flow rate due to wind:

![](media/image71.png)\


#### Field: Discharge Coefficient for Openings with Respect to Buoyancy Driven Flow

This field is used to enter a value for the coefficient used to determine natural air exchanges from buoyancy. Stack or buoyancy effects will cause exterior air to move in and out of the cavity. Cd is an arbitrary discharge coefficient that depends on the geometry of the opening. Cd should probably be in the range 0.1 to 1.0. Increasing Cd will increase the amount of natural ventilation. The following equations show how Cd is used in the program to predict the volume flow rate due to buoyancy:

![](media/image72.png)   (if ![](media/image73.png) )

![](media/image74.png)   (if ![](media/image75.png)  and baffle is vertical)

where ![](media/image76.png)  is the value input into the field above for the height scale for bouyancy-driven ventilation.

#### Field(s): Surface <1 thru x> Name

The remaining fields are used to name the **BuildingSurface:Detailed** objects that are associated with the exterior naturally vented cavity. These are the underlying heat transfer surfaces and are defined elsewhere in the input file. These surfaces should all specify OtherSideConditionsModel as their exterior environment. The input object can currently accommodate up to ten surfaces, but it is extensible by modifying the Energy+.idd entry.

An example IDF entry is

~~~~~~~~~~~~~~~~~~~~

    SurfaceProperty:ExteriorNaturalVentedCavity,
        PVRoofPaverExtVentCav1 ,   ! Name
        PVRoofPaverSystem1,        ! OtherSideConditionsModel Object Name
        0.02,    ! Area Fraction of Openings
        0.9,     ! Thermal Emissivity of Exterior Baffle Material
        0.92,    ! Solar Absorbtivity of Exterior Baffle
        0.05,    ! Height scale for bouyancy-driven ventilation
        0.05,    ! Effective Thickness of Cavity Behind Exterior Baffle
        0.97,    ! Ratio of Actual surface area to projected surface area
        Smooth , ! Roughness of collector
        0.1 ,    ! Cv, Effectiveness for perforations with respect to Wind
        0.5 ,    ! Cd, Discharge Coefficient for Openings with respect to bouyancy-driven flow
        Zn001:Roof001 ;    ! Surface Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

In addition to related output that can be obtained for all surfaces, these outputs are available for exterior naturally vented cavity configurations:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Surface Exterior Cavity Air Drybulb Temperature [C]
    HVAC,Average, Surface Exterior Cavity Baffle Surface Temperature [C]
    HVAC,Average, Surface Exterior Cavity Total Natural Ventilation Air Change Rate [ACH]
    HVAC,Average, Surface Exterior Cavity Total Natural Ventilation Mass Flow Rate [kg/s]
    HVAC,Average, Surface Exterior Cavity Natural Ventilation from Wind Mass Flow Rate [kg/s]
    HVAC,Average, Surface Exterior Cavity Natural Ventilation from Buoyancy Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### Surface Exterior Cavity Air Drybulb Temperature [C]

The temperature of air inside the cavity behind the baffle.

#### Surface Exterior Cavity Baffle Surface Temperature [C]

The surface temperature of the exterior baffle material itself.

#### Surface Exterior Cavity Total Natural Ventilation Air Change Rate [ACH]

The rate of natural ventilation air exchange between the plenum and ambient when the collector is inactive in Air Changes per Hour.

#### Surface Exterior Cavity Total Natural Ventilation Mass Flow Rate [kg/s]

The mass flow rate of natural ventilation air exchange between the plenum and ambient when the collector is inactive.

#### Surface Exterior Cavity Natural Ventilation from Wind Mass Flow Rate [kg/s]

The part of mass flow rate of natural ventilation air exchange between the plenum and ambient when the collector is inactive due to wind-driven forces.

#### Surface Exterior Cavity Natural Ventilation from Buoyancy Mass Flow Rate [kg/s]

The part of mass flow rate of natural ventilation air exchange between the plenum and ambient when the collector is inactive due to bouyancy-driven forces.

## SurfaceProperty:SolarIncidentInside

This object can be used as an alternative to the standard (automatic) EnergyPlus calculation of the solar radiation incident on interior surfaces of the building. Using this method, the normal EnergyPlus calculation is replaced with a schedule of solar incidence values that are calculated outside the program.

### Inputs

#### Field: Name

The name of the [SurfaceProperty:SolarIncidentInside](#surfacepropertysolarincidentinside) object  Must be unique between all [SurfaceProperty:SolarIncidentInside](#surfacepropertysolarincidentinside) objects.

#### Field: Surface Name

The building surface associated with this object (ref [BuildingSurface:Detailed](#buildingsurfacedetailed)). Solar absorptance values in the schedule file will be applied to the inside of this surface.

#### Field: Construction Name

The building construction associated with this object (ref [Construction](#construction)).  It is possible that the Energy Management System will change the construction associated with the surface; if the construction is changed, a new set of data may need to be applied to the current surface.

#### Field: Inside Surface Incident Sun Solar Radiation Schedule Name

This field specifies the name of a schedule that contains the values for incident solar radiation.  Values from the schedule data will be used to replace the absorbed solar radiation that would normally be calculated by EnergyPlus.  Units in the external schedule file must be W/m^2^.

Example for [SurfaceProperty:SolarIncidentInside](#surfacepropertysolarincidentinside) using a compact schedule:

~~~~~~~~~~~~~~~~~~~~

      Schedule:Compact,
        North Wall SSG,          !- Name
        Positive Number,         !- Schedule Type Limits Name
        Through: 12/31,          !- Field 1
        For: AllDays,            !- Field 2
        Until: 07:00,10,         !- Field 3
        Until: 17:00,20,         !- Field 5
        Until: 24:00,15;         !- Field 7

      SurfaceProperty:SolarIncidentInside,
    North Wall Solar Incident,  !- Name
    Room102 North Wall,         !- Surface Name
    Room Wall - North,          !- Construction Name
    North Wall SSG;             !- Schedule Name
~~~~~~~~~~~~~~~~~~~~

## ComplexFenestrationProperty:SolarAbsorbedLayers

This object can be used as an alternative to the standard (automatic) EnergyPlus calculation of the solar radiation absorbed by fenestration systems in the building.  Using this method, the normal EnergyPlus calculation is replaced with a schedule of solar absorptance values that are calculated outside the program.

### Inputs

#### Field: Name

The name of the [ComplexFenestrationProperty:SolarAbsorbedLayers](#complexfenestrationpropertysolarabsorbedlayers) object  Must be unique between all [ComplexFenestrationProperty:SolarAbsorbedLayers](#complexfenestrationpropertysolarabsorbedlayers) objects.

#### Field: Fenestration Surface

The fenestration surface associated with this object (ref [FenestrationSurface:Detailed](#fenestrationsurfacedetailed)).  Values from the schedule data will be used to replace the absorbed solar radiation in the fenestration layers that would normally be calculated by EnergyPlus.  Value units in the schedule must be W/m^2^.

#### Field: Construction Name

The building construction associated with this object (ref [Construction](#construction)).  It is possible that the Energy Management System will change the construction associated with the surface; if the construction is changed, a new set of data may need to be applied to the current surface.

#### Field: Layer 1 Solar Radiation Absorbed Schedule Name

Specifies the name of a schedule that contains the absorbed solar radiation values in units of W/m^2^.  Values from the schedule are used for the absorbed solar radiation of the first (outside) layer .

#### Field: Layer 2 Solar Radiation Absorbed Schedule Name

Specifies the name of a schedule that contains absorbed solar radiation values in units of W/m^2^.  Values from the schedule are used for the absorbed solar radiation of the second layer .

#### Field: Layer 3 Solar Radiation Absorbed Schedule Name

Specifies the name of a schedule that contains absorbed solar radiation values in units of W/m^2^.  Values from the schedule are used for the absorbed solar radiation for the third layer .

#### Field: Layer 4 Solar Radiation Absorbed Schedule Name

Specifies the name of a schedule that contains absorbed solar radiation values in units of W/m^2^.  Values from the schedule are used for the absorbed solar radiation of the fourth layer.

#### Field: Layer 5 Solar Radiation Absorbed Schedule Name

Specifies the name of a schedule that contains absorbed solar radiation values in units of W/m^2^.  Values from the schedule are used for the absorbed solar radiation of the fifth layer .

Example for [ComplexFenestrationProperty:SolarAbsorbedLayers](#complexfenestrationpropertysolarabsorbedlayers) with compact schedules:

~~~~~~~~~~~~~~~~~~~~

      Schedule:Compact,
        Layer 1,                 !- Name
        Positive Number,         !- Schedule Type Limits Name
        Through: 12/31,          !- Field 1
        For: AllDays,            !- Field 2
        Until: 07:00,1,         !- Field 3
        Until: 17:00,2,         !- Field 5
        Until: 24:00,1.5;         !- Field 7

      Schedule:Compact,
        Layer 2,                 !- Name
        Positive Number,         !- Schedule Type Limits Name
        Through: 12/31,          !- Field 1
        For: AllDays,            !- Field 2
        Until: 07:00,0.8,        !- Field 3
        Until: 17:00,1.2,        !- Field 5
        Until: 24:00,1.0;        !- Field 7

      Schedule:Compact,
        Layer 3,                 !- Name
        Positive Number,         !- Schedule Type Limits Name
        Through: 12/31,          !- Field 1
        For: AllDays,            !- Field 2
        Until: 07:00,1,          !- Field 3
        Until: 17:00,2.1,        !- Field 5
        Until: 24:00,1.7;        !- Field 7

      ComplexFenestrationProperty:SolarAbsorbedLayers,
        South Window Solar Absorbed Layers, !- Name
    Room 102 South Window,              !- Fenestration surface name
    CFS_Glz_2,                          !- Construction Surface name
    Layer 1,                            !- Absorbed solar energy in layer 1
    Layer 2,                            !- Absorbed solar energy in layer 2
    Layer 3;                            !- Absorbed solar energy in layer 3
~~~~~~~~~~~~~~~~~~~~

## GeometryTransform

This object provides a simple method of altering the footprint geometry of a model. The intent is to provide a single parameter that can be used to reshape the building description contained in the rest of the input file. This object was implemented for use in parametric massing studies and with the optimization program GenOpt. Although building footprint is often constrained in practice, analysts may find this object useful for investigating how building form impacts daylighting and solar gains on the east and west facades with out having to change all of the surface geometry input.

Aspect Ratio is defined as the overall length in the East-West direction divided by the overall length in the North-South direction.

This object should be used with considerable care since it will completely alter the geometry modeled by EnergyPlus and may have unintended side effects. The surface areas of all horizontal surfaces may change radically with corresponding changes in [Zone](#zone) floor areas. The total floor area will not change but individual horizontal surfaces will gain and loose area. Vertical surfaces will have the same height but will gain and lose length. Lighting and electrical equipment design levels for individual zones will likely have a different energy per unit area in the transformed geometry.

The surface geometry must be set to **Relative**, see **GlobalGeometryRules**. Of course, the coordinates must be entered in relative coordinates as well.

Since windows in EnergyPlus need to be rectangular, it is possible to define a horizontal window (skylight) that once transformed is no longer rectangular and will cause EnergyPlus to halt. To avoid this problem, horizontal windows should be defined orthogonal to the Cardinal directions and building rotation (see **Building**) used to orient the final form with respect to North.

The object doesn't create any specific output, but the results of using it can be understood by viewing DXF output files. Figure 43 shows an example of a building that has been morphed using the Aspect Ratio Transform object. Using this object allowed the same geometry input to generate both of the models represented in by their DXF output files.

![Example of Geometry Transform -- Aspect Ratio](media/example-of-geometry-transform-aspect-ratio.jpeg)


### Inputs

#### Field: Plane of Transform

This field specifies the plane that the geometry transform should act on. It is currently restricted to altering the horizontal footprint of a building and must be set to "XY."

#### Field: Current  Aspect Ratio

This field specifies the aspect ratio of the building geometry described in the rest of the input file. It is used to scale the new aspect ratio. If this field is set to 1.0, then the altered building will not necessarily have the new aspect ratio defined in the next field.

#### Field: New Aspect Ratio

This field specifies the aspect ratio that the building described in the rest of the input file will be changed to.

## Zone Property View Factors

EnergyPlus has two options for specifying the thermal radiation exchange view factors between surfaces in a zone: the approximate option and the user input option.  Because the actual geometric arrangement within a zone is very complex, the approximate method of including thermal mass and other forced exchanges is more realistic than trying to come up with "exact" view factors. However, in some research situations it might be desirable to have control of the view factors used. For this reason, a user input mode has been included in EnergyPlus. The two modes are described in the next sections.

### Approximate Option

The first option produces approximate results and uses an area weighted scheme to calculate "view factors" between surfaces within a thermal zone. Each surface uses the total area that it can "see" among the other surfaces. The approximate view factor from this surface to each other surface is then the area of the receiving surface over the sum of areas that are visible to the sending surface.

In order to account in some limited way for the fact that certain surfaces will not see each other, several assumptions have been built into this view factor approximation. First, a surface cannot see itself. Second, surfaces with approximately the same azimuth (facing direction) and tilt ("same" being within a built in limit) will not see each other. This means that a window will not see the wall that it is placed on, for example. Third, floors cannot see each other. Fourth, if the surface is a floor, ceiling, roof, or internal mass, the rule for the same azimuth and tilt eliminating radiant exchange between surfaces is waived when the receiving surface is floor, roof, ceiling, or internal mass as long as both surfaces are not floors.

Note that this does not take into account that surfaces may be "around the corner" from each other and in reality not see each other at all. Rooms are assumed to be convex rather than concave in this method.

To summarize, using the Surface "Class", the approximate view factors have:

#. No surface sees itself.
#. No Floor sees another floor.
#. All other surface types see Internal Mass.
#. All other surface types see floors.
#. Floors always see ceilings.
#. Floors always see roofs.
#. All other surfaces whose tilt or facing angle differences are greater than 10 degrees see each other.

If geometry is correct, conditions 1,3, and 7 should take care of all surfaces, but the other conditions supply common sense when the geometry is incorrect. More information about the EnergyPlus view factor calculation is contained in the Engineering Reference document.

### User Input View Factors

The second option for specifying view factors requires user input values. These should be used with care in research or special situations. The object available for this is [ZoneProperty:UserViewFactors:bySurfaceName](#zonepropertyuserviewfactorsbysurfacename).

## ZoneProperty:UserViewFactors:bySurfaceName

The method of entering user view factors is to entere each surface name and its view factor value to other surfaces in the zone

### Inputs

#### Field: Zone Name

This field is the zone name for the view factors.

#### Field: From Surface 1

This field specifies the name of the "from surface".

#### Field: To Surface 1

This field specifies the name of the "to surface".

#### Field: Factor 1

This value is the view factor for "from Surface" to "to Surface".

An IDF Example:

~~~~~~~~~~~~~~~~~~~~

    ZoneProperty:UserViewFactors:bySurfaceName,Lshaped Zone,
      Lshaped Zone:South Wall,Lshaped Zone:South Wall,0.000000,
      Lshaped Zone:South Wall,Lshaped Zone:East Wall,0.101310,
    <snip>
~~~~~~~~~~~~~~~~~~~~