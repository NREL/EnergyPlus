# Group – Room Air Models

## Room Air Models

The group of input objects described in this section is used to account for non-uniform room air temperatures that may occur within the interior air volume of a zone. Room air modeling was added to EnergyPlus starting with [Version](#version) 1.2. Although there are many types of analyses (comfort, indoor air quality, etc) that might benefit from localized modeling of how room air varies across space, only the *temperature* distribution of room air within the zone is currently addressed in EnergyPlus. This allows surface heat transfer and air system heat balance calculations to be made taking into account natural thermal stratification of air and different types of intentional air distribution designs such as under-floor and side-wall displacement ventilation that purport to extract room air at higher-than-mean temperatures. Note that EnergyPlus does **not** have completely general methods of modeling room air that are applicable to every conceivable type of airflow that might occur in a zone. Such models (e.g. RANS-CFD) are too computationally expensive to use with EnergyPlus for the foreseeable future. The models that are available in EnergyPlus offer only limited modeling capabilities for select room airflow configurations. Also note that because the complete mixing model for room air has long been the standard in building energy simulation, there is not currently a consensus on how to best model non-uniform air temperatures in buildings. Therefore, it is up to the user to have a good understanding of when, where, and how to apply the room air models available in EnergyPlus. The rest of this section provides some guidance in the way of examples and further discussion of the models available in EnergyPlus.

EnergyPlus offers the different types of air models listed in the table below along with the input objects associated with the use of that model.

Table: Summary of Room Air Models

**Air Model Key**|**Air model Algorithm**|**Applicability**|**Input Objects Required**
------------------------------|------------------------------------|------------------------------|---------------------------------------
Mixing|Well-Mixed |All zones|None, default
UserDefined|User Defined|All zones|‘RoomAirModelType', ‘[RoomAir:TemperaturePattern:UserDefined](#roomairtemperaturepatternuserdefined)',' RoomAir:TemperaturePattern:\*\*\*'
OneNodeDisplacementVentilation|Mundt|displacement ventilation|‘RoomAirModelType', ‘[RoomAirSettings:OneNodeDisplacementVentilation](#roomairsettingsonenodedisplacementventilation)', ‘[RoomAir:Node](#roomairnode)''
ThreeNodeDisplacementVentilation|UCSD Displacement Ventilation |displacement ventilation |‘[RoomAirModelType](#roomairmodeltype)' , ‘[RoomAirSettings:ThreeNodeDisplacementVentilation](#roomairsettingsthreenodedisplacementventilation)'
UnderFloorAirDistributionInterior|UCSD UFAD Interior Model|Interior zones served by a UFAD system|‘RoomAirModelType', ‘[RoomAirSettings:UnderFloorAirDistributionInterior](#roomairsettingsunderfloorairdistributioninterior)'
UnderFloorAirDistributionExterior|UCSD UFAD Exterior Model|Exterior zones served by a UFAD system|‘RoomAirModelType',,' [RoomAirSettings:UnderFloorAirDistributionExterior](#roomairsettingsunderfloorairdistributionexterior)'
CrossVentilation|UCSD Cross Ventilation|cross ventilation|‘RoomAirModelType' , ‘[RoomAirSettings:CrossVentilation](#roomairsettingscrossventilation)'

## RoomAirModelType 

EnergyPlus uses the [RoomAirModelType](#roomairmodeltype)  object to determine which air model is available for use in a given zone during the simulation. If no [RoomAirModelType](#roomairmodeltype)  object is specified (for each zone or the whole building), then EnergyPlus will run with the conventional, completely mixing air model (for each zone or the whole building). Include a [RoomAirModelType](#roomairmodeltype)  for each zone that the user wants modeled using a more detailed method. Currently only a single [RoomAirModelType](#roomairmodeltype) object can be specified for each zone; you cannot switch between models during a simulation. However, the UCSD Displacement, Cross Ventilation and UFAD models switch from displacement to mixing ventilation when the operating conditions do not give rise to unmixed flow. The following parameters are fields required by the [RoomAirModelType](#roomairmodeltype)  object.

### Inputs

#### Field: Name

This alpha field is the air model name selected by user. It is used as an identifier

#### Field: Zone Name

This alpha field indicates the unique name of a [Zone](#zone) object defined elsewhere in the input file. The type of room air model selected will be applied to this zone.

#### Field: Room-Air Modeling Type

This alpha field indicates the room-air model used for the specified zone. Currently, there are three options for different air models. Entering the keyword ‘Mixing'  specifies the  conventional complete-mixing air model. Note that Mixing is the default and no [RoomAirModelType](#roomairmodeltype)  object would be needed to use the complete-mixing model. Entering the keyword ‘UserDefined ‘specifies the User Defined Room Air Temperature Patterns. Entering the keyword ‘OneNodeDisplacementVentilation ‘specifies the Mundt one node displacement ventilation air model for displacement ventilation. Entering the keyword ‘ThreeNodeDisplacementVentilation‘ specifies the three-node displacement ventilation model developed by the University of California, San Diego (UCSD DV). Entering the keyword ‘CrossVentilation' specifies the two-zone cross ventilation model developed by the University of California, San Diego (UCSD CV). Entering the keyword ‘UnderFloorAirDistributionInterior‘ specifies the two-node interior zone under floor air distribution model developed by the University of California, San Diego (UCSD UFI). Entering the keyword ‘UnderFloorAirDistributionExterior‘ specifies the two-node exterior zone under floor air distribution model developed by the University of California, San Diego (UCSD UFE).

#### Field: Air Temperature Coupling Strategy

This alpha field indicates how air temperatures predicted by the air models are used by the surface heat balance calculation. Two different coupling schemes are available: Direct (also known as *T-couple*) or Indirect (*DT-couple*). In general, indirect coupling is more suited to situations where room air is well controlled and/or the room air model is less robust. Direct coupling is more suited for floating zone air temperatures and/or accurate room air models.

The Mundt model can use either coupling scheme; the UCSD DV, UCSD CV, UCSD UFI, and UCSD UFE models ignore this field and use direct coupling.

Input either *Direct* or *Indirect*

An example idf entry follows

~~~~~~~~~~~~~~~~~~~~

    RoomAirModelType,
        MOD1,      !- Room-Air Model Name
        ZONE ONE,  !- Zone Name
        ThreeNodeDisplacementVentilation,  !- Room-Air Modeling Type
        Direct;    !- Air Temperature Coupling Strategy
~~~~~~~~~~~~~~~~~~~~

## RoomAir:TemperaturePattern:UserDefined

This object is used to explicitly define temperature patterns that are to be applied to the mean air temperature within a thermal zone. This Room Air modeling option is made available for a number of reasons. It allows modeling the consequences of air temperature variations during the design phase when little information is available about the specifics of the air distribution system or positioning of various loads. This option can be used to evaluate the energy implications of different design targets for the temperature patterns. It also provides a method of modeling the annual energy use implications for air temperature distributions determined using separate analyses or measurements. For example, this option could be used to understand the annual energy implications of an air distribution system that has been previously analyzed using Computational Fluid Dynamics.

This approach differs from the other Room Air modeling in that the static temperature pattern is not really *modeled* so that it will respond to conditions that develop during the simulation. More sophisticated dynamic Room Air models will adjust the temperature pattern based on various factors, such as air system flow rate, floor temperature, or rates of internal heat gains. The user-defined temperature distribution patterns are fixed at the beginning and EnergyPlus simply provides results that include the implications of those patterns. This user-defined distribution option may also be useful for checking dynamic Room Air models by using "bounding" analysis.

Note that using this object carries a certain degree of responsibility. It would be very easy to define a pattern that is non-physical and will lead to erroneous results. The user-defined temperature distributions should (probably) be balanced about the mean so that basic conservation of energy laws are not violated.

### Inputs

#### Field: Name 

This field provides a unique name for this object.

#### Field: Zone Name

This field provides the unique name of a zone described elsewhere in the file.

#### Field: Availability Schedule Name

This field provides the name of a schedule that will determine if this model is available. When not available the room air is modeled as completely mixed. When it is available, then a user-defined temperature distribution will be applied. This schedule should be set to "1.0" when model is available and "0.0" when the model is not to be used. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Pattern Control Schedule Name

This field provides the name of schedule that will be used to control which user-defined RoomAir temperature pattern will be applied at any given time. This schedule needs to have integer values that are closely coordinated with those defined as the second field in one of the RoomAir:TemperaturePattern:\* objects described below. These schedule values provide the link to the actual patterns to be used throughout the day. This allows controlling which user-defined pattern is used at different times during the simulation. For example, one could use one pattern when the air system is scheduled to be on, and a different pattern when the air system is schedule to be off. Or if the user has knowledge of how the air temperature pattern changes over the course of day in response to changing thermal loads, then this schedule can be used to control when individual patterns are used. For example, a control schedule could use a pattern designated as number 100 from 18:00 to 6:00, pattern number 200 from 6:00 to 12:00, and pattern number 300 from 12:00 to 18:00.

An example of this object is

~~~~~~~~~~~~~~~~~~~~

    RoomAir:TemperaturePattern:UserDefined,
      Ground Floor South Air Temp Contrls , ! Name
      ZN2_S_1 ,          ! Zone Name
      Always_on ,        ! Availability Schedule Name
      Roomair Pattern 1; ! Pattern Control Schedule Name
~~~~~~~~~~~~~~~~~~~~

## RoomAir:TemperaturePattern:ConstantGradient

This object is used to model room air with a fixed temperature gradient in the vertical direction. This fixed-slope method is about the simplest distribution pattern.

In addition to the vertical temperature gradient, there are three other parameters included in the pattern that are important. The first two might affect how the air system conditioning the room is operated. The first describes the temperature difference between the mean air temperature and the point where the sensor of a drybulb thermostat is situated. The second describes the temperature difference between the mean and the point where system air is being extracted from the zone. This is considered important because the changes in temperature difference between supply and return can affect how an air system is controlled to meet the loads. The third parameter can affect the zone air heat balance by altering the temperature of the air leaving the zone through exhaust fans.

One example of a source of input data for the vertical temperature gradient is ANSI/ASHRAE Standard 55-2004 Thermal Environmental Conditions for Human Occupancy. Table 5.2.4.3 in this Standard specifies an allowable vertical temperature difference between head level and ankle level of 3ºC (5ºF). If we assume a head to ankle length scale of 1.5 m (5 ft), this leads to a temperature gradient of 3ºC/1.5m, or 2.0 ºC/m.

### Inputs

#### Field: Name

This field provides a unique name for this object.

#### Field: Control Integer for Pattern Control Schedule Name

This field should contain an integer value that is unique among all other RoomAir:TemperaturePattern:\* objects. The value used here needs to be in the Pattern Control Schedule for those times when this pattern is to be used for the Room Air Temperature Distribution.

#### Field: Thermostat Offset

This field specifies the temperature difference between where the thermostat is situated and the mean air temperature.

#### Field: Return Air Offset 

This field specifies the temperature difference between the air leaving the zone and returning to the air system and the mean air temperature.

#### Field: Exhaust Air Offset

This field specifies the temperature difference between the air leaving the zone and being exhausted out of the building and the mean air temperature.

#### Field: Temperature Gradient 

This field specifies the gradient, or slope, of temperature changes in the vertical direction in ºK/m.

An example of this object is:

~~~~~~~~~~~~~~~~~~~~

    RoomAir:TemperaturePattern:ConstantGradient,
      half C per Meter,       ! Name
      10005,   ! Control Integer for Pattern Control Schedule Name
      0.0,     ! Thermostat Offset (Temp at thermostat- Mean Air Temp) [C]
      1.0,     ! Return Air Offset (Tleaving - Mean Air Temp )  [C]
      1.0,     ! Exhaust Air Offset (Texhaust - Mean Air Temp)  [C]
      0.5;     ! Temperature Gradient [K/m]
~~~~~~~~~~~~~~~~~~~~

## RoomAir:TemperaturePattern:TwoGradient

This object provides various controls over the value of the gradient used for determining the pattern of room air temperatures. It is similar to previous object [RoomAir:TemperaturePattern:ConstantGradient](#roomairtemperaturepatternconstantgradient) object but simplifies the potentially arduous task of preparing and scheduling a large number of those objects. With this object, two different gradients are entered and user is given several options for controlling how the program will interpolate between the two bounds. The user inputs the height of the location of thermostat, return air, and exhaust air in meters rather than the temperature offset.

### Inputs

#### Field: Name

This field provides a unique name for this object.

#### Field: Control Integer for Pattern Control Schedule Name

This field should contain an integer value that is unique among all other RoomAir:TemperaturePattern:\* objects. The value used here needs to be in the Pattern Control Schedule for those times when this pattern is to be used for the Room Air Temperature Distribution.

#### Field: Thermostat Height

This field specifies the distance above the floor where the thermostat is situated. This height is used by the model to determine the thermostat temperature relative to the mean air temperature by applying the gradient.

#### Field: Return Air Height 

This field specifies the distance above the floor where the air leaves the zone and returns to the air system. and the mean air temperature. This height is used by the model to determine the return air temperature relative to the mean air temperature by applying the gradient.

#### Field: Exhaust Air Height

This field specifies the distance above the floor where the air leaves the zone and enters and exhaust device such as an exhaust fan. This height is used by the model to determine the exhaust air temperature relative to the mean air temperature by applying the gradient.

#### Field: Temperature Gradient Lower Bound

This field specifies the gradient, or slope, of temperature changes in the vertical direction in ºC/m.

#### Field: Temperature Gradient Upper  Bound

This field specifies the gradient, or slope, of temperature changes in the vertical direction in ºC/m.

#### Field: Gradient Interpolation Mode 

This field specifics how the program will vary between the two gradients. Select one of the following keywords to choose the simulation data used to scale:  ‘Outdoor Environment Drybulb Temperature', ‘[Zone](#zone) Drybulb Temperature', ‘Delta Outdoor and [Zone](#zone) Temperature', ‘Sensible Cooling Load', and ‘Sensible Heating Load'. These are explained in detail below. All of these options have several things in common. They are essentially hard-coded. There is no support for a general method. The interpolation scheme is based on some variable that might reasonably be expected to correlate with gradient changes. This variable's current value is used to continually adjust the value of the vertical gradient for room air temperature.

**OutdoorDryBulbTemperature:** This key directs the program to interpolate between upper and lower values of the vertical gradient based on the outdoor air temperature. If the outdoor temperature exceeds the upper limit set in the next field, then the gradient entered in the ‘Temperature Gradient Upper Bound' field is used. Similarly if the outdoor air temperature is below the value set in the ‘Lower Temperature' field, then the gradient entered in the ‘Temperature Gradient Lower Bound' is used. For outdoor temperatures that lie between the upper and lower bounds, the gradient is determined by linear interpolation between the two.

**ZoneDryBulbTemperature**: This key directs the program to interpolate between upper and lower values of the vertical gradient based on the mean zone air temperature. If the mean zone air temperature exceeds the upper limit set in the next field, then the gradient entered in the ‘Temperature Gradient Upper Bound' field is used. Similarly if the mean zone air temperature is below the value set in the ‘Lower Temperature' field, then the gradient entered in the ‘Temperature Gradient Lower Bound' is used. For mean zone air temperatures that lie between the upper and lower bounds, the gradient is determined by linear interpolation between the two.

**ZoneAndOutdoorTemperatureDifference**:  This key directs the program to interpolate between upper and lower values of the vertical gradient based on the difference between the outdoor environment and the mean zone air temperature. If the temperature difference exceeds the upper limit set in the next field, then the gradient entered in the ‘Temperature Gradient Upper Bound' field is used. Similarly if the temperature difference is below the value set in the ‘Lower Temperature' field, then the gradient entered in the ‘Temperature Gradient Lower Bound' is used. For temperature differences that lie between the upper and lower bounds, the gradient is determined by linear interpolation between the two.

**SensibleCoolingLoad**:  This key directs the program to interpolate between upper and lower values of the vertical gradient based on the sensible cooling load. If the cooling load exceeds the upper limit set in the next field, then the gradient entered in the ‘Temperature Gradient Upper Bound' field is used. Similarly if the cooling load is below the value set in the ‘Lower Temperature' field, then the gradient entered in the ‘Temperature Gradient Lower Bound' is used. For cooling loads that lie between the upper and lower bounds, the gradient is determined by linear interpolation between the two.

**SensibleHeatingLoad**:  This key directs the program to interpolate between upper and lower values of the vertical gradient based on the sensible heating load. If the heating load exceeds the upper limit set in the next field, then the gradient entered in the ‘Temperature Gradient Upper Bound' field is used. Similarly if the heating load is below the value set in the ‘Lower Temperature' field, then the gradient entered in the ‘Temperature Gradient Lower Bound' is used. For heating loads that lie between the upper and lower bounds, the gradient is determined by linear interpolation between the two.

#### Field: Upper Temperature Bound

This field is used to enter the upper bound on temperature values in Celsius. It is required for the interpolation modes based on temperature.

#### Field: Lower Temperature Bound

This field is used to enter the lower bound on temperature values in Celsius. It is required for the interpolation modes based on temperature.

#### Field: Upper Heat Rate Bound

This field is used to enter the upper bound on heat rate values. It is required for the interpolation modes based on load. Both heating and cooling loads are entered as positive numbers (in watts).

#### Field: Lower Heat Rate Bound

This field is used to enter the lower bound on heat rate values. It is required for the interpolation modes based on load. Both heating and cooling loads are entered as positive numbers (in watts).

An example of this object follows. This pattern will apply no gradient (effectively the mixing model) if zone air temperatures are 22.5 C or lower. It will apply a gradient of 1 K/m if zone temperatures are 26.0 C or higher. For zone air temperatures between 22.5 and 26.0 it will determine the gradient by linear interpolation and use a gradient between 0.0 and 1.0 K/m depending on where the zone air temperature is in the range.

~~~~~~~~~~~~~~~~~~~~

    RoomAir:TemperaturePattern:TwoGradient,
      Mixed to one C per M by Zone DB, ! Name
      2002,                 ! Control Integer for Pattern Control Schedule
      1.1,                  ! Thermostat Height meters
      4.5,                  ! Return Air Height
      3.5,                  ! Exhaust Air Height
      0.0, ! Temperature Gradient Lower Bound K/m
      1.0, ! Temperature Gradient Upper  Bound K/m
      ZoneDrybulbTemperature, ! Gradient Interpolation Mode
      26.0,               ! Upper Temperature [C]
      22.5,               ! Lower Temperature [C]
      , ! Upper Heat Rate [W]
      ; ! Lower Heat Rate [W]
~~~~~~~~~~~~~~~~~~~~

## RoomAir:TemperaturePattern:NondimensionalHeight

This object defines a distribution pattern for air temperatures relative to the current mean air temperature as a function of height. The height, referred to as Zeta, is non-dimensional by normalizing with the zone ceiling height. (The actual zone ceiling height can be explicitly entered in the ‘[Zone](#zone)' object but if not it is calculated by EnergyPlus from the surfaces attached to the zone.)  The temperature differences are not non-dimensional and remain in units of degrees Celsius.

An example of a vertical temperature pattern is shown in the figure below. The pattern itself is treated as a piecewise, linear model of air temperature as a function of height. This Zeta-DeltaTai curve, or lookup table, is then mapped to surfaces defined elsewhere in the file. The centroid of each surface and zone ceiling height are used to automatically assign Zeta values within the program. The zone named in the referencing [RoomAir:TemperaturePattern:UserDefined](#roomairtemperaturepatternuserdefined) object is used to determine which surfaces will be associated with the pattern when it is applied. A single pattern object can be reused for multiple zones and times.

In addition to the vertical temperature pattern there are three other parameters included in the pattern that are important. The first two might affect how the air system conditioning the room is operated. The first describes the temperature difference between the mean air temperature and the point where the sensor of a drybulb thermostat is situated. The second describes the temperature difference between the mean and the point where system air is being extracted from the zone. This is considered important because the changes in temperature difference between supply and return can affect how an air system is controlled to meet the loads. The third parameter can affect the zone air heat balance by altering the temperature of the air leaving the zone through exhaust fans.

![Example of a Vertical Air Temperature Pattern](media/example-of-a-vertical-air-temperature-pattern.png)


### Inputs

#### Field: Name

This field provides a unique name for this object.

#### Field: Control Integer for Pattern Control Schedule Name

This field should contain an integer value that is unique among all other RoomAir Temperature Pattern objects. The value used here needs to be in the Pattern Control Schedule for those times when this pattern is to be used for the Room Air Temperature Distribution.

#### Field: Thermostat Offset

This field specifies the temperature difference between where the thermostat is situated and the mean air temperature.

#### Field: Return Air Offset 

This field specifies the temperature difference between the air leaving the zone and returning to the air system and the mean air temperature.

#### Field: Exhaust Air Offset

This field specifies the temperature difference between the air leaving the zone and being exhausted out of the building and the mean air temperature.

#### Field Set Zeta and Temperature Difference 

The remaining fields contain pairs of values that define a lookup table for the temperature pattern in the vertical direction. The first value is Zeta and the second value is DeltaTai.   This object is extensible, by duplicating the last two fields and revising the IDD – note that you will have to replace "inner" semi-colons with commas.

#### Field: Pair <#> Zeta non-dimensional Height

Zeta is the normalized height and should be a fractional value from 0.0 to 1.0. A value of 0.0 corresponds to the floor and a value of 1.0 corresponds to the ceiling. The Zeta values need to be in increasing value.

#### Field: Pair <#> Delta Adjacent Air Temperature

DeltaT~ai~ is the temperature difference between the air temperature at the associated Zeta (T~ai~) and the mean air temperature and is given in degrees Celsius.

An example of this object corresponding to the figure above is:

~~~~~~~~~~~~~~~~~~~~

    RoomAir:TemperaturePattern:NondimensionalHeight,
      Rough UFAD Approx  , ! Name
      3001 , ! Control Integer for Pattern Control Schedule
            ! note reference this entry in Schedule
      -1.0 ,   ! Thermostat Offset
      1.5 , ! Return Air Offset (Tleaving - Mean Air Temp ) deg C
      1.75,   ! Exhaust Air Offset (Texhaust - Mean Air Temp) deg C
      0.05,  -1.8,   ! pair 1 (Zeta , DeltaTai)
      0.1,   -1.7 ,  ! pair 2 (Zeta , DeltaTai)
      0.2,   -0.8 ,  ! pair 3 (Zeta , DeltaTai)
      0.3,   -0.2,   ! pair 4 (Zeta , DeltaTai)
      0.4,    0.5,   ! pair 5 (Zeta , DeltaTai)
      0.5,    0.8,   ! pair 6 (Zeta , DeltaTai)
      0.6,    1.2,   ! pair 7 (Zeta , DeltaTai)
      0.7,    1.4,   ! pair 8 (Zeta , DeltaTai)
      0.8,    1.4,   ! pair 9 (Zeta , DeltaTai)
      0.9,   1.42,   ! pair 10 (Zeta , DeltaTai)
      0.95,  1.44;   ! pair 11 (Zeta , DeltaTai)
~~~~~~~~~~~~~~~~~~~~

## RoomAir:TemperaturePattern:SurfaceMapping

This object defines a distribution pattern for the air temperatures adjacent to individual surfaces. This object uses the specific names of individual surfaces defined elsewhere in the model. This pattern allows controlling the adjacent air temperature on a surface-by-surface basis rather than by height. This allows modeling different adjacent air temperatures on the opposite sides of the zone.

In addition to the surface mappings there are three other parameters included in the pattern that are important. The first two might affect how the air system conditioning the room is operated. The first describes the temperature difference between the mean air temperature and the point where the sensor of a drybulb thermostat is situated. The second describes the temperature difference between the mean and the point where system air is being extracted from the zone. This is considered important because the changes in temperature difference between supply and return can affect how an air system is controlled to meet the loads. The third parameter can affect the zone air heat balance by altering the temperature of the air leaving the zone through exhaust fans.

### Inputs

#### Field: Name

This field provides a unique name for this object.

#### Field: Control Integer for Pattern Control Schedule Name

This field should contain an integer value that is unique among all other RoomAir Temperature Pattern objects. The value used here needs to be in the Pattern Control Schedule for those times when this pattern is to be used for the RoomAir Temperature Distribution.

#### Field: Thermostat Offset

This field specifies the temperature difference between where the thermostat is situated and the mean air temperature.

#### Field: Return Air Offset 

This field specifies the temperature difference between the air leaving the zone and returning to the air system and the mean air temperature.

#### Field: Exhaust Air Offset

This field specifies the temperature difference between the air leaving the zone and being exhausted out of the building and the mean air temperature.

#### Field Set: Surface Name, Temperature Difference

#### Fields (6 and on): Pairs of Surface Names and Temperature Differences

The remaining fields contain pairs that define a lookup table for the temperature pattern on a surface-by-surface basis. This object is extensible, by duplicating the last two fields and revising the IDD – note that you will have to replace "inner" semi-colons with commas.

#### Field: Surface Name Pair <#>

The name of a surface defined elsewhere in the input file.

#### Field: Delta Adjacent Air Temperature Pair <#>

DeltaT~ai~ is the temperature difference between the air temperature adjacent to the associated surface (T~ai~) and the mean air temperature and is given in degrees Celsius.

An example of this object, which might be used to elevate temperatures near west windows in the afternoon, is:

~~~~~~~~~~~~~~~~~~~~

    RoomAir:TemperaturePattern:SurfaceMapping,
      GroundFloor SW Corner Hot Near West Wall, ! Name
      4001, ! Control Integer for Pattern Control Schedule Name
      0.0, ! Thermostat Offset(Temp at thermostat- Mean Air Temp)
      0.0, ! Return Air Offset  deg C
      0.0, ! Exhaust Air Offset
      ZN1_SW_1:W_ExtWall:1 ,  0.8,  ! pair 1 (Surface Name , DeltaTai)
      ZN1_SW_1:W_ExtWall:2 ,  0.9,  ! pair 2 (Surface Name , DeltaTai)
      ZN1_SW_1:W_ExtWall:3 ,  1.0,  ! pair 3 (Surface Name , DeltaTai)
      ZN1_SW_1:W_ExtWall:4,   1.1,  ! pair 4 (Surface Name , DeltaTai)
      ZN1_SW_1:W_ExtWall:5,   1.3,  ! pair 5 (Surface Name , DeltaTai)
      ZN1_SW_1:W_ExtWall:6,   1.5,  ! pair 6 (Surface Name , DeltaTai)
      ZN1_SW_1:W_ExtWall:7,   1.7,  ! pair 7 (Surface Name , DeltaTai)
      ZN1_SW_1:W_ExtWall:8,   2.1,  ! pair 8 (Surface Name , DeltaTai)
      ZN1_SW_1:W_ExtWall:9,   2.4 ; ! pair 8 (Surface Name , DeltaTai)
~~~~~~~~~~~~~~~~~~~~

## RoomAir:Node

The [RoomAir:Node](#roomairnode) object is used to define air nodes for a nodal air model. The number of air node objects that need to be specified depends on the nodal air model selected. (However, currently only the Mundt model uses this object). In order to use the Mundt model, the user must specify six or more [RoomAir:Node](#roomairnode) objects of different types for each zone. The exact number of [RoomAir:Node](#roomairnode) in the model will vary based on the resolution of walls. If walls (heat transfer surfaces) are split into separate segments in the vertical direction, then more air nodes of type ‘MundtRoom' will be useful. At a minimum, for the Mundt model RoomAir Nodes of the following type are required: ‘Inlet, ‘Floor, ‘Control, ‘Ceiling, ‘MundtRoom, and ‘Return.'

### Inputs

#### Field: Name

This alpha field is a name for the air node. It should be unique and is used as an identifier

#### Field: Node Type

This alpha field indicates the type of this air node. The following explains options available for use with the Mundt nodal air model.

**Inlet** is specified for the air node located where the system air enters the zone.

**Floor** is specified for the air node located near the floor.

**Control** is specified for the air node located near the thermostat.

**Ceiling** is specified for the air node located near the ceiling.

**MundtRoom** is specified for the air node located adjacent to the wall(s).

**Return** is specified for the air node located where the system air leaves the zone.

#### Field: Zone Name

This field indicates the name of the zone (Ref: [Zone](#zone)) that this air node belongs to. This should be the unique name of a zone object defined elsewhere in the input file.

#### Field: Height of Nodal Control Volume Center

This numeric field indicates the height of the air node center. Air models such as the Mundt model compute the air temperature as a function of height and the value entered here will be used to determine a result for this node. The value should be specified in meters and relative to the floor of the zone.

#### Field: Surface <#> Name

These remaining alpha fields indicate the names of the surfaces (Ref: Surface) that are interacting convectively with this air node. This field is optional and related to the previous field. Currently, at most 20 surfaces are allowed to interact with a single air node. Only those nodes that interact with the inside faces of surfaces need to specify surface names. Each surface should couple to no more than 1 node.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    RoomAir:Node,
        WESTZN:FLOORAIR,    !- Node Name
        Floor,      !- Node Type
        WEST ZONE,  !- Name of Zone to Which the Air Node Belongs
        0.1,        !- Height of Nodal Control Volume Center {m}
        WESTZN:FLOOR:LEFF,  !- surface name
        WESTZN:FLOOR:RIGHT; !- surface name
~~~~~~~~~~~~~~~~~~~~

## RoomAirSettings:OneNodeDisplacementVentilation

The [RoomAirSettings:OneNodeDisplacementVentilation](#roomairsettingsonenodedisplacementventilation) object is used to specify additional input parameters required by the Mundt model that are not available in other input objects in EnergyPlus. A single object will be used for the zone.

### Inputs

#### Field: Zone Name

This alpha field indicates the name of the zone (Ref: [Zone](#zone)) for the required input parameters as specified in the following fields.

#### Field: Fraction of Convective Internal Loads Added to Floor Air

This numeric field indicates what fraction of the convective part of the internal gains is added to the air near the floor in the zone specified.

#### Field: Fraction of Infiltration Internal Loads Added to Floor Air

This numeric field indicates what fraction of the infiltration air enters near the floor in the zone specified.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    RoomAirSettings:OneNodeDisplacementVentilation,
        WEST ZONE,  !- Zone Name
        0.1,  !- Fraction of internal loads from the convective Floor Air
        0.1;  !- Fraction of internal loads from the Infiltration Air
~~~~~~~~~~~~~~~~~~~~

## RoomAirSettings:ThreeNodeDisplacementVentilation

This model is applicable to spaces that are served by a low velocity floor-level displacement ventilation air distribution system. Furthermore, the dominant sources of heat gain should be from people and other localized sources located in the occupied part of the room. The model should be used with caution in zones which have large heat gains or losses through exterior walls or windows or which have considerable direct solar gain. The model predicts three temperatures in the room:

A foot level temperature (T~FLOOR~). The floor region is 0.2 meters deep and T~FLOOR~ represents the temperature at the mid-point of the region.

An occupied subzone temperature (T~OC~), representing the temperature in the region between the floor layer and the upper, mixed layer.

An upper node representing the mixed-layer/outflow temperature (T~MX~) essential for overall energy budget calculations and for modeling comfort effects of the upper layer temperature.

![Schematic representation of the three temperature points and temperature gradients](media/schematic-representation-of-the-three.png)


The following fields are used to define an instance of the ‘UCSD Displacement Ventilation Model Controls' object.

### Inputs

#### Field: Zone Name

This field provides the unique name of a zone described elsewhere in the file. A single instance of the ‘UCSD Displacement Ventilation Model Controls' object is needed for each zone that is to be modeled using this method.

####  Field: Gain Distribution Schedule Name

This field specifies the unique name of schedule defined elsewhere in the input file. The schedule values are the fractions of the convective portion of the internal gains in the occupied subzone that remain in the occupied subzone. The remainder of the convective portion of the internal gains in the occupied subzone enters the plumes and is carried to the upper subzone. The types of internal gains that are assumed to be located in the occupied subzone are:

- people
- task lights
- electric equipment
- gas equipment
- hot water equipment
- steam equipment
- other equipment
- baseboard heat

Types of internal gains that are assumed to be in the upper subzone are:

- general lights
- tubular daylighting devices
- high temperature radiant heaters

The schedule values should be between 0 and 1. A value of 1 means that all the convection gains from equipment, task lights and people are dispersed in the lower occupied subzone. Conversely a value of 0 puts all the lower subzone convective gains into the plumes rising into the upper well-mixed subzone.

#### Field: Number of Plumes per Occupant

This field specifies number of plumes per occupant. Plumes are associated with localized sources of convective heat gain from people and equipment. For example, a value of 2.0 would be used if each occupant has a computer that generates a separate plume that does not merge with the plume from the occupant in the lower, occupied, subzone.

#### Field: Thermostat Height

This field is the height in meters of the thermostat/temperature control sensor above the floor.

#### Field: Comfort Height

The height in meters above the floor at which air temperature is calculated for comfort purposes. The air temperature at this height is used in calculating the available measures of comfort: Fanger, Pierce or KSU. The default is 1.1 meters.

#### Field: Temperature Difference Threshold for Reporting

This field specifies a minimum temperature difference between the upper mixed subzone and the occupied subzone that will be used to trigger whether or not the displacement ventilation auxiliary outputs will be calculated. These outputs are *Room Air [Zone](#zone) Transition Height*, *Room Air [Zone](#zone) Recommended Minimum Flow Fraction*, *Room Air [Zone](#zone) Average Temperature Gradient* and *Room Air [Zone](#zone) Maximum Temperature Gradient*. They are set to negative values when the temperature difference is less than the threshold and the output *Room Air [Zone](#zone) Is Mixed Status* is set to 1.

The value should be greater than or equal to zero and is in units of degrees Centigrade. The default value is 0.4 degrees C.

An example input:

~~~~~~~~~~~~~~~~~~~~

    RoomAirSettings:ThreeNodeDisplacementVentilation,
        ZONE ONE,  !- Zone Name
        Constant - .2,  !- Gain Distribution Schedule Name
         1,  !- Number of Plumes per Occupant
          ,  !- Thermostat Height
          ,  !- Comfort Height
        .3;  !- Temp. Difference Threshold for Displacement Ventilation
~~~~~~~~~~~~~~~~~~~~

## RoomAirSettings:CrossVentilation

The UCSD Cross Ventilation Room Air Model provides a simple model for heat transfer and temperature prediction in cross ventilated rooms. Cross Ventilation (CV) is common in many naturally ventilated buildings, with air flowing through windows, open doorways and large internal apertures across rooms and corridors in the building.

The CV model is used in EnergyPlus in the context of natural ventilation simulations using the AirflowNetwork airflow prediction model. Typical CV room flows are characterized by two clearly distinguishable flow regions that have different airflow velocities and temperature:

- Jet regions in front of the inflow windows
- Recirculation regions in the portions of the room that are not directly in front of the windows.

Each inflow aperture has one jet region while the recirculation regions are treated as a whole, with a single temperature and characteristic velocity. The default EnergyPlus perfectly mixed single temperature node room air approach is not suitable for these partially mixed flows. The proposed CV model uses multiple nodes with distinct air temperature and airflow velocity (one node for the recirculations plus one additional node for each inflow aperture).

![Schematic representation of room air geometry a) schematic representation of a room geometry that generates cross ventilation airflow. b) the proposed model distinguishes two regions in the flow: jet and recirculation (shown here in a CFD simulation of one half of a symmetrical room).](media/schematic-representation-of-room-air-geometry.png)


![Schematic top view –possible  airflow patterns in cross-ventilation.](media/schematic-top-view-possible-airflow-patterns.png)


The following fields are used to define an instance of the ‘UCSD Cross Ventilation Model Controls' object.

### Inputs

#### Field: Zone Name

This field provides the name of the zone to which this object applies. A single instance of the ‘UCSD Cross Ventilation Model Controls' object is needed for each zone modeled using this method.

#### Field: Gain Distribution Schedule Name

This field specifies the unique name of schedule defined elsewhere in the input file. The schedule values define the fractions of the convective portion of the internal gains in the jet and recirculation regions.

The schedule values should be between 0 and 1. A value of 1 means that all the convective gains are dispersed in the jet region. Conversely a value of 0 puts all the convective gains into the recirculation region.

#### Field: Airflow Region Used for Thermal Comfort Evaluation

Required field whenever thermal comfort is predicted. Defines air temperature and mean airflow velocity that will be used in the Fanger model. Conditions must refer to one of the two regions defined in the model: jet or recirculation.

Possible choices: Jet or Recirculation.

### Outputs

[Zone](#zone),Average,Room Air [Zone](#zone) Jet Region Temperature [C]

[Zone](#zone),Average,Room Air [Zone](#zone) Recirculation Region Temperature [C]

[Zone](#zone),Average,Room Air [Zone](#zone) Jet Region Average Air Velocity [m/s]

[Zone](#zone),Average,Room Air [Zone](#zone) Recirculation Region Average Air Velocity [m/s]

[Zone](#zone),Average,Room Air [Window](#window) Jet Region Average Air Velocity [m/s] [Zone](#zone),Average,Room Air [Zone](#zone) Recirculation and Inflow Rate Ratio []

[Zone](#zone),Average,Room Air [Zone](#zone) Inflow Opening Area [m2]

[Zone](#zone),Average,Room Air [Zone](#zone) Room Length [m]

[Zone](#zone),Average,Room Air [Zone](#zone) Is Mixing Status []

[Zone](#zone),Average,Room Air [Zone](#zone) Is Recirculating Status []

#### Room Air Zone Jet Region Temperature [C]

Average air temperature in the jet region of the flow in degrees C. If there is more than one inflow window this output will be the inflow-area-weighted average of the jet region temperature.

#### Room Air Zone Recirculation Region Temperature [C]

Average air temperature in the recirculation region of the flow in degrees C.

#### Room Air Zone Jet Region Average Air Velocity [m/s]

Average airflow velocity in the jet region of the flow in meters per second. If there is more than one inflow window this output will be the inflow area weighted area of the jet inflow velocities.

#### Room Air Window Jet Region Average Air Velocity [m/s]

Average airflow velocity in the jet region in front of the window, in meters per second.

#### Room Air Zone Recirculation Region Average Air Velocity [m/s]

Average airflow velocity in the recirculation region of the flow in meters per second.

#### Room Air Zone Recirculation and Inflow Rate Ratio []

Ratio between airflow rate in the recirculation regions and the total inflow rate, non-dimensional.

#### Room Air Zone Inflow Opening Area [m2]

Area of the inflow aperture in square meters. This area can change due to variation in wind direction (as inflow aperture changes) or variations in the opening schedule.

#### Room Air Zone Room Length [m]

Length of the room along the cross ventilation direction, in meters.

#### Room Air Zone Is Mixing Status []

An integer flag that indicates whether the zone is mixed (single node well-mixed zone model used) or significant momentum conservation is occurring (UCSD CV model used). The value 1 means well-mixed; 0 means cross ventilation is present. Transition to mixed flow can occur due to three mechanism: reduction in inflow velocity, reduction in inflow aperture area and dominance of buoyancy effects. A value of 1 is yes, a value of 0 is no.

#### Room Air Zone Is Recirculating Status []

An integer flag that indicates whether recirculations are present in the flow. A cross ventilation flow does not have recirculations whenever the inflow area is similar to the room cross section area (such as airflow in a typical corridor). A value of 1 is yes, a value of 0 is no.

## RoomAirSettings:UnderFloorAirDistributionInterior

This model is applicable to interior spaces that are served by an underfloor air distribution system. The dominant sources of heat gain should be from people, equipment, and other localized sources located in the occupied part of the room. The model should be used with caution in zones which have large heat gains or losses through exterior walls or windows or which have considerable direct solar gain. The model predicts two temperatures in the room:

- An occupied subzone temperature (T~OC~), representing the temperature in the region between the floor and the boundary of the upper subzone..
- An upper subzone temperature (T~MX~) essential for overall energy budget calculations and for modeling comfort effects of the upper layer temperature.

The following fields are used to define an instance of the ‘UCSD UFAD Interior Model Controls' object.

### Inputs

#### Field: Zone Name

This field provides the unique name of a zone described elsewhere in the file. A single instance of the ‘[RoomAirSettings:UnderFloorAirDistributionInterior](#roomairsettingsunderfloorairdistributioninterior)' object is needed for each zone that is to be modeled using this method.

#### Field: Number of Diffusers 

The total number of diffusers in this zone. This field can allowed to Autocalculate (in which case it is set to the design occupancy level; i.e., number of people). If the design occupancy is low or zero but there are still heat sources that could generate buoyancy driven plumes, the user should input a value based on the design supply air flow rate of the zone and the design flow rate of an individual diffuser. In the absence of any other information, divide the zone area by 100 ft^2^. The default for this field is Autocalculate.

#### Field: Power per Plume

The power in watts incorporated in a buoyancy driven thermal plume. Normally we assume all the loads of a workstation create a single plume so that this represents the convective heat gain from a workstation – 1 person, 1 computer terminal, plus any task lighting. A typical value would be 220 watts. However, the model assumes an "equivalent" plume derived from the zone extraction rate normalized to the number of workstations/occupants. This field should be allowed to default – the program will calculate a value based upon the occupancy and the extraction rate.  The default is Autocalculate.

#### Field: Design Effective Area of Diffuser

This is the design air flow opening area in square meters of a single diffuser. The default value depends on the diffuser type. For swirl diffusers it is 0075 m^2^, for variable area diffusers 0.035 m^2^, for horizontal swirl diffusers 0.006 m^2^, and for linear bar grilles 0.03 m^2^. The default is *Autocalculate*..

#### Field: Diffuser Slot Angle from Vertical

This input describes the angle at which air emerges from the diffusers. It should be the angle  in degrees between the vertical and the diffuser slots. The defaults depend on the diffuser type: for swirl diffusers it is 28 degrees, for variable area diffusers 45 degrees, for DV diffusers 73 degrees, and for linear bar grilles 15 degrees.

#### Field: Thermostat Height

This field is the height in meters of the thermostat/temperature control sensor above the floor. The default is 1.2 meters.

#### Field: Comfort Height

The height in meters above the floor at which air temperature is calculated for comfort purposes. The air temperature at this height is used in calculating the available measures of comfort: Fanger, Pierce or KSU. The default is 1.1 meters.

#### Field: Temperature Difference Threshold for Reporting

This field specifies a minimum temperature difference between the upper subzone and the occupied subzone that will be used to trigger whether or not the UFAD auxiliary outputs will be calculated. These outputs are *Room Air [Zone](#zone) Transition Height* and *Room Air [Zone](#zone) Average Temperature Gradient*. They are set to zero when the temperature difference is less than the threshold and the output *Room Air [Zone](#zone) Is Mixed Status* is set to 1.

The value should be greater than or equal to zero and is in units of degrees Centigrade. The default value is 0.4 degrees C.

#### Field: Diffuser Type

The choices for this alpha field are **Swirl** **| **VariableArea** **| **HorizontalSwirl | LinearBarGrille | Custom.** The swirl and displacement diffusers are fixed area diffusers. The variable area diffusers maintain an approximately constant exit velocity. Linear bar grilles are normally used in exterior zonesand are fixed area diffusers. Custom is used to signify that the user intends to input the coefficients A – E (see below) rather than let the program set the coefficients based on diffuser type. The default is *Swirl*.

#### Field: Transition Height

An optional field to allow the user to specify the transition height (meters above floor) rather than have the program calculate it. The default is 1.7 meters.

#### Field: Coefficient A

The coefficient A in the formula: Kc = A\*Gamma\*\*B + C + D\*Gamma + E\*Gamma\*\*2. Gamma is a variable that characterizes the amount of stratification in a UFAD zone. Kc is the fraction of the total internal convective heat gain that is assigned to the lower (occupied) subzone. The coefficients in the formula are defaulted based upon diffuser type. A user would normally allow this field to default. The default is *autocalculate*.

#### Field: Coefficient B

The coefficient B in the formula: Kc = A\*Gamma\*\*B + C + D\*Gamma + E\*Gamma\*\*2. Gamma is a variable that characterizes the amount of stratification in a UFAD zone. Kc is the fraction of the total internal convective heat gain that is assigned to the lower (occupied) subzone. The coefficients in the formula are defaulted based upon diffuser type. A user would normally allow this field to default. The default is *autocalculate*.

#### Field: Coefficient C

The coefficient C in the formula: Kc = A\*Gamma\*\*B + C + D\*Gamma + E\*Gamma\*\*2. Gamma is a variable that characterizes the amount of stratification in a UFAD zone. Kc is the fraction of the total internal convective heat gain that is assigned to the lower (occupied) subzone. The coefficients in the formula are defaulted based upon diffuser type. A user would normally allow this field to default. The default is *autocalculate*.

#### Field: Coefficient D

The coefficient D in the formula: Kc = A\*Gamma\*\*B + C + D\*Gamma + E\*Gamma\*\*2. Gamma is a variable that characterizes the amount of stratification in a UFAD zone. Kc is the fraction of the total internal convective heat gain that is assigned to the lower (occupied) subzone. The coefficients in the formula are defaulted based upon diffuser type. A user would normally allow this field to default. The default is *autocalculate*.

#### Field: Coefficient E

The coefficient E in the formula: Kc = A\*Gamma\*\*B + C + D\*Gamma + E\*Gamma\*\*2. Gamma is a variable that characterizes the amount of stratification in a UFAD zone. Kc is the fraction of the total internal convective heat gain that is assigned to the lower (occupied) subzone. The coefficients in the formula are defaulted based upon diffuser type. A user would normally allow this field to default. The default is *autocalculate*.

An example input is:

~~~~~~~~~~~~~~~~~~~~

      RoomAirModelType,
        SPACE5-1 RoomAir Model,  !- Name
        SPACE5-1,                !- Zone Name
        UnderFloorAirDistributionInterior,  !- Room-Air Modeling Type
        DIRECT;                  !- Air Temperature Coupling Strategy
    RoomAirSettings:UnderFloorAirDistributionInterior,
        SPACE5-1,                !- Zone Name
        Autocalculate,           !- Number of Diffusers
        Autocalculate,           !- Power per Plume
        Autocalculate,           !- Design Effective Area of Diffuser {m2}
        Autocalculate,           !- Diffuser Slot Angle from Vertical {deg}
        ,                        !- Thermostat Height {m}
        ,                        !- Comfort Height {m}
        0.001,         !- Temperature Difference Threshold for Reporting {deltaC}
        Swirl,                   !- Diffuser Type
        1.7,                     !- Transition Height {m}
        Autocalculate,           !- Coefficient A
        Autocalculate,           !- Coefficient B
        Autocalculate,           !- Coefficient C
        Autocalculate,           !- Coefficient D
        Autocalculate;           !- Coefficient E
~~~~~~~~~~~~~~~~~~~~

## RoomAirSettings:UnderFloorAirDistributionExterior

This model is applicable to exterior spaces that are served by an underfloor air distribution system. The dominant sources of heat gain should be from people, equipment, and other localized sources located in the occupied part of the room, as well as convective gain coming from a warm window. The model predicts two temperatures in the room:

- An occupied subzone temperature (T~OC~), representing the temperature in the region between the floor and the boundary of the upper subzone..
- An upper subzone temperature (T~MX~) essential for overall energy budget calculations and for modeling comfort effects of the upper layer temperature.

The following fields are used to define an instance of the ‘UCSD UFAD Exterior Model Controls' object.

### Inputs

#### Field: Zone Name

This field provides the unique name of a zone described elsewhere in the file. A single instance of the ‘[RoomAirSettings:UnderFloorAirDistributionExterior](#roomairsettingsunderfloorairdistributionexterior)' object is needed for each zone that is to be modeled using this method.

#### Field: Number of Diffusers 

#### **The total number of diffusers in this zone. This field can allowed to Autocalculate (in which case it is set to the design occupancy level; i.e., number of people). If the design occupancy is low or zero but there are still heat sources that could generate buoyancy driven plumes, the user should input a value based on the design supply air flow rate of the zone and the design flow rate of an individual diffuser. In the absence of any other information, divide the zone area by 100 ft^2^. The default for this field is Autocalculate.**

#### Field: Power per Plume

#### **The power in watts incorporated in a buoyancy driven thermal plume. Normally we assume all the loads of a workstation create a single plume so that this represents the convective heat gain from a workstation – 1 person, 1 computer terminal, plus any task lighting. A typical value would be 220 watts. However, the model assumes an "equivalent" plume derived from the zone extraction rate normalized to the number of workstations/occupants. This field should be allowed to default – the program will calculate a value based upon the occupancy and the extraction rate.  The default is Autocalculate.**

#### Field: Design Effective Area of Diffuser

This is the design air flow opening area in square meters of a single diffuser. The default value depends on the diffuser type. For swirl diffusers it is 0075 m^2^, for variable area diffusers 0.035 m^2^, for horizontal swirl diffusers 0.006 m^2^, and for linear bar grilles 0.03 m^2^. The default is *Autocalculate*..

#### Field: Diffuser Slot Angle from Vertical

This input describes the angle at which air emerges from the diffusers. It should be the angle  in degrees between the vertical and the diffuser slots. The defaults depend on the diffuser type: for swirl diffusers it is 28 degrees, for variable area diffusers 45 degrees, for DV diffusers 73 degrees, and for linear bar grilles 15 degrees.

#### Field: Thermostat Height

This field is the height in meters of the thermostat/temperature control sensor above the floor. The default is 1.2 meters.

#### Field: Comfort Height

The height in meters above the floor at which air temperature is calculated for comfort purposes. The air temperature at this height is used in calculating the available measures of comfort: Fanger, Pierce or KSU. The default is 1.1 meters.

#### Field: Temperature Difference Threshold for Reporting

This field specifies a minimum temperature difference between the upper subzone and the occupied subzone that will be used to trigger whether or not the UFAD auxiliary outputs will be calculated. These outputs are *Room Air [Zone](#zone) Transition Height* and *Room Air [Zone](#zone) Average Temperature Gradient*. They are set to zero when the temperature difference is less than the threshold and the output *Room Air [Zone](#zone) Is Mixed Status* is set to 1.

The value should be greater than or equal to zero and is in units of degrees Centigrade. The default value is 0.4 degrees C.

The value should be greater than or equal to zero and is in units of degrees Centigrade. The default value is 0.4 degrees C.

#### Field: Diffuser Type

The choices for this alpha field are **Swirl** **| **VariableArea** **| **HorizontalSwirl | LinearBarGrille | Custom.** The swirl and displacement diffusers are fixed area diffusers. The variable area diffusers maintain an approximately constant exit velocity. Linear bar grilles are normally used in exterior zonesand are fixed area diffusers. Custom is used to signify that the user intends to input the coefficients A – E (see below) rather than let the program set the coefficients based on diffuser type. The default is *Swirl*.

#### Field: Transition Height

An optional field to allow the user to specify the transition height (meters above floor) rather than have the program calculate it. The default is 1.7 meters.

#### Field: Coefficient A

The coefficient A in the formula: Kc = A\*Gamma\*\*B + C + D\*Gamma + E\*Gamma\*\*2. Gamma is a variable that characterizes the amount of stratification in a UFAD zone. Kc is the fraction of the total internal convective heat gain that is assigned to the lower (occupied) subzone. The coefficients in the formula are defaulted based upon diffuser type. A user would normally allow this field to default. The default is *autocalculate*.

#### Field: Coefficient B

The coefficient B in the formula: Kc = A\*Gamma\*\*B + C + D\*Gamma + E\*Gamma\*\*2. Gamma is a variable that characterizes the amount of stratification in a UFAD zone. Kc is the fraction of the total internal convective heat gain that is assigned to the lower (occupied) subzone. The coefficients in the formula are defaulted based upon diffuser type. A user would normally allow this field to default. The default is *autocalculate*.

#### Field: Coefficient C

The coefficient C in the formula: Kc = A\*Gamma\*\*B + C + D\*Gamma + E\*Gamma\*\*2. Gamma is a variable that characterizes the amount of stratification in a UFAD zone. Kc is the fraction of the total internal convective heat gain that is assigned to the lower (occupied) subzone. The coefficients in the formula are defaulted based upon diffuser type. A user would normally allow this field to default. The default is *autocalculate*.

#### Field: Coefficient D

The coefficient D in the formula: Kc = A\*Gamma\*\*B + C + D\*Gamma + E\*Gamma\*\*2. Gamma is a variable that characterizes the amount of stratification in a UFAD zone. Kc is the fraction of the total internal convective heat gain that is assigned to the lower (occupied) subzone. The coefficients in the formula are defaulted based upon diffuser type. A user would normally allow this field to default. The default is *autocalculate*.

#### Field: Coefficient E

The coefficient E in the formula: Kc = A\*Gamma\*\*B + C + D\*Gamma + E\*Gamma\*\*2. Gamma is a variable that characterizes the amount of stratification in a UFAD zone. Kc is the fraction of the total internal convective heat gain that is assigned to the lower (occupied) subzone. The coefficients in the formula are defaulted based upon diffuser type. A user would normally allow this field to default. The default is *autocalculate*.

An example input is:

~~~~~~~~~~~~~~~~~~~~

      RoomAirSettings:UnderFloorAirDistributionExterior,
        SPACE1-1,                !- Zone Name
        Autocalculate,           !- Number of Diffusers per Zone
        Autocalculate,           !- Power per Plume
        Autocalculate,           !- Design Effective Area of Diffuser {m2}
        Autocalculate,           !- Diffuser Slot Angle from Vertical {deg}
        ,                        !- Thermostat Height {m}
        ,                        !- Comfort Height {m}
        0.001,                   !- Temperature Difference Threshold for Reporting {deltaC}
        LinearBarGrille,         !- Diffuser Type
        1.7,                     !- Transition Height {m}
        Autocalculate,           !- Coefficient A
        Autocalculate,           !- Coefficient B
        Autocalculate,           !- Coefficient C
        Autocalculate,           !- Coefficient D
        Autocalculate;           !- Coefficient E
~~~~~~~~~~~~~~~~~~~~

### Outputs

Each room air model sets up outputs specific to that model. The effect of room air modeling is usually to adjust the adjacent air temperature along the inside surface of heat transfer surfaces. The output report "Surface Int Adjacent Air Temperature [C]" is provided for this and described under Surface Outputs.

#### Zone Predicted Sensible Load Room Air Correction Factor

This output variable provides the value of the correction factor applied to the predicted zone loads.  Predicted zone load to setpoint is (usually) used to control HVAC air systems and the presence of a room air model has an impact on the loads.  The predicited loads are multiplied by the factor in this output variable.  The value is always 1.0 when there is no Room Air model.  When there is a Room Air model, this output shows how that model is increasing or decreasing overall zone loads experienced by the air system.

### Outputs

The user-defined air temperature pattern that interpolates between two gradients produces the following output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Room Air Zone Vertical Temperature Gradient [K/m]
~~~~~~~~~~~~~~~~~~~~

#### Room Air Zone Vertical Temperature Gradient [K/m]

This output variable is the result of the interpolation performed by the user-defined roomair model using [RoomAir:TemperaturePattern:TwoGradient](#roomairtemperaturepatterntwogradient).  This is the temperature gradient in the vertical direction.  The units are degrees Kelvin per meter.

### Outputs

The following output is available for the Mundt model.

~~~~~~~~~~~~~~~~~~~~

    Room Air Node Air Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Room Air Node Air Temperature [C]

This output variable provides the drybulb air temperature used in, or calculated by, the Mundt model. The selection key is the name of an air node defined in a ROOMAIR Node object.

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Room Air Zone Mixed Subzone Temperature [C]
    HVAC,Average,Room Air Zone Occupied Subzone Temperature [C]
    HVAC,Average,Room Air Zone Floor Subzone Temperature [C]
    HVAC,Average,Room Air Zone Transition Height [m]
    HVAC,Average,Room Air Zone Recommended Minimum Flow Fraction
    HVAC,Average,Room Air Zone Is Mixed Status []
    HVAC,Average,Room Air Zone Average Temperature Gradient [K/m]
    HVAC,Average,Room Air Zone Maximum Temperature Gradient [K/m]
    HVAC,Average,Room Air Zone Thermal Comfort Effective Air Temperature [C]
    HVAC,Average,Room Air Zone Thermostat Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Room Air Zone Mixed Subzone Temperature [C]

The temperature of the upper well-mixed subzone in degrees C.

#### Room Air Zone Occupied Subzone Temperature [C]

The average temperature of the lower, stratified, occupied subzone in degrees C.

#### Room Air Zone Floor Subzone Temperature [C]

The temperature of the region near the floor in degrees C.

#### Room Air Zone Transition Height  [m]

The height above the floor, in meters, of the boundary between the lower occupied subzone and the upper well-mixed subzone.

#### Room Air Zone Recommended Minimum Flow Fraction []

The ratio of the minimum recommended flow rate to the actual flow rate. Here flow rate means the sum of infiltration, ventilation, mixing and system air flow rates. The minimum flow is the flow needed to establish the boundary between the occupied and mixed subzones at 1.5 meters.

#### Room Air Zone Is Mixed Status []

An integer flag that indicates whether the zone is mixed (single node well-mixed zone model used) or stratified (UCSD DV model used). The value 1 means well-mixed; 0 means stratified.

#### Room Air Zone Average Temperature Gradient  [K/m]

The temperature gradient between the middle of the floor region and the middle of the well-mixed upper subzone in degrees Kelvin per meter.

#### Room Air Zone Maximum Temperature Gradient [K/m]

The maximum of the temperature gradient between the middle of the floor region and the middle of the occupied subzone and the temperature gradient between the middle of the occupied subzone and the middle of the well-mixed subzone. The gradient is in degrees Kelvin per meter.

#### Room Air Zone Thermal Comfort Effective Air Temperature [C]

The temperature at the user specified comfort height in degrees C.

#### Room Air Zone Thermostat Temperature [C]

The temperature at the height of the thermostat (specified by user input) in degrees C.

### Outputs

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Room Air Zone Jet Region Temperature [C]
    Zone,Average,Room Air Zone Recirculation Region Temperature [C]
    Zone,Average,Room Air Zone Jet Region Average Air Velocity [m/s]
    Zone,Average,Room Air Zone Recirculation Region Average Air Velocity [m/s]
    Zone,Average,Room Air Zone Inflow Opening Area[m2]
    Zone,Average,Room Air Zone Room Length [m]
    Zone,Average,Room Air Zone Recirculation and Inflow Rate Ratio []
    Zone,Average,Room Air Zone Is Mixing Status []
    Zone,Average,Room Air Zone Is Recirculating Status []
~~~~~~~~~~~~~~~~~~~~

#### Room Air Zone Jet Region Temperature [C]

Average air temperature in the jet region of the flow in degrees C.

#### Room Air Zone Recirculation Region Temperature [C]

Average air temperature in the recirculation region of the flow in degrees C.

#### Room Air Zone Jet Region Average Air Velocity [m/s]

Average airflow velocity in the jet region of the flow in meters per second.

#### Room Air Zone Recirculation Region Average Air Velocity [m/s]

Average airflow velocity in the recirculation region of the flow in meters per second.

#### Room Air Zone Inflow Opening Area [m2]

Area of the inflow aperture in square meters. This area can change due to variation in wind direction (as inflow aperture changes) or variations in the opening schedule.

#### Room Air Zone Room Length [m]

Length of the room along the cross ventilation direction, in meters.

#### Room Air Zone Recirculation and Inflow Rate Ratio []

Ratio between airflow rate in the recirculation region and inflow rate, non-dimensional.

#### Room Air Zone Is Mixing Status []

An integer flag that indicates whether the zone is mixed (single node well-mixed zone model used) or significant momentum conservation is occurring (UCSD CV model used). The value 1 means well-mixed; 0 means cross ventilation is present. Transition to mixed flow can occur due to three mechanism: reduction in inflow velocity, reduction in inflow aperture area and dominance of buoyancy effects. A value of 1 is yes, a value of 0 is no.

#### Room Air Zone Is Recirculating Status []

An integer flag that indicates whether recirculations are present in the flow. A cross ventilation flow does not have recirculations whenever the inflow area is similar to the room cross section area (such as airflow in a typical corridor). A value of 1 is yes, a value of 0 is no.

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Room Air Zone Mixed Subzone Temperature [C]
    HVAC,Average,Room Air Zone Occupied Subzone Temperature [C]
    HVAC,Average,Room Air Zone Transition Height [m]
    HVAC,Average,Room Air Zone Is Mixed Status []
    HVAC,Average,Room Air Zone Average Temperature Gradient [K/m]
    HVAC,Average,Room Air Zone Effective Comfort Air Temperature [C]
    HVAC,Average,Room Air Zone Thermostat Temperature [C]
    HVAC,Average,Room Air Zone Transition Height Gamma Value [ ]
    HVAC,Average,Room Air Zone Plume Heat Transfer Rate [W]
    HVAC,Average,Room Air Zone Temperature Stratification Fraction []
    HVAC,Average,Room Air Zone Window Plume Heat Transfer Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Room Air Zone Mixed Subzone Temperature [C]

The temperature of the upper subzone in degrees C.

#### Room Air Zone Occupied Subzone Temperature [C]

The temperature of the lower, occupied subzone in degrees C.

#### Room Air Zone Transition Height [m]

The height above the floor, in meters, of the boundary between the lower occupied subzone and the upper subzone..

#### Room Air Zone Is Mixed Status []

An integer flag that indicates whether the zone is mixed (single node well-mixed zone model used) or stratified (UCSD UFI model used). The value 1 means well-mixed; 0 means stratified.

#### Room Air Zone Average Temperature Gradient [K/m]

The temperature gradient between the middle of the occupied subzone and the middle of the upper subzone in degrees Kelvin per meter.

#### Room Air Zone Effective Comfort Air Temperature [C]

The temperature at the user specified comfort height in degrees C.

#### Room Air Zone Thermostat Temperature [C]

The temperature at the height of the thermostat (specified by user input) in degrees C.

#### Room Air Zone Transition Height Gamma Value []

Value of gamma – a dimensionless "height" used in calculating the transition height. Lower values of gamma indicate increased stratification, higher values less. Generally the values should be between 2 and 30.

#### Room Air Zone Plume Heat Transfer Rate [W]

The heat in watts driving the plumes in the occupied subzone.

#### Room Air Zone Window Plume Heat Transfer Rate [W]

The convective heat gain from windows in an UnderFloorAirDistributionExterior zone.

#### Room Air Zone Temperature Stratification Fraction []

This output, (Phi) is a measure of temperature stratification in the space. It is the difference between the occupied subzone temperature and the supply temperature divided by difference between the return temperature and the supply temperature. Technically it is equal to Kc. As phi approaches 1, the space is fully mixed. As phi approaches 0, the space becomes fully stratified. Expressed as an equation:

*(T~occ~* – *T~sup~) / (T~ret~* *- T~sup~)*