# Group – Exterior Energy Use Equipment

To facilitate the reporting of exterior consumption (to the building's heat balance/loads considerations), several objects have been included. The consumption related to these items will appear on meters specific to the items.

## Exterior:Lights

### Inputs

#### Field: Name

This descriptive name allows the values of exterior lights consumption to appear in the "normal" output variable list as well as the meters. It cannot be blank nor can it be duplicated by other [Exterior:Lights](#exteriorlights) statements.

#### Field: Schedule Name

A schedule will allow the exterior lights consumption to be operationally different, hour to hour as well as seasonally. Fractional values in the basic schedule will be applied to the design level field below.

#### Field: Design Level

This field (in Watts) is typically used to represent the maximum electrical input to exterior lighting fixtures that is then multiplied by a schedule fraction (see previous field). In EnergyPlus, this is slightly more flexible in that the lighting design level could be a "diversity factor" applied to a schedule of real numbers. Note that while the schedule value can vary from hour to hour and seasonally, the design level field is constant for all simulation environments.

#### Field: Control Option

This field is used to determine how the exterior lights are controlled. There are currently two options, ‘ScheduleNameOnly' and ‘AstronomicalClock.'  If this field is omitted or left blank then the program will default to Schedule Name Only mode. The ‘ScheduleNameOnly' mode dictates that the exterior lights always follow the schedule named in the field above. The ‘AstronomicalClock' mode dictates that despite what the schedule indicates, the exterior lights will not run when the sun is up. Using the Astronomical Clock mode makes it simple to model exterior lights that are controlled by a photocell or other controller that ensures that outdoor lights will not run during the daytime. However, the Astronomical Clock control works off of the position of the sun and therefore does not operate exactly like a photocell. During the night, the schedule values are still applied in the usual way.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Grounds [Lights](#lights)", "Facade [Lights](#lights)", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the lights will be assigned to the "General" end-use subcategory.

An IDF Example:

~~~~~~~~~~~~~~~~~~~~

    Exterior:Lights,
      OutsideLights,       !- Name
      ON,                  !- SCHEDULE Name
      1000,                !- Design Level
      AstronomicalClock,  !- Control Option
      Grounds Lights;      !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

### Outputs

Output for exterior lights appears in three possible places. It will appear on two meters (Electricity:Facility and ExteriorLights:Electricity) as well as in its own designated value in the standard output file.

Output for exterior lights appears in three possible places. It will appear on two meters (Electricity:Facility and ExteriorLights:Electricity) as well as in its own designated value in the standard output file.

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Exterior Lights Electric Power [W]
    Zone,Sum,Exterior Lights Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Exterior Lights Electric Power [W]

#### Exterior Lights Electric Energy [J]

These are the electric power and energy consumed by the exterior lights.

## Exterior:FuelEquipment

### Inputs

#### Field: Name

This descriptive name allows the values of exterior equipment consumption to appear in the "normal" output variable list as well as the meters. It cannot be blank nor can it be duplicated by other [Exterior:FuelEquipment](#exteriorfuelequipment) or [Exterior:WaterEquipment](#exteriorwaterequipment) statements.

#### Field: Fuel Use Type

This field designates the appropriate meter for the exterior fuel equipment. Valid fuel types are: Electricity, NaturalGas, PropaneGas, FuelOil#1, FuelOil#2, Diesel, Gasoline, Coal, Steam, [DistrictHeating](#districtheating), [DistrictCooling](#districtcooling), OtherFuel1 and OtherFuel2. The fuel type triggers the application of consumption amounts to the appropriate energy meters.

#### Field: Schedule Name

A schedule will allow the exterior lights consumption to be operationally different, hour to hour as well as seasonally. Fractional values in the basic schedule will be applied to the design level field below.

#### Field: Design Level

This field (Watts) is typically used to represent the maximum input to exterior fixtures that is then multiplied by a schedule fraction (see previous field). In EnergyPlus, this is slightly more flexible in that the design level could be a "diversity factor" applied to a schedule of real numbers. Note that while the schedule value can vary from hour to hour and seasonally, the design level field is constant for all simulation environments.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Fountains", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the equipment will be assigned to the "General" end-use subcategory.

IDF Examples:

~~~~~~~~~~~~~~~~~~~~

    Exterior:FuelEquipment,OutsideGasEq,Gas,ON,1000;
    Exterior:FuelEquipment,OutsideCoalEq,Coal,ON,1000;
    Exterior:FuelEquipment,OutsideFuelOil1Eq,FuelOil#1,ON,1000;
    Exterior:FuelEquipment,OutsideFuelOil2Eq,FuelOil#2,ON,1000;
    Exterior:FuelEquipment,OutsideLPGEq,PropaneGas,ON,1000;
~~~~~~~~~~~~~~~~~~~~

## Exterior:WaterEquipment

### Inputs

#### Field: Name

This descriptive name allows the values of exterior equipment consumption to appear in the "normal" output variable list as well as the meters. It cannot be blank nor can it be duplicated by other [Exterior:FuelEquipment](#exteriorfuelequipment) or [Exterior:WaterEquipment](#exteriorwaterequipment) statements.

#### Field: FuelUseType

For [Exterior:WaterEquipment](#exteriorwaterequipment), only "Water" is current allowed for this field.

#### Field: Schedule Name

A schedule will allow the exterior water consumption to be operationally different, hour to hour as well as seasonally. Fractional values in the basic schedule will be applied to the design level field below.

#### Field: Design Level

This field (in m3/s) is typically used to represent the maximum volumetric flow for water fixtures that is then multiplied by a schedule fraction (see previous field). In EnergyPlus, this is slightly more flexible in that the exterior water equipment design level could be a "diversity factor" applied to a schedule of real numbers. Note that while the schedule value can vary from hour to hour and seasonally, the design level field is constant for all simulation environments.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Landscaping", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the equipment will be assigned to the "General" end-use subcategory.

IDF Example:

~~~~~~~~~~~~~~~~~~~~

    Exterior:WaterEquipment,OutsideWaterEq,Water,ON,10000;
~~~~~~~~~~~~~~~~~~~~

### Outputs

Output for exterior equipment consumption appears in three possible places. It will appear on two meters (<Fuel Type>:Facility and ExteriorEquipment:<Fuel Type>) as well as in its own designated value in the standard output file, Exterior Equipment <Fuel Type> Energy. Consumption also appears on any user-defined end-use subcategory meters, i.e., <End-Use Subcategory>:ExteriorEquipment:<Fuel Type>. All outputs are in [J] except water consumption which is in [m3]. There are also rate versions

~~~~~~~~~~~~~~~~~~~~

    Zone, Average, Exterior Equipment Fuel Rate [W]
    Zone,Sum, Exterior Equipment Electric Energy [J]
    Zone,Sum, Exterior Equipment Gas Energy [J]

    Zone,Average, Exterior Equipment Water Volume Flow Rate [m3/s]
    Zone,Sum, Exterior Equipment Water Volume [m3]
    Zone,Sum, Exterior Equipment Mains Water Volume [m3]

    Zone,Sum, Exterior Equipment Coal Energy [J]
    Zone,Sum, Exterior Equipment FuelOil#1 Energy [J]
    Zone,Sum, Exterior Equipment FuelOil#2 Energy [J]

    Zone,Sum, Exterior Equipment Propane Energy [J]

    Zone,Sum, Exterior Equipment Gasoline Energy [J]

    Zone,Sum, Exterior Equipment Diesel Energy [J]
    Zone,Sum, Exterior Equipment Steam Energy [J]
    Zone,Sum, Exterior Equipment OtherFuel1 Energy [J]
    Zone,Sum, Exterior Equipment OtherFuel2 Energy [J]
    Zone,Sum, Exterior Equipment District Heating Energy [J]
    Zone,Sum, Exterior Equipment District Cooling Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Exterior Equipment Fuel Rate [W]

#### Exterior Equipment <Fuel Type> Energy [J]

These are the fuel consumption rate and energy for the exterior equipment.

#### Exterior Equipment Water Volume Flow Rate [m3/s]

#### Exterior Equipment Water Volume [m3]

These are the water consumption rate and volume for the exterior equipment.

Exterior Equipment Mains Water Volume [m3]This is the water volume drawn for the mains water service to serve the exterior equipment.