# Group – Operational Faults

## Introduction to Operational Faults Modeling

Most of the buildings, either new or old, have operational faults in the sensors, controllers, meters, equipment and systems. Being able to model and simulate these faults and their impact on energy performance of buildings is crucial to improve accuracy of building simulations and to support the retrofit of buildings.

To date, the main practitioner use of EnergyPlus has been for new construction design. With the new high priority attached by USDOE to retrofit and improved operation of existing buildings, there is a need to extend the capabilities of EnergyPlus to model existing buildings, including faulty operation:

Retrofit analysis: starts with calibrated simulation; the ability to estimate the severity of common faults is expected to improve the accuracy and transparency of the calibrated model and hence the increase accuracy of the analysis of different retrofit measures.

Commissioning providers can use the fault models to demonstrate the saving to be expected from fixing faults found in retro-commissioning

Support for building operation by using the calibrated model, including unfixed faults, as a real-time reference model to detect, and verify the diagnosis of, newly occurring faults.

The users in these cases are practitioners, not power users, so it is needed to implement the fault models using conventional EnergyPlus objects rather than the EMS, which, in any case, could only be used to model limited types of faults.

## Operational Fault Objects

EnergyPlus contains a number of objects to model operational faults of sensors, meters, equipment and systems. The current implementation allows the model of sensors faults with air economizers and heating and cooling coil fouling.

The objects used by EnergyPlus to model sensors faults in an air economizer and the coil fouling are as follows:

- FaultModel:TemperatureSensorOffset:OutdoorAir
- FaultModel:HumiditySensorOffset:OutdoorAir
- FaultModel:EnthalpySensorOffset:OutdoorAir
- FaultModel:TemperatureSensorOffset:ReturnAir
- FaultModel:EnthalpySensorOffset:ReturnAir
- FaultModel:Fouling:Coil

## FaultModel:TemperatureSensorOffset:OutdoorAir 

This object defines the offset of an outdoor air dry-bulb temperature sensor that is used to determine applicability of an air economizer.

### Inputs

#### Field: Name

This is the user-defined name of the fault.

#### Field: Availability Schedule Name 

This field provides the name of a schedule that will determine if this fault is applicable. When a fault is not applicable it is not modeled in the simulations. When it is applicable, then a user-defined sensor offset and a severity schedule will be applied. This schedule should be set to "1.0" when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Severity Schedule Name 

This field provides the name of a schedule that represents severity of a fault. This is used as a multiplier to the user-defined temperature offset. This schedule should be set to a non-zero value when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Controller Object Type  

This field defines the controller object type that this fault is associated with. Choices are from a list of applicable controller types. Current implementation supports air economizer – the choice is [Controller:OutdoorAir](#controlleroutdoorair).

#### Field: Controller Object Name 

This field defines the name of the controller object associated with the fault. It should be one of the objects with the defined types.

#### Field: Temperature Sensor Offset 

This field defines the offset of the temperature sensor. A positive value means the sensor reads a temperature that is higher than the real value. A negative value means the sensor reads a temperature that is lower than the real value. A "0.0" value means no offset. Default is 0.0. The units are in degrees Celsius.

## FaultModel:HumiditySensorOffset:OutdoorAir 

This object defines the offset of an outdoor air humidity sensor that is used to determine applicability of an air economizer.

### Inputs

#### Field: Name

This is the user-defined name of the fault.

#### Field: Availability Schedule Name 

This field provides the name of a schedule that will determine if this fault is applicable. When a fault is not applicable it is not modeled in the simulations. When it is applicable, then a user-defined sensor offset and a severity schedule will be applied. This schedule should be set to "1.0" when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Severity Schedule Name 

This field provides the name of a schedule that represents severity of a fault. This is used as a multiplier to the user-defined temperature offset. This schedule should be set to a non-zero value when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Controller Object Type  

This field defines the controller object type that this fault is associated with. Choices are from a list of applicable controller types. Current implementation supports air economizer – the choice is [Controller:OutdoorAir](#controlleroutdoorair).

#### Field: Controller Object Name 

This field defines the name of the controller object associated with the fault. It should be one of the objects with the defined types.

#### Field: Humidity Sensor Offset 

This field defines the offset of the humidity ratio sensor. A positive value means the sensor reads a humidity ratio that is higher than the real value. A negative value means the sensor reads a humidity ratio that is lower than the real value. A "0.0" value means no offset. Default is 0.0. The units are in kgWater/kgDryAir.

## FaultModel:EnthalpySensorOffset:OutdoorAir 

This object defines the offset of an outdoor enthalpy sensor that is used to determine applicability of an air economizer.

### Inputs

#### Field: Name

This is the user-defined name of the fault.

#### Field: Availability Schedule Name 

This field provides the name of a schedule that will determine if this fault is applicable. When a fault is not applicable it is not modeled in the simulations. When it is applicable, then a user-defined sensor offset and a severity schedule will be applied. This schedule should be set to "1.0" when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Severity Schedule Name 

This field provides the name of a schedule that represents severity of a fault. This is used as a multiplier to the user-defined temperature offset. This schedule should be set to a non-zero value when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Controller Object Type  

This field defines the controller object type that this fault is associated with. Choices are from a list of applicable controller types. Current implementation supports air economizer – the choice is [Controller:OutdoorAir](#controlleroutdoorair).

#### Field: Controller Object Name 

This field defines the name of the controller object associated with the fault. It should be one of the objects with the defined types.

#### Field: Enthalpy Sensor Offset 

This field defines the offset of the enthalpy sensor. A positive value means the sensor reads an enthalpy that is higher than the real value. A negative value means the sensor reads an enthalpy that is lower than the real value. A "0.0" value means no offset. Default is 0.0. The units are in J/kg.

## FaultModel:TemperatureSensorOffset:ReturnAir 

This object defines the offset of a return air dry-bulb temperature sensor that is used to determine applicability of an air economizer.

### Inputs

#### Field: Name

This is the user-defined name of the fault.

#### Field: Availability Schedule Name 

This field provides the name of a schedule that will determine if this fault is applicable. When a fault is not applicable it is not modeled in the simulations. When it is applicable, then a user-defined sensor offset and a severity schedule will be applied. This schedule should be set to "1.0" when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Severity Schedule Name 

This field provides the name of a schedule that represents severity of a fault. This is used as a multiplier to the user-defined temperature offset. This schedule should be set to a non-zero value when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Controller Object Type  

This field defines the controller object type that this fault is associated with. Choices are from a list of applicable controller types. Current implementation supports air economizer – the choice is [Controller:OutdoorAir](#controlleroutdoorair).

#### Field: Controller Object Name 

This field defines the name of the controller object associated with the fault. It should be one of the objects with the defined types.

#### Field: Temperature Sensor Offset 

This field defines the offset of the temperature sensor. A positive value means the sensor reads a temperature that is higher than the real value. A negative value means the sensor reads a temperature that is lower than the real value. A "0.0" value means no offset. Default is 0.0. The units are in degrees Celsius.

## FaultModel:EnthalpySensorOffset:ReturnAir 

This object defines the offset of a return air enthalpy sensor that is used to determine applicability of an air economizer.

### Inputs

#### Field: Name

This is the user-defined name of the fault.

#### Field: Availability Schedule Name 

This field provides the name of a schedule that will determine if this fault is applicable. When a fault is not applicable it is not modeled in the simulations. When it is applicable, then a user-defined sensor offset and a severity schedule will be applied. This schedule should be set to "1.0" when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Severity Schedule Name 

This field provides the name of a schedule that represents severity of a fault. This is used as a multiplier to the user-defined temperature offset. This schedule should be set to a non-zero value when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Controller Object Type  

This field defines the controller object type that this fault is associated with. Choices are from a list of applicable controller types. Current implementation supports air economizer – the choice is [Controller:OutdoorAir](#controlleroutdoorair).

#### Field: Controller Object Name 

This field defines the name of the controller object associated with the fault. It should be one of the objects with the defined types.

#### Field: Enthalpy Sensor Offset 

This field defines the offset of the enthalpy sensor. A positive value means the sensor reads an enthalpy that is higher than the real value. A negative value means the sensor reads an enthalpy that is lower than the real value. A "0.0" value means no offset. Default is 0.0. The units are in J/kg.

## FaultModel:Fouling:Coil 

This object defines the fouling for a simple water cooling coil or simple water heating coil.

### Inputs

#### Field: Name

This is the user-defined name of the fault.

#### Field: Coil Name 

This field defines the name of the simple cooling coil or simple heating coil that has the fouling fault.

#### Field: Availability Schedule Name 

This field provides the name of a schedule that will determine if this fault is applicable. When a fault is not applicable it is not modeled in the simulations. When it is applicable, then user-defined fouling and a severity schedule will be applied. This schedule should be set to "1.0" when a fault is applicable and "0.0" when it is not. If this field is blank, the schedule has values of 1 for all time periods

#### Field: Severity Schedule Name 

This field provides the name of a schedule that represents severity of a fault. This is used as a multiplier to the user-defined fouling. If this field is blank, the schedule has values of 1 (no changes to fouling) for all time periods.

#### Field: Fouling Input Method  

There are two methods to input the fouling, i.e. FouledUARated and FoulingFactor. User chooses the appropriate method to determine the coil fouling.

#### Field: UAFouled 

This is the overall coil UA value including the coil fouling when the "FouledUARated" method is used. The unit is W/K.

#### Field: Water Side Fouling Factor 

The following four fields specify the required inputs when the "FoulingFactor" method is used. This field defines the fouling factor for the water side. The units are in m^2^-K/W.

#### Field: Air Side Fouling Factor  

This field defines the fouling factor for the air side. The units are in m^2^-K/W.

#### Field: Outside Coil Surface Area 

This field defines outside surface area of the cooling or heating coil. The units are in m^2^.

#### Field: Inside to Outside Coil Surface Area Ratio 

This field specifies the inside to outside surface area ratio of the cooling or heating coil.

IDF examples:

~~~~~~~~~~~~~~~~~~~~

      ! example faults for an air economizer

    Schedule:Compact,
        OATSeveritySch,          !- Name
        On/Off,                  !- Schedule Type Limits Name
        Through: 6/30,           !- Field 1
        For: AllDays,            !- Field 2
        Until: 24:00,0,          !- Field 3
        Through: 12/31,          !- Field 4
        For: AllDays,            !- Field 5
        Until: 24:00,1;          !- Field 6

    FaultModel:TemperatureSensorOffset:OutdoorAir,
       OATFault,                 !- Name
       ALWAYS_ON,                !- Availability Schedule Name
       OATSeveritySch,           !- Severity Schedule Name
       Controller:OutdoorAir,    !- Controller Object Type
       VAV_1_OA_Controller,      !- Controller Object Name
       -2.0;                      !- Temperature Sensor Offset, deg C

    FaultModel:TemperatureSensorOffset:ReturnAir,
       RATFault,                 !- Name
       ,                         !- Availability Schedule Name
       ,                         !- Severity Schedule Name
       Controller:OutdoorAir,    !- Controller Object Type
       VAV_2_OA_Controller,      !- Controller Object Name
       -2.0;                     !- Temperature Sensor Offset, deg C

    FaultModel:EnthalpySensorOffset:ReturnAir,
       RAHFault,                 !- Name
       ,                         !- Availability Schedule Name
       ,                         !- Severity Schedule Name
       Controller:OutdoorAir,    !- Controller Object Type
       VAV_2_OA_Controller,      !- Controller Object Name
       -2000;                    !- Enthalpy Sensor Offset, J/Kg

    ! example faults for a fouling coil

    FaultModel:Fouling:Coil,
       CoolingCoilFault,        !- Name
       VAV_2_CoolC,             !- Heating or Cooling Coil Name
       ALWAYS_ON,               !- Availability Schedule Name
       ,                        !- Severity Schedule Name
       FouledUARated,           !- Fouling Input Method
       3000,                    !- UAFouled, W/K
       ,                        !- Water Side Fouling Factor, m2-K/W
       ,                        !- Air Side Fouling Factor, m2-K/W
       ,                        !- Outside Coil Surface Area, m2
       ;                        !- Inside to Outside Coil Surface Area Ratio
~~~~~~~~~~~~~~~~~~~~