# Group – ExternalInterface

The [ExternalInterface](#externalinterface) allows coupling EnergyPlus to the [Building](#building) Controls Virtual Test Bed (BCVTB). It supports the import of Functional Mock-up Units (FMUs) for co-simulation as well as the export of EnergyPlus as a FMU for co-simulation. BCVTB is a software environment that allows expert users to couple different simulation programs for distributed simulation or for a real-time simulation that is connected to a building control system. For example, the BCVTB allows simulation of the building envelope and HVAC system in EnergyPlus and the control logic in MATLAB/Simulink, while exchanging data between the two programs as they simulate. The BCVTB can be downloaded from http://simulationresearch.lbl.gov/bcvtb. A FMU is a component which implements the Functional Mock-up Interface (FMI) standard (http://www.modelisar.com).

## ExternalInterface

This object activates the external interface of EnergyPlus. If the object [ExternalInterface](#externalinterface) is present, then the external interface objects listed below will receive their values from the BCVTB interface or from FMUs at each zone time step. If this object is not present, then the values of these objects will be fixed at the value declared in the "initial value" field of the corresponding object, and a warning will be written to the EnergyPlus error file.

### Inputs

#### Field: Name

The name of the external interface. The only valid entries are **PtolemyServer**, **FunctionalMockupUnitImport**, and **FunctionalMockupUnitExport**.

An example IDF object is:

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface,           !- Object to activate the external interface
     PtolemyServer;              !- Name of external interface
~~~~~~~~~~~~~~~~~~~~

## Building Controls Virtual Test Bed (BCVTB)

The following objects are designed to couple EnergyPlus with the BCVTB.

## ExternalInterface:Schedule

This input object is similar to [Schedule:Compact](#schedulecompact). However, during the time stepping, its value is set to the value received from the external interface. During the warm-up period and the system sizing, its value is set to the value specified by the field "initial value."

### Inputs

#### Field: Name

This field should contain a unique (within all DaySchedules) designation for this schedule. It is referenced by WeekSchedules to define the appropriate schedule values.

#### Field: Schedule Type Limits Name

This field contains a reference to the Schedule Type Limits object. If found in a list of Schedule Type Limits (see above), then the restrictions from the referenced object will be used to validate the current field values.

#### Field: Initial Value

This field contains the schedule value that is used during the warm-up period and during the system sizing.

~~~~~~~~~~~~~~~~~~~~

    ! Cooling schedule. This schedule is set directly by the external interface.
    ! During warm-up and system-sizing, it is fixed at 24 degC.
      ExternalInterface:Schedule,
        TSetCoo,                 !- Name
        Temperature,             !- ScheduleType
        24;                      !- Initial value, used during warm-up

    ! Heating schedule. This schedule is set directly by the external interface.
    ! During warm-up and system-sizing, it is fixed at 20 degC.
      ExternalInterface:Schedule,
        TSetHea,                 !- Name
        Temperature,             !- ScheduleType
        20;                      !- Initial value, used during warm-up
~~~~~~~~~~~~~~~~~~~~

## ExternalInterface:Actuator

This object maps a value received from the external interface to an actuator of the Energy Management System. The object is similar to [EnergyManagementSystem:Actuator](#energymanagementsystemactuator). However, during the time stepping, its value is set to the value received from the external interface. During the warm-up period and the system sizing, its value is set to the value specified by the field "initial value."

### Inputs

#### Field: Name

This field contains a unique name for the actuator. No spaces are allowed in the object name. This name will be a global read-only variable in Erl programs and cannot duplicate any other global scope Erl variable.

#### Field: Actuated Component Unique Name

This field defines a unique name for the specific entity that is to be controlled. The names for each individual component are listed in the EDD output file when Verbose mode is used – see the input object **Output:EnergyManagementSystem** for more on the EDD file. These will often be user-defined names of input objects or system nodes, but some actuators are automatically setup by the program and will not be completely user-defined.

#### Field: Actuated Component Type

The field defines the type of the entity that is to be controlled by the actuator. The component types available vary with the specifics of individual models. The types of components that can be used as actuators in a specific model are listed in the EDD output file – see the input object **Output:EnergyManagementSystem** for more on the EDD file. Components can be object types defined elsewhere in the IDD but there are other types of entities such as nodes and system-level actuators that do not directly correspond to IDF objects.

#### Field: Actuated Component Control Type

This field defines the type of control to be done on the specific entity being controlled. The control types available are listed in the EDD output. Specific components may have more than one type of control available, such as flow rate or temperature, and this field is used to distinguish between them.

#### Field: Initial Value

This field contains the initial value. If a value is specified, then this value is used during the warm-up period and the system sizing. If no value is specified, then the actuated component will only be updated once the time stepping starts, i.e., after the warm-up and the system-sizing. This allows, for example, to schedule a setpoint with a [Schedule:Compact](#schedulecompact) during warm-up and system-sizing, and then overwrite the value of this schedule during each zone time step based on a feedback control loop that is implemented in the BCVTB.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:Actuator,
        Zn001_Wall001_Win001_Shading_Deploy_Status,  !- Name
        Zn001:Wall001:Win001,    !- Actuated Component Unique Name
        Window Shading Control,  !- Actuated Component Type
        Control Status,          !- Actuated Component Control Type
         ;                       ! initial value
~~~~~~~~~~~~~~~~~~~~

## ExternalInterface:Variable

This input object is similar to [EnergyManagementSystem:GlobalVariable](#energymanagementsystemglobalvariable). However, during the time stepping, its value is set to the value received from the external interface. During the warm-up period and the system sizing, its value is set to the value specified by the field "initial value." This object can be used to move data into Erl subroutines.

### Inputs

#### Field: Name

This field becomes the global Erl variable name that can be referenced in the EnergyPlus Runtime Language. No spaces are allowed in the object name. The name must be unique across all global scope variables including those declared as sensor and actuators and the built-in variables.

#### Field: Initial Value

This field contains the initial value that is used during the warm-up period and during the system sizing.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:Variable,
        yShade,                  !- Name of Erl variable
        1;                       !- Initial value
~~~~~~~~~~~~~~~~~~~~

## Functional Mock-up Unit (FMU) Import

The following objects are designed to couple EnergyPlus with FMUs for co-simulation.

## ExternalInterface:FunctionalMockupUnitImport

This object defines the FMU that will be linked to EnergyPlus.

### Inputs

#### Field: FMU File Name

This field contains the name of the FMU file including the extension. The field should include a full path with file name, for best results. The field must be <= 100 characters. The file name must not include commas or an exclamation point. A relative path or a simple file name should work with version 7.0 or later when using EP-Launch even though EP-Launch uses temporary directories as part of the execution of EnergyPlus. If using RunEPlus.bat to run EnergyPlus from the command line, a relative path or a simple file name may work if RunEPlus.bat is run from the folder that contains EnergyPlus.exe.

#### Field: FMU Timeout

This field contains the communication timeout value in milli-seconds to allow interprocess communication to take place.

#### Field: FMU LoggingOn

This field contains the loggingOn value to enable or disable debug in FMU. If loggingOn=1, debug logging is enabled. If loggingOn=0, debug logging is disabled.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:FunctionalMockupUnitImport,
        MoistAir.fmu,            !- FMU File Name
        15,                      !- FMU Timeout
        0;                       !- FMU LoggingOn
~~~~~~~~~~~~~~~~~~~~

## ExternalInterface:FunctionalMockupUnitImport:From:Variable

This input object maps the names of the output variables of EnergyPlus to the input variables of the FMU.

### Inputs

#### Field: EnergyPlus Key Value

This input object maps the names of the EnergyPlus output variables (**Output:Variable** or **EnergyManagementSystem:OutputVariable**) to the input variables of the FMU.

#### Field: EnergyPlus Variable Name

This field contains the output variable name as defined in the InputOutput Reference. For an **EnergyManagementSystem:OutputVariable**, the EnergyPlus Variable Name is the name of the **EnergyManagementSystem:OutputVariable**'s object.

#### Field: FMU File Name

This field contains the name of the FMU file including the extension. The field should include a full path with file name, for best results. The field must be <= 100 characters. The file name must not include commas or an exclamation point. A relative path or a simple file name should work with version 7.0 or later when using EP-Launch even though EP-Launch uses temporary directories as part of the execution of EnergyPlus. If using RunEPlus.bat to run EnergyPlus from the command line, a relative path or a simple file name may work if RunEPlus.bat is run from the folder that contains EnergyPlus.exe.

#### Field: FMU Instance Name

This field contains the EnergyPlus instance name of the FMU. See External Interface Application guide for more details.

#### Field: FMU Variable Name

This field contains the name of the variable in the FMU that will be mapped to the corresponding variable in EnergyPlus.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:FunctionalMockupUnitImport:From:Variable,
        Environment,             !- EnergyPlus Key Value
        Site Outdoor Air Drybulb Temperature,    !- EnergyPlus Variable Name
        MoistAir.fmu,            !- FMU File Name
        Model1,                  !- FMU Instance Name
        TDryBul;                 !- FMU Variable Name

    ExternalInterface:FunctionalMockupUnitImport:From:Variable,
        ZONE ONE,                !- EnergyPlus Key Value
        Zone Mean Air Temperature, !- EnergyPlus Variable Name
        MoistAir.fmu,            !- FMU File Name
        Model1,                  !- FMU Instance Name
        TRooMea;                 !- FMU Variable Name

    ExternalInterface:FunctionalMockupUnitImport:From:Variable,
        Environment,             !- EnergyPlus Key Value
        Site Outdoor Air Relative Humidity,  !- EnergyPlus Variable Name
        MoistAir.fmu,            !- FMU File Name
        Model1,                  !- FMU Instance Name
        outRelHum;               !- FMU Variable Name

    ExternalInterface:FunctionalMockupUnitImport:From:Variable,
        ZONE ONE,                !- EnergyPlus Key Value
        Zone Air Relative Humidity,  !- EnergyPlus Variable Name
        MoistAir.fmu,            !- FMU File Name
        Model1,                  !- FMU Instance Name
        rooRelHum;               !- FMU Variable Name
~~~~~~~~~~~~~~~~~~~~

## ExternalInterface:FunctionalMockupUnitImport:To:Schedule

This input object is similar to [Schedule:Compact](#schedulecompact). However, during the time stepping, its value is set to the value received from the external interface. During the system sizing, its value is set to the value specified by the field "initial value."

### Inputs

#### Field: EnergyPlus Variable Name

This field contains a unique (within all Schedules) designation for this schedule in EnergyPlus.

#### Field: Schedule Type Limits Name

This field contains a reference to the Schedule Type Limits object. If found in a list of Schedule Type Limits (see above), then the restrictions from the referenced object will be used to validate the current field values.

#### Field: FMU File Name

This field contains the name of the FMU file including the extension. The field should include a full path with file name, for best results. The field must be <= 100 characters. The file name must not include commas or an exclamation point. A relative path or a simple file name should work with version 7.0 or later when using EP-Launch even though EP-Launch uses temporary directories as part of the execution of EnergyPlus. If using RunEPlus.bat to run EnergyPlus from the command line, a relative path or a simple file name may work if RunEPlus.bat is run from the folder that contains EnergyPlus.exe.

#### Field: FMU Instance Name

This field contains the EnergyPlus instance name of the FMU. See External Interface Application guide for more details.

#### Field: FMU Variable Name

This field contains the name of the variable in the FMU that will be mapped to the schedule in EnergyPlus.

#### Field: Initial Value

This field contains the schedule value that is used during the system sizing.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:FunctionalMockupUnitImport:To:Schedule,
        FMU_OthEquSen_ZoneOne,   !- EnergyPlus Variable Name
        Any Number,              !- Schedule Type Limits Names
        MoistAir.fmu,            !- FMU File Name
        Model1,                  !- FMU Instance Name
        QSensible,               !- FMU Variable Name
        0;                       !- Initial Value

    ExternalInterface:FunctionalMockupUnitImport:To:Schedule,
        FMU_OthEquLat_ZoneOne,   !- EnergyPlus Variable Name
        Any Number,              !- Schedule Type Limits Names
        MoistAir.fmu,            !- FMU File Name
        Model1,                  !- FMU Instance Name
        QLatent,                 !- FMU Variable Name
        0;                       !- Initial Value
~~~~~~~~~~~~~~~~~~~~

## ExternalInterface:FunctionalMockupUnitImport:To:Actuator

This object maps a value received from the external interface to an actuator of the Energy Management System. The object is similar to [EnergyManagementSystem:Actuator](#energymanagementsystemactuator). However, during the warm-up and time stepping, its value is set to the value received from the external interface. During the system sizing, its value is set to the value specified by the field "initial value."

### Inputs

#### Field: EnergyPlus Variable Name

This field contains a unique name for the actuator. No spaces are allowed in the object name. This name will be a global read-only variable in Erl programs and cannot duplicate any other global scope Erl variable.

#### Field: Actuated Component Unique Name

This field defines a unique name for the specific entity that is to be controlled. The names for each individual component are listed in the EDD output file when Verbose mode is used – see the input object **Output:EnergyManagementSystem** for more on the EDD file. These will often be user-defined names of input objects or system nodes, but some actuators are automatically setup by the program and will not be completely user-defined.

#### Field: Actuated Component Type

The field defines the type of the entity that is to be controlled by the actuator. The component types available vary with the specifics of individual models. The types of components that can be used as actuators in a specific model are listed in the EDD output file – see the input object **Output:EnergyManagementSystem** for more on the EDD file. Components can be object types defined elsewhere in the IDD but there are other types of entities such as nodes and system-level actuators that do not directly correspond to IDF objects.

#### Field: Actuated Component Control Type

This field defines the type of control to be done on the specific entity being controlled. The control types available are listed in the EDD output. Specific components may have more than one type of control available, such as flow rate or temperature, and this field is used to distinguish between them.

#### Field: FMU File Name

This field contains the name of the FMU file including the extension. The field should include a full path with file name, for best results. The field must be <= 100 characters. The file name must not include commas or an exclamation point. A relative path or a simple file name should work with version 7.0 or later when using EP-Launch even though EP-Launch uses temporary directories as part of the execution of EnergyPlus. If using RunEPlus.bat to run EnergyPlus from the command line, a relative path or a simple file name may work if RunEPlus.bat is run from the folder that contains EnergyPlus.exe.

#### Field: FMU Instance Name

This field contains the EnergyPlus instance name of the FMU. See External Interface Application guide for more details.

#### Field: FMU Variable Name

This field contains the name of the variable in the FMU that will be mapped to the actuator in EnergyPlus.

#### Field: Initial Value

This field contains the value that is used during the system sizing.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:FunctionalMockupUnitImport:To:Actuator,
    Zn001_Wall001_Win001_Shading_Deploy_Status,  !- EnergyPlus Variable Name
        Zn001:Wall001:Win001,                 !- Actuated Component Unique Name
        Window Shading Control,                  !- Actuated Component Type
        Control Status,                       !- Actuated Component Control Type
        ShadingController.fmu,                   !- FMU File Name
        Model1,                                  !- FMU Instance Name
        yShade,                                  !- FMU Variable Name
        6;                                       !- Initial Value
~~~~~~~~~~~~~~~~~~~~

## ExternalInterface:FunctionalMockupUnitImport:To:Variable

This input object is similar to [EnergyManagementSystem:GlobalVariable](#energymanagementsystemglobalvariable). However, during the warm-up and time stepping, its value is set to the value received from the external interface. During the system sizing its value is set to the value specified by the field "initial value." This object can be used to move data into Erl subroutines.

### Inputs

#### Field: EnergyPlus Variable Name

This field becomes the global Erl variable name that can be referenced in the EnergyPlus Runtime Language. No spaces are allowed in the object name. The name must be unique across all global scope variables including those declared as sensor and actuators and the built-in variables.

#### Field: FMU File Name

This field contains the name of the FMU file including the extension. The field should include a full path with file name, for best results. The field must be <= 100 characters. The file name must not include commas or an exclamation point. A relative path or a simple file name should work with version 7.0 or later when using EP-Launch even though EP-Launch uses temporary directories as part of the execution of EnergyPlus. If using RunEPlus.bat to run EnergyPlus from the command line, a relative path or a simple file name may work if RunEPlus.bat is run from the folder that contains EnergyPlus.exe.

#### Field: FMU Instance Name

This field contains the EnergyPlus instance name of the FMU. See External Interface Application guide for more details.

#### Field: FMU Variable Name

This field contains the name of the variable in the FMU that will be mapped to the corresponding variable in EnergyPlus.

#### Field: Initial Value

This field contains the initial value that is used during the system sizing.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:FunctionalMockupUnitImport:To:Variable,
        Shade_Signal,            !- EnergyPlus Variable Name
        ShadingController.fmu,   !- FMU File Name
        Model1,                  !- FMU Instance Name
        yShade,                  !- FMU Variable Name
        1;                       !- Initial Value
~~~~~~~~~~~~~~~~~~~~

## Functional Mock-up Unit (FMU) Export

The following objects are designed to support the export of EnergyPlus as a FMU for co-simulation.

## ExternalInterface:FunctionalMockupUnitExport:From:Variable

This object exposes the EnergyPlus output variables (**Output:Variable** or **EnergyManagementSystem:OutputVariable**) to the outside simulation program.

### Inputs

#### Field: EnergyPlus Key Value

This field contains a Key Value for an EnergyPlus output variable (ref Output:Variable/.rdd file). For an **EnergyManagementSystem:OutputVariable**, the EnergyPlus Key Value needs to be set to "**EMS**".

#### Field: EnergyPlus Variable Name

This field contains the Variable Name as defined in the InputOutput Reference. For an **EnergyManagementSystem:OutputVariable**, the EnergyPlus Variable Name is the name of the **EnergyManagementSystem:OutputVariable**'s object.

#### Field: FMU Variable Name

This field contains the name of the variable in the model description file of the FMU that will be mapped to the corresponding variable in EnergyPlus.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:FunctionalMockupUnitExport:From:Variable,
        Environment,             !- EnergyPlus Key Value
        Site Outdoor Air Drybulb Temperature,  !- EnergyPlus Variable Name
        TDryBul;                 !- FMU Variable Name

    ExternalInterface: FunctionalMockupUnitExport:From:Variable,
        ZONE ONE,                  !- EnergyPlus Key Value
        Zone Mean Air Temperature, !- EnergyPlus Variable Name
        TRooMea;                   !- FMU Variable Name

    ExternalInterface: FunctionalMockupUnitExport:From:Variable,
        Environment,                !- EnergyPlus Key Value
        Site Outdoor Air Relative Humidity,  !- EnergyPlus Variable Name
        outRelHum;                  !- FMU Variable Name

    ExternalInterface:FunctionalMockupUnitExport:From:Variable,
        ZONE ONE,                    !- EnergyPlus Key Value
        Zone Air Relative Humidity,  !- EnergyPlus Variable Name
        rooRelHum;                   !- FMU Variable Name
~~~~~~~~~~~~~~~~~~~~

## ExternalInterface:FunctionalMockupUnitExport:To:Schedule

This object is similar to [Schedule:Compact](#schedulecompact). However, during the time stepping, its value is set to the value received from the external interface. During the warm-up period and the system sizing, its value is set to the value specified by the field "initial value."

### Inputs

#### Field: EnergyPlus Variable Name

This field contains a unique (within all Schedules) designation for this schedule in EnergyPlus.

#### Field: Schedule Type Limits Name

This field contains a reference to the Schedule Type Limits object. If found in a list of Schedule Type Limits (see above), then the restrictions from the referenced object will be used to validate the current field values.

#### Field: FMU Variable Name

This field contains the name of the variable in the model description file of the FMU that will be mapped to the schedule in EnergyPlus.

#### Field: Initial Value

This field contains the schedule value that is used during the warm-up period and during the system sizing.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:FunctionalMockupUnitExport:To:Schedule,
        FMU_OthEquSen_ZoneOne,   !- EnergyPlus Variable Name
        Any Number,              !- Schedule Type Limits Names
        QSensible,               !- FMU Variable Name
        0;                       !- Initial Value
~~~~~~~~~~~~~~~~~~~~

## ExternalInterface:FunctionalMockupUnitExport:To:Actuator

This object maps a value received from the external interface to an actuator of the Energy Management System. The object is similar to [EnergyManagementSystem:Actuator](#energymanagementsystemactuator). However, during the time stepping, its value is set to the value received from the external interface. During the warm-up period and the system sizing, its value is set to the value specified by the field "initial value."

### Inputs

#### Field: EnergyPlus Variable Name

This field contains a unique name for the actuator. No spaces are allowed in the object name. This name will be a global read-only variable in Erl programs and cannot duplicate any other global scope Erl variable.

#### Field: Actuated Component Unique Name

This field defines a unique name for the specific entity that is to be controlled. The names for each individual component are listed in the EDD output file when Verbose mode is used – see the input object **Output:EnergyManagementSystem** for more on the EDD file. These will often be user-defined names of input objects or system nodes, but some actuators are automatically setup by the program and will not be completely user-defined.

#### Field: Actuated Component Type

The field defines the type of the entity that is to be controlled by the actuator. The component types available vary with the specifics of individual models. The types of components that can be used as actuators in a specific model are listed in the EDD output file – see the input object **Output:EnergyManagementSystem** for more on the EDD file. Components can be object types defined elsewhere in the IDD but there are other types of entities such as nodes and system-level actuators that do not directly correspond to IDF objects.

#### Field: Actuated Component Control Type

This field defines the type of control to be done on the specific entity being controlled. The control types available are listed in the EDD output. Specific components may have more than one type of control available, such as flow rate or temperature, and this field is used to distinguish between them.

#### Field: FMU Variable Name

This field contains the name of the variable in the model description file of the FMU that will be mapped to the actuator in EnergyPlus.

#### Field: Initial Value

This field contains the initial value. If a value is specified, then this value is used during the warm-up period and the system sizing. If no value is specified, then the actuated component will only be updated once the time stepping starts, i.e., after the warm-up and the system-sizing.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:FunctionalMockupUnitExport:To:Actuator,
    Zn001_Wall001_Win001_Shading_Deploy_Status,  !- EnergyPlus Variable Name
        Zn001:Wall001:Win001,                  !- Actuated Component Unique Name
        Window Shading Control,                !- Actuated Component Type
        Control Status,                        !- Actuated Component Control Type
        yShade,                                !- FMU Variable Name
        6;                                     !- Initial Value
~~~~~~~~~~~~~~~~~~~~

## ExternalInterface:FunctionalMockupUnitExport:To:Variable

This input object is similar to [EnergyManagementSystem:GlobalVariable](#energymanagementsystemglobalvariable). However, during the time stepping, its value is set to the value received from the external interface. During the warm-up period and the system sizing, its value is set to the value specified by the field "initial value." This object can be used to move data into Erl subroutines.

### Inputs

#### Field: EnergyPlus Variable Name

This field becomes the global Erl variable name that can be referenced in the EnergyPlus Runtime Language. No spaces are allowed in the object name. The name must be unique across all global scope variables including those declared as sensor and actuators and the built-in variables.

#### Field: FMU Variable Name

This field contains the name of the variable in the model description file of the FMU that will be mapped to the corresponding variable in EnergyPlus.

#### Field: Initial Value

This field contains the initial value that is used during the warm-up period and during the system sizing.

~~~~~~~~~~~~~~~~~~~~

    ExternalInterface:FunctionalMockupUnitExport:To:Variable,
        Shade_Signal,            !- EnergyPlus Variable Name
        yShade,                  !- FMU Variable Name
        1;                       !- Initial Value
~~~~~~~~~~~~~~~~~~~~