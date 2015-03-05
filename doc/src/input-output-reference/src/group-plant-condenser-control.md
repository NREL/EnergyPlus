# Group – Plant-Condenser Control

## Operation Schemes (Plant and Condenser)

Plants and condenser loops must have some mechanism for controlling the operation of the loop and which equipment is available under what conditions. Since there may be multiple control schemes that are assigned various priorities associated with each loop, an overall operation scheme must be defined. The overall scheme consists of the object name, an identifying name that is referenced in the main plant or condenser loop statement, and a list of operation schemes. Each operation scheme must have the type of operation scheme, its identifying name, and the schedule that defines its availability. Note that the order in which the individual operation schemes appear in this list defines its priority versus the others with the same scheduled availability. The first scheme appearing in the list is given the highest priority, the second scheme has second highest priority, etc. In other words, if according to its schedule, the first operation scheme is available, then it is used by the simulation to define how the plant or condenser loop operates. If it is not available, the second operation scheme in the list is checked to see if it is available until the highest priority scheme that is also available is found.  To allow for simultaneous heating and cooling, there is one configuration where multiple operation schemes are available at a given timestep.  This is allowed if a user inputs both a heating range based operation and a cooling range based operation with overlapping schedules.  The simulation will look at the current loop demand to determine if the cooling scheme should be used or the heating scheme.  In other cases, if two schemes overlap (are available at the same time), a fatal error will be encountered.

## PlantEquipmentOperationSchemes

### Inputs

#### Field: Name

This alpha field contains the identifying name given to the Plant Operation Scheme. This name is placed in the Plant Loop object to select this scheme.

#### Field Set: (Component Object Type, Name, Schedule) up to 8

Operation schemes are listed in "priority" order.  Note that each scheme must address the entire load and/or condition ranges for the simulation. The actual one selected for use will be the first that is "Scheduled" on.  That is, if control scheme 1 is not "on" and control scheme 2 is -- then control scheme 2 is selected. Only plant equipment should be listed on a Control Scheme for this item.

#### Field: Control Scheme <#> Object Type

This alpha field contains the keyword for the type of control scheme used. The options for plant control schemes are:

- PlantEquipmentOperation:Uncontrolled
- PlantEquipmentOperation:CoolingLoad
- PlantEquipmentOperation:HeatingLoad
- PlantEquipmentOperation:ComponentSetpoint
- PlantEquipmentOperation:UserDefined

#### Field: Control Scheme <#> Name

This alpha field contains the identifying name given to the control scheme. These must be the named versions of the referenced Control Scheme (e.g.. a Uncontrolled Loop Operation with this name or a Cooling or Heating Load Range Based Operation with this name).

#### Field: Control Scheme <#> Schedule

This alpha field contains the schedule name for the control scheme. This schedule consists of weeks and days, with the days containing "0 or 1" for each hour of the day. This binary schedule (0 for off, 1 for on) determines if the control scheme is operating for that hour of day or not.

Examples of this statement in two different IDFs:

~~~~~~~~~~~~~~~~~~~~

    PlantEquipmentOperationSchemes,
        CW Loop Operation,       !- Name
        PlantEquipmentOperation:CoolingLoad,  ! Control Scheme 1 Object Type
        Peak Operation,          !- Control Scheme 1 Name
        On Peak,                 !- Control Scheme 1 Schedule
        PlantEquipmentOperation:CoolingLoad,  ! Control Scheme 2 Object Type
        Off Peak Operation,      !- Control Scheme 2 Name
        Off Peak;                !- Control Scheme 2 Schedule

    PlantEquipmentOperationSchemes,
        CW Loop Operation,       !- Name
        PlantEquipmentOperation:CoolingLoad,  ! Control Scheme 1 Object Type
        Central Chiller Only,    !- Control Scheme 1 Name
        PlantOnSched;            !- Control Scheme 1 Schedule
~~~~~~~~~~~~~~~~~~~~

In the first IDF example, "[PlantEquipmentOperation:CoolingLoad](#plantequipmentoperationcoolingload)" is the type of control scheme. "Peak Operation" and "Off Peak Operation" are the names of two different load range based controls defined separately (see below -- ). "On Peak" and "Off Peak" are schedules defined elsewhere in the input file.

The load range based operation scheme has two statements associated with it: a main statement that defines the ranges that individual priority settings are valid and the lists of equipment that may be used for each range.

## CondenserEquipmentOperationSchemes

### Inputs

#### Field: Name

This alpha field contains the identifying name given to the Condenser Operation Scheme.

#### Field Set: (Component Object Type, Name, Schedule) up to 8

Operation schemes are listed in "priority" order.  Note that each scheme must address the entire load and/or condition ranges for the simulation. The actual one selected for use will be the first that is "Scheduled" on.  That is, if control scheme 1 is not "on" and control scheme 2 is -- then control scheme 2 is selected. Only condenser equipment should be listed on a Control Scheme for this item.

#### Field: Control Scheme <#> Object Type

This alpha field contains the keyword for the type of control scheme used. The options for plant control schemes are:

This alpha field contains the keyword for the type of control scheme used. Currently, the available options are:

- PlantEquipmentOperation:Uncontrolled
- PlantEquipmentOperation:CoolingLoad
- PlantEquipmentOperation:HeatingLoad
- PlantEquipmentOperation:OutdoorDryBulb
- PlantEquipmentOperation:OutdoorWetBulb
- PlantEquipmentOperation:OutdoorDewpoint
- PlantEquipmentOperation:OutdoorRelativeHumidity
- PlantEquipmentOperation:OutdoorDryBulbDifference
- PlantEquipmentOperation:OutdoorWetBulbDifference
- PlantEquipmentOperation:OutdoorDewpointDifference
- PlantEquipmentOperation:UserDefined

The condenser operation schemes apply to the equipment on the ‘supply side' of the condenser loop—pumps, cooling towers, ground coupled heat exchangers, etc. The keywords select the algorithm that will be used to determine which equipment is available for each timestep. The Load Schemes **schemes select a user specified set of equipment for each user specified range of a particular simulation variable. Load schemes compare the demand on the condenser supply side with specified load ranges and associated equipment lists. Outdoor **schemes compare the current value of an environmental parameter with user specified ranges of that parameter.

#### Field: Control Scheme <#> Name

This alpha field contains the identifying name given to the control scheme.

#### Field: Control Scheme <#> Schedule

This alpha field contains the schedule name for the control scheme. This schedule consists of weeks and days, with the days containing "0 or 1" for each hour of the day. This binary schedule (0 for off, 1 for on) determines which control scheme is operating for a particular hour. The schedule facilitates daily or seasonal changes in control schemes to meet demand side management or energy conservation goals.

Scheduling conflicts are handled by selecting the first available scheme in order of appearance in the list.

Examples of this statement in two IDFs:

~~~~~~~~~~~~~~~~~~~~

    CondenserEquipmentOperationSchemes,
        Tower Loop Operation,    !- CondenserOperationSchemeName
        PlantEquipmentOperation:CoolingLoad,  Control Scheme 1 Object Type
        Year Round Tower Operation,  !- Control Scheme 1 Name
        PlantOnSched;            !- Control Scheme 1 Schedule

    CondenserEquipmentOperationSchemes,Tower Loop Operation,
        PlantEquipmentOperation:CoolingLoad, Peak Load Operation, AFTERNOONSCHEDULE,
        PlantEquipmentOperation:CoolingLoad, OffPeak Load Operation, MORNINGSCHEDULE,
        PlantEquipmentOperation:OutdoorWetBulb, NightOperation, NIGHTSCHEDULE;
~~~~~~~~~~~~~~~~~~~~

Each schedule (AFTERNOONSCHEDULE, MORNINGSCHEDULE, NIGHTSCHEDULE) invokes its associated operation scheme (Peak Load Operation, OffPeak Load Operation, NightOperation). The type of operation scheme and its name references the input syntax associated with the range specification and the associated equipment.

## PlantEquipmentOperation:Uncontrolled

Uncontrolled loop operation simply specifies a group of equipment that runs ‘uncontrolled'. If the loop runs, this equipment will run also, unless turned off by the loop flow resolver to maintain continuity in the fluid loop.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the control scheme.

#### Field: Equipment List Name

This alpha field contains the identifying name for the Equipment List. This field ties to the Load Range Equipment List input structure, which must also be defined.

#### Field: Equipment List Name

This alpha field contains the identifying name for the Equipment List. This field ties to the [PlantEquipmentList](#plantequipmentlist) or [CondenserEquipmentList](#condenserequipmentlist) input structure, which must also be defined.

## PlantEquipmentOperation:CoolingLoad

### Inputs

## PlantEquipmentOperation:HeatingLoad

The [PlantEquipmentOperation:CoolingLoad](#plantequipmentoperationcoolingload)  and [PlantEquipmentOperation:HeatingLoad](#plantequipmentoperationheatingload) objects define the different ranges and which equipment list is valid for each range. After the keyword and the identifying name, a series of data trios is expected. In each trio, there is a lower limit for the load range, an upper limit for the load range, and a name that links to an equipment availability list (the [PlantEquipmentList](#plantequipmentlist) or [CondenserEquipmentList](#condenserequipmentlist) objects).

### Inputs

#### Field: Name

This alpha field contains the identifying name for the control scheme.

#### Field Set: (Lower limit, Upper Limit, Equip List name) up to 10

#### Field: Load Range <#> Lower Limit

This numeric field contains the lower demand range (W) for the equipment list. If demand is below this value, then this equipment list will not turn on to meet the demand.

#### Field: Load Range <#> Upper Limit

This numeric field contains the upper demand range (W) for the equipment list. If demand is above this value, then this equipment list will not turn on to meet the demand.

#### Field: Range <#> Equipment List Name

This alpha field contains the identifying name for the Equipment List (i.e., the name of a [PlantEquipmentList](#plantequipmentlist) or [CondenserEquipmentList](#condenserequipmentlist) object).

An example of these statements in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    PlantEquipmentOperation:CoolingLoad, Peak Operation,
            0,7000,   Chiller Plant,
            7000,24500,   Chiller Plant and Purchased,
            24500,500000, Purchased Only;
~~~~~~~~~~~~~~~~~~~~

This particular load based operation scheme (above) has three different ranges. "Chiller Plant", "Chiller Plant and Purchased", and "Purchased Only" are names which link to various [PlantEquipmentList](#plantequipmentlist) or [CondenserEquipmentList](#condenserequipmentlist) objects as described below. Gaps may be left in the load ranges specified, but to operate equipment over the entire range the upper limit of a given range must equal the lower limit of the next range as shown in the example. If gaps are left in the load ranges specified, a warning message will be issued when the load to be met falls within a load range "gap". If the user wishes to leave a load range "gap" for a specific reason (no equipment to operate on this plant [or condenser] loop within this load range) and does not want the warning messages to be generated, then specify a lower limit, upper limit and equipment list name for the gap and do not specify any equipment in the associated equipment list, as shown below.

~~~~~~~~~~~~~~~~~~~~

    PlantEquipmentOperation:CoolingLoad, Peak Operation,
        0,7000,   Chiller Plant,
        7000,7100, NoEquipmentOperationOnThisPlantLoop,
        7100,24500,   Chiller Plant and Purchased,
        24500,500000, Purchased Only;

    PlantEquipmentList,
        Chiller Plant,           !- Name
        Chiller:Electric,        !- Equipment 1 Object Type
        Big Chiller;             !- Equipment 1 Name

    PlantEquipmentList,
        NoEquipmentOperationOnThisPlantLoop;    !- Name

      PlantEquipmentList,
        Chiller Plant and Purchased,  !- Name
        Chiller:Electric,        !- Equipment 1 Object Type
        Big Chiller,             !- Equipment 1 Name
        DistrictCooling,         !- Equipment 2 Object Type
        Purchased Cooling;       !- Equipment 2 Name

      PlantEquipmentList,
        Purchased Only,          !- Name
        DistrictCooling,         !- Equipment 1 Object Type
        Purchased Cooling;       !- Equipment 1 Name
~~~~~~~~~~~~~~~~~~~~

## PlantEquipmentOperation:OutdoorDryBulb

### Inputs

## PlantEquipmentOperation:OutdoorWetBulb 

### Inputs

## PlantEquipmentOperation:OutdoorRelativeHumidity

### Inputs

## PlantEquipmentOperation:OutdoorDewpoint

The outdoor operation objects define the different ranges of the various environmental parameters and which equipment list is valid for each range. After the keyword and the identifying name, a series of data trios is expected. In each trio, there is a lower limit for the load range, an upper limit for the load range, and a name that links to an equipment availability list (the [CondenserEquipmentList](#condenserequipmentlist)).

The fields for each of these are:

### Inputs

#### Field: Name

This alpha field contains the identifying name for the control scheme.

#### Field Set: (Lower limit, Upper Limit, Equip List name) up to 10

#### Field: <specfic type> Range <#> Lower Limit

This numeric field contains the lower limit (C for temperature operations, Percent for relative humidity operations) for the equipment list. If specific value is below this value, then this equipment list will not turn on to meet the demand.

#### Field: <specific type> Range <#> Upper Limit

This numeric field contains the lower limit (C for temperature operations, Percent for relative humidity operations) for the equipment list. If specific value is above this value, then this equipment list will not turn on to meet the demand.

#### Field: Range <#> Equipment List Name

This alpha field contains the identifying name for the Equipment List (i.e., the name of a [CondenserEquipmentList](#condenserequipmentlist) object).

## PlantEquipmentOperation:OutdoorDryBulbDifference

### Inputs

## PlantEquipmentOperation:OutdoorWetBulbDifference

### Inputs

## PlantEquipmentOperation:OutdoorDewpointDifference

The Delta Temperature based control strategies help to control any condenser equipment based on the difference between a reference node temperature and any environmental temperature. For example a cooling tower can be controlled by a strategy, which looks at the difference between the tower inlet temperature and wet-bulb temperature. A difference range is specified for each equipment list. IDD excerpts are shown below:

### Inputs

#### Field: Name

This alpha value contains the identifying name of the control strategy. This name appears as ‘control scheme name' in Condenser Operation Scheme object.

#### Field: Reference Temperature Node Name

This alpha value specifies the reference node for the operation scheme identified in the first field.

#### Field Set: (Lower limit, Upper limit, Equipment List)

#### Field: <specfic type> Range <#> Lower Limit

This numeric field specifies the minimum temperature difference required for the equipment specified in the equipment list to operate.

#### Field: <specfic type> Range <#> Upper Limit

This numeric field specifies the maximum temperature difference below which the equipment specified in the equipment list is available.

#### Field: Range <#> Equipment List Name

This alpha field contains the identifying name for the Equipment List (i.e., the name of a [CondenserEquipmentList](#condenserequipmentlist) object).

## PlantEquipmentOperation:ComponentSetpoint

In addition to load range based control on the plant loop, sequencing the plant components based on the outlet temperature of individual equipment is allowed. This scheme is common to many present-day Energy Management Systems sequencing. In this scheme, the sequencing is done based on the order specified in the control object described below.

### Inputs

#### Field: Name

This field specifies the name of the operation scheme.

#### Field Set: Equipment Object Type, Name, Demand Calculation Node, Setpoint Node, Flow Rate, Operation Type)

#### Field: Equipment <#> Object Type

This field specifies the type of equipment controlled by scheme.

#### Field: Equipment <#> Name

This field specifies the name of the controlled equipment.

#### Field: Demand Calculation <#> Node Name

The component demand will be calculated using the difference between the temperature at the demand node and the component set point temperature.

#### Field: Setpoint <#> Node Name

Each component controlled under temperature based control will have its own set point different from the loop set point. This field specifies component set point node (Generally its outlet temperatures). This node is acted upon by a SetpointManager in order to obtain the setpoint at any simulation timestep.

#### Field: Component <#> Flow Rate

This numeric field specifies the design flow rate for the component specified in earlier fields. This flow rate is used to calculate the component demand. The field can be set to autosize, if the user wants the program to calculate the design flow. This would generally be set to autosize when the user does not know the component flow rate and does a sizing calculation for the corresponding component.

#### Field: Operation<#> Type

This alpha field specifies the operation mode for the equipment. The equipment can be in any of the three modes viz. Cooling, Heating and Dual. Dual is used when the components both as heating and cooling equipment (for example heat pumps).

And, as an example in an IDF:

~~~~~~~~~~~~~~~~~~~~

    PlantEquipmentOperationSchemes,
        CW Loop Operation,       !- PlantOperationSchemeName
        PlantEquipmentOperation:ComponentSetpoint, ! Control Scheme 1 Object Type
        Test Scheme,                         !- Control Scheme 1 Name
        On;                                  !- Control Scheme 1 Schedule

    PlantEquipmentOperation:ComponentSetpoint,
        Test Scheme,
        CHILLER:ELECTRIC,        !- Equipment 1 Object Type
        Little Chiller,          !- Equipment 1 Name
        Little Chiller Inlet Node, !- Demand Calculation 1 Node Name
        Little Chiller Outlet Node, !- Setpoint 1 Node Name
        0.0011,                     !- Component 1 Flow Rate
        COOLING,                    !- Operation 1 Type
        CHILLER:ELECTRIC,        !- Equipment 2 Object Type
        Big Chiller,             !- Equipment 2 Name
        Big Chiller Inlet Node,  !- Demand Calculation 2 Node Name
        Big Chiller Outlet Node, !- Setpoint 2 Node Name
        0.0011,                  !- Component 2 Flow Rate
        COOLING;                 !- Operation 2 Type
~~~~~~~~~~~~~~~~~~~~

## PlantEquipmentList

### Inputs

## CondenserEquipmentList

The [PlantEquipmentList](#plantequipmentlist)  and [CondenserEquipmentList](#condenserequipmentlist)  specify available plant and condenser loop equipment respectively for any loop operation scheme. Each statement contains the object name, an identifying name (which links the definition back to one of the operation scheme statements) and a variable length of data pairs. These pairs refer to a plant equipment type and an identifying name. The type in this list of pairs must correspond to a valid plant object as described in the next subsection.

> Note: If a [PlantEquipmentList](#plantequipmentlist) or [CondenserEquipmentList](#condenserequipmentlist) object is specified with no equipment object types or equipment names, then the corresponding PlantEquipmentOperation:\* object will assume all available equipment on this plant (or condenser) loop should be OFF (not operate) within the specified lower/upper limit.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the Equipment List.

#### Field Set: (Object Type, Name) up to 10

Remember the order of equipment on this list determines operation priority. Equipment on the list first will try to meet demand first. These fields tie to the Equipment Definition input structure, which must also be defined.

#### Field: Equipment <#> Object Type

This alpha field contains the keyword for the type of equipment in operation. Example: ([Chiller:Electric](#chillerelectric), [DistrictCooling](#districtcooling)). The full list of allowable plant equipment is valid plant objects as described in the next subsection.

#### Field: Equipment <#> Name

This alpha field contains the identifying name for the equipment in operation. This distinguishes between two different pieces of equipment of the same type.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    PlantEquipmentList, Chiller Plant and Purchased,
            Chiller:Electric, Big Chiller,
            DistrictCooling, Purchased Cooling;
~~~~~~~~~~~~~~~~~~~~

Note that this list is the middle range defined in the [PlantEquipmentOperation:CoolingLoad](#plantequipmentoperationcoolingload) statement named "Peak Operation" above. This defines the equipment available when the load encountered by the plant (chiller water demand, for example) is between 7000 W and 24500 W. EnergyPlus will run the "Big Chiller" first up to its capacity and then attempt to meet the remaining load (while in that range) with the next piece of equipment (in this case, "Purchased:Cooling") in the list.