# Group – User Defined HVAC and Plant Component Models

This group of EnergyPlus input objects provides methods for users to include their own custom models for HVAC and plant components as well as plant equipment operation. This is helpful because not every conceivable type of HVAC equipment or system configuration has already had models implemented in EnergyPlus.  HVAC or plant components, or even entire system configurations, might be so new, unusual, or proprietary that no model has yet been implemented in the software.  An analyst faced with project that needs to model such a device or system could do so using one or more of the user defined components described in this section.

Some building energy performance rating systems allow analysts to option of using "exceptional calculation methods" when a simulation program cannot perform the function required to be modeled. These user defined component models are intended to provide a means for including such exceptional calculations within the context of a full EnergyPlus model.  This allows using EnergyPlus for everything it can model and extending it yourself to include a new type of HVAC or Plant component.

These objects are advanced.  Making good use of them requires the user to have a detailed understanding of EnergyPlus HVAC and Plant models as well as the Energy Management System (EMS).  The EnergyPlus Runtime Language (Erl) is used to program the component model.  The Application Guide for EMS provides more discussion of how to use the objects in this group – see the chapter called User-Defined Component Models.

## ZoneHVAC:ForcedAir:UserDefined

This object is used to define a generic zone air unit for custom component models.  This object has a primary air connection that connects to a zone through exhaust and inlet nodes.  There is an optional secondary air connection that can be used as an air-based source or heat rejection connection.  As many has three different plant connections can be made to the zone unit.  Water storage tanks can be connected for supply or collection.  An ambient zone can be connected for skin losses to be treated as separate internal gains.

### Inputs

#### Field: Name

This field defines a unique user-assigned name for an instance of a custom, forced-air zone conditioning unit.  Any reference to this zone unit by another object will use this name.

#### Field: Overall Model Simulation Program Calling Manager Name

This field specifies the Erl programs that are used to model the custom zone unit.  This field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere.  The program manager referenced here should include all the Erl programs that are to be run each time the component is simulated and use the calling point called UserDefinedComponentModel.

#### Field: Model Setup and Sizing Program Calling Manager Name

This field specifies the Erl programs that are used to initialize and size the custom zone unit.  This field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere and uses the calling point called UserDefinedComponentModel.  The program manager referenced here should include all the Erl programs that are needed to do any initial setup that should occur before the main modeling programs are run.  These include calculating and setting values for things that do not vary over time such as component sizes, control parameters, modeling constants etc.  The user defined component will trigger this calling manager at the beginning of each new environment period.

#### Field: Primary Air Inlet Node Name

This field defines the name of the HVAC system node from which the zone unit draws its inlet air from the zone. This node must be the name of a zone air exhaust node (ref. ZoneHVAC:EquipmentConnection). This field is required.

#### Field: Primary Air Outlet Node Name

This field defines the name of the HVAC system node to which the zone unit sends its outlet air to the zone.  This node must be the name of a zone air inlet node (ref. ZoneHVAC:EquipmentConnection). This field is required.

#### Field: Secondary Air Inlet Node Name

This field defines the name of a HVAC system node from which the zone unit can draw additional air.  This air inlet stream has a variety of possible uses including outdoor air supply for ventilation or a condenser inlet for heat rejection.  This field is optional.

#### Field: Secondary Air Outlet Node Name

This field defines the name of a HVAC system node to which the zone unit can send additional air.  This air outlet stream has a variety of possible uses including outdoor air relief for exhaust or a condenser outlet for heat rejection.  This field is optional.

#### Field: Number of Plant Loop Connections

This fields defines the number of different plant loop connections that the zone unit will use.  Up to three separate loops can be connected if desired.  This field is required.  Enter a 0 if the unit does not use plant at all.

#### Field: Plant Connection 1 Inlet Node Name

This field defines the name of a plant system node from which the zone unit can draw fluid.  This is the inlet to the component model from the first plant loop that is connected to the zone unit.

#### Field: Plant Connection 1 Outlet Node Name

This field defines the name of a plant system node to which the zone unit can send fluid.  This is the outlet from the component model to the first plant loop that is connected to the zone unit.

#### Field: Plant Connection 2 Inlet Node Name

This field defines the name of a plant system node from which the zone unit can draw fluid.  This is the inlet to the component model from the second plant loop that is connected to the zone unit.

#### Field: Plant Connection 2 Outlet Node Name

This field defines the name of a plant system node to which the zone unit can send fluid.  This is the outlet from the component model to the second plant loop that is connected to the zone unit.

#### Field: Plant Connection 3 Inlet Node Name

This field defines the name of a plant system node from which the zone unit can draw fluid.  This is the inlet to the component model from the third plant loop that is connected to the zone unit.

#### Field: Plant Connection 3 Outlet Node Name

This field defines the name of a plant system node to which the zone unit can send fluid.  This is the outlet from the component model to the third plant loop that is connected to the zone unit.

#### Field: Supply Inlet Water Storage Tank Name

This field is used to describe where the zone unit obtains water if it is to be connected to a water storage tank.  This water could be used for evaporative cooling. If a name of a [WaterUse:Storage](#waterusestorage) object is used here, then the unit can obtain its water from that tank.  This field is optional.

#### Field: Collection Outlet Water Storage Tank Name

This field is used to describe where the zone unit sends water it has collected if is to be stored in a tank.  This water could be collected from coil condensate.  If a name of a [WaterUse:Storage](#waterusestorage) object is used here, then the unit can send water to that tank.  This field is optional.

#### Field: Ambient Zone Name

This field is used to connect ancillary losses from the zone unit to a thermal zone.  The unit may have inefficiencies, leaks, or other non-ideal operation that results in some untended impact on the space surrounding the unit.  These "skin losses" can be assigned to a thermal zone and appear as internal gains for the zone named here.  This does not need to be the same thermal zone as the zone that unit is intended to condition.

An example of this input object follows.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:ForcedAir:UserDefined,
       Zone1WindAC,  !- Name
       Zone 1 Window AC Model Program Manager , !- Overall Model Simulation Program Calling Manager Name
       Zone 1 Window AC Init Program Manager , !- Model Setup and Sizing Program Calling Manager Name
       Zone1WindACAirInletNode , !- Primary Air Inlet Node Name
       Zone1WindACAirOutletNode, !- Primary Air Outlet Node Name
       Zone1WindACOAInNode,  !- Secondary Air Inlet Node Name
       Zone1WindACExhNode,  !- Secondary Air Outlet Node Name
       0 ; !- Number of Plant Loop Connections
~~~~~~~~~~~~~~~~~~~~

## AirTerminal:SingleDuct:UserDefined

This object is used to define a generic single duct air terminal unit for custom component models.  This object has a primary air connection that connects a multi-zone air handler to a zone.  There is an optional secondary air connection that can be used and an air-based source or heat rejection connection.  As many as two different plant connections can be made to the air terminal unit.  Water storage tanks can be connected for supply or collection.  An ambient zone can be connected for skin losses to be treated as separate internal gains.

### Field: Name

This field defines a unique user-assigned name for an instance of a custom, air terminal unit.  Any reference to this air terminal by another object will use this name.

### Field: Overall Model Simulation Program Calling Manager Name

This field specifies the Erl programs that are used to model the custom air terminal unit.  This field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere.  The program manager referenced here should include all the Erl programs that are to be run each time the component is simulated and use the calling point called UserDefinedComponentModel.

### Field: Model Setup and Sizing Program Calling Manager Name

This field specifies the Erl programs that are used to initialize and size the custom air terminal unit.  This field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere and uses the calling point called UserDefinedComponentModel.  The program manager referenced here should include all the Erl programs that are needed to do any initial setup that should occur before the main modeling programs are run.  These include calculating and setting values for things that do not vary over time such as component sizes, control parameters, modeling constants etc.  The user defined component will trigger this calling manager at the beginning of each new environment period.

### Field: Primary Air Inlet Node Name

This field defines the name of the HVAC system node from which the air terminal unit draws its inlet air.  This node must be the name of a HVAC system node that is listed in a zone splitter (ref. [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter)). This field is required.

### Field: Primary Air Outlet Node Name

This field defines the name of the HVAC system node to which the air terminal unit sends its outlet air to the zone.  This node must be the name of a zone air inlet node (ref. ZoneHVAC:EquipmentConnection). This field is required.

### Field: Secondary Air Inlet Node Name

This field defines the name of a HVAC system node from which the air terminal unit can draw additional air.  This air inlet stream has a variety of possible uses including outdoor air supply for ventilation or a condenser inlet for heat rejection.  This field is optional.

### Field: Secondary Air Outlet Node Name

This field defines the name of a HVAC system node to which the air terminal unit can send additional air.  This air outlet stream has a variety of possible uses including outdoor air relief for exhaust or a condenser outlet for heat rejection.  This field is optional.

### Field: Number of Plant Loop Connections

This fields defines the number of different plant loop connections that the air terminal unit will use.  Up to two separate loop can be connected if desired.  This field is required.  Enter a 0 if the unit does not use plant at all.

### Field: Plant Connection 1 Inlet Node Name

This field defines the name of a plant system node from which the air terminal unit can draw fluid.  This is the inlet to the component model from the first plant loop that is connected to the air terminal unit.

### Field: Plant Connection 1 Outlet Node Name

This field defines the name of a plant system node to which the air terminal unit can send fluid.  This is the outlet from the component model to the first plant loop that is connected to the air terminal unit.

### Field: Plant Connection 2 Inlet Node Name

This field defines the name of a plant system node from which the air terminal unit can draw fluid.  This is the inlet to the component model from the second plant loop that is connected to the air terminal unit.

### Field: Plant Connection 2 Outlet Node Name

This field defines the name of a plant system node to which the air terminal unit can send fluid.  This is the outlet from the component model to the second plant loop that is connected to the air terminal unit.

### Field: Supply Inlet Water Storage Tank Name

This field is used to describe where the air terminal unit obtains water if it is to be connected to a water storage tank.  This water could be used for evaporative cooling. If a name of a [WaterUse:Storage](#waterusestorage) object is used here, then the unit can obtain its water from that tank.  This field is optional.

### Field: Collection Outlet Water Storage Tank Name

This field is used to describe where the air terminal unit sends water it has collected if is to be stored in a tank.  This water could be collected from coil condensate.  If a name of a [WaterUse:Storage](#waterusestorage) object is used here, then the unit can send water to that tank.  This field is optional.

### Field: Ambient Zone Name

This field is used to connect ancillary losses from the air terminal unit to a thermal zone.  The unit may have inefficiencies, leaks, or other non-ideal operation that results in some untended impact on the space surrounding the unit.  These "skin losses" can be assigned to a thermal zone and appear as internal gains for the zone named here.  This does not need to be the same thermal zone as the zone that terminal unit is intended to condition.

An example of this input object follows.

~~~~~~~~~~~~~~~~~~~~

    AirTerminal:SingleDuct:UserDefined,
       SPACE1-1 VAV Reheat,  !- Name
       Space1 ATU Sim Programs , !- Overall Model Simulation Program Calling Manager Name
       Space1 ATU Init Programs , !- Model Setup and Sizing Program Calling Manager Name
       SPACE1-1 ATU In Node , !- Primary Air Inlet Node Name
       SPACE1-1 In Node , !- Primary Air Outlet Node Name
       ,  !- Secondary Air Inlet Node Name
       ,  !- Secondary Air Outlet Node Name
       1,  !- Number of Plant Loop Connections
       SPACE1-1 Zone Coil Water In Node , !- Plant Connection 1 Inlet Node Name
       SPACE1-1 Zone Coil Water Out Node; !- Plant Connection 1 Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

## Coil:UserDefined

This object is used to define a generic coil for custom component modeling of a device that processes air as part of an air handeler.  The Coil:UserDefined object appears directly on a [Branch](#branch) object used to define the supply side of an air handler, or in the [AirLoopHVAC:OutdoorAirSystem:EquipmentList](#airloophvacoutdoorairsystemequipmentlist) object used to define outdoor air systems.  This object has one or two air connections and an optional plant connection.  Water storage tanks can be connected for supply or collection.  An ambient zone can be connected for skin losses to be treated as separate internal gains.

### Field: Name

This field defines a unique user-assigned name for an instance of a custom air coil.  Any reference to this coil by another object will use this name.

### Field: Overall Model Simulation Program Calling Manager Name

This field specifies the Erl programs that are used to model the custom coil.  This field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere.  The program manager referenced here should include all the Erl programs that are to be run each time the component is simulated and use the calling point called UserDefinedComponentModel.

### Field: Model Setup and Sizing Program Calling Manager Name

This field specifies the Erl programs that are used to initialize and size the custom coil.  This field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere and uses the calling point called UserDefinedComponentModel.  The program manager referenced here should include all the Erl programs that are needed to do any initial setup that should occur before the main modeling programs are run.  These include calculating and setting values for things that do not vary over time such as component sizes, control parameters, modeling constants etc.  The user defined component will trigger this calling manager at the beginning of each new environment period.

### Field: Number of Air Connections

This field defines the number of air loop connections that the coil will use.  The coil must have at least one connection but a second can be used by entering a 2.

### Field: Air Connection 1 Inlet Node Name

This field defines the name of the HVAC system node from which the coil draws its inlet air for its first air loop connection. This node must be the outlet of the HVAC system component located upstream, if there is one.  This field is required.

### Field: Air Connection 1 Outlet Node Name

This field defines the name of the HVAC system node to which the coil sends its outlet air for its first air loop connection. This node must be the inlet of the HVAC system component located downstream, if there is one.  This field is required.

### Field: Air Connection 2 Inlet Node Name

This field defines the name of the HVAC system node from which the coil draws its inlet air for its second air loop connection. This node must be the outlet of the HVAC system component located upstream, if there is one.  This field is optional.

### Field: Air Connection 2 Outlet Node Name

This field defines the name of the HVAC system node to which the coil sends its outlet air for its second air loop connection. This node must be the inlet of the HVAC system component located downstream, if there is one.  This field is optional.

### Field: Plant Connection is Used

This field defines whether or not the plant loop connection is going to be used.  Enter a Yes if the coil will use plant, enter No if not.  This field is required.

### Field: Plant Connection Inlet Node Name

This field defines the name of a plant system node from which the coil can draw fluid.  This is the inlet to the component model from the plant loop that is connected to the coil.

### Field: Plant Connection Outlet Node Name

This field defines the name of a plant system node to which the coil can send fluid.  This is the outlet from the component model to the plant loop that is connected to the coil.

### Field: Supply Inlet Water Storage Tank Name

This field is used to describe where the coil obtains water if it is to be connected to a water storage tank.  This water could be used for evaporative cooling. If a name of a [WaterUse:Storage](#waterusestorage) object is used here, then the unit can obtain its water from that tank.  This field is optional.

### Field: Collection Outlet Water Storage Tank Name

This field is used to describe where the coil sends water it has collected if is to be stored in a tank.  This water could be collected from coil condensate.  If a name of a [WaterUse:Storage](#waterusestorage) object is used here, then the unit can send water to that tank.  This field is optional.

### Field: Ambient Zone Name

This field is used to connect ancillary losses from the coil to a thermal zone.  The coil may have inefficiencies, leaks, or other non-ideal operation that results in some untended impact on the space surrounding the unit.  These "skin losses" can be assigned to a thermal zone and appear as internal gains for the zone named here.  For example, this would be the mechanical room where the coil is located, not necessarily one of the zone served by the air handler.

An example of this input object follows.

~~~~~~~~~~~~~~~~~~~~

    Coil:UserDefined,
       Main Cooling Coil 1, !- Name
       Main Cooling Coil Model Program Manager , !- Overall Model Simulation Program Calling Manager Name
       Main Cooling Coil Init Program Manager , !- Model Setup and Sizing Program Calling Manager Name
       1 , !- Number of Air Connections
       Mixed Air Node 1 , !- Air Connection 1 Inlet Node Name
       Main Cooling Coil 1 Outlet Node , !- Air Connection 1 Outlet Node Name
       , !- Air Connection 2 Inlet Node Name
       , !- Air Connection 2 Outlet Node Name
       Yes , !- Plant Connection is Used
       Main Cooling Coil 1 Water Inlet Node, !- Plant Connection Inlet Node Name
       Main Cooling Coil 1 Water Outlet Node; !- Plant Connection Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

## PlantComponent:UserDefined

This object is used to define a generic plant component for custom component models.  This object can connect to up to four different plant loops.  There is an optional air connection that can be used as an air-based source or heat rejection connection.  Water storage tanks can be connected for supply or collection.  An ambient zone can be connected for skin losses to be treated as separate internal gains.

The PlantComponent:UserDefined object appears directly on the [Branch](#branch) object used to describe the plant.

### Field: Name

This field defines a unique user-assigned name for an instance of a custom plant component.  Any reference to this component by another object will use this name.

### Field: Main Model Program Calling Manager Name

This field specifies the Erl programs that are used to model the plant component any time that the device is called to simulate.  This field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere.  The program manager referenced here should include all the Erl programs that are to be run each time the component is simulated and use the calling point called UserDefinedComponentModel.

### Field: Number of Plant Loop Connections

This fields defines the number of different plant loop connections that the component will use.  Up to four separate loops can be connected if desired.  This field is required.

### FeildSet: {inlet node name, outlet node name, loading mode, flow request mode, initialization program calling manager, simulation program calling manager.}

Each of the plant loop connections used is defined by a set of 6 input fields that contain and inlet node, and outlet node, a loading mode, a flow request mode, a program calling manager for setup and sizing, and a program calling manager for model calculations that should run when this loop is being simulated.

### Field: Plant Connection X Inlet Node Name

This field defines the name of a plant system node from which the component can draw fluid.  This is the inlet to the component model from the "x" plant loop that is connected to the component.

### Field: Plant Connection X Outlet Node Name

This field defines the name of a plant system node to which the component can send fluid.  This is the outlet from the component model to the first plant loop that is connected to the component.

### Field: Plant Connection X Loading Mode

This field defines the nature of the plant component with respect to how it affects the loads experienced by the "X" plant loop connection. One of the following choices must be selected depending on the purpose of the component model and this particular plant loop connection.

- **DemandsLoad**. This type of loading is used for plant connections that place a load on the loop.  
- **MeetsLoadWithPassiveCapacity**.  This type of loading is used for plant connections where the component has some capacity to meet loads but it is not really of the type that could be actively controlled.  
- **MeetsLoadWithNominalCapacity**.  This type of loading is used for plant connections where the component has controllable capacity to meet loads and no outlet temperature restrictions.  
- **MeetsLoadWithNominalCapacityLowOutLimit**.  This type of loading is used for plant connections where the component has controllable capacity to meet loads but with a lower limit on the fluid temperature at the outlet node.  
- **MeetsLoadWithNominalCapacityHiOutLimit**.  This type of loading is used for plant connections where the component has controllable capacity to meet loads but with an upper limit on the fluid temperature at the outlet node.  

### Field: Plant Connection X Loop Flow Request Mode

This field defines the nature of the plant component with respect to how it affects the overall flow rate for the loop. One of the following three choices must be made depending on the nature of the plant component and this particular loop connection.

- **NeedsFlowIfLoopOn**.  Devices with this flow request mode will contribute to the overall loop flow rate but will not initiate flow themselves.  
- **NeedsFlowAndTurnsLoopOn**.  Devices with this flow request mode will contribute to the overall loop flow rate and initiate flow.  
- **ReceivesWhateverFlowAvailable**.  Devices with this flow request mode will not contribute to the overall loop flow rate and do not initiate flow themselves.

### Field: Plant Connection X Initialization Program Calling Manager Name

This field specifies the Erl programs that are used to model the plant component and should execute when the component is called to simulate by this particular loop connection.  This field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere.  The program manager referenced here should include all the Erl programs that are to be run for this "X" plant loop connection and use the calling point called UserDefinedComponentModel.

### Field: Plant Connection X Simulation Program Calling Manager Name

This field specifies the Erl programs that are used to initialize and size the plant component with regard to this particular plant connection.  This field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere and uses the calling point called UserDefinedComponentModel.  The program manager referenced here should include all the Erl programs that are needed to do any initial setup that should occur before the main modeling programs are run.  These include calculating and setting values for things that do not vary over time such as component sizes, control parameters, modeling constants etc.  The user defined component will trigger this calling manager during the intitial setup routines for plant systems.

### Field: Air Connection Inlet Node Name

This field defines the name of the HVAC system node from which the plant component draws its inlet air for its air loop connection.  This field is optional.

### Field: Air Connection Outlet Node Name

This field defines the name of the HVAC system node to which the plant component sends its outlet air for its air loop connection.  This field is optional.

### Field: Supply Inlet Water Storage Tank Name

This field is used to describe where the plant component obtains water if it is to be connected to a water storage tank.  This water could be used for evaporative cooling. If a name of a [WaterUse:Storage](#waterusestorage) object is used here, then the component can obtain its water from that tank.  This field is optional.

### Field: Collection Outlet Water Storage Tank Name

This field is used to describe where the plant component sends water it has collected if is to be stored in a tank.  This water could be collected from coil condensate.  If a name of a [WaterUse:Storage](#waterusestorage) object is used here, then the unit can send water to that tank.  This field is optional.

### Field: Ambient Zone Name

This field is used to connect ancillary losses from the plant component to a thermal zone.  The component may have inefficiencies, leaks, or other non-ideal operation that results in some untended impact on the space surrounding the device.  These "skin losses" can be assigned to a thermal zone and appear as internal gains for the zone named here.  For example, this would be the mechanical room where the plant component is located.

An example of this input object follows:

~~~~~~~~~~~~~~~~~~~~

    PlantComponent:UserDefined,
       Central Chiller,  !- Name
       , !- Main Model Program Calling Manager Name
       1 ,  !- Number of Plant Loop Connections
       Central Chiller Inlet Node,   !- Plant Connection 1 Inlet Node Name
       Central Chiller Outlet Node , !- Plant Connection 1 Outlet Node Name
       MeetsLoadWithNominalCapacityLowOutLimit, !- Plant Connection 1 Loading Mode
       NeedsFlowIfLoopOn , !- Plant Connection 1 Loop Flow Request Mode
       Chiller model init programs, !- Plant Connection 1 Initialization Program Calling Manager Name
       Chiller model simulation programs ,!- Plant Connection 1 Simulation Program Calling Manager Name
       , !- Plant Connection 2 Inlet Node Name
       , !- Plant Connection 2 Outlet Node Name
       , !- Plant Connection 2 Loading Mode
       , !- Plant Connection 2 Loop Flow Request Mode
       , !- Plant Connection 2 Initialization Program Calling Manager Names
       , !- Plant Connection 2 Simulation Program Calling Manager Name
       , !- Plant Connection 3 Inlet Node Name
       , !- Plant Connection 3 Outlet Node Name
       , !- Plant Connection 3 Loading Mode
       , !- Plant Connection 3 Loop Flow Request Mode
       , !- Plant Connection 3 Initialization Program Calling Manager Name
       , !- Plant Connection 3 Simulation Program Calling Manager Name
       , !- Plant Connection 4 Inlet Node Name
       , !- Plant Connection 4 Outlet Node Name
       , !- Plant Connection 4 Loading Mode
       , !- Plant Connection 4 Loop Flow Request Mode
       , !- Plant Connection 4 Initialization Program Calling Manager Name
       , !- Plant Connection 4 Simulation Program Calling Manager Name
       Central Chiller Condenser Inlet Node, !- Air Connection Inlet Node Name
       Central Chiller Condenser Outlet Node, !- Air Connection Outlet Node Name
       , !- Supply Inlet Water Storage Tank Name
       , !- Collection Outlet Water Storage Tank Name
       ; !- Ambient Zone Name
~~~~~~~~~~~~~~~~~~~~

## PlantEquipmentOperation:UserDefined

This object is used to define a generic plant equipment operation scheme.  The [PlantEquipmentOperation:UserDefined](#plantequipmentoperationuserdefined) object appears directly in either a [PlantEquipmentOperationSchemes](#plantequipmentoperationschemes) or a [CondenserEquipmentOperationSchemes](#condenserequipmentoperationschemes) object.  The user can configure exactly how supply side equipment is dispatched to meet loads by writing EnergyPlus Runtime Language programs and using the EMS features.  There are no output variables specifically associated with this object but the outcome of load distribution actuators can be examined using the output variable called Plant Component Distributed Demand Rate.

### Inputs

#### Field: Name

This field specifies the unique user-assigned name of the custom operation scheme.

#### Field: Main Model Program Calling Manager Name

This field specifies the Erl programs that are used to model the custom plant operation scheme.  The field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere.  The program manager referenced here should include all the Erl programs that are to be run each time the operation scheme is simulated and use the calling point called UserDefinedComponentModel.

#### Field: Initialization Program Calling Manager Name

This field specifies the Erl programs that are used to initialize and do an one-time setup needed for the custom operation scheme. This field should contain the name of an [EnergyManagementSystem:ProgramCallingManager](#energymanagementsystemprogramcallingmanager) that is defined elsewhere and uses the calling point called UserDefinedComponentModel. The program manager referenced here should include all the Erl programs that are needed to do any initial setup that should occur before the main programs are run. These include calculating and setting values for things that do not vary over time such as component capacities, control range parameters, various constants etc.  The user defined operation scheme will trigger this calling manager at the beginning of each new environment period.

#### Field Set: (Equipment Object Type, Equipment Name)

Up to ten pairs of fields can be used to define the equipment governed by this custom operation scheme.

#### Field: Equipment <#> Object Type

This field specifies the type of equipment, or object name, for each machine to be controlled by the scheme.

#### Field: Equipment <#> Name

This field specifies the user selected name of the equipment to be controlled by the scheme.

The following example shows control of a chilled water plant with two chillers that have different sizes and load dispatch is scaled so that each chiller runs at the same part load ratio.

~~~~~~~~~~~~~~~~~~~~

    PlantEquipmentOperation:UserDefined,
      CoolSys1 Operation Scheme,  !- Name
      Cooling_dispatch , !-  Main Model Program Calling Manager Name
      Init_Chiller_Capacity, !-  Initialization Program Calling Manager Name
      Chiller:Electric:ReformulatedEIR,  !- Equipment 1 Object Type
      CoolSys1 Chiller 1,      !- Equipment 1 Name
      Chiller:Electric:ReformulatedEIR,  !- Equipment 2 Object Type
      CoolSys1 Chiller 2;      !- Equipment 2 Name

    EnergyManagementSystem:ProgramCallingManager,
       Init_Chiller_Capacity,
       UserDefinedComponentModel,
       Init_Chiller_Capacity_Values;

    EnergyManagementSystem:Program,
       Init_Chiller_Capacity_Values,
       Set totChilCap = Chil1_Cap + Chil2_Cap;

    EnergyManagementSystem:GlobalVariable,
       totChilCap;

    EnergyManagementSystem:ProgramCallingManager,
       Cooling_dispatch,
       UserDefinedComponentModel,
       Cooling_dispatch_Values;

    EnergyManagementSystem:Program,
       Cooling_dispatch_Values,
       IF CoolSys1_LoopDmnd < 0.0,
         Set UniformPLR  = CoolSys1_LoopDmnd / totChilCap,
         Set UniformPLR  =  @min UniformPLR 1.0,
         SET Chil1_Disptch = UniformPLR*Chil1_Cap,
         SET Chil2_Disptch = UniformPLR*Chil2_Cap,
       ELSE,
         SET Chil1_Disptch = 0.0 ,
         SET Chil2_Disptch = 0.0 ,
       ENDIF;

    EnergyManagementSystem:InternalVariable, CoolSys1_LoopDmnd,CoolSys1 Operation Scheme,Supply Side Current Demand Rate; !,[W]
    EnergyManagementSystem:InternalVariable, Chil1_Cap, CoolSys1 Chiller 1, Chiller Nominal Capacity;
    EnergyManagementSystem:InternalVariable, Chil2_Cap, CoolSys1 Chiller 2, Chiller Nominal Capacity;

    EnergyManagementSystem:Actuator, Chil1_Disptch,CoolSys1 Operation Scheme:CoolSys1 Chiller 1,Plant Equipment Operation,Distributed Load Rate; ! [W]
    EnergyManagementSystem:Actuator, Chil2_Disptch,CoolSys1 Operation Scheme:CoolSys1 Chiller 2,Plant Equipment Operation,Distributed Load Rate; ! [W]
~~~~~~~~~~~~~~~~~~~~