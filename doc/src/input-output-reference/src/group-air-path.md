# Group -- Air Path

## AirLoopHVAC:SupplyPath, AirLoopHVAC:ReturnPath

The zone supply and return air paths are used to describe the configuration of the flow path that the supply air takes in moving from the main air handler to the zone terminal units and that the return air takes on leaving the zones and returning to the central return air duct. The air paths consist of a group of components – AirLoopHVAC:ZoneSplitters and AirLoopHVAC:SupplyPlenums for the supply path and AirLoopHVAC:ReturnPlenums and AirLoopHVAC:ZoneMixers for the return path. The connectivity of the paths is established by the components' node connections. There are no branches defined in the zone air path constructs.

For a typical single duct air system there will be one [AirLoopHVAC:SupplyPath](#airloophvacsupplypath) and one [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath). A typical dual duct system would have 2 AirLoopHVAC:SupplyPaths – one for the hot air and one for the cold air – and 1 [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath).

The [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) component provides the capability to have supply air distributed to the conditioned zones through one or more plenum zones such as would occur in an underfloor air distribution system (UFAD). The [AirLoopHVAC:ReturnPlenum](#airloophvacreturnplenum) component allows the program to model return air plenums. Of course plenums are always optional. The simplest [AirLoopHVAC:SupplyPath](#airloophvacsupplypath) would consist of only an [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter); similarly the simplest return air path consists of a single [AirLoopHVAC:ZoneMixer](#airloophvaczonemixer). A UFAD system supply air path might consist of a single [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter) connecting to multiple AirLoopHVAC:SupplyPlenums which in turn could each supply one or more conditioned zones. A system with return plenums would have an [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath) in which the conditioned zones connect to the return plenum inlets and the return plenum's outlets connect to a single [AirLoopHVAC:ZoneMixer](#airloophvaczonemixer).

Both the zone supply and return plenums are solved in the heat balance as thermal zones which couple through heat conduction with adjacent zones. Since they are solved as thermal zones the solution scheme involves the zone predictor-corrector technique which will lag the temperature at the system timestep in every zone. See Summary of Predictor-Corrector Procedure in the EnergyPlus Engineering Reference.

*Below are the descriptions of the zone supply and return path statements. Note that each statement type has an identifying name, a single inlet/outlet node name, and a list of component type/name pairs. For a single zone being served by an air loop, the inlet to its air distribution unit and the zone return air outlet can be the zone equipment inlet and outlet nodes directly, thus eliminating the need to specify an [AirLoopHVAC:SupplyPath](#airloophvacsupplypath) and an AirLoopHVAC:ReturnPath.*

## AirLoopHVAC:SupplyPath 

The [AirLoopHVAC:SupplyPath](#airloophvacsupplypath) lists one or more [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter) and [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) components comprising the path. The components are listed in flow order: upstream to downstream and may be in series, parallel, or both. Connectivity is established by means of the individual component inlet and outlet nodes.

### Inputs

#### Field: Name

Unique name to identify the supply air path..

#### Field: Supply Air Path Inlet Node Name

The name of an inlet node for the zone equipment half of the air loop. This should be one of the nodes named in the [AirLoopHVAC](#airloophvac) field: Demand Side Inlet Node Names.

#### Field Set Component Type and Name

The remaining fields are sets of two repeated times: a component type and a name.  These pairs of fields define the components on the supply air path.

#### Field: Component <#> Object Type

Start of the component list for the [AirLoopHVAC:SupplyPath](#airloophvacsupplypath). This field should contain either *AirLoopHVAC:SupplyPlenum* or *AirLoopHVAC:ZoneSplitter*.

#### Field: Component <#> Name

Unique name of the [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) or [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter) component.

## AirLoopHVAC:ReturnPath

The [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath) lists one or more [AirLoopHVAC:ZoneMixer](#airloophvaczonemixer) and [AirLoopHVAC:ReturnPlenum](#airloophvacreturnplenum) components comprising the path. The components are listed in flow order: upstream to downstream and may be in series, parallel, or both. Connectivity is established by means of the individual component inlet and outlet nodes. At this time there can only be 1 [AirLoopHVAC:ZoneMixer](#airloophvaczonemixer) in an [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath).

### Inputs

#### Field: Name

Unique name to identify the return air path.

#### Field: Return Air Path Outlet Node Name

The name of the node that is the outlet to the air loop from the [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath). This should be the same node named in the [AirLoopHVAC](#airloophvac) field: Demand Side Outlet Node Name.

#### Field Set Component Type and Name

The remaining fields are sets of two repeated times: a component type and a name.  These pairs of fields define the components on the return air path.

#### Field: Component <#> Object Type

Start of the component list for the [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath). This field should contain either *AirLoopHVAC:ReturnPlenum* or *AirLoopHVAC:ZoneMixer*.

#### Field: Component <#> Name

Unique name of the [AirLoopHVAC:ReturnPlenum](#airloophvacreturnplenum) or [AirLoopHVAC:ZoneMixer](#airloophvaczonemixer).

Example inputs are shown below

~~~~~~~~~~~~~~~~~~~~

     AirLoopHVAC:SupplyPath,
        SupplyAirPath 1,            !- Name
        Zone Equip In Node,         !- Supply Air Path Inlet Node Name
        AirLoopHVAC:SupplyPlenum,   !- Component 1 Object Type
        Supply-Plenum-1,            !- Component 1 Name
        AirLoopHVAC:ZoneSplitter,   !- Component 2 Object Type
        Zone Supply Air Splitter 1; !- Component 2 Name

    AirLoopHVAC:ReturnPath,
        ReturnAirPath1,             !- Name
        PLENUM-1 Out Node,          !- Return Air Path Outlet Node Name
        Zone Return Plenum,         !- Component 1 Object Type
        Return-Plenum-1;            !- Component 1 Name
~~~~~~~~~~~~~~~~~~~~

## AirLoopHVAC:ReturnPlenum

The simple building shown in the figure below consists of a controlled zone and a return plenum zone that is used for the return air. This simple configuration contains a ground floor controlled zone with a window and a return plenum zone which handles the roof load. The return air plenum is a special type of component since there is both a heat balance connection and building description and a system airflow connection that transfers the airflow directly from the controlled zone to the return plenum zone in the system air simulation. The input described in this section is mainly just for the system airflow connections in the HVAC algorithms. In addition the return plenum zone must be fully input as a zone object for the heat balance calculation. The zone description needs to contain the wall descriptions - the interzone ceiling and floor, and for this simple case the roof. All the other attributes of a zone that can be specified in the building description can be used with a return plenum zone if necessary: scheduled loads, windows, etc.

![Illustration of Zone Return Plenum](media/illustration-of-zone-return-plenum.png)


After all the building zone attributes are specified, the [AirLoopHVAC:ReturnPlenum](#airloophvacreturnplenum) object is specified and included as one of the named components in an [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath).

### Inputs

#### Field: Name

A unique identifying name identifying the [AirLoopHVAC:ReturnPlenum](#airloophvacreturnplenum) system component.

#### Field: Zone Name

The zone name specified in the heat balance portion of the input. This piece of input connects the zone return plenum system component to the heat balance data structure.

#### Field: Zone Node Name

The name of the zone node. The zone node name connects the heat balance data structure to the system airflow data structure through this node.  This node name is defined by the user in the field "[Zone](#zone) Air Node Name" in the [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) object for the zone named in the previous field.

#### Field: Outlet Node Name

The name of the return plenum outlet node. For the zone return plenum there is only one outlet node that can be specified.

#### Field: Induced Air Outlet Node or NodeList Name

The name of an induced air outlet node or the name of a [NodeList](#nodelist) of such nodes. These nodes are to be used as the secondary inlet air nodes of powered induction terminal units (See **AirTerminal:SingleDuct:SeriesPIU:Reheat** and **AirTerminal:SingleDuct:ParallelPIU:Reheat**).

#### Field: Inlet <#> Node Name

The name of a plenum inlet node. There is no limit to the number of inlet nodes, and there may be no duplicate inlet node names. (Note that some EnergyPlus editing tools may allow only 500 inlet node names, but this may be increased by extending the object in the Energy+.idd file.)

An IDF example of a zone return plenum component specification:

~~~~~~~~~~~~~~~~~~~~

      AirLoopHVAC:ReturnPlenum,
        Return-Plenum-1,            !- Name
        PLENUM-1,                   !- Zone Name
        PLENUM-1 Node,              !- Zone Node Name
        PLENUM-1 Out Node,          !- Outlet Node Name
        PLENUM-1 Induced Air Nodes, !- Induced Air Outlet Node or NodeList Name
        SPACE1-1 Out Node,          !- Inlet 1 Node Name
        SPACE2-1 Out Node,          !- Inlet 2 Node Name
        SPACE3-1 Out Node,          !- Inlet 3 Node Name
        SPACE4-1 Out Node,          !- Inlet 4 Node Name
        SPACE5-1 Out Node;          !- Inlet 5 Node Name

      NodeList,
        PLENUM-1 Induced Air Nodes,    !- Name
        SPACE1-1 ATU Sec Node,         !- Node 1 Name
        SPACE2-1 ATU Sec Node,         !- Node 2 Name
        SPACE3-1 ATU Sec Node,         !- Node 3 Name
        SPACE4-1 ATU Sec Node;         !- Node 4 Name
~~~~~~~~~~~~~~~~~~~~

Below is an example of the [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath) for the simple case shown above.

~~~~~~~~~~~~~~~~~~~~

      AirLoopHVAC:ReturnPath,
        ReturnAirPath1,          !- Name
        PLENUM-1 Out Node,       !- Return Air Path Outlet Node Name
        AirLoopHVAC:ReturnPlenum,!- Component 1 Object Type
        Return-Plenum-1;         !- Component 1 Name
~~~~~~~~~~~~~~~~~~~~

## AirLoopHVAC:SupplyPlenum

The building shown in the figure below consists of three controlled zones served by an [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) beneath the floor.

![Illustration of AirLoopHVAC:SupplyPlenum](media/illustration-of-airloophvac-supplyplenum.png)


The [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) has a heat balance connection and building description, and a system airflow connection which transfers the airflow from the supply plenum zone to the controlled zones in the system simulation. The input described in this section is mainly just for the system airflow connections in the HVAC algorithms. In addition the supply plenum zone must be fully input in the building zone description. The supply plenum zone description needs to contain the wall descriptions, the interzone ceiling and floor, and the ground connection. All the other attributes of a zone that can be specified in the building description can be utilized with a supply plenum zone if necessary, i.e. scheduled loads, windows, etc.

After all the building zone attributes are specified, the [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) object is specified and included as one of the named components in an [AirLoopHVAC:SupplyPath](#airloophvacsupplypath).

### Inputs

#### Field: Name

A unique identifying name identifying the [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) system component.

#### Field: Zone Name

The zone name specified in the heat balance portion of the input. This piece of input connects the [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) system component to the heat balance data structure.

#### Field: Zone Node Name

The name of the zone node. The zone node name connects the heat balance data structure to the system airflow data structure through this node.  This node name is defined by the user in the field "[Zone](#zone) Air Node Name" in the [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) object for the zone named in the previous field.

#### Field: Inlet Node Name

The name of the inlet node to the supply plenum. The [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) component can send air to many outlets, but there is only one inlet.

#### Field: Outlet <#> Node Name

The name of a plenum outlet node. There is no limit to the number of outlet nodes, and there may be no duplicate outlet node names. (Note that some EnergyPlus editing tools may allow only 500 outlet node names, but this may be increased by extending the object in the Energy+.idd file.)

An IDF example of an [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) component specification:

~~~~~~~~~~~~~~~~~~~~

     AirLoopHVAC:SupplyPlenum,
        Supply Plenum 1,             ! Name
        SUPPLY PLENUM ZONE,          ! Zone Name
        Supply Plenum 1 Zone Node,   ! Zone Node Name
        Zone Equipment Inlet Node,   ! Inlet Node Name
        Supply Plenum 1 Outlet Node; ! Outlet 1 Node Name
~~~~~~~~~~~~~~~~~~~~

Below is an example of the [AirLoopHVAC:SupplyPath](#airloophvacsupplypath) for the building shown above.

~~~~~~~~~~~~~~~~~~~~

     AirLoopHVAC:SupplyPath,
        TermReheatSupplyPath,        ! Name
        Zone Equipment Inlet Node,   ! Supply Air Path Inlet Node Name
        AirLoopHVAC:SupplyPlenum,    ! Component 1 Object Type
        Supply Plenum 1,             ! Component 1 Name
        AirLoopHVAC:ZoneSplitter,    ! Component 2 Object Type
        Zone Supply Air Splitter;    ! Component 2 Name
~~~~~~~~~~~~~~~~~~~~

## AirLoopHVAC:ZoneSplitter 

The [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter) is a component that splits airflow from a single inlet to multiple outlets. This component must be referenced in an [AirLoopHVAC:SupplyPath](#airloophvacsupplypath) object. The input contains only node connection information.

### Inputs

#### Field: Name 

Unique name for this zone splitter.

#### Field: Inlet Node Name

The name of the single inlet node going into the splitter.

#### Field: Outlet <#> Node Name 

The name of a splitter outlet node. There is no limit to the number of outlet nodes, and there may be no duplicate outlet node names. (Note that some EnergyPlus editing tools may allow only 500 outlet node names, but this may be increased by extending the object in the Energy+.idd file.)

An example is shown below.

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:ZoneSplitter,
          Zone Supply Air Splitter,   ! Name
          Zone Equipment Inlet Node,  ! Inlet Node Name
          Zone 1 Damper Inlet Node,   ! Outlet 1 Node Name
          Zone 2 Damper Inlet Node,   ! Outlet 2 Node Name
          Zone 3 Damper Inlet Node;   ! Outlet 3 Node Name
~~~~~~~~~~~~~~~~~~~~

## AirLoopHVAC:ZoneMixer

The [AirLoopHVAC:ZoneMixer](#airloophvaczonemixer) takes the air from multiple inlets and mixes the streams together to be sent out of a single outlet node. This is a simple process of summing flows and averaging the air properties. This compoment may be part of an air loop return path or part of an induction terminal unit. When used as a return air mixer, this component must be listed in an [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath) object, and the inlets may be zone return air nodes or [AirLoopHVAC:ReturnPlenum](#airloophvacreturnplenum) outlet nodes. When used in an induction terminal unit (ref. [AirTerminal:SingleDuct:SeriesPIU:Reheat](#airterminalsingleductseriespiureheat), [AirTerminal:SingleDuct:ParallelPIU:Reheat](#airterminalsingleductparallelpiureheat), and [AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction](#airterminalsingleductconstantvolumefourpipeinduction)), one inlet node is the supply (primary) air inlet from the central air handler, and the second inlet node is the induced (secondary) air inlet. These node names should match equivalent node names in the terminal unit.

### Inputs

#### Field: Name 

Unique name for this zone mixer.

#### Field: Outlet Node Name

The name of the single outlet node leaving the mixer.

#### Field: Inlet <#> Node Name 

The name of a mixer inlet node. There is no limit to the number of inlet nodes, and there may be no duplicate inlet node names. (Note that some EnergyPlus editing tools may allow only 500 inlet node names, but this may be increased by extending the object in the Energy+.idd file.)

An input example is shown below.

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:ZoneMixer,
             Zone Return Air Mixer,        ! Name
             Return Air Mixer Outlet Node, ! Outlet Node Name
             Zone 1 Outlet Node,           ! Inlet 1 Node Name
             Zone 2 Outlet Node,           ! Inlet 2 Node Name
             Zone 3 Outlet Node;           ! Inlet 3 Node Name
~~~~~~~~~~~~~~~~~~~~