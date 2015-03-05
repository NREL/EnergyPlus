# Group – Node-Branch Management

**Nodes**, **Branch** **Lists** and **Branches** define the topography of the HVAC connections.

## NodeList

For convenience, the [NodeList](#nodelist) object can be used to identify all the nodes for a particular use. A node list is not always required. The advantage of listing all the nodes in this manner is that input fields in other objects that ask for a node name can refer to either a single node or a list of nodes using this object. .

There are other lines of input syntax that require the use of a [NodeList](#nodelist) as part of the input. These syntax items include: OutdoorAir:[NodeList](#nodelist) and the SetpointManager objects. In these two cases, the use of a [NodeList](#nodelist) statement is mandatory if there is more than one node in this particular "list". For example, if the outside air only has a single inlet node, then the name of that node can replace the node list name in the input syntax. As a result of this mandatory specification of node lists for certain types of input, nodes can, obviously, and may be members of more than one list.

### Inputs

#### Field: Name

This alpha designation must be unique among all the Node List Names. It is used as reference in various objects (e.g. [OutdoorAir:NodeList](#outdoorairnodelist)).

#### Field: Node <#> Name  (up to 25 allowed)

Each unique Node identifier creates a Node structure with various attributes depending on the type of node. Typical values include Temperature, Mass Flow Rate, and Temperature Set Point. The alpha designators are associated with a particular node.

Though each [NodeList](#nodelist) is limited to 25 nodes, several node lists may be entered if you choose to identify all the nodes up front. For example:

~~~~~~~~~~~~~~~~~~~~

    NodeList, Nodes1-25,
    NODE_1,NODE_2,NODE_3,NODE_4,NODE_5,NODE_6,NODE_7,NODE_8,NODE_9,NODE_10,
    NODE_11,NODE_12,NODE_13,NODE_14,NODE_15,NODE_16,NODE_17,NODE_18,NODE_19,NODE_20,
    NODE_21,NODE_22,NODE_23,NODE_24,NODE_25;
    NodeList, Nodes26-49,NODE_26,NODE_27,NODE_28,NODE_29,NODE_30,
    NODE_31,NODE_32,NODE_33,NODE_34,NODE_35,NODE_36,NODE_37,NODE_38,NODE_39,NODE_40,
    NODE_41,NODE_42,NODE_43,NODE_44,NODE_45,NODE_46,NODE_47,NODE_48,NODE_49;
~~~~~~~~~~~~~~~~~~~~

### Outputs

A simple detail of Nodes and numbers assigned appears in the **eplusout.bnd** file:

~~~~~~~~~~~~~~~~~~~~

    ! <Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>
     Node,1,OUTSIDE AIR INLET NODE 1,Air,2
     Node,2,VAV SYS 1 INLET NODE,Air,3
     Node,3,MIXED AIR NODE 1,Air,5
     Node,4,MAIN COOLING COIL 1 OUTLET NODE,Air,3
     Node,5,MAIN HEATING COIL 1 OUTLET NODE,Air,4
     Node,6,VAV SYS 1 OUTLET NODE,Air,7…
~~~~~~~~~~~~~~~~~~~~

In addition, properties of each node is available during the simulation:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,System Node Temperature [C]
    HVAC,Average,System Node Last Timestep Temperature [C]
    HVAC,Average,System Node Mass Flow Rate [kg/s]
    HVAC,Average,System Node Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,System Node Setpoint Temperature [C]
    HVAC,Average,System Node Setpoint High Temperature [C]
    HVAC,Average,System Node Setpoint Low Temperature [C]
    HVAC,Average,System Node Setpoint Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,System Node Setpoint Minimum Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,System Node Setpoint Maximum Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,System Node Relative Humidity [%]
    HVAC,Average,System Node Pressure [Pa]
    HVAC,Average,System Node Standard Density Volume Flow Rate [m3/s]
    HVAC,Average,System Node Enthalpy [J/kg]
    HVAC,Average,System Node Last Timestep Enthalpy [J/kg]
    HVAC,Average,System Node Wetbulb Temperature [C]
    HVAC,Average,System Node Dewpoint Temperature [C]
    HVAC,Average,System Node Quality []
    HVAC,Average,System Node Height [m]

    The following node variable is also available for system nodes that are for "air":
    HVAC, Average, System Node Current Density Volume Flow Rate [m3/s]
    HVAC, Average, System Node Current Density [kg/m3]

    The following node variables are "advanced" and normally used for debugging unusual cases:
    HVAC,Average,System Node Minimum Temperature [C]
    HVAC,Average,System Node Maximum Temperature [C]
    HVAC,Average,System Node Minimum Limit Mass Flow Rate [kg/s]
    HVAC,Average,System Node Maximum Limit Mass Flow Rate [kg/s]
    HVAC,Average,System Node Minimum Available Mass Flow Rate [kg/s]
    HVAC,Average,System Node Maximum Available Mass Flow Rate [kg/s]
    HVAC,Averge,System Node Requested Mass Flow Rate [kg/s]
    HVAC,Average,System Node Setpoint Mass Flow Rate [kg/s]

    The following node variable reports node carbon dioxide concentration when carbon dioxide is simulated (ref. ZoneAirContaminantBalance):
    HVAC,Average,System Node CO2 Concentration [ppm]

    The following node variable reports node generic contaminant concentration when generic contaminant is simulated (ref. ZoneAirContaminantBalance):
    HVAC,Average,System Node Generic Air Contaminant Concentration [ppm]
~~~~~~~~~~~~~~~~~~~~

#### System Node Temperature [C]

The current temperature at a system node in degrees C. For air nodes, this is the dry-bulb temperature.

#### System Node Last Timestep Temperature [C]

The temperature at a system node for the previous timestep.

#### System Node Mass Flow Rate [kg/s]

The current mass flow rate at a system node in kg/s.

#### System Node Humidity Ratio [kgWater/kgDryAir]

The current humidity ratio at a system node in kg-water/kg-dry-air. Not applicable for liquid nodes.

#### System Node Setpoint Temperature [C]

The current setpoint temperature at a system node in degrees C. Some controllers and equipment types sense this single setpoint.

#### System Node Setpoint High Temperature [C]

The current upper setpoint temperature at a system node in degrees C. Some controllers and equipment types use dual setpoints at a single node.

#### System Node Setpoint Low Temperature [C]

The current upper setpoint temperature at a system node in degrees C. Some controllers and equipment types use dual setpoints at a single node.

#### System Node Setpoint Humidity Ratio [kgWater/kgDryAir]

The current humidity ratio setpoint at a system node in kg-water/kg-dry-air. Some controllers and equipment types sense this single setpoint. Not applicable for liquid nodes.

#### System Node Setpoint Minimum Humidity Ratio [kgWater/kgDryAir] 

The current minimum desired humidity ratio at a system node in kg-water/kg-dry-air. Some controllers and equipment types use the min/max values as dual setpoints at a single node. Not applicable for liquid nodes.

#### System Node Setpoint Maximum Humidity Ratio [kgWater/kgDryAir] 

The current maximum desired humidity ratio at a system node in kg-water/kg-dry-air. Some controllers and equipment types use the min/max values as dual setpoints at a single node. Not applicable for liquid nodes.

#### System Node Relative Humidity [%]

The current relative humidity (calculated from the node temperature, node humidity ratio and standard barometric pressure for the location). Not applicable for liquid nodes.

#### System Node Pressure [Pa]

The current pressure at a system node in Pa.

#### System Node Standard Density Volume Flow Rate [m^3^/s]

The current volume flow rate at a system node in m^3^/s.  This is report is calculated from the mass flow using standardized values for density that do not vary over time (except for steam which varies with quality).  For water nodes this density is determined at a temperature of 5.05ºC.  For air nodes this density is determined for dry air at the standard barometric pressure for the location's elevation, and a temperature of 20.0ºC.  For air nodes, also see the report "System Node Current Density Volume Flow Rate."

#### System Node Enthalpy [J/kg]

The current enthalpy at a system node in J/kg.

#### System Node Last Timestep Enthalpy [J/kg]

The enthalpy at the system node for the previous timestep in Joules per kilogram.

#### System Node Wetbulb Temperature [C]

The current wet-bulb temperature at a system node in degrees C. Not applicable for liquid nodes.

#### System Node Dewpoint Temperature [C]

The current dewpoint temperature at a system node in degrees C. It is calculated from the System Node Temperature, the System Node Humidity Ratio and the Site Outdoor Air Barometric Pressure. Not applicable for liquid nodes.

#### System Node Quality []

The current system node vapor fraction/percent {0.0-1.0}.

#### System Node Height [m]

The current system node height {m}. Only applicable to outdoor air nodes.

**Additional Node Reporting for Air Nodes**

The following system node variable is available for nodes with a fluid type of air.

#### System Node Current Density Volume Flow Rate [m^3^/s]

The current volume flow rate at a system node in m^3^/s based on the current density.  This report differs from the one called "System Node Standard Density Volume Flow Rate" in that it uses an air density calculated for the current moist air conditions rather than a standard density.

#### System Node Current Density [kg/m3]

The current air density at a system node in kg/m3.  This is the density used to calculate the volume flow rate above.  The density is calculated using current outdoor air barometric pressure and the drybulb temperature and humidity ratio at the air node.

**Advanced Node Reporting**

The following system node variables are available when you request "Diagnostics, DisplayAdvancedReportVariable;". They are normally used for debugging unusual system cases and may not be available for all systems. Also, their definitions may change based on the usage in the system.

#### System Node Minimum Temperature [C]

#### System Node Maximum Temperature [C]

These node variables represent the min/max of loops or branches.

#### System Node Minimum Limit Mass Flow Rate [kg/s]

#### System Node Maximum Limit Mass Flow Rate [kg/s]

These node variables hold the maximum possible and the minimum allowable flow rates for a particular component. As such, they represent the "hardware limit" on the flow rate for the component. By convention, these variables are stored at the component inlet node. Since components share their nodes (the outlet node of one component is the inlet node of the next component), the protocol must be strictly followed. These variables are set by each component based on the design flow limits of the equipment.

#### System Node Minimum Available Mass Flow Rate [kg/s]

#### System Node Maximum Available Mass Flow Rate [kg/s]

These node variables represent the *loop* or *branch* maximum and minimum flow rate for the current configuration of the loop on which the component resides. The central plant routines manage these variables dynamically to resolve flow rates through branches located inside splitters and mixers.

#### System Node Setpoint Mass Flow Rate [kg/s]

The current mass flow rate setpoint at a system node in kg/s. Some controllers and equipment types sense this single setpoint.

#### System Node Requested Mass Flow Rate [kg/s]

This output is the current mass flow rate request at a system node in kg/s.  Only used for plant simulations and nodes.  This is the mass flow rate "requested" by a component.  This is a record of what the component wanted which can be useful for diagnostics when the actual simulated flow is different than expected.  This value is also used to determine overall loop flow rates.

#### System Node CO2 Concentration [ppm]

The average carbon dioxide concentration, in parts per million (ppm), at a system node for the timestep being reported.

#### System Node Generic Air Contaminant Concentration [ppm]

The average generic contaminant ****concentration, in parts per million (ppm), at a system node for the timestep being reported.

## BranchList

A branch list is intended to define the branches or portions of a particular loop that are present on an individual plant or condenser loop. Thus, the [BranchList](#branchlist) syntax simply provides a list of branch names (see [Branch](#branch) syntax) that then refer to more detailed syntax in another part of the input file. The syntax for describing a list of branches is given below.

> Branches in [BranchList](#branchlist) objects should be listed in flow order: inlet branch, then parallel branches, then outlet branch. Branches (within a splitter/mixer) are simulated in the order listed.

### Inputs

#### Field: Name

A unique name to identify the branch list.

#### Field: Branch <#> Name

[Branch](#branch) name in the list.  As noted above, Branches in [BranchList](#branchlist) objects MUST be listed in flow order: inlet branch, then parallel branches, then outlet branch. Branches are simulated in the order listed.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    BranchList, Cooling Supply Side Branches,
         CW Pump Branch,
         Little Chiller Branch,
         Big Chiller Branch,
         Purchased Cooling Branch,
         Supply Bypass Branch,
        Cooling Supply Outlet;
~~~~~~~~~~~~~~~~~~~~

### Outputs

No output directly related to [Branch](#branch) Lists current is presented.

## Branch

Branches can be considered the mid-level grouping of items in the EnergyPlus HVAC loop scheme. Components are the lowest level of information. A collection of components in series forms a branch. Every branch must have at least one component, but it may have several components in series. The air loop also has "unitary" objects that are compound objects that wrap components and when they are used it is the unitary objects that appear on the [Branch](#branch) and not the child components.  The **[Branch](#branch)** defines the order that components appear on the branch. Moreover, a collection of branches and their connecting information form a loop. Therefore, a branch is a collection of components while a loop is a collection of branches. Thus, components are specified for particular HVAC loops by assigning them to branches and then connecting those branches together to form a loop.

For hydronic plant systems, the [Branch](#branch) object is used on both the demand side and the supply side of the plant loop.  For HVAC air systems, the [Branch](#branch) object is only used to describe the components on the supply side of an air loop. The demand side of the air loop does not use the [Branch](#branch) object.  The outside air system appears on a [Branch](#branch) object but it does not use a [Branch](#branch) object itself to describe the components that included in the outside air system.  So-called "ZoneHVAC:\*" equipment, HVAC equipment directly associated with a zone, do not use the [Branch](#branch) object.

It should be noted that each component also has at least two nodes associated with it—an inlet node and an outlet node. These nodes, by default, are also part of the branch to which their components are assigned.  Although it may appear redundant to list the node names in both the [Branch](#branch) and in the component object being referenced, this is needed because many components are connected to more than one loop at a time and the node names are needed here to distinguish which portion of the component is being attached to this particular branch and its loop.  It should be noted that each branch must have at least one component on it. A "null" branch such as one where the first component on the loop is in reality a splitter or mixer should be defined as a single Pipe component for plant, or a single [Duct](#duct) component for air systems.

### Inputs

#### Field: Name

This alpha field is the unique, user-defined name that allows the branch to be referenced by other elements such as the [PlantLoop](#plantloop) and [CondenserLoop](#condenserloop) statements.

#### Field: Maximum Flow Rate

This field is the maximum flow rate (m^3^/sec) allowed on a particular branch. This number will set an absolute maximum flow rate for all components on the loop. Even if components are defined with a higher flow rate, EnergyPlus will limit the flow on this particular branch to the value entered here by the user. Currently this is used in the Air Loop and is not a required field in the Plant and Condenser Loop. The example files for the Plant and Condenser Loops leave this field blank to simplify flow input.

#### Field: Pressure Drop Curve Name

This is an optional field used to specify pressure drop information for this particular branch. Currently this is only available in plant/condenser fluid loops: [PlantLoop](#plantloop) and [CondenserLoop](#condenserloop). Air loops require a full airflow network, so this field is ignored if found on an air loop. The value entered for this field must match a pressure curve object also found in the input. This curve can be of the type: [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Exponent](#curveexponent), or [Curve:Functional:PressureDrop](#curvefunctionalpressuredrop). In order to then perform a pressure simulation for the loop that this branch is located on, a flag must be set in the loop object. Reference the [PlantLoop](#plantloop) and [CondenserLoop](#condenserloop) field "Pressure Simulation Type" for allowable settings. See the Engineering Reference document for further information on pressure simulation in plant loops.

#### Field Set (Branch Specifications)

Following the two introductory items, there are groups of 5 repeated items: a type, a name, inlet and outlet node names, and a branch control type. All of these items refer to one particular component on the branch. The first component defined by these items will be the first component on this branch, etc. through the end of the list.  This object is extensible and can use as many field sets as needed.

#### Field: Component <#> Object Type

This field is the component object type (object class name). The component type refers to other elements within the input file and specifies what kind of component this is. The objects in the  Group – Plant Equipment section are valid here when the [Branch](#branch) is used in Plant specifications. Fans, coils, unitary systems, and Outside Air Systems are also valid component types for the [Branch](#branch) object. Pipes or Ducts are also valid for this field.  No splitter or mixer type objects are allowed.

#### Field: Component <#> Name

The component name is the unique, user-defined reference name to distinguish between potentially multiple occurrences of a particular component type in an input file. The set of component type and component name must be unique. (You can only have 1 Boiler object with name MYBOILER).

#### Field: Component <#> Inlet Node Name

This field references an inlet node name that will be found in the actual component specification.  The inlet node, and the outlet node specified in the next field, should correspond to component model's connections that are intended for the loop that this [Branch](#branch) is assigned to.  For example a water-cooled chiller will have two inlet nodes, one for chilled water return and one for condenser water return and so the chiller will appear on two [Branch](#branch) objects accordingly.

#### Field: Component <#> Outlet Node Name

This field references an outlet node name that will be found in the actual component specification.

#### Field: Component <#> Branch Control Type

This field is being deprecated and is no longer used or required. Prior to version 7 it was used by plant to resolve branch flow rates but now this is handled automatically by the program.  The field will be removed in a future version.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    Branch,
        Air Loop Main Branch,    !- Branch Name
        1.75,                    !- Maximum Branch Flow Rate {m3/s}
        ,                        !- Pressure Drop Curve Name
        AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass,  !- Comp1 Type
        GasHeat CBVAV System,    !- Comp1 Name
        Air Loop Inlet Node,     !- Comp1 Inlet Node Name
        Air Loop Outlet Node,    !- Comp1 Outlet Node Name
        ;                  !- Comp1 Branch Control Type
~~~~~~~~~~~~~~~~~~~~

And a more complex one:

~~~~~~~~~~~~~~~~~~~~

      BRANCH,
        Air Loop Main Branch,    !- Branch Name
        1.3,                     !- Maximum Branch Flow Rate {m3/s}
        ,                        !- Pressure Drop Curve Name
        FAN:SIMPLE:ConstVolume,  !- Comp1 Type
        Supply Fan 1,            !- Comp1 Name
        Air Loop Inlet Node,     !- Comp1 Inlet Node Name
        Cooling Coil Air Inlet Node,  !- Comp1 Outlet Node Name
        ,                  !- Comp1 Branch Control Type
        COIL:Water:DetailedFlatCooling,  !- Comp2 Type
        Detailed Cooling Coil,   !- Comp2 Name
        Cooling Coil Air Inlet Node,  !- Comp2 Inlet Node Name
        Air Loop Outlet Node,    !- Comp2 Outlet Node Name
        ;                 !- Comp2 Branch Control Type
~~~~~~~~~~~~~~~~~~~~

### Outputs

The components on a branch are in series and that list is in simulation and connection order. Each branch must obey the rules: the outlet for component 1 has to have the same node name as the inlet for component 2, and so forth.

Eplusout.err contains the test for "individual branch integrity":

~~~~~~~~~~~~~~~~~~~~

       ************* Testing Individual Branch Integrity
       ************* Testing Branch=AIR LOOP MAIN BRANCH
       ************* ....passed
       ************* Testing Branch=COOLING DEMAND INLET
       ************* ....passed
       ************* Testing Branch=COOLING COIL BRANCH
       ************* ....passed
       ************* Testing Branch=DEMAND BYPASS BRANCH
       ************* ....passed
       ************* Testing Branch=COOLING DEMAND OUTLET
       ************* ....passed
       ************* Testing Branch=COOLING SUPPLY OUTLET
       ************* ....passed
~~~~~~~~~~~~~~~~~~~~

The addition of pressure simulation for branches in fluid loops brings a new output at the branch level:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Plant Branch Pressure Difference [Pa]
~~~~~~~~~~~~~~~~~~~~

#### Plant Branch Pressure Difference [Pa]

This field allows the user to output the pressure drop of a particular branch on a plant loop or condenser loop. This output is only recognized if a valid pressure simulation is being performed. To do this, the user must specify a pressure simulation type as an input in the [PlantLoop](#plantloop) or [CondenserLoop](#condenserloop) object.

## ConnectorList

A connector list is a collection of either one or two connection devices ([Connector:Mixer](#connectorsplitter-connectormixer) or [Connector:Splitter](#connectorsplitter)). Due to the definition of a loop in EnergyPlus, there is a maximum of one mixer and one splitter allowed for each loop segment. This limitation still allows a tremendous amount of flexibility to the user and can also still be solved within a reasonable amount of time and without a complex solver routine. The connection list simply allows the specification of the types and names of these devices. This allows the main loop statements to have the flexibility to have multiple lists that can have one or more item.

### Inputs

#### Field: Name

This field is an identifying name that will be referenced to main loop statements using this connector list. The name must be unique among connector list names.

#### Field-Set: Connection Object Type and Identifying Name (up to 2 allowed)

Depending on the loop, either one or two of the connectors will be specified. If this particular loop only has a single connector (either [Connector:Mixer](#connectorsplitter-connectormixer) or [Connector:Splitter](#connectorsplitter)), then only the first two items (type of connection and identifying name of connector) are included. If both a mixer and splitter are present, the additional type of connection and identifying name must be included. Note that the order that the items appear is inconsequential—either the [Connector:Mixer](#connectorsplitter-connectormixer) or the [Connector:Splitter](#connectorsplitter) may appear first. This will not affect the simulation.

#### Field: Connector 1 Object Type

This field contains the connector type **Connector:Mixer** or **Connector:Splitter**.

#### Field: Connector 1 Name

This field is an identifying name for the connector. It will be used by other objects in the input file.

#### Field: Connector 2 Object Type (optional)

This field contains the connector type **Connector:Mixer** or **Connector:Splitter**.

#### Field: Connector 2 Name (optional)

This field is an identifying name for the connector. It will be used by other objects in the input file.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

            ConnectorList LIST, Cooling Supply Side Connectors,
                    Connector:Splitter, CW Loop Splitter,
                    Connector:Mixer, CW Loop Mixer;
~~~~~~~~~~~~~~~~~~~~

### Outputs

No output directly related to Connector Lists is currently presented.

## Pipe:Adiabatic

In reality, every component is connected to its closest neighbors via a pipe or a duct. At the current time, such detail is not needed in EnergyPlus. Thus, the [Pipe:Adiabatic](#pipeadiabatic) component is currently used more as a connection device (for branches that really do not have any components associated with them) or as a bypass than anything else. As such, its input is very simple and its algorithm is equally simple. The current algorithm for a pipe is simply to pass the inlet conditions to the outlet of the pipe component.

### Inputs

#### Field: Name

This alpha field is used as an identifying field for the pipe.

#### Field: Inlet Node Name

This alpha field is used as an identifying field for the pipe inlet node.

#### Field: Outlet Node Name

This alpha field is used as an identifying field for the pipe outlet node.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

            Pipe:Adiabatic,
                 Demand Side Inlet Pipe,
                 CW Demand Inlet Node,
                 CW Demand Entrance Pipe Outlet Node;
~~~~~~~~~~~~~~~~~~~~

### Outputs

There are no outputs directly related to Pipes.

## Pipe:Adiabatic:Steam

To connect the various components in a steam system regardless of it being the condensate or the steam side of the loop steam pipe needs to be used. In reality, every component is connected to its closest neighbors via a pipe or a duct. At the current time, such detail is not needed in EnergyPlus. Thus, the [Pipe:Adiabatic:Steam](#pipeadiabaticsteam) component is currently used more as a connection device (for branches that really do not have any components associated with them). As such, its input is very simple and its algorithm is equally simple. The current algorithm for a pipe is simply to pass the inlet conditions to the outlet of the pipe component.

### Inputs

#### Field: Name

This alpha field is used as an identifying field for the steam pipe.

#### Field: Inlet Node Name

This alpha field is used as an identifying field for the steam pipe inlet node.

#### Field: Outlet Node Name

This alpha field is used as an identifying field for the steampipe outlet node.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    Pipe:Adiabatic:Steam,
        Steam Demand 1 Steam Inlet Pipe,  !- PipeName
        Steam Demand 1 Steam Demand Inlet Node,  !- Inlet Node Name
        Steam Demand 1 Steam Demand Entrance Pipe Outlet Node;  !- Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

There are no outputs directly related to Steam Pipes.

## Pipe:Indoor

This object specifies inputs which are used to simulate the heat transfer from a plant loop pipe placed in a zone or when a user schedule is used to specify an environment.

The data definition for object is shown below.

### Inputs

#### Field: Name

This alpha field is used as an identifying field for the pipe.

#### Field: Construction Name

This alpha field references a 'wall' construction object that gives a layer-by-layer description of the pipe wall and its insulation.  The construction object follows standard conventions, describing material properties for each layer beginning with the outermost insulation layer and ending with the pipe wall layer.

#### Field: Fluid Inlet Node Name

This alpha field contains the name of the pipe fluid inlet node.

#### Field: Fluid Outlet Node Name

This alpha field contains the name of the pipe fluid outlet node.

#### Field: Environment Type

Environment type is the environment in which the pipe is placed. It can be either **[Zone](#zone)** or **Schedule**. If specified as [Zone](#zone), a zone name must be specified in the next field. If specified as Schedule, the Ambient Temperature [Zone](#zone) can be left blank, while a schedule must be specified for the temperature and air velocity.

#### Field: Ambient Temperature Zone Name

If **Zone** is specified as the environment type, this field is used to specify the name of the zone in which the pipe is located.  The zone temperature is used to calculate the heat transfer rate from the pipe.

#### Field: Ambient Temperature Schedule Name

If **Schedule** is specified as the environment type, this field is used to specify the name of the temperature schedule that gives the ambient air temperature surrounding the pipe.  This temperature is used as the outside boundary condition to calculate heat transfer from the pipe.

#### Field: Ambient Air Velocity Schedule Name

If **Schedule** is specified as the environment type, this field is used to specify the name of the velocity schedule that gives the air velocity near the pipe.  This velocity is used to calculate the convection heat transfer coefficient used in the pipe heat transfer calculation.

#### Field: Pipe Inside Diameter

This field is used to enter the inside diameter of the pipe in units of m. Pipe inside diameter must be a positive number.

#### Field: Pipe Length

This field is used to enter the length of the pipe in units of m. Pipe length must be a positive number.

An example of this object in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    Pipe:Indoor,
        Pipe Heat Transfer Towers,    !- name of outside panel heat exchanger
        Insulated Pipe,          !- Construction name
        Condenser Tower Outlet Node,  !- Comp1 Inlet Node Name
        HTPipe Outlet Node,  !- Comp1 Outlet Node Name
        Water,                   !- Fluid name
        Zone,        !- field Ambient Temperature Outside Air Node name
        0.05,                    !- Pipe Inside Diameter (thickness in construction data)
    100.0;                   !- pipe length
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Pipe Fluid Heat Transfer Rate [W]
    HVAC,Average, Pipe Ambient Heat Transfer Rate [W]
    HVAC,SUM, Pipe Fluid Heat Transfer Energy [J]
    HVAC,SUM, Pipe Ambient Heat Transfer Energy [J]
    HVAC,Average, Pipe Mass Flow Rate [kg/s]
    HVAC,Average, Pipe Inlet Temperature [C]
    HVAC,Average, Pipe Outlet Temperature [C]
    HVAC,Average, Pipe Volume Flow Rate [m3/s]
~~~~~~~~~~~~~~~~~~~~

#### Pipe Fluid Heat Transfer Rate [W]

The output provides the total amount of heat loss/gain in the fluid from pipe inlet to outlet.

#### Pipe Ambient Heat Transfer Rate [W]

The output provides the amount of heat loss/gain from pipe wall to the environment.

#### Pipe Fluid Heat Transfer Energy [J]

Total energy fluid has lost/gained between pipe inlet and outlet. It is metered on EnergyTransfer with an end use of Pipes.

#### Pipe Ambient Heat Transfer Energy [J]

Total energy lost from the external pipe surface.

#### Pipe Mass Flow Rate [kg/s]

Mass flow rate of the fluid in the pipe.

#### Pipe Inlet Temperature [C]

#### Pipe Outlet Temperature [C]

Temperature of fluid at entering and exiting of pipe.

#### Pipe Volume Flow Rate [m3/s]

Fluid volumetric flow rate, (mass flow rate / density)

## Pipe:Outdoor

This object specifies inputs which are used to simulate the heat transfer from a plant loop pipe placed in an outdoor environment.

### Inputs

#### Field: Name

This alpha field is used as an identifying field for the pipe.

#### Field: Construction Name

This alpha field references a 'wall' construction object that gives a layer-by-layer description of the pipe wall and its insulation.  The construction object follows standard conventions, describing material properties for each layer beginning with the outermost insulation layer and ending with the pipe wall layer.

#### Field: Fluid Inlet Node Name

This alpha field contains the name of the pipe fluid inlet node.

#### Field: Fluid Outlet Node Name

This alpha field contains the name of the pipe fluid outlet node.

#### Field: Ambient Temperature Outside Air Node Name

Ambient temperature outdoor air node is a node representing outdoor air with which pipe transfers heat. The node should be included in an outdoor air node list.

#### Field: Pipe Inside Diameter

This field is used to enter the inside diameter of the pipe in units of m. Pipe inside diameter must be a positive quantity.

#### Field: Pipe Length

This field is used to enter the length of the pipe in units of m. Pipe length must be a positive   quantity.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    Pipe:Outdoor,
        Pipe Heat Transfer Towers,    !- name of outside panel heat exchanger
        Insulated Pipe,          !- Construction name
        Condenser Tower Outlet Node, !- Comp1 Inlet Node Name
        HTPipe Outlet Node,  !- Comp1 Outlet Node Name
        Water,                   !- Fluid name
        PipeHeatTransfer Inlet Node,     !- field Ambient Temperature Outside Air Node name
        0.05,                    !- Pipe Inside Diameter (thickness given in construction data)
    100.0;                   !- pipe length
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Pipe Fluid Heat Transfer Rate [W]
    HVAC,SUM, Pipe Fluid Heat Transfer Energy [J]
    HVAC,Average, Pipe Mass Flow Rate [kg/s]
    HVAC,Average, Pipe Inlet Temperature [T]
    HVAC,Average, Pipe Outlet Temperature [T]
    HVAC,Average, Pipe Volume Flow Rate [m3/s]
~~~~~~~~~~~~~~~~~~~~

#### Pipe Fluid Heat Transfer Rate [W]

The output provides the total amount of heat loss/gain in the fluid from pipe inlet to outlet.

#### Pipe Fluid Heat Transfer Energy [J]

Total energy fluid has lost/gained between pipe inlet and outlet. It is metered on EnergyTransfer with an end use of Pipes.

#### Pipe Mass Flow Rate [kg/s]

Mass flow rate of the fluid in the pipe.

#### Pipe Inlet Temperature [C]

#### Pipe Outlet Temperature [C]

Temperature of fluid at entering and exiting of pipe.

#### Pipe Volume Flow Rate [m3/s]

Fluid volumetric flow rate, (mass flow rate / density)

## Pipe:Underground

This object specifies inputs which are used to simulate the heat transfer from a plant loop pipe placed underground.

### Inputs

#### Field: Name

This alpha field is used as an identifying field for the pipe.

#### Field: Construction Name

 This alpha field references a basic construction object that gives a layer-by-layer description of the pipe wall and insulation. The construction object follows standard conventions, describing material properties for each layer beginning with the outermost layer, the insulation, if it exists, and ending with the pipe wall layer.  The construction object may have either one or two layers.  If only one, then the material must be the pipe wall.  If two, then insulation is utilized.

#### Field: Fluid Inlet Node Name

 This alpha field contains the name of the pipe fluid inlet node.

#### Field: Fluid Outlet Node Name

 This alpha field contains the name of the pipe fluid outlet node.

#### Field: Sun Exposure

 This alpha field allows the user to specify one of two options: NoSun or SunExposed.  For SunExposed, the simulation includes diffuse and beam solar effects on the ground surface.  For NoSun, no solar effects are included.

#### Field: Pipe Inside Diameter

 This field is used to enter the inside diameter of the pipe in units of m. Pipe inside diameter must be a positive quantity.

#### Field: Pipe Length

 This field is used to enter the length of the pipe in units of {m}. Pipe length must be a positive quantity.

#### Field: Soil Material Name

This references a [Material](#material-and-material-properties) object that contains the soil properties and thickness.  Note that when defining the soil layer, the thickness should be the thickness of soil between the pipe wall and the ground surface.

#### Field: Average Soil Surface Temperature

If a GroundTemperatures:Surface object is not given in the input, this is #1 of 3 inputs that must be given directly.  This represents the annual average soil temperature above the pipe.  This field can be calculated in advance using the separate CalcSoilSurfTemp program.

#### Field: Amplitude of Soil Surface Temperature

If a GroundTemperatures:Surface object is not given in the input, this is #2 of 3 inputs that must be given directly.  This represents the annual average soil temperature variation from the average temperature itself.  For example, if this were represented as a sine wave, this would simply be the amplitude of the curve.  This field can be calculated in advance using the separate CalcSoilSurfTemp program.

#### Field: Phase Constant of Soil Surface Temperature

If a GroundTemperatures:Surface object is not given in the input, this is #3 of 3 inputs that must be given directly.  This represents the time elapsed from the beginning of the year to the date of minimum surface temperature.  For example, if this were represented as a sine wave, this would simply be the phase shift of the curve.  This field can be calculated in advance using the separate CalcSoilSurfTemp program

In order to avoid having to run the preprocessor program to generate soil temperature, the user may choose to simply input a GroundTemperatures:Surface object.  This object inputs average monthly surface temperatures.  These temperatures are then used within the model to develop average ground surface data.  This ground surface data can then be used as part of the model boundary condition set.  Without a set of surface ground temperatures, the model will require user input of the three last input fields.

An example of this object in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    Pipe:Underground,
        Pipe Heat Transfer Towers,          !- Name of Pipe
        Insulated Buried Pipe,              !- Construction Name
        Condenser Tower Outlet Node,        !- Inlet Node Name
        HTPipe Outlet Node,                 !- Outlet Node Name
        Water,                              !- Fluid Name
        SunExposed,                         !- Sun Exposure
        0.05,                               !- Pipe Inside Diameter
        20.0,                               !- pipe Length
        Buried Pipe Soil,                   !- Soil Material
        13,                                 !- Average Soil Surface Temperature
        1.5,                                !- Amplitude of Soil Surface Temperature
        30;                                 !- Phase Constant of Soil Surface Temperature

      Construction,
        Insulated Buried Pipe,              !- Name
        Buried Pipe Insulation,             !- Layer #1
        Buried Pipe Steel;                  !- Layer #2

      Material,
        Buried Pipe Soil,                   !- Name
        Smooth,                             !- Roughness
        1.5,                                !- Thickness {m}
        0.36,                               !- Conductivity {W/m-K}
        2000.0,                             !- Density {kg/m3}
        1200.0,                             !- Specific Heat {J/kg-K}
        0.9,                                !- Thermal Absorptance
        0.5,                                !- Solar Absorptance
        0.5;                                !- Visible Absorptance
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC, Average, Pipe Fluid Heat Transfer Rate [W]
    HVAC, SUM, Pipe Fluid Heat Transfer Energy [J]
    HVAC, Average, Pipe Mass Flow Rate [kg/s]
    HVAC, Average, Pipe Volume Flow Rate [m3/s]
    HVAC, Average, Pipe Inlet Temperature [C]
    HVAC, Average, Pipe Outlet Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Pipe Fluid Heat Transfer Rate [W]

The output provides the total amount of heat loss/gain in the fluid from pipe inlet to outlet.

#### Pipe Fluid Heat Transfer Energy [J]

Total energy fluid has lost/gained between pipe inlet and outlet. It is metered on

EnergyTransfer with an end use of Pipes.

#### Pipe Mass Flow Rate [kg/s]

 Mass flow rate of the fluid in the pipe.

#### Pipe Volume Flow Rate [m3/s]

Fluid volumetric flow rate, (mass flow rate / density)

#### Pipe Inlet Temperature [C]

#### Pipe Outlet Temperature [C]

Temperature of fluid at entrance and exit of pipe.

## PipingSystem:Underground Class Objects

Objects in this class are used to simulate the heat transfer processes of a piping system placed underground, which may or may not be in contact with a number of zone surfaces.  There are three related objects, all of which are prefixed with PipingSystem:Underground :

- PipingSystem:Underground:Domain
- PipingSystem:Underground:PipeCircuit
- PipingSystem:Underground:PipeSegment

## PipingSystem:Underground:Domain

This section documents the domain object, which is used to specify information for the overall ground domain, including thermal properties, mesh parameters, and surface interaction.

### Inputs

#### Field: Name

This alpha field is used as an identifying field for the ground domain.

#### Field: XMax, YMax, ZMax

These numeric fields are used to specify the overall domain size, in each of the three Cartesian dimensions.  The domain will be meshed from {x, y, z} = {0, 0, 0} to {Xmax, Ymax, Zmax}.

Note that for this object, the x-z plane lies parallel to the ground surface, such that the y-dimension can be thought of as representing the burial depth.  This is in contrast to the way building surface coordinates are defined with the x-y plane lying parallel to the ground.  Also note that any pipe segments that end up being used in a ground domain will be placed axially in the z-dimension.  Thus, pipe flow will always be in either the positive z or negative z direction.

#### Field: X-Direction Mesh Density Parameter

This numeric integer field represents the number of cells to be placed between any two "domain partitions" during mesh development.  A domain partition may be thought of as a basement wall/floor, or a pipe.  Once these components are laid out in the domain, this field number of cells are placed between the partitions.  Further information on this is found in the engineering reference manual.

#### Field: X-Direction Mesh Type

This alpha field represents the type of mesh to create when placing cells between "domain partitions."  Two options are available: "uniform" and "symmetricgeometric."  For uniform, the cells are placed in the region with all cells having equal size.  For symmetric-geometric, the cells are compressed toward the partitions so that there are more cells on either side of the mesh region.  This can help focus computational intensity on the boundary areas where it is likely more desired.  For symmetric-geometric mesh distribution, the mesh density parameter should be an even number to provide the symmetric condition.

#### Field: X-Direction Geometric Coefficient

This numeric field represents the compression of cell distribution if the x-direction mesh type is set to "symmetricgeometric."  If the mesh type is uniform, this field is not interpreted.  For symmetric geometric, a value in this field equal to 1 would result in a uniform distribution, while a value of 2 would provide a highly skewed distribution.

#### Field: Y-Direction Mesh Density Parameter

See Field: X-Direction Mesh Density Parameter.

#### Field: Y-Direction Mesh Type

See Field: X-Direction Mesh Type.

#### Field: Y-Direction Geometric Coefficient

See Field: X-Direction Geometric Coefficient.

#### Field: Z-Direction Mesh Density Parameter

See Field: X-Direction Mesh Density Parameter.

#### Field: Z-Direction Mesh Type

See Field: X-Direction Mesh Type.

#### Field: Z-Direction Geometric Coefficient

See Field: X-Direction Geometric Coefficient.

#### Field: Soil Thermal Conductivity

The thermal conductivity of the soil, in W/m-K.

#### Field: Soil Density

The bulk density of the soil, in kg/m3.

#### Field: Soil Specific Heat

The specific heat of dry soil, in J/kg-K.  If moisture is defined in this object, moisture and freezing effects are accounted for by varying the specific heat value.

#### Field: Soil Moisture Content Volume Fraction

A nominal value of soil moisture content to be used when evaluating soil thermal properties.

#### Field: Soil Moisture Content Volume Fraction at Saturation

A nominal value of soil moisture content when the soil is saturated, this is used in evaluating thermal properties of freezing soil

#### Field: Kusuda-Achenbach Average Surface Temperature

The annual average surface temperature to be applied to the Kusuda-Achenbach farfield boundary temperature correlation.

#### Field: Kusuda-Achenbach Average Amplitude of Surface Temperature

The annual average surface temperature variation from average.  This is also used in the Kusuda-Achenbach temperature correlation.

#### Field: Kusuda-Achenbach Phase Shift of Minimum Surface Temperature

The phase shift of minimum surface temperature, or the day of the year when the minimum surface temperature occurs.

#### Field: This Domain Includes Basement Surface Interaction

A yes/no field stating whether zone surfaces should be coupled to this domain.

#### Field: Width of Basement Floor in Ground Domain

If this domain does include basement interaction, this represents the width of the domain cutaway to include the basement.  In essence, this is the width of basement floor to include in the ground simulation.  This width should *include* the basement wall thickness.

#### Field: Width of Basement Floor in Ground Domain

If this domain does include basement interaction, this represents the depth of the domain cutaway to include the basement.  In essence, this is the burial depth of the basement to include in the ground simulation.  This depth should include the basement floor thickness.

#### Field: Shift Pipe X Coordinates By Basement Width

If this domain does include basement interaction, this flag specifies whether the pipe segment X-values should then be shifted by the basement width.  In essence, this specifies the reference point for pipe segment x-values: if NO then the reference is domain x=0, if YES then the reference is the basement width point.

#### Field: Name of Basement Wall Boundary Condition Model

The name of an OtherSideConditions model defined in the input file and referenced by the surfaces which should be included as "wall surfaces" for the basement interaction part of the ground heat transfer model.

#### Field: Name of Basement Wall Boundary Condition Model

The name of an OtherSideConditions model defined in the input file and referenced by the surfaces which should be included as "floor surfaces" for the basement interaction part of the ground heat transfer model.

#### Field: Convergence Criterion for the Outer Cartesian Domain Iteration Loop

The maximum temperature deviation within any cell between one iteration and another to decide that the Cartesian domain has converged to within tolerance.  A smaller value will improve accuracy and computation time.  A smaller value should be accompanied by a higher number of iterations if maximum accuracy is desired.

#### Field: Maximum Iterations in the Outer Cartesian Domain Iteration Loop

The maximum number of iterations to make when performing temperature updates of the Cartesian coordinate system.  The actual number of iterations made will of course depend on transient conditions and convergence tolerance.

#### Field: Number of Pipe Circuits Entered for this Domain

The number of pipe circuit objects which will be defined in the following fields.

#### Field: Pipe Circuit 1 {… to N; extensible}

A [PipingSystem:Underground:PipeCircuit](#pipingsystemundergroundpipecircuit) to be included in this domain.  This indicates that the pipe circuit and all of the underlying pipe segments will be included during mesh development and heat transfer calculations within this domain.  Any number of pipe circuits can be added, assuming all underlying pipe segments do not cause any conflicts for the mesh development engine (overlapping pipes, etc.).

## PipingSystem:Underground:PipeCircuit

This section documents the pipe circuit object, which is used to specify information for the plant loop topology, such as inlet and outlet connections.  This object also groups together pipe segments to define flow paths within a given pipe circuit.

### Inputs

#### Field: Name

This alpha field is used as an identifying field for the pipe circuit.

#### Field: Pipe Thermal Conductivity

 The thermal conductivity of the pipe, in W/m-K.

#### Field: Pipe Density

 The bulk density of the pipe, in kg/m3.

#### Field: Pipe Specific Heat

 The specific heat of the pipe, in J/kg-K.

#### Field: Pipe Inner Diameter

 The inner diameter of the pipe, in m.

#### Field: Pipe Outer Diameter

 The outer diameter of the pipe, in m.

#### Field: Design Flow Rate

 The "design" flow in this pipe circuit, in m3/s.  This is the flow which this component will request during the simulation.  Of course, other components and overall plant management will decide the flow actually given to this component.

#### Field: Circuit Inlet Node Name

 The inlet node name for this circuit, which is used when placing this circuit on a branch on a plant or condenser loop.

#### Field: Circuit Outlet Node Name

 The outlet node name for this circuit, which is used when placing this circuit on a branch on a plant or condenser loop.

#### Field: Convergence Criterion for the Inner Radial Iteration Loop

 The maximum temperature deviation within any cell between one iteration and another to decide that the radial domain has converged to within tolerance.  A smaller value will improve accuracy and computation time.  A smaller value should be accompanied by a higher number of iterations if maximum accuracy is desired.

#### Field: Maximum Iterations in the Inner Radial Iteration Loop

 The maximum number of iterations to make when performing temperature updates of the radial coordinate system.  The actual number of iterations made will of course depend on transient conditions and convergence tolerance.

#### Field: Number of Soil Nodes in the Inner Radial Near Pipe Mesh Region

 The number of soil nodes to discretize pipe cells.  More information on mesh development is provided in the engineering reference manual.

#### Field: Radial Thickness of Inner Radial Near Pipe Mesh Region

 The radial distance used to discretize pipe cells.  More information on mesh development is provided in the engineering reference manual.

#### Field: Number of Pipe Segments Entered for this Pipe Circuit

 The number of pipe segment objects which will be defined in the following fields.

#### Field: Pipe Segment 1 {… to N; extensible}

 A [PipingSystem:Underground:PipeSegment](#pipingsystemundergroundpipesegment) to be included in this pipe circuit.  The circuit is set up so that flow passes through the *N* segments in the order they are entered here.

## PipingSystem:Underground:PipeSegment

This section documents the pipe segment object, which is used to specify information for a single pipe segment placed at some x, y coordinate in the ground.  The flow direction is also defined in this object to allow for careful description of varying flow paths.  This can be useful for accounting for short circuiting effects if two pipes are placed in counterflow vs. parallel flow.

### Inputs

#### Field: Name

This alpha field is used as an identifying field for the pipe segment.

#### Field: X Position

This numeric field represents the x-direction placement of the pipe.  If no basement is included in the simulation, this is measured from x=0.  If a basement is included, and the domain object specifies that pipes should be shifted, this is measured from the basement wall.

#### Field: Y Position

This numeric field represents the burial depth of the pipe, measured from the ground surface.  This will always be a positive number.

#### Field: Flow Direction

This alpha field is used to define the flow direction within this pipe segment.  The choices are "increasing" or "decreasing."  These can be chosen carefully to encapsulate specific short-circuiting effects due to varying flow configurations (circuiting).

An example of this object in an IDF is offered here for a foundation heat exchanger

~~~~~~~~~~~~~~~~~~~~

      PipingSystem:Underground:Domain,
        My Piping System,        !- Name
        12,                      !- Xmax
        4.5,                     !- Ymax
        36.84,                   !- Zmax
        2,                       !- XMeshCount
        Uniform,                 !- XMeshType
        ,                        !- XGeometricCoeff
        2,                       !- YMeshCount
        Uniform,                 !- YMeshType
        ,                        !- YGeometricCoeff
        6,                       !- ZMeshCount
        Uniform,                 !- ZMeshType
        ,                        !- ZGeometricCoeff
        1.08,                    !- GroundThermalConductivity
        962,                     !- GroundDensity
        2576,                    !- GroundSpecificHeat
        30,                      !- MoistureContent
        50,                      !- MoistureContentAtSaturation
        15.5,                    !- KusudaGroundTemp
        12.8,                    !- KusudaGroundTempAmplitude
        17.3,                    !- KusudaPhaseShift
        Yes,                     !- DomainHasBasement
        6,                       !- BasementWidthInDomain
        2.5,                     !- BasementDepthInDomain
        Yes,                     !- ShiftPipeXValuesByBasementWidth
        BasementWallOSCM,        !- BasementWallBoundaryConditionModel
        BasementFloorOSCM,       !- BasementFloorBoundaryConditionModel
        0.005,                   !- CartesianIterationConvergenceCriterion
        100,                     !- CartesianMaxIterations
        0.408,                   !- EvapotranspirationGroundCoverParameter
        1,                       !- NumPipeCircuits
        My Pipe Circuit;         !- PipeCircuit

      PipingSystem:Underground:PipeCircuit,
        My Pipe Circuit,         !- Name
        0.3895,                  !- PipeThermalConductivity
        641,                     !- PipeDensity
        2405,                    !- PipeSpecificHeat
        0.016,                   !- PipeInnerDiameter
        0.02667,                 !- PipeOuterDiameter
        0.004,                   !- DesignFlowRate
        Piping System Inlet Node, !- InletNode
        Piping System Outlet Node, !- OutletNode
        0.001,                   !- RadialIterationConvergenceCriterion
        100,                     !- RadialMaxIterations
        2,                       !- RadialMeshCount
        0.03,                    !- RadialMeshThickness
        6,                       !- NumSegments
        Segment 1,               !- Segment1
        Segment 2,               !- Segment2
        Segment 3,               !- Segment3
        Segment 4,               !- Segment4
        Segment 5,               !- Segment5
        Segment 6;               !- Segment6

      PipingSystem:Underground:PipeSegment,
        Segment 1,               !- Name
        0.67,                    !- X
        2.20,                    !- Burial Depth
        IncreasingZ;             !- Flow Direction

      PipingSystem:Underground:PipeSegment,
        Segment 2,               !- Name
        0.95,                    !- X
        2.20,                    !- Burial Depth
        IncreasingZ;             !- Flow Direction

      PipingSystem:Underground:PipeSegment,
        Segment 3,               !- Name
        1.23,                    !- X
        2.20,                    !- Burial Depth
        IncreasingZ;             !- Flow Direction

      PipingSystem:Underground:PipeSegment,
        Segment 4,               !- Name
        1.40,                    !- X
        1.94,                    !- Burial Depth
        DecreasingZ;             !- Flow Direction

      PipingSystem:Underground:PipeSegment,
        Segment 5,               !- Name
        1.40,                    !- X
        1.66,                    !- Burial Depth
        DecreasingZ;             !- Flow Direction

      PipingSystem:Underground:PipeSegment,
        Segment 6,               !- Name
        1.40,                    !- X
        1.39,                    !- Burial Depth
        DecreasingZ;             !- Flow Direction
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC, Average, Pipe Circuit Mass Flow Rate [kg/s]
    HVAC, Average, Pipe Circuit Inlet Temperature [C]
    HVAC, Average, Pipe Circuit Outlet Temperature [C]
    HVAC, Average, Pipe Segment Inlet Temperature [C]
    HVAC, Average, Pipe Segment Outlet Temperature [C]
    HVAC, Average, Pipe Circuit Fluid Heat Transfer Rate [W]
    HVAC, Average, Pipe Segment Fluid Heat Transfer Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Pipe Circuit Mass Flow Rate [kg/s]

The output provides the mass flow rate currently being sent through the pipe circuit.

#### Pipe Circuit Inlet Temperature [C] 

#### Pipe Circuit Outlet Temperature [C]

Temperature of fluid at the inlet and outlet of a given pipe circuit.

#### Pipe Segment Inlet Temperature [C]

#### Pipe Segment Outlet Temperature [C]

Temperature of fluid at the inlet and outlet of a given pipe segment.

#### Pipe Circuit Fluid Heat Transfer Rate [W]

This is the fluid heat gain or loss in the pipe circuit, in Watts.

#### Pipe Segment Fluid Heat Transfer Rate [W]

This is the fluid heat gain or loss in the pipe segment, in Watts.

## Duct

The [Duct](#duct) object is a component for air systems that is a direct analogue for [Pipe:Adiabatic](#pipeadiabatic) in the fluid loops. It is used when it is necessary (due to the HVAC system configuration) to have a branch that has no functional components. This case most often arises for a bypass branch. Since every branch must have at least one component, the [Duct](#duct) component is used for this situation. The duct is an adiabatic, pass-through component; all of its inlet conditions are passed through unchanged to its outlet.

### Inputs

#### Field: Name

This is the unique name for this component. Any reference to this component (in a [BranchList](#branchlist), for instance) will refer to it by this name.

#### Field: Inlet Node Name

The name of the component's air inlet node.

#### Field: Outlet Node Name

The name of the component's air outlet node.

Below is an example input for [Duct](#duct).

~~~~~~~~~~~~~~~~~~~~

    DUCT,
        VAV Sys 1 Bypass Duct,            !- Name
        VAV Sys 1 Bypass Duct Inlet Node, !- Inlet Node Name
        VAV Sys 1 Bypass Duct Outlet Node;!- Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

There are no outputs for [Duct](#duct).