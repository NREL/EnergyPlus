# eplusout.bnd

The "branch node details" (bnd) file is intended to give enough information that one could (with a chosen software) diagram the nodes and components of the HVAC system. It may or may not achieve that objective. Of more use may be its illustration of node connection/branch errors that aren't detected by the software. This file has the details to support any "node connection" errors that will be noted in the eplusout.err file. Branch validation is shown in this file. Branches are checked to assure that each output node of the branch element is an input node to the next branch element. Cross-branch checking is not done directly within the program though the details will illustrate some problems of that nature.

Supply and Return Air Paths are also checked and feedback about each item are shown.

As is standard with many EnergyPlus output files, this file is CSV (comma-delimited) such that it can be read by spreadsheet programs for further manipulation by the user.

An example will illustrate. Notes about the reporting are highlighted in green.

~~~~~~~~~~~~~~~~~~~~

    Program Version,EnergyPlus, <version>
    ! This file shows details about the branches, nodes, and other
    ! elements of the flow connections.
    ! This file is intended for use in "debugging" potential problems
    ! that may also be detected by the program, but may be more easily
    ! identified by "eye".
    ! This file is also intended to support software which draws a
    ! schematic diagram of the HVAC system.
    ! ===============================================================
    ! #Nodes,<Number of Unique Nodes>
     #Nodes,11
    List of all nodes follows. # references may be an indication of faulty node spec (or not)
    ! <Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>
     Node,1,SUPPLY INLET NODE,Air,3
     Node,2,FAN INLET NODE,Air,4
    <reduced for brevity>
    Node,10,ZONE EQUIPMENT OUTLET NODE,Air,2
     Node,11,RELIEF AIR OUTLET NODE,Air,1
    ! ===============================================================
    ! Suspicious nodes have 0 references. It is normal for some nodes, however.
    ! Suspicious nodes have 0 references.  It is normal for some nodes, however.
    ! Listing nodes with 0 references (culled from previous list):
    ! <Suspicious Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>
     Suspicious Node,7,MAIN ZONE NODE,Air,0
    List of branches follow.
    ! <#Branch Lists>,<Number of Branch Lists>
     #Branch Lists,1
    ! <Branch List>,<Branch List Count>,<Branch List Name>,<Loop Name>,<Loop Type>,<Number of Branches>
    ! <Branch>,<Branch Count>,<Branch Name>,<Loop Name>,<Loop Type>,<Branch Inlet Node Name>,<Branch Outlet Node Name>
     Branch List,1,AIR LOOP BRANCHES,EVAP COOLER SYSTEM,Air,1
       Branch,1,AIR LOOP MAIN BRANCH,EVAP COOLER SYSTEM,Air,SUPPLY INLET NODE,SUPPLY OUTLET NODE
    ! ===============================================================
    ! <#Supply Air Paths>,<Number of Supply Air Paths>
    ! <#Supply Air Paths>,<Number of Supply Air Paths>
     #Supply Air Paths,1
    ! <Supply Air Path>,<Supply Air Path Count>,<Supply Air Path Name>,<AirLoopHVAC Name>
    ! <#Components on Supply Air Path>,<Number of Components>
    ! <Supply Air Path Component>,<Component Count>,<Component Type>,<Component Name>,<AirLoopHVAC Name>
    ! <#Outlet Nodes on Supply Air Path Component>,<Number of Nodes>
    ! <Supply Air Path Component Nodes>,<Node Count>,<Component Type>,<Component Name>,<Inlet Node Name>,<Outlet Node Name>,<AirLoopHVAC Name>
     Supply Air Path,1,ZONE SUPPLY AIR PATH,EVAP COOLER SYSTEM
       #Components on Supply Air Path,1
       Supply Air Path Component,1,AIRLOOPHVAC:ZONESPLITTER,ZONE SUPPLY AIR SPLITTER,EVAP COOLER SYSTEM
         #Outlet Nodes on Supply Air Path Component,1
         Supply Air Path Component Nodes,1,AIRLOOPHVAC:ZONESPLITTER,ZONE SUPPLY AIR SPLITTER,ZONE EQUIPMENT INLET NODE,MAIN ZONE INLET NODE,EVAP COOLER SYSTEM
    ! <#Nodes on Supply Air Path>,<Number of Nodes>
    ! <Supply Air Path Node>,<Node Type>,<Node Count>,<Node Name>,<AirLoopHVAC Name>
    #Nodes on Supply Air Path,2
       Supply Air Path Node,Inlet Node,1,ZONE EQUIPMENT INLET NODE,EVAP COOLER SYSTEM
       Supply Air Path Node,Outlet Node,2,MAIN ZONE INLET NODE,EVAP COOLER SYSTEM
    ! ===============================================================
    ! <#Return Air Paths>,<Number of Return Air Paths>
    ! <#Return Air Paths>,<Number of Return Air Paths>
     #Return Air Paths,1
    ! <Return Air Path>,<Return Air Path Count>,<Return Air Path Name>,<AirLoopHVAC Name>
    ! <#Components on Return Air Path>,<Number of Components>
    ! <Return Air Path Component>,<Component Count>,<Component Type>,<Component Name>,<AirLoopHVAC Name>
    ! <#Inlet Nodes on Return Air Path Component>,<Number of Nodes>
    ! <Return Air Path Component Nodes>,<Node Count>,<Component Type>,<Component Name>,<Inlet Node Name>,<Outlet Node Name>,<AirLoopHVAC Name>
     Return Air Path,1,ZONE RETURN AIR PATH,EVAP COOLER SYSTEM
       #Components on Return Air Path,1
       Return Air Path Component,1,AIRLOOPHVAC:ZONEMIXER,ZONE RETURN AIR MIXER,EVAP COOLER SYSTEM
         #Inlet Nodes on Return Air Path Component,1
         Return Air Path Component Nodes,1,AIRLOOPHVAC:ZONEMIXER,ZONE RETURN AIR MIXER,MAIN ZONE OUTLET NODE,ZONE EQUIPMENT OUTLET NODE,EVAP COOLER SYSTEM
    ! <#Nodes on Return Air Path>,<Number of Nodes>
    ! <Return Air Path Node>,<Node Type>,<Node Count>,<Node Name>,<AirLoopHVAC Name>
       #Nodes on Return Air Path,2
       Return Air Path Node,Outlet Node,1,ZONE EQUIPMENT OUTLET NODE,EVAP COOLER SYSTEM
       Return Air Path Node,Inlet Node,2,MAIN ZONE OUTLET NODE,EVAP COOLER SYSTEM
    ! ===============================================================
    ! #Outside Air Nodes,<Number of Outside Air Nodes>
    ! #Outdoor Air Nodes,<Number of Outdoor Air Nodes>
     #Outdoor Air Nodes,1
    ! <Outdoor Air Node>,<NodeNumber>,<Node Name>
     Outdoor Air Node,5,OUTSIDE AIR INLET NODE
    ! ===============================================================
    Component sets. Very important for node connection error detection.
    ! <#Component Sets>,<Number of Component Sets>
     #Component Sets,4
    ! <Component Set>,<Component Set Count>,<Parent Object Type>,<Parent Object Name>,<Component Type>,<Component Name>,<Inlet Node ID>,<Outlet Node ID>,<Description>
     Component Set,1,BRANCH,AIR LOOP MAIN BRANCH,AIRLOOPHVAC:OUTDOORAIRSYSTEM,OUTSIDE AIR SYSTEM,SUPPLY INLET NODE,FAN INLET NODE,Air Nodes
     Component Set,2,BRANCH,AIR LOOP MAIN BRANCH,FAN:CONSTANTVOLUME,SUPPLY FAN,FAN INLET NODE,EVAP COOLER INLET NODE,Air Nodes
     Component Set,3,BRANCH,AIR LOOP MAIN BRANCH,EVAPORATIVECOOLER:DIRECT:CELDEKPAD,EVAPORATIVE COOLER,EVAP COOLER INLET NODE,SUPPLY OUTLET NODE,Evap Air Nodes
     Component Set,4,AIRLOOPHVAC:OUTDOORAIRSYSTEM,OUTSIDE AIR SYSTEM,OUTDOORAIR:MIXER,OUTSIDE AIR MIXING BOX,OUTSIDE AIR INLET NODE,FAN INLET NODE,Air Nodes
    Similar details for Plant Loops, Condenser Loops, Controlled Zones, etc.
~~~~~~~~~~~~~~~~~~~~