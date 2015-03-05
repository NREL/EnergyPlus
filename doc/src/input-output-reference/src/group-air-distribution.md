# Group – Air Distribution

## AirLoopHVAC

Air loops along with zone equipment form the entire forced air heating and cooling system (air side). The main statement for defining an air loop is the *[AirLoopHVAC](#airloophvac)* object described here. As with the main plant and condenser statements, several of the items in the [AirLoopHVAC](#airloophvac) object definition are actually names that refer to other objects in the input file. After the [AirLoopHVAC](#airloophvac) object name, four such list pointers are encountered: for controllers, system availability, branches, and connectors. The Controller List defines how the air loop will respond due to various inputs (control signals). The Availability Manager List refers to methods for controlling when a system is operational. This may be as simple as a schedule to define when the system is shutdown. The [Branch](#branch) List lists the branches that comprise the primary air system. Finally, the Connector List lists the connections between the branches. The branches and connections together define the primary air system topology.

The only numeric input is the design primary air flow rate. This item is input just before the [BranchList](#branchlist) name.

The next series of names refer to nodes for the various inlet and outlet points of the air loop. The air loop starts where the zone equipment ends. This is a single point consisting of the return air duct once all of the zone return air streams have been collected. While in reality, there is really only one point, for clarity within the simulation components and consistency with the other HVAC loop sections, this point in the systems is defined as two points: one that resides with the zone equipment simulation and one that is acted upon by the air loop simulation. Both node names must be entered into the input file, and both nodes must have unique names. Similarly, the end points of the air loop that also correspond to the beginning points of the zone equipment loop must also be defined. These consist of names referencing lists of up to three pairs of node names as shown below. Up to three air loop outlets are allowed to accommodate the simulation of three deck systems.

### Inputs

#### Field: Name

This field is a unique, user assigned name for a single instance of an [AirLoopHVAC](#airloophvac) object. Any other object referencing this [AirLoopHVAC](#airloophvac) will use this name.

#### Field: Controller List Name

This field is the name of a ControllerList object. A Controller List is simply a list of controllers giving both controller name and type. This Controller List specifies all the controllers that will act on this primary air loop. The order of the controllers in the list is significant: controllers are simulated sequentially in the order given in the Controller List.

#### Field: Availability Manager List Name

This field is the name of a AvailabilityManagerList object. A Availability Manager List is a list of Availability Managers giving both Availability Manager type and name. The availability managers in the list apply to this primary air loop. That is, they determine when and if this air loop is on or off, overriding the control provided by the central fan on/off schedule.

#### Field: Design Supply Air Flow Rate

This is the system primary air design volumetric flow rate in cubic meters per second.

#### Field: Branch List Name

This field is the object name of a [BranchList](#branchlist) object (see [BranchList](#branchlist) and [Branch](#branch)). The [BranchList](#branchlist) named here specifies all the branches composing the primary air system. These branches, together with the Connectors listed in the [ConnectorList](#connectorlist), define the primary air system topology.

#### Field: Connector List Name

This field is the name of [ConnectorList](#connectorlist) object. This [ConnectorList](#connectorlist) object lists all the Connectors (by type and name) that are included in this primary air system. These Connectors, together with the Branches in the [BranchList](#branchlist), define the topology of the primary air system.

#### Field: Supply Side Inlet Node Name

The name of the air entrance node of this primary air system. This is the inlet node for return air for this air system.

#### Field: Demand Side Outlet Node Name

The name of the air outlet node for the zone equipment group ([ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) objects) attached to the primary air system. This should be the outlet node of a [AirLoopHVAC:ZoneMixer](#airloophvaczonemixer) in the [AirLoopHVAC:ReturnPath](#airloophvacsupplypath-airloophvacreturnpath) for the zone equipment group attached to this primary air system.

#### Field: Demand Side Inlet Node Names

This field can be the name of a node which is the air inlet node for the zone equipment group (see [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) objects) attached to this primary air system. Or, this field can be the name of a node list containing one or more nodes (up to 3). These nodes should be the inlet nodes to the [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter) or [AirLoopHVAC:SupplyPlenum](#airloophvacsupplyplenum) in each of the AirLoopHVAC:SupplyPaths for the zone equipment groups attached to this primary air system. For single duct systems, there is only one node name in this list. For two and three duct systems, the order of the nodes in this list must correspond with the order of the nodes in the Supply Side Outlet Node Names list.

#### Field: Supply Side Outlet Node Names

This field can be the name of a node which is the air outlet node for each supply duct of this primary air system. Or, this field can be the name of a node list containing one or more nodes (up to 3). The list can contain the names of up to three nodes. For single duct systems, there is only one node name in this list. For two and three duct systems, the order of the nodes in this list must correspond with the order of the nodes in the Demand Side Inlet Node Names list.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC,
      Main Dual Duct Air Loop,           ! Primary Air Loop Name
      Dual Duct System 1 Controllers,    ! Controller List
      Dual Duct System 1 Schedule List , ! System Availability Manager List
      1.3 ,                              ! Primary air design volumetric flow rate
      Dual Duct Air Loop Branches ,      ! Air Loop Branch List Name
      Dual Duct Connectors ,             ! Air Loop Connector List Name
      Supply Fan Inlet Node ,            ! ReturnAir AirLoop Inlet Node
      Return Air Mixer Outlet ,          ! ZoneEquipGroup Outlet Node
      Zone Equipment Inlet Node List ,   ! SupplyAirPath ZoneEquipGroup Inlet Nodes
      Air Loop Outlet Node List ;        ! AirLoop Outlet Nodes
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Air System Simulation Cycle On Off Status
    HVAC,Sum,HVAC System Solver Iteration Count []
    HVAC,Sum,Air System Solver Iteration Count []
    HVAC,Sum,Air System Simulation Maximum Iteration Count []
    HVAC,Sum,Air System Simulation Iteration Count []
    HVAC,Sum,Air System Component Model Simulation Calls []
~~~~~~~~~~~~~~~~~~~~

#### Air System Simulation Cycle On Off Status

This field is the availability status of an [AirLoopHVAC](#airloophvac). This status flag is a result of the calculations made by the System Availability Manager(s) listed in a System Availability Manager List. When a single availability manager is used in a System Availability Manager List, this is also the availability status reported by the specific availability manager (Ref. System Availability Manager Outputs). When multiple availability managers are used in a System Availability Manager List, the loop availability status is determine by the rules associated with AirLoopHVACs (see rules described for Group – System Availability Managers). The control status outputs are represented using integers 0 through 3.. These integers represent *NoAction* (0), *ForceOff* (1), *CycleOn* (2), and *CycleOnZoneFansOnly* (3). Since the status output is averaged, the output result may not correspond to the values described here when output variable frequencies other than detailed are used. Use the "detailed" reporting frequency (Ref. [Output:Variable](#outputvariable) object) to view the availability status at each simulation timestep.

#### HVAC System Solver Iteration Count []

#### Air System Solver Iteration Count []

#### Air System Simulation Maximum Iteration Count []

#### Air System Simulation Iteration Count []

#### **Air System Component Model Simulation Calls []**

These variables are simply counters of how many iterations were executed. The count for any given HVAC time step will be the iterations executed before convergence was achieved, or the max allowed in which case the simulation throws a warning and proceeds to the next time step.

### Outputs

This section provides more detailed information on the reporting available for outdoor air ventilation.  Sixteen cooling and heating load variables and eight energy summary variables are available that report the impact of system outdoor air on zone loads, system demand and total energy use. The representative air system shown in the diagram below shows outdoor air (OA), return air (RA), supply air (SA) and mixed air (MA).

![Example System for Ventilation Loads Report](media/example-system-for-ventilation-loads-report.jpeg)


The overall effect of outdoor air on the system shown above can be summarized by considering the mixing box. In this system, part or all of the return air is replaced by outdoor air. The presence of any heat recovery will already be taken into account and thus will automatically be accounted for by using the outdoor air inlet conditions to the mixing box (point OA in the diagram) rather than actual outdoor air conditions. Thus, the overall energy impact of outdoor air (ventilation) on a particular system can be evaluated by multiplying the outdoor air mass flow rate by the enthalpy difference between the outdoor air entering the mixing box (OA) and the return air entering the mixing box (RA') as shown in the following equation.

![](media/image204.png)\


#### Ventilation Flow Outputs

The fresh air flow into each zones are reported using the ventilation flow output variables. The total outdoor air flow in a primary air system is divided to each zone based on the fraction of flow each zone is supplied. There are six variables which deals with the mechanical ventilation flow reporting. Those six variables are:

#### Zone Mechanical Ventilation Mass Flow Rate [kg/s]

Reports the average outdoor air mass flow rate to any zone over the reporting interval.

#### Zone Mechanical Ventilation Mass [kg]

Reports the total outdoor air mass that has been supplied to any zone over the reporting interval.

#### Zone Mechanical Ventilation Standard Density Volume Flow Rate [m3/s]

Reports the average outdoor air volume flow rate to any zone over the reporting interval, calculated using a standard density for air.  Standard density in EnergyPlus corresponds to 20ºC drybulb, dry air, and nominally adjusted for elevation.

#### Zone Mechanical Ventilation Standard Density Volume [m3]

Reports the total outdoor air volume that has been supplied to a zone over the reporting interval, calculated using a standard density for air.  Standard density in EnergyPlus corresponds to 20ºC drybulb, dry air, and nominally adjusted for elevation.

#### Zone Mechanical Ventilation Current Density Volume Flow Rate [m3/s]

Reports the average outdoor air volume flow rate to any zone over the reporting interval, calculated using the current density for zone air.

#### Zone Mechanical Ventilation Current Density Volume [m3]

Reports the total outdoor air volume that has been supplied to a zone over the reporting interval, calculated using the current density for zone air.

#### Zone Mechanical Ventilation Air Changes per Hour [ach]

Reports the air changes per hour in the zone due to the outside fresh air supplied by mechanical ventilation system.

The example syntax below shows the basic ventilation flow variables reported on an hourly basis.

~~~~~~~~~~~~~~~~~~~~

    Output:Variable,*,Zone Mechanical Ventilation Mass Flow Rate,Hourly;
    Output:Variable,*,Zone Mechanical Ventilation Mass,Hourly;
    Output:Variable,*,Zone Mechanical Ventilation Standard Density Volume Flow Rate,Hourly;
    Output:Variable,*,Zone Mechanical Ventilation Standard Density Volume,Hourly;
    Output:Variable,*,Zone Mechanical Ventilation Current Density Volume Flow Rate,Hourly;
    Output:Variable,*,Zone Mechanical Ventilation Current Density Volume,Hourly;
    Output:Variable,*, Zone Mechanical Ventilation Air Changes per Hour,Hourly;
~~~~~~~~~~~~~~~~~~~~

#### Ventilation Load Reports

The impact of system outdoor air on a particular zone may be calculated by summing the mass flow weighted ![](media/image205.png)  over the supply air paths (both cooling and heating) serving the zone as follows:

![](media/image206.png)\


Four output variables each for cooling and heating report the impact of the ventilation air on the zone load in the absence of ventilation air system interactions. The ventilation load output variables are:

#### Zone Mechanical Ventilation Cooling Load Increase Due to Overheating Energy [J]

#### Zone Mechanical Ventilation Heating Load Increase Due to Overcooling Energy [J]

The cooling/heating load that would occur once ventilation air met the zone cooling/heating load and continued to overcool/overheat the zone. No system effects are accounted for.

#### Zone Mechanical Ventilation Cooling Load Decrease Energy [J]

#### Zone Mechanical Ventilation Heating  Load Decrease[J]

The decrease in zone cooling/heating load that would occur as a result of ventilation air introduced directly into the zone. No system effects are accounted for.

#### Zone Mechanical Ventilation No Load Heat Removal Energy [J]

#### Zone Mechanical Ventilation No Load Heat Addition Energy [J]

The addition or removal of heat to a zone with no load. The heat addition or removal is due to mechanical ventilation while the zone thermostat is in the deadband.

To summarize:

**Cooling:**

**Zone Mechanical Ventilation No Load Heat Removal Energy [J]**

Cooling that would be provided directly to zone by ventilation air. Ventilation occurred with no zone load.

**Zone Mechanical Ventilation Cooling Load Increase Energy [J]**

The increase in zone cooling load that would occur as a result of ventilation air introduced directly into the zone. No system effects are accounted for.

**Zone Mechanical Ventilation Cooling Load Increase Due to Overheating Energy [J]**

The cooling load that would occur once ventilation air met the zone heating load and continued to overheat the zone. No system effects are accounted for.

**Zone Mechanical Ventilation Cooling Load Decrease Energy [J]**

The decrease in zone cooling load that would occur as a result of ventilation air introduced directly into the zone. No system effects are accounted for.

**Heating:**

**Zone Mechanical Ventilation No Load Heat Addition Energy [J]**

Heating that would be provided directly to zone by ventilation air. Ventilation occurred with no zone load.

**Zone Mechanical Ventilation Heating Load Increase Energy [J]**

The increase in zone heating load that would occur as a result of ventilation air introduced directly into the zone. No system effects are accounted for.

**Zone Mechanical Ventilation Heating Load Increase Due to Overcooling Energy [J]**

The heating load that would occur once ventilation air met the zone cooling load and continued to overcool the zone. No system effects are accounted for.

**Zone Mechanical Ventilation Heating Load Decrease Energy [J]**

The decrease in zone heating load that would occur as a result of ventilation air introduced directly into the zone. No system effects are accounted for.

The output variables are calculated by comparing the zone ventilation load with the zone cooling or heating load.  ‘Ventilation cooling' decreases the zone cooling load until the cooling load has been eliminated. The remaining ‘ventilation cooling' is reported as ‘overcooling'. ‘Overheating' is calculated in the same manner. The actual system operation, which determines whether or not an ‘overcooling' or ‘overheating' load actually results in increased energy consumption, is not considered in the calculation of these output variables.

The ventilation zone load output variables are shown in Table 24. The variables report the maximum potential "cost" or "benefit" of ventilation air introduced directly into the zone.

Table: Ventilation Load Output Variables

|No [Zone](#zone) Load|[Zone](#zone) Cooling Load|[Zone](#zone) Heating Load
|------------|-----------------|-----------------
**Ventilation**|**Cooling**|Zone Mechanical Ventilation No Load Heat Removal Energy|Zone Mechanical Ventilation Cooling Load Decrease Energy|Zone Mechanical Ventilation Heating Load Increase Energy
**

[Zone](#zone) Mechanical Ventilation Heating Load Increase Due to Overcooling Energy

**Ventilation Heating**|Zone Mechanical Ventilation No Load Heat Addition Energy|Zone Mechanical Ventilation Cooling Load Increase Energy|Zone Mechanical Ventilation Heating Load Decrease Energy
**

[Zone](#zone) Mechanical Ventilation Cooling Load Increase Due to Overheating Energy

The example syntax below shows the basic ventilation load variables reported on a monthly basis.

~~~~~~~~~~~~~~~~~~~~

      Output:Variable,*,Zone Mechanical Ventilation Heating Load Increase Energy,monthly;
      Output:Variable,*,Zone Mechanical Ventilation Heating Load Decrease Energy,monthly;
      Output:Variable,*,Zone Mechanical Ventilation No Load Heat Addition Energy,monthly;
      Output:Variable,*,Zone Mechanical Ventilation Cooling Load Increase Energy,monthly;
      Output:Variable,*,Zone Mechanical Ventilation Cooling Load Decrease Energy,monthly;
      Output:Variable,*,Zone Mechanical Ventilation No Load Heat Removal Energy,monthly;
~~~~~~~~~~~~~~~~~~~~

Reporting on a timestep or hourly level would produce a detailed report with variables intermingled with the other output variables that might be requested in the input file.

Another method will more easily encapsulate the report:

~~~~~~~~~~~~~~~~~~~~

    OutputControl:Table:Style, HTML;     !- ColumnSeparator

    Output:Table:Monthly,
      Ventilation Loads,       !- Name
      ,                        !- DigitsAfterDecimal
    Zone Mechanical Ventilation No Load Heat Removal Energy,
             SumOrAverage,
    Zone Mechanical Ventilation Cooling Load Increase Due to Overheating Energy,
             SumOrAverage,
    Zone Mechanical Ventilation Cooling Load Decrease Energy,
             SumOrAverage,
    Zone Mechanical Ventilation Cooling Load Increase Energy,
             SumOrAverage,
    Zone Mechanical Ventilation No Load Heat Addition Energy,
             SumOrAverage,
    Zone Mechanical Ventilation Heating Load Increase Due to Overcooling Energy,
             SumOrAverage,
    Zone Mechanical Ventilation Heating Load Decrease Energy,
             SumOrAverage,
    Zone Mechanical Ventilation Heating Load Increase Energy,
             SumOrAverage;
~~~~~~~~~~~~~~~~~~~~

This combination will report the ventilation loads on a monthly basis in a HTML style report that can be easily read in a web browser. Review the Output:Table:Monthly object for other methods of display or further options on this report item.

## Systems Level Reporting

Most output variables are a single entity reporting. Meters are a combination of like variables – specifically for Energy Consumption. This section introduces a slightly different concept – where output variables described herein are a combination of several similar items – specifically targeted at HVAC/System level reporting. This reporting is coupled with an Air Loop (ref: [AirLoopHVAC](#airloophvac))

All items shown in this section are typical "output variables" and can be reported with the [Output:Variable](#outputvariable) object.

Or, as is shown by example here, perhaps a more readable form is to generate them in Tabular Form with the "Output:Table:Monthly" object.

![View of System Level Reporting](media/view-of-system-level-reporting.png)


## System Loads Outputs

In this category, the total system load is reported. Two aspects are reported here: Heating and Cooling. The following two variables report the heat energy that system components (including packaged equipment, fans, main coils, reheat coils, humidifiers, desiccant dehumidifiers, evaporative coolers and heat exchangers) add or remove from the air loop. Each variable within these grouping wil

### Air System Total Heating Energy

Heat Addition to the Air Loop (Sum of all components) in Joules

### Air System Total Cooling Energy

Heat Removal from the Air Loop (Sum of all components) in Joules

In ‘Output:Table:Monthly' format shown below.

~~~~~~~~~~~~~~~~~~~~

      OutputControl:Table:Style,HTML;

      Output:Table:Monthly,
        System Loads,       !- Name
        ,                        !- DigitsAfterDecimal
        Air System Total Heating Energy,  SumOrAverage,
        Air System Total Cooling Energy,  SumOrAverage;
~~~~~~~~~~~~~~~~~~~~

## System Energy Use Outputs

System Energy Use consists of six output variables that report the total energy (in Joules) and water consumption (in m^3^) for all system components and including packaged equipment, fans, main coils, reheat coils, humidifiers, desiccant dehumidifiers, and evaporative coolers. The output variables shown below show hot and cold water, steam, electric and gas energy. Hot and cold water energy includes all ‘district cooling' and ‘district heating' energy.

### Air System Hot Water Energy

Hot Water Consumption for the system (Joules).

### Air System Steam Energy

Steam Consumption for the system (Joules).

### Air System Chilled Water Energy

Chilled Water Consumption for the system (Joules).

### Air System Electric Energy

Electric Consumption for the system (Joules).

### Air System Gas Energy

Gas (Natural Gas and Propane) Consumption for the system (Joules).

### Air System Water Volume

Water Consumption for the system (humidifiers and evaporative coolers) (m3).

The Standard reports file includes these variables reporting in the Tabular report form as "Air Loop System Energy and Water Use".

~~~~~~~~~~~~~~~~~~~~

      OutputControl:Table:Style,HTML;

      Output:Table:Monthly,
        Air Loop System Energy and Water Use,       !- Name
        ,                        !- DigitsAfterDecimal
        Air System Hot Water Energy ,  SumOrAverage,
        Air System Steam Energy ,  SumOrAverage,
        Air System Chilled Water Energy,  SumOrAverage,
        Air System Electric Energy,  SumOrAverage,
        Air System Gas Energy,  SumOrAverage,
        Air System Water Volume,  SumOrAverage;
~~~~~~~~~~~~~~~~~~~~

## System Component Loads Outputs

Reporting System Component Loads shows the energy transferred to or extracted from the air loop by system component type. Fans, Cooling Coils, Heating Coils, Heat exchangers, humidifiers, evaporative coolers, and desiccant dehumidifiers show heat transfer rates in Joules. The components may be located in outdoor air systems bundled with packaged equipment or specified as stand-alone components. The reporting accounts for both main branch and reheat coils.

The Standard reports file includes these variables reporting in the Tabular report form as "Air Loop System Component Loads".

~~~~~~~~~~~~~~~~~~~~

      OutputControl:Table:Style,HTML;

      Output:Table:Monthly,
        Air Loop System Component Loads,       !- Name
        ,                        !- DigitsAfterDecimal
        Air System Fan Air Heating Energy,  SumOrAverage,
        Air System Cooling Coil Total Cooling Energy,  SumOrAverage,
        Air System Heating Coil Total Heating Energy,  SumOrAverage,
        Air System Heat Exchanger Total Heating Energy,  SumOrAverage,
        Air System Heat Exchanger Total Cooling Energy,  SumOrAverage,
        Air System Humidifier Total Heating Energy,  SumOrAverage,
        Air System Evaporative Cooler Total Cooling Energy,  SumOrAverage,
        Air System Desiccant Dehumidifier Total Cooling Energy,  SumOrAverage;
~~~~~~~~~~~~~~~~~~~~

### Air System Fan Air Heating Energy

Energy added to the air loop by Fans (Joules)

### Air System Cooling Coil Total Cooling Energy

Energy removed from air loop by Cooling Coils (Joules)

### Air System Heating Coil Total Heating Energy

Energy added to air loop by Heating Coils (Joules)

### Air System Heat Exchanger Total Heating Energy

Energy added to air loop by air-to-air heat recovery heat exchangers (Joules)

### Air System Heat Exchanger Total Cooling Energy

Energy removed from air loop by air-to-air heat recovery heat exchangers (Joules)

### Air System Humidifier Total Heating Energy

Energy added to air loop by humidifiers (Joules)

### Air System Evaporative Cooler Total Cooling Energy

Energy removed from air loop by evaporative coolers (Joules)

### Air System Desiccant Dehumidifier Total Cooling Energy

Energy removed from air loop by desiccant dehumidifiers (Joules)

### Air System Solar Collector Total Heating Energy

Energy added to air loop by solar collectors (Joules)

### Air System Solar Collector Total Cooling Energy

Energy removed from air loop by solar collectors (Joules)

### Air System User Defined Air Terminal Total Heating Energy

Energy added to air loop by user defined air terminal units (Joules)

### Air System User Defined Air Terminal Total Cooling Energy

Energy removed from air loop by user defined air terminal units (Joules)

## System Component Energy Use Outputs

Reporting System Component Energy Use consists of eleven output variables that report the total energy consumption (in Joules) of system components by energy type and coil type. Electric energy consumption is reported for fans, heating and cooling coils in packaged (DX) systems, electric resistance heating coils, humidifiers, evaporative coolers and desiccant dehumidifiers.

### Air System Fan Electric Energy

Electric consumption for fans. (Joules)

### Air System Heating Coil Hot Water Energy

Consumption for heating coil hot water (both purchased and plant supplied) (Joules)

### Air System Cooling Coil Chilled Water Energy

Consumption for cooling coil chilled water (both purchased and plant supplied) (Joules)

### Air System DX Heating Coil Electric Energy

Compressor plus condenser fan electricity use. (Joules)

### Air System DX Cooling Coil Electric Energy

Compressor plus condenser fan electricity use. (Joules)

### Air System Heating Coil Electric Energy

Electricity consumption for the heating coil. (Joules)

### Air System Heating Coil Gas Energy

Gas (Propane and Natural Gas) consumption for the heating coil. (Joules)

### Air System Heating Coil Steam Energy

Steam consumption for the heating coil (steam coils). (Joules)

### Air System Humidifier Electric Energy

Electricity consumption for the humidifiers. (Joules)

### Air System Evaporative Cooler Electric Energy

Electricity consumption for the evaporative coolers. (Joules)

### Air System Desiccant Dehumidifier Electric Energy

Electricity consumption for the desiccant dehumidifiers. (Joules)

The Standard reports file includes these variables reporting in the Tabular report form as "Air Loop System Component Energy Use".

~~~~~~~~~~~~~~~~~~~~

      OutputControl:Table:Style,HTML;

      Output:Table:Monthly,
        Air Loop System Component Energy Use,       !- Name
        ,                        !- DigitsAfterDecimal
        Air System Fan Electric Energy,  SumOrAverage,
        Air System Heating Coil Hot Water Energy,  SumOrAverage,
        Air System Cooling Coil Chilled Water Energy,  SumOrAverage,
        Air System DX Heating Coil Electric Energy,  SumOrAverage,
        Air System DX Cooling Coil Electric Energy,  SumOrAverage,
        Air System Heating Coil Electric Energy,  SumOrAverage,
        Air System Heating Coil Gas Energy,  SumOrAverage,
        Air System Heating Coil Steam Energy,  SumOrAverage,
        Air System Humidifier Electric Energy,  SumOrAverage,
        Air System Evaporative Cooler Electric Energy,  SumOrAverage,
        Air System Desiccant Dehumidifier Electric Energy,  SumOrAverage;

~~~~~~~~~~~~~~~~~~~~

## Overall Air Loop Syntax

A map for Air Loop input syntax is shown in the following diagram.

![Air Loop Input Syntax Map](media/air-loop-input-syntax-map.png)


## AirLoopHVAC:ControllerList

The syntax for the [AirLoopHVAC:ControllerList](#airloophvaccontrollerlist) definition is shown below. As with other lists in EnergyPlus input, the object title and identifying name are followed by type-name pairs. In this case, the types are controller types. The order in which controllers appear on this list also define the priority as described below. The identifying name refers back to the name recorded in the [AirLoopHVAC](#airloophvac) statement.

### Inputs

#### Field: Name

The user designated unique name of an instance of a Controller List. Any object referencing this Controller List will do so using this name.

#### Field Set (Object Type, Controller Name) up to 8

After the identifying name, input for the controller list consists of up to 8 pairs of data items: a controller type and a controller name. The first controller listed has the highest priority, the second the second highest priority, and so forth.

#### Field: Controller <x> Object Type

The type of  controller. This should be a key word defining a class of controllers such as [Controller:WaterCoil](#controllerwatercoil).

#### Field: Controller <x> Name

The name of a controller object (such as a [Controller:WaterCoil](#controllerwatercoil)) defined elsewhere in the input file.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:ControllerList,
               Dual Duct System 1 Controllers,
               Controller:WaterCoil, Main Cooling Coil Controller,
               Controller:WaterCoil, Main Heating Coil Controller;
~~~~~~~~~~~~~~~~~~~~

## AvailabilityManagerAssignmentList

The [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) defines the applicable managers used for an [AirLoopHVAC](#airloophvac) or [PlantLoop](#plantloop). The priority of availability managers is based on a set of rules and are specific to the type of loop. The output from each Availability Manager is an availability status flag. This flag can have the values *NoAction*, *ForceOff*, *CycleOn*, or *CycleOnZoneFansOnly* (used only for air loops). The availability status flags for the Availability Managers referenced by an air or plant loop are used to set the availability status flag for each loop. For the air loops, *ForceOff* takes precedence: if any of the loop's availability managers are showing status *ForceOff*, the loop status will be *ForceOff*. Next in precedence is *CycleOnZoneFansOnly,* followed by *CycleOn*, and *NoAction*. For the plant loops, there is no precedence among the Availability Manager status flag values. Instead, the first availability manager giving a status flag value other than *NoAction* sets the status for the loop. The Availability Managers are executed in Availability Manager List order.

Special rules also apply for which managers may be listed in a Availability Manager list. The Hybrid Ventilation Control Manager (object: [AvailabilityManager:HybridVentilation](#availabilitymanagerhybridventilation)) is a special type of manager and is never specified in a Availability Manager List (it is used stand-alone for a specific air loop). All other types of availability managers may be listed in the Availability Manager List used for AirLoopHVACs. For Plant Loops, the Night Cycle and Night Ventilation managers (objects: [AvailabilityManager:NightCycle](#availabilitymanagernightcycle)  and [AvailabilityManager:NightVentilation](#availabilitymanagernightventilation)) are not allowed in the Availability Manager List.

### Inputs

#### Field: Name

The name of the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist)  object. This is referenced by [AirLoopHVAC](#airloophvac) and [PlantLoop](#plantloop) objects.

#### Field Set (Availability Manager Object Type, Name)

Managers are listed by pairs of data items:  *Availability Manager Object Type* and *Availability Manager Name*. The managers are simulated down the list and calculate a control status for use by the [AirLoopHVAC](#airloophvac) or [PlantLoop](#plantloop). The priority of each manager used for a specific loop is based on the rules described above. Availability managers are not currently used for condenser loops. The availability managers, along with the [AirLoopHVAC](#airloophvac) and [PlantLoop](#plantloop) object, report the control status calculated each simulation timestep. These output variables can be used to prioritize the managers according to the required control strategy. Six managers are accomodated in the list by default, however, this IDD specification is extensible. Additional pairs may be added by directly editing the IDD.

#### Field: Availability Manager <x> Object Type

The key word defining the type of manager, e.g. [AvailabilityManager:NightCycle](#availabilitymanagernightcycle).

#### Field: Availability Manager<x>  Name 

The name of a AvailabilityManager object defined elsewhere in the input file.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManagerAssignmentList,
      Collector Loop Availability Manager List,  !- Name
      AvailabilityManager:HighTemperatureTurnOff,    !- System Availability Manager Type 1
      High Temperature Turn Off Availability Manager,  !- System Availability Manager Name 1
      AvailabilityManager:LowTemperatureTurnOn ,     !- System Availability Manager Type 2
      Low Temperature Turn On Availability Manager,  !- System Availability Manager Name 2
      AvailabilityManager:DifferentialThermostat,    !- System Availability Manager Type 3
      Differential Thermostat Availability Manager;  !- System Availability Manager Name 3
~~~~~~~~~~~~~~~~~~~~

## AirLoopHVAC:OutdoorAirSystem

The Outside Air System (object [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem)) is a subsystem of an [AirLoopHVAC](#airloophvac). It handles the mixed air portion of the primary air system: the system relief air, the outside air inlet, and any components and controllers associated with the system relief air and outside air streams. From the perspective of the primary air loop the Outside Air System is treated as a single component. As a subsystem, it can contain one or more components and controllers.

The input for the Outside Air System consists of a system name, a controller list name, an equipment list name, and an availability manager list name. The controller list simply lists, by type and unique name, all the controllers in the subsystem. The controllers will be simulated in list order. The equipment list lists all the components in the subsystem, by type and name. The equipment is simulated in list order. Finally, the availability manager list gives the type and name of the availability managers used by the subsystem.

The equipment inlet/outlet must be sequential with no loops - the simulation can only handle a straight-through air path, both on the primary air side and on the secondary air side, if any. Heat exchanger secondary air inlets need to be independent of the primary air stream – usually relief air is used.

### Inputs

#### Field: Name

The unique, user assigned name for a single instance of an Outside Air System. Any other object referencing this Outside Air System will use this name.

#### Field: Name: Controller List Name

This field is the name of a [AirLoopHVAC:ControllerList](#airloophvaccontrollerlist) object. A [AirLoopHVAC:ControllerList](#airloophvaccontrollerlist) is simply a list of controllers giving both controller name and type. This Controller List specifies all the controllers that will act on this outside air system. The order of the controllers in the list is significant: controllers are simulated sequentially in the order given in the Controller List. Typically the Controller List would contain a [Controller:OutdoorAir](#controlleroutdoorair). If there are chilled water or hot water coils in the outdoor air system, each such coil will need a [Controller:WaterCoil](#controllerwatercoil).

#### Field: Outdoor Air Equipment List Name

This field is the name of an [AirLoopHVAC:OutdoorAirSystem:EquipmentList](#airloophvacoutdoorairsystemequipmentlist) object. An [AirLoopHVAC:OutdoorAirSystem:EquipmentList](#airloophvacoutdoorairsystemequipmentlist) is simply a list of components giving both component name and type. This Outdoor Air Equipment List specifies all the components that will be simulated in this outside air system. The order of the components in the list is significant: components are simulated sequentially in the order given in the Outdoor Air Equipment List. Typically the equipment list would contain at least an [OutdoorAir:Mixer](#outdoorairmixer). If there is more than one component, the components must be listed in order from the outside air to the [OutdoorAir:Mixer](#outdoorairmixer) (the [OutdoorAir:Mixer](#outdoorairmixer) is last).

#### Field: Availability Manager List Name

This field is the name of a [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) object. An Availability Manager List is a list of Availability Managers giving both Availability Manager Object Type and Name. **Use of this input is optional.** This field may be omitted; it is not currently used by the program.

An IDF example, including the [AirLoopHVAC](#airloophvac), and the controller and equipment lists.

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC,Typical Terminal Reheat 1,
               Reheat System 1 Controllers,
               Reheat System 1 Avail List,
               1.3,
               Air Loop Branches,,
               Air Loop Inlet Node, Return Air Mixer Outlet,
               Zone Equipment Inlet Node, Air Loop Outlet Node;

    AirLoopHVAC:ControllerList,
               Reheat System 1 Controllers,
               Controller:WaterCoil, Main Cooling Coil Controller;

    BranchList, Air Loop Branches,
               Air Loop Main Branch;

    Branch, Air Loop Main Branch,
               1.3,
      ,
               AirLoopHVAC:OutdoorAirSystem, OA Sys 1,
               Air Loop Inlet Node, Mixed Air Node,PASSIVE
               Fan:ConstantVolume, Supply Fan 1,
               Mixed Air Node, Cooling Coil Air Inlet Node, ACTIVE,
               Coil:Cooling:Water:DetailedGeometry, Detailed Cooling Coil,
               Cooling Coil Air Inlet Node, Air Loop Outlet Node, PASSIVE;

    AvailabilityManagerAssignmentList, Reheat System 1 Avail List,
               AvailabilityManager:Scheduled, Reheat System 1 Avail;

    AvailabilityManager:Scheduled, Reheat System 1 Avail,
               FanAndCoilAvailSched;
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:OutdoorAirSystem,
           OA Sys 1,
           OA Sys 1 Controllers,
           OA Sys 1 Equipment;

    AirLoopHVAC:ControllerList,
               OA Sys 1 Controllers,
               Controller:OutdoorAir, OA Controller 1;

    AirLoopHVAC:OutdoorAirSystem:EquipmentList,
               OA Sys 1 Equipment,
               HeatExchanger:AirToAir:FlatPlate,OA Heat Recovery 1,
               OutdoorAir:Mixer, OA Mixing Box 1;
~~~~~~~~~~~~~~~~~~~~

### Outputs

The impact of using outside air/mechanical ventilation is described in the section: Outdoor Air Ventilation Outputs.

## AirLoopHVAC:OutdoorAirSystem:EquipmentList

Used to specify the components in the outdoor air system. The components will be simulated in the order in which they occur in the list.

### Inputs

#### Field: Name

The user designated unique name of an instance of an Air Loop Equipment List. Any object referencing this Air Loop Equipment List will do so using this name.

#### Field Set (Component Object Type,, Component Name) up to 8

After the identifying name, the list consists of up to 8 pairs of data items:

#### Field: Component <x> Object Type

This field specifies the keyword for the type of component used.

#### Field: Component <x> Name

This field is the unique name of the component specified in the previous field. This named object must appear in the IDF.

An example from an IDF:

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:OutdoorAirSystem:EquipmentList,
               OA Sys 1 Equipment,
               HeatExchanger:AirToAir:FlatPlate,OA Heat Recovery 1,
               OutdoorAir:Mixer, OA Mixing Box 1;
~~~~~~~~~~~~~~~~~~~~

## OutdoorAir:Node

The [OutdoorAir:Node](#outdoorairnode) object declares an HVAC system node for outdoor air conditions. The program automatically sets the air conditions at these nodes to correspond to the ambient environmental conditions at the beginning of every timestep. The outdoor air node is typically used as the inlet air node to an HVAC component such as the [OutdoorAir:Mixer](#outdoorairmixer) object. Multiple [OutdoorAir:Node](#outdoorairnode) objects can be used in an input file, however, duplicate node names are not allowed.

The *Height Above Ground* field is used to adjust the weather file air conditions, e.g., outdoor dry-bulb and wet-bulb air temperatures, for atmospheric variation with height. This variation can become a significant factor when modeling tall buildings. See the Engineering Reference section on Atmospheric Variation for a description of the algorithm for variation of atmospheric properties with height. A blank entry or a value less than zero for this field indicates that the height will be ignored and the weather file conditions will be used.

> **OutdoorAir:Node** and **OutdoorAir:NodeList** both set a node to outdoor air conditions. **OutdoorAir:Node** modifies the weather file conditions if a height has been specified. **OutdoorAir:NodeList** does not have a height input and always uses the weather file conditions without modification. The same node name may not be used with both of these objects.

### Inputs

#### Field: Name

The unique name for this outdoor air node.

#### Field: Height Above Ground

The height [m] of the node above ground level. A value greater than zero allows the weather file conditions, e.g., outdoor dry-bulb and wet-bulb air temperatures, to be adjusted according to atmospheric variation with height.

A blank entry or value less than zero indicates that the height will be ignored and the weather file conditions will be used.

An example IDF:

~~~~~~~~~~~~~~~~~~~~

    OutdoorAir:Node,
      OA Node 1;  !- Name

    OutdoorAir:Node,
      Floor 10 Outdoor air Inlet Node,  !- Name
      30.0;  !- Height Above Ground {m}
~~~~~~~~~~~~~~~~~~~~

## Outdoor Air Node outputs:

The ambient dry-bulb air temperature and flow rate at the outdoor air node can be monitored using the system node output variables:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,System Node Temperature [C]
    HVAC,Average,System Node Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

e.g.

~~~~~~~~~~~~~~~~~~~~

    Output:Variable,
    Floor 10 Outdoor air Inlet Node,
    System Node Temperature,
    Hourly;
~~~~~~~~~~~~~~~~~~~~

## OutdoorAir:NodeList

The program needs to know which HVAC system nodes are inlets for outdoor air. Knowing this, the program can set the conditions at these nodes to the outdoor conditions at the start of each major timestep. The [OutdoorAir:NodeList](#outdoorairnodelist) provides the means for specifying which nodes are outdoor air nodes.

The input is flexible: there may be one or more [OutdoorAir:NodeList](#outdoorairnodelist) in each input file. Each list contains up to 25 names. The names can be the name of a unique HVAC system node, or the name of a Node List which will contain the actual node names. Duplicate node names are ignored.

> **OutdoorAir:Node** and **OutdoorAir:NodeList** both set a node to outdoor air conditions. **OutdoorAir:Node** modifies the weather file conditions if a height has been specified. **OutdoorAir:NodeList** does not have a height input and always uses the weather file conditions without modification. The same node name may not be used with both of these objects.

### Inputs

#### Field: Node or NodeList name

The name of an HVAC system node or of a [NodeList](#nodelist) object. There can be up to 25 names.

An example IDF:

~~~~~~~~~~~~~~~~~~~~

    OutdoorAir:NodeList,
        OutsideAirInletNodes;

    NodeList,OutsideAirInletNodes,
           Outdoor air Inlet Node;
~~~~~~~~~~~~~~~~~~~~

## OutdoorAir:Mixer

The [OutdoorAir:Mixer](#outdoorairmixer) is the most common component used in an outdoor air system. The outdoor air mixer has 2 inlet air streams: the system return air and the outdoor air. It has 2 outlet air streams: the system relief air and the mixed air. This is a passive component. It takes the inlet mass flows and conditions plus the relief air mass flow and calculates the mixed air flow rate and conditions (as well as the relief air conditions). The inlet and relief mass flow must be set outside the component – most commonly by an outdoor air controller. The [OutdoorAir:Mixer](#outdoorairmixer) can also be used in compound components such as a fan coil unit.

Input for this object is very simple: a unique name plus the node names of the 2 inlet nodes and the 2 outlet nodes.

### Inputs

#### Field: Name

A unique user assigned name for a particular outdoor air mixer component. Any reference to this component by another object will use this name.

#### Field: Mixed Air Node Name

The name of the HVAC system node which is the outlet for the mixed air stream.

#### Field: Outdoor Air Stream Node Name

The name of the HVAC system node which is the inlet for the outdoor air stream.

#### Field: Relief Air Stream Node Name

The name of the HVAC system node which is the outlet for the system relief air.

#### Field: Return Air Stream Node Name

The name of the HVAC system node which is the inlet for the return air stream.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    OutdoorAir:Mixer,
        Zone1WindACOAMixer,           ! name
        Zone1WindACOAMixerOutletNode, ! mixer outlet node
        Zone1WindACOAInNode,          ! mixer OA node
        Zone1WindACExhNode,           ! mixer relief node
        Zone1WindACAirInletNode;      ! mixer inlet node
~~~~~~~~~~~~~~~~~~~~