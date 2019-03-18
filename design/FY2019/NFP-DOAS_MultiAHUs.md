# **Model DOAS Supplying Air to Inlets of Multiple AHUs**

## Lixing Gu
## Floirda Solar Energy Center

### First draft: 3/11/19

### **Justification for Feature Update:**

This new feature will allow EnergyPlus to model an outside air system supplying ventilation air to multiple air handlers.

Currently a DOAS (dedicated outdoor air system) in EnergyPlus is a normal AirLoopHVAC (aka air loop)  object with the recirculated air flow set to zero. In EnergyPlus the air loops are independent of each other: one air loop can not provide air to another. Outside air is provided to each air system by the AirLoopHVAC:OutdoorAirSystem object. Each outdoor air system can supply only one air loop. All this means that EnergyPlus can't simulate a common HVAC layout: a single multi-floor DOAS serving, through an air shaft, multiple single floor air handling units.

FSEC will implement the new feature based on request shown below:

#### Modal DOAS to Multiple Air Handling Units

o	This feature develops the modeling and simulation approach for a dedicated outdoor air system (DOAS) connected to multiple air handling units (AHUs). Many buildings have a separate DOAS system that feeds outdoor air directly to individual AHUs on each building floor. Currently EnergyPlus can only model a DOAS delivering outdoor air directly to zones or to the inlet or outlet of zone equipment acting as terminal units. This feature will allow a single DOAS to supply air to the outdoor air inlet of multiple air systems.

o	Ability to attach one DOAS to multiple AirLoopHVAC objects would be helpful to model: DOAS connected to multiple rooftop units (or) multiple SZVAV/SZCV units

o	This feature will develop the modeling and simulation approach for a dedicated outdoor air system (DOAS) connected to multiple air handling units (AHUs). Many buildings have a separate DOAS system that feeds outdoor air directly to individual AHUs on each building floor. Currently EnergyPlus can only model a DOAS delivering outdoor air directly to zones or to the inlet or outlet of some zone equipment acting as terminal units. This feature will allow a single DOAS to supply air to the outdoor air inlet of multiple air systems.


### **Conference Call Conclusions**
None

### **Overview**

We propose 3 new objects and a revised object to accomplish the goal. 

####New objects

The proposed new objects are AirLoopHVAC:DedicatedOutdoorAirSystem, AirLoopHVAC:DedicatedOutdoorAirSystem:Mixer, and AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter.

AirLoopHVAC:DedicatedOutdoorAirSystem

The first new object is AirLoopHVAC:DedicatedOutdoorAirSystem. It will take a sum of outdoor air from multiple AirLoopHAVC outdoor units as mass flow rate, pre-treat it through cooling and heating coils, and deliver pre-treated air to outdoor air inlets. The object has following components:

	Object Name	
	Modified AirLoopHVAC:OutdoorAirSystem Object Name
	SetpoinManager:OutdoorAirPreTreat name
	AvalaibilityManager name
	Inlet Node Name
	Outlet Node Name
	AirLoopHVAC:DedicatedOutdoorAirSystem:Mixer name
	AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter Name

	Note:
	AirLoopHVAC:OutdoorAirSystem has a field to provide equipment list with all allowed equipment, such as 
	Heatting and Cooling coils
	A supply fan to deliver  

The structure of the new object is to combine AirLoopHVAC and Branch objects. It does not require supply and demand nodes. In addition, AirLoopHVAC handles zones eventually, while the proposed new object will deal with outdoor air only. The new object also eliminates any possible zone mass balance requirements caused by exhaust fans and others, because the object does not deal with zones directly. Any possible mass balance will be solved inside an AirLoopHVAC itself. 

AirLoopHVAC:DedicatedOutdoorAirSystem:Mixer

The object has a single outlet node and multiple inlet nodes. The outlet node is the inlet node of the AirLoopHVAC:DedicatedOutdoorAirSystem object. The multiple inlet nodes are the relief nodes from Multiple AirLoopHAVC OA mixers.   

AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter

The object has a single inlet node and multiple outlet nodes. The inlet node is the outlet node of the AirLoopHVAC:DedicatedOutdoorAirSystem object. The multiple outlet nodes are the OA inlet nodes from Multiple AirLoopHAVC OA mixers.

####Revised object

The revised object is AirLoopHVAC:OutdoorAirSystem, by adding optional fields to allow connection to the new object: AirLoopHVAC:DedicatedOutdoorAirSystem.

AirLoopHVAC:OutdoorAirSystem

Multiple optional fields will be added at the end of the existing object:

	Number of AirLoopHVAC served by AirLoopHVAC:DedicatedOutdoorAirSystem
	AirLoopHVAC 1 name
	AirLoopHVAC 2 name
	....

####Possible calculation procedure

Here is a possible calculation procedure:

1. Loop AirLoopHVAC to calculate mass flow rate at each OA mixers served by AirLoopHVAC:DedicatedOutdoorAirSystem
2. Assign a sum of OA mass flow rate into AirLoopHVAC:DedicatedOutdoorAirSystem as the object mass flow rate
3. Predict load using outdoor air condition and SetpointManager:OutdoorAirPretreat
4. Calculate outlet node conditions iteratively using the predicted load until the conditions of outlet node of AirLoopHVAC:DedicatedOutdoorAirSystem is very close to the OA inlet node conditions in multiple AirLoopHVAC
5. Assign outlet node conditions into OA mixer OA inlet conditions via AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter

Note:

Since the AirLoopHVAC:OutdoorAirSystem object is listed as a component of AirLoopHVAC:DedicatedOutdoorAirSystem, it will allow an economizer to setup the mass flow rate, in addition to a sum of OA systems of multiple AirLoopHVAC. However, due to the limited time and budget, this feature is not implemented and will be developed later. 

![Figure 1](AirLoopDOAS1.jpg) 

### **Approach**

A new module will be created as SimAirLoopDOAS to handle 3 new objects: AirLoopHVAC:DedicatedOutdoorAirSystem, AirLoopHVAC:DedicatedOutdoorAirSystem:Mixer, and AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter. 

At the same time, other modules will be modified to make proposed new feature work successfully.

####SimAirLoopDOAS

It contains common functions:

#####SimAirLoopDOAS

A driver for this module with external connection

#####GetInput

Read input file and assign inputs to variables and structs

#####Init

Initialize values without changes at the current iteration before calculation. At the same time, predicted load will be calculated between outdoor air conditions and setpoint manager.

#####CalcAirLoopDOAS

Calculate outputs using iteration. This section is similar with the SimOAComponent function in the MixedAir module. The difference is that each component is called with setpoint condition for this particular component in the SimOAComponent function, so that component is controlled individually, while the proposed function will send load calculated in the init function to all compoenent and iterated with partload ratio. Therefore, the proposed function will be converged in all components.
   
#####Sizing

Calculate sizes based on outdoor air conditions and setpoint manager using existing equipment listed in validOASysEquipmentNames.

#####Other support functions

The other functions will be added to meet iteration requirements.

Note: A limited types of cooling and heating coils will be used for this new feature. A full coverage of all types of coils will be enhanced later. 

The code format will be similar with the UnitarySystem module or other module.

In addition, the AirLoopHVAC:OutdoorAirSystem will handle OA rate based on needs from AirLoopHVAC objects and may not have economizer functions for the time being.

####Other revisions

#####AirLoopHVAC:OutdoorAirSystem

The struct of OutsideAirSysProps in the DataAirLoop module will be modified to add two variables, corresponding to new fields:
	int NumAirLoops
 	 Array1D_int AirLoopIndex

The function of GetOutsideAirSysInputs in the MixedAir module will be modified to read new fields

#####OutdoorAir:Mixer

The MixedAir module will be modified to allow OAMixer to accept outlet nodes from AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter as Outdoor Air Stream Node, and inlet nodes of AirLoopHVAC:DedicatedOutdoorAirSystem:Mixer as Relief Air Stream Node.

####Calling

SimAirLoopDOAS will be looped at the end of SolveAirLoopControllers (preferred) or SimAirLoops in the SimAirServingZones module. A addiitonal convergence check will be performed to ensure each OAcontroller gets proper OA flow rates and inlet conditions after calling SimAirLoopDOAS.   

## Testing/Validation/Data Sources ##

A test file with multiple airloops and a single AirLoopHVAC:DedicatedOutdoorAirSystem will be created to ensure DOAS Supplying Air to Inlets of Multiple AHUs will work properly. 


## Input Output Reference Documentation ##

###New objects
AirLoopHVAC:DedicatedOutdoorAirSystem
AirLoopHVAC:DedicatedOutdoorAirSystem:Mixer
AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter

###Revised object
AirLoopHVAC:OutdoorAirSystem
OutdoorAir:Mixer

## Input Description ##

###New objects
AirLoopHVAC:DedicatedOutdoorAirSystem

	AirLoopHVAC:DedicatedOutdoorAirSystem,
       \min-fields 10
       \memo Defines a central forced air system to provide dedicated outdoor air to multiple
       \memo AirLoopHVACs.
   	A1, \field Name
       \required-field
       \type alpha
       \reference DOASAirLoops
   	A2, \field AirLoopHVAC:OutdoorAirSystem Name
       \note Enter the name of an AirLoopHVAC:OutdoorAirSystem object.
       \type object-list
       \object-list validBranchEquipmentNames
   	A3, \field Availability Manager List Name
       \note Enter the name of an AvailabilityManagerAssignmentList object.
       \type object-list
       \object-list SystemAvailabilityManagerLists
   	A4, \field SetpointManager:OutdoorAirPretreat Name
       \note Name of a SetpointManager:OutdoorAirPretreat object used to determine object loads
       \note for equipment listed in AirLoopHVAC:OutdoorAirSystem
       \required-field
       \type object-list
   	A5, \field Inlet Node Name
       \note Name of inlet node where air enters the dedicated ourdoor air loop.
       \required-field
       \type node
   	A6, \field Outlet Node Name
       \note Name of outlet node where air leaves the dedicated ourdoor air loop.
       \required-field
       \type node
   	A7, \field AirLoopHVAC:DedicatedOutdoorAirSystem:Mixer Name
       \note Name of AirLoopHVAC:DedicatedOutdoorAirSystem:Mixer.
       \object-list AirLoopHVACDedicatedOutdoorAirSystemMixerNames
       \required-field
       \type node
   	A8; \field AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter Name
       \note Name of AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter.
       \object-list AirLoopHVACDedicatedOutdoorAirSystemSplitterNames
       \required-field
       \type node

AirLoopHVAC:DedicatedOutdoorAirSystem:Mixer

	AirLoopHVAC:DedicatedOutdoorAirSystem:Mixer,
       \extensible:1 Just duplicate last field and comments (changing numbering, please)
       \memo Mix N inlet air streams from Relief Air Stream Node in OutdoorAir:Mixer objects 
       \memo served by AirLoopHVAC objects listed in AirLoopHVAC:OutdoorAirSysteminto one 
       \memo (currently 500 per air loop, but extensible). Node names cannot
       \memo be duplicated within a single mixer list.
   	A1, \field Name
       \required-field
       \reference AirLoopHVACDedicatedOutdoorAirSystemMixerNames
   	A2, \field Outlet Node Name
       \required-field
       \type node
   	A3, \field Inlet 1 Node Name
       \begin-extensible
       \required-field
       \type node
   	A4, \field Inlet 2 Node Name
       \type node
   	A5, \field Inlet 3 Node Name
       \type node
   	A6, \field Inlet 4 Node Name
       \type node
   	A7, \field Inlet 5 Node Name
       \type node
   	A8, \field Inlet 6 Node Name
       \type node
   	A9, \field Inlet 7 Node Name
       \type node
   	A10, \field Inlet 8 Node Name
        \type node
   	A11, \field Inlet 9 Node Name
        \type node
   	A12, \field Inlet 10 Node Name
        \type node
   	A13, \field Inlet 11 Node Name
        \type node
    ........

AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter

	AirLoopHVAC:DedicatedOutdoorAirSystem:Splitter,
       \extensible:1 Just duplicate last field and comments (changing numbering, please)
       \memo Split one air stream from AirLoopHVAC:DedicatedOutdoorAirSystem outlet node into N \memo outlet streams (currently 500 per air loop, but extensible).  Node names
       \memo should be Outdoor Air Stream Node Name in OutdoorAir:Mixer objects served by
       \memo AirLoopHVAC objects listed in AirLoopHVAC:OutdoorAirSystem.
   	A1, \field Name
       \required-field
       \reference AirLoopHVACDedicatedOutdoorAirSystemSplitterNames
   	A2, \field Inlet Node Name
       \required-field
       \type node
   	A3, \field Outlet 1 Node Name
       \begin-extensible
       \required-field
       \type node
   	A4, \field Outlet 2 Node Name
       \type node
   	A5, \field Outlet 3 Node Name
       \type node
   	A6, \field Outlet 4 Node Name
       \type node
   	A7, \field Outlet 5 Node Name
       \type node
   	A8, \field Outlet 6 Node Name
       \type node
   	A9, \field Outlet 7 Node Name
       \type node
    ......

###Revised objects

AirLoopHVAC:OutdoorAirSystem

	AirLoopHVAC:OutdoorAirSystem,
       \memo Outdoor air subsystem for an AirLoopHVAC. Includes an outdoor air mixing box and
       \memo optional outdoor air conditioning equipment such as heat recovery, preheat, and precool
       \memo coils. From the perspective of the primary air loop the outdoor air system is treated
       \memo as a single component.
       \min-fields 3
   	A1, \field Name
       \required-field
       \type alpha
       \reference-class-name validBranchEquipmentTypes
       \reference validBranchEquipmentNames
   	A2, \field Controller List Name
       \note Enter the name of an AirLoopHVAC:ControllerList object.
       \required-field
       \type object-list
       \object-list ControllerLists
   	A3, \field Outdoor Air Equipment List Name
       \note Enter the name of an AirLoopHVAC:OutdoorAirSystem:EquipmentList object.
       \required-field
       \type object-list
       \object-list AirLoopOAEquipmentLists
   	A4, \field Availability Manager List Name
       \note Enter the name of an AvailabilityManagerAssignmentList object.
       \type object-list
       \object-list SystemAvailabilityManagerLists
	N1, \field Number of AirLoopHVAC
       \type integer
       \note Enter the number of the AirLoopHAVC served by AirLoopHVAC:DedicatedOutdoorAirSystem
   	A5, \field AirLoopHVAC 1 Name
      \note The rest of fields are optional and extensible. It requires AirLoopHVAC names served by   
      \note an AirLoopHVAC:DedicatedOutdoorAirSystem.
      \begin-extensible
       \type AirLoopHVAC name
   	A6, \field AirLoopHVAC 2 Name
       \type AirLoopHVAC name
   	A7, \field AirLoopHVAC 3 Name
       \type AirLoopHVAC name
   	A8, \field AirLoopHVAC 4 Name
       \type AirLoopHVAC name
   	A9, \field AirLoopHVAC 5 Name
       \type AirLoopHVAC name
   	A10, \field AirLoopHVAC 6 Name
       \type AirLoopHVAC name
    ......


	OutdoorAir:Mixer,
      \memo Outdoor air mixer. Node names cannot be duplicated within a single OutdoorAir:Mixer
      \memo object or across all outdoor air mixers.
  	A1, \field Name
      \required-field
      \type alpha
      \reference OutdoorAirMixers
      \reference-class-name validOASysEquipmentTypes
      \reference validOASysEquipmentNames
  	A2, \field Mixed Air Node Name
      \note Name of Mixed Air Node
      \required-field
      \type node
  	A3, \field Outdoor Air Stream Node Name
      \note Name of Outdoor Air Stream Node
      \required-field
      \type node
  	A4, \field Relief Air Stream Node Name
      \note Name of Relief Air Stream Node
      \required-field
      \type node
  	A5; \field Return Air Stream Node Name
      \note Name of Return Air Stream Node
      \required-field
      \type node


## Outputs Description ##

insert text

## Engineering Reference ##

insert text

## Example File and Transition Changes ##

insert text

## References ##

insert text

# **Reference: DesDoc_OASysToMultAirHandlers2.md**

# **Dedicated OA System Serving Multiple Air Handlers**
## **Fred Buhl LBNL 1/20/2016**
### **Justification for Feature Update:**
This new feature will allow EnergyPlus to model an outside air system supplying ventilation air to multiple air handlers.

Currently a DOAS (dedicated outdoor air system) in EnergyPlus is a normal AirLoopHVAC (aka air loop)  object with the recirculated air flow set to zero. In EnergyPlus the air loops are independent of each other: one air loop can not provide air to another. Outside air is provided to each air system by the AirLoopHVAC:OutdoorAirSystem object. Each outdoor air system can supply only one air loop. All this means that EnergyPlus can't simulate a common HVAC layout: a single multi-floor DOAS serving, through an air shaft, multiple single floor air handling units.

### **Conference Call Conclusions**
#### **Jan 06, 2016**
*  Should the DOAS serving multiple air handlers be modeled on the AirLoopHVAC object or the AirLoopHVAC:OutdoorAirSystem? Consensus seemed to be on the AirLoopHVAC:OutdoorAirSystem. 
*  The DOAS will be supplying ventilation air. To allow economizer action, each  AirLoopHVAC:OutdoorAirSystem it connects to will need 2 outside air inlets. One for the DOAS connection and one for direct access to outside air for economizer action.
*  Should the DOAS have branches or just in/out nodes like OutdoorAirSystem? I favor directly replicating OutdoorAirSystem.
#### **June 25, 2016**
* Contacted Fred W. Porter at NORESCO. Here are his comments
 * The DOAS AHU mostly needs to control discharge air temps, preheat, dehumidify, possibly w/reheat, and these days utilize energy recovery. The OA flow to each AHU may vary based on DCV and AHU scheduling and so the OA through the DOAS needs to vary at the same time.  The exhaust to the DOAS will almost always include toilet exhaust and may include some relief air. EA back to the DOAS will usually be 20% less than the OA so this has a very significant effect on performance. These units are sometimes the same as the unit supplying OA to zonal fan coils, etc, or directly to a few zones, so the solution should really be a flexible extension of that DOAS representation I think. DOAS units for AHUs typically won’t try to achieve “neutral” DAT however, so usually won’t have reheat coils, wraparound heat pipes, or passive dehumidification wheels. (As soon as I say  typically or usually, I get some design with the exact opposite within a week.) They almost always deliver the air to  the “inlet,” i.e. the mixed air plenum of the recirc AHU.
* Larry Scheier confirmed that TRACE allows economizers on the AHU's in DOAS to AHU configurations. This means we will definitely need to add another outside air node (the economizer node) to the OutdoorAir:Mixer.  Brent Griffith suggested that we add it at the end of the input fields, allowing existing input files to work as is. 
* From Fred W. Porter's comments, it is clear that many DOA systems feed zones and zone equipment directly  as well as air handling units. Brent suggested handling this by defining a separate air loop (AirLoopHVAC plus zone demand side) to handle this. This seems like a good solution to me.

### **Overview**
#### Configurations

Given the feedback in the comments section, we will be attempting to simulate a dierct outside air system (DOAS), typically on the building's roof, serving multiple air handling units (AHUs), typically on separate floors. The DOAS can also serve some zones directly. Each AHU will be able to operate in economizer mode by using an additional outside air node for the economizer air (equivalent to an outside air damper that can fully close.

#### EnergyPlus description
We will use the existing EnergyPlus object AirLoopHVAC:OutdoorAirSystem to describe the DOAS serving multiple air handlers. Let's call this use of AirLoopHVAC:OutdoorAirSystem an Outdoor Air System to Multiple Air Handlers (OAStoMAH) vs Outdoor Air System in Single Air Loop (OASinSAL). Just as  AirLoopHVAC:OutdoorAirSystem can currently supply treated outdoor air to a single AirLoopHVAC the new use of  AirLoopHVAC:OutdoorAirSystem will supply treated outdoor air to multiple AirLoopHVAC's. This would correspond in the real world to a DOAS supplying multiple air handlers. In the EnergyPlus model each AirLoopHVAC will still need an AirLoopHVAC:OutdoorAirSystem. But additionally an AirLoopHVAC:OutdoorAirSystem can connect to each AirLoopHVAC through the outdoor air inlet of the OutdoorAir:Mixer contained in its AirLoopHVAC:OutdoorAirSystem.

Since the OAStoMAH will supply multiple AirLoopHVAC's, we need a splitter to divide its output among the AirLoopHVAC's. Similarly we will need to use a mixer to combine the relief air coming from the AirLoopHVAC's and send it to the OAStoMAH. We will borrow from AirLoopHVAC:ZoneSplitter and AirLoopHVAC:ZoneMixer but will define new objects AirLoopHVAC:OutdoorAirSystem:Mixer and AirLoopHVAC:OutdoorAirSystem:Splitter. As noted above, we will add an additional outside air node to Outdoor:AirMixer to allow for economizer action.

### **Approach**
The demand side (the AirLoopHVAC's) will set the ventilation air flow rate for the OAStoMAH. The OAStoMAH can't be simulated without knowing how much ventilation air the AirLoopHVAC's need. On the other hand the AirLoopHVAC's can't be simulated without knowing the temperature and humidity ratio of the ventilation air being supplied to them. This appears to be a situation that requires iteration between the OAStoMAH and the AirLoopHVAC's that it is supplying, with an accompanying impact on solution complexity and run time.

*For simple ventilation air calculations*, including basic demand control ventilation, it is possible to separate the ventilation air flow demand calculation from the AirLoopHVAC simulation. This calculation currently resides in the subroutine *CalcOAController* in the module *MixedAir*. The calculation takes place when the *AirLoopHVAC:OutdoorAirSystem* is simulated during the simulation of the air loop. We propose to separate this calculation out of CalcOAController and place it in a separate module. The plan is then to:

1. calculate the ventilation demand from all the zones served by the air loops that are served by each  OAStoMAH;
2. Simulate the OAStoMAH;
3. simulate the AirLoopHVAC's pretty much as is done currently;

Note that the calculation of the zone exhaust air flow rate will need to be "moved up" in the simulation sequence since the zone exhaust can set a hard lower limit on the ventilation air flow rate for an air loop. This provides an opportunity to move the zone exhaust air into the function *CalcAirFlowSimple* (invoked from *ManageHVAC* and elsewhere). The zone exhaust air flow could then be balanced against infiltration and simple ventilation air flow for all the zones in an air loop. If the zone exhaust exceeds the total of zone infiltration, simple zone ventilation, and air loop ventilation air flow induced infiltration could be apportioned to the zones in the air loop *before* the actual HVAC simulation takes place. This would eliminate the nagging problem of unbalanced zone exhaust flow. **Note:** *isn't this already being done or partially done?*

*For more complex ventilation demand calculations*, such as the system-based ventilation rate procedure, the ventilation demand depends on knowing the system supply air flow rate, which requires simulating the air loop and zone equipment. There are several ways to proceed:

1. use the last HVAC time step values in the ventilation rate procedure calculation;
2. use the last time step values for the air loop ventilation air inlet conditions, simulate the air loops, simulate the OutdoorAirSystemHVAC, then simulate the air loops again;
3. create some general iterative procedure.

Here we will start seeing some impact on run time. Our proposal is to use method 1 to start with

*In summary* there will need to be considerable rearranging of the current high-level HVAC simulation code, but with considerable benefit in increased capability and modest increase on run time or solution complexity. We hope to be able to reuse much of the code currently used for simulating AirLoopHVAC:OutdoorAirSystem's.

*Sizing:* we propose to use the existing *Sizing:System* object for  OAStoMAHs. The OAStoMAHs will be sized to the sum of its air loop design minimum ventilation air flow rates.
### **Implementation**
#### High Level Scheme
* Simulate AirLoopHVAC:OutdoorAirSystem:Mixer's. Call the data array OASysMixer. 
  1.  Loop over the AirLoopHVAC:OutdoorAirSystem:Mixer's
       * Loop over the inlet nodes (air loop OA mixer relief nodes). Combine the inlet flows, temperatures, humidity ratios etc. and put the results on the outlet node (an OAStoMAH return node.
* Loop over AirLoopHVAC:OutdoorAirSystem's. If it is a OAStoMAH, simulate it. If it is a OASinSAL, skip.
  1. Loop over PrimaryAirSystem's served by each OAStoMAH.
      * Call SimOAController().
  2. Sum the AirLoopFlow().MinOutAir flows and place on the inlet node of the OAStoMAH.
  3. Simulate the OAStoMAH. Place the outlet node results on the corresponding inlet node of the AirLoopHVAC:OutdoorAirSystem:Splitter or the outside air node of a DOAS air loop.
  4. Simulate the AirLoopHVAC:OutdoorAirSystem:Splitter.
       * Pass the conditions ( temperature, humidity ratio, etc.) on to the outlet nodes. Split the flow according to the AirLoopFlow().MinOutAir flows already calculated.

We're done! Now we move on to the existing calculation.

Except there are some issues.

1. As noted before, we're going to have to precalculate  exhaust air. The AirLoopHVAC:OutdoorAirSystem:Splitter will be able to accept air flow from exhaust fans. And SimOAController() needs to know the exhaust flow.
2. The current AirLoopHVAC:OutdoorAirSystem simulation doesn't simulate Controller:WaterCoil's. Instead it "moves them up" to to the encompassing air loop's controller list. It puts them first in the list so they are simulated first, so all is well. Obviously this won't work for OAStoMAH's, although we can retain it for OASinSAL's. We will need to call the Controller:WaterCoil iteration scheme in order to control the water coils. This adds some complexity to the use of AirLoopHVAC:OutdoorAirSystem's, but should not be too difficult.
#### Schematic Input

    OutdoorAirSysHVAC
      Name
      Controller List Name
      Equipment List Name
      Availability Manager List Name
      Splitter Name
      Mixer Name

    OutdoorAirSysHVAC:Mixer
      Name
      List of Air Loops

    OutdoorAirSysHVAC:Splitter
      List of Air Loops

*Question*: Do we need "List of Air Loops" twice? Aren't the the same? Then it could go at the end of OutdoorAirSysHVAC and we could automatically generate the splitter and mixer. This would mess up auto-drawing the configuration? Notice we are already auto-generating all node connections. Nodes would not occur as input in this scheme. They would be extracted from the components and the air loops. This means that every component that could occur in a DOAS would need a *GetMyOutletAirNode* function. Is this the route that we are going? Otherwise we will need some node name inputs. Note that AirLoopHVAC:ZoneSplitter etc. have node inputs. Should we just copy them for consistency? Connector:Splitter and Connector:Mixer use branches, which OutdoorAirSysHVAC doesn't have.

*Answer* The lists in the Mixer and Splitter might not be the same. For instance, we might want to add exhaust fan flow to the mixer. Then it would be a list of air loops and exhaust fans. It was agreed that use of node names in the input should be deprecated.



*Question*: What about DOAS direct to zone? You've only mentioned air loops.

*Answer* These will zones need to be lumped into a "do nothing" air loop by the user. I suspect component Duct will come in handy.

#### Schematic Input: 2nd try
Instead of defining a new OutAirSysHVAC object, I've decided to reuse AirLoopHVAC:OutdoorAirSystem. We will add 2 optional fields on the end for the splitter and mixer. We will distinguish the 2 types (serving 1 air loop or serving multiple air loops) in a type field in the data structure. Our input will now look like:

    AirLoopHVAC:OutdoorAirSystem
      Name
      Controller List Name
      Equipment List Name
      Availability Manager List Name
      Splitter Name
      Mixer Name

    AirLoopHVAC:OutdoorAirSystem:Splitter
      Name
      List of Air Loops

    AirLoopHVAC:OutdoorAirSystem:Mixer
      Name
      List of Air loops and exhaust fan outlet nodes



### **Testing/Validation Source(s):**
Creating an example / test file for this new capability will be a major task in itself. We will most likely start with RefBldgMediumOfficeNew2004_Chicago. Once we have this file, we will use it to compare a "do nothing" OutdoorAirSystemHVAC with the case where there is no OutdoorAirSystemHVAC. We can also compare cases where the ventilation air conditioning is done within the air loop versus where it is done by an OutdoorAirSystemHVAC. 

### **IO Ref (draft)**

We propose a new object which will be similar to AirLoopHVAC:OutdoorAirSystem. Also new objects OutdoorAirSystemHVAC:Mixer and OutdoorAirSystemHVAC:Splitter.

#### **Proposed Repart Variable Names:**

#### **Proposed additions to meters:**

### **EngRef (draft)**
We aren't far enough along yet to do this. The *Air Loop* and *Air Loop Simulation* sections will need extensive revising.

### **Example file and Transition changes:**

### **Other documents:**


