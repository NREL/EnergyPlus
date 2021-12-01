Dedicated Exhaust System for Flexible Exhausts Configurations
=============================================================

**J. Yuan & M.J. Witte, GARD Analytics**

 - Original Date: July 30, 2021
 - Revised: August 2, 2021, revise and start design
 - Revised: Dec 3, 2021, 

## Justification for New Feature ##

This new feature intends to provide a convenient way for multiple exhausts in different AirLoops to be combined to a central exhaust system. The need for this proposed feature stems from the practice of modeling general exhaust with a central exhaust duct. In a typical configuration, airflows are applied to offices and laboratories spaces from multiple airloops, and then the return or exhaust airflows are re-routed to different exhaust systems due to different exhaust heat recovery processes for the office spaces the lab spaces exhausts. This configuration can be shown in the following example (credit: the figure is provided by Sagar Rao at Affiliated Engineers Inc., the original requester of the feature): 

![Exhaust System Configuration](Dedicated_Airstream.png)

Currently modeling this type of configuration in EnergyPlus requires workarounds (to be described in the overview section) and it quite cumbersome to implement for large projects. According to the original requester, the workaround method could also complain about missing “exhaust fans” in the energy report  the  during compliance reviews (e.g. CA Title-24, pp. 257, 2019 version [1], or ASHRAE 90.1 [2]). This calls for an easier way to model a such re-routed exhaust streams, which would also allow more accurate part loads calculation and reporting on the exhaust fans. The proposed work is thus about implementing such a new feature that will allow more flexible exhaust streams routing to meet the modeling needs described above. 

Another application for such exhaust systems is health care. ANSI/ASHRAE/ASHE Standard 170-2017 [3] Ventilation of Health Care Facilities specifies that certain types of spaces have a requirement that "All Room Air Exhausted Directly to Outdoors," e.g. ER waiting rooms, laboratories, laundry, etc. This adds the requirement for exhaust flow to track supply flow which is not currently possible at the zone level.

## E-mail and Conference Call Conclusions ##

### E-mail Communications ###

- A few email exchanges with the original requester prior to and during the development of the NFP to clarify the development needs;
- Comment in [pull request #9925](https://github.com/NREL/EnergyPlus/pull/8925#issuecomment-891994628) regarding application for health care spaces with requirement that "All Room Air Exhausted Directly to Outdoors."

### Conference Call Communications ###

- A discussion at the Technicality conference call occurred before the development of the current NFP;

- An informal phone conversation with the original requester was conducted during the development of the current NFP about the development needs and the current workaround method.

## Overview ##

A few existing EnergyPlus modules are investigated for their potential capabilities for implementing the exhaust systems configurations mentioned above. Here the capabilities and limitations of each of these solutions (of trying to use existing modules) are discussed, and will be used to develop a new strategy for developing the new feature. 

The first candidate to model such a system is to use the AirLoopHVAC:Mixer object, which needs to be used in an Dedicated Outdoor Air System (AirLoopHVAC:DedicatedOutdoorAirSystem). When using this object, multiple OutdoorAir:Mixer relief air nodes can be connected together with the AirLoopHVAC:Mixer to form one common exhaust outlet (to the outdoor) or to a heat recovery device. This seems to be a good candidate for combining multiple relief air streams to a general exhaust. However, the limitation is that the AirLoopHVAC:Mixer is only intended to be used by (and with) the AirLoopHVAC:DedicatedOutdoorAirSystem object, and it cannot be used for regular non-DOAS airloop system. Further, it is not intend to use with multiple return loops that span across multiple (DOAS) airloops, similar to the "intertwined" configuration shown in the example figure above.

The second potential candidate for modeling such an exhaust system is the AirLoopHVAC:ZoneMixer object to configure an AirLoopHVAC:ReturnPath object. The zone mixer will be able to combine the return airflow of multiple zones into one common outlet for a return path. The current limitation of this method is that the "return path" in general ends at the point where the collected return air combines into a central return duct. The exhaust path from the zones could take another path with the existing "return path". This configuration is not intend to use with multiple return loops that span across multiple airloops, either.

A third potential candidate is using the general Connector:Mixer object to develop the airloop topography. The Connector:Mixer will taken several "branches" and connect them to a common outlet branch. This means that the Connector:Mixer actually works on the branch level and explicitly branches need to be defined for each incoming and outgoing connections, which will be cumbersome to use when trying to connect many zones on larger projects. The configuration is also subject to the restrictions that the airloop system that multiple routed exhaust system is difficult to be correctly recognized by EnergyPlus. A few experiment trials were conducted to establish such a system using the method. However, although the branch-node topology looked correct, the air loop would not correctly simulate the exhaust airflows in such branch-node connections, due to the upstream-to-downstream modeling procedure of the air loop. 

Currently, the original requester for the feature uses a workaround that uses EMS sensors to gather the different zone exhaust information and "virtually" mixed them together via some pieces of EMS program code, and then feed the gather flow information to a "dummy" actuatable system to accomplish the goal of modeling such a configuration. 

Based on the existing modules' capabilities and limitations, we proposed to add two new IDF objects to model such a  re-routed and recombined exhaust system configuration, and to allow quick setup and scaling up of such a configure for larger simulation projects.

## Approach ##

### Exhasut System ###

An AirLoopHVAC:ExhaustSystem is defines how the exhaust air goes out from the airloop(s). It is somehow similar to a "Return Path", as an "Exhaust Path" here.  However, it is different from the return path in that the exhaust path usually come with a central exhasut fan. It will also allow individual zone's exhaust to be specified in details as a zone equipment.  

The following new objects will be added to allow an AirLoopHVAC:ExhaustSystem to be specified: 
```
AirLoopHVAC:ExhaustSystem,
    Central Exhaust,            !- Name
    Exhaust Avail List,         !- Availability Manager List Name
    Fan:SystemModel,            !- Fan Object Type
    Central Exhaust Fan,        !- Fan Name
    Zone 1 ExhaustSystem Node,        !- Inlet Node 1
    Zone 2 ExhaustSystem Node,        !- Inlet Node 2
    Zone 3 ExhaustSystem Node,        !- Inlet Node 3
    Zone 4 ExhaustSystem Node;        !- Inlet Node 4
```

The central fan model for this object needs to be either FAN:SYSTEMMODEL or FAN:COMPONENTMODEL. The regular fan models such as Fan:OnOff, Fan:ConstantVolume, or Fan:VariableVolume could not be used with the current object.

#### Implicit Mixer object similar to AirLoopHVAC:ZoneMixer or AirLoopHVAC:Mixer ####

With the way how the new AirLoopHVAC:ExhaustSytem is configured above, it contains an implicit mixer object will connect the airflows coming through the exhaust inlets (the inlet nodes in the input) and mix them to the central exhaust fan inlet. The mixed air outlet will be the central Exhasut fan's inlet node. In the current development, this choice would allow an implicit mixer to connect the incoming exhaust streams to the central exhaust fan in the exhaust system. 

### ZoneHVAC:ExhaustSystem ###

The inlet nodes inputs in the AirLoopHVAC:ExhaustSystem object are the exit nodes of the newly added ZoneHVAC:ExhaustSystem objects, which will be introduced here.
Each zone that connected to an inlet of inlet node of the AirLoopHVAC:ExhaustSystem is required to have a ZoneHVAC:ExhaustSystem to described the zone exhaust connections, designs, and controls. 

One piece of important information about each of the individual zone exhausts is that there should be at least some information about the design flow rate, which might be important for sizing and simulations. This should be based on either a design (exhaust) flow rate by input or via zone exhaust fan (design flow) inputs. 
The ZoneHVAC:ExhaustSystem object is going to be added to describe the exhaust design flow information:

```
ZoneHVAC:ExhaustSystem,
    Zone2 Exhaust System,           !-Name
    HVACOperationSchd,              !- Availability Schedule Name
    Zone2 Exhaust Node,             !- Inlet Node Name
    Zone2 ExhaustSystem Node,       !- Outlet Node Name
    0.1,                            !- Design Flow Rate {m3/s}
    Scheduled,                      !- Flow Control Type (Scheduled, Passive, FollowSupply, ????)
    Zone2 Min Exhaust Flow Sched,   !- Minimum Flow Fraction Schedule Name
    Zone2 Exhaust Flow Sched,       !- Flow Fraction Schedule Name
    ,                               !- Supply Node or NodeList Name (used with FollowSupply control type)
    ,                               !- System Availability Manager Name
    ,                               !- Minimum Zone Temperature Limit Schedule Name
    FlowBalancedSched;              !- Balanced Exhaust Fraction Schedule Name
```

#### ZoneHVAC:ExhaustSystem as Zone Equipment ####

The newly ZoneHVAC:ExhaustSystem is considered to be an zone equipment, and can be added to the zone equipment list. 

It is also possible to included multipel ZoneHVAC:ExhaustSystem for a single zone, correponding to the design that multiple exhausts could goes together into a central exhaust system, such as mutiple exhaust hoods in a laboratry [4, 5]. 

### Reporting ###

One issue for using the methods above is which Air loop should the exhaust system belongs when it spans across more than one air loops. For reporting, a new section of the tabular report will be added to list the operations of the exhaust systems. For each exhaust system, the (design, max, and min) central exhaust flow, temperature and humidity conditions will be added to a tabular Table named "General Exhaust Systems". For each exhaust system reported, the flow conditions at each indivual exhaust will also be reported.  

### IDD changes ###

The following IDD blocks will be added to the Energy+.idd file.

#### IDD Addition for AirLoopHVAC:ExhaustSystem ####

After the AirLoopHVAC:ReturnPath block and before the AirLoopHVAC:ExhaustMixer (to be added) blocks:
```
AirLoopHVAC:ExhaustSystem,
       \extensible:1 - Just duplicate last field (changing numbering, please)
       \memo define the general exhaust systems with 
       \memo a central exhaust fan
  A1 , \field Name
       \required-field
	   \note Name of the general exhaust system
  A2 , \field Availability Schedule Name
       \note Availability schedule name for this zone exhaust system. Schedule value > 0 means it is available.
       \note If this field is blank, the exhaust system is always available.      
       \type object-list
       \object-list ScheduleNames
  A3 , \field Fan Object Type
       \required-field
       \type choice
       \key Fan:SystemModel
       \key Fan:ComponentModel
  A4 , \field Fan Name
       \required-field
       \type object-list
       \object-list FansSystemModel
       \object-list FansComponentModel
  A5 , \field Inlet Node 1 Object Type
       \begin-extensible  
       \note Inlet Node Name
  A6 , \field Inlet Node 2 Object Type
       \note Inlet Node Name
  A7 , \field Inlet Node 3 Object Type
       \note Inlet Node Name
  A8 , \field Inlet Node 4 Object Type
       \note Inlet Node Name
```

#### #### IDD Addition for ZoneHVAC:ExhaustSystem ####

At the end of the `ZoneHVAC Forced Air Units` group (or another position might be at the end of the `Zone HVAC Air Loop Terminal Units` group):

```
ZoneHVAC:ExhaustSystem,
       \memo Define dedicated exhaust systems that 
       \memo combines exhausts of multiple AirLoopHVAC systems
  A1 , \field Name
       \required-field
  A2 , \field Availability Schedule Name
       \note Availability schedule name for this exhaust system. Schedule value > 0 means it is available.
       \note If this field is blank, the exhaust system is always available.      
       \type object-list
       \object-list ScheduleNames
  A3 , \field Inlet Node Name
       \note Inlet node name for the exhaust system
  A4 , \field Outlet Node Name
       \note Outlet node name for the exhaust system
  N1 , \field Deisgn Exhaust Flow Rate [m3/s] 
       \note Design Exhaust Flow Rate
  A5 , \field Flow Control Type
       \note Control type of the exhaust fan
       \type choice
       \key Scheduled
       \key Passive
       \key FollowSupply
  A6 , \field Minimum exhaust flow fraction schedule name
       \note Schedule name of the minimum exhaust flow fraction
       \type object-list
       \object-list ScheduleNames
  A7 , \field Exhaust flow fraction schedule name
       \note Schedule name of the exhaust flow fraction
       \type object-list
       \object-list ScheduleNames
  A8 , \field Supply Node or NodeList Name
       \note To be used with FollowSupply control type)
  A9, \field System Availability Manager Name
       \type object-list
       \object-list AvailabilityManagerNames
  A10, \field Minimum Zone Temperature Limit Schedule Name
       \note Schedule name of the Minimum Zone Temperature Limit
       \type object-list
       \object-list ScheduleNames
  A11, \field Balanced Exhaust Fraction Schedule Name
       \note Schedule name of the Balance Exhaust Fraction
       \type object-list
       \object-list ScheduleNames
```

### Air mass and heat balance ###

For the traditional air loop supply and return paths assumes that the supply and return occurs within the same air loop. However, the heat and mass balance setup is more complicated when the exhaust system could potentially span across more than one air loop systems. The mass balance needs to be reconsidered for such a configuration. 

The heat and mass balance of the new exhaust system will properly connect and combine the individual incoming node's flow rate and conditions, to form the output for the common outlets under different scenarios. The heat transfer involves temperature and enthalpy balance, especially when the central exhaust air goes to another sensible or total heat recovery device. Further, if the concentration solving for the contaminants is turned on for the simulation, the contaminant (e.g. CO2, generic contaminants etc) mass balance also needs to be established for the newly added exhaust system. 

For a simpler scenario, the each individual zone would have the return air volume based on the zones' individual load conditions (as well as the supply flow conditions). The general exhaust would them be a proper aggregation of the individual zones' return or exhaust air conditions. For a more complicated scenario, for example when the exhaust fan is maintaining a certain amount of constant exhaust flow, the individual node's return conditions would also be re-balanced and (iteratively) develop a balanced flow rate and exhaust condition. 

### Controls and operation modes ###

For the control and operation modes of such an exhaust system, two scenarios will be considered: 

1. The first scenario will cover the mode where the central exhaust flow will be driven by the upstream airflow rates; in this case the exhaust system main flow will be determined by  the individual the branches' flow rates and fraction schedules.

2. The second operation mode would consider that the exhaust fan can operate at a given flow rate, while the upstream zones may need re-balancing if one or more upstream zone exhausts are not actively controlled. In this scenario, the exhaust system will impact the upstream zone exhaust flow rate and a re-balancing scheme will be need to re-balance the exhaust flow rate in the upstream branch(es).

### Sizing ###

The sizing for the central exhaust fan would depend on the sum of the individual branches' design flow rates. A reasonable  sizing scheme needs to be developed to size the central exhaust fan, by looking up the upstream information such as exhaust capacities, fraction schedules, and design flow requirements.

### Reporting ###

Although the central exhaust fan has its own report as an individual component, it would be beneficial to also have the exhaust fan included in the HVAC system or airloop reports. In the proposed development, the related central exhaust fan outputs, such fan flow rate and energy usage, will be added to each of the airloops that the exhaust is connected to.

## Testing and Validation ##

A few unit tests will be developed to verify that: 
1. the new input objects can be processed correctly, via one or two unit test case(s);
2. the zone, branch, and exhausts mass air flow balances, via one or more unit test case(s);
3. the exhaust system results, output variables, and reports are working properly via one unit test.

## Example File and Transition Changes ##

One new example file will be added to the test suite to demonstrate how to use this feature. 

Since the feature is based on completely newly added blocks, an older version would not carry the feature. Therefore a transition program is not needed for converting from earlier versions.

## Input Output Reference Documentation ##

The proposed new feature development will add the following contents to the Input Output Reference document:

The AirLoopHVAC:ExhaustSystem and ZoneHVAC:ExhaustSystem objects are used to describe the way that the exhaust air streams are configured. These objects provide a convenient way for the exhaust air streams from multiple air loops to be rerouted and recombined to form one or more new exhausts. The exhaust system is typically composed of a central exhaust fan, the inlet nodes, (and actually an implicit exhaust mixer that could combine exhaust air streams from multiple air loops.

### AirLoopHVAC:ExhaustSystem Input Fields ###

The AirLoopHVAC:ExhaustSystem will take the following input fields:

#### Field: Name ####

This input field is for the name of the exhaust system. 

#### Field: Availability Schedule ####

This is the availability manager list schedule name for the exhaust system object. 

#### Field: Fan Object Type ####

This is the type of fan object type for the central exhaust fan. The avaiable choices are Fan:SystemModle or Fan:ComponentModel.

#### Field: Fan Name ####

This is a required field for the name of the central exhaust fan.

#### Field: Inlet Node 1 ####

This is the name of the first component object in the exhaust system. This is a required field.

#### Field: Inlet Node <#> ####

This is the name of the additional components object in the exhaust system. Additional inlet nodes could be specified for the exhaust system if applicable. 

The remaining fields are one repeated item: the inlet nodes. These fields define the inlet nodes for the exhaust system. The field is extensible so Component 2 or more could also be specified following the first object.

An example of the AirLoopHVAC:ExhaustSystem input object is like this:
```
AirLoopHVAC:ExhaustSystem,
    Central Exhaust,            !- Name
    Exhaust Avail List,         !- Availability Manager List Name
    Fan:SystemModel,            !- Fan Object Type
    Central Exhaust Fan,        !- Fan Name
    Zone 1 ExhaustSystem Node,        !- Inlet Node 1
    Zone 2 ExhaustSystem Node,        !- Inlet Node 2
    Zone 3 ExhaustSystem Node,        !- Inlet Node 3
    Zone 4 ExhaustSystem Node;        !- Inlet Node 4
```

### ZoneHVAC:ExhaustSystem Input Fields ###

The input fields for the ZoneHVAC:ExhaustSystem object are as follows.

#### Field: Name ####

This input field is the for the name of the ZoneHVAC:ExhaustSystem.

#### Field: Availablity Schedule Name ####

This is the aviability schdule name of the ZoneHVAC:Exhaust equipment.

#### Field: Inlet Node Name ####

This is the inlet node name of the ZoneHVAC:ExhaustSystem object. Typically, this would be an exhaust node of the zone. 

#### Field: Outlet Node Name ####

This is the outlet node name of the ZoneHVAC:ExhaustSystem object. This node name would be used to connected to the AirLoopHVAC:ExhaustSystem object. 

#### Field: Design Flow Rate {m3/s} ####

This numerical field is the design exhaust flow rate of the exhaust system in [m3/s]. 

#### Field: Flow Control Type ####

This field is the the control type on how the exhaust flows are controlled. The available choices are: Scheduled, Passive, and FollowSupply.

#### Field: Minimum Flow Fraction Schedule Name ####

This is the schedule name for the minimum exhaust flow fraction. If left empty, the default value would be zero. 

#### Field: Flow Fraction Schedule Name ####

This is the schedule name for the exhaust flow fraction. If left empty, the default value would be zero. 

#### Field: Supply Node or NodeList Name ####
This is the supply air node or nodelist name, which should be used for the `FollowSupply` flow control type. 

#### Field: System Availability Manager Name ####

This is the system availability manager name of the supply node or nodelist. 

#### Field: Minimum Zone Temperature Limit Schedule Name ####

This will be the minimum zone temperature limit schedule name. It will be used to shut down the exhaust flow rate to the minimum value if the zone temperature get below this temperature. 

#### Field: Balanced Exhaust Fraction Schedule Name ####

This field is for the schedule name of the balance exhaust fraction. With this fraction, the equipment would be considered air-balanced. 

An example of the ZoneHVAC:ExhaustSystem input object is like this:
```
ZoneHVAC:ExhaustSystem,
    Zone2 Exhaust System,           !-Name
    HVACOperationSchd,              !- Availability Schedule Name
    Zone2 Exhaust Node,             !- Inlet Node Name
    Zone2 ExhaustSystem Node,       !- Outlet Node Name
    0.1,                            !- Design Flow Rate {m3/s}
    Scheduled,                      !- Flow Control Type (Scheduled, Passive, FollowSupply, ????)
    Zone2 Min Exhaust Flow Sched,   !- Minimum Flow Fraction Schedule Name
    Zone2 Exhaust Flow Sched,       !- Flow Fraction Schedule Name
    ,                               !- Supply Node or NodeList Name (used with FollowSupply control type)
    ,                               !- System Availability Manager Name
    ,                               !- Minimum Zone Temperature Limit Schedule Name
    FlowBalancedSched;              !- Balanced Exhaust Fraction Schedule Name
```

## Input Description ##

See the Input Output Reference documentation contents update above. 

## Outputs Description ##

The following output will be added the to the new exhaust system: 

```
Central Exhaust Fan Energy [J]
Central Exhaust Fan Power Rate [W];
Central Exhaust Fan Runtime Fraction [];
Central Exhaust Fan Volumetric Flow Rate [m3/s];
Central Exhaust Fan Mass Flow Rate [kg/s];
Central Exhaust Fan pressure drop [Pa];
Individual Exhaust Volumetric Flow Rate [m3/s]; 
Individual Exhaust Mass Flow Rate Rate [kg/s];
```

In addition, the tabular report will now also add the central exhaust fans' flow rates and energy usages to each of the connected HVAC and airloops that the exhaust system is connected.


## Engineering Reference ##

There would be no change about the fundamental methods in the the Engineering Reference. One potential addition though, is to document the assumptions and methods used to re-balance the zone exhaust airflows under the central exhaust fan driven operation mode. 

## References ##

NA


## Designs ##

### Air Loop Calling Tree ###

#### getExhaustSystemInput() ####

A function to process the input fields for the AirLoopHVAC:ExhaustSystem object. It will read input information in the exhaust system set and update internal data for the components (such as the central exhaust fan, zone exhaust fans, and exhaust mixers) being read in.

#### SizeExhaustSystemFans() ####

This function will be added to size the central exhaust system fans. 

#### SimAirLoopExhaustSystems() ####

This function will be added to simulate the central exhaust system in the airloop. Here, since we choose not to tie a central exhaust system to a particular airloop, in this function it could loop through all the central exhaust systems to do the simulation. 

One piece of information that needed exchange from the Zone Equipment calling tree is about the flow conditions from the incoming inlet of the AirLoopHVAC:ExhaustSystem objects. It seems that the 

#### AirLoopHVAC:ExhaustSystem data struct ####

This struct definition and declaration will create a new data struct for the AirLoopHVAC:ExhaustSystem object.

When the AirlooopHVAC:ExhaustSystem is on, the central exhaust fan needs to know all of the incoming flows set by the ZoneHVAC:ExhaustSystem objects so that the total flow rate can be set. Therefore the AirloopHVAC:ExhuastSystem objects need to be called before zone equipment to set the AirLoop exhaust system's status for the Zone Equipment exhaust system simulation, which seems to be a natural order of calling. 

However, when modeling the central exhasut fans in the airloop, the zone equipment airflow status needs to be feed back to the Air Loop, in order to simulate the exhaust fan flows. This is not a natural order of calling. One additional airloop level exhasut system call need to be added after the zone equipment exhaust module calling.  

### Zone Equipment Calling Tree ###

#### SimZoneHVACExhaustAirSystem() ####

A proper simulation entry point for the exhaust system simulation would be after the 
```
    CalcZoneLeavingConditions(state, FirstHVACIteration);
```
and before the function
```
    SimReturnAirPath(state);
```
near the end of the `SimZoneEquipment()` function in ZoneEquipmentManager.cc.
 
The new `SimZoneExhaustAirSystem()` should be added here to conduct the AirLoopHVAC:ExhaustSystem simulation. 

However, the situation could be complicated by the general exhaust system's operation. Here based on If the AirloopHVAC:ExhaustSystem's condition, (e.g. "is on" or "is off"), the zone air balance will be treated differently. For example, when the AirLoopHVAC:ExhaustSystem is off, all inlet flows for the connected zone exhausts should be set to zero. This needs to be known at the time of the zone air mass balance. A zone mass balance, in this case, the call to the newly adde SimZoneExhaustAirSystem() should be called before the zone mass balance calculation: 
```
    CalcZoneMassBalance(state, FirstHVACIteration);
```

#### Modleing AirloopHVAC central exhaust airflow ####

After the zone equipment exhaust systems' airflows are modeled, here the airloop's central airlfow should be modeled (again). This time, the central exhaust fan airflow should be udpated based on the zone exhaust airflows of the connected zone exhaust. 

#### ZoneHVAC:ExhaustSystem data struct #### 

This struct definition and declaration will create a new data struct for the ZoneHVAC:ExhaustSystem object.

### ReportAirLoopExhaustSystem() ###

The function is for reporting the variables related to the exhaust systems, such as the fans' flow rates, energy usages, and pressure drops. 

## Reference ##

[1] Building Energy Efficiency Standards - Title 24, California Energy Commission. Link: https://www.energy.ca.gov/programs-and-topics/programs/building-energy-efficiency-standards

[2] ASHRAE Standard 90.1-2019. Energy Standard for buildings except low-rise Residential buildings. ASHRAE, Atlanta.

[3] ANSI/ASHRAE/ASHE Standard 170-2017, Ventilation of Health Care Facilities. ASHRAE, Atlanta.

[4] T. Smith, G.C. Bell, High-Performance Laboratory Exhaust Devices. Link: https://labs21.lbl.gov/workshop/AdvCourse-HPLabExhDev-5.pdf

[5] D. MacDonald, 2016. Laboratory Design Fundamentals. Link: https://ashraemadison.org/images/ASHRAE_Madison_Lab_Fundamentals_03_14_2016.pdf
