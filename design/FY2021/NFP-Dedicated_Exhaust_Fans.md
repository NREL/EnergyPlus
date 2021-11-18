Dedicated Exhaust System for Flexible Exhausts Configurations
=============================================================

**J. Yuan & M.J. Witte, GARD Analytics**

 - Original Date: July 30, 2021
 - Revised: August 2, 2021, revise and start design
 - Revised: Oct ??, 2021, 

## Justification for New Feature ##

This new feature intends to provide a convenient way for multiple exhausts in different AirLoops to be combined to a central exhaust system. The need for this proposed feature stems from the practice of modeling general exhaust with a central exhaust duct. In a typical configuration, airflows are applied to offices and laboratories spaces from multiple airloops, and then the return or exhaust airflows are re-routed to different exhaust systems due to different exhaust heat recovery processes for the office spaces the lab spaces exhausts. This configuration can be shown in the following example (credit: the figure is provided by Sagar Rao at Affiliated Engineers Inc., the original requester of the feature): 

![Exhaust System Configuration](Dedicated_Airstream.png)

Currently modeling this type of configuration in EnergyPlus requires workarounds (to be described in the overview section) and it quite cumbersome to implement for large projects. According to the original requester, the workaround method could also complain about missing “exhaust fans” in the energy report  the  during compliance reviews (e.g. CA Title-24, pp. 257, 2019 version). This calls for an easier way to model a such re-routed exhaust streams, which would also allow more accurate part loads calculation and reporting on the exhaust fans. The proposed work is thus about implementing such a new feature that will allow more flexible exhaust streams routing to meet the modeling needs described above. 

Another application for such exhaust systems is health care. ANSI/ASHRAE/ASHE Standard 170-2013 Ventilation of Health Care Facilities specifies that certain types of spaces have a requirement that "All Room Air Exhausted Directly to Outdoors," e.g. ER waiting rooms, laboratories, laundry, etc. This adds the requirement for exhaust flow to track supply flow which is not currently possible at the zone level.

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

An AirLoopHVAC:ExhaustSystem is made to be something similar to a "Return Path"--so it is really like an "Exhaust Path" here. The specifications of the exhasut system would then be similar to that of a return path, including objects such as AirLoopHVAC:ZoneMixer. However, it could be different from the return path, in that in general, this exhaust path would have a central exhasut fan specified on it. Also, it should also allow individual zone's exhaust fan to be connected to the zone exhaust or plenum exhaust, as part of the exhaust system. Further, beyond the central exhaust fan, there could also be another heat/enthalpy exchanger for the heat recovery purposes. 

The following new objects will be added to allow an AirLoopHVAC:ExhaustSystem to be described: 
```
AirLoopHVAC:ExhaustSystem,
    Central Exhaust,            !- Name
    Exhaust Avail List,         !- Availability Manager List Name
    Fan:ZoneExhaust,            !- Component 1 Object Type
    Zone1 Exhaust Fan;          !- Component 1 Name
	ZoneHVAC:ExhaustSystem,     !- Component 2 Object Type
	Zone 2 Exhaust system;      !- Component 2 Name
    AirLoopHVAC:ExhaustMixer,   !- Component 3 Object Type
    Exhaust Mixer 1,            !- Component 3 Name
    Fan:SystemModel,            !- Component 4 Object Type
    Central Exhaust Fan,        !- Component 4 Name
```

The central fan model for this object needs to be either FAN:SYSTEMMODEL or FAN:COMPONENTMODEL. The regular fan models such as Fan:OnOff, Fan:ConstantVolume, or Fan:VariableVolume could not be used with the current object.

### ZoneHVAC:ExhaustSystem ###

The ZoneHVAC:ExhaustSystem is also to be added as a new object, as a more advanced version of fan:zoneexhaust connected to a zone exhaust. It will allow the exhaust system to use the newer fan:systemmodel or fan:componentmodel: 
```
ZoneHVAC:ExhaustSystem,
    Zone2 Exhaust System,           !-Name
    HVACOperationSchd,              !- Availability Schedule Name
    Zone2 Exhaust Node,             !- Inlet Node Name
    Exhaust Mixer Inlet Node 2,     !- Outlet Node Name
    0.1,                            !- Design Flow Rate {m3/s}
    Fan:SystemModel,                !- Fan Object Type (could be blank if this is passive)
    Zone2 Exhaust Fan,              !- Fan Name
    Scheduled,                      !- Fan Control Type (Scheduled, Passive, FollowSupply, ????)
    Zone2 Exhaust Fan Flow Sched,   !- Flow Fraction Schedule Name
    ,                               !- Supply Node or NodeList Name (used with FollowSupply control type)
    ,                               !- System Availability Manager Name
    ,                               !- Minimum Zone Temperature Limit Schedule Name
    FlowBalancedSched;              !- Balanced Exhaust Fraction Schedule Name
```

### ZoneHVAC:ExhaustSystem enforcement choice ###

One piece of important information about each of the indivual zone exhausts is that there should be at least some information about the design flow rate, which might be important for sizing and simulation. This should be based on either a design (exhaust) flow rate by input or via zone xhaust fan (design flow) inputs. 

One way to deal with the problem is to enforce an implementaton of ZoneHVAC:ExhaustSystem for each connected zone exhaust, making is a required object for for every zone that connects to the AirLoopHVAC:ExhaustSystem. The Zone:ExhaustSystem can either have its own fan (in which the fan will give the information) or be passive. A fan (Fan:SystemModel or Fan:ComponentModel) or design flow rate is required if any of the ZoneHVAC:ExhaustSystem objects do not have a fan.

```
AirLoopHVAC:ExhaustSystem,
    Central Exhaust,            !- Name
    Exhaust Avail List,         !- Availability Manager List Name
    ZoneHVAC:ExhaustSystem,     !- Component 1 Object Type
    Zone1 Exhaust System,       !- Component 1 Name
    ZoneHVAC:ExhaustSystem,     !- Component 2 Object Type
    Zone2 Exhaust System,       !- Component 2 Name
    AirLoopHVAC:Mixer,          !- Component 3 Object Type
    Exhaust Mixer;              !- Component 3 Name
    Fan:SystemModel,            !- Component 4 Object Type
    Central Exhaust Fan,        !- Component 4 Name
```

The ZoneHVAC:ExhaustSystem enforcement choice would affect how the mixer should be designed below. In general, if the ZoneHVAC:ExhaustSystem is enforced for each zone, there is no need to add new design flow rate information to a mixer (see the Mixer choice in the next section). Here denote the option of enforcing ZoneHVAC:ExhaustSystem on each connected exhaus as Choice 1; and the option of not enforceing it as Choice 2. 

### Reuse/Expand existing AirLoopHVAC:ZoneMixer or AirLoopHVAC:Mixer ####

If the design exhaust flow rate can be obtained someplace else, the existing AirLoopHVAC:Mixer or AirLoopHVAC:ZoneMixer object could be expanded and reused for in the exhaust system. For example, originally the AirLoopHVAC:ZoneMixer is only allowed in a return path, or in a PIU like zone equipment. A severe warning would show up if the zone mixer is not used (or referenced) with one of the following objects to be used with AirLoopHVAC:ReturnPath, AirTerminal:SingleDuct:SeriesPIU:Reheat, AirTerminal:SingleDuct:ParallelPIU:Reheat, or AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction. 

In the current development, this choice would allow an AirLoopHVAC:Mixer to be used as the connectors in the "Exhaust Path" system. This means that the zone mixer can be connected to the exhaust node of a zone, the outlet node a fan:zoneexhaust object, or the exhaust fan outlet of a newly developed ZoneHVAC:ExhaustSystem. 

### Overall design choice combinations ###

Depending on the ZoneHVAC:ExhaustSystem enforcement choice (a and b) and the mixer choice, the overall design choice can be in general the following two combinations: 

#### Option 1b: ####
Enforcing ZoneHVAC:ExhaustSystem to all exhausts; reusing/expanding current AirLoopHVAC:ZoneMixer.

In this case, what should be included in the parent AirLoopHVAC:ExhaustSystem object should included the ZoneHVAC:Exhaustem objects, the mixers, and the central exhaust fans: 
```
AirLoopHVAC:ExhaustSystem,
    Central Exhaust,            !- Name
    Exhaust Avail List,         !- Availability Manager List Name
    ZoneHVAC:ExhaustSystem,     !- Component 1 Object Type
    Zone1 Exhaust System,       !- Component 1 Name
    ZoneHVAC:ExhaustSystem,     !- Component 2 Object Type
    Zone2 Exhaust System,       !- Component 2 Name
    AirLoopHVAC:Mixer,          !- Component 3 Object Type
    Exhaust Mixer;              !- Component 3 Name
    Fan:SystemModel,            !- Component 4 Object Type
    Central Exhaust Fan,        !- Component 4 Name
```

#### Option 1a: ####
Another possiblity is not to enforce ZoneHVAC:ExhaustSystem to all exhausts; and still reuse/expand current AirLoopHVAC:ZoneMixer. This is similar to 1b, except now that the ZoneHVAC:ExhaustSystem is not enforced for all exhausts. This allows some flexibility and convinience in specifying a mix of some zones exhaust fans and some zones without fancy exhaust fans (passive). In this scenario, the code should check to make sure adquate design flow rate information could be come from some input information (either an input exhaust flow rate or an exhaust fan/system design flow rate specification. 

### Other considerations ###

#### Report considerations ####
One issue for using the methods above is which Air loop should the exhaust system belongs when it spans across more than one air loops. One general rule would should be tied to an airloop that it connects to; but there should also be a check to let one airloop to have at most one exhaust system. 

#### ZoneHVAC:ExhaustSystem as Zone Equipment ####
The newly ZoneHVAC:ExhaustSystem should be considered to be an zone equipment, similar to the existing Fan:ZoneExhaust object. 

### IDD changes ###

The following IDD blocks will be added to the Energy+.idd file.

#### IDD Addition for AirLoopHVAC:ExhaustSystem ####

After the AirLoopHVAC:ReturnPath block and before the AirLoopHVAC:ExhaustMixer (to be added) blocks:
```
AirLoopHVAC:ExhaustSystem,
       \extensible:2 - Just duplicate last two fields and comments (changing numbering, please)
       \memo Define the zone exhaust systems that 
       \memo can be used for an AirLoopHVAC:ExhaustSystem
  A1 , \field Name
       \required-field
  A2 , \field Availability Schedule Name
       \note Availability schedule name for this zone exhaust system. Schedule value > 0 means it is available.
       \note If this field is blank, the exhaust system is always available.      
       \type object-list
       \object-list ScheduleNames
  A3 , \field Component 1 Object Type
       \begin-extensible  
       \required-field
       \type choice
       \key Fan:SystemModel
       \key Fan:ComponentModel
       \key Fan:ZoneExhaust
  A4 , \field Component 1 Name
       \required-field
       \type object-list
       \object-list FansSystemModel
       \object-list FansComponentModel
       \object-list AirLoopHVACExhaustMixerNames
       \object-list FansZoneExhaust
  A5 , \field Component 2 Object Type
       \type choice
       \key Fan:SystemModel
       \key Fan:ComponentModel
       \key AirLoopHVAC:ExhaustMixer
       \key Fan:ZoneExhaust
  A6 , \field Component 2 Name
       \type object-list
       \object-list FansSystemModel
       \object-list FansComponentModel
       \object-list AirLoopHVACExhaustMixerNames
       \object-list FansZoneExhaust
  A7 , \field Component 3 Object Type
       \type choice
       \key Fan:SystemModel
       \key Fan:ComponentModel
       \key AirLoopHVAC:ExhaustMixer
       \key Fan:ZoneExhaust
  A8;  \field Component 3 Name
       \type object-list
       \object-list FansSystemModel
       \object-list FansComponentModel
       \object-list AirLoopHVACExhaustMixerNames
       \object-list FansZoneExhaust
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
  A5 , \field Fan Object Type 
       \note Type of exhaust fan object
       \note This field could be blank if the exhaust is passive
       \type choice
       \key Fan:SystemModel
       \key Fan:ComponentModel
  A6 , \field Fan Name
       \note Name of the exhaust fan object
       \type object-list
       \object-list FanNames
  A7 , \field Fan Control Type
       \note Control type of the exhaust fan
       \type choice
       \key Scheduled
       \key Passive
       \key FollowSupply
  A8 , \field 
       \note Schedule name of the exhaust flow fraction
       \type object-list
       \object-list ScheduleNames
  A9 , \field Supply Node or NodeList Name
       \note To be used with FollowSupply control type)
  A10, \field System Availability Manager Name
       \type object-list
       \object-list AvailabilityManagerNames
  A11, \field Minimum Zone Temperature Limit Schedule Name
       \note Schedule name of the Minimum Zone Temperature Limit
       \type object-list
       \object-list ScheduleNames
  A12, \field Balanced Exhaust Fraction Schedule Name
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

The AirLoopHVAC:ExhaustSystem and AirLoopHVAC:ExhaustMixer objects are used to describe the way that the exhaust air streams are configured. These objects provide a convenient way for the exhaust air streams from multiple air loops to be rerouted and recombined to form one or more new exhausts. The exhaust system is typically composed of a central exhaust fan, a exhaust mixer that could combine exhaust air streams from multiple air loops, and/or the exhaust fans from some other zones.

### AirLoopHVAC:ExhaustSystem Input Fields ###

The AirLoopHVAC:ExhaustSystem will take the following input fields:

#### Field: Name ####

This input field is for the name of the exhaust system. 

#### Field: Availability Manager List Name ####

This is the availability manager list schedule name for the exhaust system object. 

#### Field Set Component Object Type and Name ####

The remaining fields are sets of two repeated items: a component object type and a name. These pairs of fields define the components for the exhaust system.

#### Field: Component 1 Object Type ####

This is a required field for the first component in the exhaust system, typically this would be a central exhaust fan. The possible choices are: Fan:SystemModel, Fan:ComponentModel, AirLoopHVAC:ExhaustMixer, or Fan:ZoneExhaust.

#### Field: Component 1 Object Name ####

This is the name of the first component object in the exhaust system. This is a required field.

#### Field: Component <#> Object Type ####

Additional components could be specified for the exhaust system if applicable. The possible choices are the same as those for the first component: Fan:SystemModel, Fan:ComponentModel, AirLoopHVAC:ExhaustMixer, or Fan:ZoneExhaust.

The field is extensible so Component 3 or more could also be specified following the second object.

#### Field: Component <#> Object Name ####

This is the name of the additional components object in the exhaust system.

An example of the AirLoopHVAC:ExhaustSystem input object is like this:
```
AirLoopHVAC:ExhaustSystem,
    Central Exhaust,            !- Name
    Exhaust Avail List,         !- Availability Manager List Name
    Fan:SystemModel,            !- Component 1 Object Type
    Central Exhaust Fan,        !- Component 1 Name
    AirLoopHVAC:ExhaustMixer,   !- Component 2 Object Type
    Exhaust Mixer 1,            !- Component 2 Name
    Fan:ZoneExhaust,            !- Component 3 Object Type
    Zone1 Exhaust Fan;          !- Component 3 Name
```

### ZoneHVAC:ExhaustSystem Input Fields ###

The ZoneHVAC:ExhaustSystem input fields are as follows. 

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
```

In addition, the tabular report will now also add the exhaust fans' flow rates and energy usages to each of the connected HVAC and airloops that the exhaust system is connected.

## Engineering Reference ##

There would be not change about the fundamental methods in the the Engineering Reference. One potential addition though, is to document the assumptions and methods used to re-balance the zone exhaust airflows under the central exhaust fan driven operation mode. 

## References ##

NA


## Designs ##

### SimExhaustAirSystem() ###

A proper simulation entry point for the exhaust system simulation would be after the 
```
    CalcZoneLeavingConditions(state, FirstHVACIteration);
```
and before the function
```
    SimReturnAirPath(state);
```
near the end of the `SimZoneEquipment()` function in ZoneEquipmentManager.cc.
 
The new `SimExhaustAirSystem()` should be added here to conduct the AirLoopHVAC:ExhaustSystem simulation. 

There should also be some airloop level calls (or a new function if needed) about the exhasut air system.

### SizeExhaustSystemFan() ###

This function will be added to size the central exhaust system fans. 

### getExhaustSystemInput() ###

A function to process the input fields for the AirLoopHVAC:ExhaustSystem object. It will read input information in the exhaust system set and update internal data for the components (such as the central exhaust fan, zone exhaust fans, and exhaust mixers) being read in.

### AirLoopHVAC:ExhaustSystem data struct ###

This struct definition and declaration will create a new data struct for the AirLoopHVAC:ExhaustSystem object.

### ZoneHVAC:ExhaustSystem data struct ###

This struct definition and declaration will create a new data struct for the ZoneHVAC:ExhaustSystem object.

### ReportAirLoopExhaustSystem() ###

The function is for reporting the variables related to the exhaust systems, such as the fans' flow rates, energy usages, and pressure drops. 

