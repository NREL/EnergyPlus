Dedicated Exhaust System for Flexible Exhausts Configurations
=============================================================

**J. Yuan & M.J. Witte, GARD Analytics**

 - Original Date: July 30, 2021

## Justification for New Feature ##

This new feature intends to provide a convenient way for multiple exhausts in different AirLoops to be combined to a central exhaust system. The need for this proposed feature stems from the practice of modeling general exhaust with a central exhaust duct. In a typical configuration, airflows are applied to offices and laboratories spaces from multiple airloops, and then the return or exhaust airflows are re-routed to different exhaust systems due to different exhaust heat recovery processes for the office spaces the lab spaces exhausts. This configuration can be shown in the following example (credit: the figure is provided by Sagar Rao at Affiliated Engineers Inc., the original requester of the feature): 

![Exhaust System Configuration](Dedicated_Airstream.png)

Currently modeling this type of configuration in EnergyPlus requires workarounds (to be described in the overview section) and it quite cumbersome to implement for large projects. According to the original requester, the workaround method could also complain about missing “exhaust fans” in the energy report  the  during compliance reviews (e.g. CA Title-24, pp. 257, 2019 version). This calls for an easier way to model a such re-routed exhaust streams, which would also allow more accurate part loads calculation and reporting on the exhaust fans. The proposed work is thus about implementing such a new feature that will allow more flexible exhaust streams routing to meet the modeling needs described above. 

## E-mail and Conference Call Conclusions ##

### E-mail Communications ###

- A few email exchanges with the original requester prior to and during the development of the NFP to clarify the development needs;

### Conference Call Communications ###

- A discussion at the Technicality conference call occurred before the development of the current NFP;

- An informal phone conversation with the original requester was conducted during the development of the current NFP about the development needs and the current workaround method.

## Overview ##

A few existing EnergyPlus modules are investigated for their potential capabilities for implementing the exhaust systems configurations mentioned above. Here the capabilities and limitations of each of these solutions (of trying to use existing modules) are discussed, and will be used to develop a new strategy for developing the new feature. 

The first candidate to model such a system is to use the AirLoopHVAC:Mixer object, which needs to be used in an Dedicated Outdoor Air System (AirLoopHVAC:DedicatedOutdoorAirSystem). When using this object, multiple OutdoorAir:Mixer relief air nodes can be connected together with the AirLoopHVAC:Mixer to form one common exhaust outlet (to the outdoor) or to a heat recovery device. This seems to be a good candidate for combining multiple relief air streams to a general exhaust. However, the limitation is that the AirLoopHVAC:Mixer is only intended to be used by (and with) the AirLoopHVAC:DedicatedOutdoorAirSystem object, and it cannot be used for regular non-DOAS airloop system. Further, it is not intend to use with multiple return loops that span across multiple (DOAS) airloops, similar to the "intertwined" configuration shown in the example figure above.

The second potential candidate for modeling such an exhaust system is the AirLoopHVAC:ZoneMixer object to configure an AirLoopHVAC:ReturnPath object. The zone mixer will be able to combine the return airflow of multiple zones into one common outlet for a return path. The current limitation of this method is that the "return path" in general ends at the point where the collected return air combines into a central return duct. The exhaust air part is still further down in the airloop path. This configuration is not intend to use with multiple return loops that span across multiple airloops, either.

A third potential candidate is using the general Connector:Mixer object to develop the airloop topography. The Connector:Mixer will taken several "branches" and connect them to a common outlet branch. This means that the Connector:Mixer actually works on the branch level and explicitly branches need to be defined for each incoming and outgoing connections, which will be cumbersome to use when trying to connect many zones on larger projects. The configuration is also subject to the restrictions that the airloop system that multiple routed exhaust system is difficult to be correctly recognized by EnergyPlus.

Currently, the original requester for the feature (S. Rao at Affiliated Engineers Inc) uses a workaround that uses EMS sensors to gather the different zone exhaust information and "virtually" mixed them together via some pieces of EMS program code, and then feed the gather flow information to a "dummy" actuatable system to accomplish the goal of modeling such a configuration. 

Based on the existing modules' capabilities and limitations, we proposed to add two new IDF objects to model such a  re-routed and recombined exhaust system configuration, and to allow quick setup and scaling up of such a configure for larger simulation projects.

## Approach ##

The following new objects will be added to allow an AirLoopHVAC:ExhaustSystem to be described: 
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

The central fan model for this object needs to be either FAN:SYSTEMMODEL or FAN:COMPONENTMODEL. The regular fan models such as Fan:OnOff, Fan:ConstantVolume, or Fan:VariableVolume could not be used with the current object.

The AirLoopHVAC:ExahaustMixer is also to be added as a new object: 
```
AirLoopHVAC:ExhaustMixer,
    Exhaust Mixer 1,                !-Name
    Central Exhaust Fan Inlet Node, !- Outlet Node Name
    Zone2 Exhaust Node,             !- Inlet 1 Node Name
    0.1,                            !- Inlet 1 Design Flow Rate {m3/s}
    Zone2 Exhaust Flow Schedule,    !- Inlet 1 Flow Fraction Schedule Name
    Zone1 Exhaust Fan Outlet Node,  !- Inlet 2 Node Name
    ,                               !- Inlet 2 Design Flow Rate {m3/s}
    ;                               !- Inlet 2 Flow Fraction Schedule Name
```

In the example IDF block above, the Inlet 2 Design Flow Rate (m3/s) might not be needed as it is an existing zone exhaust fan. The set up intends to allow some flexibility in the configurations in that one or more of the connect branch to the mixer can be without a branch fan. Further, the exhaust mixer will allow more than 2 inlet branches to be connected to the mixer.

### IDD changes ###

The following IDD blocks will be added to the Energy+.idd file.

#### IDD Addition for AirLoopHVAC:ExhaustSystem ####

After the AirLoopHVAC:ReturnPath block and before the AirLoopHVAC:ExhaustMixer (to be added) blocks:
```
AirLoopHVAC:ExhaustSystem,
       \extensible:2 - Just duplicate last two fields and comments (changing numbering, please)
       \memo Define dedicated exhaust systems that 
       \memo combines exhausts of multiple AirLoopHVAC systems
  A1 , \field Name
       \required-field
  A2 , \field Availability Schedule Name
       \note Availability schedule name for this exhaust system. Schedule value > 0 means it is available.
       \note If this field is blank, the exhaust system is always available.      
       \type object-list
       \object-list ScheduleNames
  A3 , \field Component 1 Object Type
       \begin-extensible  
       \required-field
       \type choice
       \key Fan:SystemModel
       \key Fan:ComponentModel
       \key AirLoopHVAC:ExhaustMixer
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

#### IDD Addition for AirLooopHVAC:ExhaustMixer ####

After the AirLoopHVAC:ReturnPath block and the AirLoopHVAC:DedicatedOutdoorAirSystem blocks:
```
AirLoopHVAC:ExhaustMixer,
       \extensible:3 - Just duplicate last three fields and comments (changing numbering, please)
       \memo Mix N exhaust air streams from Relief Air Stream Node
       \memo served by multiple AirLoopHVAC objects into one
       \memo (currently 10 as default, but extensible). Node names cannot
       \memo be duplicated within a single Exhaust mixer list.
  A1 , \field Name
       \required-field
       \reference AirLoopHVACExhaustMixerNames
  A2 , \field Outlet Node Name
       \required-field
       \type node
  A3 , \field Inlet 1 Node Name
       \begin-extensible
       \type node
  N1 , \field Inlet 1 Design Flow Rate {m3/s}
       \units m3/s
       \autosizable
       \default autosize
  A4 , \field Inlet 1 Flow Fraction Schedule Name
       \note Flow fraction schedule name for this Inlet. Schedule value is in range [0,1].
       \type object-list
       \object-list ScheduleNames
  A5 , \field Inlet 2 Node Name
       \type node
  N2 , \field Inlet 2 Design Flow Rate {m3/s}
       \units m3/s
       \autosizable
       \default autosize
  A6 , \field Inlet 2 Flow Fraction Schedule Name
       \note Flow fraction schedule name for this Inlet. Schedule value is in range [0,1].
       \type object-list
       \object-list ScheduleNames
  A7 , \field Inlet 3 Node Name
       \required-field
       \type node
  N3 , \field Inlet 3 Design Flow Rate {m3/s}
       \units m3/s
       \autosizable
       \default autosize
  A8 , \field Inlet 3 Flow Fraction Schedule Name
       \note Flow fraction schedule name for this Inlet. Schedule value is in range [0,1].
       \type object-list
       \object-list ScheduleNames
  A9 , \field Inlet 4 Node Name
       \type node
  N4 , \field Inlet 4 Design Flow Rate {m3/s}
       \units m3/s
       \autosizable
       \default autosize
  A10; \field Inlet 4 Flow Fraction Schedule Name
       \note Flow fraction schedule name for this Inlet. Schedule value is in range [0,1].
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

### AirLooopHVAC:ExhaustMixer Input fields ###

#### Field: Name ####

This field is the name of the exhaust mixer. 

#### Field: Outlet Node Name ####

This field is the name of the outlet node of the mixer. It is the node where all the incoming exhaust air streams are fully combined and mixed.

#### Field Set Inlet Nodes Information ####

The remaining fields are sets of three repeated items: an inlet node name, the design flow rate for the inlet node, and the flow fraction schedule name for the inlet node. These sets of fields define the inlet nodes for the exhaust mixer.

#### Field: Inlet 1 Node Name ####

This is the node for the first incoming exhaust air stream. This is a required field for the first component in the exhaust mixer.

#### Field: Inlet 1 Design Flow Rate (m3/s) ####

This field specifies the design flow rate for the first incoming exhaust stream.

#### Field: Inlet 1 Flow Fraction Schedule Name ####

This is the flow fraction schedule of Inlet 1 (based on the Design Flow Rate). If left blank, the default value would be 1. 

#### Field: Inlet <#> Node Name ####

Additional inlet nodes could be specified for the exhaust mixer if applicable.

The field is extensible so Inlet 3 or more could also be specified following the second object. 

#### Field: Inlet <#> Design Flow Rate (m3/s) ####

This field specifies the design flow rate for the additional incoming exhaust stream.

#### Field: Inlet <#> Flow Fraction Schedule Name ####

This is the flow fraction schedule of the additional inlet (based on the Design Flow Rate). If left blank, the default value would be 1. 

Here is an example input block for the AirLoopHVAC:ExhaustMixer object: 

```
AirLoopHVAC:ExhaustMixer,
    Exhaust Mixer 1,                !-Name
    Central Exhaust Fan Inlet Node, !- Outlet Node Name
    Zone2 Exhaust Node,             !- Inlet 1 Node Name
    0.1,                            !- Inlet 1 Design Flow Rate {m3/s}
    Zone2 Exhaust Flow Schedule,    !- Inlet 1 Flow Fraction Schedule Name
    Zone1 Exhaust Fan Outlet Node,  !- Inlet 2 Node Name
    ,                               !- Inlet 2 Design Flow Rate {m3/s}
    ;                               !- Inlet 2 Flow Fraction Schedule Name
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
```

In addition, the tabular report will now also add the exhaust fans' flow rates and energy usages to each of the connected HVAC and airloops that the exhaust system is connected.

## Engineering Reference ##

There would be not change about the fundamental methods in the the Engineering Reference. One potential addition though, is to document the assumptions and methods used to re-balance the zone exhaust airflows under the central exhaust fan driven operation mode. 

## References ##

NA


## Designs ##

### AirLoopHVAC:ExhaustSystem data struct ###

This struct definition and declaration will create a new data struct for the AirLoopHVAC:ExhaustSystem object.

### AirLoopHVAC:ExhaustMixer data struct ###

This struct definition and declaration will create a new data struct for the AirLoopHVAC:ExhaustMixer object. This could be simlar to AirLoopHVAC:Mixer or AirLoopHVAC:ZoneMier object. 

A potential proposal is like this: 
```
    struct AirLoopExhaustMixer
    {
        std::string name;
        static AirLoopExhaustMixer *factory(EnergyPlusData &state, int object_type_of_num, std::string const &objectName);
        int numOfInletNodes;
        int m_AirLoopExhaustMixer_Num;
        int OutletNodeNum;
        std::string OutletNodeName;
        std::vector<std::string> InletNodeName;
        std::vector<int> InlteFractionScheduleIndex;
        std::vector<Real64> InletDesignFlowRate;
        std::vector<int> InletNodeNum;
        Real64 OutletTemp;
        Real64 OutletHumRat;

        // default constructor
        AirLoopExhaustMixer() : numOfInletNodes(0), m_AirLoopMixer_Num(0), OutletNodeNum(0), OutletTemp(0.0), OutletHumRat(0.0)
        {
        }

        ~AirLoopExhaustMixer() = default; // destructor

        static void getAirLoopExhaustMixer(EnergyPlusData &state);
        void CalcAirLoopExhaustMixer(EnergyPlusData &state);
    };
```

The proposed code is similar to the current AirLoopHVAC:Mixer. Two additional vector element will be added to accommodate the inlet fraction schedule index and the inlet design flow rate need for AirLoopHVAC:ExhaustMixer. Further, another another additional member--Real64 OutletHumRat--is added to the struct, in that this could be important information for energy and mass balance, and also for potential total heat (enthalpy) recovery procedures.

It is also beneficial to use the factory method for the AirLoopHVAC:ExhaustSystem and the AirLoopHVAC:ExhaustMixer object creation.

### getAirLoopExhaustMixer() ###

This function will get the input information from the AirLoopHVAC:ExhaustMixer objects processed. Related information input includes 

### CalcAirLoopExhaustMixer() ###

It is a function to simulate the AirLoopHVAC:ExhaustMixer mixing process. The temperature, humidity, and mass flow rate will be calculated and updated based on the incoming streams' conditions.

### getAirLoopExhaustSystemInput() ###

A function to process the input fields for the AirLoopHVAC:ExhaustSystem object. It will read input information in the exhaust system set and update internal data for the components (such as the central exhaust fan, zone exhaust fans, and exhaust mixers) being read in.

### SimAirLoopExhaustSystem() ###

A function to set up the AirLoopHVAC:ExhaustSystem simulation. This will call the components included in the exhaust system to conduct simulation for each individual components, including the newly added AirLoopHVAC:Mixer object.

### ReportAirLoopExhaustSystem() ###

The function is for reporting the variables related to the exhaust systems, such as the fans' flow rates, energy usages, and pressure drops. 

