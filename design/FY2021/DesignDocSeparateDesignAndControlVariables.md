



Separate Design and Control Variables
================

**Jermy Thomas, Dareum Nam, NREL.**

 - Original date TBD


## Table of Contents ##

[Justification for New Feature](#justification-for-new-feature)

[E-mail, Conference Call, Other Communications and Conclusions](#e-mail-conference-call-other-communications-and-conclusions)

[Approach/Timeline/Design Rationale](#approachtimelinedesign-rationale)

[Testing/Validation/Data Sources](#testingvalidationdata-sources)

[Input Output Reference Documentation](#input-output-reference-documentation)

[Engineering Reference](#engineering-reference)

[Example File and Transition Changes](#example-file-and-transition-changes)

[References](#references)

[Next Steps](#next-steps)

[Changes to IDD file ](#changes-to-idd-file )

## Justification for New Feature ##

LowTemperatureRadiant and BaseBoard objects in EnergyPlus currently has design and control fields lumped together. This results in a large number of fields for these objects as seen below:
 - The `ZoneHVAC:LowTemperatureRadiant:VariableFlow` object has 34 fields
 - The `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` object has 34 fields
 - The `ZoneHVAC:Baseboard:RadiantConvective:Water` object has 17+ fields*
 - The `ZoneHVAC:Baseboard:RadiantConvective:Steam` object has 17+ fields*
 
 Lumping of design and control parameters together results in many of the variables being repeated for every zone which could have been instead shared among the zones. Lumping everything together is also less organized and causes confusion for the users. There have been requests from users to split these objects into design and control variables. 

Splitting the objects into design and control variables may result in 

 1. Increased ease of creating new objects, with fewer fields to fill in. 
 2. Decreased confusion from too many fields and may result in fewer errors from having too many variables. 
 3. If there is a case where different radiant systems (for example, LowTemperatureRadiant:VariableFlow and LowTemperatureRadiant:ConstantFlow) exist together in a model, a design object could provide common information to both these systems.
 
 (*The number of fields will increase with the number of surfaces) 

## E-mail, Conference Call, Other Communications and Conclusions ##

University of Illinois at Urbana-Champaign and Center for the Built Environment(CBE) at the University of California Berkeley requests this new feature. The original request is like below:

"Potentially separate the lowtemp:radiant objects such that one main object contains the design parameters of the radiant system (e.g. amount of tubing, hot and chilled water loop connections), while the other contains the control parameters (e.g. two position, modulating, zone circulator pump, etc.)." (Quote from CBE)   Right now, all of the parameters associated with a radiant system in EnergyPlus are contained in a single input syntax.  The concern here is that much of the control information is probably pretty similar from system to system within a single user input file.  So, there could be less work and smaller files if the input was broken up into two separate inputs, allowing many radiant systems to re-use a single control definition.  This could potentially be applied to other input syntax beyond the low temperature radiant systems.


[comment]: <> (### Questions and Comments Received through June 1 ##)
[comment]: <> (### Responses/Clarifications through June 1 ###)
[comment]: <> (### Conference Call Conclusions June 5 ###)

 
## Approach/Timeline/Design Rationale ##

### Timeline:


 - Design document:   
	 - Send for reviews------------------ Nov X 2020   
	 - Finalize design document-------- Dec 18 2020
 - Implementation:   
	 - First draft of Implementation---- Jan 31 2021
	 - Final Implementation------------- Feb 28 2021
 - Testing:   
	 - Testing done, bugs worked out--- Mar 30 2021
 - Buffer: 1 month

##### Comment #####
Project to end by April 2021 since Jermy's internship ends by May 2021. 

### Approach/Design Rationale ###

The proposed approach is to identify the design variables in the LowTemperatureRadiant and Baseboard objects and create separate design objects for the LowTemperatureRadiant and Baseboard objects.

<span style="display:block;text-align:center">![How the objects will be split](https://github.com/NREL/EnergyPlus/blob/Separate-Design-and-Control-Variables/design/FY2021/BigSplit.jpg)

**Figure: How the objects will be split**

<span style="display:block;text-align:center">![How the design objects will be brokendown](https://github.com/NREL/EnergyPlus/blob/Separate-Design-and-Control-Variables/design/FY2021/Breakdown.jpg)

**Figure: How the design objects will be brokendown**

#### ZoneHVAC:LowTemperatureRadiant objects ####

We decided to ignore the `ZoneHVAC:LowTemperatureRadiant:Electric` since it only has 12 fields, and therefore does not have a drastic need to reduce inputs like the VariableFlow and ConstantFlow objects do. Also, `ZoneHVAC:LowTemperatureRadiant:Electric` is different from the fluid (`ZoneHVAC:LowTemperatureRadiant:VariableFlow` and `ZoneHVAC:LowTemperatureRadiant:ConstantFlow`) objects. Therefore, trying to find parameters common to all three would result in very few design parameters (only two in our preliminary study) that could be extracted out. Therefore, the `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` were inspected and nine common parameters were identified. The common variables could be set as design variables since it seems like they could be shared between different `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and/or `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` objects in a model.

The following common variables were decided to be set as control variables/unable to be grouped into the design parameters since they will be specific to every object in a radiant system:

 - Name 
 - Zone Name 
 - Surface Name or Radiant Surface Group Name
 - Hydronic Tubing Length  
 - Heating Design Capacity {W} 
 - Heating Water Inlet Node Name  
 - Heating Water Outlet Node Name 
 - Maximum Hot Water Flow
 - Cooling Design Capacity {W} 
 - Maximum Cold Water Flow 
 - Cooling Water Inlet Node Name
 - Cooling Water Outlet Node Name
 - Number of Circuits
 - Circuit Length

##### Comments #####
 - The best way to decide what would be ideal system parameters would be to get feedback in some way from the current users. This would mean that the current users of radiant systems should be identified as well. 
 - Some of the common objects are optional fields or depend on other fields. I just wanted to point out that it should not be assumed that every field has the same importance. 

#### ZoneHVAC:Baseboard:RadiantConvective Objects ####

Common fields for all four ZoneHVAC:Baseboard:RadiantConvective objects were identified, and the following common variables were decided to be set as control variables/unable to be grouped as design parameters since they will be specific to every object in a radiant system:

-   Name
-   Inlet Node Name
-   Outlet Node Name
-   Heating & Cooling Design Capacity {W}
-   Maximum Water Flow Rate
-   Surface Name, Fraction of Radiant Energy to Surface

Since only three design objects could be identified as common fields that could be used as design variables to all four baseboard objects, this does not warrant the need to differentiate between design and control variables. Also, it may be that a group of similar baseboard objects may be more commonly used as compared to others. Therefore, different types of baseboard object groupings were explored to find a group that would have a larger number of design variables. Grouping the `ZoneHVAC:Baseboard:RadiantConvective:Water` and `ZoneHVAC:Baseboard:RadiantConvective:Steam` as radiant baseboard  objects that used fluids for heating seemed like a good choice since it enabled grouping seven common variables that could to be set as design variables:

##### Comments ######
 - As in the LowTemperatureRadiant objects, some of the common objects are optional fields or depend on other fields. I just wanted to point out that it should not be assumed that every field has the same importance. 


## Testing/Validation/Data Sources ##

Regression tests will be done to ensure that no changes to the current models have been made.  

## Input Output Reference Documentation ##

### Inputs Description ###

The new objects will have the fields that were removed from the old/modified objects. 

#### ZoneHVAC:LowTemperatureRadiant Objects

##### *New object* - `ZoneHVAC:LowTemperatureRadiant:Design` ####

This `ZoneHVAC:LowTemperatureRadiant:Design` object is referenced in the `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` objects and provide additional design parameters to them. Multiple `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` can be mapped to a single `ZoneHVAC:LowTemperatureRadiant:Design` object.

*Field: Name*    
*Field: Availability Schedule Name*     
*Field: Temperature Control Type*    
*Field: Fluid to Radiant Surface Heat Transfer Model*
*Field: Hydronic Tubing Inside Diameter*    
*Field: Hydronic Tubing Outside Diameter*    
*Field: Hydronic Tubing Conductivity*    
*Field: Condensation Control Type*    
*Field: Condensation Control Dewpoint Offset*    
*Field: Changeover Delay Time Period Schedule*

##### *Modified objects* - `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and  `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` ####

All of the fields in `ZoneHVAC:LowTemperatureRadiant:Design` will be removed from `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and  `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` objects. Additionally, we add the following field and description:

*Field: Design object*

This field cannot be blank, and it should point to one of the `ZoneHVAC:LowTemperatureRadiant:Design` objects. 

#### ZoneHVAC:Baseboard:RadiantConvective Objects

##### *New object* - `ZoneHVAC:Baseboard:RadiantConvective:Design` ####

This `ZoneHVAC:Baseboard:RadiantConvective:Design` object is referenced in the `ZoneHVAC:Baseboard:RadiantConvective:Water` and `ZoneHVAC:Baseboard:RadiantConvective:Steam` objects and provide additional design parameters to them. Multiple `ZoneHVAC:Baseboard:RadiantConvective:Water` and `ZoneHVAC:Baseboard:RadiantConvective:Steam` can be mapped to a single `ZoneHVAC:Baseboard:RadiantConvective:Design` object.

*Field: Name*    
*Field: Availability Schedule Name*    
*Field: Fraction Radiant*    
*Field: Fraction of Radiant Energy Incident on People*    
*Field: Heating Design Capacity Method*    
*Field: Heating Design Capacity Per Floor Area {W/m2}*    
*Field: Fraction of Autosized Heating Design Capacity*    
*Field: Convergence Tolerance*    

##### *Modified objects* - `ZoneHVAC:Baseboard:RadiantConvective:Water` and  `ZoneHVAC:Baseboard:RadiantConvective:Steam` ####

All of the fields in `ZoneHVAC:Baseboard:RadiantConvective:Design` will be removed from `ZoneHVAC:Baseboard:RadiantConvective:Water` and  `ZoneHVAC:Baseboard:RadiantConvective:Steam` objects. Additionally, we add the following field and description:

*Field: Design object*

This field cannot be blank, and it should point to one of the `ZoneHVAC:Baseboard:RadiantConvective:Design` objects. 

### Outputs Description ###

No changes. 

## Engineering Reference ##

No changes. 

## Example File and Transition Changes ##

All of the current example files that use `ZoneHVAC:LowTemperatureRadiant:VariableFlow`, `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` and `ZoneHVAC:Baseboard:RadiantConvective:Water`, `ZoneHVAC:Baseboard:RadiantConvective:Steam` will be modified so that the design and parameters are different.

## References ##

None.

## Next Steps ##

Get feedback from the EnergyPlus team regarding:

 1. If these groupings make sense. 
 2. If any design variables need to be moved to control variables and vice versa. 
 3. Any other recommended changes.


## Changes to IDD file ##

### New IDD Objects ####

**ZoneHVAC:LowTemperatureRadiant:Design,**

    	    \memo Design parameters for ZoneHVAC:LowTemperatureRadiant objects
       A1 , \field Name
            \required-field
            \reference-class-name validBranchEquipmentTypes
            \reference validBranchEquipmentNames
            \reference ZoneEquipmentNames
       A2 , \field Availability Schedule Name
            \note Availability schedule name for this system. Schedule value > 0 means the system is available.
            \note If this field is blank, the system is always available.
            \type object-list
            \object-list ScheduleNames
       A3 , \field Fluid to Radiant Surface Heat Transfer Model
            \note This parameter identifies how the heat transfer between
            \note fluid being circulated through the radiant system and
            \note the radiant system (slab) is modeled.  ConvectionOnly
            \note means that only convection between the fluid and the
            \note inside surface of the pipe is modeled using a conventional
            \note equation for flow inside a pipe.  ISOStandard models
            \note convection between the fluid and the inside of
            \note of the pipe and conduction through the pipe material using
            \note equations specific to ISO Standard 11855-2.
            \type choice
            \key ConvectionOnly
            \key ISOStandard
            \default ConvectionOnly
        A4 , \field Temperature Control Type
            \note (Temperature on which unit is controlled)
            \type choice
            \key MeanAirTemperature
            \key MeanRadiantTemperature
            \key OperativeTemperature
            \key OutdoorDryBulbTemperature
            \key OutdoorWetBulbTemperature
            \key SurfaceFaceTemperature
            \key SurfaceInteriorTemperature
            \default MeanAirTemperature
       N1 , \field Hydronic Tubing Inside Diameter
            \units m
            \minimum> 0
            \default 0.013
            \ip-units in
       N2 , \field Hydronic Tubing Outside Diameter
            \units m
            \minimum> 0
            \default 0.016
            \ip-units in
       N3 , \field Hydronic Tubing Conductivity
            \note Conductivity of the tubing/piping material
            \units W/m-K
            \minimum> 0
            \default 0.35          
       A5, \field Condensation Control Type
            \type choice
            \key Off
            \key SimpleOff
            \key VariableOff
            \default SimpleOff
       N4, \field Condensation Control Dewpoint Offset
            \units C
            \default 1.0
       A6, \field Number of Circuits
            \type choice
            \key OnePerSurface
            \key CalculateFromCircuitLength
            \default OnePerSurface
       A7; \field Changeover Delay Time Period Schedule
            \note Changeover delay schedule name for this system.  Schedule value <= 0 allows changeover with no delay
            \note The schedule values are interpretted as hours.
            \note If this field is blank, the system allows changeover with no delay
            \type object-list
            \object-list ScheduleNames

**ZoneHVAC:Baseboard:RadiantConvective:Design,**

           \memo Design parameters for ZoneHVAC:Baseboard:RadiantConvective objects
      A1,  \field Name
           \required-field
           \reference-class-name validBranchEquipmentTypes
           \reference validBranchEquipmentNames
           \reference ZoneEquipmentNames
      A2,  \field Availability Schedule Name
           \note Availability schedule name for this system. Schedule value > 0 means the system is available.
           \note If this field is blank, the system is always available.
           \type object-list
           \object-list ScheduleNames
      A3,  \field Heating Design Capacity Method
           \type choice
           \key HeatingDesignCapacity
           \key CapacityPerFloorArea
           \key FractionOfAutosizedHeatingCapacity
           \default HeatingDesignCapacity
           \note Enter the method used to determine the heating design capacity.
           \note HeatingDesignCapacity = > selected when the design heating capacity value or autosize
           \note is specified. CapacityPerFloorArea = > selected when the design heating capacity is
           \note determine from user specified heating capacity per floor area and zone floor area.
           \note FractionOfAutosizedHeatingCapacity = > is selected when the design heating capacity is
           \note determined from a user specified fraction and the auto-sized design heating capacity.
      N1 , \field Heating Design Capacity Per Floor Area
           \type real
           \units W/m2
           \minimum 0.0
           \note Enter the heating design capacity per zone floor area. Required field when the heating design
           \note capacity method field is CapacityPerFloorArea.
      N2 , \field Fraction of Autosized Heating Design Capacity
           \type real
           \minimum 0.0
           \default 1.0
           \note Enter the fraction of auto - sized heating design capacity.Required field when capacity the
           \note heating design capacity method field is FractionOfAutosizedHeatingCapacity.
      N3,  \field Convergence Tolerance
           \type real
           \minimum> 0.0
           \default 0.001
      N4,  \field Fraction Radiant
           \required-field
           \type real
           \minimum 0
           \maximum 1
      N5,  \field Fraction of Radiant Energy Incident on People
           \type real
           \minimum 0
           \maximum 1


### Modified IDD Objects ####

**ZoneHVAC:LowTemperatureRadiant:VariableFlow,**

            \memo Low temperature hydronic radiant heating and/or cooling system embedded in a building
            \memo surface (wall, ceiling, or floor). Controlled by varying the hot or chilled water
            \memo flow to the unit.
            \min-fields 26
       A1 , \field Name
            \required-field
            \reference-class-name validBranchEquipmentTypes
            \reference validBranchEquipmentNames
            \reference ZoneEquipmentNames
       A2 , \field Zone Name
            \note Name of zone system is serving
            \type object-list
            \object-list ZoneNames
       A3 , \field Surface Name or Radiant Surface Group Name
            \note Identifies surfaces that radiant system is embedded in.
            \note For a system with multiple surfaces, enter the name of
            \note a ZoneHVAC:LowTemperatureRadiant:SurfaceGroup object.
            \type object-list
            \object-list RadiantSurfaceNames
            \object-list RadiantGroupNames
       N1 , \field Hydronic Tubing Length
            \note (total length of pipe embedded in surface)
            \units m
            \minimum> 0
            \autosizable
            \default autosize
       A4,  \field Setpoint Control Type
            \note How setpoint temperature is defined
            \type choice
            \key HalfFlowPower
            \key ZeroFlowPower
            \default HalfFlowPower
       A5 , \field Heating Design Capacity Method
            \type choice
            \key HeatingDesignCapacity
            \key CapacityPerFloorArea
            \key FractionOfAutosizedHeatingCapacity
            \default HeatingDesignCapacity
            \note Enter the method used to determine the heating design capacity.
            \note HeatingDesignCapacity = > selected when the design heating capacity value or autosize
            \note is specified. CapacityPerFloorArea = > selected when the design heating capacity is
            \note determine from user specified heating capacity per floor area and zone floor area.
            \note FractionOfAutosizedHeatingCapacity = > is selected when the design heating capacity is
            \note determined from a user specified fraction and the auto-sized design heating capacity.
       N2 , \field Heating Design Capacity
            \type real
            \units W
            \minimum 0.0
            \autosizable
            \ip-units W
            \default autosize
            \note Enter the design heating capacity.Required field when the heating design capacity method
            \note HeatingDesignCapacity.
       N3 , \field Heating Design Capacity Per Floor Area
            \type real
            \units W/m2
            \minimum 0.0
            \note Enter the heating design capacity per zone floor area. Required field when the heating design
            \note capacity method field is CapacityPerFloorArea.
       N4 , \field Fraction of Autosized Heating Design Capacity
            \type real
            \minimum 0.0
            \default 1.0
            \note Enter the fraction of auto - sized heating design capacity.Required field when capacity the
            \note heating design capacity method field is FractionOfAutosizedHeatingCapacity.
       N5 , \field Maximum Hot Water Flow
            \units m3/s
            \minimum 0
            \autosizable
            \ip-units gal/min
       A6 , \field Heating Water Inlet Node Name
            \type node
       A7, \field Heating Water Outlet Node Name
            \type node
       N6 , \field Heating Control Throttling Range
            \units deltaC
            \minimum 0
            \default 0.5
       A8, \field Heating Control Temperature Schedule Name
            \type object-list
            \object-list ScheduleNames
       A9, \field Cooling Design Capacity Method
            \type choice
            \key None
            \key CoolingDesignCapacity
            \key CapacityPerFloorArea
            \key FractionOfAutosizedCoolingCapacity
            \default CoolingDesignCapacity
            \note Enter the method used to determine the cooling design capacity for scalable sizing.
            \note CoolingDesignCapacity => selected when the design cooling capacity value is specified or
            \note auto-sized. CapacityPerFloorArea => selected when the design cooling capacity is determined
            \note from user-specified cooling capacity per floor area and total floor area of the cooled zone
            \note served by the hydraulic unit. FractionOfAutosizedCoolingCapacity => is selected when the
            \note design cooling capacity is determined from a user-specified fraction and the auto-sized
            \note design cooling capacity of the system.
       N7, \field Cooling Design Capacity
            \type real
            \units W
            \minimum 0.0
            \autosizable
            \note Enter the design cooling capacity. Required field when the cooling design capacity method
            \note CoolingDesignCapacity.
       N8, \field Cooling Design Capacity Per Floor Area
            \type real
            \units W/m2
            \minimum 0.0
            \note Enter the cooling design capacity per total floor area of cooled zones served by the unit.
            \note Required field when the cooling design capacity method field is CapacityPerFloorArea.
       N9, \field Fraction of Autosized Cooling Design Capacity
            \type real
            \minimum 0.0
            \note Enter the fraction of auto-sized cooling design capacity. Required field when the cooling
            \note design capacity method field is FractionOfAutosizedCoolingCapacity.
       N10, \field Maximum Cold Water Flow
            \units m3/s
            \minimum 0
            \autosizable
            \ip-units gal/min
       A10, \field Cooling Water Inlet Node Name
            \type node
       A11, \field Cooling Water Outlet Node Name
            \type node
       N11, \field Cooling Control Throttling Range
            \units deltaC
            \minimum 0
            \default 0.5
       A12, \field Cooling Control Temperature Schedule Name
            \type object-list
            \object-list ScheduleNames
       A13, \field Number of Circuits
            \type choice
            \key OnePerSurface
            \key CalculateFromCircuitLength
            \default OnePerSurface
       N12, \field Circuit Length
            \units m
            \default 106.7
       A14, \field Design Object Name
            \type object-list
            \object-list LowTemperatureRadiant:Design Object Name

**ZoneHVAC:LowTemperatureRadiant:ConstantFlow,**

            \memo Low temperature hydronic radiant heating and/or cooling system embedded in a building
            \memo surface (wall, ceiling, or floor). Controlled by varying the hot or chilled water
            \memo temperature circulating through the unit.
            \min-fields 26
       A1 , \field Name
            \required-field
            \reference-class-name validBranchEquipmentTypes
            \reference validBranchEquipmentNames
            \reference ZoneEquipmentNames
       A2 , \field Zone Name
            \note Name of zone system is serving
            \type object-list
            \object-list ZoneNames
       A3 , \field Surface Name or Radiant Surface Group Name
            \note Identifies surfaces that radiant system is embedded in.
            \note For a system with multiple surfaces, enter the name of
            \note a ZoneHVAC:LowTemperatureRadiant:SurfaceGroup object.
            \type object-list
            \object-list RadiantSurfaceNames
            \object-list RadiantGroupNames
       N1 , \field Hydronic Tubing Length
            \note (total length of pipe embedded in surface)
            \units m
            \minimum> 0
            \autosizable
            \default autosize
       N2 , \field Running Mean Outdoor Dry-Bulb Temperature Weighting Factor
            \note this is the weighting factor in the equation that calculate the running mean outdoor dry-bulb temperature
            \note as a weighted average of the previous day’s running mean outdoor dry-bulb temperature and the previous day’s
            \note average outdoor dry-bulb temperature this value is only used by EnergyPlus when the user elects to use the
            \note RunningMeanOutdoorDryBulbTemperature control type
            \minimum 0.0
            \maximum 1.0
            \default 0.8
       N3 , \field Rated Flow Rate
            \units m3/s
            \ip-units gal/min
            \autosizable
       A4 , \field Pump Flow Rate Schedule Name
            \note Modifies the Rated Flow Rate of the pump on a time basis
            \note the default is that the pump is ON and runs according to its other
            \note operational requirements specified above.  The schedule is for special
            \note pump operations. Values here are between 0 and 1 and are multipliers
            \note on the previous field (Rated Flow Rate).
            \type object-list
            \object-list ScheduleNames
       N4 , \field Rated Pump Head
            \units Pa
            \default 179352
            \note default head is 60 feet
       N5 , \field Rated Power Consumption
            \units W
       N6 , \field Motor Efficiency
            \type real
            \minimum 0.0
            \maximum 1.0
            \default 0.9
       N7, \field Fraction of Motor Inefficiencies to Fluid Stream
            \minimum 0.0
            \maximum 1.0
            \default 0.0
       A5 , \field Heating Water Inlet Node Name
            \type node
       A6 , \field Heating Water Outlet Node Name
            \type node
       A7 , \field Heating High Water Temperature Schedule Name
            \note Water and control temperatures for heating work together to provide
            \note a linear function that determines the water temperature sent to the
            \note radiant system.  The current control temperature (see Temperature Control Type above) is
            \note compared to the high and low control temperatures at the current time.
            \note If the control temperature is above the high temperature, then the
            \note inlet water temperature is set to the low water temperature.  If the
            \note control temperature is below the low temperature, then the inlet
            \note water temperature is set to the high water temperature.  If the control
            \note temperature is between the high and low value, then the inlet water
            \note temperature is linearly interpolated between the low and high water
            \note temperature values.
            \type object-list
            \object-list ScheduleNames
       A8 , \field Heating Low Water Temperature Schedule Name
            \type object-list
            \object-list ScheduleNames
       A9 , \field Heating High Control Temperature Schedule Name
            \type object-list
            \object-list ScheduleNames
       A10 , \field Heating Low Control Temperature Schedule Name
            \type object-list
            \object-list ScheduleNames
       A11 , \field Cooling Water Inlet Node Name
            \type node
       A12, \field Cooling Water Outlet Node Name
            \type node
       A13, \field Cooling High Water Temperature Schedule Name
            \note See note for Heating High Water Temperature Schedule above for
            \note interpretation information (or see the Input/Output Reference).
            \type object-list
            \object-list ScheduleNames
       A14, \field Cooling Low Water Temperature Schedule Name
            \type object-list
            \object-list ScheduleNames
       A15, \field Cooling High Control Temperature Schedule Name
            \type object-list
            \object-list ScheduleNames
       A16, \field Cooling Low Control Temperature Schedule Name
            \type object-list
            \object-list ScheduleNames
       A17, \field Number of Circuits
            \type choice
            \key OnePerSurface
            \key CalculateFromCircuitLength
            \default OnePerSurface
       N8, \field Circuit Length
            \units m
            \default 106.7
       A18, \field Design Object Name
            \type object-list
            \object-list LowTemperatureRadiant:Design Object Name


**ZoneHVAC:Baseboard:RadiantConvective:Water,**

           \extensible:2 - repeat last two fields, remembering to remove ; from "inner" fields.
           \memo The number of surfaces can be expanded beyond 100, if necessary, by adding more
           \memo groups to the end of the list
           \min-fields 12
      A1,  \field Name
           \required-field
           \reference-class-name validBranchEquipmentTypes
           \reference validBranchEquipmentNames
           \reference ZoneEquipmentNames
      A2,  \field Inlet Node Name
           \required-field
           \type node
      A3,  \field Outlet Node Name
           \required-field
           \type node
      N1,  \field Rated Average Water Temperature
           \note Rated average water temperature is the average of the inlet and outlet water temperatures
           \note at rated conditions.
           \type real
           \maximum 150.0
           \minimum 20.0
           \units C
           \default 87.78
      N2,  \field Rated Water Mass Flow Rate
           \note Standard is I=B=R Rating document where all baseboards are rated at either 0.063 kg/s (1 gpm)
           \note or 0.252 kg/s (4 gpm).  It is recommended that users find data for the baseboard heater that
           \note corresponds to performance at 0.063 kg/s unless the flow rate is expected to be above 0.252 kg/s.
           \note If the flow rate is expected to be above 0.252 kg/s, this field should be 0.252 kg/s.
           \type real
           \maximum 10.0
           \minimum> 0.0
           \units kg/s
           \default 0.063
      N3 , \field Heating Design Capacity
           \type real
           \units W
           \minimum 0.0
           \autosizable
           \ip-units W
           \default autosize
           \note Enter the design heating capacity. Required field when the heating design capacity method
           \note HeatingDesignCapacity. This input field is rated heating capacity. Users must multiply the
           \note actual finned length published in the literature to determine the rated capacity. Rated
           \note Capacity is for an inlet air dry-bulb temperature of 18.0C, the Rated Water Mass Flow Rate
           \note of 0.063kg/s or 0.252kg/s, and the Rated Average Water Temperature between 32.2C and 115.6C.
      N4,  \field Maximum Water Flow Rate
           \required-field
           \autosizable
           \type real
           \units m3/s
           \ip-units gal/min
      A4, \field Design Object Name
           \type object-list
           \object-list ZoneHVAC:Baseboard:RadiantConvective:Design Object Name
      A5,  \field Surface 1 Name
           \begin-extensible
           \note Radiant energy may be distributed to specific surfaces
           \type object-list
           \object-list AllHeatTranSurfNames
      N5, \field Fraction of Radiant Energy to Surface 1
           \type real
           \minimum 0
           \maximum 1
      A6,  \field Surface 2 Name
           \type object-list
           \object-list AllHeatTranSurfNames
      N6, \field Fraction of Radiant Energy to Surface 2
           \type real
           \minimum 0
           \maximum 1
    .
    .
    .
     A104, \field Surface 100 Name
           \type object-list
           \object-list AllHeatTranSurfNames
     N104; \field Fraction of Radiant Energy to Surface 100
           \type real
           \minimum 0
           \maximum 1

**ZoneHVAC:Baseboard:RadiantConvective:Steam,**

           \min-fields 11
           \extensible:2 - repeat last two fields, remembering to remove ; from "inner" fields.
           \memo The number of surfaces can be expanded beyond 100, if necessary, by adding more
           \memo groups to the end of the list
      A1,  \field Name
           \required-field
           \reference-class-name validBranchEquipmentTypes
           \reference validBranchEquipmentNames
           \reference ZoneEquipmentNames
      A2,  \field Inlet Node Name
           \required-field
           \type node
      A3,  \field Outlet Node Name
           \required-field
           \type node
      N1 , \field Heating Design Capacity
          \type real
          \units W
          \minimum 0.0
          \autosizable
          \ip-units W
          \default autosize
          \note Enter the design heating capacity.Required field when the heating design capacity method
          \note HeatingDesignCapacity.
      N2,  \field Degree of SubCooling
           \type real
           \minimum 1.0
           \default 5.0
           \units deltaC
      N3,  \field Maximum Steam Flow Rate
           \required-field
           \type real
           \autosizable
           \minimum> 0.0
           \units m3/s
      A4, \field Design Object Name
           \type object-list
           \object-list ZoneHVAC:Baseboard:RadiantConvective:Design Object Name
      A5,  \field Surface 1 Name
           \begin-extensible
           \note Radiant energy may be distributed to specific surfaces
           \type object-list
           \object-list AllHeatTranSurfNames
      N4,  \field Fraction of Radiant Energy to Surface 1
           \type real
           \minimum 0
           \maximum 1
      A6,  \field Surface 2 Name
           \type object-list
           \object-list AllHeatTranSurfNames
      N5, \field Fraction of Radiant Energy to Surface 2
           \type real
           \minimum 0
           \maximum 1
    .
    .
    .
     A104, \field Surface 100 Name
           \type object-list
           \object-list AllHeatTranSurfNames
     N103; \field Fraction of Radiant Energy to Surface 100
           \type real
           \minimum 0
           \maximum 1
