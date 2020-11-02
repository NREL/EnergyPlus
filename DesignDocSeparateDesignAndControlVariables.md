
Separate Design and Control Variables
================

**Jermy Thomas, Dareum Nam, NREL.**

 - Original date TBD


## Table of Contents ##

[Justification for New Feature](#justification-for-new-feature)

[E-mail, Conference Call, Other Communications and Conclusions](#e-mail-conference-call-other-communications-and-conclusions)

[Approach/Timeline](#approachtimeline)

[Testing/Validation/Data Sources](#testingvalidationdata-sources)

[Input Output Reference Documentation](#input-output-reference-documentation)

[Engineering Reference](#engineering-reference)

[Example File and Transition Changes](#example-file-and-transition-changes)

[References](#references)

[Design](#design)

[Next Steps](#next-steps)

## Justification for New Feature ##

The LowTemperatureRadiant and BaseBoard objects in EnergyPlus currently has the  control and design fields lumped together. This results in a large number of fields for these objects as seen in the objects below:
 - The `ZoneHVAC:LowTemperatureRadiant:VariableFlow` object has 34 fields
 - The `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` object has 34 fields
 - The `ZoneHVAC:Baseboard:RadiantConvective:Water` object has 17+ fields*
 - The `ZoneHVAC:Baseboard:RadiantConvective:Steam` object has 17+ fields*
 
 Lumping of design and control parameters together results in many of the variables being repeated for every zone. This causes difficulty and confusion for the users as the control variables get lost in the mixture of design and control variables. There have been requests from users to split these fields into design and control variables. 

Splitting the variables into control and design variables may result in 

 1. Ease of creating new objects, with less fields to fill in. 
 2. Decreased confusion from too many fields, and may result in less errors from having too many variables. 
 3. If there is a case where different types of radiant systems (for example, LowTemperatureRadiant:VariableFlow and LowTemperatureRadiant:ConstantFlow) exist together in a model, a design object could provide common information to both these systems.
 
 (*The number of fields will increase with number of surfaces) 

## E-mail, Conference Call, Other Communications and Conclusions ##

Summary of communication from UC Berkeley:

Separate Design and Control Parameters

“Potentially separate the lowtemp: radiant objects such that one main object contains the design parameters of the radiant system (e.g. amount of tubing, hot and chilled water loop connections), while the other contains the control parameters (e.g. two position, modulating, zone circulator pump, etc.).” (Quote from CBE)   Right now, all of the parameters associated with a radiant system in EnergyPlus are contained in a single input syntax.  The concern here is that much of the control information is probably pretty similar from system to system within a single user input file.  So, there could be less work and smaller files if the input was broken up into two separate inputs, allowing many radiant systems to re-use a single control definition.  This could potentially be applied to other input syntax beyond the low temperature radiant systems.


[comment]: <> (### Questions and Comments Received through June 1 ##)
[comment]: <> (### Responses/Clarifications through June 1 ###)
[comment]: <> (### Conference Call Conclusions June 5 ###)

 
## Approach/Timeline ##

Timeline:


 - Design document:   
	 - Send for reviews------------------ Nov 9 2020   
	 - Finalize design document-------- Dec 18 2020
 - Implementation:   
	 - First draft of Implementation---- Jan 31 2021
	 - Final Implementation------------- Feb 28 2021
 - Testing:   
	 - Testing done, bugs worked out--- Mar 30 2021
 - Buffer: 1 month

##### Comment #####
Project to end by April 2021 since Jermy's internship ends by then. 

The proposed approach is to identify the design variables in the LowTemperatureRadiant and Basebaoard objects and create separate design objects for the LowTemperatureRadiant and Basebaoard objects.


## Testing/Validation/Data Sources ##

Regression tests will be done, to ensure that no changes to the current models have been made. 

## Input Output Reference Documentation ##

### Inputs Description ###

The new objects will have the fields that were removed from the old/modified objects. There will be no changes made to the description so I am ignoring that here to save space and avoid unnecessary reading. 

#### ZoneHVAC:LowTemperatureRadiant Objects

##### *New object* - `ZoneHVAC:LowTemperatureRadiant:Design` ####

This `ZoneHVAC:LowTemperatureRadiant:Design` object is referenced in the `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` objects and provide additional design parameters to them. Multiple `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` can be mapped to a single `ZoneHVAC:LowTemperatureRadiant:Design` object.

*Field: Name*    
*Field: Availability Schedule Name*    
*Field: Radiant Exchange Method*    
*Field: Fluid to Radiant Surface Heat Transfer Model*    
*Field: Temperature Control Type*    
*Field: Hydronic Tubing Inside Diameter*    
*Field: Hydronic Tubing Outside Diameter*    
*Field: Hydronic Tubing Conductivity*    
*Field: Condensation Control Type*    
*Field: Condensation Control Dewpoint Offset*    
*Field: Changeover Delay Time Period Schedule*

##### *Modified objects* - `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and  `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` ####

All of the fields in `ZoneHVAC:LowTemperatureRadiant:Design` will be removed from `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and  `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` objects. Additionally, we add the following field and description:

*Field: Design object*

This field cannot be blank and it should point to one of the `ZoneHVAC:LowTemperatureRadiant:Design` objects. 

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

This field cannot be blank and it should point to one of the `ZoneHVAC:Baseboard:RadiantConvective:Design` objects. 

### Outputs Description ###

As of the current iteration of this project, the outputs are to remain the same. 
## Engineering Reference ##

TBD.

As of now, changes to the Engineering Reference document will be minimal since the proposed changes do not include changes to any physical phenomenon. 

## Example File and Transition Changes ##

All of the current example files that use `ZoneHVAC:LowTemperatureRadiant:VariableFlow`, `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` and `ZoneHVAC:Baseboard:RadiantConvective:Water`, `ZoneHVAC:Baseboard:RadiantConvective:Steam` will be modified so that the design and parameters are different.

## References ##

None.

## Design ##

Four types of radiant systems can be modeled in EnergyPlus. In this project, we focus on the LowTemperatureRadiant and Baseboard:RadiantConvective objects. We assume that the current input fields for these objects are the parameters that will be split into design and control parameters. This is attempted using the following three steps:

 1. Identifying and differentiating between design, and control variables. (Finish Design Document)
 2. Implementation
 3. Testing

### 1. Identifying and differentiating between design and control variables ###

#### ZoneHVAC:LowTemperatureRadiant objects ####

There are three ZoneHVAC:LowTemperatureRadiant objects with the following number of input fields:

 1. `ZoneHVAC:LowTemperatureRadiant:VariableFlow` - 34 fields
 2. `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` - 34 fields
 3. `ZoneHVAC:LowTemperatureRadiant:Electric` - 12 fields

We decided to ignore the `ZoneHVAC:LowTemperatureRadiant:Electric` since it only has 12 fields, and therefore does not have a drastic need to reduce inputs as the VariableFlow and ConstantFlow systems. Also, `ZoneHVAC:LowTemperatureRadiant:Electric` being different from the fluid (`ZoneHVAC:LowTemperatureRadiant:VariableFlow` and `ZoneHVAC:LowTemperatureRadiant:ConstantFlow`) objects, trying to find parameters common to all three would result in very few design parameters (only two in our preliminary study) that could be extracted out. 

Therefore, the `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` were inspected and nine common parameters were identified. 

The following common variables could be set as design variables since it seems like they could be shared between different `ZoneHVAC:LowTemperatureRadiant:VariableFlow` and/or `ZoneHVAC:LowTemperatureRadiant:ConstantFlow` objects in a model:

 - Availability Schedule Name
 - Fluid to Radiant Surface Heat Transfer Model  
 - Temperature Control Type  
 - Hydronic Tubing Inside Diameter 
 - Hydronic Tubing Outside Diameter 
 - Hydronic Tubing Conductivity
 - Condensation Control Type
 - Condensation Control Dewpoint Offset
 - Changeover Delay Time Period Schedule

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
 - Best way to decide what would be ideal system parameters would be to get feedback in some way from the current users. This would mean that the current users of radiant systems should be identified as well. 
 - Some of the common objects are optional fields or depend on other fields. Just wanted to point out that it should not be assumed that every field has the same importance. 

#### ZoneHVAC:Baseboard:RadiantConvective Objects ####

Five types of ZoneHVAC:Baseboard + one type of ZoneHVAC:CoolingPanel objects can be modelled using EnergyPlus.  Out of these six objects, four are RadiantConvective and these were focused on in this study. These objects are 

 1. `ZoneHVAC:Baseboard:RadiantConvective:Water` with 17+ fields
 2. `ZoneHVAC:Baseboard:RadiantConvective:Steam` with 17+ fields
 3. `ZoneHVAC:Baseboard:RadiantConvective:Electric` with 13+ fields
 4. `ZoneHVAC:CoolingPanel:RadiantConvective:Water` with 22+ fields

Common fields for all four objects together were identified and the following common variables were decided to be set as control variables/unable to be grouped as design parameters since they will be specific to every object in a radiant system:

 - Name 
 - Inlet Node Name 
 - Outlet Node Name 
 - Heating & Cooling Design Capacity {W}  
 - Maximum Water Flow Rate 
 - Surface Name, Fraction of Radiant Energy to Surface

It was identified that the following parameters can be considered to be common if all four objects are considered:

 1. Availability Schedule Name
 2. Fraction Radiant
 3. Fraction of Radiant Energy Incident on People

Since three design objects does not warrant the need to differentiate between design and control variables, and also it may be that a group of the similar baseboard objects may be more commonly used as compared to others, different types of groupings were identified to find if more variables may be grouped to find if this would yield a larger chunk of parameters that could be set aside as design variables. 

Grouping the `ZoneHVAC:Baseboard:RadiantConvective:Water` and `ZoneHVAC:Baseboard:RadiantConvective:Steam` as radiant baseboard  objects that used fluids for heating seemed like a natural grouping that yielded  the following seven common variables that could to be set as design variables:

 - Availability Schedule Name 
 - Fraction Radiant  
 - Fraction of Radiant Energy Incident on People 
 - Heating Design  Capacity Method 
 - Heating Design Capacity Per Floor Area {W/m2} 
 - Fraction of Autosized Heating Design Capacity 
 - Convergence Tolerance

##### Comments #####
 - As in the LowTemperatureRadiant objects, some of the common objects are optional fields or depend on other fields. Just wanted to point out that it should not be assumed that every field has the same importance. 

### 2. Implementation ###

#### Changes to the code ####

TBD

#### Changes to the IDD file ####

TBD

#### Changes to the test files ####

TBD

### 3. Testing ###

TBD

## Next Steps ##

Get feedback from the EnergyPlus team regarding:

 1. If these groupings make sense. 
 2. If any design variables need to be moved to control variables and vice versa. 
 3. Any other recommended changes.

