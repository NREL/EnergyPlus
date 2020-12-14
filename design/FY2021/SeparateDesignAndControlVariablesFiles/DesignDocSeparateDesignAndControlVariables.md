

Separate Design and Control Variables
================

**Jermy Thomas, Dareum Nam, NREL.**


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

Conversation with E+ Team on Slack:
1. Create a design object for each radiant temp and baseboard object.
2. Maybe name the design oobject something else. 
 
## Approach/Timeline/Design Rationale ##

### Timeline:


 - Design document:   
	 - Send for reviews------------------ Nov 18 2020   
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

<span style="display:block;text-align:center">![How the objects will be split](https://github.com/NREL/EnergyPlus/blob/Separate-Design-and-Control-Variables/design/FY2021/SeparateDesignAndControlVariablesFiles/BigSplit.jpg)

<span style="display:block;text-align:center">**Figure: How the objects will be split**

<span style="display:block;text-align:center">![How the design objects will be brokendown](https://github.com/NREL/EnergyPlus/blob/Separate-Design-and-Control-Variables/design/FY2021/SeparateDesignAndControlVariablesFiles/BKDwn1.jpg)

<span style="display:block;text-align:center">![How the design objects will be brokendown](https://github.com/NREL/EnergyPlus/blob/Separate-Design-and-Control-Variables/design/FY2021/SeparateDesignAndControlVariablesFiles/BKDwn2.jpg)

<span style="display:block;text-align:center">![How the design objects will be brokendown](https://github.com/NREL/EnergyPlus/blob/Separate-Design-and-Control-Variables/design/FY2021/SeparateDesignAndControlVariablesFiles/BKDwn3.jpg)

<span style="display:block;text-align:center">**Figure: How the design objects will be brokendown**

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

Since only three design objects could be identified as common fields that could be used as design variables to all four baseboard objects, this does not warrant the need to differentiate between design and control variables. Also, it may be that a group of similar baseboard objects may be more commonly used as compared to others. Therefore, different types of baseboard object groupings were explored to find a group that would have a larger number of design variables. Grouping the `ZoneHVAC:Baseboard:RadiantConvective:Water` and `ZoneHVAC:Baseboard:RadiantConvective:Steam` as radiant baseboard  objects that used fluids for heating seemed like a good choice since it enabled grouping six common variables that could to be set as design variables:

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

Appropriate changes will be made to the Fortran transition InputRulesFiles. 

## References ##

None.

## Next Steps ##

Get feedback from the EnergyPlus team regarding:

 1. If these groupings make sense. 
 2. If any design variables need to be moved to control variables and vice versa. 
 3. Any other recommended changes.


## Changes to IDD file ##

The metadata to remain same, so it was taken out to improve readability. 

### New IDD Objects ####

**ZoneHVAC:LowTemperatureRadiant:Design,**

       \memo Design parameters for ZoneHVAC:LowTemperatureRadiant objects
       A1 , \field Name
       A2 , \field Fluid to Radiant Surface Heat Transfer Model
       A3 , \field Temperature Control Type
       N1 , \field Hydronic Tubing Inside Diameter
       N2 , \field Hydronic Tubing Outside Diameter
       N3 , \field Hydronic Tubing Conductivity      
       A4 , \field Condensation Control Type
       N4 , \field Condensation Control Dewpoint Offset
       A5 ; \field Changeover Delay Time Period Schedule

**ZoneHVAC:Baseboard:RadiantConvective:Design,**

           \memo Design parameters for ZoneHVAC:Baseboard:RadiantConvective objects
      A1 ,  \field Name
      A2 ,  \field Heating Design Capacity Method
      N1 ,  \field Heating Design Capacity Per Floor Area
      N2 ,  \field Fraction of Autosized Heating Design Capacity
      N3 ,  \field Convergence Tolerance
      N4 ,  \field Fraction Radiant
      N5 ;  \field Fraction of Radiant Energy Incident on People

### Modified IDD Objects ####

**ZoneHVAC:LowTemperatureRadiant:VariableFlow,**

            \min-fields 26
       A1 , \field Name
       A2 , \field Availability Schedule Name
       A3 , \field Zone Name
       A4 , \field Surface Name or Radiant Surface Group Name
       N1 , \field Hydronic Tubing Length
       A5 ,  \field Setpoint Control Type
       A6 , \field Heating Design Capacity Method
       N2 , \field Heating Design Capacity
       N3 , \field Heating Design Capacity Per Floor Area
       N5 , \field Maximum Hot Water Flow
       A7 , \field Heating Water Inlet Node Name
       A8 , \field Heating Water Outlet Node Name
       N6 , \field Heating Control Throttling Range
       A9 , \field Heating Control Temperature Schedule Name
       A10, \field Cooling Design Capacity Method
       N7 , \field Cooling Design Capacity
       N8 , \field Cooling Design Capacity Per Floor Area
       N9 , \field Fraction of Autosized Cooling Design Capacity
       N10, \field Maximum Cold Water Flow
       A11, \field Cooling Water Inlet Node Name
       A12, \field Cooling Water Outlet Node Name
       N11, \field Cooling Control Throttling Range
       A13, \field Cooling Control Temperature Schedule Name
       A14, \field Number of Circuits
       N12, \field Circuit Length
       A15; \field Design Object Name

**ZoneHVAC:LowTemperatureRadiant:ConstantFlow,**

            \min-fields 26
       A1 , \field Name
       A2 , \field Availability Schedule Name
       A3 , \field Zone Name
       A4 , \field Surface Name or Radiant Surface Group Name
       N1 , \field Hydronic Tubing Length
       N2 , \field Running Mean Outdoor Dry-Bulb Temperature Weighting Factor
       N3 , \field Rated Flow Rate
       A5 , \field Pump Flow Rate Schedule Name
       N4 , \field Rated Pump Head
       N5 , \field Rated Power Consumption
       N6 , \field Motor Efficiency
       N7 , \field Fraction of Motor Inefficiencies to Fluid Stream
       A6 , \field Heating Water Inlet Node Name
       A7 , \field Heating Water Outlet Node Name
       A8 , \field Heating High Water Temperature Schedule Name
       A9 , \field Heating Low Water Temperature Schedule Name
       A10, \field Heating High Control Temperature Schedule Name
       A11, \field Heating Low Control Temperature Schedule Name
       A12, \field Cooling Water Inlet Node Name
       A13, \field Cooling Water Outlet Node Name
       A14, \field Cooling High Water Temperature Schedule Name
       A15, \field Cooling Low Water Temperature Schedule Name
       A16, \field Cooling High Control Temperature Schedule Name
       A17, \field Cooling Low Control Temperature Schedule Name
       A18, \field Number of Circuits
       N8 , \field Circuit Length
       A19; \field Design Object Name

**ZoneHVAC:Baseboard:RadiantConvective:Water,**

          \min-fields 8
      A1, \field Name
      A2, \field Availability Schedule Name
      A3, \field Inlet Node Name
      A4, \field Outlet Node Name
      N1, \field Rated Average Water Temperature
      N2, \field Rated Water Mass Flow Rate
      N3, \field Heating Design Capacity
      N4, \field Maximum Water Flow Rate
      A5, \field Design Object Name
      A6, \field Surface 1 Name
      N5, \field Fraction of Radiant Energy to Surface 1
      A7, \field Surface 2 Name
      N6, \field Fraction of Radiant Energy to Surface 2
    .
    .
    .
     A105, \field Surface 100 Name
     N104; \field Fraction of Radiant Energy to Surface 100

**ZoneHVAC:Baseboard:RadiantConvective:Steam,**

           \min-fields 7
      A1,  \field Name
      A2,  \field Availability Schedule Name
      A3,  \field Inlet Node Name
      A4,  \field Outlet Node Name
      N1,  \field Heating Design Capacity
      N2,  \field Degree of SubCooling
      N3,  \field Maximum Steam Flow Rate
      A5,  \field Design Object Name
      A6,  \field Surface 1 Name
      N4,  \field Fraction of Radiant Energy to Surface 1
      A7,  \field Surface 2 Name
      N5,  \field Fraction of Radiant Energy to Surface 2
    .
    .
    .
     A105, \field Surface 100 Name
     N103; \field Fraction of Radiant Energy to Surface 100

