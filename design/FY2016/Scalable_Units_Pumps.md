#Scalable Units for Pump Sizing Calculations#

B. Griffith

Energy Archmage Company, for SEI/Trane.

23 October, 2015 

##Justification for Feature Update :

Scalable units for HVAC component models allow users and interface developers to better control the outcomes from autosizing.  The general idea is to add new fields to input objects that expose parameters used to scale sizes.  The design electrical power of pumps is currently scaled from a single assumed constant (0.78 W/((m3/s)-Pa) that is hard-coded inside the program.  There is no way for users and interface developers to assemble a library of pumps that have varying levels of efficiency that can be used with autosizing.  Exposing the scaling factor as a new user input, while retaining the original assumed efficiency as a default for that input, is justified because it will retain legacy behavior while also allowing users and interface developers to specify pump efficiency without having to use hard sizes.  This is part of a long-standing request for scalable units from a major interface developer.

##Overview:
This proposal involves modest changes to all five existing pump objects. The original, hard-coded sizing factor that is used to calculate autosize values for pump "Rated Power Consumption" will be replaced by a defaulted user input. For completeness, the variable speed pump will also add the capability of scaling the minimum flow rate using a new input for a sizing factor.   

##Approach:

New Input fields will be added to the end of all five existing pump input objects: Pump:VariableSpeed, Pump:ConstantSpeed, Pump:VariableSpeed:Condensate, HeaderedPumps:ConstantSpeed, HeaderedPumps:VariableSpeed.  The hard coded sizing parameter of 0.78 will then be available to users to revise if desired.  The sizing calculations will be changed to use the new input.



##Input/Output Reference:##

Input/Output Reference changes have been made in the file 01d-InputOutputReference.md (on the branch Scalable-Units-Pumps). This is a snippet for the new field. 
 
#### Field: Design Power per Unit Flow Rate per Unit Head

This field is optional.  If omitted the default value of 0.78 W/((m3/s)-Pa) will be used. This input is used when the input field Design Power Consumption is set to autosize.  It allows setting the efficiency of the impeller and drive when calculating the pump power consumption for the design flow rate, motor efficiency, and pump head. 

In addition to explaining the new input fields in the usual manner, existing input fields for pumps have been improved to reflect current practice.  For example, the use of "Rated" has been changed to "Design" in many input fields. Field descriptions are improved to reflect autosizing and defaults. 



## Implementation ##
The implementation of scalable units for pump objects will follow the existing patterns present in the code. Source code files Pumps.hh and Pumps.cc will be changed to read in the new input values and use them during sizing calculations.   
### OO Design###
No refactoring of existing code is planned.  This will be a minimal implementation with only slight changes to get input and sizing calculations. 

### Data Structures ###

The pump data structure (struct PumpSpecs) will be modified to add one variable to hold the user input for the power scaling factor and a second variable to hold the user input for minimum flow scaling factor. 

##Input Data Dictionary##

Proposed IDD changes have been drafted in the file Energy+.idd.in (on the branch Scalable-Units-Pumps).  In addition to the new input fields, some of the existing field names have been changed and /notes improved.  The new fields have been located at the end of the input object to avoid creating transition burden. 

##Output##

No new outputs are proposed.

##Engineering Reference (draft)##

Engineering reference changes have been drafted in the file 10-Sizing.md (on the branch Scalable-Units-Pumps).  Changes are very small, just to clarify that the 0.78 factor is a default that can be input rather than assumed. 

##Testing/Validation/Data Source(s)##
None.

##Example File and Transition changes##

A selection of existing example files will be revised to use the new input fields. 

No Transition needed as all the new input fields are located at the end of the input objects and are optional. 

##Other documents:

None.
