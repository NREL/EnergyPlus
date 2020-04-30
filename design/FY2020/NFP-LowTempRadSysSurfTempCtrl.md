Low Temperature Radiant System Surface Temperature Control
================

**Rick Strand, University of Illinois at Urbana-Champaign**

 - Original Date: April 29, 2020
 - Revision Date: (none, original version)
 

## Justification for New Feature ##

Based on research that has been done in the past, the Center for the Built Environment at the University of California at Berkeley has identified some necessary improvements to the EnergyPlus low temperature radiant system models.  These improvements would allow for more appropriate modeling of those systems in practice and would bring EnergyPlus up to date on current best practices in this area.  There are two groups of implementation requests and this document describes the first series of changes which all generally relate to how a low temperature radiant system is controlled.

## E-mail and Conference Call Conclusions ##

External Communications: To date, the author has had contact with researchers at the CBE both via email and through a 2 hour Zoom meeting.  As a result of the emails and the Zoom meeting, the author of this NFP created a “wishlist” document that summarized the request from CBE, the current status of the EnergyPlus code in relation to the CBE request, and an analysis of the potential implementation of the request in EnergyPlus (potential approach, priority, and estimated level of effort).  Some of the information in this NFP is based on that “wishlist” document.  That document was sent to the researchers from the CBE for their review, and the response was their support of the priorities and the approach as written.

## Overview ##

The first portion of this first series of changes relates to the options for controlling a radiant system.  Currently in EnergyPlus, the user has the option to control the system based on the zone MAT, zone MRT, zone Operative Temperature, the outdoor dry-bulb temperature, and the outdoor wet-bulb temperature.  Many systems in practice, particularly ones that have a significant amount of mass/concrete, tend to use surface temperature control.  This can mean that either the actual surface of the radiant slab is used to control the system or that the temperature at some depth is used as the control parameter.  Both of these surface control types are to be implemented with this work.

The second portion of this first series of changes relates to the throttling limits for controls.  At the current time in EnergyPlus, there is a minimum throttling range of 0.5C.  This was done to avoid potential instability that was assumed would happen if the system bounced back and forth between full on and full off operation.  However, there are some systems in practice which actually do operate like a two-position valve.  Thus, it would be helpful to model such a system in EnergyPlus as well.

The third and final portion of this first series of changes relates to how EnergyPlus uses the setpoint temperature.  Currently, the setpoint temperature is defined as the point at which the system is running at 50% of full flow for a variable flow.  This has led to some confusion since current industry standards use this setpoint as the point at which there is no flow.  This involves changing how this setpoint is interpreted inside EnergyPlus for the variable flow radiant system only.  The constant flow and the electric radiant systems already comply with the industry standard.

## Approach ##

For simplicity in development, these three items may be separated into three separate issues in GitHub and Pivotal Tracker.

For the changes relating to surface temperature control, the approach here will be to add surface control by using either the inside surface face temperature or a temperature at some depth rather than the other temperature options.  From there, the different radiant system models will use that to provide appropriate control for the system.  As this is done, this portion of the control algorithm will be converted to a new subroutine since all three of the radiant system models use similar code to grab the appropriate temperature for control.

The approach for dealing with the temperature internal to the radiant slab will be solved by using existing input.  In the Construction:InternalSource input, the user already has the possibility to request a temperature at a particular location.  This information will now also be used for control when the user picks the slab (interior) temperature as the controlling parameter.  In addition, when the user requests a 2-D simulation of the radiant system, the temperature inside the slab will be changed.  Currently, in the 2-D solution, the temperature is taken at the depth requested by the user but in line with the “source” point.  The temperature used for control in practice is more aligned with the mid-point between tubing.  So, the calculation of that internal temperature during a 2-D solution will be changed so that the temperature is at the depth requested but at the mid-point between the tubes rather than in line with them.

The approach to dealing with the on-off control is to relax the check on throttling range so that a throttling range can be less than 0.5 but no smaller than 0.0.  If the throttling range is less than 0.5, a warning message will be produced and the simulation allowed to continue.  There will have to be some additional code in the electric radiant system that avoids a divide by zero since in one place it divides by throttling range, but when the user picks a 0.0 throttling range, it can simply do a slightly modified sequence where the panel is either on or off which should be fairly straight forward.

The change for the variable flow radiant system to use a zero flow when the setpoint is met is a matter of adjusting the equations that define what the flow is.  Currently, if defines the zero flow point as the control temperature adjusted by half of the throttling range (or basically 50% flow at the setpoint temperature rather than 0% flow).  This change is relatively straight forward and will require testing as well as documentation changes to reflect this.  As this is a change in the interpretation of an input parameter rather than an actual change to the input in EnergyPlus, no transition changes are recommended.

## Testing/Validation/Data Sources ##

Testing will be done using unit tests as well as new/modified input files in the existing EnergyPlus series of test files.  The changes/additions will include at least one example for each of the new control types and also looking at using a 0.0 throttling range.  The setpoint temperature for variable flow radiant systems will be checked comparison EnergyPlus output before and after the change.  So, this will rely on existing EnergyPlus test files and modified versions of those files.  

## Input Output Reference Documentation ##

For the new control options (surface face and surface internal), the text for variable flow radiant systems currently is:

1.34.7.1.7 Field: Temperature Control Type.  This field specifies along with the throttling range and setpoint schedules how the user wishes to control the hydronic radiant system. The temperature denoted in the setpoint schedule can refer to one of five different temperatures: the zone mean air temperature, the zone mean radiant temperature, the zone operative temperature, the outdoor dry-bulb temperature, or the outdoor wet-bulb temperature. The choice of temperature is controlled by the current field—temperature control type. The user must selectfrom the following options:

* MeanAirTemperature
* MeanRadiantTemperature
* OperativeTemperature
* OutdoorDryBulbTemperature
* OutdoorWetBulbTemperature

Operative temperature for radiant system controls is the average of Mean Air Temperature and Mean Radiant Temperature. If the user does not select a control type, MeanAirTemperature control is assumed by EnergyPlus. See the throttling range and control temperature schedule fields below for more information.

The list will be expanded to include the options SurfaceFaceTemperature and SurfaceInteriorTemperature.  In the last paragraph after the description of the Operative temperature, new text will be added to the effect:

The SurfaceFaceTemperature option allows the user to control the radiant system using the inside face surface temperature of the radiant system (the inside surface temperature).  The SurfaceInteriorTemperature option will allow the user to control the radiant system using a surface temperature that is calculated inside the radiant system.  This point will be defined by the Construction:InternalSource description for the system.  In that input, the user has the option to calculate a temperature at a particular point in the construction.  The radiant system will then use this information for controlling the slab not just producing temperatures for outputting at that point.  Users should consult the input for Field: Temperature Calculation Requested After Layer Number for more information.

Similar changes need to be made in the constant flow and electric radiant systems.

Note that the text for the Field: Temperature Calculation Requested After Layer Number in Contstruction:InternalSource will also be modified to reflect this additional interpretation of this input field and the change to its interpretation for 2-D solutions.  Here is the proposed additional text:

In addition, this field is also used by the radiant system model when the user selects the SurfaceInterior control method.  In this control type, the user is controlling the system based on a temperature on the interior of the slab.  This field then also sets this point inside the slab for this control.  Note also that when this field is used in conjunction with a 2-D solution (see next input field) that the interior temperature being calculated (and then used for control) will be calculated at the depth specified and at the mid-point between the tubing (not at the horizontal point in-line with the tubing).


## Input Description ##

Changes need to be made to existing descriptions (see previous section).

## Outputs Description ##

No new input is being proposed so no changes need to be made to the output descriptions except clarification of the existing output (Surface Internal User Specified Location Temperature) similar to what is mentioned above for the input.

## Engineering Reference ##

A variety of changes will need to be made to the Engineering Reference in the section on Low Temperature Radiant System Controls.  The new control options will need to be defined and the changes to the throttling range and its interpretation will also need to be made.

## Example File and Transition Changes ##

New and modified example files will be created to test the new controls that are implemented in this work.  No transition changes are expected since the new controls will simply be a new option for an existing input field.

## References ##

Google document authored by CBE (their own internal document)
Current EnergyPlus code and documentation
RadiantControlsWorkWishlistWithEvaluation.docx (author’s internal document, submitted to CBE and NREL and available upon request)



