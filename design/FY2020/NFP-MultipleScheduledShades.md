Allow Multiple Scheduled Shades to Reference a Single Window
================

**Jason Glazer, GARD Analytics**

 - May 12, 2020
 

## Justification for New Feature ##

Two related requests to add flexiblity to window and shading have been made. The first is to allow modeling a window with 
different shading coefficient during the summer and window. This is based on a requirement in "ANSI/RESNET/ICC 301-2014 
Standard for the Calculation and Labeling of the Energy Performance of Low-Rise Residential Buildings" available here:

http://www.resnet.us/wp-content/uploads/archive/resblog/2016/01/ANSI-RESNET-ICC_301-2014-Second-Edition-Publish-Version.pdf

which requires (see page 14) that glazing should have an interior shade coefficient of 0.70 in the summer and 0.85 in the 
winter.

When trying to implement this using multiple WindowShadingControl objects, each using a scheduled shading 
control, to reference a single window now produces an error (“Fenestration surface named "FOOBAR" appears on
more than one WindowShadingControl list”). 

The second request is related to allowing multiple WindowMaterial:* objects to be used for a window. This would allow 
EnergyPlus to model a window with, for example, both insect screens and interior shades. This requirement is prompted by
CBECC-Res and EnergyPlus Comparison: Report to the California Energy Commission which stated "Windows cannot be modeled 
with multiple coverings (e.g., insect screens and interior drapes)."

The first request will be the focus of the effort, if effort remains after resolving the first request, the second request
will be also addressed.


## E-mail and  Conference Call Conclusions ##

insert text

## Overview ##

Address the first request to allow multiple WindowShadingControl object to be used for a single window and not produce
an error. Error message that is currently produced “Fenestration surface named "FOOBAR" appears on
more than one WindowShadingControl list” will be removed to accommodate residential buildings where 
interior shading is defined by summer and winter shading factors. 

Also, where multiple WindowShadingControls each deploy the same shade on the same window(s) in a zone, no error 
message will be generated and the shade will be deployed if any of the WindowShadingControls are activated.

## Approach ##

The error message is currently produced in SufaceGeometry::InitialAssociateWindowShadingControlFenestration and is due to
a single windows shading control pointer allowed for each Surface() in EnergyPlus. This will be changed to allow for a vector
of references and all code related to this will be modified to allow multiple window shading controls.

If the schedules enable two different WindowShadingControls to be active at the same time (scheduled or not), and 
they deploy the same shading material or shading construction they will any being control activation will prompt the 
deployment of the shading material or shaded construction. In other words, if multiple WindowShadingControls activate the 
same shade, they will behave in an "OR" fashion where any active WindowShadingControl will deploy the shade. 

The implementation will provide appropriate error checking for, e.g., warnings when two or more shading control schedules that 
overlap, errors if they overlap with different materials, warnings if two shading controls that are not scheduled, etc.
Testing will also be conducted to see the difficulty if allowing WindowShadingControls to control shades in other zones that are 
part of the same solar enclosure.

## Testing/Validation/Data Sources ##

insert text

## Input Output Reference Documentation ##

No new fields or objects will be added but paragraphs will be added to WindowShadingControl describing this new capability.

## Input Description ##

insert text

## Outputs Description ##

insert text

## Engineering Reference ##

insert text

## Example File and Transition Changes ##

No transition is needed. A new example file that demonstrates this capability will be added.

## References ##

insert text



