Allow Multiple Scheduled Shades to Reference a Single Window
================

**Jason Glazer, GARD Analytics**

 - May 11, 2020
 

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
interior shading is defined by summer and winter shading factors. The implementation will provide appropriate 
error checking  for, e.g., two shading control schedules that overlap, two shading controls that are not scheduled, etc.

## Approach ##

The error message is currently produced in SufaceGeometry::InitialAssociateWindowShadingControlFenestration and is due to
a single windows shading control pointer allowed for each Surface() in EnergyPlus. This will be changed to allow for a vector
of references and all code related to this will be modified to allow multiple window shading controls.

## Testing/Validation/Data Sources ##

insert text

## Input Output Reference Documentation ##

No new fields or objects will be added but a paragraph will be added to WindowShadingControl describing this new capability.

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



