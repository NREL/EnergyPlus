Simplified Shading
================

**Yujie Xu, Tianzhen Hong**

**Lawrence Berkeley National Laboratory**

- Original Date: April 25, 2022
- Modified Date: April 27, 2022

## Justification for New Feature ##

Currently, EnergyPlus models shading devices in *WindowMaterial:Shade* and *WindowMaterial:ComplexShade*. Even for the non-complex shade, users need to specify at least nine input fields including solar and visible transmittance and reflectance, infrared hemispherical emissivity and transmittance, thickness, conductivity, etc. This level of complexity could hinder the application of the shading model in EnergyPlus: when the required input fields are difficult to acquire, users might choose to not model shading devices at all. One such case could be converting models from other simulation tools with less detailed shading methods. The omission of shading devices could lead to an overestimation of cooling loads in the summer and an underestimation of the heating load in winter.

The proposed new feature will enable a simplified way to specify the properties of a shading device with a multiplier for solar transmittance and visible transmittance. The multiplier can be a single fixed value or a schedule of values. An idd object *WindowMaterial:SimplifiedShade* will be added to enable users to specify the multiplier. This multiplier resembles the shading-fraction in DOE-2, which can adjust the solar heat gain through windows with a multiplier schedule.

This post on unmethours.com shows a need for such a simplified shading method in EnergyPlus: https://unmethours.com/question/342/what-assumptions-do-you-make-for-modeling-blinds/ 

## Overview ##

Shading devices in EnergyPlus are modeled as a physical layer inside or outside of a window, or between glass layers within a window. Currently, EnergyPlus models in great detail the convective and radiative interaction between the shading layer and the glass layers and the indoor or outdoor environment, as well as the heat absorbed by the shading device.

The most commonly used *WindowMaterial:Shading* requires nine inputs, which might not be readily available. For example, DOE-2 or eQUEST can model shading devices as a fractional adjustment to reduce the solar transmission from the outside to the inside. When converting such DOE-2 or eQUEST models into an EnergyPlus model, users have to go through a trial-and-error process, as there is no corresponding fractional adjustment in EnergyPlus.

This feature will provide a simplified way of discounting indoor-outdoor solar and visual transmission using a similar fractional adjustment as DOE-2. This allows easier translation between EnergyPlus and other building energy modeling tools. In this feature, the shading will not be modeled as a physical layer as EnergyPlus currently does. It only adjusts the solar and visible transmittance of the corresponding window, without considering the detailed interactions between shading devices and other adjacent objects or environments.

## Approach	##

With the added *WindowMaterial:SimplifiedShade* object, users will be able to input a fractional adjustment α between 0 and 1, either as a constant value or as a schedule. The fractional adjustment will be multiplied to the solar transmittance (`state.dataMaterial->Material(MaterNum).Trans`) and visual transmittance (`state.dataMaterial->Material(MaterNum).TransVis`) of the corresponding windows before entering heat balance calculation. The solar transmittance and visible transmittance of the *WindowMaterial:Glazing* object is from the input fields “Solar Transmittance at Normal Incidence” and “Visible Transmittance at Normal Incidence”. For *WindowMaterial:Glazing:RefractionExtinctionMethod* object, the solar and visible transmittance is derived from user input thickness, solar and visible extinction coefficient, and refraction. For the simple glazing object, the adjustment will be applied to the Solar Heat Gain Coefficient and the Visible Transmittance.

## Testing/Validation/Data Source(s) ##

The feature will be tested and demonstrated with a test file derived from *UnitarySystem_DXCoilSystemAuto.idf* by adding a *WindowMaterial:SimplifiedShade* object. The time-step output of window solar heat gains and transmitted visible illuminance will be manually checked (also using extreme values: 0 for perfect reflective shade and 1 for no shade or perfect transparent shade).

## IDD Object changes ##

A new object *WindowMaterial:SimplifiedShade* will be created.

    WindowMaterial:SimplifiedShade,
          \min-fields 3
      A1, \field Name
          \required-field
          \type alpha
          \reference MaterialName
          \reference WindowShadesScreensAndBlinds
      A2, \field Shading Multiplier Input Method
          \type choice
          \key FixedMultiplier
          \key ScheduledMultiplier
          \default FixedMultiplier
      N1, \field Shading Multiplier
          \note a constant multiplier for window solar transmittance 
          \note and visible transmittance
          \type real
          \units dimensionless
          \minimum 0
          \maximum 1
          \default 0.7
      A3; \field Shading Multiplier Schedule Name
          \note The schedule values should be greater than or equal 
          \note to 0 and less than or equal to 1.
          \type object-list
          \object-list ScheduleNames

## Proposed additions to Meters ##

N/A
 
## Proposed Report Variables ##

N/A

## References ##

