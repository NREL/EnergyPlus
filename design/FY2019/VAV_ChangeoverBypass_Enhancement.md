Enhancement: VAV Changeover Bypass
=================================================================

**Richard Raustad, FSEC**

 - 29 March 2019 - NFP - Initial Draft

## Justification for New Feature ##

The existing AirloopHVAC:UnitaryHeatCool:VAVChangeoverBypass model, a constant volume flow system, uses a bypass duct to short-circuit excess supply air back to the inlet of the unit. The following figure from the Engineering Reference illustrates the use and location of the bypass duct. 

<center>![](/image5053.png)</center>
<center>Figure 1. Schematic of Changeover Bypass VAV Unitary System with Blow Through Fan</center>

With this configuration the treated supply air that bypasses the zones mixes with the return air prior to entering the outdoor air mixer. At the higher bypass fractions this can lead to unreasonably low or high air temperatures entering the OA mixing box. When the AirloopHVAC:OutdoorAirSystem is used external to this model (upstream of the Air Inlet Node in the previous figure), mixing the bypass air in this fashion (i.e., after the external OA Mixer) leads to unexpected economizer operation since the true mixed air temperature entering the external OA Mixer would be different had the bypass air been mixed prior to the external OA Mixer.

Tiejun Wu at Carrier was the first to notice this problem and last year offered corrective suggestions:

    5/30/2018 - 1. Air side economizer. Thermodynamically this configuration is different from a
                   real configuration where the bypass mixer is upstream of the OA mixer. When bypass
                   flow ratio is low, the difference is small, but when bypass flow ratio is high, the
                   difference is significant.  This may trigger economizer to start operation at a
                   higher outdoor temperature than it normally would.
                2. “LoadPriority” control option. Currently there are “CoolingPriority”, “HeatingPriority”
                   and “ZonePriority” control options. With these options, the system could easily
                   get stuck in a mode or frequently cycling between cooling and heating mode.
                   In the attached Carrier VVT control sequence, the mode of operation is determined
                   by comparing the total zone cooling demand and heating demand (page 24).
                   System changeover is based on both a mode change request and minimum elapsed time in a mode.
                3. Model ceiling plenum bypass and return. In field application, many VVT system use
                   a ceiling return plenum configuration. With this configuration, the bypass air will get
                   discharged into the ceiling plenum and then mix with zone return air before flow back to
                   the unit. The current unitary system object assumes adiabatic ducted bypass which may
                   underestimate the system load when used to model a ceiling plenum return configuration.
                   Attached please find a Carrier VVT bypass system layout diagram (Figure 42 and 43 on page 35).

    Suggestions: 1.  Model an air side economizer properly.
                 2.  Add a priority control mode “LoadPriority” with minimum run time setting in a mode.
                 3.  Allow modeling of ceiling plenum bypass and return. 

The solution to the above mentioned problem is to move the internal bypass duct away from the model and mix the bypass air using an AirloopHVAC:ReturnPlenum or AirloopHVAC:ZoneMixer (if a plenum is not used). Figure 2 represents the proposed enhancement to the Changeover Bypass model. The bypass duct leaves the system at the Bypass Duct Splitter Node and connects to existing air loop mixing objects.

<center>![](/ChangeoverBypassVAV_ReturnPlenumOrMixer.png)</center>
<center>Figure 2. Proposed changes to Changeover Bypass VAV Unitary System Model</center>


###Proposed Enhancement Summary:

 1.  Allow AirloopHVAC:UnitaryHeatCool:VAVChangeoverBypass object to be connected to a return plenum or zone mixer.
 2.  Expand priority control type options to include LoadPriority and minimum time limit.


 
## E-mail and  Conference Call Conclusions ##

3/28/2019 - Provide initial schematic of proposed changes to select individuals for review.

3/28/2019 - Initial response agrees that these changes would address the underlying issue.

3/29/2019 - provide Initial draft NFP


## Overview and Approach ##

When a user connects the HVAC system's bypass duct to a return plenum or mixing box, the internal calculation of the mixed air stream conditions will no longer be required. The model will check to determine if the splitter node is used as an inlet node in the return plenum or zone mixer object. If so, inlet air conditions will be passed directly to the mixed air node. If not, and as the model currently does now, a mixing problem is used to determine the mixed air node conditions.

*Pros* - Backward compatibility with previous model configuration, enhanced economizer operation and more realistic configuration with real world applications.

*Cons* - None. 

## Testing/Validation/Data Sources ##

Build example files and check results.

## Input Output Reference Documentation ##

Doc changes will include description of new control option.

## Engineering Reference ##

Doc changes will include description of new control option and new configuration example figure.

## Example File and Transition Changes ##

Proposed example file:

 - Create a copy of the `ChangeoverBypassVAV.idf` example file to include an example for the connection to the AirloopHVAC:ZoneMixer.

Transition will not be required.


## References ##





