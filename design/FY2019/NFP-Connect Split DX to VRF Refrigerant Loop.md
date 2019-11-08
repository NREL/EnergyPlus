﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿Connecting Split DX AHU Components to VRF Refrigeration Loops
================

**Richard Raustad, Florida Solar Energy Center**

 - 11/7/19 - Draft NFP
 -

## Justification for New Feature ##

One of the recent developments in the variable refrigerant flow systems (VRF) field is the ability to connect terminal devices other than indoor split fan coils to a VRF refrigerant loop. These new terminal devices include DOAS AHUs and rooftop mounted split DX AHUs for space conditioning and ventilation.

Nearly all VRF applications need ventilation. Previously, ventilation had to be provided by separate packaged rooftop type DOAS equipment since VRF indoor units cannot handle raw outdoor air directly and VRF condensing units could not serve DOAS equipment. In addition, one of the energy efficiency limitations of VRF systems is the lack of an outdoor air economizer cycle. Both challenges have now been solved.

Multiple major manufacturers such as Daikin, Mitsubishi, LG, and Toshiba-Carrier offer the ability to connect split DX DOAS AHUs to VRF condensing units. In addition, Toshiba-Carrier condensing units can be connected to multiple rooftop-mounted split DX AHUs that provide space conditioning and ventilation and can offer outdoor air economizer cycles. These developments extend the application of VRF systems and the energy efficiency of these systems.

Currently, VRF systems in EnergyPlus only allow indoor DX fan coil terminals to be connected to a VRF refrigerant loop.

## E-mail and  Conference Call Conclusions ##


## Overview ##

This task will expand the capabilities of the VRF model to allow terminal devices such as split DX DOAS AHUs and split DX AHUs for space conditioning and ventilation to be connected to VRF refrigerant loops. The following figure is excerpt from the Eng. Ref. (Figure 16.51, pg 1163, V9.2) and modified to include VRF air loop and outdoor air equipment (red circle).

Figure 1: ![Figure 1](https://github.com/NREL/EnergyPlus/blob/NFP---Connect-split-DX-to-VRF-refrigerant-loop/design/FY2019/VRFTU%20Allowed%20in%20Airloop-OASys.png) Variable Refrigerant Flow Heat Pump (draw through fan placement)


## Approach ##

The common connection of VRF TU's and the VRF condenser is the ZoneTerminalUnitList object. All ZoneHVAC:TerminalUnit:VariableRefrigerantFlow objects connected to a single VRF condenser (AirConditioner:VariableRefrigerantFlow or AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:*) are listed in the ZoneTerminalUnitList object and this objects name is entered as an input in the VRF condenser object.

The most straight-forward approach appears to be allowing the ZoneHVAC:TerminalUnit:VariableRefrigerantFlow object to be connected as zone (existing), air loop and outdoor air system equipment. Using this methodology, limited changes to the AirConditioner:VariableRefrigerantFlow and AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:* objects are anticipated.

## Controls:
  ZoneHVAC Equipment: no changes (currently load based control)
  AirloopHVAC Equipment: requires thermostat control zone input, load based control
  OutdoorAir Equipment: controlled to outlet node set point temperature (and humidity ratio?)

## Testing/Validation/Data Sources ##

New test files will be created using VRF TUs in the air loop and outdoor air system.

## Input Output Reference Documentation ##

IO Ref text will be updated to include these new capabilities.

## Input Description ##

Changes to the IDD appear to be minimal as no new objects will be added to E+. The only changes to model inputs are the allowed location of the TU object and thermostat location for controlling air loop equipment.

```
ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,  
        \memo Zone terminal unit with variable refrigerant flow (VRF) DX cooling and heating coils  
        \memo (air-to-air heat pump). The VRF terminal units are served by an  
        \memo AirConditioner:VariableRefrigerantFlow or  
        \memo AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:* system.  
        \min-fields 19  
  A1 ,  \field Zone Terminal Unit Name  
        \required-field  
        \type alpha  
        \reference ZoneTerminalUnitNames  
        \reference DOAToZonalUnit  
        \reference ZoneEquipmentNames  

        -- new references as follows (not yet tested) --  
        \reference-class-name validBranchEquipmentTypes  
        \reference validBranchEquipmentNames  
        \reference-class-name validOASysEquipmentTypes  
        \reference validOASysEquipmentNames  

        -- new field --  
  A19;  \field Controlling Zone or Thermostat Location  
        \note Used only for AirloopHVAC equipment on a main branch  
        \type object-list  
        \object-list ZoneNames  
        \note Zone name where thermostat is located. Required for control of air loop equipment.  
```

## Outputs Description ##

No new outputs are anticipated.

## Engineering Reference ##

New schematic diagram (similar to above) and corresponding text description.

## Example File and Transition Changes ##

New example file will be included.
No transition required.

## References ##

    Manufacturer  Documentation
    ------------  -------------------------------------------------------------------
    Daikin        https://www.daikinac.com/content/commercial/ventillation-units/dvs-dedicated-outside-air-system
    Mitsubishi    https://www.mitsubishipro.com/products/city-multi-vrf/ventilation/premisys-dedicated-outdoor-air-systems
    Mitsubishi    http://meus1.mylinkdrive.com/item/PremiSys.html
    LG            https://lghvac.com/commercial/product-type/?productTypeId=a2x44000003XR0s&iscommercial=true
    Carrier       https://www.carrier.com/commercial/en/us/products/variable-refrigerant-flow/toshiba-carrier-vrf-products/toshiba-carrier-indoor-units/40qq/



