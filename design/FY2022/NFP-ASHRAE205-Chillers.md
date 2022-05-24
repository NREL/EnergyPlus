# ASHRAE-205 Chillers

**Tanaya Mankad, Neal Kruis; Big Ladder Software**

## Overview

ASHRAE Standard 205 introduces a comprehensive method of equipment specification, which includes performance calculation parameters that depend on measured data instead of theoretical curves. 
As a first step to introducing the full set of equipment specified by 205, we will integrate the representation of liquid-cooled chillers as a standalone modeling object.

## Implementation

EnergyPlus will implement a new Chiller object deriving from BaseChillerSpecs, which will reflect the 205 specification for liquid-cooled chillers. 

![ASHRAE205LiquidCooledChillerSpecs](NFP-ASHRAE205-Chillers.png)

Standard 205 representations are stored in either human-readable JSON or serialized CBOR format. The new libtk205 (Toolkit 205) library supports all operations relating to opening and parsing the representation file, and will link with EnergyPlus to populate the new EP Chiller object.

Critically, integrating the Toolkit 205 library will introduce *Git submodules* into the EnergyPlus source hierarchy. A typical implementation would simply add the entire libtk205 repository as a subdirectory of `/third_party`, where it would be built and linked just as existing third-party libraries are. The main CMakeLists.txt file will be modified to automatically pull this submodule repository whenever EnergyPlus is cloned, so the dependency stays in sync with EnergyPlus with no additional user input.

### Error conditions / Validation

### Calculations

Performance calculation in Sim function

Scaling outputs from files for other-sized chillers

## Testing



## IDD Changes and Transition

Add new object `Chillers:ASHRAE205`:

<!-- Do we need reference temperatures?
  N4 , \field Reference Leaving Chilled Water Temperature
       \type real
       \default 6.67
       \units C
  N5 , \field Reference Entering Condenser Fluid Temperature
       \type real
       \default 29.4
       \units C
  N6 , \field Reference Chilled Water Flow Rate
       \type real
       \units m3/s
       \minimum> 0
       \autosizable
       \ip-units gal/min
  N7 , \field Reference Condenser Fluid Flow Rate
       \type real
       \units m3/s
       \autosizable
       \minimum 0.0
       \ip-units gal/min
       \note This field is only used for Condenser Type = AirCooled or EvaporativelyCooled
       \note when Heat Recovery is specified
-->
<!-- Do we need to be messing around with this if it's in the file? (NO)
  N3 , \field Reference COP
       \note Efficiency of the chiller compressor (cooling output/compressor energy input).
       \note Condenser fan power should not be included here.
       \type real
       \units W/W
       \required-field
       \minimum> 0.0
-->
```
Chiller:ASHRAE205,
   \memo @@@
  A1 , \field Name
       \type alpha
       \reference Chillers
       \required-field
       \reference-class-name validPlantEquipmentTypes
       \reference validPlantEquipmentNames
       \reference-class-name validBranchEquipmentTypes
       \reference validBranchEquipmentNames
  A2 , \field Filename
       \type alpha
       \reference Chillers
       \required-field
  N1 , \field Reference Capacity
       \type real
       \units W
       \required-field
       \minimum> 0.0
       \autosizable
  N2, \field Sizing Factor
       \note Multiplies the autosized capacity and flow rates
       \type real
       \minimum> 0.0
       \default 1.0
  A3 , \field Chilled Water Inlet Node Name
       \type node
       \required-field
  A4 , \field Chilled Water Outlet Node Name
       \type node
       \required-field
  A5 , \field Condenser Inlet Node Name
       \type node
       \note Not required if air-cooled or evaporatively-cooled
  A6 , \field Condenser Outlet Node Name
       \type node
       \note Not required if air-cooled or evaporatively-cooled
  A7, \field Heat Recovery Inlet Node Name
       \type node
       \note For 205 expansion; heat recovery not currently supported
  A8, \field Heat Recovery Outlet Node Name
       \type node
       \note For 205 expansion; heat recovery not currently supported
  A9; \field End-Use Subcategory
       \note Any text may be used here to categorize the end-uses in the ABUPS End Uses by Subcategory table.
       \type alpha
       \retaincase
       \default General
```

## Documentation

Much of the content above will be modified to describe the feature in "Input Output Reference", and details of the aggregation calculations in "Engineering Reference".