# ASHRAE-205 Chillers

**Tanaya Mankad, Neal Kruis; Big Ladder Software**

## Overview

ASHRAE Standard 205P introduces a comprehensive procedure for equipment performance characterization, which relies on calculation parameters that are given in an equipment representation specification. The standard is in its fourth public review at the time of this writing; an overview can be found at http://data.ashrae.org/standard205/. 
As a first step to introducing the full set of equipment specified by 205, we will integrate the representation of chillers as a standalone modeling object. Although the current specification for chillers, known as RS0001, is limited to liquid-cooled chillers, it will evolve to contain other capabilities.

## Implementation

EnergyPlus will implement a new Chiller object deriving from BaseChillerSpecs, which will reflect the 205 specification for liquid-cooled chillers. 

![ASHRAE205ChillerSpecs](NFP-ASHRAE205-Chillers.png)

Standard 205 representations are stored in either human-readable JSON or serialized CBOR (Concise Binary Object Representation) format. The new libtk205 (Toolkit 205) library, part of the open-source project [open205](https://github.com/open205), supports all operations relating to opening and parsing the representation file, and will link with EnergyPlus to populate the new EnergyPlus Chiller object. 

### Calculations

One of the advantages of ASHRAE Standard 205 is that available equipment operating conditions must be provided explicitly in a representation. These data are straightforward to arrange in a tabular format, which the libtk205 library passes to the same multi-dimensional interpolation engine ([btwxt](https://github.com/bigladder/btwxt)) which is already used in EnergyPlus to calculate performance at different operating conditions. Rather than being extracted from a regression curve, which can have issues with physicality and boundary conditions, equipment performance is interpolated within the closest available data to the operating point. The performance calculation will be embedded in the `simulate()` function of our `ASHRAE205ChillerSpecs` class. 

One complication from relying on a 205 chiller representation arises when design conditions require a chiller capacity other than what is specified in the representation file. We expect that the existing autosizing algorithm in EnergyPlus will apply to variables given in a representation, but that autosizing will need to be added to the interpolated performance outputs. The sizing calculations will also include a *Sizing Factor* as enumerated in the [IDD](#markdown-header-idd-changes-and-transition) section below.

## Testing

In order to test the new `ASHRAE205ChillerSpecs` object, we will create a new model based on an existing one, e.g. `ReformulatedEIRChillerSpecs`. A new ASHRAE 205 representation file will be created using performance characteristics from the known model, using its regression curve to back-calculate the tabular parameter and lookup information required by the new representation.

## IDD Changes and Transition

We will add a new object `Chillers:ASHRAE205`. As ASHRAE 205 eliminates the need to specify regression coefficients, the IDD entry primarily describes the equipment topology.

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
   \memo This chiller model is based on the ASHRAE Standard 205 representation specification for chillers.
  A1 , \field Name
       \type alpha
       \reference Chillers
       \required-field
       \reference-class-name validPlantEquipmentTypes
       \reference validPlantEquipmentNames
       \reference-class-name validBranchEquipmentTypes
       \reference validBranchEquipmentNames
  A2 , \field Representation File Name
       \note The name of the ASHRAE205 RS0001 (chiller) representation file
       \type alpha
       \retaincase
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
