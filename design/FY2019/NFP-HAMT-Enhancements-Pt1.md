# NFP: HAMT Model Airflow Enhancements Part I #

## Justification for New Feature ##
The current HAMT model does not include air leakage in its calculation of the moisture deposition. Air movement as a moisture transport mechanism is typically far more important than vapor diffusion. Air leaks through walls, roofs, and floors can have the most damaging effect on the durability of a house. Uncontrolled airflow through the shell not only carries moisture into framing cavities, causing mold and rot, but it also can account for a huge portion of a home's energy use and can cause indoor air quality problems.

## Overview ##
The modification adds an internal moisture source in HAMT components due to air flow through the component. The moisture source is attached to a user-selected layer inside the component. Input options to determine the air flow are added, including a user-defined air flow, air flow determined from stack height, air permeance of the component and ventilation overpressure as well as are flow computed with the AirflowNetwork model.

These modifications to EnergyPlus are part of a larger effort to enhance the moisture transport capabilities of EnergyPlus. This NFP describes the first two steps of this process:

Step 1 - Enable air leakage to be applied as a source/sink at any arbitrary point inside the simulation model. Manual inputs of air flow rates is required.

Step 2 - Allow air leakage rates to be controlled by air pressure gradients induced by wind forces, temperature differences and ventilation system.

The necessary inputs will be developed to allow both user specified airflows and airflows calculated using the AirflowNetwork feature. The HAMT model calculations will be modified to account for this additional mode of transport.

## Approach ##
The HAMT model subdivides a wall into set of cells and computes the movement of moisture through the wall. Equation (3.36) in the Engineering Reference describes the calculation that is done. A source term that adds moisture to a specific cell is added to this equation of the form

```
S_i A_ij
```

where

```
S_i = m_dot(c_src - c_i_sat)
```

and

`m_dot` - air flow through the envelope per unit area (m^3/(m^2 h))

`c_src` - water vapor concentration in the source volume (kg/m^3)

`c_i_sat` - water vapor saturation concentration for cell `i` (kg/m^3)

## Engineering Reference ##
The HAMT section of the Engineering Reference will be modified to include and describe the new source term.

## I/O Reference ##
The I/O Reference will be modified to describe the additional moisture source inputs. A moisture source object is attached to a particular layer in a manner similar to the way that heat sources are specified. Two options are being considered.

### Modify Existing Object ###

The first option modifies the existing internal sources object to include the needed moisture-related parameters. This is the preferred approach as it makes fewer changes to the IDD.

```
Construction:InternalSources,
       \memo Start with outside layer and work your way to the inside Layer
       \memo Up to 10 layers total, 8 for windows
       \memo Internal heat and moisture sources are specified per layer
       \memo Enter the material name for each layer
  A1,  \field Name
       \required-field
       \type alpha
       \reference ConstructionNames
  N1,  \field Thermal Source Present After Layer Number
       \required-field
       \type integer
       \minimum 1
       \note refers to the list of materials which follows
  N2,  \field Temperature Calculation Requested After Layer Number
       \required-field
       \type integer
       \note refers to the list of materials which follows
  N3,  \field Dimensions for the CTF Calculation
       \required-field
       \type integer
       \minimum 1
       \maximum 2
       \note 1 = 1-dimensional calculation, 2 = 2-dimensional calculation
  N4,  \field Tube Spacing
       \required-field
       \type real
       \units m
       \note uniform spacing between tubes or resistance wires in direction
       \note perpendicular to main intended direction of heat transfer
  N5,  \field Moisture Source Present In Layer Number
       \required-field
       \type integer
       \minimum 1
       \maximum 10
       \note Layer number to which the moisture source should be applied (from outside to inside)
       \note refers to the list of layers specified for the construction applied to the above surface
       \note If a source occurs on a layer surface, the layer should be split into two layers (thin where source occurs) and the source added to the thin layer
  A2,  \field Source Type
       \required-field
       \type choice
       \key UserDefined
       \key StackAndOverPressure
       \key AirflowNetwork
       \minimum 0
       \maximum 3
       \note Type of Moisture Source Calculation
       \note UserDefined: User defined input of air flow through component
       \note StackAndOverPressure: Calculation of dynamic air flow through component due to stack effect and ventilation overpressure (according to Kuenzel, Zirkelbach and Schafazcek 2012)
       \note AirflowNetwork: Calculation of dynamic air flow through component with AirflowNetwork model
  N6,  \field Air Flow Rate
       \units m/s
       \type real
       \minimum -0.001
       \maximum 0.001
       \note Air flux density in m3 per m2 and second.
       \note Only required for Source Type UserDefined.
  N7,  \field Stack height
       \units m
       \type real
       \minimum 0
       \maximum 100
       \note Height of the connected airspace in the building envelope element
       \note Only required for Source Type StackAndOverPressure.
  N8,  \field Component Air Permeance
       \units m/s
       \type real
       \minimum 0
       \maximum 1
       \note Moisture specific air permeance of the component in m3 per m2, hour and Pascal.
       \note Only required for Source Type StackAndOverPressure.
  N9,  \field Mechanical Ventilation Overpressure
       \units Pa
       \type real
       \minimum 0
       \maximum 100
       \note Constant mechanical ventilation overpressure in Pascal.
       \note Only required for Source Type StackAndOverPressure.
  A3,  \field Outside Layer
       \required-field
       \type object-list
       \object-list MaterialName
  A4,  \field Layer 2
       \type object-list
       \object-list MaterialName
  A5,  \field Layer 3
       \type object-list
       \object-list MaterialName
  A6,  \field Layer 4
       \type object-list
       \object-list MaterialName
  A7,  \field Layer 5
       \type object-list
       \object-list MaterialName
  A8,  \field Layer 6
       \type object-list
       \object-list MaterialName
  A9,  \field Layer 7
       \type object-list
       \object-list MaterialName
  A10, \field Layer 8
       \type object-list
       \object-list MaterialName
  A11, \field Layer 9
       \type object-list
       \object-list MaterialName
  A12; \field Layer 10
       \type object-list
       \object-list MaterialName
```

### Modify Existing Object, Add Two New Objects ###

This option modifies the existing sources object to move the source information (both thermal and moisture) out into separate objects.

```
Construction:InternalHeatSource,
       \memo Internal heat source to be attached to a construction layer
       \memo Enter the name for the heat source
  A1,  \field Name
       \required-field
       \type alpha
       \reference InternalHeatSourceNames
  N1,  \field Dimensions for the CTF Calculation
       \required-field
       \type integer
       \minimum 1
       \maximum 2
       \note 1 = 1-dimensional calculation, 2 = 2-dimensional calculation
  N2;  \field Tube Spacing
       \required-field
       \type real
       \units m
       \note uniform spacing between tubes or resistance wires in direction
       \note perpendicular to main intended direction of heat transfer
       
Construction:InternalMoistureSource,
      \memo HeatBalanceAlgorithm = CombinedHeatAndMoistureFiniteElement solution algorithm only.
      \memo Adds moisture source to selected layer inside the component.
      \memo Has no effect with other HeatBalanceAlgorithm solution algorithms
  A1, \field Name
      \required-field
      \type alpha
      \reference InternalMoistureSourceNames
  A2, \field Source Type
      \required-field
      \type choice
      \key UserDefined
      \key StackAndOverPressure
      \key AirflowNetwork
      \note Type of Moisture Source Calculation
      \note UserDefined: User defined input of air flow through component
      \note StackAndOverPressure: Calculation of dynamic air flow through component due to stack effect and ventilation overpressure (according to Kuenzel, Zirkelbach and Schafazcek 2012)
      \note AirflowNetwork: Calculation of dynamic air flow through component with AirflowNetwork model
  N1, \field Air Flow Rate
      \units m/s
      \type real
      \minimum -0.001
      \maximum 0.001
      \note Air flux density in m3 per m2 and second.
      \note Only required for Source Type UserDefined.
  N2, \field Stack height
      \units m
      \type real
      \minimum 0
      \maximum 100
      \note Height of the connected airspace in the building envelope element
      \note Only required for Source Type StackAndOverPressure.
  N3, \field Component Air Permeance
      \units m/s
      \type real
      \minimum 0
      \maximum 1
      \note Moisture specific air permeance of the component in m3 per m2, hour and Pascal.
      \note Only required for Source Type StackAndOverPressure.
  N4; \field Mechanical Ventilation Overpressure
      \units Pa
      \type real
      \minimum 0
      \maximum 100
      \note Constant mechanical ventilation overpressure in Pascal.
      \note Only required for Source Type StackAndOverPressure.

Construction:InternalSources,
       \memo Start with outside layer and work your way to the inside Layer
       \memo Up to 10 layers total, 8 for windows
       \memo Internal heat and moisture sources are specified per layer
       \memo Enter the material name for each layer
  A1,  \field Name
       \required-field
       \type alpha
       \reference ConstructionNames
  A2,  \field Internal Heat Source Name
       \type object-list
       \object-list InternalHeatSourceNames
  N1,  \field Thermal Source Present After Layer Number
       \required-field
       \type integer
       \minimum 1
       \note refers to the list of materials which follows
  N2,  \field Temperature Calculation Requested After Layer Number
       \required-field
       \type integer
       \note refers to the list of materials which follows
  A3,  \field Internal Moisture Source Name
       \type object-list
       \object-list InternalMoistureSourceNames
  N3,  \field Moisture Source Present In Layer Number
       \required-field
       \type integer
       \minimum 1
       \maximum 10
       \note Layer number to which the moisture source should be applied (from outside to inside)
       \note refers to the list of layers specified for the construction applied to the above surface
       \note If a source occurs on a layer surface, the layer should be split into two layers (thin where source occurs) and the source added to the thin layer
  A4,  \field Outside Layer
       \required-field
       \type object-list
       \object-list MaterialName
  A5,  \field Layer 2
       \type object-list
       \object-list MaterialName
  A6,  \field Layer 3
       \type object-list
       \object-list MaterialName
  A7,  \field Layer 4
       \type object-list
       \object-list MaterialName
  A8,  \field Layer 5
       \type object-list
       \object-list MaterialName
  A9,  \field Layer 6
       \type object-list
       \object-list MaterialName
  A10, \field Layer 7
       \type object-list
       \object-list MaterialName
  A11, \field Layer 8
       \type object-list
       \object-list MaterialName
  A12, \field Layer 9
       \type object-list
       \object-list MaterialName
  A13; \field Layer 10
       \type object-list
       \object-list MaterialName
```

## Output Details ##
No additional outputs will be added during this phase of the work.

## Example File and Transition Changes ##
An example file will be created or modified to demonstrate the new feature.

## Discussion and Comments
TBD

## References ##
NA
