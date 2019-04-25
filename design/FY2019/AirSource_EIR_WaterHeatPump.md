# Adding Air Source Capability to the EIR Water-to-Water Heat Pump

The EIR-formulated water-to-water heat pump model was added with EnergyPlus 9.1.0.
This model allows users to evaluate energy calculations on water source plants where a heat pump provides heating and cooling.
The EIR formulation is desired by users as the formulation is accepted as an accurate representation of heat pump operation under common conditions.

## Justification

The water-to-water model is a welcome addition to the EnergyPlus codebase, however an even further desired feature is to collect multiple water-based components and provide them with heating and cooling from an air-source heat pump.
As such, this task will modify the current EIR-formulated heat pump and enable the physics of an air-source condenser.

## Code Changes

It is expected that the code changes will be primarily in the EIR Water to Water Heat Pump Module.
The module will be renamed to a more generalized EIR-Waterside-Heat-Pump or something equivalent.
There will be other stray code changes in plant management routines to instantiate and manage simulation of the newly modified component.

## Transition

The existing EIR Water to Water Heat Pump component will be renamed to a more general component name, such as EIR:WaterSide:Cooling.
I'm definitely open to suggestion on the name, I just want to avoid making yet another component, so the component should capture both air and water source simulation.
A new condenser source type field will need to be added, along with potentially an outdoor air node for the condenser inlet and optional outlet.
The transition tool will need to rename the object and specify a default condenser type (Water-Source).

