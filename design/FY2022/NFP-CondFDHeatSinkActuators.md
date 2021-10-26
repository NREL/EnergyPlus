# CondFD Heat Sink Actuators

**Matt Mitchell, NREL**

October 26, 2021 - Initial Draft

## Overview

Last year, internal NREL users requested the addition of EMS actuators to the CondFD surface layers to control material layer conductivity and specific heat values. This project was accomplished and completed. Now, in a continuation of their work, they have requested adding EMS actuators to add electrical heat gain to the material layers. The only addition to EnergyPlus will be the addition of this actuator, which will be a one-directional way for EMS users to specify electrically-driven heat addition to material layers.

## Implementation

The EMS actuators for conductivity and specific heat operate at the material-layer level and apply property changes to all nodes within that material layer. Similarly, this proposed actuator will operate at the material-layer level and will distribute the heat addition uniformly throughout the material layer. All control of these heat inputs will be reliant on the user, who will implement the controls algorithms in EMS or Python.

The CondFD model is an implicit model formulation, so we expect the solution to remain stable with this heat addition, though, this has yet to be fully confirmed.

Users will be limited to only being able to  *add* heat to the material layers. I.e. the material layer will be a heat sink for electrically-driven Joule heating. There will be no provision for the user to source heat from the material layer. This heat addition will be treated as an electrical input to that material layer, a new output variable will be created, and this output variable will be added to the electric usage meter.

## Testing

The "PythonPlugin1ZoneUncontrolledCondFD" test files will be modified to include usage of the new actuator. This file will be used to validate the model to ensure that energy is conserved and that the actuators are working as expected.

A series of unit tests will also be added to improve coverage on the CondFD code.

## Documentation

Documentation will be added in the EMS Reference describing the usage and limits of this actuator.

## IDD Changes and Transition

None expected.
