# Air Loop Simulation Environment Variables

To facilitate comparing the runtime performance of various solution techniques for the air loop simulation a mechanism to track runtime statistics has been implemented in EnergyPlus. To facilitate debugging the air loop simulation a tracing mechanism operating either at the air loop-level or at the controller-level has been implemented in EnergyPlus.  To use, one uses the "Set" command and the value as indicated.

### TRACK_AIRLOOP: Runtime performance tracker for air loop simulation

Setting to "yes" will cause EnergyPlus to track the performance of the air loop simulation at runtime and dump the results in a file named "statistics.HVACControllers.csv" upon ending the simulation.  (Uses logical variable **TrackAirLoopEnvFlag** in module DataSystemVariables).

~~~~~~~~~~~~~~~~~~~~

    Set TRACK_AIRLOOP=yes
~~~~~~~~~~~~~~~~~~~~

### TRACE_AIRLOOP: Air loop simulation tracer

Setting to "yes" will cause EnergyPlus to write to a trace file named "controller.<Air Loop Name>.csv" the converged solutions of all controllers defined on each air loop, at each HVAC iteration.  (Uses logical variable **TraceAirLoopEnvFlag** in module DataSystemVariables).

~~~~~~~~~~~~~~~~~~~~

    Set TRACE_AIRLOOP=yes
~~~~~~~~~~~~~~~~~~~~

### TRACE_HVACCONTROLLER: Individual HVAC controller tracer

Setting to "yes" will cause EnergyPlus to write to a trace file named "controller.<Controller Name>.csv" a detailed description of each controller iteration at each HVAC iteration.  (Uses logical variable **TraceControllerEnvFlag** in module DataSystemVariables).

~~~~~~~~~~~~~~~~~~~~

    Set TRACE_HVACCONTROLLER=yes
~~~~~~~~~~~~~~~~~~~~