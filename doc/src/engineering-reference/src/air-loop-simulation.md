# Air Loop Simulation

A complete simulation of each primary air system – zone equipment air loop is done in the following manner.

- If this is the first simulation this system time-step, just call *ManageAirLoops* (simulates the primary air systems) and *ManageZoneEquipment* (simulates the zone equipment sets) once and quit. This initial pass is simulated with full design air flow rates and allows the zone equipment to set the flow rates for each zone that will meet the zone loads.
- Otherwise loop over primary air systems and zone equipment sets until the temperatures, flow rates, enthalpies, humidity ratios etc. agree to within tolerance at each primary air system – zone equipment gap.

~~~~~~~~~~~~~~~~~~~~

    DO WHILE ((SimAirLoops .OR. SimZoneEquipment) .AND. (IterAir.LE.MaxAir) )
      IF (SimAirLoops) THEN
        CALL ManageAirLoops(FirstHVACIteration,SimAirLoops,SimZoneEquipment)
        SimPlantDemandLoops = .TRUE
        SimElecCircuits =.TRUE.
      END IF

      IF (SimZoneEquipment) THEN
        CALL ResolveAirLoopFlowLimits(IterAir+1)
        CALL ManageZoneEquipment(FirstHVACIteration,SimZoneEquipment,SimAirLoops)
        SimPlantDemandLoops = .TRUE.
        SimElecCircuits =.TRUE.
      END IF

      IterAir = IterAir + 1

    END DO

    CALL ResolveLockoutFlags(SimAirLoops)
~~~~~~~~~~~~~~~~~~~~

The logical flags *SimAirLoops* and *SimZoneEquipment* are used to signal whether the primary air systems or the zone equipment sets need to be resimulated. These flags are set by the subroutine *UpdateHVACInterface* which is called  from within *ManageAirLoops* and *ManageZoneEquipment* at the end of each half-loop simulation. *UpdateHVACInterface* (when called from *ManageAirLoops*) **passes the values at the outlet nodes of a primary air system on to the inlet nodes of the corresponding zone equipment half-loop and similarly (when called from *ManageZoneEquipment*) **passes the values of the outlet nodes of a zone equipment half-loop on to the inlet nodes of its corresponding  primary air system. Each time *UpdateHVACInterface* is called it also checks whether the values at the half-loop outlet nodes are in agreement with the values at the downstream half-loop inlet nodes. If they are not it sets the simulate flag of the downstream half-loop to *true*. The values checked by *UpdateHVACInterface* and their tolerances are as follows.

**Quantities**|**Tolerances**
---------------------------|----------------------------
specific enthalpy [J/kg}|10.0
mass flow rate [kg/s]|0.01
humidity ratio [kg H~2~O / kg dry air]|0.0001
quality|0.01
air pressure [Pa]|10.0
temperature [C]|0.01
energy [J]|10.0

*ResolveAirLoopFlowLimits* is invoked to deal with zone equipment – primary air system flow mismatches. For instance the zone air terminal units (ATUs) may be asking for more air than the central fan can supply. In this case *ResolveAirLoopFlowLimits* takes the air flow that the fan can supply and apportions it among the ATUs in proportion to their design maximum air flow rates (*ResolveAirLoopFlowLimits* sets the ![](media/image1855.png)  at the entering node of each ATU in the system).

At the end of the air loop simulation *ResolveLockoutFlags* is called. This subroutine checks if any air system component has requested that the economizer be locked out. If such a request has been made and if the economizer is active, *ResolveLockoutFlags* sets *SimAirLoops* to *true* and the *EconoLockout* flag to *true* to ensure that the air loop will be resimulated with the economizer forced off.