# Group -- Non-Zone Equipment

The non-zone equipment group consists of plant objects that connect to the demand side of the plant loop but are not associated with a a specific [Zone](#zone) (and do not have their own group). There is currently one non-zone equipment object called  [LoadProfile:Plant](#loadprofileplant). This object places a scheduled demand load on the plant.

Non-zone equipment allows the option of performing a plant-only EnergyPlus simulation, i.e. without any zones. This can be useful for comparing the performance of various plant configurations or components without the added complexity of a full building description.

## LoadProfile:Plant

The [LoadProfile:Plant](#loadprofileplant) object is used to simulate a scheduled demand profile. This can be useful when the building loads are already known. Demanded load and flow rate are schedules specified in the object definition. The load profile can specify heating and cooling loads. Cooling loads are entered as negative numbers. The actual load met is dependent on the performance of the supply loop components.

The [LoadProfile:Plant](#loadprofileplant) object must be connected on the demand side of the plant loop. It should be located on a [Branch](#branch) with its control type set to "Active." If desired, multiple [LoadProfile:Plant](#loadprofileplant) objects can be combined in series and/or parallel.

### Inputs

#### Field: Name

The unique name of the [LoadProfile:Plant](#loadprofileplant) object.

#### Field: Inlet Node Name

The name of the inlet node connection to the plant loop.

 This node should be on the demand side.

#### Field: Outlet Node Name

The name of the outlet node connection to the plant loop.

#### Field: Load Schedule Name

Reference to the schedule object specifying the load profile [W].

#### Field: Peak Flow Rate

The peak demanded water flow rate [m^3^/s]. This value is multiplied by the flow rate fraction schedule values (below) to determine the actual volumetric flow rate.

#### Field: Flow Rate Fraction Schedule Name

Reference to the schedule object specifying the flow rate fraction relative to the value in the field Peak Flow Rate (above).

An example of this object follows.

~~~~~~~~~~~~~~~~~~~~

    LoadProfile:Plant,
      Load Profile 1,                     !- Name
      Demand Load Profile 1 Inlet Node,   !- Inlet Node Name
      Demand Load Profile 1 Outlet Node,  !- Outlet Node Name
      Load Profile 1 Load Schedule,       !- Load Schedule Name {W}
      0.003,                              !- Peak Flow Rate {m3/s}
      Load Profile 1 Flow Frac Schedule;  !- Flow Rate Fraction Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The following output variables and meters are reported for the [LoadProfile:Plant](#loadprofileplant) object:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Plant Load Profile Mass Flow Rate [kg/s]
    HVAC,Average,Plant Load Profile Heat Transfer Rate [W]
    HVAC,Sum,Plant Load Profile Heat Transfer Energy [J]
    HVAC,Sum,Plant Load Profile Heating Energy [J]
    HVAC,Sum,Plant Load Profile Cooling Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Plant Load Profile Mass Flow Rate [kg/s]

This is the mass flow of the fluid passing through the load profile object, in kg/s.

#### Plant Load Profile Heat Transfer Rate [W]

#### Plant Load Profile Heat Transfer Energy [J]

These report the overall heat transfer rate and energy for the load profile object, in Watts or Joules.

#### Plant Load Profile Cooling Energy [J]Plant Load Profile Heating Energy [J]

These report the overall cooling or heating energy that the load profile object places on the plant loop, in Joules.

The inlet and outlet node temperatures and mass flow rates can be monitored using the system node output variables:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,System Node Temperature [C]
    HVAC,Average,System Node Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~