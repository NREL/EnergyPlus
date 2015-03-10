# Group -- Humidifiers

## Humidifier:Steam:Electric

The electric steam humidifier is a component that represents an electrically heated, self contained steam humidifier. The component uses electrical energy to convert ordinary tap water to steam which it then injects into the supply air stream by means of a blower fan. The actual unit might be an electrode-type humidifier or a resistance-type humidifier.

The humidifier model includes local control of the humidifier unit to meet a humidity ratio setpoint on its air outlet node. A set point manager is needed to put a setpoint on the exit node but no other local controllers are needed. The humidifier will add moisture to meet the humidity ratio setpoint.

### Inputs

#### Field: Name

A unique user assigned name for a particular humidifier unit. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the unit can run during a given time period. A schedule value of 0 indicates that the unit is off for that time period. A schedule value greater than 0 indicates that the unit can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Rated Capacity

The nominal full output water addition rate of the unit in m^3^/sec of water at 5.05 C. This field is autosizable.

#### Field: Rated Power

The nominal full output power consumption of the unit in watts, exclusive of the blower fan power consumption and any standby power. This field can be autosized. When it is autosized, its calculated from the rated capacity in kg/s and the enthalpy rise in J/kg of the feed water from the a reference temperature of liquid water at 20°C to a saturated steam at 100°C.

#### Field: Rated Fan Power

The nominal full output power consumption of the blower fan in watts.

#### Field: Standby Power

The standby power consumption in watts. This amount of power will be consumed whenever the unit is available (as defined by the availability schedule).

#### Field: Air Inlet Node Name

The name of the HVAC system node from which the unit draws inlet air.

#### Field: Air Outlet Node Name

The name of the HVAC system node to which the unit sends its outlet air.

#### Field: Water Storage Tank Name

This field is optional. If left blank or omitted, then the humidifier obtains its water directly from the mains water. If the name of a Water Storage Tank is specified, then the humidifier will try to obtain its water from that tank. If the tank can't provide all the water then the rest will be drawn from the mains and the humidifier will still operate.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Humidifier:Steam:Electric,
          Humidifier 1,          !- Name
          FanAndCoilAvailSched,  !- Availability Schedule Name
          0.00000379,            !- Rated Capacity {m3/s}
          10200.,                !- Rated Power {W}
          27.,                   !- Rated Fan Power {W}
          2.,                    !- Standby Power {W}
          Cooling Coil Air Outlet Node, !- Air Inlet Node Name
          Air Loop Outlet Node;  !- Air Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Humidifier Water Volume Flow Rate [m3/s]HVAC,Sum,Humidifier Water Volume[m3]
    HVAC,Average,Humidifier Electric Power[W]
    HVAC,Sum,Humidifier Electric Energy [J]
    Zone,Meter,Humidifier:Water [m3]Zone,Meter,Humidifier:Electricity [J]
    HVAC,Average,Humidifier Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Humidifier Storage Tank Water Volume [m3]
    HVAC,Average,Humidifier Starved Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Humidifier Starved Storage Tank Water Volume [m3]
    Zone,Meter,Humidifier:MainsWater [m3]
    HVAC,Sum,Humidifier Mains Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Humidifier Water Volume Flow Rate [m3/s]

This field reports the water consumption rate of the steam humidifier in cubic meters of water per second.

#### Humidifier Water Volume[m3]

This ouput is the cubic meters of water consumed by the steam humidifier over the timestep being reported.

#### Humidifier Electric Power[W]

This output is the electricity consumption rate in Watts of the steam humidifier.

#### Humidifier Electric Energy [J]

This is the electricity consumption in Joules of the steam humidifier over the timestep being reported.

#### Humidifier:Water [m3]

This meter output contains the sum of the water consumed (in cubic neters of water during the report timestep) by all the steam humidifiers at the HVAC level in the simulation.

#### Humidifier:Electricity [J]

This meter output contains the sum of the electricity consumed (in Joules during the report timestep) by all the steam humidifiers at the HVAC level in the simulation.

#### Humidifier Storage Tank Water Volume Flow Rate [m3/s]

#### Humidifier Storage Tank Water Volume [m3]

These outputs contain the rate and volume of water obtained from water storage tank. These are only present if the humidifier is connected to a Water Storage Tank for its water supply.

#### Humidifier Starved Storage Tank Water Volume Flow Rate [m3/s]

#### Humidifier Starved Storage Tank Water Volume [m3]

These outputs contain the rate and volume of water that could not be obtained from the water storage tank. The component will still operate as if it did get all the water with the balance obtained directly from the mains

#### Humidifier Mains Water Volume [m3]

This output contains the volume of water obtained from the mains.