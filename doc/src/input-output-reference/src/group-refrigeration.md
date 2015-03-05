# Group – Refrigeration

There are two ways to model a supermarket refrigeration system.  The first, and simplest, approach models the combination of compressor(s) and condenser as a single refrigeration compressor rack object. (A list is used to enter multiple case or walk-in names.)

For the first approach, the input objects needed are:

- One [Refrigeration:CompressorRack](#refrigerationcompressorrack) object
- At least one refrigeration load, which may be any combination of:

- [Refrigeration:Case](#refrigerationcase) or 
- Refrigeration:WalkIn or
- [Refrigeration:CaseAndWalkInList](#refrigerationcaseandwalkinlist) objects .

The second approach requires more input objects but also allows the user to model more complex systems.  This detailed approach must be used whenever loads are transferred from one system to another, such as with secondary loops, cascade condensers, or mechanical subcoolers. Again lists are used to enter multiple load or compressor names.

For the detailed approach, the input objects needed are:

- One [Refrigeration:System](#refrigerationsystem) object
- At least one refrigeration load object which may include any combination of:

- [Refrigeration:Case](#refrigerationcase), 
- Refrigeration:WalkIn, 
- [Refrigeration:CaseAndWalkInList](#refrigerationcaseandwalkinlist) (may include any combination of cases and walk in cooler names, OR, a list of air chiller names)
- [Refrigeration:SecondarySystem](#refrigerationsecondarysystem),  
- [Refrigeration:Condenser:Cascade](#refrigerationcondensercascade) (as a load, cooling a lower-temperature system) objects,
- [Refrigeration:TransferLoadList](#refrigerationtransferloadlist) (may include cascade condenser loads and/or secondary loop loads)
- Refrigeration:AirChiller

- At least one [Refrigeration:Compressor](#refrigerationcompressor) object (multiple compressors are entered using a [Refrigeration:CompressorList](#refrigerationcompressorlist)), 
- One condenser object which may be either:

- [Refrigeration:Condenser:AirCooled](#refrigerationcondenseraircooled),
- [Refrigeration:Condenser:EvaporativeCooled](#refrigerationcondenserevaporativecooled),
- [Refrigeration:Condenser:WaterCooled](#refrigerationcondenserwatercooled), or
- [Refrigeration:Condenser:Cascade](#refrigerationcondensercascade) (rejecting heat from this system)

- The object [Refrigeration:Subcooler](#refrigerationsubcooler) may optionally be included to describe either a liquid suction or mechanical subcooler.

Output variables are also available to describe the total heat exchange between all refrigeration objects and the zones containing these objects. These variables are described at the end of this section after all the refrigeration objects.

## Refrigeration:CompressorRack

The refrigeration compressor rack object works in conjunction with the refrigeration case and walkin objects (Ref. [Refrigeration:Case](#refrigerationcase) and Refrigeration:WalkIn) to simulate the performance of a refrigerated case system. This object models the electric consumption of the rack compressors and the condenser fans. Heat removed from the refrigerated cases, walkins, and compressor/condenser fan heat can be rejected either outdoors or to a zone. Compressor rack waste heat can also be reclaimed for use by an optional air- or water-heating coil (Ref. [Coil:Heating:Desuperheater](#coilheatingdesuperheater) and [Coil:WaterHeating:Desuperheater](#coilwaterheatingdesuperheater)).

If heat is rejected outdoors, condenser cooling can either be accomplished by direct air flow, evaporative water cooling, or a by water-cooled condenser with appropriate plant loop piping. With evaporative cooling, water is sprayed through the air stream to effectively lower the air temperature experienced by the condenser coil as a result of water evaporation. The use of a water-cooled condenser requires the definition of a plant loop to supply cooling to the condenser.  Waste heat can be reclaimed and stored using a water storage device.

The inputs for the compressor rack object include a name, the heat rejection location, the compressor rack coefficient of performance (COP) at design conditions, the design condenser fan power, and the type of condenser cooling. The model requires two curve objects to describe performance at off-design conditions. If the condenser is water-cooled, the inlet and outlet node names as well as the water outlet temperature schedule name are required.  If the condenser has evaporative cooling, additional inputs for evaporative condenser effectiveness, condenser air flow rate, basin heater parameters, water pump power, water source, and an evaporative cooling availability schedule are available. The names of refrigerated cases and walkins connected to the compressor rack are the final inputs to the model.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a refrigeration compressor rack. Any reference to this compressor rack by another object will use this name.

#### Field: Heat Rejection Location

The location of the compressor rack's condenser. The compressor rack condenser heat can be directed "Outdoors" to model an outdoor air or water-cooled condenser or "[Zone](#zone)" to model a condenser located in a zone (e.g., a stand-alone packaged refrigerated case with integral condenser located in a conditioned zone). The default for this field is "Outdoors". If the heat rejection location is "[Zone](#zone)" and no walk-in coolers are served by this compressor rack, then all refrigerated cases connected to this compressor rack must be located in the same zone. If however, walk-in coolers are also served by this compressor rack, then the heat rejection zone name must be specified (see Heat Rejection [Zone](#zone) Name field below) and cases and walk-ins can be located in multiple zones.

> NOTE: When modeling a heat reclaim coil, the heat rejection location must be "Outdoors". If the heat rejection location is "[Zone](#zone)", the total amount of waste heat available for reclaim (e.g., by a desuperheater heating coil) is set to zero by this compressor rack object and the simulation proceeds.

#### Field: Design Compressor Rack COP

The coefficient of performance (COP) for the compressor rack at design conditions (compressors only, excluding condenser fan power). This value must be greater than zero, with a default value of 2.0 if this field is left blank. This value should represent the compressor rack COP corresponding to the lowest evaporating temperature of any case or walkin served by the rack.

#### Field: Compressor Rack COP Function of Temperature Curve Name

The name of the curve object defining the change in compressor rack COP as a function of the temperature of air entering the condenser. The curve object will be evaluated using the zone air dry-bulb temperature when rack heat rejection location equals "[Zone](#zone)" and the outdoor air dry-bulb temperature when rack heat rejection location equals "Outdoors" and the condenser type is air-cooled. As explained below, if the condenser is cooled by evaporative or water loop cooling, the curve object is evaluated using an effective temperature. The output from this curve is multiplied by the design compressor rack COP to yield the actual COP at the specific air temperature entering the condenser. This curve must be cubic or quadratic ([Curve:Cubic](#curvecubic) or [Curve:Quadratic](#curvequadratic)), and should be normalized to 1.0 at the condenser entering air temperature at which the design compressor rack COP occurs. This curve should represent the compressor rack COP corresponding to the lowest evaporating temperature of any case served by the rack.

#### Field: Design Condenser Fan Power

This field defines the design power for the condenser fan(s) in Watts. This field is applicable for air or evaporative cooling only.  If the condenser is water-cooled, the fan power is captured in the cooling object (e.g., cooling tower).  If applicable, this value must be greater than 0.0, with a default value of 250 Watts if this field is left blank.

#### Field: Condenser Fan Power Function of Temperature Curve Name

This field is the name of the curve object defining the change in condenser fan power as a function of the temperature of air entering the condenser. This curve is used to simulate the modulation of air flow by the condenser fans (e.g., staging, multi-speed, or variable speed) as a function of temperature.  The curve object will be evaluated using the zone air dry-bulb temperature when rack heat rejection location equals "[Zone](#zone)" and the outdoor air dry-bulb temperature when rack heat rejection location equals "Outdoors" and the condenser type is air-cooled. As explained below, if the condenser is cooled by evaporative cooling, the curve object is evaluated using an effective temperature. The output from this curve is multiplied by the design condenser fan power to yield the actual fan power at the specific air temperature entering the condenser. This curve must be cubic or quadratic ([Curve:Cubic](#curvecubic) or [Curve:Quadratic](#curvequadratic)), and should be normalized to 1.0 at the condenser entering air temperature at which the design condenser fan power occurs. The actual condenser fan power is not allowed to exceed the design condenser fan power defined in the previous input field or go below zero (i.e., the output of the curve object is limited to values from 0.0 to 1.0). If this field is left blank, then the model assumes the condenser fan power is at the design power level when any of the refrigerated cases connected to this rack are operating.

#### Field: Condenser Type

When the heat rejection location is "Outdoors", the condenser may be either cooled by dry air by specifying "AirCooled", by wet evaporative cooling by specifying "EvaporativelyCooled", or by a water loop by specifying "WaterCooled".  If the condenser type is "WaterCooled", then a plant cooling loop will need to be defined using other EnergyPlus objects, e.g., plant loop, pump, cooling tower, etc.  The default value is "AirCooled".

#### Field: Water-cooled Condenser Inlet Node Name

When the condenser type is "WaterCooled", a node name for the water-side condenser inlet must be provided.

#### Field: Water-cooled Condenser Outlet Node Name

When the condenser type is "WaterCooled", a node name for the water-side condenser outlet must be provided.

#### Field: Water-cooled Loop Flow Type

When the condenser type is "WaterCooled", the type of flow loop should be specified.  The two choices are **VariableFlow**, in which a [Pump:VariableSpeed](#pumpvariablespeed) needs to  be included in the plant loop, or **ConstantFlow**, in which the loop circuit has a constant flow rate, typically associated with a [Pump:ConstantSpeed](#pumpconstantspeed) object.  If the flow type is VariableFlow, the flow needed to remove the condenser heat energy will be calculated and requested of the pump.  If the flow type is Constant Flow, the outlet water temperature will be determined based on the fixed loop flow rate and heat energy to be removed.  The default type is **VariableFlow**. Refer to additional discussion in the Engineering Reference.

#### Field: Water-cooled Condenser Outlet Temperature Schedule Name

When the condenser type is "WaterCooled", and when the water-cooled loop flow type is "Variable Flow", the name of a schedule (Ref: Schedule) that defines the desired condenser water outlet temperature must be provided.  The schedule may define an outlet temperature that varies through time.

#### Field: Water-cooled Condenser Design  Flow Rate

When the condenser type is "WaterCooled", and when the water-cooled loop flow type is "Constant Flow", this is the design water flow rate in m^3^/s that will be requested initially.  This requested flow will be passed to the loop simulation, and resulting actual flow will be dependent upon supply system capabilities (e.g., pump capability).  The design flow rate must always be less than the maximum flow rate, defined below.

#### Field: Water-cooled Condenser Maximum  Flow Rate

When the condenser type is "Water Cooled", this is the maximum water flow rate in m^3^/s that will be allowed through the condenser.  When the loop flow type is Variable Flow, if the calculated flow rate is higher than the maximum flow rate, an error message will be generated, and the flow rate will be reset to the maximum flow rate.

#### Field: Water-cooled Condenser Maximum Water Outlet Temperature

When the condenser type is "WaterCooled", this field specifies the maximum allowed water temperature in degrees C leaving the compressor rack condenser. The default value is 55 degrees C.

#### Field: Water-cooled Condenser Minimum Water Inlet Temperature

When the condenser type is "WaterCooled", this field specifies the minimum allowed water temperature in degrees C entering the compressor rack condenser. The default value is 10 degrees C.  Refer to additional discussion in the Engineering Reference.

#### Field: Evaporative Condenser Availability Schedule Name

When the condenser type is "EvaporativelyCooled", the name of the optional schedule (Ref: Schedule) that specifies the time periods that evaporative cooling is available/unavailable. In some colder climates, evaporative cooling is periodically discontinued and the basin sumps drained to avoid freezing. In these times, the condenser runs as a typical dry air cooled condenser, and related evaporative cooling systems (e.g., water pump, basin heaters) do not operate. Use of this optional schedule permits modeling of planned, seasonal interruptions of evaporative cooling. All schedule values must be greater than or equal to zero. Typically, an ON/OFF schedule type is used with values being either 0 or 1. A schedule value of 1 indicates that evaporative cooling is available during the defined time period, and a value of 0 denotes that evaporative cooling is not available during the defined time period. If the schedule name is omitted (blank) and Condenser Type = "Evap Cooled", then the model assumes that evaporative cooling of the condenser is available for the entire simulation period.

#### Field: Evaporative Condenser Effectiveness

When the condenser type is "EvaporativelyCooled", this field specifies the effectiveness of the evaporative system in modifying the temperature of the air entering the condenser coil. The resulting effective temperature is determined as

![](media/image310.png)\


where:

![](media/image311.png) = effective dry-bulb temperature of air entering the condenser cooling coil (°C)

![](media/image312.png) = outdoor air wet-bulb temperature (°C)

![](media/image313.png) = outdoor air dry-bulb temperature (°C)

![](media/image314.png) = evaporative condenser effectiveness.

The resulting condenser inlet air temperature is used by the Compressor Rack COP as a Function of Temperature Curve and the Condenser Fan Power as a Function of Temperature Curve. The default value for this field is 0.9, although valid entries can range from 0.0 to 1.0.

If the two function-of-temperature curves (i.e., Compressor Rack COP as a Function of Temperature Curve and Condenser Fan Power as a Function of Temperature Curve) are based on wet-bulb temperature rather than dry-bulb temperature, the evaporative condenser effectiveness should be set to 1.0 for consistency.

#### Field: Evaporative Condenser Air Flow Rate

When the condenser type is "EvaporativelyCooled", the air volume flow rate, in m^3^ per second, entering the evaporative condenser. This value is used to calculate the amount of water evaporated when evaporatively cooling the condenser inlet air. The value for this field must be greater than zero. This input field is also autocalculatable, equivalent to 0.000144 m^3^/s per watt of total cooling capacity [850 cfm/ton] where the total cooling capacity is the sum of the rated total cooling capacities for the refrigerated cases connected to this compressor rack (Ref. [Refrigeration:Case](#refrigerationcase)).

#### Field: Basin Heater Capacity

When the condenser type is "EvaporativelyCooled", this field defines the power level of the basin heater, if applicable, used to avoid water freezing in an outdoor evaporative cooler basin. This numeric field contains the capacity of the electric basin heater in Watts per degree Kelvin. This field is used in conjunction with the Basin Heater Set Point Temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the dry-bulb temperature of the condenser coil inlet air. The basin heater only operates when the condenser fan is off (i.e., no compressor heat rejection). The basin heater capacity must be greater than or equal to zero, with a default value of 200 W/K if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

When the condenser type is "EvaporativelyCooled", this numeric field contains the set point temperature (°C) for the basin heater described in the previous field. The basin heater is active when the outdoor air dry-bulb temperature falls below this set point temperature, as long as the condenser fan is off. The default value is 2°C if this field is left blank.

#### Field: Design Evaporative Condenser Water Pump Power

When the condenser type is "EvaporativelyCooled", the rated power of the evaporative condenser water pump in Watts. This value is used to calculate the power required to pump the water used to evaporatively cool the condenser inlet air. The value for this field must be greater than or equal to 0, with a default value of 1000 Watts if this field is left blank. This input field is also autocalculatable, equivalent to 0.004266 W per watt [15 W/ton] of total cooling capacity where the total cooling capacity is the sum of the rated total cooling capacities for the refrigerated cases and walk-ins connected to this compressor rack (Ref. [Refrigeration:Case](#refrigerationcase) and Refrigeration:WalkIn)).

#### Field: Evaporative Water Supply Tank Name

When the condenser type is "EvaporativelyCooled", this field is used to define where the compressor rack obtains water used for evaporative cooling of its condenser. If this field is left blank, the unit will obtain water directly from the mains (Ref. Water Mains Temperatures). If the name of a Water Storage Tank object is used here, then the unit will obtain its water from that tank.

#### Field: Condenser Air Inlet Node Name

This optional alpha field contains the name of the node from which the outdoor compressor rack condenser draws its outdoor air. This field is applicable only when the heat rejection location is "Outdoors" (if the heat rejection location is "[Zone](#zone)", this field is disregarded). If this field is left blank, the outdoor air temperature entering the condenser (dry-bulb or wet-bulb) is taken directly from the weather data. If this field is not blank, this node name must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air temperature from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air temperature is taken directly from the weather data.

#### Field: End-Use Subcategory

This field allows the specification of a user-defined end-use subcategory (e.g., "Low Temperature Rack"). A new meter for reporting is created for each unique subcategory (Ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is left blank, the equipment will be assigned to the "General" end-use subcategory.

#### Field: Refrigeration Case Name or WalkIn Name or CaseAndWalkInList Name

This alpha field is the name of a single refrigerated case, a single walk in cooler,  the name of a CaseAndWalkInList for multiple refrigerated cases and/or walk-ins, the name of a refrigeration chiller, or the name of a CaseAndWalkInList for multiple refrigeration chillers connected to this compressor rack. Note that if a CaseAndWalkInList contains the name(s) of refrigeration chillers, it CANNOT have the name(s) or any refrigerated cases or walkins . This field is required.

#### Field: Heat Rejection Zone Name

If the Heat Rejection Location is "[Zone](#zone)" and any walk-in coolers are connected to this compressor rack, then this field is required. The compressor rack heat rejection will impact the air heat balance in this zone.

The following is an example input for a  Refrigeration Compressor Rack with water cooled condenser.

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:CompressorRack,
        MediumTempRack,          !- Name
        Outdoors,                !- Heat Rejection Location
        1.7,                     !- Design Compressor Rack COP {W/W}
        RackCOPfTCurve,          !- Compressor Rack COP as a Function of Temperature Curve Name
        1025.0,                  !- Design Condenser Fan Power {W}
        RackCondFanCurve,        !- Condenser Fan Power as a Function of Temperature Curve Name
        WaterCooled,             !- Condenser Type
        Condenser Inlet Node,    !- Water-cooled Condenser Inlet Node Name
        Condenser Outlet Node,   !- Water-cooled Condenser Outlet Node Name
        VariableFlow,            !- Water-cooled Loop Flow Type
        Cond Outlet Temp Sch,    !- Water-cooled Condenser Outlet Temperature Schedule Name
        ,                        !- Water-cooled Condenser Design Flow Rate {m3/s}
        0.003,                   !- Water-cooled Condenser Maximum Flow Rate {m3/s}
        55,                      !- Water-cooled Condenser Maximum Water Outlet Temperature {C}
        ,                        !- Water-cooled Condenser Minimum Water Inlet Temperature {C}
        ,                        !- Evaporative Condenser Availability Schedule Name
        ,                        !- Evaporative Condenser Effectiveness {dimensionless}
        ,                        !- Evaporative Condenser Air Volume Flow Rate {m3/s}
        ,                        !- Basin Heater Capacity {W/K}
        ,                        !- Basin Heater Set Point Temperature {C}
        ,                        !- Design Evaporative Condenser Water Pump Power {W}
        ,                        !- Evaporative Water Supply Tank Name
        ,                        !- Condenser Air Inlet Node Name
        ,                        !- End-Use Subcategory
        MediumTempCaseList;      !- Refrigeration Case Name or WalkIn Name or CaseAndWalkInList Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    If Condenser Type = Water Cooled:
    System,Average, Refrigeration Compressor Rack Condenser Refrigerant Mass Flow Rate [kg/s]
    System,Average, Refrigeration Compressor Rack Condenser Heat Transfer Rate [W]
    System,Sum, Refrigeration Compressor Rack Condenser Heat Transfer Energy [J]

    IF THE RACK SERVES CASES AND/OR WALKINS:
    Zone,Average, Refrigeration Compressor Rack Electric Power [W]
    Zone,Sum, Refrigeration Compressor Rack Electric Energy [J]
    Zone,Average, Refrigeration Compressor Rack Condenser Fan Electric Power [W]
    Zone,Sum, Refrigeration Compressor Rack Condenser Fan Electric Energy [J]
    Zone,Average, Refrigeration Compressor Rack Total Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Compressor Rack Total Heat Transfer Energy [J]
    Zone,Average, Refrigeration Compressor Rack COP [W/W]
    If Condenser Type = Evap Cooled:
    Zone,Average, Refrigeration Compressor Rack Evaporative Condenser Pump Electric Power [W]
    Zone,Sum, Refrigeration Compressor Rack Evaporative Condenser Pump Electric Energy [J]
    Zone,Average, Refrigeration Compressor Rack Evaporative Condenser Basin Heater Electric Power [W]
    Zone,Sum, Refrigeration Compressor Rack Evaporative Condenser Basin Heater Electric Energy [J]
    Zone,Average, Refrigeration Compressor Rack Evaporative Condenser Water Volume Flow Rate [m^3^/s]
    Zone,Sum, Refrigeration Compressor Rack Evaporative Condenser Water Volume [m^3^]

    If Heat Rejection Location = Zone:
    Zone,Average, Refrigeration Compressor Rack Zone Sensible Heating Rate [W]
    Zone,Sum, Refrigeration Compressor Rack Zone Sensible Heating Energy [J]
    Zone,Average, Refrigeration Compressor Rack Return Air Sensible Heating Rate [W]
    Zone,Sum, Refrigeration Compressor Rack Return Air Sensible Heating Energy [J]

    IF THE RACK SERVES REFRIGERATION CHILLERS
    HVAC,Average, Refrigeration Air Chiller Compressor Rack Electric Power [W]
    HVAC,Sum, Refrigeration Air Chiller Compressor Rack Electric Energy [J]
    HVAC,Average,Refrigeration Air Chiller System Compressor Runtime Fraction []
    HVAC,Average, Refrigeration Air Chiller Compressor Rack Condenser Fan Electric Power [W]
    HVAC,Sum, Refrigeration Air Chiller Compressor Rack Condenser Fan Electric Energy [J]
    HVAC,Average, Refrigeration Air Chiller Compressor Rack Total Heat Transfer Rate [W]
    HVAC,Sum, Refrigeration Air Chiller Compressor Rack Total Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller Compressor Rack COP [W/W]

    If Condenser Type = Evap Cooled:
    HVAC,Average, Refrigeration Air Chiller Compressor Rack Evaporative Condenser Pump Electric Power [W]
    HVAC,Sum, Refrigeration Air Chiller Compressor Rack Evaporative Condenser Pump Electric Energy [J]
    HVAC,Average, Refrigeration Air Chiller Compressor Rack Evaporative Condenser Basin Heater Electric Power [W]
    HVAC,Sum, Refrigeration Air Chiller Compressor Rack Electric PowerRefrigeration Air Chiller Compressor Rack Evaporative Condenser Basin Heater Electric Energy [J]
    HVAC,Average, Refrigeration Air Chiller Compressor Rack Evaporative Condenser Water Volume Flow Rate [m3/s]
    HVAC,Sum, Refrigeration Air Chiller Compressor Rack Evaporative Condenser Water Volume [m3]
    If Heat Rejection Location = Zone:
    HVAC,Average, Refrigeration Air Chiller Compressor Rack Zone Sensible Heating Rate [W]
    HVAC,Sum, Refrigeration Air Chiller Compressor Rack Zone Sensible Heating Energy [J]
    HVAC,Average, Refrigeration Air Chiller Compressor Rack Return Air Sensible Heating Rate [W]
    HVAC,Sum, Refrigeration Air Chiller Compressor Rack Return Air Sensible Heating Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Refrigeration Compressor Rack Condenser Mass Flow Rate [kg/s]

When condenser type is Water Cooled, this is the mass flow rate of water flowing through the water cooled condenser.

#### Refrigeration Compressor Rack Condenser Heat Transfer Rate [W]

When condenser type is Water Cooled, this is the total heat transfer across the condenser (i.e., compressor energy and case cooling).

#### Refrigeration Compressor Rack Condenser Heat Transfer Energy [J]

When condenser type is Water Cooled, this is the total heat energy flowing across the condenser for the timestep being reported.

#### IF THE RACK SERVES CASES AND OR WALKINS:

#### Refrigeration Compressor Rack Electric Power [W]

This output is the electric power input to the rack compressor(s) in Watts.

#### Refrigeration Compressor Rack Electric Energy [J]

This is the electricity consumption of the rack compressor(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Compressor Runtime Fraction []

This is the runtime operating ratio for the compressor.

#### Refrigeration Compressor Rack Condenser Fan Electric Power [W]

This output is the electric input to the rack condenser fan(s) in Watts.

#### Refrigeration Compressor Rack Condenser Fan Electric Energy [J]

This is the electricity consumption of the rack condenser fan(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Compressor Rack Total Heat Transfer Rate [W]

This output is the total heat transfer rate of the  refrigeration compressor rack in Watts. It is the sum of all of the heat transfer rates for the refrigerated cases and walk-ins that are connected to this rack. This value does not include compressor or condenser fan heat.

#### Refrigeration Compressor Rack Total Heat Transfer Energy [J]

This is the total heat transfer of the  refrigeration compressor rack in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Compressor Rack COP [W/W]

This field is the  refrigeration compressor rack coefficient of performance. It is the actual compressor rack COP for the timestep being reported, accounting for the COP variation as a function of temperature. It is calculated as the Design Compressor Rack COP times the Compressor Rack COP as a Function of Temperature Curve evaluated at the effective condenser entering air temperature for the timestep being reported.

#### Refrigeration Compressor Rack Evaporative Condenser Pump Electric Power [W]

This is the electrical power requirement in Watts for the timestep being reported for the water pump used with evaporative cooling of the compressor rack condenser.

#### Refrigeration Compressor Rack Evaporative Condenser Pump Electric Energy [J]

This is the electricity consumption in Joules of the water pump used with evaporative cooling of the compressor rack condenser for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Compressor Rack Evaporative Condenser Basin Heater Electric Power [W]

This is the electrical power requirement in Watts for the timestep being reported for the water heater in the basin of the evaporative system used to cool the compressor rack condenser.

#### Refrigeration Compressor Rack Evaporative Condenser Basin Heater Electric Energy [J]

This is the electricity consumption in Joules of the water heater used to prevent freezing of the evaporative cooling system for the compressor rack condenser for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Compressor Rack Evaporative Condenser Water Volume Flow Rate [m3/s]

The volumetric flow rate in m^3^/s of water consumed while providing evaporative cooling of the compressor rack condenser.

#### Refrigeration Compressor Rack Evaporative Condenser Water Volume [m3]

This is the water consumed by evaporation in m^3^ while providing evaporative cooling of the compressor rack condenser for the timestep being reported. This output is also added to a meter with Resource Type = Water, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Compressor Rack Zone Sensible Heating Rate [W]

This field is the rate of sensible heating in Watts provided to the zone by condenser waste heat rejection, which impacts the zone air heat balance.

#### Refrigeration Compressor Rack Zone Sensible Heating Energy [J]

This field is the sensible heating energy in Joules provided to the zone by condenser waste heat rejection for the timestep being reported.

#### Refrigeration Compressor Rack Return Air Sensible Heating Rate [W]

This field is the rate of sensible heating in Watts provided by condenser waste heat rejection to the HVAC return air (zone return air path outlet node), which impacts the HVAC return air temperature. If the HVAC system is off for a simulation timestep (no return air mass flow), then this sensible heating is actually provided to the zone air instead (even though a non-zero value is reported here).

#### Refrigeration Compressor Rack Return Air Sensible Heating Energy [J]

This field is the sensible heating energy in Joules provided by condenser waste heat rejection to the HVAC return air (zone return air path outlet node) for the timestep being reported. If the HVAC system is off for a simulation timestep (no return air mass flow), then this sensible heating is actually provided to the zone air instead (even though a non-zero value is reported here).

#### IF THE RACK SERVES CHILLERS

#### Refrigeration Air Chiller Compressor Rack Electric Power [W]

This output is the electric power input to the rack compressor(s) in Watts.

#### Refrigeration Air Chiller Compressor Rack Electric Energy [J]

This is the electricity consumption of the rack compressor(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller System Compressor Runtime Fraction []

This is the runtime fraction for the compressor serving air chillers.

#### Refrigeration Air Chiller Compressor Rack Condenser Fan Electric Power [W]

This output is the electric input to the rack condenser fan(s) in Watts.

#### Refrigeration Air Chiller Compressor Rack Condenser Fan Electric Energy [J]

This is the electricity consumption of the rack condenser fan(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller Compressor Rack Total Heat Transfer Rate [W]

This output is the total heat transfer rate of the  refrigeration compressor rack in Watts. It is the sum of all of the heat transfer rates for the refrigerated cases and walk-ins that are connected to this rack. This value does not include compressor or condenser fan heat.

#### Refrigeration Air Chiller Compressor Rack Total Heat Transfer Energy [J]

This is the total heat transfer of the  refrigeration compressor rack in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller Compressor Rack COP [W/W]

This field is the  refrigeration compressor rack coefficient of performance. It is the actual compressor rack COP for the timestep being reported, accounting for the COP variation as a function of temperature. It is calculated as the Design Compressor Rack COP times the Compressor Rack COP as a Function of Temperature Curve evaluated at the effective condenser entering air temperature for the timestep being reported.

#### Refrigeration Air Chiller Compressor Rack Evaporative Condenser Pump Electric Power [W]

This is the electrical power requirement in Watts for the timestep being reported for the water pump used with evaporative cooling of the compressor rack condenser.

#### Refrigeration Air Chiller Compressor Rack Evaporative Condenser Pump Electric Energy [J]

This is the electricity consumption in Joules of the water pump used with evaporative cooling of the compressor rack condenser for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller Compressor Rack Evaporative Condenser Basin Heater Electric Power [W]

This is the electrical power requirement in Watts for the timestep being reported for the water heater in the basin of the evaporative system used to cool the compressor rack condenser.

#### Refrigeration Air Chiller Compressor Rack Evaporative Condenser Basin Heater Electric Energy [J]

This is the electricity consumption in Joules of the water heater used to prevent freezing of the evaporative cooling system for the compressor rack condenser for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller Compressor Rack Evaporative Condenser Water Volume Flow Rate [m^3^/s]

The volumetric flow rate in m^3^/s of water consumed while providing evaporative cooling of the compressor rack condenser.

#### Refrigeration Air Chiller Compressor Rack Evaporative Condenser Water Volume [m^3^]

This is the water consumed by evaporation in m^3^ while providing evaporative cooling of the compressor rack condenser for the timestep being reported. This output is also added to a meter with Resource Type = Water, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller Compressor Rack Zone Sensible Heating Rate [W]

This field is the rate of sensible heating in Watts provided to the zone by condenser waste heat rejection, which impacts the zone air heat balance.

#### Refrigeration Air Chiller Compressor Rack Zone Sensible Heating Energy [J]

This field is the sensible heating energy in Joules provided to the zone by condenser waste heat rejection for the timestep being reported.

#### Refrigeration Air Chiller Compressor Rack Return Air Sensible Heating Energy [J]

#### Refrigeration Air Chiller Compressor Rack Return Air Sensible Heating Rate [W]

These are the sensible heating energy, in Joules or Watts, provided to the HVAC system return air node by condenser heat rejection at the compressor rack.

## Refrigeration:Case

The Refrigeration Case object works in conjunction with a compressor rack, a refrigeration system, or a secondary loop object ([Refrigeration:CompressorRack](#refrigerationcompressorrack), [Refrigeration:System](#refrigerationsystem), or [Refrigeration:SecondarySystem](#refrigerationsecondarysystem)) to simulate the performance of a refrigerated case system. The refrigerated case model uses performance information at rated conditions along with performance curves for latent case credits and defrost heat load to determine performance at off-rated conditions. Energy use for lights, fans and anti-sweat heaters is modeled based on inputs for nominal power, schedules, and control type. The refrigerated case model accounts for the sensible and latent heat exchange with the surrounding environment (termed "case credits") which impacts the temperature and humidity in the zone where the case is located.

**A dataset has been provided containing refrigerated case input data for a large number of refrigerated cases from multiple manufacturers. See RefrigerationCases.idf.**

The Refrigeration Case object inputs include a case name, an availability schedule name, the zone name for the location of the case, and the temperature and relative humidity of the ambient (zone) air surrounding the refrigerated case at the rating conditions for case performance. Additional inputs include the total cooling capacity, latent heat ratio, and run time fraction of the refrigerated case at rated conditions. The case length and operating temperature (average temperature of air/products within the case) must also be specified.

The refrigerated case model requires two curves to describe performance at off-rated conditions (i.e., at different zone temperature and humidity levels): the latent case credit curve and the defrost energy correction curve. The user must enter cubic performance curves with the independent variable being user selectable (case temperature, zone relative humidity, or zone dewpoint temperature).

The user can select from eight case defrost types, with additional inputs being required depending on the type selected. The user must enter a defrost schedule (unless Case Defrost Type = None), and an optional defrost drip-down schedule can be specified to allow additional time for melted frost to drain from the cooling coil following the regular defrost period.

Inputs are required for case fans, lights, and anti-sweat heaters. Case lighting can be controlled by the user through entering a schedule. The case fans operate whenever the cooling coil is operating and during defrost periods for certain case defrost types. Anti-sweat heater power can be controlled by several methods which are user selectable. The model assumes that the fans are contained within the thermal envelope of the case and provide a direct heat load on the case cooling coil. For lighting and anti-sweat heaters, the user can specify the fraction of their respective heat loads that directly impact the case cooling coil (with the remainder of their heat load impacting the surrounding zone air).

The user has the option of specifying a case restocking schedule and a schedule for modifying the case credits if needed. Finally, the user can specify the under case HVAC return air fraction which determines the portion of the case credits that directly impact the HVAC return air conditions (with the remainder of the case credits impacting the zone sensible and latent loads).

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a refrigerated case. Any reference to this case by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the refrigerated case can operate during a given time period. A schedule value greater than 0 (maximum schedule value of 1.0 is typically used) indicates that the case will operate during a given time period. A value equal to 0 denotes that the case does not operate (everything is OFF: refrigeration, fans, lights, anti-sweat, etc.). Typically the refrigerated case will operate throughout the day (i.e., the schedule will contain 1 for all time periods); however, cases require maintenance and/or cleaning and this can be modeled accordingly using this schedule if desired. If this field is left blank, the case is assumed to be available at all time periods.

#### Field: Zone Name

The name of the zone where the refrigerated case is located. The refrigerated case will impact the air heat balance in this zone. When modeling multiple refrigerated cases connected to a single compressor rack where the condenser heat is rejected to the zone (ref: [Refrigeration:CompressorRack](#refrigerationcompressorrack)), **all** of the refrigerated cases connected to that compressor rack must be located in the same zone.

#### Field: Rated Ambient Temperature

Dry-bulb temperature of the ambient (zone) air surrounding the refrigerated case at the rating conditions for refrigerated case performance. The default value for this field is 23.9˚C (75˚F). This temperature must be greater than 0˚C and greater than the case operating temperature.

#### Field: Rated Ambient Relative Humidity

Relative humidity of the ambient (zone) air surrounding the refrigerated case at the rating conditions for refrigerated case performance. The default value for this field is 55% RH.

#### Field: Rated Total Cooling Capacity per Unit Length

The total, full load cooling capacity (sensible plus latent) in watts per unit length of refrigerated case (W/m) at rated conditions (i.e., at the Rated Ambient Temperature, Rated Ambient Relative Humidity, Case Operating Temperature, and with the Standard Case Fan Power per Unit Length and the Standard Case Lighting Power per Unit Length). The value entered for this field must be greater than zero, with a default value of 1900 W/m if the field is blank.

#### Field: Rated Latent Heat Ratio

The latent heat ratio (LHR=latent capacity divided by total cooling capacity) of the refrigerated case at rated conditions (i.e., at the Rated Ambient Temperature, Rated Ambient Relative Humidity, Case Operating Temperature, and with the Standard Case Fan Power per Unit Length and the Standard Case Lighting Power per Unit Length). The value for this field can range from zero to 1.0, with a default value of 0.3 if the field is blank.

#### Field: Rated Runtime Fraction

The runtime fraction of the refrigerated case at rated conditions (i.e., at the Rated Ambient Temperature, Rated Ambient Relative Humidity, and Case Operating Temperature). Even at rated conditions, refrigerated cases typically include additional cooling capacity to account for product stocking and recovery from defrost. The rated runtime fraction for refrigerated cases typically ranges from 0.8 to 0.9. The entered value for this field must be greater than 0.0 and less than or equal to 1.0, and the default value is 0.85 if the field is blank.

#### Field: Case Length

The length of the refrigerated case in meters. The entered value for this field must be greater than 0.0, and the default value is 3 meters if the field is blank.

#### Field: Case Operating Temperature

The average temperature of the air and products within the refrigerated case in ˚C. The refrigerated case manufacturer typically provides this information in the product literature. Typical operating temperatures for medium temperature cases are 1.6˚C to 7.2˚C (35˚F to 45˚F). Typical operating temperatures for low temperatures cases are -28.9˚C to -17.8˚C        (-20˚F to 0˚F). The entered value for this field must be less than 20˚C and lower than the Rated Ambient Temperature. The default value is 1.1˚C if the field is blank.

#### Field: Latent Case Credit Curve Type

This alpha field defines the form of the curve used to modify the latent case credits (latent heat removed from the zone where the refrigerated case is located) at off-rated conditions. The valid choices for this field are:

- CaseTemperatureMethod

- This method defines the variation in latent case credits as a cubic function of Case Operating Temperature. The result from the cubic curve is multiplied by the difference between the rated ambient relative humidity and the actual zone relative humidity, and one minus this value is multiplied by the Rated LHR to give the operating LHR at the actual zone humidity condition. (Representative cooefficient values for single-shelf horizontal and multi-shelf vertical display cases are given in the EnergyPlus Engineering Reference.)

- RelativeHumidityMethod

- This method defines the variation in latent case credits as a cubic function of ambient (zone) air relative humidity.

- DewpointMethod

- This method defines the variation in latent case credits as a cubic function of ambient (zone) air dewpoint temperature.

The default curve type is the Case Temperature Method. Refer to the Engineering Reference for further information on how latent case credits are modeled.

#### Field: Latent Case Credit Curve Name 

The name of a **cubic** performance curve (ref: Performance Curves) that parameterizes the variation of the latent case credits at off-rated conditions. The curve should be normalized to have a value of 1.0 at the rated ambient air conditions defined above.

#### Field: Standard Case Fan Power per Unit Length

The total standard fan power in Watts per unit length of refrigerated case (W/m). This value represents the fan power included in the field Rated Total Cooling Capacity per Unit Length, and is used to determine the sensible case credits. The entered value for this field must be greater than or equal to zero, and the default value is 75.0 W/m if the field is blank.

#### Field: Operating Case Fan Power per Unit Length

The total operating fan power in Watts per unit length of refrigerated case (W/m). Enter the actual power for the installed fans. The entered value for this field must be greater than or equal to zero, and the value is set equal to the standard case fan powers if the field is blank.

#### Field: Standard Case Lighting Power per Unit Length

The total standard lighting power in Watts per unit length of refrigerated case (W/m). This value should represent the lighting power provided by the case manufacturer and included in the "Rated Total Cooling Capacity per Unit Length" field and is used to determine the sensible case credits. For cases where the manufacturer does not include the lights in the Rated Total Cooling Capacity, this value should be zero (0.0). The entered value for this field must be greater than or equal to zero, and the default value is 90.0 W/m if the field is blank.

#### Field: Installed Case Lighting Power per Unit Length

The total installed lighting power in Watts per unit length of refrigerated case (W/m). Enter the actual power for the installed lights. This number may be greater or less than the standard lighting power value, depending upon the manufacturer's practice in specifying the case rated cooling capacity and whether or not energy-efficient lights are being modeled.  The next field (i.e. Case Lighting Schedule Name) may be used to specify the name of a schedule that contains the fraction of the Installed Case Lighting Power per Unit Length to be modeled for each timestep of the simulation period. The entered value for this field must be greater than or equal to zero, and the value is set equal to the standard case lighting power if the field is blank.

#### Field: Case Lighting Schedule Name

The name of the schedule (ref: Schedule) that denotes the fraction of installed refrigerated case lights that operate during a given time period. A schedule value greater than 0 indicates that the lights will operate during that time period (maximum schedule value of 1.0 means lights are fully on at the Installed Case Lighting Power per Unit Length level). A schedule value of zero denotes that the lights are off. The refrigerated case lights will typically operate only when the store is open for business and can be scheduled off as desired via this schedule. If this field is left blank, the default schedule has a value of 1 for all time periods.

#### Field: Fraction of Lighting Energy To Case

The fraction of the lighting energy that directly contributes to the case heat load. The remainder of the lighting energy (1 – fraction) is a heating load to the zone where the case is located. This field can be used to model lighting ballasts and bulbs located outside the air curtain of the refrigerated case. The value for this field can range from zero to 1.0, with a default value of 1.0 if the field is blank.

#### Field: Case Anti-Sweat Heater Power per Unit Length

The electric anti-sweat heater power in watts per unit length of refrigerated case (W/m). The entered value for this field must be greater than or equal to zero, and the default value is 0 W/m if the field is blank.

#### Field: Minimum Anti-Sweat Heater Power per Unit Length

The minimum electric anti-sweat heater power in watts per unit length of refrigerated case (W/m). Anti-sweat heater power will remain at or above this value for anti-sweat heater control types linear, dewpoint method, or heat balance method. Other anti-sweat heater control types disregard this field. The entered value for this field must be greater than or equal to zero, and the default value is 0 W/m if the field is blank.

#### Field: Anti-Sweat Heater Control Type

The type of anti-sweat heater control used for this refrigerated case. Valid choices are None, Constant, Linear, DewpointMethod, or HeatBalanceMethod. The default is None if the field is blank. Refer to the Engineering Reference for further information on how the different anti-sweat heater controls are modeled.

#### Field: Humidity at Zero Anti-Sweat Heater Energy

The value of this numeric field is only used when the Linear anti-sweat heater control type is selected. Enter the zone relative humidity where anti-sweat heater energy use is zero. Negative values for relative humidity may be entered. Other anti-sweat heater control types disregard this field. The value entered for this field must be less than the Rated Ambient Relative Humidity specified above. The default for this field is -10.0.

#### Field: Case Height

The value of this numeric field is used when Heat Balance Method anti-sweat heater control is selected. Enter the case height in meters. Other anti-sweat heater control types disregard this field. The entered value for this field must be greater than zero when Heat Balance Method anti-sweat heater control is selected, and the default value is 1.5 meters if the field is blank.

#### Field: Fraction of Anti-Sweat Heater Energy To Case

This value denotes the fraction of anti-sweat heater energy that results in a direct heat load to the refrigerated case. The remainder of the anti-sweat heater energy (1-fraction) is a heating load to the zone where the refrigerated case is located. The value for this field can range from zero to 1.0, with a default value of 1.0 if the field is blank.

#### Field: Case Defrost Power per Unit Length

The defrost power in watts per unit length of refrigerated case (W/m). This input is required for hot-gas, hot-fluid, or electric defrost types and is used to evaluate the load on the case as well as power or heat consumption. Refrigerated case manufacturers do not typically provide information on the heat input for hot-gas and hot-fluid defrost. Information provided for electric defrost power can be substituted here for refrigerated cases using hot-gas or hot-fluid defrost if other information is not available. Only electric Case Defrost Types consume electricity during the defrost period. The entered value for this field must be greater than or equal to zero, and the default value is 0 W/m if the field is blank.

#### Field: Case Defrost Type

The type of defrost used for this refrigerated case. Valid choices are None, OffCycle, HotGas, HotGaswithTemperatureTermination, Electric, ElectricwithTemperatureTermination, HotFluid, and HotFluidwithTemperatureTermination. The default defrost type is OffCycle if the field is blank. Refer to the Engineering Reference for further information on how the different case defrost types are modeled. In particular, hot-fluid defrost systems may be effective with much shorter defrost times than other systems, which should be reflected in the defrost and drip-down schedules.

#### Field: Case Defrost Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the refrigerated case requires defrosting. A schedule value of 1.0 indicates that defrost will be on during a given time period. A value equal to 0 denotes that the defrost is off. Defrost schedules normally repeat the duration and number of defrost cycles for each day of the year. The refrigerated case manufacturer typically provides this information with the product literature. The use of Compact Schedules (ref. Schedules) are ideal for this purpose. In a typical supermarket with many cases, it is important to diversify the defrost schedules in order to avoid large swings in the load placed upon the remainder of the refrigeration system.

#### Field: Case Defrost Drip-Down Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the refrigerated case requires additional time for draining condensate after the defrost period. A schedule value of 1.0 indicates that the defrost drip-down period is on during a given time period. A value equal to 0 denotes that the defrost drip-down period is over. The refrigerated case manufacturer typically provides this information with the product literature.

Each defrost drip-down period specified in this schedule should start at the same time as the corresponding defrost period specified in the schedule for Case Defrost Schedule, and the drip-down schedule defrost period should always be longer than or equal to the length of the defrost schedule time period. For example, if the case defrost schedule contains a defrost period from 7:00 to 7:15, you would specify a case defrost drip-down period from 7:00 to 7:20 if you wanted to model a 5 minute condensate drip-down period after the regular defrost period. If no drip-down schedule is entered, then the case defrost schedule (specified for the previous input field) will be used. The use of Compact Schedules (ref. Schedules) are ideal for this purpose.

#### Field: Defrost Energy Correction Curve Type

This alpha field defines the form of the correction curve used to modify the defrost energy use (and the associated load on the case cooling coil) at off-rated conditions. The valid choices for this field are:

- None

- This choice specifies that a defrost energy correction curve will not be used.

- CaseTemperatureMethod

- This method defines the variation in defrost energy as a cubic function of Case Operating Temperature. The result from the cubic curve is multiplied by the difference between the rated ambient relative humidity and the actual zone relative humidity, and one minus this value is multiplied by the Case Defrost Power to give the (average) operating defrost power at the actual zone humidity condition. (Representative cooefficient values for single-shelf horizontal and multi-shelf vertical display cases are given in the EnergyPlus Engineering Reference.)

- RelativeHumidityMethod

- This method defines the variation in defrost energy as a cubic function of ambient (zone) air relative humidity.

- DewpointMethod

- This method defines the variation in defrost energy as a cubic function of ambient (zone) air dewpoint temperature.

The default curve type is None. The methods described here (e.g. Case Temperature, Relative Humidity, and Dewpoint) are applicable only to Electric with Temperature Termination, Hot-Gas with Temperature Termination, and Hot-Brine with Temperature Termination case defrost types. Refer to the Engineering Reference for further information on how the defrost energy correction types are modeled.

#### Field: Defrost Energy Correction Curve Name

The name of a **cubic** performance curve (ref: Performance Curves) that parameterizes the variation of the defrost energy (and the associated load on the case cooling coil) at off-rated conditions. The curve should be normalized to have the value of 1.0 at the rated ambient air conditions defined above. The defrost energy correction curve name is used only for the Electric with Temperature Termination, Hot-Gas with Temperature Termination, and Hot-Brine with Temperature Termination case defrost types.

#### Field: Under Case HVAC Return Air Fraction

This field denotes the fraction of HVAC system (air loop) return air that passes beneath the refrigerated case, if any. At times it is necessary to design HVAC systems with under case returns to avoid overcooling the area around the refrigerated case, thus providing a more comfortable environment for the zone occupants. This return air fraction affects the portion of the case credits (sensible and latent) that are applied to the air returning to the HVAC system, while the remainder of the case credits directly impacts the zone air heat balance. Refer to the Engineering Reference for further information on how this is modeled. The sum of the Under Case HVAC Return Air Fractions for all refrigerated cases located in a single zone must be less than or equal to 1.0. The value for this field can range from zero to 1.0, with a default value of 0 if the field is blank.

#### Field: Refrigerated Case Restocking Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the refrigerated case is being restocked with product. The schedule should contain values in units of Watts per unit length of refrigerated case (W/m). This field can be used to denote the additional load imposed on the refrigerated case from stocking the case with product that is warmer than the refrigerated case temperature. This information is difficult to find and may require additional research for proper modeling; however, this schedule is available for this purpose if desired. If restocking of the refrigerated case will not be simulated, enter a schedule with values of zero for all time periods or leave this field blank.

#### Field: Case Credit Fraction Schedule Name

The name of the schedule (ref: Schedule) that denotes a fraction of both the sensible and latent case credits to be applied to the zone and/or the HVAC system return air. Schedule values must be from 0 to 1. This allows correction of the case credits for various refrigerated case types. For instance, if glass door refrigerated display cases are installed in a store that is operated 12 hours per day, then the doors will remain closed during the unoccupied times and would therefore reduce the sensible and latent case credits during the unoccupied time. Leaving this field blank will result in no case credit fraction being applied during the simulation.

#### Field: Design Evaporator Temperature or Brine Inlet Temperature

The value of this numeric field is used only with the detailed refrigeration system and is not read for the compressor-rack system. For the refrigeration system, it is used to evaluate compressor performance and is also used when the evaporator pressure and temperature are allowed to float at loads less than the design load. If the case is cooled by a secondary system (ref: [Refrigeration:SecondarySystem](#refrigerationsecondarysystem)), this value is the brine inlet temperature. The minimum value is -70C and the maximum value is 40C.  The default value is 5C less than the case temperature.

#### Field: Average Refrigerant Charge Inventory

The value of this optional field is the refrigerant inventory present in the refrigerated case during ordinary operation. The value is used to produce an estimate of the total refrigerant present in the refrigeration system. The value is entered in kg/m.

The following is an example input for a refrigerated case.

~~~~~~~~~~~~~~~~~~~~

     Refrigeration:Case,
      Multi-Deck Dairy/Deli Merchandiser with Synergy-E,  !- Name[Based on Hill Phoenix 6DMLH-NRG]
      ,   !- Availability Schedule [Used to turn case on/off including all power draws (ie fans, lights, etc)]
      UserProvideZoneName,    !- Zone Name [Location of Fixture]
      23.89,                  !- Rated Ambient Temperature {C}  [75øF]
      55,                     !- Rated Ambient Relative Humidity {percent}
      1394,         !- Rated Total Cooling Capacity per Unit length {W/m}  [1,450 Btu/hr/ft = 11,600 Btu/hr]
      0.30,                   !- Rated Latent Heat Ratio  [ Rated Latent Capacity]
      0.85,                   !- Rated Runtime Fraction
      2.4,                    !- Case Length {m}  [8 ft]
      3.33,                   !- Case Operating Temperature {C}  [38øF]
      CaseTemperatureMethod,  !- Latent Case Credit Curve Type
      Multi_Shelf_Vertical_Latent_Case_Credit_Curve,            !- Latent Case Credit Curve Name
      27.3,                   !- Standard Case Fan Power per Unit Length {W/m}  [8.3 W/ft]
      27.3,                   !- Operating Case Fan Power per Unit Length {W/m}  [8.3 W/ft = 67 W]
      0.0,                    !- Standard Case Lighting Power per Unit Length {W/m}  [0.0 W/ft]
      114.8,                  !- Installed Case Lighting Power per Unit Length {W/m}  [35.0 W/ft = 280 W]
      ,                       !- Case Lighting Schedule Name
      1.0,                    !- Fraction Of Lighting Energy To Case
      0.0,                    !- Case Anti-Sweat Heater Power per Unit Length {W/m}  [0.0 W/ft = 0 W]
      ,                       !- Minimum Anti-Sweat Heater Power per Unit Length {W/m}  []
      None,                   !- Anti-Sweat Heater Control Type
      ,                       !- Humidity At Zero Anti-Sweat Heater Energy {%}
      ,                       !- Case Height {m}  []
      1.0,                    !- Fraction of Anti-Sweat Heater Energy To Case
      0.0,                    !- Case Defrost Power per Unit Length {W/m}  [0.0 W/ft  0 W]
      Off Cycle,              !- Case Defrost Type
      UserProvideDefSched6PerDay45MinEa,                        !- Case Defrost Schedule Name
      UserProvideDefSched4PerDay42MinEa,                        !- Case Defrost Drip-Down Schedule
      CaseTemperatureMethod,  !- Defrost Energy Correction Curve Type
      Multi Shelf Vertical,   !- Defrost Energy Correction Curve Name
      0.00,                   !- Under Case HVAC Return Air Fraction
      ,                       !- Refrigerated Case Restocking Schedule Name  [Not modeling any restocking]
      ,                       !- Case Credit Fraction Schedule Name
      -3.33,                  !- Design Evaporator Temperature or Brine Inlet Temperature {C}  [26øF]
      ;                       !- Average Refrigerant Charge Inventory {kg/m}  [ = ]

~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Refrigeration Case Evaporator Total Cooling Rate [W]
    Zone,Sum,Refrigeration Case Evaporator Total Cooling Energy [J]
    Zone,Average,Refrigeration Case Evaporator Sensible Cooling Rate [W]
    Zone,Sum,Refrigeration Case Evaporator Sensible Cooling Energy [J]
    Zone,Average, Refrigeration Case Evaporator Latent Cooling Rate [W]
    Zone,Sum, Refrigeration Case Evaporator Latent Cooling Energy [J]
    Zone,Average,Refrigeration Case Zone Sensible Cooling Rate [W]
    Zone,Sum,Refrigeration Case Zone Sensible Cooling Energy [J]
    Zone,Average,Refrigeration Case Zone Sensible Heating Rate [W]
    Zone,Sum,Refrigeration Case Zone Sensible Heating Energy [J]
    Zone,Average,Refrigeration Case Zone Latent Rate [W]
    Zone,Sum,Refrigeration Case Zone Latent Energy [J]
    Zone,Average,Refrigeration Case Return Air Sensible Cooling Rate [W]
    Zone,Sum,Refrigeration Case Return Air Sensible Cooling Energy [J]
    Zone,Average,Refrigeration Case Return Air Sensible Heating Rate [W]
    Zone,Sum,Refrigeration Case Return Air Sensible Heating Energy [J]
    Zone,Average,Refrigeration Case Return Air Latent Rate [W]
    Zone,Sum,Refrigeration Case Return Air Latent Energy [J]
    Zone,Average,Refrigeration Case Evaporator Fan Electric Power [W]
    Zone,Sum,Refrigeration Case Evaporator Fan Electric Energy [J]
    Zone,Average,Refrigeration Case Lighting Electric Power [W]
    Zone,Sum,Refrigeration Case Lighting Electric Energy [J]
    Zone,Average,Refrigeration Case Latent Credit Curve Value []
    If case defrost type is Electric, Hot-Gas, or Hot-Brine with Temperature Termination
    Zone,Average,Refrigeration Case Defrost Energy Correction Curve Value
    If anti-sweat heater control type is not equal to None:
    Zone,Average,Refrigeration Case Anti Sweat Electric Power [W]
    Zone,Sum,Refrigeration Case Anti Sweat Electric Energy [J]
    If case defrost type is Electric or Electric with Temperature Termination:
    Zone,Average,Refrigeration Case Defrost Electric Power [W]
    Zone,Sum,Refrigeration Case Defrost Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Refrigeration Case Evaporator Total Cooling Rate [W]

This field is the total (sensible plus latent) cooling rate of the refrigerated case evaporator coil in Watts.

#### Refrigeration Case Evaporator Total Cooling Energy [J]

This field is the total (sensible plus latent) cooling of the refrigerated case evaporator coil in Joules over the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Case Evaporator Sensible Cooling Rate [W]

This field is the sensible cooling rate of the refrigerated case evaporator coil in Watts.

#### Refrigeration Case Evaporator Sensible Cooling Energy [J]

This field is the sensible cooling of the refrigerated case evaporator coil in Joules over the timestep being reported.

#### Refrigeration Case Evaporator Latent Cooling Rate [W]

This field is the latent cooling (dehumidification) rate of the refrigerated case evaporator coil in Watts.

#### Refrigeration Case Evaporator Latent Cooling Energy [J]

This field is the latent cooling (dehumidification) of the refrigerated case evaporator coil in Joules over the timestep being reported.

#### Refrigeration Case Zone Sensible Cooling Rate [W]

This field is the rate of sensible cooling case credits delivered to the zone in Watts. If an under case return duct is simulated, only a portion of the sensible case credits are applied to the zone. A positive value is reported when the zone is cooled by sensible case credits, otherwise a zero is reported.

#### Refrigeration Case Zone Sensible Cooling Energy [J]

This field is the amount of sensible cooling case credit energy delivered to the zone in Joules. If an under case return duct is simulated, only a portion of the sensible case credits are applied to the zone. A positive value is reported when the zone is cooled by sensible case credits, otherwise a zero is reported.

#### Refrigeration Case Zone Sensible Heating Rate [W]

This field is the rate of sensible heating case credits delivered to the zone in Watts. If an under case return duct is simulated, only a portion of the sensible case credits are applied to the zone. A positive value is reported when the zone is heated by sensible case credits, otherwise a zero is reported.

#### Refrigeration Case Zone Sensible Heating Energy [J]

This field is the amount of sensible heating case credit energy delivered to the zone in Joules. If an under case return duct is simulated, only a portion of the sensible case credits are applied to the zone. A positive value is reported when the zone is heated by sensible case credits, otherwise a zero is reported.

#### Refrigeration Case Zone Latent Rate [W]

This field is the rate of latent cooling (dehumidification) case credits delivered to the zone in Watts. If an under case return duct is simulated, only a portion of the latent case credits are applied to the zone. A negative value (or zero) will be reported when the refrigerated case provides dehumidification (thereby reducing the zone latent load).

#### Refrigeration Case Zone Latent Energy [J]

This field is the amount of latent cooling (dehumidification) case credit energy delivered to the zone in Joules. If an under case return duct is simulated, only a portion of the latent case credits are applied to the zone. A negative value (or zero) will be reported when the refrigerated case provides dehumidification (thereby reducing the zone latent load).

#### Refrigeration Case Return Air Sensible Cooling Rate [W]

This field is the rate of sensible cooling case credits delivered to the return air duct (zone return air node) in Watts. If an under case return duct is simulated, a portion of the sensible case credits are applied to the HVAC (zone) return air. A positive value is reported when the return air is cooled by sensible case credits, otherwise a zero is reported. If the HVAC system is off for a simulation timestep (no return air mass flow), then this sensible cooling is actually provided to the zone air instead (even though a non-zero value is reported here).

#### Refrigeration Case Return Air Sensible Cooling Energy [J]

This field is the amount of sensible cooling case credit energy delivered to the return air duct (zone return air node) in Joules. If an under case return duct is simulated, a portion of the sensible case credits are applied to the HVAC (zone) return air. A positive value is reported when the return air is cooled by sensible case credits, otherwise a zero is reported. If the HVAC system is off for a simulation timestep (no return air mass flow), then this sensible cooling is actually provided to the zone air instead (even though a non-zero value is reported here).

#### Refrigeration Case Return Air Sensible Heating Rate [W]

This field is the rate of sensible heating case credits delivered to the return air duct (zone return air node) in Watts. If an under case return duct is simulated, a portion of the sensible case credits are applied to the HVAC (zone) return air. A positive value is reported when the return air is heated by sensible case credits, otherwise a zero is reported. If the HVAC system is off for a simulation timestep (no return air mass flow), then this sensible heating is actually provided to the zone air instead (even though a non-zero value is reported here).

#### Refrigeration Case Return Air Sensible Heating Energy [J]

This field is the amount of sensible heating case credit energy delivered to the return air duct (thereby reducing the zone latent load). If the HVAC system is off for a simulation timestep (no return air mass flow), then this latent energy is actually provided to the zone air instead (even though a non-zero value is reported here).

#### Refrigeration Case Return Air Latent Energy [J]

#### Refrigeration Case Return Air Latent Rate [W]

These are the amount of latent cooling (dehumidification) case credit energy delivered to the return air duct (zone return air node) in Joules or Watts. If an under case return duct is simulated, only a portion of the latent case credits are applied to the HVAC (zone) return air. A negative value (or zero) will be reported since the refrigerated case provides dehumidification (thereby reducing the zone latent load). If the HVAC system is off for a simulation timestep (no return air mass flow), then this latent energy is actually provided to the zone air instead (even though a non-zero value is reported here).

#### Refrigeration Case Evaporator Fan Electric Power [W]

This field is the electric power input to the refrigerated case fan(s) in Watts.

#### Refrigeration Case Evaporator Fan Electric Energy [J]

This field is the electricity consumption of the refrigerated case fan(s) in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Case Lighting Electric Power [W]

This field is the electric power input to the refrigerated case lights in Watts.

#### Refrigeration Case Lighting Electric Energy [J]

This field is the electricity consumption of the refrigerated case lights in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Case Defrost Energy Correction Curve Value

This field is the output of the Defrost Energy Correction Curve which denotes the variation in defrost energy at off-rated conditions. The output of the defrost energy correction curve directly impacts the energy used during the defrost time period. It does not, however, impact the duration of the defrost period. This output is available only for Electric with Temperature Termination, Hot-Gas with Temperature Termination, or Hot-Brine with Temperature Termination case defrost types.

#### Refrigeration Case Latent Credit Curve Value []

This field is the output of the Latent Case Credit Curve which denotes the variation in latent case credits at off-rated conditions. The output of the latent case credit curve directly impacts the amount of frost formation on the refrigerated case evaporator coil and the requirement for defrost.

#### Refrigeration Case Anti Sweat Electric Power [W]

This field is the electric power input to the refrigerated case anti-sweat heater in Watts. This output is available unless anti-sweat heater control type is specified as NONE.

#### Refrigeration Case Anti Sweat Electric Energy [J]

This field is the total electricity consumption of the refrigerated case anti-sweat heater in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). This output is available unless anti-sweat heater control type is specified as NONE.

#### Refrigeration Case Defrost Electric Power [W]

This field is the electric power input to the refrigerated case electric defrost heater(s) in Watts. This output is available if case defrost type is Electric or Electric with Temperature Termination.

#### Refrigeration Case Defrost Electric Energy [J]

This field is the total electricity consumption of the refrigerated case electric defrost heater(s) in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). This output is available if case defrost type is Electric or Electric with Temperature Termination.

## Refrigeration:CaseAndWalkInList

This object provides a list of all the refrigerated cases and/or walk in coolers cooled by a single refrigeration system (ref: Refrigeraion:CompressorRack and [Refrigeration:System](#refrigerationsystem)). This list is extensible. Note that the names of all cases, walk-ins,air chillers, and caseandwalkinlists must be unique.  That is, you cannot give a list the same name as one of the cases.  Similarly, a walkin cannot have the same name as a case.This list may contain a combination of case and walk-in names OR a list of air chiller names.  Air chillers may not be included in any list that also includes cases or walk-ins.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a CaseAndWalkInList. Any reference to this list by another object will use this name.

#### Field: Case or WalkIn or Air Chiller <x> Name

Identifies a particular refrigerated case or walk-in. The name will be validated against the  Refrigeration Case, Refrigeration WalkIn, and Air Chiller names (ref: [Refrigeration:Case](#refrigerationcase),  Refrigeration:WalkIn, and [Refrigeration:AirChiller](#refrigerationairchiller)) in the input file. Up to 20 are provided in the IDD; this object is extensible.

The following is an example input for a CaseAndWalkInList.

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:CaseAndWalkInList,
        MediumTempCaselist,      !- Name
        UprightDairyCase,        !- Refrigeration Case or WalkIn Name #1
        LunchmeatCase,           !- Refrigeration Case or WalkIn Name #2
        MeatWalkIn,              !- Refrigeration Case or WalkIn Name #3
        DeliCaseOne;             !- Refrigeration Case or WalkIn Name #4

~~~~~~~~~~~~~~~~~~~~

## Refrigeration:WalkIn

The Refrigeration WalkIn object works in conjunction with a compressor rack, a refrigeration system, or a refrigeration secondary system object (Ref. [Refrigeration:CompressorRack](#refrigerationcompressorrack), [Refrigeration:System](#refrigerationsystem), or [Refrigeration:SecondarySystem](#refrigerationsecondarysystem)) to simulate the performance of a walk-in cooler. The walk-in cooler model uses information at rated conditions along with input descriptions for heat transfer surfaces facing multiple zones to determine performance. Energy use for lights, fans, and floor and anti-sweat heaters is modeled based on inputs for nominal power, schedules, and control type. The walk-in cooler model accounts for the sensible and latent heat exchange with the surrounding environment (termed "case credits") which impacts the temperature and humidity in each zone adjacent to the walk-in.

The walk-in cooler object inputs include a name, an availability schedule name, the rated cooling capacity, the rated operating temperature, the rated cooling source temperature, the rated total heating power and heating power schedule, the rated fan and lighting power and schedules, defrost type, defrost control type, defrost schedule name, drip-down schedule name, defrost power, the portion of the defrost energy used to melt ice (only for temperature termination control type), restocking schedule, refrigerant inventory, and the floor area and U-value.

For each zone adjacent to the walk-in, the user must input the zone name and the insulated surface area and U-value facing that zone. The user must also specify, for two types of doors, the door area, height, U-value, door opening schedule name, and any door opening protection.

Under case HVAC return air fraction, available for refrigerated cases, is not available for walk-in coolers.

### Field: Name

A unique user-assigned name for an instance of a refrigerated walk-in. Any reference to this walk-in by another object will use this name.

### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the walk in can operate during a given time period. A schedule value greater than 0 (maximum schedule value of 1.0 is typically used) indicates that the walkin will operate during a given time period. A value equal to 0 denotes that the case does not operate (everything is OFF: refrigeration, fans, lights, anti-sweat, etc.). Typically the walkin will operate throughout the day (i.e., the schedule will contain 1 for all time periods); however, walkins require maintenance and/or cleaning and this can be modeled accordingly using this schedule if desired. If this field is left blank, the default schedule has a value of 1 for all time periods.

### Field: Rated Coil Cooling Capacity

The total, full load cooling capacity (sensible plus latent) in watts (W) at rated conditions The value entered for this field must be greater than zero, with no default value.

### Field: Operating Temperature

The rated average temperature of the air and products within the walk-in cooler in ˚C. The entered value for this field must be less than 20˚C. There is no default value.

### Field: Rated Cooling Source Temperature

For a DX evaporator coil, enter the saturated evaporating temperature in ˚C.  For a fluid coil, enter the fluid coil entrance temperature in ˚C. There is no default value. This number is used, with temperatures for other refrigeration loads on any one system, to set that system's minimum suction pressure or minimum circulating fluid temperature. (This value is not used if the walkin is cooled by a compressor rack object.)

### Field: Rated Total Heating Power

The total heating power in watts including all anti-sweat, door, drip-pan, and floor heaters (W). This value is required and has no default value.

### Field: Heating Power Schedule Name

The name of the schedule (ref: Schedule) that denotes the fraction of heater power that operates during a given time period. A schedule value of zero denotes that all heaters are off. A schedule value greater than 0 indicates that some portion of the total heater power will operate during that time period (maximum schedule value of 1.0 means all heaters are fully on). For example, if door and floor heaters represent 50% of the total heater power and are on all the time, the minimum schedule value would be 0.5. If anti-sweat heaters represent 40% of the total heater power and are only on during certain hours, the schedule value during those hours would be increased by 0.4. If this field is left blank, the default schedule has a value of 1 for all time periods.

### Field: Rated Cooling Coil Fan Power

The cooling coil fan power in watts (W). This value has a default value of 375W. This fan is assumed to run continuously except during electric, hot brine, or hot gas defrost periods.

### Field: Rated Circulation Fan Power

The circulation fan power in watts (W). This value has a default value of 0 W. This fan is assumed to run continuously.

### Field: Rated Total Lighting Power

The total lighting power in watts including both display and task lighting (W). This value is required and has no default value.

### Field: Lighting Schedule Name

The name of the schedule (ref: Schedule) that denotes the fraction of walk-in lights that operate during a given time period. A schedule value greater than 0 indicates that the lights will operate during that time period (maximum schedule value of 1.0 means lights are fully on). A schedule value of zero denotes that the lights are off. If this field is left blank, the default schedule has a value of 1 for all time periods.

### Field: Defrost Type

The type of defrost used for this walk-in. Valid choices are None, Off-Cycle, HotFluid, and Electric. The default defrost type is Electric if the field is blank. HotFluid includes both hot gas and hot brine defrost systems. Refer to the Engineering Reference for further information on how the different defrost types are modeled.

### Field: Defrost Control Type

The type of defrost control used for this walkin. Valid choices are TimeSchedule and TemperatureTermination. The default is TimeSchedule if the field is blank. Refer to the Engineering Reference for further information on how the different defrost controls are modeled.

### Field: Defrost Schedule Name

The name of the schedule (ref: Schedule) that denotes when the walkin requires defrosting. A schedule value of 1.0 indicates that defrost will be on during a given time period if the defrost control type is "TimeSchedule". For the TemperatureTermination defrost control type, the defrost will start with the schedule value changes from 0 to 1, but will end when the ice is melted. A value equal to 0 denotes that the defrost is off. Defrost schedules normally repeat the duration and number of defrost cycles for each day of the year. The walkin manufacturer typically provides this information with the product literature. If TemperatureTermination control type is used, the defrost schedule is used for the defrost cycle start time and the defrost cycle end time is not allowed to extend beyond the scheduled off time. The use of Compact Schedules (ref. Schedules) are ideal for this purpose.

### Field: Defrost Drip-Down Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the walkin requires additional time for draining condensate after the defrost period. A schedule value of 1.0 indicates that the defrost drip-down period is on during a given time period. A value equal to 0 denotes that the defrost drip-down period is over. The walkin manufacturer typically provides this information with the product literature.

Each defrost drip-down period specified in this schedule should start at the same time as the corresponding defrost period specified in the schedule for Defrost Schedule, and the drip-down schedule defrost period should always be longer than or equal to the length of the defrost schedule time period. For example, if the defrost schedule contains a defrost period from 7:00 to 7:15, you would specify a case defrost drip-down period from 7:00 to 7:20 if you wanted to model a 5 minute condensate drip-down period after the regular defrost period. If no drip-down schedule is entered, then the defrost schedule (specified for the previous input field) will be used. The use of Compact Schedules (ref. Schedules) are ideal for this purpose.

### Field: Defrost Power

The defrost power in watts. This input is required for hot-fluid (hot gas or hot brine), or electric defrost types. Walkin manufacturers do not typically provide information on the heat input for hot gas and hot brine defrost. Information provided for electric defrost power can be substituted here for walkins using hot-gas or hot-brine defrost if other information is not available. Only electric Defrost Types consume electricity during the defrost period. The entered value for this field must be greater than or equal to zero.

### Field: Temperature Termination Defrost Fraction to Ice

When cooling coils go through a defrost cycle, only a portion of the defrost energy is actually used to melt the ice.  The rest of the defrost energy goes to increasing the temperature of the coils themselves and to the walkin environment. The Temperature Termination defrost control type calculates the end of the defrost cycle that corresponds to melting all the ice. Therefore, the user must input this fractional value. The default value is 0.7 for electric defrost and 0.3 for hot fluid defrost. Refer to the Engineering Reference for further information on how the defrost energy control types are modeled.

### Field: Restocking Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the walkin is being restocked with product. The schedule should contain values in units of Watts (note this is different from the restocking schedule values for cases that are entered per unit length). This field can be used to denote the additional load imposed on the walkin from stocking the walkin with product that is warmer than the walkin temperature. This information is difficult to find and may required additional research for proper modeling; however, this schedule is available for this purpose if desired. If restocking of the refrigerated walkin will not be simulated, enter a schedule with values of zero for all time periods or leave this field blank.

### Field: Average Refrigerant Charge Inventory

The value of this optional field is the refrigerant inventory present in the walkin during ordinary operation. The value is used to produce an estimate of the total refrigerant present in the refrigeration system. The value is entered in kg.

### Field: Insulated Floor Surface Area

The floor area in square meters. This value is required and has no default value.

### Field: Insulated Floor U-Value

The floor themal transmittance (in W/m^2^-K). This value has a default value of 0.3154. (This corresponds to R18 in Archaic American Insulation Units.  To convert other R-values to thermal transmittance, divide 5.678 by the R-value.  For example, R15 is 0.3785 W/m^2^-K and R5 is 1.136 W/m^2^-K.)

**THE REMAINING 12 FIELDS FOR THE WALK-IN COOLER MUST BE REPEATED FOR EACH ZONE WHICH IS IN CONTACT WITH A WALK-IN WALL, CEILING, OR DOOR. The IDD includes fields for 3 zones, but can be extended by repeating the last 12 values in the object.**

### Field: Zone <x> Name

The name of a zone adjoining the walkin. The walkin will impact the air heat balance in this zone. This zone must represent a conditioned space, that is, it must appear in a [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) object.

### Field: Total Insulated Surface Area Facing  Zone <x>

The total surface area (walls and ceilings) facing this particular zone in square meters. This value is required and has no default value.

### Field: Insulated Surface U-Value Facing Zone <x>

The surface (walls and ceilings) themal transmittance (in W/m^2^-K). This value has a default value of 0.3154. (This corresponds to R18 in Archaic American Insulation Units.  To convert other R-values to thermal transmittance, divide 5.678 by the R-value.  For example, R15 is 0.3785 W/m^2^-K and R5 is 1.136 W/m^2^-K.)

### Field: Area of Glass Reach In Doors Facing Zone <x>

The total area of glass doors facing this particular zone in square meters. The default is 0.0.

### Field: Height of Glass Reach In Doors Facing Zone <x>

The glass reach in door height in meters. The default is 1.5.

### Field: Glass Reach In Door U-Value Facing Zone <x>

The glass door themal transmittance (in W/m^2^-K). This field has a default value of 1.136. (This corresponds to R5 in Archaic American Insulation Units.)

### Field: Glass Reach In Door Opening Schedule Name Facing Zone <x>

The name of the schedule (ref: Schedule) that denotes the fraction of time glass doors are open during a given time period. The schedule values should be between 0 and 1.0. If no schedule is specified, the doors are assumed to be open 5% of the time (corresponding to a schedule value of 0.05).

### Field: Area of Stocking Doors Facing Zone <x>

The total area of stock doors facing this particular zone in square meters. The default is 0.0.

### Field: Height of Stocking Doors Facing Zone <x>

The stocking door height in meters. The default is 2.0.

### Field: Stocking Door U-Value Facing Zone <x>

The stocking door themal transmittance (in W/m^2^-K). This value has a default value of 0.3785. (This corresponds to R15 in Archaic American Insulation Units.)

### Field: Stocking Door Opening Schedule Name Facing Zone <x>

The name of the schedule (ref: Schedule) that denotes the fraction of time stocking doors are open during a given time period. The schedule values should be between 0 and 1.0. If no schedule is specified, the doors are assumed to be open 5% of the time (corresponding to a schedule value of 0.05).

### Field: Stocking Door Opening Protection Type Facing Zone <x>

The type of stocking door opening protection used for this walkin. Valid choices are None, AirCurtain, and StripCurtain. The default is AirCurtain if the field is blank. Use the type, StripCurtain, for both hanging strips and airlock vestibules. Refer to the Engineering Reference for further information on how the door protection types are modeled.

The following is an example input for a refrigeration walkin.

~~~~~~~~~~~~~~~~~~~~

       Refrigeration:WalkIn,
        WalkInFreezer,           !- Name
        CaseOperatingSched,      !- Availability Schedule Name
        4690.,                   !- Rated Coil Cooling Capacity {W}
        -2.22,                   !- Operating Temperature {C}
        -6.67,                   !- Rated Cooling Source Temperature {C}
        0.0,                     !- Rated Total Heating Power {W}
        CaseOperatingSched,      !- Heating Power Schedule Name
        735.,                    !- Rated Cooling Coil Fan Power {W}
        0.0,                     !- Rated Circulation Fan Power {W}
        120.,                    !- Rated Total Lighting Power {W}
        CaseLightingSched2,      !- Lighting Schedule Name
        Electric,                !- Defrost Type
        TimeSchedule,            !- Defrost Control Type
        CaseDefrostSched3,       !- Defrost Schedule Name
        CaseDripDownSched3,      !- Defrost Drip-Down Schedule Name
        5512.,                   !- Defrost Power {W}
        ,                        !- Temperature Termination Defrost Fraction to Ice {dimensionless}
        WalkInStockingSched,     !- Restocking Schedule Name
        ,                        !- Average Refrigerant Charge Inventory {kg}
        13.0,                    !- Insulated Floor Surface Area {m2}
        0.207,                   !- Insulated Floor U-Value {W/m2-C}
        BackRoom,                !- Zone Name
        43.4,                    !- Total Insulated Surface Area Facing this Zone {m2}
        .235,                    !- Insulated Surface U-Value Facing this Zone {W/m2-C}
        ,                        !- Area of Glass Reach In Doors Facing this Zone {m2}
        ,                        !- Height of Glass Reach In Doors Facing this Zone {m}
        ,                        !- Glass Reach In Door U Value Facing this Zone {W/m2-C}
        ,                        !- Glass Reach In Door Opening Schedule Name Facing this Zone
        2.0,                     !- Area of Stocking Doors Facing this Zone {m2}
        2.0,                     !- Height of Stocking Doors Facing this Zone {m}
        ,                        !- Stocking Door U Value Facing this Zone {W/m2-C}
        WIStockDoorOpenSch,      !- Stocking Door Opening Schedule Name Facing this Zone
        StripCurtain;            !- Stocking Door Opening Protection Type Facing this Zone

~~~~~~~~~~~~~~~~~~~~

## Refrigeration Case and WalkIn Outputs

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Refrigeration Walk In Evaporator Total Cooling Rate [W]
    Zone,Sum,Refrigeration Walk In Evaporator Total Cooling Energy [J]
    Zone,Average,Refrigeration Walk In Evaporator Sensible Cooling Rate [W]
    Zone,Sum,Refrigeration Walk In Evaporator Sensible Cooling Energy [J]
    Zone,Average,Refrigeration Walk In Evaporator Latent Cooling Rate [W]
    Zone,Sum,Refrigeration Walk In Evaporator Latent Cooling Energy [J]
    Zone,Average,Refrigeration Walk In Ancillary Electric Power [W]
    Zone,Sum,Refrigeration Walk In Ancillary Electric Energy [J]
    Zone,Average,Refrigeration Walk In Fan Electric Power [W]
    Zone,Sum,Refrigeration Walk In Fan Electric Energy [J]
    Zone,Average,Refrigeration Walk In Lighting Electric Power [W]
    Zone,Sum,Refrigeration Walk In Lighting Electric Energy [J]
    Zone,Average,Refrigeration Walk In Heater Electric Power [W]
    Zone,Sum,Refrigeration Walk In Heater Electric Energy [J]

    Report only for WalkIns using electric defrost
    Zone,Average,Refrigeration Walk In Defrost Electric Power [W]
    Zone,Sum,Refrigeration Walk In Defrost Electric Energy [J]

    Report for each Zone exchanging energy with the WalkIn
    Zone,Average,Refrigeration Walk In Zone Sensible Cooling Rate [W]
    Zone,Sum,Refrigeration Walk In Zone Sensible Cooling Energy [J]
    Zone,Average, Zone,Average,Refrigeration Walk In Zone Sensible Heating Rate [W]
    Zone,Sum,Refrigeration Walk In Zone Sensible Heating Energy [J]
    Zone,Average,Zone,Average,Refrigeration Walk In Zone Latent Rate [W]
    Zone,Sum,Refrigeration Walk In Zone Latent Energy [J]
~~~~~~~~~~~~~~~~~~~~

### Refrigeration Walk In Evaporator Total Cooling Rate [W]

This field is the total (sensible plus latent) cooling rate of the WalkIn evaporator coil in Watts.

### Refrigeration Walk In Evaporator Total Cooling Energy [J]

This field is the total (sensible plus latent) cooling of the WalkIn evaporator coil in Joules over the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

### Refrigeration Walk In Evaporator Sensible Cooling Rate [W]

This field is the sensible cooling rate of the WalkIn evaporator coil in Watts.

### Refrigeration Walk In Evaporator Sensible Cooling Energy [J]

This field is the sensible cooling of the WalkIn evaporator coil in Joules over the timestep being reported.

### Refrigeration Walk In Evaporator Latent Cooling Rate [W]

This field is the latent cooling (dehumidification) rate of the WalkIn evaporator coil in Watts.

### Refrigeration Walk In Evaporator Latent Cooling Energy [J]

This field is the latent cooling (dehumidification) of the WalkIn evaporator coil in Joules over the timestep being reported.

### Refrigeration Walk In Ancillary Electric Power [W]

This field is the total electricity (fan, heaters, lights, and electric defrost) used by the walkin in Watts.

### Refrigeration Walk In Ancillary Electric Energy [J]

This field is the electricity (fan, heaters, lights, and electric defrost)used by the WalkIn in Joules over the timestep being reported.

### Refrigeration Walk In Fan Electric Power [W]

This field is the electric power input to the WalkIn fan(s) in Watts.

### Refrigeration Walk In Fan Electric Energy [J]

This field is the electricity consumption of the WalkIn fan(s) in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

### Refrigeration Walk In Lighting Electric Power [W]

This field is the electric power input to the WalkIn lights in Watts.

### Refrigeration Walk In Lighting Electric Energy [J]

This field is the electricity consumption of the WalkIn lights in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

### Refrigeration Walk In Heater Electric Power [W]

This field is the electric power input to the WalkIn heaters in Watts.

### Refrigeration Walk In Heater Electric Energy [J]

This field is the total electricity consumption of the WalkIn heaters in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

### Refrigeration Walk In Defrost Electric Power [W]

This field is the electric power input to the WalkIn electric defrost heater(s) in Watts. This output is available if case defrost type is Electric.

### Refrigeration Walk In Defrost Electric Energy [J]

This field is the total electricity consumption of the WalkIn electric defrost heater(s) in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). This output is available if case defrost type is Electric.

*The following output variables are available for each zone adjacent to the walkin. An output variable identification is created for each zone exchanging energy with the walkin.  For example if the walkin, "DairyWalkIn", were exchanging energy with the zone, "SalesArea", the output variable identification would be "DairyWalkInInZoneSalesArea".*

### Refrigeration Walk In Zone Sensible Cooling Rate [W]

This field is the rate of sensible cooling case credits delivered to the zone in Watts. A positive value is reported when the zone is cooled by sensible case credits, otherwise a zero is reported.

### Refrigeration Walk In Zone Sensible Cooling Energy [J]

This field is the amount of sensible cooling case credit energy delivered to the zone in Joules. A positive value is reported when the zone is cooled by sensible case credits, otherwise a zero is reported.

### Refrigeration Walk In Zone Sensible Heating Rate [W]

This field is the rate of sensible heating case credits delivered to the zone in Watts. A positive value is reported when the zone is heated by sensible case credits, otherwise a zero is reported.

### Refrigeration Walk In Zone Sensible Heating Energy [J]

This field is the amount of sensible heating case credit energy delivered to the zone in Joules. A positive value is reported when the zone is heated by sensible case credits, otherwise a zero is reported.

### Refrigeration Walk In Zone Latent Rate [W]

This field is the rate of latent cooling (dehumidification) case credits delivered to the zone in Watts. A negative value (or zero) will be reported when the WalkIn provides dehumidification (thereby reducing the zone latent load).

### Refrigeration Walk In Zone Latent Energy [J]

This field is the amount of latent cooling (dehumidification) case credit energy delivered to the zone in Joules. A negative value (or zero) will be reported when the WalkIn provides dehumidification (thereby reducing the zone latent load).

## Refrigeration:System

The refrigeration system object simulates the performance of a supermarket refrigeration system when used along with other objects to define the refrigeration load(s), the compressor(s), and the condenser.

At least one refrigeration load object must be defined which may be one of four types of load, a refrigerated case, a walk-in cooler, a chiller providing refrigeration to a secondary loop, or a cascade condenser which absorbs heat rejected by a lower-temperature system (Ref. [Refrigeration:Case](#refrigerationcase), Refrigeration:WalkIn, [Refrigeration:SecondarySystem](#refrigerationsecondarysystem),  and [Refrigeration:Condenser:Cascade](#refrigerationcondensercascade)) If multiple loads are served by the same system , the user should use one or both of the list objects available to assign loads to the system. The first list includes all cases and walkins cooled directly by this system (Ref. [Refrigeration:CaseAndWalkInList](#refrigerationcaseandwalkinlist)). The second list includes any ‘transfer' loads, that is refrigeration loads borne by this system that are transferred from another refrigeration system via either a secondary loop chiller or a cascade condenser (Ref. [Refrigeration:TransferLoadList](#refrigerationtransferloadlist), see the Engineering Reference for more information about these types of systems).

The refrigeration system object is capable of modeling both single-stage and two-stage compression refrigeration systems.  The name of at least one compressor must be defined and a list object is available if the system is served by more than one compressor (Ref. [Refrigeration:Compressor](#refrigerationcompressor) and [Refrigeration:CompressorList](#refrigerationcompressorlist)).

Heat is rejected outdoors in a condenser by direct air flow, evaporative water cooling, by a water-cooled condenser with appropriate plant loop piping, or to a cascade condenser cooled by another refrigeration system(ref: [Refrigeration:Condenser:AirCooled](#refrigerationcondenseraircooled), [Refrigeration:Condenser:EvaporativeCooled](#refrigerationcondenserevaporativecooled), [Refrigeration:Condenser:WaterCooled](#refrigerationcondenserwatercooled), [Refrigeration:Condenser:Cascade](#refrigerationcondensercascade)). With evaporative cooling, water is sprayed through the air stream to effectively lower the air temperature experienced by the condenser coil as a result of water evaporation. The use of a water-cooled condenser requires the definition of a plant loop to supply cooling to the condenser.  If a cascade condenser is specified, the refrigeration system absorbing the rejected heat must also be defined.

The refrigeration system may also include a liquid suction and/or mechanical subcooler (Ref. [Refrigeration:Subcooler](#refrigerationsubcooler)).

The system object coordinates the energy flows between the other refrigeration objects and is used to set system parameters. Compressor waste heat can also be reclaimed for use by an optional air- or water-heating coil (Ref. [Coil:Heating:Desuperheater](#coilheatingdesuperheater) and [Coil:WaterHeating:Desuperheater](#coilwaterheatingdesuperheater)).

The inputs for the refrigeration system object, in addition to the names of the other refrigeration objects described above, include a name for this system, the minimum condensing temperature, the refrigeration system working fluid, and the type of saturated suction temperature control. Optional input fields are also provided for users seeking to keep track of refrigerant inventory and suction pipe heat gains.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a refrigeration system. Any reference to this refrigeration system by another object will use this name.

#### Field: Refrigerated Case or WalkIn or CaseAndWalkInList Name

Identifies a single case, a single walkin, single air chiller, or a particular list of refrigerated cases and walkins or air chillers, that is cooled by this refrigeration system. The name will be validated against the case, walkin, air chiller and CaseAndWalkInList names  (ref: [Refrigeration:Case](#refrigerationcase), Refrigeration:WalkIn, [Refrigeration:AirChiller](#refrigerationairchiller), and [Refrigeration:CaseAndWalkInList](#refrigerationcaseandwalkinlist)) in the input file. Only cases and walkins or air chillers served directly by the system should be included in this list.  Any cases, walkins, or air chillers served indirectly via a secondary loop should NOT be included in this list. Note that any system that serves an air chiller cannot also serve a case or walkin.

#### Field: Refrigeration Transfer Load or TransferLoad List Name

Identifies a single SecondarySystem, a single Cascade Condenser, OR or a list of Transfer Loads (where the list is comprised of SecondarySystems and/or Cascade Condensers) that are cooled by this refrigeration system. The name will be validated against the secondary system, cascade condenser, and TransferLoad list names (ref: [Refrigeration:SecondarySystem](#refrigerationsecondarysystem), [Refrigeration:Condenser:Cascade](#refrigerationcondensercascade), and [Refrigeration:TransferLoadList](#refrigerationtransferloadlist)) in the input file. Only secondary systems and cascade condensers served directly by the system should be included in this list. (NOTE – this entry is for a cascade condenser *cooled by* this system, not a condenser *absorbing heat rejected by* this system.)

#### Field: Condenser Name

The name of the condenser that is used to *reject heat from* this refrigeration system. The name will be validated against the condenser names (ref: [Refrigeration:Condenser:AirCooled](#refrigerationcondenseraircooled), [Refrigeration:Condenser:WaterCooled](#refrigerationcondenserwatercooled), [Refrigeration:Condenser:EvaporativeCooled](#refrigerationcondenserevaporativecooled), and [Refrigeration:Condenser:Cascade](#refrigerationcondensercascade)) in the input file.

#### Field:Compressor or  Compressor List Name

Identifies a single compressor, or a particular list of compressors, that provide cooling energy to this refrigeration system. The name will be validated against the compressor list names (ref: List:Refrigeration:Compressors) in the input file.  If the refrigeration systems contains two stages of compression, this field identifies the low-stage compressors connected to the two-stage system.

#### Field: Minimum Condensing Temperature

This numeric field specifies the minimum condensing temperature (C), which is usually determined by the temperature required to maintain acceptable thermal expansion valve performance.

#### Field: Refrigeration System Working Fluid Type

The type of refrigerant used by the system.  Valid choices are R11, R123, R134A, R12, R22, R404A, R507A, or R410A. This name will be validated against Fluid Names (ref: Fluid Properties section) in the input file. Note that the corresponding property data, available in FluidPropertiesRefData.idf, must by supplied in the input file.

#### Field: Suction Temperature Control Type

The type of saturated suction temperature control used by the system. Valid choices are **FloatSuctionTemperature** and **ConstantSuctionTemperature**.  If the field is blank, the default will be **ConstantSuctionTemperature**. See the Engineering Reference section, Variable Evaporator Temperature, for a discussion of this option.

#### Field: Mechanical Subcooler Name

This optional field identifies a mechanical subcooler that absorbs heat from this refrigeration system. This field should not be used for a mechanical subcooler that absorbs heat from another system. The name will be validated against the subcooler names (ref: [Refrigeration:Subcooler](#refrigerationsubcooler)) in the input file.

#### Field: Liquid Suction Heat Exchanger Subcooler Name

This optional field Identifies a particular liquid suction heat exchanger (LSHX) subcooler present in this refrigeration system. The name will be validated against the subcooler names (ref: [Refrigeration:Subcooler](#refrigerationsubcooler)) in the input file.

#### Field: Sum UA Suction Piping

This optional field is typically used when trying to determine the impact of pipe heat gains on system performance and zone heat balance, particularly in comparing a DX system to a secondary system. Enter the value for suction piping heat gain (in W/C).  That is, sum up the product of the pipe wall insulation conductance times the outer surface area of the pipe insulation. Please see the Engineering Reference for guidance in calculating this value. If the Sum UA Suction Piping is entered, the Suction Piping [Zone](#zone) Name is also required.

#### Field: Suction Piping Zone Name

This optional field is typically used when trying to determine the impact of pipe heat gains on system performance, particularly in comparing a DX system to a secondary system. (If the previous field, Sum UA Suction Piping, is blank, this field will not be used.) Enter the name of the zone where the suction piping is located. The suction piping heat gains will be calculated based upon the air temperature within this zone. The heat balance of this zone will also be affected by the piping heat exchange. Additional output variables are described at the end of this "Group-Refrigeration" section for the total impact of refrigeration on zones, including suction pipe heat exchange.

#### Field: End-Use Subcategory

This field allows the specification of a user-defined end-use subcategory (e.g., "Low Temperature Rack"). A new meter for reporting is created for each unique subcategory (Ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is left blank, the equipment will be assigned to the "General" end-use subcategory.

#### Field: Number of Compressor Stages

This field determines whether the refrigeration system contains one or two stages of compression.  In this field, enter either "1" for single-stage compression systems or "2" for two-stage compression systems.  If two stages of compression are selected, then an intercooler will be used between the compressor stages to cool the discharge of the low-stage compressors.  The default value is "1".

#### Field: Intercooler Type

This field determines the type of intercooler which is used for two-stage compression systems.  Valid choices for this field include "None", "Flash Intercooler" or "Shell-and-Coil Intercooler".  Single-stage systems require "None" while two-stage systems require either "Flash Intercooler" or "Shell-and-Coil Intercooler".  The default selection is "None", corresponding to a single-stage compression system.

#### Field: Shell-and-Coil Intercooler Effectiveness

When a shell-and-coil intercooler is selected for two-stage compression systems, this field allows the specification of the shell-and-coil intercooler effectiveness.  Values in this field will only be valid if "Shell-and-Coil Intercooler" is selected in the "Intercooler Type" field above.  The valid range of values for this field is between 0.0 and 1.0.  A default value of 0.80 will be used if none is specified.

#### Field: High-Stage Compressor or CompressorList Name

Identifies a single compressor, or a particular list of compressors, that comprise the high-stage compression of a two-stage compression refrigeration system. The name will be validated against the compressor list names (ref: List:Refrigeration:Compressors) in the input file.  Names in this field are only valid if two compressor stages have been specified in the "Number of Compressor Stages" field above.

The following is an example input for a single-stage compression refrigeration system.

~~~~~~~~~~~~~~~~~~~~

     Refrigeration:System,
        MediumTempSystem,        !- Refrigeration System Name
        MediumTempCaselist,      !-  Refrigeration Case or Walkin or CaseAndWalkInList Name
        ,                        !- Refrigeration Transfer Load or TransferLoad List Name
        AirCooledCondenserA,     !- Refrigeration Condenser Name
        MediumTempCompressorlist, !- Refrigeration Compressor or CompressorList Name
        25.0,                    !- Minimum Condensing Temperture {C}
        R134a,                   !- Refrigeration System Working Fluid
        ConstantSuctionTemperature,  !- Suction Temperature Control Type
        ,                        !- Optional mechanical subcooler name
        ,                        !- Optional LSHX subcooler name
        ,                        !- Sum UA Distribution Piping {W/K}
        ,                        !- Distribution Piping Zone Name
        MedTempRefrig;           !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

The following is an example input for a two-stage compression refrigeration system.

~~~~~~~~~~~~~~~~~~~~

     Refrigeration:System,
        LowTempSystem,           !- Refrigeration System Name
        LowTempCaselist,         !-  Refrigeration Case or Walkin or CaseAndWalkInList Name
        ,                        !- Refrigeration Transfer Load or TransferLoad List Name
        AirCooledCondenserB,     !- Refrigeration Condenser Name
        LowStageCompressorList,  !- Refrigeration Compressor or CompressorList Name
        25.0,                    !- Minimum Condensing Temperture {C}
        R404A,                   !- Refrigeration System Working Fluid
        ConstantSuctionTemperature,  !- Suction Temperature Control
        ,                        !- Optional mechanical subcooler name
        ,                        !- Optional LSHX subcooler name
        ,                        !- Sum UA Distribution Piping {W/K}
        ,                        !- Distribution Piping Zone Name
        LowTempRefrig,           !- End-Use Subcategory
        2,                       !- Number of Compressor Stages
        Shell-and-Coil Intercooler,  !- Intercooler Type
        0.7,                     !- Shell-and-Coil Intercooler Effectiveness
        HiStageCompressorList    !- High-Stage Compressor or CompressorList Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    THE FOLLOWING OUTPUTS ARE AVAILABLE FOR SYSTEMS THAT SERVE CASES AND/OR WALKINS:
    Zone,Average, Refrigeration System Total Compressor Electric Power [W]
    Zone,Sum, Refrigeration System Total Compressor Electric Energy [J]
    Zone,Average, Refrigeration System Average Compressor COP [W/W]
    Zone,Average, Refrigeration System Total Compressor Heat Transfer Rate [W]
    Zone,Sum, Refrigeration System Total Compressor Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Total Cases and Walk Ins Heat Transfer Rate [W]
    Zone,Sum, Refrigeration System Total Cases and Walk Ins Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Total Transferred Load Heat Transfer Rate [W]
    Zone,Sum, Refrigeration System Total Transferred Load Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Total Suction Pipe Heat Gain Rate [W]
    Zone,Sum, Refrigeration System Total Suction Pipe Heat Gain Energy [J]
    Zone,Average, Refrigeration System Net Rejected Heat Transfer Rate [W]
    Zone,Sum, Refrigeration System Net Rejected Heat Transfer Energy [J]
    Zone,Average,Refrigeration System Estimated Refrigerant Inventory Mass [kg]
    Zone,Average, Refrigeration System Liquid Suction Subcooler Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Liquid Suction Subcooler Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Estimated Refrigerant Inventory [kg]
    Zone,Average, Refrigeration System Estimated Refrigerant Mass Flow Rate [kg/s]
    Zone,Average, Refrigeration System Condensing Temperature [C]'
    Zone,Average, Refrigeration System Evaporating Temperature [C]
    Zone,Average, Refrigeration System Suction Pipe Suction Temperature [C]
    Zone,Average, Refrigeration System Thermostatic Expansion Valve Liquid Temperature [C]
    Zone,Average, Refrigeration System Estimated High Stage Refrigerant Mass Flow Rate [kg/s]
    Zone,Sum,Refrigeration System Total High Stage Compressor Electric Energy [J]
    Zone,Average,Refrigeration System Total High Stage Compressor Electric Power [W]
    Zone,Sum,Refrigeration System Total High Stage Compressor Heat Transfer Energy [J]
    Zone,Average,Refrigeration System Total High Stage Compressor Heat Transfer Rate [W]
    Zone,Sum,Refrigeration System Total Low and High Stage Compressor Electric Energy [J]
    Zone,Sum,Refrigeration System Total Low Stage Compressor Electric Energy [J]
    Zone,Average,Refrigeration System Total Low Stage Compressor Electric Power [W]
    Zone,Sum,Refrigeration System Total Low Stage Compressor Heat Transfer Energy [J]
    Zone,Average,Refrigeration System Total Low Stage Compressor Heat Transfer Rate [W]
    Zone,Average,Refrigeration System Estimated Low Stage Refrigerant Mass Flow Rate [kg/s]

    THE FOLLOWING OUTPUTS ARE AVAILABLE FOR SYSTEMS THAT SERVE AIR CHILLERS:

    HVAC,Average, Refrigeration Air Chiller System Total Compressor Electric Power [W]
    HVAC,Sum, Refrigeration Air Chiller System Total Compressor Electric Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Average Compressor COP [W/W]
    HVAC,Average, Refrigeration Air Chiller System Total Compressor Heat Transfer Rate [W]
    HVAC,Sum, Refrigeration Air Chiller System Total Compressor Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Total Air Chiller Heat Transfer Rate [W]
    HVAC,Sum, Refrigeration Air Chiller System Total Case and Walk In Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Total Transferred Load Heat Transfer Rate [W]
    HVAC,Sum, Refrigeration Air Chiller System Total Transferred Load Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Total Suction Pipe Heat Gain Rate [W]
    HVAC,Sum, Refrigeration Air Chiller System Total Suction Pipe Heat Gain Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Net Rejected Heat Transfer Rate [W]
    HVAC,Sum, Refrigeration Air Chiller System Net Rejected Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Liquid Suction Subcooler Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Liquid Suction Subcooler Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Estimated Refrigerant Inventory Mass [kg]
    HVAC,Average, Refrigeration Air Chiller System Estimated Refrigerant Mass Flow Rate [kg/s]
    HVAC,Average,Refrigeration Air Chiller System Intercooler Temperature [C]
    HVAC,Average,Refrigeration Air Chiller System Intercooler Pressure [Pa]
    HVAC,Average, Refrigeration Air Chiller System Condensing Temperature [C]'
    HVAC,Average, Refrigeration Air Chiller System Evaporating Temperature [C]
    HVAC,Average, Refrigeration Air Chiller System Suction Temperature [C]
    HVAC,Average, Refrigeration Air Chiller System TXV Liquid Temperature [C]
    HVAC,Average,Refrigeration Air Chiller System Estimated High Stage Refrigerant Mass Flow Rate [kg/s]
    HVAC,Sum,Refrigeration Air Chiller System Total High Stage Compressor Electric Energy [J]
    HVAC,Average,Refrigeration Air Chiller System Total High Stage Compressor Electric Power [W]
    HVAC,Sum,Refrigeration Air Chiller System Total High Stage Compressor Heat Transfer Energy [J]
    HVAC,Average,Refrigeration Air Chiller System Total High Stage Compressor Heat Transfer Rate [W]
    HVAC,Sum, Refrigeration Air Chiller System Total Low and High Stage Compressor Electric Energy [J]
    HVAC,Sum,Refrigeration Air Chiller System Total Low Stage Compressor Electric Energy [J]
    HVAC,Average,Refrigeration Air Chiller System Total Low Stage Compressor Electric Power [W]
    HVAC,Sum, Refrigeration Air Chiller System Total Low Stage Compressor Heat Transfer Energy [J]
    HVAC,Average,Refrigeration Air Chiller System Total Low Stage Compressor Heat Transfer Rate [W]
    HVAC,Average,Refrigeration Air Chiller System Estimated Low Stage Refrigerant Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### THE FOLLOWING OUTPUTS ARE AVAILABLE FOR SYSTEMS THAT SERVE CASES AND/OR WALKINS

#### Refrigeration System Total Compressor Electric Power [W]

This output is the total electric power input to the system compressor(s) in Watts.

#### Refrigeration System Total Compressor  Electric Consumption [J]

This is the electricity consumption of the system's compressor(s) in Joules for the timestep being reported. The individual consumption for each compressor is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration System Total Low Stage Compressor Electric Power [W]

This output is the total electric power input to the system's low-stage compressor(s) in Watts.  This output is valid only for two-stage compression systems.

#### Refrigeration System Total Low Stage Compressor Electric Energy [J]

This is the electricity consumption of the system's low-stage compressor(s) in Joules for the timestep being reported. The individual consumption for each compressor is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  This output is valid only for two-stage compression systems.

#### Refrigeration System Total High Stage Compressor Electric Power [W]

This output is the total electric power input to the system's high-stage compressor(s) in Watts.  This output is valid only for two-stage compression systems.

#### Refrigeration System Total High Stage Compressor Electric Energy [J]

This is the electricity consumption of the system's high-stage compressor(s) in Joules for the timestep being reported. The individual consumption for each compressor is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  This output is valid only for two-stage compression systems.

#### Refrigeration System Total Low and High Stage Compressor Electric Energy  [J]

This is the total electricity consumption of the system's low- and high-stage compressor(s) in Joules for the timestep being reported. The individual consumption for each compressor is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  This output is valid only for two-stage compression systems.

#### Refrigeration System Average Compressor COP [W/W]

This output is the system average compressor COP, the total refrigeration effect divided by the total power to the compressors.

#### Refrigeration System Total Compressor Heat Transfer Rate [W]

This output is the total heat transfer rate of the Refrigeration Compressor rack in Watts. It is the sum of all of the heat transfer rates for the refrigerated cases, walk-ins, secondary loops, cascade condensers, and mechanical subcoolers that are cooled by this system. This value does not include compressor or condenser fan heat. If specified as in input value, the suction pipe heat gains are included in this value.

#### Refrigeration System Total Compressor Heat Transfer Energy [J]

This is the total heat transfer of the Refrigeration Compressor rack in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration System Total Low Stage Compressor Heat Transfer Rate [W]

This output is the total heat transfer rate of the low-stage compressors in Watts. It is the sum of all of the heat transfer rates for the refrigerated cases, walk-ins, secondary loops, cascade condensers, and mechanical subcoolers that are cooled by this system. This value does not include compressor or condenser fan heat. If specified as in input value, the suction pipe heat gains are included in this value.  This output is valid only for two-stage compression systems.

#### Refrigeration System Total Low Stage Compressor Heat Transfer Energy [J]

This is the total heat transfer of the low-stage compressors in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  This output is valid only for two-stage compression systems.

#### Refrigeration System Total High Stage Compressor Heat Transfer Rate [W]

This output is the total heat transfer rate of the high-stage compressors in Watts. It is the sum of all of the heat transfer rates for the refrigerated cases, walk-ins, secondary loops, cascade condensers, and mechanical subcoolers that are cooled by this system. This value does not include compressor or condenser fan heat. If specified as in input value, the suction pipe heat gains are included in this value.  This output is valid only for two-stage compression systems.

#### Refrigeration System Total High Stage Compressor Heat Transfer Energy [J]

This is the total heat transfer of the high-stage compressors in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  This output is valid only for two-stage compression systems.

#### Refrigeration System Total Cases and Walk Ins Heat Transfer Rate [W]

This output is the total heat transfer rate from the refrigerated cases and walk-ins served directly by this system in Watts. It is the sum of all of the heat transfer rates for the refrigerated cases and walk-ins that are connected directly to this system. This value does not include compressor or condenser fan heat or the heat transfer for cases and walk-ins served by any connected secondary systems.

#### Refrigeration System Total Cases and Walk Ins Heat Transfer Energy [J]

This output is the total heat transfer energy from the refrigerated cases and walk-ins served directly by this system in Joules. It is the sum of all of the heat transfered for the refrigerated cases and walk-ins that are connected directly to this system. This value does not include compressor or condenser fan heat or the heat transfer for cases and walk-ins served by any connected secondary systems.

#### Refrigeration System Total Transferred Load Heat Transfer Rate [W]

This output is the sum of the heat transfer rates for any secondary loops, cascade condensers, and mechanical subcoolers cooled by this system, minus the benefit of any mechanical subcooler providing cooling to this system in Watts. Therefore, if the only transfer load between two systems is a mechanical subcooler, the same amount will show as a negative value for the system receiving the cooling effect and as a positive number for the system serving that cooling load. It also includes the pump energy for any secondary loops and the compressor energy for any cascade condenser systems that are cooled by this system. (See the Engineering Reference for more details about the loads placed by secondary systems upon the primary system.)

#### Refrigeration System Total Transferred Load Heat Transfer Energy [J]

This output is the sum of the heat transfered for any secondary loops, cascade condensers, and mechanical subcoolers cooled by this system, minus the benefit of any mechanical subcooler providing cooling to this system in Joules. Therefore, if the only transfer load between two systems is a mechanical subcooler, the same amount will show as a negative value for the system receiving the cooling effect and as a positive number for the system serving that cooling load. It also includes the pump energy for any secondary loops and the compressor energy for any cascade condenser systems that are cooled by this system. (See the Engineering Reference for more details about the loads placed by secondary systems upon the primary system.)

#### Refrigeration System Total Suction Pipe Heat Gain Rate [W]

This output is the total heat transfer rate for suction piping served by this system in Watts. Note this is an optional input, and is only available if the user has described the suction piping heat gain characteristics in the input.

#### Refrigeration System Total Suction Pipe Heat Gain Energy [J]

This output is the total heat transfer rate for suction piping served by this system in Watts. Note this is an optional input, and is only available if the user has described the suction piping heat gain characteristics in the input.

#### Refrigeration System Net Rejected Heat Transfer Rate [W]

This output is the total heat rejected by this system to the system condenser in Watts. It does not include system heat rejection that has been recovered for useful purposes. However, if a water-cooled condenser was used to provide heat to a separate water loop, the energy transferred to that loop is included here.

#### Refrigeration System Net Rejected Heat Transfer Energy [J]

This output is the total heat rejected by this system to the system condenser in Joules for the timestep being reported. It does not include system heat rejection that has been recovered for useful purposes. However, if a water-cooled condenser was used to provide heat to a separate water loop, the energy transferred to that loop is included here.

#### Refrigeration System Liquid Suction Subcooler Heat Transfer Rate [W]

This output is the total heat transferred from the liquid condensate before the thermal expansion valve to the suction gas.

#### Refrigeration System Liquid Suction Subcooler Heat Transfer Energy [J]

This output is the total heat transferred from the liquid condensate before the thermal expansion valve to the suction gas.

#### Refrigeration System Estimated Refrigerant Inventory Mass [kg]

This output is the sum of the input inventory values for the condenser, receiver, cases, and liquid pipes that are a part of this system.

#### Refrigeration System Estimated Refrigerant Mass Flow Rate [kg/s]

This output is the calculated refrigerant mass flow through the compressors for this system.

#### Refrigeration System Estimated Low Stage Refrigerant Mass Flow Rate [kg/s]

This output is the calculated refrigerant mass flow through the low-stage compressors for this system.  This output is valid only for two-stage compression systems.

#### Refrigeration System Estimated High Stage Refrigerant Mass Flow Rate [kg/s]

This output is the calculated refrigerant mass flow through the high-stage compressors for this system.  This output is valid only for two-stage compression systems.

#### Refrigeration System Intercooler Temperature [C]

This is the saturation temperature in the intercooler.  This output is valid only for two-stage compression systems.

#### Refrigeration System Intercooler Pressure [Pa]

This is the saturation pressure in the intercooler.  This output is valid only for two-stage compression systems.

#### Refrigeration System Condensing Temperature [C]

This is the saturated condensing temperature.

#### Refrigeration System Evaporating Temperature [C]

This is the saturated evaporating temperature.

#### Refrigeration System Suction Pipe Suction Temperature [C]

This is the temperature at the compressor inlet including superheat after the cases and superheat from any liquid suction heat exchangers.

#### Refrigeration System Thermostatic Expansion Valve Liquid Temperature [C] 

This is the temperature entering the thermal expansion valve before the cases, equal to the condensing temperature minus any subcooling included in the condenser or provided by mechanical and/or liquid suction heat exchanger subcoolers.

#### THE FOLLOWING OUTPUTS ARE AVAILABLE FOR SYSTEMS THAT SERVE AIR CHILLERS

#### Refrigeration Air Chiller System Total Compressor Electric Power [W]

This output is the total electric power input to the system compressor(s) in Watts.

#### Refrigeration Chiller System Total Compressor  Electric Consumption [J]

This is the electricity consumption of the system's compressor(s) in Joules for the timestep being reported. The individual consumption for each compressor is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller System Total Low Stage Compressor Electric Power [W]

This output is the total electric power input to the system's low-stage compressor(s) in Watts.  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Total Low Stage Compressor Electric Energy [J]

This is the electricity consumption of the system's low-stage compressor(s) in Joules for the timestep being reported. The individual consumption for each compressor is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Total High Stage Compressor Electric Power [W]

This output is the total electric power input to the system's high-stage compressor(s) in Watts.  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Total High Stage Compressor Electric Energy [J]

This is the electricity consumption of the system's high-stage compressor(s) in Joules for the timestep being reported. The individual consumption for each compressor is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Total Low and High Stage Compressor Electric Energy [J]

This is the total electricity consumption of the system's low- and high-stage compressor(s) in Joules for the timestep being reported. The individual consumption for each compressor is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Average Compressor COP [W/W]

This output is the system average compressor COP, the total refrigeration effect divided by the total power to the compressors

#### Refrigeration Air Chiller System Total Compressor Heat Transfer Rate [W]

This output is the total heat transfer rate of the Refrigeration Compressor rack in Watts. It is the sum of all of the heat transfer rates for the refrigerated cases, walk-ins, secondary loops, cascade condensers, and mechanical subcoolers that are cooled by this system. This value does not include compressor or condenser fan heat. If specified as in input value, the suction pipe heat gains are included in this value.

#### Refrigeration Air Chiller System Total Compressor Heat Transfer Energy [J]

This is the total heat transfer of the Refrigeration Compressor rack in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller System Total Low Stage Compressor Heat Transfer Rate [W]

This output is the total heat transfer rate of the low-stage compressors in Watts. It is the sum of all of the heat transfer rates for the refrigerated cases, walk-ins, secondary loops, cascade condensers, and mechanical subcoolers that are cooled by this system. This value does not include compressor or condenser fan heat. If specified as in input value, the suction pipe heat gains are included in this value.  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Total Low Stage Compressor Heat Transfer Energy [J]

This is the total heat transfer of the low-stage compressors in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Total High Stage Compressor Heat Transfer Rate [W]

This output is the total heat transfer rate of the high-stage compressors in Watts. It is the sum of all of the heat transfer rates for the refrigerated cases, walk-ins, secondary loops, cascade condensers, and mechanical subcoolers that are cooled by this system. This value does not include compressor or condenser fan heat. If specified as in input value, the suction pipe heat gains are included in this value.  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Total High Stage Compressor Heat Transfer Energy [J]

This is the total heat transfer of the high-stage compressors in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Total Air Chiller Heat Transfer Rate [W]

This output is the total heat transfer rate from the refrigerated cases and walk-ins served directly by this system in Watts. It is the sum of all of the heat transfer rates for the refrigerated cases and walk-ins that are connected directly to this system. This value does not include compressor or condenser fan heat or the heat transfer for cases and walk-ins served by any connected secondary systems.

#### Refrigeration Air Chiller System Total Case and Walk In Heat Transfer Energy [J]

This output is the total heat transfer energy from the refrigerated cases and walk-ins served directly by this system in Joules. It is the sum of all of the heat transfered for the refrigerated cases and walk-ins that are connected directly to this system. This value does not include compressor or condenser fan heat or the heat transfer for cases and walk-ins served by any connected secondary systems.

#### Refrigeration Air Chiller System Total Transferred Load Heat Transfer Rate [W]

This output is the sum of the heat transfer rates for any secondary loops, cascade condensers, and mechanical subcoolers cooled by this system, minus the benefit of any mechanical subcooler providing cooling to this system in Watts. Therefore, if the only transfer load between two systems is a mechanical subcooler, the same amount will show as a negative value for the system receiving the cooling effect and as a positive number for the system serving that cooling load. It also includes the pump energy for any secondary loops and the compressor energy for any cascade condenser systems that are cooled by this system. (See the Engineering Reference for more details about the loads placed by secondary systems upon the primary system.)

#### Refrigeration Air Chiller System Total Transferred Load Heat Transfer Energy [J]

This output is the sum of the heat transfered for any secondary loops, cascade condensers, and mechanical subcoolers cooled by this system, minus the benefit of any mechanical subcooler providing cooling to this system in Joules. Therefore, if the only transfer load between two systems is a mechanical subcooler, the same amount will show as a negative value for the system receiving the cooling effect and as a positive number for the system serving that cooling load. It also includes the pump energy for any secondary loops and the compressor energy for any cascade condenser systems that are cooled by this system. (See the Engineering Reference for more details about the loads placed by secondary systems upon the primary system.)

#### Refrigeration Air Chiller System Total Suction Pipe Heat Gain Rate [W]

This output is the total heat transfer rate for suction piping served by this system in Watts. Note this is an optional input, and is only available if the user has described the suction piping heat gain characteristics in the input.

#### Refrigeration Air Chiller System Total Suction Pipe Heat Gain Energy [J]

This output is the total heat transfer rate for suction piping served by this system in Watts. Note this is an optional input, and is only available if the user has described the suction piping heat gain characteristics in the input.

#### Refrigeration Air Chiller System Net Rejected Heat Transfer Rate [W]

This output is the total heat rejected by this system to the system condenser in Watts. It does not include system heat rejection that has been recovered for useful purposes. However, if a water-cooled condenser was used to provide heat to a separate water loop, the energy transferred to that loop is included here.

#### Refrigeration Air Chiller System Net Rejected Heat Transfer Energy [J]

This output is the total heat rejected by this system to the system condenser in Joules for the timestep being reported. It does not include system heat rejection that has been recovered for useful purposes. However, if a water-cooled condenser was used to provide heat to a separate water loop, the energy transferred to that loop is included here.

#### Refrigeration Air Chiller System Liquid Suction Subcooler Heat Transfer Rate [W]

This output is the total heat transferred from the liquid condensate before the thermal expansion valve to the suction gas.

#### Refrigeration Air Chiller System Liquid Suction Subcooler Heat Transfer Energy [J]

This output is the total heat transferred from the liquid condensate before the thermal expansion valve to the suction gas.

#### Refrigeration Air Chiller System Mechanical Subcooler Heat Transfer Energy [J]

#### Refrigeration Air Chiller System Estimated Refrigerant Inventory Mass [kg]

This output is the sum of the input inventory values for the condenser, receiver, cases, and liquid pipes that are a part of this system.

#### Refrigeration Air Chiller System Estimated Refrigerant Mass Flow Rate [kg/s]

This output is the calculated refrigerant mass flow through the compressors for this system.

#### Refrigeration Air Chiller System Estimated Low Stage Refrigerant Mass Flow Rate [kg/s]

This output is the calculated refrigerant mass flow through the low-stage compressors for this system.  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Estimated High Stage Refrigerant Mass Flow Rate [kg/s]

This output is the calculated refrigerant mass flow through the high-stage compressors for this system.  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Intercooler Temperature [C]

This is the saturation temperature in the intercooler.  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Intercooler Pressure [Pa]

This is the saturation pressure in the intercooler.  This output is valid only for two-stage compression systems.

#### Refrigeration Air Chiller System Condensing Temperature [C]

This is the saturated condensing temperature.

#### Refrigeration Air Chiller System Evaporating Temperature [C]

This is the saturated evaporating temperature.

#### Refrigeration Air Chiller System Suction Temperature [C]

This is the temperature at the compressor inlet including superheat after the cases and superheat from any liquid suction heat exchangers.

#### Refrigeration Air Chiller System TXV Liquid Temperature [C]

This is the temperature entering the thermal expansion valve before the cases, equal to the condensing temperature minus any subcooling included in the condenser or provided by mechanical and/or liquid suction heat exchanger subcoolers.

## Refrigeration:TranscriticalSystem

The [Refrigeration:TranscriticalSystem](#refrigerationtranscriticalsystem) object allows users to model detailed transcritical carbon dioxide (CO~2~) booster refrigeration systems used in supermarkets.  The object allows for modeling either a single stage system with medium-temperature loads or a two stage system with both medium- and low-temperature loads.

The input objects required to model a detailed transcritical CO~2~ refrigeration system include the following:

- One [Refrigeration:TranscriticalSystem](#refrigerationtranscriticalsystem) object,
- At least one refrigeration load object which may include any combination of the following:

- [Refrigeration:Case](#refrigerationcase),
- Refrigeration:WalkIn,
- [Refrigeration:CaseAndWalkInList](#refrigerationcaseandwalkinlist) (may include both cases and/or walk-in cooler names),

- At least one [Refrigeration:Compressor](#refrigerationcompressor) object (multiple compressors are entered using a [Refrigeration:CompressorList](#refrigerationcompressorlist)),
- One [Refrigeration:GasCooler:AirCooled](#refrigerationgascooleraircooled) object,

Output variables are available to describe the total heat exchange between all the refrigeration objects and the zones containing these objects.  These variables are described at the end of this section.

At least one refrigeration load object must be defined which may be one of two types of loads, including a refrigerated display case and a walk-in cooler, (Ref. [Refrigeration:Case](#refrigerationcase), and Refrigeration:WalkIn).  If multiple loads are served by the same system, the user should use the refrigerated case and walk-in list object available to assign all cases and walk-ins cooled directly by this system (Ref. [Refrigeration:CaseAndWalkInList](#refrigerationcaseandwalkinlist)).

The name of at least one compressor must be defined and a list object is available if the system is served by more than one compressor (Ref. [Refrigeration:Compressor](#refrigerationcompressor) and [Refrigeration:CompressorList](#refrigerationcompressorlist)).

Heat is rejected to the outdoors via an air-cooled gas cooler (Ref. [Refrigeration:GasCooler:AirCooled](#refrigerationgascooleraircooled)).

The [Refrigeration:TranscriticalSystem](#refrigerationtranscriticalsystem) object coordinates the energy flows between the other refrigeration objects and is used to set system parameters.

The inputs for the refrigeration system object, in addition to the names of the other refrigeration objects described above, include a name for this system, the receiver pressure, the subcooler effectiveness, and the refrigeration system working fluid.  Optional input fields are also provided for users seeking to keep track of suction pipe heat gains.

A detailed description of the transcritical CO~2~ booster refrigeration system may be found in the Refrigeration section of the Engineering Reference.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a transcritical CO~2~ refrigeration system.  Any reference to this refrigeration system by another object will use this name.

#### Field: System Type

Identifies the transcritical CO~2~ refrigeration system as either a single-stage system with only medium-temperature loads, or a two-stage system with both medium- and low-temperature loads.  Valid choices are:  SingleStage for single stage systems or TwoStage for two stage systems.

#### Field: Medium Temperature Refrigerated Case or WalkIn or CaseAndWalkInList Name

Identifies a single case, a single walk-in, or a particular list of refrigerated cases and walk-ins, that is cooled by the medium-temperature stage of the refrigeration system.  The name will be validated against the case, walk-in, and CaseAndWalkInList names (Ref. [Refrigeration:Case](#refrigerationcase), Refrigeration:WalkIn, and [Refrigeration:CaseAndWalkInList](#refrigerationcaseandwalkinlist)) in the input file.  Only medium temperature cases and walk-ins served directly by the system should be included in this list.  Note that this entry is required for both single-stage and two-stage systems.

#### Field: Low Temperature Refrigerated Case or WalkIn or CaseAndWalkInList Name

Identifies a single case, a single walk-in, or a particular list of refrigerated cases and walk-ins, that is cooled by the low-temperature stage of the refrigeration system.  The name will be validated against the case, walk-in, and CaseAndWalkInList names (Ref. [Refrigeration:Case](#refrigerationcase), Refrigeration:WalkIn, and [Refrigeration:CaseAndWalkInList](#refrigerationcaseandwalkinlist)) in the input file.  Only low temperature cases and walk-ins served directly by the system should be included in this list.  Note that this entry is valid only for two-stage systems.

#### Field: Refrigeration Gas Cooler Name

The name of the gas cooler that is used to reject heat from the transcritical refrigeration system.  The name will be validated against the gas cooler names (Ref. [Refrigeration:GasCooler:AirCooled](#refrigerationgascooleraircooled)) in the input file.

#### Field: High Pressure Compressor or Compressor List Name

Identifies a single compressor, or a particular list of compressors, that provides compression for a single-stage system or the high pressure stage of a two-stage system.  The name will be validated against the compressor list names (Ref. List:Refrigeration:Compressors) in the input file.  Note that this entry is required for both single-stage and two-stage systems.

#### Field: Low Pressure Compressor or Compressor List Name

Identifies a single compressor, or a particular list of compressors, that provides compression for the low pressure stage of a two-stage system.  The name will be validated against the compressor list names (Ref. List:Refrigeration:Compressors) in the input file.  Note that this entry is valid only for two-stage systems.

#### Field: Receiver Pressure

This numeric field specifies the refrigerant pressure (Pa) in the receiver.  The default value for the receiver pressure is 4.0 × 10^6^ Pa.

#### Field: Subcooler Effectiveness

This numeric field specifies the heat exchanger effectiveness of the subcooler.  The default value for the subcooler effectiveness is 0.4.

#### Field: Refrigeration System Working Fluid Type

The type of refrigerant used by the system.  It is assumed that carbon dioxide will be the refrigerant used in the transcritical CO~2~ refrigeration system.  However, the user must specify the name used in the input file which refers to carbon dioxide.  This name will be validated against Fluid Names (Ref. Fluid Properties section) in the input file.  Note that the corresponding property data for carbon dioxide (R744), available in FluidPropertiesRefData.idf, must be supplied in the input file.

#### Field: Sum UA Suction Piping for Medium Temperature Loads

This optional field is typically used to determine the impact of pipe heat gains on system performance and zone heat balance.  Enter the value for suction piping heat gain (in W/C) for the medium-temperature suction line, i.e., sum up the product of the pipe wall insulation conductance times the outer surface area of the pipe insulation.  Please see the Engineering Reference for guidance in calculating this value.  If the Sum UA Suction Piping for Medium Temperature Loads is entered, the Medium Temperature Suction Piping [Zone](#zone) Name is also required.

#### Field: Medium Temperature Suction Piping Zone Name

This optional field is typically used to determine the impact of pipe heat gains on system performance.  If the previous field, Sum UA Suction Piping for Medium Temperature Loads, is blank, this field will not be used.  Enter the name of the zone where the medium-temperature suction piping is located.  The suction piping heat gains will be calculated based upon the air temperature within this zone.  The heat balance of this zone will also be affected by the piping heat exchange.  Additional output variables are described at the end of this section for the total impact of refrigeration on zones, including suction pipe heat exchange.

#### Field: Sum UA Suction Piping for Low Temperature Loads

This optional field is typically used to determine the impact of pipe heat gains on system performance and zone heat balance.  Enter the value for suction piping heat gain (in W/C) for the low-temperature suction line, i.e., sum up the product of the pipe wall insulation conductance times the outer surface area of the pipe insulation.  Please see the Engineering Reference for guidance in calculating this value.  If the Sum UA Suction Piping for Low Temperature Loads is entered, the Low Temperature Suction Piping [Zone](#zone) Name is also required.  Note that this entry is valid only for two-stage systems.

#### Field: Low Temperature Suction Piping Zone Name

This optional field is typically used to determine the impact of pipe heat gains on system performance.  If the previous field, Sum UA Suction Piping for Low Temperature Loads, is blank, this field will not be used.  Enter the name of the zone where the low-temperature suction piping is located. The suction piping heat gains will be calculated based upon the air temperature within this zone.  The heat balance of this zone will also be affected by the piping heat exchange.  Additional output variables are described at the end of this section for the total impact of refrigeration on zones, including suction pipe heat exchange.  Note that this entry is valid only for two-stage systems.

#### Field: End-Use Subcategory

This field allows the specification of a user-defined end-use subcategory (e.g., "Medium Temperature System").  A new meter for reporting is created for each unique subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).  Subcategories are also reported in the ABUPS table.  If this field is left blank, the equipment will be assigned to the "General" end-use subcategory.

The following is an example input for a transcritical CO~2~ refrigeration system.

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:TranscriticalSystem,
        TransRefrigSys,          !- Name
        TwoStage,                !- System Type
        MTLoads,                 !- Medium Temperature Refrigerated Case or Walkin or CaseAndWalkInList Name
        LTLoads,                 !- Low Temperature Refrigerated Case or Walkin or CaseAndWalkInList Name
        RefrigGasCooler,         !- Refrigeration Gas Cooler Name
        HPCompressors,           !- High Pressure Compressor or CompressorList Name
        LPCompressors,           !- Low Pressure Compressor or CompressorList Name
        4000000,                 !- Receiver Pressure
        0.4,                     !- Subcooler Effectiveness
        R744,                    !- Refrigeration System Working Fluid Type
        ,                        !- Sum UA Suction Piping for Medium Temperature Loads
        ,                        !- Medium Temperature Suction Piping Zone Name
        ,                        !- Sum UA Suction Piping for Low Temperature Loads
        ,                        !- Low Temperature Suction Piping Zone Name
        TransRefrigSys;          !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    Zone,Average, Refrigeration Transcritical System Total High Pressure Compressor Electric Power [W]
    Zone,Sum, Refrigeration Transcritical System Total High Pressure Compressor Electric Energy [J]
    Zone,Average, Refrigeration Transcritical System Low Pressure Compressor Electric Power [W]
    Zone,Sum, Refrigeration Transcritical System Low Pressure Compressor Electric Energy [J]
    Zone,Sum, Refrigeration Transcritical System Total Compressor Electric Energy [J]
    Zone,Average, Refrigeration Transcritical System Average COP [W/W]
    Zone,Average, Refrigeration Transcritical System Medium Temperature Cases and Walk Ins Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Transcritical System Medium Temperature Cases and Walk Ins Heat Transfer Energy [J]
    Zone,Average, Refrigeration Transcritical System Low Temperature Cases and Walk Ins Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Transcritical System Low Temperature Cases and Walk Ins Heat Transfer Energy [J]
    Zone,Sum, Refrigeration Transcritical System Total Cases and Walk Ins Heat Transfer Energy [J]
    Zone,Average, Refrigeration Transcritical System Medium Temperature Suction Pipe Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Transcritical System Medium Temperature Suction Pipe Heat Transfer Energy [J]
    Zone,Average, Refrigeration Transcritical System Low Temperature Suction Pipe Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Transcritical System Low Temperature Suction Pipe Heat Transfer Energy [J]
    Zone,Average, Refrigeration Transcritical System High Pressure Compressor Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Transcritical System High Pressure Compressor Heat Transfer Energy [J]
    Zone,Average, Refrigeration Transcritical System Low Pressure Compressor Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Transcritical System Low Pressure Compressor Heat Transfer Energy [J]
    Zone,Average, Refrigeration Transcritical System Net Rejected Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Transcritical System Net Rejected Heat Transfer Energy [J]
    Zone,Average, Refrigeration Transcritical System Estimated Refrigerant Inventory Mass [kg]
    Zone,Average, Refrigeration Transcritical System Refrigerant Mass Flow Rate [kg/s]
    Zone,Average, Refrigeration Transcritical System Medium Temperature Evaporating Temperature [C]
    Zone,Average, Refrigeration Transcritical System Medium Temperature Suction Temperature [C]
    Zone,Average, Refrigeration Transcritical System Low Temperature Evaporating Temperature [C]
    Zone,Average, Refrigeration Transcritical System Low Temperature Suction Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Refrigeration Transcritical System Total High Pressure Compressor Electric Power [W]

This output is the total electric power input to the system's high pressure compressor(s) in Watts.

#### Refrigeration Transcritical System Total High Pressure Compressor Electric Energy [J]

This is the electricity consumption of the system's high pressure compressor(s) in Joules for the timestep being reported.  The individual consumption for each compressor is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Transcritical System Low Pressure Compressor Electric Power [W]

This output is the total electric power input to the system's low pressure compressor(s) in Watts.

#### Refrigeration Transcritical System Low Pressure Compressor Electric Energy [J]

This is the electricity consumption of the system's low pressure compressor(s) in Joules for the timestep being reported.  The individual consumption for each compressor is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Transcritical System Total Compressor Electric Energy [J]

This output is the total electric power input to all of the system's compressor(s) in Watts.

#### Refrigeration Transcritical System Average COP [W/W]

This output is the system average compressor COP, which is the total refrigeration effect divided by the total power to all of the compressors.

#### Refrigeration Transcritical System Medium Temperature Cases and Walk Ins Heat Transfer Rate [W]

This output is the total heat transfer rate from the medium temperature refrigerated cases and walk-ins served directly by this system in Watts.  It is the sum of all of the heat transfer rates for the medium temperature refrigerated cases and walk-ins that are connected directly to this system.

#### Refrigeration Transcritical System Medium Temperature Cases and Walk Ins Heat Transfer Energy [J]

This output is the total heat transfer energy from the medium temperature refrigerated cases and walk-ins served directly by this system in Joules. It is the sum of all of the heat transferred for the medium temperature refrigerated cases and walk-ins that are connected directly to this system.

#### Refrigeration Transcritical System Low Temperature Cases and Walk Ins Heat Transfer Rate [W]

This output is the total heat transfer rate from the low temperature refrigerated cases and walk-ins served directly by this system in Watts.  It is the sum of all of the heat transfer rates for the low temperature refrigerated cases and walk-ins that are connected directly to this system.

#### Refrigeration Transcritical System Low Temperature Cases and Walk Ins Heat Transfer Energy [J]

This output is the total heat transfer energy from the low temperature refrigerated cases and walk-ins served directly by this system in Joules. It is the sum of all of the heat transferred for the low temperature refrigerated cases and walk-ins that are connected directly to this system.

#### Refrigeration Transcritical System Total Cases and Walk Ins Heat Transfer Energy [J]

This output is the total heat transfer energy from all the low- and medium-temperature refrigerated cases and walk-ins served directly by this system in Joules. It is the sum of all of the heat transferred for all the refrigerated cases and walk-ins that are connected directly to this system.

#### Refrigeration Transcritical System Medium Temperature Suction Pipe Heat Transfer Rate [W]

This output is the total heat transfer rate for the medium-temperature suction piping served by this system in Watts.  Note this is an optional input, and is only available if the user has described the medium-temperature suction piping heat gain characteristics in the input.

#### Refrigeration Transcritical System Medium Temperature Suction Pipe Heat Transfer Energy [J]

This output is the total heat transfer rate for the medium-temperature suction piping served by this system in Watts.  Note this is an optional input, and is only available if the user has described the medium-temperature suction piping heat gain characteristics in the input.

#### Refrigeration Transcritical System Low Temperature Suction Pipe Heat Transfer Rate [W]

This output is the total heat transfer rate for the low-temperature suction piping served by this system in Watts.  Note this is an optional input, and is only available if the user has described the low-temperature suction piping heat gain characteristics in the input.

#### Refrigeration Transcritical System Low Temperature Suction Pipe Heat Transfer Energy [J]

This output is the total heat transfer rate for the low-temperature suction piping served by this system in Watts.  Note this is an optional input, and is only available if the user has described the low-temperature suction piping heat gain characteristics in the input.

#### Refrigeration Transcritical System High Pressure Compressor Heat Transfer Rate [W]

This output is the total heat transfer rate of the high pressure Compressors in Watts.  It is the sum of all of the heat transfer rates for the low- and medium-temperature refrigerated cases and walk-ins as well as the low pressure compressors that are cooled by this system.  This value does not include compressor or condenser fan heat.  If specified as in input value, the suction pipe heat gains are included in this value.

#### Refrigeration Transcritical System High Pressure Compressor Heat Transfer Energy [J]

This is the total heat transfer of the high pressure compressors in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Transcritical System Low Pressure Compressor Heat Transfer Rate [W]

This output is the total heat transfer rate of the low pressure Compressors in Watts.  It is the sum of all of the heat transfer rates for the low temperature refrigerated cases and walk-ins that are cooled by this system.  This value does not include compressor or condenser fan heat.  If specified as in input value, the suction pipe heat gains are included in this value.

#### Refrigeration Transcritical System Low Pressure Compressor Heat Transfer Energy [J]

This is the total heat transfer of the low pressure compressors in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Transcritical System Net Rejected Heat Transfer Rate [W]

This output is the total heat rejected by this system to the system gas cooler in Watts.  It does not include system heat rejection that has been recovered for useful purposes.

#### Refrigeration Transcritical System Net Rejected Heat Transfer Energy [J]

This output is the total heat rejected by this system to the system gas cooler in Joules for the timestep being reported.  It does not include system heat rejection that has been recovered for useful purposes.

#### Refrigeration Transcritical System Estimated Refrigerant Inventory Mass [kg]

This output is the sum of the input refrigerant inventory values for the gas cooler, receiver, cases, and liquid pipes that are a part of this system.

#### Refrigeration Transcritical System Refrigerant Mass Flow Rate [kg/s]

This output is the calculated refrigerant mass flow rate through the high pressure compressors for this system.

#### Refrigeration Transcritical System Medium Temperature Evaporating Temperature [C]

This is the saturated evaporating temperature for the medium temperature loads.

#### Refrigeration Transcritical System Medium Temperature Suction Temperature [C]

This is the temperature at the high pressure compressor inlet including superheat after the display cases and superheat from the suction line heat exchanger.

#### Refrigeration Transcritical System Low Temperature Evaporating Temperature [C]

This is the saturated evaporating temperature for the low-temperature loads.

#### Refrigeration Transcritical System Low Temperature Suction Temperature [C]

This is the temperature at the low pressure compressor inlet including superheat after the display cases.

## Refrigeration:Compressor 

The compressors are described here using manufacturer's data for curves that provide the efficiency, cooling capacity, and power consumption.  The corresponding rated values for cycle superheat and subcooling are also required.

**A dataset has been provided containing the compressor rating data, including the performance curve inputs, for a large number of compressors.  See RefrigerationCompressorCurves.idf.**

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a compressor. Any reference to this compressor by another object will use this name.

#### Field: Refrigeration Compressor Power Curve Name

This required field provides the name of the curve object that describes the compressor power as a function of the evaporating and condensing temperatures, as specified in ARI 540. The curve is of the bicubic form(ref: [Curve:Bicubic](#curvebicubic)).  However, the input order for the Energy Plus bicubic curve does not match the ARI 540 standard order. When this curve is entered, the user should use the following assignments:

N1 is ARI C1 and N2 is ARI C2.

N3 is ARI C4 and N4 is ARI C3.

N5 is ARI C6 and N6 is ARI C5,

N7 is ARI C7.

N8 is ARI C10, N9 is ARI C8, and N10 is ARI C9.

N11 is the Minimum evaporating temperature and N12 is the Maximum evaporating temperature.

N13 is the the Minimum condensing temperature and N14 is the Maximum condensing temperature

#### Field: Refrigeration Compressor Capacity Curve Name

This required field provides the name of the curve object that describes the compressor cooling capacity as a function of the evaporating and condensing temperatures, as specified in ARI 540. The curve is of the bicubic form(ref: [Curve:Bicubic](#curvebicubic)).  However, the input order for the Energy Plus bicubic curve does not match the ARI 540 standard order. When this curve is entered, the user should use the following assignments:

N1 is ARI C1 and N2 is ARI C2.

N3 is ARI C4 and N4 is ARI C3.

N5 is ARI C6 and N6 is ARI C5,

N7 is ARI C7.

N8 is ARI C10, N9 is ARI C8, and N10 is ARI C9.

N11 is the Minimum evaporating temperature and N12 is Maximum evaporating temperature.

N13 is the Minimum condensing temperature and N14 is the Maximum condensing temperature

#### Field: Rated Superheat

Some manufacturers specify a constant return gas temperature while others specify a constant superheat (the difference between the saturated evaporating temperature and the actual return gas temperature). Use this field for compressors that provide their rated superheat. (Do NOT use both this field and the Rated Return Gas Temperature field.) The rated superheat is specified in units of delta C.

#### Field: Rated Return Gas Temperature

Some manufacturers specify a constant return gas temperature ( which may also be called the Rated Suction Temperature) while others specify a constant superheat.  Use this field for compressors that provide their rated return gas temperature. (Do NOT use both this field and the Rated Superheat field.) The rated return gas temperature is specified in units of C.

#### Field: Rated Liquid Temperature

Some compressor manufactures rate their equipment according to a constant subcooling (the difference between the saturated condensing temperature and the actual liquid temperature entering the thermal expansion valve before the refrigeration load). Other manufacturers specify a constant liquid temperature.Use this field if the manufacturer specifies the rated liquid temperature.  (Do NOT use both this field and the Rated Subcooling field.) The units for this field are degrees C.

#### Field: Rated Subcooling

Some compressor manufactures rate their equipment according to a constant subcooling (the difference between the saturated condensing temperature and the actual liquid temperature entering the thermal expansion valve before the refrigeration load). Other manufacturers specify a constant liquid temperature.Use this field if the manufacturer specifies the rated subcooling.  (Do NOT use both this field and the Rated Liquid Temperature field.) The units for this field are delta C.

#### Field: End-Use Subcategory

This field allows the specification of a user-defined end-use subcategory (e.g., "Low Temperature System"). A new meter for reporting is created for each unique subcategory (Ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is left blank, the equipment will be assigned to the "General" end-use subcategory.

The following is an example input for a refrigeration compressor.

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:Compressor,
        CompressorA,             !- Name
        PowerCurveCompA,         !- Refrigeration Compressor Power Curve Name
        CapCurveCompA,           !- Refrigeration Compressor Capacity Curve Name
        10.0,                    ! – Rated Superheat {delta C}
        ,                        !- Rated Return Gas Temperature {C}
        ,                        ! – Rated Liquid Temperature{C}
        15.0,                    !- Rated Subcooling {delta C}
        MedTempRefrig;           !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    THE FOLLOWING OUTPUTS ARE AVAILABLE FOR SYSTEMS THAT SERVE CASES AND/OR WALKINS:
    Zone,Average,Refrigeration Compressor Electric Power [W]
    Zone,Sum,Refrigeration Compressor Electric Energy [J]
    Zone,Average,Refrigeration Compressor Heat Transfer Rate [W]
    Zone,Sum,Refrigeration Compressor Heat Transfer Energy [J]
    Zone,Sum,Refrigeration Compressor Run Time  Fraction []
    THE FOLLOWING OUTPUTS ARE AVAILABLE FOR SYSTEMS THAT SERVE AIR CHILLERS:
    HVAC,Average,Refrigeration Air Chiller System Compressor Electric Power [W]
    HVAC,Sum,Refrigeration Air Chiller System Compressor Electric Energy [J]
    HVAC,Average,Refrigeration Air Chiller System Compressor Heat Transfer Rate [W]
    HVAC,Sum,Refrigeration Air Chiller System Compressor Heat Transfer Energy [J]
    HVAC,Average,Refrigeration Chiller Compressor Run TimeFraction []
~~~~~~~~~~~~~~~~~~~~

#### THE FOLLOWING OUTPUTS ARE AVAILABLE FOR SYSTEMS THAT SERVE CASES AND/OR WALKINS

#### Refrigeration Compressor Electric Power [W]

This output is the electric power input to the compressor in Watts.

#### Refrigeration Compressor Electric Energy [J]

This is the electric energy consumed by the compressor in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Compressor Heat Transfer Rate [W]

This output is the heat removed from the refrigerated cases by the compressor in Watts.

#### Refrigeration Compressor  Heat Transfer Energy [J]

This is the heat removed from the refrigerated cases by the compressor in Joules for the timestep being reported.

#### Refrigeration Compressor Run Time  Fraction []

This is the fraction of the time step when the compressor ran to meet the load.  It is a value between 0.0 and 1.0.

#### THE FOLLOWING OUTPUTS ARE AVAILABLE FOR SYSTEMS THAT SERVE AIR CHILLERS

#### Refrigeration Air Chiller System Compressor Electric Power [W]

This output is the electric power input to the compressor in Watts.

#### Refrigeration Air Chiller System Compressor Electric Energy [J]

This is the electric energy consumed by the compressor in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller System Compressor Heat Transfer Rate [W]

This output is the heat removed from the refrigerated cases by the compressor in Watts.

#### Refrigeration Air Chiller System Compressor Heat Transfer Energy [J]

This is the heat removed from the refrigerated cases by the compressor in Joules for the timestep being reported.

#### Refrigeration Chiller Compressor Run Time  Fraction []

This is the fraction of the time step when the compressor ran to meet the load.  It is a value between 0.0 and 1.0.

## Refrigeration:CompressorList

This object provides a list of all the compressors included within a single refrigeration system (Ref: [Refrigeration:System](#refrigerationsystem)). Each list must contain at least one compressor. The order in which the individual compressors are listed here will be the order in which the compressors are dispatched to meet the system load.  That is, at very low loads, only compressor #1 will be operating.  As the load increases, the first two compressors will be operating, and so on. This list is extensible.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a compressor list. Any reference to this compressor list by another object will use this name. The compressor list cannot have the same name as any individual compressor.

#### Field: Refrigeration Compressor <x> Name

Identifies a particular compressor that works in conjunction with the other compressors on this list to provides cooling to a single refrigeration system. The name will be validated against the compressor names (ref: [Refrigeration:Compressor](#refrigerationcompressor)) in the input file. Up to 20 are available in the IDD; this object is extensible.

The following is an example input for a compressor list.

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:CompressorList,
        MediumTempCompressorlist, !- Refrigeration Compressor List Name
        CompressorA,             !- Refrigeration Compressor Name #1
        CompressorB,             !- Refrigeration Compressor Name #2
        CompressorC;             !- Refrigeration Compressor Name #3
~~~~~~~~~~~~~~~~~~~~

## Refrigeration:Subcooler

Two types of subcoolers are modeled by the detailed refrigeration system.  As described in the Engineering Reference, the liquid suction heat exchanger uses cool suction gas to subcool the hot condensate after it leaves the condenser and before it reaches the thermal expansion valve.  For the liquid suction heat exchanger, both the source and sink of energy are located within the same refrigeration system.  In contrast, a mechanical subcooler is used to transfer cooling capacity from one refrigeration system to another. The mechanical subcooler is listed as a part of the system for which the condensate is cooled.  However, the input data for the mechanical subcooler includes a field that identifies the system that provides the cooling capacity.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a subcooler. Any reference to this subcooler by another object will use this name.

#### Field: Subcooler Type

The type of subcooler. Valid choices are Mechanical and LiquidSuction.  If the field is blank, the default will be LiquidSuction.

#### Field: Liquid Suction Design Subcooling Temperature Difference

This numeric field is the design subcooling temperature difference (DeltaC) for a liquid suction heat exchanger and should be blank for a mechanical subcooler.

#### Field: Design Liquid Inlet Temperature

This numeric field is the design inlet temperature (C) for the hot liquid entering a liquid suction heat exchanger and should be blank for a mechanical subcooler.

#### Field: Design Vapor Inlet Temperature

This numeric field is the design inlet temperature (C) for the cool vapor entering a liquid suction heat exchanger and should be blank for a mechanical subcooler.

#### Field: Capacity-Providing System

This field is the name of the refrigeration system object that provides the cooling capacity for the mechanical subcooler (ref: [Refrigeration:System](#refrigerationsystem) Name). This field should be blank for a liquid suction heat exchanger.

#### Field: Outlet Control Temperature

This numeric field is the controlled outlet temperature (C) for subcooled liquid exiting a mechanical subcooler. This field should be blank for a liquid suction heat exchanger.

The following is example input for both liquid suction and mechanical subcoolers.

~~~~~~~~~~~~~~~~~~~~

      REFRIGERATION:SUBCOOLER,
    SampleLSHx,!Subcooler Name
    LiquidSuction,!subcooler type
    6,!design liquid suction subcooling {deltaC}
    16,!design inlet temperature on liquid side {C}
    0,!design inlet temperature on vapor side {C}
    ,!Refrigeration System Detailed Name providing cooling capacity
    ;!Control Temperature Out for subcooled liquid {C}

    ! Mechanical Subcooler (uses Med Temp System to cool low temp liquid to 10C)
      REFRIGERATION:SUBCOOLER,
    SampleMSC,!Subcooler Name
    Mechanical,!subcooler type
    ,!design liquid suction subcooling {C}
    ,!design inlet temperature on liquid side {C}
    ,!design inlet temperature on vapor side {C}
    MediumTempSystem,!Refrigeration System Detailed Name providing cooling capacity
    10;!Control Temperature Out for subcooled liquid {C}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    FOR SUBCOOLERS ON SYSTEMS SERVING CASES AND/OR WALKINS:
    Zone,Average, Refrigeration System Mechanical Subcooler Heat Transfer Rate [W]
    Zone,Sum, Refrigeration System Mechanical Subcooler Heat Transfer Energy [J]
    FOR SUBCOOLERS ON SYSTEMS SERVING AIR CHILLERS:
    Zone,Average, Refrigeration Air Chiller System Mechanical Subcooler Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Air Chiller System Mechanical Subcooler Heat Transfer Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### FOR SUBCOOLERS ON SYSTEMS SERVING CASES AND/OR WALKINS:

#### Refrigeration System Mechanical Subcooler Heat Transfer Rate [W]

This output is the cooling energy transferred from one system's compressor group to the refrigerant leaving the condenser for another refrigeration system in Watts.

#### Refrigeration System Mechanical Subcooler Heat Transfer Energy [J]

This output is the cooling energy transferred from one system's compressor group to the refrigerant leaving the condenser for another refrigeration system in Joules.

#### FOR SUBCOOLERS ON SYSTEMS SERVING AIR CHILLERS

#### Refrigeration Air Chiller System Mechanical Subcooler Heat Transfer Rate [W]

This output is the cooling energy transferred from one system's compressor group to the refrigerant leaving the condenser for another refrigeration system in Watts.

#### Refrigeration Air Chiller System Mechanical Subcooler Heat Transfer Energy [J]

This output is the cooling energy transferred from one system's compressor group to the refrigerant leaving the condenser for another refrigeration system in Joules.

## Refrigeration:Condenser:AirCooled 

The refrigeration system requires a single condenser to reject the system heat.  The air cooled condenser object is one of four options for this condenser. In an air-cooled condenser, the total heat of rejection is characterized by a linear relationship between the difference between the inlet air (usually the outside drybulb) and condensing temperatures. This curve is commonly found in the condenser manufacturers' literature. This rating curve, which corresponds to ARI 460 standards, also has an associated rated value for subcooling that should be entered. The rated condenser fan power and fan speed control type must be specified. If the condenser is not at ground level, the user may specify an air inlet node name (ref: [OutdoorAir:Node](#outdoorairnode) name). If the condenser is located inside a conditioned zone, the zone name may be specified(ref: bbbZoneNamebbb). Optional input is provided to help the user compare refrigerant inventories for different systems.  These inputs represent the design values for the refrigerant inventory within the condenser, within a receiver beneath the condenser, and in the liquid pipes between the condenser and the refrigerated cases.

### Inputs

#### Field: Name

Identifies a particular condenser that rejects heat for a one or more refrigeration system(s). The name must be unique and will be validated against all the condenser names (ref: Refrigeration:Condenser:\*) in the input file, including names used for evaporative-cooled, water-cooled, and cascade-cooled condensers.

#### Field: Rated Effective Total Heat Rejection Rate Curve Name

This field is the name of a curve object defining the condenser heat rejection as a function of the difference between the condensing and entering air temperatures. The curve should be linear ([Curve:Linear](#curvelinear)). See the Engineering Reference for more discussion on the curve coefficients.

#### Field: Rated Subcooling Temperature Difference

This numeric field specifies the rated subcooling (DeltaC) specified by the manufacturer, consistent with the heat rejection curve rating data.

#### Field: Condenser Fan Speed Control Type

The type of fan speed control used by the condenser fan. Valid choices are **Fixed**, **FixedLinear,** **VariableSpeed**, and **TwoSpeed**.  If the field is blank, **Fixed** will be used. See the Engineering Reference for a discussion of this option's effect on fan energy consumption.

#### Field: Rated Fan Power

This numeric field specifies the rated fan power (W) specified by the manufacturer, under standard rating conditions.

#### Field: Minimum Fan Air Flow Ratio

**Fan controls often include a minimum air flow ratio to avoid overheating the fan motor or for other reasons. This numeric field** should be between 0. and 1. and has a default value of 0.2**.**

#### Field: Air Inlet Node Name or Zone Name

This optional alpha field contains the name of the node from which the condenser draws its outdoor air or the name of the conditioned zone where the condenser is located. If this field is left blank, the outdoor air drybulb temperature entering the condenser is taken directly from the weather data. If this field is not blank and an outdoor air node name is entered, this node name must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air temperature from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air temperature is taken directly from the weather data. If a zone name is entered, ….bbb

#### Field: End-Use Subcategory

This field allows the specification of a user-defined end-use subcategory (e.g., "Low Temperature Rack"). A new meter for reporting is created for each unique subcategory (Ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is left blank, the equipment will be assigned to the "General" end-use subcategory.

#### Field: Condenser Refrigerant Operating Charge Inventory

This numeric field specifies the amount of refrigerant present within the condenser (kg) specified by the manufacturer, under standard rating conditions.

#### Field: Condensate Receiver Refrigerant Inventory

This numeric field specifies the amount of refrigerant present within the condensate receiver (kg) specified by the manufacturer or system designer, under standard rating conditions.

#### Field: Condensate Piping Refrigerant Inventory

This numeric field specifies the amount of refrigerant present within the condensate piping (kg) specified by the system designer, under standard rating conditions.

The following is an example input for an air-cooled condenser.

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:Condenser:AirCooled,
        MediumTempCondenser,     !- Name of condenser
        MediumTempCondenserHRCurve, !-name of condenser heat of rejection curve, linear
        0.,                      ! Rated subcooling {deltaC}
        VariableSpeed,           ! Fan speed control type
        4000.,                   ! Design condenser fan power {W}
        0.25,                    !Minimum air flow fraction through condenser fan {dimensionless}
        Refrigeration MediumTempSystem CondenserNode,  !- Condenser Air Inlet Node Name or Zone Name
        MediumTempRefrig;        !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    FOR CONDENSERS COOLING SYSTEMS SERVING CASES AND/OR WALKINS:
    Zone,Average, Refrigeration System Condenser Fan Electric Power [W]
    Zone, Sum, Refrigeration System Condenser Fan Electric Energy [J]
    Zone,Average, Refrigeration System Condenser Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Condenser Total Recovered Heat Transfer Rate [W] Zone, Sum, Refrigeration System Condenser Total Recovered Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Heat Recovered for Non-Refrigeration Purposes Energy [J]
    Zone,Average, Refrigeration System Condenser Defrost Recovered Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Defrost Recovered Heat Transfer Energy [J]
    FOR CONDENSERS COOLING SYSTEMS SERVING AIR CHILLERS:
    HVAC,Average, Refrigeration System Condenser Fan Electric Power [W]
    HVAC, Sum, Refrigeration System Condenser Fan Electric Consumption [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Rate [W] HVAC, Sum, Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### FOR CONDENSERS ON SYSTEMS SERVING CASES AND/OR WALKINS:

#### Refrigeration System Condenser Fan Electric Power [W]

This output is the electric input to the system's condenser fan(s) in Watts.

#### Refrigeration System Condenser Fan Electric Consumption [J]

This is the electricity consumption of the system's condenser fan(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration System Condenser Heat Transfer Rate [W]

This is the total heat transfer across the condenser (i.e., compressor energy and refrigeration load minus any heat recovered for defrost or other purposes).

#### Refrigeration System Condenser Heat Transfer Energy [J]

This is the total heat energy flowing across the condenser for the timestep being reported  (i.e., compressor energy and refrigeration load minus any heat recovered for defrost or other purposes).

#### Refrigeration System Condenser Total Recovered Heat Transfer Rate [W]

This is the total heat recovered from the condenser inlet flow for all purposes including defrost and water or air heating.

#### Refrigeration System Condenser Total Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser inlet flow for all purposes including defrost and water or air heating for the timestep being reported.

#### Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Rate  [W]

This is the total heat recovered from the condenser inlet flow for purposes such as water or air heating.

#### Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser inlet flow for purposes such as water or air heating for the timestep being reported.

#### Refrigeration System Condenser Defrost Recovered Heat Transfer Rate [W]

This is the total heat recovered from the condenser inlet flow for defrost purposes within the refrigeration system.

#### Refrigeration System Condenser Defrost Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser inlet flow for defrost purposes within the refrigeration system for the timestep being reported.

#### FOR CONDENSERS ON SYSTEMS SERVING AIR CHILLERS:

#### Refrigeration Air Chiller System Condenser Fan Electric Power [W]

This output is the electric input to the system's condenser fan(s) in Watts.

#### Refrigeration Air Chiller System Condenser Fan Electric Energy [J]

This is the electricity consumption of the system's condenser fan(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller System Condenser Heat Transfer Rate [W]

This is the total heat transfer across the condenser (i.e., compressor energy and refrigeration load minus any heat recovered for defrost or other purposes).

#### Refrigeration Air Chiller System Condenser Heat Transfer Energy [J]

This is the total heat energy flowing across the condenser for the timestep being reported  (i.e., compressor energy and refrigeration load minus any heat recovered for defrost or other purposes).

#### Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Rate [W]

This is the total heat recovered from the condenser inlet flow for all purposes including defrost and water or air heating.

#### Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser inlet flow for all purposes including defrost and water or air heating for the timestep being reported.

#### Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Rate  [W]

This is the total heat recovered from the condenser inlet flow for purposes such as water or air heating.

#### Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser inlet flow for purposes such as water or air heating for the timestep being reported.

#### Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Rate [W]

This is the total heat recovered from the condenser inlet flow for defrost purposes within the refrigeration system.

#### Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser inlet flow for defrost purposes within the refrigeration system for the timestep being reported.

## Refrigeration:Condenser:EvaporativeCooled

Each refrigeration system requires a single condenser.  The evaporative cooled condenser object is one of four options for this condenser. In an evaporative-cooled condenser, the total heat of rejection is characterized by a four-factor relationship between the condensing temperature and the difference between the outside wetbulb and condensing temperatures. This curve must be developed by regression from the condenser manufacturers' literature. This rating curve, which is based upon rated data taken according to ARI 460 standards, also has an associated rated value for subcooling that should be entered. The rated condenser fan power and fan speed control type must be specified. If the condenser is not at ground level, the user may specify an air inlet node name (ref: [OutdoorAir:Node](#outdoorairnode) name). Optional input is provided to help the user compare refrigerant inventories for different systems.  These input represent the design values for the refrigerant inventory within the condenser, within a receiver beneath the condenser, and in the liquid pipes between the condenser and the refrigerated cases.

Additional inputs are needed for basin heater parameters, water pump power, and water source. An evaporative cooling availability schedule is available.

### Inputs

#### Field: Name

Identifies a particular condenser that rejects heat for a one or more refrigeration system(s). The name must be unique and will be validated against all the condenser names (ref: Refrigeration:Condenser:\*) in the input file, including names used for air-cooled, water-cooled and cascade-cooled condensers.

#### Field: Rated Effective Total Heat Rejection Rate

This numeric field should be the rated heat rejection effect (W) at standard rating conditions per ARI 490.  Be sure the rating corresponds to the correct refrigerant.

#### Field: Rated Subcooling Temperature Difference

This numeric field specifies the rated subcooling (DeltaC) specified by the manufacturer, consistent with the rated value for total heat rejection.

#### Field: Fan Speed Control Type

The type of fan speed control used by the condenser fan. Valid choices are **Fixed**, **FixedLinear**, **VariableSpeed**, and **TwoSpeed**.  If the field is blank, **Fixed** will be used. See the Engineering Reference for a discussion of this option's effect on fan energy consumption.

#### Field: Rated Fan Power

This numeric field specifies the rated fan power (W) specified by the manufacturer, under standard rating conditions.

#### Field: Minimum Fan Air Flow Ratio

Fan controls often include a minimum air flow ratio to avoid overheating the fan motor or for other reasons. This numeric field has a minimum value of 0. and a default value of 0.2.

#### Field: Approach Temperature Constant Term

As described in the Engineering Reference (ref Refrigeration:Condenser:Evaporative), the heat rejection capacity factor is specified according to the form (where Twetbulb and Tcondense are in C):

Tcondense=A1 + A2(hrcf) + A3/(hrcf) + (1 + A4)(Twb)

This numeric field is the value for A1 and has a default value of 6.63, a minimum of 0. and a maximum of 20. (C)

#### Field: Approach Temperature Coefficient 2

As described in the Engineering Reference (ref Refrigeration:Condenser:Evaporative), the heat rejection capacity factor is specified according to the form (where Twetbulb and Tcondense are in C):

Tcondense=A1 + A2(hrcf) + A3/(hrcf) + (1 + A4)(Twb)

This numeric field is the value for A2 and has a default value of 0.468, a minimum of 0, and a maximum of 20 (C).

#### Field: Approach Temperature Coefficient 3

As described in the Engineering Reference (ref Refrigeration:Condenser:Evaporative), the heat rejection capacity factor is specified according to the form (where Twetbulb and Tcondense are in C):

Tcondense =A1 + A2(hrcf) + A3/(hrcf) +(1 + A4)(Twb)

This numeric field is the value for A3 and has a default value of 17.93, a minimum of 0, and a maximum of 30 (C).

#### Field: Approach Temperature Coefficient 4

As described in the Engineering Reference (ref Refrigeration:Condenser:Evaporative), the heat rejection capacity factor is specified according to the form (where Twetbulb and Tcondense are in C):

Tcondense =A1 + A2(hrcf) + A3/(hrcf) + (1 + A4)(Twb)

This numeric field is the value for A4 and has a default value of -0.322, a minimum of -20., and a maximum of 20 (dimensionless).

#### Field: Minimum Capacity Factor

This numeric field is the minimum heat rejection capacity factor in the manufacturer's data used to develop the equation described in the preceding four fields. The default value is 0.5.

#### Field: Maximum Capacity Factor

This numeric field is the maximum heat rejection capacity factor in the manufacturer's data used to develop the equation described in the preceding four fields. The default value is 5.0.

#### Field: Air Inlet Node Name

This optional alpha field contains the name of the node from which the condenser draws its outdoor air. If this field is left blank, the outdoor air drybulb temperature entering the condenser is taken directly from the weather data. If this field is not blank, this node name must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air temperature from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air temperature is taken directly from the weather data.

#### Field: Rated Air Flow Rate

The air volume flow rate, in m^3^ per second, entering the evaporative condenser. This value is used to calculate the amount of water evaporated when evaporatively cooling the condenser inlet air. The value for this field must be greater than zero. This input field is also autocalculatable, equivalent to 0.000144 m^3^/s per watt of total cooling capacity [850 cfm/ton] where the total cooling capacity is the total heat of rejection.

#### Field: Basin Heater Capacity

This field defines the power level of the basin heater, if applicable, used to avoid water freezing in an outdoor evaporative cooler basin. This numeric field contains the capacity of the electric basin heater in Watts per degree C. This field is used in conjunction with the Basin Heater Set Point Temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the dry-bulb temperature of the condenser coil inlet air. The basin heater only operates when the condenser fan is off (i.e., no compressor heat rejection). The basin heater capacity must be greater than or equal to zero, with a default value of 200 W/C if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

This numeric field contains the set point temperature (°C) for the basin heater described in the previous field. The basin heater is active when the outdoor air dry-bulb temperature falls below this set point temperature, as long as the condenser fan is off. The default value is 2°C if this field is left blank.

#### Field: Rated Water Pump Power

The rated power of the evaporative condenser water pump in Watts. This value is used to calculate the power required to pump the water used to evaporatively cool the condenser inlet air. The value for this field must be greater than or equal to 0, with a default value of 1000 Watts if this field is left blank. This input field is also autocalculatable, equivalent to 0.004266 W per Watt [15 W/ton] of total cooling capacity.

#### Field: Evaporative Water Supply Tank Name

This field is used to define where the condenser obtains water used for evaporative cooling. If this field is left blank, the unit will obtain water directly from the mains (Ref. Water Mains Temperatures). If the name of a Water Storage Tank object is used here, then the unit will obtain its water from that tank.

#### Field: Evaporative Condenser Availability Schedule Name

For evaporative cooled condensers, the name of the optional schedule (Ref: Schedule) that specifies the time periods that evaporative cooling is available/unavailable. In some colder climates, evaporative cooling is periodically discontinued and the basin sumps drained to avoid freezing and to avoid ice formation on the condenser. In these times, the condenser runs as a typical dry air cooled condenser, and related evaporative cooling systems (e.g., water pump, basin heaters) do not operate. Use of this optional schedule permits modeling of planned, seasonal interruptions of evaporative cooling. All schedule values must be greater than or equal to zero. Typically, an ON/OFF schedule type is used with values being either 0 or 1. A schedule value of 1 indicates that evaporative cooling is available during the defined time period, and a value of 0 denotes that evaporative cooling is not available during the defined time period. If the schedule name is omitted (blank), then the model assumes that evaporative cooling of the condenser is available for the entire simulation period. Note: the use of this schedule is the correct way to model an evaporative condenser in a climate subject to freezing weather.  However, because some users will take a single model description and run it for a multiple climates, the code also includes an automatic switch from wet to dry operation, as described in the Engineering Reference.

#### Field: End-Use Subcategory

This field allows the specification of a user-defined end-use subcategory (e.g., "Low Temperature Rack"). A new meter for reporting is created for each unique subcategory (Ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is left blank, the equipment will be assigned to the "General" end-use subcategory.

#### Field: Condenser Refrigerant Operating Charge Inventory

This numeric field specifies the amount of refrigerant present within the condenser (kg) specified by the manufacturer, under standard rating conditions.

#### Field: Condensate Receiver Refrigerant Inventory

This numeric field specifies the amount of refrigerant present within the condensate receiver (kg) specified by the manufacturer or system designer, under standard rating conditions.

#### Field: Condensate Piping Refrigerant Inventory

This numeric field specifies the amount of refrigerant present within the condensate piping (kg) specified by the system designer, under standard rating conditions.

The following is an example input for an evaporative condenser

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:Condenser:EvaporativeCooled,
        MedTempEvapCooledCondenser ,  !- Refrigeration Condenser Name
        64800. ,                    !- Rated Total Heat Rejection Effect, {W}
        0. ,                        !- Rated Subcooling
        VariableSpeed ,             !- Condenser Fan Speed Control
        746.,                       !- Rated Condenser Fan Power
        0.25,                       !- Minimum air flow fraction through condenser fan {dimensionless}
        6.63  ,                     !- Evaporative Condenser Approach Temp Const, {C}
        0.468 ,                     !- Evaporative Condenser Approach Temp HRCF Cooefficient
        17.93 ,                     !- Evaporative Condenser Approach Temp 1/hrcf coefficient
        -0.322,                     !- Evaporative Condenser Approach Temp Twb coefficient {1/C}
        0.6  ,                      !- Minimum Condenser Capacity Factor
        4.8  ,                      !- Maximum Condenser Capacity Factor
        Outside Air Inlet Node,     !- Condenser Air Inlet Node Name
        1.79 ,                      !- Rated Air Flow Rate {m3/s}
        66.7 ,                      !- Basin Heater Capacity {W/C}
        1.0  ,                      !- Basin Heater Set Point Temperature {C}
        250. ,                      !- Rated Water Pump Power {W}
        EvapWaterStorageTank,       !- Evaporative Water Supply Tank Name
        EvapCondAvail,              !- Evaporative Condenser Availability Schedule Name
        ,                           !- End-UseSubcategory
        21.9 ,                      !- Condenser Refrigerant Operating Charge {kg}
        10.  ,                      !- Condensate Receiver Refrigerant Inventory
        25. ;                       !- Condensate Piping Refrigerant Inventory
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    Zone,Average, Refrigeration System Condenser Fan Electric Power [W]
    Zone, Sum, Refrigeration System Condenser Fan Electric Consumption [J]
    FOR CONDENSERS COOLING SYSTEMS SERVING CASES AND/OR WALKINS:
    Zone,Average, Refrigeration System Condenser Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Condenser Total Recovered Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Total Recovered Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Condenser Defrost Recovered Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Defrost Recovered Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Condenser Pump Electric Power [W]
    Zone, Sum, Refrigeration System Condenser Pump Electric Energy [J]
    Zone,Average, Refrigeration System Condenser Basin Heater Electric Power [W]
    Zone, Sum, Refrigeration System Condenser Basin Heater Electric Energy [J]
    Zone,Average, Refrigeration System Condenser Evaporated Water Volume Flow Rate  [m3/s]
    Zone, Sum, Refrigeration System Condenser Evaporated Water Volume [m3]
    FOR CONDENSERS COOLING SYSTEMS SERVING AIR CHILLERS:
    HVAC,Average, Refrigeration Air Chiller System Condenser Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Pump Electric Power [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Pump Electric Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Basin Heater Electric Power [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Basin Heater Electric Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Evaporated Water Volume Flow Rate [m3/s]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Evaporated Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### FOR CONDENSERS ON SYSTEMS SERVING CASES AND/OR WALKINS:

#### Refrigeration System Condenser Fan Electric Power [W]

This output is the electric input to the system's condenser fan(s) in Watts.

#### Refrigeration System Condenser Fan Electric Energy [J]

This is the electricity consumption of the system's condenser fan(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration System Condenser Heat Transfer Rate [W]

This is the total heat transfer across the condenser (i.e., compressor energy and refrigeration loads) in Watts.

#### Refrigeration System Condenser Heat Transfer Energy [J]

This is the total heat energy flowing across the condenser for the timestep being reported.

#### Refrigeration System Condenser Total Recovered Heat Transfer Rate  [W]

This is the total heat recovered from the condenser refrigerant inlet flow for any purpose.

#### Refrigeration System Condenser Total Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser refrigerant inlet flow for any purpose for the timestep being reported.

#### Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Rate  [W]

This is the total heat recovered from the condenser refrigerant inlet flow for purposes such as water or air heating.

#### Refrigeration System Condenser Heat Recovered for Non-Refrigeration Purposes Energy [J]

This is the total heat recovered from the condenser refrigerant inlet flow for purposes such as water or air heating for the timestep being reported.

#### Refrigeration System Condenser Defrost Recovered Heat Transfer Rate [W]

This is the total heat recovered from the condenser refrigerant inlet flow for defrost purposes within the refrigeration system..

#### Refrigeration System Condenser Defrost Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser refrigerant inlet flow for defrost purposes within the refrigeration system for the timestep being reported.

#### Refrigeration System Condenser Pump Electric Power [W]

This is the electrical power requirement in Watts for the timestep being reported for the water pump used with evaporative cooling of the condenser.

#### Refrigeration System Condenser Pump Electric Energy [J]

This is the electricity consumption in Joules of the water pump used with evaporative cooling of the condenser for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration System Condenser Basin Heater Electric Power [W]

This is the electrical power requirement in Watts for the timestep being reported for the water heater in the basin of the evaporative system used to cool the condenser.

#### Refrigeration System Condenser Basin Heater Electric Energy [J]

This is the electricity consumption in Joules of the water heater used to prevent freezing of the evaporative cooling system for the condenser for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration System Condenser Evaporated Water Volume Flow Rate [m^3^/s]

The volumetric flow rate in m^3^/s of water consumed while providing evaporative cooling of the condenser.

#### Refrigeration System Condenser Evaporated Water Volume [m^3^]

This is the water consumed by evaporation in m^3^ while providing evaporative cooling of the condenser for the timestep being reported. This output is also added to a meter with Resource Type = Water, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### FOR CONDENSERS ON SYSTEMS SERVING AIR CHILLERS:

#### Refrigeration Air Chiller System Condenser Fan Electric Power [W]

This output is the electric input to the system's condenser fan(s) in Watts.

#### Refrigeration Air Chiller System Condenser Fan Electric Energy [J]

This is the electricity consumption of the system's condenser fan(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller System Condenser Heat Transfer Rate [W]

This is the total heat transfer across the condenser (i.e., compressor energy and refrigeration loads) in Watts.

#### Refrigeration Air Chiller System Condenser Heat Transfer Energy [J]

This is the total heat energy flowing across the condenser for the timestep being reported.

#### Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Rate  [W]

This is the total heat recovered from the condenser refrigerant inlet flow for any purpose.

#### Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser refrigerant inlet flow for any purpose for the timestep being reported.

#### Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Rate  [W]

This is the total heat recovered from the condenser refrigerant inlet flow for purposes such as water or air heating.

#### Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser refrigerant inlet flow for purposes such as water or air heating for the timestep being reported.

#### Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Rate [W]

This is the total heat recovered from the condenser refrigerant inlet flow for defrost purposes within the refrigeration system.

#### Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser refrigerant inlet flow for defrost purposes within the refrigeration system for the timestep being reported.

#### Refrigeration Air Chiller System Condenser Pump Electric Power [W]

This is the electrical power requirement in Watts for the timestep being reported for the water pump used with evaporative cooling of the condenser.

#### Refrigeration Air Chiller System Condenser Pump Electric Energy [J]

This is the electricity consumption in Joules of the water pump used with evaporative cooling of the condenser for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller System Condenser Basin Heater Electric Power [W]

This is the electrical power requirement in Watts for the timestep being reported for the water heater in the basin of the evaporative system used to cool the condenser.

#### Refrigeration Air Chiller System Condenser Basin Heater Electric Energy [J]

This is the electricity consumption in Joules of the water heater used to prevent freezing of the evaporative cooling system for the condenser for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller System Condenser Evaporated Water Volume Flow Rate [m3/s]

The volumetric flow rate in m^3^/s of water consumed while providing evaporative cooling of the condenser.

#### Refrigeration Air Chiller System Condenser Evaporated Water Volume [m3]

This is the water consumed by evaporation in m^3^ while providing evaporative cooling of the condenser for the timestep being reported. This output is also added to a meter with Resource Type = Water, End Use Key = Refrigeration, Group Key = Plant. Use of an optional subkey category is also available, with default to the General end-use subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

## Refrigeration:Condenser:WaterCooled 

The refrigeration system requires a single condenser.  The water cooled condenser object is one of four options for this condenser. Manufacturer's data is required for the rated total heat rejection, the rated condensing temperature, and the rated leaving liquid refrigerant temperature, all per ARI 450.  These values are used to calculate the subcooling included in the rated capacity. The inlet and outlet node names as well as the water outlet temperature schedule name are required.  Optional input is provided to help the user compare refrigerant inventories for different systems.  These input represent the design values for the refrigerant inventory within the condenser, within a receiver beneath the condenser, and in the liquid pipes between the condenser and the refrigerated cases.

### Inputs

#### Field: Name

Identifies a particular condenser that rejects heat for one or more refrigeration system(s). The name must be unique and will be validated against all the condenser names (ref: Refrigeration:Condenser) in the input file, including names used for air-cooled, evaporative-cooled, and cascade-cooled condensers.

#### Field: Rated Effective Total Heat Rejection Rate

This numeric field should be the rated heat rejection effect (W) at standard rating conditions per ARI 450.  Be sure the rating corresponds to the correct refrigerant.

#### Field: Rated Condensing Temperature

This numeric field provides the rated condensing temperature (C) corresponding to the rated heat rejection capacity.

#### Field: Rated Subcooling Temperature Difference

This numeric field provides the rated liquid refrigerant subcooling (DeltaC) corresponding to the rated heat rejection capacity. Note this is equal to the difference between the saturated condensing temperature and the leaving liquid refrigerant temperature.

#### Field: Rated Water Inlet Temperature

This numeric field provides the rated water inlet temperature (C) corresponding to the rated heat rejection capacity at the rated condensing temperature.

#### Field: Water Inlet Node Name

A node name for the water-side condenser inlet must be provided.

#### Field: Water Outlet Node Name

A node name for the water-side condenser outlet must be provided.

#### Field: Water-Cooled Loop Flow Type

The type of flow loop should be specified.  The two choices are **VariableFlow**, in which a [Pump:VariableSpeed](#pumpvariablespeed) must be included in the plant loop, or **ConstantFlow**, in which the loop circuit has a constant flow rate, typically associated with a Pump:ConstandSpeed object.  If the flow type is VariableFlow, the flow needed to remove the condenser heat energy will be calculated and requested of the pump.  If the flow type is ConstantFlow, the outlet water temperature will be determined based on the fixed loop flow rate and heat energy to be removed.  The default type is VariableFlow. Refer to additional discussion in the Engineering Reference.

#### Field: Water Outlet Temperature Schedule Name

When the water-cooled loop flow type is "VariableFlow", the name of a schedule (Ref: Schedule) that defines the desired condenser water outlet temperature must be provided.  The schedule may define an outlet temperature that varies through time.

#### Field: Water Design Flow Rate

When the water-cooled loop flow type is "ConstantFlow", this is the design water flow rate in m^3^/s that will be requested initially.  This requested flow will be passed to the loop simulation, and resulting actual flow will be dependent upon supply system capabilities (e.g., pump capability).  The design flow rate must always be less than the maximum flow rate, defined below.

#### Field: Water Maximum Flow Rate

This numeric field is the maximum water flow rate in m^3^/s that will be allowed through the condenser.  When the loop flow type is Variable Flow, if the calculated flow rate is higher than the maximum flow rate, an error message will be generated, and the flow rate will be reset to the maximum flow rate.

#### Field: Maximum Water Outlet Temperature

This numeric field specifies the maximum allowed water temperature in degrees C leaving the condenser. The default value is 55 degrees C.

#### Field: Minimum Water Inlet Temperature

This numeric field specifies the minimum allowed water temperature in degrees C entering the compressor rack condenser. The default value is 10 degrees C.  Refer to additional discussion in the Engineering Reference.

#### Field: End-Use Subcategory

This field allows the specification of a user-defined end-use subcategory (e.g., "Low Temperature System"). A new meter for reporting is created for each unique subcategory (Ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is left blank, the equipment will be assigned to the "General" end-use subcategory.

#### Field: Condenser Refrigerant Operating Charge Inventory

This numeric field specifies the amount of refrigerant present within the condenser (kg) specified by the manufacturer, under standard rating conditions.

#### Field: Condensate Receiver Refrigerant Inventory

This numeric field specifies the amount of refrigerant present within the condensate receiver (kg) specified by the manufacturer or system designer, under standard rating conditions.

#### Field: Condensate Piping Refrigerant Inventory

This numeric field specifies the amount of refrigerant present within the condensate piping (kg) specified by the system designer, under standard rating conditions.

The following is an example input for a water cooled condenser.

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:Condenser:WaterCooled,
        MediumTempCondenser ,    !- Name
        58000. ,                 !- Rated Effective Total Heat Rejection Rate {W}
        29.4 ,                   !- Rated Condensing Temperature {C}
        0.0 ,                    !- Rated Subcooling Temperature Difference{deltaC}
        10.0,                    !- Rated Water Inlet Temperature {C}
        Condenser Inlet Node,    !- Water Inlet Node Name
        Condenser Outlet Node,   !- Water- Outlet Node Name
        Variable Flow,           !- Water-cooled Loop Flow Type
        Cond Outlet Temp Sch,    !- Water Outlet Temperature Schedule Name
        ,                        !- Water Design Flow Rate {m3/s}
        0.003,                   !- Water Maximum Flow Rate {m3/s}
        55.,                     !- Maximum Water Outlet Temperature {C}
        ,                        !- Minimum Water Inlet Temperature {C}
        ,                        !- End-Use Subcategory
        30. ,                    !- Condenser Refrigerant Operating Charge {kg}
        65. ,                    !- Condensate Receiver Refrigerant Inventory {kg}
        20. ;                    !- Condensate Piping Refrigerant Inventory {kg}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    FOR CONDENSERS COOLING SYSTEMS SERVING CASES AND/OR WALKINS:
    Zone,Average, Refrigeration System Condenser Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Condenser Total Recovered Heat Transfer Rate [W] Zone, Sum, Refrigeration System Condenser Total Recovered Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Heat Recovered for Non-Refrigeration Purposes Energy [J]
    Zone,Average, Refrigeration System Condenser Defrost Recovered Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Defrost Recovered Heat Transfer Energy [J]
    Zone,Average, Refrigeration System Condenser Water Mass Flow Rate [kg/s]
    FOR CONDENSERS COOLING SYSTEMS SERVING AIR CHILLERS:
    HVAC,Average, Refrigeration Air Chiller System Condenser Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Rate [W] HVAC, Sum, Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Energy [J]
    HVAC,Average, Refrigeration Air Chiller System Condenser Fluid Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### FOR CONDENSERS ON SYSTEMS SERVING CASES AND/OR WALKINS:

#### Refrigeration System Condenser Heat Transfer Rate [W]

This is the total heat transfer across the condenser (i.e., compressor energy and refrigeration loads) in Watts.

#### Refrigeration System Condenser Heat Transfer Energy [J]

This is the total heat energy flowing across the condenser for the timestep being reported.

#### Refrigeration System Condenser Total Recovered Heat Transfer Rate  [W]

This is the total heat recovered from the condenser refrigerant inlet flow for any purpose.

#### Refrigeration System Condenser Total Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser refrigerant inlet flow for any purpose for the timestep being reported.

#### Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]

This is the total heat recovered from the condenser inlet flow for purposes such as water or air heating.

#### Refrigeration System Condenser Heat Recovered for Non-Refrigeration Purposes Energy  [J]

This is the total heat recovered from the condenser inlet flow for purposes such as water or air heating for the timestep being reported.

#### Refrigeration System Condenser Defrost Recovered Heat Transfer Rate [W]

This is the total heat recovered from the condenser inlet flow for defrost purposes within the refrigeration system..

#### Refrigeration System Condenser Heat Energy Recovered for Refrigeration Defrost Energy [J]

This is the total heat recovered from the condenser inlet flow for defrost purposes within the refrigeration system for the timestep being reported.

#### Refrigeration System Condenser Water Mass Flow Rate [kg/s]

This is the mass flow rate of the water used to cool the condenser in kg/s.

#### FOR CONDENSERS ON SYSTEMS SERVING AIR CHILLERS:

#### Refrigeration Air Chiller System Condenser Heat Transfer Rate [W]

This is the total heat transfer across the condenser (i.e., compressor energy and refrigeration loads) in Watts.

#### Refrigeration Air Chiller System Condenser Heat Transfer Energy [J]

This is the total heat energy flowing across the condenser for the timestep being reported.

#### Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Rate  [W]

This is the total heat recovered from the condenser refrigerant inlet flow for any purpose.

#### Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Energy [J]

This is the total heat recovered from the condenser refrigerant inlet flow for any purpose for the timestep being reported.

#### Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]

This is the total heat recovered from the condenser inlet flow for purposes such as water or air heating.

#### Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Energy  [J]

This is the total heat recovered from the condenser inlet flow for purposes such as water or air heating for the timestep being reported.

#### Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Rate [W]

This is the total heat recovered from the condenser inlet flow for defrost purposes within the refrigeration system.

#### Refrigeration Chiller System Condenser Heat Energy Recovered for Refrigeration Defrost Energy [J]

This is the total heat recovered from the condenser inlet flow for defrost purposes within the refrigeration system for the timestep being reported.

#### Refrigeration Air Chiller System Condenser Fluid Mass Flow Rate [kg/s]

This is the mass flow rate of the water used to cool the condenser in kg/s.

## Refrigeration:Condenser:Cascade

The refrigeration system requires a single condenser.  The cascade condenser object is one of four options for this condenser. The cascade condenser is unlike the other condenser options because it rejects heat to another, higher-temperature, refrigeration system. That is, the cascade condenser acts as a heat rejection object for one system, but acts as a refrigeration load for another system.  Therefore, this object will be referenced twice: first for the system rejecting heat (see the field Refrigeration Condenser Name for the object [Refrigeration:System](#refrigerationsystem)), and second for the system absorbing the heat (see the input for the field Refrigeration Transfer Load or TransferLoad List Name for the object [Refrigeration:System](#refrigerationsystem) or the field Cascade Condenser Name or Secondary System Name for the object [Refrigeration:TransferLoadList](#refrigerationtransferloadlist)).

Manufacturer's data is required for the rated condensing temperature and the rated approach temperature difference. The user must also specify the condensing temperature control type.  There are two choices. The user can specify "Fixed" and the condensing temperature will be held constant at the input value.  The user can specify "Float" if they want the condensing temperature to be set by other refrigeration loads served by the higher-temperature system.

The rated heat rejection capacity is not used except as a rough input value check on the overall system sizing. Optional input is provided to help the user compare refrigerant inventories for different systems.  These inputs represent the design values for the refrigerant inventory within the condenser, within a receiver beneath the condenser, and in the liquid pipes between the condenser and the refrigerated cases.

### Inputs

#### Field: Name

Identifies a particular condenser that rejects heat for a single refrigeration system. The name must be unique and will be validated against all the condenser names (ref: Refrigeration:Condenser:\*) in the input file, including names used for air-cooled, evaporative-cooled, and water-cooled condensers.

#### Field: Rated Condensing Temperature

This numeric field provides the rated condensing temperature (C).

#### Field: Rated Approach Temperature Difference

This numeric field provides the rated difference (Delta C) between the saturated condensing temperature for the system rejecting heat and the saturated evaporating refrigerant temperature for the system absorbing heat. The default value is 3.0 C.

#### Field: Rated Effective Total Heat Rejection Rate

This numeric field should be the rated heat rejection effect (W).  Be sure the rating corresponds to the correct refrigerant(s). This value is used for system sizing error checking.

#### Field: Condensing Temperature Control Type

The type of control used to set the saturated condensing temperature. Valid choices are **Fixed** and **Float**.  If the field is blank, **Fixed** will be used. Fixed will hold the condensing temperature constant at the value specified.  **Float** will consider the specified input as a minimum condensing temperature.  This value will be compared to the evaporating temperatures required by other loads on the higher-temperature refrigeration system, and will use the lowest temperature required to meet those loads, along with the Approach Temperature Difference, to set the condensing temperature.

#### Field: Condenser Refrigerant Operating Charge Inventory

This numeric field specifies the amount of refrigerant present within the condenser (kg) specified by the manufacturer, under standard rating conditions. This refrigerant is on the side of the condenser rejecting heat, not on the side of the condenser absorbing the heat.

#### Field: Condensate Receiver Refrigerant Inventory

This numeric field specifies the amount of refrigerant present within the condensate receiver (kg) specified by the manufacturer or system designer, under standard rating conditions.

#### Field: Condensate Piping Refrigerant Inventory

This numeric field specifies the amount of refrigerant present within the condensate piping (kg) specified by the system designer, under standard rating conditions.

The following is an example input for a cascade condenser.

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:Condenser:Cascade,
        CascadeCondenser1 ,      !- Name
        -4. ,                    !- Rated Condensing Temperature {C}
        3. ,                     !- Rated Approach Temperature Difference {DeltaC}
        20000.0 ,                !- Rated Effective Total Heat Rejection Rate {W}
        Fixed;                   !- Condensing Temperature Control Type
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    FOR CONDENSERS COOLING SYSTEMS SERVING CASES AND/OR WALKINS:
    Zone,Average, Refrigeration System Condenser Heat Transfer Rate [W]
    Zone, Sum, Refrigeration System Condenser Heat Transfer Energy [J]
    FOR CONDENSERS COOLING SYSTEMS SERVING AIR CHILLERS:
    HVAC,Average, Refrigeration Air Chiller System Condenser Heat Transfer Rate [W]
    HVAC, Sum, Refrigeration Air Chiller System Condenser Heat Transfer Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### FOR CONDENSERS ON SYSTEMS SERVING CASES AND/OR WALKINS:

#### Refrigeration System Condenser Heat Transfer Rate [W]

This is the total heat transfer across the condenser (i.e., compressor energy and refrigeration loads) in Watts.

#### Refrigeration System Condenser Heat Transfer Energy [J]

This is the total heat energy flowing across the condenser for the timestep being reported.

#### FOR CONDENSERS ON SYSTEMS SERVING AIR CHILLERS:

#### Refrigeration Air Chiller System Condenser Heat Transfer Rate [W]

This is the total heat transfer across the condenser (i.e., compressor energy and refrigeration loads) in Watts.

#### Refrigeration Air Chiller System Condenser Heat Transfer Energy [J]

This is the total heat energy flowing across the condenser for the timestep being reported.

## Refrigeration:GasCooler:AirCooled

The transcritical refrigeration system requires a single gas cooler to reject the system heat.  The total heat of rejection is characterized by a linear relationship between the difference between the outside dry-bulb and gas cooler outlet temperatures.  The rated gas cooler fan power and fan speed control type must be specified.  If the gas cooler is not at ground level, the user may specify an air inlet node name (Ref. [OutdoorAir:Node](#outdoorairnode) name).  Optional input is provided to help the user track refrigerant inventories.  These inputs represent the design values for the refrigerant inventory within the gas cooler, within a receiver, and in the liquid pipes between the gas cooler and the refrigerated cases.

### Inputs

#### Field: Name

Identifies a particular gas cooler that rejects heat for a single refrigeration system.  The name must be unique and will be validated against all the gas cooler names (Ref. [Refrigeration:GasCooler:AirCooled](#refrigerationgascooleraircooled)) in the input file.

#### Field: Rated Total Heat Rejection Rate Curve Name

This field is the name of a curve object defining the gas cooler heat rejection as a function of the difference between the gas cooler outlet and entering air temperatures. The curve should be linear ([Curve:Linear](#curvelinear)). See the Engineering Reference for more discussion on the curve coefficients.

#### Field: Gas Cooler Fan Speed Control Type

The type of fan speed control used by the gas cooler fan.  Valid choices are Fixed, FixedLinear, VariableSpeed, and TwoSpeed.  If the field is blank, Fixed will be used.  Note that fan energy consumption for the air-cooled gas cooler is computed in the same way as that of the air-cooled condenser.  For further information on fan energy calculations, see the discussion regarding air-cooled condenser fan energy in the Engineering Reference.

#### Field: Rated Fan Power

This numeric field specifies the rated fan power (W) specified by the manufacturer, under standard rating conditions.  If this field is blank, the default value of 5000 W will be used

#### Field: Minimum Fan Air Flow Ratio

Fan controls often include a minimum air flow ratio to avoid overheating the fan motor or for other reasons.  This numeric field should be between 0. and 1. and has a default value of 0.2.

#### Field: Transition Temperature

This is the temperature (C) at which the gas cooler and the refrigeration system transitions between subcritical and transcritical operation.  If this field is blank, the default value of 27.0°C will be used.

#### Field: Transcritical Approach Temperature

This is the temperature difference (C) between the refrigerant (CO~2~) exiting the gas cooler and the air entering the gas cooler during transcritical operation.  If this field is blank, the default value of 3.0°C will be used.

#### Field: Subcritical Temperature Difference

This is the temperature difference (C) between the condensing temperature and the ambient air temperature during subcritical operation.  If this field is blank, the default value of 10.0°C will be used.

**Field: Minimum Condensing Temperature**

This numeric field specifies the minimum condensing temperature (C) required to maintain stable subcritical operation.  If this field is blank, the default value of 10.0°C will be used.

**Field: Air Inlet Node Name**

This optional alpha field contains the name of the node from which the gas cooler draws its outdoor air.  If this field is left blank, the outdoor air dry-bulb temperature entering the gas cooler is taken directly from the weather data.  If this field is not blank, this node name must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air temperature from the weather data.  Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air temperature is taken directly from the weather data.

#### Field: End-Use Subcategory

This field allows the specification of a user-defined end-use subcategory (e.g., "Low Temperature Rack").  A new meter for reporting is created for each unique subcategory (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table.  If this field is left blank, the equipment will be assigned to the "General" end-use subcategory.

#### Field: Gas Cooler Refrigerant Operating Charge Inventory

This numeric field specifies the amount of refrigerant present within the gas cooler (kg) specified by the manufacturer, under standard rating conditions.  If this field is blank, the default value of 0.0 kg will be used.

#### Field: Gas Cooler Receiver Refrigerant Inventory

This numeric field specifies the amount of refrigerant present within the gas cooler receiver (kg) specified by the manufacturer or system designer, under standard rating conditions.  If this field is blank, the default value of 0.0 kg will be used.

#### Field: Gas Cooler Outlet Piping Refrigerant Inventory

This numeric field specifies the amount of refrigerant present within the gas cooling outlet piping (kg) specified by the system designer, under standard rating conditions.  If this field is blank, the default value of 0.0 kg will be used.

The following is an example input for an air-cooled gas cooler.

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:GasCooler:AirCooled,
        RefrigGasCooler,         !- Name
        GasCoolerHRCurve,        !- Rated Total Heat Rejection Rate Curve Name
        Fixed,                   !- Gas Cooler Fan Speed Control Type
        6400,                    !- Rated Fan Power
        0.2,                     !- Minimum Fan Air Flow Ratio
        27.0,                    !- Transition Temperature
        3.0,                     !- Transcritical Approach Temperature
        10.0,                    !- Subcritical Temperature Difference
        10.0,                    !- Minimum Condensing Temperature
        ,                        !- Air Inlet Node Name
        RefrigGasCooler,         !- End-Use Subcategory
        ,                        !- Gas Cooler Refrigerant Operating Charge Inventory
        ,                        !- Gas Cooler Receiver Refrigerant Inventory
        ;                        !- Gas Cooler Outlet Piping Refrigerant Inventory
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    Zone,Average, Refrigeration Transcritical System Gas Cooler Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Transcritical System Gas Cooler Heat Transfer Energy [J]
    Zone,Average, Refrigeration Transcritical System Gas Cooler Fan Electric Power [W]
    Zone,Sum, Refrigeration Transcritical System Gas Cooler Fan Electric Energy [J]
    Zone,Average, Refrigeration Transcritical System Gas Cooler Outlet Temperature [C]
    Zone,Average, Refrigeration Transcritical System Gas Cooler Outlet Pressure [Pa]
    Zone,Average, Refrigeration Transcritical System Gas Cooler Defrost Recovered Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Transcritical System Gas Cooler Defrost Recovered Heat Transfer Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Refrigeration Transcritical System Gas Cooler Heat Transfer Rate [W]

This output is the total heat transfer rate from the gas cooler in Watts, and includes compressor energy and the refrigeration load.

#### Refrigeration Transcritical System Gas Cooler Heat Transfer Energy [J]

This output is the total heat transferred from the gas cooler to the surroundings, in Joules, for the timestep being reported.

#### Refrigeration Transcritical System Gas Cooler Fan Electric Power [W]

This output is the electric input to the system's gas cooler fan(s) in Watts.

#### Refrigeration Transcritical System Gas Cooler Fan Electric Energy [J]

This is the electricity consumption of the system's gas cooler fan(s) in Joules for the timestep being reported.  This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Transcritical System Gas Cooler Outlet Temperature [C]

This output is the temperature (in C) of the refrigerant exiting the gas cooler.

#### Refrigeration Transcritical System Gas Cooler Outlet Pressure [Pa]

This output is the pressure (in Pa) of the refrigerant exiting the gas cooler.

#### Refrigeration Transcritical System Gas Cooler Defrost Recovered Heat Transfer Rate [W]

This is the total heat recovered from the gas cooler inlet flow for defrost purposes within the refrigeration system.

#### Refrigeration Transcritical System Gas Cooler Defrost Recovered Heat Transfer Energy [J]

This is the total heat recovered from the gas cooler inlet flow for defrost purposes within the refrigeration system for the timestep being reported.

## Refrigeration:TransferLoadList

A refrigeration system may provide cooling to other, secondary, systems through either a secondary loop or a cascade condenser (Ref. [Refrigeration:SecondarySystem](#refrigerationsecondarysystem) and [Refrigeration:Condenser:Cascade](#refrigerationcondensercascade)). If multiple transfer loads are served by a single primary system, use this list to group them together for reference by the primary system (see the field "Refrigeration Transfer Load or TransferLoad List Name" in the [Refrigeration:System](#refrigerationsystem) object).

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a refrigeration transfer load list. Any reference to this list by another object will use this name. In particular, the primary refrigeration system absorbing the heat from all the loads listed here will use this name to reference those loads.

#### Field: Cascade Condenser Name or Secondary System Name

Identifies a cascade condenser or secondary loop that is cooled, along with the other transfer loads listed here, by a single system. The name will be validated against the [Refrigeration:SecondarySystem](#refrigerationsecondarysystem) and [Refrigeration:Condenser:Cascade](#refrigerationcondensercascade) names in the input file (the name will also be compared against all the other condenser names, but an error will be issued if it refers to any type of condenser other than a cascade condenser).

The list is extensible; ten fields are provided in the IDD.

The following is an example input for a transfer load list.

~~~~~~~~~~~~~~~~~~~~

    Refrigeration:TransferLoadList,
        MedTempTransferLoads ,   !- Name
        CascadeCondFrozen,       !- Cascade Condenser Name or Secondary System Name
        SecondLoopDairy;         !- Cascade Condenser Name or Secondary System Name
~~~~~~~~~~~~~~~~~~~~

## Refrigeration:SecondarySystem

A refrigeration secondary system works in conjunction with refrigerated cases and walkins (Ref. [Refrigeration:SecondarySystem](#refrigerationsecondarysystem), [Refrigeration:CaseAndWalkInList](#refrigerationcaseandwalkinlist), [Refrigeration:Case](#refrigerationcase), and Refrigeration:WalkIn) to simulate the performance of a secondary loop supermarket refrigeration system. Heat from the refrigeration loads served by the secondary loop is absorbed by a primary refrigeration system (ref: [Refrigeration:System](#refrigerationsystem)). The SecondarySystem object simulates a heat exchanger that is an evaporator, or refrigeration load, on the primary refrigeration system.

The inputs for the refrigeration secondary system object include a name, which is also referenced to identify the load placed upon the primary refrigeration system. The inputs that describe the loads placed upon the secondary loop include the name of a single refrigerated case, the name of a single walk-in cooler, or a list of cases and walkin coolers.  The circulating fluid type specifies whether or not the secondary fluid remains in the liquid state throughout the loop (e.g., a glycol or brine solution) or undergoes a partial phase change while serving the refrigeration loads (e.g., a CO~2~ liquid overfeed system). The circulating fluid name must also be specified and must correspond to a name used to provide the fluid properties.

If the secondary fluid remains a liquid, the heat exchanger between the primary and secondary systems functions as an evaporator on the primary side and chills the circulating fluid on the secondary side. Inputs that describe the evaporator performance at rated conditions are used to calculate the heat exchanger effectiveness. These values include the rated evaporator capacity (which can be specified in terms of energy or fluid flow rate), the rated evaporating temperature, the rated approach temperature difference, and the rated range temperature difference.

If the secondary fluid undergoes a partial phase change, the heat exchanger between the primary and secondary systems functions as an evaporator on the primary side and as a condenser on the secondary side. The evaporator capacity, evaporating temperature, and approach temperature difference, all at full-load design conditions, are input.

The balance of the secondary system object describes the pumping system. The pumping options include a single constant speed pump, multiple constant speed pumps, or a variable frequency-drive pump.  The user can also keep track of distribution piping and receiver shell heat gains.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a refrigeration secondary system. Any reference to this refrigeration secondary system by another object will use this name. In particular, the primary refrigeration system absorbing the heat from this system will use this name to define that load.

#### Field: Refrigerated Case or WalkIn or CaseAndWalkInList Name

Identifies a single case, or walkin, or a particular list of refrigerated cases and walkins, that are cooled by this refrigeration secondary system. The name will be validated against the case, walkin, and CaseAndWalkInList names (ref. [Refrigeration:Case](#refrigerationcase), Refrigeration:WalkIn and [Refrigeration:CaseAndWalkInList](#refrigerationcaseandwalkinlist)) in the input file.

#### Field: Circulating Fluid Type

Specifies whether the fluid is always liquid (e.g., glycol solution), or undergoes a partial phase change (e.g., CO~2~ liquid overfeed system).  The options are "FluidAlwaysLiquid" and "FluidPhaseChange".

#### Field: Circulating Fluid Name

The name of the secondary circulating fluid.

For "FluidAlwaysLiquid", this name must correspond to either an ethylene or propylene glycol (ref: [FluidProperties:GlycolConcentration](#fluidpropertiesglycolconcentration)) or to a user-defined glycol (ref: [FluidProperties:Name](#fluidpropertiesname) and FluidProperties: GlycolConcentration) in the input file. Note that the corresponding property data, including [FluidProperties:Concentration](#fluidpropertiesconcentration), [FluidProperties:Temperatures](#fluidpropertiestemperatures), and [FluidProperties:GlycolConcentration](#fluidpropertiesglycolconcentration) must also be included in the input file and are provided in GlycolPropertiesRefData.idf for typical ethylene and propylene glycols.

For "FluidPhaseChange", the refrigerant used by the secondary system must be listed in the [FluidProperties:Name](#fluidpropertiesname) object.  The corresponding property data must also be supplied in the input file. Property data for many refrigererants, including R11, R123, R134A, R12, R22, R404A, R507A, R410A, and R744, are available in FluidPropertiesRefData.idf.

#### Field: Evaporator Capacity

For "FluidAlwaysLiquid", this numeric field should be the rated heat evaporator capacity (W).  Be sure the rating corresponds to the correct refrigerant and secondary circulating fluid. If this variable is specified and the rated evaporator flow rate for secondary fluid is not, then the flow rate will be calculated.  At least one of these two input variables is required.

For "FluidPhaseChange", this numeric field should be the evaporator capacity (W) at full-load design conditions. If this input is left blank, the capacity will be set to the sum of the capacities of the cases and walk-ins served by the secondary loop plus the pump motor load at full-load design conditions.

#### Field: Evaporator Flow Rate for Secondary Fluid

For "FluidAlwaysLiquid" systems, this is the rated evaporator secondary fluid flow rate in m^3^/s.  If this variable is specified and the rated evaporator capacity in watts is not, then the rated capacity will be calculated. At least one of these two input variables is required.

For "FluidPhaseChange", this field is not used (see "Phase Cange Circulating Rate").

#### Field:Evaporator Evaporating Temperature

This numeric field provides the evaporating temperature (C) corresponding to the evaporator capacity. This is the evaporating temperature on the primary side of the heat exchanger used to chill or condense the secondary loop circulating fluid.  It is NOT the temperature in any cases or walkins served by the secondary loop.

#### Field: Evaporator Approach Temperature Difference

For "FluidAlwaysLiquid", this numeric field is the rated temperature difference (DeltaC) between the circulating secondary fluid leaving the heat exchanger and the heat exchanger's evaporating temperature.

For "FluidPhaseChange", this is the temperature difference (DeltaC) between the primary refrigerant evaporating and secondary refrigerant condensing temperatures at full-load design conditions.

#### Field: Evaporator  Range Temperature Difference

For "FluidAlwaysLiquid", this numeric field is the rated temperature difference (DeltaC) between the circulating secondary fluid entering and leaving the heat exchanger. This value is not used for "FluidPhaseChange".

#### Field: Number of Pumps in Loop

This numeric field provides the integer number of pumps used to circulate the secondary heat transfer fluid. The default value is 1. Unless a variable speed pump is specified, pump energy will be calculated in linear steps to achieve the necessary flow rate.

#### Field: Total Pump Flow Rate

This is the secondary fluid circulating flow rate in m^3^/s at full-load design conditions.  For "FluidAlwaysLiquid", if no value is input, the Evaporator Flow Rate for Secondary Fluid will be used. For "FluidPhaseChange", if no value is input, the flow rate will be calculated using the Evaporator Capacity and the PhaseChange Circulating Rate.

#### Field: Total Pump Power

This numeric field should be the pump power (W) at full-load design conditions.  Be sure the rating corresponds to the correct secondary circulating fluid at the design fluid temperature. EITHER the Total Pump Power OR the Total Pump Head is required.

#### Field: Total Pump Head

This numeric field should be the design pressure drop, or head, across the secondary loop (Pa) at full load design conditions.  Be sure the rating corresponds to the correct secondary circulating fluid at the design fluid temperature. EITHER the Total Pump Power OR the Total Head is required.

#### Field: PhaseChange Circulating Rate

This field is not used for "FluidAlwaysLiquid".

For "FluidPhaseChange", the Circulating Rate is defined as the total mass flow at the pump divided by the mass flow rate of vapor returning to the separator. Values between 2.1 and 3 are common for CO~2~ systems and the default is 2.5. If Total Pump Flow Rate is also defined, the PhaseChange Circulating Rate will only be used to check whether the two values are consistent.

#### Field: Pump Drive Type

Specifies whether the pump(s) is constant speed or variable speed.  The options are "Constant" and "Variable". The default is "Constant".

#### Field: Variable Speed Pump Cubic Curve Name

The name of a **cubic** performance curve (ref: Performance Curves) that parameterizes the variation of the variable speed pump power (and the associated load on the secondary refrigeration load) at off-rated conditions. The curve should be normalized to have the value of 1.0 at full-load design conditions. The variable speed pump cubic curve name is used only for the a pump drive type of "Variable".

#### Field: Pump Motor Heat to Fluid

This choice field determines how much of the pump motor heat will be added to the circulating secondary fluid. This represents the motor efficiency for a non-hermetic motor. The default is 0.85.  For a semi-hermetic motor, enter 1.0. The value entered must be between 0.5 and 1.0.

#### Field: Sum UA Distribution Piping

This optional field is typically used when trying to compare the impact of pipe heat gains on system performance, particularly in comparing a DX system to a secondary system. Enter the value for secondary loop distribution piping heat gain (in W/C).  That is, sum up the product of the pipe insulation conductance times the outer piping insulation surface area. Please see the Engineering Reference for guidance in calculating this value.

#### Field: Distribution Piping Zone Name

This optional field is used when trying to determine the impact of pipe heat gains on system performance, particularly in comparing a DX system to a secondary system. (If the previous field, Sum UA Distribution Piping, is blank, this field will not be used.) Enter the name of the zone where the Distribution piping is located. The distribution piping heat gains will be calculated based upon the air temperature within this zone and will count as a cooling credit for this zone.

#### Field: Sum UA Receiver/Separator Shell

This optional field is typically used when trying to compare the impact of refrigeration component heat gains on system performance, particularly in comparing a conventional primary DX system to a secondary system. Enter the value for receiver/separator heat gain (in W/C).  That is, sum up the product of the tank insulaton conductance times the outer tank insulation surface area. Please see the Engineering Reference for guidance in calculating this value.

#### Field: Receiver/Separator Zone Name

This optional field is used when trying to determine the impact of refrigeration component heat gains on system performance, particularly in comparing a conventional primary DX system to a secondary system. (If the previous field, Sum UA Receiver/Separator, is blank, this field will not be used.) Enter the name of the zone where the receiver/separator is located. The heat gains will be calculated based upon the air temperature within this zone and will count as a cooling credit for this zone.

#### Field: Evaporator Refrigerant Inventory

The secondary loop is chilled by a primary system via a heat exchanger.  This field is the refrigerant inventory on the cold (primary) side of that heat exchanger, in kg. The default value is zero.

#### Field: End-Use Subcategory

This field allows the specification of a user-defined end-use subcategory (e.g., "Low Temperature Rack"). A new meter for reporting is created for each unique subcategory (Ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is left blank, the equipment will be assigned to the "General" end-use subcategory.

The following is an example input for a brine-type ("FluidAlwaysLiquid")refrigeration secondary system.

~~~~~~~~~~~~~~~~~~~~

      Refrigeration:SecondarySystem,
        SecondaryMedLoop,        !- Name
        MedTempCaseList,         !- Refrigerated Case or Walkin or CaseAndWalkInList Name
        FluidAlwaysLiquid,       !- Circulating Fluid Type
        ProGly30Percent,         !- Circulating Fluid Name
        3.5E4,                   !- Evaporator Capacity {W}
        0.0021,                  !- Evaporator Flow Rate for Secondary Fluid {M3/s}
        -12.6,                   !- Evaporator Evaporating Temperature {C}
        2.7,                     !- Evaporator Approach Temperature Difference {DeltaC}
        4.,                      !- Evaporator Range Temperature Difference {DeltaC}
        3,                       !- Number of Pumps in Loop
        .0023,                   !- Total Pump Flow Rate {M3/s}
        ,                        !- Total Pump Power {W}
        2.09E5,                  !- Total Pump Head {Pa}
        ,                        !- PhaseChange Circulating Rate
        ,                        !- Pump Drive Type
        ,                        !- Variable Speed Pump Cubic Curve Name
        1.0,                     !- Pump Motor Heat to Fluid
        21.,                     !- Sum UA Distribution Piping {W/C}
        SalesFloor,              !- Distribution Piping Zone Name
        ,                        !- Sum UA Receiver/Separator Shell
        ,                        !- Receiver/Separator Zone Name
        ,                        !- Evaporator Refrigerant Inventory {kg}
        ;                        !- End-Use Subcategory

~~~~~~~~~~~~~~~~~~~~

The following is an example input for a liquid-overfeed-type ("FluidPhaseChange") refrigeration secondary system.

~~~~~~~~~~~~~~~~~~~~

      Refrigeration:SecondarySystem,
        SecondaryLowLoop,        !- Name
        LowTempCaseList,         !- Refrigerated Case or Walkin or CaseAndWalkInList Name
        FluidPhaseChange,        !- Circulating Fluid Type
        R744,                    !- Circulating Fluid Name
        ,                        !- Evaporator Capacity {W}
        ,                        !- Evaporator Flow Rate for Secondary Fluid {M3/s}
        -39.2,                   !- Evaporator Evaporating Temperature {C}
        2.,                      !- Evaporator Approach Temperature Difference {DeltaC}
        ,                        !- Evaporator Range Temperature Difference {DeltaC}
        3,                       !- Number of Pumps in Loop
        ,                        !- Total Pump Flow Rate {M3/s}
        ,                        !- Total Pump Power {W}
        2.09E5,                  !- Total Pump Head {Pa}
        2.5,                     !- PhaseChange Circulating Rate
        Variable,                !- Pump Drive Type
        SecondaryVarSpeedPump,   !- Variable Speed Pump Cubic Curve Name
        1.0,                     !- Pump Motor Heat to Fluid
        10.4,                    !- Sum UA Distribution Piping {W/C}
        SalesFloor,              !- Distribution Piping Zone Name
        1.4,                     !- Sum UA Receiver/Separator Shell
        BackRoom,                !- Receiver/Separator Zone Name
        ,                        !- Evaporator Refrigerant Inventory {kg}
        ;                        !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    FOR SECONDARY SYSTEMS SERVING CASES AND/OR WALKINS:
    Zone,Average,Refrigeration Secondary Loop Pump Electric Power [W]
    Zone,Sum,Refrigeration Secondary Loop Pump Electric Energy [J]
    Zone,Average,Refrigeration Secondary Loop Load Heat Transfer Rate [W]
    Zone,Sum,Refrigeration Secondary Loop Load Heat Transfer Energy [J]
    Zone,Average,Refrigeration Secondary Loop Total Heat Transfer Rate [W]
    Zone,Sum,Refrigeration Secondary Loop Total Heat Transfer Energy [J]
    Zone,Average,Refrigeration Secondary Loop Estimated Refrigerant Inventory Mass[kg] Zone,Average,Refrigeration Secondary Loop Volume Flow Rate [m3/s]
    Zone,Average,Refrigeration Secondary Loop Pipe Heat Gain Rate [W]
    Zone,Average,Refrigeration Secondary Loop Pipe Heat Gain Energy [J]
    Zone,Average,Refrigeration Secondary Loop Receiver Heat Gain Rate [W]
    Zone,Average,Refrigeration Secondary Loop Receiver Heat Gain Energy [J]
    FOR SECONDARY SYSTEMS SERVING AIR CHILLERS:
    HVAC,Average,Refrigeration Air Chiller Secondary Loop Pump Electric Power [W]
    HVAC,Sum,Refrigeration Air Chiller Secondary Loop Pump Electric Energy [J]
    HVAC,Average,Refrigeration Air Chiller Secondary Loop Load Heat Transfer Rate [W]
    HVAC,Sum,Refrigeration Air Chiller Secondary Loop Load Heat Transfer Energy [J]
    HVAC,Average,Refrigeration Air Chiller Secondary Loop Total Heat Transfer Rate [W]
    HVAC,Sum,Refrigeration Air Chiller Secondary Loop Total Heat Transfer Energy [J]
    HVAC,Average,Refrigeration Air Chiller Secondary Loop Estimated Refrigerant Inventory Mass[kg]
    HVAC,Average,Refrigeration Air Chiller Secondary Loop Volume Flow Rate [m3/s]
    HVAC,Average,Refrigeration Air Chiller Secondary Loop Pipe Heat Gain Rate [W]
    HVAC,Average,Refrigeration Air Chiller Secondary Loop Pipe Heat Gain Energy [J]
    HVAC,Average,Refrigeration Air Chiller Secondary Loop Receiver Heat Gain Rate [W]
    HVAC,Average,Refrigeration Air Chiller Secondary Loop Receiver Heat Gain Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### FOR SECONDARY SYSTEMS SERVING CASES AND/OR WALKINS:

#### Refrigeration Secondary Loop Pump Electric Power [W]

This output is the total electric power input to the pump(s) in Watts.

#### Refrigeration Secondary Loop Pump Electric Energy [J]

This is the electricity consumption of the system's pump(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Secondary Loop Load Heat Transfer Rate [W]

This is the amount of refrigeration provided to the cases and walkins served by the secondary loop in W.

#### Refrigeration Secondary Loop Load Heat Transfer Energy [J] 

This is the amount of refrigeration provided to the cases and walkins served by the secondary loop in Joules for the timestep being reported.

#### Refrigeration Secondary Loop Total Heat Transfer Rate [W]

This is the total amount of refrigeration load placed upon the primary refrigeration system (including loads due to the cases and walkins plus the loads from the secondary loop pumps and any energy absorbed by the loop via pipe heat gains) in W.

#### Refrigeration Secondary Loop Total Heat Transfer Energy [J]

This is the total amount of energy placed upon the primary refrigeration system by the secondary loop in Joules for the timestep being reported.

#### Refrigeration Secondary Loop Estimated Refrigerant Inventory Mass [kg] 

This output is the sum of the input inventory values for the cases and walkins and the refrigerant circulating through the loop.

#### Refrigeration Secondary Loop Volume Flow Rate [m3/]s

This output is the calculated volume flow through the pumps for this secondary loop.

#### Refrigeration Secondary Loop Pipe Heat Gain Rate  [W]

This output is the total heat transferred to the pipes in Watts.

#### Refrigeration Secondary Loop Pipe Heat Gain Energy [J] 

This is the total heat transferred to the pipes in Joules for the timestep being reported.

#### Refrigeration Secondary Loop Receiver Heat Gain Rate  [W]

This output is the total heat transferred to the receiver in Watts.

#### Refrigeration Secondary Loop Receiver Heat Gain Energy [J] 

This is the total heat transferred to the receiver in Joules for the timestep being reported.

#### FOR SECONDARY SYSTEMS SERVING AIR CHILLERS:

#### Refrigeration Air Chiller Secondary Loop Pump Electric Power [W]

This output is the total electric power input to the pump(s) in Watts.

#### Refrigeration Air Chiller Secondary Loop Pump Electric Energy [J]

This is the electricity consumption of the system's pump(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Air Chiller Secondary Loop Load Heat Transfer Rate [W]

This is the amount of refrigeration provided to the air chillers served by the secondary loop in W.

#### Refrigeration Air Chiller Secondary Loop Load Heat Transfer Energy [J] 

This is the amount of refrigeration provided to the air chillers served by the secondary loop in Joules for the timestep being reported.

#### Refrigeration Air Chiller Secondary Loop Total Heat Transfer Rate [W]

This is the total amount of refrigeration load placed upon the primary refrigeration system (including loads due to the air chillers plus the loads from the secondary loop pumps and any energy absorbed by the loop via pipe heat gains) in W.

#### Refrigeration Air Chiller Secondary Loop Total Heat Transfer Energy [J]

This is the total amount of energy placed upon the primary refrigeration system by the secondary loop in Joules for the timestep being reported.

#### Refrigeration Air Chiller Secondary Loop Estimated Refrigerant Inventory Mass [kg] 

This output is the sum of the input inventory values for the air chillers  and the refrigerant circulating through the loop.

#### Refrigeration Air Chiller Secondary Loop Volume Flow Rate [m3/]s

This output is the calculated volume flow of coolant through the pumps for this secondary loop.

#### Refrigeration Air Chiller Secondary Loop Pipe Heat Gain Rate  [W]

This output is the total heat transferred to the pipes in Watts.

#### Refrigeration Air Chiller Secondary Loop Pipe Heat Gain Energy [J] 

This is the total heat transferred to the pipes in Joules for the timestep being reported.

#### Refrigeration Air Chiller Secondary Loop Receiver Heat Gain Rate  [W]

This output is the total heat transferred to the receiver in Watts.

#### Refrigeration Air Chiller Secondary Loop Receiver Heat Gain Energy [J]

This is the total heat transferred to the receiver in Joules for the timestep being reported.

## Additional Refrigeration Outputs Available for Each Zone

Multiple refrigeration cases and walkins may be located within a single zone.  Each zone may also exchange heat with refrigeration system suction piping, secondary loop distribution piping, and secondary loop receivers. Output variables have been prepared to sum up the total heat exchange due to all refrigeration objects within a single zone.  The output variables that start with "[Zone](#zone) Refrigeration" do not account for refrigeration provided by refrigeration chiller sets or any piping from systems serving air chillers.  The output variables that start with "[Zone](#zone) Chillers" account for all the refrigeration system elements serving air chillers.

## Additional Refrigeration Outputs for Each Zone

~~~~~~~~~~~~~~~~~~~~

    FOR THE SUM OF ALL COOLING SYSTEMS SERVING CASES AND/OR WALKINS:
    Zone,Average, Refrigeration Zone Case and Walk In Sensible Cooling Rate [W]
    Zone,Sum, Refrigeration Zone Case and Walk In Sensible Cooling Energy [J]
    Zone,Average, Refrigeration Zone Case and Walk In Heating Rate [W]
    Zone,Sum, Refrigeration Zone Case and Walk In Heating Energy [J]
    Zone,Average, Refrigeration Zone Case and Walk In Total Sensible Cooling Rate [W]
    Zone,Sum, Refrigeration Zone Case and Walk In Total Sensible Cooling Energy [J]
    Zone,Average, Refrigeration Zone Case and Walk In Total Latent Cooling Rate [W]
    Zone,Sum, Refrigeration Zone Case and Walk In Total Latent Cooling Energy [J]
    Zone,Sum, Refrigeration Zone Case and Walk In Total Cooling Energy [J]
    Zone,Average,Refrigeration Zone Case and Walk In Total Cooling Rate [W]
    Zone,Average, Refrigeration Zone Case and Walk In Total Heat Transfer Rate [W]
    Zone,Sum, Refrigeration Zone Case and Walk In Total Heat Transfer Energy [J]

    FOR THE SUM OF ALL COOLING SYSTEMS SERVING CHILLERS:
    HVAC,Average,
    Refrigeration Zone Air Chiller Sensible Heat Transfer Rate
     [W]
    HVAC,Sum,
    Refrigeration Zone Air Chiller Sensible Heat Transfer Energy
     [J]
    HVAC,Average,
    Refrigeration Zone Air Chiller Latent Cooling Rate
     [W]
    HVAC,Sum,
    Refrigeration Zone Air Chiller Latent Cooling Energy
     [J]
    HVAC,Average,
    Refrigeration Zone Air Chiller Total Cooling Rate
     [W]
    HVAC,Sum,
    Refrigeration Zone Air Chiller Total Cooling Energy
     [J]
~~~~~~~~~~~~~~~~~~~~

### FOR THE SUM OF ALL COOLING SYSTEMS SERVING CASES AND/OR WALKINS:

### Refrigeration Zone Case and Walk In Total Sensible Cooling Rate [W]

This output is the total sensible heat transfer between all refrigeration objects located in the zone in Watts. A negative value will be reported when the refrigeration objects cool (that is, remove heat from) the zone.

**Refrigeration [Zone](#zone) Case and Walk In Total Sensible Cooling Energy [J]**

This is the total sensible heat transfer between all refrigeration objects located in the zone in Joules for for the timestep being reported. A negative value will be reported when the refrigeration objects cool (that is, remove heat from) the zone.

### Refrigeration Zone Case and Walk In Total Latent Cooling Rate [W]

This output is the total latent heat transfer between all refrigeration objects located in the zone in Watts. A negative value will be reported when the refrigeration equipment provides dehumidification (thereby reducing the zone latent load).

**Refrigeration [Zone](#zone) Case and Walk In Total Latent Cooling Energy [J]**

This is the total latent heat transfer between all refrigeration objects located in the zone in Joules for for the timestep being reported. A negative value will be reported when the refrigeration equipment provides dehumidification (thereby reducing the zone latent load).

### Refrigeration Zone Case and Walk In Total Heat Transfer Rate [W]

This output is the total heat transfer (sensible plus latent) between all refrigeration objects located in the zone in Watts. A negative value will be reported when the refrigeration objects cool (that is, remove heat from) the zone.

**Refrigeration [Zone](#zone) Case and Walk In Total Heat Transfer Energy [J]**

This is the total heat transfer (sensible plus latent) between all refrigeration objects located in the zone in Joules for for the timestep being reported. A negative value will be reported when the refrigeration objects cool (that is, remove heat from) the zone.

### Refrigeration Zone Case and Walk In Sensible Cooling Rate [W]

This output is the total sensible cooling from all refrigeration objects located in the zone in Watts.

**Refrigeration [Zone](#zone) Case and Walk In Sensible Cooling Energy [J]**

This is the total sensible cooling from all refrigeration objects located in the zone in Joules for for the timestep being reported.

### Refrigeration Zone Case and Walk In Heating Rate [W]

This output is the total heating from all refrigeration objects located in the zone in Watts.

**Refrigeration [Zone](#zone) Case and Walk In Heating Energy [J]**

This is the total heating from all refrigeration objects located in the zone in Joules for for the timestep being reported.

### Refrigeration Zone Case and Walk In Total Cooling Rate [W]

This output is the total cooling (sensible plus latent) from all refrigeration objects located in the zone in Watts.

**Refrigeration [Zone](#zone) Case and Walk In Total Cooling Energy [J]**

This is the total cooling (sensible plus latent) from all refrigeration objects located in the zone in Joules for for the timestep being reported.

### FOR THE SUM OF ALL COOLING SYSTEMS SERVING AIR CHILLERS:

### Refrigeration Zone Air Chiller Heating Rate  [W]

This output is the total heating from all air chillers located in the zone in Watts.

**Refrigeration [Zone](#zone) Air Chiller Heating Energy [J]**

This is the total heating from all air chillers located in the zone in Joules for for the timestep being reported.

### Refrigeration Zone Air Chiller Sensible Cooling Rate   [W]

This output is the total sensible cooling from all air chillers located in the zone in Watts.

**Refrigeration [Zone](#zone) Air Chiller Sensible Cooling Energy [J]**

This is the total sensible cooling from all air chillers located in the zone in Joules for for the timestep being reported.

### Refrigeration Zone Air Chiller Sensible Heat Transfer Rate  [W]

This output is the total sensible heat transfer from all air chillers located in the zone in Watts. A negative value will be reported when the refrigeration objects cool (that is, remove heat from) the zone.

### Refrigeration Zone Air Chiller Sensible Heat Transfer Energy  [J]

This is the total sensible heat transfer from all air chillers located in the zone in Joules for for the timestep being reported. A negative value will be reported when the refrigeration objects cool (that is, remove heat from) the zone.

### Refrigeration Zone Air Chiller Latent Cooling Rate  [W]

This output is the total latent cooling from all air chillers located in the zone in Watts.

**Refrigeration [Zone](#zone) Air Chiller Latent Cooling Energy [J]**

This is the total latent cooling from all air chillers located in the zone in Joules for for the timestep being reported.

### Refrigeration Zone Air Chiller Water Removed Mass Flow Rate  [kg/s]

This is the total amount of water removed by all air chillers located in the zone in kg/s for for the timestep being reported.

### Refrigeration Zone Air Chiller Total Cooling Rate  [W]

This output is the total cooling (sensible plus latent) from all air chillers located in the zone in Watts.

**Refrigeration [Zone](#zone) Air Chiller Total Cooling Energy [J]**

This is the total cooling (sensible plus latent) from all air chillers located in the zone in Joules for for the timestep being reported.

## Refrigeration:AirChiller

The [Refrigeration:AirChiller](#refrigerationairchiller) object works in conjunction with a refrigeration chiller set, compressor rack, a refrigeration system, or a refrigeration secondary system object (Ref. ZoneHvac:RefrigerationChillerSet, and a [Refrigeration:CompressorRack](#refrigerationcompressorrack), [Refrigeration:System](#refrigerationsystem), or [Refrigeration:SecondarySystem](#refrigerationsecondarysystem)) to simulate the performance of an air chiller, similar to one found in a refrigerated warehouse. The air chiller model uses information at rated conditions along with the zone conditions to determine performance. Energy use for fans and heaters is modeled based on inputs for nominal power, schedules, and control type. The refrigeration chiller model accounts for the sensible and latent heat exchange with the surrounding environment.

The refrigeration chiller cooler object inputs include a name, an availability schedule name, the rated cooling capacity, the rated operating temperature, the rated cooling source temperature, the rated total heating power and heating power schedule, the rated fan power and schedules, defrost type, defrost control type, defrost schedule name, drip-down schedule name, defrost power, the portion of the defrost energy used to melt ice (only for temperature termination control type), and refrigerant inventory.

Chiller coils are rated under multiple conventions. Each rating is typically based upon a selected fin material and refrigerant cycle, with correction factors for other materials or refrigerants. Fields are provided here for those correction factors. The performance of all chiller coils depends upon the inlet air temperature, relative humidity, and flow rate. Multiple methods of expressing this relationship are provided here to accommodate the way information is provided by different manufacturers.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a refrigeration chiller. Any reference to this refrigeration chiller by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the refrigeration chiller can operate during a given time period. A schedule value greater than 0 (maximum schedule value of 1.0 is typically used) indicates that the refrigeration chiller will operate during a given time period. A value equal to 0 denotes that the case does not operate (everything is OFF: refrigeration, fans, lights, anti-sweat, etc.). Typically the refrigeration chiller will operate throughout the day (i.e., the schedule will contain 1 for all time periods); however, refrigeration chillers require maintenance and/or cleaning and this can be modeled accordingly using this schedule if desired. If this field is left blank, the default schedule has a value of 1 for all time periods.

#### Field: Capacity Rating Type

The type of capacity rating used for this refrigeration chiller. Valid choices are UnitLoadFactorSensibleOnly, CapacityTotalSpecificConditions, EuropeanSC1Standard, EuropeanSC1NominalWet, EuropeanSC2Standard, EuropeanSC2NominalWet, EuropeanSC3Standard, EuropeanSC3NominalWet, EuropeanSC4Standard, EuropeanSC4NominalWet, EuropeanSC5Standard, and EuropeanSC5NominalWet. In each case, select the rating option that corresponds to the expected service conditions.  For example, U.S. manufacturers quote a separate Unit Load Factor for wet or frosted coils.  If the evaporating temperature is less than 0C, input the frosted coil value. Within the European convention, select SC1, 2, 3, 4, or 5 depending upon the expected evaporating temperature. This field is required and there is no default value. Refer to the Engineering Reference for further information on these rating types. NOTE: If the CapacityTotalSpecificConditions rating type is specified, the input file must include the manufacturer's coil capacity correction curve in tabular form using the [Table:MultiVariableLookup](#tablemultivariablelookup) object. An example of this may be found in the RefrigeratedWarehouse.idf example file.

#### NOTE – ONLY ONE OF THE FOLLOWING TWO FIELDS IS USED

#### Field: Rated Unit Load Factor

The sensible cooling capacity in watts (W/C) at rated conditions. The value entered for this field must be greater than zero, with no default value. This value is only used if the Capacity Rating Type is UnitLoadFactorSensibleOnly. The value given must be based upon the difference between the chiller inlet and outlet air temperatures, not on the difference between the zone mean temperature and the outlet air temperature.

#### Field: Rated Capacity

The cooling capacity in watts (W) at rated conditions. The value entered for this field must be greater than zero, with no default value. This field is only used if the Capacity Rating Type is CapacityTotalSpecificConditions, EuropeanSC1Standard, EuropeanSC1NominalWet, EuropeanSC2Standard, EuropeanSC2NominalWet, EuropeanSC3Standard, EuropeanSC3NominalWet, EuropeanSC4Standard, EuropeanSC4NominalWet, EuropeanSC5Standard, or EuropeanSC5NominalWet. For CapacityTotalSpecificConditions, this capacity includes both sensible and latent at the conditions given in the next two fields.  Note that the European Standard ratings are sensible only and the European Nominal ratings include latent capacity as well. The value given here must correspond to the capacity rating type given previously.

#### Field: Rated Relative Humidity

This field is ONLY used if the Capacity Rating Type is CapacityTotalSpecificConditions and represents the relative humidity at rated conditions, expressed as a percent. The default is 85.

#### Field: Rated Cooling Source Temperature

For a DX evaporator coil, enter the saturated evaporating temperature in ˚C.  For a fluid coil, enter the fluid coil entrance temperature in ˚C. There is no default value. This number is used, with temperatures for other refrigeration loads on any one system, to set that system's minimum suction pressure or minimum circulating fluid temperature.

#### Field: Rated Temperature Difference DT1

The rated difference between the air entering the refrigeration chiller and the cooling source temperature in ˚C. The entered value for this field must be less than 20˚C. There is no default value.

#### Field: Maximum Difference Between Inlet Air and Evaporating Temperature 

The maximum difference between the air entering the refrigeration chiller and the cooling source temperature in °C used to limit capacity during pull-down. The default is 1.3 times the Rated Temperature Difference DT1.

#### Field: Coil  Material Correction Factor

This field is used to enter the manufacturer's correction factor for coil material corresponding to the rating previously input. The default is 1.0 (dimensionless).

#### Field: Refrigerant Correction Factor

This field is used to enter the manufacturer's correction factor for refrigerant corresponding to the rating previously input. The default is 1.0 (dimensionless). (Note, the refrigerant itself is specified for the detailed system or secondary loop providing the refrigerant to this coil.)

#### Field: Capacity Correction Curve Type

The type of capacity correction curve used to account for the impact of the room air conditions on the chiller capacity. Valid choices are LinearSHR60, QuadraticSHR, European, and TabularRHxDT1xTRoom. This field will default to LinearSHR60, unless the CapacityTotalSpecificConditions rating type is specified, in which case, the TabularRHxDT1xTRoom correction curve type must be used. Refer to the Engineering Reference for further information on these curve types. The resulting correction factor for LinearSHR60, QuadraticSHR, and European types will be applied to the sensible capacity and must have output values between 1 and 2. The resulting correction factor for TabularRHxDT1xTRoom will be applied to the CapacityTotalSpecificConditions capacity and can have values between 0.2 and 2. If TabularRHxDT1xTRoom is used, the curve type within the table must be specified as "Quadratic".

#### Field: Capacity Correction Curve Name

The name of the curve object defining the total refrigerating capacity. For correction curve types LinearSHR60 and QuadraticSHR, the independent variable is the Sensible Heat Ratio and the output values are between 1.0 and 2.0. For correction curve type European, EnergyPlus contains built-in capacity correction curves and specification of a capacity correction curve name is not required. For correction curve type TabularRHxDT1xTRoom, enter the name of a [Table:MultiVariableLookup](#tablemultivariablelookup) object that gives the total capacity (in W) as a function of RH, DT1, and Room Temperature; IN THAT ORDER. An example of a TabularRHxDT1xTRoom correction curve using the Table:MultiVariableLoopkup object may be found in the RefrigeratedWarehouse.idf example file.

#### Field: SHR60 Correction Factor

This field is only used when the capacity correction curve type is LinearSHR60. It should correspond to the capacity factor, that is, the total capacity divided by the sensible capacity corresponding to a Sensible Heat Ratio of 0.6. The default is 1.48 (dimensionless).

#### Field: Rated Total Heating Power

The total heating power in watts including drip-pan and any other heaters (W). This value is required and has no default value.

#### Field: Heating Power Schedule Name

The name of the schedule (ref: Schedule) that denotes the fraction of heater power that operates during a given time period. A schedule value of zero denotes that all heaters are off. A schedule value greater than 0 indicates that some portion of the total heater power will operate during that time period (maximum schedule value of 1.0 means all heaters are fully on). If this field is left blank, the default schedule has a value of 1 for all time periods.

#### Field: Fan Speed Control Type

The type of fan speed control used by the cooling coil's fan(s). Valid choices are **Fixed**, **FixedLinear**, **VariableSpeed**, and **TwoSpeed**.  If the field is blank, **Fixed** will be used. See the Engineering Reference for a discussion of this option's effect on fan energy consumption.

#### Field: Rated Fan Power

The cooling coil fan power in watts (W). This value has a default value of 375W. This fan is assumed to run continuously except during electric, hot brine, or hot gas defrost periods.

#### Field: Rated Air Flow

The cooling coil fan rated air flow in cubic meters per second (m^3^/s). This value has no default value and must be input.

#### Field: Minimum Fan Air Flow Ratio

The minimum fraction of the rated air flow for part-load fan operation, must be between 0. and 1.0. The default value is 0.2.

#### Field: Defrost Type

The type of defrost used for this refrigeration chiller. Valid choices are None, Off-Cycle, HotFluid, and Electric. The default defrost type is Electric if the field is blank. HotFluid includes both hot gas and hot brine defrost systems. Refer to the Engineering Reference for further information on how the different defrost types are modeled.

#### Field: Defrost Control Type

The type of defrost control used for this refrigeration chiller. Valid choices are TimeSchedule and TemperatureTermination. The default is TimeSchedule if the field is blank. Refer to the Engineering Reference for further information on how the different defrost controls are modeled.

#### Field: Defrost Schedule Name

The name of the schedule (ref: Schedule) that denotes when the refrigeration chiller requires defrosting. A schedule value of 1.0 indicates that defrost can be on during a given time period. A value equal to 0 denotes that the defrost is off. Defrost schedules normally repeat the duration and number of defrost cycles for each day of the year. The refrigeration chiller manufacturer typically provides this information with the product literature. If TemperatureTermination control type is used, the defrost schedule is used for the defrost cycle start time and the defrost cycle end time is not allowed to extend beyond the scheduled off time. The use of Compact Schedules (ref. Schedules) are ideal for this purpose.

#### Field: Defrost Drip-Down Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the refrigeration chiller requires additional time for draining condensate after the defrost period. A schedule value of 1.0 indicates that the defrost drip-down period is on during a given time period. A value equal to 0 denotes that the defrost drip-down period is over. The refrigeration chiller manufacturer typically provides this information with the product literature.

Each defrost drip-down period specified in this schedule should start at the same time as the corresponding defrost period specified in the schedule for Defrost Schedule, and the drip-down schedule defrost period should always be longer than or equal to the length of the defrost schedule time period. For example, if the defrost schedule contains a defrost period from 7:00 to 7:15, you would specify a case defrost drip-down period from 7:00 to 7:20 if you wanted to model a 5 minute condensate drip-down period after the regular defrost period. If no drip-down schedule is entered, then the defrost schedule (specified for the previous input field) will be used. The use of Compact Schedules (ref. Schedules) are ideal for this purpose.

#### Field: Defrost Power

The defrost power in watts. This input is required for hot-fluid (hot gas or hot brine), or electric defrost types. Refrigeration chiller manufacturers do not typically provide information on the heat input for hot gas and hot brine defrost. Information provided for electric defrost power can be substituted here for refrigeration chillers using hot-gas or hot-brine defrost if other information is not available. Only electric Defrost Types consume electricity during the defrost period. The entered value for this field must be greater than or equal to zero.

#### Field: Temperature Termination Defrost Fraction to Ice

When cooling coils go through a defrost cycle, only a portion of the defrost energy is actually used to melt the ice.  The rest of the defrost energy goes to increasing the temperature of the coils themselves and to the refrigeration chiller environment. The Temperature Termination defrost control type calculates the end of the defrost cycle that corresponds to melting all the ice. Therefore, the user must input this fractional value. The default value is 0.7 for electric defrost and 0.3 for hot fluid defrost. Refer to the Engineering Reference for further information on how the defrost energy control types are modeled.

#### Field: Vertical Location

The vertical location for this refrigeration chiller. Valid choices are Floor, Middle, and Ceiling. The default is Middle if the field is blank. The vertical location is used to transform the mixed zone air temperature to the coil inlet air temperature. Refer to the Engineering Reference for further information on how the different vertical locations are modeled.

#### Field: Average Refrigerant Charge Inventory

The value of this optional field is the refrigerant inventory present in the refrigeration chiller during ordinary operation. The value is used to produce an estimate of the total refrigerant present in the refrigeration system. The value is entered in kg.

The following is an example input for a refrigeration air chiller.

~~~~~~~~~~~~~~~~~~~~

       Refrigeration:AirChiller,
        Freezer_1AirChiller_1,    !- Name
        AvailAllYear,             !- Availability Schedule Name
        UnitLoadFactorSensibleOnly, !- Capacity Rating Type
        10900.,                   !- Rated Unit Load Factor {W/deltaC}
        ,                         !- Rated Capacity {W}
        ,                         !- Rated Relative Humidity
        -26.,                     !- Rated Cooling Source Temperature {C}
        8.,                       !- Rated Temperature Difference DT1 {DeltaC}
        11.,                      !- Maximum Temperature Difference DT1 {DeltaC}
        ,                         !- Material Correction Factor
        ,                         !- Refrigerant Correction Factor
        LinearSHR60,              !- Capacity Correction Curve Type
        ,                         !- Capacity Correction Curve Name
        1.5,                      !- SHR60 Correction Factor {dimensionless}
        200.,                     !- Rated Total Heating Power {W}
        AirChillerDripDownSched1,    !- Heating Power Schedule Name
        ,                         !- Fan Speed Control Type
        375.,                     !- Rated Fan Power {W}
        12.4,                     !- Rated Air Flow {m3/s}
        ,                         !- Minimum Fan Air Flow Ratio
        Electric,                 !- Defrost Type
        TimeSchedule,             !- Defrost Control Type
        AirChillerDefrostSched1,  !- Defrost Schedule Name
        AirChillerDripDownSched1, !- Defrost Drip-Down Schedule Name
        55066.,                  !- Defrost Power {W}
        ,                         !- Temperature Termination Defrost Fraction to Ice
        ,                         !- Vertical Location
        ;                         !- Average Refrigerant Charge Inventory {kg}

~~~~~~~~~~~~~~~~~~~~

The following is an example input for a refrigeration air chiller.

~~~~~~~~~~~~~~~~~~~~

       Refrigeration:AirChiller,
        Freezer_1AirChiller_1,    !- Name
        AvailAllYear,             !- Availability Schedule Name
        UnitLoadFactorSensibleOnly, !- Capacity Rating Type
        10900.,                   !- Rated Unit Load Factor {W/deltaC}
        ,                         !- Rated Capacity {W}
        ,                         !- Rated Relative Humidity
        -26.,                     !- Rated Cooling Source Temperature {C}
        8.,                       !- Rated Temperature Difference DT1 {DeltaC}
        11.,                      !- Maximum Temperature Difference DT1 {DeltaC}
        ,                         !- Material Correction Factor
        ,                         !- Refrigerant Correction Factor
        LinearSHR60,              !- Capacity Correction Curve Type
        ,                         !- Capacity Correction Curve Name
        1.5,                      !- SHR60 Correction Factor {dimensionless}
        200.,                     !- Rated Total Heating Power {W}
        AirChillerDripDownSched1,    !- Heating Power Schedule Name
        ,                         !- Fan Speed Control Type
        375.,                     !- Rated Fan Power {W}
        12.4,                     !- Rated Air Flow {m3/s}
        ,                         !- Minimum Fan Air Flow Ratio
        Electric,                 !- Defrost Type
        TimeSchedule,             !- Defrost Control Type
        AirChillerDefrostSched1,  !- Defrost Schedule Name
        AirChillerDripDownSched1, !- Defrost Drip-Down Schedule Name
        55066.,                  !- Defrost Power {W}
        ,                         !- Temperature Termination Defrost Fraction to Ice
        ,                         !- Vertical Location
        ;                         !- Average Refrigerant Charge Inventory {kg}

~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Refrigeration Zone Air Chiller Total Cooling Rate [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Total Cooling Energy [J]
    HVAC,Average,Refrigeration Zone Air Chiller Sensible Cooling Rate [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Sensible Cooling Energy [J]
    HVAC,Average,Refrigeration Zone Air Chiller Latent Cooling Rate [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Latent Cooling Energy [J]
    HVAC,Average, Refrigeration Zone Air Chiller Water Removed Mass Flow Rate [kg/s]
    HVAC,Average,Refrigeration Zone Air Chiller Total Electric Power [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Total Electric Energy [J]
    HVAC,Average,Refrigeration Zone Air Chiller Fan Electric Power [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Fan Electric Energy [J]
    HVAC,Average,Refrigeration Zone Air Chiller Heater Electric Power [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Heater Electric Energy [J]
    HVAC,Average,Refrigeration Zone Air Chiller Sensible Heat Ratio []
    HVAC,Average,Refrigeration Zone Air Chiller Frost Accumulation Mass [kg]
    HVAC,Average,Refrigeration Zone Air Chiller Zone Total Cooling Rate [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Zone Total Cooling Energy [J]
    HVAC,Average,Refrigeration Zone Air Chiller Zone Sensible Cooling Rate [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Zone Sensible Cooling Energy [J]
    HVAC,Average,Refrigeration Zone Air Chiller Zone Heating Rate [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Zone Heating Energy [J]

    Report only for Air Chillers using electric defrost
    HVAC,Average,Refrigeration Zone Air Chiller Defrost Electric Power [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Defrost Electric Energy [J]

    Report for each Zone exchanging energy with the Air Chiller
    HVAC,Average,Refrigeration Zone Air Chiller Sensible Cooling Rate [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Sensible Cooling Energy [J]
    HVAC,Average, Refrigeration Zone Air Chiller Heating Rate [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Heating Energy [J]
    HVAC,Average, Refrigeration Zone Air Chiller Latent Cooling Rate [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Latent Cooling Energy [J
    HVAC,Average,Refrigeration Zone Air Chiller Total Cooling Rate [W]
    HVAC,Sum,Refrigeration Zone Air Chiller Total Cooling Energy [J]
    HVAC,Average,Refrigeration Zone Air Chiller Water Removed Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### Refrigeration Zone Air Chiller Total Cooling Rate [W]

This field is the total (sensible plus latent) cooling rate of the Air Chiller evaporator coil in Watts.

#### Refrigeration Zone Air Chiller Total Cooling Energy [J]

This field is the total (sensible plus latent) cooling of the Air Chiller evaporator coil in Joules over the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Zone Air Chiller Sensible Cooling Rate [W]

#### Refrigeration Zone Air Chiller Sensible Heat Transfer Rate [W]

This field is the sensible cooling rate of the Air Chiller evaporator coil in Watts.

#### Refrigeration Zone Air Chiller Sensible Cooling Energy  [J]

#### Refrigeration Zone Air Chiller Sensible Heat Transfer Energy [J]

This field is the sensible cooling of the Air Chiller evaporator coil in Joules over the timestep being reported.

#### Refrigeration Zone Air Chiller Latent Cooling Rate  [W]

This field is the latent cooling (dehumidification) rate of the Air Chiller evaporator coil in Watts.

#### Refrigeration Zone Air Chiller Latent Cooling Energy  [J]

This field is the latent cooling (dehumidification) of the Air Chiller evaporator coil in Joules over the timestep being reported.

#### Refrigeration Zone Air Chiller Water Removed Mass Flow Rate  [kg/s]

This field is the latent cooling (dehumidification) of the Air Chiller evaporator coil in kilograms per second over the timestep being reported.

#### Refrigeration Zone Air Chiller Total Electric Power [W]

This field is the total electricity (fan, heaters, lights, and electric defrost) used by the Air Chiller in Watts.

#### Refrigeration Zone Air Chiller Total Electric Energy [J]

This field is the electricity (fan, heaters, lights, and electric defrost)used by the Refrigeration chiller in Joules over the timestep being reported.

#### Refrigeration Zone Air Chiller Fan Electric Power [W]

This field is the electric power input to the Air Chiller fan(s) in Watts.

#### Refrigeration Zone Air Chiller Fan Electric Energy  [J]

This field is the electricity consumption of the Air Chiller fan(s) in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Zone Air Chiller Heater Electric Power  [W]

This field is the electric power input to the Air Chiller heaters in Watts.

#### Refrigeration Zone Air Chiller Heater Electric Energy [J]

This field is the total electricity consumption of the Air Chiller heaters in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Refrigeration Zone Air Chiller Defrost Electric Power  [W]

This field is the electric power input to the Air Chiller electric defrost heater(s) in Watts. This output is available if case defrost type is Electric.

#### Refrigeration Zone Air Chiller Defrost Electric Energy [J]

This field is the total electricity consumption of the Air Chiller electric defrost heater(s) in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Refrigeration, Group Key = [Building](#building) (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). This output is available if case defrost type is Electric.

#### Refrigeration Zone Air Chiller Sensible Heat Ratio

This field is the Sensible Cooling Energy  Rate divided by the Total Cooling Energy Rate.

#### Refrigeration Zone Air Chiller Frost Accumulation Mass [kg]

This field is the total amount of frost present on the coil during the timestep being reported.

#### Refrigeration Zone Air Chiller Zone Total Cooling Rate [W]

This field is the rate of total cooling delivered to the zone in Watts. A positive value is reported when the zone is cooled by the air chiller, otherwise a zero is reported.

#### Refrigeration Zone Air Chiller Zone Total Cooling Energy  [J]

This field is the amount of total cooling energy delivered to the zone in Joules. A positive value is reported when the zone is cooled by the air chiller, otherwise a zero is reported.

#### Refrigeration Zone Air Chiller Zone Sensible Cooling Rate [W]

This field is the rate of sensible cooling delivered to the zone in Watts. A positive value is reported when the zone is cooled by the air chiller, otherwise a zero is reported.

#### Refrigeration Zone Air Chiller Zone Sensible Cooling Energy [J]

This field is the amount of sensible cooling energy delivered to the zone in Joules. A positive value is reported when the zone is cooled by the air chiller, otherwise a zero is reported.

#### Refrigeration Zone Air Chiller Zone Heating Rate [W]

This field is the rate of sensible heating delivered to the zone in Watts. A positive value is reported when the zone is heated by the air chiller (typically only occurs during defrost), otherwise a zero is reported.

#### Refrigeration Zone Air Chiller Zone Heating Energy [J]

This field is the amount of sensible heating energy delivered to the zone in Joules. A positive value is reported when the zone is heated by the air chiller (typically only occurs during defrost), otherwise a zero is reported.

*The following output variables are available for each zone served by any Air Chiller. An output variable identification is created for each zone exchanging energy with the Air Chiller.  For example if the Air Chiller, "DairyRefrigeration chiller", were exchanging energy with the zone, "SalesArea", the output variable identification would be "DairyRefrigeration chillerInZoneSalesArea".*

#### Refrigeration Zone Air Chiller Sensible Cooling Rate  [W]

This field is the rate of sensible cooling delivered to the zone, net of any auxiliary heat input,  in Watts..

#### Refrigeration Zone Air Chiller Sensible Cooling Energy [J]

This field is the amount of sensible cooling energy, net of any auxiliary heat input, delivered to the zone in Joules.

#### Refrigeration Zone Air Chiller Latent Cooling Rate [W]

This field is the rate of latent cooling (dehumidification) delivered to the zone in Watts. A negative value will be reported when the Air Chiller provides dehumidification (thereby reducing the zone latent load).

#### Refrigeration Zone Air Chiller Water Removed Mass Flow Rate  [kg/s]

This field is the rate of latent cooling (dehumidification) delivered to the zone in kg/s. A negative value (or zero) will be reported when the Air Chiller provides dehumidification (thereby reducing the zone latent load).

#### Refrigeration Zone Air Chiller Latent Cooling Energy [J]

This field is the amount of latent cooling (dehumidification) energy delivered to the zone in Joules. A negative value (or zero) will be reported when the Air Chiller provides dehumidification (thereby reducing the zone latent load).

#### Refrigeration Zone Air Chiller Total Cooling Rate [W]

This field is the total rate of cooling, sensible plus latent, delivered to the zone in Watts.

#### Refrigeration Zone Air Chiller Total Cooling Energy [J]

This field is the total amount of cooling, sensible plus latent, energy delivered to the zone in Joules.

#### Refrigeration Zone Air Chiller Heating Rate  [W]

This field is the rate of net heating due to any auxiliary or defrost heat input delivered to the zone,  in Watts.

#### Refrigeration Zone Air Chiller Heating Energy [J]

This field is the amount of net heating energy due to any auxiliary heat input, delivered to the zone in Joules.