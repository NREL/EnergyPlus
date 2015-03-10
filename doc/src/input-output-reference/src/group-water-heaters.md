# Group – Water Heaters

Water heater objects are components for storing and heating water. They can be coupled to a plant loop simulation or used stand-alone. Typical water heater applications are for domestic hot water heating, low-temperature radiant space heating, and energy storage for solar hot water systems or waste heat recovery.

When coupled to the plant loop, the water heater has an inlet node and outlet node on the "source side" and an inlet node and outlet node on the "use side". The source side typically draws cold water from the tank and returns warmer water, for instance, from solar hot water systems or waste heat recovery systems. The use side typically draws hot water from the tank and returns cooler water from the cold water supply mains or from the outlet of a heating system. The distinction between source and use sides is merely a convenience for reporting. They can actually be used interchangeably. If so desired, either source side or use side can be used by itself, without the other side being connected to the plant loop.

However, for a water heater that is indirectly heated (e.g. with a separate boiler), the source side can be used to provide remotely heated water to the tank.  The source side is configured to operate as a component on the demand side of a plant loop.  The design flow rate through the source side can be set by the user or autosized.  If autosized, then a Plant Sizing object is needed elsewhere in the input file for the Plant Loop serving the source side.  The water heater input includes an additional design parameter that describes how rapidly the tank can recover.

![Water Heater Configuration](media/water-heater-configuration.jpeg)


If the use side only consists of domestic hot water usage, a simple scheduled use flow rate can be specified in lieu of the full plant loop connections. The scheduled use flow rate can be used simultaneously with source side plant connections, but cannot be used with use side plant connections.

For stand-alone operation, there are no node connections to the plant loop on either source or use sides. The scheduled use flow rate determines all fluid exchange with the water tank.

There are currently two water heater objects in EnergyPlus:

- WaterHeater:Mixed
- WaterHeater:Stratified

There is also a compound object that uses the [WaterHeater:Mixed](#waterheatermixed) as part of its strategy:

- WaterHeater:HeatPump

The [WaterHeater:Mixed](#waterheatermixed) object simulates a well-mixed, single-node water tank. The [WaterHeater:Stratified](#waterheaterstratified) object simulates a stratified, multi-node water tank. Both water heater objects can be appropriate for simulating many types of water heaters and storage tanks, including gas and electric residential water heaters, and a variety of large commercial water heaters. Both objects share similar features, such as stand-alone operation, on- and off-cycle parasitic loads, and thermal losses to the zone. However, each object has its advantages which may make one water heater object more appropriate than the other depending on the application.

Advantages of **WaterHeater:Mixed**:

can simulate instantaneous/tankless water heaters

requires less input than the stratified tank

faster execution time than the stratified tank

adequate for modeling gas water heaters with no source connections.

Advantages of **WaterHeater:Stratified**:

better modeling of electric water heaters with two heating elements

better modeling of thermal storage applications which rely on stratification to improve heat transfer peformance.

## Standard Ratings

The EIO file reports the industry standard ratings of Recovery Efficiency and Energy Factor for water heater objects. The rating method is based on the GAMA and 10CFR430 test procedures. Under certain input parameters, the rating method will not succeed and a warning message will be generated. Problems occur when inputs do not allow the tank to recover to the setpoint temperature within the test period. This can occur if the maximum heater capacity is undersized, or if the deadband temperature difference is large enough that the first draw of the test does not trigger the heater to come on. In either case, the Recovery Efficiency test will not compute properly because recovery to the setpoint was not achieved.

Standard ratings for storage-only water tanks (Heater Maximum Capacity = 0) cannot be calculated and do not report anything in the EIO file.

## WaterHeater:Mixed

The [WaterHeater:Mixed](#waterheatermixed) object analytically solves the differential equation governing the energy balance of the water tank. Within a timestep, conditions are solved separately for when the heater element or burner is "on" (on-cycle) and when it is "off" (off-cycle). This approach allows ambient losses and parasitic loads to be divided into on-cycle and off-cycle effects and accounted for in detail.

For losses to the ambient environment, the ambient air temperature can be taken from a schedule, a zone, or the exterior. When used with a zone, a fraction of the skin losses can be added to the zone heat balance as internal heat gains.

Control options allow the heater to cycle or modulate to meet the load. When cycling, the heater element or burner is either on or off. The heater remains fully on while heating the tank up to the setpoint temperature. When the setpoint is reached, the heater turns off. The heater remains off until the tank temperature falls below the "cut-in" temperature, i.e., the setpoint temperature minus the deadband temperature difference. The heater continuously cycles on and off to maintain the tank temperature within the deadband. Most storage-tank water heaters cycle.

When modulating, the heater power varies between the maximum and minimum heater capacities. The heater stays on as long as the required total demand is above the minimum capacity. Below the minimum capacity, the heater will begin to cycle on and off based on the deadband temperature difference. Equipment is usually designed and rated to avoid this condition. Most tankless/instantaneous water heaters modulate.

### Inputs

#### Field: Name

The name of the [WaterHeater:Mixed](#waterheatermixed) object.

#### Field: Tank Volume

The volume of the storage tank [m^3^].  This field is autosizable if used with a Water Heater:Sizing object.  Although this field is allowed to go down to zero, even so-called "tankless" water heaters have some volume of water that is maintained around the heating elements or in the heat exchanger, typically around 0.00379 m^3^ (1 gallon).

#### Field: Setpoint Temperature Schedule Name

The reference to the schedule object specifying the hot water temperature setpoint [°C]. Also known as the "cut-out" temperature.

#### Field: Deadband Temperature Difference

The delta temperature difference [Δ°C] between the setpoint and the "cut-in" temperature at which the heater will turn on. In other words, the "cut-in" temperature is Setpoint – Deadband.

#### Field: Maximum Temperature Limit

The temperature [°C] at which the tank water becomes dangerously hot and is vented through boiling or an automatic safety. The tank temperature will never exceed the maximum. Any extra heat added to the tank is immediately vented. Note:  The maximum temperature must be greater than the setpoint temperature at all times.

#### Field: Heater Control Type

The control type can be **Cycle** or **Modulate**. Cycle is appropriate for most storage tank-type water heaters. Modulate is appropriate for most instantaneous/tankless water heaters.

#### Field: Heater Maximum Capacity

The maximum heat rate [W] that can be supplied to the water, probably the same as the "nominal" capacity.  This field is autosizable if used with a Water Heater:Sizing object.

#### Field: Heater Minimum Capacity

The minimum heat rate [W] that can be supplied to the water. This field is only used when the Heater Control Type is **Modulate**. If the total demand rate for heating is less than the minimum, even a modulating water heater will begin to cycle.

#### Field: Heater Ignition Minimum Flow Rate

NOT YET IMPLEMENTED.

#### Field: Heater Ignition Delay

NOT YET IMPLEMENTED.

#### Field: Heater Fuel Type

The type of fuel used for heating. The fuel type can be Electricity, NaturalGas, PropaneGas, FuelOil#1, FuelOil#2, Coal, Diesel, Gasoline, Steam, OtherFuel1, OtherFuel2 or [DistrictHeating](#districtheating).

#### Field: Heater Thermal Efficiency

The thermal conversion efficiency from fuel energy to heat energy for the heater element or burner. This is not the same as the overall efficiency of the water heater.

#### Field: Part Load Factor Curve Name

The reference to the curve object that relates the overall efficiency of the water heater to the Runtime Fraction (if Control Type **Cycle**) or Part Load Ratio (if Control Type **Modulate**). This is an additional multiplier applied to the Heater Thermal Efficiency to compute fuel energy use. The Part Load Factor Curve should not have a value less than 0.1 in the domain from 0 to 1. If the Part Load Factor Curve accounts for ambient losses and/or parasitic fuel consumption, these effects should not also be input into the related fields in this object as that would result in double-counting.

#### Field: Off-Cycle Parasitic Fuel Consumption Rate

Off-cycle parasitics include parts of the water heater that consume fuel when the heater is off, for example, a pilot light, or stand-by electronic control circuits. The fuel consumption rate [W] is strictly the total fuel that is consumed by all of the off-cycle parasitics.

#### Field: Off-Cycle Parasitic Fuel Type

The type of fuel used by the off-cycle parasitics. The fuel type can be Electricity, NaturalGas, PropaneGas, FuelOil#1, FuelOil#2, Coal, Diesel, Gasoline, Steam, OtherFuel1, OtherFuel2 or [DistrictHeating](#districtheating). The fuel type can be the same or different from the Heater Fuel Type.

#### Field: Off-Cycle Parasitic Heat Fraction to Tank

The fraction of off-cycle parasitic fuel energy that is converted to heat energy that ends up in the tank water. For example, a pilot light would deliver most of its heat to the tank water, as long as the thermal conversion efficiency must be taken into account, so perhaps 0.80 is reasonable. Electronic control circuits, on the other hand, do not add any heat to the tank and should be 0.

#### Field: On-Cycle Parasitic Fuel Consumption Rate

On-cycle parasitics include parts of the water heater that consume fuel when the heater is on, for example, an induction fan, or stand-by electronic control circuits. The fuel consumption rate [W] is strictly the total fuel that is consumed by all of the on-cycle parasitics.

#### Field: On-Cycle Parasitic Fuel Type

The type of fuel used by the on-cycle parasitics. The fuel type can be Electricity, NaturalGas, PropaneGas, FuelOil#1, FuelOil#2, Coal, Diesel, Gasoline, Steam, OtherFuel1, OtherFuel2 or [DistrictHeating](#districtheating). The fuel type can be the same or different from the Heater Fuel Type.

#### Field: On-Cycle Parasitic Heat Fraction to Tank

The fraction of on-cycle parasitic fuel energy that is converted to heat energy that ends up in the tank water. For example, an induction fan might (maybe) deliver a small fraction of its energy to the tank water for a value of 0.05. Electronic control circuits, on the other hand, do not add any heat to the tank and should be 0.

#### Field: Ambient Temperature Indicator

The Ambient Temperature Indicator specifies how the ambient air temperature will be indicated. The field can be **Schedule**, **[Zone](#zone)**, or **Outdoors**. If **Schedule** is used, the Ambient Temperature Schedule field provides the ambient temperature. If **[Zone](#zone)** is used, the zone air temperature of the zone specified in the Ambient Temperature [Zone](#zone) field provides the ambient temperature. If **Outdoors** is used, the outdoor dry-bulb air temperature provides the ambient temperature.

#### Field: Ambient Temperature Schedule Name

The reference to the schedule object specifying the ambient air temperature around the tank for skin losses. This field is only used if Ambient Temperature Indicator is **Schedule**.

#### Field: Ambient Temperature Zone Name

The reference to the zone object specifying the ambient air temperature around the tank for skin losses. This field is only used if Ambient Temperature Indicator is **Zone**.

#### Field: Ambient Temperature Outdoor Air Node Name

This optional alpha field specifies the outdoor air node name used to define the ambient conditions surrounding the water heater tank. This field is applicable only when the Ambient Temperature Indicator is specified as **Outdoors**, otherwise this field should be left blank. The node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air conditions from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air conditions are taken directly from the weather data.

#### Field: Off-Cycle Loss Coefficient to Ambient Temperature

The loss coefficient [W/K] to the ambient air temperature. Often this coefficient is identical to the "UA" for skin losses. However, it can also be used to model the loss effects of the flue in a combustion water heater, in addition to the skin losses.

#### Field: Off-Cycle Loss Fraction to Zone

If the Ambient Temperature Indicator is **Zone**, this field adds the specified fraction of the off-cycle losses to the zone heat balance as an internal gain.

#### Field: On-Cycle Loss Coefficient to Ambient Temperature

The loss coefficient [W/K] to the ambient air temperature. Often this coefficient is identical to the "UA" for skin losses. If the loss effects of the flue are being modeled in the Off-Cycle Loss Coefficient, than this field would have a different value accounting only for the skin losses.

#### Field: On-Cycle Loss Fraction to Zone

If the Ambient Temperature Indicator is **Zone**, this field adds the specified fraction of the on-cycle losses to the zone heat balance as an internal gain.

#### Field: Peak Use Flow Rate

The peak flow rate [m^3^/s] of domestic hot water usage for stand-alone operation, i.e., without plant loop node connections. The peak value is multiplied by the Use Flow Rate Fraction Schedule. If there are node connections, this field is not used.

#### Field: Use Flow Rate Fraction Schedule Name

The reference to the schedule object specifiying the current fraction of Peak Volumetric Use Flow Rate of domestic hot water usage for stand-alone operation.

#### Field: Cold Water Supply Temperature Schedule Name

The reference to the schedule object specifying the cold water temperature [°C] from the supply mains that makes up for the hot water lost down the drain. If blank, water temperatures are calculated by the [Site:WaterMainsTemperature](#sitewatermainstemperature) object. This field is for stand-alone operation only. If there are node connections, this field is not used.

#### Field: Use Side Inlet Node Name

The inlet node connection to the plant loop for the use side of the water heater. Typically the use side draws hot water from the tank and returns cooler water.

#### Field: Use Side Outlet Node Name

The outlet node connection to the plant loop for the use side of the water heater. Typically the use side draws hot water from the tank and returns cooler water.

#### Field: Use Side Effectiveness

This field specifies the heat transfer effectiveness between the use side water and the tank water. If the effectiveness is set to 1 then complete heat transfer occurs, simulating perfect mixing of the use side water and the tank water. If the effectiveness is lower, then the use side outlet water temperature will not be as hot as the tank water, simulating a heat exchanger.

#### Field: Source Side Inlet Node Name

The inlet node connection to the plant loop for the source side of the water heater. Typically the source side draws cold water from the tank and returns warmer water.  The source side volume flow rate is obtained from the plant loop.  The magnitude of the flow rates through the source side can be controlled by setting the Maximum [Branch](#branch) Flow Rate field in the **[Branch](#branch)** object that connects the source inlet node.

#### Field: Source Side Outlet Node Name

The outlet node connection to the plant loop for the source side of the water heater. Typically the source side draws cold water from the tank and returns warmer water.

#### Field: Source Side Effectiveness

This field specifies the heat transfer effectiveness between the source side water and the tank water. If the effectiveness is set to 1 then complete heat transfer occurs, simulating perfect mixing of the source side water and the tank water. If the effectiveness is lower, then the source side outlet water temperature will not be as hot as the tank water, simulating a heat exchanger.

#### Field: Use Side Design Flow Rate

This field is optional and is used to specify the design flow rate through the Use Side of the water heater.  The volumetric design flow rate is specified in m^3^/s.  The field is needed when the Use Side is connected to a plant loop.  The field can be autosized.  If autosized, then the input file should include a Plant Sizing object for the plant loop.  Sizing results are reported in the EIO file.

#### Field: Source Side Design Flow Rate

This field is optional and is used to specify the design flow rate through the Source Side of the water heater.  The volumetric design flow rate is specified in m^3^/s.  The field is needed when the Source Side is connected to a plant loop.  The field can be autosized.  If autosized, then the input file should include a Plant Sizing object for the plant loop.  Sizing results are reported in the EIO file.

#### Field: Indirect Water Heating Recovery Time

This field is optional and is used to provide a design parameter for autosizing design flow rates when the water heater is connected to the demand side of a plant loop.  The recovery time is expressed in hours.  This is the time that the entire volume of the tank can be heated from 14.4ºC to 57.2ºC (58ºF to 135ºF) with an inlet temperature defined as the exit temperature in the associated Plant Sizing object.  The default is 1.5 hours.  The calculation is based on log-mean temperature difference (LMTD) and includes the heat transfer effectiveness factor entered above.

#### Field: Source Side Flow Control Mode

This field is optional and is used to provide control over the logic used by the source side of the water heater to request flow. There are three choices for different modes: IndirectHeatPrimarySetpoint, IndirectHeatAlternateSetpoint, or StorageTank.    The mode called IndirectHeatPrimarySetpoint is the historical behavior prior to version 8.1.  In this mode the water heater will request flow at the source side when the main setpoint, in the input field called Setpoint Temperature Schedule Name, and deadband call for the tank to be heated.  This mode is typical for a water heater indirectly heated by a boiler.  The mode called IndirectHeatAlternateSetpoint is similar but it bases its control decisions on an alternate setpoint given in the following field.  This mode is useful when the indirect source of heat may not satisfy the load and an internal heater is used for backup.  The mode called StorageTank is for a passive tank and it always requests flow unless the tank temperature is equal to or higher than the maximum limit given in the input field called Maximum Temperature Limit.

#### Field: Indirect Alternate Setpoint Temperature Schedule Name

This field is optional and is used to provide a schedule with alternate setpoints for use with the IndirectHeatAlternateSetpoint mode in the previous field.  The input field should contain a reference to a schedule object specifying the hot water temperature setpoint [°C] to use as the "cut-out" temperature for control logic at the source side.

~~~~~~~~~~~~~~~~~~~~

    WaterHeater:Mixed,                     ! Stand-alone electric, outdoor example
        Outdoor Electric Tank,  !- Name
        0.151,  !- Tank Volume {m3}
        Hot Water Setpoint Temp Schedule,  !- Setpoint Temperature Schedule
        2.0,  !- Deadband Temperature Difference {deltaC}
        82.2222,  !- Maximum Temperature Limit {C}
        Cycle,  !- Heater Control Type {Cycle | Modulate}
        11712,  !- Heater Maximum Capacity {W}
        ,  !- Heater Minimum Capacity {W}
        ,  !- Heater Ignition Minimum Flow Rate {m3/s}
        ,  !- Heater Ignition Delay {s}
        ELECTRICITY,  !- Heater Fuel Type
        0.95,  !- Heater Thermal Efficiency
        ,  !- Part Load Factor Curve
        15,  !- Off-Cycle Parasitic Fuel Consumption Rate {W}
        ELECTRICITY,  !- Off-Cycle Parasitic Fuel Type
        0,  !- Off-Cycle Parasitic Heat Fraction To Tank
        15,  !- On-Cycle Parasitic Fuel Consumption Rate {W}
        ELECTRICITY,  !- On-Cycle Parasitic Fuel Type
        0,  !- On-Cycle Parasitic Heat Fraction To Tank
        Outdoors,  !- Ambient Temperature Indicator {Schedule | Zone | Outdoors}
        ,  !- Ambient Temperature Schedule
        ,  !- Ambient Temperature Zone
        2.36,  !- Off-Cycle Loss Coefficient To Ambient Temperature {W/K}
        ,  !- Off-Cycle Loss Fraction To Zone
        2.36,  !- On-Cycle Loss Coefficient To Ambient Temperature {W/K}
        ,  !- On-Cycle Loss Fraction To Zone
        0.000379,  !- Peak Volumetric Use Flow Rate {m3/s}
        Hot Water Demand Schedule,  !- Use Flow Rate Fraction Schedule
        Constant Mains Temp Schedule;  !- Cold Water Supply Temperature Schedule

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    WaterHeater:Mixed,                     ! Stand-alone electric, tankless example
        Tankless,  !- Name
        0.003785,  !- Tank Volume {m3}
        Hot Water Setpoint Temp Schedule,  !- Setpoint Temperature Schedule
        ,  !- Deadband Temperature Difference {deltaC}
        82.2222,  !- Maximum Temperature Limit {C}
        Modulate,  !- Heater Control Type {Cycle | Modulate}
        11712,  !- Heater Maximum Capacity {W}
        0,  !- Heater Minimum Capacity {W}
        ,  !- Heater Ignition Minimum Flow Rate {m3/s}
        ,  !- Heater Ignition Delay {s}
        ELECTRICITY,  !- Heater Fuel Type
        0.95,  !- Heater Thermal Efficiency
        ,  !- Part Load Factor Curve
        10,  !- Off-Cycle Parasitic Fuel Consumption Rate {W}
        ELECTRICITY,  !- Off-Cycle Parasitic Fuel Type
        0,  !- Off-Cycle Parasitic Heat Fraction To Tank
        30,  !- On-Cycle Parasitic Fuel Consumption Rate {W}
        ELECTRICITY,  !- On-Cycle Parasitic Fuel Type
        0,  !- On-Cycle Parasitic Heat Fraction To Tank
        Schedule,  !- Ambient Temperature Indicator {Schedule | Zone | Outdoors}
        Hot Water Ambient Temp Schedule,  !- Ambient Temperature Schedule
        ,  !- Ambient Temperature Zone
        ,  !- Off-Cycle Loss Coefficient To Ambient Temperature {W/K}
        ,  !- Off-Cycle Loss Fraction To Zone
        ,  !- On-Cycle Loss Coefficient To Ambient Temperature {W/K}
        ,  !- On-Cycle Loss Fraction To Zone
        0.000379,  !- Peak Volumetric Use Flow Rate {m3/s}
        Hot Water Demand Schedule,  !- Use Flow Rate Fraction Schedule
        ;  !- Cold Water Supply Temperature Schedule
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      WaterHeater:Mixed,                     ! Plant loop connected, gas example
        Water Heater,  !- Name
        0.454,  !- Tank Volume {m3}
        Hot Water Setpoint Temp Schedule,  !- Setpoint Temperature Schedule
        5.0,  !- Deadband Temperature Difference {deltaC}
        82.2222,  !- Maximum Temperature Limit {C}
        Cycle,  !- Heater Control Type {Cycle | Modulate}
        2000,  !- Heater Maximum Capacity {W}
        ,  !- Heater Minimum Capacity {W}
        ,  !- Heater Ignition Minimum Flow Rate {m3/s}
        ,  !- Heater Ignition Delay {s}
        NATURALGAS,  !- Heater Fuel Type
        0.80,  !- Heater Thermal Efficiency
        ,  !- Part Load Factor Curve
        ,  !- Off-Cycle Parasitic Fuel Consumption Rate {W}
        ,  !- Off-Cycle Parasitic Fuel Type
        ,  !- Off-Cycle Parasitic Heat Fraction To Tank
        ,  !- On-Cycle Parasitic Fuel Consumption Rate {W}
        ,  !- On-Cycle Parasitic Fuel Type
        ,  !- On-Cycle Parasitic Heat Fraction To Tank
        Schedule,  !- Ambient Temperature Indicator {Schedule | Zone | Outdoors}
        Hot Water Ambient Temp Schedule,  !- Ambient Temperature Schedule
        ,  !- Ambient Temperature Zone
        5.0,  !- Off-Cycle Loss Coefficient To Ambient Temperature {W/K}
        ,  !- Off-Cycle Loss Fraction To Zone
        5.0,  !- On-Cycle Loss Coefficient To Ambient Temperature {W/K}
        ,  !- On-Cycle Loss Fraction To Zone
        ,  !- Peak Volumetric Use Flow Rate {m3/s}
        ,  !- Use Flow Rate Fraction Schedule
        ,  !- Cold Water Supply Temperature Schedule
        Water Heater Use Inlet Node,  !- Use Side Inlet Node
        Water Heater Use Outlet Node,  !- Use Side Outlet Node
        1.0,  !- Use Side Effectiveness
        Water Heater Source Inlet Node,  !- Source Side Inlet Node
        Water Heater Source Outlet Node,  !- Source Side Outlet Node
        1.0;  !- Source Side Effectiveness
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    WaterHeater:Mixed,
        Indirect Water Heater,   !- Name
        1.00,                    !- Tank Volume {m3}
        Hot Water Setpoint Temperature Schedule,  !- Setpoint Temperature Schedule
        5.0,                     !- Deadband Temperature Difference {deltaC}
        82.2222,                 !- Maximum Temperature Limit {C}
        Cycle,                   !- Heater Control Type
        0.0,                     !- Heater Maximum Capacity {W}
        ,                        !- Heater Minimum Capacity {W}
        ,                        !- Heater Ignition Minimum Flow Rate {m3/s}
        ,                        !- Heater Ignition Delay {s}
        ELECTRICITY,             !- Heater Fuel Type
        0.8,                     !- Heater Thermal Efficiency
        ,                        !- Part Load Factor Curve
        ,                        !- Off-Cycle Parasitic Fuel Consumption Rate {W}
        ,                        !- Off-Cycle Parasitic Fuel Type
        ,                        !- Off-Cycle Parasitic Heat Fraction To Tank
        ,                        !- On-Cycle Parasitic Fuel Consumption Rate {W}
        ,                        !- On-Cycle Parasitic Fuel Type
        ,                        !- On-Cycle Parasitic Heat Fraction To Tank
        Zone,                    !- Ambient Temperature Indicator
        ,                        !- Ambient Temperature Schedule
        SPACE5-1,                !- Ambient Temperature Zone
        ,                        !- Ambient Temperature Outside Air Node
        5.0,                     !- Off-Cycle Loss Coefficient To Ambient Temperature {W/K}
        ,                        !- Off-Cycle Loss Fraction To Zone
        5.0,                     !- On-Cycle Loss Coefficient To Ambient Temperature {W/K}
        ,                        !- On-Cycle Loss Fraction To Zone
        ,                        !- Peak Volumetric Use Flow Rate {m3/s}
        ,                        !- Use Flow Rate Fraction Schedule
        ,                        !- Cold Water Supply Temperature Schedule
        SHWSys1 Pump-SHWSys1 Water HeaterNode,  !- Use Side Inlet Node
        SHWSys1 Supply Equipment Outlet Node,  !- Use Side Outlet Node
        1.0,                     !- Use Side Effectiveness
        Indirect Water Heater SrcSideInletNode,  !- Source Side Inlet Node
        Indirect Water Heater SrcSideOutletNode,  !- Source Side Outlet Node
        0.9,                     !- Source Side Effectiveness
        autosize,                !- Use Side Design Flow Rate
        autosize,                !- Source Side Design Flow Rate
        1.0;                     !- Indirect Water Heating Recovery Time
~~~~~~~~~~~~~~~~~~~~

### Outputs

The following output variables are reported for the [WaterHeater:Mixed](#waterheatermixed) object:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Water Heater Tank Temperature [C]
    HVAC,Average,Water Heater Final Tank Temperature [C]
    HVAC,Average,Water Heater Heat Loss Rate [W]
    HVAC,Sum,Water Heater Heat Loss Energy [J]
    HVAC,Average, Water Heater Use Side Mass Flow Rate [kg/s]
    HVAC,Average,Water Heater Use Side Inlet Temperature [C]
    HVAC,Average,Water Heater Use Side Outlet Temperature [C]
    HVAC,Average,Water Heater Use Side Heat Transfer Rate [W]
    HVAC,Sum,Water Heater Use Side Heat Transfer Energy [J]
    HVAC,Average,Water Heater Source Side Mass Flow Rate [kg/s]
    HVAC,Average,Water Heater Source Side Inlet Temperature [C]
    HVAC,Average,Water Heater Source Side Outlet Temperature [C]
    HVAC,Average,Water Heater Source Side Heat Transfer Rate [W]
    HVAC,Sum,Water Heater Source Side Heat Transfer Energy [J]
    HVAC,Average,Water Heater Off Cycle Parasitic Tank Heat Transfer Rate [W]
    HVAC,Sum,Water Heater Off Cycle Parasitic Tank Heat Transfer Energy [J]
    HVAC,Average,Water Heater On Cycle Parasitic Tank Heat Transfer Rate [W]
    HVAC,Sum,Water Heater On Cycle Parasitic Tank Heat Transfer Energy [J]
    HVAC,Average,Water Heater Total Demand Heat Transfer Rate [W]
    HVAC,Sum,Water Heater Total Demand Energy [J]
    HVAC,Average,Water Heater Heating Rate [W]
    HVAC,Sum,Water Heater Heating Energy [J]
    HVAC,Average,Water Heater Unmet Demand Heat Transfer Rate [W]
    HVAC,Sum,Water Heater Unmet Demand Heat Transfer Energy [J]
    HVAC,Average,Water Heater Venting Heat Transfer Rate [W]
    HVAC,Sum,Water Heater Venting Heat Transfer Energy [J]
    HVAC,Average,Water Heater Net Heat Transfer Rate [W]
    HVAC,Sum,Water Heater Net Heat Transfer Energy [J]
    HVAC,Sum,Water Heater Cycle On Count []
    HVAC,Average,Water Heater Runtime Fraction []
    HVAC,Average,Water Heater Part Load Ratio []
    HVAC,Average,Water Heater Electric Power [W]
    HVAC,Average,Water Heater <Fuel Type> Rate [W]
    HVAC,Sum,Water Heater <Fuel Type> Energy [J]
    HVAC,Average,Water Heater Off Cycle Parasitic <Fuel Type> Rate [W]
    HVAC,Sum,Water Heater Off Cycle Parasitic <Fuel Type> Energy [J]
    HVAC,Average,Water Heater On Cycle Parasitic <Fuel Type> Rate [W]
    HVAC,Sum,Water Heater On Cycle Parasitic <Fuel Type> Energy [J]
    HVAC,Average,Water Heater Water Volume Flow Rate [m3/s]
    HVAC,Sum,Water Heater Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Water Heater Tank Temperature [C]

The average water tank temperature.

#### Water Heater Final Tank Temperature [C]

The final water tank temperature at the end of the system timestep. If reported at the "Detailed" interval, this output variable can be used to verify an exact energy balance on the water heater. Also see the output variable:  Water Heater Net Heat Transfer Energy.

#### Water Heater Heat Loss Rate [W]

The average heat loss rate due to the off- and on-cycle loss coefficients to the ambient temperature.

#### Water Heater Heat Loss Energy [J]

The heat loss energy due to the off- and on-cycle loss coefficients to the ambient temperature.

#### Water Heater Use Side Mass Flow Rate [kg/s]

The use side mass flow rate. If stand-alone, this is the scheduled use flow rate.

#### Water Heater Use Side Inlet Temperature [C]

The inlet temperature on the use side.

#### Water Heater Use Side Outlet Temperature [C]

The outlet temperature on the use side.

#### Water Heater Use Side Heat Transfer Rate [W]

The average heat transfer rate between the use side water and the tank water.

#### Water Heater Use Side Heat Transfer Energy [J]

The heat transfer energy between the use side water and the tank water.

#### Water Heater Source Side Mass Flow Rate [kg/s]

The source side mass flow rate. If in stand-alone operation, this is 0.

#### Water Heater Source Side Inlet Temperature [C]

The inlet temperature on the source side.

#### Water Heater Source Side Outlet Temperature [C]

The outlet temperature on the source side.

#### Water Heater Source Side Heat Transfer Rate [W]

The average heat transfer rate between the source side water and the tank water.

#### Water Heater Source Side Heat Transfer Energy [J]

The heat transfer energy between the source side water and the tank water.

#### Water Heater Off Cycle Parasitic Tank Heat Transfer Rate [W]

The average heat gain rate to the tank water due to off-cycle parasitics.

#### Water Heater Off Cycle Parasitic Tank Heat Transfer Energy [J]

The heat gain energy to the tank water due to off-cycle parasitics.

#### Water Heater On Cycle Parasitic Tank Heat Transfer Rate [W]

The average heat gain rate to the tank water due to on-cycle parasitics.

#### Water Heater On Cycle Parasitic Tank Heat Transfer Energy [J]

The heat gain energy to the tank water due to on-cycle parasitics.

#### Water Heater Total Demand Heat Transfer Rate [W]

The average heating rate demanded to maintain the setpoint temperature.

#### Water Heater Total Demand Energy [J]

The heating energy demanded to maintain the setpoint temperature.

#### Water Heater Heating Rate [W]

The average heating rate supplied by the heater element or burner.

#### Water Heater Heating Energy [J]

The heating energy supplied by the heater element or burner.

#### Water Heater Unmet Demand Heat Transfer Rate [W]

The average heating rate unmet by the heater element or burner. The difference between the Total Demand Rate and the Heating Rate.

#### Water Heater Unmet Demand Heat Transfer Energy [J]

The heating energy unmet by the heater element or burner. The difference between the Total Demand Energy and the Heating Energy.

#### Water Heater Venting Heat Transfer Rate [W]

The average venting rate to keep the tank below the Maximum Temperature Limit.

#### Water Heater Venting Heat Transfer Energy [J]

The venting energy to keep the tank below the Maximum Temperature Limit.

#### Water Heater Net Heat Transfer Rate [W]

The average net heat transfer rate when considering all losses and gains.

#### Water Heater Net Heat Transfer Energy [J]

The net heat transfer energy when considering all losses and gains.

#### Water Heater Cycle On Count []

The number of times that the heater turned on in the time period.

#### Water Heater Runtime Fraction []

The fraction of the time period that the heater was running.

#### Water Heater Part Load Ratio []

The fraction of the Heater Maximum Capacity.

#### Water Heater <Fuel Type> Rate [W]

#### Water Heater Electric Power [W]

The average fuel consumption rate for the heater element or burner.

#### Water Heater <Fuel Type> Energy [J]

The fuel consumption energy for the heater element or burner.

#### Water Heater Off Cycle Parasitic <Fuel Type> Rate [W]

#### Water Heater Off Cycle Parasitic Electric Power [W]

The average fuel consumption rate for the off-cycle parasitics.

#### Water Heater Off Cycle Parasitic <Fuel Type> Energy [J]

The fuel consumption energy for the off-cycle parasitics.

#### Water Heater On Cycle Parasitic <Fuel Type> Rate [W]

#### Water Heater On Cycle Parasitic Electric Power [W]

The average fuel consumption rate for the on-cycle parasitics.

#### Water Heater On Cycle Parasitic <Fuel Type> Energy [J]

The fuel consumption energy for the on-cycle parasitics.

#### Water Heater Water Volume Flow Rate [m^3^/s]

The water consumption rate for the use side, if in stand-alone operation.

#### Water Heater Water Volume [m^3^]

The water consumption for the use side, if in stand-alone operation.

#### Water Heater Mains Water Volume [m3]

The volume of water consumption drawn from mains water service.

## WaterHeater:Stratified

The [WaterHeater:Stratified](#waterheaterstratified) object divides the water tank into multiple nodes of equal volume. The nodes are coupled by vertical conduction effects, internode fluid flow, and temperature inversion mixing. The object simultaneously solves the differential equations governing the energy balances on the nodes using a numerical method. The system timestep is divided into many small substeps that allow the simulation to capture events that occur on a very short time scale. This approach allows ambient losses and parasitic loads to be divided into on-cycle and off-cycle effects and accounted for in detail.

For losses to the ambient environment, the ambient air temperature can be taken from a schedule, a zone, or the exterior. When used with a zone, a fraction of the skin losses can be added to the zone heat balance as internal heat gains.

The [WaterHeater:Stratified](#waterheaterstratified) object allows two heating elements to be simulated. The two elements can cycle on and off to maintain the node temperature within the deadband. The Heater Priority Control field determines how the heaters work together. There are two options:  MasterSlave or Simultaneous. In the MasterSlave option, Heater 1 is the master and Heater 2 is the slave. That is, both heaters are not allowed to turn on at the same time. If the thermostats ask for heat at both Heater 1 and 2, only Heater 1 will turn on. Once Heater 1 has met the set point, it turns off and Heater 2 can turn on, if necessary. In the Simultaneousoption, Heater 1 and Heater 2 can turn on and off independently.  Autosizing is available for only Heater 1.

### Inputs

#### Field: Name

The name of the [WaterHeater:Stratified](#waterheaterstratified) object.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Laundry", "Dish Washing", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table under the "Water Systems" end-use category. If this field is omitted or blank, the water use will be assigned to the "General" end-use subcategory.

#### Field: Tank Volume

The actual volume [m3] of fluid in the tank. This field is autosizable if used with a Water Heater:Sizing object.  The actual volume is typically not equal to the nominal volume specified by the manufacturer. Actual volume is almost always 10% lower for electric water heaters, and 5% lower for gas water heaters (Burch and Erickson 2004).

Burch, J., and P. Erickson. 2004. "Using Ratings Data to Derive Simulation-Model Inputs for Storage-Tank Water Heaters". *Proceedings of the Solar 2004 Conference, 11-14 July 2004, Portland, Oregon*, American Solar Energy Society (ASES), pp. 393-398.

#### Field: Tank Height

The height [m] of the tank. For the **HorizontalCylinder** shape (see below) the height of the tank is the measure in the axial direction, i.e., the height if you were to stand the cylinder up on its end. This field is autosizable if used with a Water Heater:Sizing object.

#### Field: Tank Shape

The tank shape determines the size and skin losses of the stratified nodes. There are three options:  **VerticalCylinder**, **HorizontalCylinder**, and **Other**.

**VerticalCylinder** describes most upright residential water heaters.

HorizontalCylinder describes a few specialty water heaters and large commercial storage tanks. HorizontalCylinder can also be used to model an outdoor storage tank located above a solar collector in a thermosiphon configuration. HorizontalCylinder implies that the tank is divided into nodes of equal mass, but not equal height.

Other ****describes water heaters or storage tanks that have a uniform horizontal cross-section, but are not cylinders, e.g., a cuboid or other shape. The length of the perimeter is then specified by the Tank Perimeter field.

If blank, the default shape is **VerticalCylinder**.

#### Field: Tank Perimeter

The length of the tank perimeter [m]. This field is only used if Tank Shape is **Other**.

#### Field: Maximum Temperature Limit

The temperature [°C] at which the tank water becomes dangerously hot and is vented through boiling or an automatic safety. The tank temperature will never exceed the maximum. Any extra heat added to the tank is immediately vented. Note:  The maximum temperature must be greater than the setpoint temperature at all times.

#### Field: Heater Priority Control

The heater priority control determines how Heater 1 and Heater 2 work together. There are two options:  **MasterSlave** or **Simultaneous**. In the MasterSlave option, Heater 1 is the master and Heater 2 is the slave. In most residential electric water heaters, the heaters operate in a MasterSlave relationship. That is, both heaters are not allowed to turn on at the same time. If the thermostats ask for heat at both Heater 1 and 2, only Heater 1 will turn on. Once Heater 1 has met the set point, it turns off and Heater 2 can turn on, if necessary. In other words, only one heater can be on at any time, and Heater 1 is always has priority over Heater 2.

In the Simultaneous option, Heater 1 and Heater 2 can turn on and off independently.

If blank, the default is **MasterSlave**.

#### Field: Heater 1 Set Point Temperature Schedule Name

The reference to the schedule object specifying the hot water temperature set point [°C] for Heater 1. Also known as the "cut-out" temperature.

#### Field: Heater 1 Deadband Temperature Difference

The delta temperature difference [Δ°C] between the set point and the "cut-in" temperature at which Heater 1 will turn on. In other words, the "cut-in" temperature is Set Point – Deadband.

#### Field: Heater 1 Capacity

The heat rate [W] supplied to the water for Heater 1, probably the same as the "nominal" capacity. For residential electric water heaters, heating elements are usually 4500 W.  This field is autosizable if used with a Water Heater:Sizing object.

#### Field: Heater 1 Height

The height [m] of Heater 1 in the tank.

#### Field: Heater 2 Set Point Temperature Schedule Name

The reference to the schedule object specifying the hot water temperature set point [°C] for Heater 2. Also known as the "cut-out" temperature.

#### Field: Heater 2 Deadband Temperature Difference

The delta temperature difference [Δ°C] between the set point and the "cut-in" temperature at which Heater 2 will turn on. In other words, the "cut-in" temperature is Set Point – Deadband.

#### Field: Heater 2 Capacity

The heat rate [W] supplied to the water for Heater 1, probably the same as the "nominal" capacity. For residential electric water heaters, heating elements are usually 4500 W.

#### Field: Heater 2 Height

The height [m] of Heater 2 in the tank.

#### Field: Heater Fuel Type

The type of fuel used for both Heaters 1 and 2. The fuel type can be Electricity, NaturalGas, PropaneGas, FuelOil#1, FuelOil#2, Coal, Diesel, Gasoline, Steam, OtherFuel1, OtherFuel2 or [DistrictHeating](#districtheating).

#### Field: Heater Thermal Efficiency

The thermal conversion efficiency from fuel energy to heat energy for the heater element or burner (for both Heaters 1 and 2). This is not the same as the overall efficiency of the water heater.

#### Field: Off-Cycle Parasitic Fuel Consumption Rate

Off-cycle parasitics include parts of the water heater that consume fuel when the heater is off, for example, a pilot light, or stand-by electronic control circuits. The fuel consumption rate [W] is strictly the total fuel that is consumed by all of the off-cycle parasitics.

#### Field: Off-Cycle Parasitic Fuel Type

The type of fuel used by the off-cycle parasitics. The fuel type can be Electricity, NaturalGas, PropaneGas, FuelOil#1, FuelOil#2, Coal, Diesel, Gasoline, Steam, OtherFuel1, OtherFuel2 or [DistrictHeating](#districtheating). The fuel type can be the same or different from the Heater Fuel Type.

#### Field: Off-Cycle Parasitic Heat Fraction to Tank

The fraction of off-cycle parasitic fuel energy that is converted to heat energy that ends up in the tank water. For example, a pilot light would deliver most of its heat to the tank water, as long as the thermal conversion efficiency must be taken into account, so perhaps 0.80 is reasonable. Electronic control circuits, on the other hand, do not add any heat to the tank and should be 0.

#### Field: Off-Cycle Parasitic Height

The height [m] where any off-cycle parasitic heat gains are added to the tank.

#### Field: On-Cycle Parasitic Fuel Consumption Rate

On-cycle parasitics include parts of the water heater that consume fuel when the heater is on, for example, an induction fan, or stand-by electronic control circuits. The fuel consumption rate [W] is strictly the total fuel that is consumed by all of the on-cycle parasitics.

#### Field: On-Cycle Parasitic Fuel Type

The type of fuel used by the on-cycle parasitics. The fuel type can be Electricity, NaturalGas, PropaneGas, FuelOil#1, FuelOil#2, Coal, Diesel, Gasoline, Steam, OtherFuel1, OtherFuel2 or [DistrictHeating](#districtheating). The fuel type can be the same or different from the Heater Fuel Type.

#### Field: On-Cycle Parasitic Heat Fraction to Tank

The fraction of on-cycle parasitic fuel energy that is converted to heat energy that ends up in the tank water. For example, an induction fan might (maybe) deliver a small fraction of its energy to the tank water for a value of 0.05. Electronic control circuits, on the other hand, do not add any heat to the tank and should be 0.

#### Field: On-Cycle Parasitic Height

The height [m] where any on-cycle parasitic heat gains are added to the tank.

#### Field: Ambient Temperature Indicator

The Ambient Temperature Indicator specifies how the ambient air temperature will be indicated. The field can be **Schedule**, **[Zone](#zone)**, or **Outdoors**. If **Schedule** is used, the Ambient Temperature Schedule value provides the ambient temperature. If **[Zone](#zone)** is used, the zone air temperature of the zone specified in the Ambient Temperature [Zone](#zone) field provides the ambient temperature. If **Outdoors** is used, the outdoor dry-bulb air temperature of the outdoor air node specified in the Ambient Temperature [OutdoorAir:Node](#outdoorairnode) field provides the ambient temperature.

#### Field: Ambient Temperature Schedule Name

The reference to the schedule object specifying the ambient air temperature around the tank for skin losses. This field is only used if Ambient Temperature Indicator is **Schedule**.

#### Field: Ambient Temperature Zone

The reference to the zone object specifying the ambient air temperature around the tank for skin losses. This field is only used if Ambient Temperature Indicator is **Zone**.

#### Field: Ambient Temperature Outdoor Air Node

The reference to the outdoor air node specifying the ambient air temperature around the tank for skin losses. An outdoor air node can be defined by an [OutdoorAir:Node](#outdoorairnode) object or [OutdoorAir:NodeList](#outdoorairnodelist) object. This field is only used if Ambient Temperature Indicator is **Outdoors**.

#### Field: Uniform Skin Loss Coefficient Per Unit Area to Ambient Temperature

The uniform skin loss coefficient [W/m2-K] or U-Value of the tank to the ambient air temperature. The uniform skin loss accounts for the tank insulation and applies during both off- and on-cycle operation. The overall losses at any particular node can be further modified using the Additional Loss Coefficient fields to account for thermal shorting due to pipe penetrations, water heater feet, and any other loss effects.

#### Field: Skin Loss Fraction to Zone

If the Ambient Temperature Indicator is **Zone**, this field adds the specified fraction of the skin losses to the zone heat balance as an internal gain.

#### Field: Off-Cycle Flue Loss Coefficient to Ambient Temperature

The off-cycle flue loss coefficient [W/K] to the ambient air temperature. This field mainly applies to gas water heaters that have a flue.

#### Field: Off-Cycle Flue Loss Fraction to Zone

If the Ambient Temperature Indicator is **Zone**, this field adds the specified fraction of the off-cycle flue losses to the zone heat balance as an internal gain.

#### Field: Peak Use Flow Rate

The peak flow rate [m^3^/s] of domestic hot water usage for stand-alone operation, i.e., without plant loop node connections. The peak value is multiplied by the Use Flow Rate Fraction Schedule. If there are node connections, this field is not used.

#### Field: Use Flow Rate Fraction Schedule Name

The reference to the schedule object specifiying the current fraction of Peak Volumetric Use Flow Rate of domestic hot water usage for stand-alone operation. If blank, the fraction defaults to 1.0 at all times.

#### Field: Cold Water Supply Temperature Schedule Name

The reference to the schedule object specifying the cold water temperature [°C] from the supply mains that makes up for the hot water lost down the drain. If blank, water temperatures are calculated by the [Site:WaterMainsTemperature](#sitewatermainstemperature) object. This field is for stand-alone operation only. If there are node connections, this field is not used.

#### Field: Use Side Inlet Node Name

The inlet node connection to the plant loop for the use side of the water heater. Typically the use side draws hot water from the tank and returns cooler water.

#### Field: Use Side Outlet Node Name

The outlet node connection to the plant loop for the use side of the water heater. Typically the use side draws hot water from the tank and returns cooler water.

#### Field: Use Side Effectiveness

This field specifies the heat transfer effectiveness between the use side water and the tank water. If the effectiveness is set to 1 then complete heat transfer occurs, simulating perfect mixing of the use side water and the tank water. If the effectiveness is less than 1.0, then the use side outlet water temperature will not be as hot as the tank water at the outlet node, simulating an external heat exchanger that is indirectly coupled to the water heater tank.

#### Field: Use Side Inlet Height

The height of the use side inlet to the tank. If blank, the inlet defaults to the bottom of the tank. The inlet height cannot be higher than the tank height.

#### Field: Use Side Outlet Height

The height of the use side outlet from the tank. If blank or **autocalculate**, the inlet defaults to the top of the tank. The outlet height cannot be higher than the tank height.

#### Field: Source Side Inlet Node

The inlet node connection to the plant loop for the source side of the water heater. Typically the source side draws cold water from the tank and returns warmer water.

#### Field: Source Side Outlet Node

The outlet node connection to the plant loop for the source side of the water heater. Typically the source side draws cold water from the tank and returns warmer water.

#### Field: Source Side Effectiveness

This field specifies the heat transfer effectiveness between the source side water and the tank water. If the effectiveness is set to 1 then complete heat transfer occurs, simulating perfect mixing of the source side water and the tank water. If the effectiveness is less than 1.0, then the source side outlet water temperature will be higher than the tank water at the outlet node, simulating an external heat exchanger that is indirectly coupled to the water heater tank.

#### Field: Source Side Inlet Height

The height of the source side inlet to the tank. If blank or **autocalculate**, the inlet defaults to the top of the tank. The inlet height cannot be higher than the tank height.

#### Field: Source Side Outlet Height

The height of the source side outlet from the tank. If blank, the inlet defaults to the bottom of the tank. The outlet height cannot be higher than the tank height.

#### Field: Inlet Mode

The inlet mode of entering fluid from the use and source sides. There are two options:  **Fixed** or **Seeking**. In Fixed mode, the fluid enters at the fixed heights specified above. In Seeking mode, the fluid "seeks out" the stratified node that is closest to the inlet temperature and adds all flow to that node. The Seekingbmode provides maximum stratification. The default is **Fixed**.

#### Field: Use Side Design Flow Rate

This field is optional and is used to specify the design flow rate through the Use Side of the water heater.  The volumetric design flow rate is specified in m^3^/s.  The field is needed when the Use Side is connected to a plant loop.  The field can be autosized.  If autosized, then the input file should include a Plant Sizing object for the plant loop.  Sizing results are reported in the EIO file.

#### Field: Source Side Design Flow Rate

This field is optional and is used to specify the design flow rate through the Source Side of the water heater.  The volumetric design flow rate is specified in m^3^/s.  The field is needed when the Source Side is connected to a plant loop.  The field can be autosized.  If autosized, then the input file should include a Plant Sizing object for the plant loop.  Sizing results are reported in the EIO file.

#### Field: Indirect Water Heating Recovery Time

This field is optional and is used to provide a design parameter for autosizing design flow rates when the water heater is connected to the demand side of a plant loop.  The recovery time is expressed in hours.  This is the time that the entire volume of the tank can be heated from 14.4ºC to 57.2ºC (58ºF to 135ºF) with an inlet temperature defined as the exit temperature in the associated Plant Sizing object.  The default is 1.5 hours.  The calculation is based on log-mean temperature difference (LMTD) and includes the heat transfer effectiveness factor entered above.

#### Field: Number Of Nodes

The number of stratified nodes in the tank. There must be at least one node. The maximum number of nodes is 10, although this limit can be increased by editing the IDD.

#### Field: Additional Destratification Conductivity

An additional destratification conductivity [W/m-K] is added to the fluid conductivity of water (0.6 W/m-K) to account for vertical conduction effects along the inside of the tank wall, and perhaps other vertical components such as the flue, the cold water inlet pipe (dip tube), and the anode rod.

#### Field: Node 1-10 Additional Loss Coefficient

An additional loss coefficient [W/m-K] added to the skin losses for a given node to account for thermal shorting due to pipe penetrations, water heater feet, and any other loss effects.

#### Field: Source Side Flow Control Mode

This field is optional and is used to provide control over the logic used by the source side of the water heater to request flow. There are three choices for different modes: IndirectHeatPrimarySetpoint, IndirectHeatAlternateSetpoint, or StorageTank.    The mode called IndirectHeatPrimarySetpoint is the historical behavior prior to version 8.1.  In this mode the water heater will request flow at the source side when the main setpoint, in the input field called Setpoint Temperature Schedule Name, and deadband call for the tank to be heated.  This mode is typical for a water heater indirectly heated by a boiler.  The mode called IndirectHeatAlternateSetpoint is similar but it bases its control decisions on an alternate setpoint given in the following field.  This mode is useful when the indirect source of heat may not satisfy the load and an internal heater is used for backup.  The mode called StorageTank is for a passive tank and it always requests flow unless the tank temperature is equal to or higher than the maximum limit given in the input field called Maximum Temperature Limit.

#### Field: Indirect Alternate Setpoint Temperature Schedule Name

This field is optional and is used to provide a schedule with alternate setpoints for use with the IndirectHeatAlternateSetpoint mode in the previous field.  The input field should contain a reference to a schedule object specifying the hot water temperature setpoint [°C] to use as the "cut-out" temperature for control logic at the source side.

~~~~~~~~~~~~~~~~~~~~

    WaterHeater:Stratified,
        Electric Water Heater,  !- Name
        Water Heater,  !- End-Use Subcategory
        0.1893,  !- Tank Volume {m3}
        1.4,  !- Tank Height {m}
        VerticalCylinder,  !- Tank Shape
        ,  !- Tank Perimeter {m}
        82.2222,  !- Maximum Temperature Limit {C}
        MasterSlave,  !- Heater Priority
        Hot Water Set Point Temp Schedule,  !- Heater 1 Set Point Temperature Schedule
        2.0,  !- Heater 1 Deadband Temperature Difference {deltaC}
        4500,  !- Heater 1 Capacity {W}    (Master)
        1.0,  !- Heater 1 Height {m}
        Hot Water Set Point Temp Schedule,  !- Heater 2 Set Point Temperature Schedule
        5.0,  !- Heater 2 Deadband Temperature Difference {deltaC}
        4500,  !- Heater 2 Capacity {W}    (Slave)
        0.0,  !- Heater 2 Height {m}
        ELECTRICITY,  !- Heater Fuel Type
        0.98,  !- Heater Thermal Efficiency
        10,  !- Off-Cycle Parasitic Fuel Consumption Rate {W}
        ELECTRICITY,  !- Off-Cycle Parasitic Fuel Type
        0,  !- Off-Cycle Parasitic Heat Fraction To Tank
        ,  !- Off-Cycle Parasitic Height {m}
        10,  !- On-Cycle Parasitic Fuel Consumption Rate {W}
        ELECTRICITY,  !- On-Cycle Parasitic Fuel Type
        0,  !- On-Cycle Parasitic Heat Fraction To Tank
        ,  !- On-Cycle Parasitic Height {m}
        SCHEDULE,  !- Ambient Temperature Indicator
        Ambient Temp Schedule,  !- Ambient Temperature Schedule
        ,  !- Ambient Temperature Zone
        ,  !- Ambient Temperature Outdoor Air Node
        0.846,  !- Uniform Skin Loss Coefficient Per Unit Area To Ambient Temperature {W/m2-K}
        ,  !- Skin Loss Fraction To Zone {}
        ,  !- Off-Cycle Flue Loss Coefficient To Ambient Temperature {W/K}
        ,  !- Off-Cycle Flue Loss Fraction To Zone {}
        ,  !- Peak Volumetric Use Flow Rate {m3/s}
        ,  !- Use Flow Rate Fraction Schedule
        ,  !- Cold Water Supply Temperature Schedule
        Water Heater Use Inlet Node,  !- Use Side Inlet Node
        Water Heater Use Outlet Node,  !- Use Side Outlet Node
        1.0,  !- Use Side Effectiveness {}
        ,  !- Use Side Inlet Height {m}
        ,  !- Use Side Outlet Height {m}
        ,  !- Source Side Inlet Node
        ,  !- Source Side Outlet Node
        ,  !- Source Side Effectiveness {}
        ,  !- Source Side Inlet Height {m}
        ,  !- Source Side Outlet Height {m}
        FIXED,  !- Inlet Mode {FIXED | SEEKING}
        6,  !- Number Of Nodes
        0.1,  !- Destratification Conductivity {W/m-K}
        0.15,  !- Node 1 Additional Loss Coefficient {W/K}
        ,  !- Node 2 Additional Loss Coefficient {W/K}
        ,  !- Node 3 Additional Loss Coefficient {W/K}
        ,  !- Node 4 Additional Loss Coefficient {W/K}
        ,  !- Node 5 Additional Loss Coefficient {W/K}
        0.1;  !- Node 6 Additional Loss Coefficient {W/K}
~~~~~~~~~~~~~~~~~~~~

### Outputs

All of the output variables reported for the [WaterHeater:Mixed](#waterheatermixed) object also apply to the [WaterHeater:Stratified](#waterheaterstratified) object, with several qualifications noted below:

#### Water Heater Tank Temperature [C]

The average water tank temperature, i.e., the average of all of the node average temperatures.

#### Water Heater Final Tank Temperature [C]

The final water tank temperature at the end of the system timestep, i.e., the average of all of the final node temperatures at the end of the system timestep. If reported at the "Detailed" interval, this output variable can be used to verify an exact energy balance on the water heater.

#### Water Heater Heating Rate [W]

The total average heating rate supplied by Heater 1 plus Heater 2.

#### Water Heater Heating Energy [J]

The total heating energy supplied by Heater 1 plus Heater 2.

#### Water Heater Cycle On Count []

The number of times that either Heater 1 or Heater 2 turned on in the time period.

#### Water Heater Runtime Fraction []

The fraction of the time period that either Heater 1 or Heater 2 was running.

In addition, the [WaterHeater:Stratified](#waterheaterstratified) object also reports the following output variables:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Water Heater Heater 1 Heating Rate [W]
    HVAC,Average,Water Heater Heater 2 Heating Rate [W]
    HVAC,Sum,Water Heater Heater 1 Heating Energy [J]
    HVAC,Sum,Water Heater Heater 2 Heating Energy [J]
    HVAC,Sum,Water Heater Heater 1 Cycle On Count []
    HVAC,Sum,Water Heater Heater 2 Cycle On Count  []
    HVAC,Average,Water Heater Heater 1 Runtime Fraction []
    HVAC,Average,Water Heater Heater 2 Runtime Fraction []
    HVAC,Average,Water Heater Temperature Node <x> [C]
    HVAC,Average,Water Heater Final Temperature Node <x> [C]
~~~~~~~~~~~~~~~~~~~~

#### Water Heater Heater 1 Heating Rate [W]

The average heating rate supplied by Heater 1.

#### Water Heater Heater 2 Heating Rate [W]

The average heating rate supplied by Heater 2.

#### Water Heater Heater 1 Heating Energy [J]

The heating energy supplied by Heater 1.

#### Water Heater Heater 2 Heating Energy [J]

The heating energy supplied by Heater 2.

#### Water Heater Heater 1 Cycle On Count []

The number of times that Heater 1 turned on in the time period.

#### Water Heater Heater 2 Cycle On Count  []

The number of times that Heater 2 turned on in the time period.

#### Water Heater Heater 1 Runtime Fraction []

The fraction of the time period that Heater 1 was running.

#### Water Heater Heater 2 Runtime Fraction []

The fraction of the time period that Heater 2 was running.

#### Water Heater Temperature Node 1-10 [C]

The average node temperature.

#### Water Heater Final Temperature Node 1-10 [C]

The final node temperature at the end of the system timestep.

## WaterHeater:Sizing

The [WaterHeater:Sizing](#waterheatersizing) object is used to provide additional input data needed for designing tank volume and/or heater capacity for either the Mixed or Stratified water heaters.  This object is only needed if volume or capacity is being automatically sized.  There are no output variable associated with this object --  sizing results are reported to the EIO output file and some predefined summary reports.

A source of design input data for use with this object can be found in the current ASHRAE Handbook HVAC Applications chapter on Service Water Heating.

The idd entry for this object follows.

### Inputs

#### Field: WaterHeater Name

This field contains the unique name of the water heater being sized.  This name should match the name of a Water Heater:Mixed or a Water Heater:Stratified input object defined elsewhere in the input file.

#### Field: Design Mode

This field describes which of several methods are to be used for sizing the water heater.  There are six possible choices and one of the following should be selected:

**PeakDraw**. This design method uses the design flow rates of all the different demands placed on the water heater.  The tank size is based on how long it can meet the demand and how quickly it can recover.  The user enters the time in hours that the water heater can meet the demands.  Only the hot water uses connected to an individual water heater, or scheduled in the water heater object for stand-alone units, are included in that water heater's peak draw.

**ResidentialHUD-FHAMinimum** This design method is based on minimum permissible water heater sizes (established by HUD-FHA in its Minimum Property Standards for One- and Two-Family Living Units, No. 4900.1-1982).  The user enters the number of bathrooms and bedrooms in this input object.  The smallest allowable water heater sizes are used.

**PerPerson** This design method scales sizes based on the total number of people in all zones in the building.  Each water heater in the model will be sized using the total (peak, design) number of people for the entire model.  The number of people is determined from [People](#people) objects defined elsewhere in the input file

**PerFloorArea** This design method scales sizes based on the total floor area in all the zones in the building.  Each water heater in the model will be sized using all the floor area in the model.  The floor areas are determined from the geometry input elsewhere in the input file.

**PerUnit** This design method scales sizes based on an arbitrary number of units.  This can be used, for example, to size based on the number of rooms in a lodging building.  The user provides the number of units in an input field in this object.

**PerSolarCollectorArea** This design method scales tank volume based on the collector area for a solar hot water collector.  The collector area is summed for all the collectors in the model and each tank is sized for the total.  The collector area is determined from input for Solar Collectors defined elsewhere in the input file.

#### Field: Time Storage Can Meet Peak Draw

This field provides the time, in hours, that the tank's volume can sustain a peak draw.  It is used to size the tank's volume which is the simple product of peak draw volume flow rate and the draw time.  There is no assurance that the water will be at the desired temperature for the entire draw.  This field is only used if the Design Mode is "**PeakDraw**." For a water heater connected to a full plant loop, it should be on the supply side and the plant loop needs a Plant Sizing object and the draw rate is the Use side design flow rate. For a stand-alone water heater, the draw rate is the maximum scheduled peak use flow rate.

#### Field: Time for Tank Recovery

This field provides the the time, in hours, that tank's heater needs to recover the volume of the tank. The temperatures used to define recovery are a starting temperature of 14.4ºC (58ºF) and a final temperature of 57.2ºC (135ºF). This field is only used if the Design Mode is "**PeakDraw**."

#### Field: Nominal Tank Volume for Autosizing Plant Connections

This field is used in case the water heater is indirectly heated by its source side connections and they are also autosized.  Because of the complexity of such a water heater and the timing for when sizing calculation happen inside EnergyPlus, the Source side connection flow rates need to be reported before the tank's volume can be sized to meet Peak Draw.  This input field is used to provide a nominal tank volume to use temporarily while the flow connections are sized.  This field is only used if the Design Mode is "**PeakDraw**" and the water heater has autosized plant connections on the demand side.

#### Field: Number of Bedrooms

This field is used to enter the numer of bedrooms in the model.  This field is only used if the Design Mode is "**ResidentialHUD-FHAMinimum**."

#### Field: Number of Bathrooms

This field is used to enter the number of bathrooms in the model.  This field is only used if the Design Mode is "**ResidentialHUD-FHAMinimum**."

#### Field: Storage Capacity per Person

This field is used to enter the tank's storage volume on per-person basis.  The units are m^3^/person.  This field is only used if the Design Mode is "**PerPerson**."

#### Field: Recovery Capacity per Person

This field is used to enter the recovery capacity per person in units of m^3^/person/hr.  This is the volume of water the heater can recover in one hour per person.  Recovery is heating water from a starting temperature of 14.4ºC (58ºF) to a final temperature of 57.2ºC (135ºF).  This field is only used if the Design Mode is "**PerPerson**."

#### Field: Storage Capacity per Floor Area

This field is used to enter the tank's storage volume on a per-floor-area basis.  The units are m^3^ /m^2^ (water/floor area).  This field is only used if the Design Mode is "**PerFloorArea**."

#### Field: Recovery Capacity per Floor Area

This field is used to enter the recovery capacity per floor area in units of m^3^/m^2^/hr.  This is the volume water the heater can recover in an hour per floor area.  Recovery is heating water from a starting temperature of 14.4ºC (58ºF) to a final temperature of 57.2ºC (135ºF).  This field is only used if the Design Mode is "**PerFloorArea**."

#### Field: Number of Units

This field is used to enter the number of Units for use in sizing on per-Unit basis with the next two fields.  This field is only used if the Design Mode is "**PerUnit**."  This can be used to account for any arbitrary item such as lodging rooms, desks, water fixtures, restrooms, etc.

#### Field: Storage Capacity per Unit

This field is used to enter the tanks' storage volume on per-Unit basis.  The units are m^3^/Unit.  The number of Units is entered in the previous field.  This field is only used if the Design Mode is "**PerUnit**."

#### Field: Recovery Capacity per Unit

This field is used to enter the recover capacity per Unit in units of m^3^/Unit/hr.  This is the volume of water the heater can recover in an hour per Unit.  Recovery is heating water from a starting temperature of 14.4ºC (58ºF) to a final temperature of 57.2ºC (135ºF).  This field is only used if the Design Mode is "**PerUnit**."

#### Field: Storage Capacity per Collector Area

This field is used to enter the tank's storage volume on per-solar-collector-area basis.  The units are m^3^/m^2^.  This field is only used if the Design Mode is "**PerSolarCollectorArea**."

#### Field: Height Aspect Ratio

This field is used to scale the height of a stratified tank to preserve relative geometry for different size tanks. The Height Aspect Ratio is defined at the length scale in the vertical direction (height) divided by the length scale in the horizontal direction (diameter).  This field is only used if the water heater being sized is a Water Heater:Stratified, the tank height has been set to Autosize, and the tank shape is set to **VerticalCylinder**. This field can be used with any Design Mode.

## WaterHeater:HeatPump

The heat pump water heater (HPWH) is a compound object consisting of a water heater tank (e.g., [WaterHeater:Mixed](#waterheatermixed) or [WaterHeater:Stratified](#waterheaterstratified)), a direct expansion (DX) "coil" (i.e., an air-to-water DX compression system which includes a water heating coil, air coil, compressor, and water pump), and a fan to provide air flow across the air coil associated with the DX compression system. These objects work together to model a system which heats water using zone air, outdoor air, or a combination of zone and outdoor air as the primary heat source. Numerous configurations of tank location, inlet air source, and DX coil compressor location can be modeled, with one common configuration shown below.

![Schematic diagram for a heat pump water heater located in a zone](media/schematic-diagram-for-a-heat-pump-water.jpeg)


In this model, the heat pump water heater's DX coil is considered the primary heat source and the water tank's heater (element or burner) provides supplemental heat as necessary. The model also assumes that the heat pump's fan and water pump cycle on and off with the compressor.

To model a heat pump water heater, the input data file must include some combination of the following objects depending on the configuration to be modeled:

- [WaterHeater:HeatPump](#waterheaterheatpump) (required)
- [WaterHeater:Mixed](#waterheatermixed) or [WaterHeater:Stratified](#waterheaterstratified) (required)
- [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump) (required)
- [Fan:OnOff](#fanonoff) (required)
- [ZoneHVAC:EquipmentList](#zonehvacequipmentlist) (when the HPWH draws some or all of its air from the zone, the heat pump water heater type and name must be in this list)
- [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) (when the HPWH draws some or all of its air from the zone, the HPWH air inlet and outlet node names must be provided in this object)
- [OutdoorAir:NodeList](#outdoorairnodelist) (for HPWHs that use outdoor air as all or part of the heat source, the HPWH outdoor air node name must be provided in this list)

The input fields for the compound object are described in detail below:

### Inputs

#### Field: Name

This alpha field contains a unique user-assigned name for an instance of a heat pump water heater. Any reference to this heat pump water heater by another object will use this name.

#### Field: Availability Schedule Name

This alpha field contains the name of the schedule (ref: Schedule) that denotes whether the heat pump compressor is available to operate during a given time period. A schedule value equal to 0 denotes that the heat pump compressor is off for that time period. A value other than 0 denotes that the heat pump compressor is available to operate during that time period. During times when the heat pump compressor is scheduled off, the heater (element or burner) in the water tank object operates based on its tank set point temperature schedule and the heat pump's parasitic electric power is also off for that time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Compressor Setpoint Temperature Schedule Name

This alpha field contains the name of the schedule (ref: Schedule) that specifies the set point (or "cut-out") temperature for the heat pump compressor. Temperature values used in this schedule should be in degrees Celsius. The heat pump compressor cycles off when the tank water reaches this set point temperature. Once the heat pump compressor cycles off, the tank water temperature floats downward until it falls below the set point temperature minus the dead band temperature difference defined below (i.e., the "cut-in" temperature). At this point, the heat pump compressor cycles on and remains on until the heat pump compressor set point temperature is reached.

#### Field: Dead Band Temperature Difference

This numeric field contains the dead band temperature difference in degrees Celsius. The heat pump compressor "cut-in" temperature is defined as the compressor set point temperature defined above minus this dead band temperature difference. The heat pump compressor cycles on when the water temperature in the tank falls below the "cut-in" temperature. The heat pump compressor remains on until the water temperature in the tank rises above the compressor set point ("cut-out") temperature defined above. The dead band temperature difference must be greater than 0°C and less than or equal to 20°C. If this field is left blank, the default value is 5°C.

> In this model, the heat pump water heater's DX compression system is considered the primary heat source and the water tank's heater (element or burner) provides supplemental heat as necessary. Therefore, the cut-in temperature for the heat pump compressor (set point minus dead band temperature difference) is usually higher than the set point temperature for the heater (element or burner) in the associated water heater tank object. At times when the water heater tank set point temperature is greater than the cut-in temperature of the heat pump compressor, the heat pump compressor is disabled and the tank's heater is used to heat the water.

#### Field: Condenser Water Inlet Node Name

This alpha field contains the name of the inlet water node for the heat pump's water heating coil (condenser). This node name must also be specified in the water heater tank object as the source outlet node name, and in the DX coil object as the condenser water inlet node name.

#### Field: Condenser Water Outlet Node Name

This alpha field contains the name of the outlet water node for the heat pump's water heating coil (condenser). This node name must also be specified in the water heater tank object as the source inlet node name, and in the DX coil object as the condenser water outlet node name.

#### Field: Condenser Water Flow Rate

This numeric field contains the heat pump's condenser water flow rate in cubic meters per second. It is the actual condenser water flow rate to be simulated, which may differ from the rated condenser water volumetric flow rate specified for the heat pump's DX coil (Ref. [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump)). This water flow rate must be greater than 0 or this field is autocalculatable. If autocalculated (field value = **autocalculate**), the condenser water flow rate is set equal to the rated heating capacity of the heat pump's DX coil multiplied by 4.487E-8 m^3^/s/W. When this flow rate is different from the Rated Condenser Water Volumetric Flow Rate specified in the heat pump's DX coil object (Ref. [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump)), the user should also specify a Total Heating Capacity Modifier Curve Name (function of water flow fraction) and a Heating COP Modifier Curve Name (function of water flow fraction) in the associated DX coil object to account for differences in capacity and power consumption at the off-rated water flow rate.

#### Field: Evaporator Air Flow Rate

This numeric field contains the air flow rate across the heat pump's air coil (evaporator) in cubic meters per second. It is the actual air flow rate to be simulated, which may differ from the rated evaporator air volumetric flow rate specified for the heat pump's DX coil (Ref. [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump)). Values must be greater than 0 or this field is autocalculatable. If autocalculated (field value = **autocalculate**), the evaporator air flow rate is set equal to the rated heating capacity of the heat pump's DX coil multiplied by 5.035E-5 m^3^/s/W. When this flow rate is different from the Rated Evaporator Air Volumetric Flow Rate specified in the heat pump's DX coil object (Ref. [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump)), the user should also specify a Total Heating Capacity Modifier Curve Name (function of air flow fraction) and a Heating COP Modifier Curve Name (function of air flow fraction) in the associated DX coil object to account for differences in capacity and power consumption at the off-rated air flow rate.

#### Field: Inlet Air Configuration

This choice field defines the configuration of the air flow path through the heat pump air coil (evaporator) and fan section. Valid entries are **Schedule**, **ZoneAirOnly**, **OutdoorAirOnly**, or **ZoneAndOutdoorAir**. If ‘Schedule' is selected, names for an inlet air temperature schedule and an inlet air humidity schedule must be defined in the fields below. If ‘ZoneAirOnly' is selected, the corresponding zone name must be entered in the Inlet Air [Zone](#zone) Name field below. If ‘ZoneAndOutdoorAir' is selected, the corresponding Inlet Air [Zone](#zone) Name, Inlet Air Mixer Node Name, Outlet Air Splitter Node Name, and an Inlet Air Mixer Schedule Name must be entered in the corresponding fields below.

#### Field: Air Inlet Node Name

This alpha field contains the name of the node from which the heat pump water heater draws its inlet air. If the Inlet Air Configuration field defined above is set to ‘ZoneAirOnly' or ‘ZoneAndOutdoorAir', then this node name should be the name of a zone air exhaust node (Ref. [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections)). If the Inlet Air Configuration field is set to ‘OutdoorAirOnly', this node name should be left blank. If the Inlet Air Configuration field is set to ‘Schedule', this node name should simply be a unique name that allows the user to receive output on conditions at this node for verification purposes.

#### Field: Air Outlet Node Name

This alpha field contains the name of the node to which the heat pump water heater sends its outlet air. If the Inlet Air Configuration field defined above is set to ‘ZoneAirOnly' or ‘ZoneAndOutdoorAir', then this node name should be the name of a zone air inlet node (Ref. [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections)). If the Inlet Air Configuration field is set to ‘OutdoorAirOnly', this node name should be left blank. If the Inlet Air Configuration field is set to ‘Schedule', this node name should simply be a unique name that allows the user to receive output on conditions at this node for verification purposes.

#### Field: Outdoor Air Node Name

This alpha field contains the name of the node from which the heat pump water heater draws its outdoor air. If the Inlet Air Configuration field defined above is set to ‘ZoneAirOnly' or ‘Schedule', this node name should be left blank. If the Inlet Air Configuration field is set to ‘ZoneAndOutdoorAir' or ‘OutdoorAirOnly', this node name should be the name of an outdoor air node (Ref. [OutdoorAir:NodeList](#outdoorairnodelist)).

#### Field: Exhaust Air Node Name

This alpha field contains the name of the node to which the heat pump water heater sends its exhaust air. If the Inlet Air Configuration field defined above is set to ‘ZoneAirOnly' or ‘Schedule', this node name should be left blank. If the Inlet Air Configuration field is set to ‘ZoneAndOutdoorAir' or ‘OutdoorAirOnly', then this node name should be a unique name that allows the user to receive output on conditions at this node for verification purposes.

#### Field: Inlet Air Temperature Schedule Name

This alpha field contains the name of a schedule used to define the dry-bulb temperature of the inlet air to the heat pump air coil (evaporator) and fan section. Schedule values should be in degrees Celsius. This field is only used when the Inlet Air Configuration defined above is specified as ‘Schedule', otherwise leave this field blank.

#### Field: Inlet Air Humidity Schedule Name

This alpha field contains the name of a schedule used to define the humidity of the inlet air to the heat pump evaporator and fan section. Schedule values must be entered as relative humidity fraction from 0 to 1 (e.g., a schedule value of 0.5 means 50%RH). This field is only used when the Inlet Air Configuration defined above is specified as ‘Schedule', otherwise leave this field blank.

#### Field: Inlet Air Zone Name

This alpha field contains the name of the zone from which the heat pump evaporator and fan section draws some or all of its inlet air. This field is only used when the Inlet Air Configuration defined above is specified as ‘ZoneAirOnly' or ‘ZoneAndOutdoorAir'.

#### Field: Tank Object Type

This alpha (choice) field contains the type of water heater tank used by this heat pump water heater. Currently, the only valid choices are [WaterHeater:Mixed](#waterheatermixed) or [WaterHeater:Stratified](#waterheaterstratified).

#### Field: Tank Name

This alpha field contains the name of the specific water heater tank ([WaterHeater:Mixed](#waterheatermixed) object) used by this heat pump water heater.

#### Field: Tank Use Side Inlet Node Name

This alpha field contains the name of the use side inlet node of the water heater tank used by this heat pump water heater. This name must match the Use Side Inlet Node Name in the water heater tank object (Ref. [WaterHeater:Mixed](#waterheatermixed)). This field is required if the water heater tank use side nodes are connected to a plant loop, otherwise leave this field blank. When used, the branch object should reflect that this node is part of a [WaterHeater:HeatPump](#waterheaterheatpump) object (see branch object example below).

#### Field: Tank Use Side Outlet Node Name

This alpha field contains the name of the use side outlet node of the water heater tank used by this heat pump water heater. This name must match the Use Side Outlet Node Name in the water heater tank object (Ref. [WaterHeater:Mixed](#waterheatermixed)). This field is required if the water heater tank use side nodes are connected to a plant loop, otherwise leave this field blank. When used, the branch object should reflect that this node is part of a [WaterHeater:HeatPump](#waterheaterheatpump) object (see branch object example below).

#### Field: DX Coil Object Type

This alpha (choice) field contains the type of DX coil used by this heat pump water heater. Currently, the only valid choice is [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump).

#### Field: DX Coil Name

This alpha field contains the name of the specific DX coil ([Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump) object) used by this heat pump water heater.

#### Field: Minimum Inlet Air Temperature for Compressor Operation

This numeric field contains the minimum inlet air dry-bulb temperature entering the air coil (evaporator) and fan section, in degrees Celsius, below which the heat pump compressor does not operate. The minimum inlet air dry-bulb temperature should be greater than or equal to 5°C. If this field is left blank, the default value is 10°C.

#### Field: Compressor Location

This alpha (choice) field contains the location of the heat pump compressor and the air temperature for this location is used to control operation of the compressor's crankcase heater in the [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump) object. Valid entries are **Schedule**, **[Zone](#zone)**, or **Outdoors**. If ‘Schedule' is selected, a compressor ambient temperature schedule name must be defined in the field below; otherwise, the field below should be left blank. If ‘[Zone](#zone)' is selected, the crankcase heater operation is controlled based on the air temperature in the zone defined in the field Inlet Air [Zone](#zone) Name, and the Inlet Air Configuration must be ‘ZoneAirOnly' or ‘ZoneAndOutdoorAir'. If ‘Outdoors' is selected, crankcase heater operation is controlled based on the outdoor air temperature.

#### Field: Compressor Ambient Temperature Schedule Name

This alpha field contains the name of a schedule that defines the ambient air temperature surrounding the heat pump compressor, which is used to control the compressor's crankcase heater operation. This field is only used when the compressor location field defined above is specified as ‘Schedule', otherwise it should be left blank.

#### Field: Fan Object Type

This alpha (choice) field contains the type of fan used by this heat pump water heater. Currently, the only valid choice is Fan: OnOff.

#### Field: Fan Name

This alpha field contains the name of the specific fan ([Fan:OnOff](#fanonoff) object) used by this heat pump water heater.

#### Field: Fan Placement

This alpha (choice) field defines the placement of the fan in the heat pump water heater. Valid choices are **BlowThrough** (fan upstream of the air coil) and **DrawThrough** (fan downstream of the air coil). If this field is left blank, the default value is DrawThrough.

#### Field: On Cycle Parasitic Electric Load

This numeric field contains the on-cycle parasitic electric power in Watts. This is the parasitic electric power consumed by controls or other electrical devices associated with the heat pump water heater. This parasitic electric load is consumed whenever the heat pump compressor is operating and the model assumes that this parasitic power does not contribute to heating the water. This parasitic load does, however, affect the zone air heat balance when the heat pump water heater sends some or all of its outlet air to a zone (i.e., Inlet Air Configuration field specified as ‘ZoneAirOnly' or ‘ZoneAndOutdoorAir') and the Parasitic Heat Rejection Location field is specified as ‘[Zone](#zone)'. The minimum value for this field is 0.0, and the default value is also 0.0 if this field is left blank.

#### Field: Off Cycle Parasitic Electric Load

This numeric field contains the off-cycle parasitic electric power in Watts. This is the parasitic electric power consumed by controls or other electrical devices associated with the heat pump compressor. This parasitic electric load is consumed whenever the heat pump water heater is available but the compressor is not operating, and the model assumes that this parasitic power does not contribute to heating the water. This parasitic load does, however, affect the zone air heat balance when the heat pump water heater sends some or all of its outlet air to a zone (i.e., Inlet Air Configuration field specified as ‘ZoneAirOnly' or ‘ZoneAndOutdoorAir') and the Parasitic Heat Rejection Location field is specified as ‘[Zone](#zone)'. The minimum value for this field is 0.0, and the default value is also 0.0 if this field is left blank.

#### Field: Parasitic Heat Rejection Location

This alpha (choice) field determines where the on-cycle and off-cycle parasitic heat is rejected. Valid choices are [Zone](#zone) and Exterior. If ‘[Zone](#zone)' is selected, both the on-cycle and off-cycle parasitic heat is rejected to the zone defined in the field Inlet Air [Zone](#zone) Name, and the Inlet Air Configuration must be ‘ZoneAirOnly' or ‘ZoneAndOutdoorAir. If 'Outdoors' is selected, this parasitic heat is rejected outdoors (does not impact the zone air heat balance) regardless of the specified Inlet Air Configuration. If this field is left blank, the default value is 'Outdoors'.

#### Field: Inlet Air Mixer Node Name

This optional alpha field defines the name of the HVAC node which represents the mixture of outdoor air and zone air that enters the heat pump air coil (evaporator) and fan section. The model mixes outdoor air with zone air and places the result on this inlet air mixer node based on the Inlet Air Mixer Schedule defined below. When the schedule value is equal to 0, 100% zone air enters the evaporator coil and fan section of the heat pump water heater. When the schedule value is equal to 1, 100% outdoor air enters the evaporator coil and fan section. This node name must be provided if the Inlet Air Configuration field above is specified as ‘ZoneAndOutdoor Air', otherwise this field should be left blank.

#### Field: Outlet Air Splitter Node Name

This alpha field defines the name of the air node to which the heat pump air coil (evaporator) and fan sends all of its outlet air. The supply air flow downstream of this node is split between the zone and outdoors based on the Inlet Air Mixer schedule defined below. When the schedule value is equal to 0, the entire outlet air stream is diverted to the zone. When the schedule value is equal to 1, the entire outlet air stream is exhausted to outdoors. This node name must be provided if the Inlet Air Configuration field above is specified as ‘[Zone](#zone) and Outdoor Air', otherwise this field should be left blank.

#### Field: Inlet Air Mixer Schedule Name

This alpha field defines the name of the schedule (ref: Schedule) that denotes whether the heat pump draws its inlet air from the zone, outdoors, or a combination of zone and outdoor air. A schedule value equal to 0 indicates that the heat pump draws its inlet air from the zone. A schedule value equal to 1 denotes that the heat pump draws its inlet air from outdoors. Values between 0 and 1 denote a mixture of zone and outdoor air proportional to the schedule value. The Inlet Air Mixer schedule controls both the inlet air mixer and outlet air splitter nodes in unison to ensure that the operation of the heat pump does not contribute to zone pressurization or depressurization. For example if the Inlet Air Mixer schedule value is 0.4, then the inlet air mixer node is composed of 40% outdoor air and 60% zone air. For this same case, the outlet air splitter directs 60% of the HPWH outlet air back to the zone and 40% of the outlet air flow is exhausted outdoors. This schedule name must be provided if the Inlet Air Configuration field is specified as ‘[Zone](#zone) and Outdoor Air', otherwise this field should be left blank.

#### Field: Control Sensor Location In Stratified Tank

This alpha field defines where the tank temperature is sensed for heat pump control when the tank type is [WaterHeater:Stratified](#waterheaterstratified).  The stratified tank model produces tank temperature at different nodes in the vertical direction and various options are available for how this temperature should be sensed to control the heat pump.  There are six choices that, when combined with the input for the stratified tank, indicate which of the nodes should be used.  The default is "Heater1". The choices include:

- **Heater1**.  This indicates the tank node associated with (backup) heater #1 should be used for tank temperature control.  This is determined by the field called Heater 1 Height in the [WaterHeater:Stratified](#waterheaterstratified) object.
- **Heater2**.  This indicates the tank node associated with (backup) heater #2 should be used for tank temperature control.  This is determined by the field called Heater 2 Height in the [WaterHeater:Stratified](#waterheaterstratified) object.
- **SourceInlet**. This indicates the tank node associated with the source side inlet should be used for tank temperature control.  This is determined by the field called Source Side Inlet Height ****in the [WaterHeater:Stratified](#waterheaterstratified) object.
- **SourceOutlet**. This indicates the tank node associated with the source side outlet should be used for tank temperature control.  This is determined by the field called Source Side Outlet Height ****in the [WaterHeater:Stratified](#waterheaterstratified) object.
- **UseInlet**. This indicates the tank node associated with the use side inlet should be used for tank temperature control.  This is determined by the field called Use Side Inlet Height ****in the [WaterHeater:Stratified](#waterheaterstratified) object.
- **UseOutlet**. This indicates the tank node associated with the use side outlet should be used for tank temperature control.  This is determined by the field called Use Side Outlet Height ****in the [WaterHeater:Stratified](#waterheaterstratified) object.

Following is an example input for the Heat Pump:Water Heater compound object and the other required component objects that it references.

~~~~~~~~~~~~~~~~~~~~

    WaterHeater:HeatPump,
        PlantHeatPumpWaterHeater,!- Name
        PlantHPWHSch,            !- Availability Schedule Name
        PlantHPWHTempSch,        !- Compressor Setpoint Temperature Schedule Name
        2.0,                     !- Dead Band Temperature Difference {deltaC}
        HPPlantWaterInletNode,   !- Condenser Water Inlet Node Name
        HPPlantWaterOutletNode,  !- Condenser Water Outlet Node Name
        0.00115525,              !- Condenser Water Flow Rate {m3/s}
        1.00695,                 !- Evaporator Air Flow Rate {m3/s}
        OutdoorAirOnly,          !- Inlet Air Configuration
        ,                        !- Air Inlet Node Name
        ,                        !- Air Outlet Node Name
        HPPlantAirInletNode,     !- Outdoor Air Node Name
        HPPlantAirOutletNode,    !- Exhaust Air Node Name
        ,                        !- Inlet Air Temperature Schedule Name
        ,                        !- Inlet Air Humidity Schedule Name
        ,                        !- Inlet Air Zone Name
        WaterHeater:Mixed,       !- Tank Object Type
        HPWHPlantTank,           !- Tank Name
        HPWH Use Inlet Node,     !- Tank Use Side Inlet Node Name
        HPWH Use Outlet Node,    !- Tank Use Side Outlet Node Name
        Coil:WaterHeating:AirToWaterHeatPump,  !- DX Coil Object Type
        HPWHPlantDXCoil,         !- DX Coil Name
        11.0,                    !- Minimum Inlet Air Temperature for Compressor Operation {C}
        Outdoors,                !- Compressor Location
        ,                        !- Compressor Ambient Temperature Schedule Name
        Fan:OnOff,               !- Fan Object Type
        HPWHPlantFan,            !- Fan Name
        BlowThrough,             !- Fan Placement
        ,                        !- On Cycle Parasitic Electric Load {W}
        ,                        !- Off Cycle Parasitic Electric Load {W}
        ;                        !- Parasitic Heat Rejection Location

    NOTE: branch object required only when tank use inlet nodes are used.

      Branch,
        Central HPWH Branch,     !- Name
        0,                       !- Maximum Flow Rate {m3/s}
        ,                        !- Pressure Drop Curve Name
        WaterHeater:HeatPump,    !- Component 1 Object Type
        PlantHeatPumpWaterHeater,!- Component 1 Name
        HPWH Use Inlet Node,     !- Component 1 Inlet Node Name
        HPWH Use Outlet Node,    !- Component 1 Outlet Node Name
        PASSIVE;                 !- Component 1 Branch Control Type

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      WaterHeater:Mixed,
        HPWHPlantTank,           !- Name
        0.757,                   !- Tank Volume {m3}
        Plant Hot Water Setpoint Temp Schedule,  !- Setpoint Temperature Schedule Name
        2.0,                     !- Deadband Temperature Difference {deltaC}
        82.2222,                 !- Maximum Temperature Limit {C}
        CYCLE,                   !- Heater Control Type
        25000,                   !- Heater Maximum Capacity {W}
        0,                       !- Heater Minimum Capacity {W}
        ,                        !- Heater Ignition Minimum Flow Rate {m3/s}
        ,                        !- Heater Ignition Delay {s}
        ELECTRICITY,             !- Heater Fuel Type
        0.98,                    !- Heater Thermal Efficiency
        ,                        !- Part Load Factor Curve Name
        10,                      !- Off Cycle Parasitic Fuel Consumption Rate {W}
        ELECTRICITY,             !- Off Cycle Parasitic Fuel Type
        0,                       !- Off Cycle Parasitic Heat Fraction to Tank
        30,                      !- On Cycle Parasitic Fuel Consumption Rate {W}
        ELECTRICITY,             !- On Cycle Parasitic Fuel Type
        0,                       !- On Cycle Parasitic Heat Fraction to Tank
        Outdoors,                !- Ambient Temperature Indicator
        ,                        !- Ambient Temperature Schedule Name
        ,                        !- Ambient Temperature Zone Name
        HPWHPlantTank OA Node,   !- Ambient Temperature Outdoor Air Node Name
        0.0,                     !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}
        0.0,                     !- Off Cycle Loss Fraction to Zone
        0.0,                     !- On Cycle Loss Coefficient to Ambient Temperature {W/K}
        0.0,                     !- On Cycle Loss Fraction to Zone
        ,                        !- Peak Use Flow Rate {m3/s}
        ,                        !- Use Flow Rate Fraction Schedule Name
        ,                        !- Cold Water Supply Temperature Schedule Name
        HPWH Use Inlet Node,     !- Use Side Inlet Node Name
        HPWH Use Outlet Node,    !- Use Side Outlet Node Name
        0.98,                    !- Use Side Effectiveness
        HPPlantWaterOutletNode,  !- Source Side Inlet Node Name
        HPPlantWaterInletNode,   !- Source Side Outlet Node Name
        0.98,                    !- Source Side Effectiveness
        autosize;                !- Use Side Design Flow Rate {m3/s}

      OutdoorAir:Node,
        HPWHPlantTank OA Node;   !- Name

      Coil:WaterHeating:AirToWaterHeatPump,
        HPWHPlantDXCoil,         !- Name
        25000.0,                 !- Rated Heating Capacity {W}
        3.2,                     !- Rated COP {W/W}
        0.736,                   !- Rated Sensible Heat Ratio
        29.44,                   !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}
        22.22,                   !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}
        55.72,                   !- Rated Condenser Inlet Water Temperature {C}
        1.00695,                 !- Rated Evaporator Air Flow Rate {m3/s}
        0.00115525,              !- Rated Condenser Water Flow Rate {m3/s}
        No,                      !- Evaporator Fan Power Included in Rated COP
        No,                      !- Condenser Pump Power Included in Rated COP
        No,                      !- Condenser Pump Heat Included in Rated Heating Capacity and Rated COP
        150.0,                   !- Condenser Water Pump Power {W}
        0.1,                     !- Fraction of Condenser Pump Heat to Water
        HPPlantFanAirOutletNode, !- Evaporator Air Inlet Node Name
        HPPlantAirOutletNode,    !- Evaporator Air Outlet Node Name
        HPPlantWaterInletNode,   !- Condenser Water Inlet Node Name
        HPPlantWaterOutletNode,  !- Condenser Water Outlet Node Name
        100.0,                   !- Crankcase Heater Capacity {W}
        5.0,                     !- Maximum Ambient Temperature for Crankcase Heater Operation {C}
        WetBulbTemperature,      !- Evaporator Air Temperature Type for Curve Objects
        HPWHHeatingCapFTemp,     !- Heating Capacity Function of Temperature Curve Name
        ,                        !- Heating Capacity Function of Air Flow Fraction Curve Name
        ,                        !- Heating Capacity Function of Water Flow Fraction Curve Name
        HPWHHeatingCOPFTemp,     !- Heating COP Function of Temperature Curve Name
        ,                        !- Heating COP Function of Air Flow Fraction Curve Name
        ,                        !- Heating COP Function of Water Flow Fraction Curve Name
        HPWHPLFFPLR;             !- Part Load Fraction Correlation Curve Name

      Fan:OnOff,
        HPWHPlantFan,            !- Name
        PlantHPWHSch,            !- Availability Schedule Name
        0.7,                     !- Fan Total Efficiency
        100.0,                   !- Pressure Rise {Pa}
        2.6852,                  !- Maximum Flow Rate {m3/s}
        0.9,                     !- Motor Efficiency
        1.0,                     !- Motor In Airstream Fraction
        HPPlantAirInletNode,     !- Air Inlet Node Name
        HPPlantFanAirOutletNode; !- Air Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

**HVAC,Average,Water Heater Compressor Part Load Ratio**

**HVAC,Average,Water Heater On Cycle Ancillary Electric Power [W]**

**HVAC,Sum,Water Heater On Cycle Ancillary Electric Energy [J]**

**HVAC,Average,Water Heater Off Cycle Ancillary Electric Power [W]**

**HVAC,Sum,Water Heater Off Cycle Ancillary Electric Energy [J]**

#### Water Heater Compressor Part Load Ratio

This output is the average part-load ratio of the heat pump water heater's compressor (as well as its water pump and fan) for the timestep being reported. This output is independent of the "Water Heater Part Load Ratio" (Ref. Water Heater Outputs) which represents the part- load ratio of the supplemental heater (element or burner) in the water tank. When the water tank's (supplemental) heater set point temperature is higher than the cut-in temperature of the heat pump water heater's compressor, the heat pump compressor is disabled and the water tank's heater (element or burner) is used to heat the water. During these times the Water Heater Compressor Part Load Ratio is equal to 0.

#### Water Heater On Cycle Ancillary Electric Power [W]

#### Water Heater On Cycle Ancillary Electric Energy [J]

#### Water Heater Off Cycle Ancillary Electric Power [W]

#### Water Heater Off Cycle Ancillary Electric Energy [J]

These outputs are the parasitic electric power and consumption associated with the heat pump water heater. Specific outputs represent parasitic electrical usage during the compressor/fan on and off cycles. These outputs represent electronic controls or other electric component. The model assumes that the parasitic power does not contribute to heating the water, but it can impact the zone air heat balance depending on user inputs. The parasitic electric consumption outputs are also added to a meter with Resource Type = Electricity, End Use Key = DHW, Group Key = Plant (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).