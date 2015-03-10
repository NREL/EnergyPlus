# Group -- Electric Load Center-Generator Specifications

## ElectricLoadCenter:Transformer

This object is used to model the energy losses of transformers when they are used to transfer electricity from the grid to a building (as distribution transformers) or transfer electricity from onsite generators to the grid. Distribution transformers reduce the voltage on utility distribution lines (34.5 kV or less) to a lower secondary voltage (600 V or less) suitable for customer equipment. According to the medium used for cooling and insulation, distribution transformers can be classified into liquid-immersed and dry type transformers. According to the input voltages, distribution transformers can be classified into medium-voltage (between 600 V and 34.5kV) and low-voltage transformer (less than 600 V). The transformers used to output the surplus electricity from onsite generators to the grid usually match the cogeneration voltage to the grid. This object can be used for all types of distribution transformers and the transformers used to output electricity from onsite generators to the grid, but the model defaults applies to the low-voltage dry type transformers because they are the most common type found in commercial and industrial facilities.

Energy losses in transformers consist of the no load (core) loss and the load (winding) loss. The no load loss comes primarily from the switching of the magnetic fields in the core material. It is roughly constant and exists continuously in the core material as long as the transformer is energized. The load loss comes from the electrical resistance in the windings when there is a load on the transformer. The load loss is proportional to the load squared with a small temperature correction. The energy losses at a given load depend on the transformer's performance. This object provides the user two alternative ways to indicate the transformer's performance. First, the user can provide the no load loss and load loss at rated conditions. Second, the user can provide the nameplate efficiency and the corresponding reference conditions. The detailed algorithms to calculate the energy losses are described in the Engineering Reference.

The user needs to specify which loads are connected to the transformer. This is achieved in different manners depending on the transformer usage. For a distribution transformer, the user needs to provide a number of electric meters wired to that transformer. The input object can currently accommodate up to ten meters, but it is extensible by modifying the Energy+.idd entry. For a transformer used to output electricity from onsite generators to the grid, the user needs to indicate the transformer name in the served electric load center.

### Inputs

#### Field: Availability Schedule Name

This alpha field contains the schedule name (ref. Schedule objects) that contains information on the availability of the transformer. A schedule value greater than 0 (usually 1 is used) indicates that the transformer is available to convert AC power from one voltage to another. A value less than or equal to 0 (usually 0 is used) denotes that the transformer is not available. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Transformer Usage

This field indicates one of the two supported transformer application types: PowerInFromGrid and PowerOutFromOnsiteGeneration. The first type of transformer is used to step down voltage from the electricity grid to a building. The second type of transformer is used to match voltage from building onsite generators to the electricity grid.

#### Field: Zone Name

This field contains the name of the thermal zone where the transformer is located. Entering a valid name of zone here will direct EnergyPlus to include the transformer's losses as heat gains to the named thermal zone. If the transformer is not within a thermal zone, this field can be left blank and the thermal energy associated with transformer losses is removed from the building model.

#### Field: Radiative Fraction

This field contains the fraction of transformer's losses that enter the zone as long-wave thermal radiation. This numeric filed should have a value between 0.0 and 1.0. The balance of the losses is convective. This field is not used if the previous field for zone name is left blank.

#### Field: Rated Capacity

This field defines the rated capacity of the transformer in VA.

#### Field: Phase

This field indicates whether the transformer is a single phase or three phase type. The two alternative values are 1 or 3. The default is 3. This field is currently not used in the model.

#### Field: Conductor Material

This field specifies the winding material used by the transformer. There are two options: Copper and Aluminum. The choice will determine the thermal coefficient of resistance used to calculate the temperature correction factor for the transformer load losses. The default is Aluminum.

#### Field: Full Load Temperature Rise

This numeric field defines the temperature rise of the windings above the ambient temperature, when the transformer is loaded at its nameplate rating. For liquid-immersed distribution transformers, the full load temperature rise usually takes the value of 65 °C. For dry-type transformers, the temperature rise usually takes three optional values: 80, 115 and 150 °C. This field accepts any value between the minimum (50 °C) and the maximum (180 °C). The default is 150 °C.

#### Field: Fraction of Eddy Current Losses

This field defines the fraction of load losses resulting from the eddy currents. Transformer's load losses comprise two parts: the ohmic loss due to the current flowing in the resistance of the windings and the eddy and stray losses due to the eddy currents. This field indicates the fraction of the load losses due to the eddy currents. This numeric field should have a value between 0.0 and 1.0. The default is 0.1.

#### Field: Performance Input Method

This alpha field contains the method by which the user will specify the transformer performance: "RatedLosses" or "NominalEfficiency". If this field is left blank in the input data file, the default input method is assumed to be "RatedLosses". If the method "NominalEfficiency" is selected, the user must enter the fields for the nameplate efficiency and the corresponding reference coditions as described below. If the method "RatedLosses" is selected, then the fields for rated no load loss and load loss must be entered as described below.

#### Field: Rated No Load Loss

This field defines the no load loss (W) at rated load and conditions. The no load loss is roughly constant and exists whenever the transformer is energized. The no load loss is also called the core loss. This field is used only if the field of Performance Input Method is specified as "RatedLosses".

#### Field: Rated Load Loss

This field defines the load loss (W) at rated load and conditions. The load loss varies with the square of the load being served by the transformer. The load loss is also called the winding loss because the load loss occurs in the primary and secondary windings around the core. This field is used only if the field of Performance Input Method is specified as "RatedLosses".

#### Field: Nameplate Efficiency

This field contains the value for transformer efficiency at a given per unit load and specified reference temperature. This field is used only if the field of Performance Input Method is specified as "NominalEfficiency". The default is 0.98.

#### Field: Per Unit Load for Nameplate Efficiency

This field defines the percentage of the rated capacity at which the nameplate efficiency is measured. According to the NEMA (National Electrical Manufactures Association) Standard TP-1, the per unit load takes the value of 0.35 for dry-type distribution transformers and it takes the value of 0.50 for liquid-filled distribution transformers. The default is 0.35.

#### Field: Reference Temperature for Nameplate Efficiency

This field defines the conductor temperature at which the nameplate efficiency is measured. This field is used only if the field of Performance Input Method is specified as "NominalEfficiency". According to the NEMA Standard TP-1, the reference temperature takes the value of 75 °C for dry-type distribution transformers and it takes the value of 55 °C for liquid-filled distribution transformers. The default is 75 °C.

#### Field: Per Unit Load for Maximum Efficiency

This field defines the percentage of the rated capacity at which the maximum efficiency is obtained. This field is used only if the field of Performance Input Method is specified as "NominalEfficiency". A blank field indicates that it takes the same value as the per unit load for nameplate efficiency.

#### Field: Consider Transformer Loss for Utility Cost

This field indicates whether the transformer losses are considered to calculate utility cost. In some cases, the transformer losses are required but they are not part of the energy consumption for utility cost calculation. For example, the transformer is owned by the utility company but it locates in the building. In this case, it might be desired to model transformer energy losses for HVAC operation but the energy losses will not be accounted for utility cost.

#### Field: Electric Meter 1-10 Name

A transformer may serve different loads such as plug loads, some lighting loads, and some HVAC equipment loads. The user needs to specify which loads are connected to the transformer. This is achieved by providing a list of electric meters wired to the transformer. The input object can currently accommodate up to ten meters, but it is extensible by modifying the Energy+.idd entry. Any valid electric meter name can be used here to wire to the transformer loads. Many different meters are automatically generated by the program and will depend on the objects used throughout the input file. The typical process of determining the exact name to enter in this field involves doing an initial run of EnergyPlus and then examining the \*.mdd file which will list the meters and their resulting names. A custom meter is also supported. The meter must have electricity as its resource type.

An example input of the [ElectricLoadCenter:Transformer](#electricloadcentertransformer) input is:

~~~~~~~~~~~~~~~~~~~~

      ElectricLoadCenter:Transformer,
        Transformer 1,            !-Name
        Always On,                !- Availability Schedule Name
        PowerInFromGrid,          !- Transformer Usage
        ,                         !- Zone Name
        ,                         !- Radiative Fraction
        15000,                    !- Nameplate Rating {VA}
        3,                        !- Phase
        Aluminum,                 !- Conductor Material
        150,                      !- Full Load Temperature Rise {°C}
        0.1,                      !- Fraction of Eddy Current Losses
        NominalEfficiency,        !- Performance Input Method
        ,                         !- Rated No Load Loss {W}
        ,                         !- Rated Load Loss {W}
        0.985,                    !- Nameplate Efficiency
        0.35,                     !- Per Unit Load for Nameplate Efficiency
        75,                       !- Reference Temperature for Nameplate Efficiency {°C}
        ,                         !- Per Unit Load for Maximum Efficiency
        Yes,                      !- Consider Transformer Loss for Utility Cost
        Electricity:Building;     !- Meter 1 Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Schedule:Compact,
        Always On,               !- Name
        Fraction,                !- ScheduleType
        Through: 12/31,          !- Field #1
        For: AllDays,            !- Field #2
        Until: 24:00,            !- Field #3
        1.0;                     !- Field #4
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Transformer Efficiency[]
    HVAC,Average,Transformer Output Electric Power[W]
    HVAC,Sum,Transformer Output Elecric Energy [J]
    HVAC,Average,Transformer Input Electric Power[W]
    HVAC,Sum,Transformer Input Electric Energy [J]
    HVAC,Average,Transformer No Load Loss Rate [W]
    HVAC,Sum,Transformer No Load Loss Energy [J]
    HVAC,Average,Transformer Load Loss Rate [W]
    HVAC,Sum,Transformer Load Loss Energy [J]
    HVAC,Average,Transformer Thermal Loss Rate [W]
    HVAC,Sum,Transformer Thermal Loss Energy [J]
    HVAC,Sum,Transformer Distribution Electric Loss Energy [J]
    HVAC,Sum,Transformer Cogeneration Electric Loss Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Transformer Efficiency

This output field is the efficiency at which the distribution transformer changes alternating current from the higher primary voltage to a lower secondary voltage. The efficiency is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

#### Transformer Output Electric Power [W]

#### Transformer Output Elecric Energy [J]

These outputs are the total electricity power or energy provided by the transformer. They are equal to the metered loads which are wired to the transformer. These values are calculated for each HVAC system timestep being simulated, and the results are averaged (for power) or summed (for energy) for the timestep being reported.

#### Transformer Input Electric Power [W]

#### Transformer Input Electric Energy [J]

These outputs are the total electricity power or energy fed into the transformer. These values are calculated for each HVAC system timestep being simulated, and the results are averaged (for power) or summed (for energy) for the timestep being reported.

#### Transformer No Load Loss Rate [W]

#### Transformer No Load Loss Energy [J]

These outputs are the no load loss occurred in the transformer. These values are calculated for each HVAC system timestep being simulated, and the results are averaged (for rate) or summed (for energy) for the timestep being reported.

#### Transformer Load Loss Rate [W]

#### Transformer Load Loss Energy [J]

These outputs are the load loss occurred in the transformer. These values are calculated for each HVAC system timestep being simulated, and the results are averaged (for rate) or summed (for energy) for the timestep being reported.

#### Transformer Thermal Loss Rate [W]

#### Transformer Thermal Loss Energy [J]

These outputs are the total energy losses occurred in the transformer. They are equal to the sum of the no load loss and the load loss. These values are calculated for each HVAC system timestep being simulated, and the results are averaged (for rate) or summed (for energy) for the timestep being reported.

#### Transformer Distribution Electric Loss Energy [J]

This output is the total energy losses occurred in the transformer when it is used for input power from grid to building. It is set as zero if the transformer is used to transfer energy from onsite power generators to the electricity grid. This output is also added to a meter with ResourceType = Electricity, GroupKey = System.

#### Transformer Cogeneration Electric Loss Energy  [J]

This output is the total energy losses occurred in the transformer when it is used for input onsite cogeneration to the grid. It is set as zero if the transformer is used to transfer the electricity grid to building. This output is also added to a meter with ResourceType = ElectricityProduced, EndUseKey = Cogeneration, GroupKey = System.

## ElectricLoadCenter:Distribution

[ElectricLoadCenter:Distribution](#electricloadcenterdistribution) objects are used to include on-site electricity generators in a simulation. The electric load center dispatches generators according to operation schemes and tracks and reports the amount of electricity generated and purchased. When using on-site generators, the program provides a "net" report where the total electricity used is reduced by the amount generated on site. Electrical demand tracking is done by the internal or custom meters used by EnergyPlus for reporting. The thermal demand tracking uses internal load calculations from the plant simulation. The dispatching of different generators is based on expectations based on their nominal/rated electric power output. If the current conditions are such that the generator model determines that generation was higher or lower, then the results of the dispatch may differ from expectations.

Multiple different [ElectricLoadCenter:Distribution](#electricloadcenterdistribution) objects can be included in an input file.  A great deal of flexibility is available by mixing different load centers and operating schemes. If multiple load centers are used, the supervisory control will dispatch generators sequentially across the load centers.  Therefore, the order of these input objects in the IDF file becomes important with the generators associated with first load center in the file being the first to be managed.  A certain amount of caution is needed to avoid conflicting operating schemes.

The electricity produced from photovoltaic arrays will be reported in the electricity produced output variable and will reduce the demand that the generators will try to meet for that timestep.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the electric load center.

#### Field: Generator List Name

This alpha field contains the identifying name for the list of generators in the set.

#### Field: Generator Operation Scheme Type

This alpha field specifies the type of operating scheme for the generator set. The available operating schemes are "Baseload," "DemandLimit," "TrackElectrical," "TrackSchedule," "TrackMeter," "FollowThermal," and "FollowThermalLimitElectrical." The Baseload scheme operates the generators at their rated (requested) electric power output when the generator is scheduled ON (ref. [ElectricLoadCenter:Generators](#electricloadcentergenerators)). The Baseload scheme requests all generators scheduled ON (available) to operate, even if the amount of electric power generated exceeds the total facility electric power demand. The DemandLimit scheme limits the amount of purchased electrical from the utility to the amount specified in the input object. The DemandLimit scheme tries to have the generators meet all of the demand above the purchased electric limit set by the user in the next field. The TrackElectrical scheme tries to have the generators meet all of the electrical demand for the building. The TrackSchedule scheme tries to have the generators meet all of the electrical demand determined in a user-defined schedule. The TrackMeter scheme tries to have the generators meet all the electrical demand from a meter, which could also be a user-defined custom meter.

The DemandLimit, TrackElectrical, TrackSchedule, and TrackMeter schemes will sequentially load the available generators. All demand not met by available generator capacity will be met by purchased electrical. Therefore, if DemandLimit, TrackElectrical, TrackSchedule, or TrackMeter is utilized and the available generators are not enough to meet demand, then purchased electrical will offset the difference. If a generator is needed in the simulation for a small load and it is less than the minimum part load ratio the generator will operate at the minimum part load ratio and the excess will either reduce demand or the excess energy will be available for returning to the electric grid.

The FollowThermal and FollowThermalLimitElectrical schemes are for heat following cogeneration and run the generators to meet the thermal demand.  The FollowThermal schemes allow excess electrical generation to be exported to the grid, while the FollowThermalLimitElectrical scheme restricts generator output to a maximum of the building's current electrical demand (so that no electricity is exported).  The thermal demand is determined from the plant modeling and depends on the flow requested by other components on the demand side of the plant loop, the loop temperatures, and the loop temperature set point.  The electric load center converts the thermal load to an electrical load using a nominal ratio of the thermal to electrical power production for each generator.  For these schemes, the generator needs to be connected to the supply side of a plant loop and  components that request hot water need to be on the demand side of the plant loop.  This is different than the usual configuration for electrical following schemes where the generator is put on the demand side and request flow for the purposes of cooling the generator.  Therefore a switch from one of the electrical-based operating schemes to one of the thermal-based operating schemes requires a substantial change in plant topology.

If the load center includes electrical storage, then the choice of operating schemes will also affect how storage is managed.

#### Field: Demand Limit Scheme Purchased Electric Demand Limit

This numerical field is the user input for the demand limit above which the generators will try and meet the entire electrical load on the building minus the photovoltaic array if available.

#### Field: Track Schedule Name Scheme Schedule Name

This alpha field is the user input for the name of a schedule defined elsewhere in the input file that contains values for the "demand" loads placed on the generator(s). The schedule values should be in Watts.

#### Field: Track Meter Scheme Meter Name

This alpha field is the user input for the name of a meter. Any valid meter name can be used here to control the loads that the generator(s) will try to meet. Many different meters are automatically generated by the program and will depend on the objects used throughout the input file. The typical process of determining the exact name to enter in this field involves doing an initial run of EnergyPlus and then examining the \*.rdd file which will list the meters and their resulting names. The \*.mtd file will also be useful to understand what exactly is included in a meter. The user can also use the Output:CustomMeter objects to redefine meter names and to control what electrical loads are included on them in a flexible manner.

#### Field: Electrical Buss Type

This alpha field is used to describe how the electric load center is configured with respect to any power conditioning and/or storage equipment. There are five configurations for load centers available by using one of these keywords:

- AlternatingCurrent
- AlternatingCurrentWithStorage
- DirectCurrentWithInverter
- DirectCurrentWithInverterDCStorage
- DirectCurrentWithInverterACStorage

AlternatingCurrent is the default.  All the generators connected to a specific load center need to be of the same type (all AC or all DC).  If the generators are DC, then an inverter is needed to convert the DC to AC.  If there are DC generators and a DC electrical storage device on the buss along with an inverter, then use DirectCurrentWithInverterDCStorage in this field.  See the Engineering Reference for more information including diagrams of the load center configurations.

#### Field: Inverter Object Name

This field is used to identify the inverter connected to this load center (if any).  This field is only used if the Electrical Buss Type is set to DirectCurrentWithInverter and should contain the user-defined name of an inverter object.  There are three types of inverter models available—see [ElectricLoadCenter:Inverter:Simple](#electricloadcenterinvertersimple), [ElectricLoadCenter:Inverter:LookUpTable](#electricloadcenterinverterlookuptable),  or [ElectricLoadCenter:Inverter:FunctionOfPower](#electricloadcenterinverterfunctionofpower).  Enter the name of one of these types of inverter objects defined elsewhere in the input file.

#### Field: Electrical Storage Object Name

This field is used to identify the electrical storage connected to this load center (if any).  This field is only used if the Electrical Buss Type is set to DirectCurrentWithInverterDCStorage or DirectCurrentWithInverterACStorage.  Enter the name of an [ElectricLoadCenter:Storage:Simple](#electricloadcenterstoragesimple) object defined elsewhere in the input file.

#### Field: Transformer Object Name

This field is used to identify the transformer connected to this load center (if any). This field can be used for any electrical buss types. Enter the name of an [ElectricLoadCenter:Transformer](#electricloadcentertransformer) object defined elsewhere in the input file.

Examples of this object are:

~~~~~~~~~~~~~~~~~~~~

    ElectricLoadCenter:Distribution,
        SOFC Electric Load Center,  !- Name
        Micro-Generators,        !- Generator List Name
        Baseload,                !- Generator Operation Scheme Type
        0.0,                     !- Demand Limit Scheme Purchased Electric Demand Limit {W}
        ,                        !- Track Schedule Name Scheme Schedule Name
        ,                        !- Track Meter Scheme Meter Name
        AlternatingCurrent,      !- Electrical Buss Type
        ,                        !- Inverter Object Name
        ;                        !- Electrical Storage Object Name

      ElectricLoadCenter:Distribution,
        Electric Load Center,    !- Name
        Backup Generators,       !- Generator List Name
        DemandLimit,             !- Generator Operation Scheme Type
        10000.0,                 !- Demand Limit Scheme Purchased Electric Demand Limit {W}
        ,                        !- Track Schedule Name Scheme Schedule Name
        ,                        !- Track Meter Scheme Meter Name
        AlternatingCurrent,      !- Electrical Buss Type
        ,                        !- Inverter Object Name
        ;                        !- Electrical Storage Object Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

There are three levels of results reporting related to electric load centers and generators.  The top level of reporting provides results across the whole building and all the different electric load centers and generators.  The second level of reporting provides results for individual load centers.  The third level is for each type of generator (see individual generator descriptions).  This section discusses the first two levels.

The whole-building summary output variables are:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Facility Total Produced Electric Power [W]
    HVAC,Sum,Facility Total Produced Electric Energy [J]
    HVAC,Average,Facility Total Purchased Electric Power [W]
    HVAC,Sum,Facility Total Purchased Electric Energy [J]
    HVAC,Average,Facility Total Building Electric Demand Power [W]
    HVAC,Average,Facility Total HVAC Electric Demand Power [W]
    HVAC,Average,Facility Total Electric Demand Power [W]
~~~~~~~~~~~~~~~~~~~~

These output variables all use a default keyword "Whole [Building](#building)."

These output variables can also be accessed without specifying generation equipment by including the syntax shown below:

~~~~~~~~~~~~~~~~~~~~

    ElectricLoadCenter:Distribution,
      Electric Load Center; !- Name
~~~~~~~~~~~~~~~~~~~~

Even if no [ElectricLoadCenter:Distribution](#electricloadcenterdistribution) object is included, these output variables are also available for all simulations that have any electricity consuming equipment with the default keyword name "Electrical Service."

#### Facility Total Produced Electric Power [W]

#### Facility Total Produced Electric Energy [J]

These outputs are the total generator and photovoltaic electricity produced on-site for the entire model, and they are in both Power and Energy units.  When electrical storage is used with on-site production, the electricity put into storage is decremented from production and the electricity removed storage is added to production.  This means that losses from a round trip through electrical storage decrease on-site electricity production.

#### Facility Total Purchased Electric Power [W]

#### Facility Total Purchased Electric Energy [J]

These outputs are the total of electricity purchased for the entire model in both Power and Energy units. This value is always positive and indicates the amount of energy is purchased from the utility.

#### Facility Total Surplus Electric Energy [J]

This output is the excess electricity produced and sent out to the electrical grid. This value is always positive and indicates the surplus electricity from generation that exceeds whole-building demand and fed into the grid.

#### Facility Total Building Electric Demand Power [W]

This output variable includes all of the electric demand from the building (non-HVAC) portion of the simulation, which would contain lights, electrical equipment, exterior lights and equipment, etc.

#### Facility Total HVAC Electric Demand Power [W]

This output variable includes all of the electric demand from the HVAC portion of the simulation, which would contain fans, electric coils, pumps, chillers, cooling towers, etc.

#### Facility Total Electric Demand Power [W]

This is the total of the whole [Building](#building) and HVAC electric demands.

#### Facility Net Purchased Electric Power [W]

#### Facility Net Purchased Electric Energy [J]

These outputs are the net electricity purchased in both Power and Energy units. This value can be either positive or negative. Positive values are defined as electricity purchased from the utility. Negative values are defined as surplus electricity fed back into the grid.

Each ElectricLoadCenter also collects results from the individual generators that are connected to that load center.  The output variables available at the ElectricLoadCenter level are:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Electric Load Center Requested Electric Power [W]
    HVAC,Average,Electric Load Center Produced Electric Power [W]
    HVAC,Sum,Electric Load Center Produced Electric Energy [J]
    HVAC,Average,Electric Load Center Produced Thermal Rate [W]
    HVAC,Sum,Electric Load Center Produced Thermal Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Electric Load Center Requested Electric Power [W]

This output variable is the average electric power supply (in watts) requested by the load center from its generators for the time step being reported. For the baseload operating scheme, this output variable should equal the sum of the power supply requests for the ‘available' generators associated with this load center (ref. [ElectricLoadCenter:Generators](#electricloadcentergenerators)). In other cases, this output could be different from the sum of the power supply requests for the generators associated with this load center. For example, a generator might be requested to provide a certain amount of power but can only provide a fraction of the requested amount. In this case the load center will detect this shortfall and include it when calculating the power request for the next available generator. Therefore, the sum of the power supply requests for the individual generators associated with this load center (ref. [ElectricLoadCenter:Generators](#electricloadcentergenerators)) could be greater than the Electric Load Center Requested Electric Power output variable.

#### Electric Load Center Produced Electric Power [W]

#### Electric Load Center Produced Electric Energy [J]

These outputs are the sum of electrical energy and power produced by the generators attached to a particular load center.  The keywords for these reports are the unique names of [ElectricLoadCenter:Distribution](#electricloadcenterdistribution) objects.

#### Electric Load Center Produced Thermal Rate [W]

#### Electric Load Center Produced Thermal Energy [J]

These outputs are the sum of the thermal energy and power produced by the generators attached to a particular load center.  The keywords for these reports are the unique names of [ElectricLoadCenter:Distribution](#electricloadcenterdistribution) objects.

## ElectricLoadCenter:Generators

The [ElectricLoadCenter:Generators](#electricloadcentergenerators) object is used to provide a list of the generators to include in the simulation. The list includes the names and types of all the generators along with separate availability schedules, the rated power output, and thermal-to-electrical power ratio for each. Sets of five input fields are repeated for each generator. If more than 30 generators are needed, EnergyPlus will "auto-extend" to suit the needs but other interfaces (such as the IDF Editor may not). The user can always modify the Energy+.idd file to accommodate the extra fields necessary, but it is likely the next release of EnergyPlus will overwrite any user modifications.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the generators in the set.

#### FieldSet: Name, Object Type, Rated Power Output, Availability, Thermal Ratio up to 30

Each generator is specified by a set of 5 input fields that contain an identifying name, a keyword that indicates the type of generator, the rated output of the generator, the availability schedule of the generator, and the thermal to electrical power production ratio.

#### Field: Generator <x> Name

This alpha field contains the identifying name for the first generator in the list.

#### Field: Generator <x> Object Type

This alpha field contains the keyword for the type of generator.  The available types include:

- Generator:InternalCombustionEngine
- Generator:CombustionTurbine
- Generator:Photovoltaic
- Generator:FuelCell
- Generator:MicroCHP
- Generator:MicroTurbine
- Generator:WindTurbine

These keywords are the names of the input objects for the type of generator.

#### Field: Generator <x> Rated Electric Power Output

This numeric field contains the nominal electric power output to be requested from generator 1. It is normally equal to the rated power output of the generator in Watts. This value is used only for supervisory control and generator dispatch; the actual power output for each time step is determined by the generator models. This value affects how much a generator is loaded (i.e., requested electric power output) and can also impact the operation of an electric storage unit (e.g., LoadCenter:Storage:Simple) if one is connected to the associated load distribution center (ref. [ElectricLoadCenter:Distribution](#electricloadcenterdistribution)). Refer to Electrical Storage in the EnergyPlus Engineering Reference for more details regarding this input field and its relationship with electric storage.

#### Field: Generator <x> Availability Schedule Name

This alpha field is used as an identifying field for the generator availability schedule. If not blank, this field must contain the name of a schedule defined elsewhere in the input file.  A schedule value of <=0.0 indicates the generator is not available, while a schedule value > 0.0 indicates that the generator is available to operate. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Generator <x> Rated Thermal to Electrical Power Ratio

This numeric field contains the ratio of the rated thermal output to the rated electric output.  It is only needed and used if the operation scheme is set to **FollowThermal** or **FollowThermalLimitElectrical**.  This nominal ratio is only used for control and dispatch decisions.  This value should be generally consistent with the more detailed performance input for the individual generator component models but it is not used in those component models.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      ElectricLoadCenter:Generators,
        Backup Generators,       !- Name
        Cat Diesel,              !- Generator 1 Name
        Generator:InternalCombustionEngine,  !- Generator 1 Object Type
        50000,                   !- Generator 1 Rated Electric Power Output
        ON PEAK GENERATOR SCHEDULE,  !- Generator 1 Availability Schedule Name
        ,                        !- Generator 1 Rated Thermal to Electrical Power Ratio
        Solar Turbine,           !- Generator 2 Name
        Generator:CombustionTurbine,  !- Generator 2 Object Type
        30000,                   !- Generator 2 Rated Electric Power Output
        OFF PEAK GENERATOR SCHEDULE,  !- Generator 2 Availability Schedule Name
        ,                        !- Generator 2 Rated Thermal to Electrical Power Ratio
        Capstone C65,            !- Generator 3 Name
        Generator:Microturbine,  !- Generator 3 Object Type
        65000,                   !- Generator 3 Rated Electric Power Output
        MID PEAK GENERATOR SCHEDULE,  !- Generator 3 Availability Schedule Name
        ;                        !- Generator 3 Rated Thermal to Electrical Power Ratio
~~~~~~~~~~~~~~~~~~~~

### Outputs

A single output variable is available for each generator specified via this object as described below:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Generator Requested Electric Power [W]
~~~~~~~~~~~~~~~~~~~~

#### Generator Requested Electric Power [W]

This output variable represents the average electric power supply in Watts that is being requested from a specific generator for the time step being reported. In some instances the output value may be the Rated Electric Power Output specified for the generator in the [ElectricLoadCenter:Generators](#electricloadcentergenerators) object. If the generator is not available for a simulation time step (as indicated by its availability schedule), then the power supply request will be zero. The power supply request may be less than the rated electric power output if the overall electric power reduction target has already been met, partially or completely, by electric power produced by other generators. If an EnergyPlus Energy Management System is used to specify an electric power supply request for this generator, then that EMS request will be reflected in this output variable.

## ElectricLoadCenter:Inverter:Simple

This input object is used to model conversion from Direct Current (DC) to Alternating Current (AC) in an electric load center that contains photovoltaic modules.  Such a load center has an array of photovoltaic modules that feed an inverter DC power and the inverter produces AC power.  This input object is for the simplest inverter model and uses a fixed efficiency.  There are two other types of inverters with more complex models described below.

### Inputs

#### Field: Name

This field contains a unique name for the inverter.  The name entered must also be unique across all other types of inverters that may also be in the input file.

#### Field: Availability Schedule Name

This field contains the name of a schedule that describes when the inverter is available.  If the inverter is scheduled to not be available, by scheduling a value of 0, then it cannot produce AC power and will not consume ancillary power during standby.  Any non-zero schedule value means the inverter is available to produce AC power and consume ancillary power during standby.  If the Inverter is scheduled to be unavailable but it is being supplied with DC power anyway, then the energy is dissipated as heat. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Zone Name

This field contains the name of the thermal zone where the inverter is located.  Entering a valid name of zone here will direct EnergyPlus to include the inverter's losses as heat gains to the named thermal zone.  If the inverter is not within a thermal zone, then this field can be left blank (and the thermal energy associated with inverter losses is removed from the building model).

#### Field: Radiative Fraction

This field contains the fraction of inverter thermal losses that enter the zone as long-wave thermal radiation.  This should be a factor between 0.0 and 1.0.  The balance of the losses is convective.  This field is not used if the previous field for zone name is left blank.

#### Field: Inverter Efficiency

This field contains the value for inverter efficiency.  In the simple model, efficiency is a constant.  This is the so-called 1-term model.  Input data for different types of inverters is available at http://www.gosolarcalifornia.org/equipment/inverter_tests/summaries where the "CEC Efficiency" would be the value to enter here.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      ElectricLoadCenter:Inverter:Simple,
        Simple Ideal Inverter,   !- Name
        Always On,               !- Availability Schedule Name
        ,                        !- Zone Name
        0.0,                     !- Radiative Fraction
        1.0;                     !- Inverter Efficiency
~~~~~~~~~~~~~~~~~~~~

## ElectricLoadCenter:Inverter:FunctionOfPower

This input object is used to model conversion from Direct Current (DC) to Alternating Current (AC) in an electric load center that contains photovoltaic modules.  Such a load center has an array of photovoltaic modules that feed an inverter DC power and the inverter produces AC power.  This input object is for an inverter model where efficiency is a function of normalized power.

### Inputs

#### Field: Name

This field contains a unique name for the inverter.  The name entered must also be unique across all other types of inverters that may also be in the input file.

#### Field: Availability Schedule Name

This field contains the name of a schedule that describes when the inverter is available.  If the inverter is scheduled to not be available, by scheduling a value of 0, then it cannot produce AC power, nor contribute heat gains to the zone, and will not consume ancillary power during standby.  Any non-zero schedule value means the inverter is available to produce AC power, contribute heat gains to the zone and consume ancillary power during standby. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Zone Name

This field contains the name of the thermal zone where the inverter is located.  Entering a valid name of zone here will direct EnergyPlus to include the inverter's losses as heat gains to the named thermal zone.  If the inverter is not within a thermal zone, then this field can be left blank (and the thermal energy associated with inverter losses is removed from the building model).

#### Field: Radiative Fraction

This field contains the fraction of inverter thermal losses that enter the zone as long-wave thermal radiation.  This should be a factor between 0.0 and 1.0.  The balance of the losses is convective.  This field is not used if the previous field for zone name is left blank.

#### Field: Efficiency Function of Power Curve Name

This field contains the name of curve representing the relationship between DC power input into the inverter and the efficiency with which that power is converted to AC.  The curve can be either a [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), or [Curve:Cubic](#curvecubic).  The curve's "x" value is DC power input normalized by the following field.  The result of the curve should be power conversion efficiency expressed as a fraction between 0.0 and 1.0.

#### Field: Rated Maximum Continuous Input Power

This field contains the rated power input in watts.  This is value used to normalize input power for use with the curve named in the previous field.  This is DC power from the PV arrays going into the inverter.

#### Field: Minimum Efficiency

This field contains a minimum bound on the inverter efficiency.  This value will be used as a limit on the curve's result.

#### Field: Maximum Efficiency

This field contains a maximum bound on the inverter efficiency.  This value will be used as a limit on the curve's result.

#### Field: Minimum Power Output

This field contains a lower limit on the AC power produced by the inverter.  If the resulting power output would be below this level, then the inverter is assumed to not produce any power and is in stand by mode.  All DC power input is lost (unless it is going into storage).

#### Field: Maximum Power

This field contains an upper limit on the AC power produced by the inverter.  If the resulting power output would be above this level, then the power produced is capped at this level with the rest of input power converted to losses (unless it is going into storage).

#### Field: Ancillary Power Consumed In Standby

This field contains the ancillary power in watts used by the inverter when not producing AC power.  This is AC electricity consumed in standby mode.  Standby mode occurs when the inverter is scheduled to be available but the incoming DC power is too low.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      ElectricLoadCenter:Inverter:FunctionOfPower,
        Curve Inverter,          !- Name
        ALWAYS_ON,               !- Availability Schedule Name
        ,                        !- Zone Name
        0.3,                     !- Radiative Fraction
        Inverter Curve,          !- Efficiency Function of Power Curve Name
        15000,                   !- Rated Maximum Continuous Input Power {W}
        0.0,                     !- Minimum Efficiency
        1.0,                     !- Maximum Efficiency
        200,                     !- Minimum Power Output {W}
        20000,                   !- Maximum Power Output {W}
        0.0;                     !- Ancillary Power Consumed In Standby {W}
~~~~~~~~~~~~~~~~~~~~

## ElectricLoadCenter:Inverter:LookUpTable

This input object is used to model conversion from Direct Current (DC) to Alternating Current (AC) in an electric load center that contains photovoltaic modules.  Such a load center has an array of photovoltaic modules that feed an inverter DC power and the inverter produces AC power.  This input object is for an inverter model where efficiency is interpolated using a look up table.

This object was designed for use with data contained in the Inverter Performance Test Summaries available at http://www.gosolarcalifornia.org/equipment/inverter_tests/summaries.  All of the data needed for this object can be found there by make and model of inverter.  The core of the data set is a 3x6 matrix of efficiency at different voltage and power levels.  These form the look up table used in the model.  Note that EnergyPlus' PV models currently model only power and not the voltage and current situation.  Therefore, only the nominal voltage data are used.  This is the middle row of data in the Go Solar summaries.

### Inputs

#### Field: Name

This field contains a unique name for the inverter.  The name entered must also be unique across all other types of inverters that may also be in the input file.

#### Field: Availability Schedule Name

This field contains the name of a schedule that describes when the inverter is available.  If the inverter is scheduled to not be available, by setting a value of 0, then it cannot produce AC power, nor contribute heat gains to the zone, and will not consume ancillary power during standby.  Any non-zero schedule value means the inverter is available to produce AC power, contribute heat gains to the zone and consume ancillary power during standby. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Zone Name

This field contains the name of the thermal zone where the inverter is located.  Entering a valid name of zone here will direct EnergyPlus to include the inverter's losses as heat gains to the named thermal zone.  If the inverter is not within a thermal zone, then this field can be left blank (and the thermal energy associated with inverter losses is removed from the building model).

#### Field: Radiative Fraction

This field contains the fraction of inverter thermal losses that enter the zone as long-wave thermal radiation.  This should be a factor between 0.0 and 1.0.  The balance of the losses is convective.  This field is not used if the previous field for zone name is left blank.

#### Field: Rated Maximum Continuous Output Power

This field contains the rated maximum continuous output power in watts.

#### Field: Night Tare Loss Power

This field contains the "night tare loss" in watts.  This is modeled as ancillary power consumed during standby.

#### Field: Nominal Voltage Input

This field contains the nominal DC input voltage in volts.  This is "Vnom" in the Go Solar test summaries.

#### Field: Efficiency at 10% Power and Nominal Voltage

This field contains the fractional efficiency at nominal voltage and 10% power.

#### Field: Efficiency at 20% Power and Nominal Voltage

This field contains the fractional efficiency at nominal voltage and 20% power.

#### Field: Efficiency at 30% Power and Nominal Voltage

This field contains the fractional efficiency at nominal voltage and 30% power.

#### Field: Efficiency at 50% Power and Nominal Voltage

This field contains the fractional efficiency at nominal voltage and 50% power.

#### Field: Efficiency at 75% Power and Nominal Voltage

This field contains the fractional efficiency at nominal voltage and 75% power.

#### Field: Efficiency at 100% Power and Nominal Voltage

This field contains the fractional efficiency at nominal voltage and 100% power.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      ElectricLoadCenter:Inverter:LookUpTable,
        PV Inverter,             !- Name
        ALWAYS_ON,               !- Availability Schedule Name
        ,                        !- Zone Name
        0.25,                    !- Radiative Fraction
        14000,                   !- Rated Maximum Continuous Output Power {W}
        200.0,                   !- Night Tare Loss Power {W}
        368,                     !- Nominal Voltage Input {V}
        0.839,                   !- Efficiency at 10% Power and Nominal Voltage
        0.897,                   !- Efficiency at 20% Power and Nominal Voltage
        0.916,                   !- Efficiency at 30% Power and Nominal Voltage
        0.931,                   !- Efficiency at 50% Power and Nominal Voltage
        0.934,                   !- Efficiency at 75% Power and Nominal Voltage
        0.930;                   !- Efficiency at 100% Power and Nominal Voltage
~~~~~~~~~~~~~~~~~~~~

### Outputs

Each inverter can report the following seven output variables.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Inverter DC to AC Efficiency [ ]
    HVAC,Average,Inverter DC Input Elecric Power [W]
    HVAC,Sum,Inverter DC Input Electric Energy [J]
    HVAC,Average,Inverter AC Output Electric Power [W]
    HVAC,Sum,Inverter AC Output Electric Energy [J]
    HVAC,Average,Inverter Thermal Loss Rate [W]
    HVAC,Sum,Inverter Thermal Loss Energy [J]
    HVAC,Sum,Inverter Ancillary AC Electric Energy [J]
    HVAC,Average,Inverter Ancillary AC Electric Power [W]
~~~~~~~~~~~~~~~~~~~~

#### Inverter DC to AC Efficiency [ ]

This is the efficiency with which DC power is converted to AC power by the inverter.

#### Inverter DC Input Elecric Power [W]

#### Inverter DC Input Electric Energy [J]

These outputs are total electricity power or energy fed into the inverter.  This is Direct Current from photovoltaics (or DC-based electrical storage) going into the inverter.

#### Inverter AC Output Electric Power [W]

#### Inverter AC Output Electric Energy [J]

These outputs are the total electricity power or energy produced by the inverter.  This is Alternating Current going out of the inverter.

#### Inverter Thermal Loss Rate [W]

#### Inverter Thermal Loss Energy [J]

These outputs are the thermal power or energy losses in the inverter that stem from converting from DC to AC.

#### Inverter Ancillary AC Electric Power [W]

#### Inverter Ancillary AC Electric Energy [J]

These outputs are the Alternating Current electricity consumed by the inverter.  These are ancillary, or night tare loss, power uses by the inverter and modeled as if powered by the building's grid connection.  These ancillary power draws generally occur when the inverter is not generating power but waiting in a standby mode ready to begin generating power.

## ElectricLoadCenter:Storage:Simple

This input object is used to model storage of electricity in an electric load center.  This is a simple model that does not attempt to represent any of the characteristics of a real storage device such as a battery.  The type of power, AC or DC, depends on the configuration chosen as the Electrical Buss Type in the [ElectricLoadCenter:Distribution](#electricloadcenterdistribution) object.

### Inputs

#### Field: Name

This field contains a unique name for the electric storage device.

#### Field: Availability Schedule Name

This field contains the name of a schedule that describes when the storage device is available.  If storage is not available, by scheduling a value of 0, then no electrical energy can be stored or drawn from the device.  Any non-zero schedule value means the devices is available. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Zone Name

This field contains the name of the thermal zone where the storage device is located.  Entering a valid name of a zone here will direct EnergyPlus to include the storage losses as heat gains to the name thermal zone.  If the storage is not within a therma zone, then this field can be left blank.

#### Field: Radiative Fraction for Zone Heat Gains

This field contains the fraction of storage losses that enter the zone as long-wave thermal radiation.  This should be a factor between 0.0 and 1.0.  The balance of the losses are convective.  This field is not used if the previous field for zone name is left blank.

#### Field: Nominal Energetic Efficiency for Charging

This field contains the charging efficiency.  This is the energetic efficiency of storing electrical energy in the storage device.  A value of 1.0 means the device does not lose any energy when charging.

#### Field: Nominal Discharging Energetic Efficiency

This field contains the discharging efficiency.   This is the energetic efficiency of drawing electrical energy from the storage device.  A value of 1.0 means the device does not lose any energy when drawing power.

#### Field: Maximum Storage Capacity

This field describes the maximum amount of energy that can be stored in the device in Joules.  Once the storage device is full, no additional energy can be stored in it.

#### Field: Maximum Power for Discharging

This field describes the maximum rate at which electrical power can be discharged from the storage device in watts.

#### Field: Maximum Power for Charging

This field describes the maximum rate at which electrical power can be stored in the device in watts.

#### Field: Initial State of Charge

This field describes the value for the initial state of charge in Joules.  This is used to model the storage device as having some amount of stored energy at the beginning of the simulation period.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      ElectricLoadCenter:Storage:Simple,
        Battery,                 !- Name
        ALWAYS_ON,               !- Availability Schedule Name
        ,                        !- Zone Name
        0.0,                     !- Radiative Fraction for Zone Heat Gains
        0.7,                     !- Nominal Energetic Efficiency for Charging
        0.7,                     !- Nominal Discharging Energetic Efficiency
        1.0E11,                  !- Maximum Storage Capacity {J}
        50000,                   !- Maximum Power for Discharging {W}
        25000,                   !- Maximum Power for Charging {W}
        1.0E10;                  !- Initial State of Charge {J}
~~~~~~~~~~~~~~~~~~~~

### Outputs

Each electrical storage device can report the following seven output variables.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Electric Storage Charge State [J]
    HVAC,Average,Electric Storage Charge Power [W]
    HVAC,Sum,Electric Storage Charge Energy [J]
    HVAC,Average,Electric Storage Discharge Power [W]
    HVAC,Sum,Electric Storage Discharge Energy [J]
    HVAC,Average,Electric Storage Thermal Loss Rate [W]
    HVAC,Sum,Electric Storage Thermal Loss Energy [J]
    HVAC,Sum,Electric Storage Production Decrement Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Electric Storage Charge State [J]

This output is the state of charge of the storage device.  State of charge is the amount of electrical energy stored in the device at a point of time.  The amount of energy stored in tracked in Joules.

#### Electric Storage Charge Power [W]

#### Electric Storage Charge Energy [J]

These outputs are total electricity power or energy fed into the storage device.  This is the rate or amount of charging.

#### Electric Storage Production Decrement Energy [J]

This output is the total electricity energy decremented from electricity production because it has fed into the storage device.  This output has the opposite sign of "Electric Storage Charge Energy" but is otherwise similar.  This decrement output variable is also a meter associated with the resource type "ElectricityProduced" that reduces the metered electricity production to account for power that went into storage after production.

#### Electric Storage Discharge Power [W]

#### Electric Storage Discharge Energy [J]

These outputs are total electricity power or energy drawn from the storage device.  This is the rate or amount of discharging.  The energy from storage output variable is also a meter associated with the resource type "ElectricityProduced" that increases the metered electricity production to account for power that has come back out of storage.

#### Electric Storage Thermal Loss Rate [W]

#### Electric Storage Thermal Loss Energy [J]

These outputs are the thermal power or energy losses from both charging and drawing electrical power in or out of the storage device.  These losses are the result of inefficiencies in charging and drawing.

## ElectricLoadCenter:Storage: Battery

This object uses the kinetic battery model (KiBaM) to simulate rechargeable battery banks in an electrical load center. The battery bank is a collection of one or more individual battery modules. Given the surplus or deficit power from the electrical system and the state of charge from the previous time step, this object can model the voltage, current, and energy losses with charging and discharging during each time step. The cumulative battery damage can be also modeled and reported at the end of each simulation run.

This object allows the simulation of both lead-acid and Nickel Cadmium batteries. With input parameters derived from specific battery tests, the object is expected to support other battery types such as Lithium-ion batteries.

The kinetic battery model assumes that part of the battery's energy storage capacity is immediately available in discharging or charging while the rest is chemically bound. As a function of constant current, the battery capacity is related to three parameters: the maximum capacity at infinitesimal current, the capacity ratio of available charges, and the conversion ratio between available charges and bound charges. These parameters are usually obtained via curve fitting based on battery data sheets or test data.

Each individual battery module is modeled as a voltage source in series with an electrical resistance.  KiBaM assumes that the internal resistance is constant and the open circuit voltage varies with the electric current, the state of charge and the operation mode (charging or discharging). For an individual battery module, the open circuit voltage at any time is correlated to the voltage at fully charged/discharged state and three other regression coefficients.  These regression coefficients are usually obtained via curve fitting based on battery test data.

The object offers user the option to perform battery life calculation. If battery life is modeled, the user needs to provide a group of coefficients for the correlation between the number of cycles for battery failure and the corresponding cycle range. More detailed information can be found from the Engineering Reference.

### Field: Name

This alpha field contains the identifying name for the battery bank.

### Field: Availability Schedule Name

This alpha field contains the schedule name (ref. Schedule objects) that describes when the battery is available. A schedule value greater than 0 (usually 1 is used) indicates that electrical energy can be stored or drawn from the battery. A value less than or equal to 0 (usually 0 is used) denotes that the battery is not available. If this field is blank, the schedule has values of 1 for all time periods.

### Field: Zone Name

This field contains the name of the thermal zone where the battery is located. Entering a valid name of zone here will direct EnergyPlus to include the energy storage losses as heat gains to the named thermal zone. If the battery is not within a thermal zone, this field can be left blank and the thermal energy associated with storage losses is removed from the building model.

### Field: Radiative Fraction

This field contains the fraction of storage losses that enter the zone as long-wave thermal radiation. This numeric filed should have a value between 0.0 and 1.0. The balance of the losses is convective. This field is not used if the previous field for zone name is left blank.

### Field: Number of Battery Modules in Parallel

This field defines the number of modules connected in parallel in the battery bank.

### Field: Number of Battery Modules in Series

This field defines the number of modules connected in series in the battery bank. The total number of modules in a battery bank is equal to the number of modules in parallel times the number of modules in series.

### Field: Maximum Module Capacity

This field indicates the maximum capacity of one battery module. It is evaluated as the total Amp-hours available when a full battery is discharged at infinitesimal current. This capacity is for an individual battery module, not for the whole battery bank. The maximum capacity can be found from manufacture's data or derived from test data by curve fitting.

### Field: Initial Fractional State of Charge

This field describes the initial state of charge in terms of the fraction of maximum capacity as defined in the previous field.

### Field: Fraction of Available Charge Capacity

The kinetic battery model assumes that battery is a two-tank electrical energy storage device: an available tank and a bound tank. The available tank can be immediately charged or discharged, while the bound tank can only be charged or discharged via the available tank. This field specifies the fraction of total charge in the battery that is part of the available tank. The ratio of available charge capacity to total capacity is a parameter usually derived from test data by curve fitting.

### Field: Change Rate from Bound Charge to Available Charge

This field specifies the rate at which the charge flows between the available tank and the bound tank. It is a parameter used to calculate the conversion between available charge and chemically bound charge when charging and discharging the battery. This parameter is usually derived from test data by curve fitting.

### Field: Fully Charged Module Open Circuit Voltage

This field indicates the open circuit voltage for a fully charged battery module. It can be found from manufacture's data or battery tests.

### Field: Fully Discharged Module Open Circuit Voltage

This field indicates the open circuit voltage for a fully discharged battery module. It can be found from manufacture's data or battery tests.

### Field: Voltage Change Curve Name for Charging

This field specifies the name of a rectangular hyperbola type 2 performance curve (ref: [Curve:RectangularHyperbola2](#curverectangularhyperbola2) in Performance Curves) that determines the change of open circuit voltage (![](media/image419.png) ) as a function of the battery state of charge in charging. The change of open circuit voltage is relative to a fully discharged battery.

The curve has the following form:

![](media/image420.png)\


where X is the battery charge divided by the maximum capacity at a given current. More details can be found from the Engineering Reference.

### Field: Voltage Change Curve Name for Discharging

This field specifies the name of a rectangular hyperbola type 2 performance curve (ref: [Curve:RectangularHyperbola2](#curverectangularhyperbola2) in Performance Curves) that determines the change of open circuit voltage (![](media/image421.png) ) as a function of the battery state of charge in discharging. The change of open circuit voltage is relative to a fully charged battery.

The curve has the following form:

![](media/image422.png)\


where X is the removed charge divided by the maximum capacity at a given current. More details can be found from the Engineering Reference.

### Field: Module Internal Electrical Resistance

This field specifies the battery internal resistance in ohms. In theory, the electrical resistance within the battery varies with the state of charge and depends on whether it is in charging or discharging. In the kinetic battery model, the internal resistance is assumed as constant and the terminal voltage varies with current and state of charge. The internal resistance may be obtained from the battery manufacture or battery test data. Note that the field is for an individual module, not for the whole battery bank.

### Field: Maximum Module Discharging Current

This field indicates the maximum current at which the battery can be discharged continuously. The limit on discharge current is usually defined by the battery manufacture to avoid battery damage. The limit is for an individual battery module.

### Field: Module Cut-off Voltage

This field specifies the minimum allowable voltage, below which the battery is generally regarded as empty. The cut-off voltage is usually defined by the battery manufacture and it is for an individual battery module.

### Field: Module Charge Rate Limit

This field specifies the limit on charging current relative to the remaining charge until the battery is full. This limit reflects the common practice that the charge rate is reduced as the battery gets more charged.

### Field: Battery Life Calculation

This field indicates whether the battery life model is activated in the simulation. If the battery life model is activated, the following five inputs on curve coefficients are required; otherwise, they are not used.

### Field: Number of Cycle Bins

This field specifies the number of equally ranged cycle bins in battery life simulation. If 10 bins are used, the cycle ranges will include 10%, 20%, …, 100%.

### Field: Battery Cycle Life Curve Name

This field specifies the name of a double exponential decay curve (ref: [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay) in Performance Curves) that correlates the cycles of battery failure (C~F~) and fractional depth of discharge (R). The curve is:

![](media/image423.png)\


An example input of the ElectricLoadCenter:Storage: Battery input is:

~~~~~~~~~~~~~~~~~~~~

      ElectricLoadCenter:Storage:Battery,
        Lead Acid Battery A,  !-Name
        Always On,            !- Availability Schedule Name
        ,                     !- Zone Name
        ,                     !- Radiative Fraction
        1,                    !- Number of Battery Modules in Parallel
        1,                    !- Number of Battery Modules in Series
        86.1,                 !- Maximum Module Capacity {Ah}
        1.0,                  !- Initial Fractional State of Charge
        0.3747,               !- Fraction of Available Charge Capacity
        0.5874,               !- Change Rate from Bound Charge to Available Charge {hr^-1^}
        12.60,                !- Fully Charged Module Open Circuit Voltage {V}
        12.37,                !- Fully Discharged Module Open Circuit Voltage {V}
        BatteryChargeCurve,   !- Voltage Change Curve Name for Charging
        BatteryDischargeCurve,!- Voltage Change Curve Name for Discharging
        0.0538,               !- Module Internal Electrical Resistance {Ohm}
        100,                  !- Maximum Module Discharging Current {A}
        10.2,                 !- Module Cut-off Voltage {V}
        1,                    !- Module Charge Rate Limit {1/hr}
        Yes,                  !- Battery Life Calculation
         10,                  !- Number of Cycle Bins
        BatteryCycleCurve;    !- Battery Cycle Life Curve Name

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Curve:RectangularHyperbola2,
        BatteryChargeCurve,   !- Name
        -0.2765,              !- Coefficient1 C1
        -93.27,               !- Coefficient2 C2
        0.0068,               !- Coefficient3 C3
        0,                    !- Minimum Value of x
        1.0,                  !- Maximum Value of x
        -100,                 !- Minimum Curve Output
        100;                  !- Maximum Curve Output

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Curve:RectangularHyperbola2,
        BatteryDischargeCurve,   !- Name
        0.0899,                  !- Coefficient1 C1
        -98.24,                  !- Coefficient2 C2
        -0.0082,                 !- Coefficient3 C3
        0,                       !- Minimum Value of x
        1.0,                     !- Maximum Value of x
        -100,                    !- Minimum Curve Output
        100;                     !- Maximum Curve Output

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Curve:DoubleExponentialDecay,
        BatteryCycleCurve,     !- Name
        1380,                  !- Coefficient1 C1
        6834,                  !- Coefficient2 C2
        -8.75,                 !- Coefficient3 C3
        6747,                  !- Coefficient4 C4
        -6.22,                 !- Coefficient5 C5
        0,                     !- Minimum Value of x
        1.0;                   !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

## ElectricLoadCenter: Storage:Battery Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Electric Storage Operating Mode Index []
    HVAC,Average,Electric Storage Charge State[Ah]
    HVAC,Average, Electric Storage Charge Fraction []
    HVAC,Average,Electric Storage Charge Power[W]
    HVAC,Sum,Electric Storage Charge Energy [J]
    HVAC,Average,Electric Storage Discharge Power[W]
    HVAC,Sum,Electric Storage Discharge Energy [J]
    HVAC,Average,Electric Storage Total Current[A]
    HVAC,Average, Electric Storage Total Voltage [V]
    HVAC,Average,Electric Storage Thermal Loss Rate [W]
    HVAC,Average, Electric Storage Degradation Fraction []
    HVAC,Sum,Electric Storage Production Decrement Energy [J]
    HVAC,Sum,Electric Storage Thermal Loss Energy [J]
~~~~~~~~~~~~~~~~~~~~

### Electric Storage Operating Mode Index []

This output reports the battery mode of operation: 0 for idle; 1 for discharging; 2 for charging. It is expected that more operation modes would be added when a smart and active power dispatch controller is used in future.

### Electric Storage Charge State [Ah]

The state of charge is expressed as the amount of charge stored in the battery at a point of time. It has the same unit as the maximum capacity. This value is given for an individual battery module

### Electric Storage Charge Fraction []

This output is the ratio between the electrical storage state of charge and the maximum capacity.

### Electric Storage Charge Power [W]

### Electric Storage Charge Energy [J]

These outputs are total electricity power or energy fed into the battery. This is the rate of amount of charging.

### Electric Storage Production Decrement Energy [J]

This output is the total electricity energy decremented from electricity production because it has fed into the battery. This output has the opposite sign of "Electric Storage Charge Energy" but is otherwise similar. This decrement output variable is also a meter associated with the resource type "ElectricityProduced" that reduces the metered electricity production to account for power that went into storage after production.

### Electric Storage Discharge Power [W]

### Electric Storage Discharge Energy [J]

These outputs are total electricity power or energy drawn from the battery. This is the rate or amount of discharging. The energy from storage output variable is also a meter associated with the resource type "ElectricityProduced" that increases the metered electricity production to account for power that has come back out of storage.

### Electric Storage Total Current [A]

This output is the current to or from the battery bank depending on whether the battery is in the state of charging or discharging. The value is positive for discharging and negative for charging.

### Electric Storage Total Voltage [V]

This output is the total terminal voltage of the battery bank.

### Electric Storage Thermal Loss Rate [W]

### Electric Storage Thermal Loss Energy [J]

These outputs are the thermal power or energy losses from both charging and drawing electrical power in or out of the storage device. These losses are due to the battery internal resistance in charging and discharging.

### Electric Storage Degradation Fraction []

This output reports the fractional battery life used up at a point of time. For example, a value of 0.4 at the end of one year simulation means that the 40% of the battery life is used up, so the battery needs to be replaced every two and a half years.

## Generator:InternalCombustionEngine

The Internal Combustion (IC) Engine generator uses a modified Otto cycle. This generator model uses the electrical load and rated engine generator size to compute part-load ratios (PLR). Fuel energy input and recoverable jacket and lube oil heat are then computed. Finally, the recoverable exhaust heat is calculated.

Manufacturer's curves or tables must be obtained for IC engine generators to derive the equipment performance parameters that are specified in the quadratic curve fits. Note that simple transformation of the form of the manufacturer's curves may be required. Electric energy output/fuel energy input is related to the part-load ratio (i.e., electric/load generator capacity) with a polynomial. Recoverable jacket heat/fuel energy equipment, recoverable lube oil heat/fuel energy input equipment performance, total exhaust heat/fuel energy input equipment, exhaust gas temperature/fuel energy input equipment performance parameters are all specified with a quadratic curve fit.

When the heat recovery curve fits = 0.0, this means that no heat recovery is done on the IC engine generator exhaust gas; and no heat recovery is specified.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the IC engine generator.

#### Field: Rated Power Output

The design nominal capacity of the generator in Watts [W].

#### Field: Electric Circuit Node Name

This named node contains where the electric power is put in the program simulation for use by the electric equipment in the Load Center.

#### Field: Minimum Part Load Ratio

This field contains the minimum allowed operating fraction of full load. The input value must be >=0.0 and <=1.0.

#### Field: Maximum Part Load Ratio

This field contains the maximum allowed operating fraction of full load. The input value must be >0.0 and <=1.0.

#### Field: Optimum Part Load Ratio

This alpha field contains the optimal operating fraction of full load. This is the part load ratio that gives the optimal production of electric power with the least amount of fuel usage.

#### Field: Shaft Power Curve Name

This alpha field contains the name of the shaft power curve. The curve itself is specified separately using a curve object (ref: [Curve:QuadLinear](#curvequadlinear)). The Shaft Power Curve is a quadratic equation that determines the electric output versus the fuel used. Used to calculate the electric energy output divided by the fuel energy consumption as a function of part-load ratio. The defining equation is:

![](media/image424.png)\


where RL is the Ratio of Load to Rated Power Output.

#### Field: Jacket Heat Recovery Curve Name

This alpha field contains the name of the Recovery Jacket Heat curve. The curve itself is specified separately using a curve object (ref: [Curve:QuadLinear](#curvequadlinear)). The Jacket Heat Recovery Curve is a quadratic equation that determines the ratio of recovery jacket heat to fuel energy. Used to calculate the recoverable jacket heat as a function of part-load ratio.

The defining equation is:

![](media/image425.png)\


where RL is the Ratio of Load to Rated Power Output.

#### Field: Lube Heat Recovery Curve Name

This alpha field contains the name of the Recovery Lube Heat curve. The curve itself is specified separately using a curve object (ref [Curve:QuadLinear](#curvequadlinear)). The Lube Heat Recovery Curve is a quadratic equation that determines the ratio of recovery lube heat to fuel energy. Used to calculate the recoverable lube oil heat as a function of part-load ratio. The defining equation is:

![](media/image426.png)\


where RL is the Ratio of Load to Rated Power Output.

#### Field: Total Exhaust Energy Curve Name

This alpha field contains the name of the Total Exhaust Energy curve. The curve itself is specified separately using a curve object (ref [Curve:QuadLinear](#curvequadlinear) object). The Total Exhaust Energy Curve is a quadratic equation that determines the ratio of total exhaust energy to fuel energy. Used to calculate the total exhaust heat as a function of part-load ratio. The defining equation is:

![](media/image427.png)\


where RL is the Ratio of Load to Rated Power Output.

#### Field: Exhaust Temperature Curve Name

This alpha field contains the name of the Exhaust Temperature curve. The curve itself is specified separately using a curve object (ref [Curve:QuadLinear](#curvequadlinear) object). The Exhaust Temperature Curve is a quadratic equation that determines the absolute exhaust temperature. Used to determine the exhaust gas temperature as a function of part-load ratio. The defining equation is:

![](media/image428.png)\


where RL is the Ratio of Load to Rated Power Output.

#### U-Factor Times Area (UA) Curve

The UA curve applies to the exhaust gas heat exchanger. The curve is an equation that determines the overall heat transfer coefficient for the exhaust gases with the stack. The heat transfer coefficient ultimately helps determine the exhaust stack temperature. The defining equation is:

![](media/image429.png)\


The following two fields contain the coefficients for the equation.

#### Field: Coefficient 1 of U-Factor Times Area Curve

This numeric field contains the first coefficient for the overall heat transfer coefficient curve.

#### Field: Coefficient 2 of U-Factor Times Area Curve

This numeric field contains the second (exponential) coefficient for the overall heat transfer coefficient curve.

#### Field: Maximum Exhaust Flow per Unit of Power Output

Maximum exhaust flow per unit capacity for the generator. The parameter sets an upper limit on exhaust gas flow and exhaust gas heat recovery for the generator. Units: kg/sec per kW capacity

#### Field: Design Minimum Exhaust Temperature

This is the design engine stack saturated steam temperature in degrees C.

#### Field: Fuel Higher Heating Value

This numeric field contains the higher heating value of the fuel used in kJ/kg.

#### Field: Design Heat Recovery Water Flow Rate

Design water volumetric flow rate through heat recovery loop in m^3^/sec.

#### Field: Heat Recovery Inlet Node Name

This alpha field contains the identifying name for the generator's heat recovery side inlet node.

#### Field: Heat Recovery Outlet Node Name

This alpha field contains the identifying name for the generator's heat recovery side outlet node.

#### Field: Fuel Type

This alpha field determines the type of fuel that the generator uses.  The default is Diesel. Valid values are: **NaturalGas**, **PropaneGas**, **Diesel**, **Gasoline**, **FuelOil#1**, **FuelOil#2, OtherFuel1,** and **OtherFuel2**.

#### Field: Heat Recovery Maximum Temperature

This field sets the maximum temperature that this piece of equipment can produce for heat recovery. The idea behind this field is that the current models do not take temperatures into account for availability and they just pass Q's around the loop without a temperature limit. This temperature limit puts an upper bound on the recovered heat and limits the max temperatures leaving the component.

As temperatures in the loop approach the maximum temperature, the temperature difference between the entering water and the surfaces in the piece of equipment becomes smaller. For the given heat recovery flow rate and that temperature difference the amount of heat recovered will be reduced, and eventually there will be no heat recovered when the entering water temperature is equal to the maximum temperature specified by the user in this field. The reduced amount of heat recovered will diminish if the temperature of the loop approach is the maximum temperature, and this will show up in the reporting. This allows the user to set the availability or the quality of the heat recovered for usage in other parts of the system or to heat domestic hot water supply.

The temperature is specified in degrees C.

An IDF example showing how it is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:InternalCombustionEngine,
        Cat Diesel,              !- Name
        50000,                   !- Rated Power Output {W}
        Generator Diesel Electric Node,  !- Electric Circuit Node Name
        0.15,                    !- Minimum Part Load Ratio
        1.0,                     !- Maximum Part Load Ratio
        0.65,                    !- Optimum Part Load Ratio
        BG Shaft Power Curve,    !- Shaft Power Curve Name
        BG Recovery Jacket Heat Curve,  !- Jacket Heat Recovery Curve Name
        BG Recovery Lube Heat Curve,  !- Lube Heat Recovery Curve Name
        BG Total Exhaust Energy Curve,  !- Total Exhaust Energy Curve Name
        BG Exhaust Temperature Curve,  !- Exhaust Temperature Curve Name
        0.00952329,              !- Coefficient 1 of U-Factor Times Area Curve
        0.9,                     !- Coefficient 2 of U-Factor Times Area Curve
        0.00000063,              !- Maximum Exhaust Flow per Unit of Power Output {(kg/s)/W}
        150,                     !- Design Minimum Exhaust Temperature {C}
        45500,                   !- Fuel Higher Heating Value {kJ/kg}
        0.0,                     !- Design Heat Recovery Water Flow Rate {m3/s}
        ,                        !- Heat Recovery Inlet Node Name
        ,                        !- Heat Recovery Outlet Node Name
        Diesel;                  !- Fuel Type

      Curve:Quadratic,
        BG Shaft Power Curve,    !- Name
        0.09755,                 !- Coefficient1 Constant
        0.6318,                  !- Coefficient2 x
        -0.4165,                 !- Coefficient3 x**2
        0,                       !- Minimum Value of x
        1;                       !- Maximum Value of x

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Curve:Quadratic,
        BG Recovery Jacket Heat Curve,  !- Name
        0.25,                    !- Coefficient1 Constant
        0,                       !- Coefficient2 x
        0,                       !- Coefficient3 x**2
        0,                       !- Minimum Value of x
        1;                       !- Maximum Value of x

      Curve:Quadratic,
        BG Recovery Lube Heat Curve,  !- Name
        0.15,                    !- Coefficient1 Constant
        0,                       !- Coefficient2 x
        0,                       !- Coefficient3 x**2
        0,                       !- Minimum Value of x
        1;                       !- Maximum Value of x

      Curve:Quadratic,
        BG Total Exhaust Energy Curve,  !- Name
        0.1,                     !- Coefficient1 Constant
        0,                       !- Coefficient2 x
        0,                       !- Coefficient3 x**2
        0,                       !- Minimum Value of x
        1;                       !- Maximum Value of x

      Curve:Quadratic,
        BG Exhaust Temperature Curve,  !- Name
        425,                     !- Coefficient1 Constant
        0,                       !- Coefficient2 x
        0,                       !- Coefficient3 x**2
        0,                       !- Minimum Value of x
        1;                       !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

### Outputs

The output variables that are available for the IC engine generator are:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Generator Produced Electric Power [W]
    HVAC,Sum,Generator Produced Electric Energy [J]

    HVAC,Average,Generator Lube Heat Recovery Rate [W]
    HVAC,Sum,Generator Lube Heat Recovery Energy [J]

    HVAC,Average,Generator Jacket Heat Recovery Rate [W]
    HVAC,Sum,Generator Jacket Heat Recovery Energy [J]

    HVAC,Average,Generator Exhaust Heat Recovery Rate [W]
    HVAC,Sum,Generator Exhaust Heat Recovery Energy [J]

    HVAC,Average,Generator Total Heat Recovery Rate [W]
    HVAC,Sum,Generator Total Heat Recovery [J]

    HVAC,Average,Generator Exhaust Air Temperature [C]

    HVAC,Average,Generator Heat Recovery Inlet Temperature [C]
    HVAC,Average,Generator Heat Recovery Outlet Temperature [C]
    HVAC,Average,Generator Heat Recovery Mass Flow Rate [kg/s]

    HVAC,Average,Generator Fuel HHV Basis Rate [W]
    HVAC,Sum,Generator Fuel HHV Basis Energy [J]

    HVAC,Average,Generator <Fuel Type> Rate [W]
    HVAC,Sum,Generator <Fuel Type> Energy [J]
    HVAC,Average,Generator <Fuel Type> Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### Generator Produced Electric Power [W]

#### Generator Produced Electric Energy [J]

These outputs are the electric power output from the electric generator. Energy is metered on Cogeneration:ElectricityProduced, ElectricityProduced:Plant, and ElectricityProduced:Facility.

#### Generator Lube Heat Recovery Rate [W]

#### Generator Lube Heat Recovery Energy [J]

#### Generator Jacket Heat Recovery Rate [W]

#### Generator Jacket Heat Recovery Energy [J]

#### Generator Exhaust Heat Recovery Rate [W]

#### Generator Exhaust Heat Recovery Energy [J]

#### Generator Produced Thermal Rate [W]

#### Generator Produced Thermal Energy [J]

For electric generators with heat recovery these outputs are the components of recoverable energy available. For the IC engine generator, the following heat recovery components are reported:  Lube (engine lubricant), Jacket (engine coolant), and Exhaust (engine exhaust).  These components are totaled together for Generator Produced Thermal Rate and Generator Produced Thermal Energy reports. Generator Lube Heat Recovery Energy, Generator Jacket Heat Recovery Energy, and Generator Exhaust Heat Recovery Energy are metered on HeatRecovery:EnergyTransfer, EnergyTransfer:Plant, and EnergyTransfer:Facility.

#### Generator Exhaust Air Temperature [C]

This is the exhaust temperature leaving the engine.

#### Generator Heat Recovery Inlet Temperature [C]

#### Generator Heat Recovery Outlet Temperature [C]

#### Generator Heat Recovery Mass Flow Rate [kg/s]

These outputs are the heat recovery inlet and outlet temperatures and flow rate for generators with heat recovery.

#### Generator Fuel HHV Basis Rate [W]

#### Generator Fuel HHV Basis Energy [J]

These outputs are the generator's fuel energy and use rate.  The energy content of the fuel is based on the higher heating value (HHV).

#### Generator <Fuel Type> Rate [W]

#### Generator <Fuel Type> Energy [J]

#### Generator <Fuel Type> Mass Flow Rate [kg/s]

If the fuel type is Diesel, then Diesel is shown as the fuel type. They are the diesel fuel input to the electric generator. Consumption is metered on Cogeneration:Diesel, Diesel:Plant, and Diesel:Facility.

If the fuel type is Natural Gas, then Gas is shown as the fuel type. They are the natural gas fuel input to the electric generator. Consumption is metered on Cogeneration:Gas, Gas:Plant, and Gas:Facility.

If the fuel type is Propane, then Propane is shown as the fuel type. They are the propane fuel input to the electric generator. Consumption is metered on Cogeneration:Propane, Propane:Plant, and Propane:Facility.

If the fuel type is Gasoline, then Gasoline is shown as the fuel type. They are the gasoline fuel input to the electric generator. Consumption is metered on Cogeneration:Gasoline, Gasoline:Plant, and Gasoline:Facility.

If the fuel type is FuelOil#1, then FuelOil#1 is shown as the fuel type. They are the fuel oil input to the electric generator. Consumption is metered on Cogeneration:FuelOil#1, FuelOil#1:Plant, and FuelOil#1:Facility.

If the fuel type is FuelOil#2, then FuelOil#2 is shown as the fuel type. They are the fuel oil input to the electric generator. Consumption is metered on Cogeneration:FuelOil#2, FuelOil#2:Plant, and FuelOil#2:Facility.

If the fuel type is OtherFuel1, then OtherFuel1 is shown as the fuel type. They are the fuel oil input to the electric generator. Consumption is metered on Cogeneration:OtherFuel1, OtherFuel1:Plant, and OtherFuel1:Facility.

If the fuel type is OtherFuel2, then OtherFuel2 is shown as the fuel type. They are the fuel oil input to the electric generator. Consumption is metered on Cogeneration:OtherFuel2, OtherFuel2:Plant, and OtherFuel2:Facility.

## Generator:CombustionTurbine

EnergyPlus has two different models for combustion turbine style generators.  This one stems for a model that was originally in the BLAST computer program.  A newer model with a different formulation is also available—see the [Generator:MicroTurbine](#generatormicroturbine) input object.  The basic combustion-turbine cycle is the Brayton Cycle or open cycle, which consists of an adiabatic compression, constant pressure heating, and adiabatic expansion. The Combustion turbine model uses the electrical load and engine generator size to compute part-load ratios (PLR). Fuel energy input and recoverable lube oil heat are then computed. Finally, the recoverable exhaust heat is calculated.

Combustion turbine generators use performance parameters to compute fuel energy consumption as a function of part-load and ambient (entering) air temperature. Recoverable fuel energy equipment, recoverable lube oil heat/fuel energy input equipment performance, total exhaust heat/fuel energy input equipment, exhaust gas temperature/fuel energy input equipment performance parameters are all specified with a quadratic curve fit.

When the heat recovery curve fits = 0.0, this means that no heat recovery is done on the diesel generator exhaust gas; and no heat recovery is specified.

### Inputs

#### Field: Name

Unique name to identify this combustion turbine generator.

#### Field: Rated Power Output

The design nominal capacity of the combustion turbine generator in Watts [W].

#### Field: Electric Circuit Node Name

This Named node contains where the electric power is put in the program simulation for use by the electric equipment in the Load Center.

#### Field: Minimum Part Load Ratio

This field contains the minimum allowed operating fraction of full load. The input value must be >=0.0 and <=1.0.

#### Field: Maximum Part Load Ratio

This field contains the maximum allowed operating fraction of full load. The input value must be >0.0 and <=1.0.

#### Field: Optimum Part Load Ratio

This alpha field contains the optimal operating fraction of full load. This is the part load ratio that gives the optimal production of electric power with the least amount of fuel usage.

#### Field: Part Load Based Fuel Input Curve Name

Needs a [Curve:Quadratic](#curvequadratic) object  to be specified.

Fuel Input = a + b\*PLR + c\*PLR\*\*2

PLR = Ratio of Generator Load to Rated Power Output. This curve is multiplied to the Temperature Based Fuel Input Curve to determine Fuel Energy In.

#### Field: Temperature Based Fuel Input Curve Name

Needs a [Curve:Quadratic](#curvequadratic) object to be specified.

Fuel Input = a + b\*AT + c\*AT\*\*2  where: AT = Ambient Delta T

#### Field: Exhaust Flow Curve Name

The Total Exhaust Energy Curve is a quadratic equation that needs to be specified by a curve object. Used to calculate the total exhaust heat as a function of part-load ratio.

#### Field: Part Load Based Exhaust Temperature Curve Name

The Exhaust Temperature Curve is a quadratic equation that needs to be specified by a curve object. Used to determine the exhaust gas temperature as a function of part-load ratio.

#### Field: Temperature Based Exhaust Temperature Curve Name

The Exhaust Temperature Curve is a quadratic equation that needs to be specified by a curve object. Used to determine the exhaust gas temperature as a function of part-load ratio.

#### Field: Heat Recovery Lube Energy Curve Name

This alpha field contains the name of the Recovery Lube Heat curve. The curve itself is specified separately using a curve object (see Curve:Quadlinear object). The Recovery Lubricant Heat Curve is a quadratic equation that determines the ratio of recovery lube heat to fuel energy. Used to calculate the recoverable lube oil heat as a function of part-load ratio. The defining equation is:

![](media/image430.png)\


where RL is the Ratio of Load to Rated Power Output.

#### U-Factor Times Area (UA) Curve

The UA curve applies to the exhaust gas heat exchanger. The curve is an equation that determines the overall heat transfer coefficient for the exhaust gases with the stack. The heat transfer coefficient ultimately helps determine the exhaust stack temperature. The defining equation is:

![](media/image431.png)\


The following two fields contain the coefficients for the equation.

#### Field: Coefficient 1 of U-Factor Times Area Curve

This numeric field contains the first coefficient for the overall heat transfer coefficient curve.

#### Field: Coefficient 2 of U-Factor Times Area Curve

This numeric field contains the second (exponential) coefficient for the overall heat transfer coefficient curve.

#### Field: Maximum Exhaust Flow per Unit of Power Output

Maximum exhaust flow per unit capacity for diesel engines. The parameter sets an upper limit on exhaust gas flow and exhaust gas heat recovery for diesel engines. Units: kg/sec per kW capacity

#### Field: Design Minimum Exhaust Temperature

This is the design engine stack saturated steam temperature in degrees C.

#### Field: Design Air Inlet Temperature

Design air inlet temperature in degrees C.

#### Field: Fuel Higher Heating Value

This numeric field contains the higher heating value of the fuel used in kJ/kg.

#### Field: Design Heat Recovery Water Flow Rate

Design water volumetric flow rate through heat recovery loop in m^3^/sec.

#### Field: Heat Recovery Inlet Node Name

This alpha field contains the identifying name for the combustion turbine generator heat recovery side inlet node.

#### Field: Heat Recovery Outlet Node Name

This alpha field contains the identifying name for the combustion turbine generator heat recovery side outlet node.

#### Field: Fuel Type

This alpha field determines the type of fuel that the generator uses. Valid choices are: **NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, FuelOil#2, OtherFuel1** and **OtherFuel2**. The default is **NaturalGas**.

#### Field: Heat Recovery Maximum Temperature

This field sets the maximum temperature that this piece of equipment can produce for heat recovery. The idea behind this field is that the current models do not take temperatures into account for availability and they just pass Q's around the loop without a temperature limit. This temperature limit puts an upper bound on the recovered heat and limits the max temperatures leaving the component.

As temperatures in the loop approach the maximum temperature, the temperature difference between the entering water and the surfaces in the piece of equipment becomes smaller. For the given heat recovery flow rate and that temperature difference the amount of heat recovered will be reduced, and eventually there will be no heat recovered when the entering water temperature is equal to the maximum temperature specified by the user in this field. The reduced amount of heat recovered will diminish if the temperature of the loop approach is the maximum temperature, and this will show up in the reporting. This allows the user to set the availability or the quality of the heat recovered for usage in other parts of the system or to heat domestic hot water supply.

The temperature is specified in degrees C.

#### Field: Outdoor Air Inlet Node Name

This field is optional. It is used to explicitly define an outdoor air node for the inlet for combustion air stream. Defining an outdoor air node here allows using the height-dependent model for outdoor air conditions.

An IDF example showing how it is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:CombustionTurbine,
        Solar Turbine,           !- Name
        30000,                   !- Rated Power Output {W}
        GT gen Electric Node,    !- Electric Circuit Node Name
        0.15,                    !- Minimum Part Load Ratio
        1.0,                     !- Maximum Part Load Ratio
        0.65,                    !- Optimum Part Load Ratio
        BG PL Based Fuel Input Curve,  !- Part Load Based Fuel Input Curve Name
        BG Temp Based Fuel Input Curve,  !- Temperature Based Fuel Input Curve Name
        BG Exhaust Flow Curve,   !- Exhaust Flow Curve Name
        BG PL Based Exhaust Temp Curve,  !- Part Load Based Exhaust Temperature Curve Name
        BG Temp Based Exhaust Temp Curve,  !- Temperature Based Exhaust Temperature Curve Name
        BG Tur Recovery Lube Heat Curve,  !- Heat Recovery Lube Energy Curve Name
        0.01907045,              !- Coefficient 1 of U-Factor Times Area Curve
        0.9,                     !- Coefficient 2 of U-Factor Times Area Curve
        0.00000504,              !- Maximum Exhaust Flow per Unit of Power Output {(Kg/s)/W}
        150,                     !- Design Minimum Exhaust Temperature {C}
        25,                      !- Design Air Inlet Temperature {C}
        43500,                   !- Fuel Higher Heating Value {kJ/kg}
        0.0,                     !- Design Heat Recovery Water Flow Rate {m3/s}
        ,                        !- Heat Recovery Inlet Node Name
        ,                        !- Heat Recovery Outlet Node Name
        NaturalGas,              !- Fuel Type
        ,                        !- Heat Recovery Maximum Temperature {C}
        CT OA inlet;             !- Outdoor Air Inlet Node Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Curve:Quadratic,
        BG PL Based Fuel Input Curve,  !- Name
        9.41,                    !- Coefficient1 Constant
        -9.48,                   !- Coefficient2 x
        4.32,                    !- Coefficient3 x**2
        0,                       !- Minimum Value of x
        1;                       !- Maximum Value of x

      Curve:Quadratic,
        BG Temp Based Fuel Input Curve,  !- Name
        1.0044,                  !- Coefficient1 Constant
        -0.0008,                 !- Coefficient2 x
        0,                       !- Coefficient3 x**2
        -30,                     !- Minimum Value of x
        +30;                     !- Maximum Value of x

      Curve:Quadratic,
        BG Exhaust Flow Curve,   !- Name
        0.05,                    !- Coefficient1 Constant
        0.0,                     !- Coefficient2 x
        0.0,                     !- Coefficient3 x**2
        0,                       !- Minimum Value of x
        1;                       !- Maximum Value of x

      Curve:Quadratic,
        BG PL Based Exhaust Temp Curve,  !- Name
        450,                     !- Coefficient1 Constant
        0,                       !- Coefficient2 x
        0,                       !- Coefficient3 x**2
        0,                       !- Minimum Value of x
        1;                       !- Maximum Value of x

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Curve:Quadratic,
        BG Temp Based Exhaust Temp Curve,  !- Name
        1.005,                   !- Coefficient1 Constant
        0.0018,                  !- Coefficient2 x
        0,                       !- Coefficient3 x**2
        -30,                     !- Minimum Value of x
        +30;                     !- Maximum Value of x

      Curve:Quadratic,
        BG Tur Recovery Lube Heat Curve,  !- Name
        0.223,                   !- Coefficient1 Constant
        -0.4,                    !- Coefficient2 x
        0.2286,                  !- Coefficient3 x**2
        0,                       !- Minimum Value of x
        1;                       !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

### Outputs

The output variables that are available for the combustion turbine generator are:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Generator Produced Electric Power [W]
    HVAC,Sum,Generator Produced Electric Energy [J]
    Zone,Meter,ElectricityProduced:Facility [J]
    Zone,Meter,ElectricityProduced:Plant [J]
    Zone,Meter,Cogeneration:ElectricityProduced [J]

    HVAC,Average,Generator Lube Heat Recovery Rate [W]
    HVAC,Sum,Generator Lube Heat Recovery Energy [J]

    HVAC,Average,Generator Exhaust Heat Recovery Rate [W]
    HVAC,Sum,Generator Exhaust Heat Recovery [J]

    HVAC,Average,Generator Total Heat Recovery Rate [W]
    HVAC,Sum,Generator Total Heat Recovery [J]

    HVAC,Average,Generator Exhaust Air Temperature [C]

    HVAC,Average,Generator Heat Recovery Inlet Temperature [C]
    HVAC,Average,Generator Heat Recovery Outlet Temperature [C]
    HVAC,Average,Generator Heat Recovery Mass Flow Rate [kg/s]

    HVAC,Average,Generator <Fuel Type> Rate [W]
    HVAC,Sum,Generator <Fuel Type> Energy [J]
    HVAC,Average,Generator <Fuel Type> Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

All of these outputs, except for the Combustion use outputs, are described above under "[Generator:InternalCombustionEngine](#generatorinternalcombustionengine) Outputs."

#### Generator <Fuel Type> Rate [W]

#### Generator <Fuel Type> Energy [J]

#### Generator <Fuel Type> Mass Flow Rate [kg/s]

These outputs are the fuel input to the electric generator. Consumption is metered on Cogeneration:<Fuel Type>, <Fuel Type>:Plant, and <Fuel Type>:Facility.

## Generator:MicroTurbine

MicroTurbine generators are small combustion turbines that produce electricity on a relatively small scale (e.g., 25kW to 500kW). This model uses nominal performance at reference conditions along with several modifier curves to determine electrical power output and fuel use at non-reference conditions. Standby and ancillary power can also be taken into account. Furthermore, energy recovery from exhaust air can be used to heat water. Similar to electrical power output, thermal power (heat recovery) output is calculated using nominal performance at reference conditions with modifier curves to account for variations at non-reference conditions.

The following inputs define the MicroTurbine electric generator. The [ElectricLoadCenter:Generators](#electricloadcentergenerators)  and [ElectricLoadCenter:Distribution](#electricloadcenterdistribution) objects are used to define the availability and control of all electric generators included in the simulation (ref. [ElectricLoadCenter:Generators](#electricloadcentergenerators) and [ElectricLoadCenter:Distribution](#electricloadcenterdistribution)).

### Inputs

#### Field: Name

This alpha field specifies a unique user-defined name to identify this generator. This is a required input.

#### Field: Reference Electrical Power Output

This numeric field specifies the full-load electrical power output of the microturbine generator in Watts at reference conditions. The reference conditions are defined via additional input fields for this object (see below). This is a required input, and the value entered in this field must be greater than zero.

#### Field: Minimum Full Load Electrical Power Output

This numeric field specifies the minimum electrical power output in Watts at full-load conditions. The electrical power output of the generator is determined by multiplying the Reference Electrical Power Output by the Electrical Power Function of Temperature and Elevation modifier curve. If the result is less than the numeric value specified in this input field, then the generator's electrical power output is reset to the minimum full-load value specified in this input field. The value entered in this field must be greater than or equal to zero. If this field is left blank, the default value of 0.0 will be used.

#### Field: Maximum Full Load Electrical Power Output

This numeric field specifies the maximum electrical power output in Watts at full-load conditions. The electrical power output of the generator is determined by multiplying the Reference Electrical Power Output by the Electrical Power Function of Temperature and Elevation Modifier curve. If the result is greater than the numeric value specified in this input field, then the generator's electrical power output is reset to the maximum full-load value specified in this input field. The value entered in this field must be greater than zero. If this field is left blank, then the value entered for the Reference Electrical Power Output field (above) will be used as the Maximum Full Load Electrical Power Output.

#### Field: Reference Electrical Efficiency Using Lower Heating Value

This numeric field contains the electrical efficiency of the generator at reference conditions, based on the lower heating value of the fuel. The electrical efficiency is the electric power output divided by the fuel energy consumption rate (LHV basis). The reference conditions are defined via additional input fields for this object (see below). This is a required input, and the value entered in this field must be greater than zero and less than or equal to 1.0.

#### Field: Reference Combustion Air Inlet Temperature

This numeric field specifies the reference temperature for the combustion inlet air in degrees Celsius. If this field is left blank, the default value of 15°C will be used.

#### Field: Reference Combustion Air Inlet Humidity Ratio

This numeric field specifies the reference humidity ratio for the combustion inlet air in kgWater/kgDryAir. The value specified for this field must be greater than zero. If this field is left blank, the default value of 0.00638 (kgWater/kgDryAir) will be used.

#### Field: Reference Elevation

This numeric field specifies the reference elevation in meters (relative to sea level). The value specified for this field must be greater than or equal to -300.0 meters. If this field is left blank, the default value of 0.0 meters will be used.

#### Field: Electrical Power Function of Temperature and Elevation Curve Name

This alpha field specifies the name of a bi-quadratic performance curve (ref: Performance Curves) that parameterizes the variation of electrical power output as a function of the combustion air inlet temperature and elevation. The output of this curve is multiplied by the Reference Electrical Power Output to give the full-load power output at a specific combustion air inlet temperature and elevation (i.e., at values different from the reference conditions). This curve should be normalized to have a value of 1.0 at the reference conditions, and the curve should be valid for the range of inlet air temperatures anticipated for the simulation period and for the actual elevation of the generator.

#### Field: Electrical Efficiency Function of Temperature Curve Name

This alpha field specifies the name of a quadratic or cubic performance curve (ref: Performance Curves) that parameterizes the variation of electrical efficiency as a function of the combustion air inlet temperature. The output of this curve is multiplied by the Reference Electrical Efficiency Using Lower Heating Value to give the full-load electrical efficiency at specific combustion air inlet temperatures (i.e., at inlet air temperatures different from the Reference Combustion Air Inlet Temperature). This curve should be normalized to have a value of 1.0 at the Reference Combustion Air Inlet Temperature, and the curve should be valid for the range of inlet air temperatures anticipated for the simulation period.

#### Field: Electrical Efficiency Function of Part Load Ratio Curve Name

This alpha field specifies the name of a quadratic or cubic performance curve (ref: Performance Curves) that parameterizes the variation of electrical efficiency as a function of the generator's part-load ratio (part-load ratio is the actual electrical power output divided by the full-load electrical power output at the current operating conditions). The output of this curve is multiplied by the Reference Electrical Efficiency Using Lower Heating Value and the output of the Electrical Efficiency Function of Temperature modifier curve to give the electrical efficiency at specific part-load and combustion air inlet (temperature) conditions. This curve should be normalized to have a value of 1.0 when the generator's part-load ratio is 1.0, and the curve should be valid for the range of part-load ratios anticipated for the simulation period.

#### Field: Fuel Type

This choice field specifies the type of fuel used by the generator. Valid fueld types are PropaneGas and NaturalGas. If the field is left blank, the fuel type will be assumed to be NaturalGas.

#### Field: Fuel Higher Heating Value

This numeric field specifies the higher heating value of the fuel used in kJ/kg. The value specified for this field must be greater than zero and greater than the specified Fuel Lower Heating Value. If this field is left blank, the default value of 50,000 kJ/kg will be used.

#### Field: Fuel Lower Heating Value

This numeric field specifies the lower heating value of the fuel used in kJ/kg. The value specified for this field must be greater than zero but less than the specified Fuel Higher Heating Value. If this field is left blank, the default value of 45,450 kJ/kg will be used.

#### Field: Standby Power

This numeric field specifies the standby electric power consumed by the generator in Watts. The standby power is the electrical power consumed by the generator (e.g., air fans and controls) when the generator is available to operate but the generator electrical power output is zero (power output is not being requested by the electric load center). The value specified for this field must be greater than or equal to zero. If this field is left blank, the default value of 0.0 W will be used.

#### Field: Ancillary Power

This numeric field specifies the ancillary electric power consumed by the generator in Watts. The ancillary power is the electrical power consumed by other associated equipment (e.g., external fuel pressurization pumps) when the generator is operating. Specify this input as 0.0 if the Reference Electrical Power Output and Reference Electrical Efficiency Using Lower Heating Value input fields and associated modifier curves reflect the "net" electrical power output from the generator (i.e., ancillary power already deducted from the generator's gross electrical power output). A value greater than zero indicates that this electrical power is consumed whenever the generator is operating and will be deducted from the generator's overall electrical power output (Generator Produced Electric Power). The value specified for this field must be greater than or equal to zero. If this field is left blank, the default value of 0.0 W will be used.

#### Field: Ancillary Power Function of Fuel Input Curve Name

This alpha field specifies the name of a quadratic performance curve (ref: Performance Curves) that parameterizes the variation of ancillary power as a function of the generator's input fuel mass flow rate (kg/s). The output of this curve is multiplied by the ancillary power to give the ancillary power at a specific fuel mass flow rate. If this field is left blank, the model assumes that the modifier curve is 1.0 for the entire simulation (i.e., the ancillary power is constant whenever the generator operates).

#### Field: Heat Recovery Water Inlet Node Name

This alpha field specifies the identifying name for the generator's heat recovery water inlet node.

#### Field: Heat Recovery Water Outlet Node Name

This alpha field specifies the identifying name for the generator's heat recovery water outlet node.

#### Field: Reference Thermal Efficiency Using Lower Heat Value

This numeric field specifies the thermal efficiency (heat recovery to water) at reference conditions, based on the lower heating value of the fuel. The thermal efficiency is the thermal power output (to water) divided by the fuel energy consumption rate (LHV basis). The reference conditions are defined via additional input fields for this object. This value must be from 0.0 to 1.0.  If this field is left blank, the default value of 0.0 will be used.

#### Field: Reference Inlet Water Temperature

This numeric field specifies the reference temperature for the inlet water to the heat recovery heat exchanger in degrees Celsius.

#### Field: Heat Recovery Water Flow Operating Mode

This field is used to choose between different modes of controlling the mass flow rate of water being heated by energy recovered from exhaust air. There are two options available for this field: "PlantControl" or "InternalControl."  The "PlantControl" option indicates that the heat recovery water flow rate through the generator is determined externally (by the wider balance of plant). In this case, the generator will request the Reference Heat Recovery Water Flow Rate whenever it operates but the actual flow rate may be limited by other plant components (e.g., pump). The "InternalControl" option indicates the flow of water is controlled inside the generator based on current operating conditions. For InternalControl, the generator should (probably) include a bypass branch when connecting to the plant loop.

#### Field: Reference Heat Recovery Water Flow Rate

This numeric field is the reference heat recovery (volumetric) water flow rate in cubic meters per second. Entered values must be greater than zero.

#### Field: Heat Recovery Water Flow Rate Function of Temperature and Power Curve Name

This alpha field specifies the name of a bi-quadratic performance curve (ref: Performance Curves) that parameterizes the variation of heat recovery water flow rate as a function of the inlet water temperature and net electrical power output. This field is only used if the Heat Recovery Water Flow Operating Mode is ‘InternalControl'. The output of this curve is multiplied by the Reference Heat Recovery Water Flow Rate to give the water flow rate at the specific inlet water temperature and net power operating conditions. This curve should be normalized to have a value of 1.0 at the reference conditions, and the curve should be valid for the range of inlet water temperatures and net electrical power output anticipated for the simulation period. If this field is left blank, the model assumes that the modifier curve is 1.0 for the entire simulation.

#### Field: Thermal Efficiency Function of Temperature and Elevation Curve Name

This alpha field specifies the name of a bi-quadratic performance curve (ref: Performance Curves) that parameterizes the variation of thermal efficiency as a function of the combustion air inlet temperature and elevation. The output of this curve is multiplied by the Reference Thermal Efficiency Using Lower Heating Value to give the full-load thermal efficiency at a specific combustion air inlet temperature and elevation (i.e., at values different from the reference conditions). This curve should be normalized to have a value of 1.0 at the reference conditions, and the curve should be valid for the range of inlet air temperatures anticipated for the simulation period and for the actual elevation of the generator. If this field is left blank, the model assumes that the modifier curve is 1.0 for the entire simulation.

#### Field: Heat Recovery Rate Function of Part Load Ratio Curve Name

This alpha field specifies the name of a quadratic or cubic performance curve (ref: Performance Curves) that parameterizes the variation of heat recovery to water (thermal power output) as a function of the generator's part-load ratio (part-load ratio is the actual electrical power output divided by the full-load electrical power output at the current operating conditions). The output of this curve is multiplied by the steady-state heat recovery at the current combustion inlet air temperature and elevation to give the heat recovery rate (thermal power output) at specific part-load operating conditions. This curve should be normalized to have a value of 1.0 when the generator's part-load ratio is 1.0, and the curve should be valid for the range of part-load ratios anticipated for the simulation period. If this field is left blank, the model assumes that the modifier curve is 1.0 for the entire simulation.

#### Field: Heat Recovery Rate Function of Inlet Water Temperature Curve Name

This alpha field specifies the name of a quadratic performance curve (ref: Performance Curves) that parameterizes the variation of heat recovery to water (thermal power output) as a function of the inlet water temperature. The output of this curve is multiplied by the steady-state heat recovery at the current combustion inlet air temperature and elevation to give the heat recovery rate (thermal power output) at non-reference inlet water conditions. This curve should be normalized to have a value of 1.0 at the Reference Inlet Water Temperature, and the curve should be valid for the range of inlet water temperatures anticipated for the simulation period. If this field is left blank, the model assumes that the modifier curve is 1.0 for the entire simulation.

#### Field: Heat Recovery Rate Function of Water Flow Rate Curve Name

This alpha field specifies the name of a quadratic performance curve (ref: Performance Curves) that parameterizes the variation of heat recovery to water (thermal power output) as a function of the heat recovery water flow rate. The output of this curve is multiplied by the steady-state heat recovery at the current combustion inlet air temperature and elevation to give the heat recovery rate (thermal power output) at non-reference heat recovery water flow rates. This curve should be normalized to have a value of 1.0 at the Reference Heat Recovery Water Flow Rate, and the curve should be valid for the range of water flow rates anticipated for the simulation period. If this field is left blank, the model assumes that the modifier curve is 1.0 for the entire simulation.

#### Field: Minimum Heat Recovery Water Flow Rate

This numeric field specifies the minimum (volumetric) water flow rate through the heat recovery heat exchanger in cubic meters per second. The minimum input value is 0.0, and a value of 0.0 is assumed if this field is left blank.

#### Field: Maximum Heat Recovery Water Flow Rate

This numeric field specifies the maximum (volumetric) water flow rate through the heat recovery heat exchanger in cubic meters per second. The minimum input value for this field is 0.0, and a value of 0.0 is assumed if this field is left blank. The maximum heat recovery water flow rate must be greater than or equal to the minimum heat recovery water flow rate.

#### Field: Maximum Heat Recovery Water Temperature

This field sets the maximum water temperature, in degrees Celsius, that this generator can produce via heat recovery. This temperature limit puts an upper bound on the recovered heat and limits the max temperatures leaving the component.

As temperatures in the water loop approach this maximum temperature, the temperature difference between the entering water and the surfaces in generator's heat recovery heat exchanger becomes smaller. For the given heat recovery flow rate and that temperature difference the amount of heat recovered will be reduced, and eventually there will be no heat recovered when the entering water temperature is equal to the maximum temperature specified by the user in this field. The amount of heat recovered will diminish if the inlet water temperature approaches the maximum temperature, and this will show up in the reporting.

#### Field: Combustion Air Inlet Node Name

This alpha field specifies the name of the combustion air inlet node. If a node name is specified, this node must be an outdoor air node and must also be specified elsewhere in the input (ref: [OutdoorAir:Node](#outdoorairnode) and [OutdoorAir:NodeList](#outdoorairnodelist)). If this field is left blank, the combustion air inlet conditions are assumed to be the outdoor weather conditions used for the simulation.

#### Field: Combustion Air Outlet Node Name

This alpha field specifies the name of the combustion air outlet node.

#### Field: Reference Exhaust Air Mass Flow Rate

This numeric field is the reference exhaust air mass flow rate in kilograms per second. Entered values must be greater than zero.

#### Field: Exhaust Air Flow Rate Function of Temperature Curve Name

This alpha field specifies the name of a quadratic or cubic performance curve (ref: Performance Curves) that parameterizes the variation of exhaust air flow rate as a function of the combustion air inlet temperature. The output of this curve is multiplied by the Reference Exhaust Air Mass Flow Rate to give the exhaust air mass flow rate at non-reference combustion air inlet temperatures. This curve should be normalized to have a value of 1.0 at the Reference Combustion Air Inlet Temperature, and the curve should be valid for the range of inlet air temperatures anticipated for the simulation period. If this field is left blank, the model assumes that the modifier curve is 1.0 for the entire simulation.

#### Field: Exhaust Air Flow Rate Function of Part Load Ratio Curve Name

This alpha field specifies the name of a quadratic or cubic performance curve (ref: Performance Curves) that parameterizes the variation of exhaust air flow rate as a function of the generator's part-load ratio (part-load ratio is the actual electrical power output divided by the full-load electrical power output at the current operating conditions). The output of this curve is multiplied by the Reference Exhaust Air Mass Flow Rate to give the exhaust air mass flow rate at specific part-load operating conditions. This curve should be normalized to have a value of 1.0 when the generator's part-load ratio is 1.0, and the curve should be valid for the range of part-load ratios anticipated for the simulation period. If this field is left blank, the model assumes that the modifier curve is 1.0 for the entire simulation.

#### Field: Nominal Exhaust Air Outlet Temperature

This numeric field is the exhaust air outlet temperature at nominal (reference) conditions in degrees Celsius.

#### Field: Exhaust Air Temperature Function of Temperature Curve Name

This alpha field specifies the name of a quadratic or cubic performance curve (ref: Performance Curves) that parameterizes the variation of exhaust air outlet temperature as a function of the combustion air inlet temperature. The output of this curve is multiplied by the Nominal Exhaust Air Outlet Temperature to give the exhaust air temperature at non-reference combustion air inlet temperatures. This curve should be normalized to have a value of 1.0 at the Reference Combustion Air Inlet Temperature, and the curve should be valid for the range of inlet air temperatures anticipated for the simulation period. If this field is left blank, the model assumes that the modifier curve is 1.0 for the entire simulation.

#### Field: Exhaust Air Temperature Function of Part Load Ratio Curve Name

This alpha field specifies the name of a quadratic or cubic performance curve (ref: Performance Curves) that parameterizes the variation of exhaust air outlet temperature as a function of the generator's part-load ratio (part-load ratio is the actual electrical power output divided by the full-load electrical power output at the current operating conditions). The output of this curve is multiplied by the Nominal Exhaust Air Outlet Temperature to give the exhaust air temperature at specific part-load operating conditions. This curve should be normalized to have a value of 1.0 when the generator's part-load ratio is 1.0, and the curve should be valid for the range of part-load ratios anticipated for the simulation period. If this field is left blank, the model assumes that the modifier curve is 1.0 for the entire simulation.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

    Generator:MicroTurbine,
        Generator 3,             !- Name
        65000,                   !- Reference Electrical Power Output {W}
        29900,                   !- Minimum Full Load Electrical Power Output {W}
        65000,                   !- Maximum Full Load Electrical Power Output {W}
        0.29,                    !- Reference Electrical Efficiency Using Lower Heating Value
        15.0,                    !- Reference Combustion Air Inlet Temperature {C}
        0.00638,                 !- Reference Combustion Air Inlet Humidity Ratio (kgWater/kgDryAir)
        0.0,                     !- Reference Elevation {m}
        Power_vs_Temp_Elev,      !- Electrical Power Function of Temperature and Elevation Curve Name
        Efficiency_vs_Temp,      !- Electrical Efficiency Function of Temperature Curve Name
        Efficiency_vs_PLR,       !- Electrical Efficiency Function of Part Load Ratio Curve Name
        NaturalGas,              !- Fuel Type
        50000,                   !- Fuel Higher Heating Value {kJ/kg}
        45450,                   !- Fuel Lower Heating Value {kJ/kg}
        300,                     !- Standby Power {W}
        4500;                    !- Ancillary Power {W}

    ! Electrical Power Modifier Curve (function of temperature and elevation)
    ! x = Dry-Bulb Temperature of Combustion Inlet Air (C) and y = Elevation (meters)
      Curve:Biquadratic,
        Power_vs_Temp_Elev,      !- Name
        1.2027697,               !- Coefficient1 Constant
        -9.671305E-03,           !- Coefficient2 x
        -4.860793E-06,           !- Coefficient3 x**2
        -1.542394E-04,           !- Coefficient4 y
        9.111418E-09,            !- Coefficient5 y**2
        8.797885E-07,            !- Coefficient6 x*y
        -17.8,                   !- Minimum Value of x
        50.0,                    !- Maximum Value of x
        0.0,                     !- Minimum Value of y
        3050.;                   !- Maximum Value of y

    ! Electrical Efficiency Modifier Curve (function of temperature)
    ! x = Dry-Bulb Temperature of Combustion Inlet Air (C)
      Curve:Cubic,
        Efficiency_vs_Temp,      !- Name
        1.0402217,               !- Coefficient1 Constant
        -0.0017314,              !- Coefficient2 x
        -6.497040E-05,           !- Coefficient3 x**2
        5.133175E-07,            !- Coefficient4 x**3
        -20.0,                   !- Minimum Value of x
        50.0;                    !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    ! Electrical Efficiency Modifier Curve (function of part-load ratio)
    ! x = Part-Load Ratio (electrical load/steady-state electrical power output)
      Curve:Cubic,
        Efficiency_vs_PLR,       !- Name
        0.215290,                !- Coefficient1 Constant
        2.561463,                !- Coefficient2 x
        -3.24613,                !- Coefficient3 x**2
        1.497306,                !- Coefficient4 x**3
        0.03,                    !- Minimum Value of x
        1.0;                     !- Maximum Value of x!

      ElectricLoadCenter:Distribution,
        Electric Load Center,    !- Name
        Backup Generators,       !- Generator List Name
        DemandLimit,             !- Generator Operation Scheme Type
        10000.0,                 !- Demand Limit Scheme Purchased Electric Demand Limit {W}
        ,                        !- Track Schedule Name Scheme Schedule Name
        ,                        !- Track Meter Scheme Meter Name
        AlternatingCurrent,      !- Electrical Buss Type
        ,                        !- Inverter Object Name
        ;                        !- Electrical Storage Object Name

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      ElectricLoadCenter:Generators,
        Backup Generators,       !- Name
        Generator 1,             !- Generator 1 Name
        Generator:InternalCombustionEngine,  !- Generator 1 Object Type
        50000,                   !- Generator 1 Rated Electric Power Output
        ON PEAK GENERATOR SCHEDULE,  !- Generator 1 Availability Schedule Name
        ,                        !- Generator 1 Rated Thermal to Electrical Power Ratio
        Generator 2,             !- Generator 2 Name
        Generator:CombustionTurbine,  !- Generator 2 Object Type
        30000,                   !- Generator 2 Rated Electric Power Output
        OFF PEAK GENERATOR SCHEDULE,  !- Generator 2 Availability Schedule Name
        ,                        !- Generator 2 Rated Thermal to Electrical Power Ratio
        Generator 3,             !- Generator 3 Name
        Generator:MicroTurbine,  !- Generator 3 Object Type
        65000,                   !- Generator 3 Rated Electric Power Output
        MID PEAK GENERATOR SCHEDULE,  !- Generator 3 Availability Schedule Name
        ;                        !- Generator 3 Rated Thermal to Electrical Power Ratio

~~~~~~~~~~~~~~~~~~~~

### Outputs

The output variables that are available for the microturbine generator are:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Generator Produced Electric Power [W]
    HVAC,Sum,Generator Produced Electric Energy [J]
    HVAC,Average,Generator LHV Basis Electric Efficiency [-]
    HVAC,Average,Generator <Fuel Type> HHV Basis Rate [W]
    HVAC,Average,Generator Fuel HHV Basis Rate [W]
    HVAC,Sum,Generator <Fuel Type> HHV Basis Energy [J]
    HVAC,Sum,Generator Fuel HHV Basis Energy [J]
    HVAC,Average,Generator <Fuel Type> Mass Flow Rate [kg/s]

    If Standby Power input field > 0.0:
    HVAC,Average,Generator Standby Electric Power [W]
    HVAC,Sum,Generator Standby Electric Energy [J]

    If Ancillary Power input field > 0.0:
    HVAC,Average,Generator Ancillary Electric Power [W]
    HVAC,Sum,Generator Ancillary Electric Energy [J]

    If heat recovery water inlet and outlet node names are entered (and valid):
    HVAC,Average,Generator Produced Thermal Rate [W]
    HVAC,Sum,Generator Produced Thermal Energy [J]
    HVAC,Average,Generator Thermal Efficiency LHV Basis [-]
    HVAC,Average,Generator Heat Recovery Inlet Temperature [C]
    HVAC,Average,Generator Heat Recovery Outlet Temperature [C]
    HVAC,Average,Generator Heat Recovery Water Mass Flow Rate [kg/s]

    If exhaust air calculations are performed:
    HVAC,Average,Generator Exhaust Air Mass Flow Rate [kg/s]
    HVAC,Average,Generator Exhaust Air Temperature  [C]

~~~~~~~~~~~~~~~~~~~~

#### Generator Produced Electric Power [W]

This output variable is the average electric power produced by the generator in Watts for the timestep being reported. This is the "net" electric power produced, accounting for ancillary electric power consumed during generator operation.

#### Generator Produced Electric Energy [J]

This output variable is the electric energy produced by the generator in Joules for the timestep being reported. This output is also added to a meter with Resource Type = ElectricityProduced, End Use Key = Cogeneration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). This is the "net" electric energy produced, accounting for ancillary electric consumption during generator operation.

#### Generator LHV Basis Electric Efficiency []

This output variable is the average electric efficiency of the generator (lower heating value basis) for the timestep being reported. The electric efficiency is the Generator Produced Electric Power in Watts divided by the generator's fuel energy consumption rate in Watts (lower heating value basis).

#### Generator <Fuel Type> HHV Basis Rate [W]

This output variable is the average fuel-specific energy consumption rate of the electric generator in Watts (higher heating value basis) for the timestep being reported. <Fuel Type> is the name of the fuel used by this electric generator. <Fuel Type> can be one of the following: NaturalGas (=> ‘Gas') or PropaneGas (=>'Propane').

#### Generator Fuel HHV Basis Rate [W]

This output variable is the average fuel energy consumption rate of the electric generator in Watts (higher heating value basis) for the timestep being reported. The output variable name is non-fuel specific.

#### Generator <Fuel Type> HHV Basis Energy [J]

This output variable is the fuel-specific energy consumption of the electric generator in Joules (higher heating value basis) for the timestep being reported. This output is also added to a meter with Resource Type = <Fuel Type>, End Use Key = Cogeneration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). <Fuel Type> is the name of the fuel used by this electric generator. <Fuel Type> can be one of the following: NaturalGas (=> ‘Gas') or PropaneGas (=>'Propane').

#### Generator Fuel HHV Basis Energy [J]

This output variable is the fuel energy consumption of the electric generator in Joules (higher heating value basis) for the timestep being reported. The output variable name is non-fuel specific.

#### Generator <Fuel Type> Mass Flow Rate [kg/s]

This output variable is the average mass flow rate of fuel being consumed by the electric generator in kg/s for the timestep being reported. <Fuel Type> is the name of the fuel used by this electric generator. <Fuel Type> can be one of the following: Natural Gas (=> ‘Gas') or Propane Gas (=>'Propane').

#### Generator Standby Electric Power [W]

This output variable is the average standby electric power consumed by the generator in Watts for the timestep being reported. Standby power is electrical power consumed by the generator (e.g., air fans and controls) when the generator is available to operate but the generator electrical power output is zero (power output is not being requested by the electric load center). This output variable is only produced when the user enters a value greater than 0.0 for the input field Standby Power.

#### Generator Standby Electric Energy [J]

This output variable is the standby electric energy consumption for the generator in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Cogeneration, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). This output variable is only produced when the user enters a value greater than 0.0 for the input field Standby Power.

#### Generator Ancillary Electric Power [W]

This output variable is the average ancillary electric power consumed by the generator in Watts for the timestep being reported. Ancillary power is the electrical power consumed by other associated equipment (e.g., external fuel pressurization pumps) when the generator is operating. This output variable is only produced when the user enters a value greater than 0.0 for the input field Ancillary Power.

#### Generator Ancillary Electric Energy [J]

This output variable is the ancillary electric energy consumption for the generator in Joules for the timestep being reported. This energy consumption is already deducted from the output variable Generator Produced Electric Energy ("net" electric energy produced by the generator). This output variable is only produced when the user enters a value greater than 0.0 for the input field Ancillary Power.

#### Generator Produced Thermal Rate [W]

This output variable is the average thermal power produced (i.e., exhaust energy recovery to heat water) in Watts for the timestep being reported.

#### Generator Produced Thermal Energy [J]

This output variable is the thermal energy produced (i.e., exhaust energy recovery to heat water) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = HeatRecovery, Group Key = Plant (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Generator Thermal Efficiency LHV Basis [-]

This output variable is the average thermal efficiency of the generator (lower heating value basis) for the timestep being reported. The thermal efficiency is the Generator Produced Thermal Rate in Watts divided by the generator's fuel energy consumption rate in Watts (lower heating value basis).

#### Generator Heat Recovery Inlet Temperature [C]

This output variable is the average heat recovery inlet water temperature in degrees Celsius for the timestep being reported.

#### Generator Heat Recovery Outlet Temperature [C]

This output variable is the average heat recovery outlet water temperature in degrees Celsius for the timestep being reported.

#### Generator Heat Recovery Water Mass Flow Rate [kg/s]

This output variable is the average heat recovery water mass flow rate in kilograms per second for the timestep being reported.

#### Generator Exhaust Air Mass Flow Rate [kg/s]

This is the mass flow rate of exhaust leaving the generator.  This output is available when the model input is setup for exhaust conditions.

#### Generator Exhaust Air Temperature  [C]

This is the temperature of exhaust leaving the generator.  This output is available when the model input is setup for exhaust conditions.

## Generator:MicroCHP

This object is used to model small-scale combined heat and power (micro CHP) electric generators using the model developed by IEA/ECBCS Annex 42 – see www.cogen-sim.net.   The model was developed for both internal combustion and Stirling cycle engines, but might be used for other types of residential CHP devices.

Note that unlike other component models in EnergyPlus, this model is not normalized. Therefore, performance coefficients developed for one type and capacity of CHP device cannot be used for a device with a different capacity.

This model is an empirical, curve fit model. This object describes the connections to the rest of the building model and includes a reference to another object called [Generator:MicroCHP:NonNormalizedParameters](#generatormicrochpnonnormalizedparameters) that provides the operating characteristics.

### Inputs

#### Field: Name

This field contains a unique name for the residential CHP device.

#### Field: Performance Parameters Name

This field contains the name of a [Generator:MicroCHP:NonNormalizedParameters](#generatormicrochpnonnormalizedparameters) object defined elsewhere in the input.

#### Field: Zone Name

This field contains the name of the thermal zone where the CHP device is located in the model. If the device is outdoors, or you do not want skin losses to be added to a zone, then leave this field blank.

#### Field: Cooling Water Inlet Node Name

This field provides the name of a node that is the inlet to the CHP device.  The node should be on the plant loop that will receive the heat from the residential CHP unit. This inlet serves as the cooling water for the CHP device.

#### Field: Cooling Water Outlet Node Name

This field provides the name of a node that is the outlet to the CHP device.  The node should be on the plant loop that will receive the heat from the residential CHP unit. This outlet serves as the exit of the cooling water leaving the CHP unit.

#### Field: Air Inlet Node Name

The name of an air node that supplies the CHP unit with air for use inside the generator.

#### Field: Air Outlet Node Name

The name of an air node that receives the exhaust from the CHP unit.

#### Field: Generator Fuel Supply Name

This field contains the name of a [Generator:FuelSupply](#generatorfuelsupply) object defined elsewhere in the input file.

#### Field: Availability Schedule Name

This field contains the name of a schedule used to control whether or not the unit is available. It is "available" any time the value is greater than zero. If it is available, it may consume standby power at times when there is no request for power from the Electric Load Center. If it is not available (schedule value = 0.0) then the CHP unit is in a completely off "mode" and will not consume standby power. If this field is blank, the schedule has values of 1 for all time periods.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:MicroCHP,
        MicroCoGen1,             !- Name
        SENERTECH5_5KW,          !- Performance Parameters Name
        ZN_1_FLR_1_SEC_1,        !- Zone Name
        MICROCHP SENERTECH Pump-MicroCoGen1Node,  !- Cooling Water Inlet Node Name
        MICROCHP SENERTECH Supply Equipment Outlet Node,  !- Cooling Water Outlet Node Name
        MicroCoGen1 air inlet node,  !- Air Inlet Node Name
        ,                        !- Air Outlet Node Name
        NATURALGAS,              !- Generator Fuel Supply Name
        ALWAYS_ON;               !- Availability Schedule Name
~~~~~~~~~~~~~~~~~~~~

## Generator:MicroCHP:NonNormalizedParameters

This object is referenced by a [Generator:MicroCHP](#generatormicrochp) object and provides the non-normalized parameters for the MicroCHP generator model.

### Inputs

#### Field: Name

Unique name to identify this set of parameter data.

#### Field: Maximum Electric Power

This is the size of the CHP unit in terms of the maximum electrical power it can produce [W]. If the electric load center requests more power than this maximum, then the unit will restrict its output to this level.

#### Field: Minimum Electric Power

This is the size of the CHP unit in terms of the minimum electrical power it can produce [W]. If the electric load center requests less power than this minimum, then the unit will hold its output to this level.

#### Field: Minimum Cooling Water Flow Rate

This is the minimum flow rate of cooling water that must be available for the unit to operate [kg/s]. If the plant loop is not providing this flow of water, the unit will shut down to protect it from overheating.

#### Field: Maximum Cooling Water Temperature

This is the maximum temperature of cooling water inlet or outlet that can occur without the unit shutting down to protect from overheating [°C].

#### Field: Electrical Efficiency Curve Name

This is the name of [Curve:Triquadratic](#curvetriquadratic) object that defines the steady-state net electrical efficiency.  The electrical efficiency, ![](media/image432.png) , is a function of  the cooling water mass flow rate, ![](media/image433.png) , the temperature of the cooling water at the inlet, ![](media/image434.png) , the steady-state net electrical power produced, ![](media/image435.png) .

![](media/image436.png)\


The associated [Curve:Triquadratic](#curvetriquadratic) object should be defined with the independent variables ![](media/image437.png)  corresponding to *x*, *y*, and *z*, respectively.

#### Field: Thermal Efficiency Curve Name

This is the name of a [Curve:Triquadratic](#curvetriquadratic) object that defines the steady-state net thermal efficiency.  The thermal efficiency, ![](media/image438.png) , is a function of  the cooling water mass flow rate, ![](media/image439.png) , the temperature of the cooling water at the inlet, ![](media/image440.png) , the steady-state net electrical power produced, ![](media/image441.png) .

![](media/image442.png)\


The associated [Curve:Triquadratic](#curvetriquadratic) object should be defined with the independent variables ![](media/image443.png)  corresponding to *x*, *y*, and *z*, respectively.

#### Field: Cooling Water Flow Rate Mode

This field is used to choose between different modes of controlling the mass flow rate of cooling water. There are two options available for this field: "PlantControl" or "InternalControl."  The "PlantControl" option indicates that the cooling water flow rate through the CHP device is determined externally (by the wider balance of plant). The "InternalControl" option indicates the flow of cooling water is controlled inside the CHP device (e.g., like an automobile's thermostat).

For internal control, the following field is used to define a Biquadratic curve that will determine the cooling water flow rate. For internal control, the CHP device should (probably) include a bypass branch when connecting to the plant loop.

#### Field: Cooling Water Flow Rate Curve Name

This field contains the name of a [Curve:Biquadratic](#curvebiquadratic) object that defines the mass flow rate of cooling water, ![](media/image444.png) .  This field is only used if the prior field is set to "InternalControl."  The mass flow of cooling water is a function of steady-state power, ![](media/image445.png) , and the inlet temperature of the cooling water, ![](media/image446.png) .  The associated [Curve:Biquadratic](#curvebiquadratic) should be defined with the independent variables ![](media/image447.png)  and ![](media/image448.png)  corresponding to *x* and *y*, respectively.

#### Field: Air Flow Rate Curve Name

This is the name of a [Curve:Quadratic](#curvequadratic) object that defines the steady state air flow into the CHP device as a function of the mass flow rate of fuel.

#### Field: Maximum Net Electrical Power Rate of Change

This field contains input on the limits for how fast the generator can ramp up or down in terms of the net electrical power.

#### Field: Maximum Fuel Flow Rate of Change

This field contains input on the limits for how fast the generator can ramp up or down in terms of the fuel flow rates.

#### Field: Heat Exchanger U-Factor Times Area Value

This field describes the effective UA value for the heat exchanger that transfers heat from the generator to the cooling water.

#### Field: Skin Loss U-Factor Times Area Value

This field describes the effective UA value for the heat transfer from the generator to the surrounding air.

#### Field: Skin Loss Radiative Fraction

This field describes the split between thermal radiation and thermal convection for the heat losses from the skin of the generator.

#### Field: Aggregated Thermal Mass of Energy Conversion Portion of Generator

This field describes the aggregated thermal mass of the energy conversion portion of the generator. This includes the engine block in an internal combustion engine.

#### Field: Aggregated Thermal Mass of Heat Recovery Portion of Generator

This field describes the aggregated thermal mass of the heat recovery portion of the generator. This includes the encapsulated cooling water and heat exchanger shell in immediate thermal contact.

#### Field: Standby Power

This field describes the power used by the CHP unit's control systems while in standby operation. Standby operation mode occurs whenever the unit is available but not being called upon to produce power.  Power is entered as a positive value but indicates negative generation.

#### Field: Warm Up Mode

This field is used to choose between different modes of controlling the warm up characteristics. Residential CHP units such as Stirling engines have different warm up characteristics than internal combustion engines.  There are two warm up modes available:  "NominalEngineTemperature" or "TimeDelay."  If the former mode is input in this field, then the next three fields are used to model warm up. If the later mode is input, then the fourth field below is used to model warm up.

#### Field: Warm Up Fuel Flow Rate Coefficient

This field describes the value for the coefficient used to determine the rate of fuel flow during warm up. The coefficient, *k~f~*, is used in the following relation,

![](media/image449.png)\


#### Field: Nominal Engine Operating Temperature

This field describes the nominal engine temperature during normal, steady-state operation, ![](media/image450.png)  [°C].

#### Field: Warm Up Power Coefficient

This field describes the value for the coefficient used to determine the rate of power production during warm up. The coefficient k~p~ is used in the following relation,

![](media/image451.png)\


#### Field: Warm Up Fuel Flow Rate Limit Ratio

This field is used to describe a limit to the fuel flow rate during warm up. The limit is a ratio multiplied by the maximum fuel flow rate during normal operation (corresponding to the fuel flow for maximum electrical power). If the warm up operating fuel flow rate is twice the steady state fuel flow at maximum power, then a value of 2.0 would be entered in this field. This field is only used with the nominal engine temperature warm up mode and is intended for Stirling engines.

#### Field: Warm Up Delay Time

This field is used to model the warm up operation when the warm up mode is "TimeDelay."

Enter the time between activation and power generation in seconds.

#### Field: Cool Down Power

This field describes the power used by the CHP unit's ancillary systems while in cool down operation mode. Cool down operation mode occurs whenever the unit has recently been told to stop producing power.  Power here is consumed and entered as a positive value that indicates negative generation. For example, a component such as a cooling fan might continue to run during cool down.

#### Field: Cool Down Delay Time

This field is used to describe the time it takes for the CHP unit to cool off and complete the cool down operation. Enter the time between deactivation and the end of a cool down period in seconds.

#### Field: Restart Mode

This field is used to choose between two different control situations for the cool down operation. The two options for this field are "MandatoryCoolDown" and "OptionalCoolDown."   The former is used to direct the model to complete the entire cool down period before the generator can subsequently enter warm up mode. The latter directs the model to allow immediately switching to warm up mode, if power is requested before the cool down period is completed.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:MicroCHP:NonNormalizedParameters,
        SENERTECH5_5KW,          !- Name
        5500.0000,               !- Maximum Electric Power {W}
        0.0000,                  !- Minimum Electric Power {W}
        0.0000,                  !- Minimum Cooling Water Flow Rate {kg/s}
        80.0000,                 !- Maximum Cooling Water Temperature {C}
        SenerTechElEff,          !- Electrical Efficiency Curve Name
        SenerTechThermEff,       !- Thermal Efficiency Curve Name
        InternalControl,         !- Cooling Water Flow Rate Mode
        SenerTechCoolWaterflow,  !- Cooling Water Flow Rate Curve Name
        SenerTechAirFlow,        !- Air Flow Rate Curve Name
        1000000000.0000,         !- Maximum Net Electrical Power Rate of Change {W/s}
        1000000000.0000,         !- Maximum Fuel Flow Rate of Change {kg/s2}
        741.0000,                !- Heat Exchanger U-Factor Times Area Value {W/K}
        13.7000,                 !- Skin Loss U-Factor Times Area Value {W/K}
        0.5000,                  !- Skin Loss Radiative Fraction
        63605.6000,              !- Aggregated Thermal Mass of Energy Conversion Portion of Generator {W/K}
        1000.7000,               !- Aggregated Thermal Mass of Heat Recovery Portion of Generator {W/K}
        0.0000,                  !- Standby Power {W}
        TimeDelay,               !- Warm Up Mode
        ,                        !- Warm Up Fuel Flow Rate Coefficient
        ,                        !- Nominal Engine Operating Temperature {C}
        ,                        !- Warm Up Power Coefficient
        ,                        !- Warm Up Fuel Flow Rate Limit Ratio
        60.0000,                 !- Warm Up Delay Time {s}
        0.0000,                  !- Cool Down Power {W}
        60.0000,                 !- Cool Down Delay Time {s}
        OptionalCoolDown;        !- Restart Mode
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Generator Off Mode Time [s]
    HVAC,Sum,Generator Standby Mode Time [s]
    HVAC,Sum,Generator Warm Up Mode Time [s]
    HVAC,Sum,Generator Normal Operating Mode Time [s]
    HVAC,Sum,Generator Cool Down Mode Time [s]
    HVAC,Average,Generator Produced Electric Power [W]
    HVAC,Sum,Generator Produced Electric Energy [J]
    HVAC,Average,Generator Electric Efficiency [ ]
    HVAC,Average,Generator Thermal Efficiency [ ]
    HVAC,Average,Generator Gross Input Heat Rate [W]
    HVAC,Average,Generator Steady State Engine Heat Generation Rate [W]
    HVAC,Average, Generator Engine Heat Exchange Rate [W]
    HVAC,Average,Generator Air Mass Flow Rate [kg/s]
    HVAC,Average,Generator Fuel Molar Flow Rate [kmol/s]
    HVAC,Average,Generator Fuel Mass Flow Rate [kg/s]
    HVAC,Average,Generator Engine Temperature [C]
    HVAC,Average,Generator Coolant Inlet Temperature [C]
    HVAC,Average,Generator Coolant Outlet Temperature [C]
    HVAC,Sum,Generator Fuel HHV Basis Energy [J]
    HVAC,Average,Generator Fuel HHV Basis Rate [W]
    HVAC,Sum,Generator Fuel LHV Basis Energy [J]
    HVAC,Average,Generator Fuel LHV Basis Rate [W]
    HVAC,Average,Generator Fuel Compressor Electric Power [W]
    HVAC,Sum,Generator Fuel Compressor Electric Energy [J]
    HVAC,Average,Generator Fuel Compressor Skin Heat Loss Rate [W]
    HVAC,Average,Generator Zone Sensible Heat Transfer Rate [W]
    HVAC,Sum,Generator Zone Sensible Heat Transfer Energy [J]
    HVAC,Average,Generator Zone Convection Heat Transfer Rate [W]
    HVAC,Average,Generator Zone Radiation Heat Transfer Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Generator Off Mode Time [s]

This report is the amount of time the generator spent in "Off" mode in seconds.

#### Generator Standby Mode Time [s]

This report is the amount of time the generator spent in "Stand By" mode in seconds.

#### Generator Warm Up Mode Time [s]

This report is the amount of time the generator spent in "Warm Up" mode in seconds.

#### Generator Normal Operating Mode Time [s]

This report is the amount of time the generator spent in "Normal Operating" mode in seconds.

#### Generator Cool Down Mode Time [s]

This report is the amount of time the generator spent in "Cool Down" mode in seconds.

#### Generator Produced Electric Power [W]

This report is the net electrical power produced by the generator in watts.

#### Generator Produced Electric Energy [J]

This report is the net electrical energy produced by the generator in joules. This output variable is also put on the meter for on-site production.

#### Generator Electric Efficiency [ ]

This is the result for electrical efficiency.

#### Generator Thermal Efficiency [ ]

This is the result for thermal efficiency.

#### Generator Gross Input Heat Rate [W]

This is the gross rate of heat input (from fuel consumption) into the engine control volume.

#### Generator Steady State Engine Heat Generation Rate [W]

This is an interim value in the dynamic thermal calculation that describes the steady-state heat generation in the engine.

#### Generator Engine Heat Exchange Rate [W]

This is the rate of heat transfer within the generator between the engine section and the heat recovery section, in Watts.

#### Generator Produced Thermal Rate [W]

#### Generator Produced Thermal Energy [J]

These are the rate and energy of thermal heat transferred from the engine to the heat recovery fluid in the coolant control volume.

#### Generator Air Mass Flow Rate [kg/s]

This is the rate of air flow thru the engine on a mass basis.

#### Generator Fuel Molar Flow Rate [kmol/s]

This is the rate of fuel flow thru the engine on a molar basis.

#### Generator Fuel Mass Flow Rate [kg/s]

This is the rate of fuel flow thru the engine on a mass basis.

#### Generator Engine Temperature [C]

This is the model result for engine temperature in C. This is the lumped temperature for the engine control volume.

#### Generator Coolant Inlet Temperature [C]

This is the temperature of the inlet water used for heat recovery. It is determined by the balance of plant connected to the generator.

#### Generator Coolant Outlet Temperature [C]

This report is the model prediction for the leaving temperature of water used for heat recovery. If there is no flow this is the modeled prediction for the mass of cooling water in contact with the engine.

#### Generator Fuel HHV Basis Energy [J]

This is the energy used by the cogeneration device in terms of higher heating value in joules. This is the output variable is also put on the meter for natural gas.

#### Generator Fuel HHV Basis Rate [W]

This is the rate of fuel energy use by the cogeneration device in terms of higher heating value in watts.

#### Generator Fuel LHV Basis Energy [J]

This is the fuel energy used by the cogeneration device in terms of lower heating value in joules.

#### Generator Fuel LHV Basis Rate [W]

This is the rate of fuel energy use by the cogeneration device in terms of lower heating value in watts

#### Generator Zone Sensible Heat Transfer Rate [W]

Rate of heat gain to zone from cogeneration unit.

#### Generator Zone Sensible Heat Transfer Energy [J]

Heat gains to zone from cogeneration unit.

#### Generator Zone Convection Heat Transfer Rate [W]

Portion of rate of heat gain to zone in the form of surface convection heat transfer.

#### Generator Zone Radiation Heat Transfer Rate [W]

Portion of heat gains to zone in the form of surface radiation heat transfer (to other surfaces in zone).

## Generator:FuelCell

This object is used to model small Fuel Cell (FC) generators. IEA/ECBCS Annex 42 – see www.cogen-sim.net, developed the FC model implemented in EnergyPlus. The model can be used for two general types of fuel cells, solid oxide fuel cells (SOFC) and proton exchange membrane fuel cells (PEMFC). Warning! This is a complicated model, intended primarily for research. Because fuel cells for cogeneration (producing both heat and electricity) are not yet a mature product for building systems, the input data for this model are extremely difficult to obtain. However, the model is designed to allow investigating the relative implications of a broad array of subsystems and interactions with the building.

There should be one [Generator:FuelCell](#generatorfuelcell) object for each individual FC generator in the model. Multiple fuel cell generators can be included in a given simulation. Because there are a large number of inputs to the FC model and numerous, somewhat separable subsystems within a fuel cell, the EnergyPlus input objects have been organized into a number of separate input objects. This object simply refers to the names of all the other input objects that provide the input details for individual subsystems within the fuel cell cogeneration device. The following figure diagrams these separate subsystems. (not shown is an optional stack cooler with separate connections to heat recovery water loop and surrounding zone air).

![Fuel cell subsystems.](media/fuel-cell-subsystems..png)


Note that the FC model is formulated using the Lower Heating Value (LHV) of the fuel, whereas most other parts of EnergyPlus use the Higher Heating Value (HHV). LHV has important advantages over HHV in the context of the quality of heat recovered. While model coefficients for input performance descriptions are to be based on LHV, energy consumption and tariff calculations are also reported on HHV basis when using this model.

### Inputs

#### Field: Name

This field contains a unique name for the fuel cell.

#### Field: Power Module Name

This field contains the name of a [Generator:FuelCell:PowerModule](#generatorfuelcellpowermodule) input object.

#### Field: Air Supply  Name

This field contains the name of a [Generator:FuelCell:AirSupply](#generatorfuelcellairsupply) input object.

#### Field: Fuel Supply Name

This field contains the name of a [Generator:FuelSupply](#generatorfuelsupply) input object.

#### Field: Water Supply Name

This field contains the name of a [Generator:FuelCell:WaterSupply](#generatorfuelcellwatersupply) input object.

#### Field: Auxiliary Heater Name

This field contains the name of a [Generator:FuelCell:AuxiliaryHeater](#generatorfuelcellauxiliaryheater) input object.

#### Field: Heat Exchanger Name

This field contains the name of a Generator:FuelCell:ExhaustGasToWaterHeatExchanger input object.

#### Field: Electrical Storage Name

This field contains the name of a [Generator:FuelCell:ElectricalStorage](#generatorfuelcellelectricalstorage) input object.

#### Field: Inverter Name

This field contains the name of a [Generator:FuelCell:Inverter](#generatorfuelcellinverter) input object.

#### Field: Stack Cooler Name

This field contains the name of a Generator:FuelCell:StackCooler input object. This is optional. The presence of a stack cooler distinguishes between SOFC and PEMFC with PEMFC operating at lower temperatures and requiring a separate subsystem to maintain stack temperatures.

An example input data file (IDF) entry for this object is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:FuelCell,
        FCT SOFC,                !- Name
        FCT SOFC Power Module,   !- Power Module Name
        FCT SOFC Air Supply,     !- Air Supply Name
        FCT SOFC Fuel Supply,    !- Fuel Supply Name
        FCT SOFC Water Supply,   !- Water Supply Name
        FCT SOFC Auxiliary Heater,  !- Auxiliary Heater Name
        FCT SOFC Exhaust HX,     !- Heat Exchanger Name
        FCT SOFC Battery,        !- Electrical Storage Name
        FCT SOFC Inverter;       !- Inverter Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The following output variables are available for the fuel cell model.

~~~~~~~~~~~~~~~~~~~~

    Output:Variable,*,Generator Produced Electric Power,hourly; !- HVAC Average [W]
    Output:Variable,*,Generator Produced Electric Energy,hourly; !- HVAC Sum [J]
    Output:Variable,*,Generator Produced Thermal Rate,hourly; !- HVAC Average [W]
    Output:Variable,*,Generator Produced Thermal Energy,hourly; !- HVAC Sum [J]
    Output:Variable,*,Generator Fuel HHV Basis Energy,hourly; !- HVAC Sum [J]
    Output:Variable,*,Generator Fuel HHV Basis Rate,hourly; !- HVAC Average [W]
    Output:Variable,*,Generator Fuel LHV Basis Energy,hourly; !- HVAC Sum [J]
    Output:Variable,*,Generator Fuel Consumption Rate LHV Basis,hourly; !- HVAC Average [W]
    Output:Variable,*,FuelCell Heat Loss Rate to Zone,hourly; !- HVAC Average [W]
    Output:Variable,*,FuelCell Heat Loss Energy to Zone,hourly; !- HVAC Sum [J]
    Output:Variable,*,FuelCell Convection Heat Loss Rate to Zone,hourly; !- HVAC Average [W]
    Output:Variable,*,FuelCell Radiation Heat Loss Rate to Zone,hourly; !- HVAC Average [W]
~~~~~~~~~~~~~~~~~~~~

#### Generator Produced Electric Power [W]

This report is the net electrical power produced by the generator in watts.

#### Generator Produced Electric Energy [J]

This report is the net electrical energy produced by the generator in joules. This output variable is also put on the meter for on-site production.

#### Generator Produced Thermal Rate [W]

#### Generator Produced Thermal Energy [J]

These are the rate and energy of thermal heat transferred from the engine to the heat recovery fluid in the coolant control volume.

#### Generator Fuel HHV Basis Energy [J]

This is the energy used by the cogeneration device in terms of higher heating value in joules. This is the output variable is also put on the meter for natural gas.

#### Generator Fuel HHV Basis Rate [W]

This is the rate of fuel energy use by the cogeneration device in terms of higher heating value in watts.

#### Generator Fuel LHV Basis Energy [J]

This is the fuel energy used by the cogeneration device in terms of lower heating value in joules.

#### Generator Fuel Consumption Rate LHV Basis [W]

This is the rate of fuel energy use by the cogeneration device in terms of lower heating value in watts.

#### Generator Zone Sensible Heat Transfer Rate [W]

This variable provides the results for the total rate of skin losses from the fuel cell to the surrounding zone.

#### Generator Zone Sensible Heat Transfer Energy [J]

This variable provides the results for the total energy in skin losses from the fuel cell to the surrounding zone.

#### Generator Zone Convection Heat Transfer Rate [W]

This variable provides the results for the rate of skin losses in the form of surface convection heat transfer from the fuel cell to the surrounding zone.

#### Generator Zone Radiation Heat Transfer Rate [W]

This variable provides the results for the rate of skin losses in the form of radiation heat transfer from the fuel cell to the surrounding zone.

Using the FC model with DisplayAdvancedReportVariables (see Diagnostics object or use of operating system's environment variables) makes the following output variables available as listed in the RDD file. There are a large number of output variables that provide access to the inner workings of the model and individual subsystems that are not likely to be useful for the average user.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Generator Air Inlet Temperature [C]
    HVAC,Average,Generator Power Module Entering Air Temperature [C]
    HVAC,Average,Generator Air Molar Flow Rate [kmol/s]
    HVAC,Average,Generator Power Module Entering Air Enthalpy [W]
    HVAC,Average,Generator Blower Electric Power [W]
    HVAC,Sum,Generator Blower Electric Energy [J]
    HVAC,Average,Generator Blower Skin Heat Loss Rate [W]
    HVAC,Average,Generator Fuel Inlet Temperature [C]
    HVAC,Average,Generator Power Module Entering Fuel Temperature [C]
    HVAC,Average,Generator Fuel Molar Flow Rate [kmol/s]
    HVAC,Average,Generator Power Module Entering Fuel Enthalpy [W]
    HVAC,Average,Generator Fuel Compressor Electric Power [W]
    HVAC,Sum,Generator Fuel Compressor Electric Energy [J]
    HVAC,Average,Generator Fuel Compressor Skin Heat Loss Rate [W]
    HVAC,Average,Generator Fuel Reformer Water Inlet Temperature [C]
    HVAC,Average,Generator Power Module Entering Reforming Water Temperature [C]
    HVAC,Average,Generator Fuel Reformer Water Molar Flow Rate [kmol/s]
    HVAC,Average,Generator Fuel Reformer Water Pump Electric Power [W]
    HVAC,Sum,Generator Fuel Reformer Water Pump Electric Energy [J]
    HVAC,Average,Generator Power Module Entering Reforming Water Enthalpy [W]
    HVAC,Average,Generator Product Gas Temperature [C]
    HVAC,Average,Generator Product Gas Enthalpy [W]
    HVAC,Average,Generator Product Gas Molar Flow Rate [kmol/s]
    HVAC,Average,Generator Product Gas Ar Molar Flow Rate [kmol/s]
    HVAC,Average,Generator Product Gas CO2 Molar Flow Rate [kmol/s]
    HVAC,Average,Generator Product Gas H2O Vapor Molar Flow Rate [kmol/s]
    HVAC,Average,Generator Product Gas N2 Molar Flow Rate [kmol/s]
    HVAC,Average,Generator Product Gas O2 Molar Flow Rate [kmol/s]
    HVAC,Average,Generator Inverter Loss Power [W]
    HVAC,Average,Generator Produced DC Electric Power [W]
    HVAC,Average,Generator DC Power Efficiency [ ]
    HVAC,Average,Generator Electric Storage Charge State [J]
    HVAC,Average,Generator Produced DC Electric Power [W]
    HVAC,Sum,Generator DC Storage Charging Energy [J]
    HVAC,Average,Generator DC Storage Discharging Power [W]
    HVAC,Sum,Generator DC Storage Discharging Energy [J]
    HVAC,Average,Generator Ancillary AC Electric Power [W]
    HVAC,Sum,Generator Ancillary AC Electric Energy [J]
    HVAC,Average,Generator Zone Sensible Heat Transfer Rate [W]
    HVAC,Sum,Generator Zone Sensible Heat Transfer Energy [J]
    HVAC,Average,Generator Zone Convection Heat Transfer Rate [W]
    HVAC,Average,Generator Zone Radiation Heat Transfer Rate [W]
    HVAC,Sum, Generator Fuel Cell Model Iteration Count [ ]
    HVAC,Sum,Generator Regula Falsi Iteration Count [ ]
~~~~~~~~~~~~~~~~~~~~

#### Generator Air Inlet Temperature [C]

This variable provides the temperature of air supplied to the FC.

#### Generator Power Module Entering Air Temperature [C]

This variable provides the temperature of the air supplied to the power module after it has gone through the blower and received any heat recovered from other subsystems.

#### Generator Air Molar Flow Rate [kmol/s]

This variable provides the results for ![](media/image453.png) , the flow rate of air into the FC.

#### Generator Power Module Entering Air Enthalpy [W]

This variable provides the results for ![](media/image454.png) , which is the enthalpy flow in the air stream entering the power module relative to 25ºC.

#### Generator Blower Electric Power [W]

This variable provides the results for ![](media/image455.png) , which is the electrical power used by the air supply blower.

#### Generator Blower Electric Energy [J]

This variable provides the results for the energy used by the air supply blower. It is also added to the appropriate meters.

#### Generator Blower Skin Heat Loss Rate [W]

This variable provides the results for ![](media/image456.png) , which is the rate of energy "lost" to the surroundings.

#### Generator Fuel Inlet Temperature [C]

This variable provides the temperature of the fuel supplied to the FC.

#### Generator Power Module Entering Fuel Temperature [C]

This variable provides the temperature of the fuel supplied to the power module after it has gone through the compressor.

#### Generator Fuel Molar Flow Rate [kmol/s]

This variable provides the results for ![](media/image457.png) , which is the flow rate of fuel into the FC.

#### Generator Power Module Entering Fuel Enthalpy [W]

This variable provides the results for ![](media/image458.png) , which is the enthalpy flow in the fuel stream entering the power module relative to 25ºC.

#### Generator Fuel Compressor Electric Power [W]

This variable provides the results for ![](media/image459.png) , which is the electrical power used by the fuel supply compressor.

#### Generator Fuel Compressor Electric Energy [J]

This variable provides the results for the energy used by the fuel supply compressor. It is also added to the appropriate meters.

#### Generator Fuel Compressor Skin Heat Loss Rate [W]

This variable provides the results for the rate of energy "lost" to the surroundings.

#### Generator Fuel Reformer Water Inlet Temperature [C]

This variable provides the temperature of the water supplied to the FC for reforming.

#### Generator Power Module Entering Reforming Water Temperature [C]

This variable provides the temperature of the water supplied to the power module after it has gone through the pump.

#### Generator Fuel Reformer Water Molar Flow Rate [kmol/s]

This variable provides the results for ![](media/image460.png) , which the flow rate of reforming water into the FC.

#### Generator Fuel Reformer Water Pump Electric Power [W]

This variable provides the results for ![](media/image461.png) , which is the electrical power used by the water pump.

#### Generator Fuel Reformer Water Pump Electric Energy [J]

This variable provides the results for energy used by the water pump. It is also added to the appropriate meters.

#### Generator Power Module Entering Reforming Water Enthalpy [W]

This variable provides the results for ![](media/image462.png)  which is the enthalpy flow of the water stream entering the power module relative to 25ºC.

#### Generator Product Gas Temperature [C]

This variable provides the results for the temperature of the product gas stream leaving the fuel cell power module.

#### Generator Product Gas Enthalpy [W]

This variable provides the results for ![](media/image463.png) , which is the enthalpy flow in the product gas stream leaving the power module relative to 25ºC.

#### Generator Product Gas Molar Flow Rate [kmol/s]

This variable provides the results for the flow rate of all the product gases leaving the fuel cell power module.

#### Generator Product Gas Ar Molar Flow Rate [kmol/s]

This variable provides the results for the flow rate of the argon gas leaving the fuel cell power module.

#### Generator Product Gas CO2 Molar Flow Rate [kmol/s]

This variable provides the results for the flow rate of the carbon dioxide gas leaving the fuel cell power module.

#### Generator Product Gas H2O Vapor Molar Flow Rate [kmol/s]

This variable provides the results for the flow rate of the water gas leaving the fuel cell power module.

#### Generator Product Gas N2 Molar Flow Rate [kmol/s]

This variable provides the results for the flow rate of the nitrogen gas leaving the fuel cell power module.

#### Generator Product Gas O2 Molar Flow Rate [kmol/s]

This variable provides the results for the flow rate of the oxygen gas leaving the fuel cell power module.

#### Generator Inverter Loss Power [W]

This variable provides the results for the power losses associated with inefficiencies in the inverter.

#### Generator Produced DC Electric Power [W]

This variable provides the results for ![](media/image464.png) , which is the net DC electrical power produced by the fuel cell power module.

#### Generator DC Power Efficiency [ ]

This variable provides the results for ![](media/image465.png) , which is the electrical efficiency of the fuel cell power module.

#### Generator Electric Storage Charge State [J]

This variable provides the results for state of charge of the electrical storage device inside the fuel cell. Although the units are Joules, this is a state variable and is reported as an average rather than a sum.

#### Generator Produced DC Electric Power [W]

This variable provides the rate at which power was stored into the fuel cell's electrical storage subsystem.

#### Generator DC Storage Charging Energy [J]

This variable provides the energy stored into the fuel cell's electrical storage subsystem.

#### Generator DC Storage Discharging Power [W]

This variable provides the rate at which power was drawn from the fuel cell's electrical storage subsystem.

#### Generator DC Storage Discharging Energy [J]

This variable provides the energy drawn from the fuel cell's electrical storage subsystem.

#### Generator Ancillary AC Electric Power [W]

This variable provides the results for ![](media/image466.png) , which is the rate at which ancillary devices within the power module use electricity supplied to the fuel cell by an external source.

#### Generator Ancillary AC Electric Energy [J]

This variable provides the results for the energy used by AC ancillaries. It is also added to the appropriate meters.

#### Generator Fuel Cell Model Iteration Count  [ ]

This variable provides information on the numerical method used to calculate the FC model. EnergyPlus uses a sequential substitution iterative algorithm to solve the main heat balance equation for the FC model. This output variable indicates the number of iterations needed to converge.

#### Generator Regula Falsi Iteration Count [ ]

This variable provides information on the numerical method used to calculate product gas temperature as a function of the product gas's enthalpy. A Regula Falsi numerical method is used to invert the Shomate equation for enthalpy as a function of temperature. This output variable indicates the number of iterations needed for the Regula Falsi method to converge.

#### Generator Heat Recovery Exit Gas Temperature [C]

This is the temperature of the exiting gas for heat recovery in C.

#### Generator Heat Recovery Exit Gas H2O Vapor Fraction []

This is the water vapor fraction in the exit gas.

#### Generator Heat Recovery Water Condensate Molar Flow Rate [kmol/s]

This is the flow of condensed water in kmol/s.

## Generator:FuelCell:PowerModule

This object is used to describe the core power module subsystem of the FC. This includes the fuel cell stack, fuel reformer, and whatever ancillary devices are included inside. If the model has multiple FC generators that are of the exact same type, then only one of these objects is needed and all the [Generator:FuelCell](#generatorfuelcell) can reference it. The model uses a number of curves to describe operating performance. Input data for specific models of FC are not yet available but will be produced by IEA Annex 42 and should be available in the future.

### Inputs

#### Field: Name

This field contains a unique name for the fuel cell power module subsystem.

#### Field: Efficiency Curve Mode

This field is used to choose between different modes of inputting the electrical efficiency curve. There are two options available for this Field: "Annex42" and "Normalized". The "Annex42" option indicates that the efficiency curves are not normalized and are used for the single size and type of FC product. The "Normalized" option indicates that the efficiency curves are normalized (as is standard practice in EnergyPlus) relative to a single rating point. In the Annex42 mode, the curves directly represent efficiency as a function of fuel use, but in the Normalized mode, the curves represent an efficiency modification that is applied to the efficiency at the rating point.

#### Field: Efficiency Curve Name

This field contains the name of a [Curve:Quadratic](#curvequadratic) input object described elsewhere in the input file. The curve is a function of the net DC electric power, ![](media/image467.png)  (W), produced by the power module and describes either the electrical efficiency or the how the efficiency is modified depending on the mode selected in the previous field. This quadratic curve is just one part of the entire parametric relation used to describe electrical efficiency ![](media/image468.png) . For the Annex42 mode the full relation for efficiency is,

![](media/image469.png)\


where the [Curve:Quadratic](#curvequadratic) input object named in this field serves to define the quadratic portion in the first set of brackets. For the Normalized mode the full relation for efficiency is,

![](media/image470.png)\


where ![](media/image471.png)  is the nominal electrical efficiency at the rating point ![](media/image472.png)  which are described in the next two fields. The Normalized mode may be more useful for a simulation study that needs to vary the capacity of the FC.

#### Field: Nominal Efficiency

This field is used for the "Normalized" efficiency curve mode and provides the nominal efficiency ![](media/image473.png)  (dimensionless) at the rating point. This field can be left blank if using the "Annex42" mode.

#### Field: Nominal Electrical Power

This field is used for the "Normalized" efficiency curve mode and provides the nominal net DC electrical power produced ![](media/image474.png)  (W) at the rating point. This field can be left blank if using the "Annex42" mode.

#### Field: Number of Stops at Start of Simulation

This field is used to describe the number of times the FC has been cycled on and off prior to the start of the simulation. FC products might degrade as a result of starting and stopping the fuel cell stack, so the model includes terms to model the degradation effects of cycling FC units on and off. This is the initial value of the ![](media/image475.png) *~~*term in the efficiency relation above.

#### Field: Cycling Performance Degradation Coefficient

This field is used to describe the fixed value representing the fractional performance degradation associated with each start and stop cycle. This is the value of ![](media/image476.png)  in the efficiency relations above. If there is no degradation from cycling, then D should be 0.0.

#### Field: Number of Run Hours at Beginning of Simulation

This field is used to describe the amount of time the FC has been operating prior to the start of the simulation. FC products might degrade over time so the model includes terms to model the degradation effects of FC run time. This is the initial value of the integrated run time term ![](media/image477.png) .

#### Field: Accumulated Run Time Degradation Coefficient

This field is used to describe the fixed value representing the fractional performance degradation associated with the total amount of run time. This is the value of ![](media/image478.png)  in the efficiency relations above. If there is no degradation from run time, the L should be 0.0.

#### Field: Run Time Degradation Initiation Time Threshold

This field is used to describe the time period for which there is no degradation associated with the amount of run time. This provides the ability to model a system that initially operates without degradation for some period time before it starts to degrade. This is the value of ![](media/image479.png)  in the efficiency relations above.

#### Field: Power Up Transient Limit

This field is used to describe the maximum allowable rate at which the fuel cell can increase the level of power it produces (W/s). This is a time derivative of the net DC electrical power used to limit how fast the FC can ramp up.

#### Field: Power Down Transient Limit

This field is used to describe the maximum allowable rate at which the fuel cell can decrease the level of power it produces (W/s). This is a time derivative of the net DC electrical power used to limit how fast the FC can ramp down.

#### Field: Start Up Time

This field is used to describe the length of time (seconds) of the start up period when the FC is turned on.

#### Field: Start Up Fuel

This field is used to describe the amount of fuel (kmol) used during the entire start up period. The fuel is the same type as what is described in the [Generator:FuelSupply](#generatorfuelsupply) object elsewhere in the input file.

#### Field: Start Up Electricity Consumption

This field is used to describe the amount of electricity (Joules) used by ancillary equipment during the entire start up period.

#### Field: Start Up Electricity Produced

This field is used to describe the amount of electricity (Joules) produced by the power module during the entire start up period.

#### Field: Shut Down Time

This field is used to describe the length of time (seconds) of the shut down period.

#### Field: Shut Down Fuel

This field is used to describe the amount of fuel (kmol) used during the entire shut down period. The fuel is the same type as what is described in the [Generator:FuelSupply](#generatorfuelsupply) object elsewhere in the input file.

#### Field: Shut Down Electricity Consumption

This field is used to describe the amount of electricity (J) used by ancillary equipment during the entire shut down period.

#### Field: Ancilliary Electricity Constant Term

This field is used to describe the constant term, ![](media/image480.png) , in a relation that describes the AC electrical power used by ancillary equipment located inside the power module, ![](media/image481.png) . The model uses this relation,

![](media/image482.png)\


where ![](media/image483.png)  is the rate of fuel use in the power module (kmol/s).

#### Field: Ancilliary Electricity Linear Term

This field is used to describe the linear term, ![](media/image484.png) , in the relation shown in the previous field.

#### Field: Skin Loss Calculation Mode

This field is used to select the mode for how skin losses are modeled. Skin losses are heat energy "lost" from the power module and transferred to the surrounding thermal zone named in the following field.  There are three alpha options to choose from: "ConstantRate", "UAForProcessGasTemperature", or "QuadraticFunctionOfFuelRate."  Enter ConstantRate in this field to model skin losses using a constant rate, e.g. 100 watts. Enter UAForProcessGasTemperature to model skin losses using a "UA-Delta-T" calculation method with the temperature difference determined by the power module's product gas temperature and the surrounding zone. Enter QuadraticFunctionOfFuelRate to model skin losses as a function of the rate of fuel use.

#### Field: Zone Name

This field is used to describe the thermal zone that will receive skin and other heat losses from the FC. This should be the name of a [Zone](#zone) input object declared elsewhere in the input file.

#### Field: Skin Loss Radiative Fraction

This field is used to describe the portion of skin losses that are in the form of thermal radiation. This controls the radiative/convective split for the energy transferred to the zone by the FC.

#### Field: Constant Skin Loss Rate

This field is used to describe the constant rate of skin losses (W). This field is used with the ConstantRate mode. It can be left blank for other skin loss modes.

#### Field: Skin Loss U-Factor Times Area Term

This field is used to describe the "UA" term (W/K). This field is used with the UAForProcessGasTemperature mode. It can be left blank for other skin loss modes. "UA" is the product of the overall heat transfer coefficient "U" and the area "A."

#### Field: Skin Loss Quadratic Curve Name

This field is used to refer to the name of a quadratic curve object defined elsewhere in the input file. The quadratic curve should define skin heat loss rate as a function of the fuel use rate (kmol/s) so that,

![](media/image485.png)\


#### Field: Dilution Air Flow Rate

This field is used to describe the rate of dilution air (kmol/sec) drawn into, or through, the power module. This air stream may not be present in all FC products. It is used for limiting skin and stack heat losses and for strategies that mitigate potential safety issue were fuel gases to escape the stack.

#### Field: Stack Heat loss to Dilution Air

This field is used to describe the rate at which the fuel cell stack adds energy to the dilution air stream. It is entered as a constant heat rate (W).

#### Field: Dilution Inlet Air Node Name

This field is used to describe the air node where the dilution air is drawn from. This would typically be the name of the air node associated with the thermal zone where the FC is located.

#### Field: Dilution Outlet Air Node Name

This field is used to describe the air node where the dilution air is exhausted to. This could be the inlets to a heat recovery ventilator or an exhaust node.

#### Field: Minimum Operating Point

This field is used to describe the minimum operating point for the net DC electric power, ![](media/image486.png)  (W).

#### Field: Maximum Operating Point

This field is used to describe the maximum operating point for the net DC electric power, ![](media/image487.png)  (W).

An example input data file (IDF) entry for this object is provided below:

~~~~~~~~~~~~~~~~~~~~

    Generator:FuelCell:PowerModule,
        FCT SOFC Power Module,   !- Name
        Annex42,                 !- Efficiency Curve Mode
        FCT Power Curve,         !- Efficiency Curve Name
        0.354,                   !- Nominal Efficiency
        3400,                    !- Nominal Electrical Power {W}
        0,                       !- Number of Stops at Start of Simulation
        0.0,                     !- Cycling Performance Degradation Coefficient
        0,                       !- Number of Run Hours at Beginning of Simulation {hr}
        0.0,                     !- Accumulated Run Time Degradation Coefficient
        10000,                   !- Run Time Degradation Initiation Time Threshold {hr}
        1.4,                     !- Power Up Transient Limit {W/s}
        0.2,                     !- Power Down Transient Limit {W/s}
        0.0,                     !- Start Up Time {s}
        0.2,                     !- Start Up Fuel {kmol}
        ,                        !- Start Up Electricity Consumption {J}
        0.0,                     !- Start Up Electricity Produced {J}
        0.0,                     !- Shut Down Time {s}
        0.2,                     !- Shut Down Fuel {kmol}
        ,                        !- Shut Down Electricity Consumption {J}
        0.0,                     !- Ancilliary Electricity Constant Term
        0.0,                     !- Ancilliary Electricity Linear Term
        ConstantRate,            !- Skin Loss Calculation Mode
        ZN_1_FLR_1_SEC_5,        !- Zone Name
        0.6392,                  !- Skin Loss Radiative Fraction
        729,                     !- Constant Skin Loss Rate {W}
        0.0,                     !- Skin Loss U-Factor Times Area Term {W/K}
        ,                        !- Skin Loss Quadratic Curve Name
        6.156E-3,                !- Dilution Air Flow Rate {kmol/s}
        2307,                    !- Stack Heat loss to Dilution Air {W}
        SOFC Air HR Inlet,       !- Dilution Inlet Air Node Name
        SOFC Air HR Outlet,      !- Dilution Outlet Air Node Name
        3010,                    !- Minimum Operating Point {W}
        3728;                    !- Maximum Operating Point {W}
~~~~~~~~~~~~~~~~~~~~

## Generator:FuelCell:StackCooler

This object is optional and is used to define details needed to model the stack cooler on PEMFC.

### Field: Name

This field contains a unique name for the PEM fuel cell stack cooler

### Field: Heat Recovery Water Inlet Node Name

This field contains the node name for the water inlet to the stack cooler.

### Field: Heat Recovery Water Outlet Node Nane

This field contains the node name for the water outlet from the stack cooler.

### Field: Nominal Stack Temperature

This field contains the nominal stack operating temperature for the PEMFC, ![](media/image488.png) . This field and the next five provide values for use in the empirical equation for the rate of heat extracted from the fuel cell power module by the stack cooler using the following relation

![](media/image489.png)\


### Field: Actual Stack Temperature]

This field contains the actual stack operating temperature for the PEMFC, ![](media/image490.png) .

### Field: Coefficient r0

This field contains the coefficient r~0~ in the equation above.

### Field: Coefficient r1

This field contains the coefficient r~1~ in the equation above.

### Field: Coefficient r2

This field contains the coefficient r~2~ in the equation above.

### Field: Coefficient r3

This field contains the coefficient r~3~ in the equation above.

### Field: Stack Coolant Flow Rate

The stack cooler is assumed to have a closed water circulation loop that is used to extract heat from the stack and move it to a cogeneration heat exchanger and/or an air cooler. This field is used to enter the flow rate of coolant in this loop.

### Field: Stack Cooler U-Factor Times Area Value

This is the heat transfer coefficient between the stack and the coolant.

### Field: Fs-cogen Adjustment Factor

This field provides the value for an adjustment factor, ![](media/image491.png) , used in the following relation for the cogeneration heat transfer coefficient, ![](media/image492.png) :

![](media/image493.png)\


### Field: Stack Cogeneration Exchanger Area

This field provides the value for the cogeneration heat exchanger area, ![](media/image494.png)

### Field: Stack Cogeneration Exchanger Nominal Flow Rate

This field provides the value for ![](media/image495.png)  in the following relation for the cogeneration heat exchanger convection coefficient, ![](media/image496.png) :

![](media/image497.png)\


### Field: Stack Cogeneration Exchanger Nominal Heat Transfer Coefficient

This field provides the value for ![](media/image498.png)  in the relation above for the cogeneration heat exchanger convection coefficient, ![](media/image499.png) .

### Field: Stack Cogeneration Exchanger Nominal Heat Transfer Coefficient Exponent

This field provides the value for ![](media/image500.png)  is the relation above for the cogeneration heat exchanger convection coefficient, ![](media/image501.png) .

### Field: Stack Cooler Pump Power

This field is the power used by the stack cooler's circulating pump in Watts. It is a constant power draw whenever the stack cooler is active.

### Field: Stack Cooler Pump Heat loss

This field is the fraction of the pump power consumption that is lost to the surround zone.

### Field: Stack Air Cooler Fan Coefficient f0

This field and the next two are used to define coefficients for the following relation for the power used by the air-cooler's fan to dump excess heat into the surrounding zone:

![](media/image502.png)\


### Field: Stack Air Cooler Fan Coefficient f1

This field provides a value for the coefficient *f~1~* in the relation above.

### Field: Stack Air Cooler Fan Coefficient f2

This field provides a value for the coefficient *f~2~* in the relation above.

## Generator:FuelCell:AirSupply

This object is used to define details of the air supply subsystem.

### Inputs

#### Field: Name

This field contains a unique name for the fuel cell air supply subsystem.

#### Field: Air Inlet Node Name

This field is used to define the air node where the inlet air stream is drawn from. If the FC uses outdoor air then the node named here should be designated an outdoor air node. If the FC draws air from its surroundings, then this would be the name of the air node associated with the thermal zone containing the FC.

#### Field: Blower Power Curve Name

This field contains the name of a [Curve:Cubic](#curvecubic) input object described elsewhere in the input file. The curve provides the power used by the air blower (W) as a cubic function of the rate of air supply (kmol/s).

![](media/image503.png)\


#### Field: Blower Heat Loss Factor

This field describes the portion of blower electrical power that is "lost" to the surroundings. The rest of the energy is added to the air stream. A heat loss factor of 1.0 indicates that all the heat is lost to the surroundings and no energy is added to the air inlet stream.

#### Field: Air Supply Rate Calculation Mode

This field describes how the model will calculate the air supply rate. There are three options to choose from:  "AirRatiobyStoics", "QuadraticFunctionofElectricPower", and "QuadraticFunctionofFuelRate."  The choice will determine how the model calculates the rate of air and will depend on the formulations used to develop model input data. The choices are described in the next four fields.

#### Field: Stoichiometric Ratio

This field describes the excess air ratio beyond stoichiometric air supply. This field is only used if the air supply rate calculation mode is AirRatiobyStoics. The total air supply ratio will be the value entered here plus 1.0. Thus if 1.0 is entered in this field, the total air flow will be 2.0 times what is needed to exactly match stoichiometry with the fuel supply.

#### Field: Air Rate Function of Electric Power Curve Name

This field contains the name of a QuadraticCurve input object described elsewhere in the input file. This field is only used if the air supply rate calculation mode is QuadraticFunctionofElectricPower. The quadratic curve named here describes the first part of the relation below used to calculate the rate of inlet air, ![](media/image504.png) (kmol/s). It is a quadratic function of the net DC electrical power, ![](media/image505.png)  (W).

![](media/image507.png)\


#### Field: Air Rate Air Temperature Coefficient

This field is used to describe the air temperature coefficient, ![](media/image508.png) , used to adjust inlet air flow rate in the relations for inlet air rate.

#### Field: Air Rate Function of Fuel Rate Curve Name

This field contains the name of a [Curve:Quadratic](#curvequadratic) input object described elsewhere in the input file. This field is only used if the air supply rate calculation mode is QuadraticFunctionofFuelRate. The quadratic curve named here describes the first part of the relation used to calculate the rate of inlet air, ![](media/image509.png) (kmol/s). It is a quadratic function of the fuel flow rate, ![](media/image510.png) .

![](media/image511.png)\


#### Field: Air Intake Heat Recovery Mode

This field is used to control how heat losses from three other subsystems within the FC may or may not be recovered by the air inlet. There are six possible choices for what to use in this Field: "NoRecovery" "RecoverBurnerInverterStorage" "RecoverAuxiliaryBurner" "RecoverInverterandStorage" "RecoverInverter" and "RecoverElectricalStorage".  If heat is recovered then that means that energy "losses" from those subsystems are added to the inlet air and fed back into the FC power module. The following table clarifies the meaning of the six possible choices.

Table: Air Intake Heat Recovery Mode Choices

**Field Choice**|**Inverter Heat Losses Recovered?**|**Electrical Storage Heat Losses Recovered?**|**Auxiliary Heater Losses Recovered?**
-----------------------------|------------------------------------------------|----------------------------------------------------------|---------------------------------------------------
NoRecovery"|No|No|No
RecoverBurnerInverterStorage|Yes|Yes|Yes
RecoverAuxiliaryBurner|No|No|Yes
RecoverInverterandStorage|Yes|Yes|No
RecoverInverter|Yes|No|No
RecoverElectricalStorage|No|Yes|No

#### Field: Air Supply Constituent Mode

This field is used to choose between two different modes for describing the make up of the inlet air stream. There are two possible choices:  "AmbientAir" or "UserDefinedConstituents."  Choosing the AmbientAir mode will automatically set the composition of air to be usual at: 77.28% nitrogen, 20.73% oxygen, 1.04% water, 0.92% argon, and 0.03% carbon dioxide. Choosing UserDefinedConstituents allows the user to customize the make up the inlet air in the remaining fields.

#### Field: Number of UserDefined Constituents

This field is used to describe the number of constituents in the inlet air. This field is only used if the air supply constituent mode is set to UserDefinedConstituents. The maximum number is 5.

The remaining fields in this object are repeating pairs of fields with one pair for each constituent and the same number as described in the previous field. The pairs consist of the name of the constituent followed by the molar fraction of that constituent.

#### Field: Constituent 1-5 Name

These fields describe the air stream constituents by name. The name field for each pair must be filled with one of these choices:  "CarbonDioxide", "Nitrogen," "Oxygen," "Water," or "Argon."

#### Field: Molar Fraction 1-5

These fields describe the molar fraction of the air stream constituents. It is very important that the sum of the molar fractions add up to 1.0.

An example input data file (IDF) entry for this object is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:FuelCell:AirSupply,
        FCT SOFC Air Supply,     !- Name
        SOFC Air Inlet Node,     !- Air Inlet Node Name
        FCT Blower Power Curve,  !- Blower Power Curve Name
        1.0,                     !- Blower Heat Loss Factor
        QuadraticFunctionofElectricPower,  !- Air Supply Rate Calculation Mode
        ,                        !- Stoichiometric Ratio
        FCT Excess Air Ratio Curve,  !- Air Rate Function of Electric Power Curve Name
        2.83507E-3,              !- Air Rate Air Temperature Coefficient
        ,                        !- Air Rate Function of Fuel Rate Curve Name
        NoRecovery,              !- Air Intake Heat Recovery Mode
        UserDefinedConstituents,  !- Air Supply Constituent Mode
        5,                       !- Number of UserDefined Constituents
        Nitrogen,                !- Constituent 1 Name
        0.7728,                  !- Molar Fraction 1
        Oxygen,                  !- Constituent 2 Name
        0.2073,                  !- Molar Fraction 2
        Water,                   !- Constituent 3 Name
        0.0104,                  !- Molar Fraction 3
        Argon,                   !- Constituent 4 Name
        0.0092,                  !- Molar Fraction 4
        CarbonDioxide,           !- Constituent 5 Name
        0.0003;                  !- Molar Fraction 5
~~~~~~~~~~~~~~~~~~~~

## Generator:FuelCell:WaterSupply

This object is used to provide details of the water supply subsystem. This water is used for steam reforming of the fuel and is not the same as the water used for thermal heat recovery.

~~~~~~~~~~~~~~~~~~~~

    Generator:FuelCell:WaterSupply,
      A1, \field Name
          \required-field
          \reference FCWaterSupNames
      A2, \field Reformer Water Flow Rate Function of Fuel Rate Curve Name
          \type object-list
          \object-list QuadraticCurves
      A3, \field Reformer Water Pump Power Function of Fuel Rate Curve Name
          \type object-list
          \object-list QubicCurves
      N1, \field Pump Heat Loss Factor
      A4, \field Water Temperature Modeling Mode
          \type choice
          \key TemperatureFromAirNode
          \key TemperatureFromWaterNode
          \key TemperatureFromSchedule
          \key MainsWaterTemperature
      A5, \field Water Temperature Reference Node Name
      A6; \field Water Temperature Schedule Name
          \type object-list
          \object-list ScheduleNames
~~~~~~~~~~~~~~~~~~~~

### Inputs

#### Field: Name

This field contains a unique name for the fuel cell water supply subsystem.

#### Field: Reformer Water Flow Rate Function of Fuel Rate Curve Name

This field contains the name of a [Curve:Quadratic](#curvequadratic) input object described elsewhere in the input file. The curve provides the rate of water flow (kmol/sec) as a function of fuel flow rate (kmol/sec).

![](media/image512.png)\


If the unit does not use water for reforming then use a "null" quadratic where all the coefficients are 0.0.

#### Field: Reformer Water Pump Power Function of Fuel Rate Curve Name

This field contains the name of a [Curve:Cubic](#curvecubic) input object described elsewhere in the input file. The curve provides the power used by the water pump (W) as a cubic function of the rate of water supply (kmol/s).

![](media/image513.png)\


If the unit does not use water for reforming then use a "null" cubic where all the coefficients are 0.0.

#### Field: Pump Heat Loss Factor

This field describes the portion of pump electrical power that is "lost" to the surroundings. The rest of the energy is added to the water stream. A heat loss factor of 1.0 indicates that all the heat is lost to the surroundings and no energy is added to the water inlet stream.

#### Field: Water Temperature Modeling Mode

This field describes how the model will determine the inlet temperature of the water stream. There are four options to choose from:  "TemperatureFromAirNode," "TemperatureFromWaterNode," "TemperatureFromSchedule," and "MainsWaterTemperature."  For the first two options, the temperature of the water inlet is determined by the temperature at the node named in the next field. For the third option, the temperature is determined by the schedule in the last field. Using the MainsWaterTemperature option requires also defining a separate "[Site:WaterMainsTemperature](#sitewatermainstemperature)" input object elsewhere in the input file.

#### Field: Water Temperature Reference Node Name

This field is used to define the node where the temperature of the inlet water stream is obtained. This can be either an air node or a water node.

#### Field: Water Temperature Schedule Name

This field is used to define the name of a schedule, defined elsewhere in the input file, that will be used to determine the temperature of the water inlet stream when using the TemperatureFromSchedule mode.

An example input data file (IDF) entry for this object is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:FuelCell:WaterSupply,
        FCT SOFC Water Supply,   !- Name
        Null Quadratic,          !- Reformer Water Flow Rate Function of Fuel Rate Curve Name
        Null Cubic,              !- Reformer Water Pump Power Function of Fuel Rate Curve Name
        0.0,                     !- Pump Heat Loss Factor
        TemperatureFromAirNode,  !- Water Temperature Modeling Mode
        SOFC Air Inlet Node,     !- Water Temperature Reference Node Name
        ;                        !- Water Temperature Schedule Name

      Curve:Quadratic,
        Null Quadratic,          !- Name
        0.0,                     !- Coefficient1 Constant
        0.0,                     !- Coefficient2 x
        0.0,                     !- Coefficient3 x**2
        -1.0E+10,                !- Minimum Value of x
        1.0E+10;                 !- Maximum Value of x

      Curve:Cubic,
        Null Cubic,              !- Name
        0.0,                     !- Coefficient1 Constant
        0.0,                     !- Coefficient2 x
        0.0,                     !- Coefficient3 x**2
        0.0,                     !- Coefficient4 x**3
        -1.0E+10,                !- Minimum Value of x
        1.0E+10;                 !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

## Generator:FuelCell:AuxiliaryHeater

This object is intended for modeling an auxiliary heater, however this portion of the model is not yet available in EnergyPlus. An auxiliary heater may be present in fuel cell products so that the unit can meet much higher thermal loads than would be possible using only cogeneration. This portion of the model will be added in the future once suitable cogeneration control capabilities are also available. The input fields are described for future reference but may change. The program still requires one of these objects be included even though the data are not yet used (so that internal data structures can be allocated).

### Inputs

#### Field: Name

This field contains a unique name for the fuel cell auxiliary heater subsystem.

#### Field: Excess Air Ratio

This field describes the excess air ratio beyond stoichiometric air supply for the heater. The total air supply ratio will be the value entered here plus 1.0. Thus if 1.0 is entered in this field, the total air flow will be 2.0 times what is needed to exactly match stoichiometry with the fuel supply.

#### Field: Ancillary Power Constant Term

This field describes the constant term, ![](media/image514.png) , used to model the auxiliary burner's ancillary devices such as fans and ignition controls. The electrical power for ancillaries is calculated using,

![](media/image515.png)\


#### Field: Ancillary Power Linear Term

This field describes the linear term, ![](media/image516.png) , used to model the auxiliary burner's ancillary devices.

#### Field: Skin Loss U-Factor Times Area Value

This field describes the "UA" term (W/K) for calculating skin losses in the auxiliary burner. "UA" is the product of the overall heat transfer coefficient "U" and the area "A."  The skin heat losses are calculated using,

![](media/image517.png)\


where, ![](media/image518.png)  is temperature of product gases leaving the auxiliary burner and includes the mixing of the product gases from the fuel cell power module. Thus, the input for UA should reflect this choice of reference temperature rather than an exterior surface temperature of the auxiliary burner.

#### Field: Skin Loss Destination

This field describes the user's choice for what happens to the skin losses. There are two options:  "Surrounding[Zone](#zone)" and "AirInletForFuelCell". Entering Surrounding[Zone](#zone) will direct the model to put the "lost" energy into the surrounding thermal [Zone](#zone) named in the next field. Entering AirInletForFuelCell will direct the model to put the "lost" energy into the air inlet stream for the fuel cell to preheat that air.

#### Field: Zone Name to Receive Skin Losses

This field is used to determine the zone that will receive the skin losses. Enter the name of a [Zone](#zone) declared elsewhere in the input file. This field is only used if the "Surrounding[Zone](#zone)" mode is used in the previous field.

#### Field: Heating Capacity Units

This field describes the user's choice for how the capacity of the auxiliary heater will be defined. There are two options:  "Watts" or "kmol/s."  Entering Watts will direct the program to use the next two fields to determine the maximum and minimum heating capacity.  Entering kmol/s will direct the program to use the last two fields to determine the maximum and minimum heating capacities.

#### Field: Maximum Heating Capacity in Watts

This field is used to describe the maximum heating capacity of the auxiliary burner (W).

#### Field: Minimum Heating Capacity in Watts

This field is used to describe the minimum heating capacity of the auxiliary burner (W).

#### Field: Maximum Heating Capacity in Kmol per Second

This field is used to describe the maximum fuel use rate for the auxiliary burner (kmol/s).

#### Field: Minimum Heating Capacity in Kmol per Second

This field is used to describe the minimum fuel use rate for the auxiliary burner (kmol/s).

An example input data file (IDF) entry for this object is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:FuelCell:AuxiliaryHeater,
        FCT SOFC Auxiliary Heater,  !- Name
        0.0,                     !- Excess Air Ratio
        0.0,                     !- Ancilliary Power Constant Term
        0.0,                     !- Ancilliary Power Linear Term
        0.5,                     !- Skin Loss U-Factor Times Area Value
        SurroundingZone,         !- Skin Loss Destination
        ZN_1_FLR_1_SEC_5,        !- Zone Name to Receive Skin Losses
        Watts,                   !- Heating Capacity Units
        0.0,                     !- Maximum Heating Capacity in Watts
        0.0,                     !- Minimum Heating Capacity in Watts
        ,                        !- Maximum Heating Capacity in Kmol per Second
        ;                        !- Minimum Heating Capacity in Kmol per Second
~~~~~~~~~~~~~~~~~~~~

Generator:FuelCell:ExhaustGasToWaterHeatExchanger

This object is used to describe the exhaust gas heat exchanger subsystem of the FC used to recovery thermal energy.

#### Field: Name

This field contains a unique name for the fuel cell power module subsystem.

#### Field: Heat Recovery Water Inlet Node Name

This field contains the name of the node that connects the heat exchanger's inlet to the plant loop. This water is used for heat recovery.

#### Field: Heat Recovery Water Outlet Node Name

This field contains the name of the node that connects the heat exchanger's outlet to the plant loop.

#### Field: Heat Recovery Water Maximum Flow Rate

This field describes the design maximum flow rate of the heat recovery water (m^3^/s). The value should match the design flow rate of the plant loop connected to the FC.

#### Field: Exhaust Outlet Air Node Name

This field is used to determine which node will receive the exhaust air stream leaving the FC. This node will usually be outside and not be referenced elsewhere. However, if the exhaust stream is used in a heat recovery ventilator (as described in section 11 of the Annex 42 specification) then the node would be reference in the heat recovery ventilator object.

#### Field: Heat Exchanger Calculation Method

This field is used to direct how EnergyPlus will calculate heat exchanger performance. The Annex 42 model provides for four different methods of modeling the heat exchanger. The choices available for this field are:  "FixedEffectiveness" (method 1 in Annex 42 specification),  "EmpiricalUAeff " (method 2), "FundementalUAeff " (method 3), or "Condensing" (method 4). The remaining fields provide input data for the different methods. The heat exchanger correlations are described below.

#### Field: Method 1 Heat Exchanger Effectiveness

This field describes constant heat exchanger effectiveness, ![](media/image519.png) . This field is only used with the "FixedEffectiveness" mode, which corresponds to "method 1" in the Annex 42 specification. For this mode, the heat exchange between the exhaust gases and the heat recovery water are calculated using,

![](media/image520.png)\


where, ![](media/image521.png) is the minimum value of ![](media/image522.png) and ![](media/image523.png) .

#### Field: Method 2 Parameter hxs0

This field describes the value of ![](media/image524.png)  in the relation below for effective UA. This field is used with the "EmpiricalUAeff" mode, which corresponds to "method 2" in the Annex 42 specification and with the "Condensing" mode, which corresponds to "method 4". This method uses the log mean temperature difference (LMTD) approach to calculate the heat exchange:

![](media/image525.png)\


The user input in this field, and the next four fields, describe coefficients in an empirical relation for ![](media/image526.png) :

![](media/image527.png)\


#### Field: Method 2 Parameter hxs1

This field describes the value of ![](media/image528.png)  in the relation above for effective UA. This field is used with the "EmpiricalUAeff" mode, which corresponds to "method 2" in the Annex 42 specification and with the "Condensing" mode, which corresponds to "method 4".

#### Field: Method 2 Parameter hxs2

This field describes the value of ![](media/image529.png)  in the relation above for effective UA. This field is used with the "EmpiricalUAeff" mode, which corresponds to "method 2" in the Annex 42 specification and with the "Condensing" mode, which corresponds to "method 4".

#### Field: Method 2 Parameter hxs3

This field describes the value of ![](media/image530.png)  in the relation above for effective UA. This field is used with the "EmpiricalUAeff" mode, which corresponds to "method 2" in the Annex 42 specification and with the "Condensing" mode, which corresponds to "method 4".

#### Field: Method 2 Parameter hxs4

This field describes the value of ![](media/image531.png)  in the relation above for effective UA. This field is used with the "EmpiricalUAeff" mode, which corresponds to "method 2" in the Annex 42 specification and with the "Condensing" mode, which corresponds to "method 4".

#### Field: Method 3 h0Gas Coefficient

This field describes the value of ![](media/image532.png)  in the relation below. This field is only used with the "FundementalUAeff" mode, which corresponds to "method 3" in the Annex 42 specification. For this mode, the heat exchange is calculated using the LMTD method, but the effective UA is determined using:

![](media/image533.png)\


where,

![](media/image534.png)  is an adjustment factor,

![](media/image535.png) ,

![](media/image536.png)\


#### Field: Method 3 NdotGasRef Coefficient

This field describes the value of ![](media/image537.png)  in the relation above. This field is only used with the "FundementalUAeff" mode, which corresponds to "method 3" in the Annex 42 specification.

#### Field: Method 3 n Coefficient

This field describes the value of ![](media/image538.png)  in the relation above. This field is only used with the "FundementalUAeff" mode, which corresponds to "method 3" in the Annex 42 specification.

#### Field: Method 3 Gas Area

This field describes the value of ![](media/image539.png)  in the relation above. This field is only used with the "FundementalUAeff" mode, which corresponds to "method 3" in the Annex 42 specification.

#### Field: Method 3 h0 Water Coefficient

This field describes the value of ![](media/image540.png)  in the relation above. This field is only used with the "FundementalUAeff" mode, which corresponds to "method 3" in the Annex 42 specification.

#### Field: Method 3 N dot Water ref Coefficient

This field describes the value of ![](media/image541.png)  in the relation above. This field is only used with the "FundementalUAeff" mode, which corresponds to "method 3" in the Annex 42 specification.

#### Field: Method 3 m Coefficient

This field describes the value of ![](media/image542.png)  in the relation above. This field is only used with the "FundementalUAeff" mode, which corresponds to "method 3" in the Annex 42 specification.

#### Field: Method 3 Area Water Coefficient

This field describes the value of ![](media/image543.png)  in the relation above. This field is only used with the "FundementalUAeff" mode, which corresponds to "method 3" in the Annex 42 specification.

#### Field: Method 3 F Adjustment Factor

This field describes the value of ![](media/image544.png)  in the relation above. This field is only used with the "FundementalUAeff" mode, which corresponds to "method 3" in the Annex 42 specification.

#### Field: Method 4 hxl1 Coefficient

This field describes the value of ![](media/image545.png)  in the relation below for the rate of condensation of water from the gas stream, ![](media/image546.png) . This field is only used with the "Condensing" mode, which corresponds to "method 4" in the Annex 42 specification. This method uses a modified version of the LMTD approach to calculate heat exchange:

![](media/image547.png)\


where,

![](media/image548.png)\


#### Field: Method 4 hxl2 Coefficient

This field describes the value of ![](media/image549.png)  in the relation above for the rate of condensation of water from the gas stream, ![](media/image550.png) . This field is only used with the "Condensing" mode, which corresponds to "method 4" in the Annex 42 specification.

#### Field: Method 4 Condensation Threshold

This field describes the value of ![](media/image551.png)  in the relation above for the rate of condensation of water from the gas stream, ![](media/image552.png) . This field is only used with the "Condensing" mode, which corresponds to "method 4" in the Annex 42 specification.

An example input data file (IDF) entry for this object is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:FuelCell:ExhaustGasToWaterHeatExchanger,
        FCT SOFC Exhaust HX,     !- Name
        Small SOFC Heat Rec Inlet Node,  !- Heat Recovery Water Inlet Node Name
        Small SOFC Heat Rec Outlet Node,  !- Heat Recovery Water Outlet Node Name
        0.0004,                  !- Heat Recovery Water Maximum Flow Rate {m3/s}
        SOFC Air Outlet Node,    !- Exhaust Outlet Air Node Name
        CONDENSING,              !- Heat Exchanger Calculation Method
        ,                        !- Method 1 Heat Exchanger Effectiveness
        83.1,                    !- Method 2 Parameter hxs0
        4798,                    !- Method 2 Parameter hxs1
        -138E+3,                 !- Method 2 Parameter hxs2
        -353.8E+3,               !- Method 2 Parameter hxs3
        5.15E+8,                 !- Method 2 Parameter hxs4
        ,                        !- Method 3 h0Gas Coefficient
        ,                        !- Method 3 NdotGasRef Coefficient
        ,                        !- Method 3 n Coefficient
        ,                        !- Method 3 Gas Area
        ,                        !- Method 3 h0 Water Coefficient
        ,                        !- Method 3 N dot Water ref Coefficient
        ,                        !- Method 3 m Coefficient
        ,                        !- Method 3 Area Water Coefficient
        ,                        !- Method 3 F Adjustment Factor
        -1.96E-4,                !- Method 4 hxl1 Coefficient
        3.1E-3,                  !- Method 4 hxl2 Coefficient
        35.0;                    !- Method 4 Condensation Threshold {C}
~~~~~~~~~~~~~~~~~~~~

## Generator:FuelCell:ElectricalStorage

This object is used to describe the electrical storage subsystem for the FC. The electrical storage model is a very simple "constrained bucket" model. Future developments made add additional models for battery systems. Note that this electrical storage is embedded within the FC device.

### Inputs

#### Field: Name

This field contains a unique name for the fuel cell electrical storage subsystem.

#### Field: Choice of  Model

This field is used to direct how EnergyPlus will model electrical storage. The only choice currently available is "SimpleEfficiencyWithConstraints," which should be entered in this field. Future developments may expand the number of models available for electrical storage.

#### Field: Nominal Charging Energetic Efficiency

This field describes the value of ![](media/image553.png)  in the relation for the State of Charge, ![](media/image554.png) :

![](media/image555.png)\


This is the energetic efficiency of charging the storage device. A value of 1.0 means that the storage device does not lose any energy when charging. Note that the model can recover the "lost" energy into the air supply intake.

#### Field: Nominal Discharging Energetic Efficiency

This field describes the value of ![](media/image556.png)  in the relation for the State of Charge, ![](media/image557.png) :

![](media/image558.png)\


This is the energetic efficiency of discharging the storage device. A value of 1.0 means that the storage device does not lose any energy when discharging. Note that the model can recover the "lost" energy into the air supply intake.

#### Field: Simple Maximum Capacity

This field describes the maximum amount of electrical energy that can be stored in the device (J).

#### Field: Simple Maximum Power Draw

This field describes the maximum rate at which electrical power can be discharged from the storage device (W).

#### Field: Simple Maximum Power Store

This field describes the maximum rate at which electrical power can charge the storage device (W).

#### Field: Initial Charge State

This field describes the value for the initial state of charge (J). This allows the storage device to contain some amount of stored electricity at the beginning of the simulation period.

An example input data file (IDF) entry for this object is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:FuelCell:ElectricalStorage,
        FCT SOFC Battery,        !- Name
        SimpleEfficiencyWithConstraints,  !- Choice of Model
        1.0,                     !- Nominal Charging Energetic Efficiency
        1.0,                     !- Nominal Discharging Energetic Efficiency
        0,                       !- Simple Maximum Capacity {J}
        0,                       !- Simple Maximum Power Draw {W}
        0,                       !- Simple Maximum Power Store {W}
        0;                       !- Initial Charge State {J}
~~~~~~~~~~~~~~~~~~~~

## Generator:FuelCell:Inverter

This object is used to describe the power condition unit subsystem of the FC. This object models an inverter system contained within a fuel cell system that converts from direct current (DC) to alternating current (AC).

### Inputs

#### Field: Name

This field contains a unique name for the fuel cell inverter subsystem.

#### Field: Inverter Efficiency Calculation Mode

This field is used to direct how EnergyPlus will calculate the inverter efficiency. There are two possible choices:  "Constant" or "Quadratic".

#### Field: Inverter Efficiency

This field describes the value for inverter efficiency when using the **Constant** mode.

#### Field: Efficiency Function of DC Power Curve Name

This field names a [Curve:Quadratic](#curvequadratic) object defined elsewhere in the input file. The curve determines inverter efficiency as a function of the DC power entering the inverter as follows:

![](media/image559.png)\


An example input data file (IDF) entry for this object is provided below:

~~~~~~~~~~~~~~~~~~~~

      Generator:FuelCell:Inverter,
        FCT SOFC Inverter,       !- Name
        Quadratic,               !- Inverter Efficiency Calculation Mode
        ,                        !- Inverter Efficiency
        FCT Inverter Quadratic;  !- Efficiency Function of DC Power Curve Name

      Curve:Quadratic,
        FCT Inverter Quadratic,  !- Name
        0.560717,                !- Coefficient1 Constant
        1.24019E-4,              !- Coefficient2 x
        -2.01648E-8,             !- Coefficient3 x**2
        -1.0E+10,                !- Minimum Value of x
        1.0E+10;                 !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

## Generator:FuelSupply

This object is used to define details of the fuel supply for certain generators. This object allows the user to describe a wide array of possible fuel mixtures. The program contains data and internal methods to calculate the properties of the various mixtures of gaseous fuels. The program will calculate the Lower Heating Value (LHV), Higher Heating Value (HHV), and molecular weight for the fuel mixture. The fuel properties are then use by both fuel cell and Micro CHP models. Results of the gas phase thermochemistry modeling are also reported in the EIO file.

In addition to gaseous mixtures, the Micro CHP model is able to use generic liquid fuels and this object is used to describe the properties.

Note that the fuel consumption metering is currently handled by the Natural Gas meter. So no matter what fuel mixture is defined in this object, the aggregated reports from EnergyPlus meters for (natural) Gas will include the fuel used for the generator (with the Joule content based on HHV).

### Inputs

#### Field: Name

This field contains a unique name for the fuel cell fuel supply subsystem.

#### Field: Fuel Temperature Modeling Mode

This field describes how the model will determine the temperature of the fuel. There are two options for this field to choose from:  "TemperatureFromAirNode" and "Scheduled."  If this field contains TemperatureFromAirNode then the fuel is modeled as having the same temperature as the air node that is named in the next field. If this field contains Scheduled then the fuel is modeled as having the temperature described in a schedule defined elsewhere in the input file.

#### Field: Fuel Temperature Reference Node Name

This field contains the name of the air node used to obtain the inlet temperature of the fuel. This field is only used if the Fuel Temperature Modeling Mode is set to TemperatureFromAirNode.

#### Field: Fuel Temperature Schedule Name

This field contains the name of a temperature schedule defined elsewhere in the input file. This field is only used if the Fuel Temperature Modeling Mode is set to Scheduled. The temperature of the fuel inlet will be obtained from the schedule.

#### Field: Compressor Power Function of Fuel Rate Curve Name

This field contains the name of a [Curve:Cubic](#curvecubic) input object described elsewhere in the input file. The curve provides the electrical power used by the fuel compressor (W) as a cubic function of the rate of fuel supply (kmol/s).

![](media/image560.png)\


#### Field: Compressor Heat Loss Factor

This field describes the portion of the compressor electrical power that is "lost" to the surroundings. The rest of the energy is added to the fuel stream. A heat loss factor of 1.0 indicates that all the heat is lost to the surroundings and no energy is added to the fuel inlet stream.

#### Field: Fuel Type

Choose between "GaseousConstituents" or "LiquidGeneric".

#### Field: Liquid Generic Fuel Lower Heating Value

For fuel type "LiquidGeneric," this field is used to enter the lower heating value of the fuel in units of kJ/kg.

#### Field: Liquid Generic Fuel Higher Heating Value

For fuel type "LiquidGeneric," this field is used to enter the higher heating value of the fuel in units of kJ/kg.

#### Field: Liquid Generic Fuel Molecular Weight

For fuel type "LiquidGeneric," this field is used to enter the molecular weight of the fuel in units of g/mol.

#### Field: Liquid Generic Fuel CO2 Emission Factor

This field is not used. This part of the Annex 42 model was not implemented in EnergyPlus; see the [FuelFactors](#fuelfactors) object.

#### Field: Number of Constituents in Gaseous Constituent Fuel Supply

This field is used to describe the number of constituents in the inlet fuel supply. The maximum number of different types of constituents is currently set to 12.

The remaining fields in this object are repeating pairs of fields with one pair for each constituent and the same number as described in the previous field. The field set pairs consist of the name of the constituent followed by the molar fraction of that constituent.

#### Field: Constituent 1-12 Name

These fields describe the fuel stream constituents by name. The name field for each pair must be filled with one of these 14 choices:  "CarbonDioxide", "Nitrogen", "Oxygen", "Water", "Argon", "Hydrogen", "Methane", "Ethane", "Propane", "Butane", "Pentane", "Hexane", "Methanol", and "Ethanol".  No other fuel constituents can currently be modeled. The properties of the constituents are already contained inside the program as provided in the Annex 42 model specification.

#### Field: Constituent 1-12  Molar Fraction

These fields describe the molar fraction of the fuel stream constituents. It is very important that the sum of the molar fractions add up to 1.0.

### Outputs

In addition to the following output variables, the resulting fuel heating values are also reported to the eio file.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Generator Fuel Compressor Electric Power [W]
    HVAC,Sum,Generator Fuel Compressor Electric Energy [J]
    HVAC,Average,Generator Fuel Compressor Skin Heat Loss Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Generator Fuel Compressor Electric Power [W]

This is the rate of energy use by the gas compressor, if any, that provides fuel at a higher pressure.

#### Generator Fuel Compressor Electric Energy [J]

This is the energy used by the gas compressor, if any, that provides fuel at a higher pressure.

#### Generator Fuel Compressor Skin Heat Loss Rate [W]

Convective heat gains to zone from gas compressor, if any, that provides fuel at a higher pressure.

## Photovoltaic Generators

EnergyPlus offers different options for predicting the electricity produced by solar electric photovoltaic (PV) panels. The three different options are "Simple", "Equivalent One-Diode" and "Sandia" and the choice will determine the mathematical models used to determine energy production. The first allows the user to input an arbitrary efficiency. The other two models use empirical relationships to predict PV operating performance based on many environmental variables such as cell temperature.  The PV models can be used with either normal PV modules or as part of a hybrid photovoltaic/thermal (PVT) flat plat collector.

The generator is connected to an Electric Load Center.  The  PV models refer to surface input objects defined elsewhere in the input file.  This object describes an array that is "attached" to a surface object in order to describe its orientation and to access results of the solar insolation calculations. These can be either Heat Transfer or Shading objects (see Surfaces). This will define the orientation of the solar panel for the detailed models and also the area for the simple model. The exposure of that surface to incident solar radiation is calculated using the full set of models in EnergyPlus that are used to account for solar thermal loads arising from building windows and walls. Therefore the incident solar radiation is calculated to include the effects of shading and reflections from other surfaces declared in the input file. In addition to the output variables associated with PV models, there are numerous related output variables available for the surfaces including: 'Surface Outside Face Sunlit Area', 'Surface Outside Face Sunlit Fraction', 'Surface Outside Face Incident Solar Radiation Rate per Area', 'Surface Outside Face Incident Beam Solar Radiation Rate per Area', 'Surface Outside Face Incident Sky Diffuse Solar Radiation Rate per Area', 'Surface Outside Face Incident Ground Diffuse Solar Radiation Rate per Area', 'Surface Outside Face Beam Solar Incident Angle Cosine Value', 'Surface Outside Face Incident Sky Diffuse Ground Reflected Solar Radiation Rate per Area', 'Surface Outside Face Incident Sky Diffuse Surface Reflected Solar Radiation Rate per Area', 'Surface Outside Face Incident Beam To Beam Reflected From Surfaces Solar Radiation Rate per Area', 'Surface Outside Face Incident Beam To Diffuse Surface Reflected Solar Radiation Rate per Area', and 'Surface Outside Face Incident Beam To Diffuse Ground Reflected Solar Radiation Rate per Area.'  Note that it is possible to define a PV array area that doesn't conform to the surface area (i.e. model a PV array area that is larger or smaller than what is available on the surface) so it is the users responsibility to not over-specify area when determining how many modules will fit onto a surface. However this type of input is allowed.

EnergyPlus does not include models for ancillary equipment for a PV array including charge controllers or power-point trackers. The operation of the entire electrical system that must go along with a PV array is not currently modeled in EnergyPlus and is therefore assumed to operate in ideal ways.  The PV production models do not resolve voltage and current, only power (and energy).  Electricity production is metered based on the output of the inverter. Output results are available before and after the inverter. One assumption is that the array is assumed to be always operating at the maximum power point. Energy production is based on the assumption that the quasi-steady power prediction is constant and continuous over the simulation timestep.

For a variety of reasons, actual installations of photovoltaics are often observed to exhibit system-level problems that significantly reduce electricity production. Therefore this modeling should be considered a method of bracketing the upper end of electricity production rather than an accurate prediction of what the panels will produce. Also note that the model predictions are closely related to the solar radiation data (typically of of TMY2 origin) in the EnergyPlus weather file, that the solar data in these is usually from a model rather than direct measurements, and that solar resources encountered by a real installation in a given year are likely to differ from the weather data file.

## Generator:Photovoltaic

This object is used to describe an array of PV modules and how they are to be modeled.  A series of different PV arrays can be connected to a single electric load center (and inverter) by listing them all in an ElectricLoadCenter:Generator object.

### Inputs

#### Field: Name

This field is a unique name for the PV array.

#### Field: Surface Name

This field is the name of a surface that defines the location and geometry of the array.

#### Field: Photovoltaic Performance Object Type

This field is the type of PV performance model.  The choices are the class names for the three modeling options:

~~~~~~~~~~~~~~~~~~~~

    PhotovoltaicPerformance:Simple
    PhotovoltaicPerformance:EquivalentOne-Diode
    PhotovoltaicPerformance:Sandia.
~~~~~~~~~~~~~~~~~~~~

#### Field: Module Performance Name

This is the name of the PV performance object define elsewhere (corresponding to the object type listed in the previous field)

#### Field: Heat Transfer Integration Mode

The PV model allows for different ways of integrating with other EnergyPlus heat transfer surfaces and models and calculating cell temperature.  For [Building](#building) Integrated PV (BIPV), the "Integrated" options allow for the PV modeling to be coupled to the surface heat transfer models elsewhere in EnergyPlus.  The user must select one of these options for this field:

**Decoupled.**  The cell temperature of modules in the array is computed based on a energy balance relative to NOCT conditions.  The input fields for Module Heat Capacity and Module Heat Loss Coefficient are ignored.

**DecoupledUllebergDynamic**  the cell temperature is calculated based on a dynamic model developed by Ulleberg.  The input fields for Module Heat Capacity and Module Heat Loss Coefficient are required.

**IntegratedSurfaceOutsideFace**  The cell temperature is obtained from the outside face of the Surface:Heat Transfer named in the previous field.  Energy exported by the module is removed from the heat transfer surface (at the position determined by the associated [Construction](#construction) with Internal Source).  The input fields for Module Heat Capacity and Module Heat Loss Coefficient are ignored.

**IntegratedTranspiredCollector**  The cell temperature is obtained from the collector surface temperature in the unglazed transpired collector model.  Energy exported by the module is removed from the collector surface.  The input fields for Module Heat Capacity and Module Heat Loss Coefficient are ignored.

**IntegratedExteriorVentedCavity**  The cell temperature is obtained from the exterior baffle temperature in the naturally ventilated exterior cavity model.  Energy exported by the module is removed from the exterior baffle surface.  The input fields for Module Heat Capacity and Module Heat Loss Coefficient are ignored.

**PhotovoltaicThermalSolarCollector**  The cell temperature is obtained from the photovoltaic/thermal solar collector model.  If the PV layer is inside the collector, then the incident solar is modified by the photovoltaic/thermal solar collector model.

#### Field: Number of Series Strings in Parallel

This field is the number of series-wired strings of PV modules that are in parallel to form the PV array.  The product of this field and the next field should equal the total number of modules in the array.

#### Field: Number of Modules in Series

This field is the number of modules wired in series (on each string) to form the PV array.  The product of this field and the previous field should equal the total number of modules in the array.

### Outputs

Using the Generator:PV:Simple object makes a number of output variables available.\\

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Generator Produced DC Electric Power [W]
    HVAC,Sum,Generator Produced DC Electric Energy [J]
    HVAC,Average,Generator PV Cell Temperature [C]
    HVAC,Average,Generator PV Short Circuit Current [A]
    HVAC,Average,Generator PV Open Circuit Voltage [V]
    HVAC,Average,Generator PV Array Efficiency []
~~~~~~~~~~~~~~~~~~~~

#### Generator Produced DC Electric Power [W]

This output variable is the power of DC electricity produced by the PV array in Watts..

#### Generator Produced DC Electric Energy [J]

This output variable is the energy in DC electricity produced by the PV array, in Joules.

#### Generator PV Array Efficiency [ ]

This output variable is the resulting efficiency of the PV array .

The two more detailed PV models also have the following output variables.

#### Generator PV Cell Temperature [°C]

This output variable represents the temperature of the solar cell used in the calculation of cell performance.

#### Generator PV Short Circuit Current [A]

This output variable represents the short circuit current of the PV array, in Amps. This is provided to describe the I-V characteristics.

#### Generator PV Open Circuit Voltage [V]

This output variable represents the open circuit voltage of the PV array, in Volts. This is provided to describe the I-V characteristics.

## PhotovoltaicPerformance:Simple

The [PhotovoltaicPerformance:Simple](#photovoltaicperformancesimple) object describes a simple model of photovoltaics that may be useful for early phase design analysis. In this model the user has direct access to the efficiency with which surfaces convert incident solar radiation to electricity and need not specify arrays of specific modules. The full geometric model for solar radiation is used, including shading and reflections, to determine the incident solar resource. This model is intended to be useful for design purposes to quickly get an idea of the levels for annual production and peak power. The model can also accept arbitrary conversion efficiencies and does not require actual production units be tested to obtain performance coefficients.

### Inputs

#### Field: Name

This field is the name of the PV array. The name is only used as an identifier. Multiple instances need have unique names.

#### Field: Fraction of Surface Area with Active Solar Cells

This field is the user defined fraction for the area of surface named in the parent PV object that will have active PV cells on it. The area actually covered with solar cells will be the net area of the surface (gross area less any subsurfaces like windows) times the fraction entered here. This fraction includes the difference between PV module area and active cells within it and any losses for how closely packed modules can be arranged on surface.  The value should be between 0.0 and 1.0.

#### Field: Conversion Efficiency Input Mode

This field specifies how the PV array efficiency values are input. There are two choices, **Fixed**  and **Scheduled**.  If this field is set to ‘Fixed' then the PV array always has the efficiency value specified in the next field. If this field is set to ‘Scheduled' then the efficiency is determined by a user defined schedule named in the second field to follow.

#### Field: Value for Cell Efficiency if Fixed

This field specifies the efficiency with which solar incident energy is converted to electricity. Efficiency = (electrical power generated [W])/(power of incident solar[W]). These efficiency values are dimensionless and should be between 0.0 and 1.0.

#### Field: Efficiency Schedule Name

This field should be set to the name of schedule defined elsewhere in the input file. This schedule should specify dimensionless efficiency values between 0.0 and 1.0. This could be used for example to vary the efficiency to match results computed with more detailed models.

An example idf instance of this object follows:

~~~~~~~~~~~~~~~~~~~~

    PhotovoltaicPerformance:Simple,
       Simple PV Flat, !- Name
       0.1044 , !- Fraction of Surface area that has active solar cells
       FIXED ,  !- Conversion efficiency input mode
       0.12 ,   !- Value for cell efficiency if fixed
         ;      !- Name of Schedule that Defines Efficiency
~~~~~~~~~~~~~~~~~~~~

## PhotovoltaicPerformance:EquivalentOne-Diode

This object describes the performance characteristics of Photovoltaic (PV) modules to be modeled using an equivalent one-diode circuit.  This model is also known at the 4- or 5-parameter TRNSYS model for photovoltaics.

The following table shows several sample PV array types with their input values.

Table: Photovoltaics Array Types with Values

|Short Circuit Current|Open Circuit Voltage|Voltage at Maximum Power|Current at Maximum Power|Temperature Coefficient of Short Circuit Current|Temperature Coefficient of Open Circuit Voltage|Number of Cells in Series per Module|Cell Temperature at NOCT Conditions|Module Area
|---------------------|--------------------|------------------------|------------------------|------------------------------------------------|-----------------------------------------------|------------------------------------|-----------------------------------|-----------
**Units:**|**[A]**|**[V]**|**[V]**|**[A]**|**{A/K]**|**[V/K]**|**[-]**|**[K]**|**[m2]**
ASE 300-DFG/50 |6.2|60|50.5|5.6|0.001|-0.0038|216|318|2.43
BPsolar 275 |4.75|21.4|17|4.45|0.00065|-0.08|36|320|0.63
BPsolar 3160 |4.8|44.2|35.1|4.55|0.00065|-0.16|72|320|1.26
BPsolar 380 |4.8|22.1|17.6|4.55|0.00065|-0.08|36|320|0.65
BPsolar 4160 |4.9|44.2|35.4|4.52|0.00065|-0.16|72|320|1.26
BPsolar 5170 |5|44.2|36|4.72|0.00065|-0.16|72|320|1.26
BPsolar 585 |5|22.1|18|4.72|0.00065|-0.08|36|320|0.65
Shell SM110-12 |6.9|21.7|17.5|6.3|0.0028|-0.076|36|318|0.86856
Shell SM110-24 |3.45|43.5|35|3.15|0.0014|-0.152|72|318|0.86856
Shell SP70 |4.7|21.4|16.5|4.25|0.002|-0.076|36|318|0.6324
Shell SP75 |4.8|21.7|17|4.4|0.002|-0.076|36|318|0.6324
Shell SP140 |4.7|42.8|33|4.25|0.002|-0.152|72|318|1.320308
Shell SP150 |4.8|43.4|34|4.4|0.002|-0.152|72|318|1.320308
Shell S70 |4.5|21.2|17|4|0.002|-0.076|36|317|0.7076
Shell S75 |4.7|21.6|17.6|4.2|0.002|-0.076|36|317|0.7076
Shell S105 |4.5|31.8|25.5|3.9|0.002|-0.115|54|317|1.037
Shell S115 |4.7|32.8|26.8|4.2|0.002|-0.115|54|317|1.037
Shell ST40 |2.68|23.3|16.6|2.41|0.00035|-0.1|16|320|0.424104
UniSolar PVL-64 |4.8|23.8|16.5|3.88|0.00065|-0.1|40|323|0.65
UniSolar PVL-128 |4.8|47.6|33|3.88|0.00065|-0.2|80|323|1.25

Where:

Shunt Resistance: the value of shunt resistance is finite only if the PV Module being modeled is a thin film variety. For all crystaline silicon modules the value is essentially infinite

Shunt Resistance value: 1,000,000  (ohms)

Module Heat Loss Coefficient: this value is dependent more on the array configuration than on the module itself.

Module Heat Loss Coefficient value: 30 W/m2-K

Module Heat Capacity: this is a typical value for a silicon based sandwich construction framed PV panel.

Module Heat Capacity Value: 50,000 J/m2-K

Reference Temperature: 298K (25C)

Insolation at Reference Conditions: 1000 W/m2

Ambient Temperature at NOCT conditions: 293K (20C)

Insolation at NOCT Conditions: 800 W/m2

Average tau-alpha Product: .9

Semiconductor Bandgap: 1.12 eV

### Field: Name

This field contains the uniqe name for the photovoltaic module performance data.  The name is only used as an identifier.

### Field: Cell Type

This field is used to describe the type of technology used in the PV module.  There are two options available, **CrystallineSilicon** and **AmorphousSilicon**. The choice affects the modeling.

### Field: Number of Cells in Series

This field is an integer representing the number of individual cells wired in series to make up a single module. The typical number for a 12V crystalline silicon PV module is 36.

### Field: Active Area

This field is the active area of the PV module in m^2^.

### Field: Transmittance Absorptance Product

This field indicates the transmittance-absorptance product at normal incidence angles for the PV modules. If the  product is positive, that value will be used for all angles of incidence. If the value specified is negative, then the magnitude of the given value will be used for normal incidence and the IAM modifier correlation will be used for all other angles.

### Field: Semiconductor Bandgap

This field is the semiconductor bandgap for the PV material. The bandgap for silicon is 1.12 eV (electron volts).

### Field: Shunt Resistance

This field is the shunt (parallel) resistance (in ) in the single diode electrical model of the PV. The shunt resistance is effectively infinite for crystalline silicon based PV modules and is finite for thin film and exotic metal modules.

### Field: Short Circuit Current

This field is the short circuit current (in Amps) for an individual module in the PV array at reference conditions.

### Field: Open Circuit Voltage

This field is the open circuit voltage (in Volts) for an individual module in the PV array at reference conditions.

### Field: Reference Temperature

This field is the ambient temperature (in Kelvin) at reference conditions. The value is usually 298K

### Field: Reference Insolation

This field is the radiation level (in W/m^2^) at reference conditions. The value is usually 1000 W/m^2^.

### Field: Module Current at Maximum Power

This field is module current (in Amps) at the maximum power point and reference conditions.

### Field: Module Voltage at Maximum Power

This field is module voltage (in Volts) at the maximum power point and reference conditions.

### Field: Temperature Coefficient of Short Circuit Current

This field accounts for the fact that the module short circuit current is temperature dependent. The coefficient is given in Amps/Kelvin.

### Field: Temperature Coefficient of Open Circuit Voltage

This field accounts for the fact that the module open circuit voltage is temperature dependent. The coefficient is given in Volts/Kelvin.

### Field: Nominal Operating Cell Temperature Test Ambient Temperature

This field is the ambient temperature (in Kelvin) from the Nominal Operating Cell Temperature (NOCT) test. The value is usually 293 K

### Field: Nominal Operating Cell Temperature Test Cell Temperature

This field is the cell temperature (in Kelvin) from the Nominal Operating Cell Temperature (NOCT) test.

### Field: Nominal Operating Cell Temperature Test Insolation

This field is the insolation level (in W/m^2^) from the Nominal Operating Cell Temperature (NOCT) test. The value is usually 800 W/m^2^.

### Field: Module Heat Loss Coefficient

This field is the heat loss coefficient (in W/m^2^.K) for the array. The heat loss coefficient is dependent upon measures taken to actively or passively promote airflow over the array surface. The heat loss coefficient value is used only if the Integration and Cell Temperature Mode is set to "Decoupled Ulleberg Dynamic."

### Field: Total Heat Capacity

This field is the heat capacity (in J/m^2^.K) of the modules in the array. It describes the module's ability to store incident solar radiation internally. Such energy storage is manifested as a temperature increase in the modules that  is considered to be undesirable. The total heat capacity value is used only if the Integration and Cell Temperature Mode is set to "Decoupled Ulleberg Dynamic."

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    PhotovoltaicPerformance:EquivalentOne-Diode,
        Siemens, ! Name
        CrystallineSilicon, !cell Type
        36,      !cells in series [-]
        1.0,     !module area [m2]
        0.9,     !tau alpha product at normal incidence [-]
        1.12,    !electron bandgap [eV]
        1000000, !shunt resistance [ohms]
        6.5,     !short circuit current at reference conditions [A/K]
        21.6,    !open circuit voltage at reference conditions [V/K]
        25,      !temperature at reference conditions [C]
        1000,    !radiation at reference conditions [W/m2]
        5.9,     !current at max power
        17,      !voltage at max power
        0.002,   !temperature coefficient of short circuit current
        -0.079,  !temperature coefficient of open circuit voltage
        20,      !ambient temperature at NOCT [C]
        45,      !cell temperature at NOCT [C]
        800,     !radiation at NOCT [W/m2]
        30,      !heat loss coefficient [W/m2.K]
        50000;   !total heat capacity (only used in TC mode 1)
~~~~~~~~~~~~~~~~~~~~

## PhotovoltaicPerformance:Sandia

This ‘[PhotovoltaicPerformance:Sandia](#photovoltaicperformancesandia)' object describes the performance of a single type of module.  The model used with this object for predicting the electricity generated by photovoltaics is referred to as the Sandia model. This model is based on work done at Sandia National Lab, Albuquerque, NM by David King -- with the help of many others.

This object describes performance input data needed for specific makes and models of production PV panels using the empirical coefficients assembled by Sandia National Laboratory.  There are a large number of empirical coefficients required to use the Sandia model. These data are obtained after extensive measurements and data reduction. This testing has been performed for some types of production panels and Sandia publishes a database of module and array performance parameters on the web at www.sandia.gov/pv. The entries in the database as of January 15, 2004 were converted to idf segments and are included with the EnergyPlus release in the library file SandiaPVData.imf. There are more than hundred different module types included in the library and identified by the manufacturer's model names listed at the beginning of the file SandiaPVData.imf. This data library file is arranged for use with EpMacro but the user can also copy .idf segments directly from the macro data set.

The Sandia model itself can aggregate multiple PV modules. Therefore, an instance of a ‘[PhotovoltaicPerformance:Sandia](#photovoltaicperformancesandia)' object could actually represent an array of modules as well as the more usual single module. These can be can be thought of as simple larger modules and used by Generator:PV:Sandia objects in the usual manner.

The many empirical coefficients for the model are listed below and are described in more more detail in the EnergyPlus engineering documentation EngineeringDoc.pdf.

The field descriptions below focus on providing the parameter's variable name as identified in the Sandia database. See the the EngineeringDoc.pdf for details on how the input fields are used in the correlations. The user generally would not need to worry about the details of the fields since he/she is not likely to generate the data on their own.

### Inputs

#### Field: Name

This field provides a unique name for the PV module. This is often the manufacturer's identifier for a particular model of a PV module.

#### Field: Active Area

Units m^2^ , real number, area of active solar electric cell for the entire module.

#### Field: Number of Cells in Series

"Series_Cells" in Sandia Database.

#### Field: Number of Cells in Parallel

"Parallel_Cells" in Sandia Database

#### Field: Short-Circuit Current

"Isco" in Sandia Database. Short-circuit current is a basic parameter Typically supplied by manufacturers at Standard Report Conditions. (Amps)

#### Field: Open-Circuit Voltage

"Voco" in Sandia Database,  Open-circuit voltage is a basic parameter typically provided by manufacturers a t Standard Report Conditions. (Volts)

#### Field: Current at Maximum Power Point

"Impo" in Sandia Database. Current at maximum powerpoint is a basic parameter provided by manufacturers at Standard Report Conditions. (Amps)

#### Field: Voltage at Maximum PowerPoint

"Vmpo" in Sandia Database. Voltage at maximum powerpoint is a a basic parameter typically provided by manufacturers a t Standard Report Conditions. (Volts)

#### Field: Sandia Database Parameter aIsc

"aIsc in Sandia Database"   (1/degC)

#### Field: Sandia Database Parameter  aImp

"aImp" in Sandia Database  (1/degC)

#### Field: Sandia Database Parameter c0

"C0" in Sandia Database,    (non-dimensional)

#### Field: Sandia Database Parameter c1

"C1" in Sandia Database     (non-dimensional)

#### Field: Sandia Database Parameter Bvoc0

"Bvoco" in Sandia Database       (Volts/degC)

#### Field: Sandia Database Parameter mBVoc

"mBVoc"  in Sandia Database     (Volts/degC)

#### Field: Sandia Database Parameter BVmp0

"Bvmpo" in Sandia Database        (Volts/degC)

#### Field: Sandia Database Parameter mBVmp

"mBVmp" in Sandia Database       (Volts/degC)

#### Field:Diode Factor

"n" in Sandia Database    (non-dimensional)

#### Field: Sandia Database Parameter c2

"C2" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter c3

"C3" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter a0

"A0" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter a1

"A1" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter  a2

"A2" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter a3

"A3" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter a4

"A4" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter b0

"B0" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter b1

"B1" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter b2

"B2" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter b3

"B3" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter b4

"B4" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter b5

"B5" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter Delta(TC)

"d(Tc)" in Sandia Database   (deg C)

#### Field: Sandia Database Parameter fd

"fd" in Sandia Database  (non-dimensional)

#### Field: Sandia Database Parameter a

"a" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter b

"b" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter c4

"C4" in Sandia Database (non-dimensional)

#### Field: Sandia Database Parameter c5

"C5" in Sandia Database (non-dimensional)

#### Field: Sandia Database Parameter Ix0

"Ix0" in Sandia Database (Amps)

#### Field: Sandia Database Parameter Ixx0

"Ixx0" in Sandia Database (Amps)

#### Field: Sandia Database Parameter c6

"C6" in Sandia Database   (non-dimensional)

#### Field: Sandia Database Parameter c7

"C7" in Sandia Database   (non-dimensional)

An example of the object follows:

~~~~~~~~~~~~~~~~~~~~

    PhotovoltaicPerformance:Sandia,
       ASE-50-ATF-17_45, ! Module Name (production Name)
       0.43, ! field Active Area Acoll {m2}, single module
       36, ! NcellSer {unitless}
       1, ! NparSerCells
       2.90, ! Isc0 {Amps}
       20.70, ! Voc0 {Volts}
       2.65, ! Imp0 {Amps}
       17.00, ! Vmp0 {Volts}
       7.8e-04, ! aIsc {1/degC}
       1.0e-04, ! aImp {1/degC}
       0.99, ! C0 {unitless}
       3.0e-03, ! C1 {unitless}
       -0.07, ! BVoc0 {Volts/degC}
       0.00, ! mBVoc {Volts/degC}
       -0.07, ! BVmp0 {Volts/degC}
       0.00, ! mBVmp {Volts/degC}
       1.29, ! Diode Factor (n) {Unitless}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

       0.20, ! C2 {Unitless}
       -8.60, ! C3 {Unitless}
       0.93, ! A0 {Unitless}
       0.05, ! A1 {Unitless}
       -7.9402e-03, ! A2 {Unitless}
       5.2228e-04, ! A3 {Unitless}
       -1.3142e-05, ! A4 {Unitless}
       1.00, ! B0 {Unitless}
       -2.438e-03, ! B1 {Unitless}
       3.103e-04, ! B2 {Unitless}
       -1.246e-05, ! B3 {Unitless}
       2.112e-07, ! B4 {Unitless}
       -1.359e-09, ! B5 {Unitless}
       3.00, ! dT0 {degC}
       1.00, ! fd {Unitless}
       -3.47, ! a {Unitless}
       -0.05, ! b {Unitless}
       0.98, ! C4 {Unitless}
       0.01, ! C5 {Unitless}
       2.86, ! Ix0 {Amps}
       1.85, ! Ixx0 {Amps}
       1.13, ! C6 {Unitless}
       -0.13, ! C7 {Unitless}
       -1.359e-09, ! B5 {Unitless}
       3.00, ! dT0 {degC}
       1.00; ! fd {Unitless}
~~~~~~~~~~~~~~~~~~~~

When using EpMacro with an typical installation of EnergyPlus, the above entry can be included in an input file by adding the following two lines to an input macro file (\*.imf).

~~~~~~~~~~~~~~~~~~~~

    ##include C:\EnergyPlus\MacroDataSets\SandiaPVdata.imfASE-50-ATF-17_45[]
~~~~~~~~~~~~~~~~~~~~

## Generator:WindTurbine

A wind turbine is a component that converts the kinetic energy of the surrounding airstream into electricity. This model is intended to calculate the electrical power that a wind turbine system produces. The performance of wind turbine systems is dependent on the local environmental conditions such as wind speed and density of air at the height of the systems. An analysis of these conditions is necessary to accurately estimate power output. The model obtains the weather information from the weather data file in EnergyPlus and then determines the wind speed and air density at the specific height of the system. It also requires the user to input the annual average wind speed measured at the local site and the height of the measurement so that it factors in differences between the weather file wind data and the local wind data.

The model employs the general kinetic energy equation to calculate the performance characteristics of the horizontal axis wind turbine (HAWT) systems. It provides a simple approximation algorithm when the power coefficient, Cp, is available which represents the efficiency of the wind turbine in the wind power extraction from the ambient air stream. It also allows the user to input experimental constants so that the power coefficient can precisely be determined according to the characteristic of the airfoil of the system. As for the vertical axis wind turbine (VAWT) systems, it employs the general mathematical equations for straight-bladed Darrieus-type VAWT systems, which are common to VAWT systems. Various types of VAWT systems such as the Savonius-type and the curved-blade (or Egg-beater) type may be simulated with this same model.

It includes two different types of dynamic power control: FixedSpeedFixedPitch (FSFP) and VariableSpeedFixedPitch (VSFP). Currently, it does not include an algorithm for modeling pitch control such as FixedSpeedVariablePitch (FSVP) and VariableSpeedVariablePitch (VSVP). If the control type of the wind turbine is either FSVP or VSVP, the control type of VSFP will be assumed. In addition, constant power generation is assumed when the ambient wind speed is between the rated wind speed and the cut out wind speed, if the user specifies one of the last three options. The model also has the ability to account for transient losses associate with the power produced during dynamic control by a user-specified fraction.

The model does not include detailed algorithms for generators and inverters due to concerns for computational convergence, time, and usability. Instead, all conversion losses of these subsystems are included by applying a user-supplied total system efficiency to the maximum power extraction of the wind turbine. The field of the total system efficiency must be specified by the user.

### Inputs

#### Field: Name

A unique user assigned name for a particular wind turbine system. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the wind turbine system can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit is off and will not operate for the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Rotor Type

This field is the type of axis of the wind turbine. The user specifies either a horizontal axis wind turbine or a vertical axis wind turbine. Each type of wind turbine employs a different algorithm for the calculation of the electrical power output of the wind turbine. The default value is HorizontalAxisWindTurbine.

#### Field: Power Control

This field is the type of rotor control for the wind turbine. This protects the system against the overloading for a system with no speed or pitch control and also to maximize the energy yield for the system. Four different control types are classified in the literature: FixedSpeedFixedPitch (FSFP), FixedSpeedVariablePitch (FSVP), VariableSpeedFixedPitch (VSFP), and VariableSpeedVariablePitch (VSVP). Currently, FSFP and VSFP types can be modeled in EnergyPlus. The other two types will be modeled as VSFP. If the first FSFP control type is chosen, the model assumes the maximum power at a fixed rotor speed when the power output predicted is greater than the maximum until the rotor speed reaches the maximum wind speed (see next field). If one of the last three control options is chosen, the model assumes that the system produces a constant power at the rated wind speed when the wind speed is between the rated wind speed and cut-out wind speed. The default value is VariableSpeedFixedPitch (VSFP).

#### Field: Rated Rotor Speed

This field is the maximum rotational speed of the rotor at the rated power of the wind turbine in rev/min (revolution per minute). It is used to determine the tip speed ratio of the rotor and relative flow velocity incident on a single blade of the VAWT systems.

#### Field: Rotor Diameter

This field is the diameter of the rotor in meters. Note that this field is not the height of the blade, but the diameter of the perpendicular circle from the vertical pole in the VAWT systems. It determines the swept area of the rotor of the HAWT systems and the chordal velocity of the VAWT systems.

#### Field: Overall Height

This field is the height of the hub of the HAWT system, or of the pole of the VAWT system in meters. It is necessary to estimate local air density and the wind speed at this particular height where the wind turbine system is installed.

#### Field: Number of Blades

This field is the number of blades of the wind turbine. The azimuth angle of the rotor of the VAWT system is determined by dividing 360 degree by this field so that the model determines the chordal velocity component and the normal velocity component of the system. The default value is 3.

#### Field: Rated Power

This field is the nominal power output of the wind turbine system at the rated wind speed in Watts. Note that the user should input the maximum power of the system with no control, i.e., FSFP control type, can physically produce. Manufacturers' data also indicates it as "peak power" or "rated capacity". If the local wind speed is greater than the rated wind speed, the model assumes constant power output of this field.

#### Field: Rated Wind Speed

This field is the wind speed that the wind turbine system indicates the peak in the power curve in m/s. The system produces the maximum power at this speed and the speed of the rotor is managed based on this wind speed.

#### Field: Cut In Wind Speed

This field is the lowest wind speed where the wind turbine system can be operated in m/s. No power generation is achieved as long as the ambient wind speed is lower than this speed.

#### Field: Cut Out Wind Speed

This field is the greatest wind speed in m/s. When the wind speed exceeds this value, the wind turbine system needs to be stopped because of inefficiencies in the system. All systems that have either pitch or speed control must be stopped when the ambient wind speed exceeds this speed. Note that the user should input a wind speed above which physical damage to the system might be caused in the case of a FSFP system. It appears as "extreme/survival/design wind speed" in the literature. The system will be turned off when the ambient wind speed is over this speed.

#### Field: Fraction System Efficiency

This field is the overall system efficiency of the wind turbine system. It includes all the conversion losses as well as transient losses during the dynamic control when the ambient wind speed is between the rated wind speed and cut-out wind speed (see previous fields). The user also has the ability to specify delivery losses from the system to the local area. If the user does not enter a fraction, the model assumes the default value of 0.835. Note that the fraction must be between zero and one.

#### Field: Maximum Tip Speed Ratio

This field is the maximum tip speed ratio between the rotor velocity and ambient wind velocity. The rotor speed varies with this ratio to maximize the power output when the rotor control types are variable speed ones. This field allows the user to adjust the power output from the particular system or to find the optimal tip speed ratio of the system. Optimal tip speed ratio is dependent on the number of blades. It is typically about 6, 5, and 3 for two-bladed, three-bladed, and four-bladed rotor, respectively.  For the vertical axis wind turbine, it is smaller than horizontal axis wind turbine, and varies with the chord area. The default and maximum values are 5.0 and 12.0.

#### Field: Maximum Power Coefficient

This field is the maximum fraction of power extraction from ambient wind. If the user inputs this field, the simple approximation model is assumed. The model simply employs the value of this field into the general kinetic energy equation, so that the power output is obtained. The user can obtain this field with a simple calculation from the power curve published in almost all manufacturers' specifications by using the kinetic energy equation as:

![](media/image561.png)\


where

*P* = power production at the rated wind speed [W]

*ρ* = density of air [kg/m3]

*A* = swept area of rotor [m2]

*V* = rated wind speed [m/s]

*C~p~*~~= power coefficient

The maximum and default values are 0.59 and 0.35.

#### Field: Annual Local Average Wind Speed

This field is the local annual average wind speed that represents a representative wind profile at the location of the system in m/s. It is used to factor the difference in wind speed between the weather file wind data and the locally measured wind data so that the model minimizes uncertainties caused by improper wind data at the particular location. Considerable differences between the weather file wind data and the local wind data typically appear. The user thus needs to enter this field in order to obtain accurate local wind data. The model internally determines a multiplier and it is multiplied by the weather file wind data adjusted at the height of the system. If this field is not entered, then the model will use the wind speed from the design day or weather file information with only adjustment at the height of the rotor.

#### Field: Height for Local Average Wind Speed

This field is the height that the local wind speed is measured in meters. The annual average wind speed (see previous field) input by the user is internally recalculated by existing EnergyPlus functions at the height of the local station. This modified wind speed is then factored and applied to the weather file wind data. If the annual local average wind speed is not entered, this field is then assumed as zero. The minimum and default values are zero and 50 meters.

#### Field: Blade Chord Area

This field is the blade chord area of a single blade of VAWT system in m2. It is necessary to determine the net tangential and normal forces of a single blade.

#### Field: Blade Drag Coefficient

This field is the blade drag coefficient for a specific blade. It is for determining the tangential and normal force coefficients with the blade lift coefficient (see next field) so that the model can calculate the power output from the system. The user should be able to obtain this parameter for a specific blade from the manufacturers' data. This field is only valid for VAWT systems.

#### Field: Blade Lift Coefficient

This field is the blade lift coefficient for a specific blade. It is for determining the tangential and normal force coefficients with the blade drag coefficient (see previous field) so that the model can calculate the power output from the system. The user should also be able to obtain it for a specific blade from the manufacturers' data. This field is only valid for VAWT systems.

#### Field: Power Coefficient Parameter <x>

These six fields are the parameters for the power coefficient equation shown below. These fields are used to determine the power coefficient of the system. The analytical approximation model of the power coefficient in EnergyPlus is:

![](media/image562.png)\


with

![](media/image563.png)\


where

*C~p~*     = power coefficient

C*~1- 6~*  = empirical power coefficient parameters

*λ*        = tip speed ratio (often known as TSR)

*λi*       = tip speed ratio at ith time step

*θ*       = azimuth angle of the pitch, 0 [degree]

If the user does not input any field of these parameters, the simple approximation model for the power coefficient will be used (see previous maximum power coefficient field). That is, the analytical approximation model of the power coefficient will be chosen only if the user inputs all these six parameters. The user also has the ability to modify each parameter when the specific value of the system is available. The default values are given in the table below.

C~1~|C~2~|C~3~|C~4~|C~5~|C~6~
----|----|----|----|----|----
0.5176|116|0.4|0.0|5.0|21

These fields are only valid for HAWT systems.

An example input for the wind turbine is shown in below.

~~~~~~~~~~~~~~~~~~~~

    Generator:WindTurbine,
    WT1,           ! Name
    WT Schedule,   ! Availability Schedule Name
    HorizontalAxisWindTurbine,   ! Rotor type
    FixedSpeedVariablePitch,     ! Power control type
    41,            ! Maximum rotational speed of the rotor {rev/min}
    19.2,          ! Diameter of the rotor {m}
    30.5,          ! Overall height of the system {m}
    3,             ! Number of blades
    55000,         ! Rated power output at the rated wind speed {W}
    11,            ! Rated wind speed {m/s}
    3.5,           ! Cut In wind speed {m/s}
    25,            ! Cut Out wind speed {m/s}
    0.835,         ! Overall wind turbine system efficiency
    8,             ! Maximum tip speed ratio
    0.4,           ! Maximum power coefficient
    6.4,           ! Annual local wind speed {m/s}
    50,            ! Height of local meteorological station {m}
    ,              ! Blade chord area {m2}
    ,              ! Blade drag coefficient
    ,              ! Blade lift coefficient
    0.5176,        ! Power Coefficient C1
    116,           ! Power Coefficient C2
    0.4,           ! Power Coefficient C3
    0,             ! Power Coefficient C4
    5,             ! Power Coefficient C5
    21;            ! Power Coefficient C6
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Generator Produced Electric Power [W]
    HVAC,Sum, Generator Produced Electric Energy [J]
    HVAC,Average, Generator Turbine Local Wind Speed [m/s]
    HVAC,Average, Generator Turbine Local Air Density [kg/m3]
    HVAC,Average, Generator Turbine Power Coefficient []
    HVAC,Average, Generator Turbine Tip Speed Ratio []
    HVAC,Average, Generator Turbine Chordal Component Velocity [m/s]
    HVAC,Average, Generator Turbine Normal Component Velocity [m/s]
    HVAC,Average, Generator Turbine Relative Flow Velocity [m/s]
    HVAC,Average, Generator Turbine Attack Angle [deg]
~~~~~~~~~~~~~~~~~~~~

#### Generator Produced Electric Power [W]

This report is the electric power that the wind turbine system produces.

#### Generator Produced Electric Energy [J]

This report is the electric energy that the wind turbine system produces.

#### Generator Turbine Local Wind Speed [m/s]

This report is the local wind speed at the specific height of the wind turbine.

#### Generator Turbine Local Air Density [kg/m3]

This report is the local density of the air at the specific height of the wind turbine.

#### Generator Turbine Tip Speed Ratio []

This report is the ratio between the rotational speed of the tip of the blades and the ambient wind speed at the height of the hub or pole of the wind turbine.

#### Generator Turbine Power Coefficient []

This report represents the efficiency of the power extraction from the ambient wind of the wind turbine. It is function of the tip speed ratio and pitch angle. It is only valid for HAWT systems.

#### Generator Turbine Chordal Component Velocity [m/s]

This report is the axial velocity component along the chord of the wind turbine system. It is only valid for VAWT systems.

#### Generator Turbine Normal Component Velocity [m/s]

This report is the axial velocity component of the rotor of the wind turbine system. It is only valid for VAWT systems.

#### Generator Turbine Relative Flow Velocity [m/s]

This report is the local relative flow velocity that represents actual direction and velocity incident on the blades. It is the square root of the sum of both chordal velocity component and normal velocity component. It is only valid for VAWT systems.

#### Generator Turbine Attack Angle [deg]

This report is the azimuth angle between the relative flow velocity and the plane of chord. It varies as the wind speed increases, so that the lift and drag forces change.
