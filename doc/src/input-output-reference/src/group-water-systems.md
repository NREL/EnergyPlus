# Group – Water Systems

This group of objects is used to describe the water systems in the building. EnergyPlus offers water modeling capabilities than include not only hot water systems but also the overall water systems including cold water uses, on-site water collection (e.g., from rain, condensate, or well), and storage. HVAC components may consume or collect a significant portion of the water making it appropriate to include water modeling in EnergyPlus.

All water systems are collected into an end use category called "Water Systems". This category includes hot and cold water. Water consumed by an HVAC component will be assigned to the end use category for that component. End use subcategories are available to separate hot water from cold water if desired. Simple hot water systems can be configured as stand-alone or use a [PlantLoop](#plantloop). The Water Systems can be connected to PlantLoops for modeling heated water end uses. However PlantLoops are not used to model cold water and the WaterUse:Storages portions of the water system. Cold water systems are connected together using [WaterUse:Storage](#waterusestorage) objects. [WaterUse:Connections](#wateruseconnections) are used at the zone level to connect to both cold and hot water.

In addition to the input objects described in this section below, there are a variety of HVAC components that can be configured to interact with the water systems. These component input objects include optional fields that allow describing how the water systems are connected to them by providing the name of a storage tank. The following table lists EnergyPlus input objects that include provisions for connecting to the water systems but are described elsewhere in this document:

Table: Water Systems Objects

**Input Object Name**|**Type of Water Interactions**
----------------------------------|-------------------------------------------
Site:Precipitation|Describes rainfall for WaterUse:RainCollector
Coil:Cooling:Water|Condensate collection to WaterUse:Storage
Coil:Cooling:Water:DetailedGeometry|Condensate collection to WaterUse:Storage
Coil:Cooling:DX:SingleSpeed|Optional evaporative condenser can be supplied by [WaterUse:Storage](#waterusestorage)|Condensate collection to WaterUse:Storage
Coil:Cooling:DX:TwoSpeed|Evaporative condenser supplied by [WaterUse:Storage](#waterusestorage)|Condensate collection to WaterUse:Storage
Coil:Cooling:DX:TwoStageWithHumidityControlMode|Evaporative condenser supplied by [WaterUse:Storage](#waterusestorage)|Condensate collection to WaterUse:Storage
Coil:Cooling:DX:MultiSpeed|Evaporative condenser supplied by [WaterUse:Storage](#waterusestorage)|Condensate collection to WaterUse:Storage
Humidifier:Steam:Electric|Water supplied by WaterUse:Storage
CoolingTower:SingleSpeed|Water supplied by WaterUse:Storage
CoolingTower:TwoSpeed|Water supplied by WaterUse:Storage
CoolingTower:VariableSpeed|Water supplied by WaterUse:Storage
ZoneCoolTower:Shower|Water supplied by WaterUse:Storage
[Refrigeration:CompressorRack](#refrigerationcompressorrack) |Water supplied by WaterUse:Storage
Refrigeration:Condenser:EvaporativeCooled|Water supplied by WaterUse:Storage
EvaporativeCooler:Direct:CelDekPad|Evaporation water supplied by WaterUse:Storage
EvaporativeCooler:Indirect:CelDekPad|Evaporation water supplied by WaterUse:Storage
EvaporativeCooler:Indirect:WetCoil|Evaporation water supplied by WaterUse:Storage
EvaporativeCooler:Indirect:ResearchSpecial|Evaporation water supplied by WaterUse:Storage

## WaterUse:Equipment

The [WaterUse:Equipment](#wateruseequipment) object is a generalized object for simulating all water end uses. Hot and cold water uses are included, as well as controlled mixing of hot and cold water at the tap. The [WaterUse:Equipment](#wateruseequipment) object can be used stand-alone, or coupled into a plant loop using the [WaterUse:Connections](#wateruseconnections) object (see below). The [WaterUse:Connections](#wateruseconnections) object allows water uses to be linked to [WaterUse:Storage](#waterusestorage) objects to store and draw reclaimed water. The [WaterUse:Connections](#wateruseconnections) object can also simulate drainwater heat recovery.

The [WaterUse:Equipment](#wateruseequipment) object serves all of the same purposes as the existing objects:  [Exterior:WaterEquipment](#exteriorwaterequipment), and [HotWaterEquipment](#hotwaterequipment).

The [WaterUse:Equipment](#wateruseequipment) object does a better job of modeling domestic hot water by allowing mixing at the tap in order to account for both hot **and cold** water used at sinks, showers, etc. It also improves on the stand-alone energy modeling of domestic hot water. Because all of the temperatures and flow rates can be solved, the energy usage can be accounted for as "Purchased Heating".

In either stand-alone mode or used with the [WaterUse:Connections](#wateruseconnections) object, the [WaterUse:Equipment](#wateruseequipment) object will attempt to meet a scheduled target temperature at the tap by mixing hot and cold water flows. If the hot water flow is not hot enough, or if the flow is limited by plant loop constraints, the result is a cooler mixed water temperature and the target temperature will not be met. In stand-alone mode the hot and cold water temperatures are specified by schedules. However, if the cold water schedule is omitted, the water temperature will be calculated by the [Site:WaterMainsTemperature](#sitewatermainstemperature) object. If the hot water schedule or target temperature schedule are omitted, all water is delivered as cold water. If only using cold water, the [WaterUse:Equipment](#wateruseequipment) object is similar to the [Exterior:WaterEquipment](#exteriorwaterequipment) object.

The [WaterUse:Equipment](#wateruseequipment) object improves on the internal gains object [HotWaterEquipment](#hotwaterequipment) by associating actual water usage with a sensible and latent gain to a zone. Equipment, such as showers, that generate a significant heat gain can be modeled using the Sensible Fraction Schedule and Latent Fraction Schedule fields. The schedules are used to set the fraction of the maximum possible heat gain (based on inlet water conditions and ambient zone conditions) that should be added to the zone. The split between sensible and latent will vary depending on the type of equipment that is to be modeled. Typically, both fractions should be small numbers.

### Inputs

#### Field: Name

The unique object name for reference by other objects.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Laundry", "Dish Washing", etc. A new meter for reporting is created for each unique subcategory (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table under the "Water Systems" end-use category. If this field is omitted or blank, the water use will be assigned to the "General" end-use subcategory.

#### Field: Peak Flow Rate

The peak demanded hot water flow rate [m^3^/s]. This value is multiplied by the Flow Rate Fraction Schedule (below) to determine the actual volumetric flow rate.

#### Field: Flow Rate Fraction Schedule Name

Reference to the schedule object specifying the flow rate fraction relative to the Peak Flow Rate (above). If blank, the schedule defaults to 1.0 at all times.

#### Field: Target Temperature Schedule Name

Reference to the schedule object specifying the target water temperature [C]. Hot and cold water are mixed at the tap to attain the target temperature. If insufficient hot water is available to reach the target temperature, the result is cooler water at the tap. If blank, the target temperature defaults to the hot water supply temperature.

#### Field: Hot Water Supply Temperature Schedule Name

Reference to the schedule object specifying the hot water temperature [C]. The hot water temperature is used to calculate the "Purchased Heating" energy usage in stand-alone mode. If blank in stand-alone mode, the hot water supply temperature defaults to the cold water supply temperature. This field is ignored if the object is used with the [WaterUse:Connections](#wateruseconnections) object.

#### Field: Cold Water Supply Temperature Schedule Name

Reference to the Schedule object specifying the cold water temperature [C] from the supply mains that provides the cold water to the tap and makes up for all water lost down the drain. If blank, water temperatures are calculated by the [Site:WaterMainsTemperature](#sitewatermainstemperature) object. This field is ignored if the object is used with the [WaterUse:Connections](#wateruseconnections) object.

#### Field: Zone Name

Reference to the zone name where the water equipment will be considered as latent load to the zone.

#### Field: Sensible Fraction Schedule Name

Reference to the schedule object specifying the fraction of the maximum possible sensible heat gain (based on inlet water conditions and ambient zone conditions) that is added to the zone. If blank, the schedule defaults to 0 at all times.

#### Field: Latent Fraction Schedule Name

Reference to the schedule object specifying the fraction of the maximum possible latent heat gain (based on inlet water conditions and ambient zone conditions) that is added to the zone. If blank, the schedule defaults to 0 at all times.

IDF examples:

~~~~~~~~~~~~~~~~~~~~

      WaterUse:Equipment,
      Landscaping,  !- Name
      Landscaping,  !- End-Use Subcategory
      0.003;        !- Peak Flow Rate {m3/s}

      WaterUse:Equipment,
      Dishwashing,  !- Name
      Domestic Hot Water,  !- End-Use Subcategory
      0.001,  !- Peak Flow Rate {m3/s}
      Dishwashing Schedule,  !- Flow Rate Fraction Schedule Name
      Dishwashing Target Temp,  !- Target Temperature Schedule Name
      Hot Water Temp,  !- Hot Water Supply Temperature Schedule Name
      Mains Water;  !- Cold Water Supply Temperature Schedule Name

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      WaterUse:Equipment,
      Showers,  !- Name
      Domestic Hot Water,  !- End-Use Subcategory
      0.0002,  !- Peak Flow Rate {m3/s}
      Shower Schedule,  !- Flow Rate Fraction Schedule Name
      Shower Target Temp,  !- Target Temperature Schedule Name
      Hot Water Temp,  !- Hot Water Supply Temperature Schedule Name
      ,  !- Cold Water Supply Temperature Schedule Name
      Shower Room,  !- Zone Name
      Sensible Frac Schedule,  !- Sensible Fraction Schedule Name
      Latent Frac Schedule;  !- Latent Fraction Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The following output variables are reported for the [WaterUse:Equipment](#wateruseequipment) object:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Water Use Equipment Hot Water Mass Flow Rate [kg/s]
    HVAC,Average,Water Use Equipment Cold Water Mass Flow Rate [kg/s]
    HVAC,Average,Water Use Equipment Total Mass Flow Rate [kg/s]
    HVAC,Average,Water Use Equipment Hot Water Volume Flow Rate [m3/s]
    HVAC,Average,Water Use Equipment Cold Water Volume Flow Rate [m3/s]
    HVAC,Average,Water Use Equipment Total Volume Flow Rate [m3/s]
    HVAC,Sum,Water Use Equipment Hot Water Volume [m3]
    HVAC,Sum,Water Use Equipment Cold Water Volume [m3]
    HVAC,Sum,Water Use Equipment Total Volume [m3]
    HVAC,Sum,Water Use Equipment Mains Water Volume [m3]
    HVAC,Average,Water Use Equipment Hot Water Temperature [C]
    HVAC,Average,Water Use Equipment Cold Water Temperature [C]
    HVAC,Average,Water Use Equipment Target Water Temperature [C]
    HVAC,Average,Water Use Equipment Mixed Water Temperature [C]
    HVAC,Average,Water Use Equipment Drain Water Temperature [C]
    HVAC,Average,Water Use Equipment Heating Rate [W]
    HVAC,Sum,Water Use Equipment Heating Energy [J]
    HVAC,Average,Water Use Equipment Zone Sensible Heat Gain Rate [W]
    HVAC,Sum,Water Use Equipment Zone Sensible Heat Gain Energy [J]
    HVAC,Average,Water Use Equipment Zone Latent Gain Rate [W]
    HVAC,Sum,Water Use Equipment Zone Latent Gain Energy [J]
    HVAC,Average,Water Use Equipment Zone Moisture Gain Mass Flow Rate [kg/s]
    HVAC,Sum,Water Use Equipment Zone Moisture Gain Mass [kg]
~~~~~~~~~~~~~~~~~~~~

#### Water Use Equipment Hot Water Mass Flow Rate [kg/s]

The mass flow rate of hot water supplied to the equipment, in units of kilograms per second (kg/s).

#### Water Use Equipment Cold Water Mass Flow Rate [kg/s]

The mass flow rate of cold water supplied to the equipment, in units of kilograms per second (kg/s).

#### Water Use Equipment Total Mass Flow Rate [kg/s]

The total mass flow rate of water (hot + cold) supplied to the equipment, in units of kilograms per second (kg/s).

#### Water Use Equipment Hot Water Volume Flow Rate [m3/s]

The volumetric flow rate of hot water supplied to the equipment and consumed down the drain, in units of cubic meters per second (m^3^/s).

#### Water Use Equipment Cold Water Volume Flow Rate [m3/s]

The volumetric flow rate of cold water supplied to the equipment and consumed down the drain, in units of cubic meters per second (m^3^/s).

#### Water Use Equipment Total Volume Flow Rate [m3/s]

The total volumetric flow rate of water (hot + cold) supplied to the equipment and consumed down the drain, in units of cubic meters per second (m^3^/s).

#### Water Use Equipment Hot Water Volume [m3]

The volume of hot water supplied to the equipment and consumed down the drain, in units of cubic meters (m^3^).

#### Water Use Equipment Cold Water Volume [m3]

The volume of cold water supplied to the equipment and consumed down the drain, in units of cubic meters (m^3^).

#### Water Use Equipment Total Volume [m3]

The total volume of water (hot+cold) supplied to the equipment and consumed down the drain, in units of cubic meters (m^3^). This output is also added to a meter with Resource Type = Water, End Use Key = WaterSystems, and Group Key = Plant. The sub-categories may be entered as any type (e.g., Laundry, Dish Washing, etc. defined in [WaterUse:Equipment](#wateruseequipment) object input for End-Use Subcategory) with General being the default sub-category (ref. Output Meter)

#### Water Use Equipment Mains Water Volume [m3]

The total volume of water (hot+cold) supplied to the equipment and consumed down the drain, in units of cubic meters (m^3^). This output is also added to a meter with Resource Type = MainsWater, End Use Key = WaterSystems, and Group Key = Plant. The sub-categories may be entered as any type (e.g., Laundry, Dish Washing, etc. defined in [WaterUse:Equipment](#wateruseequipment) object input for End-Use Subcategory) with General being the default sub-category (ref. Output Meter)

#### Water Use Equipment Hot Water Temperature [C]

The temperature of hot water supplied to the equipment, in units of degrees Celsius (C).

#### Water Use Equipment Cold Water Temperature [C]

The temperature of cold water supplied to the equipment, in units of degrees Celsius (C).

#### Water Use Equipment Target Water Temperature [C]

The user-specified target temperature for the mixing of hot and cold supply flows, in units of degrees Celsius (C).

#### Water Use Equipment Mixed Water Temperature [C]

The actual mixed water temperature possible with the available hot and cold temperatures and flow rates, in units of degrees Celsius (C).

#### Water Use Equipment Drain Water Temperature [C]

The water temperature at the drain equal to the mixed water temperature minus any heat loss to the zone, in units of degrees Celsius (C).

#### Water Use Equipment Heating Rate [W]

The heating rate defined by the hot water flow and the temperature difference between the hot water supply and the cold makeup return water, in units of watts (W).

#### Water Use Equipment Heating Energy [J]

The heating energy accumulated by the heating rate above, in units of Joules (J). This output is also added to a meter with End Use Key = WaterSystems, Group Key = Plant. If both of the node names specified in the [WaterUse:Equipment](#wateruseequipment) object are blank, then this output is reported with Resource Type = [DistrictHeating](#districtheating). If either of the node names are entered, then this output is reported with Resource Type = EnergyTransfer. The sub-categories may be entered as any type (e.g., Laundry, Dish Washing, etc. defined in [WaterUse:Equipment](#wateruseequipment) object input for End-Use Subcategory) with General being the default sub-category (ref. Output Meter)

#### Water Use Equipment Zone Sensible Heat Gain Rate [W]

The sensible heat rate to the zone due to the water flow being exposed to zone air, in units of watts (W).

#### Water Use Equipment Zone Sensible Heat Gain Energy [J]

The sensible heat energy accumulated by the latent heat rate above, in units of Joules (J).

#### Water Use Equipment Zone Latent Gain Rate [W]

The latent heat rate to the zone due to the water flow being exposed to zone air, in units of watts (W).

#### Water Use Equipment Zone Latent Gain Energy [J]

The latent heat energy accumulated by the latent heat rate above, in units of Joules (J).

#### Water Use Equipment Zone Moisture Gain Mass Flow Rate [kg/s]

The moisture rate of evaporation to the zone due to the water flow being exposed to zone air, in units of kilograms per second (kg/s).

#### Water Use Equipment Zone Moisture Gain Mass [kg]

The moisture mass accumulated by the moisture rate above, in units of kilograms (kg).

## WaterUse:Connections

The [WaterUse:Connections](#wateruseconnections) object can be thought of as a subsystem that groups together multiple [WaterUse:Equipment](#wateruseequipment) components. As its name suggests, the object provides connections that are shared by these components, including:

- Inlet node and outlet node connections to a plant loop
- Connections to [WaterUse:Storage](#waterusestorage) objects to store and draw reclaimed water
- Internal connections to simulate drainwater heat recovery.

![Diagram of internal connections for WaterUse:Connections](media/diagram-of-internal-connections-for-wateruse.png)


The [WaterUse:Connections](#wateruseconnections) object can be used stand-alone or coupled into a plant loop. In stand-alone mode, the Hot and Cold Water Supply Temperature Schedules override the values for the listed [WaterUse:Equipment](#wateruseequipment) objects. When coupled to the plant, the Hot Water Supply Temperature Schedule is overridden by the actual plant loop inlet water temperature. When coupled to the plant, the [WaterUse:Connections](#wateruseconnections) object should be on a [Branch](#branch) object whose control type is set to "Active."

### Inputs

#### Field: Name

The unique object name for reference by other objects.

#### Field: Inlet Node Name

The hot water inlet node when connected to a plant loop. The hot water supplies all of the [WaterUse:Equipment](#wateruseequipment) objects that demand hot water. Insufficient supply temperature or flow results in cooler water at the tap.

#### Field: Outlet Node Name

The cold water outlet node, i.e. the cold water supply mains, when connected to a plant loop. The return node provides make-up water for the hot water lost down the drain.

#### Field: Supply Water Storage Tank Name

Reference to the [WaterUse:Storage](#waterusestorage) object that supplies cold water to the listed [WaterUse:Equipment](#wateruseequipment) objects. If the field is blank, or the tank is empty, fresh water is supplied from the water mains.

#### Field: Reclamation Water Storage Tank Name

Reference to the [WaterUse:Storage](#waterusestorage) object that stores the resulting graywater from the listed [WaterUse:Equipment](#wateruseequipment) objects. If the field is blank, the graywater is not reclaimed.

#### Field: Hot Water Supply Temperature Schedule Name

Reference to the schedule object specifying the hot water temperature [C]. If blank in stand-alone mode, the hot water supply temperature defaults to the cold water supply temperature. This field (even if blank) overrides the Hot Water Supply Temperature Schedule in all of the listed [WaterUse:Equipment](#wateruseequipment) objects.

#### Field: Cold Water Supply Temperature Schedule Name

Reference to the schedule object specifying the cold water temperature [C] from the supply mains that provides the cold water to the tap and makes up for all water lost down the drain. If blank, water temperatures are calculated by the [Site:WaterMainsTemperature](#sitewatermainstemperature) object.

This field (even if blank) overrides the Cold Water Supply Temperature Schedule in all of the listed [WaterUse:Equipment](#wateruseequipment) objects.

#### Field: Drain Water Heat Exchanger Type

The heat exchanger type to be used for drainwater heat recovery. None indicates no heat recovery is to be simulated. Ideal sets a heat exchanger effectiveness of 1.0. Effectiveness is calculated dynamically for CounterFlow and CrossFlow options.

One popular type of drainwater heat exchanger is the Gravity-Film Heat Exchanger (GFX). The GFX is most closely approximated with the CounterFlow option, but keep in mind that the UA varies more with flow rate for the GFX than for most traditional heat exchangers.

#### Field: Drain Water Heat Exchanger Destination

The heat exchanger configuration to be used for drainwater heat recovery. The configuration determines where the heat will be used after it is recovered.

Plant indicates that the all of the recovered heat will be sent to the return flow at the outlet node of the [WaterUse:Connections](#wateruseconnections) object to preheat the make-up cold water from the water mains.

Equipment indicates that the all of the recovered heat will be used to preheat the cold water flow side of the [WaterUse:Equipment](#wateruseequipment) objects listed *in this* [WaterUse:Connections](#wateruseconnections) object

PlantAndEquipment indicates the recovered heat will be divided between the plant and the [WaterUse:Equipment](#wateruseequipment) objects, as described above. This is the only option where the flow rates are equal in the drain and the heat exchanger.

#### Field: Drain Water Heat Exchanger U-Factor Times Area

The UA is the heat transfer coefficient [W/K] for the heat exchanger and is the product of U, the overall heat transfer coefficient, and A, the heat exchanger surface area.

#### Field: Water Use Equipment 1-10 Name

References to [WaterUse:Equipment](#wateruseequipment) objects.

IDF examples:

[WaterUse:Connections](#wateruseconnections),

  Domestic Water Uses,  !- Name

  Water Eq Inlet Node,  !- Inlet Node Name

  Water Eq Outlet Node, !- Outlet Node Name

  Rainwater Storage,  !- Supply Water Storage Tank Name

  Graywater Storage,  !- Reclamation Water Storage Tank Name

  ,  !- Hot Water Supply Temperature Schedule Name

  ,  !- Cold Water Supply Temperature Schedule Name

  CounterFlow,  !- Drain Water Heat Exchanger Type

  Plant,  !- Drain Water Heat Exchanger Destination

  1500,  !- Drain Water Heat Exchanger U-Factor Times Area {W/K}

  Sinks,  !- Water Use Equipment 1 Name

  Showers,  !- Water Use Equipment 2 Name

  Dishwasher,  !-  Water Use Equipment 3 Name

  Clotheswasher;  !- Water Use Equipment 4 Name

### Outputs

The following output variables are reported for the [WaterUse:Connections](#wateruseconnections) object:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Water Use Connections Hot Water Mass Flow Rate [kg/s]
    HVAC,Average,Water Use Connections Cold Water Mass Flow Rate [kg/s]
    HVAC,Average,Water Use Connections Total Mass Flow Rate [kg/s]
    HVAC,Average,Water Use Connections Drain Water Mass Flow Rate [kg/s]
    HVAC,Average,Water Use Connections Heat Recovery Mass Flow Rate [kg/s]
    HVAC,Average,Water Use Connections Hot Water Volume Flow Rate [m3/s]
    HVAC,Average,Water Use Connections Cold Water Volume Flow Rate [m3/s]
    HVAC,Average,Water Use Connections Total Volume Flow Rate [m3/s]
    HVAC,Sum,Water Use Connections Hot Water Volume [m3]
    HVAC,Sum,Water Use Connections Cold Water Volume [m3]
    HVAC,Sum,Water Use Connections Total Volume [m3]
    HVAC,Average,Water Use Connections Hot Water Temperature [C]
    HVAC,Average,Water Use Connections Cold Water Temperature [C]
    HVAC,Average,Water Use Connections Drain Water Temperature [C]
    HVAC,Average,Water Use Connections Return Water Temperature [C]
    HVAC,Average,Water Use Connections Waste Water Temperature [C]
    HVAC,Average,Water Use Connections Heat Recovery Water Temperature [C]
    HVAC,Average,Water Use Connections Heat Recovery Effectiveness []
    HVAC,Average,Water Use Connections Heat Recovery Rate [W]
    HVAC,Sum,Water Use Connections Heat Recovery Energy [J]
    HVAC,Sum,Water Use Connections Plant Hot Water Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Water Use Connections Hot Water Mass Flow Rate [kg/s]

The mass flow rate of hot water supplied to all equipment, in units of kilograms per second (kg/s).

#### Water Use Connections Cold Water Mass Flow Rate [kg/s]

The mass flow rate of cold water supplied to all equipment, in units of kilograms per second (kg/s).

#### Water Use Connections Total Mass Flow Rate [kg/s]

The total mass flow rate of water (hot + cold) supplied to all equipment, in units of kilograms per second (kg/s).

#### Water Use Connections Drain Water Mass Flow Rate [kg/s]

The mass flow rate of drainwater from all equipment, in units of kilograms per second (kg/s).

#### Water Use Connections Heat Recovery Mass Flow Rate [kg/s]

The mass flow rate of make-up water in the heat exchanger, in units of kilograms per second (kg/s).

#### Water Use Connections Hot Water Volume Flow Rate [m3/s]

The volumetric flow rate of hot water supplied to all equipment and consumed down the drain, in units of cubic meters per second (m^3^/s).

#### Water Use Connections Cold Water Volume Flow Rate [m3/s]

The volumetric flow rate of cold water supplied to all equipment and consumed down the drain, in units of cubic meters per second (m^3^/s).

#### Water Use Connections Total Volume Flow Rate [m3/s]

The total volumetric flow rate of water (hot + cold) supplied to all equipment and consumed down the drain, in units of cubic meters per second (m^3^/s).

#### Water Use Connections Hot Water Volume [m3]

The volume of hot water supplied to all equipment and consumed down the drain, in units of cubic meters (m^3^).

#### Water Use Connections Cold Water Volume [m3]

The volume of cold water supplied to all equipment and consumed down the drain, in units of cubic meters (m^3^).

#### Water Use Connections Total Volume [m3]

The total volume of water (hot+cold) supplied to all equipment and consumed down the drain, in units of cubic meters (m^3^).

#### Water Use Connections Hot Water Temperature [C]

The temperature of hot water supplied to all equipment, in units of degrees Celsius (C).

#### Water Use Connections Cold Water Temperature [C]

The temperature of cold water supplied to all equipment, in units of degrees Celsius (C).

#### Water Use Connections Drain Water Temperature [C]

The water temperature at the drain equal to the mixed water temperature minus any heat losses to the zone, in units of degrees Celsius (C).

#### Water Use Connections Return Water Temperature [C]

The temperature of make-up water returned to the plant loop, in units of degrees Celsius (C).

#### Water Use Connections Waste Water Temperature [C]

The temperature of the water leaving the heat exchanger. If no heat exchanger, the waste water temperature equals the drain water temperature, in units of degrees Celsius (C).

#### Water Use Connections Heat Recovery Water Temperature [C]

The temperature of the water leaving the heat exchanger. If no heat exchanger, the waste water temperature equals the drain water temperature, in units of degrees Celsius (C).

#### Water Use Connections Heat Recovery Effectiveness []

The effectiveness value of the heat exchanger. The units are dimensionless.

#### Water Use Connections Heat Recovery Rate [W]

The heat recovered by the heat exchanger and used to preheat the cold make-up water, in units of watts (W).

#### Water Use Connections Heat Recovery Energy [J]

The energy recovered by the heat exchanger and used to preheat the cold make-up water, in units of Joules (J).

#### Water Use Connections Plant Hot Water Energy [J]

The plant loop energy consumed by the hot water used, in units of Joules (J). This output is also added to a meter with Resource Type = PlantLoopHeatingDemand, End Use Key = WaterSystems, and Group Key = Plant (ref. Output Meter).

## WaterUse:Storage

The [WaterUse:Storage](#waterusestorage) object is a central component for complex water systems. This object is not needed if the only use of water is directly from the mains, or utility service water. If the building model is to include any on-site collection or wells or storing and reuse of graywater, then a [WaterUse:Storage](#waterusestorage) is needed. Each [WaterUse:Storage](#waterusestorage) can serve as a central node and make connections to numerous sources of supply or numerous components with demand. If a maximum capacity is not specified, the tank is assumed to have unlimited capacity. This is useful for sizing.

Storage tanks may handle a lot of water but show no use. This is because water usage is metered at the component level were water is "used up."  The only water a storage tank might use up is overflow water if it is discarded.

### Inputs

#### Field: Name

The unique object name for reference by other objects.

#### Field: Water Quality Subcategory

Describes the quality of the water contained in the tank. Used for reporting and to check that the uses and supply match the category of water quality.

#### Field: Maximum Capacity

The maximum volumetric capacity [m3] of the water tank. If blank, this field defaults to unlimited capacity.

#### Field: Initial Volume

The volume of water in the storage tank at the beginning of each simulation environmental period [m3]. This provides a starting point for the amount of water in storage.

#### Field: Design In Flow Rate

The design flow rate [m3/s] of fittings that provide water into the tank from external sources. In a scenario where a heavy rain fall is being harvested, the actual reclamation rate might be limited by pipe size or filtration. If blank, this field defaults to unlimited rate.

#### Field: Design Out Flow Rate

The design flow rate [m3/s] of fitting that withdraw water from the tank to end uses. Heavy demand (landscaping?) might be limited by pipe size or filtration. If Blank, this field defaults to unlimited rate.

#### Field: Overflow Destination

Name of a second [WaterUse:Storage](#waterusestorage) that would receive overflow. Overflow could occur if the maximum capacity is reached or if the design in flow rate is exceeded. If left blank, then the overflow is discarded and lost from the water system.

#### Field: Type of Supply Controlled by Float Valve

The storage tank can include the capability of modeling a float valve that will call for water to be added to the tank. This field is used to select the type of system used to respondto fill requests made by a float valve. The available options are ‘None', ‘Mains', ‘GroundwaterWell', or ‘OtherTank.'  The float valve settings are described in the next two fields.

#### Field: Float Valve On Capacity

The volumetric capacity [m3] of the water tank when a floating valve would turn on to allow filling the tank.

#### Field: Float Valve Off Capacity

The volumetric capacity [m3] of the water tank when a floating valve would turn off after having been filling the tank.

#### Field: Backup Mains Capacity

The volumetric capacity of the tank that indicates where a secondary float valve will maintain the volume by calling for mains water. Used if the well or other tank cannot keep up with the needs of the main float valve. If left blank or equal 0.0, then there is no mains water backup. If specified, then mains water will be drawn once the storage tank reaches this level and then the mains water will fill all the way to capacity specified in the previous field.

#### Field: Other Tank Name

This field contains the name of another [WaterUse:Storage](#waterusestorage) defined elsewhere in the input file. This field is only used if the Type of Controlled Supply field is set to ‘OtherTank' and the current tank is to be configured to be supplied by a second tank in response to a float valve.

#### Field: Water Thermal Mode

Controls the method of determining the temperature of the water in the storage tank. The only available option at the current time is ScheduledTemperature.

#### Field: Water Temperature Schedule Name

Reference to the schedule object specifying the temperature [C] of the water in the tank. This takes the place of a full thermal model of the tank.

#### Field: Ambient Temperature Indicator

Reserved for future use. The field can be Schedule, [Zone](#zone), or Outdoors.

#### Field: Ambient Temperature Schedule Name

Reserved for future use. Schedule name for ambient temperature when previous field is Schedule.

#### Field: Zone Name

Reserved for future use. Name of zone where the tank is located.

#### Field: Tank Surface Area

Reserved for future zone thermal model.

#### Field: Tank U Value

Reserved for future zone thermal model.

#### Field: Tank Outside Surface Material Name

Reserved for future zone thermal model.

IDF examples:

~~~~~~~~~~~~~~~~~~~~

     WaterUse:Storage,
      Tower 2 water tank, !- Name
      Tower water make up Holding tank, !- Water Quality Subcategory
      30.0 , !- Maximum Capacity {m3}
      10.0 , !- Initial Volume  {m3}
      20.0, !- Design In Flow Rate {m3}
      20.0, !- Design Out Flow Rate {m3}
      ,  !- Overflow Destination
      Mains, !- Type of Supply Controlled by Float Valve
      8.0 , !- Float Valve On Capacity {m3}
      10.0,  !- Float Valve Off Capacity {m3}
      3.0, !- Backup Mains Capacity {m3}
      ,  !- Other Tank Name
      ScheduledTemperature , !- Water Thermal Mode
      Water Tank Temp Sched , !- Water Temperature Schedule Name
      , !- Ambient Temperature Indicator
      , !- Ambient Temperature Schedule Name
      , !- Zone Name
      , !- Tank Surface Area {m2}
      , !- Tank U Value {W/m2-K}
      ; !- Tank Outside Surface Material Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The following output variables are available for reporting for each [WaterUse:Storage](#waterusestorage) object.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Water System Storage Tank Volume [m3]
    HVAC,Average,Water System Storage Tank Net Volume Flow Rate [m3/s]
    HVAC,Average,Water System Storage Tank Inlet Volume Flow Rate [m3/s]
    HVAC,Average,Water System Storage Tank Outlet Volume Flow Rate [m3/s]
    HVAC,Sum,Water System Storage Tank Mains Water Volume [m3]
    HVAC,Average,Water System Storage Tank Mains Water Volume Flow Rate [m3/s]
    HVAC,Average,Water System Storage Tank Water Temperature [C]
    HVAC,Average,Water System Storage Tank Overflow Volume Flow Rate [m3/s]
    HVAC,Sum,Water System Storage Tank Overflow Water Volume [m3]
    HVAC,Average,Water System Storage Tank Overflow Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Water System Storage Tank Volume [m3]

This is the volume of water stored in the storage tank, in units of cubic meters (m^3^).

#### Water System Storage Tank Net Volume Flow Rate [m3]

This is the net rate of flows in and out of the storage tank, in units of cubic meters (m^3^).

#### Water System Storage Tank Inlet Volume Flow Rate [m3/s]

This is the rate of flows into the storage tank, in units of cubic meters per second (m^3^/s).

#### Water System Storage Tank Outlet Volume Flow Rate [m3]

This is the rate of flows out of the storage tank, in units of cubic meters (m^3^).

#### Water System Storage Tank Mains Water Volume [m3]

This is the volume of water drawn from the mains in order to fill the tank, in units of cubic meters per second (m^3^/s). This output is also added to a meter with Resource Type = MainsWater, End Use Key = WaterSystem, and Group Key = System. The water quality sub-category may be entered as any type (defined in [WaterUse:Storage](#waterusestorage) object input for Water Quality Subcategory) (ref. Output Meter).

#### Water System Storage Tank Mains Water Volume Flow Rate [m3/s]

This is the rate of water draw from the mains in order to fill the tank, in units of cubic meters per second (m^3^/s).

#### Water System Storage Tank Water Temperature [C]

This is the temperature of the water in the tank, in units of degrees Celsius (C).

#### Water System Storage Tank Overflow Volume Flow Rate [m3/s]

This is the rate of water flows that overflow the tank either because of limits on how fast the tank can fill or because the tank is full and cannot handle the water provided, in units of cubic meters per seconc (m^3^/s).

#### Water System Storage Tank Overflow Water Volume [m3]

This is the volume of water overflowing the tank because of limits on how fast the tank can fill or because the tank is full and cannot handle the water provided, in units of cubic meters (m^3^).

#### Water System Storage Tank Overflow Temperature [C]

This the temperature of overflow water leaving the tank.

## WaterUse:RainCollector

The [WaterUse:RainCollector](#wateruseraincollector) object is used for harvesting rainwater falling on building surfaces. The rainwater is sent to a [WaterUse:Storage](#waterusestorage) object. In order to use this object it is necessary to also include a [Site:Precipitation](#siteprecipitation) object to describe the rates of rainfall.

### Inputs

#### Field: Name

The unique object name for reference by other objects.

#### Field: Storage Tank Name

A reference to a [WaterUse:Storage](#waterusestorage) object where the rainwater will be collected and stored for later use.

#### Field: Loss Factor Mode

The [WaterUse:RainCollector](#wateruseraincollector) model includes a loss factor that indicates the portion of incident rain that is not successfully collected. Two modes are available: Constant and Scheduled. Enter Constant in this field for the model to use a simple fixed loss factor defined in the next field. For versatility, the loss factor can follow a schedule by entering Scheduled in this field and then the name of a schedule in the second field below.

#### Field: Collection Loss Factor

Constant loss factor for how much of the incident rainwater is lost and not collected. The factor should be between 0 and 1.

#### Field: Collection Loss Factor Schedule Name

Name of a schedule that defines a variable collection loss factor if the mode is selected as Scheduled.

#### Field: Maximum Collection Rate

The maximum flow rate [m3/s] for rainwater collection. In a scenario where a heavy rain fall is being harvested, the actual reclamation rate might be limited by pipe size or the filtration system.

#### Field: Collection Surface 1-10 Name

A reference to the name of a surface object which will collect the rain. Typically the collection surfaces would correspond to roof surfaces on the building. The effective area for rainwater collection is the horizontal component (area \* cosine of the slope, etc.).  More detailed modeling of rainwater collection can be added later to account for factors such as wind speed and direction, etc.

IDF examples:

~~~~~~~~~~~~~~~~~~~~

    WaterUse:RainCollector,
      My Test Rain Collector ,!- Name
      My Test Tank ,  !-  Storage Tank Name
      CONSTANT,  !- Loss Factor Mode
      0.9 ,  !- Collection Loss Factor
      ,  !- Collection Loss Factor Schedule Name
      1.0 ,  !- Maximum Collection Rate
      SouthRoof ,  !- Collection Surface 1 Name
      NorthRoof1,  !- Collection Surface 2 Name
      NorthRoof3 ,  !- Collection Surface 3 Name
      NorthRoof2 ;  !- Collection Surface 4 Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Water System Rainwater Collector Volume Flow Rate [m3/s]
    HVAC,Sum,Water System Rainwater Collector Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Water System Rainwater Collector Volume Flow Rate [m3/s]

This output variable provides the rate of water collected by the [WaterUse:RainCollector](#wateruseraincollector), in units of cubic meters per second (m^3^/s).

#### Water System Rainwater Collector Volume [m3]

This output variable provides the volume of water collected by the [WaterUse:RainCollector](#wateruseraincollector), in units of cubic meters per second (m^3^/s). This output is also added to a meter with Resource Type = OnSiteWater, End Use Key = RainWater, and Group Key = System (ref. Output Meter).

## WaterUse:Well 

The [WaterUse:Well](#waterusewell) object is for simulating on-site water supply from a well. Well water is pumped out of the ground and into a [WaterUse:Storage](#waterusestorage). The operation of the ground water well is controlled by the associated [WaterUse:Storage](#waterusestorage) which is assumed to be operated as a vented cistern with no pressure tank. The current implementation is very simple and only requires two numeric inputs: the rated pump flow and power. A more elaborate well model may be added and many input fields are reserved for this future expansion.

### Inputs

#### Field: Name

Unique name of this well.

#### Field: Storage Tank Name

A reference to a [WaterUse:Storage](#waterusestorage) object where the rainwater will be collected and stored for later use.

#### Field: Pump Depth

Reserved for future use. Effective depth of well [m]

#### Field: Pump Rated Flow Rate

This is the nominal pump flow rate [m3/s]. This field is required.

#### Field: Pump Rated Head

Reserved for future use [Pa]. Pump head at rated conditions in Pascals.

#### Field: Pump Rated Power Consumption

This is the pump power at the nominal pump flow rate [W]. This field is required.

#### Field: Pump Efficiency

**Reserved for future use.**

#### Field: Well Recovery Rate

Reserved for future use. Rate at which ground water enters well during a sustained draw, e.g., 2 hour well test  [m3/s].

#### Field: Nominal Well Storage Volume

Reserved for future use. Capacity in well after long period of inactivity and the usual water table depth. [m3]

#### Field: Water Table Depth Mode

Reserved for future use. Two options available: Constant or Scheduled

#### Field: Water Table Depth

Reserved for future use. Depth of water table from WaterUse:Storage

#### Field: Water Table Depth Schedule Name

Reserved for future use.

An example input object:

~~~~~~~~~~~~~~~~~~~~

    WaterUse:Well,
      My Test Well,  !- Name
      Tower 1 Water Tank,  !- Storage Tank Name
      ,  !- Pump Depth
      0.1,  !- Pump Rated Flow Rate {m3/s}
      ,  !- Pump Rated Head {Pa}
      120,  !- Pump Rated Power Consumption {W}
      ,  !- Pump Efficiency
      ,  !- Well Recovery Rate {m3/s}
      ,  !- Nominal Well Storage Volume {m3}
      ,  !- Water Table Depth Mode
      ,  !- Water Table Depth
      ;  !- Water Table Depth Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The following output variables are available for reporting for each [WaterUse:Well](#waterusewell) object.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Water System Groundwater Well Requested Volume Flow Rate [m3/s]
    HVAC,Average,Water System Groundwater Well Volume Flow Rate [m3/s]
    HVAC,Sum,Water System Groundwater Well Volume [m3]
    HVAC,Average,Water System Groundwater Well Pump Electric Power [W]
    HVAC,Sum,Water System Groundwater Well Pump Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Water System Groundwater Well Requested Volume Flow Rate [m3/s]

This is the volume flow rate that the water system requested from the well, in units of cubic meters per second (m^3^/s).

#### Water System Groundwater Well Volume Flow Rate [m3/s]

This is the volume flow water of water actually obtained from the well, in units of cubic meters per second (m^3^/s).

#### Water System Groundwater Well Volume [m3]

This is the volume of water obtained from the well, in units of cubic meters (m^3^). This output is also added to a meter with Resource Type = OnSiteWater, End Use Key = WellWater, and Group Key = System (ref. Output Meter).

#### Water System Groundwater Well Pump Electric Power [W]

This is the electrical power of the pump used to extract water from the well, in units of watts (W).

#### Water System Groundwater Well Pump Electric Energy [J]

This is the electricity energy used by the pump to extract water from the well, in units of Joules (J). This output is also added to a meter with Resource Type = Electricity, End Use Key = WaterSystems, and Group Key = System (ref. Output Meter).