# Group – Unitary Equipment

## Furnace and Unitary Systems

The components

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:UnitarySystem
    AirLoopHVAC:Unitary:Furnace:HeatOnly
    AirLoopHVAC:Unitary:Furnace:HeatCool
    AirLoopHVAC:UnitaryHeatOnly
    AirLoopHVAC:UnitaryHeatCool
    AirLoopHVAC:UnitaryHeatPump:AirToAir
    AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed
~~~~~~~~~~~~~~~~~~~~

are compound components usually placed in the primary air loop as the sole component. On the zone equipment side they are usually connected to one or more zones through uncontrolled terminal units (i.e., [AirTerminal:SingleDuct:Uncontrolled](#airterminalsingleductuncontrolled) objects). The maximum or design air flow rate through the furnace or unitary system should usually be set equal to the sum of the maximum air flow rates through the terminal unit objects. However, the simulation program can usually account for unequal air flows if the user wishes to model this scenario.

## AirLoopHVAC:UnitarySystem

The [AirLoopHVAC:UnitarySystem](#airloophvacunitarysystem) object is a "virtual" component that consists of a fan component (OnOff, ConstantVolume, or VariableVolume), a cooling coil component, a heating coil component, and a reheat coil as shown in Figure 117. When a draw through configuration is desired, the fan is placed directly after the heating coil. If dehumidification control is selected, a reheat coil component is also required. If the reheat coil is present and the dehumidification control type input is not specified as CoolReheat, the reheat coil will not be active,

![Schematic of the EnergyPlus Unitary System](media/schematic-of-the-energyplus-unitary-system.jpeg)


Links to the fan, cooling coil, heating coil and reheat coil specifications are provided in the unitary system input data syntax. In addition, the control zone name and the system design operating conditions are specified by the unitary system inputs.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the unitary system.

#### Field: Control Type

This alpha field contains control type i.e. load based or setpoint based for the unitary system. Valid choices are Load and SetPoint. Load control requires a Controlling [Zone](#zone) name and SetPoint control requires set points at each coil outlet node. A single set point at the outlet of the system is allowed but not recommended.

#### Field: Controlling Zone or Thermostat Location

This alpha field contains the identifying zone name where the thermostat controlling the unitary system is located.

#### Field: Dehumidification Control Type

This alpha field contains the type of dehumidification control. The following options are valid for this field:

- **None** - meet sensible load only, no active dehumidification control
- **Multimode** - activate enhanced dehumidification mode as needed and meet sensible load. This option is used to model DX equipment with a controllable heat exchanger assisting the DX cooling coil for improved dehumidification. It is valid only with cooling coil type = [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted).
- **CoolReheat** - cool beyond the dry-bulb temperature set point as required to meet the high humidity setpoint. If cooling coil type = [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted), then the heat exchanger is assumed to always transfer energy between the cooling coil's inlet and outlet airstreams when the cooling coil is operating.

The default is **None**. For the other dehumidification control modes, the maximum humidity setpoint is used. This must be set using a **ZoneControl:Humidistat** object. When extra dehumidification is required, the system may not be able to meet the humidity setpoint if its full capacity is not adequate. If the dehumidification control type is specified as **CoolReheat**, then two additional inputs (reheat coil type and name) are also required as shown below. Although the reheat coil is required only when **CoolReheat** is selected, the optional reheat coil may be present for any of the allowed Dehumidification Control Types. If the reheat coil is present and the dehumidification control type is not specified as **CoolReheat**, the reheat coil will not be active,

#### Field: Availability Schedule Name

This alpha field contains the schedule name which contains information on the availability of the unitary system to operate. A schedule value equal to 0 denotes that the unitary system must be off for that time period. A value greater than 0 denotes that the unitary system is available to operate during that time period. This schedule may be used to completely disable the unitary system as required. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Unitary System Air Inlet Node Name

This alpha field contains the unitary system inlet node name.

#### Field: Unitary System Air Outlet Node Name

This alpha field contains the unitary system outlet node name.

#### Field: Supply Fan Object Type

This alpha field contains the identifying type of supply air fan specified for the unitary system. Fan type must be **[Fan:OnOff](#fanonoff),** **[Fan:ConstantVolume](#fanconstantvolume), or [Fan:VariableVolume](#fanvariablevolume)**. [Fan:ConstantVolume](#fanconstantvolume) is used when the Supply Air Fan Operating Mode Schedule values are never 0 and the fan operates continuously. [Fan:OnOff](#fanonoff) is used when the fan cycles on and off with the cooling or heating coil (i.e. Supply Air Fan Operating Mode Schedule values are at times 0). [Fan:VariableVolume](#fanvariablevolume) is used for variable air volume systems or multi- or variable-speed coils.

#### Field: Supply Fan Name

This alpha field contains the identifying name given to the unitary system fan.

#### Field: Fan Placement

This alpha field has two choices: **BlowThrough** or **DrawThrough**. The first choice stands for "blow through fan". This means that the unit consists of a fan followed by the main cooling and heating coils and supplemental heating coil. The fan "blows through" the cooling and heating coils. The second choice stands for "draw through fan". This means that the unit consists of the main cooling/heating coil(s) followed by a fan, with the supplemental heater located at the outlet of the fan. The fan "draws air through" the cooling/heating coil(s). If this field is left blank, the default is blow through.

**Field: Supply Air Fan Operating Mode Schedule Name**

This alpha field specifies the name of the supply air fan operating mode schedule. The supply air fan operating mode may vary during the simulation based on time-of-day or with a change of season. Schedule values of 0 denote that the unitary system supply air fan and the heating or cooling coil cycle on and off together to meet the heating or cooling load (a.k.a. AUTO fan). Schedule values other than 0 denote that the supply fan runs continuously while the heating or cooling coil cycles to meet the load.

#### Field: Heating Coil Object Type

This alpha field contains the identifying type of heating coil specified in the unitary system. The hot water and steam heating coils require specifying plant loop, branches, and connector objects to support the heating coils, and are placed on the demand side of the plantloop. Allowable coil types are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:DX:SingleSpeed
    Coil:Heating:DX:TwoSpeed
    Coil:Heating:DX:MultiSpeed
    Coil:Heating:DX:VariableSpeed
    Coil:Heating:WaterToAirHeatPump:ParameterEstimation
    Coil:Heating:WaterToAirHeatPump:EquationFit
    Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit
    Coil:Heating:Gas
    Coil:Heating:Gas:MultiStage
    Coil:Heating:Electric
    Coil:Heating:Electric:MultiStage
    Coil:Heating:Water
    Coil:Heating:Steam
    Coil:Heating:Desuperheater
~~~~~~~~~~~~~~~~~~~~

#### Field: Heating Coil Name

This alpha field contains the identifying name given to the unitary system heating coil.

#### Field: DX Heating Coil Sizing Ratio

This numeric field is used to adjust heat pump heating capacity with respect to DX cooling capacity. It is used only for DX heat pump configurations (i.e., a DX cooling and heating coil is used).

#### Field: Cooling Coil Object Type

This alpha field contains the identifying type of cooling coil specified in the unitary system. Allowable coil types are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:SingleSpeed
    Coil:Cooling:DX:TwoSpeed
    Coil:Cooling:DX:MultiSpeed
    Coil:Cooling:DX:VariableSpeed
    Coil:Cooling:DX:TwoStageWithHumidityControlMode
    CoilSystem:Cooling:DX:HeatExchangerAssisted
    Coil:Cooling:WaterToAirHeatPump:ParameterEstimation
    Coil:Cooling:WaterToAirHeatPump:EquationFit
    Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit
    Coil:Cooling:Water
    Coil:Cooling:Water:DetailedGeometry
    CoilSystem:Cooling:Water:HeatExchangerAssisted
~~~~~~~~~~~~~~~~~~~~

#### Field: Cooling Coil Name

This alpha field contains the identifying name given to the unitary system cooling coil.

#### Field: Use DOAS DX Cooling Coil

This input field eanbles DX Cooling coils to be used for 100% outdoor air dedicated outdor air system applications. There are two choices Yes or No. If Yes, the DX coil is used as 100% outdoor DX coil. If No, the DX coil is used as regular DX coil. This input file d is optional and the default is No.

#### Field: DOAS DX Cooling Coil Leaving Minimum Air Temperature

This input field is DX Cooling coils leaving minimum air temperature for frost control. The DX cooling coil leaving air temperature is not allowed to exceed this minimum air temperature. The DX cooling coil frost controller adjusts or limits the desired coil outlet air setpoint temperature when the coil outlet temperature exceeds this minimum temperature limit specified. This input field is optional and only used along with in the input field above. The minimum and maximum values of this input field are 0.0C and 7.2C, and the default value is 2.0°C.

#### Field: Latent Load Control

This alpha field defines the latent load control method. Available choices are SensibleOnlyLoadControl, LatentOnlyLoadControl, LatentWithSensibleLoadControl, or LatentOrSensibleLoadControl. The default choice is SensibleOnlyLoadControl. The SensibleOnlyLoadControl choice will operate to meet only a sensible load, and the LatentOnlyLoadConrol will operate to meet only a latent load. The LatentWithSensibleLoadControl will operate to meet the latent load only if there is a sensible load. The LatentOrSensibleLoadControl will operate to meet either a latent or sensible load.

#### Field: Supplemental Heating Coil Object Type

This alpha field contains the identifying type of supplemental heating coil specified in the unitary system. The hot water and steam heating coils require specifying plant loop, branches, and connector objects to support the heating coils, and are placed on the demand side of the plantloop. supplemental heating type must be one of:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Desuperheater
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Supplemental Heating Coil Name

This alpha field contains the identifying name given to the unitary system reheat coil.

#### Field: Supply Air Flow Rate Method During Cooling Operation

This alpha field defines the supply air flow method during cooling operation. Available choices are SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingValue, FlowPerCoolingCapacity. For each of the choices, corresponding air flow rate for cooling must be specified.

#### Field: Supply Air Flow Rate During Cooling Operation

This numeric field defines the supply air flow rate leaving the unitary system in cubic meters per second when the cooling coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Cooling Operation is **SupplyAirFlowRate**.

#### Field: Supply Air Flow Rate Per Floor Area During Cooling Operation

This numeric field defines the supply air flow rate per floor area leaving the unitary system in meters per second when the cooling coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Cooling Operation is **FlowPerFloorArea**.

#### Field: Fraction of Autosized Design Cooling Supply Air Flow Rate

This numeric field defines the fraction of autosized supply air flow rate leaving the unitary system when the cooling coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Cooling Operation is **FractionOfAutosizedCoolingValue**.

#### Field: Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation

This numeric field defines the supply air flow rate per unit of capacity leaving the unitary system when the cooling coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Cooling Operation is **FlowPerCoolingCapacity**.

#### Field: Supply Air Flow Rate Method During Heating Operation

This alpha field defines the supply air flow method during heating operation. Available choices are SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedHeatingValue, FlowPerHeatingCapacity. For each of the choices, corresponding air flow rate for heating must be specified.

#### Field: Supply Air Flow Rate During Heating Operation

This numeric field defines the supply air flow rate leaving the unitary system in cubic meters per second when the heating coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Heating Operation is **SupplyAirFlowRate**.

#### Field: Supply Air Flow Rate Per Floor Area During Heating Operation

This numeric field defines the supply air flow rate per floor area leaving the unitary system in meters per second when the heating coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Heating Operation is **FlowPerFloorArea**.

#### Field: Fraction of Autosized Design Heating Supply Air Flow Rate

This numeric field defines the fraction of autosized supply air flow rate leaving the unitary system when the heating coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Heating Operation is **FractionOfAutosizedHeatingValue**.

#### Field: Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation

This numeric field defines the supply air flow rate per unit of capacity leaving the unitary system when the heating coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Heating Operation is **FlowPerHeatingCapacity**.

#### Field: Supply Air Flow Rate Method When No Cooling or Heating is Needed

This alpha field defines the supply air flow method when neither cooling or heating is required. Available choices are SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingValue, FractionOfAutosizedHeatingValue, FlowPerCoolingCapacity, FlowPerHeatingCapacity. For each of the choices, corresponding air flow rate must be specified.

#### Field: Supply Air Flow Rate When No Cooling or Heating is Needed

This numeric field defines the supply air flow rate leaving the unitary system in cubic meters per second when neither cooling or heating is required (i.e., main cooling/heating coils and supplemental heater are off but the supply air fan operates). This field is only used when the unitary system operating mode is specified as continuous fan operation. Values must be greater than or equal to zero, or this field is autosizable. If the unitary system operating mode is specified as continuous fan operation and this value is set to zero or this field is left blank, then the model assumes that the supply air flow rate when no cooling/heating is needed is equal to the supply air flow rate when the compressor was last operating (for cooling operation or heating operation).

#### Field: Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Needed

This numeric field defines the supply air flow rate per floor area leaving the unitary system in meters per second when neither cooling or heating coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Heating Operation is **FlowPerFloorArea**.

#### Field: Fraction of Autosized Design Cooling Supply Air Flow Rate When No Cooling or Heating is Needed

This numeric field defines the fraction of autosized supply air flow rate leaving the unitary system when neither cooling or heating coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Cooling Operation is **FractionOfAutosizedCoolingValue**.

#### Field: Fraction of Autosized Design Heating Supply Air Flow Rate When No Cooling or Heating is Needed

This numeric field defines the fraction of autosized supply air flow rate leaving the unitary system when the neither cooling or heating coil is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Heating Operation is **FractionOfAutosizedHeatingValue**.

#### Field: Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation When No Cooling or Heating is Needed

This numeric field defines the supply air flow rate per unit of capacity leaving the unitary system when neither cooling or heating is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Cooling Operation is **FlowPerCoolingCapacity**.

#### Field: Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation When No Cooling or Heating is Needed

This numeric field defines the supply air flow rate per unit of capacity leaving the unitary system when neither cooling or heating is operating. Values must be greater than 0 or this field is autosizable. Required field when Supply Air Flow Rate Method During Heating Operation is **FlowPerHeatingCapacity**.

#### Field: Maximum Supply Air Temperature

This numeric field contains the design operating air outlet temperature in degrees C when the unitary system is heating. If this input field is left blank, the default value is 80 C.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation

This numeric field defines the outdoor air dry-bulb temperature above which the heat pump supplemental heating coil is disabled.  The temperature for this input field must be less than or equal to 21 C. If this input field is left blank, the default value is 21 C.

#### Field: Outdoor Dry-Bulb Temperature Sensor Node Name

This alpha field specifies the name of the outdoor node which controls the operation of the supplemental heating coil. If this field is left blank, the outdoor temperature is based solely on the weather data. If this field is not blank, the node name specified must also be listed in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor temperature from the weather data. Alternately, the node name must be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor temperature is taken directly from the weather data.

#### Field: Maximum Cycling Rate

This numeric field contains the maximum on-off cycling rate for the compressor, which occurs at 50% run time fraction. Suggested values are shown below (Henderson et al. 1999):

![](media/image282.png)\


#### Field: Heat Pump Time Constant

This numeric field contains the time constant for the cooling coil's capacity to reach steady state after startup. Suggested values are shown below (Henderson et al. 1999):

![](media/image283.png)\


#### Field: Fraction of On-Cycle Power Use

This numeric field contains the fraction of on-cycle power use to adjust the part load fraction based on the off-cycle power consumption due to crankcase heaters, controls, fans, and etc. Suggested value values are below (Henderson et al. 1999):

![](media/image284.png)\


#### Field: Heat Pump Fan Delay Time

This numeric field contains the time delay for the heat pump supply air fan to shut off after the compressor cycles off in seconds. This value can be obtained from the manufacturer or the heat pump catalog. Enter a value of zero when the heat pump's fan operating mode is continuous. Suggested value is 60 seconds.

#### Field: Ancilliary On-Cycle Electric Power

This field defines ancilliary electrical power (W) consumed during the on-cycle period (i.e., when the cooling or heating coil is operating). The model assumes that this ancilliary power does not contribute to heating the supply air. The minimum value for this field is 0.0, and the default value is also 0.0 if the field is left blank.

#### Field: Ancilliary Off-Cycle Electric Power

This field defines ancilliary electrical power (W) consumed during the off-cycle period (i.e., when the cooling and heating coil are not operating). The model assumes that this ancilliary power does not contribute to heating the supply air. The minimum value for this field is 0.0, and the default value is also 0.0 if the field is left blank.

#### Field: Design Heat Recovery Water Flow Rate

This optional input field defines the design water flow rate used if the heat recovery option is being simulated. If this value is greater than 0.0 then a heat recovery loop must be specified and attached to the multispeed heat pump using the next 2 node fields. To determine how the heat recovery algorithm works, refer to the EnergyPlus Engineering Reference in the [AirLoopHVAC:UnitarySystem](#airloophvacunitarysystem) with Heat Recovery section. The units for this input value are cubic meters per second.

#### Field: Maximum Temperature for Heat Recovery

This field sets the maximum temperature (in degrees C) that this heat pump can produce for heat recovery. The idea behind this field is that the current models do not take temperatures into account for availability and they just pass Q's around the loop without a temperature limit. This temperature limit puts an upper bound on the recovered heat and limits the max temperature leaving the component.

As temperatures in the loop approach the maximum temperature, the temperature difference between the entering water and the surfaces in the piece of equipment becomes smaller. For the given heat recovery flow rate and that temperature difference the amount of heat recovered will be reduced, and eventually there will be no heat recovered when the entering water temperature is equal to the maximum temperature specified by the user in this field. The reduced amount of heat recovered will diminish if the temperature of the loop approach is the maximum temperature, and this will show up in the reporting. This allows the user to set the availability or the quality of the heat recovered for usage in other parts of the system or to heat domestic hot water supply.

#### Field: Heat Recovery Water Inlet Node Name

This alpha field contains the identifying name for the heat recovery side inlet node.

#### Field: Heat Recovery Water Outlet Node Name

This alpha field contains the identifying name for the heat recovery side outlet node.

#### Field: Design Specification Multispeed Heat Pump Object Type

This alpha field contains the identifying type for the design specification multispeed object. This field is only needed when multispeed cooling or heating coil is specified.

#### Field: Design Specification Multispeed Heat Pump Object Name

This alpha field contains the identifying name for the design specification multispeed object. This field is only needed when multispeed cooling or heating coil is specified.

As shown in the example below, correct specification of the heat/cool unitary system requires specification of the following objects in addition to the unitary system object:

#. Fan ([Fan:OnOff](#fanonoff) or [Fan:ConstantVolume](#fanconstantvolume))
#. Cooling coil
#. Heating coil
#. Reheat coil
#. Direct air unit ([AirTerminal:SingleDuct:Uncontrolled](#airterminalsingleductuncontrolled)) for each zone served by the unitary system

~~~~~~~~~~~~~~~~~~~~

     AirLoopHVAC:UnitarySystem,
       DXAC Heat Pump 1,        !- Name
       Load,                    !- Control Type
       East Zone,               !- Controlling Zone or Thermostat Location
       ,                        !- Dehumidification Control Type
       FanAndCoilAvailSched,    !- Availability Schedule Name
       Mixed Air Node,          !- Air Inlet Node Name
       Air Loop Outlet Node,    !- Air Outlet Node Name
       Fan:OnOff,               !- Supply Air Fan Object Type
       Supply Fan 1,            !- Supply Air Fan Name
       BlowThrough,             !- Fan Placement
       FanModeSchedule,         !- Supply Air Fan Operating Mode Schedule Name
       Coil:Heating:DX:MultiSpeed,  !- Heating Coil Object Type
       Heat Pump DX Heating Coil 1, !- Heating Coil Name
       ,                            !- DX Heating Coil Sizing Ratio
       Coil:Cooling:DX:MultiSpeed,  !- Cooling Coil Object Type
       Heat Pump ACDXCoil 1,    !- Cooling Coil Name
       ,                        !- Use DOAS DX Cooling Coil
       ,                        !- DOAS DX Cooling Coil Leaving Minimum Air Temperature
       ,                        !- Latent Load Control
       Coil:Heating:Gas,        !- Supplemental Heating Coil Object Type
       Supp Gas Heating Coil 1, !- Supplemental Heating Coil Name
       SupplyAirFlowRate,       !- Supply Air Flow Rate Method During Cooling Operation
       1.7,                     !- Supply Air Flow Rate During Cooling Operation {m3/s}
       ,                        !- Supply Air Flow Rate Per Floor Area During Cooling Opertation
       ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate
       ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation
       SupplyAirFlowRate,       !- Supply Air Flow Rate Method During Heating Operation
       1.7,                     !- Supply Air Flow Rate During Heating Operation {m3/s}
       ,                        !- Supply Air Flow Rate Per Floor Area During Heating Opertation
       ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate
       ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation
       SupplyAirFlowRate,       !- Supply Air Flow Rate Method When No Cooling or Heating is Required
       0.2,                     !- Supply Air Flow Rate When No Cooling or Heating is Required {m3/s}
       ,                        !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required
       ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate
       ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate
       ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation
       ,                        !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation
       50,                      !- Maximum Supply Air Temperature {C}
       21,  !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation
       ,  !- Outdoor Dry-Bulb Temperature Sensor Node Name
       ,  !- Maximum Cycling Rate
       ,  !- Heat Pump Time Constant
       ,  !- Fraction of On-Cycle Power Use
       ,  !- Heat Pump Fan Delay Time
       ,  !- Ancilliary On-Cycle Electric Power
       ,  !- Ancilliary Off-Cycle Electric Power
       ,  !- Design Heat Recovery Water Flow Rate
       ,  !- Maximum Temperature for Heat Recovery (Maximum Heat Recovery Outlet Temperature?)
       ,  !- Heat Recovery Water Inlet Node Name
       ,  !- Heat Recovery Water Outlet Node Name
       UnitarySystemPerformance:HeatPump:Multispeed,  !- Design Specification Multispeed Heat Pump Object Type
       MyMultispeedHPSpec;  !- Design Specification Multispeed Heat Pump Object Name

    UnitarySystemPerformance:HeatPump:Multispeed,
       MyMultispeedHPSpec,      !- Name
       4,                       !- Number of Speeds for Heating
       4,                       !- Number of Speeds for Cooling
       0.235294118,             !- Speed 1 Supply Air Flow Rate During Heating Operation {m3/s}
       0.235294118,             !- Speed 1 Supply Air Flow Rate During Cooling Operation {m3/s}
       0.470588235,             !- Speed 2 Supply Air Flow Rate During Heating Operation {m3/s}
       0.470588235,             !- Speed 2 Supply Air Flow Rate During Cooling Operation {m3/s}
       0.705882353,             !- Speed 3 Supply Air Flow Rate During Heating Operation {m3/s}
       0.705882353,             !- Speed 3 Supply Air Flow Rate During Cooling Operation {m3/s}
       1.0,                     !- Speed 4 Supply Air Flow Rate During Heating Operation {m3/s}
       1.0;                     !- Speed 4 Supply Air Flow Rate During Cooling Operation {m3/s}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Unitary System Fan Part Load Ratio []
    HVAC,Average, Unitary System Compressor Part Load Ratio
    HVAC,Average,Unitary System Total Cooling Rate [W]
    HVAC,Average,Unitary System Total Heating Rate [W]
    HVAC,Average,Unitary System Sensible Cooling Rate [W]
    HVAC,Average,Unitary System Sensible Heating Rate [W]
    HVAC,Average,Unitary System Latent Cooling Rate [W]
    HVAC,Average,Unitary System Latent Heating Rate [W]
    HVAC,Average,Unitary System Ancillary Electric Power[W]
    Two speed coil outputs
    HVAC,Average,Unitary System Cycling Ratio []
    Multi speed coil outputs
    HVAC,Average,Unitary System DX Coil Cycling Ratio []
    HVAC,Average,Unitary System DX Coil Speed Ratio []
    HVAC,Average,Unitary System DX Coil Speed Level []
    HVAC,Average,Unitary System Electric Power[W]
    HVAC,Sum,Unitary System Electric Energy [J]
    HVAC,Sum,Unitary System Cooling Ancillary Electric Energy [J]
    HVAC,Sum,Unitary System Heating Ancillary Electric Energy [J]
    Multi speed coil outputs(If heat recovery is specified)
    HVAC,Average, Unitary System Heat Recovery Rate [W]
    HVAC,Average, Unitary System Heat Recovery Inlet Temperature [C]
    HVAC,Average, Unitary System Heat Recovery Outlet Temperature [C]
    HVAC,Average, Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]
    HVAC,Sum, Unitary System Heat Recovery Energy [J]
    Water to air heat pump outputs
    HVAC,Average, Unitary System Requested Sensible Cooling Rate [W]
    HVAC,Average, Unitary System Requested Latent Cooling Rate [W]
    HVAC,Average, Unitary System Requested Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Unitary System Fan Part Load Ratio []

This output variable is the ratio of actual air mass flow rate through the unitary system to the unitary system's design air mass flow rate (i.e., design volumetric flow rate converted to dry air mass flow rate). For continuous fan operation mode, this variable is always 1.0 when the unitary system is available (based on the availability schedule). For cycling fan/cycling coil operation mode, the actual air mass flow rate is calculated based on the ratio of the sensible heating (or cooling) load to the steady-state unitary system heating (or cooling) capacity. For the cycling fan mode, the runtime fraction for the unitary system fan may be different from the fan part-load ratio reported here due the part-load performance of the unitary system's heating (or cooling) coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate (e.g., [Fan:OnOff](#fanonoff)). When the speed number is greater than 1, the value is 1.0.

#### Unitary System Compressor Part Load Ratio []

This output variable is the ratio of the sensible load (heating or cooling) to the steady-state capacity of the unitary system's DX heating or cooling coil at Speed 1. The runtime fraction for the unitary system compressor may be different from the compressor part-load ratio reported here due the part-load performance of the heating/cooling coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate. When the speed number is greater than 1, the value is 1.0.

#### Unitary System DX Coil Cycling Ratio []

This output variable is the ratio of the sensible load (heating or cooling) to the steady-state capacity of the unitary system's DX heating or cooling coil (Speed 1) for the entire system timestep. The value is between 0.0 and 1.0 when the unitary system is cycling on and off its lowest speed (Speed 1) and 1.0 when the unitary system operates at speeds above 1.

#### Unitary System DX Coil Speed Ratio []

This output variable is the ratio of time in a system timestep that the compressor is at rated speed between two consecutive speed numbers ( [Compressor Speed - Compressor speed at Speed i-1] / [Compressor speed at Speed i - Compressor speed at Speed i-1]). The compressor speed ratio reports (1.0 is max, 0.0 is min) and any value in between as it is averaged over the timestep. The value is 0.0 during Speed 1 operation.

The physical meaning of the speed ratio is dependent on the compressor configuration defined in the field of child coil object: Apply Part Load Fraction to Speeds greater than 1. The allowed choice is either Yes or No. When No is entered, one compressor is assumed for all speeds.  The speed ratio represents how long the higher speed runs as a fraction of the system timestep, and the lower speed runs in the rest of the system timestep. When Yes is entered, multiple compressors are assumed, and each compressor has associated speed. The speed ratio represents how long the higher speed runs as a fraction of the system timestep, and the low speed runs in a whole system timestep.

#### Unitary System DX Coil Speed Level []

This output variable reports the maximum speed needed when the unitary system operates to meet the sensible load (heating or cooling) in a system timestep. When the value is 1, the unitary system operates at Speed 1 (lowest speed). For this case the cycling ratio is between 0.0 and 1.0, while the speed ratio is 0.0. When the speed number output variable is above one, such as i, the unitary system operation is determined by the speed ratio through linear interpolation. For example, when the speed ratio is 0.4 and the speed number is 3, the unitary system operates at Speed 3 for 40% of a system timestep and at Speed 2 for 60% of a system timestep for a single compressor. For multiple compressors, the unitary system operates at Speed 3 in the 40% of a system timestep and at Speed 2 in the whole system timestep.

#### Unitary System Total Heating Rate [W]

This output field is the total (enthalpy) heat addition rate of the unitary system to the zones it is serving in Watts. This value is calculated using the enthalpy difference of the unitary system outlet air and inlet air streams, and the air mass flow rate through the unitary system. This value is calculated for each HVAC system timestep being simulated, and the results (enthalpy addition only) are averaged for the timestep being reported.

#### Unitary System Total Cooling Rate [W]

This output field is the total (enthalpy) heat extraction rate of the unitary system from the zones it is serving in Watts. This value is calculated using the enthalpy difference of the unitary system outlet air and inlet air streams, and the air mass flow rate through the unitary system. This value is calculated for each HVAC system timestep being simulated, and the results (enthalpy extraction only) are averaged for the timestep being reported.

#### Unitary System Sensible Heating Rate [W]

This output field reports the sensible heat addition rate of the unitary system to the zones it is serving in Watts. This value is calculated using the enthalpy difference of the unitary system outlet air and inlet air streams at a constant humidity ratio, and the air mass flow rate through the unitary system. This value is calculated for each HVAC system timestep being simulated, and the results (heating only) are averaged for the timestep being reported.

#### Unitary System Sensible Cooling Rate [W]

This output field reports the moist air sensible heat extraction rate of the unitary system from the zones it is serving in Watts. This value is calculated using the enthalpy difference of the unitary system outlet air and inlet air streams at a constant humidity ratio, and the air mass flow rate through the unitary system. This value is calculated for each HVAC system timestep being simulated, and the results (cooling only) are averaged for the timestep being reported.

#### Unitary System Latent Heating Rate [W]

This output field is the latent heat addition (humidification) rate of the unitary system in Watts. This value is calculated as the difference between the total energy rate and the sensible energy rate provided by the unitary system. This value is calculated for each HVAC system timestep being simulated, and the results (latent heat addition only) are averaged for the timestep being reported.

#### Unitary System Latent Cooling Rate [W]

This output field is the latent heat extraction (dehumidification) rate of the unitary system in Watts. This value is calculated as the difference between the total energy rate and the sensible energy rate provided by the unitary system. This value is calculated for each HVAC system timestep being simulated, and the results (latent heat extraction only) are averaged for the timestep being reported.

#### Unitary System Electric Power [W]

This output field is the electricity consumption rate of the unitary system in Watts. The consumption includes electricity used by the DX coils (including crankcase heater if the fuel type is electricity), fans (indoor supply air fan and the condenser fans associated with the DX coil[s]), auxiliary power during on and off period, and the supplemental heating coil (if electric). This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported. Any non-electric energy use is not reported by the unitary system object but is reported in the associated coil objects as appropriate.

#### Unitary System Electric Energy [J]

This output field is the electricity consumption of the unitary system in Joules for the timestep being reported. The consumption includes electricity used by the DX compressor (including crankcase heater if the fuel type is electricity), fans (indoor supply air fan and the condenser fans associated with the DX coil[s]), auxiliary power during on and off period, and the supplemental heating coil (if electric). This value is calculated for each HVAC system timestep being simulated, and the results are summed for the timestep being reported. Any non-electric energy use is not reported by the unitary system object but is reported in the associated coil objects as appropriate.

#### Unitary System Ancillary Electric Power [W]

This output field is the average auxiliary electricity consumption rate (including both on-cycle and off-cycle) in Watts for the timestep being reported.

#### Unitary System Cooling Ancillary Electric Energy [J]

This is the auxiliary electricity consumption in Joules for the timestep being reported. This is the auxiliary electricity consumption during periods when the unitary system is providing cooling (DX cooling coil is operating). This output is also added to a meter with Resource Type = Electricity, End Use Key =Cooling, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Unitary System Heating Ancillary Electric Energy [J]

This is the auxiliary electricity consumption in Joules for the timestep being reported. This is the auxiliary electricity consumption during periods when the unitary system is providing heating (DX heating coil is operating). This output is also added to a meter with Resource Type = Electricity, End Use Key =Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Unitary System Heat Recovery Inlet Temperature [C]

#### Unitary System Heat Recovery Outlet Temperature [C]

#### Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]

These outputs are the heat recovery inlet and outlet temperatures and water mass flow rate for unitary systems with heat recovery.

#### Unitary System Heat Recovery Rate [W]

#### Unitary System Heat Recovery Energy [J]

For multispeed unitary systems with heat recovery, these outputs are the recoverable energy rate (in Watts) and energy (in Joules).

#### Unitary System Requested Sensible Cooling Rate [W]

This output variable is the sensible cooling requested from the zone thermostat in watts. This value is calculated using the unitary system outlet air and zone conditions, the specific heat of the zone air, and the supply air mass flow rate entering/leaving the system. This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

#### Unitary System Requested Latent Cooling Rate [W]

This output variable is the latent cooling requested from the zone humidistat in watts. This value is calculated using the unitary system outlet air and zone conditions, the heat of vaporization of water at the current zone conditions, and the supply air mass flow rate entering/leaving the system. This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

#### Unitary System Requested Heating Rate [W]

This output variable is the sensible heating requested from the zone thermostat in watts. This value is calculated using the unitary system outlet air and zone conditions, the specific heat of the zone air, and the supply air mass flow rate entering/leaving the system. This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

## UnitarySystemPerformance:HeatPump:Multispeed

### Inputs

#### Field: Name

This alpha field contains the identifying name for the multispeed performance specification.

#### Field: Number of Speeds for Heating

This field defines the number of heating speeds for the heat pump, and must match the number of heating speeds defined in the associated heating coil. The value for this input field defines the number of airflow rate ratios that must be defined for heating in the fields below. The minimum value for this field is one and the maximum value is the number specified in the coil object. If the heating coil type used in the unitary system object is not a multispeed coil type, then this field should be 1.

#### Field: Number of Speeds for Cooling

This field defines the number of cooling speeds for the heat pump, and must match the number of cooling speeds defined in the associated DX cooling coil. The value for this input field defines the number of airflow rate ratios that must be defined for cooling in the fields below. The minimum value for this field is one and the maximum value is the number specified in the coil object. If the cooling coil type used in the unitary system object is not a multispeed coil type, then this field should be 1.

#### Field: Speed 1 Supply Air Flow Ratio During Heating Operation

This numeric field defines the ratio of supply air flow rate leaving the unitary system to the maximum air flow rate specified in the coil object at maximum speed when the heating coil and/or supplemental heater are operating at Speed 1 (lowest speed). Values must be greater than 0.

#### Field: Speed 1 Supply Air Flow Ratio During Cooling Operation

This numeric field defines the ratio of supply air flow rate leaving the unitary system to the maximum air flow rate specified in the coil object at maximum speed when the DX cooling coil is operating at Speed 1 (lowest speed). Values must be greater than 0.

#### Field: Speed 2 Supply Air Flow Ratio During Heating Operation

This numeric field defines the ratio of supply air flow rate leaving the unitary system to the maximum air flow rate specified in the coil object at maximum speed when the heating coil and/or supplemental heater are operating at Speed 2. Values must be greater than 0. The entered value must be greater or equal to the flow rate ratio specified for heating speed 1.

#### Field: Speed 2 Supply Air Flow Ratio During Cooling Operation

This numeric field defines the ratio of supply air flow rate leaving the unitary system to the maximum air flow rate specified in the coil object at maximum speed when the DX cooling coil is operating at Speed 2. Values must be greater than 0. The entered value must be greater or equal to the flow rate ratio specified for cooling speed 1.

#### Field: Speed 3 Supply Air Flow Ratio During Heating Operation

This numeric field defines the ratio of supply air flow rate leaving the unitary system to the maximum air flow rate specified in the coil object at maximum speed when the heating coil and/or supplemental heater are operating at Speed 3. Values must be greater than 0. The entered value must be greater or equal to the flow rate ratio specified for heating speed 2. If the ‘Number of Speeds for Heating' is less than 3, then this field can be left blank.

#### Field: Speed 3 Supply Air Flow Ratio During Cooling Operation

This numeric field defines the ratio of supply air flow rate leaving the unitary system to the maximum air flow rate specified in the coil object at maximum speed when the DX cooling coil is operating at Speed 3. Values must be greater than 0. The entered value must be greater or equal to the flow rate ratio specified for cooling speed 2. If the ‘Number of Speeds for Cooling' is less than 3, then this field can be left blank.

#### Field: Speed 4 Supply Air Flow Ratio During Heating Operation

This numeric field defines the ratio of supply air flow rate leaving the unitary system to the maximum air flow rate specified in the coil object at maximum speed when the DX heating coil and/or supplemental heater are operating at Speed 3. Values must be greater than 0. The entered value must be greater or equal to the flow rate ratio specified for heating Speed 3. If the ‘Number of Speeds for Heating' is less than 4, then this field can be left blank.

#### Field: Speed 4 Supply Air Flow Ratio During Cooling Operation

This numeric field defines the ratio of supply air flow rate leaving the unitary system to the maximum air flow rate specified in the coil object at maximum speed when the DX cooling coil is operating at Speed 4. Values must be greater than 0. The entered value must be greater or equal to the flow rate ratio specified for cooling Speed 3. If the ‘Number of Speeds for Cooling' is less than 4, then this field can be left blank.

## AirLoopHVAC:Unitary:Furnace:HeatCool

The heat/cool furnace is a "virtual" component that consists of a fan component (OnOff or ConstantVolume), a DX cooling coil component, and a Gas or Electric heating coil component. The blow through furnace configuration is shown in Figure 116 below. When a draw through furnace configuration is desired, the fan is placed directly after the heating coil. If the dehumidification control type is specified as CoolReheat, a reheat coil component is also required. If the reheat coil is present and the dehumidification control type input is not specified as CoolReheat, the reheat coil will not be active,

![Schematic of EnergyPlus Heat/Cool Furnace](media/schematic-of-energyplus-heatcool-furnace.jpeg)


> Note: the coil order shown here has been revised from previous versions (prior to V4.0) of Energyplus to configure the cooling coil upstream of the heating coil. This configuration provides uniformity with all unitary equipment. However, for unitary HeatCool systems that do not use a reheat coil, the heating coil can also be placed upstream of the cooling coil. This optional coil placement is retained to allow compatibility with previous versions of Energyplus. For input files developed using previous versions of Energyplus, it is recommended that the coil order be revised according to the figure above.

Links to the fan, heating coil, DX cooling coil and optional reheat coil specifications are provided in the furnace input data syntax. In addition, the control zone name and the furnace design operating conditions are specified by the furnace inputs.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the unit.

#### Field: Availability Schedule Name

This alpha field contains the schedule name which contains information on the availability of the furnace to operate. A schedule value equal to 0 denotes that the furnace must be off for that time period. A value greater than 0 denotes that the furnace is available to operate during that time period. This schedule may be used to completely disable the furnace as required. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Furnace Air Inlet Node Name

This alpha field contains the furnace inlet node name.

#### Field: Furnace Air Outlet Node Name

This alpha field contains the furnace outlet node name.

**Field: Supply Air Fan Operating Mode Schedule Name**

This alpha field specifies the name of the supply air fan operating mode schedule. The supply air fan operating mode may vary during the simulation based on time-of-day or with a change of season. Schedule values of 0 denote that the furnace supply air fan and the heating or cooling coil cycle on and off together to meet the heating or cooling load (a.k.a. AUTO fan). Schedule values other than 0 denote that the supply fan runs continuously while the heating or cooling coil cycles to meet the load.

#### Field: Maximum Supply Air Temperature

This numeric field contains the design operating furnace air outlet temperature in degrees C when the furnace is heating. If this input field is left blank, the default value is 80 C.

#### Field: Supply Air Flow Rate During Cooling Operation

This numeric field defines the supply air flow rate leaving the furnace in cubic meters per second when the DX cooling coil is operating. Values must be greater than 0 or this field is autosizable.

#### Field: Supply Air Flow Rate During Heating Operation

This numeric field defines the supply air flow rate leaving the furnace in cubic meters per second when the DX heating coil and/or supplemental heater are operating. Values must be greater than 0 or this field is autosizable.

#### Field: Supply Air Flow Rate When No Cooling or Heating is Needed

This numeric field defines the supply air flow rate leaving the furnace in cubic meters per second when neither cooling or heating is required (i.e., DX coils and supplemental heater are off but the supply air fan operates). This field is only used when the furnace operating mode is specified as continuous fan operation. Values must be greater than or equal to zero, or this field is autosizable. If the furnace operating mode is specified as continuous fan operation and this value is set to zero or this field is left blank, then the model assumes that the supply air flow rate when no cooling/heating is needed is equal to the supply air flow rate when the compressor was last operating (for cooling operation or heating operation).

#### Field: Controlling Zone or Thermostat Location

This alpha field contains the identifying zone name where the thermostat controlling the furnace is located.

#### Field: Supply Fan Object Type

This alpha field contains the identifying type of supply air fan specified for the furnace. Fan type must be **[Fan:OnOff](#fanonoff)** or **[Fan:ConstantVolume](#fanconstantvolume)**. [Fan:ConstantVolume](#fanconstantvolume) is used when the Supply Air Fan Operating Mode Schedule values are never 0 and the fan operates continuously. [Fan:OnOff](#fanonoff) is used when the fan cycles on and off with the cooling or heating coil (i.e. Supply Air Fan Operating Mode Schedule values are at times 0).

#### Field: Supply Fan Name

This alpha field contains the identifying name given to the furnace fan.

#### Field: Fan Placement

This alpha field has two choices: **BlowThrough** or **DrawThrough**. The first choice stands for "blow through fan". This means that the unit consists of a fan followed by the DX coils and supplemental heating coil. The fan "blows through" the cooling and heating coils. The second choice stands for "draw through fan". This means that the unit consists of the DX coil(s) followed by a fan, with the supplemental heater located at the outlet of the fan. The fan "draws air through" the DX coil(s). If this field is left blank, the default is blow through.

#### Field: Heating Coil Object Type

This alpha field contains the identifying type of heating coil specified in the furnace. The hot water and steam heating coils require specifying plant loop, branches, and connector objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the heating coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object (Unitary Heat and Cool Furnace) itself provides the "controller" function of modulating water flow. Allowable coil types are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Heating Coil Name

This alpha field contains the identifying name given to the furnace heating coil.

#### Field: Cooling Coil Object Type

This alpha field contains the identifying type of cooling coil specified in the furnace. Only allowable coil types are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:SingleSpeed
    CoilSystem:Cooling:DX:HeatExchangerAssisted
    Coil:Cooling:DX:VariableSpeed
~~~~~~~~~~~~~~~~~~~~

#### Field: Cooling Coil Name

This alpha field contains the identifying name given to the furnace cooling coil.

#### Field: Dehumidification Control Type

This alpha field contains the type of dehumidification control. The following options are valid for this field:

- **None** - meet sensible load only, no active dehumidification control
- **Multimode** - activate enhanced dehumidification mode as needed and meet sensible load. This option is used to model DX equipment with a controllable heat exchanger assisting the DX cooling coil for improved dehumidification. It is valid only with cooling coil type = [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted).
- **CoolReheat** - cool beyond the dry-bulb temperature set point as required to meet the high humidity setpoint. If cooling coil type = [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted), then the heat exchanger is assumed to always transfer energy between the cooling coil's inlet and outlet airstreams when the cooling coil is operating.

The default is **None**. For the other dehumidification control modes, the maximum humidity setpoint is used. This must be set using a **ZoneControl:Humidistat** object. When extra dehumidification is required, the system may not be able to meet the humidity setpoint if its full capacity is not adequate. If the dehumidification control type is specified as **CoolReheat**, then two additional inputs (reheat coil type and name) are also required as shown below. Although the reheat coil is required only when **CoolRheat** is selected, the optional reheat coil may be present for any of the allowed Dehumidification Control Types. If the reheat coil is present and the dehumidification control type is not specified as **CoolReheat**, the reheat coil will not be active,

#### Field: Reheat Coil Object Type

This alpha field contains the identifying type of reheat coil specified in the furnace. The hot water and steam heating coils require specifying plant loop, branches, and connector objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the reheat coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object (Unitary Heat and Cool Furnace) itself provides the "controller" function of modulating water flow. Reheat coil type must be one of:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Desuperheater
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Reheat Coil Name

This alpha field contains the identifying name given to the furnace reheat coil.

As shown in the example below, correct specification of the heat/cool furnace requires specification of the following objects in addition to the furnace object:

#. fan ([Fan:OnOff](#fanonoff) or [Fan:ConstantVolume](#fanconstantvolume))
#. cooling coil ([Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed) or [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted))
#. heating coil ([Coil:Heating:Gas](#coilheatinggas) or [Coil:Heating:Electric](#coilheatingelectric))
#. reheat coil (optional, [Coil:Heating:Gas](#coilheatinggas), [Coil:Heating:Electric](#coilheatingelectric), or [Coil:Heating:Desuperheater](#coilheatingdesuperheater))
#. terminal unit ([AirTerminal:SingleDuct:Uncontrolled](#airterminalsingleductuncontrolled)) for each zone served by the furnace

> Note: the furnace's fan, cooling coil, heating coil and optional reheat coil must be connected in the air loop according to the configuration shown above (Figure 116) when CoolReheat is selected as the dehujmidificaiton control type. In addition, the volumetric air flow rate specified in the terminal air unit for the controlling zone should properly reflect the fractional volumetric air flow rate specified in the furnace object.

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:Unitary:Furnace:HeatCool,
        GasHeat DXAC Furnace 1, !- Name of furnace
        FanAndCoilAvailSched,   !- Availability schedule
        Air Loop Inlet Node,    !- Furnace inlet node name
        Air Loop Outlet Node,   !- Furnace outlet node name
        CycFanSchedule,         !- Supply Air Fan Operating Mode Schedule Name
        80,        !- Maximum supply air temperature from furnace heater {C}
        1.3,       !- Supply air volumetric flow rate during cooling operation {m3/s}
        1.3,       !- Supply air volumetric flow rate during heating operation {m3/s}
        0.0,       !- Design air volumetric flow rate when no heating or cooling is needed {m3/s}
        East Zone, !- Controlling zone or thermostat location
        Fan:OnOff,       !- Supply fan type
        Supply Fan 1,           !- Supply fan name
        BlowThrough,            !- Fan Placement
        Coil:Heating:Gas,       !- Heating coil type
        Furnace Heating Coil 1, !- Heating coil name
        Coil:Cooling:DX:SingleSpeed,  !- Cooling coil type
        Furnace ACDXCoil 1,     !- Cooling coil name
        None;                   !- Dehumidificatioin Control Type
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Gas,
        Furnace Heating Coil 1,         !- Coil Name
        FanAndCoilAvailSched,           !- Availability Schedule Name
        0.8,    !- Gas Burner Efficiency of the Coil
        25000,  !- Nominal Capacity of the Coil {W}
        Heating Coil Air Inlet Node,    !- Coil_Air_Inlet_Node
        Air Loop Outlet Node;           !- Coil_Air_Outlet_Node
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Coil:Cooling:DX:SingleSpeed,
        Furnace ACDXCoil 1,    !- Coil Name
        FanAndCoilAvailSched,  !- Availability Schedule
        25000,  !- Rated Total Cooling Capacity (gross) {W}
        0.75,   !- Rated SHR
        3.0,    !- Rated COP
        1.3,    !- Rated Air Volume Flow Rate {m3/s}
        DX Cooling Coil Air Inlet Node, !- Coil Air Inlet Node
        Heating Coil Air Inlet Node,    !- Coil Air Outlet Node
        WindACCoolCapFT,  !- Total Cooling Capacity Modifier Curve (function of temperature)
        WindACCoolCapFFF, !- Total Cooling Capacity Modifier Curve (function of flow fraction)
        WindACEIRFT,      !- Energy Input Ratio Modifier Curve (function of temperature)
        WindACEIRFFF,     !- Energy Input Ratio Modifier Curve (function of flow fraction)
        WindACPLFFPLR,    !- Part Load Fraction Correlation (function of part load ratio)
        CyclingFanAndCompressor;    !- Supply Air Fan Operation Mode
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Fan:OnOff,
        Supply Fan 1,                !- Fan Name
        FanAndCoilAvailSched,        !- Availability Schedule Name
        0.7,    !- Fan Total Efficiency
        600.0,  !- Delta Pressure {Pa}
        1.3,    !- Max Flow Rate {m3/s}
        0.9,    !- Motor Efficiency
        1.0,    !- Motor In Airstream Fraction
        Air Loop Inlet Node,         !- Fan_Inlet_Node
        DX Cooling Coil Air Inlet Node; !- Fan_Outlet_Node

      AirTerminal:SingleDuct:Uncontrolled,
        Zone1DirectAir,        !- Direct Air Name
        FanAndCoilAvailSched,  !- Schedule name for on/off schedule
        Zone 1 Inlet Node,     !- Zone Supply Air Node Name
        0.47;  !- Maximum air flow rate {m3/s}

      AirTerminal:SingleDuct:Uncontrolled,
        Zone2DirectAir,        !- Direct Air Name
        FanAndCoilAvailSched,  !- Schedule name for on/off schedule
        Zone 2 Inlet Node,     !- Zone Supply Air Node Name
        0.36;  !- Maximum air flow rate {m3/s}

      AirTerminal:SingleDuct:Uncontrolled,
        Zone3DirectAir,        !- Direct Air Name
        FanAndCoilAvailSched,  !- Schedule name for on/off schedule
        Zone 3 Inlet Node,     !- Zone Supply Air Node Name
        0.47;  !- Maximum air flow rate {m3/s}
~~~~~~~~~~~~~~~~~~~~

 Example of Heat/Cool Furnace Specification

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Unitary System Fan Part Load Ratio []
    HVAC,Average,Unitary System Compressor Part Load Ratio []
~~~~~~~~~~~~~~~~~~~~

#### Unitary System Fan Part Load Ratio

This output variable is the ratio of actual air mass flow rate through the furnace to the furnace's design air mass flow rate (i.e., design volumetric flow rate converted to dry air mass flow rate). For continuous fan operation mode, this variable is always 1.0 when the furnace is available (based on the availability schedule). For cycling fan/cycling coil operation mode, the actual air mass flow rate is calculated based on the ratio of the sensible heating (or cooling) load to the steady-state furnace heating (or cooling) capacity. For the cycling fan mode, the runtime fraction for the furnace fan may be different from the fan part-load ratio reported here due the part-load performance of the furnace's heating (or cooling) coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate (e.g., [Fan:OnOff](#fanonoff)).

Unitary System Compressor Part Load Ratio []

##  AirLoopHVAC:UnitaryHeatCool

The [AirLoopHVAC:UnitaryHeatCool](#airloophvacunitaryheatcool) object is the identical model to the AirLoopHAVC:Unitary:Furnace:HeatCool object. The heat/cool unitary system is a "virtual" component that consists of a fan component (OnOff or ConstantVolume), a DX cooling coil component and a Gas or Electric heating coil component as shown in Figure 117. When a draw through configuration is desired, the fan is placed directly after the heating coil. If dehumidification control is selected, a reheat coil component is also required. If the reheat coil is present and the dehumidification control type input is not specified as CoolReheat, the reheat coil will not be active,

![Schematic of Blow Through Heat/Cool Unitary System](media/schematic-of-blow-through-heatcool-unitary.jpeg)


> Note: the coil order shown here has been revised from previous versions (prior to V4.0) of Energyplus to configure the cooling coil upstream of the heating coil. This configuration provides uniformity with all unitary equipment. However, for unitary HeatCool systems that do not use a reheat coil, the heating coil can also be placed upstream of the cooling coil. This optional coil placement is retained to allow compatibility with previous versions of Energyplus. For input files developed using previous versions of Energyplus, it is recommended that the coil order be revised according to the figure above.

Links to the fan, DX cooling coil, heating coil and optional reheat coil specifications are provided in the unitary system input data syntax. In addition, the control zone name and the system design operating conditions are specified by the unitary system inputs.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the unitary system.

#### Field: Availability Schedule Name

This alpha field contains the schedule name which contains information on the availability of the unitary system to operate. A schedule value equal to 0 denotes that the unitary system must be off for that time period. A value greater than 0 denotes that the unitary system is available to operate during that time period. This schedule may be used to completely disable the unitary system as required. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Unitary System Air Inlet Node Name

This alpha field contains the unitary system inlet node name.

#### Field: Unitary System Air Outlet Node Name

This alpha field contains the unitary system outlet node name.

**Field: Supply Air Fan Operating Mode Schedule Name**

This alpha field specifies the name of the supply air fan operating mode schedule. The supply air fan operating mode may vary during the simulation based on time-of-day or with a change of season. Schedule values of 0 denote that the unitary system supply air fan and the heating or cooling coil cycle on and off together to meet the heating or cooling load (a.k.a. AUTO fan). Schedule values other than 0 denote that the supply fan runs continuously while the heating or cooling coil cycles to meet the load.

#### Field: Maximum Supply Air Temperature

This numeric field contains the design operating air outlet temperature in degrees C when the unitary system is heating. If this input field is left blank, the default value is 80 C.

#### Field: Supply Air Flow Rate During Cooling Operation

This numeric field defines the supply air flow rate leaving the unitary system in cubic meters per second when the DX cooling coil is operating. Values must be greater than 0 or this field is autosizable.

#### Field: Supply Air Flow Rate During Heating Operation

This numeric field defines the supply air flow rate leaving the unitary system in cubic meters per second when the DX heating coil and/or supplemental heater are operating. Values must be greater than 0 or this field is autosizable.

#### Field: Supply Air Flow Rate When No Cooling or Heating is Needed

This numeric field defines the supply air flow rate leaving the unitary system in cubic meters per second when neither cooling or heating is required (i.e., DX coils and supplemental heater are off but the supply air fan operates). This field is only used when the unitary system operating mode is specified as continuous fan operation. Values must be greater than or equal to zero, or this field is autosizable. If the unitary system operating mode is specified as continuous fan operation and this value is set to zero or this field is left blank, then the model assumes that the supply air flow rate when no cooling/heating is needed is equal to the supply air flow rate when the compressor was last operating (for cooling operation or heating operation).

#### Field: Controlling Zone or Thermostat Location

This alpha field contains the identifying zone name where the thermostat controlling the unitary system is located.

#### Field: Supply Fan Object Type

This alpha field contains the identifying type of supply air fan specified for the unitary system. Fan type must be **[Fan:OnOff](#fanonoff)** or **[Fan:ConstantVolume](#fanconstantvolume)**. [Fan:ConstantVolume](#fanconstantvolume) is used when the Supply Air Fan Operating Mode Schedule values are never 0 and the fan operates continuously. [Fan:OnOff](#fanonoff) is used when the fan cycles on and off with the cooling or heating coil (i.e. Supply Air Fan Operating Mode Schedule values are at times 0).

#### Field: Supply Fan Name

This alpha field contains the identifying name given to the unitary system fan.

#### Field: Fan Placement

This alpha field has two choices: **BlowThrough** or **DrawThrough**. The first choice stands for "blow through fan". This means that the unit consists of a fan followed by the DX coils and supplemental heating coil. The fan "blows through" the cooling and heating coils. The second choice stands for "draw through fan". This means that the unit consists of the DX coil(s) followed by a fan, with the supplemental heater located at the outlet of the fan. The fan "draws air through" the DX coil(s). If this field is left blank, the default is blow through.

#### Field: Heating Coil Object Type

This alpha field contains the identifying type of heating coil specified in the unitary system. The hot water and steam heating coils require specifying plant loop, branches, and connector objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the heating coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object (Unitary Heat and Cool System) itself provides the "controller" function of modulating water flow. Allowable coil types are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Heating Coil Name

This alpha field contains the identifying name given to the unitary system heating coil.

#### Field: Cooling Coil Object Type

This alpha field contains the identifying type of cooling coil specified in the unitary system. Only allowable coil types are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:SingleSpeed
    CoilSystem:Cooling:DX:HeatExchangerAssisted
    Coil:Cooling:DX:VariableSpeed
~~~~~~~~~~~~~~~~~~~~

#### Field: Cooling Coil Name

This alpha field contains the identifying name given to the unitary system cooling coil.

#### Field: Dehumidification Control Type

This alpha field contains the type of dehumidification control. The following options are valid for this field:

- **None** - meet sensible load only, no active dehumidification control
- **Multimode** - activate enhanced dehumidification mode as needed and meet sensible load. This option is used to model DX equipment with a controllable heat exchanger assisting the DX cooling coil for improved dehumidification. It is valid only with cooling coil type = [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted).
- **CoolReheat** - cool beyond the dry-bulb temperature set point as required to meet the high humidity setpoint. If cooling coil type = [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted), then the heat exchanger is assumed to always transfer energy between the cooling coil's inlet and outlet airstreams when the cooling coil is operating.

The default is **None**. For the other dehumidification control modes, the maximum humidity setpoint is used. This must be set using a **ZoneControl:Humidistat** object. When extra dehumidification is required, the system may not be able to meet the humidity setpoint if its full capacity is not adequate. If the dehumidification control type is specified as **CoolReheat**, then two additional inputs (reheat coil type and name) are also required as shown below. Although the reheat coil is required only when **CoolReheat** is selected, the optional reheat coil may be present for any of the allowed Dehumidification Control Types. If the reheat coil is present and the dehumidification control type is not specified as **CoolReheat**, the reheat coil will not be active,

#### Field: Reheat Coil Object Type

This alpha field contains the identifying type of reheat coil specified in the unitary system. The hot water and steam heating coils require specifying plant loop, branches, and connector objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the reheat coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object (Unitary Heat and Cool System) itself provides the "controller" function of modulating water flow. Reheat coil type must be one of:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Desuperheater
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Reheat Coil Name

This alpha field contains the identifying name given to the unitary system reheat coil.

As shown in the example below, correct specification of the heat/cool unitary system requires specification of the following objects in addition to the unitary system object:

#. Fan ([Fan:OnOff](#fanonoff) or [Fan:ConstantVolume](#fanconstantvolume))
#. Cooling coil ([Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed) or [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted))
#. Heating coil ([Coil:Heating:Gas](#coilheatinggas) or [Coil:Heating:Electric](#coilheatingelectric))
#. Reheat coil (optional, [Coil:Heating:Gas](#coilheatinggas), [Coil:Heating:Electric](#coilheatingelectric), or [Coil:Heating:Desuperheater](#coilheatingdesuperheater))
#. Direct air unit ([AirTerminal:SingleDuct:Uncontrolled](#airterminalsingleductuncontrolled)) for each zone served by the unitary system

> Note: the unitary system's fan, cooling coil, heating coil and optional reheat coil must be connected in the air loop according to the configuration shown above (Figure 117). In addition, the volumetric air flow rate specified in the direct air unit for the controlling zone should properly reflect the fractional volumetric air flow rate specified in the unitary system object.

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:Unitary:Furnace:HeatCool,
        GasHeat DXAC Unitary System 1, !- Name of unitary system
        FanAndCoilAvailSched,   !- Availability schedule
        Air Loop Inlet Node,    !- Unitary system inlet node name
        Air Loop Outlet Node,   !- Unitary system outlet node name
        CycFanSchedule,         !- Supply Air Fan Operating Mode Schedule Name
        80,        !- Maximum supply air temperature from unitary system heater {C}
        1.3,       !- Supply air volumetric flow rate during cooling operation {m3/s}
        1.3,       !- Supply air volumetric flow rate during heating operation {m3/s}
        0.0,       !- Design air volumetric flow rate when no heating or cooling is needed {m3/s}
        East Zone, !- Controlling zone or thermostat location
        Fan:OnOff,       !- Supply fan type
        Supply Fan 1,           !- Supply fan name
        BlowThrough,            !- Fan Placement
        Coil:Heating:Gas,       !- Heating coil type
        Unitary System Heating Coil 1,   !- Heating coil name
        Coil:Cooling:DX:SingleSpeed,  !- Cooling coil type
        Unitary System ACDXCoil 1,       !- Cooling coil name
        None;        !- High humidity control

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Coil:Heating:Gas,
        Unitary System Heating Coil 1,  !- Coil Name
        FanAndCoilAvailSched,           !- Availability Schedule Name
        0.8,    !- Gas Burner Efficiency of the Coil
        25000,  !- Nominal Capacity of the Coil {W}
        Heating Coil Air Inlet Node,    !- Coil_Air_Inlet_Node
        Air Loop Outlet Node;           !- Coil_Air_Outlet_Node

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Coil:Cooling:DX:SingleSpeed,
        Unitary System ACDXCoil 1,      !- Coil Name
        FanAndCoilAvailSched,  !- Availability Schedule
        25000,  !- Rated Total Cooling Capacity (gross) {W}
        0.75,   !- Rated SHR
        3.0,    !- Rated COP
        1.3,    !- Rated Air Volume Flow Rate {m3/s}
        DX Cooling Coil Air Inlet Node, !- Coil Air Inlet Node
        Heating Coil Air Inlet Node,    !- Coil Air Outlet Node
        WindACCoolCapFT,  !- Total Cooling Capacity Modifier Curve (function of temperature)
        WindACCoolCapFFF, !- Total Cooling Capacity Modifier Curve (function of flow fraction)
        WindACEIRFT,      !- Energy Input Ratio Modifier Curve (function of temperature)
        WindACEIRFFF,     !- Energy Input Ratio Modifier Curve (function of flow fraction)
        WindACPLFFPLR,    !- Part Load Fraction Correlation (function of part load ratio)
        CyclingFanAndCompressor;    !- Supply Air Fan Operation Mode

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Fan:OnOff,
        Supply Fan 1,                !- Fan Name
        FanAndCoilAvailSched,        !- Availability Schedule Name
        0.7,    !- Fan Total Efficiency
        600.0,  !- Delta Pressure {Pa}
        1.3,    !- Max Flow Rate {m3/s}
        0.9,    !- Motor Efficiency
        1.0,    !- Motor In Airstream Fraction
        Air Loop Inlet Node,         !- Fan_Inlet_Node
        DX Cooling Coil Air Inlet Node; !- Fan_Outlet_Node

      AirTerminal:SingleDuct:Uncontrolled,
        Zone1DirectAir,        !- Direct Air Name
        FanAndCoilAvailSched,  !- Schedule name for on/off schedule
        Zone 1 Inlet Node,     !- Zone Supply Air Node Name
        0.47;  !- Maximum air flow rate {m3/s}

      AirTerminal:SingleDuct:Uncontrolled,
        Zone2DirectAir,        !- Direct Air Name
        FanAndCoilAvailSched,  !- Schedule name for on/off schedule
        Zone 2 Inlet Node,     !- Zone Supply Air Node Name
        0.36;  !- Maximum air flow rate {m3/s}

      AirTerminal:SingleDuct:Uncontrolled,
        Zone3DirectAir,        !- Direct Air Name
        FanAndCoilAvailSched,  !- Schedule name for on/off schedule
        Zone 3 Inlet Node,     !- Zone Supply Air Node Name
        0.47;  !- Maximum air flow rate {m3/s}

~~~~~~~~~~~~~~~~~~~~

Example of Heat/Cool Unitary System Specification

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Unitary System Fan Part Load Ratio []
    HVAC,Average, Unitary System Compressor Part Load Ratio
~~~~~~~~~~~~~~~~~~~~

#### Unitary System Fan Part Load Ratio []

This output variable is the ratio of actual air mass flow rate through the unitary system to the system's design air mass flow rate (i.e., design volumetric flow rate converted to dry air mass flow rate). For continuous fan operation mode, this variable is always 1.0 when the unitary system is available (based on the availability schedule). For cycling fan/cycling coil operation mode, the actual air mass flow rate is calculated based on the ratio of the sensible heating (or cooling) load to the steady-state unitary system heating (or cooling) capacity. For the cycling fan mode, the runtime fraction for the unitary system fan may be different from the fan part-load ratio reported here due the part-load performance of the system's heating (or cooling) coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate (e.g., [Fan:OnOff](#fanonoff)).

#### Unitary System Compressor Part Load Ratio []

This output variable is the ratio of the sensible cooling load to the steady-state cooling capacity of the unitary system's DX cooling coil. The runtime fraction for the DX cooling coil compressor may be different from the compressor part-load ratio reported here due the part-load performance of the cooling coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate.

## AirLoopHVAC:UnitaryHeatPump:AirToAir

The unitary air-to-air heat pump is a "virtual" component that consists of a fan component (OnOff or ConstantVolume), a DX cooling coil component, a DX heating coil component, and a Gas or Electric supplementary heating coil component as shown in the Figure below.

 ![Schematic of EnergyPlus Unitary Air-to-Air Heat Pump (Blow Through Configuration)](media/schematic-of-energyplus-unitary-air-to-air.jpeg)


Links to the fan, DX cooling coil, DX heating coil, and supplementary heating coil specifications are provided in the heat pump's input data syntax. In addition the control zone name and the system design operating conditions are specified by the heat pump inputs.

### Inputs

####  Field: Name

This alpha field contains the identifying name for the unitary system heat pump.

#### Field: Availability Schedule Name

This alpha field contains the schedule name (ref. Schedule objects) that contains information on the availability of the heat pump to operate. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the hour. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for the hour. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Air Inlet Node Name

This alpha field contains the  name of the HVAC system node from which the heat pump draws its inlet air.

#### Field: Air Outlet Node Name

This alpha field contains the  name of the HVAC system node to which the heat pump sends its outlet air.

#### Field: Supply Air Flow Rate During Cooling Operation

This numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when the DX cooling coil is operating. Values must be greater than 0 or this field is autosizable.

#### Field: Supply Air Flow Rate During Heating Operation

This numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when the DX heating coil and/or supplemental heater are operating. Values must be greater than 0 or this field is autosizable.

#### Field: Supply Air Flow Rate When No Cooling or Heating is Needed

This numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when neither cooling or heating is required (i.e., DX coils and supplemental heater are off but the supply air fan operates). This field is only used when the heat pump operating mode is specified as continuous fan operation. Values must be greater than or equal to zero, or this field is autosizable. If the heat pump operating mode is specified as continuous fan operation and this value is set to zero or this field is left blank, then the model assumes that the supply air flow rate when no cooling/heating is needed is equal to the supply air flow rate when the compressor was last operating (for cooling operation or heating operation).

#### Field: Controlling Zone or Thermostat Location

This alpha field contains the identifying zone name where the thermostat controlling the heat pump is located.

#### Field: Supply Air Fan Object Type

This alpha field contains the identifying type of supply air fan specified for the heat pump. Fan type must be **[Fan:OnOff](#fanonoff)** or **[Fan:ConstantVolume](#fanconstantvolume)**. [Fan:ConstantVolume](#fanconstantvolume) is used when the Supply Air Fan Operating Mode Schedule values are never 0 and the fan operates continuously. [Fan:OnOff](#fanonoff) is used when the fan cycles on and off with the cooling or heating coil (i.e. Supply Air Fan Operating Mode Schedule values are at times 0).

#### Field: Supply Air Fan Name

This alpha field contains the identifying name given to the heat pump supply air fan, and should match the name specified in the corresponding fan object.

#### Field: Heating Coil Object Type

This alpha field contains the identifying type of heating coil specified in the heat pump. Heating coil type must be either [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed) or [Coil:Heating:DX:VariableSpeed](#coilheatingdxvariablespeed).

#### Field: Heating Coil Name

This alpha field contains the identifying name given to the heat pump DX heating coil, and should match the name specified in the corresponding DX heating coil object.

#### Field: Cooling Coil Object Type

This alpha field contains the identifying type of cooling coil specified in the heat pump. There are three valid choices for this field:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:SingleSpeed
    CoilSystem:Cooling:DX:HeatExchangerAssisted
    Coil:Cooling:DX:VariableSpeed
~~~~~~~~~~~~~~~~~~~~

#### Field: Cooling Coil Name

This alpha field contains the identifying name given to the heat pump cooling coil, and should match the name specified in the corresponding DX cooling coil object.

#### Field: Supplemental Heating Coil Object Type

This alpha field contains the identifying type of supplemental heating coil specified in the heat pump. The hot water and steam heating coils require specifying plant loop, branches, and connector objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the supplemental heating coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object (Airloop Air to Air Heat Pump) itself provides the "controller" function of modulating water flow. Heating coil type must be:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Supplemental Heating Coil Name

This alpha field contains the identifying name given to the heat pump supplemental heating coil, and should match the name specified in the corresponding heating coil object.

#### Field: Maximum Supply Air Temperature from Supplemental Heater

This numeric field defines the maximum allowed supply air temperature exiting the heat pump supplemental heating coil.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation

This numeric field defines the outdoor air dry-bulb temperature above which the heat pump supplemental heating coil is disabled. The temperature for this input field must be less than or equal to 21 C. If this input field is left blank, the default value is 21 C.

#### Field: Fan Placement

This alpha field has two choices: **BlowThrough** or **DrawThrough**. The first choice represents a blow through system where the supply air fan is before the DX cooling/heating coil and the supplementary heating coil. The second choice represents a draw through system where the supply air fan is between the DX cooling/heating coil and the supplementary heating coil. If this input field is left blank, the default is blow through.

**Field: Supply Air Fan Operating Mode Schedule Name**

This alpha field specifies the name of the supply air fan operating mode schedule. The supply air fan operating mode may vary during the simulation based on time-of-day or with a change of season. Schedule values of 0 denote that the unitary system supply air fan and the heating or cooling coil cycle on and off together to meet the heating or cooling load (a.k.a. AUTO fan). Schedule values other than 0 denote that the supply fan runs continuously while the heating or cooling coil cycles to meet the load.

As shown in the example below, correct specification of the air-to-air heat pump requires specification of the following objects in addition to the heat pump object:

#. Fan ([Fan:OnOff](#fanonoff) or [Fan:ConstantVolume](#fanconstantvolume))
#. Heating coil ([Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed))
#. Cooling coil ([Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed) or [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted))
#. Supplemental heating coil ([Coil:Heating:Gas](#coilheatinggas) or [Coil:Heating:Electric](#coilheatingelectric))
#. Direct air unit ([AirTerminal:SingleDuct:Uncontrolled](#airterminalsingleductuncontrolled)) for each zone served by the unitary system

#### Field: Dehumidification Control Type

This alpha input field contains the type of dehumidification control. The following options are valid for this field:

- **None** - meet sensible load only, no active dehumidification control
- **Multimode** - activate enhanced dehumidification mode as needed and meet sensible cooling load. This option is used to model DX equipment with a controllable heat exchanger assisting the DX cooling coil for improved dehumidification. It is valid only with cooling coil type = [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted).
- **CoolReheat** - cool beyond the dry-bulb temperature set point as required to meet the high humidity setpoint. The excess cooling beyond the cooling set point temperature is offset by the supplemental heating coil. If cooling coil type = [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted), then the heat exchanger is assumed to always transfer energy between the cooling coil's inlet and outlet airstreams when the cooling coil is operating.

The default is **None**. For the other dehumidification control modes, the maximum humidity setpoint is required. This must be set using a **ZoneControl:Humidistat** object. When extra dehumidification is required, the system may not be able to meet the humidity setpoint if its full capacity is not adequate. Supplemental heating coil (supplemental heating coil type and name) is a required input in AirToAir HeatPumps. The supplemental heating coil capacity must be adequate enough to meet the heating coil load and offset the excess cooling load due to extra dehumidification required to meet the high relative humidity setpoint.

> Note: the air-to-air heat pump's fan, cooling coil, heating coil and supplementary heating coil must be connected in the air loop according to the configuration shown above (Figure 118) for the blow-through fan configuration. The only other valid configuration is with a draw-through fan placement, where the fan is located between the DX heating coil and the supplementary heating coil. In addition, the volumetric air flow rate specified in the direct air unit for the controlling zone should properly reflect the fractional volumetric air flow rate specified in the heat pump object.

#### AirLoopHVAC:UnitaryHeatPump:AirToAir Example Specification

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:UnitaryHeatPump:AirToAir,
          DXAC Heat Pump 1,            ! Heat Pump name
          FanAndCoilAvailSched,        ! Heat Pump availability schedule
          Mixed Air Node,              ! Heat Pump air inlet  node
          Air Loop Outlet Node,        ! Heat Pump air outlet  node
          1.3,                  !- Supply air volumetric flow rate during cooling operation {m3/s}
          1.3,                  !- Supply air volumetric flow rate during heating operation {m3/s}
          0.0,                  !- Design air volumetric flow rate when no heating or cooling is needed {m3/s}
          East Zone,                   ! Controlling zone or thermostat location
          Fan:OnOff,            ! Supply air fan type
          Supply Fan 1,                ! Supply air fan name –- same name used in fan object
          Coil:Heating:DX:SingleSpeed,    ! Heating coil type
          Heat Pump DX Heating Coil 1, ! Heating coil name –- same name used in DX heating coil object
          Coil:Cooling:DX:SingleSpeed, !  Cooling coil type
          Heat Pump ACDXCoil 1,        ! Cooling coil name –- same name used in DX cooling coil object
          Coil:Heating:Gas,            ! Supplemental heating coil type
          Heat Pump DX Supp Heating Coil 1, ! Supplemental heating coil name–- same as in heating coil object
          50,                          ! Maximum supply air temperature from supplemental heater [C]
          21,                      ! Maximum outdoor dry-bulb temp for supplemental heating coil operation [C]
          BlowThrough,                ! Fan  placement
          CycFanSchedule,              ! Supply air fan operating mode schedule name
          CoolReheat;                  !- Dehumidification Control Type
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

       Coil:Heating:DX:SingleSpeed,
          Heat Pump DX Heating Coil 1,     ! Name of heating coil
          FanAndCoilAvailSched,            ! Heating coil schedule
          35000,                           ! Rated total heating capacity [W] (at 21.11C/8.33C)
          2.75,                            ! Rated heating COP
          1.7,                             ! Rated air flow rate [m3/s]
          Heating Coil Air Inlet Node,     ! Coil air inlet node
          SuppHeating Coil Air Inlet Node, ! Coil air outlet node
          HPACHeatCapFT,                   ! Heating capacity modifier curve (temperature,C)
          HPACHeatCapFFF,                  ! Heating capacity modifier curve (flow fraction)
          HPACHeatEIRFT,                   ! Energy input ratio modifier curve (temperature,C)
          HPACHeatEIRFFF,                  ! Energy input ratio modifier curve (flow fraction)
          HPACCoolPLFFPLR,                 ! Part load fraction modifier curve (function of part-load ratio)
          ,                         ! defrost EIR modifier curve (temp, C) not required for resistive defrost
          CyclingFanAndCompressor,                   ! Operation mode (cycling fan, cycling compressor)
          -5.0,                            ! Minimum OAT for heat pump compressor operation [C]
          5.0,                             ! Maximum outdoor dry-bulb temp for defrost operation [C]
          200.0,                           ! Crankcase heater capacity[W]
          10.0,                            ! Maximum OAT for crankcase heater operation [C]
          resistive,                       ! Defrost strategy (resistive or reverse-cycle)
          timed,                           ! Defrost control (timed or on-demand)
          0.166667,                        !Defrost time period fraction (used for timed defrost control only)
          20000;                           ! Resistive defrost heater capacity [W]
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

       Coil:Cooling:DX:SingleSpeed,
          Heat Pump ACDXCoil 1,            ! Name of cooling coil
          FanAndCoilAvailSched,            ! Availability schedule
          32000,                           ! Rated total cooling capacity [W]
          0.75,                            ! Rated sensible heat ratio
          3.0,                             ! Rated COP
          1.7,                             ! Rated air flow rate [m3/s]
          DX Cooling Coil Air Inlet Node,  ! Coil air inlet node
          Heating Coil Air Inlet Node,     ! Coil air outlet node
          HPACCoolCapFT,                   ! Cooling capacity modifier curve (temperature,C)
          HPACCoolCapFFF,                  ! Cooling capacity modifier curve (flow fraction)
          HPACCoolEIRFT,                   ! Energy input ratio modifier curve (temperature,C)
          HPACCoolEIRFFF,                  ! Energy input ratio modifier curve (flow fraction)
          HPACCoolPLFFPLR,                 ! Part load factor modifier curve (function of part-load ratio)
          CyclingFanAndCompressor;                   ! Operation mode (cycling fan, cycling compressor)

       Coil:Heating:Gas,
          Heat Pump DX Supp Heating Coil 1, ! Name of heating coil
          FanAndCoilAvailSched,             ! Availability schedule
          0.8 ,                             ! Gas Burner Efficiency of the Coil
          32000,                            ! Nominal Capacity of the Coil [W]
          SuppHeating Coil Air Inlet Node,  ! Supplementary heating coil air side inlet node
          Air Loop Outlet Node;             ! Supplementary heating coil air side outlet node

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

       Fan:OnOff,
          Supply Fan 1,                    ! Fan Name
          FanAndCoilAvailSched,            ! Fan Schedule
          0.7,                             ! Fan Total Efficiency
          300.0,                           ! Delta Pressure [N/M^2]
          1.7,                             ! Max Vol Flow Rate  [m^3/Sec]
          0.9,                             ! motor efficiency
          1.0,                             ! motor in air stream fraction
          Mixed Air Node,                  ! fan inlet node
          DX Cooling Coil Air Inlet Node;  ! fan outlet node

       AirTerminal:SingleDuct:Uncontrolled,
          Zone1DirectAir,                  ! direct air unit name
          FanAndCoilAvailSched,            ! schedule name for on/off schedule
          Zone 1 Inlet Node,               ! zone supply air node name
          0.612;                           ! maximum air flow rate, m3/s

       AirTerminal:SingleDuct:Uncontrolled,
          Zone2DirectAir,                  ! direct air unit name
          FanAndCoilAvailSched,            ! schedule name for on/off schedule
          Zone 2 Inlet Node,               ! zone supply air node name
          0.476;                           ! maximum air flow rate, m3/s

       AirTerminal:SingleDuct:Uncontrolled,
          Zone3DirectAir,                  ! direct air unit name
          FanAndCoilAvailSched,            ! schedule name for on/off schedule
          Zone 3 Inlet Node,               ! zone supply air node name
          0.612;                           ! maximum air flow rate, m3/s

~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC, Average, Unitary System Fan Part Load Ratio []
    HVAC, Average, Unitary System Compressor Part Load Ratio []
    HVAC, Average, Unitary System Dehumidification Induced Heating Demand Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Unitary System Fan Part Load Ratio []

This output variable is the ratio of actual air mass flow rate through the heat pump to the heat pump's design air mass flow rate (i.e., design volumetric flow rate converted to dry air mass flow rate). For continuous fan operation mode, this variable is always 1.0 when the furnace is available (based on the availability schedule). For cycling fan/cycling coil operation mode, the actual air mass flow rate is calculated based on the ratio of the sensible heating (or cooling) load to the steady-state heat pump heating (or cooling) capacity. For the cycling fan mode, the runtime fraction for the heat pump fan may be different from the fan part-load ratio reported here due the part-load performance of the heat pump's heating (or cooling) coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate (e.g., [Fan:OnOff](#fanonoff)).

#### Unitary System Compressor Part Load Ratio []

This output variable is the ratio of the sensible load (heating or cooling) to the steady-state capacity of the heat pump's DX heating or cooling coil. The runtime fraction for the heat pump compressor may be different from the compressor part-load ratio reported here due the part-load performance of the heating/cooling coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate.

#### Unitary System Dehumidification Induced Heating Demand Rate [W]

This output variable is the additional heating demand rate of the supplemental heating coil of an Air-to-Air heat pumps in Watts.  This additional heating demand is induced when zone air overshoots the heating setpoint due to extra dehumidification requirement to meet the high humidity setpoint. This value is always positive. This value is calculated for each HVAC system timestep, and the results are averaged for the timestep being reported.

## AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed

The multispeed air-to-air heat pump is a "virtual" component that consists of a fan component (On/Off or ConstVolume), a DX multispeed cooling coil component, a DX multispeed heating coil component, and a Gas or Electric supplemental heating coil component. This system also includes the option to use available waste energy to heat water. A schematic diagram of the air-to-air multispeed heat pump is shown below. The component connection sequence for the blow through option (shown below) from inlet to outlet is fan, cooling coil, heating coil, and supplemental heater. The connection sequence for the draw through option is cooling coil, heating coil, fan, and supplemental heater.

The main difference between this heat pump object and other EnergyPlus heat pump objects is that this object allows from two to four discrete compressor speeds for heating and cooling operation (instead of a single speed for each mode). The lowest speed is called Speed 1, and the highest speed is called Speed n (2, 3 or 4 as specified in the input syntax). This object allows a different number of speeds for cooling and heating, and each speed has an associated airflow rate. The airflow rates for the various heating speeds can be different from the airflow rates for the cooling speeds. In addition, the airflow rate when no cooling or heating is needed can also be defined. The number of cooling and heating speeds defined by the user in this heat pump object must equal the number of speeds defined in the associated coils (child objects). For example, the number of speeds for cooling defined in this heat pump object must be equal to the number of speeds defined in the associated cooling coil object.

Links to the fan, DX multispeed cooling coil, DX multispeed heating coil, and supplementary heating coil specifications are provided in the heat pump's input data syntax. In addition, the control zone name and airflow rates at the corresponding compressor speeds are specified by the heat pump syntax.

If the [ZoneControl:Thermostat:StagedDualSetpoint](#zonecontrolthermostatstageddualsetpoint) object and other zone control thermostat and humidistat are assigned to the same controlled zone in the Controlling [Zone](#zone) or Thermostat Location field, the [ZoneControl:Thermostat:StagedDualSetpoint](#zonecontrolthermostatstageddualsetpoint) object takes precedence and the stage number provided by the the [ZoneControl:Thermostat:StagedDualSetpoint](#zonecontrolthermostatstageddualsetpoint) object is used to set the speed number.

![Schematic of EnergyPlus Unitary Air-to-Air Multi Speed Heat Pump](media/schematic-of-energyplus-unitary-air-to-air-001.jpeg)


### Inputs

#### Field: Name

This alpha field contains the identifying name for the multispeed heat pump.

#### Field: Availability Schedule Name

This alpha field contains the schedule name (ref. Schedule objects) that contains information on the availability of the heat pump to operate. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for the time period. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Air Inlet Node Name

This alpha field contains the name of the HVAC system node from which the heat pump draws its inlet air.

#### Field: Air Outlet Node Name

This alpha field contains the name of the HVAC system node to which the heat pump sends its outlet air.

#### Field: Controlling Zone or Thermostat Location

This alpha field contains the identifying zone name where the thermostat controlling the multispeed heat pump is located.

#### Field: Supply Air Fan Object Type

This alpha field contains the identifying type of supply air fan specified for the heat pump. Fan type must be [Fan:OnOff](#fanonoff) or [Fan:ConstantVolume](#fanconstantvolume). [Fan:ConstantVolume](#fanconstantvolume) can only be used when the supply air fan operating mode is continuous (see field ‘Supply air fan operating mode schedule name).

#### Field: Supply Air Fan Name

This alpha field contains the identifying name given to the heat pump supply air fan, and should match the name specified in the corresponding fan object.

#### Field: Supply Air Fan Placement

This alpha field has two choices: **BlowThrough** or **DrawThrough**. The first choice stands for "blow through fan". This means that the unit consists of a fan followed by a DX multispeed cooling coil, DX multispeed heating coil, and a supplemental heating coil. The fan "blows through" the cooling and heating coils. The second choice stands for "draw through fan". This means that the unit consists of the DX cooling and heating coils followed by a fan, with the supplemental heater located at the outlet of the fan.  The fan "draws" air through the DX coils.

> **Note**: the multispeed heat pump's supply air fan, cooling coil, heating coil and supplemental heating coil must be connected according to the configuration shown above (Figure 119) for the ‘blow through' fan configuration. For the ‘draw through' fan configuration the fan must be located between the DX heating coil and the supplemental heater, whose outlet node is the system outlet node. In addition, the DX cooling coil and DX heating coil operation mode must be specified consistently with the heat pump's supply air fan operating mode (e.g., with the heat pump's supply air fan set to cycle on and off with the cooling/heating load, the DX cooling and heating coil operation mode must be CyclingFanAndCompressor). If the operation modes in the parent (heat pump) and child (coil) objects are specified differently, the operation mode in the parent object prevails.

#### Field: Supply Air Fan Operating Mode Schedule Name

This alpha field contains the schedule name (ref. Schedule objects) that contains information to control the supply air fan. Schedule values of zero mean that the supply air fan will cycle off if there is no cooling or heating load in the control zone. Non-zero schedule values mean that the supply air fan will operate continuously even if there is no cooling or heating load in the control zone. If this field is left blank, the supply air fan will operate continuously for the entire simulation period.

#### Field: Heating Coil Object Type

This alpha field contains the identifying type of heating coil specified in the heat pump.  Allowable choices for Heating coil type  are **Coil:Heating:DX:MultiSpeed**, **Coil:Heating:Electric:MultiStage**, **Coil:Heating:Gas:MultiStage**, **Coil:Heating:Water**, and  **Coil:Heating:Steam**.

#### Field: Heating Coil Name

This alpha field contains the identifying name given to the DX heating coil, and should match the name specified in the corresponding DX heating coil object.

#### Field: Minimum Outdoor Dry-Bulb Temperature for Compressor Operation

This numeric field defines the outdoor air dry-bulb temperature below which the DX heating coil turns off.  The temperature for this input field must be greater than or equal to –20 C.  If this input field is left blank, the default value is -8 C. This temperature should match the minimum compressor operating temperature specified for the multispeed heat pump's DX heating coil. This field is only needed when Heating Coil Object Type above is **Coil:Heating:DX:MultiSpeed**.

#### Field: Cooling Coil Object Type

This alpha field contains the identifying type of cooling coil specified in the heat pump.  Cooling coil type must be [Coil:Cooling:DX:MultiSpeed](#coilcoolingdxmultispeed).

#### Field: Cooling Coil Name

This alpha field contains the identifying name given to the heat pump cooling coil, and should match the name specified in the corresponding DX cooling coil object.

#### Field: Supplemental Heating Coil Object Type

This alpha field contains the identifying type of supplemental heating coil specified in the heat pump. The hot water and steam heating coils require specifying plant loop, branches, and connectors objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the supplemental heating coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object (Unitary MultiSpeed Air to Air Heat Pump) itself provides the "controller" function of modulating water flow. Heating coil type must be:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Supplemental Heating Coil Name

This alpha field contains the identifying name given to the heat pump supplemental heating coil, and should match the name specified in the corresponding heating coil object.

#### Field: Maximum Supply Air Temperature from Supplemental Heater

This numeric field defines the maximum allowed supply air temperature (in degrees C) exiting the heat pump supplemental heating coil. If the calculated supply air temperature exiting the supplemental heater exceeds this value, then it is reset to this maximum temperature. This field is autosizable.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation

This numeric field defines the outdoor air dry-bulb temperature above which the heat pump supplemental heating coil is disabled.  The temperature for this input field must be less than or equal to 21 C. If this input field is left blank, the default value is 21 C.

#### Field: Auxiliary On-Cycle Electric Power

This field defines auxiliary electrical power (W) consumed during the on-cycle period (i.e., when the cooling or heating coil is operating). The model assumes that this auxiliary power does not contribute to heating the supply air. The minimum value for this field is 0.0, and the default value is also 0.0 if the field is left blank.

#### Field: Auxiliary Off-Cycle Electric Power

This field defines auxiliary electrical power (W) consumed during the off-cycle period (i.e., when the cooling and heating coil are not operating). The model assumes that this auxiliary power does not contribute to heating the supply air. The minimum value for this field is 0.0, and the default value is also 0.0 if the field is left blank.

#### Field: Design Heat Recovery Water Flow Rate

This optional input field defines the design water flow rate used if the heat recovery option is being simulated. If this value is greater than 0.0 then a heat recovery loop must be specified and attached to the multispeed heat pump using the next 2 node fields. To determine how the heat recovery algorithm works, refer to the EnergyPlus Engineering Reference in the [AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed](#airloophvacunitaryheatpumpairtoairmultispeed) with Heat Recovery section. The units for this input value are cubic meters per second.

#### Field: Maximum Temperature for Heat Recovery

This field sets the maximum temperature (in degrees C) that this heat pump can produce for heat recovery. The idea behind this field is that the current models do not take temperatures into account for availability and they just pass Q's around the loop without a temperature limit. This temperature limit puts an upper bound on the recovered heat and limits the max temperature leaving the component.

As temperatures in the loop approach the maximum temperature, the temperature difference between the entering water and the surfaces in the piece of equipment becomes smaller. For the given heat recovery flow rate and that temperature difference the amount of heat recovered will be reduced, and eventually there will be no heat recovered when the entering water temperature is equal to the maximum temperature specified by the user in this field. The reduced amount of heat recovered will diminish if the temperature of the loop approach is the maximum temperature, and this will show up in the reporting. This allows the user to set the availability or the quality of the heat recovered for usage in other parts of the system or to heat domestic hot water supply.

#### Field: Heat Recovery Water Inlet Node Name

This alpha field contains the identifying name for the heat recovery side inlet node.

#### Field: Heat Recovery Water Outlet Node Name

This alpha field contains the identifying name for the heat recovery side outlet node.

#### Field: Supply Air Flow Rate When No Cooling or Heating is Needed

This numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when neither cooling nor heating is required (i.e., DX coils and supplemental heater are off but the supply air fan operates). This field is only used when the heat pump supply air fan is scheduled to operate continuously regardless of DX coil operation (ref. field "Supply Air Fan Operating Mode Schedule). Values must be greater than or equal to zero, or this field is autosizable. If the heat pump supply air fan is scheduled to operate continuously and the input value for this field is set to zero or this field is left blank, then the model assumes that the supply air flow rate when no cooling/heating is needed is equal to the supply air flow rate when the compressor was last operating (for cooling operation or heating operation).

#### Field: Number of Speeds for Heating

This field defines the number of heating speeds for the heat pump, and must match the number of heating speeds defined in the associated heating coil. The value for this input field defines the number of airflow rates that must be defined for heating in the field below. The minimum value for this field is one and the maximum value is four. If the Heating Coil Object Type above are **Coil:Heating:Water** or **Coil:Heating:Steam**, then this field should be 1.

#### Field: Number of Speeds for Cooling

This field defines the number of cooling speeds for the heat pump, and must match the number of cooling speeds defined in the associated DX cooling coil. The value for this input field defines the number of airflow rates that must be defined for cooling in the field below. The minimum value for this field is two and the maximum value is four.

#### Field: Speed 1 Supply Air Flow Rate During Heating Operation

This required numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when the DX heating coil and/or supplemental heater are operating at Speed 1 (lowest speed). Values must be greater than 0 or this field is autosizable.

#### Field: Speed 2 Supply Air Flow Rate During Heating Operation

This required numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when the DX heating coil and/or supplemental heater are operating at Speed 2. Values must be greater than 0 or this field is autosizable. If not autosized, the entered value must be greater or equal to the flow rate specified for heating Speed 1.

#### Field: Speed 3 Supply Air Flow Rate During Heating Operation

This numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when the DX heating coil and/or supplemental heater are operating at Speed 3. Values must be greater than 0 or this field is autosizable. If not autosized, the entered value must be greater or equal to the flow rate specified for heating Speed 2. If the ‘Number of Speeds for Heating' is less than 3, then this field can be left blank.

#### Field: Speed 4 Supply Air Flow Rate During Heating Operation

This numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when the DX heating coil and/or supplemental heater are operating at Speed 4 (high speed). Values must be greater than 0 or this field is autosizable. If not autosized, the entered value must be greater or equal to the flow rate specified for heating Speed 3. If the ‘Number of Speeds for Heating' is less than 4, then this field can be left blank.

> **Note**: When autosizable is selected for any of the supply air volumetric flow rate fields, all supply air flow fields at the different speeds must be specified as autosizable. Otherwise, a fatal error will be issued and the simulation will terminate.

#### Field: Speed 1 Supply Air Flow Rate During Cooling Operation

This required numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when the DX cooling coil is operating at Speed 1 (lowest speed). Values must be greater than 0 or this field is autosizable.

#### Field: Speed 2 Supply Air Flow Rate During Cooling Operation

This required numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when the DX cooling coil is operating at Speed 2. Values must be greater than 0 or this field is autosizable. If not autosized, the entered value must be greater or equal to the flow rate specified for cooling Speed 1.

#### Field: Speed 3 Supply Air Flow Rate During Cooling Operation

This numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when the DX cooling coil is operating at Speed 3. Values must be greater than 0 or this field is autosizable. If not autosized, the entered value must be greater or equal to the flow rate specified for cooling Speed 2. If the ‘Number of Speeds for Cooling' is less than 3, then this field can be left blank.

#### Field: Speed 4 Supply Air Flow Rate During Cooling Operation

This numeric field defines the supply air flow rate leaving the heat pump in cubic meters per second when the DX cooling coil is operating at Speed 4 (highest speed). Values must be greater than 0 or this field is autosizable. If not autosized, the entered value must be greater or equal to the flow rate specified for cooling Speed 3. If the ‘Number of Speeds for Cooling' is less than 4, then this field can be left blank.

Following is an example input for the object and its associated components.

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed,
        DXAC Heat Pump 1,        !- Name of multispeed heat pump
        FanAndCoilAvailSched,    !- Availability schedule
        Mixed Air Node,          !- Heat pump air inlet node name
        Air Loop Outlet Node,    !- Heat pump air outlet node name
        East Zone,               !- Controlling zone or thermostat location
        Fan:OnOff,               !- Supply air fan type
        Supply Fan 1,            !- Supply air fan name
        BlowThrough,             !- Supply air fan placement
        FanModeSchedule,         !- Supply air fan operating mode schedule name
        Coil:Heating:DX:MultiSpeed, Heat Pump DX Heating Coil 1,  !- Heating coil type & name
        -8.0,                    !- Minimum outdoor dry-bulb temperature for compressor operation
        Coil:Cooling:DX:MultiSpeed, Heat Pump ACDXCoil 1,    !- Cooling coil type & name
        Coil:Heating:Gas,        !- Supplemental heating coil type
        Supp Gas Heating Coil 1, !- Supplemental heating coil name
        50.0,                    !- Maximum supply air temperature from supplemental heater
        21,                      !- Maximum outdoor dry-bulb temperature for supplemental heater operation
        0,                       !- Auxiliary On-Cycle Electric Power {W}
        0,                       !- Auxiliary Off-Cycle Electric Power {W}
        0.00,                    !- Design Heat Recovery Water Flow Rate {m3/s}
        80.0,,,                    !- Maximum Temp for Heat Recovery {C} & Node names (none)
        0.2,                     !- Supply air volumetric flow rate when no cooling or heating is needed
        4,                       !- Number of speeds for heating
        4,                       !- Number of speeds for cooling
        0.4,                     !- Supply air flow rate during heating operation, Speed 1
        0.8,                     !- Supply air flow rate during heating operation, Speed 2
        1.2,                     !- Supply air flow rate during heating operation, Speed 3
        1.7,                     !- Supply air flow rate during heating operation, Speed 4
        0.4,                     !- Supply air flow rate during cooling operation, Speed 1
        0.8,                     !- Supply air flow rate during cooling operation, Speed 2
        1.2,                     !- Supply air flow rate during cooling operation, Speed 3
        1.7;                     !- Supply air flow rate during cooling operation, Speed 4
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:DX:MultiSpeed,
        Heat Pump DX Heating Coil 1,  !- Name of heat pump heating coil
        FanAndCoilAvailSched,    !- Availability Schedule
        Heating Coil Air Inlet Node,  !- Coil Air Inlet Node
        SuppHeating Coil Air Inlet Node,  !- Coil Air Outlet Node
        CyclingFanAndCompressor,           !- Supply Air Fan Operation Mode
        -8.0,                    !- Minimum Outdoor Dry-bulb Temperature for Compressor Operation {C}
        200.0,                   !- Crankcase Heater Capacity {W}
    10.0,                    !- Maximum Outdoor Dry-bulb Temperature for Crankcase Heater
                             !- Operation {C}
        HPACDefrostCAPFT,        !- Defrost energy input ratio modifier curve (temperature)
        7.22,                    !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation
        reverse-cycle,           !- Defrost Strategy
        timed,                   !- Defrost Control
        0.058333,                !- Defrost Time Period Fraction
        2000.0,                  !- Resistive Defrost Heater Capacity {W}
        No,                      !- Apply Part Load Fraction to Speeds greater than 1
        NaturalGas,              !- Fuel type
        4,                       !- Number of speeds
        7500,                    !- Rated Total Heating Capacity, Speed 1 {W}
        2.75,                    !- Rated COP, Speed 1
        0.45,                    !- Rated Air Volume Flow Rate, Speed 1 {m3/s}
        HPACHeatCapFT Speed 1,   !- Total Heating Capacity Modifier Curve, Speed 1 (temperature)
        HPACHeatCapFF Speed 1,   !- Total Heating capacity modifier curve, Speed 1 (flow fraction)
        HPACHeatEIRFT Speed 1,   !- Energy input ratio modifier curve, Speed 1 (temperature)
        HPACHeatEIRFF Speed 1,   !- Energy input ratio modifier curve, Speed 1 (flow fraction)
        HPACHeatPLFFPLR Speed 1, !- Part load fraction correlation, Speed 1 (part load ratio)
        0.2,                     !- Rated waste heat fraction of power input, Speed 1
        HAPCHeatWHFT Speed 1,    !- Waste heat modifier curve, Speed 1 (temperature)
        17500,                   !- Rated Total Heating Capacity, Speed 2 {W}
        2.75,                    !- Rated COP, Speed 2
        0.85,                    !- Rated Air Volume Flow Rate, Speed 2 {m3/s}
        HPACHeatCapFT Speed 2,   !- Total Heating Capacity Modifier Curve, Speed 2 (temperature)
        HPACHeatCapFF Speed 2,   !- Total Heating capacity modifier curve, Speed 2 (flow fraction)
        HPACHeatEIRFT Speed 2,   !- Energy input ratio modifier curve, Speed 2 (temperature)
        HPACHeatEIRFF Speed 2,   !- Energy input ratio modifier curve, Speed 2 (flow fraction)
        HPACHeatPLFFPLR Speed 2, !- Part load fraction correlation, Speed 2 (part load ratio)
        0.2,                     !- Rated waste heat fraction of power input, Speed 2
        HAPCHeatWHFT Speed 2,    !- Waste heat modifier curve, Speed 2 (temperature)
        25500,                   !- Rated Total Heating Capacity, Speed 3 {W}
        2.75,                    !- Rated COP, Speed 3
        1.25,                    !- Rated Air Volume Flow Rate, Speed 3 {m3/s}
        HPACHeatCapFT Speed 3,   !- Total Heating Capacity Modifier Curve, Speed 3 (temperature)
        HPACHeatCapFF Speed 3,   !- Total Heating capacity modifier curve, Speed 3 (flow fraction)
        HPACHeatEIRFT Speed 3,   !- Energy input ratio modifier curve, Speed 3 (temperature)
        HPACHeatEIRFF Speed 3,   !- Energy input ratio modifier curve, Speed 3 (flow fraction)
        HPACHeatPLFFPLR Speed 3, !- Part load fraction correlation, Speed 3 (part load ratio)
        0.2,                     !- Rated waste heat fraction of power input, Speed 3
        HAPCHeatWHFT Speed 3,    !- Waste heat modifier curve, Speed 3 (temperature)
        35500,                   !- Rated Total Heating Capacity, Speed 4 {W}
        2.75,                    !- Rated COP, Speed 4
        1.75,                    !- Rated Air Volume Flow Rate, Speed 4 {m3/s}
        HPACHeatCapFT Speed 4,   !- Total Heating Capacity Modifier Curve, Speed 4 (temperature)
        HPACHeatCapFF Speed 4,   !- Total Heating capacity modifier curve, Speed 4 (flow fraction)
        HPACHeatEIRFT Speed 4,   !- Energy input ratio modifier curve, Speed 4 (temperature)
        HPACHeatEIRFF Speed 4,   !- Energy input ratio modifier curve, Speed 4 (flow fraction)
        HPACHeatPLFFPLR Speed 4, !- Part load fraction correlation, Speed 4 (part load ratio)
        0.2,                     !- Rated waste heat fraction of power input, Speed 4
        HAPCHeatWHFT Speed 4;    !- Waste heat modifier curve, Speed 4 (temperature)

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      COIL:Cooling:DX:MultiSpeed,
        Heat Pump ACDXCoil 1,    !- Coil Name
        FanAndCoilAvailSched,    !- Availability Schedule
        DX Cooling Coil Air Inlet Node,  !- Coil Air Inlet Node
        Heating Coil Air Inlet Node,  !- Coil Air Outlet Node
        CyclingFanAndCompressor,           !- Supply Air Fan Operation Mode
        Outdoor Condenser Air Node, !- Condenser Air Inlet Node Name
        AirCooled,              !- Condenser Type
        ,                        !- Name of Water Storage Tank for Supply
        ,                        !- Name of Water Storage Tank for Condensate Collection
        No,                      !- Apply Part Load Fraction to Speeds greater than 1
        No,                      !- Apply Latent Degradation to Speeds greater than 1
        200.0,                   !- Crankcase Heater Capacity {W}
    10.0,                    !- Maximum Outdoor Dry-bulb Temperature for Crankcase Heater
                             !- Operation {C}
        NaturalGas,              !- Fuel type
        4,                       !- Number of speeds
        7500,                    !- Rated Total Cooling Capacity, Speed 1 (gross) {W}
        0.75,                    !- Rated SHR, Speed 1
        3.0,                     !- Rated COP, Speed 1
        0.40,                    !- Rated Air Volume Flow Rate, Speed 1 {m3/s}
        HPACCoolCapFT Speed 1,   !- Total Cooling Capacity Modifier Curve, Speed 1 (temperature)
        HPACCoolCapFF Speed 1,   !- Total Cooling Capacity Modifier Curve, Speed 1 (flow fraction)
        HPACCOOLEIRFT Speed 1,   !- Energy Input Ratio Modifier Curve, Speed 1 (temperature)
        HPACCOOLEIRFF Speed 1,   !- Energy Input Ratio Modifier Curve, Speed 1 (flow fraction)
        HPACCOOLPLFFPLR Speed 1, !- Part Load Fraction Correlation, Speed 1 (part load ratio)
        1000.0,                  !- Nominal Time for Condensate Removal to Begin, Speed 1 {s}
    1.5,                     !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent
                             !- Capacity, Speed 1 {dimensionless}
        3.0,                     !- Maximum ON/OFF Cycling Rate, Speed 1 {cycles/hr}
        45.0,                    !- Latent Capacity Time Constant, Speed 1 {s}
        0.2,                     !- Rated waste heat fraction of power input, Speed 1 {dimensionless}
        HAPCCoolWHFT Speed 1,    !- Waste heat modifier curve, Speed 1 (temperature)
        0.9,                     !- Evaporative Condenser Effectiveness, Speed 1 {dimensionless}
        0.05,                    !- Evaporative Condenser Air Volume Flow Rate, Speed 1 {m3/s}
        50,                      !- Evaporative Condenser Pump Rated Power Consumption, Speed 1 {W}
        17500,                   !- Rated Total Cooling Capacity, Speed 2 (gross) {W}
        0.75,                    !- Rated SHR, Speed 2
        3.0,                     !- Rated COP, Speed 2
        0.85,                    !- Rated Air Volume Flow Rate, Speed 2 {m3/s}
        HPACCoolCapFT Speed 2,   !- Total Cooling Capacity Modifier Curve, Speed 2 (temperature)
        HPACCoolCapFF Speed 2,   !- Total Cooling Capacity Modifier Curve, Speed 2 (flow fraction)
        HPACCOOLEIRFT Speed 2,   !- Energy Input Ratio Modifier Curve, Speed 2 (temperature)
        HPACCOOLEIRFF Speed 2,   !- Energy Input Ratio Modifier Curve, Speed 2 (flow fraction)
        HPACCOOLPLFFPLR Speed 1, !- Part Load Fraction Correlation, Speed 2 (part load ratio)
        1000.0,                  !- Nominal Time for Condensate Removal to Begin, Speed 2 {s}
    1.5,                     !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent
                             !- Capacity, Speed 2 {dimensionless}
        3.0,                     !- Maximum ON/OFF Cycling Rate, Speed 2 {cycles/hr}
        45.0,                    !- Latent Capacity Time Constant, Speed 2 {s}
        0.2,                     !- Rated waste heat fraction of power input, Speed 2 {dimensionless}
        HAPCCoolWHFT Speed 2,    !- Waste heat modifier curve, Speed 2 (temperature)
        0.9,                     !- Evaporative Condenser Effectiveness, Speed 2 {dimensionless}
        0.1,                     !- Evaporative Condenser Air Volume Flow Rate, Speed 2 {m3/s}
        60,                      !- Evaporative Condenser Pump Rated Power Consumption, Speed 2 {W}
        25500,                   !- Rated Total Cooling Capacity, Speed 3 (gross) {W}
        0.75,                    !- Rated SHR, Speed 3
        3.0,                     !- Rated COP, Speed 3
        1.25,                    !- Rated Air Volume Flow Rate, Speed 3 {m3/s}
        HPACCoolCapFT Speed 3,   !- Total Cooling Capacity Modifier Curve, Speed 3 (temperature)
        HPACCoolCapFF Speed 3,   !- Total Cooling Capacity Modifier Curve, Speed 3 (flow fraction)
        HPACCOOLEIRFT Speed 3,   !- Energy Input Ratio Modifier Curve, Speed 3 (temperature)
        HPACCOOLEIRFF Speed 3,   !- Energy Input Ratio Modifier Curve, Speed 3 (flow fraction)
        HPACCOOLPLFFPLR Speed 1, !- Part Load Fraction Correlation, Speed 3 (part load ratio)
        1000.0,                  !- Nominal Time for Condensate Removal to Begin, Speed 3 {s}
    1.5,                     !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent
                             !- Capacity, Speed 3 {dimensionless}
        3.0,                     !- Maximum ON/OFF Cycling Rate, Speed 3 {cycles/hr}
        45.0,                    !- Latent Capacity Time Constant, Speed 3 {s}
        0.2,                     !- Rated waste heat fraction of power input, Speed 3 {dimensionless}
        HAPCCoolWHFT Speed 3,    !- Waste heat modifier curve, Speed 3 (temperature)
        0.9,                     !- Evaporative Condenser Effectiveness, Speed 3 {dimensionless}
        0.2,                     !- Evaporative Condenser Air Volume Flow Rate, Speed 3 {m3/s}
        80,                      !- Evaporative Condenser Pump Rated Power Consumption, Speed 3 {W}
        35500,                   !- Rated Total Cooling Capacity, Speed 4 (gross) {W}
        0.75,                    !- Rated SHR, Speed 4
        3.0,                     !- Rated COP, Speed 4
        1.75,                    !- Rated Air Volume Flow Rate, Speed 4 {m3/s}
        HPACCoolCapFT Speed 4,   !- Total Cooling Capacity Modifier Curve, Speed 4 (temperature)
        HPACCoolCapFF Speed 4,   !- Total Cooling Capacity Modifier Curve, Speed 4 (flow fraction)
        HPACCOOLEIRFT Speed 4,   !- Energy Input Ratio Modifier Curve, Speed 4 (temperature)
        HPACCOOLEIRFF Speed 4,   !- Energy Input Ratio Modifier Curve, Speed 4 (flow fraction)
        HPACCOOLPLFFPLR Speed 1, !- Part Load Fraction Correlation, Speed 4 (part load ratio)
        1000.0,                  !- Nominal Time for Condensate Removal to Begin, Speed 4 {s}
    1.5,                     !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent
                             !- Capacity, Speed 4 {dimensionless}
        3.0,                     !- Maximum ON/OFF Cycling Rate, Speed 4 {cycles/hr}
        45.0,                    !- Latent Capacity Time Constant, Speed 4 {s}
        0.2,                     !- Rated waste heat fraction of power input, Speed 4 {dimensionless}
        HAPCCoolWHFT Speed 4,    !- Waste heat modifier curve, Speed 4 (temperature)
        0.9,                     !- Evaporative Condenser Effectiveness, Speed 4 {dimensionless}
        0.3,                     !- Evaporative Condenser Air Volume Flow Rate, Speed 4 {m3/s}
        100;                     !- Evaporative Condenser Pump Rated Power Consumption, Speed 4 {W}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Gas,
        Supp Gas Heating Coil 1,  !- Coil Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        0.8,                     !- Gas Burner Efficiency of the Coil
        45000,                   !- Nominal Capacity of the Coil {W}
        SuppHeating Coil Air Inlet Node,  !- Coil_Air_Inlet_Node
        Air Loop Outlet Node;    !- Coil_Air_Outlet_Node

    Fan:OnOff,
        Supply Fan 1,            !- Fan Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        0.7,                     !- Fan Total Efficiency
        300.0,                   !- Delta Pressure {Pa}
        1.7,                     !- Max Flow Rate {m3/s}
        0.9,                     !- Motor Efficiency
        1.0,                     !- Motor In Airstream Fraction
        Mixed Air Node,          !- Fan_Inlet_Node
        DX Cooling Coil Air Inlet Node;  !- Fan_Outlet_Node

    AirTerminal:SingleDuct:Uncontrolled,
          Zone1DirectAir,                  ! direct air unit name
          FanAndCoilAvailSched,            ! schedule name for on/off schedule
          Zone 1 Inlet Node,               ! zone supply air node name
          0.612;                           ! maximum air flow rate, m3/s

    AirTerminal:SingleDuct:Uncontrolled,
          Zone2DirectAir,                  ! direct air unit name
          FanAndCoilAvailSched,            ! schedule name for on/off schedule
          Zone 2 Inlet Node,               ! zone supply air node name
          0.476;                           ! maximum air flow rate, m3/s

    AirTerminal:SingleDuct:Uncontrolled,
          Zone3DirectAir,                  ! direct air unit name
          FanAndCoilAvailSched,            ! schedule name for on/off schedule
          Zone 3 Inlet Node,               ! zone supply air node name
          0.612;                           ! maximum air flow rate, m3/s
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Unitary System Fan Part Load Ratio []
    HVAC,Average,Unitary System Compressor Part Load Ratio []
    HVAC,Average,Unitary System DX Coil Cycling Ratio []
    HVAC,Average,Unitary System DX Coil Speed Ratio []
    HVAC,Average,Unitary System DX Coil Speed Level []
    HVAC,Average,Unitary System Electric Power[W]
    HVAC,Sum,Unitary System Electric Energy [J]
    HVAC,Average,Unitary System Total Cooling Rate [W]
    HVAC,Average,Unitary System Total Heating Rate [W]
    HVAC,Average,Unitary System Sensible Cooling Rate [W]
    HVAC,Average,Unitary System Sensible Heating Rate [W]
    HVAC,Average,Unitary System Latent Cooling Rate [W]
    HVAC,Average,Unitary System Latent Heating Rate [W]
    HVAC,Average,Unitary System Ancillary Electric Power[W]
    HVAC,Sum,Unitary System Cooling Ancillary Electric Energy [J]
    HVAC,Sum,Unitary System Heating Ancillary Electric Energy [J]
    If heat recovery is specified:
    HVAC,Average, Unitary System Heat Recovery Rate [W]
    HVAC,Average, Unitary System Heat Recovery Inlet Temperature [C]
    HVAC,Average, Unitary System Heat Recovery Outlet Temperature [C]
    HVAC,Average, Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]
    HVAC,Sum, Unitary System Heat Recovery Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Unitary System Fan Part Load Ratio []

This output variable is the ratio of actual air mass flow rate through the multispeed heat pump to the heat pump's design air mass flow rate (i.e., design volumetric flow rate converted to dry air mass flow rate) at Speed 1. For continuous fan operation mode, this variable is always 1.0 when the heat pump is available (based on the availability schedule). For cycling fan/cycling coil operation mode, the actual air mass flow rate is calculated based on the ratio of the sensible heating (or cooling) load to the steady-state heat pump heating (or cooling) capacity. For the cycling fan mode, the runtime fraction for the heat pump fan may be different from the fan part-load ratio reported here due the part-load performance of the heat pump's heating (or cooling) coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate (e.g., [Fan:OnOff](#fanonoff)). When the speed number is greater than 1, the value is 1.0.

#### Unitary System Compressor Part Load Ratio []

This output variable is the ratio of the sensible load (heating or cooling) to the steady-state capacity of the multispeed heat pump's DX heating or cooling coil at Speed 1. The runtime fraction for the heat pump compressor may be different from the compressor part-load ratio reported here due the part-load performance of the heating/cooling coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate. When the speed number is greater than 1, the value is 1.0.

#### Unitary System DX Coil Cycling Ratio []

This output variable is the ratio of the sensible load (heating or cooling) to the steady-state capacity of the multispeed heat pump's DX heating or cooling coil (Speed 1) for the entire system timestep. The value is between 0.0 and 1.0 when the heat pump is cycling on and off its lowest speed (Speed 1) and 1.0 when the multispeed heat pump operates at speeds above 1.

#### Unitary System DX Coil Speed Ratio []

This output variable is the ratio of time in a system timestep that the compressor is at rated speed between two consecutive speed numbers ( [Compressor Speed - Compressor speed at Speed i-1] / [Compressor speed at Speed i - Compressor speed at Speed i-1]). The compressor speed ratio reports (1.0 is max, 0.0 is min) and any value in between as it is averaged over the timestep. The value is 0.0 during Speed 1 operation.

The physical meaning of the speed ratio is dependent on the compressor configuration defined in the field of child coil object: Apply Part Load Fraction to Speeds greater than 1. The allowed choice is either Yes or No. When No is entered, one compressor is assumed for all speeds.  The speed ratio represents how long the higher speed runs as a fraction of the system timestep, and the lower speed runs in the rest of the system timestep. When Yes is entered, multiple compressors are assumed, and each compressor has associated speed. The speed ratio represents how long the higher speed runs as a fraction of the system timestep, and the low speed runs in a whole system timestep.

#### Unitary System DX Coil Speed Level []

This output variable reports the maximum speed needed when the heat pump operates to meet the sensible load (heating or cooling) in a system timestep. When the value is 1, the heat pump operates at Speed 1 (lowest speed). For this case the cycling ratio is between 0.0 and 1.0, while the speed ratio is 0.0. When the speed number output variable is above one, such as i, the heat pump operation is determined by the speed ratio through linear interpolation. For example, when the speed ratio is 0.4 and the speed number is 3, the heat pump operates at Speed 3 for 40% of a system timestep and at Speed 2 for 60% of a system timestep for a single compressor. For multiple compressors, the heat pump operates at Speed 3 in the 40% of a system timestep and at Speed 2 in the whole system timestep.

#### Unitary System Total Heating Rate [W]

This output field is the total (enthalpy) heat addition rate of the multispeed heat pump to the zones it is serving in Watts. This value is calculated using the enthalpy difference of the heat pump outlet air and inlet air streams, and the air mass flow rate through the heat pump. This value is calculated for each HVAC system timestep being simulated, and the results (enthalpy addition only) are averaged for the timestep being reported.

#### Unitary System Total Cooling Rate [W]

This output field is the total (enthalpy) heat extraction rate of the multispeed heat pump from the zones it is serving in Watts. This value is calculated using the enthalpy difference of the heat pump outlet air and inlet air streams, and the air mass flow rate through the heat pump. This value is calculated for each HVAC system timestep being simulated, and the results (enthalpy extraction only) are averaged for the timestep being reported.

#### Unitary System Sensible Heating Rate [W]

This output field reports the sensible heat addition rate of the multispeed heat pump to the zones it is serving in Watts. This value is calculated using the enthalpy difference of the heat pump outlet air and inlet air streams at a constant humidity ratio, and the air mass flow rate through the heat pump. This value is calculated for each HVAC system timestep being simulated, and the results (heating only) are averaged for the timestep being reported.

#### Unitary System Sensible Cooling Rate [W]

This output field reports the moist air sensible heat extraction rate of the multispeed heat pump from the zones it is serving in Watts. This value is calculated using the enthalpy difference of the heat pump outlet air and inlet air streams at a constant humidity ratio, and the air mass flow rate through the heat pump. This value is calculated for each HVAC system timestep being simulated, and the results (cooling only) are averaged for the timestep being reported.

#### Unitary System Latent Heating Rate [W]

This output field is the latent heat addition (humidification) rate of the multispeed heat pump in Watts. This value is calculated as the difference between the total energy rate and the sensible energy rate provided by the multispeed heat pump. This value is calculated for each HVAC system timestep being simulated, and the results (latent heat addition only) are averaged for the timestep being reported.

#### Unitary System Latent Cooling Rate [W]

This output field is the latent heat extraction (dehumidification) rate of the multispeed heat pump in Watts. This value is calculated as the difference between the total energy rate and the sensible energy rate provided by the multispeed heat pump. This value is calculated for each HVAC system timestep being simulated, and the results (latent heat extraction only) are averaged for the timestep being reported.

#### Unitary System Electric Power [W]

This output field is the electricity consumption rate of the multispeed heat pump in Watts. The consumption includes electricity used by the DX coils (including crankcase heater if the fuel type is electricity), fans (indoor supply air fan and the condenser fans associated with the DX coil[s]), auxiliary power during on and off period, and the supplemental heating coil (if electric). This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported. Any non-electric energy use is not reported by the heat pump object but is reported in the associated coil objects as appropriate.

#### Unitary System Electric Energy [J]

This output field is the electricity consumption of the multispeed heat pump in Joules for the timestep being reported. The consumption includes electricity used by the DX compressor (including crankcase heater if the fuel type is electricity), fans (indoor supply air fan and the condenser fans associated with the DX coil[s]), auxiliary power during on and off period, and the supplemental heating coil (if electric). This value is calculated for each HVAC system timestep being simulated, and the results are summed for the timestep being reported. Any non-electric energy use is not reported by the heat pump object but is reported in the associated coil objects as appropriate.

#### Unitary System Ancillary Electric Power [W]

This output field is the average auxiliary electricity consumption rate (including both on-cycle and off-cycle) in Watts for the timestep being reported.

#### Unitary System Cooling Ancillary Electric Energy [J]

This is the auxiliary electricity consumption in Joules for the timestep being reported. This is the auxiliary electricity consumption during periods when the heat pump is providing cooling (DX cooling coil is operating). This output is also added to a meter with Resource Type = Electricity, End Use Key =Cooling, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Unitary System Heating Ancillary Electric Energy [J]

This is the auxiliary electricity consumption in Joules for the timestep being reported. This is the auxiliary electricity consumption during periods when the heat pump is providing heating (DX heating coil is operating). This output is also added to a meter with Resource Type = Electricity, End Use Key =Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Unitary System Heat Recovery Inlet Temperature [C]

#### Unitary System Heat Recovery Outlet Temperature [C]

#### Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]

These outputs are the heat recovery inlet and outlet temperatures and water mass flow rate for multispeed heat pumps with heat recovery.

#### Unitary System Heat Recovery Rate [W]

#### Unitary System Heat Recovery Energy [J]

For multispeed heat pumps with heat recovery, these outputs are the recoverable energy rate (in Watts) and energy (in Joules).

## AirLoopHVAC:Unitary:Furnace:HeatOnly

The EnergyPlus furnace is a "virtual" component that consists of a fan component  (OnOff or ConstantVolume) and a Gas or Electric heating coil component. The blow through furnace configuration is shown in the Figure below.

![Schematic of Blow Through Furnace Model](media/schematic-of-blow-through-furnace-model.png)


Links to the fan and heating coil specifications are provided in the furnace input data syntax. In addition the control zone name and the furnace design operating conditions are specified by the furnace inputs.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the furnace.

#### Field: Availability Schedule Name

This alpha field contains the schedule name which contains information on the availability of the furnace to operate. A schedule value equal to 0 denotes that the furnace must be off for that time period. A value greater than 0 denotes that the furnace is available to operate during that time period. This schedule may be used to completely disable the unitary system as required. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Furnace Inlet Node Name

This alpha field contains the furnace inlet node name.

#### Field: Furnace Outlet Node Name

This alpha field contains the furnace outlet node name.

**Field: Supply Air Fan Operating Mode Schedule Name**

This alpha field specifies the name of the supply air fan operating mode schedule. The supply air fan operating mode may vary during the simulation based on time-of-day or with a change of season. Schedule values of 0 denote that the furnace supply air fan and the heating coil cycle on and off together to meet the heating load (a.k.a. AUTO fan). Schedule values other than 0 denote that the supply fan runs continuously while the heating coil cycles to meet the load.

#### Field: Maximum Supply Air Temperature

This numeric field contains the design operating furnace air outlet temperature in degrees C when the furnace is heating. If this input field is left blank, the default value is 80 C.

#### Field: Supply Air Flow Rate

This numeric field contains the design volumetric flow rate of the furnace in cubic meters per second. This volumetric flow rate should match the flow rate specified for the furnace fan.

#### Field: Controlling Zone or Thermostat Location

This alpha field contains the identifying zone name where the thermostat controlling the furnace is located.

#### Field: Supply Fan Object Type

This alpha field contains the identifying type of supply air fan specified for the furnace. Fan type must be **[Fan:OnOff](#fanonoff)** or **[Fan:ConstantVolume](#fanconstantvolume)**. [Fan:ConstantVolume](#fanconstantvolume) is used when the Supply Air Fan Operating Mode Schedule values are never 0 and the fan operates continuously. [Fan:OnOff](#fanonoff) is used when the fan cycles on and off with the cooling or heating coil (i.e. Supply Air Fan Operating Mode Schedule values are at times 0).

#### Field: Supply Fan Name

This alpha field contains the identifying name given to the furnace fan.

#### Field: Fan Placement

This alpha field has two choices: **BlowThrough** or **DrawThrough**. The first choice stands for "blow through fan". This means that the unit consists of a fan followed by the heating coil. The fan "blows through" the heating coil. The second choice stands for "draw through fan". This means that the unit consists of the heating coil followed by a fan. The fan "draws air through" the heating coil. If this field is left blank, the default is blow through.

#### Field: Heating Coil Object Type

This alpha field contains the identifying type of heating coil specified in the furnace. The hot water and steam heating coils require specifying plant loop, branches, and connectors objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the heating coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object (Unitary Heat Only Furnace) itself provides the "controller" function of modulating water flow. Heating coil type must be:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Heating Coil Name

This alpha field contains the identifying name given to the furnace heating coil.

As shown in the example below, correct specification of the furnace requires specification of the following objects in addition to the furnace object:

#. fan ([Fan:OnOff](#fanonoff) or [Fan:ConstantVolume](#fanconstantvolume))
#. heating coil ([Coil:Heating:Gas](#coilheatinggas) or [Coil:Heating:Electric](#coilheatingelectric))
#. direct air unit ([AirTerminal:SingleDuct:Uncontrolled](#airterminalsingleductuncontrolled)) for each zone served by the furnace

> Note: the furnace's fan and heating coil must be connected in the air loop according to the configuration shown above (Figure 120) when a blow through fan configuration is specified. If a draw through fan is used, the fan is located down stream of the heating coil. In addition, the volumetric air flow rate specified in the direct air unit for the controlling zone should properly reflect the fractional volumetric air flow rate specified in the furnace object.

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:Unitary:Furnace:HeatOnly,
        Gas Furnace 1,        !- Name
        FanAndCoilAvailSched, !- Availability Schedule Name
        Air Loop Inlet Node,  !- Furnace Air Inlet Node Name
        Air Loop Outlet Node, !- Furnace Air Outlet Node Name
        CycFanSchedule,       !- Supply Air Fan Operating Mode Schedule Name
        80,                   !- Maximum Supply Air Temperature {C}
        1.3,                  !- Supply Air Flow Rate {m3/s}
        East Zone,            !- Controlling Zone or Thermostat Location
        Fan:OnOff,            !- Supply Fan Object Type
        Supply Fan 1,         !- Supply Fan Fame
        BlowThrough,          !- Fan Placement
        Coil:Heating:Gas,     !- Heating Coil Object Type
        Furnace Coil;         !- Heating Coil Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Coil:Heating:Gas,
        Furnace Coil,          !- Coil Name
        FanAndCoilAvailSched,  !- Availability Schedule Name
        0.8,                   !- Gas Burner Efficiency of the Coil
        20000,                 !- Nominal Capacity of the Coil {W}
        Heating Coil Air Inlet Node,  !- Coil_Air_Inlet_Node
        Air Loop Outlet Node,         !- Coil_Air_Outlet_Node
        ,                      !- Coil_Temp_Setpoint_Node
        100,                   !- Parasitic Electric Load {W}
        PLFCurveforGasFurnace, !- Part load fraction correlation (function of part load ratio)
        10;                    !- Parasitic Gas Load {W}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Curve:Cubic,
        PLFCurveforGasFurnace, !- Name
        0.8,  !- Coefficient1 Constant
        0.2,  !- Coefficient2 x
        0.0,  !- Coefficient3 x**2
        0.0,  !- Coefficient4 x**3
        0,  !- Minimum Value of x
        1;  !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Fan:OnOff,
        Supply Fan 1,          !- Fan Name
        FanAndCoilAvailSched,  !- Availability Schedule Name
        0.7,    !- Fan Total Efficiency
        600.0,  !- Delta Pressure {Pa}
        1.3,    !- Max Flow Rate {m3/s}
        0.9,    !- Motor Efficiency
        1.0,    !- Motor In Airstream Fraction
        Air Loop Inlet Node,          !- Fan_Inlet_Node
        Heating Coil Air Inlet Node;  !- Fan_Outlet_Node
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      AirTerminal:SingleDuct:Uncontrolled,
        Zone1DirectAir,       !- Direct Air Name
        FanAndCoilAvailSched, !- Schedule name for on/off schedule
        Zone 1 Inlet Node,    !- Zone Supply Air Node Name
        0.47;   !- Maximum air flow rate {m3/s}

      AirTerminal:SingleDuct:Uncontrolled,
        Zone2DirectAir,       !- Direct Air Name
        FanAndCoilAvailSched, !- Schedule name for on/off schedule
        Zone 2 Inlet Node,    !- Zone Supply Air Node Name
        0.36;   !- Maximum air flow rate {m3/s}

      AirTerminal:SingleDuct:Uncontrolled,
        Zone3DirectAir,       !- Direct Air Name
        FanAndCoilAvailSched, !- Schedule name for on/off schedule
        Zone 3 Inlet Node,    !- Zone Supply Air Node Name
        0.47;   !- Maximum air flow rate {m3/s}

~~~~~~~~~~~~~~~~~~~~

Example of Heat-Only Furnace Specification

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Unitary System Fan Part Load Ratio []
~~~~~~~~~~~~~~~~~~~~

#### Unitary System Fan Part Load Ratio []

This output variable is the ratio of actual air mass flow rate through the furnace to the furnace's design air mass flow rate (i.e., design volumetric flow rate converted to dry air mass flow rate). For continuous fan operation mode, this variable is always 1.0 when the furnace is available (based on the availability schedule). For cycling fan/cycling coil operation mode, the actual air mass flow rate is calculated based on the ratio of the sensible heating load to the furnace heating capacity. For the cycling fan mode, the runtime fraction for the furnace fan may be different from the fan part-load ratio reported here due the part-load performance of the furnace's heating coil (delay at start-up to reach steady-state heating output). In general, runtime fractions are reported by individual components where appropriate (e.g., [Fan:OnOff](#fanonoff)).

## AirLoopHVAC:UnitaryHeatOnly

The [AirLoopHVAC:UnitaryHeatOnly](#airloophvacunitaryheatonly) is identical to the [AirLoopHVAC:Unitary:Furnace:HeatOnly](#airloophvacunitaryfurnaceheatonly) model. The heat-only unitary system is a "virtual" component that consists of a fan component (OnOff or ConstantVolume) and a Gas or Electric heating coil component. The blow through unitary system configuraion is shown in the Figure below.

![Schematic of Blow Through Heat-Only Unitary System](media/schematic-of-blow-through-heat-only-unitary.png)


Links to the fan and heating coil specifications are provided in the unitary system input data syntax. In addition, the control zone name and the unitary system design operating conditions are specified by the unitary system syntax.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the unitary system.

#### Field: Availability Schedule Name

This alpha field contains the schedule name which contains information on the availability of the unitary system to operate. A schedule value equal to 0 denotes that the unitary system must be off for that time period. A value greater than 0 denotes that the unitary system is available to operate during that time period. This schedule may be used to completely disable the unitary system as required. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Unitary System Air Inlet Node Name

This alpha field contains the unitary system inlet node name.

#### Field: Unitary System Air Outlet Node Name

This alpha field contains the unitary system outlet node name.

**Field: Supply Air Fan Operating Mode Schedule Name**

This alpha field specifies the name of the supply air fan operating mode schedule. The supply air fan operating mode may vary during the simulation based on time-of-day or with a change of season. Schedule values of 0 denote that the furnace supply air fan and the heating coil cycle on and off together to meet the heating load (a.k.a. AUTO fan). Schedule values other than 0 denote that the supply fan runs continuously while the heating coil cycles to meet the load.

#### Field: Maximum Supply Air Temperature

This numeric field contains the design  air outlet temperature in degrees C when the unitary system is heating. If this input field is left blank, the default value is 80 C.

#### Field: Supply Air Flow Rate

This numeric field contains the design volumetric flow rate of the unitary system in cubic meters per second. This volumetric flow rate should match the flow rate specified for the unitary system fan.

#### Field: Controlling Zone or Thermostat Location

This alpha field contains the identifying zone name where the thermostat controlling the unitary system is located.

#### Field: Supply Fan Object Type

This alpha field contains the identifying type of supply air fan specified for the unitary system. Fan type must be **[Fan:OnOff](#fanonoff)** or **[Fan:ConstantVolume](#fanconstantvolume)**. [Fan:ConstantVolume](#fanconstantvolume) is used when the Supply Air Fan Operating Mode Schedule values are never 0 and the fan operates continuously. [Fan:OnOff](#fanonoff) is used when the fan cycles on and off with the cooling or heating coil (i.e. Supply Air Fan Operating Mode Schedule values are at times 0).

#### Field: Supply Fan Name

This alpha field contains the identifying name given to the unitary system fan.

#### Field: Fan Placement

This alpha field has two choices: **BlowThrough** or **DrawThrough**. The first choice stands for "blow through fan". This means that the unit consists of a fan followed by the heating coil. The fan "blows through" the heating coil. The second choice stands for "draw through fan". This means that the unit consists of the heating coil followed by a fan. The fan "draws air through" the heating coil. If this field is left blank, the default is blow through.

#### Field: Heating Coil Object Type

This alpha field contains the identifying type of heating coil specified in the unitary system. The hot water and steam heating coils require specifying plant loop, branches, and connectors objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the heating coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object (Unitary Heat Only) itself provides the "controller" function of modulating water flow. Heating coil type must be:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Heating Coil Name

This alpha field contains the identifying name given to the unitary system heating coil.

As shown in the example below, correct specification of the heat-only unitary system requires specification of the following objects in addition to the unitary system object:

#. fan ([Fan:OnOff](#fanonoff) or [Fan:ConstantVolume](#fanconstantvolume))
#. heating coil ([Coil:Heating:Gas](#coilheatinggas) or [Coil:Heating:Electric](#coilheatingelectric))
#. direct air unit ([AirTerminal:SingleDuct:Uncontrolled](#airterminalsingleductuncontrolled)) for each zone served by the furnace

> Note: the unitary system's fan and heating coil must be connected in the air loop according to the configuration shown above (Figure 121) when a blow through fan configuration is specified. If a draw through fan is used, the fan is located down stream of the heating coil. In addition, the volumetric air flow rate specified in the direct air unit for the controlling zone should properly reflect the fractional volumetric air flow rate specified in the unitary system object.

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:UnitaryHeatOnly,
        Gas Unitary System 1, !- Name
        FanAndCoilAvailSched, !- Availability Schedule Name
        Air Loop Inlet Node,  !- Unitary System Air Inlet Node Name
        Air Loop Outlet Node, !- Unitary System Air Outlet Node Name
        CycFanSchedule,       !- Supply Air Fan Operating Mode Schedule Name
        80,                   !- Maximum Supply Air Temperature {C}
        1.3,                  !- Supply Air Flow Rate {m3/s}
        East Zone,            !- Controlling Zone or Thermostat Location
        Fan:OnOff,            !- Supply Fan Object Type
        Supply Fan 1,         !- Supply Fan Name
        BlowThrough,          !- Fan Placement
        Coil:Heating:Gas,     !- Heating Coil Object Type
        Unitary System Heating Coil; !- Heating Coil Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Coil:Heating:Gas,
        Unitary System Heating Coil, !- Coil Name
        FanAndCoilAvailSched,        !- Availability Schedule Name
        0.8,                         !- Gas Burner Efficiency of the Coil
        20000,                       !- Nominal Capacity of the Coil {W}
        Heating Coil Air Inlet Node, !- Coil_Air_Inlet_Node
        Air Loop Outlet Node,        !- Coil_Air_Outlet_Node
        ,                            !- Coil_Temp_Setpoint_Node
        100,                         !- Parasitic Electric Load {W}
        PLFCurveforUnitarySystem,    !- Part load fraction correlation (function of part load ratio)
        10;                          !- Parasitic Gas Load {W}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Curve:Cubic,
        PLFCurveforUnitarySystem,  !- Name
        0.8,  !- Coefficient1 Constant
        0.2,  !- Coefficient2 x
        0.0,  !- Coefficient3 x**2
        0.0,  !- Coefficient4 x**3
        0,  !- Minimum Value of x
        1;  !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Fan:OnOff,
        Supply Fan 1,          !- Fan Name
        FanAndCoilAvailSched,  !- Availability Schedule Name
        0.7,    !- Fan Total Efficiency
        600.0,  !- Delta Pressure {Pa}
        1.3,    !- Max Flow Rate {m3/s}
        0.9,    !- Motor Efficiency
        1.0,    !- Motor In Airstream Fraction
        Air Loop Inlet Node,          !- Fan_Inlet_Node
        Heating Coil Air Inlet Node;  !- Fan_Outlet_Node
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      AirTerminal:SingleDuct:Uncontrolled,
        Zone1DirectAir,       !- Direct Air Name
        FanAndCoilAvailSched, !- Schedule name for on/off schedule
        Zone 1 Inlet Node,    !- Zone Supply Air Node Name
        0.47;   !- Maximum air flow rate {m3/s}

      AirTerminal:SingleDuct:Uncontrolled,
        Zone2DirectAir,       !- Direct Air Name
        FanAndCoilAvailSched, !- Schedule name for on/off schedule
        Zone 2 Inlet Node,    !- Zone Supply Air Node Name
        0.36;   !- Maximum air flow rate {m3/s}

      AirTerminal:SingleDuct:Uncontrolled,
        Zone3DirectAir,       !- Direct Air Name
        FanAndCoilAvailSched, !- Schedule name for on/off schedule
        Zone 3 Inlet Node,    !- Zone Supply Air Node Name
        0.47;   !- Maximum air flow rate {m3/s}
~~~~~~~~~~~~~~~~~~~~

Example of Heat-Only Unitary System Specification

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Unitary System Fan Part Load Ratio []
~~~~~~~~~~~~~~~~~~~~

#### Unitary System Fan Part Load Ratio []

This output variable is the ratio of actual air mass flow rate through the unitary system to the unitary system's design air mass flow rate (i.e., design volumetric flow rate converted to dry air mass flow rate). For continuous fan operation mode, this variable is always 1.0 when the unitary system is available (based on the availability schedule). For cycling fan/cycling coil operation mode, the actual air mass flow rate is calculated based on the ratio of the sensible heating load to the unitary system heating capacity. For the cycling fan mode, the runtime fraction for the unitary system fan may be different from the fan part-load ratio reported here due the part-load performance of the unitary system's heating coil (delay at start-up to reach steady-state heating output). In general, runtime fractions are reported by individual components where appropriate (e.g., [Fan:OnOff](#fanonoff)).

## AirLoopHVAC:UnitaryHeatPump:WaterToAir

The unitary water-to-air heat pump is similar to the unitary air-to-air heat pump except water is used on the source side. Links to the fan, WaterToAirHeatPump cooling coil, WaterToAirHeatPump heating coil, and supplementary heating coil specifications are provided in the heat pump's input data syntax. The heat pump switches between cooling and heating depending on the zone's demand. The load side (air) of the unitary water-to-air heat pump consists of an On/Off fan component, a WaterToAirHeatPump cooling coil component, a WaterToAirHeatPump heating coil component, and a Gas, Electric, Steam, or Hot Water supplemental heating coil component. The source side (water) of the heat pump is connected to a condenser loop with a heat exchanger (ground heat exchanger or other type) or a plant loop with a heating source such as a boiler and a cooling source such as a chiller or cooling tower. The diagram below shows the setup and connection of the heat pump for the source side and load side for a ground heat exchanger configuration. Note that on the load side, the WaterToAirHeatPump cooling coil must always be placed before the WaterToAirHeatPump heating coil.

There are three type of WaterToAirHeatPump coil models available:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:WaterToAirHeatPump:ParameterEstimation
    Coil:Heating:WaterToAirHeatPump:ParameterEstimation
    Coil:Cooling:WaterToAirHeatPump:EquationFit
    Coil:Heating:WaterToAirHeatPump:EquationFit
    Coil:Cooling:WatertoAirHeatPump:VariableSpeedEquationFit
    Coil:Heating:WatertoAirHeatPump:VariableSpeedEquationFit
~~~~~~~~~~~~~~~~~~~~

![Water to Air Heat Pump Schematic for a BlowThrough Configuration with Ground Heat Exchanger](media/water-to-air-heat-pump-schematic-for-a.jpeg)


In addition, the control zone name and the system design operating conditions are specified by the heat pump inputs.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the unitary system heat pump.

#### Field: Availability Schedule Name

This alpha field contains the schedule name (ref. Schedule objects) that contains information on the availability of the heat pump to operate. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for the time period. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Air Inlet Node Name

This alpha field contains the name of the HVAC system node from which the heat pump draws its inlet air.

#### Field: Air Outlet Node Name

This alpha field contains the name of the HVAC system node to which the heat pump sends its outlet air.

#### Field: Supply Air Flow Rate 

This numeric field contains the design volumetric flow rate through the heat pump in cubic meters per second. This volume flow rate is only used when the cooling and heating coil object type is Coil:\*:WaterToAirHeatPump:ParameterEstimation. Although a value greater than 0 is required (input cannot be blank or 0), this input is not used for the EquationFit model. Instead, the supply air flow rate is determined by the input in the corresponding Coil:\*:WaterToAirHeatPump:EquationFit objects.

#### Field: Controlling Zone or Thermostat Location

This alpha field contains the identifying zone name where the thermostat controlling the heat pump is located.

#### Field: Supply Air Fan Object Type

This alpha field contains the identifying type of supply air fan specified in the heat pump. Fan type must be [Fan:OnOff](#fanonoff).

#### Field: Supply Air Fan Name

This alpha field contains the identifying name given to the heat pump supply air fan, and should match the name specified in the corresponding fan object.

#### Field: Heating Coil Object Type

This alpha field contains the identifying type of heating coil specified in the heat pump. Heating coil types are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:WaterToAirHeatPump:ParameterEstimation
    Coil:Heating:WaterToAirHeatPump:EquationFit
    Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit
~~~~~~~~~~~~~~~~~~~~

#### Field: Heating Coil Name

This alpha field contains the identifying name given to the WaterToAirHeatPump heating coil, and should match the name specified in the corresponding WaterToAirHeatPump heating coil object.

#### Field: Heating Convergence

This numeric value allows the user to determine how close the air side has to be controlled. Lower the value of convergence better the control of air side conditions and less the zone temperature fluctuations. However in a poorly designed system, a lower convergence might result in warning errors which are caused due to the iteration limit for run time fraction calculation is limited to 20.

#### Field: Cooling Coil Object Type

This alpha field contains the identifying type of cooling coil specified in the heat pump. Cooling coil types are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:WaterToAirHeatPump:ParameterEstimation
    Coil:Cooling:WaterToAirHeatPump:EquationFit
    Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit
~~~~~~~~~~~~~~~~~~~~

#### Field: Cooling Coil Name

This alpha field contains the identifying name given to the WaterToAirHeatPump cooling coil, and should match the name specified in the corresponding WaterToAirHeatPump cooling coil object.

#### Field: Cooling Convergence

This numeric value allows the user to determine how close the air side has to be controlled. Lower the value of convergence better the control of air side conditions and less the zone temperature fluctuations. However in a poorly designed system, a lower convergence might result in warning errors which are caused due to the iteration limit for run time fraction calculation is limited to 20.

#### Field: Maximum Cycling Rate

This numeric field contains the maximum on-off cycling rate for the compressor, which occurs at 50% run time fraction. Suggested values are shown below (Henderson et al. 1999):

![](media/image282.png)\


#### Field: Heat Pump Time Constant

This numeric field contains the time constant for the cooling coil's capacity to reach steady state after startup. Suggested values are shown below (Henderson et al. 1999):

![](media/image283.png)\


#### Field: Fraction of On-Cycle Power Use

This numeric field contains the fraction of on-cycle power use to adjust the part load fraction based on the off-cycle power consumption due to crankcase heaters, controls, fans, and etc. Suggested value values are below (Henderson et al. 1999):

![](media/image284.png)\


#### Field: Heat Pump Fan Delay Time

This numeric field contains the time delay for the heat pump supply air fan to shut off after the compressor cycles off in seconds. This value can be obtained from the manufacturer or the heat pump catalog. Enter a value of zero when the heat pump's fan operating mode is continuous. Suggested value is 60 seconds.

#### Field: Supplemental Heating Coil Object Type

This is the object type of the supplemental heating coil. The hot water and steam heating coils require specifying plant loop, branches, and connectors objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the supplemental heating coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object (AirLoop Unitary Water to Air Heat Pump) itself provides the "controller" function of modulating water flow. The valid choices are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Supplemental Heating Coil Name

This alpha field contains the identifying name given to the supplemental heating coil, and should match the name specified in the corresponding supplemental heating coil object.

#### Field: Maximum Supply Air Temperature from Supplemental Heater

This numeric field defines the maximum allowed supply air temperature exiting the heat pump supplemental heating coil in degrees Celsius.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation

This numeric field defines the outdoor air dry-bulb temperature in degrees Celsius above which the heat pump supplemental heating coil is disabled. The temperature for this input field must be less than or equal to 21C. If this input field is left blank, the default value is 21C.

#### Field: Outdoor Dry-Bulb Temperature Sensor Node Name

This alpha field specifies the name of the outdoor node which controls the operation of the supplemental heating coil. If this field is left blank, the outdoor temperature is based solely on the weather data. If this field is not blank, the node name specified must also be listed in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor temperature from the weather data. Alternately, the node name must be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor temperature is taken directly from the weather data.

#### Field: Fan Placement

This alpha field has two choices: **BlowThrough** or **DrawThrough**. The first choice represents a blow through system where the supply air fan is before the WaterToAirHeatPump cooling/heating coil and the supplementary heating coil. The second choice represents a draw through system where the supply fan is between the WaterToAirHeatPump cooling/heating coil and the supplementary heating coil. If this input field is left blank, the default is blow through.

#### Field: Supply Air Fan Operating Mode Schedule Name

This alpha field specifies the name of the supply air fan operating mode schedule. The supply air fan operating mode may vary during the simulation based on time-of-day or with a change of season. Schedule values of 0 denote that the supply air fan and the heating/cooling coil cycle on and off together to meet the heating or cooling load (a.k.a. AUTO fan). Schedule values other than 0 denote that the supply air fan runs continuously while the heating or cooling coil cycles to meet the load. If this field is left blank, the model assumes the supply air fan cycles with the heating or cooling coil throughout the simulation period.

As shown in the example below, correct specification of the water-to-air heat pump requires specification of the following objects in addition to the [AirLoopHVAC:UnitaryHeatPump:WaterToAir](#airloophvacunitaryheatpumpwatertoair) object:

- On/Off fan
- WaterToAirHeatPump cooling coil
- WaterToAirHeatPump heating coil
- Supplementary heating coil
- Direct air unit for each zone served by the heat pump
- Condenser demand branches

It should be noted that the volumetric air flow rate specified in the direct air unit for the controlling zone should properly reflect the fractional volumetric air flow rate specified in the heat pump object.

#### Field: Dehumidification Control Type

This alpha input field contains the type of dehumidification control. The following options are valid for this field:

- **None** - meet sensible load only, no active dehumidification control
- **CoolReheat** - cool beyond the dry-bulb temperature set point as required to meet the high humidity setpoint. The excess cooling beyond the cooling set point temperature is offset by the supplemental heating coil. 

The default is **None**. For **CoolReheat** dehumidification control modes, the maximum humidity setpoint is required. This must be set using a **ZoneControl:Humidistat** object. When extra dehumidification is required, the system may not be able to meet the humidity setpoint if its full capacity is not adequate. Supplemental heating coil (supplemental heating coil type and name) is a required input in WaterToAir HeatPumps. When dehumidification control is active the heating and the reheat load due to extra dehumidification are met with supplemetal heating coil. The supplemental heating coil capacity must be adequate enough to meet the heating coil load and offset the excess cooling load due to  extra dehumidification. The dehumidification control type CoolReheat works only with [Coil:Cooling:WaterToAirHeatPump:EquationFit](#coilcoolingwatertoairheatpumpequationfit) cooling coil type.

#### Field: Heat Pump Coil Water Flow Mode

This field specifies the way in which water flow through the heat pump coils will be modeled.  This field is only used when WatertoAirHeatPump:EquationFit coils are used. There are three options:

- Cycling
- Constant
- CyclingOnDemand

**Cycling** varies water flow through the coil based on the heat pump Part Load Ratio.  This control method is appropriate for modeling heat pumps that are outfitted with a soleniod valve which allows water to flow through the coil only when the compressor is active. This is the default for EnergyPlus V8 and later.

**Constant** provides a constant water flow regardless of heat pump operation.  Remember that EnergyPlus has two coils (a heating coil and a cooling coil) to approximate the operation of one coil that can operate in either heating mode or cooling mode.  Therefore, when the water flow mode is constant, there will be full flow through either the heating coil or the cooling coil, but not both at the same time.

**ConstantOnDemand** provides full flow through the coil whenever there is a load.  When there is no load, there is zero flow through the coil.  This control strategy represents the way EnergyPlus modeled heat pump water flow prior to [Version](#version) 8.

Following is an example of IDF usage:

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:UnitaryHeatPump:WaterToAir,
        DXAC Heat Pump 1,        !- Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        Mixed Air Node,          !- Air Inlet Node Name
        Air Loop Outlet Node,    !- Air Outlet Node Name
        2,                       !- Supply Air Flow Fate {m3/s}
        East Zone,               !- Controlling Zone or Thermostat Location
        Fan:OnOff,        !- Supply Air Fan Object Type
        Supply Fan 1,            !- Supply Air Fan Name
        Coil:Heating:WaterToAirHeatPump:ParameterEstimation,  !- Heating Coil Object Type
        Heat Pump Heating Mode,  !- Heating Coil Name
        0.001,                   !- Heating Convergence
        Coil:Cooling:WaterToAirHeatPump:ParameterEstimation,  !- Cooling Coil Object Type
        Heat Pump Cooling Mode,  !- Cooling Coil Name
        0.001,                   !- Cooling Convergence
        2.5, !- Maximum Cycling Rate {cycles/hr}
        60, !- Heat Pump Time Constant {s}
        0.01, !- Fraction of On-Cycle Power Use
        60,    !- Heat Pump Fan Delay Time {s}
        Coil:Heating:Gas,        !- Supplemental Heating Coil Object Type
        Heat Pump DX Supp Heating Coil 1,  !- Supplemental Heating Coil Name
        50,                      !- Maximum Supply Air Temperature from Supplemental Heater {C}
        21,                      !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}
        Outside Air Inlet Node, !- Outdoor Dry-Bulb Temperature Sensor Node Name
        BlowThrough,             !- Fan Placement
        CyclingFanSch,           !- Supply Air Fan Operating Mode Schedule Name
        CoolReheat;              !- Dehumidification Control Type
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Schedule:Compact,
        CyclingFanSch,           !- Name
        Fraction,                !- Schedule Type Limits Name
        Through: 12/31,          !- Field 1
        For: AllDays,            !- Field 2
        Until: 24:00,            !- Field 3
        0.0;                     !- Field 4

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Coil:Cooling:WaterToAirHeatPump:ParameterEstimation,
        Heat Pump Cooling Mode,  !- Name
        Scroll,       !- Compressor Type
        R22,      !- Refrigerant Type
        0.0015,                  !- Design Source Side Flow Rate {m3/s}
        38000,  !- Nominal Cooling Coil Capacity {W)
        0,     !- Nominal Time for Condensate Removal to Begin {s}
        0,      !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity
        3000000,                 !- High Pressure CutOff {Pa}
        0,                       !- Low Pressure CutOff {Pa}
        Water to Air Heat Pump Source Side1 Inlet Node,  !- Water Inlet Node Name
        Water to Air Heat Pump Source Side1 Outlet Node,  !- Water Outlet Node Name
        Cooling Coil Air Inlet Node,  !- Air Inlet Node Name
        Heating Coil Air Inlet Node,  !- Air Outlet Node Name
        3.78019E+03,!- Parameter 1 {W/K}
        2.80303E+03,!- Parameter 2 {W/K}
        7.93591E-01,!- Parameter 3 {C}
        1.91029E+03,!- Parameter 4 {W}
        2.66127E+00,!- Parameter 5
        1.06009E-02,!- Parameter 6
        1.65103E+00,!- Parameter 7
        9.73887E-03,!- Parameter 8
        1.04563E+03;!- Parameter 9

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Coil:Heating:WaterToAirHeatPump:ParameterEstimation,
        Heat Pump HEATING Mode,  !- Name
        Scroll,       !- Compressor Type
        R22,      !- Refrigerant Type
        0.0015,                  !- Design Source Side Flow Rate {m3/s}
        38000,                   !- Nominal Heating Coil Capacity {W}
        3000000,                 !- High Pressure CutOff
        0,                       !- Low Pressure CutOff {Pa}
        Water to Air Heat Pump Source Side2 Inlet Node,  !- Water Inlet Node Name
        Water to Air Heat Pump Source Side2 Outlet Node,  !- Water Outlet Node Name
        Heating Coil Air Inlet Node,  !- Air Inlet Node Name
        SuppHeating Coil Air Inlet Node,  !- Air Outlet Node Name
        3.91379E+03,  !- Parameter 1 {W/K}
        5.94753E-01,  !- Parameter 2 {C}
        2.49945E+03,  !- Parameter 3 {W}
        8.68734E-01,  !- Parameter 4
        7.23595E-03,  !- Parameter 5
        3.69126E+00,  !- Parameter 6
        1.75701E-05,  !- Parameter 7
        3.65348E+03;  !- Parameter 8

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Coil:Heating:Gas,
        Heat Pump DX Supp Heating Coil 1,  !- Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        0.8,                     !- Gas Burner Efficiency
        32000,                   !- Nominal Capacity {W}
        SuppHeating Coil Air Inlet Node,  !- Air Inlet Node Name
        Air Loop Outlet Node;    !- Air Outlet Node Name

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      BRANCH,
        Gshp Cooling Condenser Branch,  !- Name
        0,  !- Maximum Flow Rate {m3/s}
         , !- Pressure Drop Curve Name
        Coil:Cooling:WaterToAirHeatPump:ParameterEstimation,  !- Component 1 Object Type
        Heat Pump Cooling Mode,  !- Component 1 Name
        Water to Air Heat Pump Source Side1 Inlet Node, !- Component 1 Inlet Node Name
        Water to Air Heat Pump Source Side1 Outlet Node, !- Component 1 Outlet Node Name
        ACTIVE;  !- Component 1 Branch Control Type

      BRANCH,
        Gshp Heating Condenser Branch,  !- Name
        0,  !- Maximum Flow Rate {m3/s}
         , !- Pressure Drop Curve Name
        Coil:Heating:WaterToAirHeatPump:ParameterEstimation,  !- Component 1 Object Type
        Heat Pump Heating Mode,  !- Component 1 Name
        Water to Air Heat Pump Source Side2 Inlet Node,  !- Component 1 Inlet Node Name
        Water to Air Heat Pump Source Side2 Outlet Node,  !- Component 1 Outlet Node Name
        ACTIVE;  !- Component 1 Branch Control Type

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Fan:OnOff,
        Supply Fan 1,          !- Name
        FanAndCoilAvailSched,  !- Availability Schedule Name
        0.7,                   !- Fan Total Efficiency
        300.0,                 !– Pressure Rise {Pa}
        2.0,                   !- Maximum Flow Rate {m3/s}
        0.9,                   !- Motor Efficiency
        1.0,                   !- Motor In Airstream Fraction
        Mixed Air Node,        !- Air Inlet_Node Name
        Cooling Coil Air Inlet Node;  !- Air Outlet Node Name

      AirTerminal:SingleDuct:Uncontrolled,
        Zone1DirectAir,        !- Name
        FanAndCoilAvailSched,  !- Availability Schedule Name
        Zone 1 Inlet Node,     !- Zone Supply Air Node Name
        0.7;                   !- Maximum Air Flow Rate {m3/s}

      AirTerminal:SingleDuct:Uncontrolled,
        Zone2DirectAir,        !- Name
        FanAndCoilAvailSched,  !- Availability Schedule Name
        Zone 2 Inlet Node,     !- Zone Supply Air Node Name
        0.6;                   !- Maximum Air Flow Rate {m3/s}

      AirTerminal:SingleDuct:Uncontrolled,
        Zone3DirectAir,        !- Name
        FanAndCoilAvailSched,  !- Availability Schedule Name
        Zone 3 Inlet Node,     !- Zone Supply Air Node Name
        0.7;                   !- Maximum Air Flow Rate {m3/s}

~~~~~~~~~~~~~~~~~~~~

### Outputs

Energy use reporting for the water-to-air heat pump is documented under the heat pump coil object types:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:WaterToAirHeatPump:ParameterEstimation
    Coil:Heating:WaterToAirHeatPump:ParameterEstimation
    Coil:Cooling:WaterToAirHeatPump:EquationFit
    Coil:Heating:WaterToAirHeatPump:EquationFit
~~~~~~~~~~~~~~~~~~~~

The heat pump *demand* as well as the compressor and fan part-load ratios may be obtained with the output variables shown below.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Unitary System Requested Sensible Cooling Rate [W]
    HVAC,Average, Unitary System Requested Latent Cooling Rate [W]
    HVAC,Average, Unitary System Requested Heating Rate [W]
    HVAC,Average, Unitary System Compressor Part Load Ratio []
    HVAC,Average, Unitary System Fan Part Load Ratio
    HVAC,Average, Unitary System Dehumidification Induced Heating Demand Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Unitary System Requested Sensible Cooling Rate [W]

This output variable is the sensible cooling requested from the zone thermostat in watts. This value is calculated using the unitary heat pump outlet air and zone conditions, the specific heat of the zone air, and the supply air mass flow rate entering/leaving the system. This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

#### Unitary System Requested Latent Cooling Rate [W]

This output variable is the latent cooling requested from the zone humidistat in watts. This value is calculated using the unitary heat pump outlet air and zone conditions, the heat of vaporization of water at the current zone conditions, and the supply air mass flow rate entering/leaving the system. This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

#### Unitary System Requested Heating Rate [W]

This output variable is the sensible heating requested from the zone thermostat in watts. This value is calculated using the unitary heat pump outlet air and zone conditions, the specific heat of the zone air, and the supply air mass flow rate entering/leaving the system. This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

#### Unitary System Compressor Part Load Ratio []

This output variable is the ratio of actual load on the unitary system to the unitary system's steady state output.  This ratio is based on the nominal capacity of the unit.

#### Unitary System Fan Part Load Ratio []

This output variable is the ratio of actual air mass flow rate through the unitary system to the unitary system's design air mass flow rate (i.e., design volumetric flow rate converted to dry air mass flow rate). For continuous fan operation mode, this variable is always 1.0 when the unitary system is available (based on the availability schedule).

#### Unitary System Dehumidification Induced Heating Demand Rate [W]

This output variable is the additional heating demand rate of the supplemental heating coil of a Water-to-Air heat pumps in Watts.  This additional heating demand is induced when zone air overshoots the heating setpoint due to extra dehumidification requirement to meet the high humidity setpoint. This value is always positive. This value is calculated for each HVAC system timestep, and the results are averaged for the timestep being reported.

## AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass

The changeover-bypass variable air volume (CBVAV) unitary system is a compound object made up of other components. Each CBVAV system consists of an outdoor air mixer, direct expansion (DX) cooling coil, heating coil, and a supply air fan as shown in the figures below. [Zone](#zone) thermostats and terminal units are required in each zone served by this system. The terminal units are specific to this system type and are either [AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat](#airterminalsingleductvavheatandcoolreheat) or [AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat](#airterminalsingleductvavheatandcoolnoreheat). A zone humidistat and single zone max humidity set point manager may also be specified to help control high humidity levels. These individual components are described elsewhere in this document. The CBVAV unitary system object coordinates the operation of these components and is modeled as a type of air loop equipment (Ref. [AirLoopHVAC](#airloophvac)).

![Schematic of a CBVAV unitary system (draw through fan placement)](media/schematic-of-a-cbvav-unitary-system-draw.jpeg)


![Schematic of a CBVAV unitary system (blow through fan placement)](media/schematic-of-a-cbvav-unitary-system-blow.jpeg)


Links to the CBVAV system's supply air fan, coils, and outdoor air mixer specifications are provided in the object's input syntax. Additional inputs include system and outdoor air flow rates during heating and cooling operation, the priority control mode, and dehumidification control type. A description of each input field for the CBVAV unitary system compound object is provided below.

### Inputs

#### Field: Name

This alpha field defines a unique user-assigned name for an instance of a changeover-bypass VAV system. Any reference to this system by another object will use this name.

#### Field: Availability Schedule Name

This alpha field defines the name of the schedule (ref: Schedule) that denotes whether the system operates during a given time period. A schedule value equal to 0 denotes that the system must be off for that time period, and a schedule value greater than 0 denotes that the system is available to operate during that time period. This schedule may be used to completely disable the system (all of its coils and the supply air fan) as required. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: System Air Flow Rate During Cooling Operation

This numeric field defines the air flow rate through the system (i.e., through the fan and heating/cooling coils) in cubic meters per second when the DX cooling coil is operating. Values must be greater than 0, or this field is autosizable.

#### Field: System Air Flow Rate During Heating Operation

**This numeric field defines the air flow rate through the system (i.e., through the fan and heating/cooling coils) in cubic meters per second when the heating coil is operating. Values must be greater than 0, or this field is autosizable.**

#### Field: System Air Flow Rate When No Cooling or Heating is Needed

**This numeric field defines the air flow rate through the system (i.e., through the fan and heating/cooling coils) in cubic meters per second when neither cooling nor heating is required (i.e., the DX cooling coil and heating coil are off but the supply air fan operates). Values must be greater than or equal to zero, or this field is autosizable. This field is only used when the unitary system's supply air fan operating mode is specified as continuous fan operation (Ref. Field:** Supply air fan operating mode schedule name). If the system's supply air fan operating mode is specified as continuous fan operation and this value is set to zero or the field is left blank, then the model assumes that the system air flow rate when no heating/cooling is needed is equal to the system air flow rate when the coils were last operating (for cooling operation or heating operation).

#### Field: Outdoor Air Flow Rate During Cooling Operation

This numeric field defines the outdoor air flow rate through the system (i.e., through the Outdoor air Mixer's Outside_Air_Stream_Node) in cubic meters per second when the DX cooling coil is operating. Values must be greater than or equal to 0, or this field is autosizable. Note that the outdoor air flow rate during cooling operating can be changed during the simulation using a multiplier schedule (Ref. Field: Outdoor air volumetric flow rate multiplier schedule name). For any simulation timestep, the outdoor air flow rate during cooling operation cannot exceed the system air volumetric flow rate during cooling operation.

#### Field: Outdoor Air Flow Rate During Heating Operation

This numeric field defines the outdoor air flow rate through the system (i.e., through the Outdoor air Mixer's Outside_Air_Stream_Node) in cubic meters per second when the heating coil is operating. Values must be greater than or equal to 0, or this field is autosizable. Note that the outdoor air flow rate during heating operating can be changed during the simulation using a multiplier schedule (Ref. Field: Outdoor air volumetric flow rate multiplier schedule name). For any simulation timestep, the outdoor air flow rate during heating operation cannot exceed the system air volumetric flow rate during heating operation.

#### Field: Outdoor Air Flow Rate When No Cooling or Heating is Needed

This numeric field defines the outdoor air flow rate through the system (i.e., through the Outdoor air Mixer's Outside_Air_Stream_Node) in cubic meters per second when neither cooling nor heating is required (i.e., the DX cooling coil and heating coil are off but the supply air fan operates). Values must be greater than or equal to 0, or this field is autosizable. Note that the outdoor air flow rate when no cooling/heating is needed can be changed during the simulation using a multiplier schedule (Ref. Field: Outdoor air volumetric flow rate multiplier schedule name). For any simulation timestep, the outdoor air flow rate when no cooling/heating is needed cannot exceed the system air volumetric flow rate when no cooling/heating is needed. This field is only used when the unitary system's supply air fan operating mode is specified as continuous fan operation (Ref. Field: Supply air fan operating mode schedule name). If the system's supply air fan operating mode is specified as continuous fan operation and this value is set to zero or the field is left blank, then the model assumes that the outdoor air flow rate when no cooling/heating is needed is equal to the outdoor air flow rate when the coils were last operating (for cooling operation [i.e. Outdoor air volumetric flow rate during cooling operation] or heating operation [i.e. Outdoor air volumetric flow rate during heating operation]) and this field is not used.

#### Field: Outdoor Air Flow Rate Multiplier Schedule Name

This alpha field defines the name of a schedule (ref: Schedule) that contains multipliers for the outdoor air volume flow rates (heating, cooling, no heating/cooling). Schedule values must be from zero to 1. If this field is left blank, then the model assumes that the outdoor air multiplier is 1 for the entire simulation period.

#### Field: Air Inlet Node Name

This alpha field defines the name of the HVAC system node from which the system draws its inlet air.

#### Field: Bypass Duct Mixer Node Name

**This alpha field defines the name of the HVAC system node where the bypass air mixes with** the unitary system's inlet air. This name should match the name of the Return Air Stream Node Name for the [OutdoorAir:Mixer](#outdoorairmixer) associated with this system. This node name must be different from the system's air inlet node name.

#### Field: Bypass Duct Splitter Node Name

**This alpha field defines the name of the HVAC system node where the conditioned air is split into bypass air and supply air leaving the system (e.g., delivered to the terminal units). This splitter air node name should match the outlet node name for the last component (furthest downstream) in this unitary system. For blow through fan placement, the splitter air node is the outlet node of the heating coil. For draw through fan placement, the splitter node is the outlet node of the supply air fan.**

#### Field: Air Outlet Node Name

This alpha field defines the name of the HVAC system node to which the system sends its outlet air.

#### Field: Outdoor Air Mixer Object Type

This field specifies the type of outdoor air mixer used by this CBVAV unitary system. The outdoor air mixer component is part of the CBVAV unitary compound object. The only available outdoor air mixer type is:

~~~~~~~~~~~~~~~~~~~~

    OutdoorAir:Mixer
~~~~~~~~~~~~~~~~~~~~

#### Field: Outdoor Air Mixer Name

This alpha field defines the name of an outdoor air mixer component that composes part of the CBVAV system. The name of the outdoor air mixer's Return_Air_Stream_Node should match the bypass duct mixer node name, and be different from the CBVAV system's air inlet node name. The Mixed Air Node Name of the outdoor air mixer should be the same as the CBVAV system's supply fan inlet air node (for blow through fan placement) or the system's DX cooling coil inlet node (for draw through fan placement).

#### Field: Supply Air Fan Object Type

This alpha field defines the type of fan used by this unitary system. The only valid choices are **Fan:OnOff** and **Fan:ConstantVolume**. The input requirements for these fan objects are described elsewhere in this document.

#### Field: Supply Air Fan Name

This alpha field defines the name of the fan component that composes part of this unitary system. Note that the fan component's maximum flow rate should be greater than or equal to the largest system air volumetric flow rate specified for this unitary system (heating, cooling, and no heating/cooling). In addition, the fan's inlet air node should be the same as the outdoor air mixer's Mixed Air Node (for blow through fan placement) or the heating coil's outlet node (for draw through fan placement). The fan outlet air node should be the same as the DX cooling coil's air inlet node (for blow through fan placement) or the system's bypass duct splitter node (for draw through fan placement).

#### Field: Supply Air Fan Placement

This alpha field defines the placement of the supply air fan within this unitary system. The only valid choices are **BlowThrough** and **DrawThrough**. With blow through placement, the supply air fan is located immediately upstream of the system's cooling coil. With draw through placement, the supply air fan is located immediately downstream of the heating coil.

#### Field: Supply Air Fan Operating Mode Schedule Name

This alpha field defines the name of a schedule that specifies the supply air fan operating mode during the simulation. A schedule value of 0 denotes the fan cycles off when no cooling or heating is required, and any other value denotes that the fan runs continuously regardless of the need for heating or cooling. If this field is left blank, the model assumes continuous supply air fan operation for the entire simulation period.

#### Field: Cooling Coil Object Type

This alpha field defines the type of cooling coil used by this unitary system. There are three valid choices for this field:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:SingleSpeed
    CoilSystem:Cooling:DX:HeatExchangerAssisted
    Coil:Cooling:DX:TwoStageWithHumidityControlMode
~~~~~~~~~~~~~~~~~~~~

The input requirements for these cooling coil objects are described elsewhere in this document.

#### Field: Cooling Coil Name

This alpha field defines the name of the cooling coil used by this unitary system, and this name should match the name specified in the corresponding cooling coil object.

#### Field: Heating Coil Object Type

This alpha field defines the type of heating coil used by this unitary system. The hot water and steam heating coils require specifying plant loop, branches, and connector objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the heating coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object (CBVAV Unitary System) itself provides the "controller" function of modulating water flow. The valid choices are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:DX:SingleSpeed
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

The input requirements for these heating coil objects are described elsewhere in this document.

#### Field: Heating Coil Name

This alpha field defines the name of the heating coil used by this unitary system, and this name should match the name specified in the corresponding heating coil object.

#### Field: Priority Control Mode

This choice field defines the cooling or heating priority control mode for the unitary system. Valid choices are:

~~~~~~~~~~~~~~~~~~~~

    CoolingPriority
    HeatingPriority
    ZonePriority
~~~~~~~~~~~~~~~~~~~~

If CoolingPriority is selected, the system operates to meet the cooling load if any zone served by this system (air loop) requires cooling. If no zones require cooling, then the system operates in heating mode if needed. If HeatingPriority is selected, the system operates to meet the heating load if any zone requires heating. If no zones require heating, then the system operates in cooling mode if needed. If ZonePriority is selected, the system operates based on the maximum number of zones requiring either heating or cooling. If the number of zones requiring cooling is greater than the number of zones requiring heating, then the system operates in cooling mode. If the number of zones requiring heating is greater than the number of zones requiring cooling, then the system operates in heating mode. If the number of zones requiring cooling equals the number of zones requiring heating, then the largest combined load (i.e., the sum of the cooling loads for zones requiring cooling compared to the sum of the heating loads for zones that require heating) sets the cooling or heating operating mode for the system during that simulation timestep.

#### Field: Minimum Outlet Air Temperature During Cooling Operation

**This numeric field defines the minimum outlet air temperature leaving the system when the unit is operating to provide cooling. Values are specified in degrees Celsius and must be greater than 0. The default value is 8**C. This value must be less than or equal to the maximum outlet air temperature during heating operation.

#### Field: Maximum Outlet Air Temperature During Heating Operation

**This numeric field defines the maximum outlet air temperature leaving the system when the unit is operating to provide heating. Values are specified in degrees Celsius and must be greater than 0. The default value is 50**C. This value must be greater than or equal to the minimum outlet air temperature during cooing operation.

#### Field: Dehumidification Control Type

This alpha field contains the type of dehumidification control. The following options are valid for this field:

- **None** - meet sensible load only, no active dehumidification control
- **Multimode** - activate enhanced dehumidification mode as needed and meet sensible load. This option is used to model DX equipment with a controllable heat exchanger assisting the DX cooling coil for improved dehumidification. It is valid only with cooling coil type = [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode).
- **CoolReheat** - cool beyond the dry-bulb temperature set point as required to meet the high humidity setpoint. It is valid only with cooling coil type = [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode).

The default is **None**. For the other dehumidification control modes, the maximum humidity setpoint on the CBVAV system's air outlet node is used. This must be set using a **ZoneControl:Humidistat** and one of:

- **SetpointManager:SingleZone:Humidity:Maximum**
- **SetpointManager:MultiZone:Humidity:Maximum**
- **SetpointManager:MultiZone:MaximumHumidity:Average**

objects. When extra dehumidification is required, the system may not be able to meet the humidity setpoint if its full capacity is not adequate.

As shown in the example below, correct specification of the CBVAV unitary system requires specification of the following objects in addition to the [AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass](#airloophvacunitaryheatcoolvavchangeoverbypass) object:

#. outdoor air mixer ([OutdoorAir:Mixer](#outdoorairmixer))
#. fan ([Fan:OnOff](#fanonoff) or [Fan:ConstantVolume](#fanconstantvolume))
#. cooling coil ([Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed), [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted), or [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode))
#. heating coil ([Coil:Heating:Gas](#coilheatinggas), [Coil:Heating:Electric](#coilheatingelectric), or [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed))
#. terminal unit for each zone being served by this system ([AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat](#airterminalsingleductvavheatandcoolreheat) or [AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat](#airterminalsingleductvavheatandcoolnoreheat))

> Note: The fan, heating coil, cooling coil, and outdoor air mixer must be connected in the air loop according to the configurations shown above (Figure 123 and Figure 124).

~~~~~~~~~~~~~~~~~~~~

    AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass,
        GasHeat CBVAV System,  !- Name of unitary system
        FanAndCoilAvailSched,  !- Availability schedule name
        1.8,                   !- System air volumetric flow rate during cooling operation {m3/s}
        1.7,                   !- System air volumetric flow rate during heating operation {m3/s}
        1.6,                   !- System air volumetric flow rate when no cooling or heating is needed {m3/s}
        0.32,                  !- Outdoor air volumetric flow rate during cooling operation {m3/s}
        0.3,                   !- Outdoor air volumetric flow rate during heating operation {m3/s}
        0.27,                  !- Outdoor air volumetric flow rate when no cooling or heating is needed {m3/s}
        Outdoor Air Multiplier Schedule, !- Outdoor air volumetric flow rate multiplier schedule name
        Air Loop Inlet Node,     !- Air inlet node name
        Mixer Inlet Node,        !- Bypass duct mixer node name
        Heating Coil Air Outlet Node,    !- Bypass duct splitter node name
        Air Loop Outlet Node,    !- Air outlet node name
        OutdoorAir:Mixer,        !- Outdoor Air Mixer Object Type
        Outdoor air Mixer,       !- Outdoor air mixer name
        Fan:OnOff,        !- Supply air fan type
        Supply Fan 1,            !- Supply air fan name
        BlowThrough,            !- Supply air fan placement
        Fan OpMode Schedule,     !- Supply air fan operating mode schedule name
        Coil:Cooling:DX:TwoStageWithHumidityControlMode,  !- Cooling coil type
        ACDXCoil 2,              !- Cooling coil name
        Coil:Heating:Gas,        !- Heating coil type
        Furnace Heating Coil 1,  !- Heating coil name
        CoolingPriority,        !- Priority control mode
        10.0,                    !- Minimum outlet air temperature during cooling operation {C}
        50.0,                    !- Maximum outlet air temperature during heating operation {C}
        None;                    !- Dehumidification control type
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    OutdoorAir:Mixer,
        Outdoor air Mixer,       !- Name
        Mixed Air Node,          !- Mixed Air Node Name
        Outdoor air Inlet Node,  !- Outdoor Air Stream Node
        Relief Air Outlet Node,  !- Relief Air Stream Node Name
        Mixer Inlet Node;        !- Return Air Stream Node Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Fan:OnOff,
        Supply Fan 1,            !- Fan Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        0.7,                     !- Fan Total Efficiency
        600.0,                   !- Delta Pressure {Pa}
        1.8,                     !- Max Flow Rate {m3/s}
        0.9,                     !- Motor Efficiency
        1.0,                     !- Motor In Airstream Fraction
        Mixed Air Node,          !- Fan_Inlet_Node
        DX Cooling Coil Air Inlet Node;  !- Fan_Outlet_Node
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:TwoStageWithHumidityControlMode,
        ACDXCoil 2,              !- Coil Name
        FanAndCoilAvailSched,    !- Availability Schedule
        DX Cooling Coil Air Inlet Node, !- Coil Air Inlet Node
        Heating Coil Air Inlet Node,    !- Coil Air Outlet Node
        ,                        !- Crankcase Heater Capacity {W}
        ,                        !- Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation {C}
        2,                       !- Number of Capacity Stages
        0,                       !- Number of Enhanced Dehumidification Modes
        CoilPerformance:DX:Cooling,  !- Normal Mode Stage 1 Coil Performance Object Type
        ACDXCoil 2 Standard Mode-Stage 1,  !- Normal Mode Stage 1 Coil Performance Object Name
        CoilPerformance:DX:Cooling,  !- Normal Mode Stage 1+2 Coil Performance Object Type
        ACDXCoil 2 Standard Mode-Stage 1&2;  !- Normal Mode Stage 1+2 Coil Performance Object Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Gas,
        Furnace Heating Coil 1,  !- Coil Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        0.8,                     !- Gas Burner Efficiency of the Coil
        35000,                   !- Nominal Capacity of the Coil {W}
        Heating Coil Air Inlet Node,  !- Coil_Air_Inlet_Node
        Heating Coil Air Outlet Node; !- Coil_Air_Outlet_Node
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat,
        Zone 1 VAV System,       !- Name of System
        FanAndCoilAvailSched,    !- System Availability schedule
        Zone 1 Reheat Air Inlet Node,  !- DAMPER Air Outlet Node
        Zone 1 VAV Inlet Node,   !- UNIT Air Inlet Node
        0.583,                   !- Maximum air flow rate {m3/s}
        0.25,                    !- Zone Minimum Air Flow Fraction
        ,                        !- Control node
        Coil:Heating:Electric,   !- Reheat Component Object
        Reheat Coil Zone 1,      !- Name of Reheat Component
        0.0,                     !- Max Reheat Water Flow {m3/s}
        0.0,                     !- Min Reheat Water Flow {m3/s}
        Zone 1 Reheat Air Outlet Node,  !- UNIT Air Outlet Node
        0.001;                   !- Convergence Tolerance
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat,
        Zone 2 VAV System,       !- Name of System
        FanAndCoilAvailSched,    !- System Availability schedule
        Zone 2 Reheat Air Inlet Node,  !- DAMPER Air Outlet Node
        Zone 2 VAV Inlet Node,   !- UNIT Air Inlet Node
        0.583,                   !- Maximum air flow rate {m3/s}
        0.25,                    !- Zone Minimum Air Flow Fraction
        ,                        !- Control node
        Coil:Heating:Electric,   !- Reheat Component Object
        Reheat Coil Zone 2,      !- Name of Reheat Component
        0.0,                     !- Max Reheat Water Flow {m3/s}
        0.0,                     !- Min Reheat Water Flow {m3/s}
        Zone 2 Reheat Air Outlet Node,  !- UNIT Air Outlet Node
        0.001;                   !- Convergence Tolerance
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat,
        Zone 3 VAV System,       !- Name of System
        FanAndCoilAvailSched,    !- System Availability schedule
        Zone 3 Reheat Air Outlet Node,  !- UNIT Air Outlet Node
        Zone 3 VAV Inlet Node,   !- UNIT Air Inlet Node
        0.584,                   !- Maximum air flow rate {m3/s}
        0.25;                    !- Zone Minimum Air Flow Fraction
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Unitary System Total Heating Rate [W]
    HVAC,Sum,Unitary System Total Heating Energy [J]
    HVAC,Average,Unitary System Total Cooling Rate [W]
    HVAC,Sum,Unitary System Total Cooling Energy [J]
    HVAC,Average,Unitary System Sensible Heating Rate [W]
    HVAC,Sum,Unitary System Sensible Heating Energy [J]
    HVAC,Average,Unitary System Sensible Cooling Rate [W]
    HVAC,Sum,Unitary System Sensible Cooling Energy [J]
    HVAC,Average,Unitary System Latent Heating Rate [W]
    HVAC,Sum,Unitary System Latent Heating Energy [J]
    HVAC,Average,Unitary System Latent Cooling Rate [W]
    HVAC,Sum,Unitary System Latent Cooling Energy [J]
    HVAC,Average,Unitary System Electric Power [W]
    HVAC,Sum,Unitary System Electric Energy [J]
    HVAC,Average,Unitary System Fan Part Load Ratio []
    HVAC,Average,Unitary System Compressor Part Load Ratio []
    HVAC,Average,Unitary System Bypass Air Mass Flow Rate [kg/s]
    HVAC,Average,Unitary System Air Outlet Setpoint Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Unitary System Total Heating Rate [W]

This output field is the total (enthalpy) heat addition rate of the CBVAV system in Watts. This value is calculated using the enthalpy difference of the outlet air and inlet air streams, and the supply air mass flow rate entering/leaving the system. This value is calculated for each HVAC system timestep being simulated, and the results (enthalpy addition only) are averaged for the timestep being reported.

#### Unitary System Total Heating Energy [J]

This output field is the total (enthalpy) heat addition of the CBVAV system in Joules over the timestep being reported. This value is calculated using the enthalpy difference of the outlet air and inlet air streams, the supply air mass flow rate entering/leaving the system, and the HVAC simulation timestep. This value is calculated for each HVAC system timestep being simulated, and the results (enthalpy addition only) are summed for the timestep being reported.

#### Unitary System Total Cooling Rate [W]

This output field is the total (enthalpy) heat extraction rate of the CBVAV system in Watts. This value is calculated using the enthalpy difference of the outlet air and inlet air streams, and the supply air mass flow rate entering/leaving the system. This value is calculated for each HVAC system timestep being simulated, and the results (enthalpy extraction only) are averaged for the timestep being reported.

#### Unitary System Total Cooling Energy [J]

This output field is the total (enthalpy) heat extraction of the CBVAV system in Joules over the timestep being reported. This value is calculated using the enthalpy difference of the outlet air and inlet air streams, the supply air mass flow rate entering/leaving the system, and the HVAC simulation timestep. This value is calculated for each HVAC system timestep being simulated, and the results (enthalpy extraction only) are summed for the timestep being reported.

#### Unitary System Sensible Heating Rate [W]

This output field is the sensible heat addition rate of the CBVAV system in Watts. This value is calculated using the enthalpy difference of the outlet air and inlet air streams at a constant humidity ratio, and the supply air mass flow rate entering/leaving the system. This value is calculated for each HVAC system timestep being simulated, and the results (heating only) are averaged for the timestep being reported.

#### Unitary System Sensible Heating Energy [J]

This output field is the sensible heat addition of the CBVAV system in Joules over the timestep being reported. This value is calculated using the enthalpy difference of the outlet air and inlet air streams at a constant humidity ratio, the supply air mass flow rate entering/leaving the system, and the HVAC simulation timestep. This value is calculated for each HVAC system timestep being simulated, and the results (heating only) are summed for the timestep being reported.

#### Unitary System Sensible Cooling Rate [W]

This output field reports the moist air sensible heat extraction rate of the CBVAV system in Watts. This value is calculated using the enthalpy difference of the outlet air and inlet air streams at a constant humidity ratio, and the supply air mass flow rate entering/leaving the system. This value is calculated for each HVAC system timestep being simulated, and the results (cooling only) are averaged for the timestep being reported.

#### Unitary System Sensible Cooling Energy [J]

This output field reports the moist air sensible heat extraction of the CBVAV system in Joules over the timestep being reported. This value is calculated using the enthalpy difference of the outlet air and inlet air streams at a constant humidity ratio, the supply air mass flow rate entering/leaving the system, and the HVAC simulation timestep. This value is calculated for each HVAC system timestep being simulated, and the results (cooling only) are summed for the timestep being reported.

#### Unitary System Latent Heating Rate [W]

This output field is the latent heat addition (humidification) rate of the CBVAV system in Watts. This value is calculated as the difference between the total energy rate and the sensible energy rate provided by the system. This value is calculated for each HVAC system timestep being simulated, and the results (latent heat addition only) are averaged for the timestep being reported.

#### Unitary System Latent Heating Energy [J]

This output field is the latent heat addition (humidification) of the CBVAV system in Joules over the timestep being reported. This value is calculated as the difference between the total energy and the sensible energy delivered by the system. This value is calculated for each HVAC system timestep being simulated, and the results (latent heat addition only) are summed for the timestep being reported.

#### Unitary System Latent Cooling Rate [W]

This output field is the latent heat extraction (dehumidification) rate of the CBVAV system in Watts. This value is calculated as the difference between the total energy rate and the sensible energy rate provided by the system. This value is calculated for each HVAC system timestep being simulated, and the results (latent heat extraction only) are averaged for the timestep being reported.

#### Unitary System Latent Cooling Energy [J]

This output field is the latent heat extraction (dehumidification) of the CBVAV system in Joules over the timestep being reported. This value is calculated as the difference between the total energy and the sensible energy delivered by the system. This value is calculated for each HVAC system timestep being simulated, and the results (latent heat extraction only) are summed for the timestep being reported.

#### Unitary System Electric Power [W]

This output field is the electricity consumption rate of the CBVAV system in Watts. The consumption includes electricity used by the DX compressor (including crankcase heater), fans (indoor supply air fan and the condenser fans associated with the DX coil[s]), and the heating coil (if electric). This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

#### Unitary System Electric Energy [J]

This output field is the electricity consumption of the CBVAV system in Joules for the time period being reported. The consumption includes electricity used by the DX compressor (including crankcase heater), fans (indoor supply air fan and the condenser fans associated with the DX coil[s]), and the heating coil (if electric). This value is calculated for each HVAC system timestep being simulated, and the results are summed for the timestep being reported.

#### Unitary System Fan Part Load Ratio []

This output field is the part-load ratio of the supply air fan, which will be either zero or 1 for each simulation timestep. For this system, the fan will operate continuously for the simulation timestep if the system is available (ref. Field: Availability schedule name) and there is a cooling or heating load to be met (i.e., fan part-load ratio will equal 1). When the system is available but there is no cooling or heating load to be met, the fan will either be off for the entire timestep or on for the entire timestep depending on the supply air fan operating mode schedule (ref. Field: Supply Air Fan Operating Mode Schedule Name). This value is set for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

#### Unitary System Compressor Part Load Ratio []

This output field is the part-load ratio of the compressor used by the DX coils (cooling and heating). The compressor part-load ratio is defined as the total coil load divided by the coil steady-state capacity (steady-state capacity of first stage for multi-mode coils). This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

#### Unitary System Bypass Air Mass Flow Rate [kg/s]

This output field is the mass flow rate of air, in kg/s, being bypassed from the supply air path and blended with the air entering the CBVAV system. This value is calculated for each HVAC system timestep being simulated, and the results are averaged for the timestep being reported.

#### Unitary System Air Outlet Setpoint Temperature [C]

This output field is the dry-bulb set point temperature in degrees Celsius. This set point temperature is calculated by the model based on the zone cooling/heating loads calculated by EnergyPlus, and the priority control mode and the dehumidification control type specified for this unitary system. The CBVAV system attempts to achieve the outlet air set point temperature to the extent possible.