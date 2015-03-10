# Group – Condenser Equipment

## Equipment Types 

In each [CondenserEquipmentList](#condenserequipmentlist), various equipment types and names must be given. Each type-name pair must then have a corresponding equipment definition. This subsection lists the various equipment types that are available and examples from an IDF. Where appropriate, notes and comments on the input structure are provided.

## CoolingTower:SingleSpeed

Cooling towers are components that may be assigned to condenser loops. The cooling tower is modeled as a counterflow heat exchanger with a single-speed fan (induced draft configuration) based on Merkel's theory. The user must define tower performance via one of two methods: design heat transfer coefficient-area product (UA) and design water flow rate, or nominal tower capacity at a specific rating point. Regardless of which method is chosen, the design airflow rate and corresponding fan power must be specified. The model will also account for tower performance in the "free convection" regime, when the tower fan is off but the water pump remains on and heat transfer still occurs (albeit at a low level). If the user wants the model to account for "free convection", they must specify the corresponding airflow rate and heat transfer coefficient-area product (UA), or the nominal tower capacity during this mode of operation.

The cooling tower seeks to maintain the temperature of the water exiting the cooling tower at (or below) a set point. The set point schedule value is defined by the field "Condenser Loop Temperature Setpoint Node Name or Reference" for the [CondenserLoop](#condenserloop) object. The model first checks to determine the impact of "free convection", if specified by the user, on the tower exiting water temperature. If the exiting water temperature based on "free convection" is at or below the set point, then the tower fan is not turned on. If the exiting water temperature based on "free convection" is below the set point, the tower will operate in FluidBypass mode – portion of the water goes through the tower media and gets cooled while the remaining water flow gets bypassed, two water flows then mix together trying to meet the water setpoint temperature. If the exiting water temperature remains above the set point after "free convection" is modeled, then the tower fan is turned on to reduce the exiting water temperature to the set point. If the capacity control is FanCycling, the model assumes that part-load operation is represented by a simple linear interpolation between two steady-state regimes (i.e., tower fan on for the entire simulation timestep and tower fan off for the entire simulation timestep). Cyclic losses are not taken into account. If the capacity control is FluidBypass, the model determines the fraction of water flow to be bypassed while the remaining water goes through the tower cooling media and gets cooled, then the two water flows mix to meet the setpoint temperature. In this case, the fan runs at full speed for the entire timestep.

Cooling towers here are "wet" and consume water through evaporation, drift, and blowdown. The model can be used to predict water consumed by the towers. The last six input fields are optional and provide methods of controlling details of the water consumption calculations. The user can specifiy connections to the rest of the buildings water system by providing the name of a water storage tanks (i.e. [WaterUse:Storage](#waterusestorage) objects).

For the operation of multi-cell towers, the first step is to determine the number of cells to operate based on the cell control method – between the minimum number of cells subject to the maximum water flow rate fraction per cell, and maximum number of cells subject to the minimum water flow rate fraction per cell. If the calculated cells do not meet the loads, additional cells will be operating to help meet the loads. Inside each cell, the existing capacity controls still apply.

**For multi-cell towers, the following inputs are assumed to be for the entire tower including all cells:**

- Design Water Flow Rate; Design Air Flow Rate; Fan Power at Design Air Flow Rate;
- Air Flow Rate in Free Convection Regime; Nominal Capacity; Free Convection Capacity
- BASIN HEATER (we assume that there is a common basin)

### Inputs

#### Field: Name

This alpha field contains the identifying name for the cooling tower.

#### Field: Water Inlet Node Name

This alpha field contains the identifying name for the cooling tower's water inlet node.

#### Field: Water Outlet Node Name

This alpha field contains the identifying name for the cooling tower's water outlet node.

#### Field: Design Water Flow Rate

This numeric field contains the design water flow rate through the tower in m^3^/s. This value is the flow rate of the condenser loop water being cooled by the tower (not the flow rate of water being sprayed on the outside of the heat exchange coil). If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a water flow rate greater than zero must be defined or the field can be autosized. If autosized, a Plant Sizing object must be defined and the design water flow rate is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections). If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically assumes a water flow rate of 5.382E-8 m3/s per watt (3 gpm/ton) of tower capacity specified in the field "Nominal Capacity".

#### Field: Design Air Flow Rate

This numeric field contains the design air flow rate induced by the tower fan in m^3^/s. A value greater than zero must be defined regardless of the tower performance input method. Alternately, this field can be autosized. If autosized, the design air flow rate is calculated as follows:

![](media/image187.png)\


where a fan pressure rise of 190 Pascals and total fan efficiency of 0.5 are assumed.

**Field: Design Fan Power**

This numeric field contains the fan power (in watts) at the design air flow rate specified in the previous field. A value greater than zero must be specified regardless of the tower performance input method, or this field can be autosized. If autosized, the fan power is calculated as follows:

If "Performance Input Method" is specified as "*UFactorTimesAreaAndDesignWaterFlowRate*", then

![](media/image188.png)\


is used.

**If** "Performance Input Method" is specified as "*NominalCapacity*", then

![](media/image189.png)\


is used.

#### Field: Design U-Factor Times Area Value

This numeric field contains the heat transfer coefficient-area product (UA) in watts per Kelvin corresponding to the design air and water flow rates specified above. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a UA value greater than zero but less than or equal to 300,000 must be defined, or the field can be autosized. If autosized, a Plant Sizing object must be defined and the design tower UA value is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections), assuming a tower water inlet temperature of 35C and tower inlet air at 35C drybulb/25.6C wetbulb. If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically calculates the tower UA based on the tower capacity specified in the field "Nominal Capacity".

#### Field: Free Convection Regime Air Flow Rate 

This numeric field contains the air flow rate (m^3^/s) when the tower is in the "free convection" regime (water flow exists but tower fan is turned off). This value must be less than the value specified for the field "Design Air Flow Rate". This field may be autocalculated, in which case it is set to a fraction of the "Design Air Flow Rate" determined in the following input field. If the user does not wish to model "free convection" and is using the Performance Input Method "UFactorTimesAreaAndDesignWaterFlowRate", then this field should be set to 0.0. If the user specifies the U-Factor Times Area Value at Free Convection Air Flow Rate or Free Convection Capacity as a value greater than zero, then the free convection air flow rate must be specified greater than 0.0.

#### Field: Free Convection Regime Air Flow Rate Sizing Factor

This numeric field contains the sizing factor to use when calculating the free convection regime air flow rate.  The default is 0.1.

#### Field: Free Convection Regine U-Factor Times Area Value 

This numeric field contains the heat transfer coefficient-area product (W/K) when the tower is in the "free convection" regime (water flow exists but tower fan is turned off). This value must be less than the value specified for the field "U-Factor Times Area Value at Design Air Flow Rate". This field may be autosized, in which case it is set to a fraction of the "U-Factor Times Area Value at Design Air Flow Rate" determined in the following input field. If the user does not wish to model "free convection" and is using the  Performance Input Method "UFactorTimesAreaAndDesignWaterFlowRate", then this field should be set to 0.0. If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically calculates the tower UA based on the tower capacity specified in the field "Free Convection Capacity".\\

#### Field: Free Convection U-Factor Times Area Value Sizing Factor

This numeric field contains the sizing factor to use when calculating the free convection regime U-Factor times area value.  The default is 0.1.

#### Field: Performance Input Method

This alpha field contains the method by which the user will specify tower performance: "**UFactorTimesAreaAndDesignWaterFlowRate**" or "**NominalCapacity**". If this field is left blank in the input data file, the default input method is assumed to be "**UFactorTimesAreaAndDesignWaterFlowRate**". If the method "**UFactorTimesAreaAndDesignWaterFlowRate**" is selected, then the user must enter design UA values, design water flow rates and air flow rates as described for the previous input fields. If the method "NominalCapacity" is selected then the fields "Design Water Flow Rate", "U-Factor Times Area Value at Design Air Flow Rate" and "U-Factor Times Area Value at Free Convection Air Flow Rate" must be left blank, but the fields "Nominal Capacity" and "Free Convection Capacity" must be entered as described below.

#### Field: Heat Rejection Capacity and Nominal Capacity Sizing Ratio

This numeric field contains the value for the ratio of actual tower heat rejection to nominal capacity.  This ratio is defined at entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. Historically this ratio has been set at 1.25 based on the assumption that the tower must dissipate 0.25 W of compressor power for every what of heat removed at the chiller evaporator.  The default is 1.25.

#### Field: Nominal Capacity

This numeric input field contains the "nominal" heat rejection capacity of the cooling tower in watts, with entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. The design water flow rate is assumed to be 5.382E-8 m^3^/s per watt of nominal capacity (3 gpm/ton). The value in the previous field times this nominal tower capacity gives the actual tower heat rejection at these operating conditions (based on historical assumption that the tower must dissipate additional heat from the compressor heat for heat removed at the evaporator).

#### Field: Free Convection Capacity

This numeric input field contains the "nominal" heat rejection capacity of the cooling tower in watts when the tower is in the "free convection" regime (water flow exists but tower fan is turned off), with entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. The design water flow rate is assumed to be 5.382E-8 m^3^/s per watt of nominal tower capacity (input field above). The heat rejection capacity and nominal capacity sizing ratio is applied tof this free convection tower capacity to give the actual tower heat rejection at these operating conditions (typical value is 1.25 based on historical assumption that the tower must dissipate 0.25W of compressor heat for every watt of heat removed by the evaporator). The value specified for this field must be less than the value specified for the field "Tower Nominal Capacity". If the user does not wish to model "free convection", then this field should be set to 0.0. If the user specifies a value greater than zero, then the "Air Flow Rate in Free Convection Regime" field must contain a value greater than zero. This field can be automatically calculated using the sizing factor in the following field.

#### Field: Free Convection Nominal Capacity Sizing Factor

This numeric field contains the sizing factor to use when calculating the Free Convection Capacity. The default is 0.1.

#### Field: Basin Heater Capacity

This numeric field contains the capacity of the tower's electric basin heater in watts per degree Kelvin. This field is used in conjunction with the Basin Heater Setpoint Temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the outdoor dry-bulb temperature. The basin heater only operates when the tower fan is off and water is not flowing through the tower, regardless of the basin heater schedule described below. The basin heater capacity must be greater than or equal to zero, with a default value of zero if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

This numeric field contains the set point temperature (˚C) for the basin heater described in the previous field. The basin heater is active when the outdoor air dry-bulb temperature falls below this setpoint temperature, as long as the tower fan is off and water is not flowing through the tower. This set point temperature must be greater than or equal to 2˚C, and the default value is 2˚C if this field is left blank.

#### Field: Basin Heater Operating Schedule Name

This alpha field contains the name of the basin heater operating schedule. The basin heater operating schedule is assumed to be an on/off schedule and the heater is available to operate any time the schedule value is greater than 0. The basin heater operates when scheduled on and the outdoor air dry-bulb temperature is below the set point temperature described in the previous field. If this field is left blank, the basin heater is available to operate throughout the simulation. Regardless of this schedule, the basin heater may only operate when the cooling tower fan is off and water is not flowing through the tower.

#### Field: Evaporation Loss Mode

This field is used to choose which method is used to model the amount of water evaporated by the cooling tower. There are two options: **LossFactor** or **SaturatedExit**. The default is SaturatedExit. The user-defined loss factor is entered in the following field. By assuming that the air leaving the tower is saturated, the evaporation can be directly calculated using moist air engineering calculations with data available within the cooling tower model (and does not require additional user input).

#### Field: Evaporation Loss Factor

This field is used to specify the rate of water evaporated from the cooling tower and lost to the outside air [percent/K]. This field is only used if the Evaporation Calculation Mode is set to **LossFactor**. The evaporation loss is then calculated as a fraction of the circulating condenser water flow and varies with the temperature change in the condenser water. The value entered here is in units of percent-per-degree Kelvin. The evaporation rate will equal this value times each degree Kelvin of temperature drop in the condenser water. Typical values are from 0.15 to 0.27 [percent/K]. The default is 0.2.

#### Field: Drift Loss Percent

This field is used to specify the rate of water lost to the exiting air as entrained droplets [%]. The drift loss is a percent of the condenser water flow. Typical values for towers with efficient drift eliminators are between 0.002 and 0.2% of the condenser water flow rate. The default value is 0.008%.

#### Field: Blowdown Calculation Mode

This field specifies which method is used to determine blowdown rates. There two options **ConcentrationRatio** or **ScheduledRate**. The choice will determine which of the two models below is used. The default is ConcentrationRatio.

#### Field: Blowdown Concentration Ratio

This field is used to dynamically adjust the rate of blowdown in the cooling tower as a function of the rate of evaporation. Blowdown is water intentionally drained from the tower in order to offset the build up of solids in the water that would otherwise occur because of evaporation. The value entered here is dimensionless. It can be characterized as the ratio of solids in the blowdown water to solids in the make up water. Typical values for tower operation are 3 to 5. The default value is 3.

#### Field: Blowdown Makeup Water Usage Schedule Name

This alpha field contains the name of the schedule used to define the amount of water (m^3^/s) flushed from the basin on a periodic basis to purge the tower of mineral scale build-up and other contaminants. This schedule is only used if the Blowdown Calculation Mode is set to ScheduledRate. The amount of water use due to blowdown depends on the makeup water quality and is specific to each geographical location. Typical values range from 0.0002 to 0.0013 m^3^/s (17.3 to 112.3 m^3^/day). This water usage is in addition to the amount of water lost to the atmosphere due to evaporation and/or drift. Since blowdown occurs when the basin water contaminant concentration is high, blowdown only occurs when the cooling tower is active and water is flowing through the tower (regardless of the water usage defined by this schedule).

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the tower obtains water used for evaporative cooling. If blank or omitted, then the tower will obtain water directly from the mains. If the name of a [WaterUse:Storage](#waterusestorage) object is used here, then the tower will obtain its water from that tank. If a tank is specified, the tower will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the tower needs, then the tower will still operate and obtain the rest of the water it needs from the mains (referred to as ‘Starved' water).

#### Field: Outdoor Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the cooling tower. If this field is left blank, the outdoor air conditions entering the cooling tower are taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air conditions from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air conditions are taken directly from the weather data.

#### Field: Capacity Control

This alpha field contains the cooling capacity control for the cooling tower. Two choices are available: **FanCycling** and **FluidBypass**. During part-load conditions, there are two ways to maintain the exiting water temperature at the setpoint: either cycling the tower fan, or bypassing portion of the tower water with a three-way valve. For FluidBypass, the tower fan still runs at full speed for the entire timestep, but only portion of the water flow goes through the cooling tower media to get cooled while the remaining portion of the water flow gets bypassed. Two water flows then mix at the common water sump to meet the setpoint temperature.

#### Field: Number of Cells

This integer field contains the number of cells in the multi-cell cooling tower. If not entered, the program will assume it is a single-cell cooling tower

#### Field: Cell Control

This alpha field specifies the method used to control the number of cells used to meet the load, the two choices are:

**MinimalCell** : the program will use minimal number of cells needed, all other cells will be shut down with no water flow. It will attempt to use as few cells as possible to cool the fluid. In no case, however, will the flow per cell be allowed to exceed its maximum value defined by the *Maximum Water Flow Rate Fraction.*

**MaximalCell**: As many cells as possible will be turned on. In no case, however, will the flow per cell be allowed to drop below its minimum value specified by the *Minimum Water Flow Rate Fraction*.

#### Field: Cell Minimum Water Flow Rate Fraction

This numeric field specifies the allowable smallest fraction of the design water flow rate. Flows less than this value will commonly result in fluid distribution problems; the pressure at each nozzle will be too weak for the fluid to be sprayed out in the correct pattern, not all the fill would be wet. If this field is left blank, the default value is 0.33.

#### Field: Cell Maximum  Water Flow Rate Fraction

This numeric field specifies the allowable largest fraction of the design water flow rate. If this field is left blank, the default value is 2.5.

**Field: Sizing Factor**

This optional numeric field allows the user to specify a sizing factor for this component. The sizing factor is used when the component design inputs are autosized: the autosizing calculations are performed as usual and the results are multiplied by the sizing factor. Sizing factor allows the user to size a component to meet part of the design load while continuing to use the autosizing feature. For this component the inputs that would be altered by the sizing factor are:

Design Water Flow Rate;

Design Air Flow Rate;

Fan Power at Design Air Flow Rate;

U-Factor Times Area Value at Design Air Flow Rate;

Air Flow Rate in Free Convection Regime;

U-Factor Times Area Value at Free Convection Air Flow Rate.

Note that the U-Factor Times Area Value at Design Air Flow Rate is not *multiplied* by the Sizing Factor. Instead the design tower load is multiplied by the sizing factor and the design UA then calculated as usual. The U-Factor Times Area Value at Free Convection Air Flow Rate is set to 10% of the new design Tower UA.

Three examples of an IDF specification for this object are shown below:

~~~~~~~~~~~~~~~~~~~~

    CoolingTower:SingleSpeed,
      My Tower,                     !- Name
      Condenser Tower Inlet Node,   !- Water Inlet Node Name
      Condenser Tower Outlet Node,  !- Water Outlet Node Name
      .0011,                        !- Design Water Flow Rate (m3/s)
      16.0,                         !- Design Air Flow Rate (m3/s)
      10000.,                       !- Fan Power at Design Air Flow Rate (W)
      3500.,  !- U-Factor Times Area Value at Design Air Flow Fate (W/C)
      0.0,    !- Air Flow Rate in Free Convection Regime (m3/s)
      0.0,    !- U-Factor Times Area Value at Free Convection Air Flow Rate (W/C)
      UFactorTimesAreaAndDesignWaterFlowRate,!- Performance Input Method
      ,                             !- Nominal Capacity (W)
      ,                             !- Free Convection Capacity (W)
      ,                             !- Basin Heater Capacity {W/K}
      ,                             !- Basin Heater Setpoint Temperature {C}
      ,                             !- Basin Heater Operating Schedule Name
      ,                             !- Evaporation Loss Mode
      ,                             !- Evaporation Loss Factor (percent/K)
      ,                             !- Drift Loss Percent (percent)
      ,                             !- Blowdown Calculation Mode
      ,                             !- Blowdown Concentration Ratio
      ,                             !- Blowdown Makeup Water Usage Schedule Name
      ,                             !- Supply Water Storage Tank Name
      ,                             !- Outdoor Air Inlet Node Name
      FluidBypass,                  !- Capacity Control
      4,                            !- Number of Cells
      MinimalCell,                  !- Cell Control
      0.25,                         !- Cell Minimum Water Flow Rate Fraction
      1.50;                         !- Cell Maximum Water Flow Rate Fraction
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    CoolingTower:SingleSpeed,
      My Tower,                   !- Name
      Condenser Tower Inlet Node, !- Water Inlet Node Name
      Condenser Tower Outlet Node,!- Water Outlet Node Name
      ,                           !- Design Water Flow Rate (m3/s)
      autosize,                   !- Design Air Flow Rate (m3/s)
      1000.,                      !- Fan Power at Design Air Flow Rate (W)
      ,                !- U-Factor Times Area Value at Design Air Flow Fate (W/C)
      autosize,        !- Air Flow Rate in Free Convection Regime (m3/s)
      ,       !- U-Factor Times Area Value at Free Convection Air Flow Rate (W/C)
      NominalCapacity,            !- Performance Input Method
      95250.,                     !- Nominal Capacity (W)
      9525.;                      !- Free Convection Capacity (W)
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    CoolingTower:SingleSpeed,
        TowerWaterSys CoolTower, !- Name
        TowerWaterSys Pump-TowerWaterSys CoolTowerNode,  !- Water Inlet Node Name
        TowerWaterSys Supply Equipment Outlet Node,  !- Water Outlet Node Name
        AUTOSIZE,                !- Design Water Flow Rate {m3/s}
        AUTOSIZE,                !- Design Air Flow Rate {m3/s}
        AUTOSIZE,                !- Fan Power at Design Air Flow Rate {W}
        AUTOSIZE,   !- U-Factor Times Area Value at Design Air Flow Rate {W/K}
        AUTOSIZE,   !- Air Flow Rate in Free Convection Regime {m3/s}
        AUTOSIZE,   !- U-Factor Times Area at Free Convection Air Flow Rate {W/K}
        UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method
        ,                        !- Nominal Capacity {W}
        ,                        !- Free Convection Capacity {W}
        ,                        !- Basin Heater Capacity {W/K}
        ,                        !- Basin Heater Setpoint Temperature {C}
        ,                        !- Basin Heater Operating Schedule Name
        SaturatedExit,           !- Evaporation Loss Mode
        ,                        !- Evaporation Loss Factor {percent/K}
        0.0080,                  !- Drift Loss Percent {percent}
        ConcentrationRatio,      !- Blowdown Calculation Mode
        3.0000,                  !- Blowdown Concentration Ratio
        ,                        !- Blowdown Makeup Water Usage Schedule Name
        ,                        !- Supply Water Storage Tank Name
        TowerWaterSys CoolTowerOA ref Node,  !- Outdoor Air Inlet Node Name
        FanCycling,              !- Capacity Control
        4,                       !- Number of Cells
        MinimalCell,             !- Cell Control
        0.25,                    !- Cell Minimum Water Flow Rate Fraction
        1.50,                    !- Cell Maximum Water Flow Rate Fraction
        1.0000;                  !- Sizing Factor
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Tower Fan Electric Power [W]
    HVAC,Sum,Cooling Tower Fan Electric Energy [J]

    HVAC,Average,Cooling Tower Heat Transfer Rate [W]
    HVAC,Average,Cooling Tower Inlet Temperature [C]
    HVAC,Average,Cooling Tower Outlet Temperature [C]
    HVAC,Average,Cooling Tower Mass Flow Rate [kg/s]
    HVAC,Average,Cooling Tower Bypass Fraction []
    HVAC,Average,Cooling Tower Fan Cycling Ratio []
    HVAC,Average,Cooling Tower Operating Cells Count []

    A tower uses either mains water or storage tank for make-up water.
    When mains water is used:
    HVAC,Average,Cooling Tower Make Up Water Volume Flow Rate  [m3/s]
    HVAC,Sum,Cooling Tower Make Up Water Volume [m3]
    HVAC,Sum,Cooling Tower Make Up Mains Water Volume [m3]

    When storage tank water is used:
    HVAC,Average,Cooling Tower Make Up Water Volume Flow Rate  [m3/s]
    HVAC,Sum,Cooling Tower Make Up Water Volume [m3]
    HVAC,Sum,Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Storage Tank Water Volume [m3]
    HVAC,Sum,Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Starved Storage Tank Water Volume [m3]
    HVAC,Sum,Cooling Tower Make Up Mains Water Volume [m3]

    HVAC,Average,Cooling Tower Water Evaporation Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Evaporation Volume [m3]
    HVAC,Average,Cooling Tower Water Drift Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Drift Volume [m3]
    HVAC,Average,Cooling Tower Water Blowdown Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Blowdown Volume [m3]

    IF specified:
    HVAC,Average,Cooling Tower Basin Heater Electric Power [W]
    HVAC,Sum,Cooling Tower Basin Heater Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Tower Fan Electric Power [W]

#### Cooling Tower Fan Electric Energy [J] 

These outputs are the electric power input to the tower fans. Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility.

#### Cooling Tower Heat Transfer Rate [W]

This is the rate at which heat is removed from the condenser water loop by the tower.

#### Cooling Tower Inlet Temperature [C]

#### Cooling Tower Outlet Temperature [C]

#### Cooling Tower Mass Flow Rate [kg/s]

These outputs are the tower water inlet and outlet temperatures, and mass flow rate of the circulating condenser water loop.

#### Cooling Tower Basin Heater Electric Power [W]

#### Cooling Tower Basin Heater Electric Energy [J]

These outputs are the electric power input to the tower basin heater. Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility

#### Cooling Tower Make Up Water Volume Flow Rate [m3/s]

#### Cooling Tower Make Up Water Volume [m3]

These outputs are the water consumed by the wet cooling tower for external water sprays used to augment heat transfer. This is the total of evaporation, drift, and blowdown.

#### Cooling Tower Water Evaporation Volume Flow Rate [m3/s]

#### Cooling Tower Water Evaporation Volume [m3]

#### Cooling Tower Water Drift Volume Flow Rate [m3/s]

#### Cooling Tower Water Drift Volume [m3]

#### Cooling Tower Water Blowdown Volume Flow Rate [m3/s]

#### Cooling Tower Water Blowdown Volume [m3]

These outputs provide the breakdown of the different components of water use during cooling tower operation.

#### Cooling Tower Make Up Mains Water Volume [m3]

This is the volume of water drawn from mains service to feed the cooling tower.

If a water storage tank is used to provide water to the tower, then the following output variables will also be available.

#### Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Storage Tank Water Volume [m3]

These are the rate and volume of water provided by the Water Storage Tank.

#### Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Starved Storage Tank Water Volume [m3]

These are the rate and volume of water the Storage Tank connections was not able to provide. The starved water is assumed to come from the mains. The tower's operation is not affected by a lack of storage tank water.

#### Cooling Tower Fan Cycling Ratio []

This represents the fraction of a time-step when tower fan is on.

#### Cooling Tower Bypass Fraction []

This represents the fraction of a fluid bypassing the tower when a mixture of the tower fluid and tower return water is able to meet the set point temperature.

#### Cooling Tower Operating Cells Count []

This represents the number of cells operating at each time step.

## CoolingTower:TwoSpeed

The two-speed cooling tower is modeled in a similar fashion to the single-speed tower. The cooling tower is modeled as a counterflow heat exchanger with a two-speed fan (induced draft configuration) based on Merkel's theory. The user must define tower performance via one of two methods: heat transfer coefficient-area product (UA) and design water flow rate, or nominal tower capacity at a specific rating point. Regardless of which method is chosen, the airflow rate and corresponding fan power at both high and low fan speed must be specified. The model will also account for tower performance in the "free convection" regime, when the tower fan is off but the water pump remains on and heat transfer still occurs (albeit at a low level). If the user wants the model to account for "free convection", they must specify the corresponding air flow rate and heat transfer coefficient-area product (UA), or the nominal tower capacity during this mode of operation.

The cooling tower seeks to maintain the temperature of the water exiting the cooling tower at (or below) a set point. The set point schedule value is defined by the field "Condenser Loop Temperature Setpoint Node Name or Reference" for the [CondenserLoop](#condenserloop) object. The model first checks to determine the impact of "free convection", if specified by the user, on the tower exiting water temperature. If the exiting water temperature based on "free convection" is at or below the set point, then the tower fan is not turned on. If the exiting water temperature remains above the set point after "free convection" is modeled, then the tower fan is turned on at low speed to reduce the exiting water temperature. If operating the tower fan at low speed does not reduce the exiting water temperature to the set point, then the tower fan is increased to its high speed.

The model assumes that part-load operation is represented by a simple linear interpolation between two steady-state regimes (i.e., tower fan at high speed for the entire simulation timestep and tower fan at low speed for the entire simulation timestep, or tower fan at low speed for the entire simulation timestep and tower fan off for the entire simulation timestep). Cyclic losses are not taken into account.

Cooling towers here are "wet" and consume water through evaporation, drift, and blowdown. The model can be used to predict water consumed by the towers. The last six input fields are optional and provide methods of controlling details of the water consumption calculations. The user can specifiy connections to the rest of the buildings water system by providing the name of a [WaterUse:Storage](#waterusestorage) object.

For the operation of multi-cell towers, the first step is to determine the number of cells to operate based on the cell control method – between the minimum number of cells subject to the maximum water flow rate fraction per cell, and maximum number of cells subject to the minimum water flow rate fraction per cell. If the calculated cells do not meet the loads, additional cells will be operating to help meet the loads. Inside each cell, the existing capacity controls still apply.

**For multi-cell towers, the following inputs are assumed to be for the entire tower including all cells:**

- Design Water Flow Rate; Design Air Flow Rate; Fan Power at Design Air Flow Rate;
- Air Flow Rate in Free Convection Regime; Nominal Capacity; Free Convection Capacity
- BASIN HEATER (we assume that there is a common basin)

**Field: Name**

This alpha field contains the identifying name for the cooling tower.

### Inputs

#### Field: Water Inlet Node Name

This alpha field contains the identifying name for the cooling tower's water inlet node.

#### Field: Water Outlet Node Name

This alpha field contains the identifying name for the cooling tower's water outlet node.

#### Field: Design Water Flow Rate

This numeric field contains the design water flow rate through the tower in m^3^/s. This value is the flow rate of the condenser loop water being cooled by the tower (not the flow rate of water being sprayed on the outside of the heat exchange coil). If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a water flow rate greater than zero must be defined or the field can be autosized. If autosized, a Plant Sizing object must be defined and the design water flow rate is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections). If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically assumes a design water flow rate of 5.382E-8 m3/s per watt (3 gpm/ton) of tower capacity specified in the field "Tower High-Speed Nominal Capacity".

#### Field: High Fan Speed Air Flow Rate

This numeric field contains the tower air flow rate at high fan speed in m^3^/s. A value greater than zero must be defined regardless of the tower performance input method. Alternately, this field can be autosized. If autosized, the design air flow rate is calculated as follows:

![](media/image190.png)\


where a fan pressure rise of 190 Pascals and total fan efficiency of 0.5 are assumed.

**Field: High Fan Speed Fan Power**

This numeric field contains the fan power (in Watts) at the high-speed air flow rate specified in the previous field. A value greater than zero must be specified regardless of the tower performance input method, or this field can be autosized. If autosized, the fan power is calculated as follows:

If "Performance Input Method" is specified as "*UFactorTimesAreaAndDesignWaterFlowRate*", then

![](media/image191.png)\


is used.

**If** "Performance Input Method" is specified as "*NominalCapacity*", then

![](media/image192.png)\


is used.

#### Field: High Fan Speed U-Factor Times Area Value 

This numeric field contains the heat transfer coefficient-area product (UA) in watts per degree Celsius corresponding to the high-speed air flow rate and design water flow rate specified above. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a UA value greater than zero but less than or equal to 300,000 must be defined, or the field can be autosized. If autosized, a Plant Sizing object must be defined and the tower UA value at high fan speed is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections), assuming a tower water inlet temperature of 35C and tower inlet air at 35C drybulb/25.6C wetbulb. If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically calculates the tower UA based on the capacity specified in the field "High-Speed Nominal Capacity".

#### Field: Low Fan Speed Air Flow Rate 

This numeric field contains the tower air flow rate at low fan speed in m^3^/s. This value must be greater than zero, less than the value specified for the field "Air Flow Rate at High Fan Speed", and greater than the value specified for the field "Air Flow Rate in Free Convection Regime". This field may be autocalculated, in which case it is set to a fraction of the "Air Flow Rate at High Fan Speed" determined in the next field.

#### Field: Low Fan Speed Air Flow Rate Sizing Factor

This numeric field contains the sizing factor to use when calculating the low fan speed air flow rate.  The default is 0.5.

**Field: Low Fan Speed Fan Power**

This numeric field contains the fan power (in Watts) at the low-speed air flow rate specified in the previous field. This value must be specified greater than zero or the field may be autocalculated, in which case it is set to a fraction of the "Fan Power at High Fan Speed" determined in the next field.

#### Field: Low Fan Speed Fan Power Sizing Factor

This numeric field contains the sizing factor to use when calculating the low speed fan power.  The default is 0.16.

#### Field: Low Fan Speed U-Factor Times Area Value 

This numeric field contains the heat transfer coefficient-area product (UA) in watts per degree Celsius corresponding to the design water flow rate and low-speed air flow rate specified above. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", this value must be greater than zero but less than or equal to 300,000, less than the value specified for the field "U-Factor Times Area Value at High Fan Speed", and greater than the value specified for the field "U-Factor Times Area Value at Free Convection Air Flow Rate". This field may be autocalculated, in which case it is set to a fraction of the "U-Factor Times Area Value at High Fan Speed" determined in the following field. If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically calculates the tower UA based on the tower capacity specified in the field "Low Speed Nominal Capacity".

#### Field: Low Fan Speed U-Factor Times Area Sizing Factor

This numeric field contains the sizing factor to use when calculating the low speed heat transfer coefficient-area product (UA).  The default is 0.6.

#### Field: Free Convection Regime Air Flow Rate

This numeric field contains the air flow rate (m^3^/s) when the tower is in the "free convection" regime (water flow exists but tower fan is turned off). This value must be less than the value specified for the field "Air Flow Rate at Low Fan Speed". This field may be autocalculated, in which case it is set to a fraction of the "Air Flow Rate at High Fan Speed" determined in the following field. If the user does not wish to model "free convection" and is using the Performance Input Method "UFactorTimesAreaAndDesignWaterFlowRate", then this field should be set to 0.0. If the user specifies the U-Factor Times Area Value at Free Convection Air Flow Rate or Free Convection Capacity as a value greater than zero, then the free convection air flow rate must be specified greater than 0.0.

#### Field: Free Convection Air Flow Rate Sizing Factor

This numeric field contains the sizing factor to use when calculating the free convection regime air flow rate.  The default is 0.1.

#### Field: Free Convection U-Factor Times Area Value 

This numeric field contains the heat transfer coefficient-area product (W/°C) when the tower is in the "free convection" regime (water flow exists but tower fan is turned off). This value must be less than the value specified for the field "U-Factor Times Area Value at Low Fan Speed". This field may be autocalculated, in which case it is set to a fraction of the "U-Factor Times Area Value at High Fan Speed" determined in the following field. If the user does not wish to model "free convection" and is using the Performance Input Method "UFactorTimesAreaAndDesignWaterFlowRate", then this field should be set to 0.0. If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically calculates the tower UA based on the tower capacity specified in the field "Free Convection Capacity".

#### Field: Free Convection U-Factor Times Area Value Sizing Factor

This numeric field contains the sizing factor to use when calculating the free convection regime U-Factor times area value.  The default is 0.1.

#### Field: Performance Input Method

This alpha field contains the method by which the user will specify tower performance: "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity". If this field is left blank in the input data file, the default input method is assumed to be "UFactorTimesAreaAndDesignWaterFlowRate". If the method "UFactorTimesAreaAndDesignWaterFlowRate" is selected, then the user must enter UA values, design water flow rate and air flow rates as described for the previous input fields. If the method "NominalCapacity" is selected then the fields "Design Water Flow Rate", "U-Factor Times Area Value at High Fan Speed", "U-Factor Times Area Value at Low Fan Speed" and "U-Factor Times Area Value at Free Convection Air Flow Rate" must be left blank, but the fields "High Speed Nominal Capacity", "Low Speed Nominal Capacity" and "Free Convection Capacity" must be entered as described below.

#### Field: Heat Rejection Capacity and Nominal Capacity Sizing Ratio

This numeric field contains the value for the ratio of actual tower heat rejection to nominal capacity.  This ratio is defined at entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. Historically this ratio has been set at 1.25 based on the assumption that the tower must dissipate 0.25 W of compressor power for every what of heat removed at the chiller evaporator.  The default is 1.25.

#### Field: High Speed Nominal Capacity

This numeric input field contains the "nominal" heat rejection capacity of the cooling tower in watts under high-speed fan operation, with entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. The design water flow rate is assumed to be 5.382E-8 m^3^/s per watt of high-speed nominal capacity (3 gpm/ton). The Heat Rejection Capacity and Nominal Capacity Sizing Ratio set in the previous field is applied to this nominal tower capacity to give the actual tower heat rejection at these operating.

#### Field: Low Speed Nominal Capacity

This numeric input field contains the "nominal" heat rejection capacity of the cooling tower in watts under low-speed fan operation, with entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. The design water flow rate is assumed to be 5.382E-8 m^3^/s per watt of high-speed nominal tower capacity (input field above). The Heat Rejection Capacity and Nominal Capacity Sizing Ratio is applied to this nominal tower capacity to give the actual tower heat rejection at these operating conditions. The value specified for this field must be greater than zero but less than the value specified for the field "High-Speed Nominal Capacity".  This field may be autocalculated, in which case it is set to a fraction of the High Speed Nominal Capacity determined in the following field.

#### Field: Low Speed Nominal Capacity Sizing Factor

This numeric field contains the sizing factor to use when calculating the Low Speed Nominal Capacity. The default is 0.5.

#### Field: Free Convection Capacity

This numeric input field contains the "nominal" heat rejection capacity of the cooling tower in watts when the tower is in the "free convection" regime (water flow exists but tower fan is turned off), with entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. The design water flow rate is assumed to be 5.382E-8 m^3^/s per watt of high-speed nominal tower capacity (input field above). The Heat Rejection Capacity and Nominal Capacity Sizing Ratio is applied to this free convection tower capacity to give the actual tower heat rejection at these operating conditions.The value specified for this field must be less than the value specified for the field "Tower Low-Speed Nominal Capacity". If the user does not wish to model "free convection", then this field should be set to 0.0. If the user specifies a value greater than zero, then the "Air Flow Rate in Free Convection Regime" field must contain a value greater than zero. This field may be autocalculated, in which case it is set to a fraction of the High Speed Nominal Capacity determined in the following field.

#### Field: Free Convection Nominal Capacity Sizing Factor

This numeric field contains the sizing factor to use when calculating the Free Convection Capacity. The default is 0.1.

#### Field: Basin Heater Capacity

This numeric field contains the capacity of the tower's electric basin heater in watts per degree Kelvin. This field is used in conjunction with the Basin Heater Setpoint Temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the outdoor dry-bulb temperature. The basin heater only operates when the tower fan is off and water is not flowing through the tower, regardless of the basin heater schedule described below. The basin heater capacity must be greater than or equal to zero, with a default value of zero if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

This numeric field contains the set point temperature (˚C) for the basin heater described in the previous field. The basin heater is active when the outdoor air dry-bulb temperature falls below this setpoint temperature, as long as the tower fan is off and water is not flowing through the tower. This set point temperature must be greater than or equal to 2˚C, and the default value is 2˚C if this field is left blank.

#### Field: Basin Heater Operating Schedule Name

This alpha field contains the name of the basin heater operating schedule. The basin heater operating schedule is assumed to be an on/off schedule and the heater is available to operate any time the schedule value is greater than 0. The basin heater operates when scheduled on and the outdoor air dry-bulb temperature is below the set point temperature described in the previous field. If this field is left blank, the basin heater is available to operate throughout the simulation. Regardless of this schedule, the basin heater may only operate when the cooling tower fan is off and water is not flowing through the tower.

#### Field: Evaporation Loss Mode

This field is used to choose which method is used to model the amount of water evaporated by the cooling tower. There are two options: ‘LossFactor' or ‘SaturatedExit'. The default is SaturatedExit. The user-defined loss factor is entered in the following field. By assuming that the air leaving the tower is saturated, the evaporation can be directly calculated using moist air engineering calculations with data available within the cooling tower model (and does not require additional user input).

#### Field: Evaporation Loss Factor

This field is used to specify the rate of water evaporated from the cooling tower and lost to the outside air [percent/K]. This field is only used if the Evaporation Calculation Mode is set to LossFactor. The evaporation loss is then calculated as a fraction of the circulating condenser water flow and varies with the temperature change in the condenser water. The value entered here is in units of percent-per-degree Kelvin. The evaporation rate will equal this value times each degree Kelvin of temperature drop in the condenser water. Typical values are from 0.15 to 0.27 [percent/K]. The default is 0.2.

#### Field: Drift Loss Percent

This field is used to specify the rate of water lost to the exiting air as entrained droplets [%]. The drift loss is a percent of the condenser water flow. Typical values for towers with efficient drift eliminators are between 0.002 and 0.2% of the condenser water flow rate. The default value is 0.008%.

#### Field: Blowdown Calculation Mode

This field specifies which method is used to determine blowdown rates. There two options ConcentrationRatio or ScheduledRate. The choice will determine which of the two models below is used. The default is ConcentrationRatio

#### Field: Blowdown Concentration Ratio

This field is used to dynamically adjust the rate of blowdown in the cooling tower as a function of the rate of evaporation. Blowdown is water intentionally drained from the tower in order to offset the build up of solids in the water that would otherwise occur because of evaporation. The value entered here is dimensionless. It can be characterized as the ratio of solids in the blowdown water to solids in the make up water. Typical values for tower operation are 3 to 5. The default value is 3.

#### Field: Blowdown Makeup Water Usage Schedule Name

This alpha field contains the name of the schedule used to define the amount of water (m^3^/s) flushed from the basin on a periodic basis to purge the tower of mineral scale build-up and other contaminants. This schedule is only used if the Blowdown Calculation mode is set to ScheduledRate. The amount of water use due to blowdown depends on the makeup water quality and is specific to each geographical location. Typical values range from 0.0002 to 0.0013 m^3^/s (17.3 to 112.3 m^3^/day). This water usage is in addition to the amount of water lost to the atmosphere due to evaporation and/or drift. Since blowdown occurs when the basin water contaminant concentration is high, blowdown only occurs when the cooling tower is active and water is flowing through the tower (regardless of the water usage defined by this schedule).

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the tower obtains water used for evaporative cooling. If blank or omitted, then the tower will obtain water directly from the mains. If the name of a [WaterUse:Storage](#waterusestorage) object is used here, then the tower will obtain its water from that tank. If a tank is specified, the tower will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the tower needs, then the tower will still operate and obtain the rest of the water it needs from the mains (referred to as ‘Starved' water).

#### Field: Outdoor Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the cooling tower. If this field is left blank, the outdoor air conditions entering the cooling tower are taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air conditions from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air conditions are taken directly from the weather data.

#### Field: Number of Cells

This integer field contains the number of cells in the multi-cell cooling tower. If not entered, the program will assume it is a single-cell cooling tower

#### Field: Cell Control

This alpha field specifies the method used to control the number of cells used to meet the load, the two choices are:

**MinimalCell**: the program will use minimal number of cells needed, all other cells will be shut down with no water flow. It will attempt to use as few cells as possible to cool the fluid. In no case, however, will the flow per cell be allowed to exceed its maximum value defined by the *Maximum Water Flow Rate Fraction.*

**MaximalCell**: As many cells as possible will be turned on. In no case, however, will the flow per cell be allowed to drop below its minimum value specified by the *Minimum Water Flow Rate Fraction*.

#### Field: Cell Minimum Water Flow Rate Fraction

This numeric field specifies the allowable smallest fraction of the design water flow rate. Flows less than this value will commonly result in fluid distribution problems; the pressure at each nozzle will be too weak for the fluid to be sprayed out in the correct pattern, not all the fill would be wet. If this field is left blank, the default value is 0.33.

#### Field: Cell Maximum  Water Flow Rate Fraction

This numeric field specifies the allowable largest fraction of the design water flow rate. If this field is left blank, the default value is 2.5.

**Field: Sizing Factor**

This optional numeric field allows the user to specify a sizing factor for this component. The sizing factor is used when the component design inputs are autosized: the autosizing calculations are performed as usual and the results are multiplied by the sizing factor. Sizing factor allows the user to size a component to meet part of the design load while continuing to use the autosizing feature.For this component the inputs that would be altered by the sizing factor are:

Design Water Flow Rate;

Air Flow Rate at High Fan Speed;

Fan Power at High Fan Speed;

U-Factor Times Area Value at High Fan Speed;

Air Flow Rate at Low Fan Speed;

Fan Power at Low Fan Speed;

U-Factor Times Area Value at Low Fan Speed;

Air Flow Rate in Free Convection Regime;

U-Factor Times Area Value at Free Convection Air Flow Rate.

Note that the U-Factor Times Area Value at High Fan Speed is not *multiplied* by the Sizing Factor. Instead the design tower load is multiplied by the sizing factor and the design UA then calculated as usual. The U-Factor Times Area Value at Low Fan Speed is set to a fraction of the full load design UA determined by the field Low Fan Speed U-Factor Times Area Sizing Factor. The U-Factor Times Area Value at Free Convection Air Flow Rate is set to a fraction of the design Tower UA determined by the field Free Convection Air U-Factor Times Area Value Sizing Factor.

Four examples of an IDF specification for this object are shown below:

~~~~~~~~~~~~~~~~~~~~

    CoolingTower:TwoSpeed,
       My Tower,                      !- Name
       Condenser Tower Inlet Node,    !- Water inlet node name
       Condenser Tower Outlet Node,   !- Water outlet node name
       .0011,                         !- Design water flow rate, m3/s
       16.0,                          !- Air flow rate at high fan speed, m3/s
       10000,                         !- Fan power at high fan speed, W
       3500.,                         !- Tower UA at high fan speed, W/C
       8.0,                           !- Air flow rate at low fan speed, m3/s
       1600,                          !- Fan power at low fan speed, W
       2100.,                         !- Tower UA at low fan speed, W/C
       1.6,                           !- Air flow rate for free convection, m3/s
       350,                          !- Tower UA at free convection air flow, W/C
       UFactorTimesAreaAndDesignWaterFlowRate; !- Tower performance input method
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      CoolingTower:TwoSpeed,
        My Tower,                !- Name
        Condenser Tower Inlet Node,   !- Water Inlet Node Name
        Condenser Tower Outlet Node,  !- Water Outlet Node Name
        ,                        !- Design Water Flow Rate {m3/s}
        8.0,                     !- Air Flow Rate at High Fan Speed {m3/s}
        500,                     !- Fan Power at High Fan Speed {W}
        ,                   !- U-Factor Times Area Value at High Fan Speed {W/K}
        4.0,                     !- Air Flow Rate at Low Fan Speed {m3/s}
        125,                     !- Fan Power at Low Fan Speed {W}
        ,                   !- U-Factor Times Area Value at Low Fan Speed {W/K}
        0.8,                !- Air Flow Rate in Free Convection Regime {m3/s}
        ,     !- U-Factor Times Area Value at Free Convection Air Flow Rate {W/K}
        NominalCapacity,         !- Performance Input Method
        20000.0,                 !- High Speed Nominal Capacity {W}
        10000.0,                 !- Low Speed Nominal Capacity {W}
        2000.0;                  !- Free Convection Capacity {W}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    CoolingTower:TwoSpeed,
      Big Tower1,  !- Tower Name
      Condenser Tower 1 Inlet Node,  !- Water Inlet Node Name
      Condenser Tower 1 Outlet Node,  !- Water Outlet Node Name
      ,  !- Design Water Flow Rate {m3/s}
      8.0,  !- Design High Speed Air Flow Rate {m3/s}
      500,  !- Fan Power at Design High Speed Air Flow Rate {W}
      ,  !- Tower UA Value at Design High Speed Air Flow Rate {W/K}
      4.0,  !- Design Low Speed Air Flow Rate {m3/s}
      125,  !- Fan Power at Design Low Speed Air Flow Rate {W}
      ,  !- Tower UA Value at Design Low Speed Air Flow Rate {W/K}
      0.8,  !- Air Flow Rate in Free Convection Regime {m3/s}
      ,  !- Tower UA Value at Free Convection Air Flow Rate {W/K}
      NominalCapacity,  !- Tower Performance Input Method
      20000.0,  !- Tower High Speed Nominal Capacity
      10000.0,  !- Tower Low Speed Nominal Capacity
      2000.0,  !- Tower Free Convection Nominal Capacity
      ,        !- Basin Heater Capacity {W/K}
      ,        !- Basin Heater Setpoint Temperature {C}
      ,        !- Basin Heater Operating Schedule Name
      SaturatedExit,  !- Evaporation Loss Mode
      ,  !- Evaporation Loss Factor [%/C]
      0.008 ,  !- Drift Loss Percent
      ConcentrationRatio,  !- Blowdown Calculation Mode
      3.0,  !- Blowdown Concentration Ratio
      ,  !- Schedule Name for Makeup Water Usage due to Blowdown
      Recovery Tank;  !- Name of Water Storage Tank for Supply
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    CoolingTower:TwoSpeed,
        TowerWaterSys CoolTower,   !- Name
        TowerWaterSys Pump-TowerWaterSys CoolTowerNode,  !- Water Inlet Node Name
        TowerWaterSys Supply Equipment Outlet Node,  !- Water Outlet Node Name
        AUTOSIZE,                        !- Design Water Flow Rate {m3/s}
        AUTOSIZE,                     !- Air Flow Rate at High Fan Speed {m3/s}
        AUTOSIZE,                     !- Fan Power at High Fan Speed {W}
        AUTOSIZE,           !- U-Factor Times Area Value at High Fan Speed {W/K}
        AUTOSIZE,           !- Air Flow Rate at Low Fan Speed {m3/s}
        AUTOSIZE,                     !- Fan Power at Low Fan Speed {W}
        AUTOSIZE,           !- U-Factor Times Area Value at Low Fan Speed {W/K}
        AUTOSIZE,           !- Air Flow Rate in Free Convection Regime {m3/s}
        AUTOSIZE,   !- U-Factor Times Area at Free Convection Air Flow Rate {W/K}
        UFactorTimesAreaAndDesignWaterFlowRate,     !- Performance Input Method
        ,                 !- High Speed Nominal Capacity {W}
        ,                 !- Low Speed Nominal Capacity {W}
        ,                  !- Free Convection Capacity {W}
        ,  !- Basin Heater Capacity {W/K}
        ,  !- Basin Heater Setpoint Temperature {C}
        ,  !- Basin Heater Operating Schedule Name
        SaturatedExit,  !- Evaporation Loss Mode { LossFactor | SaturatedExit }
        ,  !- Evaporation Loss Factor
        0.0080,  !- Drift Loss Percent
        ConcentrationRatio,  !- Blowdown Calculation Mode { ConcentrationRatio |
                             !- ScheduledRate }
        3.0000,  !- Blowdown Concentration Ratio
        ,  !- Blowdown Makeup Water Usage Schedule Name
        ,  !- Supply Water Storage Tank Name
        TowerWaterSys CoolTower OA ref Node,  !- Outdoor Air Inlet Node Name
        ,         !- Number of cells available
        MinimalCell, !- Cell Control
        ,        !- Cell Minimum Water Flow Rate Fraction
        ,        !- Cell Maximum Water Flow Rate Fraction
        1.0000;  !- Sizing Factor
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Tower Fan Electric Power [W]
    HVAC,Sum,Cooling Tower Fan Electric Energy [J]

    HVAC,Average,Cooling Tower Heat Transfer Rate [W]
    HVAC,Average,Cooling Tower Inlet Temperature [C]
    HVAC,Average,Cooling Tower Outlet Temperature [C]
    HVAC,Average,Cooling Tower Mass Flow Rate [kg/s]
    HVAC,Average,Cooling Tower Fan Cycling Ratio []
    HVAC,Average,Cooling Tower Operating Cells Count []
    HVAC,Average,Cooling Tower Fan Speed Level []
    HVAC,Average,Cooling Tower Bypass Fraction []

    A tower uses either mains water or storage tank for make-up water.
    When mains water is used:
    HVAC,Average,Cooling Tower Make Up Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Make Up Water Volume [m3]
    HVAC,Sum,Cooling Tower Make Up Mains Water Volume [m3]

    When storage tank water is used:
    HVAC,Average,Cooling Tower Make Up Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Make Up Water Volume [m3]
    HVAC,Sum,Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Storage Tank Water Volume [m3]
    HVAC,Sum,Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Starved Storage Tank Water Volume [m3]
    HVAC,Sum,Cooling Tower Make Up Mains Water Volume [m3]

    HVAC,Average,Cooling Tower Water Evaporation Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Evaporation Volume [m3]
    HVAC,Average,Cooling Tower Water Drift Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Drift Volume [m3]
    HVAC,Average,Cooling Tower Water Blowdown Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Blowdown Volume [m3]

    IF specified:
    HVAC,Average,Cooling Tower Basin Heater Electric Power [W]
    HVAC,Sum,Cooling Tower Basin Heater Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Tower Fan Electric Power [W]

#### Cooling Tower Fan Electric Energy [J] 

These outputs are the electric power input to the tower fans. Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility.

#### Cooling Tower Heat Transfer Rate [W]

This is the rate at which heat is removed from the condenser water loop by the tower.

#### Cooling Tower Inlet Temperature [C]

#### Cooling Tower Outlet Temperature [C]

#### Cooling Tower Mass Flow Rate [kg/s]

These outputs are the tower water inlet and outlet temperatures, and mass flow rate.

#### Cooling Tower Basin Heater Electric Power [W]

#### Cooling Tower Basin Heater Electric Energy [J]

These outputs are the electric power input to the tower basin heater. Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility

#### Cooling Tower Make Up Water Volume Flow Rate [m3/s]

#### Cooling Tower Make Up Water Volume [m3]

These outputs are the water consumed by the wet cooling tower for external water sprays used to augment heat transfer. This is the total of evaporation, drift, and blowdown.

#### Cooling Tower Water Evaporation Volume Flow Rate [m3/s]

#### Cooling Tower Water Evaporation Volume [m3]

#### Cooling Tower Water Drift Volume Flow Rate [m3/s]

#### Cooling Tower Water Drift Volume [m3]

#### Cooling Tower Water Blowdown Volume Flow Rate [m3/s]

#### Cooling Tower Water Blowdown Volume [m3]

These outputs provide the breakdown of the different components of water use during cooling tower operation.

#### Cooling Tower Make Up Mains Water Volume [m3]

This is the volume of water drawn from mains service to feed the cooling tower. If a water storage tank is used to provide water to the tower, then the following output variables will also be available.

#### Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Storage Tank Water Volume [m3]

These are the rate and volume of water provided by the Water Storage Tank.

#### Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Starved Storage Tank Water Volume [m3]

These are the rate and volume of water the Storage Tank connections was not able to provide. The starved water is assumed to come from the mains. The tower's operation is not affected by a lack of storage tank water.

#### Cooling Tower Fan Cycling Ratio []

This represents the fraction of a time-step when tower fan is on. The fan can cycle at both speeds.

#### Cooling Tower Operating Cells Count []

This represents the number of cells operating at each time step.

#### Cooling Tower Fan Speed Level []

This represents the fan speed operating at each time step: 2 for High Speed, 1 for Low Speed, and 0 when the fan is OFF.

## CoolingTower:VariableSpeed:Merkel

This variable speed tower model is based on Merkel's theory and is similar to the single-speed and two-speed tower models.  The closed-circuit cooling tower is modeled as a counter flow heat exchanger with a variable-speed fan drawing air through the tower (induced-draft configuration). The model also includes a "free convection" regime where cooling tower performance modeled with the fan off.

For this model, Merkel's theory is modified to include adjustments developed by Scheier to alter the heat transfer effectiveness based on current wetbulb, air flow rates, and water flow rates. The input requires performance curves or lookup tables to describe these three adjustment factors.

For a multi-cell tower, the capacity and air/water flow rate inputs are for the entire tower.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the cooling tower.

#### Field: Water Inlet Node Name

This alpha field contains the identifying name for the cooling tower's water inlet node.

#### Field: Water Outlet Node Name

This alpha field contains the identifying name for the cooling tower's water outlet node.

#### Field: Performance Input Method

This alpha field contains the method by which the user will specify tower performance: "**UFactorTimesAreaAndDesignWaterFlowRate**" or "**NominalCapacity**". If this field is left blank in the input data file, the default input method is assumed to be "**NominalCapacity**". If the method "" is selected, then the user must enter design UA values, design water flow rates and air flow rates as described for the previous input fields. If the method "NominalCapacity" is selected then the fields "Design Water Flow Rate", "U-Factor Times Area Value at Design Air Flow Rate" and "U-Factor Times Area Value at Free Convection Air Flow Rate" must be left blank, but the fields "Nominal Capacity" and "Free Convection Capacity" must be entered as described below.

#### Field: Heat Rejection Capacity and Nominal Capacity Sizing Ratio

This numeric field contains the value for the ratio of actual tower heat rejection to nominal capacity.  This ratio is defined at entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. Historically this ratio has been set at 1.25 based on the assumption that the tower must dissipate 0.25 W of compressor power for every what of heat removed at the chiller evaporator.  The default is 1.25.

#### Field: Nominal Capacity

This numeric input field contains the "nominal" heat rejection capacity of the cooling tower in watts under full-speed fan operation, with entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. The Heat Rejection Capacity and Nominal Capacity Sizing Ratio set in the previous field is applied to this nominal tower capacity to give the actual tower heat rejection at these operating conditions.  This field can be autosized, in which case a [Sizing:Plant](#sizingplant) object is needed for the condenser loop.

#### Field: Free Convection Nominal Capacity

This numeric input field contains the "nominal" heat rejection capacity of the cooling tower in watts when the tower is in the "free convection" regime (water flow exists but tower fan is turned off), with entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. The Heat Rejection Capacity and Nominal Capacity Sizing Ratio is applied to this free convection tower capacity to give the actual tower heat rejection at these operating conditions.The value specified for this field must be less than the value specified for the field "Tower Low-Speed Nominal Capacity". If the user does not wish to model "free convection", then this field should be set to 0.0. If the user specifies a value greater than zero, then the "Air Flow Rate in Free Convection Regime" field must contain a value greater than zero. This field may be autocalculated, in which case it is set to a fraction of the High Speed Nominal Capacity determined in the following field.

#### Field: Free Convection Nominal Capacity Sizing Factor

This numeric field contains the sizing factor to use when calculating the Free Convection Capacity. The default is 0.1.

#### Field: Design Water Flow Rate

This numeric field contains the design water flow rate through the tower in m^3^/s. This value is the flow rate of the condenser loop water being cooled by the tower. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a water flow rate greater than zero must be defined or the field can be autosized. If autosized, a [Sizing:Plant](#sizingplant) object must be defined and the design water flow rate is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections). If "Performance Input Method" is specified as "NominalCapacity", then this field can be autocalculated using the sizing factor in the following field.

#### Field: Design Water Flow Rate per Unit of Nominal Capacity

This numeric field contains a scalable sizing factor for design water flow rate that scales with nominal capacity, in units of m^3^/s/W. The default value is 5.382\*10^-8^. This field is only used if the previous field is set to autocalculate and performance input method is NominalCapacity.  (If the performance input method is set to UFactorTimesAreaAndDesignWaterFlowRate then the design water flow rate is obtained from the plant sizing result.)

#### Field: Design Air Flow Rate

This numeric field contains the tower air flow rate at full fan speed, in m^3^/s.  The value can be autocalculated, in which case the air flow rate is based off of the scalable sizing factor in the following input field.

#### Field: Design Air Flow Rate Per Unit of Nominal Capacity

This numeric field contains the sizing factor to use when calculating the design air flow rate from the nominal capacity, in units of m^3^/s/W.  The default is 2.76316\*10^-5^.  When this field is left blank, then the default value is used but the flow rate is also scaled to account for elevation (with larger volume flow rates at higher altitudes).  When a hard value is entered, even if it is the same as the default, then the design air flow rate is not also adjusted for elevation and the scaling factor is used directly.

#### Field: Minimum Air Flow Rate Ratio

This numeric field contains the minimum air flow rate ratio. The tower fan is allowed to operate between the ratio defined here and a maximum air flow rate ratio of 1.0 (which corresponds to the design [maximum] tower air flow rate). Below this value the tower is assumed to operate in the "free convection" regime with the tower fan off. The minimum air flow rate ratio must be greater than or equal to 0.1 and less than or equal to 0.5, with a default value of 0.2 if the field is left blank.

#### Field: Design Fan Power

This numeric field contains the fan power in watts at the design (maximum) air flow rate through the tower. A value greater than zero must be specified or this field can be autocalculated. When the field is autocalculated the following input field is used to size the fan power based on nominal capacity.

#### Field: Design Fan Power Per Unit of Nominal Capacity

This numeric field contains the sizing factor to use when calculating the design fan power from the nominal capacity, in units of Watts per Watt. This field is only used if the previous is set to autocalculate. The default values is 0.0105.

#### Field: Fan Power Modifier Function of Air Flow Rate Ratio Curve Name

This alpha field contains the name of a curve or table object that describes fan power ratio (fan power/design fan power) as a function of air flow rate ratio (air flow rate/design air flow rate). The curve or table object must be for one independent variable, typically a cubic, and should be normalized to 1.0 at an air flow rate ratio of 1.0.  This field is required.

#### Field: Free Convection Regime Air Flow Rate

This numeric field contains the air flow rate (m^3^/s) when the tower is in the "free convection" regime (water flow exists but tower fan is turned off). This value must be less than the value specified for the field "Design Air Flow Rate". This field may be autocalculated, in which case it is set to a fraction of the "Design Air Flow Rate" determined in the following input field. If the user does not wish to model "free convection" and is using the Performance Input Method "UFactorTimesAreaAndDesignWaterFlowRate", then this field should be set to 0.0. If the user specifies the U-Factor Times Area Value at Free Convection Air Flow Rate or Free Convection Capacity as a value greater than zero, then the free convection air flow rate must be specified greater than 0.0.

#### Field: Free Convection Regime Air Flow Rate Sizing Factor

This numeric field contains the sizing factor to use when calculating the free convection regime air flow rate.  The default is 0.1.

#### Field: Design Air Flow Rate U-Factor Times Area Value

This numeric field contains the heat transfer coefficient-area product (UA) in watts per Kelvin corresponding to the design air and water flow rates specified above. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a UA value greater than zero but less than or equal to 300,000 must be defined, or the field can be autosized. If autosized, a Plant Sizing object must be defined and the design tower UA value is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections), assuming a tower water inlet temperature of 35C and tower inlet air at 35C drybulb/25.6C wetbulb. If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically calculates the tower UA based on the tower capacity specified in the field "Nominal Capacity".

#### Field: Free Convection Regime U-Factor Times Area Value

This numeric field contains the heat transfer coefficient-area product (W/K) when the tower is in the "free convection" regime (water flow exists but tower fan is turned off). This value must be less than the value specified for the field "U-Factor Times Area Value at Design Air Flow Rate". This field may be autosized, in which case it is set to a fraction of the "U-Factor Times Area Value at Design Air Flow Rate" determined in the following input field. If the user does not wish to model "free convection" and is using the  Performance Input Method "UFactorTimesAreaAndDesignWaterFlowRate", then this field should be set to 0.0. If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically calculates the tower UA based on the tower capacity specified in the field "Free Convection Capacity".

#### Field: Free Convection U-Factor Times Area Value Sizing Factor

This numeric field contains the sizing factor to use when calculating the free convection regime U-Factor times area value.  The default is 0.1. This field is only used if the previous field is set to autocalculate and the performance input method is UFactorTimesAreaAndDesignWaterFlowRate.

#### Field: U-Factor Times Area Modifier Function of Air Flow Ratio Curve Name

This alpha field contains the name of a curve or table object that describes how the UA value varies as a function of air flow rate ratio (air flow rate/design air flow rate).  The result of this curve is multiplied by the design UA value to adjust for air flow rate, along with the two other modifiers discussed below.  The curve or table object must be for one independent variable and should be normalized to 1.0 at an air flow rate ratio of 1.0.  This field is required.

#### Field: U-Factor Times Area Modifier Function of Wetbulb Temperature Difference Curve Name

This alpha field contains the name of a curve or table object that describes how the UA value varies as a function of the current wetbulb temperature.  The result of this curve is multiplied by the design UA value to adjust for wetbulb temperatures that differ from design conditions at 25.56°C (78°F), along with the two other modifiers discussed in the previous and following fields.  The independe variable is the design wetbulb minus the current outdoor air wetbulb, in units of degrees Celsius.  The curve or table object must be for one independent variable and should be normalized to 1.0 at a wetbulb temperature difference of 0.0.  This field is required.

#### Field: U-Factor Times Area Modifier Function of Water Flow Ratio Curve Name

This alpha field contains the name of a curve or table object that describes how the UA value varies as a function of the current water flow rate ratio (water flow rate/design water flow rate). The result of this curve is multiplied by the design UA value to adjust for water flow rates that differ from design level, along with the other two modifiers discussed above. The curve or table object must be for one independent variable and should be normalized to 1.0 at a water flow ratio of 1.0.  This field is required

#### Field: Basin Heater Capacity

This numeric field contains the capacity of the tower's electric basin heater in watts per degree Kelvin. This field is used in conjunction with the Basin Heater Setpoint Temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the outdoor dry-bulb temperature. The basin heater only operates when the tower fan is off and water is not flowing through the tower, regardless of the basin heater schedule described below. The basin heater capacity must be greater than or equal to zero, with a default value of zero if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

This numeric field contains the set point temperature (˚C) for the basin heater described in the previous field. The basin heater is active when the outdoor air dry-bulb temperature falls below this setpoint temperature, as long as the tower fan is off and water is not flowing through the tower. This set point temperature must be greater than or equal to 2˚C, and the default value is 2˚C if this field is left blank.

#### Field: Basin Heater Operating Schedule Name

**This alpha field contains the name of the basin heater operating schedule. The basin heater operating schedule is assumed to be an on/off schedule and the heater is available to operate any time the schedule value is greater than 0. The basin heater operates when scheduled on and the outdoor air dry-bulb temperature is below the set point temperature described in the previous field. If this field is left blank, the basin heater is available to operate throughout the simulation. Regardless of this schedule, the basin heater may only operate when the cooling tower fan is off and water is not flowing through the tower.**

#### Field: Evaporation Loss Mode

This field is used to choose which method is used to model the amount of water evaporated by the cooling tower. There are two options: **LossFactor** or **SaturatedExit**. The default is SaturatedExit. The user-defined loss factor is entered in the following field. By assuming that the air leaving the tower is saturated, the evaporation can be directly calculated using moist air engineering calculations with data available within the cooling tower model (and does not require additional user input).

#### Field: Evaporation Loss Factor

This field is used to specify the rate of water evaporated from the cooling tower and lost to the outside air [percent/K]. This field is only used if the Evaporation Calculation Mode is set to **LossFactor**. The evaporation loss is then calculated as a fraction of the circulating condenser water flow and varies with the temperature change in the condenser water. The value entered here is in units of percent-per-degree Kelvin. The evaporation rate will equal this value times each degree Kelvin of temperature drop in the condenser water. Typical values are from 0.15 to 0.27 [percent/K]. The default is 0.2.

#### Field: Drift Loss Percent

This field is used to specify the rate of water lost to the exiting air as entrained droplets [%]. The drift loss is a percent of the condenser water flow. Typical values for towers with efficient drift eliminators are between 0.002 and 0.2% of the condenser water flow rate. The default value is 0.008%.

#### Field: Blowdown Calculation Mode

This field specifies which method is used to determine blowdown rates. There two options **ConcentrationRatio** or **ScheduledRate**. The choice will determine which of the two models below is used. The default is ConcentrationRatio.

#### Field: Blowdown Concentration Ratio

This field is used to dynamically adjust the rate of blowdown in the cooling tower as a function of the rate of evaporation. Blowdown is water intentionally drained from the tower in order to offset the build up of solids in the water that would otherwise occur because of evaporation. The value entered here is dimensionless. It can be characterized as the ratio of solids in the blowdown water to solids in the make up water. Typical values for tower operation are 3 to 5. The default value is 3.

#### Field: Blowdown Makeup Water Usage Schedule Name

This alpha field contains the name of the schedule used to define the amount of water (m^3^/s) flushed from the basin on a periodic basis to purge the tower of mineral scale build-up and other contaminants. This schedule is only used if the Blowdown Calculation Mode is set to ScheduledRate. The amount of water use due to blowdown depends on the makeup water quality and is specific to each geographical location. Typical values range from 0.0002 to 0.0013 m^3^/s (17.3 to 112.3 m^3^/day). This water usage is in addition to the amount of water lost to the atmosphere due to evaporation and/or drift. Since blowdown occurs when the basin water contaminant concentration is high, blowdown only occurs when the cooling tower is active and water is flowing through the tower (regardless of the water usage defined by this schedule).

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the tower obtains water used for evaporative cooling. If blank or omitted, then the tower will obtain water directly from the mains. If the name of a [WaterUse:Storage](#waterusestorage) object is used here, then the tower will obtain its water from that tank. If a tank is specified, the tower will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the tower needs, then the tower will still operate and obtain the rest of the water it needs from the mains (referred to as ‘Starved' water).

#### Field: Outdoor Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the cooling tower. If this field is left blank, the outdoor air conditions entering the cooling tower are taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air conditions from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air conditions are taken directly from the weather data.

#### Field: Number of Cells

This integer field contains the number of cells in the multi-cell cooling tower. If not entered, the program will assume it is a single-cell cooling tower

#### Field: Cell Control

This alpha field specifies the method used to control the number of cells used to meet the load, the two choices are:

**MinimalCell** : the program will use minimal number of cells needed, all other cells will be shut down with no water flow. It will attempt to use as few cells as possible to cool the fluid. In no case, however, will the flow per cell be allowed to exceed its maximum value defined by the *Maximum Water Flow Rate Fraction.*

**MaximalCell**: As many cells as possible will be turned on. In no case, however, will the flow per cell be allowed to drop below its minimum value specified by the *Minimum Water Flow Rate Fraction*.

#### Field: Cell Minimum  Water Flow Rate Fraction

This numeric field specifies the allowable smallest fraction of the design water flow rate. Flows less than this value will commonly result in fluid distribution problems; the pressure at each nozzle will be too weak for the fluid to be sprayed out in the correct pattern, not all the fill would be wet. If this field is left blank, the default value is 0.33.

#### Field: Cell Maximum Water Flow Rate Fraction

This numeric field specifies the allowable largest fraction of the design water flow rate. If this field is left blank, the default value is 2.5.

#### Field: Sizing Factor

This optional numeric field allows the user to specify a sizing factor for this component. The sizing factor is used when the component design inputs are autosized: the autosizing calculations are performed as usual and the results are multiplied by the sizing factor. Sizing factor allows the user to size a component to meet part of the design load while continuing to use the autosizing feature.

An example IDF specification follows.

~~~~~~~~~~~~~~~~~~~~

      CoolingTower:VariableSpeed:Merkel,
        Big Tower1, !- Name
        Condenser Tower 1 Inlet Node, !- Water Inlet Node Name
        Condenser Tower 1 Outlet Node, !- Water Outlet Node Name
        NominalCapacity, !- Performance Input Method
        1.25, !- Heat Rejection Capacity and Nominal Capacity Sizing Ratio
        20000.0,  !- Nominal Capacity
        autocalculate, !- Free Convection Nominal Capacity
        0.1, !- Free Convection Nominal Capacity Sizing Factor
        autocalculate, !- Design Water Flow Rate
        5.382E-8, !- Design Water Flow Rate per Unit of Nominal Capacity
        autocalculate, !- Design Air Flow Rate
        2.76316E-5, !- Design Air Flow Rate Per Unit of Nominal Capacity
        0.2, !- Minimum Air Flow Rate Ratio
        autocalculate, !- Design Fan Power
        0.0105, !- Design Fan Power Per Unit of Nominal Capacity
        VS tower fan power mod func air flow ratio, !- Fan Power Modifier Function of Air Flow Rate Ratio Curve Name
        autocalculate, !- Free Convection Regime Air Flow Rate
        0.1, !- Free Convection Regime Air Flow Rate Sizing Factor
        , !- Design Air Flow Rate U-Factor Times Area Value
        , !- Free Convection Regime U-Factor Times Area Value
        , !- Free Convection U-Factor Times Area Value Sizing Factor
        VS tower UA mod func air flow ratio, !- U-Factor Times Area Modifier Function of Air Flow Ratio Curve Name
        VS tower UA mod func wetbulb difference, !- U-Factor Times Area Modifier Function of Wetbulb Temperature Difference Curve Name
        VS tower UA mod func water flow ratio; !- U-Factor Times Area Modifier Function of Water Flow Ratio Curve Name

      Curve:Cubic,
        VS tower fan power mod func air flow ratio, !- Name
        0.02 , !- Coefficient1 Constant
        0.0 ,  !- Coefficient2 x
        0.0 ,  !- Coefficient3 x**2
        0.98 , !- Coefficient4 x**3
        0.2,   !- Minimum Value of x
        1.0 ,  !- Maximum Value of x
        0.0 ,  !- Minimum Curve Output
        1.0 ,  !- Maximum Curve Output
        Dimensionless,  !- Input Unit Type for X
        Dimensionless;  !- Output Unit Type

      Curve:Quadratic,
        VS tower UA mod func air flow ratio, !- Name
        0.0 , !- Coefficient1 Constant
        1.3 , !- Coefficient2 x
        -0.3 , !- Coefficient3 x**2
        0.2 , !- Minimum Value of x
        1.0 , !- Maximum Value of x
        0.0 , !- Minimum Curve Output
        1.0 , !- Maximum Curve Output
        Dimensionless,  !- Input Unit Type for X
        Dimensionless;  !- Output Unit Type

      Curve:Linear,
        VS tower UA mod func wetbulb difference , !- Name
        1.0 , !- Coefficient1 Constant
        0.0081 , !- Coefficient2 x
        -10 , !- Minimum Value of x
        25.0 , !- Maximum Value of x
        0.85 , !- Minimum Curve Output
        1.3 , !- Maximum Curve Output
        Dimensionless,  !- Input Unit Type for X
        Dimensionless; !- Output Unit Type

      Curve:Quadratic,
        VS tower UA mod func water flow ratio, !- Name
        0.1082 , !- Coefficient1 Constant
        1.667 , !- Coefficient2 x
        -0.7713 , !- Coefficient3 x**2
        0.3 , !- Minimum Value of x
        1.0 , !- Maximum Value of x
        0.5 , !- Minimum Curve Output
        1.1 , !- Maximum Curve Output
        Dimensionless,  !- Input Unit Type for X
        Dimensionless;  !- Output Unit Type
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Tower Inlet Temperature [C]
    HVAC,Average,Cooling Tower Outlet Temperature [C]
    HVAC,Average,Cooling Tower Mass Flow Rate [kg/s]
    HVAC,Average,Cooling Tower Heat Transfer Rate [W]
    HVAC,Average,Cooling Tower Fan Electric Power [W]
    HVAC,Sum,Cooling Tower Fan Electric Energy [J]
    HVAC,Average,Cooling Tower Operating Cells Count []
    HVAC,Average,Cooling Tower Fan Speed Ratio []
    A tower uses either mains water or storage tank for make-up water.
    When mains water is used:
    HVAC,Average,Cooling Tower Make Up Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Make Up Water Volume [m3]
    HVAC,Sum,Cooling Tower Make Up Mains Water Volume [m3]

    When storage tank water is used:
    HVAC,Average,Cooling Tower Make Up Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Make Up Water Volume [m3]
    HVAC,Sum,Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Storage Tank Water Volume [m3]
    HVAC,Sum,Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Starved Storage Tank Water Volume [m3]
    HVAC,Sum,Cooling Tower Make Up Mains Water Volume [m3]

    HVAC,Average,Cooling Tower Water Evaporation Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Evaporation Volume [m3]
    HVAC,Average,Cooling Tower Water Drift Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Drift Volume [m3]
    HVAC,Average,Cooling Tower Water Blowdown Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Blowdown Volume [m3]

    IF specified:
    HVAC,Average,Cooling Tower Basin Heater Electric Power [W]
    HVAC,Sum,Cooling Tower Basin Heater Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Tower Fan Electric Power [W]

#### Cooling Tower Fan Electric Energy [J] 

These outputs are the electric power input to the tower fans. Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility.

#### Cooling Tower Heat Transfer Rate [W]

This is the rate at which heat is removed from the condenser water loop by the tower.

#### Cooling Tower Inlet Temperature [C]

#### Cooling Tower Outlet Temperature [C]

#### Cooling Tower Mass Flow Rate [kg/s]

These outputs are the tower water inlet and outlet temperatures, and mass flow rate.

#### Cooling Tower Fan Speed Ratio

This output is the air flow ratio.  The current air flow rate divided by the design air flow rate with values from 0.0 to 1.0.

#### Cooling Tower Operating Cells Count []

This represents the number of cells operating at each time step.

#### Cooling Tower Basin Heater Electric Power [W]

#### Cooling Tower Basin Heater Electric Energy [J]

These outputs are the electric power input to the tower basin heater. Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility

#### Cooling Tower Make Up Water Volume Flow Rate [m3/s]

#### Cooling Tower Make Up Water Volume [m3]

These outputs are the water consumed by the wet cooling tower for external water sprays used to augment heat transfer. This is the total of evaporation, drift, and blowdown.

#### Cooling Tower Water Evaporation Volume Flow Rate [m3/s]

#### Cooling Tower Water Evaporation Volume [m3]

#### Cooling Tower Water Drift Volume Flow Rate [m3/s]

#### Cooling Tower Water Drift Volume [m3]

#### Cooling Tower Water Blowdown Volume Flow Rate [m3/s]

#### Cooling Tower Water Blowdown Volume [m3]

These outputs provide the breakdown of the different components of water use during cooling tower operation.

#### Cooling Tower Make Up Mains Water Volume [m3]

This is the volume of water drawn from mains service to feed the cooling tower. If a water storage tank is used to provide water to the tower, then the following output variables will also be available.

#### Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Storage Tank Water Volume [m3]

These are the rate and volume of water provided by the Water Storage Tank.

#### Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Starved Storage Tank Water Volume [m3]

These are the rate and volume of water the Storage Tank connections was not able to provide. The starved water is assumed to come from the mains. The tower's operation is not affected by a lack of storage tank water.

## CoolingTower:VariableSpeed

The variable speed tower model is based on empirical curve fits of manufacturer's performance data or field measurements. The user specifies tower performance at design conditions, and empirical curves are used to determine the approach temperature and fan power at off-design conditions. The user defines tower performance by entering the inlet air wet-bulb temperature, tower range, and tower approach temperature at the design conditions. The corresponding water flow rate (within ±25% of the tower's rated water mass flow rate), air flow rate, and fan power must also be specified. The model will account for tower performance in the "free convection" regime, when the tower fan is off but the water pump remains on and heat transfer still occurs (albeit at a low level). Basin heater operation and makeup water usage (due to evaporation, drift, and blowdown) are also modeled.

The cooling tower seeks to maintain the temperature of the water exiting the cooling tower at (or below) a set point. The set point schedule is defined by the field "Condenser Loop Temperature Setpoint Node Name or Reference" for the [CondenserLoop](#condenserloop) object. The model first checks to determine the impact of "free convection" on the tower exiting water temperature. If the exiting water temperature based on "free convection" is at or below the set point, then the variable-speed tower fan is not turned on. If the exiting water temperature is above the set point after "free convection" is modeled, then the variable-speed tower fan is turned on to reduce the exiting water temperature. Tower fan power is calculated based on the tower air flow rate required to achieve the exiting water set point temperature.

Cooling towers here are "wet" and consume water through evaporation, drift, and blowdown. The model can be used to predict water consumed by the towers. The last six input fields are optional and provide methods of controlling details of the water consumption calculations. The user can specifiy connections to the rest of the buildings water system by providing the name of a [WaterUse:Storage](#waterusestorage) object.

For the operation of multi-cell towers, the first step is to determine the number of cells to operate based on the cell control method – between the minimum number of cells subject to the maximum water flow rate fraction per cell, and maximum number of cells subject to the minimum water flow rate fraction per cell. If the calculated cells do not meet the loads, additional cells will be operating to help meet the loads. Inside each cell, the existing capacity controls still apply.

**For multi-cell towers, the following inputs are assumed to be for the entire tower including all cells:**

- Design Water Flow Rate; Design Air Flow Rate; Fan Power at Design Air Flow Rate;
- Air Flow Rate in Free Convection Regime; Nominal Capacity; Free Convection Capacity
- BASIN HEATER (we assume that there is a common basin)

### Inputs

#### Field: Name

This alpha field contains the identifying name for the variable speed cooling tower.

#### Field: Water Inlet Node Name

This alpha field contains the identifying name for the cooling tower's water inlet node.

#### Field: Water Outlet Node Name

This alpha field contains the identifying name for the cooling tower's water outlet node.

#### Field: Model Type

This alpha field contains the type of empirical model used to simulate the tower's thermal performance (approach temperature). Valid choices for this field are "CoolToolsCrossFlow", "CoolToolsUserDefined", "YorkCalc", or "YorkCalcUserDefined". "CoolToolsCrossFlow" and "YorkCalc" are empirical models with the equation form and model coefficients already defined within EnergyPlus. If "CoolToolsUserDefined" or "YorkCalcUserDefined" is selected, the user must specify a valid Model Coefficient Name in the next input field to reference an appropriate [CoolingTowerPerformance:CoolTools](#coolingtowerperformancecooltools) or [CoolingTowerPerformance:YorkCalc](#coolingtowerperformanceyorkcalc) object. If a user-defined model type is selected and the specified Model Coefficient Name is not found in the input data file (idf), then a severe message is issued and the simulation will terminate.

#### Field: Model Coefficient Name

This alpha field contains the identifying name for the object(s) [CoolingTowerPerformance:CoolTools](#coolingtowerperformancecooltools) or [CoolingTowerPerformance:YorkCalc](#coolingtowerperformanceyorkcalc). A single model coefficient object may be used to define coefficients for multiple variable speed cooling tower objects (i.e., the same name may be used in this input field for more than one variable speed tower). This field is only used when the field Tower Model Type described above is set to "CoolToolsUserDefined" or "YorkCalcUserDefined", and should be left blank otherwise.

#### Field: Design Inlet Air Wet-Bulb Temperature

This numeric field specifies the inlet air wet-bulb temperature (˚C) at design conditions. This design temperature should correspond with the design values for range temperature, approach temperature, water flow rate, and air flow rate specified in the following fields. The minimum value for this field is 20˚C and the default value is 25.6˚C if this field is left blank.

#### Field: Design Approach Temperature

This numeric field specifies the tower approach temperature (˚C) at design conditions. The approach temperature is the outlet water temperature minus the inlet air wet-bulb temperature. The design approach temperature should correspond with the design values for inlet air wet-bulb temperature, range temperature, water flow rate, and air flow rate specified for this tower. The value for this field must be greater than 0˚C and the default value is 3.9˚C if this field is left blank.

#### Field: Design Range Temperature

This numeric field specifies the range temperature (˚C) at design conditions. The range temperature is defined as the inlet water temperature minus the outlet water temperature. The design range temperature should correspond with the design values for inlet air wet-bulb temperature, approach temperature, water flow rate, and air flow rate specified for this tower. The value for this field must be greater than 0˚C and the default value is 5.6˚C if this field is left blank.

#### Field: Design Water Flow Rate

This numeric field contains the design water flow rate through the tower in m^3^/s. The value entered should be within ±25% of the tower's rated water mass flow rate as specified by the manufacturer if Model Type is "CoolToolsCrossFlow" or "YorkCalc". This constraint may be different for Model Type "CoolToolsUserDefined" or "YorkCalcUserDefined" (ref. [CoolingTowerPerformance:CoolTools](#coolingtowerperformancecooltools) or [CoolingTowerPerformance:YorkCalc](#coolingtowerperformanceyorkcalc)).

This value is the flow rate of the condenser loop water being cooled by the tower. A value greater than zero must be specified or this field can be autosized. A Plant Sizing object must be defined if the field is autosized and the design water flow rate is then derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections). The design water flow rate should correspond with the design values specified for the inlet air wet-bulb temperature, approach temperature, range temperature, and air flow rate. When this field is not autosized, the condenser loop flow rate specified in other objects should be within ±25% of the tower's rated water flow rate (different range is permissible if Model Type is "User Defined").

#### Field: Design Air Flow Rate

This numeric field contains the design (maximum) air flow rate through the tower in m^3^/s. A value greater than zero must be specified or this field can be autosized. Autosizing of this field does not require a Plant Sizing object since the design air flow rate is assumed to be the design fan power multiplied by a total fan efficiency of 0.5 and divided by a fan pressure rise of 190 Pascals. A correction for altitude is also included as follows:

![](media/image193.png)\


#### Field: Design Fan Power

This numeric field contains the fan power in watts at the design (maximum) air flow rate through the tower. A value greater than zero must be specified or this field can be autosized. If autosized, a Plant Sizing object must be defined and the fan power is calculated as follows:

![](media/image194.png)\


#### Field: Fan Power Ratio Function of Air Flow Rate Ratio Curve Name

This alpha field contains the curve object name for fan power ratio (fan power/design fan power) as a function of air flow rate ratio (air flow rate/design air flow rate) [ref. Performance Curves]. The curve object must be a cubic curve and should be normalized to 1.0 at an air flow rate ratio of 1.0. If this field is left blank, a theoretical fan curve is assumed where fan power ratio is directly proportional to the air flow rate ratio cubed.

#### Field: Minimum Air Flow Rate Ratio

This numeric field contains the minimum air flow rate ratio. The tower fan is allowed to operate between the ratio defined here and a maximum air flow rate ratio of 1.0 (which corresponds to the design [maximum] tower air flow rate). Below this value, the tower is assumed to operate either in the "free convection" regime with the tower fan off, or the tower fan is cycled on/off to maintain the exiting water set point temperature. The minimum air flow rate ratio must be greater than or equal to 0.2 and less than or equal to 0.5, with a default value of 0.2 if the field is left blank.

#### Field: Fraction of Tower Capacity in Free Convection Regime

This numeric field contains the fraction of tower capacity available in the free convection regime (i.e., when the tower fan is off but water continues to flow through the tower). The tower fan does not operate when the free convection tower capacity is able to meet or exceed the exiting water set point temperature. The air flow rate through the tower in the free convection regime is assumed to be this same fraction of the tower design air flow rate. The fraction of tower capacity in free convection regime must be greater than or equal to 0 and less than or equal to 0.2, with a default value of 0.125 if this field is left blank.

#### Field: Basin Heater Capacity

This numeric field contains the capacity of the tower's electric basin heater in watts per degree Kelvin. This field is used in conjunction with the Basin Heater Setpoint Temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the outdoor dry-bulb temperature. The basin heater only operates when the tower fan is off and water is not flowing through the tower, regardless of the basin heater schedule described below. The basin heater capacity must be greater than or equal to zero, with a default value of zero if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

This numeric field contains the set point temperature (˚C) for the basin heater described in the previous field. The basin heater is active when the outdoor air dry-bulb temperature falls below this setpoint temperature, as long as the tower fan is off and water is not flowing through the tower. This set point temperature must be greater than or equal to 2˚C, and the default value is 2˚C if this field is left blank.

#### Field: Basin Heater Operating Schedule Name

This alpha field contains the name of the basin heater operating schedule. The basin heater operating schedule is assumed to be an on/off schedule and the heater is available to operate any time the schedule value is greater than 0. The basin heater operates when scheduled on and the outdoor air dry-bulb temperature is below the set point temperature described in the previous field. If this field is left blank, the basin heater is available to operate throughout the simulation. Regardless of this schedule, the basin heater may only operate when the cooling tower fan is off and water is not flowing through the tower.

#### Field: Evaporation Loss Mode

This field is used to choose which method is used to model the amount of water evaporated by the cooling tower. There are two options: ‘LossFactor' or ‘SaturatedExit'. The default is SaturatedExit. The user-defined loss factor is entered in the following field. By assuming that the air leaving the tower is saturated, the evaporation can be directly calculated using moist air engineering calculations with data available within the cooling tower model (and does not require additional user input).

#### Field: Evaporation Loss Factor

This field is used to specify the rate of water evaporated from the cooling tower and lost to the outside air [percent/K]. This field is only used if the Evaporation Calculation Mode is set to LossFactor. The evaporation loss is then calculated as a fraction of the circulating condenser water flow and varies with the temperature change in the condenser water. The value entered here is in units of percent-per-degree Kelvin. The evaporation rate will equal this value times each degree Kelvin of temperature drop in the condenser water. Typical values are from 0.15 to 0.27 [percent/K]. The default is 0.2.

#### Field: Drift Loss Percent

This numeric field contains the percent (%) of design water flow rate lost to the atmosphere at the design air flow rate due to drift, which is water droplets that are entrained in the airstream as it passes through the tower. Drift is assumed to vary directly with tower air flow rate ratio, and is in addition to the amount of water lost to the atmosphere due to evaporation and/or blowdown. Typical values for water usage due to drift are from 0.05 to 0.2% of the total water circulation rate with currently-available drift eliminators from tower manufacturers. The value entered in this field must be greater than or equal to zero, and the default is zero if the field is left blank.

#### Field: Blowdown Calculation Mode

This field specifies which method is used to determine blowdown rates. There two options ‘ConcentrationRatio' or ‘ScheduledRate'. The choice will determine which of the two models below is used. The default is ConcentrationRatio.

#### Field: Blowdown Concentration Ratio

This field is used to dynamically adjust the rate of blowdown in the cooling tower as a function of the rate of evaporation. Blowdown is water intentionally drained from the tower in order to offset the build up of solids in the water that would otherwise occur because of evaporation. The value entered here is dimensionless. It can be characterized as the ratio of solids in the blowdown water to solids in the make up water. Typical values for tower operation are 3 to 5. The default value is 3.

#### Field: Blowdown Makeup Water Usage Schedule Name

This alpha field contains the name of the schedule used to define the amount of water (m^3^/s) flushed from the basin on a periodic basis to purge the condenser loop of mineral scale build-up and other contaminants. The amount of water use due to blowdown depends on the makeup water quality and is specific to each geographical location. Typical values range from 0.0002 to 0.0013 m^3^/s (17.3 to 112.3 m^3^/day). This water usage is in addition to the amount of water lost to the atmosphere due to evaporation and/or drift. Since blowdown occurs when the basin water contaminant concentration is high, blowdown only occurs when the cooling tower is active and water is flowing through the tower (regardless of the water usage defined by this schedule).

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the tower obtains water used for evaporative cooling. If blank or omitted, then the tower will obtain water directly from the mains. If the name of a WaterUser:Storage object is used here, then the tower will obtain its water from that tank. If a tank is specified, the tower will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the tower needs, then the tower will still operate and obtain the rest of the water it needs from the mains (referred to as ‘Starved' water).

#### Field: Outdoor Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the cooling tower. If this field is left blank, the outdoor air conditions entering the cooling tower are taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air conditions from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air conditions are taken directly from the weather data.

#### Field: Number of Cells

This integer field contains the number of cells in the multi-cell cooling tower. If not entered, the program will assume it is a single-cell cooling tower

#### Field: Cell Control

This alpha field specifies the method used to control the number of cells used to meet the load, the two choices are:

**MinimalCell**: the program will use minimal number of cells needed, all other cells will be shut down with no water flow. It will attempt to use as few cells as possible to cool the fluid. In no case, however, will the flow per cell be allowed to exceed its maximum value defined by the *Maximum Water Flow Rate Fraction.*

**MaximalCel** : As many cells as possible will be turned on. In no case, however, will the flow per cell be allowed to drop below its minimum value specified by the *Minimum Water Flow Rate Fraction*.

#### Field: Cell Minimum Water Flow Rate Fraction

This numeric field specifies the allowable smallest fraction of the design water flow rate. Flows less than this value will commonly result in fluid distribution problems; the pressure at each nozzle will be too weak for the fluid to be sprayed out in the correct pattern, not all the fill would be wet. If this field is left blank, the default value is 0.33.

#### Field: Cell Maximum  Water Flow Rate Fraction

**This numeric field specifies the allowable largest fraction of the design water flow rate. If this field is left blank, the default value is 2.5.**

#### Field: Sizing Factor

This optional numeric field allows the user to specify a sizing factor for this component. The sizing factor is used when the component design inputs are autosized: the autosizing calculations are performed as usual and the results are multiplied by the sizing factor. Sizing factor allows the user to size a component to meet part of the design load while continuing to use the autosizing feature.For this component the inputs that would be altered by the sizing factor are:

Design Water Flow Rate;

Design Air Flow Rate;

Design Fan Power.

An example IDF specification for this object is shown below:

~~~~~~~~~~~~~~~~~~~~

    CoolingTower:VariableSpeed,
      Big Tower1,  !- Tower Name
      Condenser 1 Inlet Node,  !- Water Inlet Node Name
      Condenser 1 Outlet Node,  !- Water Outlet Node Name
      YorkCalc,  !- Tower Model Type
      ,  !- Tower Model Coefficient Name
      25.5556,  !- Design Inlet Air Wet-Bulb Temperature {C}
      3.8889,  !- Design Approach Temperature {C}
      5.5556,  !- Design Range Temperature {C}
      0.0015,  !- Design Water Flow Rate {m3/s}
      1.6435,  !- Design Air Flow Rate {m3/s}
      275,  !- Design Fan Power {W}
      FanRatioCurve,  !- Fan Power Ratio - function of Air Flow Rate Curve Name
      0.2,  !- Minimum Air Flow Rate Ratio
      0.125,  !- Fraction of Tower Capacity in Free Convection Regime
      450.0,  !- Basin Heater Capacity {W/K}
      4.5,  !- Basin Heater Set Point Temperature {C}
      BasinSchedule,  !- Basin Heater Operating Schedule Name
      SaturatedExit,  !- Evaporation Loss Mode
      ,  !- Evaporation Loss Factor
      0.05,  !- Makeup Water Usage due to Drift {percent}
      ScheduledRate,  !- Blowdown Calculation Mode
      BlowDownSchedule,  !- Schedule Name for Makeup Water Usage due to Blowdown
      ,  !- Name of Water Storage Tank for Supply
      ,  !- Outdoor Air Inlet Node Name
      4, !- Number of Cells
      MinimalCell, !- Cell Control
      ,  !- Cell Minimum Water Flow Rate Fraction
      ,  !- Cell Maximum Water Flow Rate Fraction
      1.000 ;  !- Sizing Factor
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Tower Fan Electric Power [W]
    HVAC,Sum,Cooling Tower Fan Electric Energy [J]

    HVAC,Average,Cooling Tower Heat Transfer Rate [W]
    HVAC,Average,Cooling Tower Inlet Temperature [C]
    HVAC,Average,Cooling Tower Outlet Temperature [C]
    HVAC,Average,Cooling Tower Mass Flow Rate [kg/s]
    HVAC,Average,Cooling Tower Fan Part Load Ratio
    HVAC,Average,Cooling Tower Air Flow Rate Ratio
    HVAC,Average,Cooling Tower Operating Cells Count

    A tower uses either mains water or storage tank for make-up water.
    When mains water is used:
    HVAC,Average,Cooling Tower Make Up Water Volume Flow Rate  [m3/s]
    HVAC,Sum,Cooling Tower Make Up Water Volume [m3]
    HVAC,Sum,Cooling Tower Make Up Mains Water Volume [m3]

    When storage tank water is used:
    HVAC,Average,Cooling Tower Make Up Water Volume Flow Rate  [m3/s]
    HVAC,Sum,Cooling Tower Make Up Water Volume [m3]
    HVAC,Sum,Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Storage Tank Water Volume [m3]
    HVAC,Sum,Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Starved Storage Tank Water Volume [m3]
    HVAC,Sum,Cooling Tower Make Up Mains Water Volume [m3]

    HVAC,Average,Cooling Tower Water Evaporation Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Evaporation Volume [m3]
    HVAC,Average,Cooling Tower Water Drift Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Drift Volume [m3]
    HVAC,Average,Cooling Tower Water Blowdown Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Blowdown Volume [m3]

    IF specified:
    HVAC,Average,Cooling Tower Basin Heater Electric Power [W]
    HVAC,Sum,Cooling Tower Basin Heater Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Tower Fan Electric Power [W]

#### Cooling Tower Fan Electric Energy [J] 

These outputs are the electric power input to the tower fan. Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility.

#### Cooling Tower Fan Part Load Ratio []

This is the on/off cycling rate of the tower fan when free convection cannot meet the set point temperature and the tower capacity at the minimum air flow rate ratio drives the tower exiting water temperature below the set point temperature. The fan part-load ratio is calculated as the ratio of the exiting water temperature in the free convection regime minus the exiting water temperature set point divided by the exiting water temperature in the free convection regime minus the exiting water temperature at the minimum air flow rate ratio. If tower air flow is at or above the minimum air flow rate ratio, then the tower fan part-load ratio is 1.0.

#### Cooling Tower Heat Transfer Rate [W]

This is the rate at which heat is removed from the condenser water loop by the tower.

#### Cooling Tower Inlet Temperature [C]

#### Cooling Tower Outlet Temperature [C]

#### Cooling Tower Mass Flow Rate [kg/s]

These outputs are the tower water inlet and outlet temperatures, and mass flow rate of the circulating condenser water loop.

#### Cooling Tower Air Flow Rate Ratio []

This is the ratio of air flow through the tower to the design air flow rate. During times when the tower can maintain the condenser loop set point temperature using free convection (when the fan in not operating), the air flow rate ratio is assumed to be equal to the field Fraction of Tower Capacity in Free Convection Regime. During times when the fan cycles on/off to maintain the outlet water set point temperature, the air flow rate ratio is calculated as the summation of Fan Part-Load Ratio multiplied by the Minimum Air Flow Rate Ratio and (1.0 - Fan Part-Load Ratio) multiplied by the fraction of Tower Capacity in Free Convection Regime.

#### Cooling Tower Basin Heater Electric Power [W]

#### Cooling Tower Basin Heater Electric Energy [J]

These outputs are the electric power input to the tower basin heater. Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility.

#### Cooling Tower Make Up Water Volume Flow Rate [m3/s]

#### Cooling Tower Make Up Water Volume [m3]

These outputs are the water consumed by the wet cooling tower for external water sprays used to augment heat transfer. This is the total of evaporation, drift, and blowdown.

#### Cooling Tower Water Evaporation Volume Flow Rate [m3/s]

#### Cooling Tower Water Evaporation Volume [m3]

#### Cooling Tower Water Drift Volume Flow Rate [m3/s]

#### Cooling Tower Water Drift Volume [m3]

#### Cooling Tower Water Blowdown Volume Flow Rate [m3/s]

#### Cooling Tower Water Blowdown Volume [m3]

These outputs provide the breakdown of the different components of water use during cooling tower operation.

#### Cooling Tower Make Up Mains Water Volume [m3]

This is the volume of water drawn from mains service to feed the cooling tower.  If a water storage tank is used to provide water to the tower, then the following output variables will also be available.

#### Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Storage Tank Water Volume [m3]

These are the rate and volume of water provided by the Water Storage Tank.

#### Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Starved Storage Tank Water Volume [m3]

These are the rate and volume of water the Storage Tank connections was not able to provide. The starved water is assumed to come from the mains. The tower's operation is not affected by a lack of storage tank water.

#### Cooling Tower Operating Cells Count []

This represents the number of cells operating at each time step.

## CoolingTowerPerformance:CoolTools

Variable speed cooling towers can be modeled by EnergyPlus with user-selectable performance based on the CoolTools correlation, YorkCalc correlation, or user-defined coefficients for either the CoolTools or YorkCalc correlations. The empirical CoolTools tower correlation uses a set of 35 coefficients to model the thermal performance (approach temperature) of a cooling tower based on four independent variables. If the user specifies Model Type = CoolToolsCrossFlow in the [CoolingTower:VariableSpeed](#coolingtowervariablespeed) object, then the 35 coefficients derived for the CoolTools simulation model are used and these coefficients are already defined within EnergyPlus. If the user specifies Model Type = CoolToolsUserDefined, then the user must enter a [CoolingTowerPerformance:CoolTools](#coolingtowerperformancecooltools) object to define the 35 coefficients that will be used by the CoolTools correlation.

The user must specify a name for the model coefficient object, and this name must be used in the [CoolingTower:VariableSpeed](#coolingtowervariablespeed) object (field Model Coefficient Name) to tell the program to use these coefficients. Next, the user enters the minimum and maximum values for inlet air wet-bulb temperature, range temperature, approach temperature, and water mass flow rate ratio to specify the valid range for which the model coefficients were derived. For all of these variables, the program issues warnings if the actual values are beyond the minimum/maximum values specified. For inlet air wet-bulb temperature and water mass flow rate ratio, the values of these variables used in the calculation of approach temperature are limited to be within the valid minimum/maximum range specified. For approach and range, the warnings are issued if the values are beyond the specified minimum/maximum range but the actual values are still used.

The CoolTools correlation has four independent variables: inlet air wet-bulb temperature (Twb), tower range temperature (Tr), water flow rate ratio (FRwater), and air flow rate ratio (FRair). Temperatures are in units of ˚C and flow rate ratios are dimensionless (actual flow rate divided by design flow rate). Using these independent variables, tower approach temperature (˚C) is calculated as follows:

Approach =   Coeff(1)  +  Coeff(2)•FRair  +  Coeff(3)•(FRair)^2^  +

Coeff(4)•(FRair)^3^  +  Coeff(5)•FRwater  +

Coeff(6)•FRair•FRwater  +  Coeff(7)•(FRair)^2^•FRwater  +

Coeff(8)•(FRwater)^2^  +  Coeff(9)•FRair•(FRwater)^2^  +

Coeff(10)•(FRwater)^3^  +  Coeff(11)•Twb  +  Coeff(12)•FRair•Twb  +

Coeff(13)•(FRair)^2^•Twb  +  Coeff(14)•FRwater•Twb  +

Coeff(15)•FRair•FRwater•Twb  +  Coeff(16)•(FRwater)^2^•Twb  +

Coeff(17)•(Twb)^2^  +  Coeff(18)•FRair•(Twb)^2^  +

Coeff(19)•FRwater•(Twb)^2^  +  Coeff(20)•(Twb)^3^  +  Coeff(21)•Tr  +

Coeff(22)•FRair•Tr  +  Coeff(23)•FRair•FRair•Tr  +

Coeff(24)•FRwater•Tr  +  Coeff(25)•FRair•FRwater•Tr  +

Coeff(26)•(FRwater)^2^•Tr  +  Coeff(27)•Twb•Tr  +

Coeff(28)•FRair•Twb•Tr  +  Coeff(29)•FRwater•Twb•Tr  +

Coeff(30)•(Twb)^2^•Tr  +  Coeff(31)•(Tr)^2^+ Coeff(32)•FRair•(Tr)^2^+

Coeff(33)•FRwater•(Tr)^2^+  Coeff(34)•Twb•(Tr)^2^+  Coeff(35)•(Tr)^3^

This object allows the user to specify model coefficients for use with the CoolTools correlation shown above. It is recommended that a broad set of cooling tower performance data be used to generate these model coefficients. The data set used to create the model coefficients should cover the entire range of water and air flow rate ratios and inlet air wet-bulb, range, and approach temperatures expected during the simulation.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the variable speed cooling tower model coefficients.

#### Field: Minimum Inlet Air Wet-Bulb Temperature

This numeric field contains the minimum inlet air wet-bulb temperature to be used by the model (approach temperature correlation). Inlet air wet-bulb temperatures less than this value will not be used; instead, the minimum inlet air wet-bulb temperature specified here will be used by the correlation and a warning will be issued.

#### Field: Maximum Inlet Air Wet-Bulb Temperature

This numeric field contains the maximum inlet air wet-bulb temperature to be used by the model (approach temperature correlation). Inlet air wet-bulb temperatures greater than this value will not be used; instead, the maximum inlet air wet-bulb temperature specified here will be used by the correlation and a warning will be issued.

#### Field: Minimum Range Temperature

This numeric field contains the minimum range temperature (inlet water temperature minus outlet water temperature) to be used by the empirical model. If the range temperature is less than this value the actual range temperature is still passed to the empirical model but a warning will be issued.

#### Field: Maximum Range Temperature

This numeric field contains the maximum range temperature (inlet water temperature minus outlet water temperature) to be used by the empirical model. If the range temperature is greater than this value the actual range temperature is still passed to the empirical model but a warning will be issued.

#### Field: Minimum Approach Temperature

This numeric field contains the minimum approach temperature (outlet water temperature minus inlet air wet-bulb temperature) to be used by the empirical model. If the calculated approach temperature is less than this value then the calculated value is still used but a warning will be issued.

#### Field: Maximum Approach Temperature

This numeric field contains the maximum approach temperature (outlet water temperature minus inlet air wet-bulb temperature) to be used by the empirical model. If the calculated approach temperature is greater than this value then the calculated value is still used but a warning will be issued.

#### Field: Minimum Water Flow Rate Ratio

This numeric field contains the minimum water flow rate ratio (ratio of actual water flow rate to rated water flow rate) to be used by the empirical model. Water flow rate ratios less than this value will not be used; instead, the minimum water flow rate ratio specified here will be used by the model and a warning will be issued.

#### Field: Maximum Water Flow Rate Ratio

This numeric field contains the maximum water flow rate ratio (ratio of actual water flow rate to rated water flow rate) to be used by the empirical model. Water flow rate ratios greater than this value will not be used; instead, the maximum water flow rate ratio specified here will be used by the model and a warning will be issued.

#### Field: Coefficient 1 to 35

These numeric fields contain the coefficients to be used by the CoolTools approach temperature correlation shown above.

An example IDF specification for this object is shown below:

~~~~~~~~~~~~~~~~~~~~

    CoolingTowerPerformance:CoolTools,
     CoolTools CrossFlow Default Model, !- Tower Model Coefficient Name
         -1.0,               !- Minimum Inlet Air Wet-Bulb Temperature {C}
         26.6667,            !- Maximum Inlet Air Wet-Bulb Temperature {C}
         1.1111,             !- Minimum Range Temperature {C}
         11.1111,            !- Maximum Range Temperature {C}
         1.1111,             !- Minimum Approach Temperature {C}
         11.1111,            !- Maximum Approach Temperature {C}
         0.75,               !- Minimum Water Flow Rate Ratio
         1.25,               !- Maximum Water Flow Rate Ratio
         0.52049709836241,                !- Coefficient 1
       -10.617046395344,                  !- Coefficient 2
        10.7292974722538,                 !- Coefficient 3
        -2.74988377158227,                !- Coefficient 4
         4.73629943913743,                !- Coefficient 5
        -8.25759700874711,                !- Coefficient 6
         1.57640938114136,                !- Coefficient 7
         6.51119643791324,                !- Coefficient 8
         1.50433525206692,                !- Coefficient 9
        -3.2888529287801,                 !- Coefficient 10
         0.0257786145353773,              !- Coefficient 11
         0.182464289315254,               !- Coefficient 12
        -0.0818947291400898,              !- Coefficient 13
        -0.215010003996285,               !- Coefficient 14
         0.0186741309635284,              !- Coefficient 15
         0.0536824177590012,              !- Coefficient 16
        -0.00270968955115031,             !- Coefficient 17
         0.00112277498589279,             !- Coefficient 18
        -0.00127758497497718,             !- Coefficient 19
         0.0000760420796601607,           !- Coefficient 20
         1.43600088336017,                !- Coefficient 21
        -0.5198695909109,                 !- Coefficient 22
         0.117339576910507,               !- Coefficient 23
         1.50492810819924,                !- Coefficient 24
        -0.135898905926974,               !- Coefficient 25
        -0.152577581866506,               !- Coefficient 26
        -0.0533843828114562,              !- Coefficient 27
         0.00493294869565511,             !- Coefficient 28
        -0.00796260394174197,             !- Coefficient 29
         0.000222619828621544,            !- Coefficient 30
        -0.0543952001568055,              !- Coefficient 31
         0.00474266879161693,             !- Coefficient 32
        -0.0185854671815598,              !- Coefficient 33
         0.00115667701293848,             !- Coefficient 34
         0.000807370664460284;            !- Coefficient 35
~~~~~~~~~~~~~~~~~~~~

### Outputs

No additional cooling tower output variables are output when this object is used.

## CoolingTowerPerformance:YorkCalc

Variable speed cooling towers can be modeled by EnergyPlus with user-selectable performance based on the CoolTools correlation, YorkCalc correlation, or user-defined coefficients for either the CoolTools or YorkCalc correlations. The empirical YorkCalc tower correlation uses a set of 27 coefficients to model the thermal performance (approach temperature) of a variable speed cooling tower based on three independent variables. If the user specifies Tower Model Type = YorkCalc in the [CoolingTower:VariableSpeed](#coolingtowervariablespeed) object, then the 27 coefficients derived for the YorkCalc simulation model are used and these coefficients are already defined within EnergyPlus. If the user specifies Tower Model Type = YorkCalcUserDefined, then the user must enter a [CoolingTowerPerformance:YorkCalc](#coolingtowerperformanceyorkcalc) object to define the 27 coefficients that will be used by the YorkCalc correlation.

The user must specify a name for the model coefficient object, and this name must be used in the [CoolingTower:VariableSpeed](#coolingtowervariablespeed) object (field Model Coefficient Name) to tell the program to use these coefficients. Next, the user enters the minimum and maximum values for inlet air wet-bulb temperature, range temperature, approach temperature, and water mass flow rate ratio to specify the valid range for which the model coefficients were derived. The user also specifies the maximum valid liquid-to-gas ratio. For all of these variables, the program issues warnings if the actual values are beyond the minimum/maximum values specified. For inlet air wet-bulb temperature and water mass flow rate ratio, the values of these variables used in the calculation of approach temperature are limited to be within the valid minimum/maximum range specified. For approach, range, and liquid-to-gas ratio the warnings are issued if the values are beyond the specified minimum/maximum range but the actual values are still used.

The YorkCalc correlation has three independent variables: inlet air wet-bulb temperature (Twb), tower range temperature (Tr), and the liquid-to-gas ratio (ratio of water flow rate ratio to air flow rate ratio = LGRatio). Temperatures are in units of ˚C and liquid-to-gas ratio is dimensionless. Using these independent variables, an approach temperature (˚C) is calculated as follows:

    Approach =      Coeff(1)  +  Coeff(2)•Twb  +  Coeff(3)•Twb^2^+  Coeff(4)•Tr  +

Coeff(5)•Twb•Tr  +  Coeff(6)•Twb^2^•Tr  +  Coeff(7)•Tr^2^  +

Coeff(8)•Twb•Tr^2^+  Coeff(9)•Twb^2^•Tr^2^+ Coeff(10)•LGRatio  +

Coeff(11)•Twb•LGRatio  +  Coeff(12)•Twb^2^•LGRatio  +

Coeff(13)•Tr•LGRatio  +  Coeff(14)•Twb•Tr•LGRatio  +

Coeff(15)•Twb^2^•Tr•LGRatio  +  Coeff(16)•Tr^2^•LGRatio  +

Coeff(17)•Twb•Tr^2^•LGRatio  +  Coeff(18)•Twb^2^•Tr^2^•LGRatio +

Coeff(19)•LGRatio^2^+  Coeff(20)•Twb•LGRatio^2^  +

Coeff(21)• Twb^2^•LGRatio^2^+  Coeff(22)•Tr•LGRatio^2^+

Coeff(23)•Twb•Tr•LGRatio^2^+  Coeff(24)•Twb^2^•Tr•LGRatio^2^+

Coeff(25)•Tr^2^•LGRatio^2^+  Coeff(26)•Twb•Tr^2^•LGRatio^2^+

Coeff(27)•Twb^2^•Tr^2^•LGRatio^2^

This object allows the user to specify model coefficients for use with the YorkCalc correlation shown above. It is recommended that a broad set of cooling tower performance data be used to generate these model coefficients. The data set used to create the model coefficients should cover the entire range of water and air flow rate ratios and inlet air wet-bulb, range, and approach temperatures expected during the simulation.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the variable speed cooling tower model coefficients.

#### Field: Minimum Inlet Air Wet-Bulb Temperature

This numeric field contains the minimum inlet air wet-bulb temperature to be used by the model (approach temperature correlation). Inlet air wet-bulb temperatures less than this value will not be used; instead, the minimum inlet air wet-bulb temperature specified here will be used by the correlation and a warning will be issued.

#### Field: Maximum Inlet Air Wet-Bulb Temperature

This numeric field contains the maximum inlet air wet-bulb temperature to be used by the model (approach temperature correlation). Inlet air wet-bulb temperatures greater than this value will not be used; instead, the maximum inlet air wet-bulb temperature specified here will be used by the model and a warning will be issued.

#### Field: Minimum Range Temperature

This numeric field contains the minimum range temperature (inlet water temperature minus outlet water temperature) to be used by the empirical model. If the range temperature is less than this value the actual range temperature is still passed to the empirical model but a warning will be issued.

#### Field: Maximum Range Temperature

This numeric field contains the maximum range temperature (inlet water temperature minus outlet water temperature) to be used by the empirical model. If the range temperature is greater than this value the actual range temperature is still passed to the empirical model but a warning will be issued.

#### Field: Minimum Approach Temperature

This numeric field contains the minimum approach temperature (outlet water temperature minus inlet air wet-bulb temperature) to be used by the empirical model. If the calculated approach temperature is less than this value then the calculated value is still used but a warning will be issued.

#### Field: Maximum Approach Temperature

This numeric field contains the maximum approach temperature (outlet water temperature minus inlet air wet-bulb temperature) to be used by the empirical model. If the calculated approach temperature is greater than this value then the calculated value is still used but a warning will be issued.

#### Field: Minimum Water Flow Rate Ratio

This numeric field contains the minimum water flow rate ratio (ratio of actual water flow rate to rated water flow rate) to be used by the empirical model. Water flow rate ratios less than this value will not be used; instead, the minimum water flow rate ratio specified here will be used by the model and a warning will be issued.

#### Field: Maximum Water Flow Rate Ratio

This numeric field contains the maximum water flow rate ratio (ratio of actual water flow rate to rated water flow rate) to be used by the empirical model. Water flow rate ratios greater than this value will not be used; instead, the maximum water flow rate ratio specified here will be used by the model and a warning will be issued.

#### Field: Maximum Liquid to Gas Ratio

This numeric field contains the maximum liquid-to-gas ratio (ratio of actual water flow rate ratio [capped to be within the minimum/maximum water flow rate ratio defined above as necessary] to actual air flow rate ratio) to be used by the empirical model. If the liquid-to-gas ratio is greater than this value the actual liquid to gas ratio is still passed to the empirical model but a warning will be issued.

#### Field: Coefficient 1 to 27

These numeric fields contain the coefficients to be used by the YorkCalc approach temperature correlation shown above.

An example IDF specification for this object is shown below:

~~~~~~~~~~~~~~~~~~~~

    CoolingTowerPerformance:YorkCalc
     YorkCalc Default Tower Model, !- Tower Model Coefficient Name
         -34.4,              !- Minimum Inlet Air Wet-Bulb Temperature {C}
         26.6667,            !- Maximum Inlet Air Wet-Bulb Temperature {C}
         1.1111,             !- Minimum Range Temperature {C}
         22.2222,            !- Maximum Range Temperature {C}
         1.1111,             !- Minimum Approach Temperature {C}
         40.0,               !- Maximum Approach Temperature {C}
         0.75,               !- Minimum Water Flow Rate Ratio
         1.25,               !- Maximum Water Flow Rate Ratio
         8.0,                !- Maximum Liquid to Gas Ratio
        -0.359741205,                     !- Coefficient 1
        -0.055053608,                     !- Coefficient 2
         0.0023850432,                    !- Coefficient 3
         0.173926877,                     !- Coefficient 4
        -0.0248473764,                    !- Coefficient 5
         0.00048430224,                   !- Coefficient 6
        -0.005589849456,                  !- Coefficient 7
         0.0005770079712,                 !- Coefficient 8
        -0.00001342427256,                !- Coefficient 9
         2.84765801111111,                !- Coefficient 10
        -0.121765149,                     !- Coefficient 11
         0.0014599242,                    !- Coefficient 12
         1.680428651,                     !- Coefficient 13
        -0.0166920786,                    !- Coefficient 14
        -0.0007190532,                    !- Coefficient 15
        -0.025485194448,                  !- Coefficient 16
         0.0000487491696,                 !- Coefficient 17
         0.00002719234152,                !- Coefficient 18
        -0.0653766255555556,              !- Coefficient 19
        -0.002278167,                     !- Coefficient 20
         0.0002500254,                    !- Coefficient 21
        -0.0910565458,                    !- Coefficient 22
         0.00318176316,                   !- Coefficient 23
         0.000038621772,                  !- Coefficient 24
        -0.0034285382352,                 !- Coefficient 25
         0.00000856589904,                !- Coefficient 26
        -0.000001516821552;               !- Coefficient 27
~~~~~~~~~~~~~~~~~~~~

### Outputs

No additional cooling tower output variables are output when this object is used.

## EvaporativeFluidCooler:SingleSpeed

Evaporative fluid coolers are components that may be assigned to condenser loops. The Evaporative fluid cooler is modeled as a counter flow heat exchanger with single-speed fans (induced draft configuration). The user must define fluid cooler performance via one of three methods: design heat transfer coefficient-area product (UA) and design water flow rate, or standard fluid cooler design capacity at a specific rating point or design capacity at non standard conditions. Regardless of which method is chosen, the design airflow rate and corresponding fan power must be specified.

The evaporative fluid cooler seeks to maintain the temperature of the water exiting the evaporative fluid cooler at (or below) a set point. The set point schedule value is defined by the field "Condenser Loop Temperature Setpoint Node Name or Reference" for the [CondenserLoop](#condenserloop) object. The model first checks to see whether inlet water temperature is at or below the set point. If so, then the fluid cooler fan is not turned on and all the flow goes through bypass. If the inlet water temperature is above the set point then the fluid cooler fan is turned on to reduce the exiting water temperature to the set point. If the capacity control is FanCycling, the model assumes that part-load operation is represented by a simple linear interpolation between two steady-state regimes (i.e., fluid cooler fan on for the entire simulation timestep and fluid cooler fan off for the entire simulation timestep). Cyclic losses are not taken into account. If the capacity control is FluidBypass, the model determines the fraction of water flow to be bypassed while the remaining water goes through the fluid cooler cooling media and gets cooled, then the two water flows mix to meet the setpoint temperature. In this case, the fan runs at full speed for the entire timestep.

Evaporative fluid coolers consume water through evaporation, drift, and blowdown. The model can be used to predict water consumed by the fluid coolers. For this purpose, the last seven input fields can either be provided in the input or if nothing is specified then the default values for these fields will be used. These fields provide the methods of controlling details of the water consumption calculations. The user can specify connections to the rest of the building's water system by providing the name of a water storage tanks (i.e. [WaterUse:Storage](#waterusestorage) objects). The schematic of the system is shown below:

![Schematic diagram for evaporative fluid cooler](media/schematic-diagram-for-evaporative-fluid.png)


Where,

h = Enthalpy (j/kg-K)

m = mass flow rate (kg/s)

Subscripts

a = air

w = water

wb = wet-bulb

in = inlet

out= outlet

### Inputs

#### Field: Name

This alpha field contains the identifying name for the fluid cooler.

#### Field: Water Inlet Node Name

This alpha field contains the identifying name for the fluid cooler's water inlet node.

#### Field: Water Outlet Node Name

This alpha field contains the identifying name for the fluid cooler's water outlet node.

#### Field: Design Air Flow Rate

This numeric field contains the design air flow rate induced by the fluid cooler fan in m^3^/s. The field must contain a value greater than zero regardless of the fluid cooler performance input method. Alternately, this field can be autosized. See Engineering Reference document for autosizing calculations.

#### Field: Design Air Flow Rate Fan Power 

This numeric field contains the fan power (in watts) at the design air flow rate specified in the previous field. The field must contain a value greater than zero regardless of the fluid cooler performance input method. Alternately, this field can be autosized. See Engineering Reference document for autosizing calculations.

#### Field: Design Spray Water Flow Rate

This numeric field contains the design spray water flow rate through the fluid cooler in m^3^/s. A value greater than zero must be specified regardless of the performance input method,

#### Field: Performance Input Method

This alpha field contains the method by which the user will specify fluid cooler performance: "UFactorTimesAreaAndDesignWaterFlowRate" or "StandardDesignCapacity" or "UserSpecifiedDesignCapacity".

#### Field: Outdoor Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the fluid cooler. If this field is left blank, the outdoor air conditions entering the fluid cooler are taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air conditions from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air conditions are taken directly from the weather data.

#### Field: Heat Rejection Capacity and Nominal Capacity Sizing Ratio

This numeric field contains the value for the ratio of actual tower heat rejection to nominal capacity.  This ratio is defined at entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. Historically this ratio has been set at 1.25 based on the assumption that the tower must dissipate 0.25 W of compressor power for every what of heat removed at the chiller evaporator.  The default is 1.25.

#### Field: Standard Design Capacity

This numeric input field contains the heat rejection capacity of the fluid cooler in watts, with entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. The design water flow rate is assumed to be 5.382E-8 m^3^/s per watt of nominal capacity (3 gpm/ton). 125% of this capacity gives the actual fluid cooler heat rejection at these operating conditions (based on historical assumption that the evaporative fluid cooler must dissipate 0.25W of compressor heat for every watt of heat removed by the evaporator). This field is only used for performance input method ="StandardDesignCapacity". For other input methods this field is ignored. The standard conditions mentioned above for "standard design capacity" are already specified in the EnergyPlus. So the input fields such as design entering water temp., design entering air wet-bulb and dry-bulb temp. and design water flow rate, if provided in the input, will be ignored for the StandardDesignCapacity performance input method. Also, the standard conditions are for water as a fluid type so this performance input method can only be used with water as a fluid type (ref. [CondenserLoop](#condenserloop) object).

#### Field: Design Air Flow Rate U-factor Times Area Value 

This numeric field contains the heat transfer coefficient-area product (UA) in watts per Kelvin corresponding to the design air and water flow rates specified above. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a UA value greater than zero but less than or equal to 300,000 must be defined, or the field can be autosized. If autosized, a Plant Sizing object must be defined and the design fluid cooler UA value is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections), the fluid cooler inlet air dry-bulb and wetbulb temperature are taken from the input. This field is only used for performance input method =" UFactorTimesAreaAndDesignWaterFlowRate". For other performance input methods, this field is ignored.

#### Field: Design Water Flow Rate

This numeric field contains the design water flow rate through the fluid cooler in m^3^/s. This value is the flow rate of the condenser loop water being cooled by the fluid cooler. This field is ignored for the "StandardDesignCapacity" performance input method. If autosized, a Plant Sizing object must be defined and the design water flow rate is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections).

#### Field: User Specified Design Capacity

This numeric input field contains the heat rejection capacity of the fluid cooler in watts. Design conditions for this capacity i.e. entering air dry-bulb temperature, entering air wet-bulb temperature and entering water temperature must be provided in the input. Only used for Performance Input Method = UserSpecifiedDesignCapacity; for other performance input methods this field is ignored.

#### Field: Design Entering Water Temperature

This numeric field contains entering water temperature at nominal conditions in degrees Celsius. The design entering water temperature must be greater than the design entering air temperature. Only used for Performance Input Method = UserSpecifiedDesignCapacity; for other performance input methods this field is ignored.

#### Field: Design Entering Air Temperature

This numeric field contains entering air dry-bulb temperature at nominal conditions in degrees Celsius. The design entering air temperature must be greater than the design entering air wet-bulb temperature. Only used for Performance Input Method = UserSpecifiedDesignCapacity; for other performance input methods this field is ignored.

#### Field: Design Entering Air Wet-bulb Temperature

This numeric field contains entering air wetbulb temperature at nominal conditions in degrees Celsius. The design entering air wet-bulb temperature must be less than the design entering air (dry-bulb) temperature. Only used for Performance Input Method = UserSpecifiedDesignCapacity; for other performance input methods this field is ignored.

#### Field: Capacity Control

This alpha field contains the cooling capacity control for the evaporative fluid cooler. Two choices are available: FanCycling and FluidBypass. During part-load conditions, there are two ways to maintain the exiting water temperature at the setpoint: either cycling the evaporative fluid cooler fan, or bypassing portion of the evaporative fluid cooler water with a three-way valve. For FluidBypass, the evaporative fluid cooler fan still runs at full speed for the entire timestep, but only portion of the water flow goes through the evaporative fluid cooler media to get cooled while the remaining portion of the water flow gets bypassed. Two water flows then mix at the common water sump to meet the setpoint temperature.

#### Field: Sizing Factor

This optional numeric field allows the user to specify a sizing factor for this component. The sizing factor is used when the component design inputs are autosized: the autosizing calculations are performed as usual and the results are multiplied by the sizing factor. Sizing factor allows the user to size a component to meet part of the design load while continuing to use the autosizing feature. For this component the inputs that would be altered by the sizing factor are:

1.Design Water Flow Rate;

2.Design Air Flow Rate;

3.Fan Power at Design Air Flow Rate;

4.U-Factor Times Area Value at Design Air Flow Rate;

Note that the U-Factor Times Area Value at Design Air Flow Rate is not multiplied by the Sizing Factor. Instead the design evaporative fluid cooler load is multiplied by the sizing factor and the design UA then calculated as usual.

#### Field: Evaporation Loss Mode

This field is used to choose which method is used to model the amount of water evaporated by the evaporative fluid cooler. There are two options: LossFactor or SaturatedExit. The default is SaturatedExit. The user-defined loss factor is entered in the following field. By assuming that the air leaving the evaporative fluid cooler is saturated, the evaporation can be directly calculated using moist air engineering calculations with data available within the evaporative fluid cooler model (and does not require additional user input).

#### Field: Evaporation Loss Factor

This field is used to specify the rate of water evaporated from the evaporative fluid cooler and lost to the outside air [percent/K]. This field is only used if the Evaporation Calculation Mode is set to LossFactor. The evaporation loss is then calculated as a fraction of the circulating condenser water flow and varies with the temperature change in the condenser water. The value entered here is in units of percent-per-kelvin. The evaporation rate will equal this value times each degree Kelvin of temperature drop in the condenser water. Empirical correlation is used to calculate default loss factor if it not explicitly specified in the input file.

#### Field: Drift Loss Percent

This field is used to specify the rate of water lost to the exiting air as entrained droplets [%]. The drift loss is a percent of the condenser water flow. Default value is under investigation for now cooling tower's evaporation loss factor is taken as default value.

#### Field: Blowdown Calculation Mode

This field specifies which method is used to determine blowdown rates. There two options ConcentrationRatio or ScheduledRate. The choice will determine which of the two models below is used. The default is ConcentrationRatio.

#### Field: Blowdown Concentration Ratio

This field is used to dynamically adjust the rate of blowdown in the evaporative fluid cooler as a function of the rate of evaporation. Blowdown is water intentionally drained from the evaporative fluid cooler in order to offset the build up of solids in the water that would otherwise occur because of evaporation. The value entered here is dimensionless. It can be characterized as the ratio of solids in the blowdown water to solids in the make up water. Default value is under investigation for now cooling tower's evaporation loss factor is taken as default value.

#### Field: Blowdown Makeup Water Usage Schedule Name

This alpha field contains the name of the schedule used to define the amount of water (m3/s) flushed from the basin on a periodic basis to purge the evaporative fluid cooler of mineral scale build-up and other contaminants. This schedule is only used if the Blowdown Calculation Mode is set to ScheduledRate. The amount of water use due to blowdown depends on the makeup water quality and is specific to each geographical location. Default value is under investigation for now cooling tower's evaporation loss factor is taken as default value.This water usage is in addition to the amount of water lost to the atmosphere due to evaporation and/or drift. Since blowdown occurs when the basin water contaminant concentration is high, blowdown only occurs when the evaporative fluid cooler is active and water is flowing through the evaporative fluid cooler (regardless of the water usage defined by this schedule).

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the evaporative fluid cooler obtains water used for evaporative cooling. If blank or omitted, then the evaporative fluid cooler will obtain water directly from the mains. If the name of a [WaterUse:Storage](#waterusestorage) object is used here, then the evaporative fluid cooler will obtain its water from that tank. If a tank is specified, the evaporative fluid cooler will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the evaporative fluid cooler needs, then the evaporative fluid cooler will still operate and obtain the rest of the water it needs from the mains (referred to as ‘Starved' water).

An IDF specification for this object is shown below:

[EvaporativeFluidCooler:SingleSpeed](#evaporativefluidcoolersinglespeed),

    Big EvaporativeFluidCooler,         !- Name

    Condenser EvaporativeFluidcooler Inlet Node,  !- Water Inlet Node Name

    Condenser EvaporativeFluidcooler Outlet Node,  !- Water Outlet Node Name

    3.02,                    !- Design Air Flow Rate {m3/s}

    2250,                    !- Design Air Flow Rate Fan Power {W}

    0.002208,                !- Design Spray Water Flow Rate {m3/s}

    UserSpecifiedDesignCapacity,                        !- Performance Input Method

    ,                        !- Outdoor Air Inlet Node Name

    1.25,                    !- Heat Rejection Capacity and Nominal Capacity Sizing Ratio

    ,                        !- Standard Design Capacity {W}

    ,                        !- Design Air Flow Rate U-factor Times Area Value {W/K}

    0.001703,                !- Design Water Flow Rate {m3/s}

    87921,                   !- User Specified Design Capacity {W}

    46.11,                   !- Design Entering Water Temperature {C}

    35,                      !- Design Entering Air Temperature {C}

    25.6;                    !- Design Entering Air Wet-bulb Temperature {C}

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Tower Fan Electric Power [W]
    HVAC,Sum,Cooling Tower Fan Electric Energy [J]
    HVAC,Average,Cooling Tower Heat Transfer Rate [W]
    HVAC,Average,Cooling Tower Inlet Temperature [C]
    HVAC,Average,Cooling Tower Outlet Temperature [C]
    HVAC,Average,Cooling Tower Mass Flow Rate [kg/s]
    HVAC,Average,Cooling Tower Bypass Fraction []
    HVAC,Average,Cooling Tower Make Up Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Make Up Water Volume [m3]
    HVAC,Average,Cooling Tower Water Evaporation Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Evaporation Volume [m3]
    HVAC,Average,Cooling Tower Water Drift Volume Flow Rate [m3/s]
    HVAC,Sum,Evaporative Fluid Cooler Water Drift [m3]
    HVAC,Average,Cooling Tower Water Blowdown Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Blowdown Volume [m3]
    HVAC,Sum,Cooling Tower Make Up Mains Water Volume [m3]
    If Supply Water Storage Tank Name is specified:
    HVAC,Average,Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Storage Tank Water Volume [m3]
    HVAC,Average,Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Starved Storage Tank Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Tower Fan Electric Power [W]

The average electric power consumption of the fluid cooler fan in Watts for the timestep being reported.

#### Cooling Tower Fan Electric Energy [J] 

Fan energy use in Joules.  Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility.

#### Cooling Tower Heat Transfer Rate [W]

This is the average rate, in Watts, at which heat is removed from the condenser water loop by the fluid cooler for the time step being reported.

#### Cooling Tower Inlet Temperature [C]

The fluid temperature at the fluid cooler inlet in degrees Celsius.

#### Cooling Tower Outlet Temperature [C]

The fluid temperature at the fluid cooler outlet in degrees Celsius.

#### Cooling Tower Mass Flow Rate [kg/s]

The average fluid mass flow rate through the fluid cooler in kg/s for the time step being reported.

#### Cooling Tower Bypass Fraction []

This output is the average fluid bypass fraction for the time step being reported. See Capacity Control input field.

#### Cooling Tower Make Up Water Volume Flow Rate [m3/s]

#### Cooling Tower Make Up Water Volume [m3]

These outputs are the external spray water consumed by the evaporative fluid cooler. This is the total of evaporation, drift, and blowdown.

#### Cooling Tower Water Evaporation Volume Flow Rate [m3/s]

#### Cooling Tower Water Evaporation Volume [m3]

#### Cooling Tower Water Drift Volume Flow Rate [m3/s]

#### Cooling Tower Water Drift Volume  [m3]

#### Cooling Tower Water Blowdown Volume Flow Rate [m3/s]

#### Cooling Tower Water Blowdown Volume [m3]

These outputs provide the breakdown of the different components of water use during fluid cooler operation.

#### Cooling Tower Make Up Mains Water Volume [m3]

The volume of make up water provided by the mains in cubic meters. If no Supply Water Storage Tank is specified, then all fluid cooler make up water is provided by the mains (same as Cooling Tower Make Up Water Volume). If a Supply Water Storage Tank is specified, then the make up from the mains only occurs when sufficient water supply is not available from the tank (same as Evaporative Fluid Cooler Water Starved by Storage Tank). This output variable is metered on HeatRejection:MainsWater, MainsWater:Plant and MainsWater:Facility.

#### Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Storage Tank Water Volume [m3]

These are the rate and volume of water provided by the Supply Water Storage Tank.

#### Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Starved Storage Tank Water Volume [m3]

These are the rate and volume of water the Supply Water Storage Tank was not able to provide. The starved water is assumed to come from the mains. The fluid cooler's operation is not affected by a lack of storage tank water.

## EvaporativeFluidCooler:TwoSpeed

The two-speed evaporative fluid cooler is modeled in a similar fashion to the single-speed evaporative fluid cooler. The evaporative fluid cooler is modeled as a counter flow heat exchanger with two-speed fan (induced draft configuration. See schematic diagram in [EvaporativeFluidCooler:SingleSpeed](#evaporativefluidcoolersinglespeed) section). The user must define fluid cooler performance via one of the three methods: design heat transfer coefficient-area product (UA) and design water flow rate, or standard fluid cooler design capacity at a specific rating point or design capacity at non standard conditions. Regardless of which method is chosen, the design airflow rate and corresponding fan power must be specified.

The evaporative fluid cooler seeks to maintain the temperature of the water exiting the evaporative fluid cooler at (or below) a set point. The set point schedule value is defined by the field "Condenser Loop Temperature Setpoint Node Name or Reference" for the [CondenserLoop](#condenserloop) object. The model first checks to see whether inlet water temperature is at or below the set point. If so, then the fluid cooler fan is not turned on and all the flow goes through bypass. If the inlet water temperature is above the set point then the fluid cooler fan is turned on at low speed to reduce the exiting water temperature to the set point. If operating the fluid cooler fan at low speed does not reduce the exiting water temperature to the set point, then the fluid cooler fan is increased to its high speed.

The model assumes that part-load operation is represented by a simple linear interpolation between two steady-state regimes (i.e., fluid cooler fan at high speed for the entire simulation timestep and fluid cooler fan at low speed for the entire simulation timestep, or fluid cooler fan at low speed for the entire simulation timestep and fluid cooler fan off for the entire simulation timestep). Cyclic losses are not taken into account.

Evaporative fluid coolers consume water through evaporation, drift, and blowdown. The model can be used to predict water consumed by the evaporative fluid coolers. For this purpose, the last seven input fields can either be provided in the input or if nothing is specified then the default values for these fields will be used. These fields provide methods of controlling details of the water consumption calculations. The user can specify connections to the rest of the buildings water system by providing the name of a water storage tanks (i.e. [WaterUse:Storage](#waterusestorage) objects).

### Inputs

#### Field: Name

This alpha field contains the identifying name for the fluid cooler.

#### Field: Water Inlet Node Name

This alpha field contains the identifying name for the fluid cooler's water inlet node.

#### Field: Water Outlet Node Name

This alpha field contains the identifying name for the fluid cooler's water outlet node.

#### Field: High Fan Speed Air Flow Rate

This numeric field contains the fluid cooler air flow rate at high fan speed in m^3^/s. A value greater than zero must be defined regardless of fluid cooler performance input method. Alternately, this field can be autosized. See Engineering Reference document for fluid cooler autosizing.

#### Field: High Fan Speed Fan Power 

This numeric field contains the fan power (in Watts) at the high-speed air flow rate specified in the previous field. A value greater than zero must be specified regardless of fluid cooler performance input method, or this field can be autosized. See Engineering Reference document for fluid cooler autosizing.

#### Field: Low Fan Speed Air Flow Rate 

This numeric field contains the fluid cooler air flow rate at low fan speed in m^3^/s. This value must be greater than zero, less than the value specified for the field "Air Flow Rate at High Fan Speed". This field may be autocalculated, in which case it is set to a fraction of the "Air Flow Rate at High Fan Speed" determined in the following field.

#### Field: Low Fan Speed Air Flow Rate Sizing Factor

This numeric field contains the sizing factor for calculating the low fan speed air flow rate as a fraction of the high fan speed air flow rate.  The default is 0.5.

#### Field: Low Fan Speed Fan Power

This numeric field contains the fan power (in Watts) at the low-speed air flow rate specified in the previous field. This value must be specified greater than zero or the field may be autocalculated, in which case it is set to a fraction of the "Fan Power at High Fan Speed" determined in the following field.

#### Field: Low Fan Speed Fan Power Sizing Factor

This numeric field contains the sizing factor for calculating the low fan speed fan power as a fraction of the high fan speed fan power.  The default is 0.16.

#### Field: Design Spray Water Flow Rate

This numeric field contains the design spray water flow rate through the fluid cooler in m^3^/s. This input field must be specified for all the performance input methods.

#### Field: Performance Input Method

This alpha field contains the method by which the user will specify fluid cooler performance: "UFactorTimesAreaAndDesignWaterFlowRate" or "StandardDesignCapacity" or "UserSpecifiedDesignCapacity".

#### Field: Outdoor Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the fluid cooler. If this field is left blank, the outdoor air conditions entering the fluid cooler are taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air conditions from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air conditions are taken directly from the weather data.

#### Field: Heat Rejection Capacity and Nominal Capacity Sizing Ratio

This numeric field contains the value for the ratio of actual tower heat rejection to nominal capacity.  This ratio is defined at entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. Historically this ratio has been set at 1.25 based on the assumption that the tower must dissipate 0.25 W of compressor power for every what of heat removed at the chiller evaporator.  The default is 1.25.

#### Field: High Speed Standard Design Capacity

This numeric input field contains the heat rejection capacity of the fluid cooler in watts, with entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. The design water flow rate is assumed to be 5.382E-8 m^3^/s per watt of nominal capacity (3 gpm/ton). The Heat Rejection Capacity and Nominal Capacity Sizing Ratio set in the previous field is applied to this capacity to give the actual fluid cooler heat rejection at these operating conditions. This field is only used for performance input method ="StandardDesignCapacity". For other input methods this field is ignored. The standard conditions mentioned above for "standard design capacity" are already specified in the EnergyPlus. So the input fields such as design entering water temp., design entering air wet-bulb and dry-bulb temp. and design water flow rate, if provided in the input, will be ignored for the StandardDesignCapacity performance input method. Also, the standard conditions are for water as a fluid type so this performance input method can only be used with water as a fluid type (ref. [CondenserLoop](#condenserloop) object).

#### Field: Low Speed Standard Design Capacity

This numeric input field contains the heat rejection capacity of the fluid cooler in watts, with entering water at 35C (95F), leaving water at 29.4C (85F), entering air at 25.6C (78F) wetbulb and 35C (95F) drybulb temperatures. The design water flow rate is assumed to be 5.382E-8 m^3^/s per watt of nominal capacity (3 gpm/ton). The Heat Rejection Capacity and Nominal Capacity Sizing Ratio is applied to this capacity to give the actual fluid cooler heat rejection at these operating conditions. This field is only used for performance input method ="StandardDesignCapacity". For other input methods this field is ignored. The standard conditions mentioned above for "standard design capacity" are already specified in the EnergyPlus. So the input fields such as design entering water temp., design entering air wet-bulb and dry-bulb temp. and design water flow rate, if provided in the input, will be ignored for the StandardDesignCapacity performance input method. Also, the standard conditions are for water as a fluid type so this performance input method can only be used with water as a fluid type (ref. [CondenserLoop](#condenserloop) object). The value specified for this field must be greater than zero but less than the value specified for the field "High-Speed Standard Design Capacity". This field may be autocalculated, in which case it is set to a fraction of the High-Speed Standard Design Capacity determined in the following field.

#### Field: Low Speed Standard Capacity Sizing Factor

This numeric field contains the sizing factor to use when calculating the Low Speed Standard Design Capacity.  The default is 0.5.

#### Field: High Fan Speed U-factor Times Area Value

This numeric field contains the heat transfer coefficient-area product (UA) in watts per Kelvin corresponding to the high speed design air and water flow rates specified above. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a UA value greater than zero but less than or equal to 2,100,000 must be defined, or the field can be autosized. If autosized, a Plant Sizing object must be defined and the design fluid cooler UA value is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections), the fluid cooler inlet air dry-bulb and wetbulb temperature are taken from the input. This field is only used for performance input method =" UFactorTimesAreaAndDesignWaterFlowRate". For other input methods this field is ignored.

#### Field: Low Fan Speed U-factor Times Area Value

This numeric field contains the heat transfer coefficient-area product (UA) in watts per degree Kelvin corresponding to the low speed design air and water flow rates specified above. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a UA value greater than zero but less than or equal to 300,000 and less than the value specified for the field "U-Factor Times Area Value at High Fan Speed" must be defined. This field may be autocalculated, in which case it is set to a factor of the "U-Factor Times Area Value at High Fan Speed" determined in the following field. This field is only used for performance input method =" UFactorTimesAreaAndDesignWaterFlowRate". For other input methods this field is ignored.

#### Field: Low Fan Speed U-Factor Times Area Sizing Factor

This field contains the sizing factor to use when calculating the Low Fan Speed U-Factor Times Area Value.  The default is 0.6.

#### Field: Design Water Flow Rate

This numeric field contains the design water flow rate through the fluid cooler in m^3^/s. This value is the flow rate of the condenser loop water being cooled by the fluid cooler. This field is ignored for the "StandardDesignCapacity" performance input method. If autosized, a Plant Sizing object must be defined and the design water flow rate is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections).

#### Field: High Speed User Specified Design Capacity

This numeric input field contains the heat rejection capacity of the fluid cooler in watts. Design conditions for this capacity i.e. entering air dry-bulb temperature, entering air wet-bulb temperature and entering water temperature must be provided in the input. Only used for Performance Input Method = UserSpecifiedDesignCapacity; for other performance input methods this field is ignored.

#### Field: Low Speed User Specified Design Capacity

This numeric input field contains the heat rejection capacity of the fluid cooler in watts. Design conditions for this capacity i.e. entering air dry-bulb temperature, entering air wet-bulb temperature and entering water temperature must be provided in the input. Only used for Performance Input Method = UserSpecifiedDesignCapacity; for other performance input methods this field is ignored. This field may be autocalculated, in which case it is set to a fraction of the "High Speed User Specified Design Capacity" determined in the following field.

#### Field: Low Speed User Specified Design Capacity Sizing Factor 

This field contains the sizing factor to use when calculating the Low-Speed User Specified Design Capacity.  The default is 0.5.

#### Field: Design Entering Water Temperature

This numeric field contains entering water temperature at nominal conditions in degrees Celsius. The design entering water temperature must be greater than the design entering air temperature. Only used for Performance Input Method = UserSpecifiedDesignCapacity; for other performance input methods this field is ignored.

#### Field: Design Entering Air Temperature

This numeric field contains entering air dry-bulb temperature at nominal conditions in degrees Celsius. The design entering air temperature must be greater than the design entering air wet-bulb temperature. Only used for Performance Input Method = UserSpecifiedDesignCapacity; for other performance input methods this field is ignored.

#### Field: Design Entering Air Wet-bulb Temperature

This numeric field contains entering air wetbulb temperature at nominal conditions in degrees Celsius. The design entering air wet-bulb temperature must be less than the design entering air (dry-bulb) temperature. Only used for Performance Input Method = UserSpecifiedDesignCapacity; for other performance input methods this field is ignored.

#### Field: High Speed Sizing Factor

This optional numeric field allows the user to specify a sizing factor for this component. The sizing factor is used when the component design inputs are autosized: the autosizing calculations are performed as usual and the results are multiplied by the sizing factor. Sizing factor allows the user to size a component to meet part of the design load while continuing to use the autosizing feature. For this component the inputs that would be altered by the sizing factor are:

Design Water Flow Rate;

Air Flow Rate at High Fan Speed;

Fan Power at High Fan Speed;

U-Factor Times Area Value at High Fan Speed;

Air Flow Rate at Low Fan Speed;

Fan Power at Low Fan Speed;

U-Factor Times Area Value at Low Fan Speed;

Note that the U-Factor Times Area Value at High Fan Speed is not multiplied by the Sizing Factor. Instead the design evaporative fluid cooler load is multiplied by the sizing factor and the design UA then calculated as usual. The U-Factor Times Area Value at Low Fan Speed is set to Low Fan Speed U-Factor Times Area Sizing Factor times the full load design UA.

#### Field: Evaporation Loss Mode

This field is used to choose which method is used to model the amount of water evaporated by the evaporative fluid cooler. There are two options: LossFactor or SaturatedExit. The default is SaturatedExit. The user-defined loss factor is entered in the following field. By assuming that the air leaving the evaporative fluid cooler is saturated, the evaporation can be directly calculated using moist air engineering calculations with data available within the evaporative fluid cooler model (and does not require additional user input).

#### Field: Evaporation Loss Factor

This field is used to specify the rate of water evaporated from the evaporative fluid cooler and lost to the outside air [percent/K]. This field is only used if the Evaporation Calculation Mode is set to LossFactor. The evaporation loss is then calculated as a fraction of the circulating condenser water flow and varies with the temperature change in the condenser water. The value entered here is in units of percent-per-kelvin. The evaporation rate will equal this value times each degree Kelvin of temperature drop in the condenser water. Empirical correlation is used to calculate default loss factor if it not explicitly specified in the input file.

#### Field: Drift Loss Percent

This field is used to specify the rate of water lost to the exiting air as entrained droplets [%]. The drift loss is a percent of the condenser water flow. Default value is under investigation for now cooling tower's evaporation loss factor is taken as default value.

#### Field: Blowdown Calculation Mode

This field specifies which method is used to determine blowdown rates. There two options ConcentrationRatio or ScheduledRate. The choice will determine which of the two models below is used. The default is ConcentrationRatio.

#### Field: Blowdown Concentration Ratio

This field is used to dynamically adjust the rate of blowdown in the evaporative fluid cooler as a function of the rate of evaporation. Blowdown is water intentionally drained from the evaporative fluid cooler in order to offset the build up of solids in the water that would otherwise occur because of evaporation. The value entered here is dimensionless. It can be characterized as the ratio of solids in the blowdown water to solids in the make up water. Default value is under investigation for now cooling tower's evaporation loss factor is taken as default value.

#### Field: Blowdown Makeup Water Usage Schedule Name

This alpha field contains the name of the schedule used to define the amount of water (m3/s) flushed from the basin on a periodic basis to purge the evaporative fluid cooler of mineral scale build-up and other contaminants. This schedule is only used if the Blowdown Calculation Mode is set to ScheduledRate. The amount of water use due to blowdown depends on the makeup water quality and is specific to each geographical location. Default value is under investigation for now cooling tower's evaporation loss factor is taken as default value.This water usage is in addition to the amount of water lost to the atmosphere due to evaporation and/or drift. Since blowdown occurs when the basin water contaminant concentration is high, blowdown only occurs when the evaporative fluid cooler is active and water is flowing through the evaporative fluid cooler (regardless of the water usage defined by this schedule).

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the evaporative fluid cooler obtains water used for evaporative cooling. If blank or omitted, then the evaporative fluid cooler will obtain water directly from the mains. If the name of a [WaterUse:Storage](#waterusestorage) object is used here, then the evaporative fluid cooler will obtain its water from that tank. If a tank is specified, the evaporative fluid cooler will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the evaporative fluid cooler needs, then the evaporative fluid cooler will still operate and obtain the rest of the water it needs from the mains (referred to as ‘Starved' water).

Examples of an IDF specification for this object are shown below:

~~~~~~~~~~~~~~~~~~~~

    EvaporativeFluidCooler:TwoSpeed,
        Big EvaporativeFluidCooler,         !- Name
        Condenser EvaporativeFluidcooler Inlet Node,  !- Water Inlet Node Name
        Condenser EvaporativeFluidcooler Outlet Node,  !- Water Outlet Node Name
        9.911,                   !- High Fan Speed Air Flow Rate {m3/s}
        autosize,                !- High Fan Speed Fan Power {W}
        4.911,                   !- Low Fan Speed Air Flow Rate {m3/s}
        0.5,                     !- Low Fan Speed Air Flow Rate Sizing Factor
        autosize,                !- Low Fan Speed Fan Power {W}
        0.16,                    !- Low Fan Speed Fan Power Sizing Factor
        0.002208,                !- Design Spray Water Flow Rate {m3/s}
        UserSpecifiedDesignCapacity,    !- Performance Input Method
        ,                        !- Outdoor Air Inlet Node Name
        1.25,                    !- Heat Rejection Capacity and Nominal Capacity Sizing Ratio
        ,                        !- High Speed Standard Design Capacity {W}
        ,                        !- Low Speed Standard Design Capacity {W}
        0.5,                     !- Low Speed Standard Capacity Sizing Factor
        ,                        !- High Fan Speed U-factor Times Area Value  {W/K}
        ,                        !- Low Fan Speed U-factor Times Area Value  {W/K}
        0.6,                     !- Low Fan Speed U-Factor Times Area Sizing Factor
        0.001703,                !- Design Water Flow Rate {m3/s}
        87921,                   !- High Speed User Specified Design Capacity {W}
        47921,                   !- Low Speed User Specified Design Capacity {W}
        0.5,                     !- Low Speed User Specified Design Capacity Sizing Factor
        46.11,                   !- Design Entering Water Temperature {C}
        35,                      !- Design Entering Air Temperature {C}
        25.6;                    !- Design Entering Air Wet-bulb Temperature {C}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Tower Fan Electric Power [W]
    HVAC,Sum,Cooling Tower Fan Electric Energy [J]
    HVAC,Average,Cooling Tower Heat Transfer Rate [W]
    HVAC,Average,Cooling Tower Inlet Temperature [C]
    HVAC,Average,Cooling Tower Outlet Temperature [C]
    HVAC,Average,Cooling Tower Mass Flow Rate [kg/s]
    HVAC,Average,Cooling Tower Make Up Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Make Up Water Volume [m3]
    HVAC,Average,Cooling Tower Water Evaporation Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Evaporation Volume [m3]
    HVAC,Average,Cooling Tower Water Drift Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Drift Volume [m3]
    HVAC,Average,Cooling Tower Water Blowdown Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Water Blowdown Volume [m3]
    HVAC,Sum,Cooling Tower Make Up Mains Water Volume [m3]
    If Supply Water Storage Tank Name is specified:
    HVAC,Average,Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Storage Tank Water Volume [m3]
    HVAC,Average,Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Tower Starved Storage Tank Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Tower Fan Electric Power [W]

The average electric power consumption of the fluid cooler fan in Watts for the timestep being reported.

#### Cooling Tower Fan Electric Energy [J] 

Fan energy use in Joules.  Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility.

#### Cooling Tower Heat Transfer Rate [W]

This is the average rate, in Watts, at which heat is removed from the condenser water loop by the fluid cooler for the time step being reported.

#### Cooling Tower Inlet Temperature [C]

The fluid temperature at the fluid cooler inlet in degrees Celsius.

#### Cooling Tower Outlet Temperature [C]

The fluid temperature at the fluid cooler outlet in degrees Celsius.

#### Cooling Tower Mass Flow Rate [kg/s]

The average fluid mass flow rate through the fluid cooler in kg/s for the time step being reported.

#### Cooling Tower Make Up Water Volume Flow Rate [m3/s]

#### Cooling Tower Make Up Water Volume [m3]

These outputs are the external spray water consumed by the evaporative fluid cooler. This is the total of evaporation, drift, and blowdown.

#### Cooling Tower Water Evaporation Volume Flow Rate [m3/s]

#### Cooling Tower Water Evaporation Volume [m3]

#### Cooling Tower Water Drift Volume Flow Rate [m3/s]

#### Cooling Tower Water Drift Volume [m3]

#### Cooling Tower Water Blowdown Volume Flow Rate [m3/s]

#### Cooling Tower Water Blowdown Volume [m3]

These outputs provide the breakdown of the different components of water use during fluid cooler operation.

#### Cooling Tower Make Up Mains Water Volume [m3]

The volume of make up water provided by the mains in cubic meters. If no Supply Water Storage Tank is specified, then all fluid cooler make up water is provided by the mains (same as Cooling Tower Make Up Water Volume). If a Supply Water Storage Tank is specified, then the make up from the mains only occurs when sufficient water supply is not available from the tank (same as Cooling Tower Starved Storage Tank Water Volume). This output variable is metered on HeatRejection:MainsWater, MainsWater:Plant and MainsWater:Facility.

#### Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Storage Tank Water Volume [m3]

These are the rate and volume of water provided by the Supply Water Storage Tank.

#### Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]

#### Cooling Tower Starved Storage Tank Water Volume [m3]

These are the rate and volume of water the Supply Water Storage Tank was not able to provide. The starved water is assumed to come from the mains. The fluid cooler's operation is not affected by a lack of storage tank water.

## FluidCooler:SingleSpeed

Fluid coolers are components that may be assigned to condenser loops. The Fluid cooler is modeled as a cross flow heat exchanger (both streams unmixed) with single-speed fans (induced draft configuration). The user must define fluid cooler performance via one of the two methods: design heat transfer coefficient-area product (UA) and design water flow rate, or nominal fluid cooler capacity at a specific rating point. Regardless of which method is chosen, the design airflow rate and corresponding fan power must be specified.

The fluid cooler seeks to maintain the temperature of the water exiting the fluid cooler at (or below) a set point. The set point schedule value is defined by the field "Condenser Loop Temperature Setpoint Node Name or Reference" for the [CondenserLoop](#condenserloop) object. The model assumes that part-load operation is represented by a simple linear interpolation between two steady-state regimes (i.e., fluid cooler fan on for the entire simulation timestep and fluid cooler fan off for the entire simulation timestep). Cyclic losses are not taken into account.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the Fluid Cooler.

#### Field: Water Inlet Node Name

This alpha field contains the identifying name for the Fluid Cooler's water inlet node.

#### Field: Water Outlet Node Name

This alpha field contains the identifying name for the Fluid Cooler's water outlet node.

#### Field: Performance Input Method

This alpha field contains the method by which the user will specify fluid cooler performance: "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".

#### Field: Design Air Flow Rate U-factor Times Area Value

This numeric field contains the heat transfer coefficient-area product (UA) in watts per Kelvin corresponding to the design air and water flow rates specified above. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a UA value greater than zero but less than or equal to 300,000 must be defined, or the field can be autosized. If autosized, a Plant Sizing object must be defined and the design fluid cooler UA value is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections), the fluid cooler inlet air dry-bulb and wetbulb temperature are taken from the input. If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically calculates the fluid cooler UA based on the fluid cooler capacity and nominal conditions specified in input file.

#### Field: Nominal Capacity

This numeric input field contains the "nominal" heat rejection capacity of the fluid cooler in watts, Nominal conditions i.e. entering air dry-bulb temperature, entering air wet-bulb temperature and Entering water temperature should be provided in the input.

#### Field: Design Entering Water Temperature

This numeric field contains entering water temperature at nominal conditions in degrees Celsius. This field must be specified for both the performance input methods. The design entering water temperature must be greater than the design entering air temperature.

#### Field: Design Entering Air Temperature

This numeric field contains entering air dry-bulb temperature at nominal conditions in degrees Celsius. This field must be specified for both the performance input methods. The design entering air temperature must be greater than the design entering air wet-bulb temperature.

#### Field: Design Entering Air Wet-bulb Temperature

This numeric field contains entering air wetbulb temperature at nominal conditions in degrees Celsius. This field must be specified for both the performance input methods. The design entering air wet-bulb temperature must be less than the design entering air (dry-bulb) temperature.

#### Field: Design Water Flow Rate

This numeric field contains the design water flow rate through the fluid cooler in m^3^/s. This value is the flow rate of the condenser loop water being cooled by the fluid cooler. A water flow rate greater than zero must be defined or the field can be autosized. If autosized, a Plant Sizing object must be defined and the design water flow rate is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections).

#### Field: Design Air Flow Rate

This numeric field contains the design air flow rate induced by the fluid cooler fan in m^3^/s. A value greater than zero must be defined regardless of the fluid cooler performance input method. Alternately, this field can be autosized. See Engineering Reference document for fluid cooler autosizing.

#### Field: Design Air Flow Rate Fan Power

This numeric field contains the fan power (in watts) at the design air flow rate specified in the previous field. A value greater than zero must be specified regardless of the performance input method, or this field can be autosized. See Engineering ref. for fluid cooler autosizing.

#### Field: Outdoor Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the fluid cooler. If this field is left blank, the outdoor air conditions entering the fluid cooler are taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air conditions from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air conditions are taken directly from the weather data.

An IDF specification for this object is shown below:

[FluidCooler:SingleSpeed](#fluidcoolersinglespeed),

    Big FLUIDCOOLER1,              !- Name

    Condenser FLUIDCOOLER 1 Inlet Node,  !- Water Inlet Node Name

    Condenser FLUIDCOOLER 1 Outlet Node, !- Water Outlet Node Name

    NominalCapacity,        !- Performance Input Method

    ,                       !- Design Air Flow Rate U-factor Times Area Value {W/K}

    58601.,                 !- Nominal Capacity {W}

    51.67,                  !- Design Entering Water Temperature {C}

    35,                     !- Design Entering Air Temperature {C}

    25.6,                   !- Design Entering Air Wetbulb Temperature {C}

    0.001388,               !- Design Water Flow Rate{m3/s}

    9.911,                  !- Design Air Flow Rate {m3/s}

    Autosize;               !- Design Air Flow Rate Fan Power {W}

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Tower Fan Electric Power [W]
    HVAC,Sum,Cooling Tower Fan Electric Energy [J]
    Zone,Meter,HeatRejection:Electricity [J]
    HVAC,Average,Cooling Tower Heat Transfer Rate [W]
    HVAC,Average, Cooling Tower Inlet Temperature [C]
    HVAC,Average, Cooling Tower Outlet Temperature [C]
    HVAC,Average,Cooling Tower Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Tower Fan Electric Power [W]

The electric power consumption of the fluid cooler fans.

#### Cooling Tower Fan Electric Energy [J] 

Fan energy use.  Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility.

#### Cooling Tower Heat Transfer Rate [W]

This is the rate at which heat is removed from the condenser water loop by the fluid cooler.

#### Cooling Tower Inlet Temperature [C]

The loop temperature at the fluid cooler inlet.

#### Cooling Tower Outlet Temperature [C]

The loop temperature at the fluid cooler outlet.

#### Cooling Tower Mass Flow Rate [kg/s]

The loop mass flow rate of the circulating condenser water loop.

## FluidCooler:TwoSpeed

The two-speed fluid cooler is modeled in a similar fashion to the single-speed fluid cooler. The fluid cooler is modeled as a cross flow heat exchanger (both stream unmixed) with  two-speed fans (induced draft configuration). The user must define fluid cooler performance via one of two methods: heat transfer coefficient-area product (UA) and design water flow rate, or nominal fluid cooler capacity at a specific rating point. Regardless of which method is chosen, the airflow rate and corresponding fan power at both high and low fan speed must be specified. The Fluid Cooler seeks to maintain the temperature of the water exiting the Fluid Cooler at (or below) a set point. The set point schedule is defined by the field "Condenser Loop Temperature Setpoint Node Name or Reference" for the [CondenserLoop](#condenserloop) object. The model first runs at low speed and calculates the fluid cooler exiting water temperature. If the exiting water temperature based on "low speed" is at or below the set point, then the fluid cooler fan runs at this speed or below this speed. If the exiting water temperature remains above the set point after "low speed" is modeled, then the fluid cooler fan runs at high speed to reduce the exiting water temperature.

The model assumes that part-load operation is represented by a simple linear interpolation between two steady-state regimes (i.e., fluid cooler fan at high speed for the entire simulation timestep and fluid cooler fan at low speed for the entire simulation timestep,). Cyclic losses are not taken into account.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the fluid cooler.

#### Field: Water Inlet Node Name

This alpha field contains the identifying name for the fluid cooler's water inlet node.

#### Field: Water Outlet Node Name

This alpha field contains the identifying name for the fluid cooler's water outlet node.

#### Field: Performance Input Method

This alpha field contains the method by which the user will specify fluid cooler performance: "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".

#### Field: High Fan Speed U-factor Times Area Value

This numeric field contains the heat transfer coefficient-area product (UA) in watts per degree Celsius corresponding to the high-speed air flow rate and design water flow rate specified above. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", then a UA value greater than zero but less than or equal to 300,000 must be defined, or the field can be autosized. If autosized, a Plant Sizing object must be defined and the fluid cooler UA value at high fan speed is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections), fluid cooler water inlet temperature, fluid cooler inlet air drybulb and wetbulb are provided in the input. If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically calculates the fluid cooler UA based on the capacity specified in the field "High Speed Nominal Capacity" and nominal conditions.

#### Field: Low Fan Speed U-factor Times Area Value

This numeric field contains the heat transfer coefficient-area product (UA) in watts per Kelvin corresponding to the design water flow rate and low-speed air flow rate specified above. If the input field "Performance Input Method" is specified as "UFactorTimesAreaAndDesignWaterFlowRate", this value must be greater than zero but less than or equal to 300,000, less than the value specified for the field "U-factor Times Area Value at High Fan Speed". This field may be autocalculated, in which case it is set to a factor of the "U-factor Times Area Value at High Fan Speed" determined in the following field. If "Performance Input Method" is specified as "NominalCapacity", then this field must be left blank since the model automatically calculates the fluid cooler UA based on the fluid cooler capacity specified in the field "Low Speed Nominal Capacity".

#### Field: Low Fan Speed U-Factor Times Area Sizing Factor

This numeric field contains the sizing factor to use when calculating the Low Fan Speed U-factor Times Area Value.  The default is 0.6.

#### Field: High Speed Nominal Capacity

This numeric input field contains the "nominal" heat rejection capacity of the fluid cooler in watts under high-speed fan operation, with nominal (design) inputs entering water temperature, entering air temperature and entering air wet-bulb temperature. The design water flow rate is also provided in the input.

#### Field: Low Speed Nominal Capacity

This numeric input field contains the "nominal" heat rejection capacity of the Fluid Cooler in watts under low-speed fan operation, with nominal (design) inputs entering water temperature, entering air temperature and entering air wet-bulb temperature. The design water flow rate is also provided in the input. The value specified for this field must be greater than zero but less than the value specified for the field "High Speed Nominal Capacity". This field may be autocalculated, in which case it is set to a fraction of the "High Speed Nominal Capacity" determined in the following field.

#### Field: Low Speed Nominal Capacity Sizing Factor

This numeric field contains the sizing factor to use when calculating the Low Speed Nominal Capacity.  The default is 0.5.

#### Field: Design Entering Water Temperature

This numeric field contains entering water temperature at nominal conditions in degrees Celsius. This field must be specified for both the performance input methods. The design entering water temperature must be greater than the design entering air temperature.

#### Field: Design Entering Air Temperature

This numeric field contains entering air dry-bulb temperature at nominal conditions in degrees Celsius. This field must be specified for both the performance input methods. The design entering air temperature must be greater than the design entering air wet-bulb temperature.

#### Field: Design Entering Air Wet-bulb Temperature

This numeric field contains entering air wet-bulb temperature at nominal conditions in degrees Celsius. This field must be specified for both the performance input methods. The design entering air wet-bulb temperature must be less than the design entering air (dry-bulb) temperature.

#### Field: Design Water Flow Rate

This numeric field contains the design water flow rate through the fluid cooler in m^3^/s. This value is the flow rate of the condenser loop water being cooled by the fluid cooler. A water flow rate greater than zero must be defined or the field can be autosized. If autosized, a Plant Sizing object must be defined and the design water flow rate is derived from the design load to be rejected by the condenser loop and the design loop delta T (Ref. Sizing and Input for Design Calculations and Component Autosizing sections).

#### Field: High Fan Speed Air Flow Rate

This numeric field contains the fluid cooler air flow rate at high fan speed in m^3^/s. A value greater than zero must be defined regardless of the fluid cooler performance input method. Alternately, this field can be autosized. See Engineering ref. for fluid cooler autosizing.

#### Field: High Fan Speed Fan Power

This numeric field contains the fan power (in Watts) at the high-speed air flow rate specified in the previous field. A value greater than zero must be specified regardless of the fluid cooler performance input method, or this field can be autosized. See Engineering ref. for fluid cooler autosizing.

#### Field: Low Fan Speed Air Flow Rate

This numeric field contains the fluid cooler air flow rate at low fan speed in m^3^/s. This value must be greater than zero, less than the value specified for the field "Air Flow Rate at High Fan Speed". This field may be autocalculated, in which case it is set to a fraction of the "Air Flow Rate at High Fan Speed" determined in the following field.

#### Field: Low Fan Speed Air Flow Rate Sizing Factor

This numeric field contains the sizing factor to use when calculating the Low Fan Speed Air Flow Rate. The default is 0.5.

#### Field: Low Fan Speed Fan Power 

This numeric field contains the fan power (in Watts) at the low-speed air flow rate specified in the previous field. This value must be specified greater than zero or the field may be autocalculated, in which case it is set to set to a fraction of the "Fan Power at High Fan Speed" determined in the following field.

#### Field: Low Fan Speed Fan Power Sizing Factor

This numeric field contains the sizing factor to use whe calculating the Low Fan Speed Fan Power.  The default is 0.16.

#### Field: Outdoor Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the Fluid Cooler. If this field is left blank, the outdoor air conditions entering the Fluid Cooler are taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air conditions from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air conditions are taken directly from the weather data.

Examples of an IDF specification for this object are shown below:

~~~~~~~~~~~~~~~~~~~~

    FluidCooler:TwoSpeed,
        Big FLUIDCOOLER1,              !- Name
        Condenser FLUIDCOOLER 1 inlet Node,  !- Water Inlet Node Name
        Condenser FLUIDCOOLER 1 Outlet Node,  !- Water Outlet Node Name
        NominalCapacity,         !- Performance Input Method
        ,                        !- High Fan Speed U-factor Times Area Value {W/K}
        ,                        !- Low Fan Speed U-factor Times Area Value {W/K}
        ,                        !- Low Fan Speed U-Factor Times Area Sizing Factor
        58601.,                  !- High Speed Nominal Capacity {W}
        28601.,                  !- Low Speed Nominal Capacity {W}
    .   0.6,                     !- Low Speed Nominal Capacity Sizing Factor
        51.67,                   !- Design Entering Water tempereture {C}
        35,                      !- Design Entering Air tempereture {C}
        25.6,                    !- Design Entering Air Wet-bulb tempereture {C}
        0.001388,                !- Design Water Flow Rate {m3/s}
        9.911,                   !- High Fan Speed Air Flow Rate {m3/s}
        autosize,                !- High Fan Speed Fan Power {W}
        autosize,                !- Low Fan Speed Air Flow Rate {m3/s}
        0.5,                     !- Low Fan Speed Air Flow Rate Sizing Factor
        autosize,                !- Low Fan Speed Fan Power {W}
        0.16;                    !- Low Fan Speed Fan Power Sizing Factor
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Tower Fan Electric Power [W]
    HVAC,Sum,Cooling Tower Fan Electric Energy [J]
    HVAC,Average,Cooling Tower Heat Transfer Rate [W]
    HVAC,Average, Cooling Tower Inlet Temperature [C]
    HVAC,Average, Cooling Tower Outlet Temperature [C]
    HVAC,Average,Cooling Tower Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Tower Fan Electric Power [W]

Average fan electric power consumed.

#### Cooling Tower Fan Electric Energy [J] 

Total energy used by the fan. Consumption is metered on HeatRejection:Electricity, Electricity:Plant, and Electricity:Facility.

#### Cooling Tower Heat Transfer Rate [W]

This is the rate at which heat is removed from the condenser water loop by the fluid cooler.

#### Cooling Tower Inlet Temperature [C]

Loop temperature at the fluid cooler inlet.

#### Cooling Tower Outlet Temperature [C]

Loop temperature at the fluid cooler outlet.

#### Cooling Tower Mass Flow Rate [kg/s]

Loop mass flow rate through the fluid cooler.

## GroundHeatExchanger:Vertical

The EnergyPlus Ground loop heat exchanger is a condenser component. This serves the condenser supply side in addition to the cooling towers and other condensing components. The following figure shows the Ground Heat Exchanger in the simulation environment.

The heat exchanger response is defined by a G-function. This is a non-dimensional function that is used to calculate the response to square heat pulses of different duration. (This function is not the same as ‘G-factors' referred to in the ASHRAE Applications Handbook). This continuous function is specified by a series of data pairs (LNTTS*i*, GFNC*i*) where,

- LNTTS*i* is the non-dimensional time: *ln(T/T~s~)*
- GFNC*i* is the G-function value

The G-function is different for each borehole field configuration (i.e. a 4x4 field has a different response than a 80x80 field) and the borehole thermal resistance. It is also dependant on the ratio of borehole spacing to depth. G-function values, for accurate simulation, have to be calculated for each specific heat exchanger design. This can be done using some commercial ground loop heat exchanger design tool and the like. A reference data set, containing examples input data for 1x2, 4x4 and 8x8 configurations and for both standard and thermally enhanced grout, have also been provided. These data are provided as examples only. Custom G-function values may be generated using an external program such as GLHEPro. For more information about the datasets and GLHEPro, see the Auxiliary Programs document section "G-Function Spreadsheet."

Further details of the implementation of this model can be found in:

Murugappan, A. *Implementing Ground Source Heat Pump and Ground Loop Heat Exchanger Models in the EnergyPlus Simulation Environment*. M.S. Thesis, Oklahoma State University, December 2002.

![Schematic of EnergyPlus Ground Loop Heat Exchanger](media/schematic-of-energyplus-ground-loop-heat.png)


The data definition for the ground loop heat exchanger from the Energy+.idd is shown below. The syntax to the specification of Borehole, U-tube and ground are illustrated in the example following.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the ground heat exchanger (GHE).

#### Field: Inlet Node Name

This alpha field contains the ground heat exchanger inlet node name.

#### Field: Outlet Node Name

This alpha field contains the ground heat exchanger outlet node name.

#### Field: Maximum Flow Rate

This numeric field contains the GHE maximum design flow rate in cubic meters per second {m^3^/s}.

#### Field: Number of Bore Holes

This numeric field contains the number of bore holes in the GHE installation.

#### Field: Bore Hole Length

This numeric field contains the length of the borehole in meters {m}.

#### Field: Bore Hole Radius

This numeric field contains the radius of the borehole in meters.

#### Field: Ground Thermal Conductivity

This numeric field contains the  thermal conductivity of the ground in W/m-K.

#### FieldSet: Ground Thermal Heat Capacity

This numeric field contains the thermal heat capacity of the ground in J/m^3^-K.

#### Field: Ground Temperature

This numeric field contains the far field temperature of the ground in °C.

#### Field: Design Flow Rate

This numeric field contains the design volume flow rate of the GHE in m^3^/s.

#### Field: Grout Thermal Conductivity

This numeric field contains the thermal conductivity of the filler material in W/m-K.

#### Field: Pipe Thermal Conductivity

This numeric field contains the thermal conductivity of the pipe in W/m-K.

#### Field: Pipe Out Diameter

This numeric field contains the outer diameter of the U-tube (pipe) in meters {m}.

#### Field: U-Tube Distance

This numeric field contains the distance between the two legs of the U-tube in meters {m}.

#### Field: Pipe Thickness

This numeric field contains the outer diameter of the U-tube (pipe) in meters.

#### Field: Maximum Length of Simulation

This numeric field contains the maximum number of years of simulation to be carried out.

#### Field: G-Function Reference Ratio

The G-Functions may be formulated slightly differently based on the program which generated them. The "raw" G-Functions are based on an borehole radius to active length ratio of 0.0005. If the physical ratio is different from this, a correction must be applied. EnergyPlus will apply the correction, based on the reference ratio entered in this field. Therefore, therefore two possible input configurations.

- If the G-Functions have not had a correction applied, then the G-Functions are still based on a reference of 0.0005, so use a value of 0.0005 in this field. EnergyPlus will adjust the G-Functions internally to create the properly referenced G-Function.
- If the correction has already been applied, then the input G-Functions are based on a reference to the actual (physical) radius/length ratio, so enter the physical radius/length in this field. Entering the actual value will nullify any internal corrections, which will avoid re-basing the G-Function set.

The software GLHEPro has been making this "pre-correction" to the data sets since version 3.1 of that software, so this input field should match the actual (physical) radius/length ratio.

#### Field: Number of Data Pairs of the G Function

The borehole response is defined by a non-dimensional ‘G-function'. This is specified as a series of data points giving values of non-dimensional time *vs* G-function value (LNTTS1, GFUNC1), (LNTTS2, GFUNC2), (LNTTS3, GFUNC3) …….. (LNTTS*n*, GFUNC*n*), This numeric field contains the number of data pairs to be read in (*n*).

#### Field: G-Function Ln(T/Ts) Value <x>

*This numeric field contains the natural log of time/steady state time: ln(T/T~s~)*

#### Field: G-Function 'G' Value <x>

This numeric field contains the G-function value of the corresponding LNTTS.

The following is an example input:

~~~~~~~~~~~~~~~~~~~~

    GroundHeatExchanger:Vertical,
        Vertical Ground Heat Exchanger,  !- Name
        GHE Inlet Node,          !- Inlet Node Name
        GHE Outlet Node,         !- Outlet Node Name
        0.00330000,              !- Maximum Flow Rate {m3/s}
        120,                     !- Number of Bore Holes
        76.2,                    !- Bore Hole Length {m}
        .635080E-01,             !- Bore Hole Radius {m}
        .692626E+00,             !- Ground Thermal Conductivity {W/m-K}
        .234700E+07,             !- Ground Thermal Heat Capacity {J/m3-K}
        13.375,                  !- Ground Temperature {C}
        0.0033,                  !- Design Flow Rate {m3/s}
        .692626E+00,             !- Grout Thermal Conductivity {W/m-K}
        .391312E+00,             !- Pipe Thermal Conductivity {W/m-K}
        2.66667E-02,             !- Pipe Out Diameter {m}
        2.53977E-02,             !- U-Tube Distance {m}
        2.41285E-03,             !- Pipe Thickness {m}
        2,                       !- Maximum Length of Simulation
        0.0005,                  !- G-Function Reference Ratio
        35,                      !- Number of Data Pairs of the G Function
        ! The G-function is defined by the following data pairs
        -15.2996, -0.348322,  ! G-Function Ln(T/Ts) Value 1, G-Function G Value 1
        -14.201,   0.022208,  ! G-Function Ln(T/Ts) Value 2, G-Function G Value 2
        -13.2202,  0.412345,  ! G-Function Ln(T/Ts) Value 3, G-Function G Value 3
        -12.2086,  0.867498,  ! G-Function Ln(T/Ts) Value 4, G-Function G Value 4
        -11.1888,  1.357839,  ! G-Function Ln(T/Ts) Value 5, G-Function G Value 5
        -10.1816,  1.852024,  ! G-Function Ln(T/Ts) Value 6, G-Function G Value 6
        -9.1815,   2.345656,  ! G-Function Ln(T/Ts) Value 7, G-Function G Value 7
        -8.6809,   2.593958,  ! G-Function Ln(T/Ts) Value 8, G-Function G Value 8
        -8.5,      2.679,     ! etc, etc.
        -7.8,      3.023,
        -7.2,      3.32,
        -6.5,      3.681,
        -5.9,      4.071,
        -5.2,      4.828,
        -4.5,      6.253,
        -3.963,    7.894,
        -3.27,     11.82,
        -2.864,    15.117,
        -2.577,    18.006,
        -2.171,    22.887,
        -1.884,    26.924,
        -1.191,    38.004,
        -0.497,    49.919,
        -0.274,    53.407,
        -0.051,    56.632,
        0.196,     59.825,
        0.419,     62.349,
        0.642,     64.524,
        0.873,     66.412,
        1.112,     67.993,
        1.335,     69.162,
        1.679,     70.476,
        2.028,     71.361,
        2.275,     71.79,
        3.003,     72.511;  !- 35 PAIRS
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Ground Heat Exchanger Average Borehole Temperature [C]
    HVAC,Average,Ground Heat Exchanger Heat Transfer Rate [W]
    HVAC,Average,Ground Heat Exchanger Inlet Temperature [C]
    HVAC,Average,Ground Heat Exchanger Outlet Temperature [C]
    HVAC,Average,Ground Heat Exchanger Mass Flow Rate [kg/s]
    HVAC,Average,Ground Heat Exchanger Average Fluid Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Ground Heat Exchanger Average Borehole Temperature [C]

This is the model result for the average temperature of the borehole heat exchanger.

#### Ground Heat Exchanger Heat Transfer Rate [W]

This is the rate of heat transfer between the working fluid and the ground heat exchanger, in Watts.

#### Ground Heat Exchanger Inlet Temperature [C]

This is the temperature of the working fluid entering the ground heat exchanger.

#### Ground Heat Exchanger Outlet Temperature [C]

This is the temperature of the working fluid leaving the ground heat exchanger.

#### Ground Heat Exchanger Mass Flow Rate [kg/s]

This is the mass flow rate of the working fluid through the heat exchanger.

#### Ground Heat Exchanger Average Fluid Temperature [C]

This is the average temperature of the working fluid inside the heat exchanger.

## GroundHeatExchanger:Pond

The pond heat exchanger model represents a shallow pond with submerged hydronic tubes through which the heat transfer fluid is circulated. The model represents a 'shallow' pond in that no attempt is made to model any stratification effects that may  be present in deeper ponds.

This type of heat exchanger is intended to be connected to the supply side of a condenser loop, and can be used with any type of plant loop. The pond may be specified as the only heat exchanger on the condenser loop (as shown in Figure 84) or it may be connected in parallel with other condenser loop heat exchangers (such as cooling towers, ground surface heat exchangers) as shown in the second figure below.

![Example of Pond Ground Heat Exchanger as only heat exchanger on condenser loop](media/example-of-pond-ground-heat-exchanger-as-only.png)


![Pond Ground Heat Exchanger with other heat exchangers on condenser loop](media/pond-ground-heat-exchanger-with-other-heat.png)


### Inputs

#### Field: Name

This alpha field contains the identifying name for the outside pond heat exchanger.

#### Field: Fluid Inlet Node Name

This alpha field contains the fluid inlet node name.

#### Field: Fluid Outlet Node Name

This alpha field contains the fluid outlet node name.

#### Field: Pond Depth

This numeric field contains the pond depth {m}.

#### Field: Pond Area

This numeric field contains the pond area {m^2}^.

#### Field: Hydronic Tubing Inside Diameter

This numeric field contains the hydronic tubing inside diameter {m}.

#### Field: Hydronic Tubing Outside Diameter

This numeric field contains the hydronic tubing outside diameter {m}.

#### Field: Hydronic Tubing Thermal Conductivity

This numeric field contains the hydronic tubing thermal conductivity in W/mK.

#### Field: Ground Thermal Conductivity

This numeric field contains the ground thermal conductivity in W/m^2^K.

#### Field: Number of Tubing Circuits

This numeric field contains the number of hydronic tubing circuits, total in parallel in this pond..

#### Field: Length of Each Tubing Circuit

This numeric field contains length {m} of each hydronic tubing circuit.

An example of the IDF is shown below.

~~~~~~~~~~~~~~~~~~~~

      GroundHeatExchanger:Pond,
        Pond 1,                  !- Name
        Condenser Tower 1 Inlet Node,  !- Fluid Inlet Node Name
        Condenser Tower 1 Outlet Node,  !- Fluid Outlet Node Name
        2.0,                     !- Pond Depth {m}
        1000.0,                  !- Pond Area {m2}
        0.02,                    !- Hydronic Tubing Inside Diameter {m}
        0.025,                   !- Hydronic Tubing Outside Diameter {m}
        0.4,                     !- Hydronic Tubing Thermal Conductivity {W/m-K}
        1.0,                     !- Ground Thermal Conductivity {W/m2-K}
        10,                      !- Number of Tubing Circuits
        50.0;                    !- Length of Each Tubing Circuit {m}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Pond Heat Exchanger Heat Transfer Rate [W]
    HVAC,Sum,Pond Heat Exchanger Heat Transfer Energy [J]
    HVAC,Average,Pond Heat Exchanger Mass Flow Rate [kg/s]
    HVAC,Average,Pond Heat Exchanger Inlet Temperature [C]
    HVAC,Average,Pond Heat Exchanger Outlet Temperature [C]
    HVAC,Average,Pond Heat Exchanger Bulk Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Pond Heat Exchanger Heat Transfer Rate [W]

#### Pond Heat Exchanger Heat Transfer Energy [J]

These outputs are the pond ground heat exchanger heat transfer rate and total energy exchange for the timestep.

#### Pond Heat Exchanger Mass Flow Rate [kg/s]

#### Pond Heat Exchanger Inlet Temperature [C]

#### Pond Heat Exchanger Outlet Temperature [C]

**These outputs are the pond fluid inlet and outlet temperatures and mass flow rate.**

#### Pond Heat Exchanger Bulk Temperature [C]

This output is the pond bulk temperature.

## GroundHeatExchanger:Surface

The surface heat exchanger model is to simulate hydronic surface ground heat exchangers. This includes pavement surfaces with embedded pipes for snow-melting or heat rejection from hybrid ground source heat pump systems.

The heat exchanger may be ground coupled or not. In the latter case the bottom surface is exposed to the wind but not solar gains. This type of heat exchanger is intended to be connected to the supply side of a condenser loop, and can be used with any type of plant loop. The surface heat exchanger may be specified as the only heat exchanger on the condenser loop (as shown in the first figure below) or it may be connected in parallel with other condenser loop heat exchangers (such as cooling towers, ground surface heat exchangers) as shown in the second figure below.

![Example of Surface Ground Heat Exchanger as only heat exchanger on condenser loop](media/example-of-surface-ground-heat-exchanger.png)


![Surface Ground Heat Exchanger with other heat exchangers on condenser loop](media/surface-ground-heat-exchanger-with-other-heat.png)


### Inputs

#### Field: Name

This alpha field contains the identifying name for the outside panel heat exchanger.

#### Field: Construction Name

This alpha field contains the construction name. It must contain a valid "[Construction](#construction)" name that is usual for Surfaces. (Ref: Group – Surface [Construction](#construction) Elements).

#### Field: Fluid Inlet Node Name

This alpha field contains the fluid inlet node name.

#### Field: Fluid Outlet Node Name

This alpha field contains the fluid outlet node name.

#### Field: Hydronic Tubing Inside Diameter

This numeric field contains the hydronic tubing inside diameter in m.

#### Field: Number of Tubing Circuits

This numeric field contains the number of hydronic tubing circuits.

#### Field: Hydronic Tube Spacing

This numeric field contains the hydronic tube spacing in m.

#### Field: Surface Length

This numeric field contains the surface length in m.

#### Field: Surface Width

This numeric field contains the surface width in m.

#### Field: Lower Surface Environment

This alpha field expresses the lower surface exposure: Exposed or Ground.

An example of this statement in an IDF is shown below:

~~~~~~~~~~~~~~~~~~~~

      GroundHeatExchanger:Surface,
        SURFACE 1,               !- Name
        Outside Surface Construction,  !- Construction Name
        Condenser Tower 1 Inlet Node,  !- Fluid Inlet Node Name
        Condenser Tower 1 Outlet Node,  !- Fluid Outlet Node Name
        0.02,                    !- Hydronic Tubing Inside Diameter {m}
        10.0,                    !- Number of Tubing Circuits
        0.3,                     !- Hydronic Tube Spacing {m}
        100.0,                   !- Surface Length {m}
        100.0,                   !- Surface Width {m}
        GROUND;                  !- Lower Surface Environment
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Ground Heat Exchanger Heat Transfer Rate [W]
    HVAC,Average,Ground Heat Exchanger Surface Heat Transfer Rate [W]
    HVAC,Sum,Ground Heat Exchanger Heat Transfer Energy [J]
    HVAC,Average,Ground Heat Exchanger Mass Flow Rate [kg/s]
    HVAC,Average,Ground Heat Exchanger Inlet Temperature [C]
    HVAC,Average,Ground Heat Exchanger Outlet Temperature [C]
    HVAC,Average,Ground Heat Exchanger Top Surface Temperature [C]
    HVAC,Average Ground Heat Exchanger Bottom Surface Temperature [C]
    HVAC,Average,Ground Heat Exchanger Top Surface Heat Transfer Energy per Area [J/m2]
    HVAC,Average,Ground Heat Exchanger Bottom Surface Heat Transfer Energy per Area[J/m2]
    HVAC,Sum,Ground Heat Exchanger Surface Heat Transfer Energy [J]
    HVAC,Average,Ground Heat Exchanger Source Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Ground Heat Exchanger Heat Transfer Rate [W]

#### Ground Heat Exchanger Surface Heat Transfer Rate [W]

#### Ground Heat Exchanger Heat Transfer Energy [J]

These outputs are the source heat transfer rate, surface heat transfer rate and the total source energy input for the timestep.

#### Ground Heat Exchanger Mass Flow Rate [kg/s]

#### Ground Heat Exchanger Inlet Temperature [C]

#### Ground Heat Exchanger Outlet Temperature [C]

**These outputs are the surface heat exchanger fluid inlet and outlet temperatures and mass flow rate.**

#### Ground Heat Exchanger Top Surface Temperature [C]

#### Ground Heat Exchanger Bottom Surface Temperature [C]

**These outputs are the surface heat exchanger top and bottom surface temperatures.**

#### Ground Heat Exchanger Top Surface Heat Transfer Energy per Area  [J/m^2^]

#### Ground Heat Exchanger Bottom Surface Heat Transfer Energy per Area [J/m^2^]

**These outputs are the surface heat exchanger top and bottom surface flux.**

#### Ground Heat Exchanger Surface Heat Transfer Energy [J]

**This is the total surface energy exchange for the timestep.**

#### Ground Heat Exchanger Source Temperature [C]

**This is the surface heat exchanger source temperature.**

## GroundHeatExchanger:HorizontalTrench

The horizontal trench ground heat exchanger object provides an alternate interface to the detailed PipingSystem:Underground:\* objects.  The same underlying simulation algorithm is utilized, providing a transient numerical simulation of soil with buried pipes and a detailed surface heat balance.  The input syntax is much smaller and useful for simple applications.  For full flexibility, use the PipingSystem:Underground:\* objects to build a detailed simulation domain and piping circuit.  For information regarding the simulation algorithms, see the engineering reference document section covering the buried piping system objects.

**Field: Name**

This alpha field is used as an identifying field for the ground heat exchanger.

**Field: Inlet Node Name**

This alpha field is the name of the inlet node of this component on a plant loop, and must match other topology definitions such as branch objects.

**Field: Outlet Node Name**

This alpha field is the name of the outlet node of this component on a plant loop, and must match other topology definitions such as branch objects.

**Field: Design Flow Rate [m3/s]**

This numeric field is the designed flow rate for this heat exchanger; the plant loop solver will attempt to meet this request based on flow and loop conditions.

**Field: Trench Length in Pipe Axial Direction [m]**

This numeric field represents the axial length of each pipe trench.  Each pipe defined for this ground heat exchanger will have the same length.  If different pipes have different lengths, they must use separate [GroundHeatExchanger:HorizontalTrench](#groundheatexchangerhorizontaltrench) objects with different lengths.

**Field: Number of Trenches**

This integer field is the number of trenches for this heat exchanger.  Since each trench has a single pipe, this defines the number of "pipe segments" for this overall heat exchanger.  The total piping length is then calculated as the trench length times the number of trenches.

**Field: Horizontal Spacing Between Pipes [m]**

This numeric field represents the horizontal spacing (pipe centroid to pipe centroid) between pipes/trenches.

**Field: Pipe Inner Diameter [m]**

This numeric field is the inner diameter of the pipe.  The same pipe properties are used for all pipes in this heat exchanger.

**Field: Pipe Outer Diameter [m]**

This numeric field is the outer diameter of the pipe.  The same pipe properties are used for all pipes in this heat exchanger.

**Field: Burial Depth [m]**

This numeric field is the burial depth of each pipe, from ground surface to pipe cross section centroid. The same pipe depth is used for all pipes in this heat exchanger.

**Field: Soil Thermal Conductivity [W/mK]**

This numeric field is the soil thermal conductivity.

**Field: Soil Density [kg/m3]**

This numeric field is the soil density.

**Field: Soil Specific Heat [J/kgK]**

This numeric field is the nominal soil specific heat, but is corrected for moisture content and in freezing conditions.

**Field: Pipe Thermal Conductivity [W/mK]**

This numeric field is the pipe thermal conductivity.

**Field: Pipe Density [kg/m3]**

This numeric field is the pipe density.

**Field: Pipe Specific Heat [J/kgK]**

This numeric field is the pipe specific heat.

**Field: Soil Moisture Content Percent [%]**

This numeric field represents the volume fraction, in percent, of water content in the soil.

**Field: Soil Moisture Content Percent at Saturation [%]**

This numeric field represents the volume fraction, in percent, of water content in the soil which results in saturation.

**Field: Kusuda-Achenbach Average Surface Temperature [C]**

This numeric field is the average annual ground surface temperature, used in the Kusuda-Achenbach undisturbed ground temperature (far-field boundary) model.  This field can be inferred (left blank) if a Site:GroundTemperatures:Shallow object is provided in the input file.

**Field: Kusuda-Achenbach Average Amplitude of Surface Temperature [C]**

This numeric field is the average annual amplitude of ground surface temperature, used in the Kusuda-Achenbach undisturbed ground temperature (far-field boundary) model.  This field can be inferred (left blank) if a Site:GroundTemperatures:Shallow object is provided in the input file.

**Field: Kusuda-Achenbach Phase Shift of Minimum Surface Temperature [days]**

This numeric field is the phase shift to minimum ground surface temperature, in days, since the beginning of the year, used in the Kusuda-Achenbach undisturbed ground temperature (far-field boundary) model.  This field can be inferred (left blank) if a Site:GroundTemperatures:Shallow object is provided in the input file.

**Field: Evapotranspiration Ground Cover Parameter [-]**

This numeric field specifies the ground cover effects used in the evapotranspiration model at the ground surface heat balance.  The values range from 0 (solid, non-permeable ground surface) to 1.5 (wild growth).

An example of this statement in an IDF is shown below:

~~~~~~~~~~~~~~~~~~~~

    GroundHeatExchanger:HorizontalTrench,
       My Pipe Circuit, !- Name
       Plant Supply Intermediate Node, !- Inlet Node Name
       Plant Supply Outlet Node, !- Outlet Node Name
       0.004,       !- Design Flow Rate
       75,          !- Trench Length in Pipe Axial Direction
       2,           !- Number of Trenches
       2.0,         !- Horizontal Spacing Between Pipes
       0.016,       !- Pipe Inner Diameter
       0.02667,     !- Pipe Outer Diameter
       1.25,        !- Burial Depth
       1.08,        !- Soil Thermal Conductivity
       962,         !- Soil Density
       2576,        !- Soil Specific Heat
       0.3895,      !- Pipe Thermal Conductivity {W/m-K}
       641,         !- Pipe Density {kg/m3}
       2405,        !- Pipe Specific Heat {J/kg-K}
       30,          !- Soil Moisture Content Volume Fraction {percent}
       50,       !- Soil Moisture Content Volume Fraction at Saturation {percent}
       15.5,        !- Kusuda-Achenbach Average Surface Temperature {C}
       12.8,     !- Kusuda-Achenbach Average Amplitude of Surface Temperature {C}
       17.3,!- Kusuda-Achenbach Phase Shift of Minimum Surface Temperature {days}
       0.408;       !- Evapotranspiration Ground Cover Parameter
~~~~~~~~~~~~~~~~~~~~

### Inputs

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Ground Heat Exchanger Mass Flow Rate [kg/s]
    HVAC,Average,Ground Heat Exchanger Inlet Temperature [C]
    HVAC,Average,Ground Heat Exchanger Outlet Temperature [C]
    HVAC,Average,Ground Heat Exchanger Fluid Heat Transfer Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Ground Heat Exchanger Mass Flow Rate [kg/s]

The output provides the mass flow rate currently being sent through the heat exchanger.

#### Ground Heat Exchanger Inlet Temperature [C]

#### Ground Heat Exchanger Outlet Temperature [C]

Temperature of fluid at the inlet and outlet of the heat exchanger.

#### Ground Heat Exchanger Fluid Heat Transfer Rate [W]

Heat transfer rate for the heat exchanger, defined as positive for **fluid heat loss**.

## HeatExchanger:FluidToFluid

A fluid-to-fluid heat exchanger designed to couple the supply side of one plant or condenser loop to the demand side of another plant or condenser loop. This heat exchanger is fairly general and can be configured for use in any application where any two loops need to be connected together. The only constraints are that that one side must be connected to the supply side of one loop and the other side connected to the demand side of a different loop.  Because the heat exchanger is intended to be generic, its two sides are distinguished by the nature of loop side being connected.  One side is called "Loop Supply Side" to indicate the heat exchanger is situated on the supply side of a loop. The other side is called "Loop Demand Side" to indicate it is on the demand side of a loop.  The heat exchanger is intended to act as a supply component for the loop connected to it as the "Loop Supply Side" and as a demand component for the loop connected to it as the "Loop Demand Side."  From the point of view of the heat exchanger model itself, the Loop Demand Side fluid serves as the source/sink to supply heating/cooling to the fluid in the Loop Supply Side.  Only hydronic "plant" fluids are allowed, no air-side connections are possible with this heat exchanger.

![Plant Fluid-to-Fluid Heat Exchanger](media/plant-fluid-to-fluid-heat-exchanger.png)


Various options are available for the heat exchanger model. The heat transfer between the two sides can be modeled using one of seven different models for different types of heat exchangers. Heat transfer is calculated and reported using a sign convention from chilled water applications where positive heat transfer indicates energy was extracted from the Loop Supply Side and added to the Loop Demand Side.

There are eleven options for different ways to control the heat exchanger.  One general type of control is "OnOff" where the flow through the heat exchanger is either fully on or fully off.  Another type of control is "Modulated" where the flow through the Loop Demand Side is controlled to try and meet a target setpoint or load on the Loop Supply Side.

This heat exchanger can be used for a wide variety of applications including chilled water, hot water, condenser, ground source, primary-secondary systems, etc.  As of [Version](#version) 8.0, this object replaces three separate objects that were available prior to version 8.0 of EnergyPlus.  The former HeatExchanger:Hydronic object corresponds to a situation where the Loop Demand Side is the demand side of condenser loop, the Loop Supply Side is a the supply side of a chilled water loop, the control type is "CoolingSetpointOnOffWithComponentOverride," and the remote override component is a chiller. The former HeatExchanger:WatersideEconomizer object corresponds to a situation where the Loop Demand Side is the demand side of condenser loop, the Loop Supply Side is a the supply side of a chilled water loop, and the control type is "CoolingDifferentialOnOff."  The former HeatExchanger:Plate object corresponds to a situation where the Loop Demand Side is the demand side of a condenser loop, the Loop Supply Side is the supply side of a second condenser loop, and the control type is "UncontrolledOn."

### Inputs

#### Field: Name

This alpha field provides the unique identifying name for this heat exchanger.

#### Field: Availability Schedule Name

This field specifies the name of an availability schedule that can be used for supervisory control of the device.  If blank, the default is that heat exchanger is always available.  If a scheduled is named here, then the heat exchanger is available for use whenever the schedule value is greater than zero.  The heat exchanger is not available whenever the schedule value is zero or less.

#### Field: Loop Demand Side Inlet Node Name

This field specifies the name of a plant system node that connects an inlet of the heat exchanger to the demand side of a loop.  This node must be on a branch located on the demand side of a plant or condenser loop.

#### Field: Loop Demand Side Outlet Node Name

This field specifies the name of a plant system node that connects an outlet of the heat exchanger to the demand side of a loop.  This node must be on a branch located on the demand side of a plant or condenser loop.

#### Field: Loop Demand Side Design Flow Rate

This field specifies the design flow rate, in m^3^/s, of the hydronic fluid passing through the heat exchanger on the Loop Demand Side. This field is autosizable. When autosized, this design flow rate is set to equal the design flow rate for the Loop Supply Side.

#### Field: Loop Supply Side Inlet Node Name

This field specifies the name of a plant system node that connects an inlet of the heat exchanger to the supply side of a loop.  This node must be on a branch located on the supply side of a plant or condenser loop.

#### Field: Loop Supply Side Outlet Node Name

This field specifies the name of a plant system node that connects an outlet of the heat exchanger to the supply side of a loop.  This node must be on a branch located on the supply side of a plant or condenser loop.

#### Field: Loop Supply Side Design Flow Rate

This field specifies the design flow rate, in m^3^/s, of the fluid passing through the heat exchanger on the Loop Supply Side. This field is autosizable. When autosized, this design flow rate is set equal to the overall design flow rate of the loop that is connected.  If a sizing factor is entered in the field below, then it is multiplied to modify the design flow rate.

#### Field: Heat Exchange Model Type

This alpha field identifies the nature of heat exchanger.  Heat exchanger model type is specified by one of the following four key word choices:

- **CrossFlowBothUnMixed**.  Specifies a single-pass, cross-flow heat exchanger.  The effectiveness will be calculated using a cross-flow heat exchanger correlation for both streams unmixed. 
- **CrossFlowBothMixed**.  Specifies a single-pass, cross-flow heat exchanger. The effectiveness will be calculated using a cross-flow heat exchanger correlation for both streams mixed. 
- **CrossFlowSupplyMixedDemandUnMixed**.  Specifes a single-pass, cross-flow heat exchanger.  The effectiveness will be calculated using a cross-flow heat exchanger correlation for flow mixed on the Loop Supply side and flow unmixed on the Loop Demand Side. 
- **CrossFlowSupplyUnMixedDemandMixed**.   Specifes a single-pass, cross-flow heat exchanger.  The effectiveness will be calculated using a cross-flow heat exchanger correlation for flow unmixed on the Loop Supply side and flow mixed on the Loop Demand Side. 
- **CounterFlow**. Specifies a counter-flow shell and tube heat exchanger.  The effectiveness will be calculated using a counter-flow shell and tube heat exchanger correlation.
- **ParallelFlow**. Specifies a parallel-flow shell and tube heat exchanger.  The effectiveness will be calculated using a parallel-flow shell and tube heat exchanger correlation.
- **Ideal**. Specifies an ideal heat exchanger.  The effectiveness will be set to '1.0' and the specified UA will be ignored.  The heat transfer rate will be calculated as the maximum possible heat transfer rate.

#### Field: Heat Exchanger U-Factor Times Area Value

This numerical field is used to specify the overall U-Factor Times Area (UA) {W/K} for use in the calculation of the heat exchanger effectiveness using the appropriate -NTU correlation.  If **Ideal** is specified as the heat exchanger type, the effectiveness will be set to 1.0. When set to autosize Heat Exchanger U-Factor Times Area Value is calculated based on an effectiveness of 1.0 where capacity is such that the temperatures in the [Sizing:Plant](#sizingplant) objects for the two loops can be maintained.

#### Field: Control Type

This field is used to specify how the heat exchanger is to be controlled during operation.  Different applications for connecting two loops will require different control behavior and different control options are needed depending on the desired behavior.  There are the following eleven key choice options to choose from:

- **UncontrolledOn**. This control mode is applicable to situations where the heat exchanger is passively running all the time and always transfers as much heat as possible between the fluid streams.  However there is one aspect of control in that it will only request flow on the Loop Demand Side when there is non-zero flow into the heat exchanger on the Loop Supply Side.  This control mode corresponds to that available in the HeatExchanger:Plate object prior to version 8.0. 
- **OperationSchemeModulated**.  This control mode is applicable to situations where the heat exchanger is controlled by an operation scheme (see objects called PlantEquipmentOperationScheme or [CondenserEquipmentOperationSchemes](#condenserequipmentoperationschemes)).  When using this control mode the heat exchanger must be listed in [PlantEquipmentList](#plantequipmentlist) or a [CondenserEquipmentList](#condenserequipmentlist) and it serves as a supply component.  The operation scheme will dispatch a load request to the heat exchanger which it will try meet by conditioning the fluid stream connected as the Loop Supply Side.  If the heat exchanger could exceed the load request, then the flow through the fluid stream connected as the Loop Demand Side will be modulated to just meet the load request.  
- **OperationSchemeOnOff**. This control mode is applicable to situations where the heat exchanger is controlled by an operation scheme (see objects called PlantEquipmentOperationScheme or [CondenserEquipmentOperationSchemes](#condenserequipmentoperationschemes)).  When using this control mode the heat exchanger must be listed in [PlantEquipmentList](#plantequipmentlist) or a [CondenserEquipmentList](#condenserequipmentlist) and it serves as a supply component.  The operation scheme will dispatch a load request to the heat exchanger which it will use as an on/off signal to decide if the heat exchange should run or not.  If it runs, it will run at full capacity and may exceed the load request.  
- **HeatingSetpointModulated**.  This control mode is applicable to situations where the Loop Demand Side can provide useful heating to the Loop Supply Side.  A heating setpoint is obtained from a node named in the following field.  If the setpoint and inlet temperatures are such that heat exchanger could transfer heat from the Loop Demand Side to the Loop Supply Side to meet the heating setpoint, then the heat exchanger will run.  The inlet temperatures must differ by more than the value set in the field called Minimum Temperature Difference to Activate Heat Exchanger for the heat exchanger to operate.  If the heat exchanger could overshoot the setpoint, then the flow through the fluid stream connected as the Loop Demand Side will be modulated to just meet the setpoint.
- **HeatingSetpointOnOff**. This control mode is applicable to situations where the Loop Demand Side can provide useful heating to the Loop Supply Side.  A heating setpoint is obtained from a node named in the following field.  If the setpoints and inlet temperatures are such that heat exchanger could transfer heat from the Loop Demand Side to the Loop Supply Side to meet the heating setpoint, then the heat exchanger will run. The inlet temperatures must differ by more than the value set in the field called Minimum Temperature Difference to Activate Heat Exchanger for the heat exchanger to operate.  If it runs, it will run at full capacity and may overshoot the setpoint.  
- **CoolingSetpointModulated**.  This control mode is applicable to situations where the Loop Demand Side can provide useful cooling to the Loop Supply Side.  A cooling setpoint is obtained from a node named in the following field.  If the setpoints and inlet temperatures are such that heat exchanger could transfer heat from the Loop Supply Side to the Loop Demand Side to meet the cooling setpoint, then the heat exchanger will run. The inlet temperatures must differ by more than the value set in the field called Minimum Temperature Difference to Activate Heat Exchanger for the heat exchanger to operate. If the heat exchanger could undershoot the setpoint, then the flow through the fluid stream connected as the Loop Demand Side will be modulated to just meet the setpoint.
- **CoolingSetpointOnOff**. This control mode is applicable to situations where the Loop Demand Side can provide useful cooling to the Loop Supply Side.  A cooling setpoint is obtained from a node named in the following field.  If the setpoints and inlet temperatures are such that heat exchanger could transfer heat from the Loop Supply Side to the Loop Demand Side to meet the cooling setpoint, then the heat exchanger will run. The inlet temperatures must differ by more than the value set in the field called Minimum Temperature Difference to Activate Heat Exchanger for the heat exchanger to operate. If it runs, it will run at full capacity and may undershoot the setpoint.  This control mode corresponds to that available in the HeatExchanger:WatersideEconomizer object prior to version 8.0.
- **DualDeadbandSetpointModulated**. This control mode is applicable to situations where the Loop Demand Side can provide either useful cooling or heating to the Loop Supply Side.  A dual deadband setpoint is obtained from a node named in the following field.  If the setpoints and inlet temperatures are such that heat exchanger could transfer heat from the Loop Demand Side to the Loop Supply Side to meet the lower setpoint, then the heat exchanger will run. If the setpoints and inlet temperatures are such that heat exchanger could transfer heat from the Loop Supply Side to the Loop Demand Side to meet the high setpoint, then the heat exchanger will run. The inlet temperatures must differ by more than the value set in the field called Minimum Temperature Difference to Activate Heat Exchanger for the heat exchanger to operate.  If the heat exchanger could overshoot the lower setpoint, or undershoot the higher setpoint, then the flow through the fluid stream connected as the Loop Demand Side will be modulated to just meet the deadband setpoint.
- **DualDeadbandSetpointOnOff**. This control mode is applicable to situations where the Loop Demand Side can provide either useful cooling or heating to the Loop Supply Side.  A dual deadband setpoint is obtained from a node named in the following field.  If the setpoints and inlet temperatures are such that heat exchanger could transfer heat from the Loop Demand Side to the Loop Supply Side to meet the lower setpoint, then the heat exchanger will run. If the setpoints and inlet temperatures are such that heat exchanger could transfer heat from the Loop Supply Side to the Loop Demand Side to meet the high setpoint, then the heat exchanger will run. The inlet temperatures must differ by more than the value set in the field called Minimum Temperature Difference to Activate Heat Exchanger for the heat exchanger to operate. If the heat exchanger runs, it will run at full capacity and may overshoot the lower setpoint or undershoot the higher setpoint.
- **CoolingDifferentialOnOff**.  This control mode is applicable to situations where the Loop Demand Side can provide useful cooling to the Loop Supply Side.  This mode is similar to CoolingSetpointOnOff except that it ignores any cooling setpoint and its control is based only on the temperature difference between Loop Demand Side and the Loop Supply Side.  The inlet temperatures must differ by more than the value set in the field called Minimum Temperature Difference to Activate Heat Exchanger for the heat exchanger to operate.   This control mode corresponds to that available in the HeatExchanger:WatersideEconomizer object prior to version 8.0.
- **CoolingSetpointOnOffWithComponentOverride**.  This control mode is applicable to situations where the heat exchanger operation is integrated with the operation of a specific chiller.  When conditions are favorable for the heat exchanger to provide cooling to the Loop Supply Side, the heat exchanger is run and the integrated chiller is turned off.  A cooling setpoint is obtained from a node named in the following field.   If it runs it will run at full capacity and may undershoot the setpoint.  The chiller that is integrated with the heat exchanger is identified by entering the names of the chiller's inlet nodes in the input fields below.  The control decision can be based on one of three different temperature signals selected in the field below called Component Override Cooling Control Temperature Mode.  The setpoint and control signal temperatures must differ by more than the value set in the field called Minimum Temperature Difference to Activate Heat Exchanger for the heat exchanger to operate. This control mode corresponds to that available in the HeatExchanger:Hydronic object prior to version 8.0. 

#### Field: Heat Exchanger Setpoint Node Name

This field specifies the name of a plant system node located on loop attached to the Loop Supply Side.  This field is used and required when the previous field is set to one of the "Setpoint" control types.  The node must have a temperature setpoint placed on it by a setpoint manager (or EMS actuator).

If the previous field is set to DualDeadbandSetpointModulated or DeadbandSetpointOnOff then there must be a setpoint manager that places both a high and low setpoint on the node named in this field. (see [SetpointManager:Scheduled:DualSetpoint](#setpointmanagerscheduleddualsetpoint)).

#### Field: Minimum Temperature Difference to Activate Heat Exchanger

This field specifies the value of a temperature tolerance used in control decisions, in deg. Celsius.  Whenever the control logic needs to compare two temperatures, the value entered in this field is used as a threshold for comparisons.

#### Field: Heat Transfer Metering End Use Type

This field specifies how the metering for heat transfer will be accounted with respect to end uses.  Although the heat exchanger consumes no energy that needs to be metered, there are also meters for heat transfers that apply to the model. The nature of the end use may vary depending on the application that the heat exchanger is being used for.  The available choices are FreeCooling, HeatRecovery, HeatRejection, HeatRecoveryForCooling, HeatRecoveryForHeating, and LoopToLoop.

#### Field: Component Override Loop Supply Side Inlet Node Name

This field specifies the name of an inlet node for the remote component that will be integrated with this heat exchanger.  This inlet should be on the supply side of a loop -- typically chilled water inlet or return for a chiller.  This field and the next two are only used for the control type called CoolingSetpointOnOffWithComponentOverride.

#### Field: Component Override Loop Demand Side Inlet Node Name

This field specifies the name of an inlet node for the remote component that will be integrated with this heat exchanger.  This inlet should be on the demand side of a loop -- typically condenser water inlet or return for a water-cooled chiller.  This field is only used for the control type called CoolingSetpointOnOffWithComponentOverride.

#### Field: Component Override Cooling Control Temperature Mode

This field specifies which type of temperature is used to control a heat exchanger that uses the control type called CoolingSetpointOnOffWithComponentOverride.  There are three options: Loop, WetBulbTemperature, and DryBulbTemperature.  The option called "Loop" directs the program to use the inlet fluid temperature at the Loop Demand Side connection of  heat exchanger for the temperature used as a signal to be compared with the setpoint. The option call "WetBulbTemperature" uses the outdoor air wetbulb temperature as the signal.  The option called "DryBulbTemperature" uses the outdoor air drybulb temperature as the signal.

#### Field: Sizing Factor

This optional field can be used to modify the results of autosize calculations.  This sizing factor is used for this heat exchanger and modifies sizing results by multiplying them by the factor entered here.  This factor is applied to the Loop Supply Side Design Flow rate and in turn affects the heat exchanger UA and the loop demand side flow rate which are derived from that flow rate.  This allows fine control over the size of the heat exchanger when using autosize in those fields.

#### Field: Operation Minimum Temperature Limit

This optional field can be used to provide supervisory control of the heat exchanger. If either of the inlet temperatures are below this limit (°C), the heat exchanger will not operate.

#### Field: Operation Maximum Temperature Limit

This optional field can be used to provide supervisory control of the heat exchanger. If either of the inlet temperatures are above this limit  (°C), the heat exchanger will not operate.

Some example IDF input objects follow.

~~~~~~~~~~~~~~~~~~~~

      HeatExchanger:FluidToFluid,
        CondenserLoop HX,!- Name
        ALWAYS_ON,     !- Availability Schedule Name
        CondenserLoop HX HX Inlet Node, !- Loop Demand Side Inlet Node Name
        CondenserLoop HX HX Outlet Node, !- Loop Demand Side Outlet Node Name
        autosize,                     !- Loop Demand Side Design Flow Rate
        CondenserLoop Pump- HXNode,   !- Loop Supply Side Inlet Node Name
        CondenserLoop HX- ChillerNode,!- Loop Supply Side Outlet Node Name
        autosize,                !- Loop Supply Side Design Flow Rate
        CounterFlow,             !- Heat Exchange Model Type
        autosize,                !- Heat Exchanger U-Factor Times Area Value
        CoolingDifferentialOnOff,    !- Control Type
        ,   !- Heat Exchanger Setpoint Node Name
        2.0,         !- Minimum Temperature Difference to Activate Heat Exchanger
        FreeCooling;             !- Heat Transfer Metering End Use Type

    HeatExchanger:FluidToFluid,
        SOURCE to TRANSFER HX , !- Name
        AlwaysOnSchedule, !- Availability Schedule Name
        SOURCE Demand HX Inlet Node , !- Loop Demand Side Inlet Node Name
        SOURCE Demand HX Outlet Node , !- Loop Demand Side Outlet Node Name
        0.003 , !- Loop Demand Side Design Flow Rate
        TRANSFER HX Supply Inlet Node, !- Loop Supply Side Inlet Node Name
        TRANSFER HX Supply Outlet Node, !- Loop Supply Side Outlet Node Name
        0.003 , !- Loop Supply Side Design Flow Rate
        CrossFlow, !- Heat Exchange Model Type
        15000 , !- Heat Exchanger U-Factor Times Area Value
        DualDeadbandSetpointModulated , !- Control Type
        TRANSFER Supply Outlet Node, !- Heat Exchanger Setpoint Node Name
        0.2 , !- Minimum Temperature Difference to Activate Heat Exchanger
        LoopToLoop; !- Heat Transfer Metering End Use Type
~~~~~~~~~~~~~~~~~~~~

### Outputs

#### Fluid Heat Exchanger Heat Transfer Rate [W] 

#### Fluid Heat Exchanger Heat Transfer Energy [J] 

These outputs are the rate and energy transferred from the Loop Supply Side to the Loop Demand Side.  The sign convention is taken from cooling or heat rejection applications such that positive values indicate cooling of the Loop Supply Side.

#### Fluid Heat Exchanger Loop Supply Side Mass Flow Rate [kg/s]

This is the system mass flow of fluid through the heat exchanger side connected as the Loop Supply Side, in kg/s.

#### Fluid Heat Exchanger Loop Supply Side Inlet Temperature [C]

This is the temperature, in degrees Celsius, of the fluid entering the heat exchanger on the side connected as the Loop Supply Side.

#### Fluid Heat Exchanger Loop Supply Side Outlet Temperature [C]

This is the temperature, in degrees Celsius, of the fluid leaving the heat exchanger on the side connected as the Loop Supply Side.

#### Fluid Heat Exchanger Loop Demand Side Mass Flow Rate [kg/s]

This is the system mass flow of fluid through the heat exchanger side connected as the Loop Demand Side, in kg/s.

#### Fluid Heat Exchanger Loop Demand Side Inlet Temperature [C]

This is the temperature, in degrees Celsius, of the fluid entering the heat exchanger on the side connected as the Loop Demand Side.

#### Fluid Heat Exchanger Loop Demand Side Outlet Temperature [C]

This is the temperature, in degrees Celsius, of the fluid leaving the heat exchanger on the side connected as the Loop Demand Side.

#### Fluid Heat Exchanger Operation Status [0=off, 1= on]

This output is a numeric flag that indicates whether or not the heat exchanger was operating or not.  If the value is 0, then the heat exchanger was not operating.  If the value is 1, then the heat was operating.

#### Fluid Heat Exchanger Effectiveness [ ]

This output is the calculated heat exchanger effectiveness (non-dimensional).  It is an intermediate value in the NTU model calculations for heat flow rate.  Values range between 0 and 1.0.  A value of 1.0 indicates ideal heat transfer.