# Group – Design Objects

## Input for Design Calculations and Component Autosizing

### Overview

In order for EnergyPlus to successfully calculate zone design heating and cooling loads and air flow rates and for the program to use these results to automatically size the HVAC components a number of input objects must be present and certain object input fields must be entered.

- *The input file should contain a* SimulationControl object. The 1^st^ field *Do [Zone](#zone) Sizing Calculation* should be entered as *Yes*. This will cause a zone sizing simulation to be done using all the sizing periods in the input file as weather. If there are no air or water loops in the HVAC input fields 2 and 3 can be set to *No.* If there are one or more air loops (i.e., there is at least one [AirLoopHVAC](#airloophvac) object in the input file) then the 2^nd^ field *Do System Sizing Calculation* should be entered as *Yes*. If there are one or more water loops (Plant Loop objects) then the 3^rd^ field *Do Plant Sizing Calculation* should be set to *Yes*. Finally either the 4^th^ field (*Run Simulation for Sizing Periods*) or the 5^th^ field (*Run Simulation for Weather File Run Periods*) should be set to *Yes* in order to autosize the components and do a real simulation using the autosized components. The component autosizing calculations are done on the first pass through the HVAC system in the real simulation.
- *There must be at least 2 (up to any number)* SizingPeriod objects present. Normally one will be for summer conditions and one for winter. The summer day should normally have the field *Day Type* set to *SummerDesignDay*. The winter design day should normally have *Day Type* set to *WinterDesignDay*.
- *To apply a global sizing factor include the* Sizing:Parameters object.
- *For each controlled zone in the input file there should be a corresponding* Sizing:[Zone](#zone) object. Similarly for each [AirLoopHVAC](#airloophvac) there should be a [Sizing:System](#sizingsystem) object. And for each Plant or Condenser Loop there should be a [Sizing:Plant](#sizingplant) object. Note however that if a controlled zone has no corresponding [Zone](#zone) Sizing object the data from the first [Zone](#zone) Sizing object will be used. Thus if all the zone sizing information is the same only one [Zone](#zone) Sizing object need be entered.
- Only controlled zones are included in the zone and system sizing calculations. Thus for a design air flow rate to be calculated for a zone, it must contain a thermostat *even though it might not need or have a thermostat in the full simulation*. An illustration would be a three zone building with a packaged single zone system and a thermostat in one of the zones. In order for the two slave zones to be included in the design air flow calculations they must be treated as if they have a thermostat: there must be a [ZoneControl:Thermostat](#zonecontrolthermostat) for each of the slave zones.
- Some attention should be paid to schedules. In a weekly schedule object the 9^th^ and 10^th^ day schedules are for summer and winter design days respectively. This means that if a SizingPeriod object has field *Day Type* set to *SummerDesignDay*  the day schedule for summer sizing periods will be in effect. Similarly if a SizingPeriod object has field *Day Type* set to *WinterDesignDay*  the day schedule for winter sizing periods will be in effect. Some possible applications of this capability are:

        #. setting internal loads (lights, equipment, occupancy) to maximum all day for cooling and to zero all day for heating;
        #. setting heating and cooling thermostat set points to constant values (no set up or set back);
        #. setting heating and cooling equipment to be always on.

None of these applications are necessarily recommended but these and other uses of the special summer/winter design day schedules may prove useful for specific situations.

- Other than zone thermostat setpoints, the sizing calculations generally know nothing about the system control inputs such as setpoints and availability schedules. The user must coordinate sizing inputs with the actual simulation control inputs.
- The sizing calculations only recognize the presence of central heating and cooling coils, preheat and precool coils and reheat coils. These are assumed to deliver the various supply temperatures specified in the [Sizing:System](#sizingsystem) and [Sizing:Zone](#sizingzone) objects. The impact of ther components such as heat recovery, dehumidifiers, fans, and pumps are not accounted for in the sizing calculations.

### Component Autosizing

For autosizing to occur at the component level the user must enter the special value *autosize* in the numeric fields for which autosizing is available. Those fields can be found by looking at the Energy+.idd data dictionary file or under individual object details in this document. Fields that can be autosized are denoted with the comment *\\autosizable*. The components and fields that are autosizable are listed in the following table. Note that spaces may be inserted in object names to facilitate readability.

Table: Details of Autosizable Objects/Fields

Component / Object Name|Autosizable Fields
-----------------------|------------------
**AirConditioner:VariableRefrigerantFlow**
|Gross Rated Total Cooling Capacity
|Gross Rated Heating Capacity
|Resistive Defrost Heater Capacity
|Water Condenser Volume Flow Rate
|Evaporative Condenser Air Flow Rate
|Evaporative Condenser Pump Rated Power Consumption
**AirLoopHVAC**
|Design Supply Air Flow Rate
**AirLoopHVAC:Unitary:Furnace:HeatCool**
|Maximum Supply Air Temperature
|Supply Air Flow Rate During Cooling Operation
|Supply Air Flow Rate During Heating Operation
|Supply Air Flow Rate When No Cooling or Heating is Needed
**AirLoopHVAC:Unitary:Furnace:HeatOnly**
|Maximum Supply Air Temperature
|Supply Air Flow Rate
**AirLoopHVAC:UnitaryHeatCool**
|Maximum Supply Air Temperature
|Supply Air Flow Rate During Cooling Operation
|Supply Air Flow Rate During Heating Operation
|Supply Air Flow Rate When No Cooling or Heating is Needed
**AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass**
|System Air Flow Rate During Cooling Operation
|System Air Flow Rate During Heating Operation
|System Air Flow Rate When No Cooling or Heating is Needed
|Outdoor Air Flow Rate During Cooling Operation
|Outdoor Air Flow Rate During Heating Operation
|Outdoor Air Flow Rate When No Cooling or Heating is Needed
**AirLoopHVAC:UnitaryHeatOnly**
|Maximum Supply Air Temperature
|Supply Air Flow Rate
**AirLoopHVAC:UnitaryHeatPump:AirToAir**
|Supply Air Flow Rate During Cooling Operation
|Supply Air Flow Rate During Heating Operation
|Supply Air Flow Rate When No Cooling or Heating is Needed
|Maximum Supply Air Temperature from Supplemental Heater
**AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed**
|Maximum Supply Air Temperature from Supplemental Heater
|Supply Air Flow Rate When No Cooling or Heating is Needed
|Speed 1 Supply Air Flow Rate During Heating Operation
|Speed 2 Supply Air Flow Rate During Heating Operation
|Speed 3 Supply Air Flow Rate During Heating Operation
|Speed 4 Supply Air Flow Rate During Heating Operation
|Speed 1 Supply Air Flow Rate During Cooling Operation
|Speed 2 Supply Air Flow Rate During Cooling Operation
|Speed 3 Supply Air Flow Rate During Cooling Operation
|Speed 4 Supply Air Flow Rate During Cooling Operation
**AirLoopHVAC:UnitaryHeatPump:WaterToAir**
|Supply Air Flow Rate
|Maximum Supply Air Temperature from Supplemental Heater
**AirLoopHVAC:UnitarySystem**
|Supply Air Flow Rate During Cooling Operation
|Supply Air Flow Rate During Heating Operation
|Supply Air Flow Rate When No Cooling or Heating is Required
|Maximum Supply Air Temperature
**AirTerminal:DualDuct:ConstantVolume**
|Maximum Air Flow Rate
**AirTerminal:DualDuct:VAV**
|Maximum Damper Air Flow Rate
**AirTerminal:DualDuct:VAV:OutdoorAir**
|Maximum Terminal Air Flow Rate
**AirTerminal:SingleDuct:ConstantVolume:CooledBeam**
|Supply Air Volumetric Flow Rate
|Maximum Total Chilled Water Volumetric Flow Rate
|Number of Beams
|Beam Length
**AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction**
|Maximum Total Air Flow Rate
|Maximum Hot Water Flow Rate
|Maximum Cold Water Flow Rate
**AirTerminal:SingleDuct:ConstantVolume:Reheat**
|Maximum Air Flow Rate
|Maximum Hot Water or Steam Flow Rate
**AirTerminal:SingleDuct:ParallelPIU:Reheat**
|Maximum Primary Air Flow Rate
|Maximum Secondary Air Flow Rate
|Minimum Primary Air Flow Fraction
|Fan On Flow Fraction
|Maximum Hot Water or Steam Flow Rate
**AirTerminal:SingleDuct:SeriesPIU:Reheat**
|Maximum Air Flow Rate
|Maximum Primary Air Flow Rate
|Minimum Primary Air Flow Fraction
|Maximum Hot Water or Steam Flow Rate
**AirTerminal:SingleDuct:Uncontrolled**
|Maximum Air Flow Rate
**AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat**
|Maximum Air Flow Rate
**AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat**
|Maximum Air Flow Rate
|Maximum Hot Water or Steam Flow Rate
**AirTerminal:SingleDuct:VAV:NoReheat**
|Maximum Air Flow Rate
**AirTerminal:SingleDuct:VAV:Reheat**
|Maximum Air Flow Rate
|Maximum Hot Water or Steam Flow Rate
**AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan**
|Maximum Cooling Air Flow Rate
|Maximum Heating Air Flow Rate
|Maximum Hot Water or Steam Flow Rate
**Boiler:HotWater**
|Nominal Capacity
|Design Water Flow Rate
**Boiler:Steam**
|Nominal Capacity
**Branch**
|Maximum Flow Rate
**Chiller:Absorption**
|Nominal Capacity
|Nominal Pumping Power
|Design Chilled Water Flow Rate
|Design Condenser Water Flow Rate
|Design Generator Fluid Flow Rate
**Chiller:Absorption:Indirect**
|Nominal Capacity
|Nominal Pumping Power
|Design Chilled Water Flow Rate
|Design Condenser Water Flow Rate
|Design Generator Fluid Flow Rate
**Chiller:CombustionTurbine**
|Nominal Capacity
|Design Chilled Water Flow Rate
|Design Condenser Water Flow Rate
|Gas Turbine Engine Capacity
**Chiller:ConstantCOP**
|Nominal Capacity
|Design Chilled Water Flow Rate
|Design Condenser Water Flow Rate
**Chiller:Electric**
|Nominal Capacity
|Design Chilled Water Flow Rate
|Design Condenser Fluid Flow Rate
|Design Heat Recovery Water Flow Rate
**Chiller:Electric:EIR**
|Reference Capacity
|Reference Chilled Water Flow Rate
|Reference Condenser Fluid Flow Rate
|Design Heat Recovery Water Flow Rate
**Chiller:Electric:ReformulatedEIR**
|Reference Capacity
|Reference Chilled Water Flow Rate
|Reference Condenser Water Flow Rate
|Design Heat Recovery Water Flow Rate
**Chiller:EngineDriven**
|Nominal Capacity
|Design Chilled Water Flow Rate
|Design Condenser Water Flow Rate
**ChillerHeater:Absorption:DirectFired**
|Nominal Cooling Capacity
|Design Chilled Water Flow Rate
|Design Condenser Water Flow Rate
|Design Hot Water Flow Rate
**ChillerHeater:Absorption:DoubleEffect**
|Nominal Cooling Capacity
|Design Chilled Water Flow Rate
|Design Condenser Water Flow Rate
|Design Hot Water Flow Rate
**ChillerHeaterPerformance:Electric:EIR**
|Reference Cooling Mode Evaporator Capacity
|Design Chilled Water Flow Rate
|Design Condenser Water Flow Rate
**Coil:Cooling:DX:MultiSpeed**
|Speed 1 Gross Rated Total Cooling Capacity
|Speed 1 Gross Rated Sensible Heat Ratio
|Speed 1 Rated Air Flow Rate
|Speed 1 Evaporative Condenser Air Flow Rate
|Speed 1 Rated Evaporative Condenser Pump Power Consumption
|Speed 2 Gross Rated Total Cooling Capacity
|Speed 2 Gross Rated Sensible Heat Ratio
|Speed 2 Rated Air Flow Rate
|Speed 2 Evaporative Condenser Air Flow Rate
|Speed 2 Rated Evaporative Condenser Pump Power Consumption
|Speed 3 Gross Rated Total Cooling Capacity
|Speed 3 Gross Rated Sensible Heat Ratio
|Speed 3 Rated Air Flow Rate
|Speed 3 Evaporative Condenser Air Flow Rate
|Speed 3 Rated Evaporative Condenser Pump Power Consumption
|Speed 4 Gross Rated Total Cooling Capacity
|Speed 4 Gross Rated Sensible Heat Ratio
|Speed 4 Rated Air Flow Rate
|Speed 4 Evaporative Condenser Air Flow Rate
|Speed 4 Rated Evaporative Condenser Pump Power Consumption
**Coil:Cooling:DX:SingleSpeed**
|Gross Rated Total Cooling Capacity
|Gross Rated Sensible Heat Ratio
|Rated Air Flow Rate
|Evaporative Condenser Air Flow Rate
|Evaporative Condenser Pump Rated Power Consumption
**Coil:Cooling:DX:SingleSpeed:ThermalStorage**
|Rated Evaporator Air Flow Rate
|Cooling Only Mode Rated Total Evaporator Cooling Capacity
|Evaporative Condenser Pump Rated Power Consumption
**Coil:Cooling:DX:TwoSpeed**
|High Speed Gross Rated Total Cooling Capacity
|High Speed Rated Sensible Heat Ratio
|High Speed Rated Air Flow Rate
|Low Speed Gross Rated Total Cooling Capacity
|Low Speed Gross Rated Sensible Heat Ratio
|Low Speed Rated Air Flow Rate
|High Speed Evaporative Condenser Air Flow Rate
|High Speed Evaporative Condenser Pump Rated Power Consumption
|Low Speed Evaporative Condenser Air Flow Rate
|Low Speed Evaporative Condenser Pump Rated Power Consumption
**Coil:Cooling:DX:VariableRefrigerantFlow**
|Gross Rated Total Cooling Capacity
|Gross Rated Sensible Heat Ratio
|Rated Air Flow Rate
**Coil:Cooling:DX:VariableSpeed**
|Gross Rated Total Cooling Capacity At Selected Nominal Speed Level
|Rated Air Flow Rate At Selected Nominal Speed Level
|Evaporative Condenser Pump Rated Power Consumption
**Coil:Cooling:Water**
|Design Water Flow Rate
|Design Air Flow Rate
|Design Inlet Water Temperature
|Design Inlet Air Temperature
|Design Outlet Air Temperature
|Design Inlet Air Humidity Ratio
|Design Outlet Air Humidity Ratio
**Coil:Cooling:Water:DetailedGeometry**
|Maximum Water Flow Rate
|Tube Outside Surface Area
|Total Tube Inside Area
|Fin Surface Area
|Minimum Airflow Area
|Coil Depth
|Fin Diameter
|Number of Tubes per Row
**Coil:Cooling:WaterToAirHeatPump:EquationFit**
|Rated Air Flow Rate
|Rated Water Flow Rate
|Gross Rated Total Cooling Capacity
|Gross Rated Sensible Cooling Capacity
**Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit**
|Gross Rated Total Cooling Capacity At Selected Nominal Speed Level
|Rated Air Flow Rate At Selected Nominal Speed Level
|Rated Water Flow Rate At Selected Nominal Speed Level
**Coil:Heating:DX:MultiSpeed**
|Resistive Defrost Heater Capacity
|Speed 1 Gross Rated Heating Capacity
|Speed 1 Rated Air Flow Rate
|Speed 2 Gross Rated Heating Capacity
|Speed 2 Rated Air Flow Rate
|Speed 3 Gross Rated Heating Capacity
|Speed 3 Rated Air Flow Rate
|Speed 4 Gross Rated Heating Capacity
|Speed 4 Rated Air Flow Rate
**Coil:Heating:DX:SingleSpeed**
|Gross Rated Heating Capacity
|Rated Air Flow Rate
|Resistive Defrost Heater Capacity
**Coil:Heating:DX:VariableRefrigerantFlow**
|Gross Rated Heating Capacity
|Rated Air Flow Rate
**Coil:Heating:DX:VariableSpeed**
|Rated Heating Capacity At Selected Nominal Speed Level
|Rated Air Flow Rate At Selected Nominal Speed Level
|Resistive Defrost Heater Capacity
**Coil:Heating:Electric**
|Nominal Capacity
**Coil:Heating:Electric:MultiStage**
|Stage 1 Nominal Capacity
|Stage 2 Nominal Capacity
|Stage 3 Nominal Capacity
|Stage 4 Nominal Capacity
**Coil:Heating:Gas**
|Nominal Capacity
**Coil:Heating:Gas:MultiStage**
|Stage 1 Nominal Capacity
|Stage 2 Nominal Capacity
|Stage 3 Nominal Capacity
|Stage 4 Nominal Capacity
**Coil:Heating:Steam**
|Maximum Steam Flow Rate
**Coil:Heating:Water**
|U-Factor Times Area Value
|Maximum Water Flow Rate
|Rated Capacity
**Coil:Heating:WaterToAirHeatPump:EquationFit**
|Rated Air Flow Rate
|Rated Water Flow Rate
|Gross Rated Heating Capacity
**Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit**
|Rated Heating Capacity At Selected Nominal Speed Level
|Rated Air Flow Rate At Selected Nominal Speed Level
|Rated Water Flow Rate At Selected Nominal Speed Level
**CoilPerformance:DX:Cooling**
|Gross Rated Total Cooling Capacity
|Gross Rated Sensible Heat Ratio
|Rated Air Flow Rate
|Evaporative Condenser Air Flow Rate
|Evaporative Condenser Pump Rated Power Consumption
**CondenserLoop**
|Maximum Loop Flow Rate
**Controller:OutdoorAir**
|Minimum Outdoor Air Flow Rate
|Maximum Outdoor Air Flow Rate
**Controller:WaterCoil**
|Controller Convergence Tolerance
|Maximum Actuated Flow
**CoolingTower:SingleSpeed**
|Design Water Flow Rate
|Design Air Flow Rate
|Design Fan Power
|Design U-Factor Times Area Value
**CoolingTower:TwoSpeed**
|Design Water Flow Rate
|High Fan Speed Air Flow Rate
|High Fan Speed Fan Power
|High Fan Speed U-Factor Times Area Value
**CoolingTower:VariableSpeed**
|Design Water Flow Rate
|Design Air Flow Rate
|Design Fan Power
**CoolingTower:VariableSpeed:Merkel**
|Nominal Capacity
|Design Water Flow Rate
|Design Air Flow Rate U-Factor Times Area Value
**EvaporativeCooler:Indirect:ResearchSpecial**
|Secondary Fan Flow Rate
**EvaporativeFluidCooler:SingleSpeed**
|Design Air Flow Rate
|Design Air Flow Rate Fan Power
|Design Air Flow Rate U-factor Times Area Value
|Design Water Flow Rate
**EvaporativeFluidCooler:TwoSpeed**
|High Fan Speed Air Flow Rate
|High Fan Speed Fan Power
|High Fan Speed U-factor Times Area Value
|Design Water Flow Rate
**Fan:ComponentModel**
|Maximum Flow Rate
|Minimum Flow Rate
|Motor Fan Pulley Ratio
|Belt Maximum Torque
|Maximum Motor Output Power
|Maximum VFD Output Power
**Fan:ConstantVolume**
|Maximum Flow Rate
**Fan:OnOff**
|Maximum Flow Rate
**FanPerformance:NightVentilation**
|Maximum Flow Rate
**Fan:VariableVolume**
|Maximum Flow Rate
**FluidCooler:SingleSpeed**
|Design Air Flow Rate U-factor Times Area Value
|Design Water Flow Rate
|Design Air Flow Rate
|Design Air Flow Rate Fan Power
**FluidCooler:TwoSpeed**
|High Fan Speed U-factor Times Area Value
|Design Water Flow Rate
|High Fan Speed Air Flow Rate
|High Fan Speed Fan Power
**HeaderedPumps:ConstantSpeed**
|Total Rated Flow Rate
|Rated Power Consumption
**HeaderedPumps:VariableSpeed**
|Total Rated Flow Rate
|Rated Power Consumption
**HeatExchanger:AirToAir:SensibleAndLatent**
|Nominal Supply Air Flow Rate
**HeatExchanger:FluidToFluid**
|Loop Demand Side Design Flow Rate
|Loop Supply Side Design Flow Rate
|Heat Exchanger U-Factor Times Area Value
**Humidifier:Steam:Electric**
|Rated Power
**HVACTemplate:Plant:Boiler**
|Capacity
**HVACTemplate:Plant:Chiller**
|Capacity
**HVACTemplate:Plant:Tower**
|High Speed Nominal Capacity
|High Speed Fan Power
|Low Speed Nominal Capacity
|Low Speed Fan Power
|Free Convection Capacity
**HVACTemplate:System:ConstantVolume**
|Supply Fan Maximum Flow Rate
|Heating Coil Capacity
|Maximum Outdoor Air Flow Rate
|Minimum Outdoor Air Flow Rate
|Humidifier Rated Electric Power
**HVACTemplate:System:DedicatedOutdoorAir**
|Supply Fan Flow Rate
|DX Cooling Coil Gross Rated Total Capacity
|DX Cooling Coil Gross Rated Sensible Heat Ratio
|Humidifier Rated Electric Power
**HVACTemplate:System:DualDuct**
|Main Supply Fan Maximum Flow Rate
|Cold [Duct](#duct) Supply Fan Maximum Flow Rate
|Hot [Duct](#duct) Supply Fan Maximum Flow Rate
|Heating Coil Capacity
|Maximum Outdoor Air Flow Rate
|Minimum Outdoor Air Flow Rate
|Humidifier Rated Electric Power
**HVACTemplate:System:PackagedVAV**
|Supply Fan Maximum Flow Rate
|Supply Fan Minimum Flow Rate
|Cooling Coil Gross Rated Total Capacity
|Cooling Coil Gross Rated Sensible Heat Ratio
|Heating Coil Capacity
|Maximum Outdoor Air Flow Rate
|Minimum Outdoor Air Flow Rate
|Humidifier Rated Electric Power
**HVACTemplate:System:Unitary**
|Supply Fan Maximum Flow Rate
|Cooling Coil Gross Rated Total Capacity
|Cooling Coil Gross Rated Sensible Heat Ratio
|Heating Coil Capacity
|Maximum Outdoor Air Flow Rate
|Minimum Outdoor Air Flow Rate
|Humidifier Rated Electric Power
**HVACTemplate:System:UnitaryHeatPump:AirToAir**
|Cooling Supply Air Flow Rate
|Heating Supply Air Flow Rate
|No Load Supply Air Flow Rate
|Cooling Coil Gross Rated Total Capacity
|Cooling Coil Gross Rated Sensible Heat Ratio
|Heat Pump Heating Coil Gross Rated Capacity
|Supplemental Heating Coil Capacity
|Maximum Outdoor Air Flow Rate
|Minimum Outdoor Air Flow Rate
|Humidifier Rated Electric Power
**HVACTemplate:System:UnitarySystem**
|Cooling Supply Air Flow Rate
|Heating Supply Air Flow Rate
|No Load Supply Air Flow Rate
|DX Cooling Coil Gross Rated Total Capacity
|DX Cooling Coil Gross Rated Sensible Heat Ratio
|Heating Coil Gross Rated Capacity
|Supplemental Heating or Reheat Coil Capacity
|Maximum Outdoor Air Flow Rate
|Minimum Outdoor Air Flow Rate
|Humidifier Rated Electric Power
**HVACTemplate:System:VAV**
|Supply Fan Maximum Flow Rate
|Supply Fan Minimum Flow Rate
|Maximum Outdoor Air Flow Rate
|Minimum Outdoor Air Flow Rate
|Humidifier Rated Electric Power
**HVACTemplate:System:VRF**
|Gross Rated Total Cooling Capacity
|Gross Rated Heating Capacity
|Resistive Defrost Heater Capacity
|Water Condenser Volume Flow Rate
|Evaporative Condenser Air Flow Rate
|Evaporative Condenser Pump Rated Power Consumption
**HVACTemplate:Zone:BaseboardHeat**
|Baseboard Heating Capacity
**HVACTemplate:Zone:ConstantVolume**
|Supply Air Maximum Flow Rate
|Baseboard Heating Capacity
**HVACTemplate:Zone:DualDuct**
|Supply Air Maximum Flow Rate
|Baseboard Heating Capacity
**HVACTemplate:Zone:FanCoil**
|Supply Air Maximum Flow Rate
|Baseboard Heating Capacity
**HVACTemplate:Zone:IdealLoadsAirSystem**
|Maximum Heating Air Flow Rate
|Maximum Sensible Heating Capacity
|Maximum Cooling Air Flow Rate
|Maximum Total Cooling Capacity
**HVACTemplate:Zone:PTAC**
|Cooling Supply Air Flow Rate
|Heating Supply Air Flow Rate
|No Load Supply Air Flow Rate
|Cooling Coil Gross Rated Total Capacity
|Cooling Coil Gross Rated Sensible Heat Ratio
|Heating Coil Capacity
|Baseboard Heating Capacity
**HVACTemplate:Zone:PTHP**
|Cooling Supply Air Flow Rate
|Heating Supply Air Flow Rate
|No Load Supply Air Flow Rate
|Cooling Coil Gross Rated Total Capacity
|Cooling Coil Gross Rated Sensible Heat Ratio
|Heat Pump Heating Coil Gross Rated Capacity
|Supplemental Heating Coil Capacity
|Baseboard Heating Capacity
**HVACTemplate:Zone:Unitary**
|Supply Air Maximum Flow Rate
|Baseboard Heating Capacity
**HVACTemplate:Zone:VAV**
|Supply Air Maximum Flow Rate
|Baseboard Heating Capacity
**HVACTemplate:Zone:VAV:FanPowered**
|Primary Supply Air Maximum Flow Rate
|Primary Supply Air Minimum Flow Fraction
|Secondary Supply Air Maximum Flow Rate
|Parallel Fan On Flow Fraction
|Baseboard Heating Capacity
**HVACTemplate:Zone:VAV:HeatAndCool**
|Supply Air Maximum Flow Rate
|Baseboard Heating Capacity
**HVACTemplate:Zone:VRF**
|Supply Air Flow Rate During Cooling Operation
|Supply Air Flow Rate When No Cooling is Needed
|Supply Air Flow Rate During Heating Operation
|Supply Air Flow Rate When No Heating is Needed
|Outdoor Air Flow Rate During Cooling Operation
|Outdoor Air Flow Rate During Heating Operation
|Outdoor Air Flow Rate When No Cooling or Heating is Needed
|Cooling Coil Gross Rated Total Capacity
|Cooling Coil Gross Rated Sensible Heat Ratio
|Heat Pump Heating Coil Gross Rated Capacity
|Baseboard Heating Capacity
**HVACTemplate:Zone:WaterToAirHeatPump**
|Cooling Supply Air Flow Rate
|Heating Supply Air Flow Rate
|No Load Supply Air Flow Rate
|Cooling Coil Gross Rated Total Capacity
|Cooling Coil Gross Rated Sensible Heat Ratio
|Heat Pump Heating Coil Gross Rated Capacity
|Supplemental Heating Coil Capacity
|Baseboard Heating Capacity
**PlantComponent:TemperatureSource**
|Design Volume Flow Rate
**PlantEquipmentOperation:ComponentSetpoint**
|Component 1 Flow Rate
|Component 2 Flow Rate
|Component 3 Flow Rate
|Component 4 Flow Rate
|Component 5 Flow Rate
|Component 6 Flow Rate
|Component 7 Flow Rate
|Component 8 Flow Rate
|Component 9 Flow Rate
|Component 10 Flow Rate
**PlantLoop**
|Maximum Loop Flow Rate
**Pump:ConstantSpeed**
|Rated Flow Rate
|Rated Power Consumption
**Pump:VariableSpeed**
|Rated Flow Rate
|Rated Power Consumption
**Pump:VariableSpeed:Condensate**
|Rated Flow Rate
|Rated Power Consumption
**Sizing:System**
|Design Outdoor Air Flow Rate
**SolarCollector:FlatPlate:PhotovoltaicThermal**
|Design Flow Rate
**ThermalStorage:ChilledWater:Mixed**
|Use Side Design Flow Rate
|Source Side Design Flow Rate
**ThermalStorage:ChilledWater:Stratified**
|Use Side Design Flow Rate
|Source Side Design Flow Rate
**UnitarySystemPerformance:HeatPump:Multispeed**
|Speed 1 Supply Air Flow Ratio During Heating Operation
|Speed 1 Supply Air Flow Ratio During Cooling Operation
|Speed 2 Supply Air Flow Ratio During Heating Operation
|Speed 2 Supply Air Flow Ratio During Cooling Operation
|Speed 3 Supply Air Flow Ratio During Heating Operation
|Speed 3 Supply Air Flow Ratio During Cooling Operation
|Speed 4 Supply Air Flow Ratio During Heating Operation
|Speed 4 Supply Air Flow Ratio During Cooling Operation
**WaterHeater:Mixed**
|Tank Volume
|Heater Maximum Capacity
|Use Side Design Flow Rate
|Source Side Design Flow Rate
**WaterHeater:Stratified**
|Tank Volume
|Tank Height
|Heater 1 Capacity
|Use Side Design Flow Rate
|Source Side Design Flow Rate
**ZoneHVAC:Baseboard:Convective:Electric**
|Nominal Capacity
**ZoneHVAC:Baseboard:Convective:Water**
|U-Factor Times Area Value
|Maximum Water Flow Rate
**ZoneHVAC:Baseboard:RadiantConvective:Electric**
|Nominal Capacity
**ZoneHVAC:Baseboard:RadiantConvective:Steam**
|Maximum Steam Flow Rate
**ZoneHVAC:Baseboard:RadiantConvective:Water**
|Rated Capacity
|Maximum Water Flow Rate
**ZoneHVAC:EnergyRecoveryVentilator**
|Supply Air Flow Rate
|Exhaust Air Flow Rate
**ZoneHVAC:EvaporativeCoolerUnit**
|Design Supply Air Flow Rate
**ZoneHVAC:FourPipeFanCoil**
|Maximum Supply Air Flow Rate
|Maximum Outdoor Air Flow Rate
|Maximum Cold Water Flow Rate
|Maximum Hot Water Flow Rate
**ZoneHVAC:HighTemperatureRadiant**
|Maximum Power Input
**ZoneHVAC:IdealLoadsAirSystem**
|Maximum Heating Air Flow Rate
|Maximum Sensible Heating Capacity
|Maximum Cooling Air Flow Rate
|Maximum Total Cooling Capacity
**ZoneHVAC:LowTemperatureRadiant:Electric**
|Maximum Electrical Power to Panel
**ZoneHVAC:LowTemperatureRadiant:VariableFlow**
|Hydronic Tubing Length
|Maximum Hot Water Flow
|Maximum Cold Water Flow
**ZoneHVAC:OutdoorAirUnit**
|Outdoor Air Flow Rate
|Exhaust Air Flow Rate
**ZoneHVAC:PackagedTerminalAirConditioner**
|Supply Air Flow Rate During Cooling Operation
|Supply Air Flow Rate During Heating Operation
|Supply Air Flow Rate When No Cooling or Heating is Needed
|Outdoor Air Flow Rate During Cooling Operation
|Outdoor Air Flow Rate During Heating Operation
|Outdoor Air Flow Rate When No Cooling or Heating is Needed
**ZoneHVAC:PackagedTerminalHeatPump**
|Supply Air Flow Rate During Cooling Operation
|Supply Air Flow Rate During Heating Operation
|Supply Air Flow Rate When No Cooling or Heating is Needed
|Outdoor Air Flow Rate During Cooling Operation
|Outdoor Air Flow Rate During Heating Operation
|Outdoor Air Flow Rate When No Cooling or Heating is Needed
|Maximum Supply Air Temperature from Supplemental Heater
**ZoneHVAC:TerminalUnit:VariableRefrigerantFlow**
|Supply Air Flow Rate During Cooling Operation
|Supply Air Flow Rate When No Cooling is Needed
|Supply Air Flow Rate During Heating Operation
|Supply Air Flow Rate When No Heating is Needed
|Outdoor Air Flow Rate During Cooling Operation
|Outdoor Air Flow Rate During Heating Operation
|Outdoor Air Flow Rate When No Cooling or Heating is Needed
**ZoneHVAC:UnitHeater**
|Maximum Supply Air Flow Rate
|Maximum Hot Water or Steam Flow Rate
**ZoneHVAC:UnitVentilator**
|Maximum Supply Air Flow Rate
|Minimum Outdoor Air Flow Rate
|Maximum Outdoor Air Flow Rate
**ZoneHVAC:VentilatedSlab**
|Maximum Air Flow Rate
|Minimum Outdoor Air Flow Rate
|Maximum Outdoor Air Flow Rate
**ZoneHVAC:WaterToAirHeatPump**
|Supply Air Flow Rate During Cooling Operation
|Supply Air Flow Rate During Heating Operation
|Supply Air Flow Rate When No Cooling or Heating is Needed
|Outdoor Air Flow Rate During Cooling Operation
|Outdoor Air Flow Rate During Heating Operation
|Outdoor Air Flow Rate When No Cooling or Heating is Needed
|Maximum Supply Air Temperature from Supplemental Heater
**ZoneHVAC:WindowAirConditioner**
|Maximum Supply Air Flow Rate
|Maximum Outdoor Air Flow Rate

There are 3 places in the input where the user can impose sizing factors.

In Sizing Parameters (object: [Sizing:Parameters](#sizingparameters)), the user can specify an over-all sizing factor. This factor is applied to all the zone design loads and air flow rates resulting from the zone sizing calculations.

In [Zone](#zone) Sizing (object: Sizing:[Zone](#zone)), the user can specify a sizing factor for a specific zone. The factor is applied to the calculated zone design loads and air flow rates for the zone named in the Sizing:[Zone](#zone) object. This sizing factor overrides the global sizing factor. That is, a zone sizing factor, if specified, replaces the global sizing factor for the named zone.

For some plant components (basically all central chillers, boilers and cooling towers) the user can specify a sizing factor that modifies the autosized component capacity and flow rates. These factors are applied after the application of global or zone sizing factors. They are primarily used to split the design load between multiple components. These sizing factors can change the autosizing of the associated loops and pumps. The following rules are followed the effect of plant component sizing factors on loops and pumps.

For supply side branches, the sizing factors of all components in series on the branch are summed and the result becomes the branch sizing factor. If there is a branch pump its autosized design flow rate is multiplied by the branch sizing factor.

For each loop, if the average of the branch sizing factors is less than 1, the loop sizing factor is set equal to the sum of the branch sizing factors. If the average is greater than 1, the loop sizing factor is set equal to the maximum of the branch sizing factors. The loop sizing factor is applied to the loop design flow rate (if autosized) and to the loop pump flow rate (if autosized).

### Mixing User-Specified and Autosized Inputs 

Mixed user-specified and autosized inputs can be successfully used if the following points and suggestions are followed.

Each component is autosized independently. Thus user input for a flow rate in one component will have no effect on other components' autosized flow rates. For instance, specifying the chilled water loop pump's rated flow rate will have no effect on the autosizing of the chiller's design evaporator flow rate or on the plant loop's autosized maximum loop flow rate.

Within a component it is best to autosize all inputs are enter specified values for all inputs. For example, in a chiller, if only the nominal capaciity is user-specified, the autosized chilled water flow rate may not be consistent with the specified capacity.

Sizing information flows only from the sizing objects to the components. The sizing calculations have no knowledge of user-specified values in a component. The only exception to this rule is that plant loop sizing will collect all component design water flow rates whether autosized or user-specified.

If the user wants to specify a zone or system air flow rate it should be done using the [Sizing:Zone](#sizingzone) and [Sizing:System](#sizingsystem)  objects rather than done in the individual components.

The plant loop flow rates are sized from the total design demand of the components connected to each loop. The components demanding water need not be autosized for the plant loop autosizing to work successfully. So the user could specify all the air side components and autosize all the plant loops and plant components. Or specify the chilled water loop flow rate, chilled water pump inputs and chiller inputs and let the condenser loop and tower autosize.

### Component Sizing Output

The results of the component autosizing calculations are reported on the *eplusout.eio* file. For each component field that has been autosized the object type, object name, field description with unit, and value are printed out as comma separated data on a line beginning with *Component Sizing*. Examples of this are shown in the Output Details and Examples document.

The complete list of objects that have autosized fields is shown in the following table. Note that spaces may be inserted in object names to facilitate readability.

Table: Complete list of Objects with autosized Fields

Object Name|Object Name
-----------|-----------
AirConditioner:VariableRefrigerantFlow|AirLoopHVAC
AirLoopHVAC:Unitary:Furnace:HeatCool|AirLoopHVAC:Unitary:Furnace:HeatOnly
AirLoopHVAC:UnitaryHeatCool|AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass
AirLoopHVAC:UnitaryHeatOnly|AirLoopHVAC:UnitaryHeatPump:AirToAir
AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed|AirLoopHVAC:UnitaryHeatPump:WaterToAir
AirLoopHVAC:UnitarySystem|AirTerminal:DualDuct:ConstantVolume
AirTerminal:DualDuct:VAV|AirTerminal:DualDuct:VAV:OutdoorAir
AirTerminal:SingleDuct:ConstantVolume:CooledBeam|AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction
AirTerminal:SingleDuct:ConstantVolume:Reheat|AirTerminal:SingleDuct:ParallelPIU:Reheat
AirTerminal:SingleDuct:SeriesPIU:Reheat|AirTerminal:SingleDuct:Uncontrolled
AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat|AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat
AirTerminal:SingleDuct:VAV:NoReheat|AirTerminal:SingleDuct:VAV:Reheat
AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan|Boiler:HotWater
Boiler:Steam|Branch
Chiller:Absorption|Chiller:Absorption:Indirect
Chiller:CombustionTurbine|Chiller:ConstantCOP
Chiller:Electric|Chiller:Electric:EIR
Chiller:Electric:ReformulatedEIR|Chiller:EngineDriven
ChillerHeater:Absorption:DirectFired|ChillerHeater:Absorption:DoubleEffect
ChillerHeaterPerformance:Electric:EIR|Coil:Cooling:DX:MultiSpeed
Coil:Cooling:DX:SingleSpeed|Coil:Cooling:DX:SingleSpeed:ThermalStorage
Coil:Cooling:DX:TwoSpeed|Coil:Cooling:DX:VariableRefrigerantFlow
Coil:Cooling:DX:VariableSpeed|Coil:Cooling:Water
Coil:Cooling:Water:DetailedGeometry|Coil:Cooling:WaterToAirHeatPump:EquationFit
Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit|Coil:Heating:DX:MultiSpeed
Coil:Heating:DX:SingleSpeed|Coil:Heating:DX:VariableRefrigerantFlow
Coil:Heating:DX:VariableSpeed|Coil:Heating:Electric
Coil:Heating:Electric:MultiStage|Coil:Heating:Gas
Coil:Heating:Gas:MultiStage|Coil:Heating:Steam
Coil:Heating:Water|Coil:Heating:WaterToAirHeatPump:EquationFit
Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit|CoilPerformance:DX:Cooling
CondenserLoop|Controller:OutdoorAir
Controller:WaterCoil|CoolingTower:SingleSpeed
CoolingTower:TwoSpeed|CoolingTower:VariableSpeed
CoolingTower:VariableSpeed:Merkel|EvaporativeCooler:Indirect:ResearchSpecial
EvaporativeFluidCooler:SingleSpeed|EvaporativeFluidCooler:TwoSpeed
Fan:ComponentModel|Fan:ConstantVolume
Fan:OnOff|FanPerformance:NightVentilation
Fan:VariableVolume|FluidCooler:SingleSpeed
FluidCooler:TwoSpeed|HeaderedPumps:ConstantSpeed
HeaderedPumps:VariableSpeed|HeatExchanger:AirToAir:SensibleAndLatent
HeatExchanger:FluidToFluid|Humidifier:Steam:Electric
HVACTemplate:Plant:Boiler|HVACTemplate:Plant:Chiller
HVACTemplate:Plant:Tower|HVACTemplate:System:ConstantVolume
HVACTemplate:System:DedicatedOutdoorAir|HVACTemplate:System:DualDuct
HVACTemplate:System:PackagedVAV|HVACTemplate:System:Unitary
HVACTemplate:System:UnitaryHeatPump:AirToAir|HVACTemplate:System:UnitarySystem
HVACTemplate:System:VAV|HVACTemplate:System:VRF
HVACTemplate:Zone:BaseboardHeat|HVACTemplate:Zone:ConstantVolume
HVACTemplate:Zone:DualDuct|HVACTemplate:Zone:FanCoil
HVACTemplate:Zone:IdealLoadsAirSystem|HVACTemplate:Zone:PTAC
HVACTemplate:Zone:PTHP|HVACTemplate:Zone:Unitary
HVACTemplate:Zone:VAV|HVACTemplate:Zone:VAV:FanPowered
HVACTemplate:Zone:VAV:HeatAndCool|HVACTemplate:Zone:VRF
HVACTemplate:Zone:WaterToAirHeatPump|PlantComponent:TemperatureSource
PlantEquipmentOperation:ComponentSetpoint|PlantLoop
Pump:ConstantSpeed|Pump:VariableSpeed
Pump:VariableSpeed:Condensate|Sizing:System
SolarCollector:FlatPlate:PhotovoltaicThermal|ThermalStorage:ChilledWater:Mixed
ThermalStorage:ChilledWater:Stratified|UnitarySystemPerformance:HeatPump:Multispeed
WaterHeater:Mixed|WaterHeater:Stratified
ZoneHVAC:Baseboard:Convective:Electric|ZoneHVAC:Baseboard:Convective:Water
ZoneHVAC:Baseboard:RadiantConvective:Electric|ZoneHVAC:Baseboard:RadiantConvective:Steam
ZoneHVAC:Baseboard:RadiantConvective:Water|ZoneHVAC:EnergyRecoveryVentilator
ZoneHVAC:EvaporativeCoolerUnit|ZoneHVAC:FourPipeFanCoil
ZoneHVAC:HighTemperatureRadiant|ZoneHVAC:IdealLoadsAirSystem
ZoneHVAC:LowTemperatureRadiant:Electric|ZoneHVAC:LowTemperatureRadiant:VariableFlow
ZoneHVAC:OutdoorAirUnit|ZoneHVAC:PackagedTerminalAirConditioner
ZoneHVAC:PackagedTerminalHeatPump|ZoneHVAC:TerminalUnit:VariableRefrigerantFlow
ZoneHVAC:UnitHeater|ZoneHVAC:UnitVentilator
ZoneHVAC:VentilatedSlab|ZoneHVAC:WaterToAirHeatPump
[ZoneHVAC:WindowAirConditioner](#zonehvacwindowairconditioner)|

### User or External Zone Design Flow Rate Inputs

In EnergyPlus the autosizing calculations start with a calculation of the zone design air flow rates using zone by zone design day simulations. The resulting zone design air flow rates and daily air flow sequences are used in the subsequent HVAC and central plant air and fluid flow design calculations and in the component autosizing calculations. The user can override or change the calculated zone design air flow rates in several ways.

#. The user can enter a value for *Sizing Factor* in the *Sizing:Parameters* object (see description below).
#. The user can specify a zone level *Zone Sizing Factor* in each *Sizing:Zone* object.
#. For each zone the user can input a *Cooling Design Air Flow Rate* and/or a *Heating Design Air Flow Rate* (and specify *Cooling Design Air Flow Method = Flow/Zone* and *Heating Design Air Flow Method = Flow/Zone*). These user inputs override the calculated values. The program divides the user input cooling or heating design air flow rate by the calculated values and uses the result as a zone sizing factor to multiply all the elements in the design heating and cooling air flow and load sequences. From this point the design calculations proceed as usual.

### User or External System Design Flow Rate Inputs

Using the results of the zone design air flow rate calculation (including any user input or altered flow rates) EnergyPlus proceeds to calculate central air system flow rates and cooling and heating loads. The results of this calculation can be overridden in the following way.

For each system ([AirLoopHVAC](#airloophvac)), in the corresponding [Sizing:System](#sizingsystem) object, specify *Cooling Design Air Flow Method* to be *Flow/System* and input a value for *Cooling Design Air Flow Rate*. **Similarly for heating specify *Heating Design Air Flow Method* to be *Flow/System* and input a value for *Heating Design Air Flow Rate*.

## DesignSpecification:OutdoorAir

This object allows for the outdoor air requirements to be defined in a common location for use by other objects. This object may be referenced by name from other objects (e.g., VAV terminal units) as required to identify an outdoor air quantity for use by that object. Note that a zone name Is not included as an input to this zone outdoor air definition and the number of people in a zone, zone floor area, and zone volume can only be determined after this object has been referenced by another. A single zone outdoor air definition may be referenced by multiple objects to specify that the same outdoor air requirements are used by those objects *or* multiple zone outdoor air objects may be defined and referenced by other objects as needed. If multiple zone outdoor air definitions are used, each outdoor air definition must have a unique name.

### Inputs

#### Field: Name

Unique identifying name. Any reference to this name by other objects will denote that the following outdoor air requirements will be used.

#### Field: Outdoor Air Method

The input must be either *Flow/Person*, *Flow/Area*, *Flow/[Zone](#zone), AirChanges/Hour*, *Sum*, or *Maximum*. *Flow/Person* means the program will use the input from the field *Outdoor Air Flow per Person* and the actual zone occupancy to calculate a zone outdoor air flow rate. *Flow/Area* means that the program will use the input from the field *Outdoor Air Flow per [Zone](#zone) Floor Area* and the actual zone floor area as the zone outdoor air flow rate. *Flow/[Zone](#zone)* means that the program will use the input of the field *Outdoor Air Flow per [Zone](#zone)* as the zone outdoor air flow rate. *AirChanges/Hour* means that the program will use the input from the field *Air Changes per Hour* and the actual zone volume (divided by 3600 seconds per hour) as the zone outdoor air flow rate. *Sum* means that the flows calculated from the fields *Outdoor Air Flow per Person,* *Outdoor Air Flow per Area, Outdoor Air Flow per [Zone](#zone)*, and *Air Changes per Hour* (using the associated conversions to m^3^/s for each field) will be added to obtain the zone outdoor air flow rate. *Maximum* means that the maximum flow derived from *Outdoor Air Flow per Person,* *Outdoor Air Flow per Area, Outdoor Air Flow per [Zone](#zone),* and *Air Changes per Hour* (using the associated conversions to m^3^/s for each field) will be used as the zone outdoor air flow rate. The default is *Flow/Person*.

#### Field: Outdoor Air Flow per Person

The design outdoor air volume flow rate per person for this zone in cubic meters per second per person. The default is 0.00944 (20 cfm per person). An outdoor air flow rate is calculated based on the total number of people for all [People](#people) statements assigned to the zone. Occupancy schedule values *are not* applied during sizing calculations and *are* applied during the remainder of the simulation. This input is used if *Outdoor Air Method* is one of *Outdoor Air Flow per Person*, *Sum*, or *Maximum*.

#### Field: Outdoor Air Flow per Zone Floor Area

The design outdoor air volume flow rate per square meter of floor area (units are m^3^/s-m^2^). This input is used if *Outdoor Air Method* is *Flow/Area, Sum* or *Maximum*. The default value for this field is 0.

#### Field: Outdoor Air Flow per Zone

The design outdoor air flow rate for this zone in cubic meters per second. This input field is used if *Outdoor Air Method* is *Flow/Zone, Sum* or *Maximum*. The default value for this field is 0.

#### Field: Outdoor Air Flow Changes per Hour

The design outdoor air volume flow rate in air changes per hour. This factor is used along with the [Zone](#zone) Volume and converted to cubic meters per second. This input field is used if *Outdoor Air Method* is *AirChanges/Hour, Sum* or *Maximum*. The default value for this field is 0.

#### Field: Outdoor Air Flow Rate Fraction Schedule Name

This field is the name of schedule that defines how outdoor air requirements change over time.  The field is optional.  If used, then the schedule values are multiplied by the outdoor air flow rate defined by the previous fields.  The schedule values must be between 0 and 1, inclusive.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    DesignSpecification:OutdoorAir
        ZoneOAData,            !- Name
        Sum,                   !- Outdoor Air Method
        0.00944,               !- Outdoor Air Flow per Person {m3/s}
        0.00305,               !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}
        ,                      !- Outdoor Air Flow per Zone {m3/s}
        ,                      !- Outdoor Air Flow Air Changes per Hour
        OARequirements Sched;  !- Outdoor Air Flow Rate Fraction Schedule Name

    Schedule:Compact,
        OARequirements Sched,    !- Name
        Any Number,              !- Schedule Type Limits Name
        Through: 12/31,          !- Field 1
        For: Weekdays SummerDesignDay WinterDesignDay,  !- Field 2
        Until: 24:00, 1.0,       !- Field 4
        For: AllOtherDays,       !- Field 5
        Until: 24:00, 0.5;       !- Field 7
~~~~~~~~~~~~~~~~~~~~

## DesignSpecification:ZoneAirDistribution

This object is used to describe the air distribution effectiveness and fraction of secondary recirculation air (return air not directly mixed with outdoor air) of a zone. It is referenced by the [Sizing:Zone](#sizingzone) and [Controller:MechanicalVentilation](#controllermechanicalventilation) objects.

### Inputs

#### *Field:* Name 

The unique user assigned name for an instance of this object. Any other object referencing this object will use this name.

#### *Field: Zone Air Distribution E*ffectiveness in Cooling Mode 

The positive numeric input for this field is the zone air distribution effectiveness when the zone is in cooling mode. Default value of this field is 1.0. ASHRAE Standard 62.1-2010 provides typical values.

#### *Field: Zone Air Distribution E*ffectiveness in Heating Mode 

The positive numeric input for this field is the zone air distribution effectiveness when the zone is in heating mode. Default value of this field is 1.0. ASHRAE Standard 62.1-2010 provides typical values as follows:

![Zone Air Distribution Effectiveness (Source: ASHRAE Standard 62.1-2010)](media/zone-air-distribution-effectiveness-source.png)


#### *Field: Zone Air Distribution* Effectiveness Schedule Name 

This optional field input points to a schedule with values of zone air distribution effectiveness. It provides a more flexible way of specifying zone air distribution effectiveness if it changes with time and/or system operating status and controls. If the schedule is specified, the zone air distribution effectiveness in cooling mode and heating mode will be ignored.

#### *Field: Zone Secon*dary Recirculation Fraction 

The non-negative numeric input for this field is the fraction of a zone's recirculation air that does not directly mix with the outdoor air. The zone secondary recirculation fraction Er is determined by the designer based on system configuration. For plenum return systems with secondary recirculation (e.g., fan-powered VAV with plenum return) Er is usually less than 1.0, although values may range from 0.1 to 1.2 depending upon the location of the ventilation zone relative to other zones and the air handler. For ducted return systems with secondary recirculation (e.g., fan-powered VAV with ducted return), Er is typically 0.0, while for those with system-level recirculation (e.g, dual-fan dual-duct systems with ducted return) Er is typically 1.0. For other system types, Er is typically 0.75. Minimum is 0.0, and default is 0.0 for single-path systems (also to maintain backward compatibility). For parallel fan-powered VAV systems, the secondary ventilation path only functions (Er > 0.0) when the fans in the VAV boxes operate, which is during heating. The local ventilation path and the benefits of secondary recirculation disappear during cooling, when the local parallel fans are off (Er = 0.0).

An example of this in an IDF context is shown:

~~~~~~~~~~~~~~~~~~~~

      DesignSpecification:ZoneAirDistribution,
        CM DSZAD ZN_1_FLR_1_SEC_1,  !- Name
        1,                       !- Zone Air Distribution Effectiveness in Cooling Mode {dimensionless}
        1,                       !- Zone Air Distribution Effectiveness in Heating Mode {dimensionless}
        ;                        !- Zone Air Distribution Effectiveness Schedule Name
~~~~~~~~~~~~~~~~~~~~

## Sizing:Parameters

This object allows the user to specify global heating and cooling sizing ratios. These ratios will be applied at the zone level to all of the zone heating and cooling loads and air flow rates. These new loads and air flow rates are then used to calculate the system level flow rates and capacities and are used in all component sizing calculations.

The user can also specify the width (in load timesteps) of a moving average window which can be used to smooth the calculated zone design flow sequences. The use of this parameter is described below.

### Inputs

#### Field: Heating Sizing Factor

The global heating sizing ratio applied to all of the zone design heating loads and air flow rates.

#### Field: Cooling Sizing Factor

The global cooling sizing ratio applied to all of the zone design cooling loads and air flow rates

#### Field: Timesteps in Averaging Window

The number of load timesteps in the zone design flow sequence averaging window. The default is 1, in which case the calculated zone design flow rates are averaged over the load timestep.

The zone design air flow rate calculation is performed assuming a potentially infinite supply of heating or cooling air at a fixed temperature. Thus the calculated design air flow rate will always be able to meet any load or change in load no matter how large or abrupt. In reality air flow rates are limited by duct sizes and fan capacities. The idealized zone design flow calculation may result in unrealistically large flow rates, especially if the user is performing the sizing calculations using thermostat schedules with night setup or setback. The calculated zone design flow rates are always averaged over the load timestep. The user may want to perform a broader average to mitigate the effect of thermostat setup and setback and prevent the warm up or cool down flow rates from dominating the design flow rate calculation.. Specifying the width of the averaging window allows the user to do this.

For example, if the load calculation timestep is 15 minutes and the user specifies the *Timesteps in Averaging [Window](#window)* to be 4, the zone design air flows will be averaged over a time period of 1 hour. Specifying 8 would result in averaging over a 2 hour period.

### Outputs

The sizing factors and the averaging window size are reported out on the *eplusout.eio* file. An example is:

~~~~~~~~~~~~~~~~~~~~

    ! <Load Timesteps in Zone Design Calculation Averaging Window>, Value
     Timesteps in Averaging Window,    1
    ! <Heating Sizing Factor Information>, Sizing Factor ID, Value
     Heating Sizing Factor, Global,   1.3000
     Heating Sizing Factor, Zone SPACE1-1,   1.3000
     Heating Sizing Factor, Zone SPACE2-1,   1.3000
     Heating Sizing Factor, Zone SPACE3-1,   1.3000
     Heating Sizing Factor, Zone SPACE4-1,   1.3000
     Heating Sizing Factor, Zone SPACE5-1,   1.3000
    ! <Cooling Sizing Factor Information>, Sizing Factor ID, Value
     Cooling Sizing Factor, Global,   1.3000
     Cooling Sizing Factor, Zone SPACE1-1,   1.3000
     Cooling Sizing Factor, Zone SPACE2-1,   1.3000
     Cooling Sizing Factor, Zone SPACE3-1,   1.3000
     Cooling Sizing Factor, Zone SPACE4-1,   1.3000
     Cooling Sizing Factor, Zone SPACE5-1,   1.3000
~~~~~~~~~~~~~~~~~~~~

## OutputControl:Sizing:Style 

As described early in the document (see: EnergyPlus Output Processing), the user may select the "style" for the sizing result files (epluszsz.<ext>, eplusssz.<ext>). This object applies to all sizing output files.

~~~~~~~~~~~~~~~~~~~~

    OutputControl:Sizing:Style,
           \memo default style for the Sizing output files is comma -- this works well for
           \memo importing into spreadsheet programs such as Excel(tm) but not so well for word
           \memo processing progams -- there tab may be a better choice.  fixed puts spaces between
           \memo the "columns"
           \unique-object
       A1; \field Column Separator
           \required-field
           \type choice
           \key Comma
           \key Tab
           \key Fixed
~~~~~~~~~~~~~~~~~~~~

### Inputs

#### Field: Column Separator

For this field, the desired separator for columns is entered. "Comma" creates comma separated fields/columns in the outputs (eplus<sizing type>.csv files are created). "Tab" creates tab separated fields/columns in the outputs (eplus<sizing type>.tab files are created). "Fixed" creates space separated fields/columns in the outputs (eplus<sizing type>.txt files are created) but these are not necessarily lined up for easy printing.

Note that both tab and comma separated files easily import into Excel™ or other spreadsheet programs. The tab delimited files can also be viewed by text editors, word processing programs and easily converted to "tables" within those programs.

## Sizing:Zone

The [Sizing:Zone](#sizingzone) object provides the data needed to perform a zone design air flow calculation for a single zone. This calculation assumes a variable amount of supply air at a fixed temperature and humidity. The information needed consists of the zone inlet supply air conditions: temperature and humidity ratio for heating and cooling. The calculation is done for every design day included in the input. The maximum cooling load and air flow and the maximum heating load and air flow are then saved for the system level design calculations and for the component automatic sizing calculations.

The [Sizing:Zone](#sizingzone) object is also the place where the user can specify the design outdoor air flow rate by referencing the name of a design specification outdoor air object. This can be specified in a number of ways (ref. DesignSpecification:OutdoorAir).This data is saved for use in the system sizing calculation or for sizing zone components that use outdoor air.

The user can also place limits on the heating and design cooling air flow rates. See  *Heating Design Air Flow Method* and *Cooling Design Air Flow Method* below and the explanations of the various heating and cooling flow input fields.

### Inputs

#### Field: Zone Name

The name of the [Zone](#zone) corresponding to this Sizing:[Zone](#zone) object. This is the zone for which the design air flow calculation will be made using the input data of this Sizing:[Zone](#zone) Object.

#### Field: Zone Cooling Design Supply Air Temperature Input Method

**The input must be either** *SupplyAirTemperature* or *TemperatureDifference*. *SupplyAirTemperature* means that the user inputs from the fields of [Zone](#zone) Cooling Design Supply Air Temperature will be used to determine the zone cooling design air flow rates. *TemperatureDifference* means that the user inputs from the fields of [Zone](#zone) Cooling Design Supply Air Temperature Difference will be used to determine the zone cooling design air flow rates.

#### Field: Zone Cooling Design Supply Air Temperature

The supply air temperature in degrees Celsius for the zone cooling design air flow rate calculation. Air is supplied to the zone at this temperature during the cooling design day simulation, The zone load is met by varying the zone air flow rate. The maximum zone flow rate is saved as the  zone cooling design air flow rate. This field is only used when [Zone](#zone) Cooling Design Supply Air Temperature Input Method = **SupplyAirTemperature**.

#### Field: Zone Cooling Design Supply Air Temperature Difference

The temperature difference between cooling design supply air temperature and room air temperature in degrees Celsius for the zone cooling design air flow rate calculation. Air is supplied to the zone at this temperature during the cooling design day simulation. The zone load is met by varying the zone air flow rate. The maximum zone flow rate is saved as the zone cooling design air flow rate. This field is only used when [Zone](#zone) Cooling Design Supply Air Temperature Input Method = **TemperatureDifference**.

#### Field: Zone Heating Design Supply Air Temperature Input Method

**The input must be either** *SupplyAirTemperature* or *TemperatureDifference*. *SupplyAirTemperature* means that the user inputs from the fields of [Zone](#zone) Heating Design Supply Air Temperature will be used to determine the zone heating design air flow rates. *TemperatureDifference* means that the user inputs from the fields of [Zone](#zone) Heating Design Supply Air Temperature Difference will be used to determine the zone heating design air flow rates.

#### Field: Zone Heating Design Supply Air Temperature

The supply air temperature in degrees Celsius for the zone heating design air flow rate calculation. Air is supplied to the zone at this temperature during the heating design day simulation, The zone load is met by varying the zone air flow rate. The maximum zone flow rate is saved as the zone heating design air flow rate. This field is only used when [Zone](#zone) Heating Design Supply Air Temperature Input Method = **SupplyAirTemperature**.

#### Field: Zone Heating Design Supply Air Temperature Difference

The temperature difference between heating design supply air temperature and room air temperature in degrees Celsius for the zone heating design air flow rate calculation. Air is supplied to the zone at this temperature during the heating design day simulation. The zone load is met by varying the zone air flow rate. The maximum zone flow rate is saved as the zone heating design air flow rate. This field is only used when [Zone](#zone) Heating Design Supply Air Temperature Input Method = **TemperatureDifference**.

#### Field: Zone Cooling Design Supply Air Humidity Ratio

The humidity ratio in kilograms of water per kilogram of dry air of the supply air in the zone cooling design air flow rate calculation.

#### Field: Zone Heating Design Supply Air Humidity Ratio

The humidity ratio in kilograms of water per kilogram of dry air of the supply air in the zone heating design air flow rate calculation.

#### Field: Design Specification Outdoor Air Object Name

This alpha field specifies the name of a [DesignSpecification:OutdoorAir](#designspecificationoutdoorair) object which specifies the design outdoor air flow rate for the zone.

#### Field: Zone Heating Sizing Factor

This input is a zone level heating sizing ratio. The zone design heating air flow rates and loads will be multiplied by the number input in this field. This input overrides the building level sizing factor input in the [Sizing:Parameters](#sizingparameters) object. And, of course, if this field is blank or zero, the global heating sizing factor from the [Sizing:Parameters](#sizingparameters) object is used.

#### Field: Zone Cooling Sizing Factor

This input is a zone level cooling sizing ratio. The zone design cooling air flow rates and loads will be multiplied by the number input in this field. This input overrides the building level sizing factor input in the [Sizing:Parameters](#sizingparameters) object. And, of course, if this field is blank or zero, the global cooling sizing factor from the [Sizing:Parameters](#sizingparameters) object is used.

#### Field: Cooling Design Air Flow Method

*The input must be either Flow/[Zone](#zone), DesignDay, or DesignDayWithLimit*. *Flow/[Zone](#zone)* means that the program will use the input of the field *Cooling Design Air Flow Rate* as the zone design cooling air flow rate. *DesignDay* means the program will calculate the zone design cooling air flow rate using the [Sizing:[Zone](#zone)](#sizingzone) input data and a design day simulation without imposing any limits other than those set by the minimum outside air requirements. *DesignDayWithLimit* means that the maximum from *Cooling Minimum Air Flow per [Zone](#zone) Floor Area* and *Cooling Minimum Air Flow* will set a lower limit on the **design maximum cooling air flow rate. The default method is *DesignDay*: i.e., the program uses the calculated design values subject to ventilation requirements.

#### Field: Cooling Design Air Flow Rate

The design zone cooling air flow rate in cubic meters per second. This input is used if *Cooling Design Air Flow Method* is specified as *Flow/Zone*. This value will be multiplied by the global or zone sizing factor and by zone multipliers.

#### Field: Cooling Minimum Air Flow per Zone Floor Area

The minimum zone cooling volumetric flow rate per square meter (units are m^3^/s-m^2^). This field is used when *Cooling Design Air Flow Method* is specified as *DesignDayWithLimit*. In this case it sets a lower bound on the zone design cooling air flow rate. In all cases the maximum flow derived from *Cooling Minimum Air Flow per [Zone](#zone) Floor Area*, *Cooling Minimum Air Flow*, and *Cooling Minimum Air Flow Fraction*  is used to set a minimum supply air flow rate for the zone for VAV systems. The default is .000762, corresponding to .15 cfm/ft^2^.  The applicable sizing factor is not applied to this value.

#### Field: Cooling Minimum Air Flow

The minimum zone cooling volumetric flow rate in m^3^/s. This field is used when *Cooling Design Air Flow Method* is specified as *DesignDayWithLimit*. In this case it sets a lower bound on the zone design cooling air flow rate. In all cases the maximum flow derived from *Cooling Minimum Air Flow per [Zone](#zone) Floor Area*, *Cooling Minimum Air Flow*, and *Cooling Minimum Air Flow Fraction*  is used to set a minimum supply air flow rate for the zone for VAV systems. The default is zero.  The applicable sizing factor is not applied to this value.

#### Field: Cooling Minimum Air Flow Fraction

The minimum zone design cooling volumetric flow rate expressed as a fraction of the zone design cooling volumetric flow rate. . In all cases the maximum flow derived from *Cooling Minimum Air Flow per [Zone](#zone) Floor Area*, *Cooling Minimum Air Flow*, and *Cooling Minimum Air Flow Fraction*  is used to set a minimum supply air flow rate for the zone for VAV systems. The default is zero. This input is currently used in sizing the fan minimum flow rate. It does not currently affect other component autosizing.

#### Field: Heating Design Air Flow Method

*The input must be either Flow/[Zone](#zone), DesignDay, or DesignDayWithLimit*. *Flow/[Zone](#zone)* means that the program will use the input of the field *Heating Design Air Flow Rate* as the zone design heating air flow rate. *DesignDay* means the program will calculate the zone design heating air flow rate using the [Sizing:[Zone](#zone)](#sizingzone) input data and a design day simulation without imposing any limits other than those set by the minimum outside air requirements. *DesignDayWithLimit* means that the maximum from *Heating Maximum Air Flow per [Zone](#zone) Floor Area* and *Heating Maximum Air Flow* will set a lower limit on the **design maximum heating air flow rate. The default method is *DesignDay*: i.e., the program uses the calculated design values subject to ventilation requirements.

#### Field: Heating Design Air Flow Rate

The design zone heating air flow rate in cubic meters per second. This input is used if *Heating Design Air Flow Method* is specified as *Flow/Zone*. This value will be multiplied by the global or zone sizing factor and by zone multipliers.

#### Field: Heating Maximum Air Flow per Zone Floor Area

The maximum zone heating volumetric flow rate per square meter (units are m^3^/s-m^2^). This field is used when *Heating Design Air Flow Method* is specified as *DesignDayWithLimit*. In this case it sets an upper bound on the zone design heating air flow rate. For this and the next two input fields, the maximum flow derived from *Heating Maximum Air Flow per [Zone](#zone) Floor Area*, *Heating Maximum Air Flow*, and *Heating Maximum Air Flow Fraction*  is used to set a maximum heating supply air flow rate for the zone for VAV systems. The default is .002032, corresponding to .40 cfm/ft^2^. If the maximum heating design flow rate calculated using these input fields is greater than the design heating flow rate calculated during sizing, these input fields have no impact on sizing. It may be more appropriate to select only one of these three fields to calculate the maximum heating design flow rate (i.e., if one ore more of these three fields is 0, it will not be used in calculating the maximum heating design flow rate).

#### Field: Heating Maximum Air Flow

The maximum zone heating volumetric flow rate in m^3^/s. This field is used when *Heating Design Air Flow Method* is specified as *DesignDayWithLimit*. In this case it sets an upper bound on the zone design heating air flow rate. For this field and the two input fields just prior to and after this field,,the maximum flow derived from *Heating Maximum Air Flow per [Zone](#zone) Floor Area*, *Heating Maximum Air Flow*, and *Heating Maximum Air Flow Fraction* is used to set a maximum heating supply air flow rate for the zone for VAV systems. The default is .1415762, corresponding to 300 cfm. If the maximum heating design flow rate calculated using these input fields is greater than the design heating flow rate calculated during sizing, these input fields have no impact on sizing. It may be more appropriate to select only one of these three fields to calculate the maximum heating design flow rate (i.e., if one ore more of these three fields is 0, it will not be used in calculating the maximum heating design flow rate).

#### Field: Heating Maximum Air Flow Fraction

The maximum zone design heating volumetric flow rate expressed as a fraction of the zone design cooling volumetric flow rate. For this and the previous two input fields, the maximum flow derived from *Heating Maximum Air Flow per [Zone](#zone) Floor Area*, *Heating Maximum Air Flow*, and *Heating Maximum Air Flow Fraction*  is used to set a maximum heating supply air flow rate for the zone for VAV systems. The default is 0.3. If the maximum heating design flow rate calculated using these input fields is greater than the design heating flow rate calculated during sizing, these input fields have no impact on sizing. It may be more appropriate to select only one of these three fields to calculate the maximum heating design flow rate (i.e., if one ore more of these three fields is 0, it will not be used in calculating the maximum heating design flow rate).

#### *Field: Design Specification* Zone Air Distribution Object Name

The name of the [DesignSpecification:ZoneAirDistribution](#designspecificationzoneairdistribution) object, defining the air distribution effectiveness and secondary recirculation air fraction, that applies to the zone or zone list. This object may be used for the same zone in the [Controller:MechanicalVentilation](#controllermechanicalventilation) object if no such [DesignSpecification:ZoneAirDistribution](#designspecificationzoneairdistribution) object is specified.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Sizing:Zone,
        SPACE5-1,                !- Name of a zone
        14.,                     !- Zone cooling design supply air temperature {C}
        50.,                     !- Zone heating design supply air temperature {C}
        0.009,                   !- Zone cooling design supply air humidity ratio {kg-H2O/kg-air}
        0.004,                   !- Zone heating design supply air humidity ratio {kg-H2O/kg-air}
        DSOA1,                   !- Design Specification Outdoor Air Object Name
        0.0,                     !- zone heating sizing factor
        0.0,                     !- zone cooling sizing factor
        designdaywithlimit,      !- Cooling Design Air Flow Method
        ,                        !- cooling design air flow rate {m3/s}
        ,                        !- Cooling Minimum Air Flow per zone area {m3/s-m2}
        ,                        !- Cooling Minimum Air Flow {m3/s}
        ,                        !- fraction of the cooling design air flow rate
        designday,               !- Heating Design Air Flow Method
        ,                        !- heating design air flow rate {m3/s}
        ,                        !- heating max air flow per zone area {m3/s-m2}
        ,                        !- heating max air flow {m3/s}
        ,                        !- fraction of the cooling design air flow rate
    DSZADO1;                 !- Design Specification Zone Air Distribution Object Name

    DesignSpecification:OutdoorAir,
    DSOA1,                   !- Name
    SUM,                     !- Outdoor Air Method
    0.00236,                 !- Outdoor Air Flow per Person
    0.000305,                !- Outdoor Air Flow per Zone Floor Area
    0.0,                     !- Outdoor Air Flow per Zone
    0.0,                     !- Outdoor Air Flow Air Changes per Hour
    ;                        !- Outdoor Air Flow Rate Fraction Schedule Name

    DesignSpecification:ZoneAirDistribution,
        DSZADO1,                 !- Name
        1.0,                     !- Zone Air Distribution Effectiveness in Cooling Mode
        1.0,                     !- Zone Air Distribution Effectiveness in Heating Mode
        ,                        !- Zone Air Distribution Effectiveness Schedule Name
        0.3;                     !- Zone Secondary Recirculation Fraction
~~~~~~~~~~~~~~~~~~~~

### Outputs

The zone design air flow rates and loads are output onto the local file "epluszsz.<ext>" where <ext> is the extension from the sizing style object (default is csv – a comma separated file *epluszsz.csv)*. The columns are clearly labeled. It will easily import into Excel or other spreadsheet program that accepts delimited files. All of these values are design air flow rates and loads *calculated by the program*. No sizing factors have been applied.

The calculated zone design air flow rates and the user input or altered zone design air flow rates are also reported on the *eplusout.eio* file. The values are printed out for each zone as comma separated records beginning with *Zone Sizing*. Items output on the *eio* file are: zone name, load type (heating or cooling), design load, calculated design air flow rate, user design air flow rate, design day name, time of peak, outside temperature at peak, outside humidity ratio at peak.

## Sizing:System

The [Sizing:System](#sizingsystem) object contains the input needed to perform a central forced air system design air flow, heating capacity, and cooling capacity calculation for a system serving one or more zones. The information needed consists of the outside environmental conditions and the design supply air temperatures, outdoor air flow rate, and minimum system air flow ratio.

The outside conditions come from the design days in the input. A system sizing calculation is performed for every design day in the input file and the resulting maximum heating and cooling air flow rates and capacities are saved for use in the component sizing calculations.

Supply air conditions are specified by inputting a supply air temperature for cooling, a supply air temperature for heating, and a preheat temperature.

The system sizing calculation sums the zone design air flow rates to obtain a system supply air flow rate. The design conditions and the outdoor air flow rate are used to calculate a design mixed air temperature. The temperature plus the design supply air temperatures allows the calculation of system design heating and cooling capacities.

### Inputs

#### Field: AirLoop Name

The name of the [AirLoopHVAC](#airloophvac) corresponding to this [Sizing:System](#sizingsystem) object. This is the air system for which the design calculation will be made using the input data of this [Sizing:System](#sizingsystem) Object.

#### Field: Type of Load to Size On

The user specified type of load on which to size the central system. The choices are *Sensible*, *Latent*, *Total* and *VentilationRequirement*. Only *Sensible* and *VentilationRequirement* are operational. *Sensible* means that the central system supply air flow rate will be determined by combining the zone design air flow rates, which have been calculated to meet the zone sensible loads from the design days. *VentilationRequirement* means that the central system supply air flow rate will be determined by the system ventilation requirement.

#### Field: Design Outdoor Air Flow Rate

The design outdoor air flow rate in cubic meters per second. Generally this should be the minimum outdoor air flow. It is used for both heating and cooling design calculations. The assumption for cooling is that any outdoor air economizer will be closed. If *Autosize* is input the outdoor air flow rate will be taken from the sum of the zone outdoor air flow rates or calculated based on the System Outdoor Air Method selection (field below).

#### Field: Minimum System Air Flow Ratio

The design minimum central air flow ratio. This ratio is the minimum system air flow rate divided by the maximum system air flow rate. The value must be between 0 and 1. For constant volume systems the ratio should be set to 1. Note that this ratio should be set to reflect what the user expects the system flow rate to be when maximum heating demand occurs. This ratio is used in calculating the central system heating capacity. Thus if the system is VAV with the zone VAV dampers held at minimum flow when there is a zone heating demand, this ratio should be set to the minimum flow ratio. If the zone VAV dampers are reverse action and can open to full flow to meet heating demand, this ratio should be set to 1.

#### Field: Preheat Design Temperature

The design air temperature exiting the preheat coil (if any) in degrees Celsius.

#### Field: Preheat Design Humidity Ratio

The design humidity ratio exiting the preheat coil (if any) in kilograms of water per kilogram of dry air. (kgWater/kgDryAir)

#### Field: Precool Design Temperature

The design air temperature exiting the precooling coil (if any) in degrees Celsius.

#### Field: Precool Design Humidity Ratio

The design humidity ratio exiting the precooling coil (if any) in kilograms of water per kilogram of dry air. (kgWater/kgDryAir)

#### Field: Central Cooling Design Supply Air Temperature

The design supply air temperature for cooling in degrees Celsius. This should be the temperature of the air exiting the central cooling coil.

#### Field: Central Heating Design Supply Air Temperature

The design supply air temperature for heating in degrees Celsius. This can be either the reset temperature for a single duct system or the actual hot duct supply air temperature for dual duct systems. It should be the temperature at the exit of the main heating coil.

#### Field: Sizing Option

If the input is *coincident* the central system air flow rate will be sized on the sum of the coincident zone air flow rates. If the input is *noncoincident* the central system air flow rate will be sized on the sum of the noncoincident zone air flow rates. The default is noncoincident.

#### Field: 100% Outdoor Air in Cooling

Entering *Yes* means the system will be sized for cooling using 100% outdoor air. Entering *No* means the system will be sized for cooling using minimum outside air (the default).

#### Field: 100% Outdoor Air in Heating

Entering *Yes* means the system will be sized for heating using 100% outdoor air. Entering *No* means the system will be sized for heating using minimum outside air (the default).

#### Field: Central Cooling Design Supply Air Humidity Ratio

The design humidity ratio in kilograms of water per kilogram of dry air at the exit of the central cooling coil. (kgWater/kgDryAir) The default is .008.

#### Field: Central Heating Design Supply Air Humidity Ratio

The design humidity ratio in kilograms of water per kilogram of dry air at the exit of the central heating coil. (kgWater/kgDryAir) The default is .008.

#### Field: Cooling Design Air Flow Method

The input of this field must be the method used to determine the airloop cooling supply air volume flow rate or the cooling supply air flow rate value. The input allowed must be is either, *DesignDay*, *Flow/System, FlowPerFloorArea*, *FractionOfAutosizedCoolingAirflow*, or *FlowPerCoolingCapacity*. *DesignDay* means the program will calculate the system design cooling air flow rate using the System Sizing input data and a design day simulation. *Flow/System* means that the the program will use the input of the field *Cooling Design Air Flow Rate* as the system design cooling air flow rate. *FlowPerFloorArea* means the program calculates the cooling supply air volume flow rate from zone floor area served by the airloop and user specified *Flow Per Floor Area* value. *FractionOfAutosizedCoolingAirflow* means the program calculates the cooling supply air volume flow rate from user specified fraction and the autosized design cooling supply air volume flow rate value determined by the simulation. *FlowPerCoolingCapacity* means the supply air volume is calculated from user specified flow per cooling capacity and design cooling capacity determined by the simulation. The default method is *DesignDay*: i.e., the program uses the calculated design values.

#### Field: Cooling Design Air Flow Rate

The design system cooling air flow rate in cubic meters per second. This input is an alternative to using the program autocalculated value. This input is used if Cooling Design Air Flow Method is Flow/System. This value will *not* be multiplied by any sizing factor or by zone multipliers. If using zone multipliers, this value must be large enough to serve the multiplied zones.

#### Field: Cooling Design Supply Air Flow Rate Per Floor Area {m3/s-m2}

Enter the cooling supply air volume flow rate per zone conditioned floor area in m3/s-m2. This field is required field when the Cooling Design air Flow Method is *FlowPerFloorArea*. This field may be left blank if a cooling coil is not included in the airloop or the Cooling Design Air Flow Method is not *FlowPerFloorArea*. The program calculates the cooling supply air volume flow rate from the cooled floor area served by the air loop and the *Flow Per Unit Area* value specified by the user.

#### Field: Fraction of Autosized Cooling Design Supply Air Flow Rate {-}

Enter the cooling supply air volume flow rate as a fraction of the airloop autosized cooling supply air flow rate. This input field is required when the Cooling Design air Flow Method is *FractionOfAutosizedCoolingAirflow*. This input field may be left blank if a cooling coil is not included in the airloop or the Cooling Design air Flow Method is not *FractionOfAutosizedCoolingAirflow*. The program calculates the cooling supply air volume flow rate from the design autosized cooling supply air flow rate and user specified fraction.

#### Field: Cooling Design Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}

Enter the cooling supply air volume flow rate per unit cooling capacity in m3/s-W. This input field is required when the Cooling Design air Flow Method is *FlowPerCoolingCapacity*. This field may be left blank if a cooling coil is not included in the airloop or the Cooling Design air Flow Method is not *FlowPerCoolingCapacity*. The program calculates the airloop cooling supply air volume flow rate from the design autosized cooling capacity and user specified *Flow Per Cooling Capacity* value.

#### Field: Heating Design Air Flow Method

The input of this field must be the method used to determine the airloop heating supply air volume flow rate or the heating supply air flow rate value. The input allowed must be is either, *DesignDay*, *Flow/System, FlowPerFloorArea*, *FractionOfAutosizedHeatingAirflow*, *FractionOfAutosizedCoolingAirflow* or *FlowPerHeatingCapacity*. *DesignDay* means the program will calculate the system design heating air flow rate using the System Sizing input data and a design day simulation. *Flow/System* means that the program will use the input of the field *Heating Design Air Flow Rate* as the system design heating air flow rate. *FlowPerFloorArea* means the program calculates the system heating supply air volume flow rate from zone floor area served by the airloop and user specified *Flow Per Floor Area* value. *FractionOfAutosizedHeatingAirflow* means the program calculates the system heating supply air volume flow rate from user specified fraction and the autosized system design heating supply air volume flow rate value determined by the simulation. *FractionOfAutosizedCoolingAirflow* means the program calculates the system heating supply air volume flow rate from user specified fraction and the autosized system design cooling supply air volume flow rate value determined by the simulation. *FlowPerHeatingCapacity* means the system heating supply air volume is calculated from user specified flow per heating capacity and design heating capacity determined by the simulation. The default method is *DesignDay*: i.e., the program uses the calculated design values.

#### Field: Heating Design Air Flow Rate

The design system heating air flow rate in cubic meters per second. This input is an alternative to using the program autocalculated value. This input is used if Heating Design Air Flow Method is Flow/System. This value will *not* be multiplied by any sizing factor or by zone multipliers. If using zone multipliers, this value must be large enough to serve the multiplied zones.

#### Field: Heating Design Supply Air Flow Rate Per Floor Area {m3/s-m2}

Enter the heating supply air volume flow rate per zone conditioned floor area in m3/s-m2. This field is required field when the Heating Design air Flow Method is *FlowPerFloorArea*. This field may be left blank if a heating coil is not included in the airloop or the Heating Design Air Flow Method is not *FlowPerFloorArea*. The program calculates the heating supply air volume flow rate from the heated or cooled floor area served by the air loop and the *Flow Per Unit Area* value specified by the user.

#### Field: Fraction of Autosized Heating Design Supply Air Flow Rate {-}

Enter the heating supply air volume flow rate as a fraction of the airloop autosized heating supply air flow rate. This input field is required when the Heating Design air Flow Method is *FractionOfAutosizedHeatingAirflow*. This input field may be left blank if heating coil is not included in the airloop or the Heatng Design air Flow Method is not *FractionOfAutosizedHeatingAirflow*. The program calculates the heating supply air volume flow rate from the design autosized heating supply air flow rate and user specified fraction.

#### Field: Fraction of Autosized Cooling Design Supply Air Flow Rate {-}

Enter the heating supply air volume flow rate as a fraction of the airloop autosized cooling supply air flow rate. This input field is required when the Heating Design air Flow Method is *FractionOfAutosizedCoolingAirflow*. This input field may be left blank if heating coil is not included in the airloop or the Heatng Design air Flow Method is not *FractionOfAutosizedCoolingAirflow*. The program calculates the heating supply air volume flow rate from the design autosized cooling supply air flow rate and user specified fraction.

#### Field: Heating Design Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}

Enter the heating supply air volume flow rate per unit heating capacity in m3/s-W. This input field is required when the Heating Design air Flow Method is *FlowPerCoolingCapacity*. This field may be left blank if a heating coil is not included in the airloop or the Heating Design air Flow Method is not *FlowPerHeatingCapacity*. The program calculates the airloop heating supply air volume flow rate from the design autosized heating capacity and user specified *Flow Per Heating Capacity* value.

#### Field: System Outdoor Air Method

The method used to calculate the system minimum outdoor air flow. The two choices are ZoneSum and VentilationRateProcedure (VRP). ZoneSum sums the outdoor air flows across all zones served by the system. VRP uses the multi-zone equations defined in 62.1-2007 to calculate the system outdoor air flow. VRP considers zone air distribution effectiveness and zone diversification of outdoor air fractions.

#### Field: Zone Maximum Outdoor Air Fraction

This positive numeric input is the zone maximum outdoor air fraction. For an air loop, when a zone requires outdoor air higher than the user specified [Zone](#zone) Maximum Outdoor Air Fraction, the zone supply air flow will be increased to cap the outdoor air fraction at the maximum value. This allows the system level outdoor air flow to be reduced while the total supply air flow increases. Valid values are from 0 to 1.0. Default is 1.0 which indicates zones can have 100% outdoor air maintaining backward compatibility. This inputs work for constant volume air systems, single and dual duct VAV systems.

#### Field Cooling Design Capacity Method

Enter the method used to determine the cooling design capacity for scalable sizing. Input allowed is either *None*, *CoolingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedCoolingCapacity*. None is used when a cooling coil is not included in the airloop. If this input field is left blank, or None is specified, then the autosized design cooling capacity determined by the program is used. *CoolingDesignCapacity* means user specifies the magnitude of cooling capacity or the program calculates the design cooling capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design cooling capacity from user specified cooling capacity per floor area and floor area of the zones served by the airloop. *FractionOfAutosizedCoolingCapacity* means the program calculates the design cooling capacity from user specified fraction and the auto-sized design cooling capacity. If the value this input field is blank or specified as None, then the next three input fields are not required. The default method is *CoolingDesignCapacity*.

#### Field: Cooling Design Capacity {W}

Enter the magnitude of the cooling capacity in Watts. This input is an alternative to using the program auto-calculated cooling capacity value. This input is a required field when the Cooling Design Capacity Method is *CoolingDesignCapacity*. This field may be left blank if a cooling coil is not included in the air loop or alternative method is specified. This input field is autosizable.

#### Field: Cooling Design Capacity Per Floor Area {W/m2}

Enter the cooling capacity per unit floor area in m3/s-m2. This field is required field when the Cooling Design Capacity Method is *CapacityPerFloorArea*. This field may be left blank if a cooling coil is not included in the airloop or the Cooling Design Capacity Method is not *CapacityPerFloorArea*. The program calculates the cooling capacity from floor area of the zones served by the airloop and the cooling capacity per unit floor area value specified by the user.

#### Field: Fraction of Autosized Cooling Design Capacity {-}

Enter the cooling capacity as a fraction of the autosized cooling capacity. This input field is required when the Cooling Design Capacity Method is *FractionOfAutosizedCoolingCapacity*. This input field may be left blank if a cooling coil is not included in the zone HVAC equipment or the Cooling Design Capacity Method is not *FractionOfAutosizedCoolingCapacity*. The program calculates the cooling capacity from the design autosized cooling capacity and user specified fraction. Design day sizing run must be specified.

#### Field: Heating Design Capacity Method

Enter the method used to determine the heating design capacity for scalable sizing. Input allowed is either *None*, *HeatingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedHeatingCapacity*. *None* is used when a heating coil is not included in the airloop. If this input field is left blank, then the autosized design heating capacity determined by the program is used. *HeatingDesignCapacity* means user specifies the magnitude of heating capacity or the program calculates the design heating capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design heating capacity from user specified heating capacity per floor area and floor area of the zones served by the airllop. *FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity. If the value this input field is blank or specified as None, then the next three input fields are not required. The default method is *HeatingDesignCapacity*.

#### Field: Heating Design Capacity {W}

Enter the magnitude of the heating capacity in Watts. This input is an alternative to using the program auto-calculated heating capacity value. This input is a required field when the Heating Design Capacity Method is *HeatingDesignCapacity*. This field may be left blank if a heating coil is not included in the airloop or alternative method is specified. This input field is autosizable.

#### Field: Heating Design Capacity Per Floor Area {W/m2}

Enter the heating capacity per unit floor area in m3/s-m2. This field is required field when the Heating Design Capacity Method is *CapacityPerFloorArea*. This field may be left blank if a heating coil is not included in the airloop or the Heating Design Capacity Method is not *CapacityPerFloorArea*. The program calculates the heating capacity from floor area of the zones served by the airloop and the heating capacity per unit floor area value specified by the user.

#### Field: Fraction of Autosized Heating Design Capacity {-}

Enter the heating capacity as a fraction of the autosized heating capacity. This input field is required when the Heating Design Capacity Method is FractionOfAutosizedHeatingCapacity. This input field may be left blank if heating coil is not included in the airloop or the Heating Design Capacity Method is not FractionOfAutosizedHeatingCapacity. The program calculates the heating capacity from the design autosized cooling capacity and user specified fraction.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Sizing:System,
        VAV Sys 1,               !- name of an AirLoopHVAC object
        sensible,                !- type of load to size on
        autosize,                !- Design (minimum) outside air volumetric flow rate {m3/s}
        0.3,                     !- minimum system air flow ratio
        4.5,                     !- Preheat design temperature {C}
        .008,                    !- Preheat design humidity ratio {kgWater/kgDryAir}
        11.0,                    !- Precool design temperature {C}
        .008,                    !- Precool design humidity ratio {kgWater/kgDryAir}
        12.8,                    !- Central cooling design supply air temperature {C}
        16.7,                    !- Central heating design supply air temperature {C}
        noncoincident,           !- Sizing Option
        no,                      !- Cooling 100% Outside Air
        no,                      !- Heating 100% Outside Air
        0.008,                   !- Central cooling design supply air humidity ratio {kgWater/kgDryAir}
        0.008,                   !- Central heating design supply air humidity ratio {kgWater/kgDryAir}
        designday,               !- Cooling Design Air Flow Method
        0,                       !- cooling design air flow rate {m3/s}
        ,                        !- Supply Air Flow Rate Per Floor Area During Cooling Operation {m3/s-m2}
        ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate {-}
        ,                        !- Design Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}
        designday,               !- Heating Design Air Flow Method
        0,                       !- heating design air flow rate {m3/s}
        ,                        !- Supply Air Flow Rate Per Floor Area During Heating Operation {m3/s-m2}
        ,                        !- Fraction of Autosized Design Heating Supply Air Flow Rate {-}
        ,                        !- Fraction of Autosized Design Cooling Supply Air Flow Rate {-}
        ,                        !- Design Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}
        ZoneSum,                 !- System Outdoor Air Method
        0.5,                     !- Zone Maximum Outdoor Air Fraction
        CoolingDesignCapacity,   !- Cooling Design Capacity Method
        autosize,                !- Cooling Design Capacity {W}
        ,                        !- Cooling Design Capacity Per Floor Area {W/m2}
        ,                        !- Fraction of Autosized Cooling Design Capacity {-}
        HeatingDesignCapacity,   !- Heating Design Capacity Method
        autosize,                !- Heating Design Capacity {W}
        ,                        !- Heating Design Capacity Per Floor Area {W/m2}
        ;                        !- Fraction of Autosized Cooling Design Capacity {-}
~~~~~~~~~~~~~~~~~~~~

### Outputs

The system design air flow rates and heating and cooling capacities are output onto the local file "eplusssz.<ext>" where <ext> is the extension from the sizing style object (default is csv – a comma separated file *eplusssz.csv)*. The columns are clearly labeled. It will easily import into Excel or other spreadsheet program that accepts delimited files. The results are calculated values and do not include any user input system flow rates.

The calculated system design air flow rates and the user input system design air flow rates are also reported on the *eplusout.eio* file. The values are printed out for each system as comma separated records beginning with *System Sizing*. An example is:

~~~~~~~~~~~~~~~~~~~~

    ! <System Sizing Information>, System Name, Field Description, Value
     System Sizing, VAV SYS 1, Calculated Cooling Design Air Flow Rate [m3/s],   1.3194
     System Sizing, VAV SYS 1, User Cooling Design Air Flow Rate [m3/s],   1.5000
     System Sizing, VAV SYS 1, Calculated Heating Design Air Flow Rate [m3/s],  0.90363
     System Sizing, VAV SYS 1, User Heating Design Air Flow Rate [m3/s],   1.0000
~~~~~~~~~~~~~~~~~~~~

## Sizing:Plant

The [Sizing:Plant](#sizingplant) object contains the input needed for the program to calculate plant loop flow rates and equipment capacities when autosizing. This information is initially used by components that use water for heating or cooling such as hot or chilled water coils to calculate their maximum water flow rates. These flow rates are then summed for use in calculating the Plant Loop flow rates.

The program will size any number of chilled water, hot water, condenser water and other plant loops. There should be one [Sizing:Plant](#sizingplant) object for each plant loop that is to be autosized.

### Inputs

#### Field: Plant or Condenser Loop Name

The name of a Plant Loop or Condenser Loop object corresponding to this [Sizing:Plant](#sizingplant) object. This is the plant loop for which this data will be used for calculating the loop flow rate.

#### Field: Loop Type

The possible inputs are *Heating, Steam, Cooling,* or *Condenser*.

#### Field: Design Loop Exit Temperature

The water temperature in degrees Celsius at the exit of the supply side of the plant loop, Thus this is the temperature of the water supplied to the inlet of chilled or hot water coils and other equipment that places loads on a plant loop.

#### Field: Loop Design Temperature Difference

The design temperature rise (for cooling or condenser loops) or fall (for heating loops) in degrees Celsius across the demand side of a plant loop.  This temperature difference is used by component models to determine flow rates required to meet design capacities.  Larger values lead to smaller design flow rates.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Sizing:Plant,
       Chilled Water Loop, ! name of loop
       Cooling,            ! type of loop
       7.22,               ! chilled water supply temperature
       6.67;               ! chilled water delta T
~~~~~~~~~~~~~~~~~~~~

### Outputs

The loop flow rates are reported on the *eplusout.eio* file along with the component sizing results.