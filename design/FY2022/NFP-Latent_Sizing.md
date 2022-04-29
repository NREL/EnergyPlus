Autosizing for High Latent Loads
================
## Initial New Feature Proposal and Design Documentation ##

**Richard Raustad, FSEC Energy Research Center**

 - Original Date: 4/29/2022
 - Final NFP Revision Date: 
 

## Justification for New Feature ##

**Feature request description:** When the internal latent load is high or dominant, E+ fails to autosize HVAC. An example is indoor growing facilities. Enhancing autosize to accommodate this situation will be a huge assist for modelers working on those facility types.

**Additional information:** Trane has an [Engineers Newsletter, volume 48–3](https://www.trane.com/content/dam/Trane/Commercial/global/products-systems/education-training/engineers-newsletters/airside-design/admapn071en-082019.pdf) describing sizing theory used for growing facilities which basically describes the zone sensible and latent loads in grow facilities and the associated psychrometric charts associated with cooling coil and reheat coil capacity requirements.

## E-mail and  Conference Call Conclusions ##

TBD

## Summary ##

EnergyPlus currently calculates a zone design sensible air flow rate based on a predicted zone sensible load and user provided supply air conditions. The following describes a mirrored approach to calculate and report the zone design latent air flow rate. The underlying principal is that the existing calculation of zone sensible load and corresponding **sensible** air flow rate provides a zone flow rate that meets the peak cooling or heating load. This calculation is based on a user provided zone temperature set point and a design supply air temperature (or delta T). If the calculated peak air flow rate is provided to a zone at the user supplied supply air temperature, the peak zone sensible load will be met (assuming design day conditions accurately represent peak weather file conditions). A similar approach is proposed for latent sizing and described in this new feature proposal.

## Overview ##

**Current Methodology:**

During zone sizing calculations, E+ currently predicts the zone sensible load based on the active thermostat set point temperature. Latent load is also predicted during zone sizing but currently unused. During zone sizing, the zone set point temperature is maintained at set point to facilitate the calculation of only the zone sensible loads in the zone load prediction (i.e., no inclusion of loads due to zone thermal capacitance or storage other than zone thermostat setup and setback). The impact of a DOAS system can be included in zone sizing calculations, if desired, to provide a more accurate zone sensible load. For each design day and each time interval, the zone sensible air mass flow rate is calculated based on the predicted zone sensible load and user specified supply air conditions. The peak zone sensible load, derived from time series data collected during zone sizing, is used to select the zone sensible design air flow rate. The results of zone sensible sizing are reported to the zone sizing report (`epluszsz.csv`) as the zone sensible cooling and heating mass flow rates and zone sensible cooling and heating loads (4 reports per zone). The peak zone sensible design air flow rate is subsequently used for zone equipment and cumulatively for air system component sizing.

**Proposed Methodology:**

A similar approach to the existing zone sensible air flow calculation will be used to determine zone latent air flow rate. The zone condition will be maintained at an assumed or humidistat set point to facilitate an accurate prediction of zone latent loads. It is proposed that zone latent sizing occur based on a user input/request, whether or not a humidistat is present, however, if a humidistat is present the zone humidistat will be used to provide the control point. If latent sizing is not requested and a humidistat is present that zone's latent load could still be calculated (or turned off as desired) based on an assumed humidistat set point. The results of zone latent sizing will be included in the zone sizing report as the zone latent cooling and heating mass flow rates and zone latent cooling and heating loads. For comparison purposes, two additional reports will be added to document the original zone sensible and latent loads prior to application of a DOAS system as Sensible Load No DOAS and Latent Load No DOAS (10 reports per zone).

The proposed methodology allows a user to determine both sensible and latent loads via a loads only type simulation ([PR #9402](https://github.com/NREL/EnergyPlus/pull/9402)). Uncontrolled zones will not have a sensible or latent load and are identified by whether or not a `ZoneHVAC:EquipmentConnections` object is associated with a zone (which identifies the zone supply air node name). 

## Approach ##

**Porposed update to Function SizeZoneEquipment**

E+ currently calculates the sensible air mass flow rate required to meet the zone sensible load. The sensible air mass flow rate and user supplied supply air temperature and humidity ratio are passed to the zone as the supply air conditions to be used in the correct step of ZoneTempPredictorCorrector (`CorrectZoneAirTemp` and `CorrectZoneHumRat`). The sensible and latent loads are also passed to `UpdateSystemOutputRequired`, which should return 0 remaining sensible and latent loads (as xxxOutputProvided).

    UpdateSystemOutputRequired(state, ActualZoneNum, SysOutputProvided, LatOutputProvided);

This new feature will use similar calculations where a zone latent load is used to calculate a zone heating and cooling latent air mass flow rate based on the user specified zone supply air humidity ratio. One caveat here is that since the latent mass flow rate will usually be different from the sensible mass flow rate, an adjustment to the supply air humidity ratio will be calculated such that both the sensible and latent loads are met each time interval to maintain the zone at the thermostat and humidistat set points. Initial testing has determined that holding the zone at a predetermined humidity level does not significantly impact the zone sensible loads or prediction of sensible air mass flow rate.

**New Feature Choice Options**

The `Sizing:System` object has a choice for `Type of Load to Size On`. Valid choices for this field are Sensible, Total and VentilationRequirement. A new key choice will be added for Latent. See Input Description section below.

**Turning Latent Sizing On and Off**

**Zone**

The inclusion or removal of the zone humidistat could trigger the zone latent load calculations used for zone equipment sizing (Option A). Alternately, proposed solution is to add two new input fields to `Sizing:Zone` that requests the type of load sizing to perform with a default that provides the same answer currently provided and an assumed humidity set point (Option B). There will likely be some coordination required between these new inputs and those proposed for the air system.

**System**

There may be zones connected to an air system that do not have a humidistat or there may be no zones with a humidistat. One or more zones on an air loop may not have significant latent loads, and therefore no humidistat, or a user may not have included humidistats and set the `Type of Load to Size On = Latent` just as a verification of latent loads. For this reason, if a system serves a zone without a humidistat, that zone's humidistat setting will be assumed as 50% RH or other user specified value (see Input Description section below), where the corresponding zone humidity ratio with respect to zone supply air humidity ratio will be used to calculate latent mass flow rate (see Model Design Document section below). Changing the key choice from Latent to other key choices will effectively turn off air system latent sizing.

## Input Description ##

A new choice field will be added to Sizing:Zone to allow control of sizing calculations. In addition to the proposed key choices, an additional key choice for `Latent Load Only` could be included for completeness. A new numeric field will be added to specify the assumed zone humidity control point which would be overridden by a zone humidistat if present (i.e., humidistat has priority). These fields could be added just after the zone supply air conditions inputs or at end of object to avoid transition. Since changing the zone conditions during zone sizing (i.e., holding a zone humidity level) will change the zone sensible load and resulting zone air flow calculations, even if only slightly, it is proposed that a mechanism be used to turn this feature off completely. When latent loads are not used during sizing, 0's will be reported to the zone sizing results file `epluszsz.csv`. The `Sensible Load Only No Latent Load` field could potentially be removed after example file diffs are scrutinized and changes to existing simulations are deemed small enough to ignore.

    Sizing:Zone,
      Ax, \field Zone Load Sizing Method
          \note Specifies the basis for sizing the zone supply air flow rate.
          \note Zone latent loads will not be used during sizing only when
          \note Zone Load Sizing Method = Sensible Load Only No Latent Load.
          \note For this case the zone humidity level will float according to
          \note the fields Cooling and Heating Design Supply Air Humidity Ratio.
          \note For all other choices the zone humidity level will be controlled.
          \note Sensible Load Only will use zone sensible air flow rate for zone
          \note component sizing. Latent loads will be reported during sizing.
          \note Sensible and Latent Load will use the larger of sensible and
          \note latent air flow rate for zone component sizing. Latent loads will
          \note be reported during sizing.
          \note Sensible Load Only No Latent Load or leaving this field blank
          \note will disable zone latent sizing and reporting. Latent loads will
          \note not be reported during sizing (reported as 0's).
          \type choice
          \key Sensible Load Only
          \key Sensible And Latent Load
          \key Sensible Load Only No Latent Load
          \default Sensible Load Only No Latent Load
      Nx, \field Zone Humidistat Set Point (Schedule Name?)
          \note Enter the zone relative humidity used for zone latent sizing
          \note calculations.
          \note A zone humidistat will take priority over this input.
          \note This field is not used if Zone Load Sizing Method = Sensible Load
          \note Only No Latent Load or a zone humidistat is present.
          \type real
          \units percent
          \default 50.0

A single key choice will be added to the Sizing:System `Type of Load to Size On` field.

    Sizing:System,
     A2,  \field Type of Load to Size On
          \note Specifies the basis for sizing the system supply air flow rate.
          \note Sensible and Total use the zone sensible design air flow rates to
          \note size the system supply air flow rate. The cooling coil will then be
          \note sized at either the peak Sensible or Total flow rate and conditions.
          \note The heating coil is always sized at the peak sensible heating load.
          \note Latent uses the larger of the zone sensible and latent design air
          \note flow rates for system sizing.
          \note VentilationRequirement uses the system ventilation requirement
          \type choice
          \key Sensible
          \key Latent (new key choice)
          \key Total
          \key VentilationRequirement
          \default Sensible


## Transition Requirements ##

If new Sizing:Zone input fields are inserted within this object a transition is required to insert 2 blank fields. If these fields are added to the end of the object, no transition is required.

## Outputs Description ##

Existing reports for zone air system sensible capacity and zone sensible and latent predicted load are:

    Zone Air System Sensible Heating Energy [J]
    Zone Air System Sensible Cooling Energy [J]
    Zone Air System Sensible Heating Rate [W]
    Zone Air System Sensible Cooling Rate [W]
    Zone Predicted Sensible Load to Setpoint Heat Transfer Rate [W]
    Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate [W]
    Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate [W]
    Zone System Predicted Sensible Load to Setpoint Heat Transfer Rate [W]
    Zone System Predicted Sensible Load to Heating Setpoint Heat Transfer Rate [W]
    Zone System Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate [W]
    Zone Predicted Moisture Load Moisture Transfer Rate [kgWater/s]
    Zone Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate [kgWater/s]
    Zone Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate [kgWater/s]
    Zone System Predicted Moisture Load Moisture Transfer Rate [kgWater/s]
    Zone System Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate [kgWater/s]
    Zone System Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate [kgWater/s]
    
Proposed new latent indicating output:     

    Zone Air System Latent Heating Energy [J]
    This output variable represents the latent heating energy in Joules that is
    actually supplied by the system to that zone for the timestep reported. This
    is the sensible heating rate multiplied by the simulation timestep. This is
    calculated and reported from the Correct step in the Zone Predictor-Corrector
    module. This field is not multiplied by zone or group multipliers.
    
    Zone Air System Latent Cooling Energy [J]
    This output variable represents the latent cooling energy in Joules that is
    actually supplied by the system to that zone for the timestep reported. This
    is the sensible cooling rate multiplied by the simulation timestep. This is
    calculated and reported from the Correct step in the Zone Predictor-Corrector
    module. This field is not multiplied by zone or group multipliers.
    
    Zone Air System Latent Heatng Rate [W]
    This output variable represents the latent heating rate in Watts that is
    actually supplied by the system to that zone for the timestep reported. 
    This is calculated and reported from the Correct step in the Zone
    Predictor-Corrector module. This field is not multiplied by zone or 
    group multipliers.
    
    Zone Air System Latent Cooling Rate [W]
    This output variable represents the latent cooling rate in Watts that is
    actually supplied by the system to that zone for the timestep reported. This
    is calculated and reported from the Correct step in the Zone Predictor-Corrector
    module. This field is not multiplied by zone or group multipliers.
    
    Zone Air System Sensible Heat Ratio []
    This is the ratio of zone air system sensible heating or cooling energy or 
    rate to the total zone air system heating or cooling energy or rate
    (sensible + Latent). This is calculated and reported from the Correct step
    in the Zone Predictor-Corrector module.
    
    Zone Air Vapor Pressure Difference [Pa]
    The output variable represents the zone vapor pressure depression or
    difference (VPD) in Pacals of the zone conditions with respect to the saturated vapor pressure at the zone air temperature.
    
The addition of latent sizing calculations opens up other places where an output related to sensible calculations could include the latent equivalent. These additional outputs will not be proposed for this latent sizing effort. For example:

    Zone Heating Setpoint Not Met Time [hr]
    Zone Heating Setpoint Not Met While Occupied Time [hr]
    Zone Cooling Setpoint Not Met Time [hr]
    Zone Cooling Setpoint Not Met While Occupied Time [hr]
    Facility Heating Setpoint Not Met Time [hr]
    Facility Cooling Setpoint Not Met Time [hr]
    Facility Heating Setpoint Not Met While Occupied Time [hr]
    Facility Cooling Setpoint Not Met While Occupied Time [hr]

    Zone Air Terminal Sensible Heating Energy [J]
    Zone Air Terminal Sensible Cooling Energy [J]
    Zone Air Terminal Sensible Heating Rate [W]
    Zone Air Terminal Sensible Cooling Rate [W]
    
    Zone Thermostat Air Temperature [C]
    Zone Thermostat Heating Setpoint Temperature [C]
    Zone Thermostat Cooling Setpoint Temperature [C]

Include new zone sizing fields in `epluszsz.csv`.

Existing `epluszsz.csv` zone sizing information:

    Des Heat Load [W]          - inludes impact of DOAS simulation
    Des Sens Cool Load [W]     - inludes impact of DOAS simulation
    Des Heat Mass Flow [kg/s]
    Des Cool Mass Flow [kg/s]

Proposed `epluszsz.csv` zone sizing information:

    Des Latent Heat Load [W]   - inludes impact of DOAS simulation
    Des Latent Cool Load [W]   - inludes impact of DOAS simulation
    Des Latent Heat Mass Flow [kg/s]
    Des Latent Cool Mass Flow [kg/s]
    Des Heat Load No DOAS [W]  - original zone load prior to DOAS simulation
    Des Cool Load No DOAS [W]  - original zone load prior to DOAS simulation

## Testing/Validation/Data Sources ##

Compare a few files using Sizing:Zone Zone Load Sizing Method = Sensible Load Only and
Zone Load Sizing Method = Sensible Load Only No Latent Load to verify the magnitude of
sizing changes when humidistat control is used during sizing calculations.

Existing example file 5ZoneAirCooled will be compared with and without latent sizing and with added zone latent loads (`OtherEquipment`) to determine impact on air flow sizing. One of the modified files will be saved as an air loop latent sizing example file.

Similar tests will be performed for existing example file PackagedTerminalHeatPump and saved as a zone equipment latent sizing example.

## Input Output Reference Documentation ##

Update Sizing:Zone and Sizing:System to include new input fields in `group-design-objects`.

## Engineering Reference ##

Update sizing discussion, deign data arrays, and equations in `zone-design-loads-and-air-flow-rates` and `system-design-loads-and-air-flow-rates`.

## Example File and Transition Changes ##

Include a zone equipment and air loop equipment example files to exercise latent sizing. transition requirements are TBD.

## References ##

 - [Trane Engineering Newsletter volume 48-3](<https://www.trane.com/content/dam/Trane/Commercial/global/products-systems/education-training/engineers-newsletters/airside-design/admapn071en-082019.pdf>)
 - [Murphy, J., Selecting DOAS Equipment with Reserve Capacity, Trane Commercial Business, Ingersoll Rand](https://www.trane.tm/content/dam/Trane/Commercial/global/products-systems/education-training/industry-articles/Apr2010ASHRAE.pdf)





## Model Design Documentation ##

The following code documentation is two fold. One is to document the existing calculations  and methodology used during sizing, and the second is to provide a template for the proposed changes. This proposal suggests to duplicate existing sensible calculations with corresponding latent calculations to provide a time series representation of both sensible and latent sizing data.

**Proposed Calculation of Latent Sizing Data**

Regardless of the approach used to size zone or air system components, an accurate representation of zone sensible and latent loads are required. For this reason it is proposed to:

1) hold zone air humidity ratio at the humidistat set point during zone sizing. Otherwise the predicted latent load will include zone moisture capacitance and storage effects.
2) calculate and save latent sizing data similar to existing sensible sizing data
3) user input for sizing method will choose which time interval data is used for component sizing (i.e, sensible only, max of sensible and latent, or using exiting calculations representing sensible only with no zone humidity control).

**Existing zone sizing calculation for sensible cooling (similar for heating)**

**ZoneEquipmentManager::SizeZoneEquipment** - Zone sizing begins by the predict step in `ZoneTempPredictorCorrector` calculating zone sensible and latent loads and then simulating the DOAS system, if applicable, to determine the amount of zone load offset by the DOAS system. If the DOAS system is included in the zone sizing calculations, only the sensible portion of the DOAS supply air to the zone is accounted for in zone sizing.

    LatOutputProvided = 0.0;
    DOASCpAir = PsyCpAirFnW(DOASSupplyHumRat);
    DOASSysOutputProvided = DOASMassFlowRate *
        DOASCpAir * (DOASSupplyTemp - Node(ZoneNode).Temp);
    // DOAS simulation does not update latent cooling provided by DOAS
    UpdateSystemOutputRequired(state, ActualZoneNum, DOASSysOutputProvided, LatOutputProvided);

Next the zone cooling supply air temperature and humidity ratio from the `Sizing:Zone` object:

    Sizing:Zone,
      N1, \field Zone Cooling Design Supply Air Temperature
      N5, \field Zone Cooling Design Supply Air Humidity Ratio

are used as the known supply air condition (`Temp, HumRat`), to then calculate the temperature difference (`DeltaTemp`, which could have instead been input by the user) between the supply air temperature and zone air temperature (thermostat set point). Then calculate the `Enthalpy` of the supply air and read the zone sensible load from the predict step, `SysOutputRequired`. Using the supply air specific heat, `CpAir`, calculate the zone sensible air `MassFlowRate`. Save these data in time series arrays for further processing. Repeat for other zones (`ControlledZoneNum`) and for each design day (`CurOverallSimDay`). A final call to `UpdateSystemOutputRequired` is made which ideally should zero out the remaining load for this zone.

    Temp = CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolDesTemp;
    HumRat = CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolDesHumRat;
    DeltaTemp = Temp - Node(ZoneNode).Temp;
    Enthalpy = PsyHFnTdbW(Temp, HumRat);
    SysOutputProvided = ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputRequired;
    CpAir = PsyCpAirFnW(HumRat);
    MassFlowRate = max(SysOutputProvided / (CpAir * DeltaTemp), 0.0);

    UpdateSystemOutputRequired(state, ActualZoneNum, SysOutputProvided, LatOutputProvided);

These data are placed on the zone supply node for use in the correct step of `ZoneTempPredictorCorrector` to calulate the resulting zone temperature and humidity ratio. In current EnergyPlus calculations, the zone temperature is maintained at the thermostat set point temperature while the zone humidity ratio floats according to the humidity ratio entering the zone at the zone supply node **sensible** mass flow rate. It is not important at this time to actually provide the zone the correct mass flow rate, what is important here is to meet the zone sensible load exactly such that the zone temperature remains at the thermostat set point.

    Node(SupplyAirNode).Temp = Temp;
    Node(SupplyAirNode).HumRat = HumRat;
    Node(SupplyAirNode).Enthalpy = Enthalpy;
    Node(SupplyAirNode).MassFlowRate = MassFlowRate;

Finally, save the time series data. The example shown here is when there is a cooling load. This is actually incorrect since the air loop may not peak at the same time as the zone and the documentation for zone conditions for both heating and cooling need to be known for each time step (e.g., should not set `HeatZoneTemp = 0`). This data saving methodoloty will be revised during this effort (i.e., HeatZoneTemp and CoolZoneTemp will always be saved regardless of load).

    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatLoad = SysOutputProvided;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatMassFlow = MassFlowRate;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatZoneTemp = Node(ZoneNode).Temp;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatZoneHumRat = Node(ZoneNode).HumRat;
    // why do we set these to 0 in E+, doesn't the allocation initialization do that?
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolLoad = 0.0;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolMassFlow = 0.0;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolZoneTemp = 0.0;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolZoneHumRat = 0.0;


    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatOutTemp = OutDryBulbTemp;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatOutHumRat = OutHumRat;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolOutTemp = OutDryBulbTemp;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolOutHumRat = OutHumRat;

**Proposed zone sizing calculations for latent cooling (similar for heating)**

**Step 1:** Save zone sensible and latent loads prior to DAOS simulation.

    // save raw zone loads without impact of outdoor air
    LatOutputProvidedNoDOAS = ZoneSysMoistureDemand(ZoneNum).RemainingOutputRequired;
    SysOutputProvidedNoDOAS = ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;

**Step 2:** Simulate DOAS system, if applicable according the the `Sizing:Zone` object, and update remaining zone loads

    Sizing:Zone,
      A8, \field Account for Dedicated Outdoor Air System
      \key Yes
      \key No
      \default No


    DOASLatOutputProvided = 0.0;
    DOASCpAir = PsyCpAirFnW(DOASSupplyHumRat);
    DOASSysOutputProvided = DOASMassFlowRate * DOASCpAir *
                           (DOASSupplyTemp - Node(ZoneNode).Temp);
    // example if test to turn off latent calcs (note: this if test may not be needed)
    if (zoneRHSizing) { // new latent sizing code
        DOASLatOutputProvided = DOASMassFlowRate * 
        (DOASSupplyHumRat - Node(ZoneNode).HumRat);
    }

    UpdateSystemOutputRequired(state, ActualZoneNum, DOASSysOutputProvided, DOASLatOutputProvided);
    
**Step 3:** Repeate zone sensible calulations for zone sensible mass flow rate (not shown, same as existing code above) and add new calculations for zone latent load and mass flow rate. Note: some of the variables (e.g., T, w, RH) shown in this example will be used for debugging purposes.

        Real64 ZoneTemp = Node(ZoneNode).Temp;
        Real64 ZoneHumRat = Node(ZoneNode).HumRat;
        Real64 ZoneRH = PsyRhFnTdbWPb(state, ZoneTemp, ZoneHumRat, OutBaroPress);
        Real64 H2OHtOfVap = PsyHgAirFnWTdb(Node(ZoneNode).HumRat, Node(ZoneNode).Temp);
        Real64 LatentAirMassFlow = 0.0;
        Real64 MoistureLoad = 0.0;
        if (zoneRHSizing) {
            // positive MoistureLoad means humidification load
            LatOutputProvided = ZoneSysMoistureDemand(ZoneNum).RemainingOutputRequired;
            MoistureLoad = LatOutputProvided * H2OHtOfVap;
            Real64 DeltaHumRat = 0.0;
            if (LatOutputProvided < 0.0) {
                DeltaHumRat = -0.005;
            } else if (LatOutputProvided > 0.0) {
                DeltaHumRat = 0.005;
            }
            EnthalpyDiff = H2OHtOfVap * DeltaHumRat;
            if (std::abs(EnthalpyDiff) > DataHVACGlobals::VerySmallMassFlow) 
                LatentAirMassFlow = MoistureLoad / EnthalpyDiff;
        }

**Step 4:** Now that zone sensible and latent mass flow rate is known, and likely different, correct the zone supply air node entering conditions to meet both the thermostat and humidistat set points. Make sure that the correct mass flow rates are saved prior to making any adjustment to zone supply node mass flow rate.

    if (zoneRHSizing) {
        if (MassFlowRate > 0.0) {
            HumRat = ZoneHumRat + LatOutputProvidedNoDOAS / MassFlowRate;
           // need to recalculate SA Temp based on new Cp?
            Enthalpy = PsyHFnTdbW(Temp, HumRat);
        } else {  // sensible zone mass flow rate is 0
            // if there is no sensible load then still need to hold zone RH at set point
            // could look at LatOutputProvided and just used some temporary mass flow rate?
            if (LatentAirMassFlow > DataHVACGlobals::VerySmallMassFlow) {
                // no need to recalculate T, Sensible load = 0 so T = Tzone
                HumRat = ZoneHumRat + LatOutputProvidedNoDOAS / LatentAirMassFlow;
                MassFlowRate = LatentAirMassFlow;
                Enthalpy = PsyHFnTdbW(Temp, HumRat);
            } else {
                LatentAirMassFlow = 0.0;
            }
        }
    }
    
**Step 5:** The final call to update `UpdateSystemOutputRequired` should 0 out any remaining sensbile and latent loads.

    UpdateSystemOutputRequired(state, ActualZoneNum, SysOutputProvided, LatOutputProvided);

Just as is done with existing zone sizing results, the data calculated here must be saved in time interval arrays for post processing.

**Proposed zone sizing variables used for post processing:**

The following section describes the new struct variables required to implement the latent sizing new feature. These new variables will be added to the code at locations similar to those used for sensible data processing.

Existing Struct variables tracking zone sizing information. Zone temperature and humidity ratio are also tracked but not shown in the example.

    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolLoad
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolMassFlow
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatLoad
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatMassFlow

New Struct variables tracking zone sizing information shown as example code in `ZoneEquipmentManager::SizeZoneEquipment`.

    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).ZoneHeatLatentLoad = MoistureLoad;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).ZoneHeatLatentMassFlow = LatentAirMassFlow;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).ZoneCoolLatentLoad = 0.0;
    CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).ZoneCoolLatentMassFlow = 0.0;

    if (SysOutputProvidedNoDOAS > 0.0) {
        CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolLoadNoDOAS = 0.0;
        CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatLoadNoDOAS = SysOutputProvidedNoDOAS;
    } else if (SysOutputProvidedNoDOAS < 0.0) {
        CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolLoadNoDOAS = -SysOutputProvidedNoDOAS;
        CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatLoadNoDOAS = 0.0;
    } else {
        CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).CoolLoadNoDOAS = 0.0;
        CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).HeatLoadNoDOAS = 0.0;
    }
    LatOutputProvidedNoDOAS *= H2OHtOfVap;
    if (zoneRHSizing) {
        if (LatOutputProvidedNoDOAS > 0.0) {
            CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).ZoneCoolLatentLoadNoDOAS = 0.0;
            CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).ZoneHeatLatentLoadNoDOAS =                     LatOutputProvidedNoDOAS;
        } else if (LatOutputProvidedNoDOAS < 0.0) {
            CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).ZoneCoolLatentLoadNoDOAS = -LatOutputProvidedNoDOAS;
            CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).ZoneHeatLatentLoadNoDOAS = 0.0;
        }
    } else {
        CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).ZoneCoolLatentLoadNoDOAS = 0.0;
        CalcZoneSizing(CurOverallSimDay, ControlledZoneNum).ZoneHeatLatentLoadNoDOAS = 0.0;
    }

**During Day Sizing Data Processing**

For each time interval the sizing data is stored (cooling example shown)

    CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeq(TimeStepInDay) +=
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolMassFlow * FracTimeStepZone;
    CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolLoadSeq(TimeStepInDay) +=
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolLoad * FracTimeStepZone;
    CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq(TimeStepInDay) +=
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneTemp * FracTimeStepZone;
    CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolOutTempSeq(TimeStepInDay) +=
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolOutTemp * FracTimeStepZone;
    CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq(TimeStepInDay) +=
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneRetTemp * FracTimeStepZone;
    CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneHumRatSeq(TimeStepInDay) +=
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneHumRat * FracTimeStepZone;
    CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolOutHumRatSeq(TimeStepInDay) +=
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolOutHumRat * FracTimeStepZone;

**End Day Sizing Data Processing**

The time interval data is averaged based on the `<Sizing:Parameters>` averaging window. Other sizing data also averages the time interval data: CoolLoadSeq, CoolZoneRetTempSeq. Moisture variables are not included in this averaging step and will likely be necessary (e.g., CoolZoneRetHumRatSeq and new latent load variables).

    Sizing:Parameters,
      N3; \field Timesteps in Averaging Window
    

    // average some of the zone sequences to reduce peakiness
    for (CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum) {
        if (!ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
        state.dataZoneEquipmentManager->AvgData = 0.0;
        MovingAvg(CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeq,
                  NumOfTimeStepInDay, NumTimeStepsInAvg,
                  state.dataZoneEquipmentManager->AvgData);
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeq = state.dataZoneEquipmentManager->AvgData;
            }

For each time interval the cooling load is compared to a scalar DesCoolLoad (initializes to 0) and the scalar is incremented to identify the peak cooling load and all associated sizing data with that time interval. The time interval of the peak load is also saved for reporting.

    for (TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex) {
        if (CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolLoadSeq(TimeStepIndex) >
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolLoad) {
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolLoad =
                CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolLoadSeq(TimeStepIndex);
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolMassFlow =
                CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeq(TimeStepIndex);
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak =
                CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq(TimeStepIndex);
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).OutTempAtCoolPeak =
                CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolOutTempSeq(TimeStepIndex);
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtCoolPeak =
                CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq(TimeStepIndex);
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtCoolPeak =
                CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneHumRatSeq(TimeStepIndex);
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).OutHumRatAtCoolPeak =
                CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolOutHumRatSeq(TimeStepIndex);

            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).TimeStepNumAtCoolMax = TimeStepIndex;
        }
    }
    
Once the peak is known, the peak load air mass flow rate is converted to volume and the coil inlet temperature and humdiity ratio are calculated.

    if (CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolMassFlow > 0.0) {
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolVolFlow =
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolMassFlow /
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolDens;
        OAFrac = CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).MinOA /
                 max(CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolVolFlow, SmallMassFlow);
        OAFrac = min(1.0, max(0.0, OAFrac));
        TimeStepAtPeak = CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).TimeStepNumAtCoolMax;
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolCoilInTemp =
            OAFrac * DesDayWeath(CurOverallSimDay).Temp(TimeStepAtPeak) +
            (1.0 - OAFrac) * CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak;
        CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolCoilInHumRat =
            OAFrac * DesDayWeath(CurOverallSimDay).HumRat(TimeStepAtPeak) +
            (1.0 - OAFrac) * CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneHumRatAtCoolPeak;
    }

I am not quite following this final step of the End Day calc. If the design day  (`CalcZoneSizing`) DesignCoolVolFlow is greater than the `CalcFinalZoneSizing` scalar DesCoolVolFlow (initializes to 0), then save design data to the CalcFinalZoneSizing array. Else, if the design day DesCoolLoad is greater than the CalcFinalZoneSizing scalar DesCoolLoad (this would indicate pull-up or pull-down based on a change in supply air delta T?), then save design data to the CalcFinalZoneSizing array. This implies the zone air temperature varies during sizing (since flow is proportional to load for a fixed supply air and zone temperature, and supply air temperature does not change) where is certainly possible that zone temperature changes with thermostat setup and setback and the determination of "peak condition" could be based either on air flow or load. I am not sure at this point why a peak load calculation would allow either indicator to set the peak. Although uncertain of the implications, the proposed latent sizing calculations will mirror the existing sensible sizing calculations.

    // from all the design periods, choose the one needing the most Cooling and save
    // all its design variables in CalcFinalZoneSizing
    if (CalcZoneSizing(state.dataSize->CurOverallSimDay, CtrlZoneNum).DesCoolVolFlow >
        CalcFinalZoneSizing(CtrlZoneNum).DesCoolVolFlow) {
        CalcFinalZoneSizing(CtrlZoneNum).DesCoolVolFlow =
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolVolFlow;
        CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad =
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolLoad;
        CalcFinalZoneSizing(CtrlZoneNum).DesCoolMassFlow =
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolMassFlow;
        CalcFinalZoneSizing(CtrlZoneNum).CoolDesDay =
            CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolDesDay;
        // other design data removed for brevity
    } else {
        CalcFinalZoneSizing(CtrlZoneNum).DesCoolDens = state.dataEnvrn->StdRhoAir;
        // save design cooling load when the there is design cooling load and the
        // design cooling volume flow rate is zero, i.e., when
        // design cooling volume flow rate is set to zero due to cooling supply air
        // temp greater than zone thermostat temperature
        if (CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolLoad >
            CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad) {
            CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad =
                CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolLoad;
            CalcFinalZoneSizing(CtrlZoneNum).CoolDesDay =
                CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolDesDay;
            CalcFinalZoneSizing(CtrlZoneNum).CoolLoadSeq =
                CalcZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolLoadSeq;
        // other design data removed for brevity
    }

**EndZoneSizingCalc Sizing Data Processing**

    // candidate EMS calling point to customize CalcFinalZoneSizing
    bool anyEMSRan;
    ManageEMS(state, EMSManager::EMSCallFrom::ZoneSizing, anyEMSRan, ObjexxFCL::Optional_int_const());

    // now apply EMS overrides (if any)
    if (AnyEnergyManagementSystemInModel) {
        for (CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum) {
            if (CalcFinalZoneSizing(CtrlZoneNum).EMSOverrideDesHeatMassOn) {
                if (CalcFinalZoneSizing(CtrlZoneNum).DesHeatMassFlow > 0.0)
                    CalcFinalZoneSizing(CtrlZoneNum).DesHeatMassFlow =
                        CalcFinalZoneSizing(CtrlZoneNum).EMSValueDesHeatMassFlow;
            }
            // and other EMS overrides
        }
    }
    
And finally, check for zero zone loads for reporting purposes and then report zone sizing results to the `epluszsz.csv` file.

    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
        if (!ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
        if (std::abs(CalcFinalZoneSizing(CtrlZoneNum).DesCoolLoad) <= 1.e-8) {
            ShowWarningError(state, "Calculated design cooling load for zone=" + CalcFinalZoneSizing(CtrlZoneNum).ZoneName + " is zero.");
            ShowContinueError(state, "Check Sizing:Zone and ZoneControl:Thermostat inputs.");
        }
        if (std::abs(CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad) <= 1.e-8) {
            ShowWarningError(state, "Calculated design heating load for zone=" + CalcFinalZoneSizing(CtrlZoneNum).ZoneName + " is zero.");
            ShowContinueError(state, "Check Sizing:Zone and ZoneControl:Thermostat inputs.");
        }
    }


    print(state.files.zsz, "Time");
    // 4 per zone existing, versus 10 per zone proposed

**Application of Latent Sizing to Component Sizing**

Component sizing is currently based on a calculation of zone air mass flow rate, a predicted zone sensible load, and a zone thermostat and supply air temperature. The peak air mass flow rate can meet the peak zone sensible load at the user specified supply air temperature. Mdot, sens = Qsens / (Cp * (Tsupply - Tzone)).

Latent sizing would be based on this same principle of calculating a zone air mass flow rate to meet the peak latent load. The final zone design air mass flow rate will either be selected, based on user input, as the peak sensible air mass flow (sensible only sizing) or the maximum of the sensible and latent air mass flow (sensible and latent sizing). Mdot, lat = (Qlat * H2OHtOfVap) / (H2OHtOfVap * (w, supply - w, zone)). H2OHtOfVap will cancel out here so will check that during implementation.

The zone latent load is independent of zone sensible load, save any dependence of moist air properties (specific heat) on resultant load calculations. I assume zone sensible loads will change slightly if zone humidity ratio changes (i.e., will now be held constant) during sizing. I intend to prove that assumption, and that the changes are small.

The proposed result of zone sizing is to record both a zone sensible mass flow rate and sensible load, and a latent mass flow rate and latent load. These time series data are the basis for zone and air system component sizing. With the addition of zone latent load sizing calculations other components should be more accurately sized to meet the imposed loads (e.g., DOAS systems or zone/air system cooling coils)

