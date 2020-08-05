Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: ‘Construction:InternalSource’

Summary: A new field was added into the middle of the object, everything else states the same (or shifted back if occurring after the new field).

Fields 1-5 remain the same.

NEW Field F6 (N5): Two-Dimensional Position of Interior Temperature Calculation Request.  Leave blank or assign a zero value (default).

Old Fields 6-15 remain the same, just shifted back one field to Fields 7-16.

# Object Change: 'ZoneHVAC:HybridUnitaryHVAC'

Fields 1-14 remain the same.

Insert field 15, Fan Heat Included in Lookup Tables.

Insert field 16, Fan Heat Gain Location.

Insert field 17, Fan Heat Gain In Airstream Fraction.

Field 18, previous field 15. No other changes.

Field 19, previous field 17. No other changes.

Remaining fields 20 onward are the same as previous fields 18 onwards. No other changes.

# Object Change: Output:DebuggingData

This object uses to accept numeric entries, where 1 = Yes and all others = No. This was changed to a choice type, accepting 'Yes' or 'No', and defaulting to 'No'.

cf #7740.

# Object Change: Output:Diagnostics

This object was made unique, and with an extensible "Key" field to add specific diagnostics.

cf #7742.

# Object Change: ‘ZoneHVAC:LowTemperatureRadiant:VariableFlow’

Summary: A new field was added into the middle of the object, everything else states the same (or shifted back if occurring after the new field).

Fields 1-7 remain the same.

NEW Field F8 (A6): Setpoint Control Type.  Leave blank or assign a value of “HalfFlowPower” (default).

Old Fields 8-29 remain the same, just shifted back one field to Fields 9-30.

# Object Change: ‘ZoneHVAC:LowTemperatureRadiant:Electric’

Summary: A new field was added into the middle of the object, everything else states the same (or shifted back if occurring after the new field).

Fields 1-9 remain the same.

NEW Field F10 (A7): Setpoint Control Type.  Leave blank or assign a value of “HalfFlowPower” (default).

Old Fields 10-11 remain the same, just shifted back one field to Fields 11-12.

# Object Change: ‘ZoneHVAC:LowTemperatureRadiant:ConstantFlow’

Summary: A new field was added into the middle of the object, everything else states the same (or shifted back if occurring after the new field).

Fields 1-7 remain the same.

NEW Field F8 (N3): Running Mean Outdoor Air Temperature Weighting Factor.  Leave blank or assign a value of “0.8” (default).

Old Fields 8-29 remain the same, just shifted back one field to Fields 9-30.

# Minor Changes:

## ZoneProperty:UserViewFactors:BySurfaceName,

The case was changed from `ZoneProperty:UserViewFactors:bySurfaceName` to `ZoneProperty:UserViewFactors:BySurfaceName` to match conventions.
This will have no effect on EnergyPlus which handles this in a case-insensitive manner.

cf #7499.

## Curve:DoubleExponentialDecay,

Two typos were corrected in field names.

9.3.0:

```
  N4 , \field Coefficient3 C4
  N5 , \field Coefficient3 C5
```

9.4.0:

```
  N4 , \field Coefficient4 C4
  N5 , \field Coefficient5 C5
```

# Object Change: `PerformancePrecisionTradeoffs'

Fields 1-2 (A1, A2) remain the same.

Change in Field 3 (A3): two more key values---Mode06 and Mode07---are added.

Field 4 (N1) remains the same; however it is no longer the last field. See the newly added Field 5 (N2) below.

Newly added Field 5 (N2): MaxAllowedDelTemp, supplied with minimu, maximum, and default values.

# Object Change: `LifeCycleCost:UsePriceEscalation`

The first field was changed from `LCC Price Escalation Name` to `Name` to conform with common conventions.
Unicity of the Name is now enforced by the InputProcessor as a result.

# epJSON extensible fields expanded

The following objects will now have an `extension` defined, so will expect an array:

```
    'MaterialProperty:PhaseChange': 'temperature_enthalpies',
    'MaterialProperty:VariableThermalConductivity': 'temperature_conductivities',
    'MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm': 'isotherm_coordinates',
    'MaterialProperty:HeatAndMoistureTransfer:Suction': 'suction_points',
    'MaterialProperty:HeatAndMoistureTransfer:Redistribution': 'redistribution_points',
    'MaterialProperty:HeatAndMoistureTransfer:Diffusion': 'data_pairs',
    'MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity': 'thermal_coordinates',
    'FenestrationSurface:Detailed': 'vertices',
    'RoomAir:Node': 'surfaces',
    'People': 'thermal_comfort_models',
    'ComfortViewFactorAngles': 'anglefactors',
    'AirflowNetwork:MultiZone:WindPressureCoefficientArray': 'wind_directions',
    'AirflowNetwork:MultiZone:WindPressureCoefficientValues': 'wind_pressure_coefficients',
    'ZoneControl:Thermostat:ThermalComfort': 'thermal_comfort_controls',
    'ZoneHVAC:OutdoorAirUnit:EquipmentList': 'equipment',
    'Coil:Cooling:DX:CurveFit:OperatingMode': 'speeds',
    'Coil:Cooling:DX:MultiSpeed': 'speed_data',
    'Coil:Heating:Gas:MultiStage': 'stage_data',
    'Coil:Cooling:DX:VariableSpeed': 'speed_data',
    'Coil:Heating:DX:VariableSpeed': 'speed_data',
    'Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit': 'speed_data',
    'Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit': 'speed_data',
    'Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed': 'speed_data',
    'AirLoopHVAC:ControllerList': 'controllers',
    'AirLoopHVAC:OutdoorAirSystem:EquipmentList': 'equipment',
    'ConnectorList': 'connectors',
    'CentralHeatPumpSystem': 'modules',
    'PlantEquipmentOperation:CoolingLoad': 'ranges',
    'PlantEquipmentOperation:HeatingLoad': 'ranges',
    'PlantEquipmentOperation:OutdoorDryBulb': 'ranges',
    'PlantEquipmentOperation:OutdoorWetBulb': 'ranges',
    'PlantEquipmentOperation:OutdoorRelativeHumidity': 'ranges',
    'PlantEquipmentOperation:OutdoorDewpoint': 'ranges',
    'PlantEquipmentOperation:OutdoorDryBulbDifference': 'ranges',
    'PlantEquipmentOperation:OutdoorWetBulbDifference': 'ranges',
    'PlantEquipmentOperation:OutdoorDewpointDifference': 'ranges',
    'PlantEquipmentOperation:ComponentSetpoint': 'equipment_controls',
    'CondenserEquipmentOperationSchemes': 'control_schemes',
    'PlantEquipmentOperationSchemes': 'control_schemes',
    'PlantEquipmentOperation:UserDefined': 'equipment',
    'Generator:FuelSupply': 'constituents',
    'FluidProperties:Temperatures': 'temperatures',
    'FluidProperties:Saturated': 'properties',
    'FluidProperties:Superheated': 'properties',
    'FluidProperties:Concentration': 'properties',
    'UtilityCost:Charge:Block': 'blocks',
    'UtilityCost:Computation': 'compute_steps',
    'OutputControl:SurfaceColorScheme': 'drawing_elements',
```

A `minItems` / `maxItems` property was added for these objects as appropriate, to put a lower/upper bound on the number of extensible groups accepted.

For example, `FenestrationSurface:Detailed`:

This object is similar to `BuildingSurface:Detailed` (which was already extensible) and other `XXX:Detailed` surface objects, in the sense that it accepts (X,Y,Z) vertex groups.
But, this object only accepts either 3 or 4 vertices.

Before you would define it in epJSON as:

```
    "FenestrationSurface:Detailed": {
        "Perimeter_bot_ZN_1_Wall_South_Window": {
            "building_surface_name": "Perimeter_bot_ZN_1_Wall_South",
            "construction_name": "Window Non-res Fixed",
            "multiplier": 1.0,
            "number_of_vertices": 4,
            "surface_type": "Window",
            "vertex_1_x_coordinate": 0.0,
            "vertex_1_y_coordinate": 0.0,
            "vertex_1_z_coordinate": 2.3293,
            "vertex_2_x_coordinate": 0.0,
            "vertex_2_y_coordinate": 0.0,
            "vertex_2_z_coordinate": 1.0213,
            "vertex_3_x_coordinate": 10.0,
            "vertex_3_y_coordinate": 0.2738,
            "vertex_3_z_coordinate": 0.0,
            "view_factor_to_ground": "Autocalculate"
        },
```

Now in 9.4.0 you will define it as:

```
    "FenestrationSurface:Detailed": {
        "Perimeter_bot_ZN_1_Wall_South_Window": {
            "building_surface_name": "Perimeter_bot_ZN_1_Wall_South",
            "construction_name": "Window Non-res Fixed",
            "multiplier": 1.0,
            "number_of_vertices": 4,
            "surface_type": "Window",
            "vertices" : [
               {
                    "vertex_x_coordinate": 0.0,
                    "vertex_y_coordinate": 0.0,
                    "vertex_z_coordinate": 2.3293
                },
                {
                    "vertex_x_coordinate": 0.0,
                    "vertex_y_coordinate": 0.0,
                    "vertex_z_coordinate": 1.0213
                },
                {
                    "vertex_x_coordinate": 10.0,
                    "vertex_y_coordinate": 0.2738,
                    "vertex_z_coordinate":  0.0
                },
             ],
             "view_factor_to_ground": "Autocalculate"
        },
```


