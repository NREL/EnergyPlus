Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: Fuel type synonyms
For all Boiler:\*, WaterHeater:\* , Generator:\* ,
OtherEquipment, Exterior:FuelEquipment, HVACTemplate:System:VRF, HVACTemplate:Plant:Boiler, ZoneHVAC:HybridUnitaryHVAC
Coil:Cooling:DX:MultiSpeed, Coil:Heating:Fuel, Coil:Heating:DX:MultiSpeed, AirConditioner:VariableRefrigerantFlow, Chiller:EngineDriven, Chiller:CombustionTurbine, 
ChillerHeater:Absorption:DirectFired, EnergyManagementSystem:MeteredOutputVariable, LifeCycleCost:UsePriceEscalation, LifeCycleCost:UseAdjustment, 
Meter:Custom, Meter:CustomDecrement, FuelFactors and other objects with a fuel type or resource type field

Field: Fuel Type (or similar)

a) change "Gas" or "Natural Gas" to "NaturalGas"

b) change "Electric" or "Elec" to "Electricity"

c) change "FuelOil#1" or "Fuel Oil #1" or "Fuel Oil" or "Distillate Oil" to "FuelOil1"

d) change "FuelOil#2" or "Fuel Oil #2" or "Residual Oil" to "FuelOil2"

e) change "PropaneGas" or "LPG" or "Propane Gas" to "Propane"

# Object Change: GlobalGeometryRules
Field 1, Starting Vertex Position:

a) change "ULC" to "UpperLeftCorner"

b) change "LLC" to "LowerLeftCorner"

c) change "LRC" to "LowerRightCorner"

d) change "URC" to "UpperRightCorner"

Field 2, Vertex Entry Direction

a) change "CCW" to "Counterclockwise"

b) change "CW" to "Clockwise"

Field 3, Coordinate System
Field 4, Daylighting Reference Point Coordinate System
and Field 5, Rectangular Surface Coordinate System

a) change "WCS" or "WorldCoordinateSystem" or "Absolute" to "World"

b) change "Rel\*" or "Relative\*" or "Local" to "Relative"

# Object change: Output:Table:SummaryReports
Any field F1:FN

a) change "ABUPS" or "BEPS" to "AnnualBuildingUtilityPerformanceSummary"

b) change "IVRS" to "InputVerificationandResultsSummary"

c) change "CSS" to "ComponentSizingSummary"

d) change "SHAD" to "SurfaceShadowingSummary"

e) change "EIO" to "InitializationSummary"
