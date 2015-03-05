# High Level HVAC Calling Tree (schematic – not all routines are shown)

ManageHVAC (in HVACManager)

ZoneAirUpdate('PREDICT', . . .) (in HVACManager)

*estimate the zone heating or cooling demand*

*

SimHVAC (in HVACManager)

ManageSetPoints (in SetPointManager)

SimSelectedEquipment (in HVACManager)

ManageAirLoops (in SimAirServingZones)

ManageZoneEquipment (in ZoneEquipmentManager)

ManageElectricLoadCenters (in ElectricPowerManager)

ManagePlantLoops (in PlantManager)

ZoneAirUpdate('CORRECT', . . .) (in HVACManager)

From the amount of heating and cooling actually provided by the HVAC system, calculate the zone temperatures.

Each of the "Manage" routines has a different structure, since the simulation to be performed is different in each case.  We will show schematic calling trees for several of the "Manage" routines.