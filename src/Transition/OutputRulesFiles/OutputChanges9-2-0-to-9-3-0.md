Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Additional field for Coil Sizing Details:Coils report

A new column "Peak Load Type to Size On" has been added to the `Coil Sizing Details:Coils` report

See [7397](https://github.com/NREL/EnergyPlus/pull/7397)

### Water to Water Air-Source EIR Heat Pump
The HeatPump:WaterToWater:EIR:Heating and HeatPump:WaterToWater:EIR:Cooling objects have been renamed to HeatPump:PlantLoop:EIR:Heating and HeatPump:PlantLoop:EIR:Cooling, and the respective output variables have been renamed from: 

*Before*
```
Water to Water Heat Pump Load Side Heat Transfer Rate
Water to Water Heat Pump Load Side Heat Transfer Energy
Water to Water Heat Pump Source Side Heat Transfer Rate
Water to Water Heat Pump Source Side Heat Transfer Energy
Water to Water Heat Pump Load Side Inlet Temperature
Water to Water Heat Pump Load Side Outlet Temperature
Water to Water Heat Pump Source Side Inlet Temperature
Water to Water Heat Pump Source Side Outlet Temperature
Water to Water Heat Pump Electric Power
Water to Water Heat Pump Electric Energy
Water to Water Heat Pump Load Side Mass Flow Rate
Water to Water Heat Pump Source Side Mass Flow Rate
```

*9.3.0*
```
Plant Loop Heat Pump Load Side Heat Transfer Rate
Plant Loop Heat Pump Load Side Heat Transfer Energy
Plant Loop Heat Pump Source Side Heat Transfer Rate
Plant Loop Heat Pump Source Side Heat Transfer Energy
Plant Loop Heat Pump Load Side Inlet Temperature
Plant Loop Heat Pump Load Side Outlet Temperature
Plant Loop Heat Pump Source Side Inlet Temperature
Plant Loop Heat Pump Source Side Outlet Temperature
Plant Loop Heat Pump Electric Power
Plant Loop Heat Pump Electric Energy
Plant Loop Heat Pump Load Side Mass Flow Rate
Plant Loop Heat Pump Source Side Mass Flow Rate
```

See [7489](https://github.com/NREL/EnergyPlus/pull/7489/)
