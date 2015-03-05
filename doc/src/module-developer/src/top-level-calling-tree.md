# Top Level Calling Tree

EnergyPlus

ProcessInput (in InputProcessor)

ManageSimulation (in SimulationManager)

ManageWeather (in WeatherManager)

ManageHeatBalance (in HeatBalanceManager)

ManageSurfaceHeatBalance (in HeatBalanceSurfaceManager)

ManageAirHeatBalance (in HeatBalanceAirManager)

CalcHeatBalanceAir (in HeatBalanceAirManager)

ManageHVAC (in HVACManager)

The HVAC part of EnergyPlus is divided into a number of simulation blocks.  At this point, there are blocks for the air system, the zone equipment, the plant equipment, and the on-site electrical equipment.  There will be simulation blocks for waste heat supply and usage as well as electricity and gas.  Within each HVAC time step, the blocks are simulated repeatedly until the conditions on each side of each block interface match up.  The following calling tree represents the high level HVAC simulation structure.  It is schematic – not all routines are shown.