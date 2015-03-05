# Unitary System and VAV using HVACTemplate Inputs

## Overview

- Rectangular single story building with 5 occupied zones and a ceiling plenum
- Packaged DX cooling with gas heat serving one zone
- VAV with reheat and return plenum serving the other 4 zones
- All equipment autosized using summer and winter design days

![Schematic for Exercise 2.](media/schematic-for-exercise-2..png)


## Details of the Exercise

### Building Description

- Single floor rectangular building 30.5 m (100 ft) by 15.2 m (50 ft) by 3m (10 ft) high. 
- Building is oriented with the long axis running east-west. 
- Floor Area 463.6 m2 (5000 ft2).
- 5 occupied zones - 4 exterior, 1 interior, zone height 2.4 m (8 ft). Exterior zone depth is 3.7 m (12 ft). 
- 1 plenum zone 0.6 m (2 ft) high. 
- Windows on all 4 facades
- South and north facades have glass doors. 
- South facing glass is shaded by overhangs. 
- Walls are wood shingle over plywood, insulation, and gypsum board. 
- Roof is gravel built up roof with mineral board insulation and plywood sheathing.
- Floor slab is 0.1 m (4 in) heavy concrete. 
- Windows and glass doors are double pane Low-e clear glass with argon gap. 
- Window to wall ratio is approximately 0.3. 
- Lighting is 16 W/m2 (1.5 W/ft2).
- Office electric equipment is 10.8 W/m2 (1.0 W/ft2). 
- 1 occupant per 9.3 m2 (100 ft2) of floor area. 
- Infiltration is 0.25 air changes per hour (always on, proportional to wind speed).
- \* Refers to specific glass type included in the EnergyPlus datasets directory
- (WindowGlassMaterials.idf) 

### Space Conditioning

Heating setpoints:21.1C (70F) occupied, 12.8C (55F) unoccupied

Cooling setpoints:23.9C (75F) occupied, 40.0C (104F, system off) unoccupied

Plenum zone not controlled

### Environment

Location:Chicago, Illinois, USA

Design Days:Summer

Winter

Annual Simulation Period:Jan 1 – Dec 31

Ground Temperatures:from Slab preprocessor (20.4 to 23.0 C)