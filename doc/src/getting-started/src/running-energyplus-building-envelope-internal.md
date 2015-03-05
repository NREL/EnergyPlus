# Running EnergyPlus, Building Envelope, Internal Loads, Reports

## Overview

- Rectangular single story building
- Windows in east and west walls
- Single zone with no interior partitions
- Lightweight construction

![Schematic for Exercise 1](media/schematic-for-exercise-1.jpeg)


The details of the building construction and operation are shown in the following tables and description. For tutorial purposes, the building is located in Chicago Illinois, one of the weather files supplied with EnergyPlus. These details are listed in a fashion to make for easy entry into EnergyPlus.

## Details of the exercise

### Surface Constructions

**Material**  (listed from outside to inside)|**Conductivity**(W/m-K)|**Thickness**(m)|**U**(W/m^2^-K)|**R**(m^2^-K/W)|**Density**(kg/m^3^)|**C~p~**(J/kg-K)
----------------------------------------------------------|------------------------------------|-----------------------------|----------------------------|----------------------------|---------------------------------|-----------------------------
*Walls*||||||
WOOD SIDING-1|0.140|0.009|15.556|0.064|530|900
FIBERGLASS QUILT-1|0.040|0.066|0.606|1.650|12|840
PLASTERBOARD-1|0.160|0.012|13.333|0.075|950|840
*Roof*||||||
ROOF DECK|0.140|0.019|7.368|0.136|530|900
FIBERGLASS QUILT-2|0.040|0.066|0.606|1.650|12|840
PLASTERBOARD-2|0.160|0.010|1.60|0.625|950|840
*Floor*||||||
C5 CONCRETE|1.73|0.1015|17.04|0.059|2243|837

### Window Properties

Type \*
Clear

Number of panes
2

Pane thickness
0.006 m

Air-gap thickness
0.0032 m

Conductivity of glass
0.9 W/m-K

Refers to specific glass type included in the EnergyPlus datasets directory

 (**WindowGlassMaterials.idf**)

### Internal Loads

Lights:  1000W, Office Lighting schedule, surface mount fluorescent

### Space Conditioning

Heating setpoint 20C, cooling setpoint 24C, no setback

### Environment

Location:Chicago, Illinois, USA

Design Days:Summer

Winter

Annual Simulation Period:Jan 1 – Dec 31

Ground Temperatures:18.2 C to 22.5 C (from Slab preprocessor, vary monthly)