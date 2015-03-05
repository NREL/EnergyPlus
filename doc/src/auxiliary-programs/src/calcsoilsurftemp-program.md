# CalcSoilSurfTemp Program

The CalcSoilSurfTemp program calculates three important parameters for the simulation of the earth tube:

- the annual average soil surface temperature
- the amplitude of soil surface temperature
- the phase constant of soil surface temperature

Since soil temperature is one of the most significant factors affecting the overall performance of earth tube system, soil temperature around the earth tube should be accurately predicted.

Before the soil temperature around earth tube can be calculated during the running period of earth tube model in EnergyPlus, the ground surface temperature straight above earth tube should be predicted. Using CalcSoilSurfTemp program, these parameters are predicted by considering the convective heat transfer between the air and ground, solar radiation absorption by the ground, long-wave radiation emitted from the soil, and latent heat loss due to the moisture evaporation at the ground surface.