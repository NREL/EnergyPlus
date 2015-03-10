# Ground Heat Transfer in EnergyPlus

# Caution

For ground-contact surfaces in EnergyPlus, it is important to specify appropriate ground temperatures. Do not use the "undisturbed" ground temperatures from the weather data. These values are too extreme for the soil under a conditioned building. For best results, use the Slab or Basement program described in this section to calculate custom monthly average ground temperatures. This is especially important for residential applications and very small buildings. If one of these ground temperature preprocessors is not used, for typical commercial buildings in the USA, a reasonable default value is 2C less than the average indoor space temperature.

# Introduction

There are two difficulties behind linking ground heat transfer calculations to EnergyPlus. One is the fact that the conduction calculations in EnergyPlus (and in DOE–2 and BLAST previously) are one-dimensional and the ground heat transfer calculations are two or three-dimensional. This causes severe modeling problems irrespective of the methods being used for the ground heat transfer calculation. The other difficulty is the markedly different time scales involved in the processes. Basically, the zone model is on an hour scale and the ground heat transfer is on a monthly time scale. The basic heat balance based zone model of EnergyPlus has to be considered as the foundation for building energy simulation at the present time and for some time in the future. Thus, it is necessary to be able to relate ground heat transfer calculations to that model.

The heat balance zone model considers a single room or thermal zone in a building and performs a heat balance on it. A fundamental modeling assumption is that the faces of the enclosure are isothermal planes. A ground heat transfer calculation usually considers an entire building and the earth that surrounds it, resulting in non-isothermal face planes where there is ground contact. While it is not impossible to imagine multi-zone, whole building models that include the surrounding earth and non-isothermal building surfaces, such models will not be practical for some time in the future, and their usefulness even then is not clear.

The EnergyPlus development team addressed the problem and decided that the most reasonable first step would be to partially decouple the ground heat transfer calculation from the thermal zone calculation. The most important parameter for the zone calculation is the outside face temperature of the building surface that is in contact with the ground. Thus this becomes a reasonable "separation plane" for the two calculations. It was further decided that the current usage of monthly average ground temperature was reasonable for this separation plane temperature as well, since the time scales of the building heat transfer processes are so much shorter than those of the ground heat transfer processes.

Using the separation plane premise, the 3D ground heat transfer programs for slabs developed by Bahnfleth (1989, 1990) were modified by Clements (2004) to produce outside face temperatures. EnergyPlus permits separate monthly average inside temperatures as input. The program produces outside face temperatures for the core area and the perimeter area of the slab. It is described in the section "Use of the Ground Temperatures with Slabs" below.

A 3D basement program also is included with EnergyPlus. This is described below in Using Ground Temperatures with Basements. It uses the same principle as the slab procedure; it determines the outside face (surface) temperature of the walls and floor of a basement in contact with the ground.

It should be noted that either for slabs or basements the ground heat transfer is usually small unless the building is very small or has some special characteristics.

Multiple Ground Temperatures shows how the OtherSideCoefficients object of EnergyPlus can be used to supply multiple ground temperatures.