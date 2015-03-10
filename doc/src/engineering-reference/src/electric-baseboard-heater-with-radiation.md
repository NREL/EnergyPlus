# Electric Baseboard Heater with Radiation and Convection

## Overview

The input object ZoneHVAC:Baseboard:RadiantConvective:Electric provides a model for electric baseboard heaters that include both convective and radiant heat addition to a space from a baseboard heater. The radiant heat transfer to people as well as surfaces within a space is determined in the same fashion as both hot water and steam baseboard heater with radiation and convection models. The electric baseboard heater transfers energy via electric resistance heating. Radiant heat calculated by the user-defined fraction from the heating capacity of a baseboard unit impacts the surface heat balances and thermal comfort of occupants in a space. EnergyPlus then assumes that the remaining convective gains from the unit are evenly spread throughout the space thus having an immediate impact on the zone air heat balance which is used to calculate the mean air temperature (MAT) within the space.

## Model Description

### Convective Electric Baseboard Heater Inputs

Like many other HVAC components, the electric baseboard model requires a unique identifying name and an availability schedule. The availability schedule defines the availability of the unit for providing condition to the space. The input also requires a capacity and efficiency for the unit. While the efficiency is a required input that defaults to unity, the capacity can be chosen to be autosized by EnergyPlus.

All inputs for the radiant heat calculation are the same as the water and steam baseboard heater with radiation and convection models in EnergyPlus. Users are required to input fractions that specify total radiant heat directly delivered to surfaces as well as people in a space. The sum of radiant energy of these distribution fractions must sum to unity, and each baseboard heater is allowed to distribute energy to up to 20 surfaces.

### Simulation and Control

When the unit is available and there is a heating load within a space, the electric baseboard unit will meet the entire remaining provided that it has enough capacity to do so. The model sets the heating capacity of a baseboard unit to the remaining heating load in the space, which any primary heating system does not meet. Note that it is set to the maximum nominal capacity of the unit, if a heating load in a space is greater than the maximum. The model then determines the radiant heat source through the simulation as

![](media/image4266.png)\


where *q~rad~* is the total radiant heating capacity, *q* is the heating capacity of the unit, and *Frac~rad~* is the user-defined fraction for the radiation. The radiant heat additions to people and surfaces are thus

![](media/image4267.png)\


![](media/image4268.png)\


where *q~people~* is the radiant heat transfer to people, *q~surface~* is the heat radiated to surfaces, *Frac~people~* is the fraction of the heat radiated to people, and *Frac~surface,i~* is the fraction of the heat radiated to surface i.

Once the radiant heat sources are determined, the model distributes the radiant heat sources to the appropriate surfaces and people for thermal comfort purposes, and EnergyPlus recalculates the surface heat balances. The model then determines differences in convection from the surfaces to the air before and after the surface heat balances are impacted due to radiant heat additions from the unit. Note that the model converts the radiant heat transfer to people to convective energy so that the heat balance includes this amount of energy, which will be lost (see High Temperature Radiant Heater Model). The model calculates the actual convective system impact of an electric baseboard heater unit as

![](media/image4269.png)\


where *q~req~*is the actual heating load that the unit should meet, *q~surf,c~~~*is the convection from the surfaces to the air after the radiant heat distribution, *q~surf,z~* is the convection from the surfaces to the air before the radiant heat distribution, and *q~conv~* is the convective heat transfer to the air.

The energy consumption of the baseboard heater is calculated using the user-supplied efficiency and the actual convective system impact calculated as

![](media/image4270.png)\


where *Q~elec~* is the energy consumption and **η** is the efficiency of the unit.

If the unit was scheduled off or there is no heating load for the zone, then there will be no heat transfer from the unit. The model assumes no heat storage in the baseboard unit itself and thus no residual heat transfer in future system time steps due to heat storage in the metal of the baseboard unit.

### References

No specific reference. Refer to the ASHRAE Handbook series for general information on different system types as needed.

## Steam Baseboard Heater with Radiation and Convection

### Overview

The steam baseboard heater model is intended to calculate the mass flow rate of the steam to meet remaining heating demand by determining the actual system impact not only to the surrounding air via convection but also to the surfaces and people via radiation. The actual system impact by the heater is the sum of the additional convective heat transfer from the surfaces to the zone air after they have been heated as well as radiant heat transferred to people and the convective heat transfer to the zone. This actual convective power tries to meet any remaining heating requirement in the zone. The model thus improves the accuracy of thermal comfort predictions and system responses. The calculation of radiant heat addition is the same as that of water baseboard heater model in EnergyPlus.

This model determines the heating capacity from the sum of the latent heat transfer and sensible cooling of water as current steam coil model in EnergyPlus does. Overall energy balances to the steam and air handle the heat exchange between the steam loop and the zone air. The mass flow rate of steam is determined based on the heating demand in the zone. The model requests the user input the desired degree of subcooling so that it determines the heating rate from the heater due to the cooling of the condensate. The user input is also used to determine the condensate outlet conditions.

### Model Description

### Steam Baseboard Heater Inputs

The steam baseboard model requires a unique identifying name, an available schedule, and steam inlet and outlet nodes. These define the availability of the unit for providing conditions to the space and the node connections that relate to the primary system. It also requires the desired degree of subcooling to calculate the heating capacity and temperature of the condensate. A maximum design flow rate is required, and the user can request this parameter to be auto-sized by EnergyPlus. In addition, a convergence tolerance is requested of the user to help define the ability of the local controls to tightly control the system output. In almost all cases, the user should simply accept the default value for the convergence tolerance unless engaged in an expert study of controls logic in EnergyPlus.

All of the inputs for the radiant heat calculation are the same as the water baseboard heater model in EnergyPlus. User inputs of the radiant fraction and of the fraction of radiant energy incident both on people and on surfaces are required to calculate radiant energy distribution from the heater to the people and surfaces. The sum of radiant energy of these distribution fractions must sum to unity, and each steam baseboard heater is allowed to distribute energy to up to 20 surfaces.

### Simulation and Control

The simulation of the main algorithm of this steam baseboard model with radiation and convection is similar to steam coil model in EnergyPlus while the simulation of radiant component is the same as the water baseboard models. This model initializes all conditions at the inlet node such as mass flow rate, temperature, enthalpy, and humidity ratio. The model then determines the heating capacity of steam baseboard, *q*, as

![](media/image4271.png)\


where ![](media/image4272.png) is the mass flow rate of steam in kg/s, ![](media/image4273.png) is the heat of vaporization of steam in J/kg, ![](media/image4274.png) is the specific heat of water in J/kg.K, and ![](media/image4275.png)  is the degree of subcooling in degree C.

The outlet steam temperature is thus

![](media/image4276.png)\


Once the heating capacity of the unit is determined, the model determines the radiant heat addition by

![](media/image4277.png)\


where *q* is the total heating capacity of the heater and *Frac~rad~* is the user-defined fraction.

The model now distributes the radiant heat additions to the appropriate surfaces, people for thermal comfort purpose, and the air in the zone. The surface heat balances are then recalculated to determine all heat sources or sinks for radiant systems in the zone. It is assumed that the radiant heat incident on people in the zone is converted to convection to the zone so that the zone heat balance includes this amount of heat which will be lost (see High Temperature Radiant Heater Model). The load met, the actual convective system impact, for the baseboard heater, *q~req~*, is therefore determined as

![](media/image4278.png)\


where *q~surf,c~~~*is convection from the surfaces to the air in the zone with radiant systems; *q~surf,z~* is zero source convection from the surfaces when radiant systems are unavailable; *q~conv~* is the convective heat transfer from the heater to the zone air; and *q~people~* is radiant heat to the people.

The simulation of radiant heat addition is the same as the water baseboard heater model. The controls are the same as shown in Figure 271.  Variable Flow Low Temperature Radiant System Controls. After all system time steps have been simulated, an "average" zone heat balance calculation is done (similar to the high temperature radiant system). If the unit was scheduled off or there is no steam flow rate through the baseboard unit, then, there will be no heat transfer from the unit. The model assumes no heat storage in the unit itself and thus no residual heat transfer in future system time steps due to heat storage in the steam or metal of the unit.

### References

No specific reference.  Refer to the ASHRAE Handbook series for general information on different system types as needed.