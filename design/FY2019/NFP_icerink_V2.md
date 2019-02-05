


Module for Ice Rink Floor Thermal Model 
=======

**Moncef Krarti & Punya Sloka Dash**

 - Original Date: 2019-01-14
 - Revision Date: 2019-01-31


## Justification for New Feature ##

Currently, EnergyPlus does not have the capabilities to model energy performance of ice rinks both indoors and outdoors. Ice rinks are highly energy intensive facilities and are part of several entertainment and sports complexes. The proposed new feature will allow the modeling of ice rinks in EnergyPlus in order to estimate both the required refrigeration thermal loads and energy use required to operate indoor ice arenas. The added feature will incorporate common refrigerants used as well as control strategies to maintain high quality ice conditions. Once complete, the new modeling functionality will allow to design, evaluate, and operate ice rink facilities using EnergyPlus. 

## E-mail and  Conference Call Conclusions ##



## Overview ##

The ZoneHVAC:LowTemperatureRadiant class can only model ConstantFLow, VariableFlow and Electric systems as of now. Indoor ice rink can be closely related to a low temperature radiant system with variable refrigerant flow, although there are some fundamental differences between the two to be taken care of before full implementation into EnergyPlus as noted below:


1.  Ice rink system uses brine as refrigerant which operates often below freezing point temperature, while low temperature radiant system uses water which is set to operate above freezing point temperature.
2.  Ice rink system is used to control the ice surface layer temperature or brine outlet temperature whereas low temperature radiant system controls room air temperature.
3.  Ice rink system only includes the floor system and related refrigeration equipment and is typically independent from zone air control system. Low temperature radiant system, however, includes HVAC equipment that is used to remove thermal load from one or several zones.

## Approach ##
The module to be developed will be based on a previous work done at the University of Colorado at Boulder and published in a series of peer-reviewed papers including Somarani et al. (2008) and Mun and Krarti (2011; 2015a, and 2015b) as listed in the References Section. Like the low temperature radiant system model, the ice rink floor system has two algorithms

1. To calculate temperature and heat flux through the floor structure with heat sink/ source.
2. To predict temperatures and heat fluxes at the sink/ source location.

 ![](https://i.imgur.com/rUuSLtH.png) 

*Figure 1. Proposed Input-Output chart for the EnergyPlus Ice Rink Floor module.*

The control strategies to be employed ( which will determine the mass flow rate of brine) are:



- __Brine outlet temperature control__
  

 ![](https://i.imgur.com/77aXTEk.jpg)



- __Ice surface temperature control__

![](https://i.imgur.com/0FReqdo.jpg)

The new input data required for the module include (Note: This list will increase or decrease as this project moves forward):


1. Ice Rink dimensions
2. Ice Rink Floor Construction 
3. Ice Setpoint Temperature
4. Initial  ice/water temperature
5. Capacity of the refrigeration system
6. Refrigerant type
7. Mass flowrate of the refrigerant
8. Control Strategy 


## Testing/Validation/Data Sources ##
The thermal model developed by Mun and Krarti (2011), was integrated into EnergyPlus (In its FORTRAN 90 format) and validated against a laboratory ice rink model Mun and Krarti (2011). For this project, further verifcation and validation analyses will be carried out using other reported data sources such as those listed in the IIHF Ice Rink Guide(https://www.iihf.com/en/static/5890/iihf-ice-rink-guide). After the module is successfully implemented and validated, a series of simulations can be performend to assess the impact of various design and operating parameters on the energy performance of indoor ice rinks.


##Limitations of the model##

The model is limited to indoor ice rink floors with specific floor construction configuration but the model can be extended to address other floor construction configuration and can ultimately reformulated to model outdoor ice rinks. As part of this effort, only basic control strategies will be implemented as part of the model as outlined in Mun and Krarti (2011), and common insulation configurations for the ice rink floor systems will be considered as noted in Mun and Krarti (2015b). 


## Modifications to other EnergyPlus Modules ##

No significant modifications will be required to any module within EnergyPlus. Some interactions between the ice rink space and the floor module will be required as part of the development work of the module. Moreover, some of the data required such refrigerant properties may be added to be the database of EnergyPlus. Some challenges may be encountered -but will not the primary tasks of this project- including potential convergence and stability of the calculations especially when the ice rink floors are coupled with the ground medium through adjustments of computing Conduction Transfer Functions (CTFs) as outlined in the work reported by Mun and Krarti (2015a). Note that the ice rink system can be considered as a special case of radiant systems and is therefore both a building heat transfer element and a controllable system. Iterations between the ice rink system and the surface heat balance routine would be necessary. 

## References ##

Mun, J., and Krarti, M., 2011, An ice rink floor thermal model suitable for whole-building energy simulation analysis, Building and Environment, 46 (5), 1087-1095. 

Mun, J., and Krarti, M., 2015a, Implementation of a new CTF method stability algorithm into EnergyPlus, Building Simulation Journal, 8, 613–620.

Mun, J., and Krarti, M., 2015b, Optimal insulation for ice rink floors, Energy and Buildings, 108, 358-364. 

Somrani, R., Mun, J., and Krarti, M., 2008, Heat transfer beneath ice-rink floors, Building and Environment, 43, 1687-1698. 

Strand, R.K., 1995, Heat source transfer functions and their application to low temperature radiant heating systems. Ph.D. thesis. University of Illinois at Urbana-Champaign.

Strand, R. K., and Baumgartner, K.T., 2005, Modeling Radiant Heating and Cooling Systems: Integration with a Whole-Building Simulation Program, Energy and Buildings 37, 4, 389–397.

NFP_icerink_v2-mk.txt
Displaying NFP_icerink_v2-mk.txt.