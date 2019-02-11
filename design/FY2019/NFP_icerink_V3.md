


Module for Ice Rink Floor Thermal Model 
=======

**Moncef Krarti & Punya Sloka Dash**

 - Original Date: 2019-01-14
 - Revision Date: 2019-02-11


## Justification for New Feature ##

Currently, EnergyPlus does not have the capabilities to model energy performance of ice rinks both indoors and outdoors. Ice rinks are highly energy intensive facilities and are part of several entertainment and sports complexes. The proposed new feature will allow the modeling of ice rinks in EnergyPlus in order to estimate both the required refrigeration thermal loads and energy use required to operate indoor ice arenas. The added feature will include the heat load associated with resurfacing ice rinks depending on a user-defined schedule. Moreover, the model will account for the latent load associated with high humidity exchanges between the floor surace and the indoors (especially during water freezing and ice resurfacing).  Once complete, the new modeling functionality will allow to design, evaluate, and operate ice rink facilities using EnergyPlus. 

## E-mail and  Conference Call Conclusions ##

N/A

## Overview ##
Indoor ice rink model can be closely related to the existing EnergyPlus model for low temperature radiant system with variable refrigerant flow, although there are some fundamental differences between the two systems to be taken care of before full implementation into EnergyPlus as noted below:


1.  Ice rink system uses brine as refrigerant which operates often below freezing point temperature, while low temperature radiant system uses water which is set to operate above freezing point temperature.
2. Ice rink surface generates high humidity levels especially during water freezing and regular ice resurfacing cycles. Therefore, dehumidication of the ice arena space may be needed. 
3. Ice rink surface need regular resurfacing by injecting high hot water volume at regular periods resulting in additional refrigeration loads.
5. In addition to thermal insulation below the floor, heating system may be used underneath the ice rink floor to avoid freezing the ground medium that may result in heating issues
4.  Ice rink system is used to control the ice surface layer temperature or brine outlet temperature whereas low temperature radiant system controls room air temperature.
5.  Ice rink system only includes the floor system and related refrigeration equipment and is typically independent from zone air control system. Low temperature radiant system, however, includes HVAC equipment that is used to remove thermal load from one or several zones.


## Approach ##
The module to be developed will be based on a previous work done at the University of Colorado at Boulder and published in a series of peer-reviewed papers including Somarani et al. (2008) and Mun and Krarti (2011; 2015a, and 2015b) as listed in the References Section. While the EnergyPlus ice rink model will be based on one-dimensional heat transfer analysis, a two-dimensional solution will be used for verification analysis especially for the water freezing and ice resurfacing periods. 



 ![](https://i.imgur.com/UfyahPF.jpg)

*Figure 1. Proposed Input-Output chart for the EnergyPlus Ice Rink Floor module.*



The new input data required for the module include (Note: This list will be updated as the algorithm implementation moves forward):


1. Ice Rink dimensions
2. Ice Rink Floor Construction 
3. Ice Set-point Temperature
4. Initial  ice/water temperature
5. Capacity of the refrigeration system
6. Refrigerant type
7. Mass flow rate of the refrigerant
8. Control Strategy 
9. schedule or number of times for ice resurfacing
10. Hot water temperature used for ice resurfacing
11. Volume of water used for ice resurfacing 

The proposed ice rink floor model will perform the following calculations:

1-  The refrigeration load during normal operation of the ice arenas as well as during ice surfacing events. The refrigeration load will be affected by several heat transfer mechanisms including radiation, conduction, and convection, as well as phase change (freezing of water) and condensation.  

2- The latent load added by the ice surface will need to be calculated including due and during ice resurfacing events. The existing EnergyPlus models for heating and dehumidication systems will be used to maintain any desired indoor temperature and humidity for the arena space.

3- When applicable, the heating load associated with the floor and subsoil heating elements. 

Two control strategies for the ice rink floor refrigeration systems will be modeled as described below:

1. Brine temperature control strategy:
![](https://i.imgur.com/dbEftfX.jpg)
2. Ice surface temperature control strategy: ![](https://i.imgur.com/7Bu0L9x.jpg)

By using these one of the two strategy, the mass flow rate of brine can be determined.    

## Testing/Validation/Data Sources ##
The thermal model developed by Mun and Krarti (2011), was integrated into EnergyPlus (In its FORTRAN 90 format) and validated against a laboratory ice rink model Mun and Krarti (2011). For this project, further verification and validation analyses will be carried out using other reported data sources such as those listed in the [IIHF Ice Rink Guide](https://www.iihf.com/en/static/5890/iihf-ice-rink-guide). After the module is successfully implemented and validated, a series of simulations can be performed to assess the impact of various design and operating parameters on the energy performance of indoor ice rinks.


##Input Output reference Documentation##

There will be requirement of new inputs in order to make this proposal work (as pointed in the approach section). The IOref has to be updated accordingly with the proposed IDD changes.



## Engineering reference ##


## Example File and Transition Changes ##
A new example file will be created for this new feature.  No significant modifications will be required to any module within EnergyPlus. Some interactions between the ice rink space and the floor module will be required as part of the development work of the module. Moreover, some of the data required such refrigerant properties may be added to be the database of EnergyPlus. Some challenges may be encountered -but will not the primary tasks of this project- including potential convergence and stability of the calculations especially when the ice rink floors are coupled with the ground medium through adjustments of computing Conduction Transfer Functions (CTFs) as outlined in the work reported by Mun and Krarti (2015a). Note that the ice rink system can be considered as a special case of radiant systems and is therefore both a building heat transfer element and a controllable system. Iterations between the ice rink system and the surface heat balance routine would be necessary. 

## References ##

Kaya, R., 2015, Energy Usage in Ice Rink Resurfacing, MS Thesis, KTH School of Industrial Engineering and Management, Stockholm, Sweden. 

IIHF, 2019, IIHF Ice Rink Guide. IIHF International Ice Hockey Federation. Accessed February 10, 2019. https://www.iihf.com/en/static/5890/iihf-ice-rink-guide.

Mun, J., and Krarti, M., 2011, An ice rink floor thermal model suitable for whole-building energy simulation analysis, Building and Environment, 46 (5), 1087-1095. 

Mun, J., and Krarti, M., 2015a, Implementation of a new CTF method stability algorithm into EnergyPlus, Building Simulation Journal, 8, 613–620.

Mun, J., and Krarti, M., 2015b, Optimal insulation for ice rink floors, Energy and Buildings, 108, 358-364. 

Somrani, R., Mun, J., and Krarti, M., 2008, Heat transfer beneath ice-rink floors, Building and Environment, 43, 1687-1698. 

Strand, R.K., 1995, Heat source transfer functions and their application to low temperature radiant heating systems. Ph.D. thesis. University of Illinois at Urbana-Champaign.

Strand, R. K., and Baumgartner, K.T., 2005, Modeling Radiant Heating and Cooling Systems: Integration with a Whole-Building Simulation Program, Energy and Buildings 37, 4, 389–397.

NFP_icerink_v2-mk.txt
Displaying NFP_icerink_v2-mk.txt.
