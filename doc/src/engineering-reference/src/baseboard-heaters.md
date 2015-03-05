# Baseboard Heaters

## Hot Water Baseboard Heater with Only Convection

### Overview

The convective water baseboard heater (Ref. ZoneHVAC:Baseboard:Convective:Water) is a simple model for establishing a convective-only heat addition to a space through a baseboard heater.  In most situations, the baseboard heater does give a significant amount of heat off via natural convection, but some heat is given off via radiation.  In this model, the radiant component is ignored and all heat is assumed to be delivered to the space via convection.  The baseboard heater is supplied with hot water from the primary system which is circulated through the inside of a finned tube within the space.  Heat is transferred from the water inside the pipe, through the tube and fins, and eventually convected to the surrounding air within the space.  EnergyPlus then assumes that this heat is evenly spread throughout the space thus having an immediate impact on the zone air heat balance which is used to calculate the mean air temperature (MAT) within the space.

The model requests an effective UA value to handle the heat exchange between the water loop and the zone air.  From there, it employs an effectiveness-NTU heat exchanger model to determine the heat transfer between the water and the zone air.  This is necessary because during the simulation only the inlet water and "inlet air" (assumed to be zone air) is known.  As a result, the effectiveness-NTU heat exchanger methodology is better suited to determine the performance of the system.

### Model Description

### Convective Water Baseboard Heater Inputs

Like many other HVAC components, the convective-only water baseboard model requires a unique identifying name, an availability schedule, and water side inlet and outlet nodes.  These define the availability of the unit for providing condition to the space and the node connections that relate to the primary system.  In addition, a convergence tolerance is requested of the user to help define the ability of the local controls to tightly control the system output.  In almost all cases, the user should simply accept the default value for the convergence tolerance unless engaged in an expert study of controls logic in EnergyPlus.

The input also requires a UA value and a maximum design flow rate for the unit.  Both of these parameters can be chosen to be auto-sized by EnergyPlus.  The UA value corresponds to the convective heat transfer from the water to the tube, the conduction through the tube and fin material, and the natural convection from the tube/fins to the zone air.  The maximum flow rate and UA value define the performance and maximum output of the baseboard unit.

### Simulation and Control

The simulation of the convective water baseboard unit follows standard effectiveness-NTU methodology.  It begins by calculating the product of the specific heat and mass flow rate for both the air and water sides of the unit (heat exchanger).  In the initialization of the model, it is assumed that the air mass flow rate is equal to 2.0 times the water mass flow rate.  The purpose of this calculation is to provide some reasonable estimate of the air mass flow rate so that the model can calculate system performance correctly.  This calculation is kept constant throughout the rest of the simulation.

Each system time step, the baseboard attempts to meet any remaining heating requirement of the zone through the following calculations:

![](media/image4234.png)\


![](media/image4235.png)\


![](media/image4236.png)\


![](media/image4237.png)\


![](media/image4238.png)\


![](media/image4239.png)\


Once the effectiveness is determined, the outlet conditions for and output from the unit are determined using the following equations:

![](media/image4240.png)\


![](media/image4241.png)\


![](media/image4242.png)\


If the unit was scheduled off or there is no water flow rate through the baseboard unit, then there will be no convection from the unit.  The model assumes no heat storage in the baseboard unit itself and thus no residual heat transfer in future system time steps due to heat storage in the water or metal of the baseboard unit.

### References

The effectiveness-NTU method is taken from Incropera and DeWitt, Fundamentals of Heat and Mass Transfer, Chapter 11.4, p. 523, eq. 11.33.  The user can always refer to the ASHRAE Handbook series for general information on different system types as needed.

## Electric Baseboard Heater with Only Convection

### Overview

The input object ZoneHVAC:Baseboard:Convective:Electric provides a model for electric baseboard heaters that assumes only convective heat addition to a space from a baseboard heater.  In most situations, the baseboard heater does give a significant amount of heat off via natural convection, but some heat is given off via radiation.  In this model, the radiant component is ignored and all heat is assumed to be delivered to the space via convection.  The baseboard heater is transferred to the unit via electric resistance heating.  EnergyPlus then assumes that this heat is evenly spread throughout the space thus having an immediate impact on the zone air heat balance which is used to calculate the mean air temperature (MAT) within the space.

### Model Description

### Convective Electric Baseboard Heater Inputs

Like many other HVAC components, the convective-only electric baseboard model requires a unique identifying name and an availability schedule.  The availability schedule defines the availability of the unit for providing condition to the space.

The input also requires a capacity and efficiency for the unit.  While the efficiency is a required input that defaults to unity, the capacity can be chosen to be auto-sized by EnergyPlus.

### Simulation and Control

When the unit is available and there is a heating load within a space, the electric baseboard unit will meet the entire remaining provided that it has enough capacity to do so.  The energy consumption of the baseboard heat is calculated using the user-supplied efficiency and the current load on the baseboard unit as follows:

![](media/image4243.png)\


If the unit was scheduled off or there is no heating load for the zone, then there will be no convection from the unit.  The model assumes no heat storage in the baseboard unit itself and thus no residual heat transfer in future system time steps due to heat storage in the metal of the baseboard unit.

### References

No specific reference.  Refer to the ASHRAE Handbook series for general information on different system types as needed.

## Hot Water Baseboard Heater with Radiation and Convection

### Overview

The water baseboard heater model is intended to calculate the mass flow rate of the water by determining the actual system impact not only to the surrounding air via convection but also to the surfaces and people via radiation. The actual system impact by the heater in the zone is the sum of the additional convective heat transfer from the surfaces to the zone air after they have been heated as well as radiant heat transferred to people and the convective heat transfer to the zone. This actual convective power tries to meet any remaining heating requirement in the zone. The model thus improves the accuracy of thermal comfort predictions and system responses.

The radiant components are calculated to interact with the surfaces in the zone through the radiant distribution fractions. Heat is also delivered to the space via convection. The baseboard heater is supplied with hot water from the primary system which is circulated through the inside of a finned tube within the space. This could also be used to model a hot water radiator (convector in the UK). Heat is transferred from the water inside the pipe, through the tube and fins. It is also not only convected to the surrounding air but also radiated to the surfaces within the space. The user is allowed to specify the percentage of radiant heat from the heater to the surfaces. In addition, the user is allowed the ability to define what fraction of radiation leaving the heater is incident directly on a person within the zone for thermal comfort purposes. This amount of heat is then used in the thermal comfort models as the high temperature radiant heater model.

This model calculates the UA values using the log mean temperature difference (LMTD) method. The calculation is based on standard conditions of both fluids obtained in a rating document or manufacturer's literature. Overall energy balances to the hot water and air handle the heat exchange between the water loop and the zone air. Once the UA for the baseboard heater has been determined, the model employs an effectiveness-NTU heat exchanger method to determine the heat transfer between the water and zone air. This is necessary because during the simulation only the inlet water and "inlet air" (assumed to be zone air) temperatures are known. As a result, the effectiveness-NTU heat exchanger methodology is suited to determine the performance of the system during the simulation.

### Model Description

### Water Baseboard Heater Inputs

Like many other HVAC components, the water baseboard model requires a unique identifying name, an available schedule, and water inlet and outlet nodes. These define the availability of the unit for providing conditions to the space and the node connections that relate to the primary system. For the calculation of the UA value of the unit, standard conditions of both fluids are necessary. The user can specifies such standard conditions from manufacturers' information such as I=B=R rating document. The user provides the rated capacity, average water temperature, water mass flow rate, and standard inlet air temperature. However, the model also has the ability to autosize the rated capacity, which allows the user simulate and design any baseboard units when manufacturer's data is unavailable.

The UA value corresponds to the convective heat transfer from the water to the tube, the conduction through the tube and fin material, the natural convection from the tube/fins to the zone air, and radiation to the surfaces. In addition, a convergence tolerance is requested of the user to help define the ability of the local controls to tightly control the system output. In almost all cases, the user should simply accept the default value for the convergence tolerance unless engaged in an expert study of controls logic in EnergyPlus.

Many of the inputs for the radiant heat calculation are the same as the high temperature radiant heater model. User inputs of the fraction radiant and of the fraction of radiant energy incident both on people and on surfaces are required to calculate radiant energy distribution from the heater to the people and surfaces. The sum of radiant energy of these distribution fractions must sum to unity, and each water baseboard heater is allowed to distribute energy to up to 20 surfaces.

### Simulation and Control

The simulation of the main algorithm of this water baseboard model with radiation and convection is similar to the convection-only baseboard model. However, this model determines the UA value of the unit based on rated heating capacities available in the manufacturer's literature. Almost all baseboards or radiators are rated under standard conditions such as water flow rate, inlet air temperature and average water temperature. Those standard conditions should thus be obtained from manufacturers' information. The model then determines the UA value of the unit, employing the log mean temperature difference (LMTD) method so that the heating output from the heater can be determined more correctly than other models.

This model initializes all the conditions at the inlet node such as mass flow rates, temperatures, and specific heat capacities. It calculates the UA value once from standard conditions of the air and water during initialization and it is kept throughout the simulation of the unit.

In case that the user has manufacturer's information regarding the rated capacity, the rated water flow rate, and the rated average water temperature, standard conditions such as the rated heating capacity, *q~std~*, average water temperature, ![](media/image4244.png) , and water mass flow rate, ![](media/image4245.png) , are known from the user inputs. The standard water inlet temperature, ![](media/image4246.png) , and outlet temperature, ![](media/image4247.png) , are thus obtained from expressions

![](media/image4248.png)\


![](media/image4249.png)\


where ![](media/image4250.png)  is the specific heat capacity of the water.

The model then assumes the air mass flow rate is twice the rated water mass flow rate. Since the inlet air temperature, ![](media/image4251.png) , of 18C and the heating capacity of the unit, i.e. rated capacity, are known, the outlet air temperature, ![](media/image4252.png) , can be obtained from expression

![](media/image4253.png)\


where ![](media/image4254.png)  is the specific heat capacity of the air.

All temperatures at each node are now known. The mean temperature difference, ![](media/image4255.png) , is thus obtained from the following expressions:

![](media/image4256.png)\


![](media/image4257.png)\


![](media/image4258.png)\


The UA value of the unit is thus

![](media/image4259.png)\


The model allows the user requests to autosize the rated capacity when the user does not know a particular baseboard unit. It also has the ability to perform parametric study to design the unit based on the design heating load in the zone. The user is also able to modify both rated average water temperature and rated water mass flow rate when manufactures' information is available while the model assumes the default values for both fields if unavailable.

In case that the user requests autosizing the rated capacity, the model employs the design heating load and design water mass flow rate in the zone, so that the standard water inlet and outlet temperatures are estimated as

![](media/image4260.png)\


![](media/image4261.png)\


where ![](media/image4262.png)  is the design heating load estimated by EnergyPlus in the zone.

Similarly, the model estimates the air outlet temperature assuming the air mass flow rate is twice the rated water mass flow rate as

![](media/image4263.png)\


Temperatures at the nodes are now known and the UA value is determined in the same fashion as the previous case.

Once the UA value is determined, the model employs an effectiveness-NTU heat exchanger method to determine the heat transfer between the water and the zone air as convection-only model does (see "Hot Water Baseboard Heater with Only Convection" model). Note that the model also assumes that the air mass flow rate is twice the water mass flow rate in the main heat exchanger algorithm.

The model then determines the radiant heat addition by

![](media/image4264.png)\


where *q* is the total heating capacity of the heater and *Frac~rad~* is the user-defined fraction.

The model now distributes the radiant heat additions to the appropriate surfaces, people for thermal comfort purpose, and the air in the zone. The surface heat balances are then recalculated to determine all heat sources or sinks for radiant systems in the zone. It is assumed that the radiant heat incident on people in the zone is converted to convection to the zone so that the zone heat balance includes this amount of heat which will be lost (see High Temperature Radiant Heater Model). The load met, the actual convective system impact, for the baseboard heater, *q~req~*, is therefore determined as

![](media/image4265.png)\


where *q~surf,c~~~* is convection from the surfaces to the air in the zone with radiant systems; *q~surf,z~* is zero source convection from the surfaces when radiant systems are unavailable; *q~conv~* is the convective heat transfer from the heater to the zone air; and *q~people~* is radiant heat to the people.

The radiant heat simulation in the water baseboard heater is very similar to the high temperature radiant system model. The controls are the same as shown in Figure 271.  Variable Flow Low Temperature Radiant System Controls.  After all system time steps have been simulated, an "average" zone heat balance calculation is done (similar to the radiant systems). If the unit was scheduled off or there is no water flow rate through the baseboard unit, then, there will be no heat transfer from the unit. The model assumes no heat storage in the unit itself and thus no residual heat transfer in future system time steps due to heat storage in the water or metal of the unit.

### References

I=B=R Ratings for Boilers. 2009. Baseboard Radiation, Finned Tube (Commercial) Radiation, and Indirect Fired Water Heaters, January 2009 Edition

Incropera and DeWitt. Fundamentals of Heat and Mass Transfer, Chapter 11.3 and 11.4, eq. 11.15, 11.17, and 11.33.

Li Lianzhong and M. Zaheeruddin. 2006. Dynamic modeling and simulation of a room with hot water baseboard heater, International Journal of Energy Research; 30, pp. 427–445