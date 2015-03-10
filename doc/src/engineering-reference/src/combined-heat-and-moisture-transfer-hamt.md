# Combined Heat and Moisture Transfer (HAMT) Model

## Overview

The combined heat and moisture transfer finite (HAMT) solution algorithm is a completely coupled, one-dimensional, finite element, heat and moisture transfer model simulating the movement and storage of heat and moisture in surfaces simultaneously from and to both the internal and external environments. As well as simulating the effects of moisture buffering, HAMT is also be able to provide temperature and moisture profiles through composite building walls, and help to identify surfaces with high surface humidity.

## HAMT Nomenclature

Dependencies on moisture content are indicated by a superscript ^w^, on heat by a superscript ^h^ and vapor pressure by a superscript ^v^.

Table: Combined Heat and Moisture Transfer Model Nomenclature

Symbol|Units|Meaning
------|-----|-------
T|°C|Temperature
RH,φ|%, fraction|Relative humidity
W|kg/m^3^|Moisture Content
![](media/image185.png) |J/m^3^C|Moisture dependent heat storage capacity
![](media/image186.png) |kg/m^3^|Moisture dependent moisture storage capacity
![](media/image187.png) |W/mC|Moisture dependent thermal conductivity
![](media/image188.png) |J/kg|Evaporation enthalpy of water (= 2, 489, 000J/kg)
![](media/image189.png) |kg/msPa|Vapor diffusion coefficient in air
![](media/image190.png) |-|Moisture dependent vapor diffusion resistance factor
P|Pa|Vapor pressure
p~ambient~|Pa|Ambient air pressure
C|J/kgC|Specific heat capacity of dry material
c^w^|J/KgC|Specific heat capacity of water (=4,180J/kg^o^C@ 20^o^C)
![](media/image191.png) |kg/m^3^|Material Density
![](media/image192.png) |kg/m^3^|Density of water (= 1000kg/m3)
![](media/image193.png) |m^2^/s|Liquid Transport Coefficient
A|m^2^|Contact Surface area
![](media/image194.png) |m^3^|Cell Volume
![](media/image195.png) |s|Time
![](media/image196.png) |s|Time step between calculations
x|m|Distance between cell centres
![](media/image197.png) |J/C|Heat Capacitance of cell i
![](media/image198.png) |kg|Moisture Capacitance of cell i
![](media/image199.png) |C/W|Heat Resistance between cells i and j
![](media/image200.png) |sPa/kg|Vapor Resistance between cells i and j
![](media/image201.png) |s/kg|Liquid Moisture Resistance between cells i and j
![](media/image202.png) |W|Heat due to Vaporisation
![](media/image203.png) |W|Heat from additional Sources
P|m3/m3|Material Porosity
p(as a superscript)|s|Present Time Step
i,j|-|Cell indices

## HAMT Model Description

Equations  and  are derived from heat and moisture balance equations and are taken from [Künzel, H.M. (1995)]. They describe a theoretical model for the transfer of heat and moisture through a material.

![](media/image204.png)\


The three terms in equation  describe the storage, transport and generation of heat respectively.

![](media/image205.png)\


The three terms in equation  describe the storage of moisture, the transport of liquid moisture and the transport of vapor respectively. The equation to calculate the vapor diffusion coefficient in air (![](media/image206.png) ) used in the third term of both equations, is also taken from Künzel,

![](media/image207.png)\


The heat storage capacity (![](media/image208.png) ) depends on the moisture content w of the material by the following equation.

![](media/image209.png)\


The moisture content of the material w and the vapor diffusion resistance factor μ depend on the relative humidity inside the material. The parameters ![](media/image210.png) , ![](media/image211.png)  and ![](media/image212.png) are also moisture dependent parameters.

The following sections describe how the above equations are used within the HAMT model.

### Surfaces, Material Layers and Cells

"Surfaces" are made of a number of layers of potentially any combination of materials. Each surface is split into its constituent materials and is then split up further into cells through its depth. HAMT will generate no more than 10 cells per material with widths that are thinner near the boundaries of each material where most changes are expected and detail is needed.

### Heat Transfer

Equation 1 can be re-written and used to describe the heat storage and transfer through the i^th^ cell in a surface.

![](media/image213.png)\


In the one dimensional case there are only two adjacent cells each labelled j. The heat generated due to vaporisation ![](media/image214.png)  can be calculated separately.

![](media/image215.png)\


Rearranging equation  and including other sources of heat (![](media/image216.png) ) such as radiation from other surfaces in the calculation gives the temperature in a cell in the next time step as,

![](media/image217.png)\


where![](media/image218.png)  is thermal heat capacitance of cell i and ![](media/image219.png)  is the thermal resistance between cells i and j.

This equation can be solved using the Gauss-Seidel iteration technique. The i^th^ cell temperature is calculated whilst the j^th^ cell temperatures are kept as up to date as possible. The iteration is stopped when the maximum difference between two consecutive calculations in all cells is less than a threshold of 0.002°C.

### Moisture Content w

The moisture content (w) of a cell is needed for the calculation of the heat transfer through the cell as it affects the thermal resistance and heat capacitance. The moisture content of cells is calculated from the relative humidity (RH) of the material. The relationship between w and the RH for each material is known as the sorption isotherm and measured data points are entered into EnergyPlus as a series of coordinates. HAMT interpolates between the measurements to obtain the moisture content of a material for any RH value. The sorption isotherm input is via the MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm object and is described in the Input Output Reference document.

### Porosity P

The porosity of a material (P) is an input variable and defined as the maximum fraction, by volume, of a material that can be taken up with moisture. It is used to calculate the maximum point on the sorption isotherm curve.  The porosity is entered for each material via the MaterialProperty:HeatAndMoistureTransfer:Settings object, as described in the Input Output Reference document.

### Moisture Dependant Thermal Conductivity k^w^

The thermal conductivity (k^w^) of the cell is determined by interpolating between data points of thermal conductivity versus the moisture content of the material, entered into EnergyPlus via the MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity object. The moisture content is determined via the sorption isotherm which gives the moisture content as a function of Relative Humidity.

### Moisture Dependant Moisture Diffusion Coefficient μ

This is used in the third term of equation  to describe the heat transfer due to vapor movement. It is determined by interpolating between data points of moisture diffusion coefficient versus the moisture content of the material, entered into EnergyPlus via the MaterialProperty:HeatAndMoistureTransfer:Diffusion object. A simple linear interpolation is used to obtain the conductivity between measured points.

### Moisture Transfer

Moisture, as well as heat, is transported through materials as either liquid (w) or vapor (p). There are two different potentials that control the movement though the material. Liquid transfer is driven by differences in relative humidity whereas vapor transfer is driven by differences in vapor pressure. Materials also have a capacity to store moisture. Equation  can be re-written for a discrete cell in a continuous material.

![](media/image220.png)\


Equation  can be rearranged to provide the relative humidity of the i^th^ cell in the next time step.

![](media/image221.png)\


where ![](media/image222.png)  is the "Moisture Capacitance" of cell i,

![](media/image223.png)\


is the moisture resistance between cells i and j and ![](media/image224.png)  is the vapor resistance  between cells i and j.

Equation  can be used together with the heat equation  in an alternate step by step fashion to calculate the new temperature and relative humidity profiles for each cell for the next time step.

### Liquid Transport Coefficient D^w^

The Moisture Dependant Liquid Transport Coefficient is entered as a series of moisture density and liquid transport coefficient data points. There are two different coefficients, one for suction, where the surface is wet due to rain, and one for redistribution where the surface is no longer wet. If the weather file has a rain flag it is used to switch between these two types of coefficient. HAMT-SUCTION and HAMT-REDISTRIBUTION.

### Moisture Dependent Moisture Capacity !!! {:error "Image in Header" :alt "" :img "media/image225.png"} !!! 

This is simply the gradient of moisture sorption isotherm at the RH of the material.

### Convective Heat Transfer

The internal and external heat transfer coefficients are used to calculate the thermal resistance of the boundary layer between the zone air and the surface of the surface. They are either supplied by the user via the advanced surface concepts object "SurfaceProperty:ConvectionCoefficients" or, if these are not provided, dynamic values are calculated.

### Convective Vapor Transfer

The internal and external vapor transfer coefficients are used to calculate the resistance to vapour transfer of the boundary layer between the zone air and the surface of the surface. They are also either supplied by the user via the advanced surface concept object SurfaceProperties:VaporCoefficients. If these are not provided then dynamic values are calculated based on the convective heat transfer coefficients.

### Initial Moisture Content

At the start of an EnergyPlus simulation "warm up" days are used to ensure that the temperatures of surfaces come to equilibrium with the environment before the simulation starts proper. Moisture content within some building fabrics can take a very long time to come to equilibrium with its environment and it is therefore necessary to set initial or typical values of moisture content for each material to be used at the start of the simulation. These initial values are entered for each material via the MaterialProperty:HeatAndMoistureTransfer:Settings object as described in the Input Output Reference document.

### Using the Model

As an illustration of the use of the Heat and Moisture Transfer (HAMT) model, the material properties for a small sample of six generic materials have been provided in the EnergyPlus Reference DataSets (MoistureMaterials.idf). The properties were synthesised from the Annex 24 database [Kumar Kumaran, M. (1996)], supplemented, when required, by data from the database of the WUFI model [WUFI (1999)] and are therefore not related to any unique, measured material. Users should consult material property catalogues and other primary sources when the properties of a specific material are required.

Moisture and heat from the surfaces are used by EnergyPlus to calculate the room air temperature and moisture content. EnergyPlus with HAMT works best with as short a time step as possible. However the optimum time step which gives a good prediction for a short computing time will very much depend on the nature of the weather and type of building. Buildings with frequent and large changes in internal and external temperature will need a small time step, maybe even 60 steps per hour. Slowly evolving temperatures and relative humidity's will not require such a short time step and 20, or even 6, steps per hour may be sufficient.

## References

Künzel, H.M. (1995) Simultaneous Heat and Moisture Transport in Building Components. One- and two-dimensional calculation using simple parameters. IRB Verlag 1995

Holman, J.P. (2002) Heat Transfer, Ninth Edition. McGraw-Hill

Winterton, R.H.S. (1997) Heat Transfer. (Oxford Chemistry Primers; 50) Oxford University Press

Kumar Kumaran, M. (1996) IEA ANNEX 24, Final Report, Volume 3

WUFI (1999) version 2.2 Simultaneous Heat and Moisture Transport in Building components. Fraunhofer IBP, Holzkirchen, Germany