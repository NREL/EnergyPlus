# Photovoltaic Arrays

The Photovoltaics.f90 module includes three different models referred to as "Simple", "Equivalent One-Diode" and "Sandia" and the choice will determine the mathematical models (and input data) used to determine the energy produced by solar/electric conversion panels. The EnergyPlus photovoltaic array models are called one at a time at the HVAC system timestep along with other electrical generation components such as gas turbines and diesel engines.

All of the photovoltaic models share the same models for predicting incident solar radiation that are also used for the solar thermal calculations and are described in the section Climate, Sky and Solar/Shading Calculations.

Note that some of the terminology used to discussed photovoltaics overlaps with terminology used to discuss Fortran programs. The word *module* may refer to a PV panel or to a fortran90 programming entity. *Model* may refer to a manufacturers production model for a specific type of PV module or to a mathematical model used for engineering analysis. *Array* may refer to a collection of PV modules wired together or to a mathematical variable with multiple elements.

The PV modules are assumed to always run when the total incident solar is greater than 0.3 Watts. If the incident solar is less than 0.3, then the modules produce no power.

PV arrays are managed by an electric load center. The load center is a "load" with respect to generating equipment but is a "supply center" for the rest of the building. PV arrays need to be connected to ElectricLoadCenter:Distribution objects that have a DC buss type.

## Simple Model

The Generator:PV:Simple object describes about the simplest model for predicting photovoltaic energy production. In this model the user specifies the efficiency with which surfaces convert incident solar radiation to electricity. (In the other models this efficiency is determined as part of the model.)  The full geometric model for solar radiation is used, including sky models, shading, and reflections, to determine the incident solar resource. The model accepts arbitrary conversion efficiencies and does not require actual production units be tested to obtain empirical performance coefficients. (The Energy+.idd sets the range of conversion efficiencies to be on [0..1], but the user could alter the Energy+.idd to extend this range if desired.)

### Mathematical Description

Table: Nomenclature for Simple Photovoltaic model

Mathematical variable|Description
---------------------|-----------
P|Electrical power produced by photovoltaics [W]
A~surf~|Net area of surface  [m^2^]
f~activ~|Fraction of surface area with active solar cells [ ]
G~T~|Total solar radiation incident on PV array [W/m^2^]
~cell~|Module conversion efficiency [ ]
~invert~|DC to AC conversion efficiency  [ ]

The usable electrical power produced by a PV surface are calculated using:

![](media/image7078.png)\


On the right hand side of this equation, only G~T~ is calculated by EnergyPlus and the rest are user inputs. Power levels are assumed constant over the timestep to arrive at energy production.

There are two modes that can be selected by the user that govern how the PV system is coupled to the building surfaces. If the integration mode is selected as ‘DECOUPLED' then no adjustments are made to account for energy extracted in the form of electricity. If the integration mode is selected as ‘INTEGRATED' then the energy extracted in the form of electricity is removed from surface heat transfer calculations using a sink term. This sink term is lagged from the previous timestep.

## Equivalent One-Diode Model

This model predicts the electrical performance of a photovoltaic (PV) array. This model is also known as the "TRNSYS PV" model.

Mathematically speaking, the EnergyPlus PV module employs equations for an empirical equivalent circuit model to predict the current-voltage characteristics of a single module. This circuit consists of a DC current source, diode, and either one or two resistors. The strength of the current source is dependent on solar radiation and the IV characteristics of the diode are temperature-dependent. The results for a single module equivalent circuit are extrapolated to predict the performance of a multi-module array.

The module employs a "four-parameter" equivalent circuit to model crystalline (both mono and poly) PV modules developed at the University of Wisconsin – Madison [2]. The values of these parameters cannot normally be obtained directly from manufacturers' catalogs. However, the PV module will automatically calculate them from commonly available data. The PV module also includes an optional incidence angle modifier correlation to calculate how the reflectance of the PV module surface varies with the angle of incidence of solar radiation.

The module determines PV current as a function of load voltage. Other OUTPUTS include current and voltage at the maximum power point along the IV curve, open-circuit voltage, short circuit current as well as electrical load met and unmet.

Table: General Nomenclature for the PV model

Mathematical variable|Description
---------------------|-----------
|Slope of PV array [degrees]
|Empirical PV curve-fitting parameter
~~|Semiconductor bandgap [eV]
~c~|Module conversion efficiency
~~~~~~|Temperature coefficient of short-circuit current [A/K]
~~~~~~|Temperature coefficient of open-circuit voltage [V/K]
|Angle of incidence for solar radiation [degrees]
|Module transmittance-absorptance product
~normal~|Module transmittance-absorptance product at normal incidence
G~T~|Total radiation incident on PV array
G~T,beam~|Beam component of incident radiation
G~T,diff~|Diffuse component of incident radiation
G~T,gnd~|Ground-reflected component of incident radiation
G~T,NOCT~|Incident radiation at NOCT conditions
G~T,ref~|Incident radiation at reference conditions
I|Current
I~L~|Module photocurrent
I~L,ref~|Module photocurrent at reference conditions
I~o~|Diode reverse saturation current
I~o,ref~|Diode reverse saturation current at reference conditions
I~sc~|Short-circuit current
I~sc,ref~|Short-circuit current at reference conditions
I~mp~|Current at maximum power point along IV curve
I~mp,ref~|Current at maximum power point along IV curve, reference conditions
IAM|Dimensionless incidence angle modifier
K|Boltzmann constant [J/K]
NP|Number of modules in parallel in array
NS|Number of modules in series in array
N~s~|Number of individual cells in module
P|PV output power
P~max~|PV output power at maximum power point along IV curve
Q|Electron charge constant
R~s~|Module series resistance []
R~sh~|Module shunt resistance []
T~c~|Module temperature [K]
T~c,NOCT~|Module temperature at NOCT conditions [K]
T~c,ref~|Module temperature at reference conditions [K]
U~L~|Array thermal loss coefficient
V|Voltage
V~mp~|Voltage at maximum power point along IV curve
V~mp,ref~|Voltage at maximum power point along IV curve, reference conditions
V~oc~|Open-circuit voltage
V~oc,ref~|Open-circuit voltage at reference conditions [V]

### Mathematical Description

#### PV Section 1: Four-Parameter Model

The four-parameter equivalent circuit model was developed largely by Townsend [1989] and is detailed by Duffie and Beckman [1991]. The model was first incorporated into a component for the TRNSYS simulation package by Eckstein [1990]. The EnergyPlus module employs the Eckstein model for crystalline PV modules, using it whenever the short-circuit IV slope is set to zero or a positive value as modified by Ulleberg [2000]. The four parameter model assumes that the slope of the IV curve is zero at the short-circuit condition:

![](media/image7079.png)\


This is a reasonable approximation for crystalline modules. The "four parameters" in the model are *I~L,ref~*, *I~o,ref,~* , and *R~s~.* These are empirical values that cannot be determined directly through physical measurement. The EnergyPlus model calculates these values from manufactures' catalog data as discussed in the following section on calculating these parameters

The four-parameter equivalent circuit is shown in the following figure:

![Equivalent circuit in the four parameter model](media/equivalent-circuit-in-the-four-parameter.png)


*V* is the load voltage and *I* is the current flowing through the load and PV.

**Determining Performance under Operating Conditions**

The IV characteristics of a PV change with both insolation and temperature. The PV model employs these environmental conditions along with the four module constants *I~L,ref~*, *I~o,ref,~* , and *R~s~* to generate an IV curve at each timestep.

The current-voltage equation of circuit shown in the previous figure is as follows:

![](media/image7081.png)\


*R~s~* and  are constants. The photocurrent *I~L~* depends linearly on incident radiation:

![](media/image7082.png)\


The reference insolation *G~ref~*  is nearly always defined as 1000 W/m^2^. The diode reverse saturation current *I~o~* is a temperature dependent quantity:

![](media/image7083.png)\


Equation  gives the current implicitly as a function of voltage. Once *I~o~* and *I~L~* are found from Eqs. 3 and 4, Newton's method is employed to calculate the PV current. In addition, an iterative search routine finds the current (*I~mp~*)*~~* and voltage (*V~mp~*)  at the point of maximum power along the IV curve.

**Calculating I~L,ref~, I~o,ref,~ , and R~s~**

The Idf specification for the PV model include several values which must be read from manufacturers' PV module catalogs. The manufactures' values are used to determine the equivalent circuit characteristics *I~L,ref~*, *I~o,ref,~* , and *R~s~*. These characteristics define an equivalent circuit that is employed to find the PV performance at each timestep, as described previously. This section describes the algebra and calculation algorithms used to solve for the four equivalent circuit characteristics.

Three of these values, *I~L,ref~*, *I~o,ref,~* , may be isolated algebraically. The first step is to substitute the current and voltage into Eq.  at the open-circuit, short circuit, and maximum power conditions:

![](media/image7084.png)\


![](media/image7085.png)\


![](media/image7086.png)\


In each case the "-1" term is may be dropped to simplify the algebra. This approximation has little influence on the right side of the equations since because the magnitude of *I~o~*is very small, generally on the order of 10^-6^ A. Some rearrangement then yields the following three expressions which isolate *I~L,ref~*, *I~o,ref,~* :

![](media/image7087.png)\


![](media/image7088.png)\


![](media/image7089.png)\


At this point an additional equation is needed in order to determine the last unknown parameter. Taking the analytical derivative of voltage with respect to temperature at the reference open-circuit condition derives the fourth equation. This analytical value is matched to the open-circuit temperature coefficient, a catalog specification:

![](media/image7090.png)\


where

![](media/image7091.png)\


The "TRNSYS PV model" uses an iterative search routine in these four equations to calculate the equivalent circuit characteristics. The first step is to set upper and lower bounds for the series resistance parameter *R~s~*:  physical constraints require the *R~s~*~~value to lie between 0 and the value such that  = *N~s~*. The initial guess for *R~s~* is midway between these bounds.  and *I~o,ref~* are found from Eq.  and Eq. , while Eq.  gives a trivial solution for *I~L,ref~*.  The model then employs Eq.  to compare the analytical and catalog values for ~voc~. When all other variables are held constant, the analytical value for ~voc~ increases monotonically with series resistance (Townsend 1989). If the analytical voltage coefficient is less than the catalog value, the lower bound for *R~s~* is reset to the present guess value. Likewise, the upper bound is set to the current value if the calculated ~voc~ is too large. After resetting the upper or lower bound for *R~s~*, a new guess value is found by averaging the bounds. This procedure repeats until *R~s~* and  converge. Note that for *I~L,ref~*, *I~o,ref,~* , and *R~s~* are assumed to be constant and are calculated only on the first call in the simulation. Alternatively, the user may enter a known series resistance by entering a **positive** value in the IDF. In this case the iterative routine described above is skipped and Eqs. , , and  find *I~L,ref~*, *I~o,ref,~* and  directly from the given value of *R~s~*.

#### PV Section 2 : Module Operating Temperature

The PV model uses one of five methods for determining cell temperature data. The cell temperature of a PV module is important because the hotter the temperature of the panel, the lower its electrical output. The cell temperature calculation method is chosen by the user in the EnergyPlus IDF file through a parameter choice in the IDD entry called Integration and Cell Temperature Mode.

If the value of this parameter is "**Decoupled NOCT Conditions**" then the cell temperature of the PV is modeled using the method from the Duffie and Beckman (1991) for estimating cell temperature. This is based upon the standard NOCT (Nominal Operating Cell Temperature) measurements to compute the module temperature Tc at each timestep. The NOCT temperature (Tc,NOCT) is the operating temperature of the module with a wind speed of 1 m/s, no electrical load, and a certain specified insolation and ambient temperature [Beckman and Duffie, 1991]. The values for insolation GT,NOCT~~ and ambient temperature *T~a,NOCT~* are usually 800 W/m^2^ and 20º C. *~c~* is the convesion efficiency of the module, which varies with ambient conditions.  is a user-defined constant.

The equation is:

![](media/image7092.png)\


If the user specifies the "**Decoupled Ulleberg Dynamic"** mode for calculating cell temperature, then a method developed by Ulleberg is used:

![](media/image7093.png)\


In other words, the cell temperature is a function of the privious cell temperature and the thermal capacity of the PV module material.

If the user specifies "**Integrated Surface Outside Face"** for this parameter, then the temperature result from EnergyPlus's modeling of surfaces is used for the cell temperature. Also the energy exported from the surface as electricity becomes a sink in the internal source modeling for the heat transfer surface.

If the user specifies "**Integrated Transpired Collector"** for this parameter, then the temperature result for the unglazed transpired collector surfaces is used for the cell temperature. Also the energy exported from the collector surface as electricity is deprecated using a source term in the collector's temperature modeling.

If the user specifies "**Integrated Exterior Vented Cavity"** for this parameter, then the temperature result for the exterior cavity is used for the cell temperature. Also the energy exported from the baffle surface as electricity is deprecated using a source term in the baffle's temperature modeling.

#### PV Section 3 : Multi-Array Modules

The electrical calculations discussed in the sections above deal only with a single module. The EnergyPlus PV component may be used to simulate arrays with any number of modules. The IDF defines the number of modules in series (NS) and modules in parallel (NP) for the entire array. The total number of modules in the array is the product of NS and NP. When simulating a single module only, both NS and NP are set to 1. The single-module values for all currents and voltages discussed in PV Section 1 are multiplied by NP or NS to find values for the entire array. This approach neglects module mismatch losses.

With the above equations, and the assumption that the panels operate at the maximum power point, it is a direct calculation to determine DC power production. The performance of an array of identical modules is assumed to be linear with the number of modules in series and parallel. The inverter efficiency is applied linearly to derate the energy production. The inverter capacity forms a limit for power production from a PV generator. A ‘load' is passed the PV array acting as a generator and various trivial calculations compare PV production to this load. If the PV array is associated with a surface that is associated with a zone, then if the zone has any multipliers associated with it, electricity production will be multiplied accordingly.

### References

Duffie, John A. and William A. Beckman. 1991. *Solar Engineering of Thermal Processes*. New York: John Wiley & Sons, Inc..

Eckstein, Jürgen Helmut. 1990. *Detailed Modeling of Photovoltaic Components*. M. S. Thesis – Solar Energy Laboratory, University of Wisconsin, Madison: 1990.

*Ulleberg, Øystein. HYDROGEMS Component Library for TRNSYS 15 User Manual*, Institute for Energy Technology, Kjeller, Norway  **

## Sandia Photovoltaic Performance Model

The third model available in EnergyPlus for predicting the electricity generated by photovoltaics is referred to as the Sandia model. This model is based on work done at Sandia National Lab, Albuquerque, NM by David King -- with the help of many others. The model consists of a series of empirical relationships with coefficients that are derived from actual testing. Once the coefficients for a particular module are available, it is straightforward matter to use the model equations to calculate five select points on the current-voltage curve.

The implementation in EnergyPlus is also based on work done by Greg Barker (2003) for the National Renewable Energy Lab who implemented the Sandia model in FORTRAN77 as a custom type (Type101) for the TRNSYS computer program.

There are several climate and solar orientation inputs to the model that are managed elsewhere in EnergyPlus including: incident beam solar, incident diffuse solar, incidence angle of beam solar, solar zenith Angle, outdoor drybulb, wind speed, and elevation.

### Mathematical Description

This section presents the mathematical description of the Sandia model from a draft report by King et, al. (2003). The core of the model predicts the performance of a single PV module. The following nomenclature and equations summarize the Sandia model.

Table: Nomenclature for Sandia PV model

Mathematical variable|Description
---------------------|-----------
I~sc~|Short-circuit current (A)
I~mp~|Current at the maximum-power point (A)
I~x~|Current at module V = 0.5 V~oc~, defines 4th point on I-V curve
I~xx~|Current at module V = 0.5 (V~oc~ + V~mp~), defines a 5th point on the I-V curve
V~oc~|Open-circuit voltage (V)
V~mp~|Voltage at maximum-power point (V)
P~mp~|Power at maximum-power point (W)
fd|Fraction of diffuse irradiance used by module
N~s~|Number of cells in series in a module's cell-string
N~p~|Number of cell-strings in parallel in module
k|Boltzmann's constant, 1.38066E-23 (J/k)
q|Elementary charge, 1.60218E-19 (coulomb)
T~c~|Cell temperature inside module (°C)
δ(T~c~)|‘Thermal voltage' per cell at temperature T~c~, approximately 1 volt for a typical 26-cell crystalline silicon module
E~e~|‘Effective' solar irradiance
E~b~|Beam solar irradiance
E~diff~|Diffuse solar irradiance
C~0~ , C~1~|Empirical coefficients relating I~mp~ to E~e~ , C~0~ + C~1~= 1 (both dimensionless)
C~2~ , C~3~|Empirical coefficients relating V~mp~ to E~e~(C~2~ dimensionless, C~3~ is 1/V)
C~4~ , C~5~|Empirical coefficients relating I~x~ to E~e~, C~4~ + C~5~ = 1 (both dimensionless)
C~6~ , C~7~|Empirical coefficients relating I~xx~ to E~e,~C~6~ + C~7~ = 1 (both dimensionless)
n|Empirically determined ‘diode factor' for individual cells
AMa|Absolute Air Mas
AOI|Solar angle-of-incidence (degrees) from normal
f~1~(AM~a~)|Empirical polynomial function used to relate short-circuit current to the solar spectrum via air mass
f~2~(AOI)|Empirical polynomial function used to relate short-circuit current to the solar angle-of-incidence
a~0~, a~1~, a~2~, a~3~, a~4~|Empirical coefficients for f~1~(AM~a~) polynomial
b~0~, b~1~, b~2~, b~3~, b~4~,b~5~,b~6~|Empirical coefficients for f~1~(AOI) polynomial
T~o~|Reference cell temperature for rating, typically fixed at 25°C
I~sco~|Short circuit current at reference conditions
I~mpo~|Max power point current at reference conditions
V~mpo~|Voltage at max power at reference conditions
V~oco~|Open circuit voltage at reference conditions
I~xo~|Current at V = 0.5 Voc and at reference conditions
I~xxo~|Current at V = 0.5 (Vmp + Voc) and at reference conditions
α~Isc~|Normalized temperature coefficient for I~sc~ (1/°C)
α~Imp~|Normalized temperature coefficient for I~mp~ (1/°C)
β~Voc~(E~e~)|Temperature coefficient for module open-circuit-voltage as function of E~e~
β~Voco~|Temperature coefficient for module open-circuit-voltage at reference conditions
m~βVoco~|Coefficient for irradiance dependence of open-circuit-voltage-temperature coefficient, often zero (V/°C)
β~Vmp~(E~e~)|Temperature coefficient for module maximum-power-voltage as a function of E~e~
β~Vmpo~|Temperature coefficient for module maximum-power-voltage at reference conditions
m~βVmpo~|Cofficient for irradiance dependence of maximum-power-voltage-temperature coefficient, often zero (V/°C)
T~m~|PV module temperature at back suface (°C)
T~a~|Ambient outdoor drybulb temperature (°C)
E|Solar irradiance incident on module surface (W/m^2^)
WS|Wind speed at standard 10-m height (m/s)
a|Empirical coefficient relating module temperature at low wind and high solar irradiance
b|Empirical coefficient relating module temperature decrease with increasing wind speed
T~c~|Temperature of solar cell inside module (°C)
E~o~|Reference solar irradiance (1000 W/m^2^)
ΔT|Temperature difference between T~c~ and T~m~at E~o~(°C), |(This is d(Tc) in Sandia database)

The current implementation in EnergyPlus focuses on determining performance at the maximum power-point but also calculates, and reports, four other points on the I-V curve so that the data are available for analyses outside of EnergyPlus. The equations below use the module performance parameters that are available in a database provided by Sandia National Laboratory (see www.sandia.gov/pv). The following equations form the basis of the Sandia model implemented in EnergyPlus:

![](media/image7094.png)\


![](media/image7095.png)\


![](media/image7096.png)\


![](media/image7097.png)\


![](media/image7098.png)\


![](media/image7099.png)\


![](media/image7100.png)\


where,

![](media/image7101.png)\


![](media/image7102.png)\


![](media/image7103.png)\


![](media/image7104.png)\


![](media/image7105.png)\


![](media/image7106.png)\


![](media/image7107.png)\


![](media/image7108.png)\


With the above equations, and the assumption that the panels operate at the maximum power point, it is a direct calculation to determine DC power production. The performance of an array of identical modules is assumed to be linear with the number of modules in series and parallel. The inverter efficiency is applied linearly to derate the energy production. The inverter capacity forms a limit for power production from a PV generator. A ‘load' is passed the PV array acting as a generator and various trivial calculations compare PV production to this load. If the PV array is associated with a surface that is associated with a zone, then if the zone has any multipliers associated with it, electricity production will be multiplied accordingly.

The equation above for T~m~ is used to predict back-of-module temperature when the mode ‘SANDIA RACK' is selected. This would be appropriate for most rack mounted PV installations. If the user selects ‘EPLUS INTEGRATED' then the back-of-module temperature is obtained from the outside face surface temperature calculated by the full complement of Heat Balance models using Conduction Transfer Functions that is native to EnergyPlus. And energy exported from the surface is accounted for using a source/sink term that is lagged from the previous timestep (pingpong).

### References

King, D.L., Boyson, W.E., Kratochvil J.A. 2003. Photovoltaic Array Performance Model. Sandia National Laboratories, Albuquerque, NM 87185, November 2003  currently in DRAFT

Barker, G. 2003 (unknown). Predicting Long-Term Performance of Photovoltaic Arrays. Submitted as deliverable for NREL subcontract LAX-1-30480-02. Currently in DRAFT

Davis, M.W., Fanney, A.H., and Dougherty B.P. 2002. Measured Versus Predicted Performance of Building Integrated Photovoltaics. from the conference Solar 2002, Sunrise on the Reliable Energy Economy, June 15-19, 2002, Reno, NV. Available from NIST website.

King, D.L. 1996. Photovoltaic Module and Array Performance Characterization Methods for All System Operating Conditions. Sandia National Laboratory. Albuquerque, NM 87185