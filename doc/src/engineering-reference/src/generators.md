# Generators

## Internal Cumbustion Engine

The engine-driven generator model was originally developed for the BLAST program and was subsequently adapted for use in EnergyPlus. The model uses the following set of equations all of which are quadratic fits to the PLR (Part Load Ratio) of the generator.  The coefficients must be derived from manufacturers data.

![](media/image7109.png)\


The electrical load and engine generator nominal load capacity are used to compute the part load ratio.

![](media/image7110.png)\


The exhaust gas temp and flow rate are used if a stack heat exchanger is used to recover waste heat from the exhaust.  This temperature is the inlet temperature to the heat exchanger which is modeled in a UA-effectiveness form:

![](media/image7111.png)\


![](media/image7112.png)\


The exhaust flow rate is then calculated as:

![](media/image7113.png)\


where T~reference~ is the reference temperature for the fuel lower heating value, and is given as 25°C in manufacturer's data, and

![](media/image7114.png)\


Finally heat recovered from the lube oil and the water jacket are accounted for as follows:

![](media/image7115.png)\


![](media/image7116.png)\


The manufacturer must supply the recoverable water jacket heat, lube oil heat and exhaust heat and associated fuel consumption for a range of load conditions.  This data is then fit to the PLR to obtain the fifteen a,b,c,d, and e coefficients.

## Turbine Generator

The combustion turbine generator model was originally developed for the BLAST program and was subsequently adapted for use in EnergyPlus. The model uses the following set of equations all of which are equation fits to the PLR (Part Load Ratio) of the generator and the entering air temperature. The coefficients must be derived from manufacturers data. For electric power generated in Watts, the fuel input rate is calculated in J/s.

![](media/image7117.png)\


The electrical load and engine generator nominal load capacity are used to compute the part load ratio.

![](media/image7118.png)\


The temperature difference shows the deviation of ambient air temperature from the manufacturers design air temperature.

![](media/image7119.png)\


A second curve fit calculates the exhaust temperature (C) by multiplying the exhaust temperature (C) for a particular part load by a correction factor based on the deviation from design temperature.

![](media/image7120.png)\


The exhaust gas temp is used if a stack heat exchanger is used to recover waste heat from the exhaust.  This temperature is the inlet temperature to the heat exchanger which is modeled in a UA-effectiveness form:

![](media/image7121.png)\


Where the design minimum exhaust temperature is a user input to the model and the exhaust mass flow rate and the UA are fit from manufacturers data as follows:

![](media/image7122.png)\


![](media/image7123.png)\


Finally, heat recovered from the lube oil is accounted for as follows:

![](media/image7124.png)\


## Microturbine Generator

Microturbine generators are small combustion turbines that produce electricity on a relatively small scale (e.g., 25kW to 500kW). This model uses nominal performance at reference conditions along with several modifier curves to determine electrical power output and fuel use at non-reference conditions. The modifier curve coefficients must be derived from manufacturers data. Standby and ancillary power can also be taken into account.

Exhaust air energy recovery for heating water can be also be modeled. Similar to electrical power output, thermal power (heat recovery to water) output is calculated using nominal performance at reference conditions with modifier curves to account for variations at non-reference conditions. The ElectricLoadCenter:Generators and ElectricLoadCenter:Distribution objects are used to define the availability and control of the electric generators included in the simulation (ref. ElectricLoadCenter:Generators and ElectricLoadCenter:Distribution).

For each simulation time step that the generator is being asked to operate (i.e., produce electrical power as determined by the ElectricLoadCenter), the full load electrical output of the generator is determined using the user-defined reference electrical power output along with a bi-quadratic modifier curve to account for differences in the combustion air inlet temperature and elevation for the current simulation time step compared to the reference temperature and elevation (i.e., the modifier curve should evaluate to 1.0 at the reference combustion air inlet temperature and reference elevation).

![](media/image7125.png)\


![](media/image7126.png)\


where:

![](media/image7127.png) = Full load electrical power output (W)

![](media/image7128.png) = Reference Electrical Power Output, user input (W)

![](media/image7129.png) = User-defined Electric Power Modifier Curve (function of temperature and elevation) evaluated at the current combustion air inlet temperature and elevation

![](media/image7130.png) = Combustion air inlet temperature (°C)

![](media/image7131.png) = Elevation (m). This value obtained from the Location object or the weather file.

The full load electrical power output of the generator is then checked against the minimum and maximum full load electrical power outputs specified by the user:

![](media/image7132.png)\


![](media/image7133.png)\


![](media/image7134.png)  = Maximum Full Load Electrical Power Output, user input (W)

![](media/image7135.png)  = Minimum Full Load Electrical Power Output, user input (W)

The actual (operating) electrical power output from the generator is determined next based on the load requested by the Electric Load Center, the generator's minimum and maximum part-load ratios, and the ancillary power.

![](media/image7136.png)\


![](media/image7137.png)\


![](media/image7138.png)\


where:

![](media/image7139.png) = Actual (operating) electrical power output (W)

![](media/image7140.png)   = Electrical power output being requested by the Electric Load Center (W)

![](media/image7141.png)  = Ancillary Power, user input (W)

![](media/image7142.png) = Part-load ratio of the electric generator

![](media/image7143.png) = Maximum part-load ratio of the electric generator (i.e., the maximum value for the independent variable [PLR] defined in the Curve:Quadratic or Curve:Cubic object for the Electrical Efficiency Modifier Curve [function of part-load ratio])

![](media/image7144.png) = Minimum part-load ratio of the electric generator (i.e., the minimum value for the independent variable [PLR] defined in the Curve:Quadratic or Curve:Cubic object for the Electrical Efficiency Modifier Curve [function of part-load ratio])

The generator's electrical efficiency is then calculated based on the user-specified reference electrical efficiency (lower heating value [LHV] basis) and two electrical efficiency modifier curves.

![](media/image7145.png)\


![](media/image7146.png)\


![](media/image7147.png)\


where:

![](media/image7148.png)  = User-defined Electrical Efficiency Modifier Curve (function of temperature) evaluated at the current combustion air inlet temperature

![](media/image7149.png)  = User-defined Electrical Efficiency Modifier Curve (function of part-load ratio) evaluated at the current operating part-load ratio

![](media/image7150.png)  = Electrical efficiency at the current operating conditions

![](media/image7151.png)  = Reference Electrical Efficiency (LHV [lower heating value] Basis),     user input

The fuel energy consumption rate (LHV Basis) is then calculated as follows:

![](media/image7152.png)\


where:

![](media/image7153.png)  = Fuel energy consumption rate, LHV basis (W)

If *ElecEff~Operating~* is equal to zero, then *P~Operating~* and ![](media/image7154.png) ~~are set to zero. The fuel mass flow rate is then calculated.

![](media/image7155.png)\


where:

![](media/image7156.png)  = Mass flow rate of fuel being consumed by the generator (kg/s), report variable "Generator <FuelType> Mass Flow Rate [kg/s]"

*LHV* = Fuel Lower Heating Value, user input (kJ/kg)

The ancillary power is calculated next using the user-specified ancillary power and ancillary power modifier curve. The ancillary power modifier curve is a quadratic function with the generator's fuel mass flow rate as the independent variable. If an ancillary power modifier curve is not specified in the input file, the modifier is assumed to be 1.0 and the ancillary power will be constant throughout the simulation.

![](media/image7157.png)\


![](media/image7158.png)\


where:

![](media/image7159.png)  = User-defined Ancillary Power Modifier Curve (function of fuel input) evaluated at the actual fuel mass flow rate. This multiplier is assumed to be 1.0 if an ancillary power modifier curve name is not specified in the input.

![](media/image7160.png)  = Ancillary power, user input (W)

![](media/image7161.png)  = Ancillary electric power at the current fuel mass flow rate (W), report variable "Generator Ancillary Electric Power [W]".

If ancillary power is constant for the simulation (e.g., no modifier curve defined), then the calculations continue as described below. However, if an ancillary power modifier curve has been defined, then the calculations described above for *P~ElecO~~perating~*, *ElecEff~Operating~*, ![](media/image7162.png) ~~and *P~Ancillary,Operating~* are recalculated in sequence until the solution converges.

The generator's "net" electrical power output is calculated as the difference between the generator's actual power output and the ancillary electric power as follows.

![](media/image7163.png)\


where:

![](media/image7164.png)  = Generator net electric power output, report variable "Generator Produced Electric Power [W]"

The fuel energy consumption rate (higher heating value basis) for the generator is then calculated as follows:

![](media/image7165.png)\


where:

![](media/image7166.png)  = fuel energy consumption rate (W), report variables "Generator <FuelType> HHV Basis Rate [W]" and "Generator Fuel HHV Basis Rate [W]"

![](media/image7167.png) = Fuel Higher Heating Value, user input (kJ/kg)

Standby electrical power may also be modeled to simulate controls or other parasitics used by the generator. The standby power is calculated only when the generator is not operating (i.e., *Load* from the Electric Load Center is zero). If the generator operates for a given timestep (i.e., *Load* > 0.0), the standby power is set equal to 0.

![](media/image7168.png)\


where:

![](media/image7169.png)  = Standby power, user input (W)

![](media/image7170.png)  = Report variable "Generator Standby Electric Power" (W)

Report variables for electric energy produced, electric efficiency (LHV basis), fuel consumption (HHV basis), standby electric consumption and ancillary electric consumption are calculated as follows:

![](media/image7171.png)\


![](media/image7172.png)\


![](media/image7173.png)\


![](media/image7174.png)\


![](media/image7175.png)\


where:

![](media/image7176.png)  = Report variable "Generator Produced Electric Energy [J]"

![](media/image7177.png) = Report variable "Generator LHV Basis Electric Efficiency [-]"

![](media/image7178.png)  = Report variables "Generator <FuelType> HHV Basis Energy [J]" and "Generator Fuel HHV Basis Energy [J]"

![](media/image7179.png)  = Report variable "Generator Standby Electric Energy [J]"

![](media/image7180.png)  =  Report variable "Generator Ancillary Electric Energy [J]"

*TimeStepSys* = HVAC system simulation time step (hr)

In addition to calculating electric power production and fuel usage, the model is able to determine thermal power (heat recovery) output for heating water.  For this case, the water flow rate through the heat recovery heat exchanger is established first. If the Heat Recovery Water Flow Operating Mode (user input) is set to Plant Control, then the Reference Heat Recovery Water Flow Rate (user input) is requested whenever the generator operates (constant value), but the actual flow rate may be restricted by other plant components (e.g., pump). If the Heat Recovery Water Flow Operating Mode is set to Internal Control, then the requested water flow when the generator operates is determined by the Reference Heat Recovery Water Flow Rate and a flow rate modifier curve.

![](media/image7181.png)\


 where:

![](media/image7182.png) = Report variable "Generator Heat Recovery Water Mass Flow Rate [kg/s]"

![](media/image7183.png) = Reference Heat Recovery Water Flow Rate (m^3^/s), user input

![](media/image7184.png) = Density of water (kg/m^3^) at 5.05°C

![](media/image7185.png) = User-defined Heat Recovery Water Flow Rate Modifier Curve (function of temperature and power) evaluated at the current inlet water temperature and net electrical power output. This multiplier is assumed to be 1.0 if a water flow rate modifier curve name is not specified in the input.

![](media/image7186.png) = Heat recovery inlet water temperature (°C), report variable "Generator Heat Recovery Inlet Temperature [C]"

![](media/image7187.png) = Net electrical power output from the generator (W)

The methodology for determining thermal power (heat recovery to water) is similar to that used for calculating electric power production. The generator's steady-state thermal efficiency is calculated based on the user-specified reference thermal efficiency (LHV basis) and a thermal efficiency modifier curve.

![](media/image7188.png)\


![](media/image7189.png)\


where:

![](media/image7190.png) = Steady-state thermal efficiency at current conditions

![](media/image7191.png) = Reference Thermal Efficiency (LHV Basis), user input

![](media/image7192.png) = User-defined Thermal Efficiency Modifier Curve (function of temperature and elevation) evaluated at the current combustion air inlet temperature and elevation. This multiplier is assumed to be 1.0 if a thermal efficiency modifier curve name is not specified in the input.

The steady-state thermal power produced (heat recovery rate) is then calculated:

![](media/image7193.png)\


The actual (operating) thermal power is then calculated using the steady-state thermal power and three modifier curves:

![](media/image7194.png) ![](media/image7195.png)

![](media/image7196.png)\


![](media/image7197.png)\


where:

![](media/image7198.png) = Report variable "Generator Produced Thermal Rate [W]"

![](media/image7199.png) = User-defined Heat Recovery Rate Modifier Curve (function of part-load ratio) evaluated at the current operating part-load ratio. This multiplier is assumed to be 1.0 if a modifier curve name is not specified in the input.

![](media/image7200.png) = User-defined Heat Recovery Rate Modifier Curve (function of inlet water temperature) evaluated at the current inlet water temperature. This multiplier is assumed to be 1.0 if a modifier curve name is not specified in the input.

![](media/image7201.png) = User-defined Heat Recovery Rate Modifier Curve (function of water flow rate) evaluated at the current heat recovery water flow rate. This multiplier is assumed to be 1.0 if a modifier curve name is not specified in the input.

The heat recovery output water temperature is then calculated.

![](media/image7202.png)\


where:

![](media/image7203.png) = Heat recovery outlet water temperature (°C), report variable "Generator Heat Recovery Outlet Temperature [C]"

![](media/image7204.png) = Heat capacity of water (J/kg-K)

If the calculated heat recovery outlet water temperature exceeds to Maximum Heat Recovery Water Temperature (user input), then the outlet water temperature is reset to the maximum temperature (user input) and the thermal power is recalculated.

If combustion air inlet and outlet node names are specified in the input, along with exhaust air flow rate and exhaust air temperature information, then the model calculates the exhaust air conditions for each simulation time step. The exhaust air mass flow rate is first calculated based on the Reference Exhaust Air Mass Flow Rate, two modifier curves and an air density adjustment. Since fans are volumetric flow devices, the ratio of the air density at actual inlet air conditions to air density at reference inlet air conditions is used as an adjustment factor.

![](media/image7205.png) ![](media/image7206.png)

![](media/image7207.png)\


where:

![](media/image7208.png) = Exhaust air mass flow rate (kg/s)

![](media/image7209.png) = Reference Exhaust Air Mass Flow Rate (kg/s), user input

![](media/image7210.png) = User-defined Exhaust Air Flow Rate Modifier Curve (function of temperature) evaluated at the current combustion air inlet temperature. This multiplier is assumed to be 1.0 if a modifier curve name is not specified in the input.

![](media/image7211.png)  = User-defined Exhaust Air Flow Rate Rate Modifier Curve (function of part-load ratio) evaluated at the current operating part-load ratio. This multiplier is assumed to be 1.0 if a modifier curve name is not specified in the input.

![](media/image7212.png) = Density of the combustion inlet air (kg/m^3^)

![](media/image7213.png) = Density of combustion inlet air at reference conditions (kg/m^3^)

In an analogous fashion, the exhaust air temperature is calculated using the Nominal (reference) Exhaust Air Outlet Temperature and two modifier curves.

![](media/image7214.png) ![](media/image7215.png)

![](media/image7216.png)\


where:

![](media/image7217.png) = Exhaust air outlet temperature (°C)

![](media/image7218.png) = Nominal Exhaust Air Outlet Temperature (°C), user input

![](media/image7219.png) = User-defined Exhaust Air Temperature Modifier Curve (function of temperature) evaluated at the current combustion air inlet temperature. This multiplier is assumed to be 1.0 if a modifier curve name is not specified in the input.

![](media/image7220.png)  = User-defined Exhaust Air Flow Rate Rate Modifier Curve (function of part-load ratio) evaluated at the current operating part-load ratio. This multiplier is assumed to be 1.0 if a modifier curve name is not specified in the input.

The above calculations for exhaust air outlet temperature assume no heat recovery to water is being done.  If thermal power (water heating) is being produced, then the exhaust air outlet temperature is recalculated as follows:

![](media/image7221.png)\


where:

![](media/image7222.png) = Heat capacity of air at the actual combustion air inlet conditions (J/kg-K)

The exhaust air outlet humidity ratio is also calculated.

![](media/image7223.png)\


where:

![](media/image7224.png) = Exhaust air outlet humidity ratio (kg/kg)

![](media/image7225.png) = Exhaust air inlet humidity ratio (kg/kg)

![](media/image7226.png) = Enthalpy of vaporization of moisture at 16°C (J/kg)

The remaining report variables are calculated as follows.

![](media/image7227.png)\


![](media/image7228.png)\


where:

![](media/image7229.png) = Report variable "Generator Produced Thermal Energy [J]"

![](media/image7230.png) = Report variable "Generator Thermal Efficiency LHV Basis [-]"

## Micro-Cogenerator 

The input object Generator:MicroCHP provides a model that is a direct implementation of a model developed by IEA Annex 42 – The Simulation of Building-Integrated Fuel Cell and Other Cogeneration Systems (FC+COGEN-SIM).  Annex 42 was formed as a working group within the International Energy Agency (IEA) program on Energy Conservation in Buildings and Community Systems (ECBCS). A full description of the model specification can be found in the report by Subtask B of FC+COGEN-SIM with the title "Specifications for Modelling Fuel Cell and Combustion-Based Residential Cogeneration Device within Whole-Building Simulation Programs."  The "Micro CHP" model in EnergyPlus is the one referred to as "A Generic Model for Combustion-based Residential Cogeneration Devices."

The Micro CHP model is a straightforward empirical model with the exception that it is dynamic with respect to thermal heat recovery where performance is cast as a function of engine temperature.  It is also dynamic with respect to possible warm up and cool down periods that may affect the ability of the generator to deliver the requested power.  The relevant model equations are:

![](media/image7231.png)\


![](media/image7232.png)\


![](media/image7233.png)\


![](media/image7234.png)\


![](media/image7235.png)\


![](media/image7236.png)\


![](media/image7237.png)\


![](media/image7238.png)\


![](media/image7239.png)\


![](media/image7240.png)\


where,

![](media/image7241.png)  is the steady-state, part load, electrical conversion efficiency of the engine (-)

![](media/image7242.png)  is the steady-state part load, thermal conversion efficiency of the engine (-)

![](media/image7243.png)  is the mass flow rate of plant fluid through the heat recovery section [kg/s]

![](media/image7244.png)  is the bulk temperature of the plant fluid entering the heat recovery section (^o^C)

![](media/image7245.png)  is the bulk temperature of the plant fluid leaving the heat recovery section (^o^C)

![](media/image7246.png) is the steady-state electrical output of the system (W),

![](media/image7247.png)  is the gross heat input into the engine (W),

![](media/image7248.png)  is the steady-state rate of heat generation within the engine (W)

![](media/image7249.png)  is the lower heating value of the fuel used by the system (J/kg or J/kmol),

![](media/image7250.png)  is the molar fuel flow rate (kmol/s)

![](media/image7251.png)  is the mass fuel flow rate (kg/s)

![](media/image7252.png)  is the mass flow rate of air thru the engine (kg/s)

![](media/image7253.png)  is the thermal capacitance of the engine control volume (W/K)

![](media/image7254.png)  is the temperature of the engine control volume (C)

![](media/image7255.png) is the effective thermal conductance between the engine control volume and the cooling water control volume (W/K).

![](media/image7256.png)  is the effective thermal conductance between the engine control volume and the surrounding environment (W/K)

![](media/image7257.png)  is the air temperature of the surrounding environment (C)

![](media/image7258.png)  is the thermal capacitance of the encapsulated cooling water and heat exchanger shell in immediate thermal contact (J/K)

![](media/image7259.png)  is the thermal capacity flow rate associated with the cooling water (W/K)

The functional forms for ![](media/image7260.png)  and ![](media/image7261.png)  are 2^nd^ order trivariate polynomials with all of the cross terms.

EnergyPlus solves these for state values for the engine mass temperature, ![](media/image7262.png) , and the outlet plant node, ![](media/image7263.png) , in the following manner. The last two equations are interrelated but otherwise ordinary differential equations with the general form

![](media/image7264.png)\


and have analytical solution

![](media/image7265.png)\


The engine temperature at the current timestep is calculated using

![](media/image7266.png)\


![](media/image7267.png)\


The plant node outlet fluid temperature (heat recovered) is solved using

![](media/image7268.png)\


![](media/image7269.png)\


The interrelation of these two is handled by sequential substitution using an iteration scheme that alternates between calculations of ![](media/image7270.png)  and ![](media/image7271.png) .  The iteration loop exits once the energy is determined to be balanced using the following criteria:

Number of iterations > 3\\

![](media/image7272.png)\


![](media/image7273.png) ![](media/image7274.png)

The Micro CHP model has a number of different operating modes.  The operating mode for a given system timestep is determined from the mode during the previous timestep, user inputs, and high-level controls from elsewhere in EnergyPlus.  The operating mode is reported for the state at the end of each timestep.  The following table summarizes the various operating modes and the criteria for switching to a new mode for any given timestep.  The EnergyPlus implementation adds the "Off" mode to the modes specified by Annex 42 which corresponds to the unit being scheduled to be unavailable.  The difference between OFF and Standby modes determines whether or not standby power is consumed.

**Operating mode**|**Main Criteria**|**Notes**
-------------------------------|------------------------------|----------------------
Off|Availability schedule value = 0|No consumption of power or fuel.
Stand By|Availability schedule value ≠ 0|Consumes stand by power but no fuel
Warm Up|Load (thermal or electric) > 0.0|Availability schedule value ≠ 0|Time Delay < elapsed time since entering warm up mode|Engine temp < nominal engine temp|Two alternate sub-modes:  Stirling Engines use warm up by "nominal engine temperature" while Internal Combustion Engines use "time delay"|Fuel is consumed but no power is produced
Normal Operation|Load (thermal or electric) > 0.0Availability schedule value ≠ 0Time Delay > elapsed time since entering warm up mode|Engine temp >= nominal temp|Fuel is consumed and power is produced
Cool Down|Load (thermal or electric) = 0.0|Availability schedule value ≠ 0|Two alternate sub-modes where engine can be forced to go thru a complete cool down cycle before allowed to go back into warm up or normal mode.|No fuel is consumed and no power is produced.

For timesteps where the generator switches from warm up mode to normal mode in the middle of the timestep, part load ration values are calculated for the portion of the time step that the generator is in normal operation.

The engine and heat recovery thermal conditions are modeled for all modes so, for example, an engine that is off but still warm could provide some hot water recovery.

The engine model can use an arbitray fuel mixture that is defined by the user – see the entry for Generator:FuelSupply.

### References

Kelly, N. and A. Ferguson. 2007. A Generic Model Specification for Combustion-based Residential Cogeneration Devices. In *Specifications for Modelling Fuel Cell and Combustion-Based Residential Cogeneration Device within Whole-Building Simulation Programs*. I. Beausoleil-Morrison and N. Kelly editors.  Draft report of Annex 42 of the International Energy Agency ECBCS.

## Fuel Cell Cogenerator

The Generator:FuelCell input objects provides a model which is a direct implementation of a model developed by IEA Annex 42 – The Simulation of Building-Integrated Fuel Cell and Other Cogeneration Systems (FC+COGEN-SIM).  Annex 42 was formed as a working group within the International Energy Agency (IEA) program on Energy Conservation in Buildings and Community Systems (ECBCS).  A full description of the model specification can be found in the report by Subtask B of FC+COGEN-SIM with the title "Specifications for Modelling Fuel Cell and Combustion-Based Residential Cogeneration Device within Whole-Building Simulation Programs."  The "Specifications for Modelling Fuel Cell Cogeneration Devices within Whole-Building Simulation Programs."

The Annex 42 Fuel Cell model is characterized as a "grey box" empirical model where a mixture of thermodynamic principles and empirical performance maps are used to model the cogeneration performance of a fairly complex device with many individual subsystems.  In EnergyPlus, the individual subsystems are separate into individual input objects such as Generator:FuelCell:PowerModule or Generator:FuelCell:ExhaustGasToWaterHeatExchanger. The resulting model is relatively complex requiring on the order of one hundred inputs.  The model is not for the faint of heart; this model is far more involved than most component models in building simulation.  This stems from the fact that fuel cell cogenerators are complicated devices that interact with the built environment in a number of ways.  Fuel cells could drawn in gas/fuel, air, and water with as many as six separate streams.  In addition to electricity and heated water, they also give off heat in the form of convection and radiation and exhaust air out of the zone.  The devices may take a long time to start up and include storage to follow loads rather than attempt to vary the power the fuel cell.  The fuel cell model allows examining system level interactions over annual timeframes that include all the important interactions with a building's energy and comfort systems.

The Annex 42 fuel cell model is described more thoroughly in the references (see below).  Here we provide a summary of the relevant model equations which are taken from the Annex 42 model specification.  The first equation is the main energy balance for the fuel cell power module (includes the fuel reformation and fuel cell stacks).  This energy balance is used to model the enthalpy of the product gases that leave the fuel cell power module.

![](media/image7275.png)\


The remaining equations describe various terms and the balance of systems.  The electrical efficiency is modeled using:

![](media/image7276.png)\


![](media/image7277.png)\


In several places the model is formulated to offer different options.  For example, the flow rate of process air can be described either as a function of electrical power produced or the fuel flow rate.

![](media/image7278.png)\


or

![](media/image7279.png)\


![](media/image7280.png)\


![](media/image7281.png)\


![](media/image7282.png)\


![](media/image7283.png)\


![](media/image7284.png)\


![](media/image7285.png)\


![](media/image7286.png)\


![](media/image7287.png)\


or

![](media/image7288.png)\


where,

![](media/image7289.png)  is an adjustment factor,

![](media/image7290.png) ,

![](media/image7291.png)\


![](media/image7292.png)\


![](media/image7293.png)\


![](media/image7294.png)\


![](media/image7295.png)\


![](media/image7296.png)\


![](media/image7297.png)\


![](media/image7298.png)\


![](media/image7299.png)\


![](media/image7300.png)\


![](media/image7301.png)\


The Annex 42 fuel cell was implemented directly in EnergyPlus.  A sequential substitution method is used to handle all the interactions between the different subsystems.  The main energy balance drawn for the fuel cell power module is rearranged to put all the terms on the right hand side.  The enthalpy of the product gas stream is determined from this energy balance.  The Shomate equation is used to evaluate the enthalpy and specific heat of the various streams.  The EnergyPlus implementation evaluates fluid properties using the average temperature of inlet and outlet streams whereas the Annex 42 specification often uses just the inlet temperature.  The Shomate equation is inverted using the regula falsi numerical method available within EnergyPlus to calculate the temperature of the product gases from their enthalpy.

### References

Beausoleil-Morrison, I., A. Schatz, and F. Marechal. 2006. A model for simulating the thermal and electrical production of small-scale solid-oxide fuel cell cogeneration systems within building simulation programs.  *HVAC & R Research*. Amer. Soc. Heating, Ref. Air-Conditioning Eng. Inc. Atlanta, GA.

Beausoleil-Morrison, I., A. Weber, F. Marechal, and B. Griffith. 2007. Specifications for Modelling Fuel Cell Cogeneration Devices within Whole-Building Simulation Programs. In *Specifications for Modelling Fuel Cell and Combustion-Based Residential Cogeneration Device within Whole-Building Simulation Programs*. I. Beausoleil-Morrison and N. Kelly editors.  **Draft** report of Annex 42 of the International Energy Agency ECBCS.

## Custom Fuel Supply for Generators

The Generator:FuelSupply input object in EnergyPlus implements a fairly comprehensive capability to calculate properties of fuel mixtures from a description of the molar composition of all the constituents.  The fuel supply modeling is based on the specifications prepared by IEA Annex 42 for their generator models.  This modeling capability allows taking into consideration the exact gas composition of local natural gas service.  Or the user can explore the implications of an various alternative fuels such as alcohols or biogas.  An unlimited number of possible mixtures can be analyzed.

Gas phase thermochemistry calculations and data are programmed into EnergyPlus to handle the set of constituents listed in the table below.  The relevant properties of each fuel constituent, *i*, are calculated as a function of temperature using the Shomate equation:

![](media/image7302.png)\


where,

![](media/image7303.png)  is the enthalpy (J/kmol)

![](media/image7304.png)  is the molar enthalpy at the standard state (J/kmol)

![](media/image7305.png)  is the temperature of the gas (K)

A, B, C, D, E, F, H are the coefficients for the Shomate equation.

The lower heating value (LHV) of a fuel mixture is calculated from the molar fractions using:

![](media/image7306.png)\


Where,

![](media/image7307.png)\


*x* is the number of carbon atoms

*y* is the number of hydrogen atoms

Similarly, the higher heating value (HHV) of the fuel mixture is calculated using:

![](media/image7308.png)\


Where,

![](media/image7309.png)\


The Shomate coefficients used in EnergyPlus are listed in the table below.  Data source "NIST" indicates the data were directly from Chemistry WebBook.  Data source "CHEMKIN" indicates the data were developed by curve fitting library data for the CHEMKIN commercial program (which uses the Gorden-McBride polynomial rather than the Shomate formulation).

Constituent|A|B|C|D|E|F|H|Source
-----------|-|-|-|-|-|-|-|------
N~2~|26.092|8.218801|-1.976141|0.159274|0.044434|-7.98923|0.0|NIST
O~2~|29.659|6.137261|-1.186521|0.09578|-0.219663|-9.861391|0.0|NIST
Ar|20.786|2.8259E-7|-1.4642E-7|1.0921E-8|-3.6614E-8|-6.19735|0.0|NIST
CO~2~|24.99735|55.18696|-33.69137|7.948387|-0.136638|-403.6075|-393.5224|NIST
H~2~O(gas)|29.0373|10.2573|2.81048|-0.95914|0.11725|-250.569|-241.8264|CHEMKIN
H~2~O(liq)|-203.606|1523.29|-3196.413|2474.455|3.85533|-256.5478|-285.8304|NIST
H~2~|33.066178|-11.363417|11.432816|-2.772874|-0.158558|-9.9808|0.0|NIST
CH~4~|-0.703029|108.4773|-42.52157|5.862788|0.678565|-76.84376|-74.8731|NIST
C~2~H~6~|-3.03849|199.202|-84.9812|11.0348|0.30348|-90.0633|-83.8605|CHEMKIN
C~3~H~8~|-23.1747|363.742|-222.981|56.253|0.61164|-109.206|-103.855|CHEMKIN
C~4~H~10~|-5.24343|426.442|-257.955|66.535|-0.26994|-149.365|-133.218|CHEMKIN
C~5~H~12~|-34.9431|576.777|-338.353|76.8232|1.00948|-155.348|-146.348|CHEMKIN
C~6~H~14~|-46.7786|711.187|-438.39|103.784|1.23887|-176.813|-166.966|CHEMKIN
CH~3~OH|14.1952|97.7218|-9.73279|-12.8461|0.15819|-209.037|-201.102|CHEMKIN
C~2~H~5~OH|-8.87256|282.389|-178.85|46.3528|0.48364|-241.239|-234.441|CHEMKIN

### References

**Beausoleil-Morrison,** I., A. Weber, F. Marechal, and B. Griffith. 2007. Specifications for Modelling Fuel Cell Cogeneration Devices within Whole-Building Simulation Programs. In *Specifications for Modelling Fuel Cell and Combustion-Based Residential Cogeneration Device within Whole-Building Simulation Programs*. I. Beausoleil-Morrison and N. Kelly editors.  Report of Annex 42 of the International Energy Agency ECBCS.

NIST. 2003. Chemistry WebBook, National Institute of Standards and Technology Standard Reference Database Number 69, March 2003 Release, http://webbook.nist.gov/chemistry/.

Gordon S. and B.J. McBride. 1971. Computer program for calculation of complex chemical equilibrium composition, rocket performance, incident and reflected shocks and Chapman-Jouguet detonations.  NASA SP-273.

## Wind Turbine

### Overview

The wind turbine (object Generator:WindTurbine) model is intended to estimate the production of electric power of both horizontal and vertical axis wind turbine systems. Due to the cubic relationship between the wind speed and the power produced by a wind turbine, the performance of these systems is highly dependent on local wind conditions. However, differences between typical meteorological year (TMY) wind data attached to the simulation and local wind data at the site where wind turbine system is installed typically appear. The model thus estimates the air density and wind speed at the particular height of the system and factors differences between the wind speed from the TMY weather data and the local wind speed. The weather data file should thus be included in the simulation. The model also requires inputs of both an annual average wind speed that represents accurate wind profile at the location and the height where this annual average wind speed was determined.

The model calculates the power production by both horizontal axis wind turbines (HAWT) and vertical axis wind turbines (VAWT) from generic mathematical equations. Currently, a variable speed control scheme is available in EnergyPlus. The model assumes constant power generation at the rated power and the rated wind speed when the ambient wind speed is between the rated wind speed and cut out wind speed. The model does not attempt to model various types of subsystems of the entire wind turbine system such as shafts, generators and inverters due to computational convergence, time, and usability. Instead, the total system efficiency includes both conversion losses occurring during the DC-AC-DC conversion processes and delivery losses.

### Model Description

The wind turbine is modeled as a generation component that produces electricity and delivers it directly to buildings. Wind turbine components are executed at the beginning of each time step called by the HVAC manager, and the electric load will be corrected with electricity from the wind turbine. The model calculates electricity production that both HAWTs and VAWTs produce from general mathematical equations. The model then passes the electricity to the electric load center in EnergyPlus at each HVAC system time step. The electric load center then determines the whole building electrical demand, deducting the power output by wind turbine along with any power production by photovoltaic components from the total electrical demand requested in the building. Excessive production of electricity greater than needed from wind turbine along with photovoltaic components is either sold or stored as the user specifies.

### Input and Data

The user must input the required information according to the IO Reference Manual (ref: Generator:WindTurbine). The wind turbine model in EnergyPlus requires a unique identifying name and an availability schedule. The schedule name must refer to a valid schedule type (range 0-1) and contain values of fractional operation. Various inputs describes wind turbine configuration such as rotor type, control type, rotor diameter, overall height, and number of blades. Rated data provided in the manufacturer's literature determines overall electricity production by using generic equations. These inputs include rated power, rated wind speed, cut in wind speed, cut out wind speed, fraction system efficiency, and maximum tip speed ratio. Two inputs such as annual local average wind speed and height for local average wind speed define local wind conditions at a specific location so that the model predicts wind speed and air density at the height of the wind turbine at the location.

HAWT systems need a maximum power coefficient and empirical power coefficient parameters *C~1~* through *C~6~*. The maximum power coefficient controls overall performance of the rotor which defines the power extraction efficiency from the ambient air stream. The model predicts power generation more accurately when the user inputs the empirical power coefficients *C~1~* through *C~6~* for a specific wind turbine. Three additional inputs for VAWT system are required. The model requests blade lift and drag coefficients corresponding to the maximum tip speed ratio so that tangential and normal force coefficients are obtained. Blade chord area is also requested for calculating forces on a single blade.

### Simulation and Control

Given the inputs needed, the wind turbine model analyzes local wind speed since wind speed is critical to determine the production of electricity of wind turbine systems. To minimize uncertainty involved with wind data, it factors differences between annual average wind speed from weather data and local annual average wind speed at the particular height of the local meteorological station. It reads annual average wind speed from statistical weather file that is automatically copied during the simulation. Note that the user should attach a weather data to the simulation (for a design day simulation, the wind speed data from the design day description is used). This annual average wind speed is converted into a wind speed at the height at which local annual average wind speed that the user inputs is measured and then factored as:

![](media/image7310.png)\


![](media/image7311.png)\


Note that the wind speed factor *F~v~* of 1.0 is assigned, if the user does not input the local wind conditions or the weather data file is not attached to the simulation.

The local air density can be obtained by using EnergyPlus psychrometric functions as follows:

![](media/image7312.png)\


![](media/image7313.png)\


![](media/image7314.png)\


![](media/image7315.png)\


The model converts TMY wind speed into a wind speed at the specific height of the wind turbine rotor (*V~z~*) at the location by using EnergyPlus function as:

![](media/image7316.png)\


The local wind speed at the rotor height (*V~L~~ocal~*) at the location is thus:

![](media/image7317.png)\


The tip speed ratio (TSR) can be obtained as:

![](media/image7318.png)\


### Horizontal Axis Wind Turbine

Once the local wind speed and air density are determined, the model calculates electrical power produced by a wind turbine system according to the rotor type. For HAWT systems, two different approximations are available. The model uses an analytical approximation when the user inputs all six empirical coefficient parameters *C~1~* through *C~6~*. The equations that define the analytical approximation are:

![](media/image7319.png)\


![](media/image7320.png)\


Note that the model allows changing the rotor speed to meet the maximum tip speed ratio at each time step. That is, the tip speed ratio calculated is limited by the maximum tip speed ratio. Similarly, the power coefficient calculated is also set to the maximum if the calculated is greater than the maximum.

Assuming maximum of rotor angle, i.e. zero, the power production of the wind turbine is thus obtained by:

![](media/image7321.png)\


The model assumes the simple approximation, if any of empirical power coefficient parameters is not input. The power production of wind turbine is directly obtained from the kinetic energy equation:

![](media/image7322.png)\


Here, the model defines *P~W~* as rated power output at the rated wind speed, if either the power production of wind turbine or local wind speed is greater than the rated power or rated wind speed, respectively. The power coefficient in this particular case is thus recalculated as:

![](media/image7323.png)\


The overall power production that includes conversion loss and delivery loss is thus:

![](media/image7324.png)\


### Vertical Axis Wind Turbine

![Flow velocities and force diagram of a single blade airfoil(Adapted from Mazharul Islam et al., 2008)](media/flow-velocities-and-force-diagram-of-a-single.jpeg)


If tip speed ratio at the time step is greater than the maximum tip speed ratio, the model estimates actual rotor speed at the time step as:

![](media/image7326.png)\


The model then employs general mathematical expressions for the aerodynamic analysis of straight-bladed Darrieus-type VAWTs to predict the power production by VAWTs. Assuming quasi-steady state, the induced wind speed (*V~a~*) on the rotor is defined as:

![](media/image7327.png)\


The chordal velocity (*V~c~*), normal velocity (*V~n~*), and relative flow velocity (*W*) as shown in figure above can be expressed as:

![](media/image7328.png)\


![](media/image7329.png)\


![](media/image7330.png)\


The expression for the non-dimensional angle of attack (*α*) with no consideration of blade pitch is:

![](media/image7331.png)\


The tangential and normal force coefficients, respectively, are expressed as:

![](media/image7332.png)\


![](media/image7333.png)\


The net tangential and normal forces are obtained from the following expressions:

![](media/image7334.png)\


![](media/image7335.png)\


Average tangential force on a single blade can be defined as:

![](media/image7336.png)\


Substituting the values of *F~t~* and arranging tangential force on azimuth angle, ![](media/image7337.png) , equation above can be written as:

![](media/image7338.png)\


The expression of the total torque for the number of blades is defined as:

![](media/image7339.png)\


The power production of wind turbine is thus:

![](media/image7340.png)\


The model also defines *P~W~* as the rated power output at the rated wind speed, if either the power production of wind turbine or local wind speed is greater than the rated power.

The overall power production delivered from a wind turbine system is thus:

![](media/image7341.png)\


Table: Nomenclature for Wind Turbine model

Variable|Description|Units
--------|-----------|-----
A~R~|swept area of rotor|m2
A~C~|blade chord area|m2
a|site wind exponent, 0.22|
a~met~|wind exponent, 0.14|
C~d~|blade drag coefficient, 0.9|
C~l~|blade lift coefficients, 0.05|
C~n~|normal force coefficient|
C~p~|power coefficient (performance coefficient)|
C~t~|tangential force coefficient|
C~1-6~|empirical power coefficient parameters|
g~o~|standard gravity|m/s2
F~n~|normal force in radial direction|N.m (J)
F~t~|tangential force|N.m (J)
F~ta~|average tangential force|N.m (J)
F~v~|wind speed factor|
H|height of local wind speed measurement|m
H~met~|height of turbine, 10|m
N|number of blade|
P|overall power production delivered to building  |W
P~Local~|outdoor static air pressure at rotor height|Pa
P~W~|wind turbine power produced |W
Q|overall torque|N.m
R|turbine radius|m
T~Local~|local air temperature at rotor height |℃
v|ambient wind speed |m/s
v~1~|upstream wind speed |m/s
v~2~|wind speed on the turbine |m/s
v~3~|downstream wake velocity|m/s
V~a~|induced velocity|m/s
V~AnnualAvg~|annual average wind speed from TMY weather data|m/s
V~Local~|local wind speed at the location of the system|m/s
V~LocalTMY~|annual average wind speed converted at the local station height|m/s
V~c~|chordal velocity component|m/s
V~n~|normal velocity component|m/s
V~Z~|wind speed adjusted at rotor height |m/s
W|relative flow velocity|m/s
Z|height of wind turbine rotor|m
α|blade angle of attack|deg
θ|azimuth angle in VAWT and pitch angle in HAWT|deg
ρ~Local~|local density of air at rotor height|kg/m3
ω|angular velocity of turbine |rad/s
ω~Local~|local humidity ratio at rotor height |kg-H2O/kg-air
x|exponent, 1.5|
λ|tip speed ratio|
λ~i~|tip speed ratio at the i^th^ pitch |
δ~met~|wind boundary layer thickness of meteorological station, 270 |m
δ|site boundary layer thickness, 370 |m
η|wind turbine system efficiency |

### References

Siegfried Heier. 2006. Grid Integration of Wind Energy Conversion Systems, Second Edition. Wiley, Chap. 2, pp.31-44.

Mazharul Islam, David S.K. Ting and Amir Fartaj. 2008. Aerodynamic Models for Darrieus-type Sraight-bladed Vertical Axis Wind Turbines. Renewable & Sustainable Energy Reviews, Volume 12, pp.1087-1109.

ASHRAE. 2005. Handbook of Fundamentals, pp 16.3-16.4, Atlanta: ASHRAE.