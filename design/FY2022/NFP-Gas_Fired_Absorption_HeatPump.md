Equation-Fit Based Gas Fired Absorption Heat Pump (GAHP) Module
=============================================================

**J. Yuan & M.J. Witte, GARD Analytics**

 - Original Date: April 18, 2021
 - Revised: 


## Justification for New Feature ##
The goal of the proposed new feature development is to add a new EnergyPlus model for a gas-fired ammonia-water absorption heat pump which represents a potential new product line for space heating and domestic hot water (DHW) heating applications [1]. The equipment is intended to used in cold climates as a heat pump to provided space-heating to the space and hot water heating for domestic hot water. Currently, the heat pump is modeled by the feature requester using the "PlantComponent:UserDefined" object to connect the EMS programs into the plant simulation. This method is not standardized and lack scalability for different connections and equipment configurations. The proposed new feature will address the model needs for convenient and scalable modeling of such gas-fired absorption heatpump (GAHP) systems. 

Current EnergyPlus has capabilities to model an absorption chillerheater and a heat pump separately. For example, an existing EnergyPlus object that can provide the basis for the absorption chiller is ChillerHeater:Absorption:DirectFired; and an existing EnergyPlus object that can provide the heat pump modeling capability is HeatPump:PlantLoop:EIR:Cooling/Heating. 

ChillerHeater:Absorption:DirectFired: This is an DOE-2 type direct-fired absorption chiller models that allows heating and cooling connections (and operations) at the same time. Direct fired gas absorption chiller-heater using performance curves. This component models fuel-fired heating and cooling with a possible air-cooled condenser option. However, the heating mode is intended to represent simply a boiler, but not a heat pump. The fundamental difference is that the general ChillerHeater:Absorption:DirectFired has an independent condenser units to reject heat; however, the gas-fired heat pump to be modeled in this study actually will have heat extracted from the condenser and feed to the hot-water or domestic hot water demand loop. 

HeatPump:PlantLoop:EIR:Cooling/Heating: An EIR formulated air-to-water (or water-to-water) heat pump model. This component can model a general -purpose heat pump, either air-to-water or water-to-water. However, it does not have a fuel type option; and the heating mode currently does not allow defrost operations. Further, parameter input options are more general for a traditional electric heat pump configuration; but not specifically to a gas-fired absorption heat pump to be modeled in the current NFP. In order to model a gas-fired absorption heatpump, many input fields, including a set of different curves, need to be expanded or allow alternatives in order to fit GAHP in a the current general-purpose heat pump model. 

Further, for both of the two existing models ChillerHeater:Absorption:DirectFired and HeatPump:PlantLoop:EIR:Cooling/Heating---since these two models are for a general purpose absorption chiller heater or for a general purpose heat pump model (for both heating and cooling), they can be cumbersome in specifying, sizing, operations, and controls for the GAHP equipment modeling which is mainly in heating mode only. Further, there are more features that is applicable to the gas-fired absorption heat pump but not available in the current ChillerHeater:Absorption:DirectFired and HeatPump:PlantLoop:EIR:Cooling/Heating models, such as defrost options, flow control method, and standing by electricity options. 


## E-mail and Conference Call Conclusions ##
### E-mail Communications ###
### Conference Call Communications ###


## Overview ##

The proposed new feature will implement a gas-fire absorption heat pump model that would use equation-fit model with manufacture provided parameters. Currently the requester (GTI) uses a customized plant system component and EMS programs to implement the gas-fired absorption heat pump. The implemented model [1] is as follows: 

$$ GAHP Heating Capacity = Rated Heating Capacity \times CAPFT $$

$$ CAPFT = a1 \times Tamb + b1 \times Tamb^{2} +c1 \times Tret + d1 \times Tret^{2} + e1 \times Tret \times Tamb + f1 $$

$$ PLR = \frac{Load}{Capacity} (0.2 <= PLR <= 1) $$

$$ Fuel Usage = \frac{Load \times EIRFT \times EIRFPLR \times EIRDEFROST}{CRF} $$

$$ EIRFT = a2 \times Tamb + b2 \times Tamb^{2} +c2 \times Tret + d2 \times Tret^{2} + e2 \times Tret \times Tamb + f2 $$

$$ EIRFPLR = 0.0865 \times PLR^{2} - 0.006 \times PLR + 0.9814, for 0.25 <= PLR <=1 $$

$$ EIRDEFROST = -0.0011 \times Tamb^{2} - 0.006 \times Tamb + 1.0317, for -8.89^{\circ}C <= Tamb <= 3.33^{\circ}C $$

$$ CRF = 0.4167 \times CR + 0.5833 $$

$$ CR = \frac{PLR}{PLR_{min}}, for 0.2 <= PLR <= 0.25 $$


## Approach ##

The following new IDF object will be added to EnergyPlus to implement the absorption heat pump model, based on a new input data dictionary (IDD) definition has been proposed by GTI (e-mail from Alex Fridlyand on Nov 29, 2021), with a few addition of essential entries. The name of the object is HeatPump:AirToWater:FuelFired:Heating. 

### IDF Object for HeatPump:AirToWater:FuelFired:Heating

```
HeatPump:AirToWater:FuelFired:Heating,
    AbsorptionHeatPumpName1, !- Name
    Inlet_Node_Name, !-Water Inlet Node Name
    Outlet_Node_Name, !-Water Outlet Node Name
    Air_Source_Node_Name, ! Air Source Node Name
    NaturalGas, !-Fuel Type
    General, !-End-Use Subcategory
    10000, !-Nominal Heating Capacity
    2, !-Design Flow Rate
    60, !-Design Supply Temperature
    11.1, !-Design Temperature Lift
    1.1, ! Sizing Factor
    NotModulated, !-Flow Mode
    DryBulb, !-Outdoor Air Temperature Curve Input Variable
    EnteringCondenser, !-Water Temperature Curve Input Variable
    CAPFT_CurveName, !-Normalized Capacity Function of Temperature Curve Name
    Fuel_EIRFT_CurveName, !-Fuel Energy Input Ratio Function of Temperature Curve name
    Fuel_EIRFPLR_CurveName, !-Fuel Energy Input Ratio Function of PLR Curve Name
    0.1, !-Minimum Part Load Ratio
    1, !-Maximum Part Load Ratio
    Fuel_EIRDefrost_CurveName, !-Fuel Energy Input Ratio Defrost Adjustment Curve name
    5, !-Defrost Operation Minimum Time
    30, !-Defrost Operation Maximum Time
    CRF_CurveName, !-Cycling Ratio Factor Curve Name
    900, !-Nominal Auxiliary Electric Power
    Aux_Elec_EIRFT_CurveName, !-Auxiliary Electric Energy Input Ratio Function of Temperature Curve Name
    Aux_EIRPLR_CurveName, !-Auxiliary Electric Energy Input Ratio Function of PLR Curve Name
    300; !-Standby Electric Power
```

### IDD entry for HeatPump:AirToWater:FuelFired:Heating ###

A HeatPump:AirToWater:FuelFired:Heating object defines the basic inputs for an equation-fit gas-fired absorption heat pump. 
```
HeatPump:AirToWater:FuelFired:Heating,
  \memo The object defines a gas-fired absorption heat pump based on equation-fit models.
  \min-fields 10
  A1 , \field Name
       \required-field
       \note Name of the gas fired absorption heat pump system system
  A2 , \field Water Inlet Node Name
       \required-field
       \type node
       \note Inlet node name of the water side connection
  A3 , \field Water Outlet Node Name
       \required-field
       \type node
       \note Outlet node name of the water side connection
  A4 , \field Air Source Node Name
       \type object-list
       \object-list OutdoorAirNodeNames
       \note This is the air source node name, which is the evaporator side of the heat pump in heating mode.
       \note Enter the name of an OutdoorAir:Node object.
  A5 , \field Fuel Type
       \required-field
       \type choice
       \key NaturalGas
       \key Propane
       \key FuelOilNo1
       \key FuelOilNo2
       \key Diesel
       \key Gasoline
       \key Coal
       \key OtherFuel1
       \key OtherFuel2
       \default NaturalGas
       \note Fuel Type (NaturalGas, Propane, Gasoline, Diesel etc.)
  A6 , \field End-Use Subcategory
       \type alpha
       \retaincase
       \default General
       \note Any text may be used here to categorize the end-uses in the ABUPS End Uses by Subcategory table.
  N1 , \field Nominal Heating Capacity
       \autosizable
       \minimum> 0
       \note Nominal Heating Capacity in [W] (autosizeable)
  N2 , \field Design Flow Rate (autosizeable)
       \autosizable
       \minimum> 0
       \note Design Flow Rate in m3/s (autosizeable)
  N3 , \field Design Supply Temperature
       \default 60
       \note Design Supply Temperature in [degree C]
  N4 , \field Design Temperature Lift
       \autosizable
       \default 11.1
       \note Design Temperature Lift in [degree C]
  N5 , \field Sizing Factor
       \minimum 1.0
       \default 1.0
       \note Sizing Factor for equipment sizing
  A7 , \field Flow Mode
       \required-field
       \type choice
       \key NotModulated
       \key ConstantFlow
       \key LeavingSetpointModulated
       \default NotModulated
       \note Flow Mode for the water side of the gas-fired absorption heat pump
  A8 , \field Outdoor Air Temperature Curve Input Variable
       \required-field
       \type choice
       \key DryBulb
       \key WetBulb
       \default DryBulb
       \note Outdoor air temperature curve input variable;
       \note The options are Outdoor Air Dry Bulb or Wet Bulb temperature for curves
  A9 , \field Water Temperature Curve Input Variable
       \required-field
       \type choice
       \key EnteringCondenser
       \key LeavingCondenser
       \default EnteringCondenser
       \note Water Temperature curve input variable - Condenser Entering or Leaving Water Temperature for curves
  A10, \field Normalized Capacity Function of Temperature Curve Name
       \required-field
       \type object-list
       \object-list BivariateFunctions
       \note: CAPFT - Normalized Capacity Function of Temperature Curve name,
       \note which is a biquadratic curve or a lookup table.
  A11, \field Fuel Energy Input Ratio Function of Temperature Curve name
       \required-field
       \type object-list
       \object-list BivariateFunctions
       \note EIRFT - Fuel Energy Input Ratio Function of Temperature Curve name,
       \note which is a biquadratic curve or a lookup table.
  A12, \field Fuel Energy Input Ratio Function of PLR Curve Name
       \required-field
       \type object-list
       \object-list UnivariateFunctions
       \note EIRFPLR - Fuel Energy Input Ratio Function of Part Load Ratio(PLR) Curve name,
       \note which is a cubic curve or a lookup table.
  N6 , \field Minimum Part Load Ratio
       \minimum> 0.0
       \maximum 1.0
       \default 0.1
       \note Minimum Part Load Ratio (PLR) in between 0 and 1
  N7 , \field Maximum Part Load Ratio
       \minimum> 0.0
       \default 1.0
       \note Maximum Part Load Ratio (PLR) in between 0 and 1
  A13, \field Fuel Energy Input Ratio Defrost Adjustment Curve name
       \type object-list
       \object-list UnivariateFunctions
       \note EIRDEFROST - Energy Input Ratio Defrost Adjustment Curve name,
       \note which is a cubic curve or a lookup table function of Outdoor Air Temperature.
  N8 , \field Defrost Operation Minimum Time
       \minimum 0
       \default 5
       \note Defrost operation minimum time Tmin in [minute]
  N9, \field Defrost Operation Maximum Time
       \minimum> 0
       \default 30
       \note Defrost operation maximum time Tmax in [minute] 
  A14, \field Cycling Ratio Factor Curve Name
       \type object-list
       \object-list UnivariateFunctions
       \note Cycling Ratio Factor (CRF) Curve Name,
       \note which is a cubic curve or a lookup table function of Cycling Ratio (defined as = PLR/PLRmin).
  N10, \field Nominal Auxiliary Electric Power
       \minimum 0
       \note Nominal Auxiliary Electric Power in [W]
  A15, \field Auxiliary Electric Energy Input Ratio Function of Temperature Curve Name
       \type object-list
       \object-list BivariateFunctions
       \note Auxiliary Electric EIRFT - Auxiliary Electric Energy Input Ratio Function of Temperature Curve Name,
       \note which is a biquadratic curve or a lookup table.
       \note which accounts for system internal fans, pumps, and electronics
  A16, \field Auxiliary Electric Energy Input Ratio Function of PLR Curve Name
       \type object-list
       \note Auxiliary Electric EIRFPLR - Auxiliary Electric Energy Input Ratio Function of PLR (Part Load Ratio) Curve Name,
       \note which is a cubic curve or a lookup table.
  N11;  \field Standby Electric Power
       \minimum 0
       \note Standby Electric Power in [W]
```

### Output Variables ###

Output variables will reported for operation conditions, such as heating energy (rate), fuel energy (rate), electricity energy (rate)load, PLR, flow rate values. 

The newly added output variables are listed as follows:

```
HVAC,sum,Gas-fired Absorption HeatPump Heating Energy [J]
HVAC,average,Gas-fired Absorption HeatPump Heating Power Rate [W]
HVAC,sum,Gas-fired Absorption HeatPump Heating Fuel Energy Usage [J]
HVAC,average,Gas-fired Absorption HeatPump Heating Fuel Power Rate [W]
HVAC,sum,Gas-fired Absorption HeatPump Heating Electricity Energy Usage [J]
HVAC,average,Gas-fired Absorption HeatPump Heating Electricity Power Rate [W]
HVAC,average,Gas-fired Absorption HeatPump Runtime Fraction [];
HVAC,average,Gas-fired Absorption HeatPump Volumetric Flow Rate [m3/s];
HVAC,average,Gas-fired Absorption HeatPump Mass Flow Rate [m3/s];
HVAC,average,Gas-fired Absorption HeatPump Inlet Temperature [C]
HVAC,average,Gas-fired Absorption HeatPump Outlet Temperature [C]
```

### Sizing ###

The sizing for a couple of design parameters, such as nominal heating capacity, and the design flow rate.

## Testing and Validation ##

One or two unit tests will be developed to verify that: 
1. the new input objects can be processed correctly;
2. the model simulation system results, output variables, and reports are working properly.

## Example File and Transition Changes ##

One new example file will be added to the test suite to demonstrate how to use this feature. 

Since the feature is based on completely newly added blocks, an older version would not carry the feature. Therefore a transition program is not needed for converting from earlier versions.


## Input Output Reference Documentation ##

The proposed new feature development will add the following contents to the Input Output Reference document:

### HeatPump:AirToWater:FuelFired:Heating Input Fields ###

The HeatPump:AirToWater:FuelFired:Heating will take the following input fields:

#### Field: Name ####
The name of the gas-fired absorption heatpump system.

#### Fiedl: ####

#### Field: Water Inlet Node Name ####

Note that here the "water" is literally mean "water". Rather it is the working fluid in the connected water side plant loop, which is usually some kind of glycol-water solution. 

#### Field: Fuel Type ####
This alpha field determines the type of fuel that the chiller uses. The default is NaturalGas. Valid values are NaturalGas, Propane, Diesel, Gasoline, FuelOilNo1, FuelOilNo2, OtherFuel1, OtherFuel2.

#### Field: Sizing Factor ####
This optional numeric field allows the user to specify a sizing factor for this component. The sizing factor is used when the component design inputs are autosized: the autosizing calculations are performed as usual and the results are multiplied by the sizing factor. For this component the inputs that would be altered by the sizing factor are: Nominal Capacity and Design Flow Rate. Sizing factor allows the user to size a component to meet part of the design load while continuing to use the autosizing feature.

#### Field: ####

#### Field: Flow Mode ####
This choice field determines how the gas-fired absorption heatpump operates with respect to the intended fluid flow through the device. There are three different choices for specifying operating modes for the intended flow behavior: "NotModulated," "ConstantFlow," and "LeavingSetpointModulated." NotModulated is useful for either variable or constant speed pumping arrangements where the boiler is passive in the sense that although it makes a nominal request for its design flow rate it can operate at varying flow rates. ConstantFlow is useful for constant speed pumping arrangements where the heat pump's request for flow is more strict and can increase the overall loop flow. LeavingSetpointModulated changes the heatpump model to internally vary the flow rate so that the temperature leaving the heatpump matches a setpoint. In all cases the operation of the external plant system can also impact the flow through the heatpump---for example if the relative sizes and operation are such that flow is restricted and the requests cannot be met. The default, if not specified, is NotModulated.

#### Field: Temperature Curve Input Variable 2 ####
This field sets the second independent variable in the three temperature dependent performance curves to either the leaving or entering condenser water temperature. Manufacturers express the performance of their heat pumps using either the leaving condenser water temperature or the entering condenser water temperature. Valid choices for this field are: LeavingCondenser or EnteringCondenser. It is important that the performance curves and this field are consistent with each other. The default is EnteringCondenser.

#### Field: Maximum Part Load Ratio ####
A positive fraction that represents the maximum heating output possible when operated continually at rated temperature conditions divided by the available heating capacity at those same conditions. If greater than 1.0, the equipment is typically thought of as capable of being overloaded. The default is 1.0.


#### Field: ####

An example of the HeatPump:AirToWater:FuelFired:Heating input object is like this:

```
HeatPump:AirToWater:FuelFired:Heating,
    AbsorptionHeatPumpName1, !- Name
    Inlet_Node_Name, !-Water Inlet Node Name
    Outlet_Node_Name, !-Water Outlet Node Name
    Air_Source_Node_Name, ! Air Source Node Name
    NaturalGas, !-Fuel Type
    General, !-End-Use Subcategory
    10000, !-Nominal Heating Capacity
    2, !-Design Flow Rate
    60, !-Design Supply Temperature
    11.1, !-Design Temperature Lift
    1.1, ! Sizing Factor
    NotModulated, !-Flow Mode
    DryBulb, !-Outdoor Air Temperature Curve Input Variable
    EnteringCondenser, !-Water Temperature Curve Input Variable
    CAPFT_CurveName, !-Normalized Capacity Function of Temperature Curve Name
    Fuel_EIRFT_CurveName, !-Fuel Energy Input Ratio Function of Temperature Curve name
    Fuel_EIRFPLR_CurveName, !-Fuel Energy Input Ratio Function of PLR Curve Name
    0.1, !-Minimum Part Load Ratio
    1, !-Maximum Part Load Ratio
    Fuel_EIRDefrost_CurveName, !-Fuel Energy Input Ratio Defrost Adjustment Curve name
    5, !-Defrost Operation Minimum Time
    30, !-Defrost Operation Maximum Time
    CRF_CurveName, !-Cycling Ratio Factor Curve Name
    900, !-Nominal Auxiliary Electric Power
    Aux_Elec_EIRFT_CurveName, !-Auxiliary Electric Energy Input Ratio Function of Temperature Curve Name
    Aux_EIRPLR_CurveName, !-Auxiliary Electric Energy Input Ratio Function of PLR Curve Name
    300; !-Standby Electric Power
```

## Input Description ##

See the Input Output Reference documentation contents update above.


## Outputs Description ##

The following output will be added the to the new gas-fired absorption heatpump system: 

```
HVAC,sum,Gas-fired Absorption HeatPump Heating Energy [J]
HVAC,average,Gas-fired Absorption HeatPump Heating Power Rate [W]
HVAC,sum,Gas-fired Absorption HeatPump Heating Fuel Energy Usage [J]
HVAC,average,Gas-fired Absorption HeatPump Heating Fuel Power Rate [W]
HVAC,sum,Gas-fired Absorption HeatPump Heating Electricity Energy Usage [J]
HVAC,average,Gas-fired Absorption HeatPump Heating Electricity Power Rate [W]
HVAC,average,Gas-fired Absorption HeatPump Runtime Fraction [];
HVAC,average,Gas-fired Absorption HeatPump Volumetric Flow Rate [m3/s];
HVAC,average,Gas-fired Absorption HeatPump Mass Flow Rate [m3/s];
HVAC,average,Gas-fired Absorption HeatPump Inlet Temperature [C]
HVAC,average,Gas-fired Absorption HeatPump Outlet Temperature [C]
```

## Engineering Reference ##

The fundamental methods and equations used for the model, as well as the original references will be added to the the Engineering Reference.

## Designs ##

### getGasFireAbsorptionHeatPumpInput() ###

A function to process the input fields for the HeatPump:AirToWater:FuelFired:Heating object. It will read input information for the system's connection, equipment sizing, and operation mode.

### SizeGasFiredAbsorptionHeatPump() ###

This function will be added to size the system's nominal heat capacity and design flow rate.

### SimGasFiredAbsorptionHeatPump() ###

### ReportGasFiredAbsorptionHeatPumpSystem() ###

The function is for reporting the variables related to the gas-fired absorption heatpump system, such as the heating energy (or rate), electricity usage (rate), and exhaust fans' flow rates, energy usages, and pressure drops.

### Other possible functions ###

#### Initialize() ####

#### getDesignCapcities() ####

## Reference ##

[1] Fridlyand, Alex; Glanville, Paul; and Garrabrant, Michael, 2021. "Pathways to Decarbonization of Residential Heating," International High Performance Buildings Conference. Paper 354. https://docs.lib.purdue.edu/ihpbc/354).

