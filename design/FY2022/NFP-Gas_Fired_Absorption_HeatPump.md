Equation-Fit Based Gas Fired Absorption Heat Pump Module
=============================================================

**J. Yuan & M.J. Witte, J. Glazer, GARD Analytics**

 - Original Date: April 18, 2021
 - Revised: 


## Justification for New Feature ##

Current EnergyPlus has capabilities to model an absorption chillerheater and a heat pump separately. For example, an existing EnergyPlus object that can provide the basis for the absorption chiller is ChillerHeater:Absorption:DirectFired; and an existing EnergyPlus object that can provide the heat pump modeling capability is HeatPump:PlantLoop:EIR:Cooling/Heating. 

ChillerHeater:Absorption:DirectFired: Direct fired gas absorption chiller-heater using performance curves. This component models fuel-fired heating and cooling with an air-cooled condenser option. However, the heating mode is simply a boiler, not a heat pump.

HeatPump:PlantLoop:EIR:Cooling/Heating: An EIR formulated water-to-water (or air-to-water) heat pump model. This component can model and air-to-water heat pump, but does not have a fuel type option, and the heating mode does not have any defrost options.

The goal of the proposed new feature development is to add a new EnergyPlus model for an ammonia-water absorption heat pump. Previous modeling of this equipment using EnergyPlus EMS is described in [1]. Here, the heat pump was modeled using the "PlantComponent:UserDefined" object to connect the EMS programs into the plant simulation. This method is not standarized and lack scalability for different connections and equipment configurations. The proposed new feature will address the model needs for convinient and scacale modeling of such gas-fired absorption heatpump systems. 

## E-mail and Conference Call Conclusions ##
### E-mail Communications ###
### Conference Call Communications ###


## Overview ##

The proposed new feature will implement a chillerheat absorption heat pump model that would use equation-based model with manufacture provided parameters. Currently the requester (GTI) uses a customized plant system component and EMS programs to implement the gas-fired absorption heat pump. The implemented model [1] are as follows: 

$$ GAHP Heating Capacity =Rated Heating Capacity \times CAPFT $$

$$ CAPFT = a1 \times Tamb + b1 \times Tamb^{2} +c1 \times Tret + d1 \times Tret^{2} + e1 \times Tret \times Tamb + f1 $$

$$ PLR = \frac{Load}{Capacity} (0.2 <= PLR <= 1) $$

$$ Gas Usage = \frac{Load \times EIRFT \times EIRFPLR \times EIRDEFROST}{CRF} $$

$$ EIRFT = a2 \times Tamb + b2 \times Tamb^{2} +c2 \times Tret + d2 \times Tret^{2} + e2 \times Tret \times Tamb + f2 $$

$$ EIRFPLR = 0.0865 \times PLR^{2} - 0.006 \times PLR + 0.9814, for 0.25 <= PLR <=1 $$

$$ EIRDEFROST = -0.0011 \times Tamb^{2} - 0.006 \times Tamb + 1.0317, for -8.89^{\circ}C <= Tamb <= 3.33^{\circ}C $$

$$ CRF = 0.4167 \times CR + 0.5833 $$

$$ CR = \frac{PLR}{PLR_{min}}, for 0.2 <= PLR <= 0.25 $$

## Approach ##

The following new IDF object will be added to EnergyPlus to implement the absorption heat pump model, based on a new input data dictionary (IDD) definition has been proposed by GTI (e-mail from Alex Fridlyand on Nov 29, 2021), with a few addition of essential entries. The name of the object is HeatPump:AirToWater:FuelFiredEquationFit:Heating. 

### IDF Object for HeatPump:AirToWater:FuelFiredEquationFit:Heating

```
HeatPump:AirToWater:FuelFiredEquationFit:Heating,
    AbsorptionHeatPumpName1, !- Name
    Inlet_Node_Name, !-Heat Pump Inlet Node Name
    Outlet_Node_Name, !-Heat Pump Outlet Node Name
    NaturalGas, !-Fuel Type (NaturalGas, Propane, Diesel, Gasoline, FuelOilNo1, FuelOilNo2, OtherFuel1, OtherFuel2)
    0.99, !-Fuel Burner Efficiency
    10000, !-Nominal Heating Capacity [W] (autosizeable)
    2, !-Design Flow Rate [kg/s] (autosizeable)
    60, !-Design Supply Temperature [degree C] (default 60°C)
    11.1, !-Design Temperature Lift [degree C] (default 11.1°C)
    1.1, ! Sizing Factor
    NotModulated; !-Flow Mode (NotModulated, ConstantFlow, or LeavingSetpointModulated)
    OutdoorAirDryBulb, !-Temp1 Variable Type - Outdoor Air Dry Bulb or Wet Bulb (for following curves)
    HeatPumpEnteringWaterTemperature, !-Temp2 Variable Type - Heat Pump Entering or Leaving Water Temperature (for following curves)
    CAPFT_CurveName, !-CAPFT Name - Normalized Capacity Function of Temperature Curve name (biquadratic curve or lookup table)
    Fuel_EIRFT_CurveName, !-Fuel EIRFT - Fuel Energy Input Ratio Function of Temperature Curve name (biquadratic curve or lookup table)
    Fuel_EIRFPLR_CurveName, !-Fuel EIRFPLR - Fuel Energy Input Ratio Function of PLR Curve name (cubic curve or lookup table)
    0.1, !-Minimum Part Load Ratio (0-1)
    1, !-Maximum Part Load Ratio (0-1)
    Fuel_EIRDefrost_CurveName, !-Fuel EIRDEFROST - Energy Input Ratio Defrost Adjustment Curve name (cubic curve or lookup table function of Temp1 above)
    5, !-Defrost operation Tmin [minute]
    30, !-Defrost operation Tmax [minute]
    CRF_CurveName, !-Cycling Ratio Factor Curve Name (cubic curve or lookup table function of Cycling Ratio = PLR/PLRmin)
    900, !-Nominal Aux. Electricity Use (W)
    Aux_Elec_EIRFT_CurveName, !-Aux. Electric EIRFT (biquadratic curve or lookup table) - accounts for system internal fans, pumps, and electronics
    Aux_EIRPLR_CurveName, !-All: Aux. Electric EIRFPLR (cubic curve or lookup table)
    300; !-Standby Electricity Use (W)
```

### IDD entry for HeatPump:AirToWater:FuelFiredEquationFit:Heating ###

A HeatPump:AirToWater:FuelFiredEquationFit:Heating defines the basic inputs for an equation based gas-fired absorption heat pump. 

```
HeatPump:AirToWater:FuelFiredEquationFit:Heating,
  \memo The object defines a gas-fired absorption heat pump which is based on equation-fit models.
  A1 , \field Name
       \required-field
       \note Name of the gas fired absorption heat pump system system
  A2 , \field Heat Pump Inlet Node Name
       \required-field
       \type node
       \note Inlet node name of the heat pump connection
  A3 , \field Heat Pump Outlet Node Name
       \required-field
       \type node
       \note Outlet node name of the heat pump connection
  A4 , \field Fuel Type
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
  N1 , \field Fuel Burner Efficiency
       \minimum> 0
       \maximum 1
       \default 1
       \note Fuel Burner Efficiency (0-1)
  N2 , \field Nominal Heating Capacity
       \autosizable
       \minimum >0
       \note Nominal Heating Capacity in [W] (autosizeable)
  N3 , \field Design Flow Rate (autosizeable)
       \autosizable
       \minimum >0
       \note Design Flow Rate in m3/s (autosizeable)
  N4 , \field Design Supply Temperature
       \default 60
       \note Design Supply Temperature in [degree C]
  N5 , \field Design Temperature Lift
       \default 11.1
       \note Design Temperature Lift in [degree C]
  N6 , \field Sizing Factor
       \minimum 1.0
       \default 1.0
       \note Sizing Factor for equipment sizing
  A14, \field Flow Mode
       \required-field
       \type choice
       \key NotModulated
       \key ConstantFlow
       \key LeavingSetpointModulated
       \default NotModulated
       \note Flow Mode similar to a boiler flow mode classification
  A5 , \field Temp1 Variable Type
       \required-field
       \type choice
       \key OutdoorAirDryBulb
       \key OUtdoorAirWetBulb
       \default OutdoorAirDryBulb
       \note Temp1 Variable Type - Outdoor Air Dry Bulb or Wet Bulb (for following curves)
  A6 , \field Temp2 Variable Type
       \required-field
       \type choice
       \key HeatPumpEnteringWaterTemperature
       \key HeatPumpLeavingWaterTemperature
       \default HeatPumpEnteringWaterTemperature
       \note Temp2 Variable Type - Heat Pump Entering or Leaving Water Temperature (for following curves)
  A7 , \field CAPFT Curve Name
       \required-field
       \type object-list
       \object-list BivariateFunctions
       \note: CAPFT Name - Normalized Capacity Function of Temperature Curve name (biquadratic curve or lookup table)
  A8 , \field Fuel EIRFT Curve Name
       \required-field
       \type object-list
       \object-list BivariateFunctions
       \note Fuel EIRFT - Fuel Energy Input Ratio Function of Temperature Curve name (biquadratic curve or lookup table)
  A9 , \field Fuel EIRFPLR Curve Name
       \required-field
       \type object-list
       \object-list UnivariateFunctions
       \note Fuel EIRFPLR - Fuel Energy Input Ratio Function of PLR Curve name (cubic curve or lookup table)
  N7 , \field Minimum Part Load Ratio 
       \minimum 0.0
       \maximum 1.0
       \default 0.1
       \note Minimum Part Load Ratio (PLR) in between 0 and 1
  N8 , \field Maximum Part Load Ratio 
       \minimum 0.0
       \maximum 1.0
       \default 1.0
       \note Maximum Part Load Ratio (PLR) in between 0 and 1
  A10, \field Fuel EIRDEFROST Curve Name
       \type object-list
       \object-list UnivariateFunctions
       \note Fuel EIRDEFROST - Energy Input Ratio Defrost Adjustment Curve name (cubic curve or lookup table function of Temp1 above)
  N9 , \field Defrost operation Minimum Time
       \minimum 0
       \default 5
       \note Defrost operation Minimum Time Tmin in [minute]
  N10, \field Defrost operation Maximum Time
       \minimum> 0
       \default 30
       \note Defrost operation Maximum Time Tmax in [minute] 
  A11, \field Cycling Ratio Factor Curve Name
       \type object-list
       \object-list UnivariateFunctions
       \note Cycling Ratio Factor (CRF) Curve Name (cubic curve or lookup table function of Cycling Ratio = PLR/PLRmin)
  N11, \field Nominal Auxiliary Electricity Use
       \minimum 0
       \note Nominal Auxiliary Electricity Use in [W]
  A12, \field Auxiliary Electric EIRFT Curve Name
       \type object-list
       \object-list BivariateFunctions
       \note Auxiliary Electric EIRFT Curve Name (biquadratic curve or lookup table) - accounts for system internal fans, pumps, and electronics
  A13, \field  Auxiliary Electric EIRFPLR Curve Name
       \type object-list
       \note Auxiliary. Electric EIRFPLR Curve Name (cubic curve or lookup table)
  N12;  \field Standby Electricity Use
       \minimum 0
       \note Standby Electricity Use in [W]

```

### Output Variables ###

Output variables will reported for operation conditions, such as heating energy (rate), fuel energy (rate), electricity energy (rate)load, plr, flow rate values. 

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

### HeatPump:AirToWater:FuelFiredEquationFit:Heating Input Fields ###

The eatPump:AirToWater:FuelFiredEquationFit:Heating will take the following input fields:

#### Field: Name ####
The name of the gas-fired absorption heatpump system.

#### Field Name ####

#### Field: Sizing Factor ####
This optional numeric field allows the user to specify a sizing factor for this component. The sizing factor is used when the component design inputs are autosized: the autosizing calculations are performed as usual and the results are multiplied by the sizing factor. For this component the inputs that would be altered by the sizing factor are: Nominal Capacity and Design Flow Rate. Sizing factor allows the user to size a component to meet part of the design load while continuing to use the autosizing feature.

#### Field: ####

#### Field: Flow Mode ####
This choice field determines how the gas-fired absorption heatpump operates with respect to the intended fluid flow through the device. There are three different choices for specifying operating modes for the intended flow behavior: "NotModulated," "ConstantFlow," and "LeavingSetpointModulated." NotModulated is useful for either variable or constant speed pumping arrangements where the boiler is passive in the sense that although it makes a nominal request for its design flow rate it can operate at varying flow rates. ConstantFlow is useful for constant speed pumping arrangements where the heatpump's request for flow is more strict and can increase the overall loop flow. LeavingSetpointModulated changes the heatpump model to internally vary the flow rate so that the temperature leaving the heatpump matches a setpoint. In all cases the operation of the external plant system can also impact the flow through the heatpump---for example if the relative sizes and operation are such that flow is restricted and the requests cannot be met. The default, if not specified, is NotModulated.

#### Field: ####

#### Field: ####

An example of the HeatPump:AirToWater:FuelFiredEquationFit:Heating input object is like this:

```
HeatPump:AirToWater:FuelFiredEquationFit:Heating,
    AbsorptionHeatPumpName1, !- Name
    Inlet_Node_Name, !-Heat Pump Inlet Node Name
    Outlet_Node_Name, !-Heat Pump Outlet Node Name
    NaturalGas, !-Fuel Type (NaturalGas, Propane, Diesel, Gasoline, FuelOilNo1, FuelOilNo2, OtherFuel1, OtherFuel2)
    0.99, !-Fuel Burner Efficiency
    10000, !-Nominal Heating Capacity [W] (autosizeable)
    2, !-Design Flow Rate [kg/s] (autosizeable)
    60, !-Design Supply Temperature [degree C] (default 60°C)
    11.1, !-Design Temperature Lift [degree C] (default 11.1°C)
    1.1, ! Sizing Factor
    NotModulated; !-Flow Mode (NotModulated, ConstantFlow, or LeavingSetpointModulated)
    OutdoorAirDryBulb, !-Temp1 Variable Type - Outdoor Air Dry Bulb or Wet Bulb (for following curves)
    HeatPumpEnteringWaterTemperature, !-Temp2 Variable Type - Heat Pump Entering or Leaving Water Temperature (for following curves)
    CAPFT_CurveName, !-CAPFT Name - Normalized Capacity Function of Temperature Curve name (biquadratic curve or lookup table)
    Fuel_EIRFT_CurveName, !-Fuel EIRFT - Fuel Energy Input Ratio Function of Temperature Curve name (biquadratic curve or lookup table)
    Fuel_EIRFPLR_CurveName, !-Fuel EIRFPLR - Fuel Energy Input Ratio Function of PLR Curve name (cubic curve or lookup table)
    0.1, !-Minimum Part Load Ratio (0-1)
    1, !-Maximum Part Load Ratio (0-1)
    Fuel_EIRDefrost_CurveName, !-Fuel EIRDEFROST - Energy Input Ratio Defrost Adjustment Curve name (cubic curve or lookup table function of Temp1 above)
    5, !-Defrost operation Tmin [minute]
    30, !-Defrost operation Tmax [minute]
    CRF_CurveName, !-Cycling Ratio Factor Curve Name (cubic curve or lookup table function of Cycling Ratio = PLR/PLRmin)
    900, !-Nominal Aux. Electricity Use (W)
    Aux_Elec_EIRFT_CurveName, !-Aux. Electric EIRFT (biquadratic curve or lookup table) - accounts for system internal fans, pumps, and electronics
    Aux_EIRPLR_CurveName, !-All: Aux. Electric EIRFPLR (cubic curve or lookup table)
    300; !-Standby Electricity Use (W)
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

A function to process the input fields for the eatPump:AirToWater:FuelFiredEquationFit:Heating object. It will read input information for the system's connection, equipment sizing, and operation mode.

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

