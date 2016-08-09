Other Fuel Heating Coils
========================

**Noel Merket, NREL**

 - Original Date: 9 Aug 2016
 - Revision Date
 

## Justification for New Feature ##

Similar to [#5746](https://github.com/NREL/EnergyPlus/issues/5746) , furnaces currently only support gas or electric heating coils. Propane and fuel oil are commonly used fuel types, especially for residential models. This is similar to [#5656](https://github.com/NREL/EnergyPlus/issues/5656) / [#5746](https://github.com/NREL/EnergyPlus/issues/5746) which added a fuel type field to `OtherEquipment`. 

## E-mail and  Conference Call Conclusions ##

See GitHub issue [#5752](https://github.com/NREL/EnergyPlus/issues/5752).

## Overview ##

I propose creating a `Coil:Heating:OtherFuel` object that has all the same fields as `Coil:Heating:Gas` with the addtion of a user selectable fuel type field. The equipment would work the same as `Coil:Heating:Gas` otherwise.

## Approach ##

Modify the code in EnergyPlus that handles `Coil:Heating:Gas` objects to handle either that or the new `Coil:Heating:OtherFuel`. Change the input routine to appropriately set the fuel type depending on which object is used.

## Testing/Validation/Data Sources ##

I would use the existing EnergyPlus test files and modify a few of them to use the new object and see if the results come out as expected. 

## Input Output Reference Documentation ##

The other fuel heating coil is a simple capacity model with a user inputted burner efficiency. The default for the burner efficiency is 80%. This coil will be simpler than shown in Figure \[fig:example-air-loop-heating-cooling-coil\] since it will only have air nodes to connect it in the system. The coil can be used in the air loop simulation or in the zone equipment as a reheat coil. Depending on where it is used determines if this coil is temperature or capacity controlled. If used in the air loop simulation it will be controlled to a specified temperature scheduled from the Setpoint Manager. If it is used in zone equipment, it will be controlled from the zone thermostat by meeting the zone demand.

### Inputs

#### Field: Name

A unique identifying name for each coil.

#### Field: Fuel Type

This field designates the appropriate fuel type for the coil. Valid fuel types are: PropaneGas, FuelOil#1, FuelOil#2, Diesel, Gasoline, Coal, Steam, DistrictHeating, DistrictCooling, OtherFuel1 and OtherFuel2. The fuel type triggers the application of consumption amounts to the appropriate energy meters. 

#### Field: Availability Schedule Name

Schedule that defines when the coil is available. If the schedule s value is 0.0, then the coil is not available and flow will not be requested. If the schedule s value is &gt; 0.0 (usually 1 is used), the coil is available. If this field is blank, the schedule has values of 1 for all time periods. Schedule values must be &gt; = 0 and &lt; = 1.

#### Field: Burner Efficiency

This is user inputted burner efficiency (decimal, not percent) and is defaulted to 80%.

#### Field: Nominal Capacity

This is the maximum capacity of the coil (W). This controlled coil will only provide the needed capacity to meet the control criteria whether it is temperature or capacity controlled. This field is autosizable.

#### Field: Air Inlet Node Name

The name of the air inlet to the coil, i.e. Heating Coil Air Inlet Node.

#### Field: Air Outlet Node Name

The name of the air outlet from the coil, i.e. Heating Coil Air Outlet Node.

#### Field: Temperature Setpoint Node Name

If the coil is used in the air loop simulation and is temperature controlled using a Set Point Manager, then the node that is the control node needs to be specified here. If the coil is used in an air terminal unit, the coil is load controlled and a control node set point is not required.

#### Field: Parasitic Electric Load

This is the parasitic electric load associated with the coil operation, such as an inducer fan, etc.. This will be modified by the PLR (or coil runtime fraction if a part-load fraction correlation is provided in the next input field) to reflect the time of operation in a simulation timestep.

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of fuel consumption rate by the heating coil as a function of the part load ratio (PLR, sensible heating load/nominal capacity of the heating coil). For any simulation timestep, the nominal fuel consumption rate (heating load/burner efficiency) is divided by the part-load fraction (PLF) if a part-load curve has been defined. The part-load curve accounts for efficiency losses due to transient coil operation.

The part-load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the heating coil runs continuously for the simulation timestep). For PLR values between 0 and 1 ( 0 &lt; = PLR &lt; 1), the following rules apply:

PLF &gt; = 0.7 and PLF &gt; = PLR

If PLF &lt; 0.7 a warning message is issued, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the heating coil is defined a PLR/PLF. If PLF &lt; PLR, then a warning message is issues and the runtime fraction of the coil is limited to 1.0.

A typical part load fraction correlation for a conventional gas heating coil (e.g., residential furnace) would be:

           PLF = 0.8 + 0.2(PLR)

#### Field: Parasitic Fuel Load

This numeric field is the parasitic fuel load associated with the coil's operation (Watts), such as a standing pilot light. The model assumes that this parasitic load is consumed only for the portion of the simulation timestep where the heating coil is not operating.

### Outputs

-   HVAC,Sum,Heating Coil Air Heating Energy <span>\[</span>J<span>\]</span>

-   HVAC,Average,Heating Coil Air Heating Rate <span>\[</span>W<span>\]</span>

-   HVAC,Sum,Heating Coil &lt;Fuel Type&gt; Energy <span>\[</span>J<span>\]</span>

-   HVAC,Average,Heating Coil &lt;Fuel Type&gt; Rate <span>\[</span>W<span>\]</span>

-   HVAC,Sum,Heating Coil Electric Energy <span>\[</span>J<span>\]</span>

-   HVAC,Average,Heating Coil Electric Power <span>\[</span>W<span>\]</span>

-   HVAC,Average,Heating Coil Runtime Fraction <span>\[\]</span>

-   HVAC,Sum,Heating Coil Ancillary &lt;Fuel Type&gt; Energy <span>\[</span>J<span>\]</span>

-   HVAC,Average,Heating Coil Ancillary &lt;Fuel Type&gt; Rate <span>\[</span>W<span>\]</span>

#### Heating Coil Air Heating Energy <span>\[</span>J<span>\]</span>

This field is the total heating output of the coil in Joules over the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = HeatingCoils, Group Key = System (ref. Output:Meter objects).

#### Heating Coil Air Heating Rate <span>\[</span>W<span>\]</span>

This field is the average heating rate output of the coil in Watts over the timestep being reported.

#### Heating Coil &lt;Fuel Type&gt; Energy <span>\[</span>J<span>\]</span>

This field is the fuel consumption of the heating coil in Joules over the timestep being reported, including the impacts of part-load performance if a part load fraction correlation is specified. This output is also added to a meter with Resource Type = &lt;Fuel Type&gt;, End Use Key = Heating, Group Key = System (ref. Output:Meter objects).

#### Heating Coil &lt;Fuel Type&gt; Rate <span>\[</span>W<span>\]</span>

This field is the average fuel consumption rate of the coil in Watts over the timestep being reported, including the impacts of part-load performance if a part load fraction correlation is specified.

#### Heating Coil Electric Energy <span>\[</span>J<span>\]</span>

This field is the electric consumption of the heating coil auxiliaries in Joules over the timestep being reported (e.g., inducer fan). This output is also added to a meter with Resource Type = Electricity, End Use Key = Heating, Group Key = System (ref. Output:Meter objects).

#### Heating Coil Electric Power <span>\[</span>W<span>\]</span>

This field is the average electric consumption rate of the heating coil auxiliaries (e.g., inducer fan) in Watts over the timestep being reported.

#### Heating Coil Runtime Fraction <span>\[\]</span>

This field is the runtime fraction of the coil over the timestep being reported.

#### Heating Coil Ancillary &lt;Fuel Type&gt; Energy <span>\[</span>J<span>\]</span>

This field is the parasitic fuel consumption of the heating coil in Joules over the timestep being reported (e.g., standing pilot light). The model assumes that the parasitic load is accumulated only for the portion of the simulation timestep where the fuel heating coil is not operating. This output is also added to a meter with Resource Type = &lt;Fuel Type&gt;, End Use Key = Heating, Group Key = System (ref. Output:Meter objects).

#### Heating Coil Ancillary &lt;Fuel Type&gt; Rate <span>\[</span>W<span>\]</span>

This field is the average parasitic fuel consumption rate of the heating coil (e.g., standing pilot light) in Watts over the timestep being reported. The model assumes that the parasitic load is present only for the portion of the simulation timestep where the heating coil is not operating.

## Engineering Reference ##

Will modify the *Gas Air Heating Coil* section to discuss both `Coil:Heating:Gas` and `Coil:Heating:OtherFuel` since all the engineering will be the same with the exception of the fuel type.

## Example File and Transition Changes ##

I will create an example file based on some of the other example files with this new object.

No transition will be necessary because it will be an entirely new object.

## References ##

n/a



