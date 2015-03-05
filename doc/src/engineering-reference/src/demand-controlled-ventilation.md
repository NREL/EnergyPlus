# Demand Controlled Ventilation

ASHRAE Standard 62.1, Ventilation for Acceptable Indoor Air Quality, contains provisions that allow building ventilation systems to vary the amount of outdoor ventilation air delivered to occupied zones based on feedback from sensors that monitor various indoor air contaminants (ASHRAE 2007). Although not a contaminant of concern in most buildings, carbon dioxide (CO~2~) levels can be monitored as an indicator of building occupancy and the associated human bioeffluent concentration. CO~2~-based demand controlled ventilation (DCV) is being increasingly used to modulate outdoor ventilation air based on real-time occupancy (Emmerich and Persily 1997, Schell et al. 1998, Schell and Int-Hout 2001). Modulating the outdoor ventilation air while maintaining proper indoor air quality has the potential for large energy savings compared to constant rate ventilation systems that are typically designed to provide outdoor ventilation air based on maximum anticipated occupancy.

EnergyPlus can model CO~2~-based DCV by the ventilation rate procedure (VRP) defined in ASHRAE Standard 62.1-2007/2010 for single and multiple path systems, and the indoor air quality procedure (IAQP) defined in Standard 62. The VRP first calculates the breathing-zone outdoor air flow rate based on two components – the zone occupant component and the zone floor area component, then it calculates the zone supply outdoor air flow rate considering the zone air distribution effectiveness and secondary recirculation (for mult-path systems only), and finally calculates the system outdoor air flow rate considering the zone diversity and system ventilation efficiency. The user must include the following five objects in their input data file in order to model CO~2~-based DCV (using VRP or IAQP):

- **AirLoopHVAC:OutdoorAirSystem** to simulate the mixed air box of the air loop
- **Controller:MechanicalVentilation** with the DCV flag set to 'Yes' to determine the minimum outdoor air flow rate to be provided by the mixed air box
- **Controller:OutdoorAir** to control the outside air flow rate introduced via the mixed air box
- **DesignSpecification:OutdoorAir** to describe the outdoor air requirements for each zone
- **DesignSpecification:ZoneAirDistribution** to describe air distribution effectiveness and secondary recirculation (for multi-path ventilation systems) for each zone

The outdoor air system (AirLoopHVAC:OutdoorAirSystem) is a subsystem of an AirLoopHVAC which handles the mixed air portion of the primary air system: the system relief air, the outside air inlet, and any components and controllers associated with the system relief air and outside air streams. The inputs for this object are fully described in the EnergyPlus Input Output Reference. Determining the outdoor air ventilation rate and introducing this ventilation via the mixed air box are accomplished by the mechanical ventilation and outdoor air controller objects.

The VRP currently requires outdoor air ventilation rates to be determined based on the floor area of each occupied zone plus the number of people in each zone. The number of people varies based on the setting of the DCV flag in the Controller:MechanicalVentilation object. For using occupancy at every time step, the DCV flag must be set to 'Yes'. The outdoor air ventilation rate can then be reset dynamically as operating conditions change (e.g., variations in occupancy). The Controller:MechanicalVentilation object simplifies the procedure for calculating these outdoor air ventilation requirements and resetting them based on varying occupancy levels. This is particularly useful for large air distribution systems that serve a number of different zone types with varying occupancy levels.

The IAQP (ASHRAE 2007) is a design procedure in which outdoor air intake and other system design parameters are based on an analysis of contaminant sources, and contaminant concentration targets. Although carbon dioxide is not considered as an indoor contaminant but is considered as an indicator of indoor air quality in buildings, EnergyPlus uses IAQP to control carbon dioxide. In addition, a generic contaminant may be introduced. The current IAQP in EnergyPlus allows credit to be taken for controls that can be reliably demonstrated to result in indoor carbon dioxide and generic contaminant concentrations equal to or lower that those achieved using the VRP. The IAQP may also be used where the design is intended to attain specific target carbon dioxide and generic contaminant levels.

## Ventilation Rate Procedure

### Calculation of zone minimum outdoor air flow

For the i-th zone, first the breathing-zone outdoor air flow is calculated:

![](media/image4435.png)\


Where:

![](media/image4436.png) = the breathing-zone outdoor air flow, m³/s

![](media/image4437.png) = the ventilation rate per person, m³/s-person

![](media/image4438.png) = the number of occupants for the zone

![](media/image4439.png) = the ventilation rate per floor area, m³/s-m²

![](media/image4440.png) = the floor area of the zone, m²

![](media/image4441.png) = the index of the zone or zone list

Next, the i-th zone outdoor air flow is calculated:

![](media/image4442.png)\


Where:

![](media/image4443.png) = the zone outdoor air flow, m³/s

![](media/image4444.png) = the zone air distribution effectiveness, see following figure for ASHRAE recommended values.

![Zone Air Distribution Effectiveness Typical Values (Source: ASHRAE Standard 62.1-2010)](media/zone-air-distribution-effectiveness-typical.png)


### Calculation of system minimum outdoor air flow

For single zone systems, the system outdoor air flow,

![](media/image4446.png)\


For 100% outdoor air multi-zone systems, the system outdoor air flow,

![](media/image4447.png)\


For non 100% outdoor air multi-zone systems, the system outdoor air flow,

![](media/image4448.png)\


Where:

![](media/image4449.png)  the uncorrected system outdoor air flow, m³/s

![](media/image4450.png)\


![](media/image4451.png)  = the uncorrected system outdoor air fraction

![](media/image4452.png)\


![](media/image4453.png) = the system supply air flow, m³/s

![](media/image4454.png)  = the zone outdoor air fraction

![](media/image4455.png)\


![](media/image4456.png)  = the zone supply air flow, m³/s

![](media/image4457.png) = the system ventilation efficiency, determined as the minimum of the zone ventilation efficiency among all ventilation zones served by the air handler.

*Ev* = minimum (*Evz*)

*Evz*= the zone ventilation efficiency, determined differently for single-path and multi-path systems.

**For single-path systems,**

Evz = 1 + Xs – Z~d,i~

**For multi-path systems,**

*Evz = (Fa + Xs · Fb – Z~d,i~ · Ep · Fc )/Fa*

where system air fractions *Fa*, *Fb*, and *Fc* are determined as follows

*Fa = Ep + (1 – Ep) · Er*

*Fb = Ep*

*Fc = 1 – (1 – Ez) · (1 – Er) · (1 – Ep)*

*Ep=* the zone primary air fraction. For dual-fan dual-duct systems, the zone primary air is the air from the cold duct.

*Ep = Vpz /Vdz,i*

*Er* = the zone secondary recirculation fraction

Single-path systems are special cases of multi-path systems where *Er* = 0, *Ep* = 1, *Fa* = *Fb =* *Fc* = 1.0

The zone secondary recirculation fraction Er is determined by the designer based on system configuration. For plenum return systems with secondary recirculation (e.g., fan-powered VAV with plenum return) Er is usually less than 1.0, although values may range from 0.1 to 1.2 depending upon the location of the ventilation zone relative to other zones and the air handler. For ducted return systems with secondary recirculation (e.g., fan-powered VAV with ducted return), Er is typically 0.0, while for those with system-level recirculation (e.g, dual-fan dual-duct systems with ducted return) Er is typically 1.0. For other system types, Er is typically 0.75.

The program then calculates the minimum outdoor air flow fraction based on the information provided in the object Controller:MechanicalVentilation and the maximum branch air flow rate as follows:

MechVentOutsideAirMinFrac = ![](media/image4458.png)  RhoStd / ![](media/image4459.png) here:

MechVentOutsideAirMinFrac = outdoor air minimum fraction based on all zones specified in the Controller:MechanicalVentilation object

Controller:OutdoorAir controls the amount of outdoor ventilation air introduced via the mixed air box based on several user inputs. The user can define the minimum outdoor air flow rate as a percentage of the system's supply air flow rate (e.g., for a variable-air volume system) or a fixed minimum outdoor air flow rate (not as a percentage but a fixed value) (field MinimumLimit). CO~2~-based DCV, using the Controller:MechanicalVentilation object in conjunction with the Controller:OutdoorAir object, allows a third option for setting the minimum outdoor air flow. Economizer operation can also be specified to increase the outdoor air flow above the minimum flow rate to provide free cooling when conditions permit (Controller:OutdoorAir, field Economizer Control Type).

> EnergyPlus uses the largest outdoor air flow rate calculated by the various methods described above when modeling system performance (as long this rate doesn't exceed the maximum flow rate specified for the main air loop branch or for the outdoor air controller itself).

The method used to calculate the outdoor ventilation air flow rate for each system simulation time step is described in more detail below. The figure below schematically illustrates air flow paths used in the calculation of outdoor air flow rate.

![Demand Control Ventilation -- Air Flow Paths](media/demand-control-ventilation-air-flow-paths.png)


The minimum outdoor air flow rate is first calculated based on the minimum outdoor air flow rate and the minimum outdoor air schedule value as defined by the user inputs for the object Controller:OutdoorAir:

![](media/image4461.png)\


where:

![](media/image4462.png) = minimum outdoor air flow rate for this time step, kg/s

![](media/image4463.png) = minimum outdoor air flow rate defined in Controller:OutdoorAir, m^3^/s

*MinOAScheduleValue* = minimum outdoor air schedule value defined by the schedule identified in Controller:OutdoorAir

*RhoStd*= standard air density (1.204 kg/m^3^) adjusted for the local barometric pressure (standard barometric pressure corrected for altitude, ASHRAE 1997 HOF pg. 6.1).

The outdoor air minimum fraction is then calculated as the ratio of the minimum outdoor air flow rate calculated above to the maximum air flow rate defined in the Branch statement for the main air loop (converted to mass flow rate).

![](media/image4464.png)\


where:

![](media/image4465.png) = Max branch air volume flow rate times *RhoStd,* kg/s

The program then calculates the minimum outdoor air flow fraction (*MechVentOutsideAirMinFrac*) according to the VRP based on the information provided in the object Controller:MechanicalVentilation and the maximum branch air flow rate.

The algorithm then uses the larger of these outdoor air minimum fractions in subsequent calculations, and also makes sure that the resulting fraction is between 0 and 1.

![](media/image4466.png)\


![](media/image4467.png)\


The algorithm goes on to determine if economizer operation is possible based on the user inputs and the current conditions of the outdoor air and return air. If conditions permit economizer operation, the outdoor air flow fraction is increased beyond the minimum fraction to meet the mixed air setpoint temperature (setpoint temperature assigned to the node defined in field "Control_Node" of Controller:OutdoorAir).

![](media/image4468.png)\


The mass flow rate of outdoor air is then calculated based on the outdoor air fraction determined above and the mixed (supply) air mass flow rate:

![](media/image4469.png)\


where:

![](media/image4470.png) = mass flow rate of outdoor air, kg/s

![](media/image4471.png) = fraction of outdoor air in the mixed (supply) air stream

![](media/image4472.png) = mass flow rate of the mixture of return air and outdoor ventilation air, kg/s

The algorithm checks to make sure the calculated outdoor air mass flow rate is greater than or equal to the air flow rate being exhausted.

![](media/image4473.png)\


If a fixed minimum outdoor air flow rate is specified (field Minimum Limit Type in Controller:OutdoorAir) for a continuous air flow system, the program makes sure that the outdoor air mass flow rate is greater than or equal to the minimum outdoor air flow rate specified by the user.

![](media/image4474.png)\


The outdoor air mass flow rate should be less than or equal to the mixed (supply) air flow rate, and the outdoor air flow rate is reset if necessary.

![](media/image4475.png)\


The outdoor air mass flow rate should also be less than or equal to the maximum outdoor air flow rate specified by the user, and the outdoor air flow rate is reset if necessary.

![](media/image4476.png)\


where:

![](media/image4477.png) = maximum outdoor air mass flow rate, kg/s = maximum outdoor air volume flow rate from Controller:OutdoorAir times *RhoStd*

Finally, the relief air flow rate is calculated as the difference between the outside and exhaust air mass flow rates.

![](media/image4478.png)\


## Indoor Air Quality Procedure

Like VRP, the user must include the following three objects in their input data file in order to model CO~2~-based DCV with IAQP:

- AirLoopHVAC:OutdoorAirSystem to simulate the mixed air box of the air loop
- Controller:MechanicalVentilation to determine the minimum outside air flow rate to be provided by the mixed air box
- Controller:OutdoorAir to control the outside air flow rate introduced via the mixed air box

The outdoor air mass flow rate provided by the air loop is calculated as below:

![](media/image4479.png)\


where:

![](media/image4480.png) = outdoor air mass flow rate for the ith zone as calculated according to section Carbon Dioxide Prediction located elsewhere in this document.

N = number of zones served by the air loop, which is provided in the input for a Controller:MechanicalVentilation object

## Proportional Control

Like Ventilation Rate Procedure and the Indoor Air Quality Procedure, the following three objects must be included in the input data file in order to model CO~2~-based DCV with Proportional Control:

- **AirLoopHVAC:OutdoorAirSystem** to simulate the mixed air box of the air loop
- **Controller:MechanicalVentilation** to determine the minimum outside air flow rate to be provided by the mixed air box
- **Controller:OutdoorAir** to control the outside air flow rate introduced via the mixed air box

For the i-th zone, the outdoor air mass flow rate provided by the air loop is calculated as below:

#. The required intake flow of outdoor air for the design zone population, *P~z,i~*

![](media/image4481.png)\


#. The required intake flow of outdoor air when the zone is unoccupied i.e.  *P~z,i~* = 0

![](media/image4482.png)\


#. The target indoor CO~2~ concentration at ![](media/image4483.png)

![](media/image4484.png)\


#. The target indoor CO~2~ concentration at ![](media/image4485.png)  is either a user input in the object ZoneControl:ContaminantController or equal to the outdoor CO~2~ concentration. The default is outdoor CO~2~ concentration.

![](media/image4486.png)\


When the indoor CO~2~ concentration equals ![](media/image4487.png) , ![](media/image4488.png)  should equal ![](media/image4489.png) . When the indoor CO~2~ concentration equals ![](media/image4490.png) , ![](media/image4491.png)  should equal ![](media/image4492.png) . When the indoor CO~2~ concentration is between ![](media/image4493.png)  and ![](media/image4494.png) , a controller should adjust outdoor air intake flow ![](media/image4495.png)  proportionally between ![](media/image4496.png)  and ![](media/image4497.png) :

![](media/image4498.png)\


Where,

![](media/image4499.png)  = Required outdoor air flow rate per person, (m^3^/s)/person

![](media/image4500.png)  = Required outdoor air flow rate per unit area, (m^3^/s)/m^2^

![](media/image4501.png)  = Design zone population, number of people

![](media/image4502.png)  = Zone floor area, m^2^

![](media/image4503.png)  = The zone air distribution effectiveness

![](media/image4504.png)  = CO~2~ generation rate, (m^3^/s)/person (Specified in the People object)

![](media/image4505.png)  = CO~2~ concentration in the outdoor air, ppm

![](media/image4506.png)  = CO~2~ concentration in the space for the design conditions, ppm

![](media/image4507.png)  = Minimum CO~2~ concentration in the space, ppm

![](media/image4508.png)  = Actual CO~2~ concentration in the space, ppm

![](media/image4509.png)  = Required intake of outdoor air flow rate at ![](media/image4510.png) , (m^3^/s)

![](media/image4511.png)  = Required intake of outdoor air flow rate at ![](media/image4512.png) , (m^3^/s)

![](media/image4513.png)  = Required intake of outdoor air flow rate at ![](media/image4514.png) , (m^3^/s)

Except ![](media/image4515.png)  above, all other variables are already available in EnergyPlus (See Eng. Reference for "Ventilation Rate Procedure" above for further details). ![](media/image4516.png)  can be specified in the ZoneControl:ContaminantController object as a schedule. If  ![](media/image4517.png)  is not specified in the ZoneControl:ContaminantController object, then outdoor air CO2 concentration will be used as the minimum. In order for "ProportionalControl" to be active, the following conditions must be met, otherwise default "VentilationProcedure" will be modeled and a warning will be issued during runtime:

#. "Carbon Dioxide Control Availability Schedule Name" input field in the ZoneControl:ContaminantController object must be greater than zero.
#. CO2 gain from people in the zone must be greater than zero.
#. "Outdoor air flow per person" and "Outdoor air flow per zone floor area" in the corresponding "DesignSpecification:OutdoorAir" object must be greater than zero.

## References

ASHRAE. 2007. *ANSI/ASHRAE Standard 62.1-2007, Ventilation for acceptable indoor air quality*. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.

ASHRAE. 2010. *ANSI/ASHRAE Standard 62.1-2010, Ventilation for acceptable indoor air quality*. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.

Emmerich, S.J. and A.K. Persily. 1997. Literature review on CO~2~-based demand-controlled ventilation. *ASHRAE Transactions* 103(2):229-243.

Schell, M.B., S.C. Turner, R.O. Shim. 1998. Application of CO~2~-based demand controlled ventilation using ASHRAE Standard 62-1989. *ASHRAE Transactions* 104(2):1213-1225.

Schell, M. and D. Int-Hout. 2001. Demand control ventilation using CO~2~. *ASHRAE Journal*, February.