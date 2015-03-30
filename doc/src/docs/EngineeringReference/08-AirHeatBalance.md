
Air Heat Balance Manager / Processes
====================================

Convection from Surfaces
------------------------

This contribution is expressed using the convective heat transfer coefficient as follows:

<div>\[{q_{conv}} = \sum\limits_{i = 1}^{nsurfaces} {{h_{c,i}}{A_i}\left( {{T_a} - {T_{s,i}}} \right)} \]</div>

The inside heat transfer coefficient is modeled from a choice of correlations.

Convection from Internal Sources
--------------------------------

This component is the companion part of the radiant contribution from internal gains described previously.  It is added directly into the air heat balance. Such a treatment also violates the tenets of the heat balance since the surface temperature of the surfaces producing the internal loads exchange heat with the zone air through normal convective processes.   However, once again, the details required to include this component in the heat balance are generally not available, and its direct inclusion into the air heat balance is a reasonable approach..

Infiltration/Ventilation
------------------------

### Infiltration

Any outdoor air that enters by way of infiltration is assumed to be immediately mixed with the zone air. The determination of the amount of infiltration air is quite complicated and subject to significant uncertainty. In the most common procedure, the infiltration quantity is converted from a number of air changes per hour (ACH) and included in the zone air heat balance using the outside temperature at the current simulation time step.

EnergyPlus contains three models for infiltration. The first is the “Design Flow Rate” model that was inherited from EnergyPlus’ predecessor programs. It is accessed through the ZoneInfiltration:DesignFlowRate object and is based on environmental conditions modifying a design flow rate.  The second is the “Effective Leakage Area” model based on Sherman and Grimsrud (1980) and accessed using the ZoneInfiltration:EffectiveLeakageArea input object.  The third is the “Flow Coefficient” model based on Walker and Wilson (1998) and accessed using the ZoneInfiltration:FlowCoefficient input object. The model formulations for the Effective Leakage Area and Flow Coefficient models are from the ASHRAE Handbook of Fundamentals (2001 Chapter 26; 2005 Chapter 27) where they are referred to as “Basic” and “Enhanced”, respectively.

### Infiltration Design Flow Rate

Infiltration (Ref Object: ZoneInfiltration:DesignFlowRate) is the unintended flow of air from the outdoor environment directly into a thermal zone. Infiltration is generally caused by the opening and closing of exterior doors, cracks around windows, and even in very small amounts through building elements. In this model, the user defines a design flow rate that can be modified by temperature differences and windspeed. The basic equation (Coblenz and Achenbach 1963) used to calculate infiltration with this model is:

<div>\[Infiltration = \left( {{I_{design}}} \right)\left( {{F_{schedule}}} \right)\left[ {A + B\left| {\left( {{T_{zone}} - {T_{odb}}} \right)} \right| + C\left( {WindSpeed} \right) + D\left( {Windspee{d^2}} \right)} \right]\]</div>

More advanced infiltration calculations are possible using the EnergyPlus AirflowNetwork model for natural infiltration driven by wind when the HVAC system does not operate and/or driven by wind and forced air for times when the HVAC system operates. Exfiltration (the leakage of zone air to the outside) is generally handled better as zone exhaust air in the zone equipment description.

The question of typical values for these coefficients is subject to debate. Ideally, one should do a detailed analysis of the infiltration situation and then determine a custom set of coefficients using methods such as those laid out in Chapter 26 of the ASHRAE Handbook of Fundamentals. The EnergyPlus defaults are 1,0,0,0 which gives a constant volume flow of infiltration under all conditions.

BLAST (one of the EnergyPlus predecessors) used the following values as defaults:  0.606, 0.03636, 0.1177, 0. These coefficients produce a value of 1.0 at 0C deltaT and 3.35 m/s (7.5 mph) windspeed, which corresponds to a typical summer condition. At a winter condition of 40C deltaT and 6 m/s (13.4 mph) windspeed, these coefficients would increase the infiltration rate by a factor of 2.75.

In DOE-2 (the other EnergyPlus predecessor), the air change method defaults are (adjusted to SI units) 0, 0, 0.224 (windspeed), 0. With these coefficients, the summer conditions above would give a factor of 0.75, and the winter conditions would give 1.34. A windspeed of 4.47 m/s (10 mph) gives a factor of 1.0.

The source of the BLAST defaults is noted in the BLAST documentation as:

"Empirical equation and the coefficient default were determined from ASHRAE journal articles and other data on the effects of outdoor weather conditions."

The source of the DOE-2 defaults is based on examining the infiltration relationships described in the ASHRAE Handbook of Fundamentals.

The EnergyPlus example files use all of the above, the BLAST defaults in some (e.g., GeometryTest), the DOE-2 defaults in some (e.g., 5ZoneAirCooled), and the EnergyPlus defaults in some (e.g., LgOffVAVDetCoil).

### Infiltration by Effective Leakage Area

The Effective Leakage Area model is based on Sherman and Grimsrud (1980) and accessed using the ZoneInfiltration:EffectiveLeakageArea input object.  The model formulation used in EnergyPlus is from the ASHRAE Handbook of Fundamentals (2001 Chapter 26; 2005 Chapter 27) where it is referred to as the “Basic” model.

The Effective Leakage Area, or Sherman-Grimsrud, model is:

<div>\[Infiltration = \left( {{F_{Schedule}}} \right)\frac{{{A_L}}}{{1000}}\sqrt {{C_s}\Delta T + {C_w}{{\left( {WindSpeed} \right)}^2}} \]</div>

where,

<div img="image1877.txt">\({F_{schedule}}\)</div> is a value from a user-defined schedule,

<div img="image1878.txt">\({A_L}\)</div> is the effective air leakage area in cm<sup>2</sup> that corresponds to a 4 Pa pressure differential,

<div img="image1879.txt">\({C_s}\)</div> is the coefficient for stack-induced infiltration in (L/s)<sup>2</sup>/(cm<sup>4</sup>·K),

<div img="image1880.txt">\(\Delta T\)</div> is the absolute temperature difference between zone air and outdoor air,

<div img="image1881.txt">\({C_w}\)</div> is the coefficient for wind-induced infiltration in (L/s)<sup>2</sup>/(cm<sup>4</sup>·(m/s)<sup>2</sup>), and

<div img="image1882.txt">\(WindSpeed\)</div> is the local wind speed.

### Infiltration by Flow Coefficient

The Flow Coefficient model is based on Walker and Wilson (1998) and accessed using the ZoneInfiltration:FlowCoefficient input object.  The model formulation used in EnergyPlus is from the ASHRAE Handbook of Fundamentals (2001 Chapter 26; 2005 Chapter 27) where it is referred to as the “Enhanced” or “AIM-2” model.

The Enhanced, or AIM-2, model is:

<div>\[Infiltration = \left( {{F_{Schedule}}} \right)\sqrt {{{\left( {c\,{C_s}\Delta {T^n}} \right)}^2} + {{\left( {c\,{C_w}{{\left( {s * WindSpeed} \right)}^{2n}}} \right)}^2}} \]</div>

where,

<div img="image1884.txt">\({F_{schedule}}\)</div> is a value from a user-defined schedule,

<div img="image1885.txt">\(c\)</div> is the flow coefficient in m<sup>3</sup>/(s·Pa<sup>n</sup>),

<div img="image1886.txt">\({C_s}\)</div> is the coefficient for stack-induced infiltration in (Pa/K)<sup>n</sup>,

<div img="image1887.txt">\(n\)</div> is the pressure exponent,

<div img="image1888.txt">\({C_w}\)</div> is the coefficient for wind-induced infiltration in (Pa·s<sup>2</sup>/m<sup>2</sup>)<sup>n</sup>, and

<div img="image1889.txt">\(s\)</div> is the shelter factor.

#### References:

Coblenz, C. W. and Achenbach, P. R. 1963. Field Measurement of Ten Electrically-Heated Houses. ASHRAE Transactions pp 358-365.

Sherman, M.H. and D.T. Grimsrud. 1980. Infiltration-pressurization correlation: Simplified physical modeling. ASHRAE Transactions 86(2):778

Walker, I.S., and D.J. Wilson. 1998. Field validation of equations for stack and wind driven air infiltration calculations.  International Journal of HVAC&R Research 4(2).

ASHRAE Handbook of Fundamentals. 2005. Chapter 27. (and 2001 Chapter 26).

### Ventilation

EnergyPlus contains two models for ventilation.  The “Design Flow Rate” model, inherited from EnergyPlus’ predecessor programs, is accessed through the ZoneVentilation:DesignFlowRate object and is based on environmental conditions modifying a design flow rate.  The “Wind and Stack with Open Area” model, based on equations defined in Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals, is accessed using the ZoneVentilation:WindandStackOpenArea input object. Since the “Wind and Stack with Open Area” object requires the height difference between the midpoint of the lower opening and the neutral pressure level, which is difficult to estimate, this object should be used with care (e.g., research only).

These two ventilation objects can be used alone or in combination to determine ventilation air for a zone. If multiple ZoneVentilation:\* objects are specified for a zone, then the total zone ventilation flow rate is the sum of the ventilation air flow rates calculated by each ZoneVentilation object.

### Ventilation Design Flow Rate

Ventilation (Ref Object: ZoneVentilation:DesignFlowRate) is the purposeful flow of air from the outdoor environment directly into a thermal zone in order to provide some amount of non-mechanical cooling.  Ventilation as specified by this input syntax is intended to model “simple” ventilation as opposed to the more detailed ventilation investigations that can be performed with the AirflowNetwork model.  Simple ventilation in EnergyPlus can be controlled by a schedule and through the specification of minimum, maximum and delta temperatures. The temperatures can be either single constant values for the entire simulation or schedules which can vary over time. Specific details are given in the Input/Output reference document. As with infiltration, the actual flow rate of ventilation can be modified by the temperature difference between the inside and outside environment and the wind speed. The basic equation used to calculate ventilation using this model is:

<div>\[Ventilation = \left( {{V_{design}}} \right)\left( {{F_{schedule}}} \right)\left\lfloor {A + B\left| {{T_{zone}} - {T_{odb}}} \right| + C\left( {WindSpeed} \right) + D\left( {WindSpee{d^2}} \right)} \right\rfloor \]</div>

More advanced ventilation calculations are possible using the EnergyPlus AirflowNetwork model.

The following description is copied from the Infiltration discussion above. The question of typical values for these coefficients is subject to debate. Ideally, one should do a detailed analysis of the ventilation situation and then determine a custom set of coefficients using methods such as those laid out in Chapter 26 of the ASHRAE Handbook of Fundamentals. The EnergyPlus defaults are 1,0,0,0 which gives a constant volume flow of ventilation under all conditions.

BLAST (one of the EnergyPlus predecessors) used the following values as defaults:  0.606, 0.03636, 0.1177, 0. These coefficients produce a value of 1.0 at 0C deltaT and 3.35 m/s (7.5 mph) windspeed, which corresponds to a typical summer condition. At a winter condition of 40C deltaT and 6 m/s (13.4 mph) windspeed, these coefficients would increase the ventilation rate by a factor of 2.75.

In DOE-2 (the other EnergyPlus predecessor), the air change method defaults are (adjusted to SI units) 0, 0, 0.224 (windspeed), 0. With these coefficients, the summer conditions above would give a factor of 0.75, and the winter conditions would give 1.34. A windspeed of 4.47 m/s (10 mph) gives a factor of 1.0.

The source of the BLAST defaults is noted in the BLAST documentation as:

"Empirical equation and the coefficient default were determined from ASHRAE journal articles and other data on the effects of outdoor weather conditions."

The source of the DOE-2 defaults is based on examining the infiltration relationships described in the ASHRAE Handbook of Fundamentals.

The EnergyPlus example files use all of the above, the BLAST defaults in some (e.g., AirflowNetwork\_Simple\_house), the DOE-2 defaults in some (e.g., VentilationSimpleTest – has all 3), and the EnergyPlus defaults in some (e.g., 5ZoneNightVent2).

### Ventilation by Wind and Stack with Open Area

For this model (Ref Object: ZoneVentilation:WindandStackOpenArea), the ventilation air flow rate is a function of wind speed and thermal stack effect, along with the area of the opening being modeled. This object can be used alone or in combination with ZoneVentilation:DesignFlowRate objects. This model is intended for simplified ventilation calculations as opposed to the more detailed ventilation investigations that can be performed with the AirflowNetwork model. Using the “Wind and Stack with Open Area” model, the natural ventilation flow rate can be controlled by a multiplier fraction schedule applied to the user-defined opening area and through the specification of minimum, maximum and delta temperatures. The temperatures can be either single constant values for the entire simulation or schedules which can vary over time. The equation used to calculate the ventilation rate driven by wind is given by Eq. 37 in Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals:

<div>\[{Q_w} = {C_w}{A_{opening}}{F_{schedule}}V\]</div>

where,

*Q<sub>w</sub>*  = Volumetric air flow rate driven by wind [m<sup>3</sup>/s]

*C<sub>w</sub>*  = Opening effectiveness [dimensionless]

*A<sub>opening</sub>*        = Opening area [m<sup>2</sup>]

*F<sub>schedule</sub>* = Open area fraction [user-defined schedule value, dimensionless]

*V*    = Local wind speed [m/s]

If the user specifies “Autocalculate” for the Opening Effectiveness input field, the opening effectiveness is calculated for each simulation time step based on the angle between the actual wind direction and the Effective Angle (a user-defined input) using the following equation:

<div>\[{C_w} = 0.55 - \frac{{\left| {EffectiveAngle - WindDirection} \right|}}{{180}}*0.25\]</div>

The difference |EffectiveAngle – WindDirection| should be between 0 and 180 degrees. If the difference |EffectivAngle – WindDirection| is greater than 180, the difference is reset to be minus 180 degrees. This equation is a linear interpolation using the values recommended by the 2009 ASHRAE Handbook of Fundamentals (page 16.13): 0.5 to 0.6 for perpendicular winds and 0.25 to 0.35 for diagonal winds.

The equation used for calculating the ventilation rate due to stack effect is given by Eq. 38 in Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals:

<div>\[{Q_s} = {C_D}{A_{opening}}{F_{schedule}}\sqrt {2g\Delta {H_{NPL}}(|{T_{zone}} - {T_{odb}}|/{T_{zone}})} \]</div>

where,

*Q<sub>s</sub>*  = Volumetric air flow rate due to stack effect [m<sup>3</sup>/s]

*C<sub>D</sub>*  = Discharge coefficient for opening [dimensionless]

*A<sub>opening</sub>*        = Opening area [m<sup>2</sup>]

*F<sub>schedule</sub>* = Open area fraction [user-defined schedule value, dimensionless]

*ΔH<sub>NPL</sub>*        = Height from midpoint of lower opening to the neutral pressure level [m].

Estimation of this value is difficult; refer to Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals for guidance.

*T<sub>zone</sub>*            = Zone air dry-bulb temperature [K]

*T<sub>odb</sub>*<sub>                        </sub> = Local outdoor air dry-bulb temperature [K]

The following equation, given by Eq. 39 in Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals, is used to calculate the Discharge Coefficient for Opening when the user sets the value for this input field to “Autocalculate”:

<div>\[{C_D} = 0.40 + 0.0045\left| {{T_{zone}} - {T_{odb}}} \right|\]</div>

The total ventilation rate calculated by this model is the quadrature sum of the wind and stack air flow components:

<div>\[Ventilatio{n_{WindAndStack}} = \sqrt {{Q_s}^2 + {Q_w}^2} \]</div>

If desired, a simple summation, instead of quadrature summation, can be realized by inputting two ZoneVentilation:WindAndStackOpenArea objects. One object can be defined with only a wind-driven component by setting C<sub>D</sub>=0, and the other object can have only stack-effect inputs specified and set C<sub>w</sub>=0.

### Zone Air Balance Outdoor Airflow (ZoneAirBalance:OutdoorAir)

ASHRAE 2009 Handbook of Fundamentals specifies that any unbalanced supply or exhaust ventilation air, Q<sub>u,v</sub>, to a zone causes pressurization/depressurization that influences the flow of infiltration air and thus should be combined with natural infiltration, Q<sub>n</sub>, (and, if present, unbalanced duct leakage, Qu,l) in superposition. Balanced ventilation airflow, Q<sub>b,v</sub>, to a zone (such as an ERV or HRV with balanced exhaust and intake air flows) does not interact with infiltration air and is added in whole:

<div>\[Q = \sqrt {{Q_n}^2 + {Q_{u,v}}^2 + ({Q_{u,l}}^2)}  + {Q_{b,v}}\]</div>

where,

*Q*    = Combined outdoor airflow with infiltration, balanced and unbalanced outdoor air flows, and unbalanced duct leakage [m<sup>3</sup>/s]

*Q<sub>n</sub>*   = Natural infiltration airflow [m<sup>3</sup>/s]

*Q<sub>b,v</sub>* = Balanced ventilation airflow, excluding infiltration [m<sup>3</sup>/s]

*Q<sub>u,v</sub>* = Unbalanced ventilation airflow, excluding infiltration [m<sup>3</sup>/s]

*Q<sub>u,l</sub>*  = Unbalanced duct leakage: the difference between supply and return leaks [m<sup>3</sup>/s]

The natural infiltration airflow includes all outdoor airflows from all ZoneInfiltration:\* objects for the same zone.

<div>\[{Q_n} = \sum\limits_i {{Q_{Infiltration,i}}} \]</div>

where,

*Q<sub>Infiltration,i</sub>*      = Outdoor airflow rate given in the ith ZoneInfiltration:\* objects for the same zone

The balanced ventilation airflow is the sum of outdoor airflows from all ZoneVentilation: DesignFlowRate objects with Ventilation Type = Balanced:

<div>\[{Q_{b,v}} = \sum\limits_i {{Q_{v,Balanced,i}}} \]</div>

where,

*Q<sub>v,Balanced,i</sub>*     = Ventilation rate with “Balanced” ventilation type defined in the ith ZoneVentilation:DesignFlowRate object for the same zone

The unbalanced ventilation airflow is given by the following equation:

<div>\[{Q_{u,v}} = {\left[ {{{\left( {\sum\limits_i {{Q_{v,Natural,i}}}  + \sum\limits_i {{Q_{v,Wind,i}}} } \right)}^2} + {{\left( {\sum\limits_i {{Q_{v,Intake,i}}} } \right)}^2} + {{\left( {\sum\limits_i {{Q_{v,Exhaust,i}}} } \right)}^2} + {{\left( {\sum\limits_i {Max\left( {0,\;({Q_{ERV,Exh,i}} - {Q_{ERV,Sup,i}})} \right)} } \right)}^2}} \right]^{0.5}}\]</div>

where

*Q<sub>v,Exhaust,i</sub>*      = Ventilation rate with “Exhaust” type in the ith ZoneVentilation:DesignFlowRate object for the same zone

*Q<sub>v,Intake,i</sub>*        = Ventilation rate with “Intake” type in the ith ZoneVentilation:DesignFlowRate object for the same zone

*Q<sub>v,Natural,i</sub>*       = Ventilation rate with “Natural” type in the ith ZoneVentilation:DesignFlowRate object for the same zone

*Q<sub>v,Wind,v</sub>*        = Ventilation rate in the ith ZoneVentilation:WindandStackOpenArea object for the same zone

*Q<sub>ERV,Sup,i</sub>*       = Supply flow rate given in the ith ZoneHVAC:EnergyRecoveryVentilator object

*Q<sub>ERV,Exh,i</sub>*       = Exhaust flow rate given in the ith ZoneHVAC:EnergyRecoveryVentilator object

For Ventilation Type = Intake in the ZoneVentilation:DesignFlowRate object, an appropriate amount of fan heat will be ignored and the outdoor temperature will be used in the zone air heat balance equation.

This object provides a simple airflow interaction model without having to use the AirflowNetwork capabilities, when the Air Balance Method is specified as Quadrature.

### Reference

ASHRAE. 2009. 2009 ASHRAE Handbook – Fundamentals, Chapter 16, Atlanta: American Society of Heating, Refrigerating, and Air-Conditioning Engineers, Inc.

Air Exchange
------------

Air exchange and interchange between zones is treated as a convective gain. Temperature-difference-controlled or constant air mixing can be specified as a one-way or cross-zone phenomenon modeled using the ZoneMixing or ZoneCrossMixing objects. Air exchange through doorways between refrigerated spaces can be modeled using the ZoneRefrigerationMixing object.

For one-way mixing (using ZoneMixing object(s)), the mixing air flow is only used for the energy and mass balance for the receiving zone. The mass referred in this section includes air, water vapor and CO<sub>2</sub>. The source zone energy and mass balance are not effected, although the user may choose to enter complementary pairs of one-way mixing objects. Multiple mixing flows can be defined for any receiving zone. For cross-zone mixing (using ZoneCrossMixing object(s)), the mixing air flow impacts the mass and energy balances in both the source and receiving zones. No more than one ZoneCrossMixing object can be used for any receiving zone. A separate ZoneCrossMixing object must be used for each of the two zones exchanging air if the mixing flow is bi-directional and based on a temperature difference greater than zero.

For refrigerated space air exchange (using ZoneRefrigerationDoorMixing object(s)), the mixing air flow impacts the mass and energy balances in both the source and receiving zones. A single object accounts for the two-way air flow with the energy and mass exchanges determined by  the air density difference between the two zones.

### Temperature Difference Controlled Air Exchange

The volume of air flow into the receiving zone is specified by the user with a number of control parameters and schedules listed in the Input Output Guide. The user can turn this one-way flow on or off based on the temperature difference between the source and receiving zones, or it may be independent of the temperature difference. The density and specific heat of the air flowing into the receiving zone are determined using the average temperature and average humidity ratio in the source and receiving zones. The humidity ratio of the air flowing into the receiving zone is set equal to the humidity ratio of the source zone. The mass, moisture,and energy terms are then used as described in two previous sections, Basis for the Zone and Air System Integration, and Moisture Predictor-Corrector.

<div>\[\begin{array}{l}{\rho_{{\rm{Avg}}}} = f\left( {\frac{{{T_{{\rm{ReceivingZone}}}} + {T_{{\rm{SourceZone}}}}}}{2},\frac{{{W_{{\rm{ReceivingZone}}}} + {W_{{\rm{SourceZone}}}}}}{2},{P_{{\rm{Barometric}}}}} \right)\\\{c_{{\rm{P,Avg}}}} = f\left( {\frac{{{T_{{\rm{ReceivingZone}}}} + {T_{{\rm{SourceZone}}}}}}{2},\frac{{{W_{{\rm{ReceivingZone}}}} + {W_{{\rm{SourceZone}}}}}}{2}} \right)\\\{{\dot m}_{{\rm{MixingFlowToReceivingZone}}}} = \sum\limits_{{\rm{All Source Zones}}} {\left( {{\rho_{{\rm{Avg}}}} \times {{\dot V}_{{\rm{Air}}}}} \right)} \\\{{\dot Q}_{{\rm{MixingFlowToReceivingZone}}}} = \sum\limits_{{\rm{All Source Zones}}} {\left[ {{\rho_{{\rm{Avg}}}} \times {c_{{\rm{P,Avg}}}} \times {{\dot V}_{{\rm{Air}}}} \times \left( {{T_{{\rm{SourceZone}}}} - {T_{{\rm{ReceivingZone}}}}} \right)} \right]} \\Moistur{e_{{\rm{MixingFlowToReceivingZone}}}} = \sum\limits_{{\rm{All Source Zones}}} {\left[ {{\rho_{{\rm{Avg}}}} \times {{\dot V}_{{\rm{Air}}}} \times \left( {{W_{{\rm{SourceZone}}}}} \right)} \right]} \end{array}\]</div>

where:

c<sub>P,Avg</sub>                                   = Average specific heat of air within the two zones (J/kg.K)

<div img="image1901.txt">\({\dot m_{{\rm{MixingFlowToReceivingZone}}}}\)</div>          = Mass of moist air flowing into the receiving zone (kg<sub>air</sub>/s)

Moisture<sub>MixingFlowToReceivingZone</sub>    = Moisture mass flow rate into the receiving zone (kg<sub>H2O</sub>/s)

P<sub>Barometric</sub>                              = Outside barometric pressure (Pa)

ρ<sub>Avg\\                                                                         </sub> = Average density of air within the two zones (kg/s)

<div img="image1902.txt">\({\dot Q_{{\rm{MixingFlowToReceivingZone}}}}\)</div>          = Energy added to receiving zone air by mixing mass flow (W)

T<sub>ReceivingZone                                                      </sub> = Temperature in the Receiving Zone (<sup>o</sup>C)

T<sub>SourceZone</sub>                              = Temperature in the Source Zone (<sup>o</sup>C)

<div img="image1903.txt">\({\dot V_{Air}}\)</div>                                   = Volume rate of air flow defined by the user (m<sup>3</sup>/s)

W<sub>ReceivingZone</sub>              = Humidity Ratio in the Receiving Zone (kg<sub>H2O</sub>/kg<sub>dry\\ air</sub>)

W<sub>SourceZone</sub>                             = Humidity Ratio in the Source Zone (kg<sub>H2O</sub>/kg<sub>dry\\ air</sub>)

For cross-mixing, the mass of moist air exchanged between the two zones is assumed to be equal.  Again, the density and specific heat are based on the average conditions in the two zones. Note that the temperature and humidity ratio differences ensure that when the energy and  moisture terms are used in the Moisture Predictor-Corrector, they correctly reflect a loss or gain in each zone.

<div>\[\begin{array}{l}{{\dot m}_{{\rm{MixingFlowToReceivingZone}}}} = {{\dot m}_{{\rm{MixingFlowToSourceZone}}}} = {\rho_{{\rm{Avg}}}} \times {{\dot V}_{{\rm{Air}}}}\\\{{\dot Q}_{{\rm{MixingFlowToSourceZone}}}} = {\rho_{{\rm{Avg}}}} \times {c_{{\rm{P,Avg}}}} \times {{\dot V}_{{\rm{Air}}}} \times \left( {{T_{{\rm{ReceivingZone}}}} - {T_{{\rm{SourceZone}}}}} \right)\\\{{\dot Q}_{{\rm{MixingFlowToReceivingZone}}}} = {\rho_{{\rm{Avg}}}} \times {c_{{\rm{P,Avg}}}} \times {{\dot V}_{{\rm{Air}}}} \times \left( {{T_{{\rm{SourceZone}}}} - {T_{{\rm{ReceivingZone}}}}} \right)\\Moistur{e_{{\rm{MixingFlowToSourceZone}}}} = {\rho_{{\rm{Avg}}}} \times {{\dot V}_{{\rm{Air}}}} \times \left( {{W_{{\rm{ReceivingZone}}}}} \right)\\Moistur{e_{{\rm{MixingFlowToReceivingZone}}}} = {\rho_{{\rm{Avg}}}} \times {{\dot V}_{{\rm{Air}}}} \times \left( {{W_{{\rm{SourceZone}}}}} \right)\end{array}\]</div>

where:

<div img="image1905.txt">\({\dot m_{{\rm{MixingFlowToSourceZone}}}}\)</div>             = Mass of moist air flowing into the source zone (kg<sub>air</sub>/s)

Moisture<sub>MixingFlowToSourceZone</sub>       = Moisture mass flow rate into the source zone (kg<sub>H2O</sub>/s)

<div img="image1906.txt">\({\dot Q_{{\rm{MixingFlowToSourceZone}}}}\)</div>             = Sensible energy added to source zone air by mixing mass flow (W)

<div img="image1907.txt">\({\dot Q_{{\rm{MixingFlowToReceivingZone}}}}\)</div>          = Sensible energy added to receiving zone air by mixing mass flow, W

<div img="image1908.txt">\(Moistur{e_{{\rm{MixingFlowToSourceZone}}}}\)</div>= Latent load added to source zone air by mixing mass flow (kg<sub>H2O</sub>/s)

<div img="image1909.txt">\(Moistur{e_{{\rm{MixingFlowToReceivingZone}}}}\)</div>= Latent load added to receiving zone air by mixing mass flow (kg<sub>H2O</sub>/s)

### Density Difference Controlled Air Exchange

When closed refrigerated spaces exchange air with other closed spaces, the air flow is determined by the difference in air density between the two spaces. The fundamental assumption for this case is that the mass of dry air exchanged between the two spaces is the same.(Gosney and Olama,  1975] This assumption applies to situations where the colder of the two spaces is essentially sealed to other air flows, that is, there are no open doors or exhaust air flows. Multiple refrigeration door mixing objects can be used for the zone, but if there are multiple doors open at the same time for any significant amount of time, the model will not give results appropriate for that condition.

The sensible and latent energy loads are modeled according to the guidance specified in (ASHRAE 2006d, ASHRAE 2009, and Gosney and Olama, 1975).  Equal dry air exchange is assumed, that is, the mass of dry air infiltrating into the receiving zone is assumed to equal the mass of dry air infiltrating out of the source zone.

<div>\[\begin{array}{l}{{\rm{h}}_{{\rm{ZoneA}}}},{\rho_{{\rm{ZoneA}}}} = f({T_{{\rm{ZoneA}}}},{W_{{\rm{ZoneA}}}},{P_{{\rm{Barometric}}}})\\\{{\rm{h}}_{{\rm{ZoneB}}}},{\rho_{{\rm{ZoneB}}}} = f({T_{{\rm{ZoneB}}}},{W_{{\rm{ZoneB}}}},{P_{{\rm{Barometric}}}})\\\{\rho_{{\rm{ZoneA}}}} > {\rho_{{\rm{ZoneB}}}}\\\{{{\rm{\dot Q}}}_{{\rm{Mixing}}}}{\rm{ }} = {\rm{ }}{{{\rm{\dot Q}}}_{{\rm{FullFlow}}}}{\rm{  }} \times {\rm{  Schedul}}{{\rm{e}}_{{\rm{DoorOpen}}}}{\rm{  }} \times {\rm{ }}{{\rm{F}}_{{\rm{Flow}}}}{\rm{  }} \times {\rm{ (1  -  }}{{\rm{F}}_{{\rm{Protection}}}}{\rm{)}}\\\{{{\rm{\dot Q}}}_{{\rm{FullFlow}}}} = B \times {\rm{(}}{{\rm{h}}_{{\rm{ZoneB}}}}{\rm{ - }}{{\rm{h}}_{{\rm{ZoneA}}}}{\rm{)}}\\\{\rm{B}} = {\rm{0}}{\rm{.221}} \times {{\rm{A}}_{{\rm{Door}}}} \times {\rho_{{\rm{ZoneA}}}} \times {{\rm{F}}_{{\rm{Density}}}}\sqrt {\left( {{\rm{1 - }}\frac{{{\rho_{{\rm{ZoneB}}}}}}{{{\rho_{{\rm{ZoneA}}}}}}} \right) \times {\rm{g}} \times {{\rm{H}}_{{\rm{Door}}}}} \\\{{\rm{F}}_{{\rm{Density}}}}{\rm{ }} = {\left[ {\frac{2}{{1 + {{\left( {{\raise0.7ex\hbox{${{\rho_{{\rm{ZoneA}}}}}$} \!\mathord{\left/ {\vphantom {{{\rho_{{\rm{ZoneA}}}}} {{\rho_{{\rm{ZoneB}}}}}}}\right.}\!\lower0.7ex\hbox{${{\rho_{{\rm{ZoneB}}}}}$}}} \right)}^{{\raise0.7ex\hbox{$1$} \!\mathord{\left/ {\vphantom {1 3}}\right.}\!\lower0.7ex\hbox{$3$}}}}}}} \right]^{{\rm{1}}{\rm{.5}}}}{\rm{ }}\\\{{{\rm{\dot m}}}_{{\rm{DryAirZonesAB }}}} = \frac{{{{{\rm{\dot Q}}}_{{\rm{Mixing}}}}}}{{{\rm{(}}{{\rm{h}}_{{\rm{ZoneB}}}}{\rm{ - }}{{\rm{h}}_{{\rm{ZoneA}}}}{\rm{)}}}}{\rm{ }} = {\rm{B }} \times {\rm{  Schedul}}{{\rm{e}}_{{\rm{DoorOpen}}}}{\rm{  }} \times {\rm{ }}{{\rm{F}}_{{\rm{Flow}}}}{\rm{  }} \times {\rm{ (1  -  }}{{\rm{F}}_{{\rm{Protection}}}}{\rm{)}}\\\{{\dot m}_{{\rm{MixingFlowZoneBtoA}}}} = \sum\limits_{{\rm{All ZoneBs}}} {\left( {{{{\rm{\dot m}}}_{{\rm{DryAirZonesAB }}}} \times \left( {1 + {{\rm{W}}_{{\rm{ZoneB}}}}} \right)} \right)} \\\{{\dot Q}_{{\rm{MixingFlowZoneBtoA}}}} = \sum\limits_{{\rm{All ZoneBs}}} {\left[ {{{{\rm{\dot m}}}_{{\rm{ZoneBtoA }}}} \times {c_{{\rm{P,ZoneB}}}} \times \left( {{T_{{\rm{ZoneB}}}} - {T_{{\rm{ZoneA}}}}} \right)} \right]} \\Moistur{e_{{\rm{MixingFlowZoneBtoA}}}} = \sum\limits_{{\rm{All ZoneBs}}} {\left[ {{{{\rm{\dot m}}}_{{\rm{ZoneBtoA }}}} \times \left( {{W_{{\rm{ZoneB}}}} - {{\rm{W}}_{{\rm{ZoneA}}}}} \right)} \right]} \\\{{\dot m}_{{\rm{MixingFlowZoneAtoB}}}} = \sum\limits_{{\rm{All ZoneAs}}} {\left( {{{{\rm{\dot m}}}_{{\rm{DryAirZonesAB }}}} \times \left( {1 + {{\rm{W}}_{{\rm{ZoneA}}}}} \right)} \right)} \\\{{\dot Q}_{{\rm{MixingFlowZoneAtoB}}}} = \sum\limits_{{\rm{All ZoneAs}}} {\left[ {{{{\rm{\dot m}}}_{{\rm{ZoneAtoB}}}} \times {c_{{\rm{P,ZoneA}}}} \times \left( {{T_{{\rm{ZoneA}}}} - {T_{{\rm{ZoneB}}}}} \right)} \right]} \\Moistur{e_{{\rm{MixingFlowZoneAtoB}}}} = \sum\limits_{{\rm{All ZoneAs}}} {\left[ {{{{\rm{\dot m}}}_{{\rm{ZoneAtoB}}}} \times \left( {{W_{{\rm{ZoneA}}}} - {W_{{\rm{ZoneB}}}}} \right)} \right]} \end{array}\]</div>

where:

A<sub>door</sub>            =  Area of door between Zones A and B (m<sup>2</sup>)

F<sub>Flow</sub>            =  Doorway flow factor, = 0.8 if ΔT &gt; 11<sup>o</sup>C; =1.1 if ΔT &lt;= 11<sup>o</sup>C

F<sub>Protection</sub>        = Doorway protection factor, = 0 for no protection; =  0.5 for an air curtain; and 0.9 for a strip curtain (dimensionless)

g                =  Gravitational constant (m/s<sup>2</sup>)

h<sub>ZoneA                     </sub> =  enthalpy of the air within Zone A (J/kg)

h<sub>ZoneB                     </sub> =  enthalpy of the air within Zone B (J/kg)

H<sub>door</sub>            =  Height of door between source and receiving zones (m)

Q<sub>FullFlow</sub>        = Sensible and latent refrigeration load (on Zone A) for fully established flow (W)

Q<sub>Mixing</sub>          = Sensible and latent mixing refrigeration load on Zone A for the time step (W)

m<sub>DryAirZoneAB</sub>   = Mass of dry air exchanged between zones A and B (kg<sub>air</sub>/s)

Schedule<sub>DoorOpen</sub>        = Value scheduled by user, fraction of time door open during time step (dimensionless)

W<sub>ZoneA                  </sub> =  Humidity ratio of the air within Zone A (kg<sub>H2O</sub>/kg<sub>air</sub>)

W<sub>ZoneB</sub>          =  Humidity ratio of air within Zone B (kg<sub>H2O</sub>/kg<sub>air</sub>)

Ρ<sub>ZoneA                    </sub> =  Density of air within Zone A (kg/m<sup>3</sup>)

ρ<sub>ZoneB</sub>           =  Density of air within Zone B (kg/m<sup>3</sup>)

### References

ASHRAE. 2006d. *Refrigeration Handbook*, Chapter 13. Atlanta: American Society of Heating,

Refrigerating and Air-Conditioning Engineers, Inc.

ASHRAE. 2009. *Fundamentals Handbook*, Chapter 1. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.

Gosney, W.B., Olama, G.A.-L. 1975. Heat and Enthalpy Gains through Cold Room Doorways,  Proceedings of the Institute of Refrigeration, vol. 72, pp 31-41

Calculation of Zone Air Temperature
-----------------------------------

The zone air heat balance is the primary mechanism for linking the loads calculation to the system simulation.  As such, the zone air temperature becomes the main interface variable.  Its role in the integration process was described previously (“Basis for the Zone and Air System Integration”).
