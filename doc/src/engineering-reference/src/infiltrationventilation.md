# Infiltration/Ventilation

## Infiltration

Any outdoor air that enters by way of infiltration is assumed to be immediately mixed with the zone air. The determination of the amount of infiltration air is quite complicated and subject to significant uncertainty. In the most common procedure, the infiltration quantity is converted from a number of air changes per hour (ACH) and included in the zone air heat balance using the outside temperature at the current simulation time step.

EnergyPlus contains three models for infiltration. The first is the "Design Flow Rate" model that was inherited from EnergyPlus' predecessor programs. It is accessed through the ZoneInfiltration:DesignFlowRate object and is based on environmental conditions modifying a design flow rate.  The second is the "Effective Leakage Area" model based on Sherman and Grimsrud (1980) and accessed using the ZoneInfiltration:EffectiveLeakageArea input object.  The third is the "Flow Coefficient" model based on Walker and Wilson (1998) and accessed using the ZoneInfiltration:FlowCoefficient input object. The model formulations for the Effective Leakage Area and Flow Coefficient models are from the ASHRAE Handbook of Fundamentals (2001 Chapter 26; 2005 Chapter 27) where they are referred to as "Basic" and "Enhanced", respectively.

## Infiltration Design Flow Rate

Infiltration (Ref Object: ZoneInfiltration:DesignFlowRate) is the unintended flow of air from the outdoor environment directly into a thermal zone. Infiltration is generally caused by the opening and closing of exterior doors, cracks around windows, and even in very small amounts through building elements. In this model, the user defines a design flow rate that can be modified by temperature differences and windspeed. The basic equation (Coblenz and Achenbach 1963) used to calculate infiltration with this model is:

![](media/image1819.png)\


More advanced infiltration calculations are possible using the EnergyPlus AirflowNetwork model for natural infiltration driven by wind when the HVAC system does not operate and/or driven by wind and forced air for times when the HVAC system operates. Exfiltration (the leakage of zone air to the outside) is generally handled better as zone exhaust air in the zone equipment description.

The question of typical values for these coefficients is subject to debate. Ideally, one should do a detailed analysis of the infiltration situation and then determine a custom set of coefficients using methods such as those laid out in Chapter 26 of the ASHRAE Handbook of Fundamentals. The EnergyPlus defaults are 1,0,0,0 which gives a constant volume flow of infiltration under all conditions.

BLAST (one of the EnergyPlus predecessors) used the following values as defaults:  0.606, 0.03636, 0.1177, 0. These coefficients produce a value of 1.0 at 0C deltaT and 3.35 m/s (7.5 mph) windspeed, which corresponds to a typical summer condition. At a winter condition of 40C deltaT and 6 m/s (13.4 mph) windspeed, these coefficients would increase the infiltration rate by a factor of 2.75.

In DOE-2 (the other EnergyPlus predecessor), the air change method defaults are (adjusted to SI units) 0, 0, 0.224 (windspeed), 0. With these coefficients, the summer conditions above would give a factor of 0.75, and the winter conditions would give 1.34. A windspeed of 4.47 m/s (10 mph) gives a factor of 1.0.

The source of the BLAST defaults is noted in the BLAST documentation as:

"Empirical equation and the coefficient default were determined from ASHRAE journal articles and other data on the effects of outdoor weather conditions."

The source of the DOE-2 defaults is based on examining the infiltration relationships described in the ASHRAE Handbook of Fundamentals.

The EnergyPlus example files use all of the above, the BLAST defaults in some (e.g., GeometryTest), the DOE-2 defaults in some (e.g., 5ZoneAirCooled), and the EnergyPlus defaults in some (e.g., LgOffVAVDetCoil).

## Infiltration by Effective Leakage Area 

The Effective Leakage Area model is based on Sherman and Grimsrud (1980) and accessed using the ZoneInfiltration:EffectiveLeakageArea input object.  The model formulation used in EnergyPlus is from the ASHRAE Handbook of Fundamentals (2001 Chapter 26; 2005 Chapter 27) where it is referred to as the "Basic" model.

The Effective Leakage Area, or Sherman-Grimsrud, model is:

![](media/image1820.png)\


where,

![](media/image1821.png)  is a value from a user-defined schedule,

![](media/image1822.png)  is the effective air leakage area in cm^2^ that corresponds to a 4 Pa pressure differential,

![](media/image1823.png)  is the coefficient for stack-induced infiltration in (L/s)^2^/(cm^4^·K),

![](media/image1824.png)  is the absolute temperature difference between zone air and outdoor air,

![](media/image1825.png)  is the coefficient for wind-induced infiltration in (L/s)^2^/(cm^4^·(m/s)^2^), and

![](media/image1826.png)  is the local wind speed.

## Infiltration by Flow Coefficient 

The Flow Coefficient model is based on Walker and Wilson (1998) and accessed using the ZoneInfiltration:FlowCoefficient input object.  The model formulation used in EnergyPlus is from the ASHRAE Handbook of Fundamentals (2001 Chapter 26; 2005 Chapter 27) where it is referred to as the "Enhanced" or "AIM-2" model.

The Enhanced, or AIM-2, model is:

![](media/image1827.png)\


where,

![](media/image1828.png)  is a value from a user-defined schedule,

![](media/image1829.png)  is the flow coefficient in m^3^/(s·Pa^n^),

![](media/image1830.png)  is the coefficient for stack-induced infiltration in (Pa/K)^n^,

![](media/image1831.png)  is the pressure exponent,

![](media/image1832.png)  is the coefficient for wind-induced infiltration in (Pa·s^2^/m^2^)^n^, and

![](media/image1833.png)  is the shelter factor.

### References:

Coblenz, C. W. and Achenbach, P. R. 1963. Field Measurement of Ten Electrically-Heated Houses. ASHRAE Transactions pp 358-365.

Sherman, M.H. and D.T. Grimsrud. 1980. Infiltration-pressurization correlation: Simplified physical modeling. ASHRAE Transactions 86(2):778

Walker, I.S., and D.J. Wilson. 1998. Field validation of equations for stack and wind driven air infiltration calculations.  International Journal of HVAC&R Research 4(2).

ASHRAE Handbook of Fundamentals. 2005. Chapter 27. (and 2001 Chapter 26).

## Ventilation

EnergyPlus contains two models for ventilation.  The "Design Flow Rate" model, inherited from EnergyPlus' predecessor programs, is accessed through the ZoneVentilation:DesignFlowRate object and is based on environmental conditions modifying a design flow rate.  The "Wind and Stack with Open Area" model, based on equations defined in Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals, is accessed using the ZoneVentilation:WindandStackOpenArea input object. Since the "Wind and Stack with Open Area" object requires the height difference between the midpoint of the lower opening and the neutral pressure level, which is difficult to estimate, this object should be used with care (e.g., research only).

These two ventilation objects can be used alone or in combination to determine ventilation air for a zone. If multiple ZoneVentilation:\* objects are specified for a zone, then the total zone ventilation flow rate is the sum of the ventilation air flow rates calculated by each ZoneVentilation object.

## Ventilation Design Flow Rate

Ventilation (Ref Object: ZoneVentilation:DesignFlowRate) is the purposeful flow of air from the outdoor environment directly into a thermal zone in order to provide some amount of non-mechanical cooling.  Ventilation as specified by this input syntax is intended to model "simple" ventilation as opposed to the more detailed ventilation investigations that can be performed with the AirflowNetwork model.  Simple ventilation in EnergyPlus can be controlled by a schedule and through the specification of minimum, maximum and delta temperatures. The temperatures can be either single constant values for the entire simulation or schedules which can vary over time. Specific details are given in the Input/Output reference document. As with infiltration, the actual flow rate of ventilation can be modified by the temperature difference between the inside and outside environment and the wind speed. The basic equation used to calculate ventilation using this model is:

![](media/image1834.png)\


More advanced ventilation calculations are possible using the EnergyPlus AirflowNetwork model.

The following description is copied from the Infiltration discussion above. The question of typical values for these coefficients is subject to debate. Ideally, one should do a detailed analysis of the ventilation situation and then determine a custom set of coefficients using methods such as those laid out in Chapter 26 of the ASHRAE Handbook of Fundamentals. The EnergyPlus defaults are 1,0,0,0 which gives a constant volume flow of ventilation under all conditions.

BLAST (one of the EnergyPlus predecessors) used the following values as defaults:  0.606, 0.03636, 0.1177, 0. These coefficients produce a value of 1.0 at 0C deltaT and 3.35 m/s (7.5 mph) windspeed, which corresponds to a typical summer condition. At a winter condition of 40C deltaT and 6 m/s (13.4 mph) windspeed, these coefficients would increase the ventilation rate by a factor of 2.75.

In DOE-2 (the other EnergyPlus predecessor), the air change method defaults are (adjusted to SI units) 0, 0, 0.224 (windspeed), 0. With these coefficients, the summer conditions above would give a factor of 0.75, and the winter conditions would give 1.34. A windspeed of 4.47 m/s (10 mph) gives a factor of 1.0.

The source of the BLAST defaults is noted in the BLAST documentation as:

"Empirical equation and the coefficient default were determined from ASHRAE journal articles and other data on the effects of outdoor weather conditions."

The source of the DOE-2 defaults is based on examining the infiltration relationships described in the ASHRAE Handbook of Fundamentals.

The EnergyPlus example files use all of the above, the BLAST defaults in some (e.g., AirflowNetwork_Simple_house), the DOE-2 defaults in some (e.g., VentilationSimpleTest – has all 3), and the EnergyPlus defaults in some (e.g., 5ZoneNightVent2).

## Ventilation by Wind and Stack with Open Area

For this model (Ref Object: ZoneVentilation:WindandStackOpenArea), the ventilation air flow rate is a function of wind speed and thermal stack effect, along with the area of the opening being modeled. This object can be used alone or in combination with ZoneVentilation:DesignFlowRate objects. This model is intended for simplified ventilation calculations as opposed to the more detailed ventilation investigations that can be performed with the AirflowNetwork model. Using the "Wind and Stack with Open Area" model, the natural ventilation flow rate can be controlled by a multiplier fraction schedule applied to the user-defined opening area and through the specification of minimum, maximum and delta temperatures. The temperatures can be either single constant values for the entire simulation or schedules which can vary over time. The equation used to calculate the ventilation rate driven by wind is given by Eq. 37 in Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals:

![](media/image1835.png)\


where,

*Q~w~*= Volumetric air flow rate driven by wind [m^3^/s]

*C~w~*= Opening effectiveness [dimensionless]

*A~opening~*= Opening area [m^2^]

*F~schedule~* = Open area fraction [user-defined schedule value, dimensionless]

*V*= Local wind speed [m/s]

If the user specifies "Autocalculate" for the Opening Effectiveness input field, the opening effectiveness is calculated for each simulation time step based on the angle between the actual wind direction and the Effective Angle (a user-defined input) using the following equation:

![](media/image1836.png)\


The difference |EffectiveAngle – WindDirection| should be between 0 and 180 degrees. If the difference |EffectivAngle – WindDirection| is greater than 180, the difference is reset to be minus 180 degrees. This equation is a linear interpolation using the values recommended by the 2009 ASHRAE Handbook of Fundamentals (page 16.13): 0.5 to 0.6 for perpendicular winds and 0.25 to 0.35 for diagonal winds.

The equation used for calculating the ventilation rate due to stack effect is given by Eq. 38 in Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals:

![](media/image1837.png)\


where,

*Q~s~*= Volumetric air flow rate due to stack effect [m^3^/s]

*C~D~*= Discharge coefficient for opening [dimensionless]

*A~opening~*= Opening area [m^2^]

*F~schedule~* = Open area fraction [user-defined schedule value, dimensionless]

*ΔH~NPL~*= Height from midpoint of lower opening to the neutral pressure level [m].

Estimation of this value is difficult; refer to Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals for guidance.

*T~zone~*= Zone air dry-bulb temperature [K]

*T~odb~*= Local outdoor air dry-bulb temperature [K]

The following equation, given by Eq. 39 in Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals, is used to calculate the Discharge Coefficient for Opening when the user sets the value for this input field to "Autocalculate":

![](media/image1838.png)\


The total ventilation rate calculated by this model is the quadrature sum of the wind and stack air flow components:

![](media/image1839.png)\


If desired, a simple summation, instead of quadrature summation, can be realized by inputting two ZoneVentilation:WindAndStackOpenArea objects. One object can be defined with only a wind-driven component by setting C~D~=0, and the other object can have only stack-effect inputs specified and set C~w~=0.

## Zone Air Balance Outdoor Airflow (ZoneAirBalance:OutdoorAir) 

ASHRAE 2009 Handbook of Fundamentals specifies that any unbalanced supply or exhaust ventilation air, Q~u,v~, to a zone causes pressurization/depressurization that influences the flow of infiltration air and thus should be combined with natural infiltration, Q~n~, (and, if present, unbalanced duct leakage, Qu,l) in superposition. Balanced ventilation airflow, Q~b,v~, to a zone (such as an ERV or HRV with balanced exhaust and intake air flows) does not interact with infiltration air and is added in whole:

![](media/image1840.png)\


where,

*Q*= Combined outdoor airflow with infiltration, balanced and unbalanced outdoor air flows, and unbalanced duct leakage [m^3^/s]

*Q~n~*= Natural infiltration airflow [m^3^/s]

*Q~b,v~*= Balanced ventilation airflow, excluding infiltration [m^3^/s]

*Q~u,v~*= Unbalanced ventilation airflow, excluding infiltration [m^3^/s]

*Q~u,l~*= Unbalanced duct leakage: the difference between supply and return leaks [m^3^/s]

The natural infiltration airflow includes all outdoor airflows from all ZoneInfiltration:\* objects for the same zone.

![](media/image1841.png)\


where,

*Q~Infiltration,i~*= Outdoor airflow rate given in the ith ZoneInfiltration:\* objects for the same zone

The balanced ventilation airflow is the sum of outdoor airflows from all ZoneVentilation: DesignFlowRate objects with Ventilation Type = Balanced:

![](media/image1842.png)\


where,

*Q~v,Balanced,i~*= Ventilation rate with "Balanced" ventilation type defined in the ith ZoneVentilation:DesignFlowRate object for the same zone

The unbalanced ventilation airflow is given by the following equation:

![](media/image1843.png)\


where

*Q~v,Exhaust,i~*= Ventilation rate with "Exhaust" type in the ith ZoneVentilation:DesignFlowRate object for the same zone

*Q~v,Intake,i~*= Ventilation rate with "Intake" type in the ith ZoneVentilation:DesignFlowRate object for the same zone

*Q~v,Natural,i~*= Ventilation rate with "Natural" type in the ith ZoneVentilation:DesignFlowRate object for the same zone

*Q~v,Wind,v~*= Ventilation rate in the ith ZoneVentilation:WindandStackOpenArea object for the same zone

*Q~ERV,Sup,i~*= Supply flow rate given in the ith ZoneHVAC:EnergyRecoveryVentilator object

*Q~ERV,Exh,i~*= Exhaust flow rate given in the ith ZoneHVAC:EnergyRecoveryVentilator object

For Ventilation Type = Intake in the ZoneVentilation:DesignFlowRate object, an appropriate amount of fan heat will be ignored and the outdoor temperature will be used in the zone air heat balance equation.

This object provides a simple airflow interaction model without having to use the AirflowNetwork capabilities, when the Air Balance Method is specified as Quadrature.

## Reference

ASHRAE. 2009. 2009 ASHRAE Handbook – Fundamentals, Chapter 16, Atlanta: American Society of Heating, Refrigerating, and Air-Conditioning Engineers, Inc.