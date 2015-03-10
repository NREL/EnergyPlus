# Group – Airflow

An important characteristic of energy consumption in buildings is the airflow between zones and airflow due to natural ventilation (e.g., open windows) or mechanically-induced ventilation (e.g., exhaust air fans). This group of objects describes those elements.

The AirflowNetwork model can also be used to model infiltration and mixing (zone-to-zone air flow) with or without the HVAC air distribution system operating (see Group – Airflow Network).

## ZoneInfiltration:DesignFlowRate

Infiltration is the unintended flow of air from the outdoor environment directly into a thermal zone. Infiltration is generally caused by the opening and closing of exterior doors, cracks around windows, and even in very small amounts through building elements. The basic equation used to calculate infiltration with this object is:

![](media/image109.png)\


More advanced infiltration calculations are possible using the EnergyPlus AirflowNetwork model for natural infiltration driven by wind and/or by forced air. Infiltration described by the equation shown above is entered into EnergyPlus using the following syntax. Exfiltration (the leakage of zone air to the outside) is generally handled better as zone exhaust air in the zone equipment description. The equation must always yield a non-negative results; negative values are set to 0.0.

The question of typical values for these coefficients is subject to debate. Ideally, one should do a detailed analysis of the infiltration situation and then determine a custom set of coefficients using methods such as those laid out in Chapter 26 of the ASHRAE Handbook of Fundamentals. The EnergyPlus defaults are 1,0,0,0 which give a constant volume flow of infiltration under all conditions.

BLAST (one of the EnergyPlus predecessors) used the following values as defaults:  0.606, 0.03636, 0.1177, 0. These coefficients produce a value of 1.0 at 0C deltaT and 3.35 m/s (7.5 mph) windspeed, which corresponds to a typical summer condition. At a winter condition of 40C deltaT and 6 m/s (13.4 mph) windspeed, these coefficients would increase the infiltration rate by a factor of 2.75.

In DOE-2 (the other EnergyPlus predecessor), the air change method defaults are (adjusted to SI units) 0, 0, 0.224 (windspeed), 0. With these coefficients, the summer conditions above would give a factor of 0.75, and the winter conditions would give 1.34. A windspeed of 4.47 m/s (10 mph) gives a factor of 1.0.

The source of the BLAST defaults is noted in the BLAST documentation as:

"Empirical equation and the coefficient default were determined from ASHRAE journal articles and other data on the effects of outdoor weather conditions."

The source of the DOE-2 defaults is based on examining the infiltration relationships described in the ASHRAE Handbook of Fundamentals.

The EnergyPlus example files use all of the above, the BLAST defaults in some (e.g., GeometryTest), the DOE-2 defaults in some (e.g., 5ZoneAirCooled), and the EnergyPlus defaults in some (e.g., LgOffVAVDetCoil).

The local outdoor dry-bulb temperature used in the above basic equation (T~odb~) is typically a function of the height of the zone centroid above ground. The corresponding zone name is given in the second field. The local outdoor dry-bulb temperature calculation procedure is given in the section of "Local Outdoor Air Temperature Calculation" in the Engineering Reference.

The local outdoor wind speed used in the above basic equation (WindSpeed) is also a function of the height of the zone centroid above ground. The corresponding zone name is given in the second filed. The local outdoor wind speed calculation procedure is given in the section of "Local Wind Speed Calculation" in the Engineering Reference.

> Note: When the value of the Wind Speed Profile Exponent field in the [Site:HeightVariation](#siteheightvariation) is equal to 0.0. The local wind speed is always equal to the wind speed given in the weather data and will not be dependent on zone centroid height.  Similarly, if the value of the Air Temperature Gradient Coefficient is set equal to 0 the local air dry-bulb temperature is also always equal to the air dry-bulb temperature given in the weather data and will not be dependent on zone centroid height.

One or more infiltration objects can be defined for each zone, and the resulting infiltration rate for the zone will simply be the summation of the flow rates specified by the infiltration objects.

### Inputs

#### Field: Name

The name of the [ZoneInfiltration:DesignFlowRate](#zoneinfiltrationdesignflowrate) object. This needs to be unique across all different ZoneInfiltration objects.

#### Field: Zone or ZoneList Name

This field is the name of the zone (ref: [Zone](#zone)) or [ZoneList](#zonelist) (ref: ZoneLIst) and attaches a particular infiltration statement to a thermal zone or set of thermal zones in the building. When the [ZoneList](#zonelist) option is used then this infiltration definition is applied to each of the zones in the zone list effecting a global definition for the amount of infiltration flow rate in the zone. The Zonelist option can be used effectively with the flow/area, flow/exteriorarea, flow/exteriorwallarea, or airchanges/hour of the Design Flow Rate Calculation Method.

The name of the actual infiltration object becomes <[Zone](#zone) Name> <[ZoneInfiltration:DesignFlowRate](#zoneinfiltrationdesignflowrate) Object Name> and should be less than the standard length (100 characters) for a name field. If it is greater than this standard length, it may be difficult to specify in output reporting as it will be truncated. A warning will be shown if the generated name is greater than 100 characters. If it duplicates another such concatenated name, there will be a severe error and terminate the run.

#### Field: Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design volume flow rate (I~design~) (see Design Flow Rate Calculation Method field and related subsequent fields). This fraction between 0.0 and 1.0 is noted as F~schedule~ in the above equation.

#### Field: Design Flow Rate Calculation Method

This field is a key/choice field that tells which of the next four fields are filled and is descriptive of the method for calculating the design volume flow rate. The key/choices are:

- Flow/Zone
- With this choice, the method used will be a straight insertion of the design volume flow rate.  (The Design Flow Rate field should be filled.)
- Flow/Area
- With this choice, the method used will be a factor per floor area of the zone. (The flow per [Zone](#zone) Area field should be filled).
- Flow/ExteriorArea
- With this choice, the method used will be a factor per exterior surface area of the zone. (The flow per Exterior Surface Area field should be filled).
- Flow/ExteriorWallArea
- With this choice, the method used will be a factor per exterior wall surface area of the zone. (The flow per Exterior Surface Area field should be filled).
- AirChanges/Hour
- With this choice, the method used will be the number of air changes per hour for the infiltration amount. This factor, along with the [Zone](#zone) Volume, will be used to determine the Design Flow Rate. (The Air Changes per Hour field should be filled).

#### Field: Design Flow Rate

This field denotes the full design volume flow rate (m^3^/s). The previous field should choose "flow/zone" as the choice. The design volume flow rate (noted as I~design~ in the above equation) is the maximum amount of infiltration expected at design conditions. The design value is modified by the schedule fraction (see Field:Schedule Name) and user specified coefficients (see "coefficient" fields below). The resulting volume flow rate is converted to mass flow using the current outdoor air density at each time step.

#### Field: Flow per Zone Floor Area

This factor (m^3^/s-m^2^) is used, along with the [Zone](#zone) Area to determine the maximum Design Flow Rate as described in the Design Flow Rate field. The choice from the method field should be "Flow/Area".

#### Field: Flow per Exterior Surface Area

This factor (m^3^/s-m^2^) is used, along with the Exterior Surface Area in the [Zone](#zone) to determine the maximum Design Flow Rate as described in the Design Flow Rate field. The choice from the method field should be "Flow/Exteriorarea" or "Flow/ExteriorWallArea".

#### Field: Air Changes per Hour

This factor is used, along with the [Zone](#zone) Volume to determine the maximum Design Flow Rate as described in the Design Flow Rate field. The choice from the method field should be "AirChanges/Hour".

#### Field: Constant Term Coefficient

This number is the "A" parameter in the above infiltration equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter, however, is a constant under all conditions and is not modified by any environmental effect. As a result, it is dimensionless.

#### Field: Temperature Term Coefficient

This number is the "B" parameter in the above infiltration equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter is modified by the temperature difference between the outdoor and indoor air dry-bulb temperatures. The units for this parameter are inverse Celsius.

#### Field: Velocity Term Coefficient

This number is the "C" parameter in the above infiltration equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter is modified by the speed of wind being experienced outside the building. The units for this parameter are s/m.

#### Field: Velocity Squared Term Coefficient

This number is the "D" parameter in the above infiltration equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter is modified by square of the speed of wind being experienced outside the building. The units for this parameter are s^2^/m^2^.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    ZoneInfiltration:DesignFlowRate,
      Infiltration 1,   !- Name
      DORM ROOMS AND COMMON AREAS, !- Zone Name
      Infiltration Sch, !- Schedule Name
      Flow/Zone,      !- Design Flow Rate Calculation Method
      2.831685,       !- Design Flow Rate {m3/s}
      ,               !- Flow per Zone Floor Area {m3/s/m2}
      ,               !- Flow per Exterior Surface Area {m3/s/m2}
      ,               !- Air Changes per Hour
      0.6060000    ,  !- Constant Term Coefficient
      3.6359996E-02,  !- Temperature Term Coefficient
      0.1177165    ,  !- Velocity Term Coefficient
      0.0000000E+00;  !- Velocity Squared Term Coefficient
~~~~~~~~~~~~~~~~~~~~

Global Infiltration example:

~~~~~~~~~~~~~~~~~~~~

      ZoneList,OfficeZones,Left Fork, Middle Fork, Right Fork;

      ZoneInfiltration:DesignFlowRate,
        OfficeZones Infiltration,       !- Name
        OfficeZones,             !- Zone or ZoneList Name
        Infiltration Sch,        !- Schedule Name
        AirChanges/Hour,         !- Design Flow Rate Calculation Method
        ,                        !- Design Flow Rate {m3/s}
        ,                        !- Flow per Zone Floor Area {m3/s-m2}
        ,                        !- Flow per Exterior Surface Area {m3/s-m2}
        4.7,                     !- Air Changes per Hour
        0.6060000,               !- Constant Term Coefficient
        3.6359996E-02,           !- Temperature Term Coefficient
        0.1177165,               !- Velocity Term Coefficient
        0.0000000E+00;           !- Velocity Squared Term Coefficient
~~~~~~~~~~~~~~~~~~~~

## ZoneInfiltration:EffectiveLeakageArea

[ZoneInfiltration:EffectiveLeakageArea](#zoneinfiltrationeffectiveleakagearea) model is similar to the other infiltration objects but uses a different equation to model the unintended flow of air from the outdoor environment directly into a thermal zone.  Infiltration is generally caused by the opening and closing of exterior doors, cracks around windows, and even in very small amounts through building elements.  This model is based on work by Sherman and Grimsrud (1980) and is appropriate for smaller, residential-type buildings.  The equation used to calculate infiltration in the effective leakage area model is:

![](media/image110.png)\


where ![](media/image111.png)  is the average difference between zone air temperature and the outdoor air temperature and the other coefficients are described below.

Note that the coefficients for the "EffectiveLeakageArea" model are not interchangeable with the similarly named coefficients in the "FlowCoefficient" model (see [ZoneInfiltration:FlowCoefficient](#zoneinfiltrationflowcoefficient) object).

One or more infiltration objects of different types can be defined for each zone, and the resulting infiltration rate for the zone will simply be the summation of the flow rates specified by the infiltration objects.

### Inputs

#### Field: Name

The name of the [ZoneInfiltration:EffectiveLeakageArea](#zoneinfiltrationeffectiveleakagearea) object.  This needs to be unique across all different ZoneInfiltration objects.

#### Field: Zone Name

This field is the name of the zone (ref: [Zone](#zone)) and attaches a particular infiltration statement to a thermal zone in the building.

#### Field: Schedule Name

This field is the name of a schedule (ref: Schedule) that modifies the volume flow rate calculated by the model. This fraction between 0.0 and 1.0 is noted as F~schedule~ in the above equation.

#### Field: Effective Air Leakage Area

This field is the effective air leakage area, in cm^2^, at 4 Pa.  This is the value ![](media/image112.png) in the equation above.  Effective leakage area data can be obtained from a whole-building pressure test (eg. blower door test).  ASHRAE Handbook of Fundamentals also lists typical values component leakage areas for low-rise residential (e.g. Table 1 in Chapter 26 of HoF 2001).  The value should correspond to a pressure difference of 4 Pa.

#### Field: Stack Coefficient

This field is the value of the stack coefficient, ![](media/image113.png)  in the equation above.  The coefficient has units of (L/s)^2^/(cm^4^·K).  Values for the "Basic Model Stack Coefficient" listed in the ASHRAE Handbook of Fundamentals (2005 and 2001) are:

One story house
Two story house
Three story house

0.000145
0.000290
0.000435

#### Field: Wind Coefficient

This field is the value of the wind coefficient, ![](media/image114.png)  in the equation above.  The coefficient has units of (L/s)^2^/(cm^4^·(m/s)^2^).  Values for the "Basic Model Wind Coefficient" listed in the ASHRAE Handbook of Fundamentals (2005 chapter 27; 2001, Chapter 26) depend on the type of shelter and are listed in the following tables.

Shelter class
One story house
Two story house
Three story house

1
0.000319
0.000420
0.000494

2
0.000246
0.000325
0.000382

3
0.000174
0.000231
0.000271

4
0.000104
0.000137
0.000161

5
0.000032
0.000042
0.000049

Shelter class
Description

1
No obstructions or local shielding

2
Typical shelter for an isolated rural house

3
Typical shelter caused by other buildings across the street

4
Typical shelter for urban buildings on larger lots

5
Typical shelter produced by buildings that are immediately adjacent.

An example IDF object is

~~~~~~~~~~~~~~~~~~~~

    ZoneInfiltration:EffectiveLeakageArea,
        LIVING ZONE Infil 1,     !- Name
        LIVING ZONE,             !- Zone Name
        INF-SCHED,               !- Schedule Name
        500.0,                   !- Effective Air Leakage Area
        0.000145,                !- Stack Coefficient
        0.000174 ;               !- Wind Coefficient
~~~~~~~~~~~~~~~~~~~~

## ZoneInfiltration:FlowCoefficient

[ZoneInfiltration:FlowCoefficient](#zoneinfiltrationflowcoefficient) model is similar to the other infiltration objects but uses a different equation to model the unintended flow of air from the outdoor environment directly into a thermal zone.  Infiltration is generally caused by the opening and closing of exterior doors, cracks around windows, and even in very small amounts through building elements.  This reformulated model is based on the AIM-2 model by Walker and Wilson (1998) and is appropriate for smaller, residential-type buildings.  The equation used to calculate infiltration in the flow coefficient model is:

![](media/image115.png)\


Where ![](media/image116.png)  is the average difference between zone air temperature and the outdoor air temperature and the other coefficients are described below.

Note that the coefficients for the "Flow Coefficient" model are not interchangeable with the similarly named coefficients in the "Effective Leakage Area" model (see [ZoneInfiltration:EffectiveLeakageArea](#zoneinfiltrationeffectiveleakagearea) object).

One or more infiltration objects of different types can be defined for each zone, and the resulting infiltration rate for the zone will simply be the summation of the flow rates specified by the infiltration objects.

### Inputs

#### Field: Name

The name of the [ZoneInfiltration:FlowCoefficient](#zoneinfiltrationflowcoefficient) object. This needs to be unique across all different ZoneInfiltration objects.

#### Field: Zone Name

This field is the name of the zone (ref: [Zone](#zone)) and attaches a particular infiltration statement to a thermal zone in the building.

#### Field: Schedule Name

This field is the name of a schedule (ref: Schedule) that modifies the volume flow rate calculated by the model. This fraction between 0.0 and 1.0 is noted as F~schedule~ in the above equation.

#### Field: Flow Coefficient

This field is the flow coefficient in m^3^/(s·Pa^n^).  This is the value ![](media/image117.png)  in the equation above.  The flow coefficient can be determined from the effective leakage area and whole-building pressure tests (eg. blower door test).

#### Field: Stack Coefficient

This field is the value of the stack coefficient, ![](media/image118.png)  in the equation above.  The coefficient has units of (Pa/K)^n^. Values for the "Enhanced Model Stack Coefficient" listed in the ASHRAE Handbook of Fundamentals (2005 and 2001) are:

One story house
Two story house
Three story house

With Flue
0.069
0.089
0.107

No Flue
0.054
0.078
0.098

#### Field: Pressure Exponent

This field is the value of the pressure exponent, *n* in the equation above.  The pressure exponent generally lies between 0.6 and 0.7 with a typical value of *n* = 0.67 for the enhanced model.

#### Field: Wind Coefficient

This field is the value of the wind coefficient, ![](media/image119.png)  in the equation above.  The coefficient has units of (Pa·s^2^/m^2^)^n^^^.  Values for the "Enhanced Model Wind Coefficient" listed in the ASHRAE Handbook of Fundamentals (2005 and 2001) are:

One story house
Two story house
Three story house

Basement/slab;With Flue
0.142
0.156
0.167

Basement/slab; No Flue
0.156
0.170
0.170

Crawlspace; With Flue
0.128
0.142
0.154

Crawlspace;
No Flue
0.128
0.142
0.151

#### Field: Shelter Factor

This field is the value of the wind coefficient, ![](media/image120.png)  in the equation above.  The coefficient is dimensionless.  Values for the "Enhanced Model Shelter Factor" listed in the ASHRAE Handbook of Fundamentals (2005 and 2001) are:

Shelter class
No Flue
One story house with flue
Two story house with flue
Three story house with flue

1
1.00
1.10
1.07
1.06

2
0.90
1.02
0.98
0.97

3
0.70
0.86
0.81
0.79

4
0.50
0.70
0.64
0.61

5
0.30
0.54
0.47
0.43

Shelter class
Description

1
No obstructions or local shielding

2
Typical shelter for an isolated rural house

3
Typical shelter caused by other buildings across the street

4
Typical shelter for urban buildings on larger lots

5
Typical shelter produced by buildings that are immediately adjacent.

An Example IDF object is:

~~~~~~~~~~~~~~~~~~~~

    ZoneInfiltration:FlowCoefficient,
        LIVING ZONE Infil 1,     !- Name
        LIVING ZONE,             !- Zone Name
        INF-SCHED,               !- Schedule Name
        0.05,                    !- Flow Coefficient
        0.089,                   !- Stack Coefficient
         0.67  ,                 !- Pressure Exponent
        0.156 ,                  !- Wind Coefficient
        0.64;                    !- Shelter Factor
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Zone Infiltration Sensible Heat Loss Energy [J]
    HVAC,Sum,Zone Infiltration Sensible Heat Gain Energy [J]
    HVAC,Sum,Zone Infiltration Latent Heat Loss Energy [J]
    HVAC,Sum,Zone Infiltration Latent Heat Gain Energy [J]
    HVAC,Sum,Zone Infiltration Total Heat Loss Energy [J]
    HVAC,Sum,Zone Infiltration Total Heat Gain Energy [J]
    HVAC,Average,Zone Infiltration Current Density Volume Flow Rate [m3/s]
    HVAC,Average,Zone Infiltration Standard Density Volume Flow Rate [m3/s]
    HVAC,Sum,Zone Infiltration Current Density Volume [m3]
    HVAC,Sum,Zone Infiltration Standard Density Volume [m3]
    HVAC,Sum,Zone Infiltration Mass [kg]
    HVAC,Sum,Zone Infiltration Mass Flow Rate [kg/s]
    HVAC,Average,Zone Infiltration Air Change Rate [ach]
~~~~~~~~~~~~~~~~~~~~

> Note: If ZoneInfiltration:\* objects and [ZoneAirBalance:OutdoorAir](#zoneairbalanceoutdoorair) (with Air Balance Method = Quadrature) objects reference the same [Zone](#zone) Name, the above infiltration output variables will not be reported for that zone. Instead, output variables for zone outdoor air flow will be reported by the corresponding [ZoneAirBalance:OutdoorAir](#zoneairbalanceoutdoorair) object.

#### Zone Infiltration Sensible Heat Loss Energy [J]

The sensible (temperature) heat loss that occurs when the infiltration air temperature (outdoor) < zone air temperature.

#### Zone Infiltration Sensible Heat Gain Energy [J]

The sensible (temperature) heat gain that occurs when the infiltration air temperature (outdoor) >= zone air temperature.

#### Zone Infiltration Latent Heat Loss Energy [J]

The latent heat loss that occurs when the infiltration air humidity ratio (outdoor) < zone air humidity ratio.

#### Zone Infiltration Latent Heat Gain Energy [J]

The latent heat gain that occurs when the infiltration air humidity ratio (outdoor) >= zone air humidity ratio.

#### Zone Infiltration Total Heat Loss Energy [J]

The total heat loss that occurs when the sum of [Zone](#zone) Infiltration Sensible Heat Gain Energy and [Zone](#zone) Infiltration Latent Heat Gain Energy < the sum of [Zone](#zone) Infiltration Sensible Heat Loss Energy and [Zone](#zone) Infiltration Latent Heat Loss Energy.

#### Zone Infiltration Total Heat Gain Energy [J]

The total heat gain that occurs when the sum of [Zone](#zone) Infiltration Sensible Heat Gain Energy and [Zone](#zone) Infiltration Latent Heat Gain Energy >= the sum of [Zone](#zone) Infiltration Sensible Heat Loss Energy and [Zone](#zone) Infiltration Latent Heat Loss Energy.

#### Zone Infiltration Current Density Volume  [m3]

#### Zone Infiltration Current Density Volume Flow Rate [m3/s]

These outputs are the total volume and volume flow rate of infiltration air based on the current density of zone air.

#### Zone Infiltration Standard Density Volume [m3]

#### Zone Infiltration Standard Density Volume Flow Rate [m3/s]

These outputs are the total volume and volume flow rate of infiltration air based on the standard density of air.  Standard density in EnergyPlus corresponds to 20ºC drybulb, dry air, and nominally adjusted for elevation.

#### Zone Infiltration Mass [kg]

The mass flow of the Infiltration air.

#### Zone Infiltration Mass Flow Rate [kg/s]

The mass flow rate of the Infiltration air.

#### Zone Infiltration Air Change Rate [ach]

The rate of infiltration in air changes per hour.

## ZoneVentilation:DesignFlowRate

Ventilation is the purposeful flow of air from the outdoor environment directly into a thermal zone in order to provide some amount of non-mechanical cooling. Ventilation, as specified by the input syntax for the [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) object, is intended to model "simple" ventilation as opposed to the more detailed ventilation investigations that can be performed with the AirflowNetwork model or with air systems that have outdoor air mixers. [Zone](#zone) ventilation, as specified via this input object, can be controlled by a schedule and through the specification of minimum, maximum and delta temperatures as described below. The temperatures can be either single constant values for the entire simulation or schedules which can vary over time. As with infiltration, the actual flow rate of ventilation air can be modified by the temperature difference between the inside and outside environment and the wind speed. The basic equation used to calculate ventilation with this model is:

![](media/image121.png)\


Similar to infiltration, the question of typical values for these coefficients is subject to debate. Ideally, one should do a detailed analysis of the ventilation situation and then determine a custom set of coefficients using methods such as those laid out in Chapter 26 of the ASHRAE Handbook of Fundamentals. The EnergyPlus defaults are 1,0,0,0 which give a constant volume flow of ventilation under all conditions. The following discussion is duplicated from the infiltration design flow rate object. The equation must always yield a non-negative results; negative values are set to 0.0.

BLAST (one of the EnergyPlus predecessors) used the following values as defaults:  0.606, 0.03636, 0.1177, 0. These coefficients produce a value of 1.0 at 0C deltaT and 3.35 m/s (7.5 mph) windspeed, which corresponds to a typical summer condition. At a winter condition of 40C deltaT and 6 m/s (13.4 mph) windspeed, these coefficients would increase the infiltration rate by a factor of 2.75.

In DOE-2 (the other EnergyPlus predecessor), the air change method defaults are (adjusted to SI units) 0, 0, 0.224 (windspeed), 0. With these coefficients, the summer conditions above would give a factor of 0.75, and the winter conditions would give 1.34. A windspeed of 4.47 m/s (10 mph) gives a factor of 1.0.

The source of the BLAST defaults is noted in the BLAST documentation as:

"Empirical equation and the coefficient default were determined from ASHRAE journal articles and other data on the effects of outdoor weather conditions."

The source of the DOE-2 defaults is based on examining the *infiltration* relationships described in the ASHRAE Handbook of Fundamentals.

The local outdoor dry-bulb temperature used in the above basic equation (T~odb~) is typically a function of the height of the zone centroid above ground. The corresponding zone name is given in the second field. The local outdoor dry-bulb temperature calculation procedure is described in the "Local Outdoor Air Temperature Calculation" section of the Engineering Reference.

The local outdoor wind speed used in the above basic equation (WindSpeed) is also a function of the height of the zone centroid above ground. The corresponding zone name is given in the second field. The local outdoor wind speed calculation procedure is described in the "Local Wind Speed Calculation" section of the Engineering Reference.

> Note: When the value of the Wind Speed Profile Exponent field in the [Site:HeightVariation](#siteheightvariation) object is equal to 0.0, the local wind speed is always equal to the wind speed given in the weather data and will not be dependent on zone centroid height.  Similarly, if the value of the Air Temperature Gradient Coefficient is set equal to 0, the local air dry-bulb temperature is also always equal to the air dry-bulb temperature given in the weather data and will not be dependent on zone centroid height.

One or more ventilation objects (i.e., [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) and/or [ZoneVentilation:WindandStackOpenArea](#zoneventilationwindandstackopenarea)) can be defined for each zone, and the resulting ventilation rate for the zone will simply be the summation of the flow rates specified by the ventilation objects.

More advanced ventilation calculations are possible using the EnergyPlus AirflowNetwork model.

### Inputs

#### Field: Name

The name of the [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) object.

#### Field: Zone or ZoneList Name

This field is the name of the zone (ref: [Zone](#zone)) or [ZoneList](#zonelist) (ref: [ZoneList](#zonelist)) and attaches a particular ventilation statement to a thermal zone or set of thermal zones in the building. When the [ZoneList](#zonelist) option is used then this ventilation definition is applied to each of the zones in the zone list effecting a global definition for the amount of infiltration flow rate in the zone. The Zonelist option can be used effectively with the flow/area, flow/person, or airchanges/hour of the Design Flow Rate Calculation Method.

The name of the actual ventilation object becomes <[Zone](#zone) Name> <[ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) Object Name> and should be less than the standard length (100 characters) for a name field. If it is greater than this standard length, it may be difficult to specify in output reporting as it will be truncated. A warning will be shown if the generated name is greater than 100 characters. If it duplicates another such concatenated name, there will be a severe error and terminate the run.

#### Field: Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design volume flow rate (V~design~) (see Design Flow Rate Calculation Method field and related subsequent fields). This fraction between 0.0 and 1.0 is noted as F~schedule~ in the above equation.

#### Field: Design Flow Rate Calculation Method

This field is a key/choice field that tells which of the next four fields are filled and is descriptive of the method for calculating the design volume flow rate. The key/choices are:

- Flow/Zone
- With this choice, the method used will be a straight insertion of the design volume flow rate.  (The Design Flow Rate field should be filled.)
- Flow/Area
- With this choice, the method used will be a factor per floor area of the zone. (The Flow per [Zone](#zone) Floor Area field should be filled).
- Flow/Person
- With this choice, the method used will be a factor per nominal number of people in the zone. (The Flow per Person field should be filled).
- AirChanges/Hour
- With this choice, the method used will be the number of air changes per hour for the infiltration amount. This factor, along with the [Zone](#zone) Volume, will be used to determine the Design Flow Rate. (The Air Changes per Hour field should be filled).

#### Field: Design Flow Rate

This field denotes the full design volume flow rate (m^3^/s). The previous field should choose "flow/zone" as the choice. The design volume flow rate (noted as V~design~ in the above equation) is the maximum amount of ventilation expected at design conditions. The design value is modified by the schedule fraction (see Field: Schedule Name) and user specified coefficients (see four "coefficient" fields below).

#### Field: Flow Rate per Zone Floor Area

This factor (m^3^/s-m^2^) is used, along with the [Zone](#zone) Area to determine the maximum Design Volume Flow Rate as described in the Design Volume Flow Rate field. The choice from the method field should be "flow/area".

#### Field: Flow Rate per Person

This factor (m^3^/s-person) is used, along with the nominal (maximum) number of occupants (people) in the [Zone](#zone) to determine the maximum Design Volume Flow Rate as described in the Design Volume Flow Rate field. The choice from the method field should be "flow/person".

#### Field: Air Changes per Hour

- With this choice, the method used will be the number of air changes per hour for the ventilation amount. This factor, along with the [Zone](#zone) Volume, will be used to determine the Design Flow Rate. The choice from the method field should be "AirChanges/Hour".

#### Field: Ventilation Type

This alpha character string defines the type of ventilation as one of the following options: Natural, Exhaust, Intake, or Balanced. Natural ventilation is assumed to be air movement/exchange as a result of openings in the building façade and will not consume any fan energy. Values for fan pressure and efficiency for natural ventilation are ignored. For either Exhaust or Intake, values for fan pressure and efficiency define the fan electric consumption. For Natural and Exhaust ventilation, the conditions of the air entering the space are assumed to be equivalent to outside air conditions. For Intake and Balanced ventilation, an appropriate amount of fan heat is added to the entering air stream. For Balanced ventilation, both an intake fan and an exhaust fan are assumed to co-exist, both having the same flow rate and power consumption (using the entered values for fan pressure rise and fan total efficiency). Thus, the fan electric consumption for Balanced ventilation is twice that for the Exhaust or Intake ventilation types which employ only a single fan.

#### Field: Fan Pressure Rise

This is the pressure rise experienced across the fan in Pascals (N/m^2^). This is a function of the fan and plays a role in determining the amount of energy consumed by the fan.

#### Field: Fan Total Efficiency

This value is the overall efficiency of the fan, i.e., the ratio of the power delivered to the fluid to the electrical input power. It is the product of the motor efficiency and the impeller efficiency. The motor efficiency is the power delivered to the shaft divided by the electrical power input to the motor. The impeller efficiency is power delivered to the fluid (air) divided by the shaft power. The power delivered to the fluid is the mass flow rate of the air multiplied by the pressure rise divided by the air density. This input value must be between 0 and 1."

#### Field: Constant Term Coefficient

This number is the "A" parameter in the above ventilation equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter, however, is a constant under all conditions and is not modified by any environmental effect. As a result, it is dimensionless.

#### Field: Temperature Term Coefficient

This number is the "B" parameter in the above ventilation equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter is modified by the temperature difference between the outdoor and indoor air dry-bulb temperatures. The units for this parameter are inverse Celsius.

#### Field: Velocity Term Coefficient

This number is the "C" parameter in the above ventilation equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter is modified by the speed of wind being experienced outside the building. The units for this parameter are s/m.

#### Field: Velocity Squared Term Coefficient

This number is the "D" parameter in the above ventilation equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter is modified by square of the speed of wind being experienced outside the building. The units for this parameter are s^2^/m^2^.

#### Field: Minimum Indoor Temperature

This is the indoor temperature (in Celsius) below which ventilation is shutoff. The minimum value for this field is -100.0C and the maximum value is 100.0C. The default value is -100.0C if the field is left blank. This lower temperature limit is intended to avoid overcooling a space and thus result in a heating load. For example, if the user specifies a minimum temperature of 20C, ventilation is assumed to be available if the zone air temperature is above 20C. If the zone air temperature drops below 20C, then ventilation is automatically turned off.

#### Field: Minimum Indoor Temperature Schedule Name

This alpha field defines the name of a schedule (ref. Schedule objects) which contains the minimum indoor temperature (in Celsius) below which ventilation is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field and has the same functionality as the Minimum Indoor Temperature field. If the user enters a valid schedule name, the minimum temperature values specified in this schedule will override the constant value specified in the Minimum Indoor Temperature field.

#### Field: Maximum Indoor Temperature

This is the indoor temperature (in Celsius) above which ventilation is shutoff. The minimum value for this field is -100.0C and the maximum value is 100.0C. The default value is 100.0C if the field is left blank. This upper temperature limit is intended to avoid overheating a space and thus result in a cooling load. For example, if the user specifies a maximum temperature of 28C, ventilation is assumed to be available if the zone air temperature is below 28C. If the zone air temperature increases to 28C, then ventilation is automatically turned off.

#### Field: Maximum Indoor Temperature Schedule Name

This alpha field defines the name of a schedule (ref. Schedule objects) which contains the maximum indoor temperature (in Celsius) above which ventilation is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field and has the same functionality as the Maximum Indoor Temperature field. If the user enters a valid schedule name, the maximum temperature values specified in this schedule will override the constant value specified in the Maximum Indoor Temperature field.

#### Field: Delta Temperature

This is the temperature difference (in Celsius) between the indoor and outdoor air dry-bulb temperatures below which ventilation is shutoff. The minimum value for this field is -100.0C and the default value is also -100.0C if the field is left blank. This field allows ventilation to be stopped if the temperature outside is too warm and could potentially heat the space. For example, if the user specifies a delta temperature of 2C, ventilation is assumed to be available if the outside air temperature is at least 2C cooler than the zone air temperature. If the outside air dry-bulb temperature is less than 2C cooler than the indoor dry-bulb temperature, then ventilation is automatically turned off.

The values for this field can include negative numbers. This allows ventilation to occur even if the outdoor temperature is above the indoor temperature. The Delta Temperature is used in the code in the following way:

~~~~~~~~~~~~~~~~~~~~

    IF ((IndoorTemp - OutdoorTemp) < DeltaTemperature) Then ventilation is not allowed.
~~~~~~~~~~~~~~~~~~~~

Thus, if a large negative number is input for DeltaTemperature, the ventilation can be kept on even if the outdoor temperature is greater than the indoor temperature. This is useful for uncontrolled natural ventilation (open windows) or as a way to estimate the effect of required ventilation air for load calculations.

#### Field: Delta Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the temperature difference (in Celsius) between the indoor and outdoor air dry-bulb temperatures below which ventilation is shutoff as a function of time. The minimum temperature difference value in the schedule can be -100C. This field is an optional field and has the same functionality as the Delta Temperature field. If the user enters a valid schedule name, the delta temperature values specified in this schedule will override the constant value specified in the Delta Temperature field.

#### Field: Minimum Outdoor Temperature

This is the outdoor temperature (in Celsius) below which ventilation is shut off. The minimum value for this field is -100.0C and the maximum value is 100.0C. The default value is           -100.0C if the field is left blank. This lower temperature limit is intended to avoid overcooling a space, which could result in a heating load.

#### Field: Minimum Outdoor Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the minimum outdoor temperature (in Celsius) below which ventilation is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field and has the same functionality as the Minimum Outdoor Temperature field. If the user enters a valid schedule name, the temperature values in this schedule will override the constant value specified in the Minimum Outdoor Temperature field.

#### Field: Maximum Outdoor Temperature

This is the outdoor temperature (in Celsius) above which ventilation is shut off. The minimum value for this field is -100.0C and the maximum value is 100.0C. The default value is 100.0C if the field is left blank. This upper temperature limit is intended to avoid overheating a space, which could result in a cooling load.

#### Field: Maximum Outdoor Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the minimum outdoor temperature (in Celsius) above which ventilation is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field and has the same functionality as the Maximum Outdoor Temperature field. If the user enters a valid schedule name, the temperature values in this schedule will override the constant value specified in the Maximum Outdoor Temperature field.

#### Field: Maximum Wind Speed

This is the wind speed (m/s) above which ventilation is shut off. This can help simulate conditions where one would normally close windows to avoid chaos in a space (papers blowing around, etc.).

Two IDF examples are provided below:

~~~~~~~~~~~~~~~~~~~~

    ZoneVentilation:DesignFlowRate,
      Ventilation 1,  !- Name
      ZONE 2,         !- Zone Name
      Simple Vent,    !- Schedule Name
      Flow/Zone,      !- Design Volume Flow Rate calculation method
      6.131944,       !- Design Volume Flow Rate {m3/s}
      ,               !- Volume Flow Rate per area {m3/s/m2}
      ,               !- Volume Flow Rate per person {m3/s/person}
      ,               !- Air Changes Per Hour
      INTAKE,         !- Ventilation Type
      400.0,          !- Fan Pressure Rise{Pa}
      0.9,            !- Fan Total Efficiency
      0.6060000    ,  !- Constant Term Coefficient
      2.0199999E-02,  !- Temperature Term Coefficient
      5.9800001E-04,  !- Velocity Term Coefficient
      0.0000000E+00!- Velocity Squared Term Coefficient
      18.0,           !- Minimum Indoor Temperature {C}
      ,               !- Minimum Indoor Temperature Schedule Name
      ,               !- Maximum Indoor Temperature {C}
      ,               !- Maximum Indoor Temperature Schedule Name
      1.0;            !- Delta temperature {C}

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    ZoneVentilation:DesignFlowRate,
        SPACE1-1 Ventl 1,        !- Name
        SPACE1-1,                !- Zone Name
        NightVentSched,          !- SCHEDULE Name
        Flow/Zone,               !- Design Volume Flow Rate calculation method
        .05295,                  !- Design Volume Flow Rate {m3/s}
        ,                        !- Volume Flow Rate per area {m3/s/m2}
        ,                        !- Volume Flow Rate per person {m3/s/person}
        ,                        !- Air Changes Per Hour
        Intake,                  !- Ventilation Type
        67.,                     !- Fan Pressure Rise
        .7,                      !- Fan Total Efficiency
        1,                       !- Constant Term Coefficient
        0,                       !- Temperature Term Coefficient
        0,                       !- Velocity Term Coefficient
        0,                       !- Velocity Squared Term Coefficient
        ,                        !- Minimum Indoor Temperature {C}
        MinIndoorTemp,           !- Minimum Indoor Temperature Schedule Name
        ,                        !- Maximum Indoor Temperature {C}
        MaxIndoorTemp,           !- Maximum Indoor Temperature Schedule Name
        ,                        !- Delta Temperature {deltaC}
        DeltaTemp,               !- Delta Temperature Schedule Name
        ,                        !- Minimum Outdoor Temperature {C}
        MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name
        ,                        !- Maximum Outdoor Temperature {C}
        MaxOutdoorTemp,          !- Maximum Outdoor Temperature Schedule Name
        40;                      !- Maximum WindSpeed {m/s}
~~~~~~~~~~~~~~~~~~~~

Global Ventilation example:

~~~~~~~~~~~~~~~~~~~~

      Zonelist, West-East Zones, West Zone, East Zone;

      ZoneVentilation:DesignFlowRate,
        West-East Zones Ventilation,         !- Name
        West-East Zones,         !- Zone or ZoneList Name
        VentSched,               !- Schedule Name
        AirChanges/Hour,         !- Design Flow Rate Calculation Method
        ,                        !- Design Flow Rate {m3/s}
        ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}
        ,                        !- Flow Rate per Person {m3/s-person}
        1.7,                        !- Air Changes per Hour
        Intake,                  !- Ventilation Type
        67.,                     !- Fan Pressure Rise {Pa}
        0.7,                     !- Fan Total Efficiency
        1,                       !- Constant Term Coefficient
        0,                       !- Temperature Term Coefficient
        0,                       !- Velocity Term Coefficient
        0,                       !- Velocity Squared Term Coefficient
        ,                        !- Minimum Indoor Temperature {C}
        MinIndoorTemp,           !- Minimum Indoor Temperature Schedule Name
        ,                        !- Maximum Indoor Temperature {C}
        MaxIndoorTemp,           !- Maximum Indoor Temperature Schedule Name
        ,                        !- Delta Temperature {deltaC}
        DeltaTemp,               !- Delta Temperature Schedule Name
        ,                        !- Minimum Outdoor Temperature {C}
        MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name
        ,                        !- Maximum Outdoor Temperature {C}
        MaxOutdoorTemp,          !- Maximum Outdoor Temperature Schedule Name
        40;                      !- Maximum Wind Speed {m/s}
~~~~~~~~~~~~~~~~~~~~

## ZoneVentilation:WindandStackOpenArea

For this model, the ventilation air flow rate is a function of wind speed and thermal stack effect, along with the area of the opening being modeled. This object can be used alone or in combination with [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) objects. This model is intended for simplified ventilation calculations as opposed to the more detailed ventilation investigations that can be performed with the AirflowNetwork model. Using the "Wind and Stack with Open Area" model, the natural ventilation flow rate can be controlled by a multiplier fraction schedule applied to the user-defined opening area and through the specification of minimum, maximum and delta temperatures. The temperatures can be either single constant values for the entire simulation or schedules which can vary over time. The equation used to calculate the ventilation rate driven by wind is:

![](media/image122.png)\


where,

*Q~w~*= Volumetric air flow rate driven by wind [m^3^/s]

*C~w~*= Opening effectiveness [dimensionless]

*A~opening~*= Opening area [m^2^]

*F~schedule~* = Open area fraction [user-defined schedule value, dimensionless]

*V*= Local wind speed [m/s]

The equation used to calculate the ventilation rate due to stack effect is:

![](media/image123.png)\


where,

*Q~s~*= Volumetric air flow rate due to stack effect [m^3^/s]

*C~D~*= Discharge coefficient for opening [dimensionless]

*A~opening~*= Opening area [m^2^]

*F~schedule~* = Open area fraction [user-defined schedule value, dimensionless]

*ΔH~NPL~*= Height from midpoint of lower opening to the neutral pressure level [m].

Estimation of this value is difficult; refer to Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals for guidance.

*T~zone~*= [Zone](#zone) air dry-bulb temperature [K]

*T~odb~*= Local outdoor air dry-bulb temperature [K]

The total ventilation rate calculated by this model is the quadrature sum of the wind and stack air flow components:

![](media/image124.png)\


The local outdoor air dry-bulb temperature used in the stack effect equation (T~odb~) is typically a function of the height of the zone centroid above ground. The corresponding zone name is given in the second field. The local outdoor air dry-bulb temperature calculation procedure is described in the "Local Outdoor Air Temperature Calculation" section of the Engineering Reference.

The local outdoor wind speed used in the above wind-driven equation (V) is also a function of the height of the zone centroid above ground. The corresponding zone name is given in the second field. The local outdoor wind speed calculation procedure is described in the "Local Wind Speed Calculation" section of the Engineering Reference.

> Note: When the value of the Wind Speed Profile Exponent field in the [Site:HeightVariation](#siteheightvariation) object is equal to 0.0, the local wind speed is always equal to the wind speed given in the weather data and will not be dependent on zone centroid height.  Similarly, if the value of the Air Temperature Gradient Coefficient is set equal to 0, the local air dry-bulb temperature is also always equal to the air dry-bulb temperature given in the weather data and will not be dependent on zone centroid height.

One or more ventilation objects (i.e., [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) and/or [ZoneVentilation:WindandStackOpenArea](#zoneventilationwindandstackopenarea)) can be defined for each zone, and the resulting ventilation rate for the zone will simply be the summation of the flow rates specified by the ventilation objects.

More advanced ventilation calculations are possible using the EnergyPlus AirflowNetwork model.

### Inputs

#### Field: Name

The name of the [ZoneVentilation:WindandStackOpenArea](#zoneventilationwindandstackopenarea) object.

#### Field: Zone Name

This field is the name of the zone (ref: [Zone](#zone)) and attaches a particular ventilation statement to a thermal zone in the building.

#### Field: Opening Area

This is the opening area exposed to outdoors (m^2^) in a zone.

#### Field: Opening Area Fraction Schedule Name

This field is the name of the schedule (ref: Schedule) which modifies the Opening Area value (see previous field). The schedule values must be any positive number between 0 and 1 as a fraction. The actual opening area in a zone for a particular simulation time step is defined as the product of the Opening Area input field and the value specified by the schedule named in this input field.

#### Field: Opening Effectiveness

This field is the opening effectiveness (C~w~). The value must be between 0.0 and 1.0 or the value can be autocalculated. If a real value is input, that constant value will be used in the calculations. Otherwise, this field can be left blank (default = Autocalculate) or the user can input **Autocalculate.** Based on recommended values provided in Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals, C~w~ = 0.55 for perpendicular winds and C~w~ = 0.3 for diagonal winds. For Autocalculate, any angles between perpendicular and diagonal are linearly interpolated between 0.3 and 0.55 by the model.

#### Field: Effective Angle

This is the angle in degrees counting from the North clockwise to the opening outward normal. The value must be between 0 and 360, with the default being 0 if this input field is left blank. The Effective Angle is 0 if the opening outward normal faces North, 90 if faces East, 180 if faces South, and 270 if faces West. The value is fixed and independent of coordinate system defined in the [GlobalGeometryRules](#globalgeometryrules) object. This input field value is used to calculate the angle between the wind direction and the opening outward normal to determine the opening effectiveness values when the input field Opening Effectiveness = Autocalculate.

#### Field: Height Difference

This is the height difference between the midpoint of the lower opening and the neutral pressure level in meters. This value is a required user input.

> Note: Estimation of the height difference is difficult for natural ventilated buildings. Chapter 16 of the 2009 ASHRAE Handbook of Fundamentals may provide guidance for estimating the height difference.

#### Field: Discharge Coefficient for Opening

This is the discharge coefficient for the opening (C~D~). The value must be between 0.0 and 1.0, or the value can be autocalculated. If a real value is input, that constant value will be used in the calculations. Otherwise, this field can be left blank (default = Autocalculate) or the user can input **Autocalculate.** For Autocalculate, the program will determine the discharge coefficient based on the following equation:

![](media/image125.png)\


#### Field: Minimum Indoor Temperature

This is the indoor temperature (in Celsius) below which ventilation is shutoff. The minimum value for this field is -100.0C and the maximum value is 100.0C. The default value is           -100.0C if the field is left blank. This lower temperature limit is intended to avoid overcooling a space and thus result in a heating load. For example, if the user specifies a minimum temperature of 20C, ventilation is assumed to be available if the zone air temperature is above 20C. If the zone air temperature drops below 20C, then ventilation is automatically turned off.

#### Field: Minimum Indoor Temperature Schedule Name

This alpha field defines the name of a schedule (ref. Schedule objects) which contains the minimum indoor temperature (in Celsius) below which ventilation is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field and has the same functionality as the Minimum Indoor Temperature field. If the user enters a valid schedule name, the minimum temperature values specified in this schedule will override the constant value specified in the Minimum Indoor Temperature field.

#### Field: Maximum Indoor Temperature

This is the indoor temperature (in Celsius) above which ventilation is shutoff. The minimum value for this field is -100.0C and the maximum value is 100.0C. The default value is 100.0C if the field is left blank. This upper temperature limit is intended to avoid overheating a space and thus result in a cooling load. For example, if the user specifies a maximum temperature of 28C, ventilation is assumed to be available if the zone air temperature is below 28C. If the zone air temperature increases to 28C, then ventilation is automatically turned off.

#### Field: Maximum Indoor Temperature Schedule Name

This alpha field defines the name of a schedule (ref. Schedule objects) which contains the maximum indoor temperature (in Celsius) above which ventilation is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field and has the same functionality as the Maximum Indoor Temperature field. If the user enters a valid schedule name, the maximum temperature values specified in this schedule will override the constant value specified in the Maximum Indoor Temperature field.

#### Field: Delta Temperature

This is the temperature difference (in Celsius) between the indoor and outdoor air dry-bulb temperatures below which ventilation is shutoff. The minimum value for this field is -100.0C and the default value is also -100.0C if the field is left blank. This field allows ventilation to be stopped if the temperature outside is too warm and could potentially heat the space. For example, if the user specifies a delta temperature of 2C, ventilation is assumed to be available if the outside air temperature is at least 2C cooler than the zone air temperature. If the outside air dry-bulb temperature is less than 2C cooler than the indoor dry-bulb temperature, then ventilation is automatically turned off.

The values for this field can include negative numbers. This allows ventilation to occur even if the outdoor temperature is above the indoor temperature. The Delta Temperature is used in the code in the following way:

~~~~~~~~~~~~~~~~~~~~

    IF ((IndoorTemp - OutdoorTemp) < DeltaTemperature) Then ventilation is not allowed.
~~~~~~~~~~~~~~~~~~~~

Thus, if a large negative number is input for DeltaTemperature, the ventilation can be kept on even if the outdoor temperature is greater than the indoor temperature. This is useful for uncontrolled natural ventilation (open windows) or as a way to estimate the effect of required ventilation air for load calculations.

#### Field: Delta Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the temperature difference (in Celsius) between the indoor and outdoor air dry-bulb temperatures below which ventilation is shutoff as a function of time. The minimum temperature difference value in the schedule can be -100C. This field is an optional field and has the same functionality as the Delta Temperature field. If the user enters a valid schedule name, the delta temperature values specified in this schedule will override the constant value specified in the Delta Temperature field.

#### Field: Minimum Outdoor Temperature

This is the outdoor temperature (in Celsius) below which ventilation is shut off. The minimum value for this field is -100.0C and the maximum value is 100.0C. The default value is           -100.0C if the field is left blank. This lower temperature limit is intended to avoid overcooling a space, which could result in a heating load.

#### Field: Minimum Outdoor Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the minimum outdoor temperature (in Celsius) below which ventilation is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field and has the same functionality as the Minimum Outdoor Temperature field. If the user enters a valid schedule name, the temperature values in this schedule will override the constant value specified in the Minimum Outdoor Temperature field.

#### Field: Maximum Outdoor Temperature

This is the outdoor temperature (in Celsius) above which ventilation is shut off. The minimum value for this field is -100.0C and the maximum value is 100.0C. The default value is 100.0C if the field is left blank. This upper temperature limit is intended to avoid overheating a space, which could result in a cooling load.

#### Field: Maximum Outdoor Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the minimum outdoor temperature (in Celsius) above which ventilation is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field and has the same functionality as the Maximum Outdoor Temperature field. If the user enters a valid schedule name, the temperature values in this schedule will override the constant value specified in the Maximum Outdoor Temperature field.

#### Field: Maximum Wind Speed

This is the wind speed (m/s) above which ventilation is shut off. This can help simulate conditions where one would normally close windows to avoid chaos in a space (papers blowing around, etc.).

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

      ZoneVentilation:WindandStackOpenArea,
        ZONE 3 Ventl 1,          !- Name
        ZONE 3,                  !- Zone Name
        0.5,                     !- Opening Area {m2}
        Constant,                !- Opening Area Fraction Schedule Name
        AutoCalculate,           !- Opening Effectiveness
        0.0,                     !- Effective Angle {deg}
        1.0,                     !- Height Difference {m}
        AutoCalculate,           !- Discharge Coefficient for Opening
        18.0,                    !- Minimum Indoor Temperature {C}
        ,                        !- Minimum Indoor Temperature Schedule Name
        ,                        !- Maximum Indoor Temperature {C}
        ,                        !- Maximum Indoor Temperature Schedule Name
        1.0;                     !- Delta Temperature {deltaC}
~~~~~~~~~~~~~~~~~~~~

**

### Outputs

Current ventilation output variables:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Zone Ventilation Sensible Heat Loss Energy [J]
    HVAC,Sum,Zone Ventilation Sensible Heat Gain Energy [J]
    HVAC,Sum,Zone Ventilation Latent Heat Loss Energy [J]
    HVAC,Sum,Zone Ventilation Latent Heat Gain Energy [J]
    HVAC,Sum,Zone Ventilation Total Heat Loss Energy [J]
    HVAC,Sum,Zone Ventilation Total Heat Gain Energy [J]
    HVAC,Average,Zone Ventilation Current Density Volume Flow Rate [m3/s]
    HVAC,Average,Zone Ventilation Standard Density Volume Flow Rate [m3/s]
    HVAC,Sum,Zone Ventilation Current Density Volume [m3]
    HVAC,Sum,Zone Ventilation Standard Density Volume [m3]
    HVAC,Sum,Zone Ventilation Mass [kg]
    HVAC,Sum,Zone Ventilation Mass Flow Rate [kg/s]
    HVAC,Average,Zone Ventilation Air Change Rate [ach]
    HVAC,Sum,Zone Ventilation Fan Electric Energy [J]
    HVAC,Average,Zone Ventilation Air Inlet Temperature [C]
~~~~~~~~~~~~~~~~~~~~

> Note: If ZoneVentilation:\* objects and [ZoneAirBalance:OutdoorAir](#zoneairbalanceoutdoorair) (with Air Balance Method = Quadrature) objects reference the same [Zone](#zone) Name, the above ventilation output variables will not be reported for that zone. Instead, output variables for zone outdoor air flow will be reported by the corresponding [ZoneAirBalance:OutdoorAir](#zoneairbalanceoutdoorair) object.

#### Zone Ventilation Sensible Heat Loss Energy [J]

The sensible (temperature) heat loss that occurs when the ventilation inlet air temperature < zone air temperature. If multiple ventilation objects are specified for a particular zone, the ventilation inlet air temperature is from all [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) and [ZoneVentilation:WindandStackOpenArea](#zoneventilationwindandstackopenarea) objects specified for the zone.

#### Zone Ventilation Sensible Heat Gain Energy [J]

The sensible (temperature) heat gain that occurs when the ventilation inlet air temperature >= zone air temperature. If multiple ventilation objects are specified for a particular zone, the ventilation inlet air temperature is from all [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) and [ZoneVentilation:WindandStackOpenArea](#zoneventilationwindandstackopenarea) objects specified for the zone.

#### Zone Ventilation Latent Heat Loss Energy [J]

The latent heat loss that occurs when the Ventilation air humidity ratio (outdoor) < zone air humidity ratio from all [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) and [ZoneVentilation:WindandStackOpenArea](#zoneventilationwindandstackopenarea) objects specified for the zone.

#### Zone Ventilation Latent Heat Gain Energy [J]

The latent heat gain that occurs when the Ventilation air humidity ratio (outdoor) >= zone air humidity ratio from all [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) and [ZoneVentilation:WindandStackOpenArea](#zoneventilationwindandstackopenarea) objects specified for the zone.

#### Zone Ventilation Total Heat Loss Energy [J]

The total heat loss that occurs when the sum of [Zone](#zone) Ventilation Sensible Heat Gain Energy and [Zone](#zone) Ventilation Latent Heat Gain Energy < the sum of [Zone](#zone) Ventilation Sensible Heat Loss Energy and [Zone](#zone) Ventilation Latent Heat Loss Energy.

#### Zone Ventilation Total Heat Gain Energy [J]

The total heat gain that occurs when the sum of [Zone](#zone) Ventilation Sensible Heat Gain Energy and [Zone](#zone) Ventilation Latent Heat Gain Energy >= the sum of [Zone](#zone) Ventilation Sensible Heat Loss Energy and [Zone](#zone) Ventilation Latent Heat Loss Energy.

#### Zone Ventilation Current Density Volume [m3]

#### Zone Ventilation Current Density Volume Flow Rate [m3/s]

These outputs are the total volume and volume flow rate of ventilation air based on the current density of zone air.

#### Zone Ventilation Standard Density Volume [m3]

#### Zone Ventilation Standard Density Volume Flow Rate [m3/s]

These outputs are the total volume and volume flow rate of ventilation air based on the standard density of air.  Standard density in EnergyPlus corresponds to 20ºC drybulb, dry air, and nominally adjusted for elevation.

#### Zone Ventilation Mass [kg]

This output is the total mass flow into a particular zone from outdoors.

#### Zone Ventilation Mass Flow Rate [kg/s]

This output is the total mass flow rate into a particular zone from outdoors.

#### Zone Ventilation Air Change Rate [ach]

The volume flow rate of the ventilation air in air changes per hour.

#### Zone Ventilation Fan Electric Energy [J]

The fan electrical consumption for Intake or Exhaust ventilation types (for [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) objects only).

#### Zone Ventilation Air Inlet Temperature [C]

This is equal to the outdoor air temperature except when INTAKE fan power is included (for [ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) objects only). When intake fan power is used, then the additional heat due to the fan is added to the outdoor temperature and this will be reported as the inlet air temperature. If multiple ventilation objects are specified for a particular zone, the reported value is the mass flow weighted temperature for the multiple ventilation objects specified for this zone.

## ZoneAirBalance:OutdoorAir

This model calculates a combined zone outdoor airflow by including interactions between mechanical ventilation, infiltration and duct leakage. It is mainly applied to a single zone (e.g., residential) building. The model combines all outdoor airflows from ZoneInfiltration and ZoneVentilation objects in the same zone. This object also includes the induced outdoor airflows due to unbalanced duct leakage, and unbalanced outdoor airflows introduced by unbalanced airflows from [ZoneHVAC:EnergyRecoveryVentilator](#zonehvacenergyrecoveryventilator) objects when the exhaust airflow is greater than the supply outdoor airflow. This model is intended for simplified outdoor airflow calculations as opposed to the more detailed outdoor airflow investigations that can be performed with the AirflowNetwork model. The equation used to calculate the combined zone outdoor airflow is:

![](media/image126.png)\


where,

*Q*= Combined outdoor airflow with infiltration, balanced and unbalanced outdoor air flows, and unbalanced duct leakage [m^3^/s]

*Q~n~*= Natural infiltration airflow from ZoneInfiltration:\* objects [m^3^/s]

*Q~b,v~*= Balanced ventilation airflow, excluding infiltration [m^3^/s]

*Q~u,v~*= Unbalanced ventilation airflow, excluding infiltration [m^3^/s]

*Q~u,l~*= Unbalanced duct leakage: the difference between supply and return leaks [m^3^/s]

This object cannot be used simultaneously with the EnergyPlus AirflowNetwork model.  If the AirflowNetwork model is active for a simulation time step, the Air Balance Method is reset to "None" for that time step. More advanced outdoor airflow calculations are possible using the EnergyPlus AirflowNetwork model.

This object does not combine any airflows from [Fan:ZoneExhaust](#fanzoneexhaust) objects and is independent of HVAC equipment operation.

This object will not work with the [AvailabilityManager:HybridVentilation](#availabilitymanagerhybridventilation) object in the same zone, when the Simple Airflow Control Type Schedule Name is provided in the HybridVentilation object. For this case, the Air Balance Method is reset to "None".

### Inputs

#### Field: Name

The name of the ZoneOutdoorAir:Combined object.

#### Field: Zone Name

This field is the name of the zone (ref: [Zone](#zone)) and attaches a combined outdoor airflow statement to a thermal zone in the building.

#### Field: Air Balance Method

This choice field determines the air balance method. Two choices are Quadrature and None. If Quadrature, the combined zone outdoor air flow is calculated based on the above equation. If None, no combining of outdoor air will be performed (i.e., any ZoneInfiltration:\* and ZoneVentilation:\* objects specified for this zone are simulated individually).

#### Field: Induced Outdoor Air Due to Unbalanced Duct Leakage

This is the induced outdoor airflow rate, in m^3^/s, due to unbalanced duct leakage. If left blank, the default value is 0.

#### Field: Induced Outdoor Air Schedule Name

This field is the name of the schedule (ref: Schedule) which modifies the induced outdoor airflow rate value (see previous field). The schedule values must be any positive number between 0 and 1 as a fraction. The actual induced outdoor airflow rate in a zone for a particular simulation time step is defined as the product of the Induced Outdoor Air Due to Unbalanced [Duct](#duct) Leakage input field and the value specified by the schedule named in this input field.

> Note: Since this object is independent of HVAC operation, the inputs for the last two fields should be carefully selected to match HVAC system operation schedules.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    ZoneAirBalance:OutdoorAir,
        ZONE 2 Balance 1,        !- Name
        ZONE 2,                  !- Zone Name
        Quadrature,              !- Air Balance Method
        0.00,                    !- Induced Outdoor Air Due to Unbalanced Duct Leakage {m3/s}
        Constant;                !- Induced Outdoor Air Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

[ZoneAirBalance:OutdoorAir](#zoneairbalanceoutdoorair) output variables will be provided when the Air Balance Method is Quadrature. Output variables from the associated ZoneVentilation:\* and ZoneInfiltration:\* objects for the same zone will not be produced when [ZoneAirBalance:OutdoorAir](#zoneairbalanceoutdoorair) output variables are available. If the Air Balance Method = None, then no ZoneAirBalance:OutputAir outputs will be produced and the associated ZoneVentilation:\* and ZoneInfiltration:\* objects will specify their output variables for the zone.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Zone Combined Outdoor Air Sensible Heat Loss Energy [J]
    HVAC,Sum,Zone Combined Outdoor Air Sensible Heat Gain Energy [J]
    HVAC,Sum,Zone Combined Outdoor Air Latent Heat Loss Energy [J]
    HVAC,Sum,Zone Combined Outdoor Air Latent Heat Gain Energy [J]
    HVAC,Sum,Zone Combined Outdoor Air Total Heat Loss Energy [J]
    HVAC,Sum,Zone Combined Outdoor Air Total Heat Gain Energy [J]
    HVAC,Average,Zone Combined Outdoor Air Current Density Volume Flow Rate [m3/s]
    HVAC,Average,Zone Combined Outdoor Air Standard Density Volume Flow Rate [m3/s]
    HVAC,Sum,Zone Combined Outdoor Air Current Density Volume [m3]
    HVAC,Sum,Zone Combined Outdoor Air Standard Density Volume [m3]
    HVAC,Sum,Zone Combined Outdoor Air Mass [kg]
    HVAC,Sum,Zone Combined Outdoor Air Mass Flow Rate [kg/s]
    HVAC,Average, Zone Combined Outdoor Air Changes per Hour [ach]
    HVAC,Sum, Zone Combined Outdoor Air Fan Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Zone Combined Outdoor Air Sensible Heat Loss Energy [J]

The sensible (temperature) heat loss that occurs when the outdoor air temperature < zone air temperature.

#### Zone Combined Outdoor Air Sensible Heat Gain Energy [J]

The sensible (temperature) heat gain that occurs when the outdoor air temperature >= zone air temperature.

#### Zone Combined Outdoor Air Latent Heat Loss Energy [J]

The latent heat loss that occurs when the outdoor air humidity ratio < zone air humidity ratio.

#### Zone Combined Outdoor Air Latent Heat Gain Energy [J]

The latent heat gain that occurs when the outdoor air humidity ratio >= zone air humidity ratio.

#### Zone Combined Outdoor Air Total Heat Loss Energy [J]

The total heat loss that occurs when the sum of [Zone](#zone) Combined Outdoor Air Sensible Heat Gain Energy and [Zone](#zone) Combined Outdoor Air Latent Heat Gain Energy < the sum of [Zone](#zone) Combined Outdoor Air Sensible Heat Loss Energy and [Zone](#zone) Combined Outdoor Air Latent Heat Loss Energy.

#### Zone Combined Outdoor Air Total Heat Gain Energy [J]

The total heat gain that occurs when the sum of [Zone](#zone) Combined Outdoor Air Sensible Heat Gain Energy and [Zone](#zone) Combined Outdoor Air Latent Heat Gain Energy >= the sum of [Zone](#zone) Combined Outdoor Air Sensible Heat Loss Energy and [Zone](#zone) Combined Outdoor Air Latent Heat Loss Energy.

#### Zone Combined Outdoor Air Current Density Volume [m3]

#### Zone Combined Outdoor Air Current Density Volume Flow Rate [m3/s]

These outputs are the total volume and volume flow rate of outdoor air based on the current density of zone air.

#### Zone Combined Outdoor Air Standard Density Volume [m3]

#### Zone Combined Outdoor Air Standard Density Volume Flow Rate [m3/s]

These outputs are the total volume and volume flow rate of outdoor air based on the standard density of air.  Standard density in EnergyPlus corresponds to 20ºC drybulb, dry air, and nominally adjusted for elevation.

#### Zone Combined Outdoor Air Mass [kg]

This output is the total mass flow into a particular zone from outdoors.

#### Zone Combined Outdoor Air Mass Flow Rate [kg/s]

This output is the total mass flow rate into a particular zone from outdoors.

#### Zone Combined Outdoor Air Changes per Hour [ach]

The volume flow rate of the ventilation air in air changes per hour.

#### Zone Combined Outdoor Air Fan Electric Energy [J]

The fan electrical consumption for Intake, Exhaust, or Balanced ventilation types (for

[ZoneVentilation:DesignFlowRate](#zoneventilationdesignflowrate) objects used to calculate a combined zone outdoor airflow).

## ZoneMixing

In EnergyPlus, the [ZoneMixing](#zonemixing) syntax is intended to allow simplified treatment of air exchange between zones. Note that this statement only affects the energy balance of the "receiving" zone and that this statement will not produce any effect on the "source" zone. Mixing statements can be complementary and include multiple zones, but the balancing of flows between zones is left to the user's discretion. The use of the Cross Mixing syntax (ref: Cross Mixing) can allow for automatic balancing of flows and energy flow rates between zones but is hindered by the limitation of only a single cross mixing statement per zone. The use of Refrigeration [Door](#door) Mixing syntax (ref: Refrigeration [Door](#door) Mixing) automatically balances the flow and energy between two zones and allows multiple mixing statements per zone. More advanced mixing calculations are possible using the EnergyPlus AirflowNetwork model for multi-zone airflow with or without HVAC system operation. Mixing is entered using the following syntax.

### Inputs

#### Field: Name

The name of the [ZoneMixing](#zonemixing) object.

#### Field: Zone Name

This field is the name of the zone (ref: [Zone](#zone)) receiving the amount of air being exchanged and attaches a particular mixing statement to a thermal zone in the building.

#### Field: Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design volume flow rate parameter (see next field). This fraction between 0.0 and 1.0 modifies the design level parameter.

#### Field: Design Flow Rate Calculation Method

This field is a key/choice field that tells which of the next four fields are filled and is descriptive of the method for calculating the design volume flow rate. The key/choices are:

- Flow/Zone
- With this choice, the method used will be a straight insertion of the design volume flow rate.  (The Design Flow Rate field should be filled.)
- Flow/Area
- With this choice, the method used will be a factor per floor area of the zone. (The Flow per [Zone](#zone) Floor Area field should be filled).
- Flow/Person
- With this choice, the method used will be a factor per nominal number of people in the zone. (The Flow per Person field should be filled).
- AirChanges/Hour
- With this choice, the method used will be the number of air changes per hour for the infiltration amount. This factor, along with the [Zone](#zone) Volume, will be used to determine the Design Flow Rate. (The Air Changes per Hour field should be filled).

#### Field: Design Flow Rate

This field denotes the full design volume flow rate (m^3^/s). The previous field should choose "flow/zone" as the choice. The design volume flow rate is the maximum amount of mixing air expected. The design value is modified by the schedule fraction (see Field: Schedule Name) and user specified coefficients (see four "coefficient" fields below).

#### Field: Flow Rate per Zone Floor Area

This factor (m^3^/s-m^2^) is used, along with the [Zone](#zone) Area to determine the maximum Design Volume Flow Rate as described in the Design Volume Flow Rate field. The choice from the method field should be "Flow/Area".

#### Field: Flow Rate per Person

This factor (m^3^/s-person) is used, along with the nominal (maximum) number of occupants (people) in the [Zone](#zone) to determine the maximum Design Volume Flow Rate as described in the Design Volume Flow Rate field. The choice from the method field should be "Flow/Person".

#### Field: Air Changes per Hour

- With this choice, the method used will be the number of air changes per hour for the mixing amount. This factor, along with the [Zone](#zone) Volume, will be used to determine the Design Flow Rate. The choice from the method field should be "AirChanges/Hour".

#### Field: Source Zone Name

This field is the name of the "source" zone (ref: [Zone](#zone)) that exhausts the amount of air specified by the design level and schedule fields to the zone named in the zone name field.

#### Field: Delta Temperature

This number controls when mixing air from the source zone is sent to the receiving zone. This parameter is a temperature and is expressed in units of Celsius. If this field is positive, the temperature of the zone from which the air is being drawn (source zone) must be "Delta Temperature" warmer than the receiving zone air or else no mixing occurs. If this field is negative, the temperature of the source zone must be "Delta Temperature" cooler than the receiving zone air or else no mixing occurs. If this parameter is zero, mixing occurs regardless of the relative zone temperatures.

#### Field: Delta Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the temperature difference (in Celsius) between the source zone and receiving zone air dry-bulb temperatures as a function of time. This field is an optional field and has the same functionality as the Delta Temperature field. If the user enters a valid schedule name, the delta temperature values specified in this schedule will supersede the constant value specified in the Delta Temperature field.

#### Field: Minimum Zone Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the minimum receiving zone temperature (in Celsius) below which mixing is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the minimum zone temperature control is not applied.

#### Field: Maximum Zone Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the maximum receiving zone temperature (in Celsius) above which mixing is shutoff as a function of time. The maximum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the maximum zone temperature control is not applied.

> **Note:** The maximum zone temperature when mixing is shutoff must be greater than or equal to the minimum zone temperature when mixing is shutoff at any given time. Otherwise, warnings will be issued and the maximum zone shutoff temperature will be set to the minimum zone shutoff temperature.

#### Field: Minimum Source Zone Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the minimum source zone temperature (in Celsius) below which mixing is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the minimum source zone temperature control is not applied.

#### Field: Maximum Source Zone Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the maximum source zone temperature (in Celsius) above which mixing is shutoff as a function of time. The maximum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the maximum source zone temperature control is not applied.

> **Note:** The maximum source zone temperature when mixing is shutoff must be greater than or equal to the minimum source zone temperature when mixing is shutoff at any given time. Otherwise, warnings will be issued and the maximum source zone shutoff temperature will be set to the minimum source zone shutoff temperature.

#### Field: Minimum Outdoor Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the minimum outdoor temperature (in Celsius) below which mixing is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the minimum outdoor temperature control is not applied.

#### Field: Maximum Outdoor Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the maximum outdoor temperature (in Celsius) above which mixing is shutoff as a function of time. The maximum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the maximum outdoor temperature control is not applied.

> **Note:** The maximum outdoor temperature when mixing is shutoff must be greater than or equal to the minimum outdoor temperature which mixing is shutoff at any given time. Otherwise, a warning will be issued and the maximum outdoor shutoff temperature will be set to the minimum outdoor shutoff temperature.

An IDF Example:

~~~~~~~~~~~~~~~~~~~~

    ZoneMixing,
      Kitchen_ZN_1_FLR_1 Exhaust Fanmixing_0,  !- Name
      Kitchen_ZN_1_FLR_1,  !- Zone Name
      Hours_of_operation,  !- Schedule Name
      Flow/Zone,  !- Design Flow Rate Calculation Method
      1.4540,  !- Design Level
       ,  !- Volume Flow Rate per Area {m3/s/m2}
       ,  !- Volume Flow Rate Per Person {m3/s/person}
       ,  !- Air Changes per Hour {ACH}
      CAFETERIA_ZN_1_FLR_1,  !- Source Zone Name
      0.0;  !- Delta Temperature
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Zone Mixing Volume [m3]
    HVAC,Sum,Zone Mixing Current Density Volumetric Flow Rate [m3/s]
    HVAC,Sum,Zone Mixing Standard Density Volumetric Flow Rate [m3/s]
    HVAC,Sum,Zone Mixing Mass [kg]
    HVAC,Sum,Zone Mixing Mass Flow Rate [kg/s]
    HVAC,Sum,Zone Mixing Sensible Heat Loss Energy [J]
    HVAC,Sum,Zone Mixing Sensible Heat Gain Energy [J]
    HVAC,Sum,Zone Mixing Latent Heat Loss Energy [J]
    HVAC,Sum,Zone Mixing Latent Heat Gain Energy [J]
    HVAC,Sum,Zone Mixing Total Heat Loss [J]
    HVAC,Sum,Zone Mixing Total Heat Gain Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Zone Mixing Volume [m3]

The air volume in m^3^ entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep.

#### Zone Mixing Current Density Volumetric Flow Rate [m3/s]

The air volumetric flow rate in m^3^/s entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep using the density of air evaluated at current zone air conditions.

#### Zone Mixing Standard Density Volumetric Flow Rate [m3/s]

The air volumetric flow rate in m^3^/s entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep using the density of air evaluated at standard conditions.

#### Zone Mixing Mass [kg]

The air mass in kg entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep. The air mass is calculated using the air volume flow from each "source" zone and the density of air calculated for the average conditions (temperature and humidity) between the "source" and "receiving" zones.

#### Zone Mixing Mass Flow Rate [kg/s]

The air mass flow rate in kg/s entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep.

#### Zone Mixing Sensible Heat Loss Energy [J]

#### Zone Mixing Sensible Heat Gain Energy [J]

The sensible (temperature) heat transfer due to the sum of mixing, cross-mixing, and refrigeration-door mixing in the host (receiving) zone is the sum of all the incoming air mass flow rates multiplied by the elapsed time, the specific heat (calculated for the average conditions (temperature and humidity) between the "source" and "receiving" zones) and the temperature differences between the host zone and corresponding source zones. If the heat transfer is negative, the heat transfer is considered to be a zone mixing sensible heat loss. If the heat transfer is positive, the heat transfer is considered to be a zone mixing sensible heat gain.

#### Zone Mixing Latent Heat Loss Energy [J]

#### Zone Mixing Latent Heat Gain Energy [J]

The latent heat transfer due to the sum of mixing, cross-mixing, and refrigeration-door mixing in the host (receiving) zone is the sum of all the incoming air mass flow rates multiplied by the elapsed time, the heat of vaporization (calculated for the average conditions (temperature and humidity) between the "source" and "receiving" zones) and the humidity ratio differences between the host zone and corresponding source zones. If the heat transfer is negative, the heat transfer is considered to be a zone mixing latent heat loss. If the heat transfer is positive, the heat transfer is considered to be a zone mixing latent heat gain.

#### Zone Mixing Total Heat Loss Energy [J]

The total heat loss due to the sum of mixing, cross-mixing, and refrigeration-door mixing that occurs when the sum of [Zone](#zone) Mixing Sensible Heat Gain Energy and [Zone](#zone) Mixing Latent Heat Gain Energy < the sum of [Zone](#zone) Mixing Sensible Heat Loss Energy and [Zone](#zone) Mixing Latent Heat Loss Energy.

#### Zone Mixing Total Heat Gain Energy [J]

The total heat gain due to the sum of mixing, cross-mixing, and refrigeration-door mixing that occurs when the sum of [Zone](#zone) Mixing Sensible Heat Gain Energy and [Zone](#zone) Mixing Latent Heat Gain Energy >= the sum of [Zone](#zone) Mixing Sensible Heat Loss Energy and [Zone](#zone) Mixing Latent Heat Loss Energy.

## ZoneCrossMixing

The [ZoneCrossMixing](#zonecrossmixing) syntax is ideally suited for two zones that exchange an equal amount of air between each other and do not have any air exchange with other zones. As with Mixing (ref: Mixing), this is a simplified view of interzone airflow in EnergyPlus. The main difference between Mixing and Cross Mixing is that Cross Mixing has an energy effect on both the source and the receiving zone, thus maintaining both the air mass and energy balances in the two zones.

Cross Mixing can be entered once, in one of the mixing zones; or twice, once for each zone. The object should be entered once if Delta Temperature > 0.0 and it is desirable to have mixing only when the source zone  is warmer than the receiving zone. This might be the case when the warmer zone is below the colder zone and the mixing is buoyancy driven. If the zones are next to each other, separated by an open doorway, it would be more suitable to input a cross mixing object for each zone. Then mixing would occur if the zone temperatures differed by Delta Temperature or greater regardless of which is the warmer zone.

If Delta Temperature = 0.0, Cross Mixing can be entered either once or twice: the effect is exactly the same.

Cross Mixing is entered using the following syntax.

### Inputs

#### Field: Name

The name of the [ZoneCrossMixing](#zonecrossmixing) object.

#### Field: Zone Name

This field is the name of the zone (ref: [Zone](#zone)) receiving the amount of air being exchanged and attaches a particular cross mixing statement to a thermal zone in the building.

#### Field: Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design volume flow rate parameter (see next field). This fraction between 0.0 and 1.0 modifies the design level parameter.

#### Field: Design Flow Rate Calculation Method

This field is a key/choice field that tells which of the next four fields are filled and is descriptive of the method for calculating the design volume flow rate. The key/choices are:

- Flow/Zone
- With this choice, the method used will be a straight insertion of the design volume flow rate.  (The Design Flow Rate field should be filled.)
- Flow/Area
- With this choice, the method used will be a factor per floor area of the zone. (The Flow per [Zone](#zone) Floor Area field should be filled).
- Flow/Person
- With this choice, the method used will be a factor per nominal number of people in the zone. (The flow per person field should be filled).
- AirChanges/Hour
- With this choice, the method used will be the number of air changes per hour for the infiltration amount. This factor, along with the [Zone](#zone) Volume, will be used to determine the Design Flow Rate. (The Air Changes per Hour field should be filled).

#### Field: Design Flow Rate

This field denotes the full design volume flow rate (m^3^/s). The previous field should choose "flow/zone" as the choice. The design volume flow rate is the maximum amount of mixing air expected. The design value is modified by the schedule fraction (see Field: Schedule Name) and user specified coefficients (see four "coefficient" fields below).

#### Field: Flow Rate per Zone Floor Area

This factor (m^3^/s-m^2^) is used, along with the [Zone](#zone) Area to determine the maximum Design Volume Flow Rate as described in the Design Volume Flow Rate field. The choice from the method field should be "Flow/Area".

#### Field: Flow Rate per Person

This factor (m^3^/s-person) is used, along with the nominal (maximum) number of occupants (people) in the [Zone](#zone) to determine the maximum Design Volume Flow Rate as described in the Design Volume Flow Rate field. The choice from the method field should be "Flow/Person".

#### Field: Air Changes per Hour

- With this choice, the method used will be the number of air changes per hour for the mixing amount. This factor, along with the [Zone](#zone) Volume, will be used to determine the Design Flow Rate. The choice from the method field should be "AirChanges/Hour".

#### Field: Source Zone Name

This field is the name of the "source" zone (ref: [Zone](#zone)) that exhausts the amount of air specified by the design level and schedule fields to the zone named in the zone name field. In reality, the "source" and "receiving" zones are interchangeable since the cross-mixed air affects both zones.

#### Field: Delta Temperature

This number controls when mixing air from the source zone is sent to the receiving zone. This parameter is a temperature and is expressed in units of Celsius. If this field is positive, the temperature of the zone from which air is being drawn ("source zone") must be "Delta Temperature" warmer than the zone air or no mixing occurs. If this field is zero, mixing occurs regardless of the relative air temperatures. Negative values for "Delta Temperature" are not permitted.

#### Field: Delta Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the temperature difference (in Celsius) between the source zone and receiving zone air dry-bulb temperatures as a function of time. This field is an optional field and has the same functionality as the Delta Temperature field. If the user enters a valid schedule name, the delta temperature values specified in this schedule will supersede the constant value specified in the Delta Temperature field.

#### Field: Minimum Zone Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the minimum receiving zone temperature (in Celsius) below which cross mixing is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the minimum zone temperature control is not applied.

#### Field: Maximum Zone Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the maximum receiving zone temperature (in Celsius) above which cross mixing is shutoff as a function of time. The maximum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the maximum zone temperature control is not applied.

> **Note:** The maximum zone temperature when cross mixing is shutoff must be greater than or equal to the minimum zone temperature when cross mixing is shutoff at any given time. Otherwise, warnings will be issued and the maximum zone shutoff temperature will be set to the minimum zone shutoff temperature.

#### Field: Minimum Source Zone Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the minimum source zone temperature (in Celsius) below which cross mixing is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the minimum source zone temperature control is not applied.

#### Field: Maximum Source Zone Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the maximum source zone temperature (in Celsius) above which cross mixing is shutoff as a function of time. The maximum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the maximum source zone temperature control is not applied.

> **Note:** The maximum source zone temperature when cross mixing is shutoff must be greater than or equal to the minimum source zone temperature when cross mixing is shutoff at any given time. Otherwise, warnings will be issued and the maximum source zone shutoff temperature will be set to the minimum source zone shutoff temperature.

#### Field: Minimum Outdoor Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the minimum outdoor temperature (in Celsius) below which cross mixing is shutoff as a function of time. The minimum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the minimum outdoor temperature control is not applied.

#### Field: Maximum Outdoor Temperature Schedule Name

This alpha field contains the name of a schedule (ref. Schedule objects) which contains the maximum outdoor temperature (in Celsius) above which cross mixing is shutoff as a function of time. The maximum temperature value in the schedule can be -100C and the maximum value can be 100C. This field is an optional field. If this field is not entered, the maximum outdoor temperature control is not applied.

> **Note:** The maximum outdoor temperature when cross mixing is shutoff must be greater than or equal to the minimum outdoor temperature which cross mixing is shutoff at any given time. Otherwise, a warning will be issued and the maximum outdoor shutoff temperature will be set to the minimum outdoor shutoff temperature.

An IDF Example:

~~~~~~~~~~~~~~~~~~~~

    ZoneCrossMixing,
        1stFloor-Garage,  !- Name
        GARAGE,      !- Zone Name
        Always On,   !- SCHEDULE Name
        Flow/Zone,               !- Design Flow Rate calculation method
        0.1,                     !- Design Flow Rate {m3/s}
        ,                        !- Flow Rate per Area {m3/s/m2}
        ,                        !- Flow Rate per Person {m3/s/person}
        ,                        !- Air Changes Per Hour
        1ST-FLOOR,   !- Source Zone Name
        1.0;         !- Delta temp
    ZoneCrossMixing,
        Garage-1stFloor,  !- Name
        1ST-FLOOR,   ! Zone Name
        Always On,   !- SCHEDULE Name
        flow/zone,               !- Design Flow Rate calculation method
        0.1,                     !- Design Flow Rate {m3/s}
        ,                        !- Flow Rate per area {m3/s/m2}
        ,                        !- Flow Rate per person {m3/s/person}
        ,                        !- Air Changes Per Hour
        GARAGE,      ! Source Zone Name
        1.0;         ! Delta temp
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Zone Mixing Volume [m3]
    HVAC,Sum,Zone Mixing Current Density Volumetric Flow Rate [m3/s]
    HVAC,Sum,Zone Mixing Standard Density Volumetric Flow Rate [m3/s]
    HVAC,Sum,Zone Mixing Mass [kg]
    HVAC,Sum,Zone Mixing Mass Flow Rate [kg/s]
    HVAC,Sum,Zone Mixing Sensible Heat Loss Energy [J]
    HVAC,Sum,Zone Mixing Sensible Heat Gain Energy [J]
    HVAC,Sum,Zone Mixing Latent Heat Loss Energy [J]
    HVAC,Sum,Zone Mixing Latent Heat Gain Energy [J]
    HVAC,Sum,Zone Mixing Total Heat Loss Energy [J]
    HVAC,Sum,Zone Mixing Total Heat Gain Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Zone Mixing Volume [m3]

The air volume in m^3^ entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep.

#### Zone Mixing Current Density Volumetric Flow Rate [m3/s]

The air volumetric flow rate in m^3^/s entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep using the density of air evaluated at current zone air conditions.

#### Zone Mixing Standard Density Volumetric Flow Rate [m3/s]

The air volumetric flow rate in m^3^/s entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep using the density of air evaluated at standard conditions.

#### Zone Mixing Mass [kg]

The air mass in kg entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep. The air mass is calculated using the air volume flow from each "source" zone and the density of air calculated for the average conditions (temperature and humidity) between the "source" and "receiving" zones.

#### Zone Mixing Mass Flow Rate [kg/s]

The air mass flow rate in kg/s entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep.

#### Zone Mixing Sensible Heat Loss Energy [J]

#### Zone Mixing Sensible Heat Gain Energy [J]

The sensible (temperature) heat transfer due to the sum of mixing, cross-mixing, and refrigeration-door mixing in the host (receiving) zone is the sum of all the incoming air mass flow rates multiplied by the elapsed time, the specific heat (calculated for the average conditions (temperature and humidity) between the "source" and "receiving" zones) and the temperature differences between the host zone and corresponding source zones. If the heat transfer is negative, the heat transfer is considered to be a zone mixing sensible heat loss. If the heat transfer is positive, the heat transfer is considered to be a zone mixing sensible heat gain.

#### Zone Mixing Latent Heat Loss Energy [J]

#### Zone Mixing Latent Heat Gain Energy [J]

#### **The latent heat transfer due to the sum of mixing, cross-mixing, and refrigeration-door mixing in the host (receiving) zone is the sum of all the incoming air mass flow rates multiplied by the elapsed time, the heat of vaporization** **(calculated for the average conditions (temperature and humidity) between the "source" and "receiving" zones) and the humidity ratio differences between the host zone and corresponding source zones. If the heat transfer is negative, the heat transfer is considered to be a zone mixing latent heat loss. If the heat transfer is positive, the heat transfer is considered to be a zone mixing latent heat gain.** 

#### Zone Mixing Total Heat Loss Energy [J]

The total heat loss due to the sum of mixing, cross-mixing, and refrigeration-door mixing that occurs when the sum of [Zone](#zone) Mixing Sensible Heat Gain Energy and [Zone](#zone) Mixing Latent Heat Gain Energy < the sum of [Zone](#zone) Mixing Sensible Heat Loss Energy and [Zone](#zone) Mixing Latent Heat Loss Energy.

#### Zone Mixing Total Heat Gain Energy [J]

The total heat gain due to the sum of mixing, cross-mixing, and refrigeration-door mixing that occurs when the sum of [Zone](#zone) Mixing Sensible Heat Gain Energy and [Zone](#zone) Mixing Latent Heat Gain Energy >= the sum of [Zone](#zone) Mixing Sensible Heat Loss Energy and [Zone](#zone) Mixing Latent Heat Loss Energy.

## ZoneRefrigerationDoorMixing

The [ZoneRefrigerationDoorMixing](#zonerefrigerationdoormixing) syntax is ideally suited for two zones, at least one of which is refrigerated, that exchange an equal amount of dry air. They may also have mixed air from other zones, but only one object should be entered for any one pair of zones. As with Mixing (ref: Mixing), this is a simplified view of interzone airflow in EnergyPlus. The [ZoneRefrigerationDoorMixing](#zonerefrigerationdoormixing) approach shares some features of both Mixing and Cross Mixing. Like Cross Mixing, RefrigerationDoorMixing has an energy effect on both the source and the receiving zone, thus maintaining both the air mass and energy balances in the two zones. Like Mixing, the refrigerated zone can exchange air with multiple zones.  Unlike either of the other two mixing objects, the RefrigeratedDoorMixing always calculates the air exchange based on the zone temperature and relative humidity. That is, the user does not specify the air flow rate. The user can moderate the flow through a door-opening schedule.

Unlike, Cross Mixing, Refrigeration [Door](#door) Mixing can only be entered once for any unique pair of zones. It doesn't matter which zone is listed first and the zones will automatically switch back and forth between source and receiving zones depending upon which zone is colder.

Refrigeration [Door](#door) Mixing is entered using the following syntax.

### Inputs

#### Field: Name

The name of the [ZoneRefrigerationDoorMixing](#zonerefrigerationdoormixing) object.

#### Field: Zone 1  Name

This field is the name of one of the two zones (ref: [Zone](#zone)) exchanging air and attaches a particular refrigeration door  mixing statement to both thermal zones in the building.

#### Field: Zone 2  Name

This field is the name of the other zone (ref: [Zone](#zone)) exchanging air and attaches a particular refrigeration door  mixing statement to both thermal zones in the building.

#### Field: Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the door opening between the two zones and should contain values between  0.0 and 1.0.

#### Field: Door Height

This field denotes the door opening height (m). The default value is 3 m.

#### Field: Door  Area

This field denotes the door opening area (m^2^). The default value is 9 m^2^.

#### Field: Door Protection Type

This field is a key/choice field that tells how the door is protected. The impact of this choice is decribed in the Engineering Reference. The key/choices are:

- None
- None, or no door protection is the default choice
- AirCurtain
- StripCurtain

An IDF Example:

~~~~~~~~~~~~~~~~~~~~

    ZoneRefrigerationDoorMixing,
        Freezer1_Cooler1,  !- Name
        Freezer1,          !- Zone 1 Name
        Cooler1,           !- Zone 2 Name
        Freezer1DoorSched, !- Schedule Name
        1.8,               !- Door height {m}
        2.3,               !- Door area {m2}
        StripCurtain;      !- Door protection type
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Zone Mixing Volume [m3]
    HVAC,Sum,Zone Mixing Current Density Volumetric Flow Rate [m3/s]
    HVAC,Sum,Zone Mixing Standard Density Volumetric Flow Rate [m3/s]
    HVAC,Sum,Zone Mixing Mass [kg]
    HVAC,Sum,Zone Mixing Mass Flow Rate [kg/s]
    HVAC,Sum,Zone Mixing Sensible Heat Loss Energy [J]
    HVAC,Sum,Zone Mixing Sensible Heat Gain Energy [J]
    HVAC,Sum,Zone Mixing Latent Heat Loss Energy [J]
    HVAC,Sum,Zone Mixing Latent Heat Gain Energy [J]
    HVAC,Sum,Zone Mixing Total Heat Loss Energy [J]
    HVAC,Sum,Zone Mixing Total Heat Gain Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Zone Mixing Volume [m3]

The air volume in m^3^ entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep.

#### Zone Mixing Current Density Volumetric Flow Rate [m3/s]

The air volumetric flow rate in m^3^/s entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep using the density of air evaluated at current zone air conditions.

#### Zone Mixing Standard Density Volumetric Flow Rate [m3/s]

The air volumetric flow rate in m^3^/s entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep using the density of air evaluated at standard conditions.

#### Zone Mixing Mass [kg]

The air mass in kg entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep. The air mass is calculated using the air volume flow from each "source" zone and the density of air calculated for the average conditions (temperature and humidity) between the "source" and "receiving" zones.

#### Zone Mixing Mass Flow Rate [kg/s]

The air mass flow rate in kg/s entering the zone due to the sum of mixing, cross-mixing, and refrigeration-door mixing during the hour or timestep.

#### Zone Mixing Sensible Heat Loss Energy [J]

#### Zone Mixing Sensible Heat Gain Energy [J]

The sensible (temperature) heat transfer due to the sum of mixing, cross-mixing, and refrigeration-door mixing in the host (receiving) zone is the sum of all the incoming air mass flow rates multiplied by the elapsed time, the specific heat (calculated for the average conditions (temperature and humidity) between the "source" and "receiving" zones) and the temperature differences between the host zone and corresponding source zones. If the heat transfer is negative, the heat transfer is considered to be a zone mixing sensible heat loss. If the heat transfer is positive, the heat transfer is considered to be a zone mixing sensible heat gain.

#### Zone Mixing Latent Heat Loss Energy [J]

#### Zone Mixing Latent Heat Gain Energy [J]

#### **The latent heat transfer due to the sum of mixing, cross-mixing, and refrigeration-door mixing in the host (receiving) zone is the sum of all the incoming air mass flow rates multiplied by the elapsed time, the heat of vaporization** **(calculated for the average conditions (temperature and humidity) between the "source" and "receiving" zones) and the humidity ratio differences between the host zone and corresponding source zones. If the heat transfer is negative, the heat transfer is considered to be a zone mixing latent heat loss. If the heat transfer is positive, the heat transfer is considered to be a zone mixing latent heat gain.** 

#### Zone Mixing Total Heat Loss Energy [J]

The total heat loss due to the sum of mixing, cross-mixing, and refrigeration-door mixing that occurs when the sum of [Zone](#zone) Mixing Sensible Heat Gain Energy and [Zone](#zone) Mixing Latent Heat Gain Energy < the sum of [Zone](#zone) Mixing Sensible Heat Loss Energy and [Zone](#zone) Mixing Latent Heat Loss Energy.

#### Zone Mixing Total Heat Gain Energy [J]

- The total heat gain due to the sum of mixing, cross-mixing, and refrigeration-door mixing that occurs when the sum of [Zone](#zone) Mixing Sensible Heat Gain Energy and [Zone](#zone) Mixing Latent Heat Gain Energy >= the sum of [Zone](#zone) Mixing Sensible Heat Loss Energy and [Zone](#zone) Mixing Latent Heat Loss Energy.

## ZoneEarthtube (Earth Tube)

An earth tube is a long, underground metal or plastic pipe through which air is drawn. During cooling season, as air travels through the pipe, it gives up some of its heat to the surrounding soil and enters the room as cooler air. Similarly, during heating season, as air travels through the pipe, it receives some of its heat from the soil and enters the room as warmer air. Simple earth tubes in EnergyPlus can be controlled by a schedule and through the specification of minimum, maximum, and delta temperatures as described below. As with infiltration and ventilation, the actual flow rate of air through the earth tube can be modified by the temperature difference between the inside and outside environment and the wind speed. The basic equation used to calculate air flow rate of earth tube in EnergyPlus is:

![](media/image127.png)\


For the simulation of the earth tube, a weather data file is required and, therefore, the earth tube cannot run without weather data file. The required input fields to simulate the earth tube include the average soil surface temperature, the amplitude of soil surface temperature, and the phase constant of soil surface temperature. These fields should be calculated in advance by using a separate stand-alone program (CalcSoilSurfTemp) and should be input into earth tube.

### CalcSoilSurfTemp – Auxiliary Programs Document

The CalcSoilSurfTemp program is simple and requires only two input fields : soil condition and soil surface condition in addition to a valid weather file. For soil condition, the user should select the number corresponding to the actual condition of the soil surrounding the earth tube from the four following options: 1. HEAVY AND SATURATED, 2. HEAVY AND DAMP, 3. HEAVY AND DRY and 4. LIGHT AND DRY. This determines the thermal diffusivity and thermal conductivity of the surrounding soil. For soil surface conditions, the user should select the number corresponding to the actual condition of the ground surface above the earth tube from the eight following options: 1. BARE AND WET, 2. BARE AND MOIST, 3. BARE AND ARID, 4. BARE AND DRY, 5. COVERED AND WET, 6. COVERED AND MOIST, 7. COVERED AND ARID and 8. COVERED AND DRY. This determines the absorption coefficient and the fraction of evaporation rate of the ground surface.

From this information and an analysis of the weather for the location selected, the CalcSoilSurfTemp program (ref. Auxiliary Programs document) calculates the three parameters listed above. The user must then add these parameters as input into EnergyPlus. The full input description of an earth tube in EnergyPlus is given below.

### Field: Zone Name

This field is the name of the zone (ref: [Zone](#zone)) and attaches a particular earth tube statement to a thermal zone in the building.

### Field: Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design volume flow rate parameter (see next field). This fraction between 0.0 and 1.0 is noted as F~schedule~ in the above equation.

### Field: Design Flow Rate

This number (noted as E~design~ in the above equation) is the maximum amount of air mass flow rate of the earth tube expected at design conditions. The flow rate is expressed in units of m^3^/s. The design value is modified by the schedule fraction (see previous field) and user specified coefficients (see last four fields).

### Field: Minimum Zone Temperature when Cooling

This is the indoor temperature (in Celsius) below which the earth tube is shut off. This lower temperature limit is intended to avoid overcooling a space and thus result in a heating load. For example, if the user specifies a minimum temperature of 20C, earth tube is assumed to be available if the zone air temperature is above 20C. If the zone air temperature drops below 20C, then earth tube is automatically turned off.

### Field: Maximum Zone Temperature when Heating

This is the indoor temperature (in Celsius) above which the earth tube is shut off. This higher temperature limit is intended to avoid overheating a space and thus result in a cooling load. For example, if the user specifies a maximum temperature of 20C, earth tube is assumed to be available if the zone air temperature is below 20C. If the zone air temperature rises above 20C, then earth tube is automatically turned off.

### Field: Delta Temperature

This is the temperature difference (in Celsius) between the indoor and outdoor air dry-bulb temperatures below which the earth tube is shut off. This is to allow the earth tube to be stopped either if the temperature outside is too warm and could potentially heat the space or if the temperature outside is too cold and could potentially cool the space. For example, if the user specifies a delta temperature of 2C, earth tube is assumed to be available if the temperature difference between indoor and outdoor temperature is at least 2C. If the outside air dry-bulb temperature is less than 2C cooler or warmer than the indoor dry-bulb temperature, then the earth tube is automatically turned off.

### Field: Earthtube Type

This alpha character string defines the type of earth tube as one of the following options: Natural, Exhaust, or Intake. A natural earth tube is assumed to be air movement/exchange that will not consume any fan energy or is the result of natural air flow through the tube and into the building. Values for fan pressure and efficiency for a natural flow earth tube are ignored. For either EXHAUST or Intake, values for fan pressure and efficiency define the fan electric consumption. For Natural and Exhaustearth tubes, the conditions of the air entering the space are assumed to be equivalent to the air which is cooled or heated by passing along the pipe. For Intake earth tubes, an appropriate amount of fan heat is added to the air stream.

### Field: Fan Pressure Rise

This is the pressure rise experienced across the fan in Pascals (N/m^2^). This is a function of the fan and plays a role in determining the amount of energy consumed by the fan.

### Field: Fan Total Efficiency

This is the total fan efficiency (a decimal number between 0.0 and 1.0). This is a function of the fan and plays a role in determining the amount of energy consumed by the fan.

### Field: Pipe Radius

This is the radius of the earth tube/pipe (in meters). This plays a role in determining the amount of heat transferred from the surrounding soil to the air passing along the pipe. If the pipe has non-circular cross section, user can use the concept of hydraulic diameter as follows.

![](media/image128.png)\


However, since this field requires the pipe radius, hydraulic diameter should be divided by two.

### Field: Pipe Thickness

This is the thickness of the pipe wall (in meters). This plays a role in determining the amount of heat transferred from the surrounding soil to the air passing along the pipe.

### Field: Pipe Length

This is the total length of the pipe (in meters). This plays a role in determining the amount of heat transferred from the surrounding soil to the air passing along the pipe. As the length of the pipe becomes longer, the amount of the heat transfer becomes larger.

### Field: Pipe Thermal Conductivity

This is the thermal conductivity of the pipe (in W/mC). This plays a role in determining the amount of heat transferred from the surrounding soil to the air passing along the pipe.

### Field: Pipe Depth Under Ground Surface

This is the depth of the pipe under the ground surface (in meters). This plays a role in determining the temperature of the soil surrounding the pipe.

### Field: Soil Condition

This alpha character string defines the actual condition of the soil surrounding the earth tube and can be one of any of the following options: HeavyAndSaturated, HeavyAndDamp, HeavyAndDry or LightAndDry. This determines the thermal diffusivity and thermal conductivity of the surrounding soil, which play a role in determining the amount of heat transferred from the surrounding soil to the air passing along the pipe.

### Field: Average Soil Surface Temperature

This is the annual average soil surface temperature straight above the earth tube, which plays a role in determining the temperature of the soil surrounding the pipe. This field should be calculated in advance using the separate CalcSoilSurfTemp program.

### Field: Amplitude of Soil Surface Temperature

This is the amplitude of soil surface temperature above the earth tube, which plays a role in determining the temperature of the soil surrounding the pipe. This is the difference between the maximum and minimum soil surface temperature for the whole year divided by two. This field should be calculated in advance using the separate CalcSoilSurfTemp program.

### Field: Phase Constant of Soil Surface Temperature

This is the phase constant of the soil surface temperature straight above the earth tube, which play a role in determining the temperature of the soil surrounding the pipe at particular time. This is the time elapsed from the beginning of the year until the soil surface temperature reaches the minimum value of the year. This field should be calculated in advance using the separate CalcSoilSurfTemp program.

### Field: Constant Term Flow Coefficient

This number is the "A" parameter in the above earth tube equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter, however, is a constant under all conditions and is not modified by any environmental effect. As a result, it is dimensionless.

### Field: Temperature Term Flow Coefficient

This number is the "B" parameter in the above earth tube equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter is modified by the temperature difference between the outdoor and indoor air dry-bulb temperatures. The units for this parameter are inverse Celsius.

### Field: Velocity Term Flow Coefficient

This number is the "C" parameter in the above earth tube equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter is modified by the speed of wind being experienced outside the building. The units for this parameter are s/m.

### Field: Velocity Squared Term Flow Coefficient

This number is the "D" parameter in the above earth tube equation. It is part of the user specified modifying parameters that are a function of environmental factors. This parameter is modified by square of the speed of wind being experienced outside the building. The units for this parameter are s^2^/m^2^.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    EARTHTUBE,
      Zone 2,           !- Zone Name
      Simple EarthTube, !- Schedule Name
      3.425198,         !- Design Volume Flow Rate
      10.0,             !- Minimum Zone Temperature when Cooling
      30.0,             !- Maximum Zone Temperature when Heating
      1.0,              !- Delta Temperature
      NATURAL,          !- EarthTube Type
      350.0,            !- Fan Pressure Rise
      0.9,              !- Fan Total Efficiency
      0.25,             !- Pipe Radius
      0.2,              !- Pipe Thickness
      15.0,             !- Pipe Length
      200.0,            !- Pipe Thermal Conductivity
      3.5,              !- Pipe Depth Under Ground Surface
      HeavyAndDamp,     !- Soil Condition
      15.0,             !- Average Soil Surface Temperature
      5.6,              !- Amplitude of Soil Surface Temperature
      0.0,              !- Phase Constant of Soil Surface Temperature
      0.6060000    ,    !- Constant Term Flow Coef
      2.0199999E-02,    !- Temp Term Flow Coef
      5.9800001E-04,    !- Velocity Term Flow Coef
      0.0000000E+00;    !- Velocity**2 Term Flow Coef
~~~~~~~~~~~~~~~~~~~~

## ZoneEarthTube Outputs

Current Earth Tube output variables:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Earth Tube Zone Sensible Cooling Energy [J]
    HVAC,Average,Earth Tube Zone Sensible Cooling Rate [W]
    HVAC,Sum,Earth Tube Zone Sensible Heating Energy [J]
    HVAC,Average,Earth Tube Zone Sensible Heating Rate [W]
    HVAC,Sum,Earth Tube Air Flow Volume [m3]
    HVAC,Average,Earth Tube Air Current Density Volumetric Flow Rate [m3/s]
    HVAC,Average,Earth Tube Air Standard Density Volumetric Flow Rate [m3/s]
    HVAC,Sum,Earth Tube Air Flow Mass [kg]
    HVAC,Average,Earth Tube Air Mass Flow Rate [kg/s]
    HVAC,Sum,Earth Tube Fan Electric Energy [J]
    HVAC,Average,Earth Tube Fan Electric Power [W]
    HVAC,Average,Earth Tube Zone Inlet Air Temperature [C]
    HVAC,Average,Earth Tube Ground Interface Temperature [C]
    HVAC,Average,Earth Tube Outdoor Air Heat Transfer Rate [W]
~~~~~~~~~~~~~~~~~~~~

### Earth Tube Zone Sensible Cooling Energy [J]

### Earth Tube Zone Sensible Cooling Rate [W]

These are the energy and rate associated with the zone cooling provided by the air from the earth tube.  This occurs when the earth tube outlet air temperature is less than zone air temperature.

### Earth Tube Zone Sensible Heating Energy [J]

### Earth Tube Zone Sensible Heating Rate [W]

These are the energy and rate associated with the zone heating provided by the air from the earth tube.  This occurs when the earth tube outlet air temperature is greater than the zone air temperature.

### Earth Tube Air Flow Volume [m3]

The volume flow of air through the earth tube.

### Earth Tube Air Current Density Volumetric Flow Rate [m3/s]

The volume flow rate of air through the earth tube evaluating density at current zone conditions.

### Earth Tube Air Standard Density Volumetric Flow Rate [m3/s]

The volume flow rate of air through the earth tube evaluating density at standard conditions.

### Earth Tube Air Flow Mass [kg]

The mass flow of air through the earth tube.

### Earth Tube Air Mass Flow Rate [kg/s]

The mass flow rate of air through the earth tube.

### Earth Tube Fan Electric Energy [J]

### Earth Tube Fan Electric Power [W]

These are the fan electricity consumption and power for intake or exhaust earth tube types.

### Earth Tube Zone Inlet Air Temperature [C]

This is the temperature of the air entering the zone after passing through the earth tube [C].  This temperature includes the cooling or heating of outdoor air as it passes along the pipe.  When intake fan assist is used, then the additional heat due to the fan is included in the inlet air temperature.

### Earth Tube Ground Interface Temperature [C]

This is the average temperature of the ground along the outer surface of the earth tube [C].

### Earth Tube Outdoor Air Heat Transfer Rate [W]

This is the rate of heat transfer from the earth tube to the outdoor air [W].  Positive values indicate the rate at which outdoor air is preheated; negative values indicate the rate of precooling.

## ZoneCoolTower:Shower

A cooltower (which is sometimes referred to as a wind tower or a shower cooling tower) is a component that is intended to model a passive downdraught evaporative cooling (PDEC) that is designed to capture the wind at the top of a tower and cool the outside air using water evaporation before delivering it to a space.  The air flow in these systems is natural as the evaporation process increases the density of the air causing it to fall through the tower and into the space without the aid of a fan.  A cooltower typically consists of a water spray or an evaporative pad, a shaft, and a water tank or reservoir.  Wind catchers to improve the wind-driven performance at the top of the tower are optional.  Water is pumped over an evaporative device by water pump which is the only component consumed power for this system.  This water cools and humidifies incoming air and then the cool, dense air naturally falls down through shaft and leaves through large openings at the bottom of cooltowers.

The shower cooling tower can be controlled by a schedule and the specification of maximum water flow rate and volume flow rate as well as minimum indoor temperature.  The actual flow rate of water and air can be controlled as users specify the fractions of water loss and flow schedule.  The required input fields include effective tower height and exit area to obtain the temperature and flow rate of the air exiting the tower.  A schedule and rated power for the water pump are also required to determine the power consumed. The component typically has a stand alone water system that is not added to the water consumption from mains. However, users are required to specify the water source through an optional field, the name of water supply storage tank, in case any water comes from a water main. The model is described more fully in the Engineering Reference document.

This model requires weather information obtained from either design day or weather file specifications.  The control is accomplished by either specifying the water flow rate or obtaining the velocity at the outlet with inputs and weather conditions when the water flow rate is unknown.  As with infiltration, ventilation, and earth tubes, the component is treated in a similar fashion to "natural ventilation" in EnergyPlus.

### Inputs

#### Field: Name

This field is a unique user assigned name for each cooltower.  Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

This field is the name of the schedule that denotes whether the cooltower can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the cooltower is available and can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the cooltower is not available and must be off for the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Zone Name

This field is the name of the zone (ref: [Zone](#zone)) and attaches a particular cooltower statement to a thermal zone in the building.

#### Field: Water Supply Storage Tank Name

This field is optional.  It is used to describe where the cooltower obtains water used for evaporative cooling.  If blank or omitted, then the cooltower will obtain water directly from local water main.  If the name of a Water Storage Tank object is used here, then the cooltower will obtain its water from that tank.  If a tank is specified, the cooltower will attempt to obtain all the water it uses from the tank.  However, if the tank cannot provide all the water the cooltower needs, then the cooltower will still operate and obtain the rest of the water it needs from the mains (referred to as ‘Starved' water).

#### Field: Flow Control Type

This field specifies how the user wishes to control the cooltower.  The air flow from the cooltower may be controlled by either the water flow rate along with the water pump schedule or naturally driven by wind flow.  The user must select from the following options: **WaterFlowSchedule** and **WindDrivenFlow**.  If the user either wishes to control the water flow rate or has information about the water flow rate, **WaterFlowSchedule** must be selected.  **WindDrivenFlow** for cooltower flow control must be selected when the water flow rate is unknown and the user wishes to have the flow rate of both air and water controlled by the external weather conditions (wind speed).  If the user does not select a control type, **WindDrivenFlow** is assumed as a flow control type.

#### Field: Pump Flow Rate Schedule Name

This field modifies the maximum flow rate of water through the cooltower in m3/sec. This input is "optional." If the user does not enter a schedule, the flow rate through the cooltower is assumed to be constant during all hours that it is operating based on the value entered in the previous input field. Note that the values for this schedule must be between zero and one.

#### Field: Maximum Water Flow Rate

This field is the maximum water flow rate distributed to the tower in m3/s. This limit is intended to avoid over estimation of water flow rate, which leads higher air flow rate and exit temperature of the air.

#### Field: Effective Tower Height

This field is the effective tower height for evaporative cooling, from the water spray to the top of the exit in m.

#### Field: Airflow Outlet Area

This field is the area at the exit of the tower in m2.  This field is used to determine the air flow rate leaving the tower with the velocity of the air flow.

#### Field: Maximum Air Flow Rate

This field is the maximum volumetric flow rate of the air leaving the tower in m3/s.  This airflow maximum allows the cooltower performance to be limited if the outside wind speed and the tower height are relatively high and thus result in high airflow supplied to the space.

#### Field: Minimum Indoor Temperature

This field is the minimum indoor temperature in Celsius below which cooltower is shutoff.  This lower temperature limit is intended to avoid overcooling a space and thus result in a heating load.  For example, if the user specifies a minimum temperature of 20°C, cooltower is assumed to be available if the zone air temperature is above 20°C.  If the zone air temperature drops below 20°C, then cooltower is automatically turned off.

#### Field: Fraction of Water Loss 

This field specifies the fraction of the loss of water during either operation or transient operation. If the user does not enter a fraction, no loss of water is assumed and the cooltower water consumption includes only evaporation.  Note that the fraction must be between zero and one.

#### Field: Fraction of Flow Schedule

This field specifies the fraction of the airflow that actually goes to the outside.  The user who wishes to control the actual flow to the inside of the building must specify the value of the fraction.  If the user does not enter a fraction, the calculated flow rate is the "zone cooltower volume flow rate."  Note that the fraction must be between zero and one.

#### Field: Rated Power Consumption

This field is the pump's rated power consumption in Watts.

Below is an example input for a cooltower.

~~~~~~~~~~~~~~~~~~~~

    ZoneCoolTower:Shower
    Cool Tower 1,         !- Name of cooltowers
    Zone 1,               !- Zone name
    Simple Vent,          !- Schedule
    ,                     !- Name of water supply storage tanks
    WindDrivenFlow,       !- Flow control type
    0.0005,               !- Water flow rate from the spray in m3/s
    ,                     !- schedule for flow rate (optional, non-existent means constant)
    5.0,                  !- Effective tower height in m
    1.0,                  !- Exit area in m2
    10.0,                 !- Maximum supply air volume flow rate in m3/s
    18.0,                 !- Minimum indoor temperature to prevent overcooling in C
    0.05,                 !- Fraction of Water loss
    0.05,                 !- Fraction of flow that goes to outside
    250.0;                !- Rated power consumption in W
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Zone Cooltower Sensible Heat Loss Energy [J]
    HVAC,Average,Zone Cooltower Sensible Heat Loss Rate [W]
    HVAC,Sum,Zone Cooltower Latent Heat Loss Energy [J]
    HVAC,Average,Zone Cooltower Latent Heat Loss Rate [W]
    HVAC,Sum,Zone Cooltower Air Volume [m3]
    HVAC,Average,Zone Cooltower Air Current Density Volumetric Flow Rate [m3/s]
    HVAC,Average,Zone Cooltower Air Standard Density Volumetric Flow Rate [m3/s]
    HVAC,Sum,Zone Cooltower Air Mass [kg]
    HVAC,Average,Zone Cooltower Air Mass Flow Rate [kg/s]
    HVAC,Average,Zone Cooltower Air Inlet Temperature [C]
    HVAC,Average,Zone Cooltower Air Inlet Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,Zone Cooltower Air Outlet Temperature [C]
    HVAC,Average,Zone Cooltower Air Outlet Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,Zone Cooltower Pump Electric Power [W]
    HVAC,Sum,Zone Cooltower Pump Electric Energy [J]
    HVAC,Sum,Zone Cooltower Water Volume [m3]
    HVAC,Sum,Zone Cooltower Mains Water Volume [m3]
    HVAC,Sum,Zone Cooltower Storage Tank Water Volume [m3]
    HVAC,Sum,Zone Cooltower Starved Mains Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Zone Cooltower Sensible Heat Loss Energy [J]

#### Zone Cooltower Sensible Heat Loss Rate [W]

The sensible heat loss that occurs when the temperature at the exit of the cooltower is less than that of the zone.

#### Zone Cooltower Latent Heat Loss Energy [J]

#### Zone Cooltower Latent Heat Loss Rate [W]

The latent heat loss that occurs when the humidity ratio at the exit of the cooltower is greater than that of the zone.

#### Zone Cooltower Air Current Density Volumetric Flow Rate [m3/s]

The volumetric flow rate of the air leaving the cooltower evaluating density at current zone conditions.

#### Zone Cooltower Air Standard Density Volumetric Flow Rate [m3/s]

The volumetric flow rate of the air leaving the cooltower evaluating density at standard conditions.

#### Zone Cooltower Air Volume [m3]

The sum of actual volumetric flow of the air leaving cooltower

#### Zone Cooltower Air Mass Flow Rate [kg/s]

The mass flow rate of the air leaving the cooltower

#### Zone Cooltower Air Mass [kg]

The sum of actual mass flow of the air leaving the cooltower

#### Zone Cooltower Air Inlet Temperature [C]

The dry-bulb temperature of the outdoor air the inlet of the cooltower

#### Zone Cooltower Air Inlet Humidity Ratio [kgWater/kgDryAir]

 The humidity ratio of the outdoorair at the inlet of the cooltower

#### Zone Cooltower Air Outlet Temperature [C]

The temperature at the exit of the cooltower.

#### Zone Cooltower Air Outlet Humidity Ratio [kgWater/kgDryAir]

The humidity ratio of the air at the exit of the cooltower.

#### Zone Cooltower Water Volume  [m3]

The water consumption includes not only the direct thermodynamics of water evaporation but also other sources of consumption such as drift or concentration blow down specified by users throughout all processes during the operation.

#### Zone Cooltower Mains Water Volume  [m3]

This is the water consumed by the cooltower that actually be met by the mains water. This output variable appears only when the water comes from mains.

#### Zone Cooltower Storage Tank Water Volume  [m3]

This is the water consumed by the cooltower that actually be met by the water storage tank.  If any amount of the water is starved from mains, this water consumption is the difference between the [Zone](#zone) Cooltower Water Volume and the following output.  This output variable appears only when the water comes from storage tank waters.

#### Zone Cooltower Starved Mains Water Volume [m3]

This is the source (mains) of water consumed by the cooltower that could not actually be met by the storage tank.  This output variable appears only when the water comes from storage tank waters.

#### Zone Cooltower Pump Electric Power [W]

The power consumed by the recirculating pump in Watts.

#### Zone Cooltower Pump Electric Energy [J]

The energy consumed by the recirculating pump in Joules.

## ZoneThermalChimney (Thermal Chimney)

A thermal chimney is a vertical shaft utilizing solar radiation to enhance the natural ventilation in buildings. It consists of a absorber wall, air gap and glass cover with high solar transmissivity. For the high solar absorption, it is usually south facing.

The key output parameter in the thermal chimney model is the enhanced amount of natural ventilation rate caused by the presence of a thermal chimney. In order to determine the enhanced ventilation, the discharge air temperature from a thermal chimney should be calculated, which, in turn, should be computed based on the information on the absorber wall temperature, glass cover temperature and the vertical air temperature distribution within the thermal chimney. Among them, energy balances for the absorber wall and the glass cover are carried out using the existing algorithm currently available in EnergyPlus, which has the similar approach to the Trombe wall. As stated in the Trombe wall object as well, this approach allows the flexibility for users to specify the various wall parameters and to explore unusual configurations. On the other hand, the vertical air temperature distribution and the resultant discharge air temperature of the thermal chimney are computed using the separate thermal chimney algorithm described in the Engineering Reference document.

Similar to the Trombe wall model, a zone is coupled to the desired surface via an interzone partition. To simulate the thermal chimney, the Solar Distribution field in the [Building](#building) object should be set to FullInteriorAndExterior so that the majority of the solar flux is directed on the absorber wall. For a normal sized thermal chimney zone, the user can set the [Zone](#zone) Inside Convection Algorithm to "Detailed", which takes into account natural convection effects intended for a normal zone. For a narrow cavity zone having high aspect ratios, there is no built-in algorithm for calculating the correct convection coefficients on the inside of thermal chimney zone walls. One option is to use the "Detailed" convection algorithm similar to the Trombe wall model. However, some error may be incurred when used with a narrow zone. Another option is to use the [SurfaceProperty:ConvectionCoefficients](#surfacepropertyconvectioncoefficients) object to schedule coefficients that have been determined beforehand by the user. In addition, the wall construction of the adjoining zone must be the mirror image of the wall construction in the thermal chimney zone.

The full input description of a thermal chimney in EnergyPlus is given below.

### Field: Name

This field is a unique user assigned name for an instance of the thermal chimney system.

### Field: Zone Name

This field is the name of the thermal chimney zone (ref: [Zone](#zone)). Since the thermal chimney is not only a system component but also a zone itself, this field is also necessary. It should be differentiated from the *[Zone](#zone) name* field described later.

### Field: Availability Schedule Name

This field is the name of the schedule (ref: Schedule) that denotes whether the thermal chimney can operate during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the system is available and can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the system is not available and must be off for the time period. If this field is blank, the schedule has values of 1 for all time periods.

### Field: Width of the Absorber Wall

This number is the width of the absorber wall in the thermal chimney.  The width is expressed in units of m. Even though this value is specified in Surface objects as well, this value is used to compute the discharge air temperature and the enhanced ventilation rate caused by the thermal chimney.

### Field: Cross Sectional Area of Air Channel Outlet

This number is the cross sectional area of air channel outlet. The area is expressed in units of m^2^. The enhanced air flow rate by the thermal chimney is dependent on cross sectional areas of air channel inlet and outlet. Cross sectional areas of air channel inlet will be described later in conjunction with the distance from the top of thermal chimney to each inlet and relative ratios of air flow rates passing through each inlet.

### Field: Discharge Coefficient

This dimensionless number is the discharge coefficient of the thermal chimney. The ventilation rate enhanced by the thermal chimney is also dependent on the discharge coefficient.

### Field: Zone <#> Name

This field is the name of the zone (ref: [Zone](#zone)) to which the thermal chimney is attached. It is used in conjunction with the next three fields. Note that up to 20 sets of zone name, distance from the top of the thermal chimney to each inlet, relative ratios of air flow rates passing through each zone and cross sectional areas of each air channel inlet may be entered for a single thermal chimney if multiple zones share the common thermal chimney.

### Field: Distance from Top of Thermal Chimney to Inlet <#>

This field is the distance from the top of the thermal chimney to each inlet corresponding to each zone. It is used in conjunction with the zone name, relative ratios of air flow rates passing through each zone and cross sectional areas of each air channel inlet. The distance is expressed in units of m. The air flow rate enhanced by the thermal chimney is dependent on the distance between the thermal chimney outlet and inlet.

### Field: Relative Ratios of Air Flow Rates Passing through Zone <#>

This dimensionless number is the relative ratio of air flow rates enhanced by the thermal chimney passing through each zone. The total air flow rate enhanced by the thermal chimney is distributed to each zone based on this number if multiple zones share the common thermal chimney. It is used in conjunction with the zone name, the distance from the top of the thermal chimney to each inlet and cross sectional areas of each air channel inlet. Note that the sum of all ratios must be equal to 1.0.

### Field: Cross Sectional Areas of Air Channel Inlet <#>

This field is the cross sectional areas of each air channel inlet corresponding to each zone. It is used in conjunction with the zone name, the distance from the top of the thermal chimney to each inlet and relative ratios of air flow rates passing through each zone. The area is expressed in units of m^2^. The air flow rate enhanced by the thermal chimney is dependent on cross sectional areas of air channel inlet and outlet.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    ZoneThermalChimney,
      ThermalChimney1,               !- Name of Thermal Chimney System
      ThermalChimneyZone,            !- Name of Thermal Chimney Zone
      ThermalChimneyAvail,           !- Availability Schedule Name
      3.5,                           !- Width of the Absorber Wall
      0.04,                          !- Cross Sectional Area of Air Channel Outlet
      0.8,                           !- Discharge Coefficient
      Zone1,                         !- Zone Name 1
      8.0,                           !- Distance from the Top of the Thermal Chimney to Inlet 1
      0.8,                           !- Relative Ratios of Air Flow Rates Passing through Zone 1
      0.02,                          !- Cross Sectional Areas of Air Channel Inlet 1
      Zone2,                         !- Zone Name 2
      5.0,                           !- Distance from the Top of the Thermal Chimney to Inlet 2
      0.2,                           !- Relative Ratios of Air Flow Rates Passing through Zone 2
      0.02;                          !- Cross Sectional Areas of Air Channel Inlet 2
~~~~~~~~~~~~~~~~~~~~

## ZoneThermalChimney Outputs

Current ThermalChimney output variables:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Zone Thermal Chimney Current Density Volumetric Flow Rate [m3/s]
    HVAC,Average,Zone Thermal Chimney Standard Density Volumetric Flow Rate [m3/s]
    HVAC,Average,Zone Thermal Chimney Mass Flow Rate [kg/s]
    HVAC,Average,Zone Thermal Chimney Outlet Temperature [C]
    HVAC,Sum,Zone Thermal Chimney Heat Loss Energy [J]
    HVAC,Sum,Zone Thermal Chimney Heat Gain Energy [J]
    HVAC,Sum,Zone Thermal Chimney Volume [m3]
    HVAC,Sum,Zone Thermal Chimney Mass [kg]
~~~~~~~~~~~~~~~~~~~~

### Zone Thermal Chimney Heat Loss Energy [J]

The sensible (temperature) heat loss of each zone that occurs when the thermal chimney cools the zone air.

### Zone Thermal Chimney Heat Gain Energy [J]

The sensible (temperature) heat gain of each zone that occurs when the thermal chimney heats the zone air.

### Zone Thermal Chimney Volume [m3]

The air volumetric flow of each zone enhanced by the thermal chimney.

### Zone Thermal Chimney Mass [kg]

The air mass flow of each zone enhanced by the thermal chimney.

### Zone Thermal Chimney Current Density Volumetric Flow Rate [m3/s]

The total air volumetric flow rate caused by the thermal chimney evaluating density at the current zone conditions.

### Zone Thermal Chimney Standard Density Volumetric Flow Rate [m3/s]

The total air volumetric flow rate caused by the thermal chimney evaluating density at standard conditions.

### Zone Thermal Chimney Mass Flow Rate [kg/s]

The total air mass flow rate caused by the thermal chimney.

### Zone Thermal Chimney Outlet Temperature [C]

The temperature of the air which is discharged from the thermal chimney through the outlet.

## ZoneAirMassFlowConservation

This global object allows users to trigger the zone air mass flow conservation calculation when desired. This object has two input fields; the first choice input field allows the user whether to enforce or not to enforce the zone air mass flow conservation; and the second input field allows the user to specify how infiltration object mass flow rate is calculated for zone air mass flow balance calculation.  The first input field of this object has two choice KEYs: "Yes" and "No". If this input is specified as "Yes", then energy plus attempts to enforce the zone mass conservation, or else if it is specified as "No", then EnergyPlus calculation defaults to zone air flow balance calculation that does not include zone mixing objects and that assumes self-balanced simple flow objects procedure, which may not necessarily enforce zone air mass flow conservation unless the user has specified a balanced flow to begin with. The zone air mass flow conservation primarily accounts for the zonemixing objects air flow in the zone air flow mass balance calculation. In additional to the zonemixing object flow, the procedure accounts for zone exhaust fan flows by providing additional infiltration air flows when required in order to balance the zone air mass flow.  Hence, zonemixing object must to be defined to trigger zone air mass flow conservation calculation, whereas infiltration object is required only for zones which are used as a source zone of the zone mixing object.  [Zone](#zone) air mass flow balance calculation is enforced both for the receiving and source zones of every mixing object defined. The zone air mass flow conservation calculation uses two steps procedure.

First, the [ZoneMixing](#zonemixing) object mass flow rate is adjusted or modified in order to balance zone air mass flow while assuming the zone infiltration object air mass flow self-balanced. This step will always results in balanced zone air mass for receiving zones of [ZoneMixing](#zonemixing) object but it may not necessarily result in a balanced air mass flow for source zones.  Infiltration objects air mass flow rate defined for receiving zones are always calculated based on user inputs and assumed to be self-balanced. The infiltration mass flow rate of zones that serve only as a source zone may require adjusting base infiltration flow, which is calculated based on user inputs in the infiltration objects, in order to balance the zone air mass flow, i.e., the second calculation step replenishes the source zones with additional infiltration air mass flow when required. This second step is required in zones which serve as a source zone for zone mixing objects and when the zone mixing source mass flow rate exceeds the supply air mass flow rate.  There are two calculation procedures that users can choose from on how the infiltration flow rate is calculated for source zones that need infiltration object mass flow in order to balance the zone air mass flow.  The second optional input field "Source [Zone](#zone) Infiltration Treatment" provides two Key choice inputs: "**AddInfiltrationFlow**" and "**AdjustInfiltrationFlow**".

**AddInfiltrationFlow**: Energyplus adds infiltration air mass flow rate on top of the base infiltration flow, which is calculated using the infiltration object user inputs, in order to balance the zone air mass flow.  This additional infiltration air mass flow is not self-balanced, i.e., it is always assumed incoming flow. If no infiltration air is required in order to be balance the zone air mass flow, then the additional infiltration air mass flow rate is set to zero. The base infiltration flow calculated using the infiltration object user inputs is always assumed to be self-balanced.

**AdjustInfiltrationFlow**: Energyplus may adjust the base flow calculated using the infiltration object user inputs if it is required in order to balance the zone air mass flow.  If it is not required to adjust the base infiltration air flow, then the base infiltration air mass flow, which is calculated from user input of the infiltration object, is retained and assumed self-balanced. The report variable "[Zone](#zone) Infiltration Air Mass Flow Balance Status" indicates whether the infiltration object air mass flow is adjusted or not.  If the value of this report variable is **0**, then the zone infiltration object mass flow rate is not included in the zone mass flow balance hence the infiltration air flow rate calculated based on the user specified inputs is manintained as is and assumed self-balanced for current timestep.  If the value of this report variable is 1, then the zone infiltration object mass flow rate is included in the zone mass flow balance, hence the user specified infiltration rate is modified and it is considered as incoming flow to the zone, i.e., self-balanced assumption is not valid for current time step.

This object is optional, only required in the input data file if the user wishes to enforce the zone air mass flow balance calculation that includes zonemixing and infiltration objects.

### Inputs

#### Field: Adjust Zone Mixing For Zone Air Mass Flow Balance 

It has two choice KEYs: "Yes" and "No".  If this input is specified as "Yes", then Energyplus attempts to enforce the zone mass conservation, or else if it is specified as "No", then EnergyPlus calculation defaults to the existing procedure, which may not necessarily enforce zone mass conservation unless the user specified a balanced flow to begin with.  The default input is "No".  Note that "No" input may also results in balanced flow depending on the system specified. If this input field is specified as "No", then the next input field it not used.

#### Field: Source Zone Infiltration Treatment 

It has two choice KEYs: "AddInfiltrationFlow" and "AdjustInfiltrationFlow".  If this input is specified as "AddInfiltrationFlow", then Energyplus adds infiltration air mass flow on top of the base infiltration flow calculated using the infiltration object user inputs in order to balance the zone air mass flow.  The additional infiltration air mass flow is not self-balanced.  If this input is specified as "AdjustInfiltrationFlow", then Energyplus may adjust the base flow calculated using the infiltration object user inputs if it is required inorder to balance the zone air mass flow.  If it not required to adjust the base infiltration flow calculated using the user specified infiltration object inputs, then the base infiltration air mass flow is assumed self-balanced.

And, a default IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

      ZoneAirMassFlowConservation,
      Yes,                       !- Adjust Zone Mixing For Zone Air Mass Flow Balance
      AdjustInfiltrationFlow;    !- Source Zone Infiltration Treatment
~~~~~~~~~~~~~~~~~~~~

### Outputs

Current [ZoneAirMassFlowConservation](#zoneairmassflowconservation) output variables:

~~~~~~~~~~~~~~~~~~~~

    HVAC, Average, Zone Supply Air Mass Flow Rate [kg/s]
    HVAC, Average, Zone Exhaust Air Mass Flow Rate [kg/s]
    HVAC, Average, Zone Return Air Mass Flow Rate [kg/s]
    HVAC, Average, Zone Mixing Receiving Air Mass Flow Rate [kg/s]
    HVAC, Average, Zone Mixing Source Air Mass Flow Rate [kg/s]
    HVAC, Average, Zone Infiltration Air Mass Flow Balance Status, []
    HVAC, Average, Zone Mass Balance Infiltration Air Mass Flow Rate, [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### Zone Supply Air Mass Flow Rate [kg/s]

This output variable represents the total supply air mass flow rate of a zone. The value is determined by summing the supply air mass flow rates contributions from all supply air inlet nodes of a zone.

#### Zone Exhaust Air Mass Flow Rate [kg/s]

This output variable represents the total exhaust air mass flow rate of a zone. The value is determined by summing the exhaust air mass flow rates contributions from all exhaust air nodes of a zone.

#### Zone Return Air Mass Flow Rate [kg/s]

This output variable represents the total return air mass flow rate of a zone. The value is determined by summing the return air mass flow rates contributions from return air nodes of a zone.

#### Zone Mixing Receiving Air Mass Flow Rate [kg/s]

This output variable represents the total zone mixing air mass flow rate of a receiving zone from one or more mixing objects. The value is determined by summing the air mass flow contributions from all zone mixing objects connected to a single receiving zone.

#### Zone Mixing Source Air Mass Flow Rate [kg/s]

This output variable represents the total zone mixing source air mass flow rate of a source zone feeding one or more mixing objects. The value is determined by summing the air mass flow contributions from all zone mixing objects connected to a single source zone.

#### Zone Infiltration Air Mass Flow Balance Status []

This output variable indicates the status of the infiltration object mass flow rate use for balancing the zone mass flow at each time step. It has values of either **0** or **1**.  If the value of this report variable is **0** then the zone infiltration object mass flow rate is not used in the zone mass conservation calculation, hence the infiltration rate calculated based on the user specified inputs is manintained and the infiltration rate is assumed as self-balanced for current timestep.  If the value is 1 then the zone infiltration object mass flow rate is included in the zone air mass flow balance calculation, hence the user specified infiltration rate is modified and it is considered as incoming flow to the zone, i.e., self-balanced assumption is not valid for this zone and current time step.

#### Zone Mass Balance Infiltration Air Mass Flow Rate [kg/s]

This output variable represents the zone infiltration air mass flow rate in kg/s.  This output variable is reported only for source zones and when the zone air mass flow balance flag is set to "Yes" and its value depends on the "Source [Zone](#zone) Infiltration Treatment" method specified.  When the infiltration treatment method selected is "AddInfiltrationFlow" this report variable represents additional infiltration air mass flow rate added on top of the base infiltration air flow calculated using the user inputs inroder to balance the zone air mass flow. In this case, the base infiltration air mass flow calculated using the user specified input is assumed self-balanced.  When the infiltration treatment method selected is "AdjustInfiltrationFlow" this report variable represents the base infiltration air mass flow calculated using the user inputs and can be adjuted as needed in roder to balance the zone air mass flow. If the value of the output variable "[Zone](#zone) Infiltration Air Mass Flow Balance Status" is **0**, then the infiltration air mass flow rate calculated based on the user specified inputs is manintained and the infiltration rate is assumed as self-balanced for current timestep, or else  if [Zone](#zone) Infiltration Air Mass Flow Balance Status" is **1**, then user specified infiltration rate is adjusted and it is considered as incoming flow to the zone, i.e., self-balanced assumption is not valid for this zone and current time step.