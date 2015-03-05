# Group – Internal Gains (People, Lights, Other internal zone equipment)

Not all the influence for energy consumption in the building is due to envelope and ambient conditions. This group of objects describes other internal gains that may come into play ([People](#people), [Lights](#lights), Various Equipment Types).

## People

The people statement is used to model the occupant's effect on the space conditions. The following definition addresses the basic affects as well as providing information that can be used to report the thermal comfort of a group of occupants. The Fanger, Pierce Two-Node, and Kansas State University Two-Node thermal comfort models are available in EnergyPlus. A user may select any of these models for each [People](#people) statement by simply adding the appropriate choice keyword after the air velocity schedule name. Thermal comfort calculations will only be made for people statements that include specific requests for these thermal comfort models. . This object also requires input of carbon dioxide generation rate based on people activity level for zone carbon dioxide simulations.

### Inputs

#### Field: Name

The name of the [People](#people) object. Must be unique across all [People](#people) objects.

#### Field: Zone or ZoneList Name

This field is the name of the zone (ref: [Zone](#zone)) or [ZoneList](#zonelist) (ref: [ZoneList](#zonelist)) and links a particular people statement to a thermal zone or set of thermal zones in the building. When the [ZoneList](#zonelist) option is used then this people definition is applied to each of the zones in the zone list effecting a global definition for the number of people in the zone. The Zonelist option can be used effectively with the people/area and area/person options of the Number of [People](#people) Calculation Method.

The name of the actual people object becomes <[Zone](#zone) Name> <[People](#people) Object Name> and should be less than the standard length (100 characters) for a name field. If it is greater than this standard length, it may be difficult to specify in output reporting as it will be truncated. A warning will be shown if the generated name is greater than 100 characters. If it duplicates another such concatenated name, there will be a severe error and terminate the run.

#### Field: Number of People Schedule Name

This field is the name of the schedule (ref: Schedules) that modifies the number of people parameter (see Number of [People](#people) Calculation Method and related fields). The schedule values can be any positive number. The actual number of people in a zone as defined by this statement is the product of the number of people field and the value of the schedule specified by name in this field.

#### Field: Number of People Calculation Method

This field is a key/choice field that tells which of the next three fields are filled and is descriptive of the method for calculating the nominal number of occupants (people) in the [Zone](#zone). The key/choices are:

- People
- With this choice, the method used will be a straight insertion of the number of occupants (people).  (The Number of [People](#people) field should be filled.)
- People/Area
- With this choice, the method used will be a factor per floor area of the zone. (The [People](#people) per [Zone](#zone) Floor Area field should be filled).
- Area/Person
- With this choice, the method used will be a factor of floor area per person. (The [Zone](#zone) Floor Area per Person field should be filled).

#### Field: Number of People

This field is used to represent the maximum number of people in a zone that is then multiplied by a schedule fraction (see schedule field). In EnergyPlus, this is slightly more flexible in that the number of people could be a "diversity factor" applied to a schedule of real numbers. Note that while the schedule value can vary from hour to hour, the number of people field is constant for all simulation environments.

#### Field: People per Zone Floor Area

This factor (person/m^2^) is used, along with the [Zone](#zone) Floor Area to determine the maximum number of people as described in the Number of [People](#people) field. The choice from the method field should be "people/area".

#### Field: Zone Floor Area per Person

This factor (m^2^/person) is used, along with the [Zone](#zone) Floor Area to determine the maximum number of people as described in the Number of [People](#people) field. The choice from the method field should be "area/person".

#### Field: Fraction Radiant

This field is a decimal number between 0.0 and 1.0 and is used to characterize the type of heat being given off by people in a zone. The number specified in this field will be multiplied by the total sensible energy emitted by people to give the amount of long wavelength radiation gain from human beings in a zone. The remainder of the sensible load is assumed to be convective heat gain. Note that latent gains from people are not included in either the radiant or convective heat gains. See the Engineering Reference document for more details.

#### Field: Sensible Heat Fraction

The user can use this field to specify a fixed sensible fraction for the heat gain due to this PEOPLE object. Normally the program calculates the sensible/latent split; this field gives the user control over this split. This field is autocalculated: if the field is blank or **autocalculate**, the program will calculate the sensible/latent split; if a value is entered, it will be used as the sensible fraction of the current total heat gain.

#### Field: Activity Level Schedule Name

This field is the name of the schedule that determines the amount of heat gain per person in the zone under design conditions. This value is modified somewhat based on a correlation to account for variations in space temperature. The schedule values may be any positive number and the units for this parameter is Watts per person. This schedule represents the total heat gain per person including convective, radiant, and latent. An internal algorithm is used to determine what fraction of the total is sensible and what fraction is latent. Then, the sensible portion is divided into radiant and convective portions using the value specified for Fraction Radiant (above). See the Engineering Reference document for more details.

Values for activity level can range anywhere from approximately 100-150 Watts per person for most office activities up to over 900 Watts per person for strenuous physical activities such as competitive wrestling. The following table (Table 11) is based on Table 4 from the 2005 ASHRAE Handbook of Fundamentals, page 8.6. In addition to the information from the ASHRAE HOF, there is an added column of values in W/Person such as necessary for the activity level schedule values. This column uses the standard adult body surface area of 1.8 m^2^ to multiply the activity levels in W/m^2^ that are used in the table. Warnings are produced when the activity level schedule values fall outside normal ranges. Having too low or too high values can also skew thermal comfort reporting values.

#### Field: Carbon Dioxide Generation Rate

This numeric input field specifies carbon dioxide generation rate per person with units of m3/s-W. The total carbon dioxide generation rate from this object is:

Number of [People](#people) \* [People](#people) Schedule \* [People](#people) Activity \* Carbon Dioxide Generation Rate. The default value is 3.82E-8 m3/s-W (obtained from ASHRAE Standard 62.1-2007 value at 0.0084 cfm/met/person over the general adult population). The maximum value can be 10 times the default value.

#### Field: Enable ASHRAE 55 comfort warnings

This field accepts either "Yes" or "No" as values. When "Yes" is specified, warnings are generated when the space conditions are outside of the ASHRAE 55 comfort range as discussed in the sections that follow titled "Simplified ASHRAE 55-2004 Graph Related Outputs" and "Simplified ASHRAE 55 Warnings." The default is not to provide these warnings so if you want to know if your space is outside this comfort range you must set this field to Yes.

Table: Metabolic Rates for Various Activities

Activity|Activity Level W/Person EnergyPlus Schedule Value|Activity Level W/m^2^|met\*
--------|-------------------------------------------------|---------------------|-----
*Resting*
Sleeping|72|40|0.7
Reclining|81|45|0.8
Seated, quiet|108|60|1
Standing, relaxed|126|70|1.2
*Walking (on level surface)*
3.2 km/h (0.9 m/s)|207|115|2
4.3 km/h (1.2 m/s)|270|150|2.6
6.4 km/h (1.8 m/s)|396|220|3.8
*Office Activities*
Reading, seated|99|55|1
Writing|108|60|1
Typing|117|65|1.1
Filing, seated|126|70|1.2
Filing, standing|144|80|1.4
Walking about|180|100|1.7
Lifting/packing|216|120|2.1
*Miscellaneous Occupational Activities*
Cooking|171 to 207|95 to 115|1.6 to 2.0
Housecleaning|207 to 360|115 to 200|2.0 to 3.4
Seated, heavy limb movement|234|130|2.2
Machine work| ||
sawing (table saw)|189|105|1.8
light (electrical industry)|207 to 252|115 to 140|2.0 to 2.4
heavy|423|235|4
Handling 50 kg bags|423|235|4
Pick and shovel work|423 to 504|235 to 280|4.0 to 4.8
*Miscellaneous Leisure Activities*
Dancing, social|252 to 459|140 to 255|2.4 to 4.4
Calisthenics/exercise|315 to 423|175 to 235|3.0 to 4.0
Tennis, singles|378 to 486|210 to 270|3.6 to 4.0
Basketball|522 to 792|290 to 440|5.0 to 7.6
Wrestling, competitive|738 to 909|410 to 505|7.0 to 8.7

**\*Note that one met = 58.1 W/m^2^**

#### Field: Mean Radiant Temperature Calculation Type

This field specifies the type of Mean Radiant Temperature (MRT) calculation the user wishes to use for the thermal comfort model. At the present time, there are two options for MRT calculation type: zone averaged and surface weighted. The default calculation is "ZoneAveraged". In the zone averaged MRT calculation, the MRT used for the thermal comfort calculations is for an "average" point in the zone. MRT is calculated based on an area-emissivity weighted average of all of the surfaces in the zone. In cases where the emissivity of all of the surfaces are sufficiently small (near zero), the mean radiant temperature will be set to the mean air temperature of the space to avoid divide by zero errors. The other MRT calculation type is "SurfaceWeighted". The goal of this calculation type is to estimate a person in the space close to a particular surface without having to define exact view factors for all of the surfaces and the location of the person in the space. The MRT used in the thermal comfort calculations when the "surface weighted" calculation type is selected is actually the average of the temperature of the surface to which the person is closest (defined by the next field "Surface Name") and the zone averaged MRT (defined above). The surface temperature alone is not used because in theory the maximum view factor from a person to any flat surface is roughly 0.5. In the "surfaceweighted" calculation, the surface in question actually gets slightly more weighting than 50% since the surface selected is still a part of the zone average MRT calculation. Again, this simplification was made to avoid the specification of view factors and the exact location of the person.

A third option is to use "anglefactor". This option allows for more explicit positioning of the person within the space by defining the angle factors from the person to the various surfaces in the zone. This option requires the user to list the surfaces that the person can see from a radiation standpoint and also define the angle (or view) factor for each surface. The AngleFactorList object (see next object description) is intended to give the user this opportunity.

#### Field: Surface Name/Angle Factor List Name

This field is only valid when the user selects "surfaceweighted" for the MRT calculation type (see the previous input field description). In this case, the field is the name of a surface within the zone the people are residing. This surface will be used in the MRT calculation as defined above to come up with a more representative MRT for a person near a particular surface. The MRT used for thermal comfort calculations using the "surface weighted" MRT calculation method is the average of the temperature of the surface specified in this field and the "zone averaged" MRT (see the Mean Radiant Temperature calculation type field above).

#### Field: Work Efficiency Schedule Name

This field is the name of the schedule that determines the efficiency of energy usage within the human body that will be used for thermal comfort calculations. Note that all energy produced by the body is assumed to be converted to heat for the zone heat balance calculation. A value of zero corresponds to all of the energy produced in the body being converted to heat. A value of unity corresponds to all of the energy produced in the body being converted to mechanical energy. The values for this parameter defined in the schedule must be between 0.0 and 1.0. Any value greater than zero will result in a reduction of heat that impacts the thermal comfort energy balance of a person within the space, resulting in PMV results appearing lower than expected.  Ensure that if this value is non-zero, the base activity level is chosen to ensure that the net activity converted to heat and zone conditions are sufficient to maintain thermal comfort..

#### Field: Clothing Insulation Calculation Method

This field is a key/choice field that tells which of the next two fields are filled and is descriptive of the method for calculating the clothing insulation value of occupants (people) in the [Zone](#zone). The key/choices are:

- ClothingInsulationSchedule

- With this choice, the method used will be a straight insertion of the scheduled clothing insulation values of occupants (people).  (The Clothing Insulation Schedule Name field should be filled.)

- DynamicClothingModelASHRAE55

- With this choice, the method used will be the dynamic predictive clothing insulation model developed by Schiavon and Lee (2013) based on 6,333 selected observations taken from ASHRAE RP-884 and RP-921 databases. It varies the clothing insulation as a function of outdoor air temperature measured at 6am as illustrated below.

- CalculationMethodSchedule

- With this choice, the method used can be either the ClothingInsulationSchedule or the DynamicClothingModelASHRAE55, depending on a schedule (to be entered as the next field) that determines which method to use in different time of a day. When this option is chosen, the next field "Clothing Insulation Calculation Method Schedule Name" is a required input.

![Graphical representation fo the dynamic predictive clothing insulation model](media/graphical-representation-fo-the-dynamic.png)


#### Field: Clothing Insulation Calculation Method Schedule Name

This field specifies which clothing insulation method (ClothingInsulationSchedule or DynamicClothingModelASHRAE55) to use at a particular time of the day. A schedule value of 1 means the ClothingInsulationSchedule method, and 2 means the DynamicClothingModelASHRAE55 method. This field is only required when the "Clothing Insulation Calculation Method" field is set to **CalculationMethodSchedule**. If this field is left blank, the specified clothing insulation calculation method will be used and not changed during the simulation.

#### Field: Clothing Insulation Schedule Name

This field is the name of the schedule that defines the amount of clothing being worn by a typical zone occupant during various times in the simulation period. The choice from the Clothing Insulation Calculation Method field should be "**ClothingInsulationSchedule**". This parameter must be a positive real number and has units of Clo. Typical values for Clo can be seen in the ASHRAE 2009 HOF Table 7, page 9.8 (for clothing ensembles) and Table 8, page 9.9 (for garment values) ) or www.cbe.berkeley.edu/comforttool/.

#### Field: Air Velocity Schedule Name

This field is the name of the schedule that approximates the amount of air movement in the space as a function of time throughout the simulation period. The user has control over this parameter through the schedule that requires the individual numbers in the schedule to be positive real numbers having units of meters per second.

#### Field: Thermal Comfort Model Type (up to 5 allowed)

The final one to five fields are optional and are intended to trigger various thermal comfort models within EnergyPlus. By entering the keywords Fanger, Pierce, KSU, AdaptiveASH55,, and AdaptiveCEN15251, the user can request the Fanger, Pierce Two-Node, Kansas State UniversityTwo-Node, and the adaptive comfort models of the ASHRAE Standard 55 and CEN Standard 15251 results for this particular people statement. Note that since up to five models may be specified, the user may opt to have EnergyPlus calculate the thermal comfort for people identified with this people statement using all five models if desired. Note that the KSU model is computationally intensive and may noticeably increase the execution time of the simulation. For descriptions of the thermal comfort calculations, see the Engineering Reference document.

The following IDF example allows for a maximum of 31 people with scheduled occupancy of "Office Occupancy", 60% radiant using an Activity Schedule of "Activity Sch". The example allows for thermal comfort reporting.

~~~~~~~~~~~~~~~~~~~~

    People,
      Kitchen_ZN_1_FLR_1,  !- Name
      Kitchen_ZN_1_FLR_1,  !- Zone or ZoneList Name
      BLDG_OCC_SCH,  !- Number of People Schedule Name
      People,  !- Number of People Calculation Method
      25.2000,,,  !- Number of People, People per Zone Floor Area, Zone Floor Area per Person
      0.3000,  !- Fraction Radiant
      AUTOCALCULATE,  !- Sensible Heat Fraction
      ACTIVITY_SCH,  !- Activity Level Schedule Name
      3.82E-8        !- Carbon Dioxide Generation Rate {m3/s-W}
      No,  !- Enable ASHRAE 55 Comfort Warnings
      ZoneAveraged,  !- Mean Radiant Temperature Calculation Type
      ,  !- Surface Name/Angle Factor List Name
      WORK_EFF_SCH,  !- Work Efficiency Schedule Name
      CLOTHING_SCH,  !- Clothing Insulation Schedule Name
      AIR_VELO_SCH,  !- Air Velocity Schedule Name
      Fanger;  !- Thermal Comfort Model 1 Type
~~~~~~~~~~~~~~~~~~~~

A simpler example, without using the thermal comfort reporting option:

~~~~~~~~~~~~~~~~~~~~

    People,
        RIGHT FORK,              !- Name
        RIGHT FORK,              !- Zone or ZoneList Name
        Dorm Occupancy,          !- Number of People Schedule Name
        people,                  !- Number of People Calculation Method
        8.00000,                 !- Number of People,
       ,                         !- People per Zone Floor Area
       ,                         !- Zone Floor Area per Person
        0.6000000,               !- Fraction Radiant
        Autocalculate,           !- Sensible Heat Fraction
        Activity Sch,            !- Activity level Schedule Name
~~~~~~~~~~~~~~~~~~~~

And with the sensible fraction specified:

~~~~~~~~~~~~~~~~~~~~

    People,
        SPACE1-1 People 1,       !- Name
        SPACE1-1,                !- Zone or ZoneList Name
        OCCUPY-1,                !- Number of People Schedule Name
        people,                  !- Number of People Calculation Method
        11,                      !- Number of People
        ,                        !- People per Zone Floor Area
        ,                        !- Zone Floor Area per Person
        0.3,                     !- Fraction Radiant
        0.55,                    !- Sensible Heat Fraction
        ActSchd;                 !- Activity level Schedule Name
~~~~~~~~~~~~~~~~~~~~

Global [People](#people) Object:

~~~~~~~~~~~~~~~~~~~~

      ZoneList,AllOccupiedZones,SPACE1-1,SPACE2-1,SPACE3-1,SPACE4-1,SPACE5-1;

      People,
        AllZones with People,       !- Name
        AllOccupiedZones,        !- Zone or ZoneList Name
        OCCUPY-1,                !- Number of People Schedule Name
        People/Area,             !- Number of People Calculation Method
        ,                        !- Number of People
        .11,                     !- People per Zone Floor Area {person/m2}
        ,                        !- Zone Floor Area per Person {m2/person}
        0.3,                     !- Fraction Radiant
        ,                        !- Sensible Heat Fraction
        ActSchd;                 !- Activity Level Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

[People](#people) objects have output variables for individual objects and for zone totals.

[People](#people) specific outputs include:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,People Occupant Count []
    Zone,Sum,People Radiant Heating Energy [J]
    Zone,Average,People Radiant Heating Rate [W]
    Zone,Sum,People Convective Heating Energy [J]
    Zone,Average,People Convective Heating Rate [W]
    Zone,Sum,People Sensible Heating Energy [J]
    Zone,Average,People Sensible Heating Rate [W]
    Zone,Sum,People Latent Gain Energy [J]
    Zone,Average,People Latent Gain Rate [W]
    Zone,Sum,People Total Heating Energy [J]
    Zone,Average,People Total Heating Rate [W]
    Zone,Average,Zone People Occupant Count []
    Zone,Sum,Zone People Radiant Heating Energy [J]
    Zone,Average,Zone People Radiant Heating Rate [W]
    Zone,Sum,Zone People Convective Heating Energy [J]
    Zone,Average,Zone People Convective Heating Rate [W]
    Zone,Sum,Zone People Sensible Heating Energy [J]
    Zone,Average,Zone People Sensible Heating Rate [W]
    Zone,Sum,Zone People Latent Gain Energy [J]
    Zone,Average,Zone People Latent Gain Rate [W]
    Zone,Sum,Zone People Total Heating Energy [J]
    Zone,Average,People Air Temperature [C]
    Zone,Average,People Air Relative Humidity [%]
    Zone,Average,Zone People Total Heating Rate [W]
    Zone,Average,Zone Thermal Comfort Mean Radiant Temperature [C]
    Zone,Average,Zone Thermal Comfort Operative Temperature [C]
    Zone,Average,Zone Thermal Comfort Fanger Model PMV []
    Zone,Average,Zone Thermal Comfort Fanger Model PPD [%]
    Zone,Average,Zone Thermal Comfort Clothing Surface Temperature [C]
    Zone,Average,Zone Thermal Comfort Pierce Model Effective Temperature PMV []
    Zone,Average,Zone Thermal Comfort Pierce Model Standard Effective Temperature PMV []
    Zone,Average,Zone Thermal Comfort Pierce Model Discomfort Index []
    Zone,Average,Zone Thermal Comfort Pierce Model Thermal Sensation Index []
    Zone,Average,Zone Thermal Comfort KSU Model Thermal Sensation Index []
    Zone,Average,Zone Thermal Comfort ASHRAE 55 Adaptive Model 80% Acceptability Status []
    Zone,Average,Zone Thermal Comfort ASHRAE 55 Adaptive Model 90% Acceptability Status []
    Zone,Average,Zone Thermal Comfort ASHRAE 55 Adaptive Model Running Average Outdoor Air Temperature [C]
    Zone,Average,Zone Thermal Comfort ASHRAE 55 Adaptive Model Temperature [C]
    Zone,Average,Zone Thermal Comfort CEN 15251 Adaptive Model Category I Status []
    Zone,Average,Zone Thermal Comfort CEN 15251 Adaptive Model Category II Status []
    Zone,Average,Zone Thermal Comfort CEN 15251 Adaptive Model Category III Status
    Zone,Average,Zone Thermal Comfort CEN 15251 Adaptive Model Running Average Outdoor Air Temperature [C]
    Zone,Average,Zone Thermal Comfort CEN 15251 Adaptive Model Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### People Occupant Count []

This field is the number of people for this PEOPLE object during the timestep in question.

#### People Radiant Heating Rate [W]

#### People Radiant Heating Energy [J]

These output variables are the amount of radiant heat gain for this [People](#people) object in Watts (for rate) or Joules. This is determined by the current sensible heat gain from people to the zone and the "Fraction Radiant" specified in the input. The radiant gains from people are distributed to the surfaces using an area weighting scheme.

#### People Convective Heating Rate [W]

#### People Convective Heating Energy [J]

These output variables are the amount of convective heat gain for this [People](#people) object in Watts (for rate) or Joules. This is determined by the current sensible heat gain from people to the zone and the "Fraction Radiant" specified in input. Note that the radiant and convective gains should add up to the sensible heat gain from people. The convective heat gain from people is added to the zone air heat balance directly.

#### People Latent Gain Rate [W]

#### People Latent Gain Energy [J]

These output variables are the amount of latent heat gain for this [People](#people) object in Watts (for rate) or Joules. This amount is based on the number of people in the space as well as the total amount of energy produced by a typical person defined by the activity schedule in the input. An internal algorithm is used to determine what fraction of the total is sensible and what fraction is latent. Details about this split are included in the Engineering Reference document.

#### People Sensible Heating Rate [W]

#### People Sensible Heating Energy [J]

These output variables are the amount of sensible heat gain for this [People](#people) object in Watts (for rate) or Joules. This amount is based on the number of people in the space as well as the total amount of energy produced by a typical person defined by the activity schedule in the input. An internal algorithm (described in the Engineering Reference document) is used to determine what fraction of the total is sensible and what fraction is latent. The sensible plus the latent heat gain from people equals the total gain specified in the input.

#### People Total Heating Rate [W]

#### People Total Heating Energy [J]

These output variables are the total amount of heat gain for this [People](#people) object in Watts (for rate) or Joules. This is derived from the activity level times the number of occupants.

#### People Air Temperature [C]

This output variable represents the zone air temperature based on the Fanger thermal comfort model.  If there is a [ZoneControl:Thermostat:ThermalComfort](#zonecontrolthermostatthermalcomfort) object specified and the thermal zone is occupied, then the value of "[People](#people) Air Temperature" is determined based on the thermal comfort hat satisfies the thermal comfort setpoint PMV value specified; othwesie, it is set to average zone air temperature.

#### People Air Relative Humidity [%]

This output variable represents the zone air relative humidity based on the Fanger thermal comfort model.  If there is a [ZoneControl:Thermostat:ThermalComfort](#zonecontrolthermostatthermalcomfort) object specified and the thermal zone is occupied, then the value of "[People](#people) Air Relative Humidity" is determined from the mean zone air temperature and zone air humidity ratio that satisfies the thermal comfort setpoint PMV value specified; othwesie, it is calculated from the zone air temperature and humidity ratio averaged over the time step.

#### Zone People Occupant Count []

This field is the total number of people within the zone during the timestep in question.

#### Zone People Radiant Heating Rate [W]

#### Zone People Radiant Heating Energy [J]

These output variables are the amount of radiant heat gain from people within the zone in Watts (for rate) or Joules. This is determined by the current sensible heat gain from people to the zone and the "Fraction Radiant" specified in the input. The radiant gains from people are distributed to the surfaces using an area weighting scheme.

#### Zone People Convective Heating Rate [W]

#### Zone People Convective Heating Energy [J]

These output variables are the amount of convective heat gain from people within the zone in Watts (for rate) or Joules. This is determined by the current sensible heat gain from people to the zone and the "Fraction Radiant" specified in input. Note that the radiant and convective gains should add up to the sensible heat gain from people. The convective heat gain from people is added to the zone air heat balance directly.

#### Zone People Latent Gain Rate [W]

#### Zone People Latent Gain Energy [J]

These output variables are the amount of latent heat gain from people within the zone in Watts (for rate) or Joules. This amount is based on the number of people in the space as well as the total amount of energy produced by a typical person defined by the activity schedule in the input. An internal algorithm is used to determine what fraction of the total is sensible and what fraction is latent. Details about this split are included in the Engineering Reference document.

#### Zone People Sensible Heating Rate [W]

#### Zone People Sensible Heating Energy [J]

These output variables are the amount of sensible heat gain from people within the zone in Watts (for rate) or Joules. This amount is based on the number of people in the space as well as the total amount of energy produced by a typical person defined by the activity schedule in the input. An internal algorithm (described in the Engineering Reference document) is used to determine what fraction of the total is sensible and what fraction is latent. The sensible plus the latent heat gain from people equals the total gain specified in the input.

#### Zone People Total Heating Rate [W]

#### Zone People Total Heating Energy [J]

These output variables are the total amount of heat gain from people within the zone in Watts (for rate) or Joules. Derived from the activity level times the number of occupants, this is summed for each people object within a zone.

#### Zone Thermal Comfort Mean Radiant Temperature [C]

This output variable is the mean radiant temperature used in the thermal comfort calculations.  This value is computed according to the "MRT Calculation Type" specified in the PEOPLE object.  If a high temperature radiant system is present in the zone, this value will be adjusted according to the current heater operation and the "Fraction of radiant energy incident on people" specified in the HIGH TEMP RADIANT SYSTEM object.

#### Zone Thermal Comfort Operative Temperature [C]

This output variable is the operative temperature as defined by the thermal comfort operations. Specifically, it is the average of the thermal comfort mean radiant temperature and the zone air temperature.

> Note for all Thermal Comfort reporting:  Though the published values for thermal comfort "vote" have a discrete scale (e.g. –3 to +3 or –4 to +4), the calculations in EnergyPlus are carried out on a continuous scale and, thus, reporting may be "off the scale" with specific conditions encountered in the space. This is not necessarily an error in EnergyPlus – rather a different approach that does not take the "limits" of the discrete scale values into account.

#### Zone Thermal Comfort Fanger Model PMV []

This field is the "predicted mean vote" (PMV) calculated using the Fanger thermal comfort model.

#### Zone Thermal Comfort Fanger Model PPD [%]

This field is the "predicted percentage of dissatisfied" (PPD) calculated using the Fanger thermal comfort model.

#### Zone Thermal Comfort Clothing Surface Temperature [C]

This output variable is the calculation of the clothing surface temperature using the Fanger thermal comfort model.

#### Zone Thermal Comfort Pierce Model Effective Temperature PMV []

This field is the "predicted mean vote" (PMV) calculated using the effective temperature and the Pierce two-node thermal comfort model.

#### Zone Thermal Comfort Pierce Model Standard Effective Temperature PMV []

This field is the "predicted mean vote" (PMV) calculated using the "standard" effective temperature and the Pierce two-node thermal comfort model.

#### Zone Thermal Comfort Pierce Model Discomfort Index []

This field is the "discomfort index" calculated using the the Pierce two-node thermal comfort model.

#### Zone Thermal Comfort Pierce Model Thermal Sensation Index []

This field is the "thermal sensation index" (PMV) calculated using the Pierce two-node thermal comfort model.

#### Zone Thermal Comfort KSU Model Thermal Sensation Vote []

This field is the "thermal sensation vote" (TSV) calculated using the KSU two-node thermal comfort model.

#### Zone Thermal Comfort ASHRAE 55 Adaptive Model 90% Acceptability Status []

This field is to report whether the operative temperature falls into the 90% acceptability limits of the adaptive comfort in ASHRAE 55-2010. A value of 1 means within (inclusive) the limits, a value of 0 means outside the limits, and a value of -1 means not applicable.

#### Zone Thermal Comfort ASHRAE 55 Adaptive Model 80% Acceptability Status [ ]

This field is to report whether the operative temperature falls into the 80% acceptability limits of the adaptive comfort in ASHRAE 55-2010. A value of 1 means within (inclusive) the limits, a value of 0 means outside the limits, and a value of -1 means not applicable.

#### Zone Thermal Comfort ASHRAE 55 Adaptive Model Running Average Outdoor Air Temperature [C]

This field reports the mean monthly outdoor air temperature, an input parameter for the ASHRAE-55 adaptive comfort model. This can be computed in two ways. If the .stat file is provided for the simulation, this field will reflect the monthly daily average temperature.

If the .epw file is used, the field reports the simple running average of the daily average outdoor dry-bulb temperatures of the previous 30 days.

#### Zone Thermal Comfort ASHRAE 55 Adaptive Model Temperature [C]

This field reports the ideal indoor operative temperature, or comfort temperature, as determined by the ASHRAE-55 adaptive comfort model. The 80% acceptability limits for indoor operative temperature are defined as no greater than 2.5 degrees C from the adaptive comfort temperature. The 90% acceptability limits are defined as no greater than 3.5 degrees C from the adaptive comfort temperature.

#### Zone Thermal Comfort CEN 15251 Adaptive Model Category I Status

This field is to report whether the operative temperature falls into the Category I (90% acceptability) limits of the adaptive comfort in the European Standard EN15251-2007. A value of 1 means within (inclusive) the limits, a value of 0 means outside the limits, and a value of -1 means not applicable.

#### Zone Thermal Comfort CEN 15251 Adaptive Model Category II Status

This field is to report whether the operative temperature falls into the Category II (80% acceptability) limits of the adaptive comfort in the European Standard EN15251-2007. A value of 1 means within (inclusive) the limits, a value of 0 means outside the limits, and a value of -1 means not applicable.

#### Zone Thermal Comfort CEN 15251 Adaptive Model Category III Status

This field is to report whether the operative temperature falls into the Category III (65% acceptability) limits of the adaptive comfort in the European Standard EN15251-2007. A value of 1 means within (inclusive) the limits, a value of 0 means outside the limits, and a value of -1 means not applicable.

#### Zone Thermal Comfort CEN 15251 Adaptive Model Running Average Outdoor Air Temperature

This field reports the weighted average of the outdoor air temperature of the previous five days, an input parameter for the CEN-15251 adaptive comfort model.

#### Zone Thermal Comfort CEN 15251 Adaptive Model Temperature

This field reports the ideal indoor operative temperature, or comfort temperature, as determined by the CEN-15251 adaptive comfort model. Category I, II, and II limits for indoor operative temperature are defined as no greater than 2, 3, and 4 degrees C from this value respectively.

### Outputs

The following output variables are all based on whether the humidity ratio and the operative temperature is within the region shown in ASHRAE Standard 55-2004 in Figure 5.2.1.1. For these outputs the operative temperature is simplified to be the average of the air temperature and the mean radiant temperature. For summer, the 0.5 Clo level is used and, for winter, the 1.0 Clo level is used. The graphs below are based on the following tables which extend the ASHRAE values to zero humidity ratio.

Table: Winter Clothes (1.0 Clo)

Operative Temperature (C)|Humidity Ratio (kgWater/kgDryAir)
-------------------------|---------------------------------
19.6|0.012
23.9|0.012
26.3|0.000
21.7|0.000

![Winter Comfort Range](media/winter-comfort-range.png)


Table: Summer Clothes (0.5 Clo)

Operative Temperature (C)|Humidity Ratio (kgWater/kgDryAir)
-------------------------|---------------------------------
23.6|0.012
26.8|0.012
28.3|0.000
25.1|0.000

![Summer Comfort Range](media/summer-comfort-range.png)


#### Zone Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time[hr]

The time when the zone is occupied that the combination of humidity ratio and operative temperature is not in the ASHRAE 55-2004 summer clothes region (see above)

#### Zone Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time[hr]

The time when the zone is occupied that the combination of humidity ratio and operative temperature is not in the ASHRAE 55-2004 winter clothes region (see above)

#### Zone Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time[hr]

The time when the zone is occupied that the combination of humidity ratio and operative temperature is not in the ASHRAE 55-2004 summer or winter clothes region (see above)

#### Facility Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time[hr]

The time when any zone is occupied that the combination of humidity ratio and operative temperature is not in the ASHRAE 55-2004 summer clothes region (see above)

#### Facility Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time [hr]

The time when any zone is occupied that the combination of humidity ratio and operative temperature is not in the ASHRAE 55-2004 winter clothes region (see above)

#### Facility Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time [hr]

The time when any zone is occupied that the combination of humidity ratio and operative temperature is not in the ASHRAE 55-2004 summer or winter clothes region (see above)

## Simplified ASHRAE 55 Warnings

The simplified ASHRAE 55 calculations may be computed for occupied zones and, possibly, warnings are shown on the .err file at the end of each simulated environment. To enable this option set the "Enable ASHRAE 55 comfort warnings" field of the [People](#people) object to Yes. These warnings will not be generated by default.

If you enable the warnings, the simplified ASHRAE 55 calculations are done for occupied zones and, possibly, warnings are shown on the .err file at the end of each simulated environment.

~~~~~~~~~~~~~~~~~~~~

       ** Warning ** More than 4% of time (350.4 hours) uncomfortable in zone ZSF1
       **   ~~~   ** 553.0 hours were uncomfortable based on ASHRAE 55-2004 graph (Section 5.2.1.1)
       **   ~~~   ** During Environment [10/01 - 09/30]: CHICAGO IL USA TMY2-94846 WMO#=725300
       ** Warning ** More than 4% of time (350.4 hours) uncomfortable in zone ZNF1
       **   ~~~   ** 827.8 hours were uncomfortable based on ASHRAE 55-2004 graph (Section 5.2.1.1)
       **   ~~~   ** During Environment [10/01 - 09/30]: CHICAGO IL USA TMY2-94846 WMO#=725300
       ** Warning ** More than 4% of time (350.4 hours) uncomfortable in zone ZSF2
       **   ~~~   ** 593.5 hours were uncomfortable based on ASHRAE 55-2004 graph (Section 5.2.1.1)
       **   ~~~   ** During Environment [10/01 - 09/30]: CHICAGO IL USA TMY2-94846 WMO#=725300
       ** Warning ** More than 4% of time (350.4 hours) uncomfortable in zone ZNF2
       **   ~~~   ** 875.8 hours were uncomfortable based on ASHRAE 55-2004 graph (Section 5.2.1.1)
       **   ~~~   ** During Environment [10/01 - 09/30]: CHICAGO IL USA TMY2-94846 WMO#=725300
~~~~~~~~~~~~~~~~~~~~

You may decide if you need to change parameters to reduce these "uncomfortable" hours. The individual output variables shown previously may help you get more details on when these are occurring.

Following are some suggestions that might be applicable:

- Eliminate occupancy when conditioning equipment is off.
- Note that the ASHRAE graph lower limit is (19.6C to 21.7C) – heating setpoints may need to be nearer 22.2C (72F) than 21.1C (70F).
- Unoccupied heating setpoint should be nearer 16.7C (62F) rather than 12.8C (55F) to reduce the start up recovery.
- Start the occupied setpoint schedule, fan availability schedule, cooling pump availability schedule, reheat coil availability, one hour before occupancy. Seasonal turn on and off of equipment may cause more warnings (but potentially more energy consumption).
- Unoccupied cooling setpoint should be nearer 29.4C (85F) rather than 40.0 (104F) to reduce the start up recovery.

## ComfortViewFactorAngles

When requesting EnergyPlus to do a thermal comfort calculation, the program user has three options for defining how the mean radiant temperature will be calculated. The user may select "zoneaveraged" which results in a mean radiant temperature that is characteristic of an "average" location near the center of the zone. The user may also elect to place the person near a particular surface by selecting "surfaceweighted" in the [People](#people) statement. This takes the average of the zone mean radiant temperature and the temperature of the surface that the person is near and uses this value as the mean radiant temperature when calculating thermal comfort.

The third option is for the user to more explicitly position the person within the space by defining the angle factors from the person to the various surfaces in the zone. This option requires the user to list the surfaces that the person can see from a radiation standpoint and also define the angle (or view) factor for each surface. The AngleFactorList input line is intended to give the user this opportunity.

### Inputs

#### Field: Name

This field is an unique user assigned name for the list of surfaces that can be seen radiantly by the person for whom thermal comfort is to be evaluated. Any reference to this list by a [People](#people) statement will use this name.

#### Field: Zone Name

[Zone](#zone) Name for this surface list. Each of the surfaces listed must be in this zone.

#### Field: Surface <#> Name

This field is the name of a surface in the zone seen by the person.

#### Field: Angle Factor <#>

This field is the fraction that this surface contributes to the total mean radiant temperature. This can be thought of as a weighting factor for this surface and the actual mean radiant temperature used in the thermal comfort model is simply the sum of all angle factors multiplied by the corresponding inside surface temperature. Note that the Surface Name/Angle Factor pair can be repeated up to 20 times. The sum of all angle factors within any angle factor list must equal unity, otherwise the program will not accept the input as valid.

An example IDF with an electric low temperature radiant system is shown below.

~~~~~~~~~~~~~~~~~~~~

    ComfortViewFactorAngles,
        South Zone Angle Factors, !- name of angle factor list
        Sourth Zone,              !- Zone Name
        Zn001:Flr001,             !- Surface name 1
        0.75,                     !- Angle factor for surface 1
        Zn001:Wall001,            !- Surface name 2
        0.15,                     !- Angle factor for surface 2
        Zn001:Roof001,            !- Surface name 3
        0.10;                     !- Angle factor for surface 3
~~~~~~~~~~~~~~~~~~~~

## Lights

The [Lights](#lights) statement allows you to specify information about a zone's electric lighting system, including design power level and operation schedule, and how the heat from lights is distributed thermally.

A zone may have multiple [Lights](#lights) statements. For example, one statement may describe the general lighting in the zone and another the task lighting. Or you can use multiple [Lights](#lights) statements for a zone that has two or more general lighting systems that differ in design level, schedule, etc.

### Inputs

#### Field: Name

The name of the [Lights](#lights) object.

#### Field: Zone or ZoneList Name

The field is the name of the thermal zone (ref: [Zone](#zone)) or [ZoneList](#zonelist) (ref: ZoneLIst) and links this [Lights](#lights) statement to a thermal zone or set of thermal zones in the buidling. When the [ZoneList](#zonelist) option is used then this lights definition is applied to each of the zones in the zone list effecting a global definition for the amount of light wattage in the zone. The Zonelist option can be used effectively with the watts/area and watts/person options of the Design Level Calculation Method.

The name of the actual lights object becomes <[Zone](#zone) Name> <[Lights](#lights) Object Name> and should be less than the standard length (100 characters) for a name field. If it is greater than this standard length, it may be difficult to specify in output reporting as it will be truncated. A warning will be shown if the generated name is greater than 100 characters. If it duplicates another such concatenated name, there will be a severe error and terminate the run.

#### Field: Schedule Name

The name of the schedule that modifies the lighting power design level (see Design Level Calculation Method field and related subsequent fields). The schedule values can be any positive number. The electrical input for lighting in a particular timestep is the product of the design level and the value of this schedule in that timestep. If the design level is the maximum lighting power input the schedule should contain values between 0.0 and 1.0.

#### Field: Design Level Calculation Method

This field is a key/choice field that tells which of the next three fields are filled and is descriptive of the method for calculating the nominal lighting level in the [Zone](#zone). The key/choices are:

- LightingLevel
- With this choice, the method used will be a straight insertion of the lighting level (Watts) for the [Zone](#zone).  (The Lighting Level field should be filled.)
- Watts/Area
- With this choice, the method used will be a factor per floor area of the zone. (The Watts per [Zone](#zone) Floor Area field should be filled).
- Watts/Person
- With this choice, the method used will be a factor of lighting level (watts) per person. (The Watts per person field should be filled).

#### Field: Lighting Level

This is typically the maximum electrical power input (in Watts) to lighting in a zone, including ballasts, if present. This value is multiplied by a schedule fraction (see previous field) to get the lighting power in a particular timestep. In EnergyPlus, this is slightly more flexible in that the lighting design level could be a "diversity factor" applied to a schedule of real numbers.

#### Field: Watts per Zone Floor Area

This factor (watts/m^2^) is used, along with the [Zone](#zone) Floor Area to determine the maximum lighting level as described in the Lighting Level field. The choice from the method field should be "watts/area".

#### Field: Watts per Person

This factor (watts/person) is used, along with the number of occupants (people) to determine the maximum lighting level as described in the Lighting Level field. The choice from the method field should be "watts/person".

#### Heat Gains from Lights:

The electrical input to lighting ultimately appears as heat that contributes to zone loads or to return air heat gains. In EnergyPlus this heat is divided into four different fractions. Three of these are given by the input fields Return Air Fraction, Fraction Radiant and Fraction Visible. A fourth, defined as the fraction of the heat from lights convected to the zone air, is calculated by the program as:

f~convected~ = 1.0 – (Return Air Fraction + Fraction Radiant + Fraction Visible)

You will get an error message if Return Air Fraction + Fraction Radiant + Fraction Visible exceeds 1.0.

These fractions depend on the type of lamp and luminaire, whether the luminaire is vented to the return air, etc.

#### Field: Return Air Fraction

The fraction of the heat from lights that goes into the zone return air (i.e., into the zone outlet node). If the return air flow is zero or the zone has no return air system, the program will put this fraction into the zone air. Return Air Fraction should be non-zero only for luminaires that are return-air ducted  (see Table 14 and Figure 51). (However, see the field "Return Air Fraction Is Calculated from Plenum Temperature," below, for an approach to modeling the case where Return Air Fraction is caused by *conduction* between a luminaire that is in contact with a return-air plenum.)

#### Field: Fraction Radiant

The fraction of heat from lights that goes into the zone as long-wave (thermal) radiation. The program calculates how much of this radiation is absorbed by the inside surfaces of the zone according the area times thermal absorptance product of these surfaces.

#### Field: Fraction Visible

The fraction of heat from lights that goes into the zone as visible (short-wave) radiation. The program calculates how much of this radiation is absorbed by the inside surfaces of the zone according the area times solar absorptance product of these surfaces.

Approximate values of Return Air Fraction, Fraction Radiant and Fraction Visible are given in Table 14 for overhead fluorescent lighting for the luminaire configurations shown in Figure 51.

Table: Approximate values of Return Air Fraction, Fraction Radiant and Fraction Visible for overhead fluorescent lighting for different luminaire configurations. These values assume that no light heat goes into an adjacent zone. Source: *Lighting Handbook: Reference & Application*, 8^th^ Edition, Illuminating Engineering Society of North America, New York, 1993, p. 355.

**Field Name**|**Luminaire Configuration, Fluorescent Lighting**
---------------------------|--------------------------------------------------------------
|Suspended  |Surfacemount|Recessed|Luminousand louveredceiling|Return-airducted
Return Air Fraction|0.0|0.0|0.0|0.0|0.54
Fraction Radiant|0.42|0.72|0.37|0.37|0.18
Fraction Visible|0.18|0.18|0.18|0.18|0.18
*f~convected~*|*0.40*|*0.10*|*0.45*|*0.45*|*0.10*

![Overhead fluorescent luminaire configurations.](media/overhead-fluorescent-luminaire.png)


#### Field: Fraction Replaceable

This field defines the daylighting control for the LIGHTS object.

If **Daylighting:Controls** or **Daylighting:DELight:Controls** is specified for the zone, this field is used as an on/off flag for dimming controls. If set to 0.0, the lights are not dimmed by the daylighting controls. If set to 1.0, the lights are allowed to be dimmed by the daylighting controls.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Task [Lights](#lights)", "Hall [Lights](#lights)", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the lights will be assigned to the "General" end-use subcategory.

#### Field: Return Air Fraction Calculated from Plenum Temperature

Accepts values Yes or No (the default). Yes is for advanced used only. In this case the program will calculate the return air fraction by assuming that it is due to conduction of some of the light heat into the zone's return air plenum and that the amount of the conduction depends on the plenum air temperature. A Yes value should only be used for luminaires that are recessed and non-vented, as shown in Figure 52.

The value you enter for the Return Air Fraction field will be ignored and you can enter, for fluorescent lighting, Fraction Radiant = 0.37 and Fraction Visible = 0.18, as indicated in Table 14.

This feature requires that the coefficients described below be determined from measurements or detailed calculations since they are very sensitive to the luminaire type, lamp type, thermal resistance between fixture and plenum, etc.

If "Return Air Fraction Is Calculated from Plenum Temperature" = Yes, the return air fraction is calculated *each timestep* from the following empirical correlation:

(Return Air Fraction)~calculated~ = C~1~ – C~2~ x T~plenum~

where T~plenum~ is the previous-time-step value of the return plenum air temperature (C),

and C~1~ and C~2~  are the values of the coefficients entered in the next two fields.

To compensate for the change in the return air fraction relative to its input value, the program modifies Fraction Radiant and f~convected~ by a scale factor such that

(Return Air Fraction)~calculated~ + (Fraction Radiant)~modified~+ (f~convected~)~modified~ + (Fraction Visible)~input~ = 1.0

It is assumed that Fraction Visible is a constant equal to its input value.

#### Field: Return Air Fraction Function of Plenum Temperature Coefficient 1

The coefficient C~1~ in the equation for (Return Air Fraction)~calculated~.

#### Field: Return Air Fraction Function of Plenum Temperature Coefficient 2

The coefficient C~2~  in the equation for (Return Air Fraction)~calculated~. Its units are 1/^O^C.

![Vertical section through a zone and its return air plenum showing recessed lighting (not to scale). The heat from lights is divided into four fractions, three of which—ReturnAirFraction, FractionRadiant and FractionConvected—depend on plenum air temperature.](media/vertical-section-through-a-zone-and-its.png)


An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Lights,
        RIGHT FORK Lights 1,     !- Name
        RIGHT FORK,              !- Zone Name
        Office Lighting,         !- SCHEDULE Name
        LightingLevel,          !- Design Level calculation method
        1039.706,,,                !- Lighting Level {W}
        0.0000000E+00,           !- Return Air Fraction
        0.4000000,               !- Fraction Radiant
        0.2000000,               !- Fraction Visible
        1.0,                     !- Fraction Replaceable
        GeneralLights;           !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

Global [Lights](#lights) Object:

~~~~~~~~~~~~~~~~~~~~

      ZoneList,AllOccupiedZones,SPACE1-1,SPACE2-1,SPACE3-1,SPACE4-1,SPACE5-1;

      Lights,
        AllZones with Lights,       !- Name
        AllOccupiedZones,        !- Zone or ZoneList Name
        LIGHTS-1,                !- Schedule Name
        Watts/Area,              !- Design Level Calculation Method
        ,                        !- Lighting Level {W}
        16,                      !- Watts per Zone Floor Area {W/m2}
        ,                        !- Watts per Person {W/person}
        0.2,                     !- Return Air Fraction
        0.59,                    !- Fraction Radiant
        0.2,                     !- Fraction Visible
        0,                       !- Fraction Replaceable
        GeneralLights;           !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

### Outputs

If daylighting controls are operating in the zone, all of the [Lights](#lights) objects with a Fraction Replaceble  greater than zero will be reduced by a multiplicative factor that accounts for how much the electric lighting is lowered due to daylighting.

[Lights](#lights) objects have output variables for individual objects and for zone totals.

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Lights Electric Power [W]
    Zone,Sum,Lights Radiant Heat Gain [J]
    Zone,Average,Lights Radiant Heating Rate [W]
    Zone,Sum,Lights Visible Radiation Heating Energy [J]
    Zone,Average,Lights Visible Radiation Heating Rate [W]
    Zone,Sum,Lights Convective Heating Energy [J]
    Zone,Average,Lights Convective Heating Rate [W]
    Zone,Sum,Lights Return Air Heating Energy [J]
    Zone,Average,Lights Return Air Heating Rate [W]
    Zone,Sum,Lights Total Heating Energy [J]
    Zone,Average,Lights Total Heating Rate [W]
    Zone,Sum,Lights Electric Energy [J]
    Zone,Average,Zone Lights Electric Power [W]
    Zone,Sum,Zone Lights Radiant Heating Energy [J]
    Zone,Average,Zone Lights Radiant Heating Rate [W]
    Zone,Sum,Zone Lights Visible Radiation Heating Energy [J]
    Zone,Average,Zone Lights Visible Radiation Heating Rate [W]
    Zone,Sum,Zone Lights Convective Heating Energy [J]
    Zone,Average,Zone Lights Convective Heating Rate [W]
    Zone,Sum,Zone Lights Return Air Heating Energy [J]
    Zone,Average,Zone Lights Return Air Heating Rate [W]
    Zone,Sum,Zone Lights Total Heating Energy [J]
    Zone,Average,Zone Lights Total Heating Rate [W]
    Zone,Sum,Zone Lights Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Lights Electric Power [W]

The electric power input for the lights.

#### Lights Radiant Heating Rate [W]

#### Lights Radiant Heating Energy [J]

The amount of heat gain from lights that is in the form of long-wave (thermal) radiation entering the zone. This heat is absorbed by the inside surfaces of the zone according to an area times long-wave absorptance weighting scheme.

#### Lights Visible Radiation Heating Rate [W]

#### Lights Visible Radiation Heating Energy [J]

The amount of heat gain from lights that is in the form of visible (short-wave) radiation entering the zone. This heat is absorbed by the inside surfaces of the zone according  to an area times short-wave absorptance weighting scheme.

#### Lights Convective Heating Rate [W]

#### Lights Convective Heating Energy [J]

The amount of heat gain from lights that is convected to the zone air.

#### Lights Return Air Heating Rate [W]

#### Lights Return Air Heating Energy [J]

The amount of heat gain from lights that goes into the zone's return air (and, therefore, does not directly contribute to the zone load). If the zone has no return air system or the zone's air system is off, this heat will be added to the zone air.

#### Lights Total Heating Rate [W]

#### Lights Total Heating Energy [J]

The total heat gain from lights. It is the sum of the following four outputs, i.e., Total Heat Gain = Return Air Heat Gain + Radiant Heat Gain + Visible Heat Gain + Convective Heat Gain. It is also equal to the electrical input to the lights.

#### Lights Electric Energy [J]

The lighting electrical consumption including ballasts, if present. These will have the same value as [Lights](#lights) Total Heating Energy (above).

#### Zone Lights Electric Power [W]

The electric power input for all lights in the zone.

#### Zone Lights Radiant Heating Rate [W]

#### Zone Lights Radiant Heating Energy [J]

The amount of heat gain from all lights in the zone that is in the form of long-wave (thermal) radiation entering the zone. This heat is absorbed by the inside surfaces of the zone according to an area times long-wave absorptance weighting scheme.

#### Zone Lights Visible Radiation Heating Rate [W]

#### Zone Lights Visible Radiation Heating Energy [J]

The amount of heat gain from all lights in the zone that is in the form of visible (short-wave) radiation entering the zone. This heat is absorbed by the inside surfaces of the zone according  to an area times short-wave absorptance weighting scheme.

#### Zone Lights Convective Heating Rate [W]

#### Zone Lights Convective Heating Energy [J]

The amount of heat gain from all lights in the zone that is convected to the zone air.

#### Zone Lights Return Air Heating Rate [W]

#### Zone Lights Return Air Heating Energy [J]

The amount of heat gain from all lights in the zone that goes into the zone's return air (and, therefore, does not directly contribute to the zone load). If the zone has no return air system or the zone's air system is off, this heat will be added to the zone air.

#### Zone Lights Total Heating Rate [W]

#### Zone Lights Total Heating Energy [J]

The total heat gain from all lights in the zone. It is the sum of the following four outputs, i.e., Total Heat Gain = Return Air Heat Gain + Radiant Heat Gain + Visible Heat Gain + Convective Heat Gain. It is also equal to the electrical input to the lights.

#### Zone Lights Electric Energy [J]

The lighting electrical consumption for all lights in the zone including ballasts, if present. This will have the same value as [Zone](#zone) [Lights](#lights) Total Heating Energy (above). However, this amount is also shown in the Electricity meters that are associated with the zone:

Electricity:Facility,

Electricity:Building,

Electricity:Zone:<Zone Name>.

In addition, depending on use, it will be shown in:

InteriorLights: Electricity and

InteriorLights:Electricity:Zone :<Zone Name>.

### Outputs

As described in the [Lights](#lights) Outputs, values for lights will show up on the following meters:

Table: Distribution of [Lights](#lights) to Meters

Meter Name|Scope|Lights Specfics
----------|-----|---------------
Electricity:Facility|Entire Facility|All
Electricity:Building|All Zones|All
Electricity:Zone:<Zone Name>|Specific Zone|All
InteriorLights:Electricity|All Zones|Lights Use
InteriorLights:Electricity:Zone:<Zone Name>|Specific Zone|Lights Use
<End-Use Subcategory>:InteriorLights:Electricity|Specific Subcategory|Lights Use

## ElectricEquipment

The object models equipment in the zone which consumes electricity, such as computers, televisions, and cooking equipment, also known as "plug loads." All of the energy consumed by the equipment becomes a heat gain in the zone or is lost (exhausted) as specified below.

### Inputs

#### Field: Name

The name of the [ElectricEquipment](#electricequipment) object.

#### Field: Zone or ZoneList Name

This field is the name of the zone (ref: [Zone](#zone)) or [ZoneList](#zonelist) (ref: [ZoneList](#zonelist)) and attaches a particular electric equipment statement to a thermal zone or set of thermal zones in the building. When the [ZoneList](#zonelist) option is used then this electric equipment definition is applied to each of the zones in the zone list effecting a global definition for the amount of electric wattage in the zone. The Zonelist option can be used effectively with the watts/area and watts/person options of the Design Level Calculation Method.

The name of the actual electric equipment object becomes <[Zone](#zone) Name> <[ElectricEquipment](#electricequipment) Object Name> and should be less than the standard length (100 characters) for a name field. If it is greater than this standard length, it may be difficult to specify in output reporting as it will be truncated. A warning will be shown if the generated name is greater than 100 characters. If it duplicates another such concatenated name, there will be a severe error and terminate the run.

#### Field: Schedule Name

This field is the name of the schedule that modifies the design level parameter for electric equipment (see Design Level Calculation Method field and related subsequent fields). The schedule values can be any positive number. The actual electrical input for equipment in a zone as defined by this statement is the product of the design level field and the value of the schedule specified by name in this field.

#### Field: Design Level Calculation Method

This field is a key/choice field that tells which of the next three fields are filled and is descriptive of the method for calculating the nominal electric equipment level in the [Zone](#zone). The key/choices are:

- EquipmentLevel
- With this choice, the method used will be a straight insertion of the electric equipment level (Watts) for the [Zone](#zone).  (The Design Level field should be filled.)
- Watts/Area
- With this choice, the method used will be a factor per floor area of the zone. (The Watts per [Zone](#zone) Floor Area field should be filled).
- Watts/Person
- With this choice, the method used will be a factor of equipment level (watts) per person. (The Watts per Person field should be filled).

#### Field: Design Level

This field (in Watts) is typically used to represent the maximum electrical input to equipment in a zone that is then multiplied by a schedule fraction (see previous field). In EnergyPlus, this is slightly more flexible in that the electric equipment design level could be a "diversity factor" applied to a schedule of real numbers. Note that while the schedule value can vary from hour to hour, the design level field is constant for all simulation environments.

#### Field: Watts per Zone Floor Area

This factor (watts/m^2^) is used, along with the [Zone](#zone) Area to determine the maximum equipment level as described in the Design Level field. The choice from the method field should be "Watts/Area".

#### Field: Watts per Person

This factor (watts/person) is used, along with the number of occupants (people) to determine the maximum equipment level as described in the Design Level field. The choice from the method field should be "Watts/Person".

#### Heat Gains from Electric Equipment:

The electrical input to the equipment ultimately appears as heat that contributes to zone loads. In EnergyPlus this heat is divided into four different fractions. Three of these are given by the input fields Fraction Latent, Fraction Radiant and Fraction Lost. A fourth, defined as the fraction of the heat from electric equipment convected to the zone air, is calculated by the program as:

f~convected~ = 1.0 – (Fraction Latent + Fraction Radiant + Fraction Lost)

You will get an error message if Fraction Latent + Fraction Radiant + Fraction Lost exceeds 1.0.

#### Field: Fraction Latent

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of latent heat given off by electric equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by electric equipment to give the amount of latent energy produced by the electric equipment. This energy affects the moisture balance within the zone.

#### Field: Fraction Radiant

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of long-wave radiant heat being given off by electric equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by electric equipment to give the amount of long wavelength radiation gain from electric equipment in a zone.

#### Field: Fraction Lost

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of "lost" heat being given off by electric equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by electric equipment to give the amount of heat which is "lost" and does not impact the zone energy balances. This might correspond to electrical energy converted to mechanical work or heat that is vented to the atmosphere.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Computers", "Copy Machines", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the equipment will be assigned to the "General" end-use subcategory.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    ElectricEquipment,
        DORM ROOMS AND COMMON AREAS ElecEq 1,  !- Name
        DORM ROOMS AND COMMON AREAS,  !- Zone Name
        Residence Equipment,     !- SCHEDULE Name
        EquipmentLevel,         !- Design Level calculation method
        9210.921,                !- Design Level {W}
        ,                        !- Watts per Zone Floor Area {watts/m2}
        ,                        !- Watts per Person {watts/person}
        0.0000000E+00,           !- Fraction Latent
        0.3000000,               !- Fraction Radiant
        0.0000000E+00,           !- Fraction Lost
        Computers;               !- End-use Subcategory
~~~~~~~~~~~~~~~~~~~~

Global [ElectricEquipment](#electricequipment) example:

~~~~~~~~~~~~~~~~~~~~

      ZoneList,AllOccupiedZones,SPACE1-1,SPACE2-1,SPACE3-1,SPACE4-1,SPACE5-1;

      ElectricEquipment,
        AllZones with Electric Equipment,       !- Name
        AllOccupiedZones,        !- Zone or ZoneList Name
        EQUIP-1,                 !- Schedule Name
        Watts/Person,            !- Design Level Calculation Method
        ,                        !- Design Level {W}
        ,                        !- Watts per Zone Floor Area {W/m2}
        96,                      !- Watts per Person {W/person}
        0,                       !- Fraction Latent
        0.3,                     !- Fraction Radiant
        0;                       !- Fraction Lost
~~~~~~~~~~~~~~~~~~~~

## GasEquipment

The object models equipment in the zone which consumes natural gas, such as cooking equipment or a gas fireplace. All of the energy consumed by the equipment becomes a heat gain in the zone or is lost (exhausted) as specified below.

### Inputs

#### Field: Name

The name of the [GasEquipment](#gasequipment) object.

#### Field: Zone or ZoneList Name

This field is the name of the thermal zone (ref: [Zone](#zone)) or [ZoneList](#zonelist) (ref: [ZoneList](#zonelist)) and attaches a particular gas equipment statement to a thermal zone or set of thermal zones in the building. When the [ZoneList](#zonelist) option is used then this gas equipment definition is applied to each of the zones in the zone list effecting a global definition for the amount of gas in the zone. The Zonelist option can be used effectively with the watts/area and watts/person options of the Design Level Calculation Method.

The name of the actual gas equipment object becomes <[Zone](#zone) Name> <[GasEquipment](#gasequipment) Object Name> and should be less than the standard length (100 characters) for a name field. If it is greater than this standard length, it may be difficult to specify in output reporting as it will be truncated. A warning will be shown if the generated name is greater than 100 characters. If it duplicates another such concatenated name, there will be a severe error and terminate the run.

#### Field: Schedule Name

This field is the name of the schedule that modifies the design level parameter for gas equipment (see Design Level Calculation Method field and related subsequent fields). The schedule values can be any positive number. The actual energy input for gas equipment in a zone as defined by this statement is the product of the design level field and the value of the schedule specified by name in this field.

#### Field: Design Level Calculation Method

This field is a key/choice field that tells which of the next three fields are filled and is descriptive of the method for calculating the nominal gas equipment level in the [Zone](#zone). The key/choices are:

- EquipmentLevel
- With this choice, the method used will be a straight insertion of the gas equipment level (Watts) for the [Zone](#zone).  (The Design Level field should be filled.)
- Watts/Area or Power/Area
- With this choice, the method used will be a factor per floor area of the zone. (The Power per [Zone](#zone) Floor Area field should be filled).
- Watts/Person or Power/Person
- With this choice, the method used will be a factor of equipment level (watts) per person. (The Power per Person field should be filled).

#### Field: Design Level

This field (in Watts) is typically used to represent the maximum energy input to gas equipment in a zone that is then multiplied by a schedule fraction (see previous field). In EnergyPlus, this is slightly more flexible in that the gas equipment design level could be a "diversity factor" applied to a schedule of real numbers. Note that while the schedule value can vary from hour to hour, the design level field is constant for all simulation environments.

#### Field: Power per Zone Floor Area

This factor (watts/m^2^) is used, along with the [Zone](#zone) Area to determine the maximum equipment level as described in the Design Level field. The choice from the method field should be "**Watts/Area**" or "**Power/Area**".

#### Field: Power per Person

This factor (watts/person) is used, along with the number of occupants (people) to determine the maximum equipment level as described in the Design Level field. The choice from the method field should be "**Watts/Person**" or "**Power/Person**".

#### Heat Gains from Gas Equipment:

The fuel input to the equipment ultimately appears as heat that contributes to zone loads. In EnergyPlus this heat is divided into four different fractions. Three of these are given by the input fields Fraction Latent, Fraction Radiant and Fraction Lost. A fourth, defined as the fraction of the heat from gas equipment convected to the zone air, is calculated by the program as:

f~convected~ = 1.0 – (Fraction Latent + Fraction Radiant + Fraction Lost)

You will get an error message if Fraction Latent + Fraction Radiant + Fraction Lost exceeds 1.0.

#### Field: Fraction Latent

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of latent heat given off by gas equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by gas equipment to give the amount of latent energy produced by the gas equipment. This energy affects the moisture balance within the zone.

#### Field: Fraction Radiant

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of long-wave radiant heat being given off by gas equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by gas equipment to give the amount of long wavelength radiation gain from gas equipment in a zone.

#### Field: Fraction Lost

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of "lost" heat being given off by gas equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by gas equipment to give the amount of heat which is "lost" and does not impact the zone energy balances. This might correspond to input energy converted to mechanical work or heat that is vented to the atmosphere.

#### Field: Carbon Dioxide Generation Rate

This numeric input field specifies carbon dioxide generation rate with units of m3/s-W. The default value of 0.0 assumes the equipment is fully vented to outdoors. In the absence of better information, the user might consider using a value of 3.45E-8 m3/s-W which assumes the equipment is not vented to outdoors. This value is converted from natural gas CO~2~ emission rate at 11.7 lbs CO~2~ per therm. The CO~2~~~emission rate is provided by U.S. Energy Information Administration, "Frequently Asked Questions - Environment, Questions About Environmental Emissions",  http://tonto.eia.doe.gov/ask/environment_faqs.asp#CO2_quantity, January 2010. The maximum value for this input field is 3.45E-7 m3/s-W.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Cooking", "Clothes Drying", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the equipment will be assigned to the "General" end-use subcategory.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    GasEquipment,
        DORM ROOMS AND COMMON AREAS GasEq 1,  !- Name
        DORM ROOMS AND COMMON AREAS,  !- Zone Name
        Gas Eq Sch,              !- Schedule Name
        EquipmentLevel,          !- Design Level Calculation Method
        29287.51,                !- Design Level {W}
        ,                        !- Power per Zone Floor Area {W/m2}
        ,                        !- Power per Person {W/Person}
        0,                       !- Fraction Latent
        0.3,                     !- Fraction Radiant
        0,                       !- Fraction Lost
        0,                       !- Carbon Dioxide Generation Rate {m3/s-W}
        Cooking;                 !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

Global Gas Equipment example:

~~~~~~~~~~~~~~~~~~~~

      ZoneList,OfficeZones,Left Fork, Middle Fork, Right Fork;

      GasEquipment,
        Office Zones with Gas Equipment,       !- Name
        OfficeZones,               !- Zone Name
        Gas Eq Sch,              !- Schedule Name
        Watts/Area,              !- Design Level Calculation Method
        ,                        !- Design Level {W}
        197,                     !- Power per Zone Floor Area {W/m2}
        ,                        !- Power per Person {W/Person}
        0.0000000E+00,           !- Fraction Latent
        0.3000000,               !- Fraction Radiant
        0.0000000E+00;           !- Fraction Lost
~~~~~~~~~~~~~~~~~~~~

## HotWaterEquipment

The object models hot water equipment in the zone which consumes district heating, such as cooking equipment or process loads. All of the energy consumed by the equipment becomes a heat gain in the zone or is lost (exhausted) as specified below. This object consumes district heating energy directly and does not cause a load on a hot water plant loop or water heater. For domestic hot water uses, such as sinks and showers, see [WaterUse:Equipment](#wateruseequipment).

### Inputs

#### Field: Name

The name of the [HotWaterEquipment](#hotwaterequipment) object.

#### Field: Zone or ZoneList Name

This field is the name of the thermal zone (ref: [Zone](#zone)) or [ZoneList](#zonelist) (ref: [ZoneList](#zonelist)) and attaches a particular hot water equipment statement to a thermal zone or set of thermal zones in the building. When the [ZoneList](#zonelist) option is used then this hot water equipment definition is applied to each of the zones in the zone list effecting a global definition for the amount of hot water in the zone. The Zonelist option can be used effectively with the watts/area and watts/person options of the Design Level Calculation Method.

The name of the actual hot water equipment object becomes <[Zone](#zone) Name> <[HotWaterEquipment](#hotwaterequipment) Object Name> and should be less than the standard length (100 characters) for a name field. If it is greater than this standard length, it may be difficult to specify in output reporting as it will be truncated. A warning will be shown if the generated name is greater than 100 characters. If it duplicates another such concatenated name, there will be a severe error and terminate the run.

#### Field: Schedule Name

This field is the name of the schedule that modifies the design level parameter for hot water equipment (see Design Level Calculation Method field and related subsequent fields). The schedule values can be any positive number. The actual energy input for hot water equipment in a zone as defined by this statement is the product of the design level field and the value of the schedule specified by name in this field.

#### Field: Design Level Calculation Method

This field is a key/choice field that tells which of the next three fields are filled and is descriptive of the method for calculating the nominal hot water equipment level in the [Zone](#zone). The key/choices are:

- EquipmentLevel
- With this choice, the method used will be a straight insertion of the hot water equipment level (Watts) for the [Zone](#zone).  (The Design Level field should be filled.)
- Watts/Area or Power/Area
- With this choice, the method used will be a factor per floor area of the zone. (The Power per [Zone](#zone) Floor Area field should be filled).
- Watts/Person or Power/Person
- With this choice, the method used will be a factor of equipment level (watts) per person. (The Power per Person field should be filled).

#### Field: Design Level

This field (in Watts) is typically used to represent the maximum energy input to hot water equipment in a zone that is then multiplied by a schedule fraction (see previous field). In EnergyPlus, this is slightly more flexible in that the hot water equipment design level could be a "diversity factor" applied to a schedule of real numbers. Note that while the schedule value can vary from hour to hour, the design level field is constant for all simulation environments.

#### Field: Power per Zone Floor Area

This factor (watts/m^2^) is used, along with the [Zone](#zone) Area to determine the maximum equipment level as described in the Design Level field. The choice from the method field should be "**Watts/Area**" or "**Power/Area**".

#### Field: Power per Person

This factor (watts/person) is used, along with the number of occupants (people) to determine the maximum equipment level as described in the Design Level field. The choice from the method field should be "**Watts/Person**" or "**Power/Person**".

#### Heat Gains from Hot Water Equipment:

The fuel input to the equipment ultimately appears as heat that contributes to zone loads. In EnergyPlus this heat is divided into four different fractions. Three of these are given by the input fields Fraction Latent, Fraction Radiant and Fraction Lost. A fourth, defined as the fraction of the heat from hot water equipment convected to the zone air, is calculated by the program as:

f~convected~ = 1.0 – (Fraction Latent + Fraction Radiant + Fraction Lost)

You will get an error message if Fraction Latent + Fraction Radiant + Fraction Lost exceeds 1.0.

#### Field: Fraction Latent

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of latent heat given off by hot water equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by hot water equipment to give the amount of latent energy produced by the hot water equipment. This energy affects the moisture balance within the zone.

#### Field: Fraction Radiant

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of long-wave radiant heat being given off by hot water equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by hot water equipment to give the amount of long wavelength radiation gain from hot water equipment in a zone.

#### Field: Fraction Lost

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of "lost" heat being given off by hot water equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by hot water equipment to give the amount of heat which is "lost" and does not impact the zone energy balances. This might correspond to input energy converted to mechanical work or heat that is vented to the atmosphere.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Cooking", "Clothes Drying", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) obejct). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the equipment will be assigned to the "General" end-use subcategory.

IDF Examples:

~~~~~~~~~~~~~~~~~~~~

    HotWaterEquipment,
        SPACE2-1 HWEq 1,         !- Name
        SPACE2-1,                !- Zone Name
        EQUIP-1,                 !- SCHEDULE Name
        EquipmentLevel,         !- Design Level calculation method
        300,                     !- Design Level {W}
        ,                        !- Power per Zone Floor Area {watts/m2}
        ,                        !- Power per Person {watts/person}
        0.2,                     !- Fraction Latent
        0.1,                     !- Fraction Radiant
        0.5,                     !- Fraction Lost
        Dishwashing;             !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

Global Hot Water Equipment example:

~~~~~~~~~~~~~~~~~~~~

      ZoneList,OfficeZones,Left Fork, Middle Fork, Right Fork;

      HotWaterEquipment,
        Office Zones with Hot Water Equipment,       !- Name
        OfficeZones,             !- Zone Name
        HotWater Eq Sch,         !- Schedule Name
        Watts/Area,              !- Design Level Calculation Method
        ,                        !- Design Level {W}
        50,                      !- Power per Zone Floor Area {W/m2}
        ,                        !- Power per Person {W/Person}
        0.0000000E+00,           !- Fraction Latent
        0.3000000,               !- Fraction Radiant
        0.0000000E+00;           !- Fraction Lost
~~~~~~~~~~~~~~~~~~~~

## SteamEquipment

The object models steam equipment in the zone which consumes district heating, such as cooking equipment or process loads. All of the energy consumed by the equipment becomes a heat gain in the zone or is lost (exhausted) as specified below. This object consumes district heating energy directly and does not cause a load on a steam plant loop.

### Inputs

#### Field: Name

The name of the [SteamEquipment](#steamequipment) object.

#### Field: Zone or ZoneList Name

This field is the name of the thermal zone (ref: [Zone](#zone)) and attaches a particular steam equipment statement to a thermal zone or set of thermal zones in the building. When the [ZoneList](#zonelist) option is used then this steam equipment definition is applied to each of the zones in the zone list effecting a global definition for the amount of steam in the zone. This option can be used effectively with the watts/area and watts/person options of the Design Level Calculation Method.

#### Field: Schedule Name

This field is the name of the schedule that modifies the design level parameter for steam equipment (see Design Level Calculation Method field and related subsequent fields). The schedule values can be any positive number. The actual energy input for steam equipment in a zone as defined by this statement is the product of the design level field and the value of the schedule specified by name in this field.

#### Field: Design Level Calculation Method

This field is a key/choice field that tells which of the next three fields are filled and is descriptive of the method for calculating the nominal steam equipment level in the [Zone](#zone). The key/choices are:

- EquipmentLevel
- With this choice, the method used will be a straight insertion of the steam equipment level (Watts) for the [Zone](#zone).  (The Design Level field should be filled.)
- Watts/Area or Power/Area
- With this choice, the method used will be a factor per floor area of the zone. (The Power per [Zone](#zone) Floor Area field should be filled).
- Watts/Person or Power/Person
- With this choice, the method used will be a factor of equipment level (watts) per person. (The Power per Person field should be filled).

#### Field: Design Level

This field (in Watts) is typically used to represent the maximum energy input to steam equipment in a zone that is then multiplied by a schedule fraction (see previous field). In EnergyPlus, this is slightly more flexible in that the steam equipment design level could be a "diversity factor" applied to a schedule of real numbers. Note that while the schedule value can vary from hour to hour, the design level field is constant for all simulation environments.

#### Field: Power per Zone Floor Area

This factor (watts/m^2^) is used, along with the [Zone](#zone) Area to determine the maximum equipment level as described in the Design Level field. The choice from the method field should be "**Watts/Area**" or "**Power/Area**".

#### Field: Power per Person

This factor (watts/person) is used, along with the number of occupants (people) to determine the maximum equipment level as described in the Design Level field. The choice from the method field should be "**Watts/Person**" or "**Power/Person**".

#### Heat Gains from Steam Equipment:

The fuel input to the equipment ultimately appears as heat that contributes to zone loads. In EnergyPlus this heat is divided into four different fractions. Three of these are given by the input fields Fraction Latent, Fraction Radiant and Fraction Lost. A fourth, defined as the fraction of the heat from steam equipment convected to the zone air, is calculated by the program as:

f~convected~ = 1.0 – (Fraction Latent + Fraction Radiant + Fraction Lost)

You will get an error message if Fraction Latent + Fraction Radiant + Fraction Lost exceeds 1.0.

#### Field: Fraction Latent

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of latent heat given off by steam equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by steam equipment to give the amount of latent energy produced by the steam equipment. This energy affects the moisture balance within the zone.

#### Field: Fraction Radiant

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of long-wave radiant heat being given off by steam equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by steam equipment to give the amount of long wavelength radiation gain from steam equipment in a zone.

#### Field: Fraction Lost

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of "lost" heat being given off by steam equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by steam equipment to give the amount of heat which is "lost" and does not impact the zone energy balances. This might correspond to input energy converted to mechanical work or heat that is vented to the atmosphere.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Cooking", "Clothes Drying", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the equipment will be assigned to the "General" end-use subcategory.

IDF Examples:

~~~~~~~~~~~~~~~~~~~~

    SteamEquipment,
        SPACE4-1 ElecEq 1,       !- Name
        SPACE4-1,                !- Zone Name
        EQUIP-1,                 !- SCHEDULE Name
        EquipmentLevel,          !- Design Level calculation method
        1050,                    !- Design Level {W}
        ,                        !- Power per Zone Floor Area {watts/m2}
        ,                        !- Power per Person {watts/person}
        0.5,                     !- Fraction Latent
        0.3,                     !- Fraction Radiant
        0,                       !- Fraction Lost
        Laundry;                 !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

## OtherEquipment

Other Equipment object is provided as an additional source for heat gains or losses directly to the zone.  That is to say, a loss can be entered by putting a negative value into the Design Level field(s). Note, too, that this object does not have an end-use component – gains or losses do not show up in the bottom energy lines (except as influencing overall zone gains or losses).

### Inputs

#### Field: Name

The name of the [OtherEquipment](#otherequipment) object.

#### Field: Zone or ZoneList Name

This field is the name of the thermal zone (ref: [Zone](#zone)) and attaches a particular other equipment statement to a thermal zone or set of thermal zones in the building. When the [ZoneList](#zonelist) option is used then this other equipment definition is applied to each of the zones in the zone list effecting a global definition for the amount of other in the zone. This option can be used effectively with the watts/area and watts/person options of the Design Level Calculation Method.

#### Field: Schedule Name

This field is the name of the schedule that modifies the design level parameter for other equipment (see Design Level Calculation Method field and related subsequent fields). The schedule values can be any positive number. The actual energy input for other equipment in a zone as defined by this statement is the product of the design level field and the value of the schedule specified by name in this field.

#### Field: Design Level Calculation Method

This field is a key/choice field that tells which of the next three fields are filled and is descriptive of the method for calculating the nominal other equipment level in the [Zone](#zone). The key/choices are:

- EquipmentLevel
- With this choice, the method used will be a straight insertion of the other equipment level (Watts) for the [Zone](#zone).  (The Design Level field should be filled.)
- Watts/Area or Power/Area
- With this choice, the method used will be a factor per floor area of the zone. (The Power per [Zone](#zone) Floor Area field should be filled).
- Watts/Person or Power/Person
- With this choice, the method used will be a factor of equipment level (watts) per person. (The Power per Person field should be filled).

#### Field: Design Level

This field (in Watts) is typically used to represent the maximum energy input to other equipment in a zone that is then multiplied by a schedule fraction (see previous field). In EnergyPlus, this is slightly more flexible in that the other equipment design level could be a "diversity factor" applied to a schedule of real numbers. This value can be negative to denote a loss. Note that while the schedule value can vary from hour to hour, the design level field is constant for all simulation environments.

#### Field: Power per Zone Floor Area

This factor (watts/m^2^) is used, along with the [Zone](#zone) Area to determine the maximum equipment level as described in the Design Level field. This value can be negative to denote a loss. The choice from the method field should be "**Watts/Area**" or "**Power/Area**".

#### Field: Power per Person

This factor (watts/person) is used, along with the number of occupants (people) to determine the maximum equipment level as described in the Design Level field. This value can be negative to denote a loss. The choice from the method field should be "**Watts/Person**" or "**Power/Person**".

#### Heat Gains/Losses from Other Equipment:

The fuel input to the equipment ultimately appears as heat that contributes to zone loads. In EnergyPlus this heat is divided into four different fractions. Three of these are given by the input fields Fraction Latent, Fraction Radiant and Fraction Lost. A fourth, defined as the fraction of the heat from other equipment convected to the zone air, is calculated by the program as:

f~convected~ = 1.0 – (Fraction Latent + Fraction Radiant + Fraction Lost)

You will get an error message if Fraction Latent + Fraction Radiant + Fraction Lost exceeds 1.0.

#### Field: Fraction Latent

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of latent heat given off by other equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by other equipment to give the amount of latent energy produced by the other equipment. This energy affects the moisture balance within the zone.

#### Field: Fraction Radiant

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of long-wave radiant heat being given off by other equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by other equipment to give the amount of long wavelength radiation gain from other equipment in a zone.

#### Field: Fraction Lost

This field is a decimal number between 0.0 and 1.0 and is used to characterize the amount of "lost" heat being given off by other equipment in a zone. The number specified in this field will be multiplied by the total energy consumed by other equipment to give the amount of heat which is "lost" and does not impact the zone energy balances. This might correspond to input energy converted to mechanical work or heat that is vented to the atmosphere.

IDF Examples

~~~~~~~~~~~~~~~~~~~~

    OtherEquipment,
        BASE-1 OthEq 1,          !- Name
        BASE-1,                  !- Zone Name
        ALWAYSON,                !- SCHEDULE Name
        EquipmentLevel,          !- Design Level calculation method
        6766.,                   !- Design Level {W}
        ,                        !- Power per Zone Floor Area {watts/m2}
        ,                        !- Power per Person {watts/person}
        0,                       !- Fraction Latent
        0.3,                     !- Fraction Radiant
        0;                       !- Fraction Lost
~~~~~~~~~~~~~~~~~~~~

### Outputs

Each type of equipment object has output variables for individual objects and for zone totals.

~~~~~~~~~~~~~~~~~~~~

    Electric Equipment
    Zone,Average,Electric Equipment Electric Power [W]
    Zone,Sum,Electric Equipment Electric Energy [J]
    Zone,Sum,Electric Equipment Radiant Heating Energy [J]
    Zone,Average,Electric Equipment Radiant Heating Rate [W]
    Zone,Sum,Electric Equipment Convective Heating Energy [J]
    Zone,Average,Electric Equipment Convective Heating Rate [W]
    Zone,Sum,Electric Equipment Latent Gain Energy [J]
    Zone,Average,Electric Equipment Latent Gain Rate [W]
    Zone,Sum,Electric Equipment Lost Heat Energy [J]
    Zone,Average,Electric Equipment Lost Heat Rate [W]
    Zone,Sum,Electric Equipment Total Heating Energy [J]
    Zone,Average,Electric Equipment Total Heating Rate [W]
    Zone,Average,Zone Electric Equipment Electric Power [W]
    Zone,Sum,Zone Electric Equipment Electric Energy [J]
    Zone,Sum,Zone Electric Equipment Radiant Heating Energy [J]
    Zone,Average,Zone Electric Equipment Radiant Heating Rate [W]
    Zone,Sum,Zone Electric Equipment Convective Heating Energy [J]
    Zone,Average,Zone Electric Equipment Convective Heating Rate [W]
    Zone,Sum,Zone Electric Equipment Latent Gain Energy [J]
    Zone,Average,Zone Electric Equipment Latent Gain Rate [W]
    Zone,Sum,Zone Electric Equipment Lost Heat Energy [J]
    Zone,Average,Zone Electric Equipment Lost Heat Rate [W]
    Zone,Sum,Zone Electric Equipment Total Heating Energy [J]
    Zone,Average,Zone Electric Equipment Total Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Gas Equipment
    Zone,Average,Gas Equipment Gas Rate [W]
    Zone,Sum,Gas Equipment Gas Energy [J]
    Zone,Sum,Gas Equipment Radiant Heating Energy [J]
    Zone,Sum,Gas Equipment Convective Heating Energy [J]
    Zone,Sum,Gas Equipment Latent Gain Energy [J]
    Zone,Sum,Gas Equipment Lost Heat Energy [J]
    Zone,Sum,Gas Equipment Total Heating Energy [J]
    Zone,Average,Gas Equipment Radiant Heating Rate [W]
    Zone,Average,Gas Equipment Convective Heating Rate [W]
    Zone,Average,Gas Equipment Latent Gain Rate [W]
    Zone,Average,Gas Equipment Lost Heat Rate [W]
    Zone,Average,Gas Equipment Total Heating Rate [W]
    Zone,Average,Zone Gas Equipment Gas Rate [W]
    Zone,Sum,Zone Gas Equipment Gas Energy [J]
    Zone,Sum,Zone Gas Equipment Radiant Heating Energy [J]
    Zone,Average,Zone Gas Equipment Radiant Heating Rate [W]
    Zone,Sum,Zone Gas Equipment Convective Heating Energy [J]
    Zone,Average,Zone Gas Equipment Convective Heating Rate [W]
    Zone,Sum,Zone Gas Equipment Latent Gain Energy [J]
    Zone,Average,Zone Gas Equipment Latent Gain Rate [W]
    Zone,Sum,Zone Gas Equipment Lost Heat Energy [J]
    Zone,Average,Zone Gas Equipment Lost Heat Rate [W]
    Zone,Sum,Zone Gas Equipment Total Heating Energy [J]
    Zone,Average,Zone Gas Equipment Total Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    HotWater Equipment
    Zone,Average,Hot Water Equipment District Heating Rate [W]
    Zone,Sum,Hot Water Equipment District Heating Energy [J]
    Zone,Sum,Hot Water Equipment Radiant Heating Energy [J]
    Zone,Average,Hot Water Equipment Radiant Heating Rate [W]
    Zone,Sum,Hot Water Equipment Convective Heating Energy [J]
    Zone,Average,Hot Water Equipment Convective Heating Rate [W]
    Zone,Sum,Hot Water Equipment Latent Gain Energy [J]
    Zone,Average,Hot Water Equipment Latent Gain Rate [W]
    Zone,Sum,Hot Water Equipment Lost Heat Energy [J]
    Zone,Average,Hot Water Equipment Lost Heat Rate [W]
    Zone,Sum,Hot Water Equipment Total Heating Energy [J]
    Zone,Average,Hot Water Equipment Total Heating Rate [W]
    Zone,Average,Zone Hot Water Equipment District Heating Rate [W]
    Zone,Sum,Zone Hot Water Equipment District Heating Energy [J]
    Zone,Sum,Zone Hot Water Equipment Radiant Heating Energy [J]
    Zone,Average,Zone Hot Water Equipment Radiant Heating Rate [W]
    Zone,Sum,Zone Hot Water Equipment Convective Heating Energy [J]
    Zone,Average,Zone Hot Water Equipment Convective Heating Rate [W]
    Zone,Sum,Zone Hot Water Equipment Latent Gain Energy [J]
    Zone,Average,Zone Hot Water Equipment Latent Gain Rate [W]
    Zone,Sum,Zone Hot Water Equipment Lost Heat Energy [J]
    Zone,Average,Zone Hot Water Equipment Lost Heat Rate [W]
    Zone,Sum,Zone Hot Water Equipment Total Heating Energy [J]
    Zone,Average,Zone Hot Water Equipment Total Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Steam Equipment
    Zone,Average,Steam Equipment District Heating Rate [W]
    Zone,Sum,Steam Equipment District Heating Energy [J]
    Zone,Sum,Steam Equipment Radiant Heating Energy [J]
    Zone,Average,Steam Equipment Radiant Heating Rate [W]
    Zone,Sum,Steam Equipment Convective Heating Energy [J]
    Zone,Average,Steam Equipment Convective Heating Rate [W]
    Zone,Sum,Steam Equipment Latent Gain Energy [J]
    Zone,Average,Steam Equipment Latent Gain Rate [W]
    Zone,Sum,Steam Equipment Lost Heat Energy [J]
    Zone,Average,Steam Equipment Lost Heat Rate [W]
    Zone,Sum,Steam Equipment Total Heating Energy [J]
    Zone,Average,Steam Equipment Total Heating Rate [W]
    Zone,Average,Zone Steam Equipment District Heating Rate [W]
    Zone,Sum,Zone Steam Equipment District Heating Energy [J]
    Zone,Sum,Zone Steam Equipment Radiant Heating Energy [J]
    Zone,Average,Zone Steam Equipment Radiant Heating Rate [W]
    Zone,Sum,Zone Steam Equipment Convective Heating Energy [J]
    Zone,Average,Zone Steam Equipment Convective Heating Rate [W]
    Zone,Sum,Zone Steam Equipment Latent Gain Energy [J]
    Zone,Average,Zone Steam Equipment Latent Gain Rate [W]
    Zone,Sum,Zone Steam Equipment Lost Heat Energy [J]
    Zone,Average,Zone Steam Equipment Lost Heat Rate [W]
    Zone,Sum,Zone Steam Equipment Total Heating Energy [J]
    Zone,Average,Zone Steam Equipment Total Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Other Equipment
    Zone,Sum,Other Equipment Radiant Heating Energy [J]
    Zone,Average,Other Equipment Radiant Heating Rate [W]
    Zone,Sum,Other Equipment Convective Heating Energy [J]
    Zone,Average,Other Equipment Convective Heating Rate [W]
    Zone,Sum,Other Equipment Latent Gain Energy [J]
    Zone,Average,Other Equipment Latent Gain Rate [W]
    Zone,Sum,Other Equipment Lost Heat Energy [J]
    Zone,Average,Other Equipment Lost Heat Rate [W]
    Zone,Sum,Other Equipment Total Heating Energy [J]
    Zone,Average,Other Equipment Total Heating Rate [W]
    Zone,Sum,Zone Other Equipment Radiant Heating Energy [J]
    Zone,Average,Zone Other Equipment Radiant Heating Rate [W]
    Zone,Sum,Zone Other Equipment Convective Heating Energy [J]
    Zone,Average,Zone Other Equipment Convective Heating Rate [W]
    Zone,Sum,Zone Other Equipment Latent Gain Energy [J]
    Zone,Average,Zone Other Equipment Latent Gain Rate [W]
    Zone,Sum,Zone Other Equipment Lost Heat Energy [J]
    Zone,Average,Zone Other Equipment Lost Heat Rate [W]
    Zone,Sum,Zone Other Equipment Total Heating Energy [J]
    Zone,Average,Zone Other Equipment Total Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Electric Equipment Electric Power [W]

#### Electric Equipment Electric Energy [J]

The electric equipment electric power consumption in Watts (for power) or Joules (for energy). It is the sum of the radiant, convective, latent and lost components. This energy use is added to the electricity meters that are associated with the zone – Electricity:Facility, Electricity:Buidling, Electricity:Zone:<Zone Name>, InteriorEquipment:Electricity: :Zone:<Zone Name>, and <End-Use Subcategory>:InteriorEquipment:Electricity.

#### Gas Equipment Gas Rate [W]

#### Gas Equipment Gas Energy [J]

The gas equipment natural gas consumption in Watts (for power) or Joules (for energy). It is the sum of the radiant, convective, latent and lost components. This energy use is added to the electricity meters that are associated with the zone – Gas:Facility, Gas:Buidling, Gas:Zone:<Zone Name>, InteriorEquipment:Gas: :Zone:<Zone Name>, and <End-Use Subcategory>:InteriorEquipment:Gas.

#### Hot Water Equipment District Heating Rate [W]

#### Hot Water Equipment District Heating Energy [J]

The hot water equipment district heating consumption in Watts (for power) or Joules (for energy). It is the sum of the radiant, convective, latent and lost components. This energy use is added to the district heating meters that are associated with the zone – DistrictHeating:Facility, DistrictHeating:Buidling, DistrictHeating:Zone:<Zone Name>, InteriorEquipment: DistrictHeating: :Zone:<Zone Name>, and <End-Use Subcategory>:InteriorEquipment: [DistrictHeating](#districtheating).

#### Steam Equipment District Heating Rate [W]

#### Steam Equipment District Heating Energy [J]

The steam equipment district heating consumption in Watts (for power) or Joules (for energy). It is the sum of the radiant, convective, latent and lost components. This energy use is added to the district heating meters that are associated with the zone – DistrictHeating:Facility, DistrictHeating:Buidling, DistrictHeating:Zone:<Zone Name>, InteriorEquipment: DistrictHeating: :Zone:<Zone Name>, and <End-Use Subcategory>:InteriorEquipment: [DistrictHeating](#districtheating).

> Note that zone energy consumption is not reported for OTHER EQUIPMENT and does not go on any meter.

## ZoneContaminantSourceAndSink:CarbonDioxide

The [ZoneContaminantSourceAndSink:CarbonDioxide](#zonecontaminantsourceandsinkcarbondioxide) object allows users to input carbon dioxide sources or sinks in a zone. Note that carbon dioxide generation within a zone can also be specified using [People](#people) and [GasEquipment](#gasequipment) objects. Multiple [ZoneContaminantSourceAndSink:CarbonDioxide](#zonecontaminantsourceandsinkcarbondioxide)  objects can be specified for the same zone.

### Inputs

#### Field: Name

The name of the [ZoneContaminantSourceAndSink:CarbonDioxide](#zonecontaminantsourceandsinkcarbondioxide) object. The name for each [ZoneContaminantSourceAndSink:CarbonDioxide](#zonecontaminantsourceandsinkcarbondioxide) object must be unique.

#### Field: Zone Name

This field is the name of the zone (ref: **Zone**) and links a particular [ZoneContaminantSourceAndSink:CarbonDioxide](#zonecontaminantsourceandsinkcarbondioxide) object to a thermal zone in the building.

#### Field: Design Generation Rate

This field denotes the design carbon dioxide generation rate (m^3^/s). The design value is modified by the schedule fraction (see Field: Schedule Name). The resulting volumetric generation rate is converted to mass generation rate using the current zone indoor air density at each time step. The rate can be either positive or negative. A positive value represents a source rate (CO~2~ addition to the zone air) and a negative value represents a sink rate (CO~2~ removal from the zone air).

#### Field: Schedule Name

This field is the name of the schedule (ref: Schedules) that modifies the design carbon dioxide generation rate (see previous field). The schedule values can be any positive number between 0.0 and 1.0. For each simulation time step, the actual CO~2~~~generation rate in a zone is the product of the Design Generation Rate field (above) and the value specified by this schedule.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    ZoneContaminantSourceAndSink:CarbonDioxide,
        NORTH_ZONE CO2,        !- Name
        NORTH_ZONE,            !- Zone Name
        1.e-6,                 !- Design Generation Rate {m3/s}
    CO2 Source Schedule;   !- Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Contaminant Source or Sink CO2 Gain Volume Flow Rate [m3/s]
    HVAC,Average, Zone Contaminant Source or Sink CO2 Gain Volume Flow Rate [m3/s]
~~~~~~~~~~~~~~~~~~~~

#### Contaminant Source or Sink CO2 Gain Volume Flow Rate [m3/s]

This output is the net carbond dioxide internal gain/loss in m3/s for an individual [ZoneContaminantSourceAndSink:CarbonDioxide](#zonecontaminantsourceandsinkcarbondioxide) object.

#### Zone Contaminant Source or Sink CO2 Gain Volume Flow Rate [m3/s]

This output variable reports the total (net) carbon dioxide internal gains/losses in cubic meters per second for a zone, including impacts from three objects: [ZoneContaminantSourceAndSink:CarbonDioxide](#zonecontaminantsourceandsinkcarbondioxide), [People](#people), and [GasEquipment](#gasequipment). Positive values denote carbon dioxide generation (gain or source), while negative values denote carbon dioxide removal (loss or sink).

## ZoneContaminantSourceAndSink:GenericContaminant:Constant 

The ZoneContaminantSourceAndSink:GenericContaminant:Constant object specifies the generic contaminant generation rate and removal rate coefficient in a zone. The associated fraction schedules are required for allowing users to change the magnitude of sources and sinks. The object is equivalent to the combination of the constant coefficient model and the burst source model defined in the sources and sinks element types of CONTAM 3.0. The basic equation used to calculate generic contaminant source and sink for the constant model is given below:

![](media/image87.png)\


where

*S~f~*~~= Contaminant generic contaminant source strength [m^3^/s]

*G~f~*~~= Generic contaminant generation rate [m^3^/s]

*F~G~*~~= Fraction value from the source fraction schedule at a given time [dimensionless]

*R~f~*~~= Generic contaminant effective removal rate [m^3^/s]

*F~R~*~~= Fraction value from the sink fraction schedule at a given time [dimensionless]

*C~f~*~~= Generic contaminant concentration value at a given previous time step [ppm]

### Field: Name

This field represents a unique identifying name in a group referring Generic contaminantSourceAndSinkNames.

### Field: Zone Name

This field signifies the Name of the zone with constant generic contaminant source and sink.

### Field: Maximum Generation Rate

This field denotes the full generic contaminant maximum generation rate (m^3^/s). The design generation rate is the maximum amount of generic contaminant expected at design conditions. The design value is modified by the schedule fraction (see Field:Generation Schedule Name).

### Field: Generation Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design generation rate (G~f~). This fraction between 0.0 and 1.0 is noted as F~G~ in the above equation.

### Field: Design Removal Coefficient

This field denotes the full generic contaminant design removal coefficient (m^3^/s). The design removal rate is the maximum amount of generic contaminant expected at design conditions times the generic contaminant concentration in the same zone. The design value is modified by the schedule fraction (see Field:Removal Schedule Name).

### Field: Removal Schedule Name

**This field is the name of the schedule (ref: Schedule) that modifies the maximum design generation rate (R**~f~). This fraction between 0.0 and 1.0 is noted as F~R~ in the above equation.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

      ZoneContaminantSourceAndSink:GenericContaminant:Constant,
        NORTH ZONE GC,          !- Name
        NORTH ZONE,             !- Zone Name
        1.0E-6,                 !- Design Generation Rate {m3/s}
        GC Source Schedule,     !- Schedule Name
        1.0E-7,                 !- Design Removal Coefficient {m3/s}
        GC Removal Schedule;    !- Removal Schedule Name
~~~~~~~~~~~~~~~~~~~~

## ZoneContaminantSourceAndSink:GenericContaminant:Constant Outputs

When a ZoneContaminantSourceAndSink:GenericContaminant:Constant object is specified, the following output variables are available:

~~~~~~~~~~~~~~~~~~~~

    ZONE,Average, Generic Air Contaminant Constant Source Generation Volume Flow Rate [m3/s]
~~~~~~~~~~~~~~~~~~~~

### Generic Air Contaminant Constant Source Generation Volume Flow Rate [m3/s]

This output is the average generic contaminant generation rate from each ZoneContaminantSourceAndSink:GenericContaminant:Constant object. The generation rate is a sum of generation and removal rates. The zone air generic contaminant level at the previous zone time step is used in the removal rate calculation.

## SurfaceContaminantSourceAndSink:GenericContaminant:PressureDriven 

The SurfaceContaminantSourceAndSink:GenericContaminant:PressureDriven object specifies the generic contaminant generation rate coefficient, which is used to calculate the generation rate due to the pressure difference across the surface. The object is equivalent to the pressure driven model defined in the sources and sinks element types of CONTAM 3.0. This object assumes to work with the AirflowNetwork model. The surface has to be defined in the AirflowNetwork:Multizone:Surface. Although the model is designed to be applied to radon and soil gas entry, it is expanded to be applied to all contaminant transport, including generic contaminant. However, it should be used in caution. The basic equation used to calculate generic contaminant source for the pressure driven constant model is provided below:

![](media/image88.png)\


where

*S~f~*~~= Generic contaminant source strength [m^3^/s]

*H~f~*~~= Generic contaminant generation rate coefficient [m^3^/s]

*F~G~*~~= Fraction value from the source fraction schedule at a given time [dimensionless]

*n=* Flow power exponent

P*~i~*~~= [Zone](#zone) pressure [Pa]

P*~j~*~~= Pressure in an adjacent zone for a interior surface or outdoor for an exterior surface [Pa]

### Field: Name

The field signifies the unique identifying name in a group referring GenericContaminantSourceAndSinkNames.

### Field: Surface Name

This field represents the name of the surface as a generic contaminant source using the pressure driven model.

### Field: Design Generation Rate Coefficient

This field denotes the generic contaminant design generation coefficient (m^3^/s). The design generation rate is the maximum amount of generic contaminant expected at design conditions times the pressure difference with a power exponent across a surface. The design value is modified by the schedule fraction (see Field:Generation Schedule Name).

### Field: Generation Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design generation rate. This fraction between 0.0 and 1.0 is noted as F~G~ in the above equation.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

      SurfaceContaminantSourceAndSink:GenericContaminant:PressureDriven,
        WEST ZONE GC BLD,       !- Name
        Zn001:Wall001,          !- Surface Name
        1.0E-6,                 !- Design Generation Rate Coefficient {m3/s}
        GC Source Schedule,     !- Generation Schedule Name
        0.5;                    !- Generation Exponent
~~~~~~~~~~~~~~~~~~~~

## SurfaceContaminantSourceAndSink:GenericContaminant:PressureDriven Outputs

When a SurfaceContaminantSourceAndSink:GenericContaminant:PressureDriven object is specified, the following output variables are available:

~~~~~~~~~~~~~~~~~~~~

    ZONE,Average,Generic Air Contaminant Pressure Driven Generation Volume Flow Rate [m3/s]
~~~~~~~~~~~~~~~~~~~~

### Generic Air Contaminant Pressure Driven Generation Volume Flow Rate [m3/s]

This output is the average generic contaminant generation rate from each SurfaceContaminantSourceAndSink:GenericContaminant:PressureDriven object.

## ZoneContaminantSourceAndSink:GenericContaminant:CutoffModel 

The ZoneContaminantSourceAndSink:Generic contaminant:CutoffModel object specifies the generic contaminant generation rate based on the cutoff concentration model. The basic equation used to calculate generic contaminant source for the pressure driven constant model is given below:

![](media/image89.png)\


where

*S~f~*~~= Generic contaminant source strength [m^3^/s]

*G~f~*~~= Generic contaminant generation rate [m^3^/s]

*F~G~*~~= Fraction value from the source fraction schedule at a given time [dimensionless]

*C~cutoff~* = Cutoff concentration at which emission ceases [ppm]

*C~f~*~~= Generic contaminant concentration value at a given previous time step [ppm]

### Field: Name

The field signifies the unique identifying name in a group referring GenericContaminantSourceAndSinkNames.

### Field: Zone Name

This field represents the name of the zone with generic contaminant source and sink using the cutoff model.

### Field: Design Generation Rate

This field denotes the full generic contaminant design generation rate (m^3^/s). The design generation rate is the maximum amount of generic contaminant expected at design conditions. The design value is modified by the schedule fraction (see Field:Generation Schedule Name).

### Field: Generation Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design generation rate (G~f~). This fraction between 0.0 and 1.0 is noted as F~G~ in the above equation.

### Field: Cutoff Concentration

This field is the generic contaminant cutoff concentration level where the source ceases its emission.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    ZoneContaminantSourceAndSink:GenericContaminant:CutoffModel,
        NORTH ZONE GC CutoffModel,          !- Name
        NORTH ZONE,             !- Zone Name
        1.0E-5,                 !- Design Generation Rate {m3/s}
        GC Source Schedule,     !- Schedule Name
        100000;                 !- Cutoff Concentration {ppm}
~~~~~~~~~~~~~~~~~~~~

## ZoneContaminantSourceAndSink:GenericContaminant:CutoffModel Outputs

When a ZoneContaminantSourceAndSink:GenericContaminant:CutoffModel object is specified, the following output variables are available:

~~~~~~~~~~~~~~~~~~~~

    ZONE,Average, Generic Air Contaminant Cutoff Model Generation Volume Flow Rate [m3/s]
~~~~~~~~~~~~~~~~~~~~

### Generic Air Contaminant Cutoff Model Generation Volume Flow Rate [m3/s]

This output is the average generic contaminant generation rate from each SurfaceContaminantSourceAndSink:GenericContaminant: CutoffModel object.

## ZoneContaminantSourceAndSink:GenericContaminant:DecaySource 

The ZoneContaminantSourceAndSink:GenericContaminant:DecaySource object specifies the generic contaminant generation rate based on the decay source model. The basic equation used to calculate generic contaminant source for the decay source model is given below:

![](media/image90.png)\


where

*S~f~*~~= Generic contaminant source strength [m^3^/s]

*G~f~*~~= Initial generic contaminant generation rate [m^3^/s]

*F~G~*~~= Fraction value from the source fraction schedule at a given time [dimensionless]

t = Time since the start of emission [second]

*t~c~* = Decay time constant [second]

### Field: Name

This field is the unique identifying name in a group referring GenericContaminantSourceAndSinkNames.

### Field: Zone Name

This field represents the name of the zone with a generic contaminant decaying source.

### Field: Initial Generation Rate

This field denotes the initial generic contaminant design generation rate (m^3^/s). The generation is controlled by a schedule, as defined in the next field. Generic contaminant generation begins when the schedule changes from a zero to a non-zero value (between zero and one). The initial generation rate is equal to the schedule value times the initial generation rate. A single schedule may be used to initiate several emissions at different times. (see Field:Generation Schedule Name).

### Field: Generation Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design generation rate (G~f~). This fraction between 0.0 and 1.0 is noted as F~G~ in the above equation. When the value is equal to 1, the generation rate is used and time is reset to zero. When the value is equal to zero, the schedule value is ignored in the equation.

### Field: Decay Time Constant

This field is the time at which the generation rate reaches 0.37 of the original rate.

> Note: The variable t, time since the start of emission, will be reset to zero, when a new run period starts, or the generation schedule value is equal to zero.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    ZoneContaminantSourceAndSink:GenericContaminant:DecaySource,
        WEST ZONE GC DecaySource,           !- Name
        WEST ZONE,              !- Zone Name
        1.0E-5,                 !- Initial Emission Rate {m3/s}
        GC Source Schedule,     !- Schedule Name
        100000;                  !- Delay Time Constant {s}
~~~~~~~~~~~~~~~~~~~~

## ZoneContaminantSourceAndSink:GenericContaminant:DecaySource Outputs

When a ZoneContaminantSourceAndSink:GenericContaminant:DecaySource object is specified, the following output variables are available:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Generic Air Contaminant Decay Model Generation Volume Flow Rate [m3/s]
    Zone,Average,Generic Air Contaminant Decay Model Generation Emission Start Elapsed Time [s]
~~~~~~~~~~~~~~~~~~~~

### Generic Air Contaminant Decay Model Generation Volume Flow Rate [m3/s]

This output is the average generic contaminant decay rate from each SurfaceContaminantSourceAndSink:GenericContaminant:DecaySource object.

### Generic Air Contaminant Decay Model Generation Emission Start Elapsed Time [s]

This output is the decay time since the start of emission. The start time is either at the beginning of each run period, including design day simulations, or the time when the egenration schedule value is zero.

## SurfaceContaminantSourceAndSink:GenericContaminant:BoudaryLayerDiffusion 

The SurfaceContaminantSourceAndSink:GenericContaminant:BoudaryLayerDiffusion object specifies the generic contaminant generation rate from surface diffusion. The object is equivalent to the boundary layer diffusion model driven model defined in the sources and sinks element types of CONTAM 3.0.

The boundary layer diffusion controlled reversible sink/source model with a linear sorption isotherm follows the descriptions presented in [Axley 1991]. The boundary layer refers to the region above the surface of a material through which a concentration gradient exists between the near-surface concentration and the air-phase concentration. The rate at which a contaminant is transferred onto a surface (sink) is defined as:

![](media/image91.png)\


where

*h* = Average film mass transfer coefficient over the sink [m/s]

**  = Film density of air [kg/m^3^]

*A*  = Surface area of the adsorbent [m^2^]

*C~f~* = Concentration in the air at the previous time step [ppm]

*C~s~* = Concentration in the adsorbent [ppm]

*k*  = Henry adsorption constant or partition coefficient [dimensionless]

### Field: Name

This field signifies a unique identifying name in a group referring GenericContaminanteSourceAndSinkNames.

### Field: Surface Name

This field denotes the name of the surface as a generic contaminant reversible source or sink using the boundary layer diffusion model.

### Field: Film Transfer Coefficient

This field specifies the average mass transfer coefficient of the contaminant generic contaminant within the boundary layer (or film) above the surface of the adsorbent.

### Field: Henry Partition Coefficient 

This field denotes the generic contaminant Henry partition coefficient in the units of dimensionless. The coefficient relates the concentration of the contaminant generic contaminant in the bulk-air to that at the surface of the adsorption material.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    SurfaceContaminantSourceAndSink:GenericContaminant:BoudaryLayerDiffusion,
        WEST ZONE GC BLD,       !- Name
        Zn001:Wall001,          !- Surface Name
        1.0E-2,                 !- Mass Transfer Coefficient {m/s}
        GC Source Schedule,     !- Schedule Name
        2.0;                    !- Henry adsorption constant or partition coefficient
~~~~~~~~~~~~~~~~~~~~

## SurfaceContaminantSourceAndSink:GenericContaminant:BoundaryLayerDiffusion Outputs

When a SurfaceContaminantSourceAndSink:GenericContaminant:BoudaryLayerDiffusion object is specified, the following output variables are available:

~~~~~~~~~~~~~~~~~~~~

    ZONE,Average, Generic Air Contaminant Boundary Layer Diffusion Generation Volume Flow Rate [m3/s]
    ZONE,Average, Generic Air Contaminant Boundary Layer Diffusion Inside Face Concentration [ppm]
~~~~~~~~~~~~~~~~~~~~

### Generic Air Contaminant Boundary Layer Diffusion Generation Volume Flow Rate [m3/s]

This output is the average generic contaminant generation rate from each SurfaceContaminantSourceAndSink:GenericContaminant:BoudaryLayerDiffusion object.

### Generic Air Contaminant Boundary Layer Diffusion Inside Face Concentration [ppm]

This output is the average generic contaminant level at the interior surface.

## SurfaceContaminantSourceAndSink:GenericContaminant:DepositionVelocitySink 

The SurfaceContaminantSourceAndSink:GenericContaminant:DepositionVelocitySink object specifies the generic contaminant removal rate from a surface. The object is equivalent to the deposition velocity sink model defined in CONTAM 3.0 sources and sinks element types.

The deposition velocity model provides for the input of a sink's characteristic in the familiar term of deposition velocity. The removal stops when the sink concentration level is higher than the zone air concentration level. The deposition velocity model equation is:

![](media/image92.png)\


where

S~f~(*t*) = Removal rate at time t [m^3^/s]

*~d~*~~ = Deposition velocity [m/s]

*A~s~* = Deposition surface area [m^2^]

*m* = Element multiplier [dimensionless]

*C*~f~(*t*) = Concentration of contaminant generic contaminant at the previous time step [ppm]

F~R~ = Schedule or control signal value at time *t* [-]

### Field: Name

This field denotes a unique identifying name in a group referring GenericContaminant SourceAndSinkNames.

### Field: Surface Name

This field represents the name of the surface as a generic contaminant sink using the deposition velocity sink model.

### Field: Deposition Velocity

This field specifies the deposition velocity to the surface of the adsorbent in the units of m/s.

### Field: Removal Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design removal rate (S~f~). This fraction between 0.0 and 1.0 is noted as F~R~ in the above equation.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    SurfaceContaminantSourceAndSink:GenericContaminant:DepositionVelocitySink,
        EAST ZONE GC DVS,       !- Name
        Zn002:Wall001,          !- Surface Name
        1.0E-3,                 !- Deposition Velocity {m/s}
        GC Source Schedule;     !- Schedule Name
~~~~~~~~~~~~~~~~~~~~

## SurfaceContaminantSourceAndSink:GenericContaminant:DepositionVelocitySink Outputs

The following output variables are available when the SurfaceContaminantSourceAndSink:GenericContaminant:DepositionVelocitySink

object is specified.

~~~~~~~~~~~~~~~~~~~~

    ZONE,Average, Generic Air Contaminant Deposition Velocity Removal Volume Flow Rate [m3/s]
~~~~~~~~~~~~~~~~~~~~

### Generic Air Contaminant Deposition Velocity Removal Volume Flow Rate [m3/s]

This output is the average generic contaminant generation rate from each SurfaceContaminantSourceAndSink:GenericContaminant:DepositionVelocitySink object.

## ZoneContaminantSourceAndSink:GenericContaminant:DepositionRateSink 

The ZoneContaminantSourceAndSink:GenericContaminant:DepositionRateSink object specifies the generic contaminant removal rate from a zone. The object is equivalent to the deposition rate sink model defined in CONTAM 3.0 sources and sinks element types.

The deposition rate model provides for the input of a sink's characteristic in the familiar term of deposition rate in a zone. The removal stops when the sink concentration level is higher than the zone air concentration level. The deposition rate model equation is:

![](media/image93.png)\


Where

S~f~(*t*) = Removal rate at time t [m^3^/s]

*k~d~* = Deposition rate [1/T]

*V~z~* = [Zone](#zone) volume [m^3^]

*m* = Element multiplier [dimensionless]

*C*~f~(*t*) = Concentration of generic contaminant at the previous time *step* [ppm]

F~R~ = Schedule or control signal value at time *t* [dimensionless]

### Field: Name

This field denotes a unique identifying name in a group referring GenericContaminantSourceAndSinkNames.

### Field: Zone Name

This field represents the name of the zone as a generic contaminant sink using the deposition rate sink model.

### Field: Deposition Rate

This field specifies the deposition rate to the zone in the units of 1/s.

### Field: Removal Schedule Name

This field is the name of the schedule (ref: Schedule) that modifies the maximum design removal rate (S~f~). This fraction between 0.0 and 1.0 is noted as F~R~ in the above equation.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    ZoneContaminantSourceAndSink:GenericContaminant:DepositionRateSink,
        NORTH ZONE GC DRS,      !- Name
        NORTH ZONE,             !- Zone Name
        1.0E-5,                 !- Deposition Rate {m3/s}
        GC Source Schedule;     !- Schedule Name
~~~~~~~~~~~~~~~~~~~~

## ZoneContaminantSourceAndSink:GenericContaminant:DepositionRateSink Outputs

When the ZoneContaminantSourceAndSink:GenericContaminant:DepositionRateSink object is specified, the following output variables are available:

~~~~~~~~~~~~~~~~~~~~

    ZONE,Average, Generic Air Contaminant Deposition Rate Removal Volume Flow Rate [m3/s]
~~~~~~~~~~~~~~~~~~~~

### Generic Air Contaminant Deposition Rate Removal Volume Flow Rate [m3/s]

This output is the average generic contaminant generation rate from each ZoneContaminantSourceAndSink:GenericContaminant:DepositionRateSink object.

## ZoneBaseboard:OutdoorTemperatureControlled

This object specifies outside temperature-controlled (OTC) baseboard heating. The capacities (high and low) are specified in W at the temperatures specified. The schedule allows both capacities to change hourly on a proportional basis. This baseboard heater does not operate if the outdoor dry-bulb is above the high temperature limit. Between the high temperature and the low temperature, the capacity is interpolated (linear) between the high and the low capacity values. Below the low temperature, the capacity is set at the low capacity value. This allows the user to add baseboard heat to a perimeter zone starting at a prescribed temperature and then slowly increases this capacity to a max value.

**Example:**

Temperature High = 10 CCapacity High = 100,000 W

Temperature Low = 0 CCapacity Low  = 500,000 W

Table: Outdoor Temperature Controlled Baseboard Heat Temperature vs. Capacity

Outdoor Dry-bulb(C)|Baseboard Output
-------------------|----------------
>10|0
10|100,000
8|180,000
6|260,000
4|340,000
2|420,000
0|500,000
< 0|500,000
|

### Inputs

#### Field: Name

The name of the [ZoneBaseboard:OutdoorTemperatureControlled](#zonebaseboardoutdoortemperaturecontrolled) object.

#### Field: Zone Name

This field is the name of the zone and attaches the baseboard heat equipment statement to a thermal zone in the building.

#### Field: Schedule Name

This field is the name of the schedule that modifies the capacities (high and low) for baseboard heat equipment (see next four fields). The schedule values can be any positive number. The actual energy input for the baseboard equipment in a zone as defined by this statement depends on the actual outdoor temperature and where that temperature is in the range of Low Temperature to High Temperature..

#### Field: Capacity at Low Temperature

This is the baseboard equipment capacity (Watts) at the low temperature limit. This is the maximum capacity of the baseboard equipment in full load operation. This field is autosizable.

#### Field: Low Temperature

If the outdoor dry-bulb temperature (degrees Celsius) is at or below the low temperature the baseboard heater operates at the low temperature capacity. This field is autosizable. The lowest design outdoor dry bulb temperature is chosen, if autosized.

#### Field: Capacity at High Temperature

This is the baseboard equipment capacity (Watts) at the high temperature limit. This field is autosizable. The capacity at low temperature is prorated against the reference low and high temperature fields, if autosized.

#### Field: High Temperature

If the outdoor dry-bulb temperature (degrees Celsius) is greater than the high temperature the baseboard heater will not operate. This field is autosizable. If autosized, this is equal to the design zone heating setpoint temperature described below, so that the capacity at high temperature is zero.

#### Field: Fraction Radiant

This field is a decimal number between 0.0 and 1.0 and is used to characterize the type of heat being given off by baseboard heat equipment in a zone.  The number specified in this field will be multiplied by the total energy consumed by the baseboard heat equipment to give the amount of long wavelength radiation gain to the zone.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Perimeter Baseboards", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the baseboard equipment will be assigned to the "General" end-use subcategory.

#### Field: Design Zone Heating Setpoint

This is heating setpoint temperature in the zone where the unit serves. This is used to autosize high temperature and capacity at high temperature fields. The default value is 20°C.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    ZoneBaseboard:OutdoorTemperatureControlled,
        SPACE4-1 BBHeat 1,       !- Name
        SPACE4-1,                !- Zone Name
        EQUIP-1,                 !- SCHEDULE Name
        1500,                    !- Capacity at low temperature {W}
        0,                       !- Low Temperature {C}
        500,                     !- Capacity at high temperature {W}
        10,                      !- High Temperature {C}
        0.5,                     !- Fraction Radiant
        Baseboard Heat,          !- End-Use Subcategory
        20;                      !- Design Zone Heating Setpoint {C}
~~~~~~~~~~~~~~~~~~~~

### Outputs

[ZoneBaseboard:OutdoorTemperatureControlled](#zonebaseboardoutdoortemperaturecontrolled) objects have output variables for individual objects and for zone totals. The following outputs are available:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Baseboard Electric Power [W]
    Zone,Sum,Baseboard Electric Energy [J]
    Zone,Sum,Baseboard Radiant Heating Energy [J]
    Zone,Average,Baseboard Radiant Heating Rate [W]
    Zone,Sum,Baseboard Convective Heating Energy [J]
    Zone,Average,Baseboard Convective Heating Rate [W]
    Zone,Sum,Baseboard Total Heating Energy [J]
    Zone,Average,Baseboard Total Heating Rate [W]
    Zone,Average,Zone Baseboard Electric Power [W]
    Zone,Sum,Zone Baseboard Electric Energy [J]
    Zone,Sum,Zone Baseboard Radiant Heating Energy [J]
    Zone,Average,Zone Baseboard Radiant Heating Rate [W]
    Zone,Sum,Zone Baseboard Convective Heating Energy [J]
    Zone,Average,Zone Baseboard Convective Heating Rate [W]
    Zone,Sum,Zone Baseboard Total Heating Energy [J]
    Zone,Average,Zone Baseboard Total Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Baseboard Electric Power [W]

This field is the electric power for the [ZoneBaseboard:OutdoorTemperatureControlled](#zonebaseboardoutdoortemperaturecontrolled) object in Watts.

#### Baseboard Electric Energy [J]

The outdoor temperature controlled baseboard heat option is assumed to be fueled by electricity. This field is the same as the Baseboard Total Heating Energy (above) in joules.

#### Baseboard Radiant Heating Rate [W]

#### Baseboard Radiant Heating Energy [J]

These output variables are the amount of radiant heat gain for the [ZoneBaseboard:OutdoorTemperatureControlled](#zonebaseboardoutdoortemperaturecontrolled) object in Watts (for rate) or Joules. This is determined by the current heat gain from the heater to the zone and the "Fraction Radiant" specified in the input. The radiant gains (long wavelength) are distributed to the surfaces using an area weighting scheme.

#### Baseboard Convective Heating Rate [W]

#### Baseboard Convective Heating Energy [J]

These output variables are the amount of convective heat gain for the [ZoneBaseboard:OutdoorTemperatureControlled](#zonebaseboardoutdoortemperaturecontrolled) object in Watts (for rate) or Joules. This is determined by the current heat gain from the heater to the zone and the "Fraction Radiant" specified in input (1-FractionRadiant = FractionConvected). The convective heat gain is added to the zone air heat balance directly.

#### Baseboard Total Heating Rate [W]

#### Baseboard Total Heating Energy [J]

These output variables are the amount of heat gain for the [ZoneBaseboard:OutdoorTemperatureControlled](#zonebaseboardoutdoortemperaturecontrolled) object in Watts (for rate) or Joules. This is determined by the sum of the radiant and convective heat gains from the baseboard heat.

#### Zone Baseboard Electric Power [W]

This field is the electric power for all [ZoneBaseboard:OutdoorTemperatureControlled](#zonebaseboardoutdoortemperaturecontrolled) objects within the zone in Watts.

#### Zone Baseboard Electric Energy [J]

The outdoor temperature controlled baseboard heat option is assumed to be fueled by electricity. This field is the same as the Baseboard Total Heating Energy (above) in joules. However, this amount is also shown in the Electricity meters that are associated with the zone – Electricity:Facility, Electricity:Buidling, Electricity:Zone:<Zone Name>, and InteriorEquipment:Electricity.

#### Zone Baseboard Radiant Heating Rate [W]

#### Zone Baseboard Radiant Heating Energy [J]

These output variables are the amount of radiant heat gain for all [ZoneBaseboard:OutdoorTemperatureControlled](#zonebaseboardoutdoortemperaturecontrolled) objects within the zone in Watts (for rate) or Joules. This is determined by the current heat gain from the heater to the zone and the "Fraction Radiant" specified in the input. The radiant gains (long wavelength) are distributed to the surfaces using an area weighting scheme.

#### Zone Baseboard Convective Heating Rate [W]

#### Zone Baseboard Convective Heating Energy [J]

These output variables are the amount of convective heat gain for all [ZoneBaseboard:OutdoorTemperatureControlled](#zonebaseboardoutdoortemperaturecontrolled) objects within the zone in Watts (for rate) or Joules. This is determined by the current heat gain from the heater to the zone and the "Fraction Radiant" specified in input (1-FractionRadiant = FractionConvected). The convective heat gain is added to the zone air heat balance directly.

#### Zone Baseboard Total Heating Rate [W]

#### Zone Baseboard Total Heating Energy [J]

These output variables are the amount of heat gain for all [ZoneBaseboard:OutdoorTemperatureControlled](#zonebaseboardoutdoortemperaturecontrolled) objects within the zone in Watts (for rate) or Joules. This is determined by the sum of the radiant and convective heat gains from the baseboard heat.

### Outputs

As described in each of the equipment sections, values for specific equipments will show up on the following meters:

Table: Distribution of Equipment to Meters

Meter Name|Scope|Equipment Specfics
----------|-----|------------------
Electricity:Facility|Entire Facility|Electric Equipment, OTC Baseboard Heat
Electricity:Building|All Zones|Electric Equipment, OTC Baseboard Heat
Electricity:Zone:<Zone Name>|Specific Zone|Electric Equipment, OTC Baseboard Heat
InteriorEquipment:Electricity|AllZones|Electric Equipment, OTC Baseboard Heat
<End-Use Subcategory>:InteriorEquipment:Electricity|Specific Subcategory|Electric Equipment, OTC Baseboard Heat
Gas:Facility|Entire Facility|Gas Equipment
Gas:Building|All Zones|Gas Equipment
Gas:Zone:<Zone Name>|Specific Zone|Gas Equipment
InteriorEquipment:Gas|AllZones|Gas Equipment
<End-Use Subcategory>:InteriorEquipment:Gas|Specific Subcategory|Gas Equipment