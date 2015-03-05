# Zone Outdoor Air Design Data

Outdoor air design data may be required for many aspects of a building computer model. Sizing of HVAC equipment, infiltration and ventilation, and specific outdoor air requirements for different zone types are a few examples where required outdoor air quantities may vary. Since there would be a significant chance for data input errors if each individual aspect of the simulation model allowed for independent input of outdoor air design data, this general object is used to define outdoor air design data and this data set may be used throughout the building simulation model.

The design data is provided as a group of inputs that are recognized by designers as standard practice. This information may be used individually or used as a group to calculate summations or maximums of the entered data. These design data include values for:

Outdoor air per person

Outdoor air per zone floor area

Outdoor air per zone

Outdoor air changes per hour

This design data is entered in an outdoor air design data object and may be referenced by other objects during the simulation. A single specification for outdoor air design data may be used by all other appropriate objects within EnergyPlus, or multiple outdoor air design data objects may be specified and these design data objects may be used as necessary by other objects when outdoor air design quantaties vary for any reason.

## Design Outdoor Air Calculation

The outdoor air design data is entered as a group and referenced through name association to this group of design data (Ref. DesignSpecification:OutdoorAir). The reference name in the following example is "ZoneOAData".

A DesignSpecification:OutdoorAirexample:

~~~~~~~~~~~~~~~~~~~~

    DesignSpecification:OutdoorAir,
        ZoneOAData,            !- Name
        Maximum,               !- Outdoor Air Method
        0.00944,               !- Outdoor Air Flow per Person {m3/s}
        0.00305;               !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}
        ,                      !- Outdoor Air Flow per Zone
        ,                      !- Outdoor Air Flow Air Changes per Hour
        Min OARequirements Sched; !- Outdoor Air Flow Rate Fraction Schedule Name
~~~~~~~~~~~~~~~~~~~~

Given this set of data, the quantity of outdoor air is calculated based on the Outdoor Air Method specified in each outdoor air design data object. In this example, the maximum of the per person and per zone floor area is used to establish the outdoor air quantity.

As previously mentioned, this group of outdoor air design data is reference by other objects used in the simulation. The reference is by the *name* of the outdoor air design data object. Any reference to this name by other objects simply means that the object referencing this data set will use the values specified in this common object. Note that a zone name is not included in the list of data and the calculation of occupancy, zone floor area, or zone volume is implied through this named reference and *the connection to a zone via the referencing object*. For example, if a terminal unit references an outdoor air design data object, the zone served by that terminal unit is used to determine the occupancy, zone floor area, and zone volume in the following calculations.

![](media/image2197.png)\


![](media/image2198.png)\


![](media/image2199.png)\


![](media/image2200.png)\


where:

![](media/image2201.png) = outdoor air volume flow rate based on occupancy, [m^3^/s]

![](media/image2202.png) = number of occupants in zone, [people]

![](media/image2203.png) = outdoor air volume flow rate per person, [m^3^/s-person]

![](media/image2204.png) = outdoor air volume flow rate based on zone floor area, [m^3^/s]

![](media/image2205.png) = zone floor area, [m^2^]

![](media/image2206.png) = outdoor air volume flow rate per zone floor area, [m^3^/s-m^2^]

![](media/image2207.png) = zone outdoor air volume flow rate, [m^3^/s]

![](media/image2208.png) = outdoor air volume flow rate per zone, [m^3^/s]

![](media/image2209.png) = outdoor air volume flow rate based on air changes per hour, [m^3^/s]

![](media/image2210.png) = zone volume, [m^3^]

![](media/image2211.png) = outdoor air volume flow in air changes per hour, [m^3^/s-m^3^]

Given the calculations for each specific type of design data, the method used to calculate the outdoor air design data is then based on a user selected method for this specific outdoor air design data object. The outdoor air methods used to calculate the outdoor air quantity and the associated value for outdoor air volume flow rate are shown here.

Flow/Person => ![](media/image2212.png)

Flow/Area => ![](media/image2213.png)

Flow/Zone => ![](media/image2214.png)

AirChanges/Hour => ![](media/image2215.png)

Sum => ![](media/image2216.png)

Maximum => ![](media/image2217.png)

If an Outdoor Air Flow Rate Fraction Schedule Name is specified, the flow rate determined above will be multiplied by the current schedule value.

Examples of objects that reference the outdoor air design data object are:

- AirTerminal:SingleDuct:VAV:NoReheat
- AirTerminal:SingleDuct:VAV:Reheat

## References

ASHRAE Fundamentals 2001. 2001 ASHRAE Fundamentals Handbook. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.

Pedersen, C.O., D.E. Fisher, and R.J. Liesen. 1997. A heat balance based cooling load calculation procedure. ASHRAE Transactions, Vol. 103(2), pp. 459-468.

Pedersen, C.O. 2001. Toolkit for Building Load Calculations. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.