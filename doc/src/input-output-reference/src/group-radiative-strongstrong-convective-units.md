# Group – Radiative **/** Convective Units

This section describes the radiative/convective zone equipment units.  The following units are included in this section:

- ZoneHVAC:Baseboard:Convective:Electric
- ZoneHVAC:Baseboard:RadiantConvective:Water
- ZoneHVAC:Baseboard:RadiantConvective:Steam
- ZoneHVAC:Baseboard:RadiantConvective:Electric
- ZoneHVAC:Baseboard:Convective:Water
- ZoneHVAC:HighTemperatureRadiant
- ZoneHVAC:LowTemperatureRadiant:ConstantFlow
- ZoneHVAC:LowTemperatureRadiant:Electric
- ZoneHVAC:LowTemperatureRadiant:VariableFlow

## ZoneHVAC:Baseboard:RadiantConvective:Water

The objective of this model is to calculate the convective and radiant heat transfer from water baseboard heaters to the people and the surfaces within a zone so that surface heat balances can take into account the radiant heat transfer to the surfaces and thus enhance the accuracy of thermal comfort predictions within the space. The radiant heat gains are distributed to the surfaces by fractions defined by user input.

### Inputs

#### Field: Name

A unique user assigned name for an instance of a hot water baseboard heater unit. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the hot water baseboard heater unit can run during a given time period. A schedule value equal to 0 denotes that the unit must be off for that time period. A value greater than 0 denotes that the unit is available to operate during that time period. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Inlet Node Name

This field is the name of the hot water inlet node for the baseboard heater.

#### Field: Outlet Node Name

This field is the name of the hot water outlet node for the baseboard heater.

#### Field: Rated Average Water Temperature

This field is the rated average water temperature for the baseboard heater which is published in the manufacturer's literature in degree Celsius. It typically ranges from 65.56C to 115.36C in the I=B=R rating document while the lowest allowable temperature is 32.22C. The default value is 87.78C. If the user does not enter this field, the default value is assumed.

#### Field: Rated Water Mass Flow Rate

#### **This field is the rated standard water flow rate in kg/s which is published as part of the manufacturer's literature. It is used by the manufacturers when determining the rated capacity (see next field). The default value is 0.063kg/s. If it is blank or zero, the default values is assumed.**

#### Field: Heating Design Capacity Method

Enter the method used to determine the heating design capacity or the method for scalable sizing. Input allowed is either *HeatingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedHeatingCapacity*. If this input field is left blank or zero, then autosizing is assumed. *HeatingDesignCapacity* means user specifies the magnitude of heating capacity or the program calculates the design heating capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design heating capacity from user specified heating capacity per floor area and floor area of the zone served by the radiant baseboard unit. *FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity. The default method is *HeatingDesignCapacity*.

#### Field: Heating Design Capacity {W}

This field is the radiant-convective water baseboard rated capacity in watts at a rated water mass flow rate (see input field Rated Mass Flow Rate). Almost all publications from manufacturers indicate it as W/m (Btuh per linear foot). The user thus must multiply it by the active length of the unit. The active length is available in the literature. Manufacturers are required to publish the difference between active and total length of the unit (I=B=R rating for boilers baseboard radiation, 2009).If it is blank or zero, autosizing is assumed. Design day sizing run must be specified for autosizing.

#### Field: Heating Design Capacity Per Floor Area {W/m2}

Enter the rated heating capacity per unit floor area of radiant-convective water baseboard unit in m3/s-m2. This field is required field when the Heating Design Capacity Method is *CapacityPerFloorArea*. The program calculates the heating capacity from floor area of the zone served by the radiant baseboard unit and the heating capacity per unit floor area value specified by the user. This field may be left blank.

#### Field: Fraction of Autosized Heating Design Capacity {-}

Enter the heating capacity as a fraction of the autosized heating capacity of radiant-convective water baseboard. This input field is required when the Heating Design Capacity Method is *FractionOfAutosizedHeatingCapacity*. The program calculates the heating capacity from the design autosized heating capacity and user specified fraction. Design day sizing run must be specified. This field may be left blank. Default value is 1.0.

#### Field: Maximum Water Flow Rate

This field is the maximum water volumetric flow rate in m^3^/sec.  It can be autosized by EnergyPlus.

#### Field: Convergence Tolerance

This field is the control tolerance for the unit heating output. The unit is controlled by matching the unit output to the zone demand. For hot water baseboards, the model must be numerically inverted to obtain a specified output. The convergence tolerance is the error tolerance used to terminate the numerical inversion procedure. Basically this is the fraction:

![](media/image301.png)\


The default is 0.001.

#### Field: Fraction Radiant

#### **This field specifies what fraction of the power input to the baseboard heater is actually transferred to the space as radiant heat. The fraction should be between 0 and 1. This is the portion of the total power that is modeled as radiation. The portion that is radiant heat transfer from the baseboard heater is distributed to people and specific surfaces using the remaining fields. Note that the sum of the fractions in the remaining fields (people and surfaces) must equal 1.0 so that all the radiant power is distributed properly. For more information on the specification of this parameter, please see the Engineering Reference for EnergyPlus.**

#### Field: Fraction of Radiant Energy Incident on People

This field specifies the fraction of radiant portion of heat transfer to the zone from the baseboard heater that is incident directly on people within the space. This has an impact on the predicted thermal comfort of the zone occupants. Note that although this energy is "radiant" it is actually modeled in the zone heat balance as convective energy (like an internal gain). The basic assumption here is that most radiant energy falling on people will most likely be rereleased to the zone air by convection. This is a simplification of reality, but it maintains the overall energy balance.

#### Field Set: Surface Name, Fraction of Radiant Energy to Surface

#### **The following two items are repeated up to a maximum of 20 surface/fraction pairs. At least one surface/fraction pair must appear in an input file. In other words, at least one surface must be identified as a recipient of radiant energy from the baseboard heater.**

#### **Field:** Surface <x> Name

This field is the name of the first surface to which radiant heat transfer from the baseboard heater is distributed. Used in conjunction with the next field, it helps to define the distribution of the radiant energy on the surfaces within the zone. Note that up to 20 pairs of surface names and corresponding fractions may be entered for a single radiant heater system.

#### Field: Fraction of Radiant Energy to Surface <x>

This field is paired with the preceding surface name (previous field) to define the fraction of radiant heat transfer leaving the baseboard heater that is incident on a particular surface. Users should take into account the directionality of baseboard heaters and their location when defining the value for this input field.

**Note on Fraction of Radiant Energy Incident on [People](#people) and to Surfaces**

The radiant energy from the baseboard heater is defined by the total energy input to the baseboard heater from the water loop times the fraction radiant field shown above. This radiant energy is distributed to surfaces and people using the surface and fraction pairs and the fraction to people input by the user. These fractions to people and surfaces must add up to 1.0. In other words, in an input file, the following relation should be maintained by the user input:

![](media/image302.png)\


An example IDF for the water baseboard is shown below.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:Baseboard:RadiantConvective:Water,
        SPACE2-1 Baseboard,      !- Baseboard Name
        ReheatCoilAvailSched,    !- Availability Schedule Name
        SPACE2-1 Zone Coil Water In Node,  !- Inlet_Node
        SPACE2-1 Zone Coil Water Out Node,  !- Outlet_Node
        76.67,                   !- Average Water Temperature {C}
        0.063,                   !- Standard Water Mass Flow Rate {Kg/s}
        HeatingDesignCapacity,   !- Heating Design Capacity Method
        3000.0,                  !- Heating Design Capacity{ W }
        ,                        !- Heating Design Capacity Per Floor Area{ W / m2 }
        ,                        !- Fraction of Autosized Heating Design Capacity{ -}
        0.0013,                  !- Max Water Volumetric Flow Rate {m3/s}
        0.001,                   !- Convergence Tolerance
        0.3,                     !- Fraction radiant
        0.3,                     !- Fraction of radiant energy incident on people
        C2-1,                    !- Surface 1 Name
        0.1,                     !- Fraction of radiant energy from heater distributed to surface 1
        F2-1,                    !- Surface 2 Name
        0.2,                     !- Fraction of radiant energy from heater distributed to surface 2
        SB21,                    !- surface 3 Name
        0.1,                     !- fraction of radiant energy from heater distributed to surface 3
        SB23,                    !- surface 4 Name
        0.1,                     !- fraction of radiant energy from heater distributed to surface 4
        SB25,                    !- surface 5 Name
        0.1,                     !- fraction of radiant energy from heater distributed to surface 5
        WR-1,                    !- surface 6 Name
        0.1;                     !- fraction of radiant energy from heater distributed to surface 6
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Baseboard Total Heating Rate [W]
    HVAC,Average,Baseboard Convective Heating Rate [W]
    HVAC,Average,Baseboard Radiant Heating Rate [W]
    HVAC,Sum,Baseboard Total Heating Energy [J]
    HVAC,Sum,Baseboard Total Heating Energy [J]
    HVAC,Sum,Baseboard Convective Heating Energy [J]
    HVAC,Sum,Baseboard Radiant Heating Energy [J]
    HVAC,Meter,Baseboard:EnergyTransfer [J]
~~~~~~~~~~~~~~~~~~~~

#### Baseboard Total Heating Rate [W]

This is the actual convective heat addition rate of the baseboard to the zone in Watts.  This value includes the heat convected to the zone air from the baseboard unit, the heat radiated to people in the zone from the baseboard unit, and the additional convection from surfaces within the zone that have been heated by radiation from the baseboard unit.  This value will be different from (and almost always less than) the next field.

#### Baseboard Convective Heating Rate [W]

This field reports the rate at which convective heat addition is transferred from the baseboard to the zone in Watts.

#### Baseboard Radiant Heating Rate [W]

This field reports the rate at which radiant heat addition is transferred from the baseboard to the people and the surfaces within the zone in Watts.

#### Baseboard Total Heating Energy  [J]

This is the actual, total convective energy transferred directly and indirectly from the baseboard to the zone it is serving in Joules.  Direct convective heat transfer includes the convective portion of the baseboard unit.  Indirect convective heat transfer includes heat radiated to people that is then convected to the zone air and heat that is radiated to the surfaces and then convected to the zone air.

#### Baseboard Convective Heating Energy [J]

This field reports the amount of convective energy transferred from the baseboard directly to the zone air in Joules.

#### Baseboard Radiant Heating Energy [J]

This field reports the amount of radiant energy transferred from the baseboard to the zone in Joules.

#### Baseboard Hot Water Mass Flow Rate [kg/s]

This field reports the water mass flow rate at the inlet node of a baseboard unit.

#### Baseboard Air Mass Flow Rate [kg/s]

This field reports the air mass flow rate at the inlet node of a baseboard unit.

#### Baseboard Water Inlet Temperature [C]

This field reports the water temperatures at the inlet node of a baseboard unit.

#### Baseboard Water Outlet Temperature [C]

This field reports the water temperatures at the outlet node of a baseboard unit.

#### Baseboard Air Inlet Temperature [C]

This field reports the air temperatures at the inlet node of a baseboard unit.

#### Baseboard Air Outlet Temperature [C]

This field reports the air temperatures at the outlet node of a baseboard unit.

## ZoneHVAC:Baseboard:RadiantConvective:Steam

The objective of this model is to calculate the convective and radiant heat transfer from steam baseboard heaters to the people and the surfaces within a zone so that surface heat balances can take into account the radiant heat transfer to the surfaces and thus enhance the accuracy of thermal comfort predictions within the space. The radiant heat gains are distributed to the surfaces by fractions defined by the user. Users are requested to provide degree of sub cooling to estimate the outlet conditions of the condensate.

### Inputs

#### Field: Name

A unique user assigned name for an instance of a steam baseboard heater unit. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the steam baseboard heater unit can run during a given time period. A schedule value equal to 0 denotes that the unit must be off for that time period. A value greater than 0 denotes that the unit is available to operate during that time period. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Inlet Node Name

This field is the name of the steam inlet node for the baseboard heater.

#### Field: Outlet Node Name

This field is the name of the steam outlet node for the baseboard heater.

#### Field: Heating Design Capacity Method

Enter the method used to determine the heating design capacity for scalable sizing. Input allowed is either *HeatingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedHeatingCapacity*. If this input field is left blank or zero, then autosizing is assumed. *HeatingDesignCapacity* means user specifies the magnitude of maximum heating capacity or the program calculates the maximum design heating capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design heating capacity from user specified heating capacity per floor area and floor area of the zone served by the radiant baseboard unit. *FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity. The default method is *HeatingDesignCapacity*.

#### Field: Heating Design Capacity {W}

This field is for the a radiant/convective steam baseboard unit heating capacity in watts. This field can be autosized by EnergyPlus. When the Heating Design Capacity Method is *HeatingDesignCapacity*  and this input is is blank, autosizing is assumed. Design day sizing run must be specified.

#### Field: Heating Design Capacity Per Floor Area {W/m2}

Enter the heating capacity per unit floor area in m3/s-m2 of radiant/convective steam baseboard. This field is required field when the Heating Design Capacity Method is *CapacityPerFloorArea*. The program calculates the heating capacity from floor area of the zone served by the steam baseboard unit and the heating capacity per unit floor area value specified by the user. This field may be left blank.

#### Field: Fraction of Autosized Heating Design Capacity {-}

Enter the heating capacity as a fraction of the autosized heating capacity of a radiant/convective steam baseboard unit. This input field is required when the Heating Design Capacity Method is *FractionOfAutosizedHeatingCapacity*. The program calculates the heating capacity from the design autosized heat capacity and user specified fraction. Design day sizing run must be specified. This field may be left blank. Default value is 1.0.

#### Field: Degree of SubCooling 

This field is the temperature drop of the condensate in the coil of the baseboard heater. The steam condensates in the coil and changes the phase to the water, giving up the latent heat of steam to the air in the steam to air heat exchanger. The condensate due to the phase change then cools by certain degree in the coil, so that this amount of heat is added to the zone. The minimum value is 1.0 Celsius and default is 5.0 Celsius.

#### Field: Maximum Steam Flow Rate

This field is the maximum steam volumetric flow rate in m^3^/s through the steam baseboard heater.  The steam volumetric flow rate is calculated at 100C and 101325 Pa. It can be autosized by EnergyPlus.

#### Field: Convergence Tolerance

This field is the control tolerance for the unit heating output. The unit is controlled by matching the unit output to the zone demand. For steam baseboards, the model must be numerically inverted to obtain a specified output. The convergence tolerance is the error tolerance used to terminate the numerical inversion procedure. Basically this is the fraction:

![](media/image303.png)\


The default is 0.001.

#### Field: Fraction Radiant

This field specifies what fraction of the power input to the baseboard heater is actually transferred to the space as radiant heat. The fraction should be between 0 and 1. This is the portion of the total power that is modeled as radiation. The portion that is radiant heat transfer from the baseboard heater is distributed to people and specific surfaces using the remaining fields. Note that the sum of the fractions in the remaining fields (people and surfaces) must equal 1.0 so that all the radiant power is distributed properly.

#### Field: Fraction of Radiant Energy Incident on People

This field specifies the fraction of radiant portion of heat transfer to the zone from the baseboard heater that is incident directly on people within the space. This has an impact on the predicted thermal comfort of the zone occupants. Note that although this energy is "radiant" it is actually modeled in the zone heat balance as convective energy (like an internal gain). The basic assumption here is that most radiant energy falling on people will most likely be rereleased to the zone air by convection. This is a simplification of reality, but it maintains the overall energy balance.

#### Field Set: Surface Name, Fraction of Radiant Energy to Surface

The following two items are repeated up to a maximum of 20 surface/fraction pairs. At least one surface/fraction pair must appear in an input file. In other words, at least one surface must be identified as a recipient of radiant energy from the baseboard heater.

#### Field: Surface <x> Name

This field is the name of the first surface to which radiant heat transfer from the baseboard heater is distributed. Used in conjunction with the next field, it helps to define the distribution of the radiant energy on the surfaces within the zone. Note that up to 20 pairs of surface names and corresponding fractions may be entered for a single radiant heater system.

#### Field: Fraction of Radiant Energy to Surface <x>

This field is paired with the preceding surface name (previous field) to define the fraction of radiant heat transfer leaving the baseboard heater that is incident on a particular surface. Users should take into account the directionality of baseboard heaters and their location when defining the value for this input field.

#### Note on Fraction of Radiant Energy Incident on People and to Surfaces

The radiant energy from the baseboard heater is defined by the total energy input to the baseboard heater from the steam loop times the fraction radiant field shown above. This radiant energy is distributed to surfaces and people using the surface and fraction pairs and the fraction to people input by the user. These fractions to people and surfaces must add up to 1.0. In other words, in an input file, the following relation should be maintained by the user input:

![](media/image304.png)\


An example IDF for the steam baseboard is shown below.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:Baseboard:RadiantConvective:Steam,
    Zone1Baseboard,       !- Baseboard name
    FanAndCoilAvailSched, !- On/off schedule
    Steam Inlet Node,     !- Steam inlet node
    Steam Outlet Node,    !- Steam outlet node
    HeatingDesignCapacity,!- Heating Design Capacity Method
    autosize,             !- Heating Design Capacity{ W }
    ,                     !- Heating Design Capacity Per Floor Area{ W / m2 }
    ,                     !- Fraction of Autosized Heating Design Capacity{ -}
    5.0,                  !- Degree of sub cooling in the coil {C}
    autosize,             !- Maximum steam volumetric flow rate {m3/s}
    0.001,                !- Tolerance
    0.3,                  !- Fraction radiant
    0.3,                  !- Fraction of radiant energy that is incident directly on people
    OuterWall,            !- Surface 1 Name
    0.4,                  !- Fraction of radiant energy from heater distributed to surface 1
    Ceiling,              !- Surface 2 Name
    0.1,                  !- Fraction of radiant energy from heater distributed to surface 2
    InnerWall,            !- Surface 3 Name
    0.1,                  !- Fraction of radiant energy from heater distributed to surface 3
    Floor,                !- Surface 4 Name
    0.1;                  !- Fraction of radiant energy from heater distributed to surface 4
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Baseboard Total Heating Rate [W]
    HVAC,Average,Baseboard Convective Heating Rate [W]
    HVAC,Average,Baseboard Radiant Heating Rate [W]
    HVAC,Sum,Baseboard Total Heating Energy [J]
    HVAC,Sum,Baseboard Convective Heating Energy [J]
    HVAC,Sum,Baseboard Radiant Heating Energy [J]
    HVAC,Meter,Baseboard:EnergyTransfer [J]
    HVAC,Average,Baseboard Steam Mass Flow Rate [kg/s]
    HVAC,Average,Baseboard Steam Inlet temperature [C]
    HVAC,Average,Baseboard Steam Outlet temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Baseboard Total Heating Rate [W]

This is the actual convective heat addition rate of the baseboard to the zone in Watts.  This value includes the heat convected to the zone air from the baseboard unit, the heat radiated to people in the zone from the baseboard unit, and the additional convection from surfaces within the zone that have been heated by radiation from the baseboard unit. This value will be different from (and almost always less than) the next field.

#### Baseboard Convective Heating Rate [W]

This field reports the rate at which convective heat addition is transferred from the baseboard to the zone in Watts.

#### Baseboard Radiant Heating Rate [W]

This field reports the rate at which radiant heat addition is transferred from the baseboard to the people and the surfaces within the zone in Watts.

#### Baseboard Total Heating Energy [J]

This is the actual, total convective energy transferred directly and indirectly from the baseboard to the zone it is serving in Joules. Direct convective heat transfer includes the convective portion of the baseboard unit. Indirect convective heat transfer includes heat radiated to people that is then convected to the zone air and heat that is radiated to the surfaces and then convected to the zone air.

#### Baseboard Convective Heating Energy [J]

This field reports the amount of convective energy transferred from the baseboard directly to the zone air in Joules.

#### **Baseboard Radiant Heating Energy** [J]

This field reports the amount of radiant energy transferred from the baseboard to the zone in Joules.

#### Baseboard Steam Energy [J]

#### Baseboard Steam Rate [W]

These are the total heat addition energy and rate of steam used by the baseboard to heat the zone it is serving in Joules and Watts.

#### **Baseboard** Steam Mass Flow Rate [kg/s]

This field reports the steam mass flow rate at the inlet node of a baseboard unit.

#### **Baseboard** Steam Inlet Temperature [C]

This field reports the steam temperatures at the inlet node of a baseboard unit.

#### **Baseboard** Steam Outlet Temperature [C]

This field reports the steam temperatures at the outlet node of a baseboard unit.

## ZoneHVAC:Baseboard:RadiantConvective:Electric

The electric baseboard heater is a component in the zone equipment simulation. Heat from this device is radiated to people and surfaces and also convected to the surrounding air. The electric baseboard model includes the impact of the radiant heat addition to people and surfaces so that the thermal comfort and surface heat balances are impacted. The component is controlled to meet any remaining zone load not met by other equipment baseboard operates to meet the remaining zone load and the total electric consumption is calculated by dividing by the efficiency of the baseboard.

### Inputs

#### Field: Name

A unique user assigned name for an instance of an electric baseboard heater unit. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the electric baseboard heater unit can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit will be off for the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Heating Design Capacity Method

Enter the method used to determine the heating design capacity for scalable sizing. Input allowed is either *HeatingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedHeatingCapacity*. If this input field is left blank or zero, then autosizing is assumed. *HeatingDesignCapacity* means user specifies the magnitude of maximum or nominal heating capacity or the program calculates the maximum or nominal design heating capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design heating capacity from user specified heating capacity per floor area and floor area of the zone served by the radiant baseboard unit. *FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity. The default method is *HeatingDesignCapacity*.

#### Field: Heating Design Capacity {W}

This field is for the radiant-convective electric baseboard unit nominal heating capacity in watts. This field can be autosized by EnergyPlus. This input field is autosizable. When the Heating Design Capacity Method is *HeatingDesignCapacity*  and this input is is blank, autosizing is assumed. Design day sizing run must be specified.

#### Field: Heating Design Capacity Per Floor Area {W/m2}

Enter the heating capacity per unit floor area in m3/s-m2 of radiant/convective electric baseboard. This field is required field when the Heating Design Capacity Method is *CapacityPerFloorArea*. The program calculates the heating capacity from floor area of the zone served by the radiant baseboard unit and the heating capacity per unit floor area value specified by the user. This field may be left blank.

#### Field: Fraction of Autosized Heating Design Capacity {-}

Enter the heating capacity as a fraction of the autosized heating capacity for radiant/convective electric baseboard. This input field is required when the Heating Design Capacity Method is *FractionOfAutosizedHeatingCapacity*. The program calculates the heating capacity from the design autosized heat capacity and user specified fraction. Design day sizing run must be specified. This field may be left blank. Default value is 1.0.

#### Field: Efficiency

This is the overall electrical efficiency of the electric baseboard unit. The zone load met by this unit is divided by the electrical efficiency to obtain the total electric energy used.

#### Field: Fraction Radiant

This field specifies what fraction of the power input to the baseboard heater is actually transferred to the space as radiant heat. The fraction should be between 0 and 1. This is the portion of the total power that is modeled as radiation. The portion that is radiant heat transfer from the baseboard heater is distributed to people and specific surfaces using the remaining fields. Note that the sum of the fractions in the remaining fields (people and surfaces) must equal 1.0 so that all the radiant power is distributed properly.

#### Field: Fraction of Radiant Energy Incident on People

This field specifies the fraction of the radiant portion of heat transfer to the zone from the baseboard heater that is incident directly on people within the space. This has an impact on the predicted thermal comfort of the zone occupants. Note that although this energy is "radiant" it is actually modeled in the zone heat balance as a convective energy (like an internal gain). The basic assumption here is that radiant energy falling on people will most likely be rereleased to the zone air by convection. This is a simplification of reality, but it maintains the overall energy balance.

#### Field Set: Surface Name, Fraction of Radiant Energy to Surface

The following two items are repeated up to a maximum of 20 surface/fraction pairs. At least one surface/fraction pair must appear in an input file. In other words, at least one surface must be identified as a recipient of radiant energy from the baseboard heater.

#### Field Set: Surface <x> Name

This field is the name of the first surface to which radiant heat transfer from the baseboard heater is distributed. Used in conjunction with the next field, it helps to define the distribution of the radiant energy on the surfaces within the zone. Note that up to 20 pairs of surface names and corresponding fractions may be entered for a single radiant heater system.

#### Field Set: Fraction of Radiant Energy to Surface <x>

This field is paired with the preceding surface name (previous field) to define the fraction of radiant heat transfer leaving the baseboard heater that is incident on a particular surface. Users should take into account the directionality of baseboard heaters and their location when defining the value for this input field.

#### Note on Fraction of Radiant Energy Incident on People and to Surfaces

The radiant energy from the baseboard heater is defined by the total energy input to the baseboard heater times the fraction radiant field shown above. This radiant energy is distributed to surfaces and people using the surface and fraction pairs and the fraction to people input by the user. These fractions to people and surfaces must add up to 1.0. In other words, in an input file, the following relation should be maintained by the user input:

![](media/image305.png)\


Below is an example input for an Electric Baseboard Heater.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:Baseboard:RadiantConvective:Electric,
    Baseboard 1,   !- Name
    BB Schedule,   !- Availability Schedule Name
    HeatingDesignCapacity,   !- Heating Design Capacity Method
    5000,          !- Heating Design Capacity{ W }
    ,              !- Heating Design Capacity Per Floor Area{ W / m2 }
    ,              !- Fraction of Autosized Heating Design Capacity{ -}
    0.97,          !- Efficiency of the baseboard
    0.3,           !- Fraction radiant
    0.3,           !- Fraction of radiant energy that is incident directly on people
    EastWall,      !- Surface 1 name
    0.3,           !- Fraction of radiant energy from heater distributed to surface 1
    WestWall,      !- Surface 2 name
    0.1,           !- Fraction of radiant energy from heater distributed to surface 2
    NorthWall,     !- Surface 3 name
    0.1,           !- Fraction of radiant energy from heater distributed to surface 3
    SouthWall,     !- Surface 4 name
    0.1,           !- Fraction of radiant energy from heater distributed to surface 4
    Ceiling,       !- Surface 5 name
    0.1;           !- Fraction of radiant energy from heater distributed to surface 5
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Baseboard Total Heating Rate [W]
    HVAC,Average,Baseboard Radiant Heating Rate [W]
    HVAC,Average,Baseboard Convective Heating Rate [W]
    HVAC,Average,Baseboard Electric Power[W]
    HVAC,Sum,Baseboard Total Heating Energy [J]
    HVAC,Sum,Baseboard Radiant Heating Energy [J]
    HVAC,Sum,Baseboard Convective Heating Energy [J]
    HVAC,Sum,Baseboard Electric Energy [J]
    HVAC,Meter,Baseboard:EnergyTransfer [J]
    HVAC,Meter,Heating:Electricity[J]
~~~~~~~~~~~~~~~~~~~~

#### Baseboard Total Heating Rate [W]

This is the actual convective heat addition rate of the baseboard to the zone in Watts. This value includes the heat convected to the zone air from the baseboard unit, the heat radiated to people in the zone from the baseboard unit, and the additional convection from surfaces within the zone that have been heated by radiation from the baseboard unit. This value will be different from (and almost always less than) the next field.

#### Baseboard Radiant Heating Rate [W]

This field reports the rate at which radiant heat addition is transferred from the baseboard to the people and the surfaces within the zone in Watts.

#### Baseboard Convective Heating Rate [W]

This field reports the rate at which convective heat addition is transferred from the baseboard to the zone in Watts.

#### Baseboard Electric Energy [J]

This is the electric energy used by the baseboard in Joules.

#### Baseboard Electric Power [J]

This is the electric power required by the baseboard to heat the zone it is serving in Watts

#### Baseboard Total Heating Energy [J] 

This is the actual convective energy transferred directly and indirectly from the baseboard heater to the zone it is serving in Joules. Direct convective heat transfer includes the convective portion of the baseboard unit. Indirect convective heat transfer includes heat radiated to people that is then convected to the zone air and heat that is radiated to the surfaces and then convected to the zone air.

#### Baseboard Radiant Heating Energy [J]

This field reports the amount of radiant energy transferred from the baseboard to the zone in Joules.

#### Baseboard Convective Heating Energy [J]

This field reports the amount of convective energy transferred from the baseboard directly to the zone air in Joules.

## ZoneHVAC:Baseboard:Convective:Water

The hot water baseboard heater is a component of zone equipment. The component is controlled to meet any remaining zone load not met by other equipment in the zone that have higher heating priority. The control is accomplished by throttling the hot water flow. Input resembles that for the simple heating coil: there is an availability schedule, an overall UA, and a maximum hot water mass flow rate. The unit is connected to a hot water loop (demand side) with an inlet and outlet node. Finally, there is the convergence tolerance, which is the tolerance on how closely the baseboard outlet will meet the zone load. Of course, this tolerance is relative to the zone load.

### Inputs

#### Field: Name

A unique user assigned name for an instance of a hot water baseboard heater unit. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the hot water baseboard heater unit can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Inlet Node Name

The name of the hot water inlet node for the baseboard heater.

#### Field: Outlet Node Name

The name of the hot water outlet node for the baseboard heater.

#### Field: Heating Design Capacity Method

Enter the method used to determine the heating design capacity for scalable sizing. Input allowed is either *HeatingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedHeatingCapacity*. If this input field is left blank or zero, then autosizing is assumed. *HeatingDesignCapacity* means user specifies the magnitude of maximum or nominal heating capacity or the program calculates the maximum or nominal design heating capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design heating capacity from user specified heating capacity per floor area and floor area of the zone served by the hot water baseboard heater. *FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity. The default method is *HeatingDesignCapacity*.

#### Field: Heating Design Capacity {W}

This field is for the hot water baseboard heater design heating capacity in watts. This field can be autosized by EnergyPlus. This input field is autosizable. When the Heating Design Capacity Method is *HeatingDesignCapacity*  and this input is is blank, autosizing is assumed. Design day sizing run must be specified id autosize is specified.

#### Field: Heating Design Capacity Per Floor Area {W/m2}

Enter the heating capacity per unit floor area in m3/s-m2 of hot water baseboard heater. This field is required field when the Heating Design Capacity Method is *CapacityPerFloorArea*. The program calculates the heating capacity from floor area of the zone served by hot water baseboard heater and the heating capacity per unit floor area value specified by the user. This field may be left blank.

#### Field: Fraction of Autosized Heating Design Capacity {-}

Enter the heating capacity as a fraction of the autosized heating capacity for the hot water baseboard heater. This input field is required when the Heating Design Capacity Method is *FractionOfAutosizedHeatingCapacity*. The program calculates the heating capacity from the design autosized heat capacity and user specified fraction. Design day sizing run must be specified. This field may be left blank. Default value is 1.0.

#### Field: U-Factor Times Area Value

The overall heat transfer coefficient for the baseboard heater in watts per degree Celsius at design conditions. The U-Factor Times Area Value (UA) is used in an NTU - effectiveness calculation. An estimate of the UA can be obtained from:

![](media/image306.png)\


where q is the heat transferred from water to the air in watts; T~water, avg~ is the average water temperature in degrees C; and T~air, avg~ is the average air temperature in degrees C. Or the LMTD temperature difference can be used.

#### Field: Maximum Water Flow Rate

The maximum hot water volumetric flow rate in m^3^/sec.

#### Field Convergence Tolerance

The control tolerance for the unit heating output. The unit is controlled by matching the unit output to the zone demand. For hot water baseboards, the model must be numerically inverted to obtain a specified output. The convergence tolerance is the error tolerance used to terminate the numerical inversion procedure. Basically this is the fraction:

![](media/image307.png)\


The default is 0.001.

An example IDF for the hot water convective baseboard is shown below.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:Baseboard:Convective:Water,
            Zone3Baseboard, ! name
            FanAndCoilAvailSched, ! on/off schedule
            Zone 3 Reheat Water Inlet Node, ! water inlet node
            Zone 3 Reheat Water Outlet Node, ! water outlet node
            HeatingDesignCapacity, !- Heating Design Capacity Method
            autosize,!- Heating Design Capacity{ W }
            ,        !- Heating Design Capacity Per Floor Area{ W / m2 }
            ,        !- Fraction of Autosized Heating Design Capacity{ -}
            500.,    ! UA
            0.0013,  ! maximum water flow rate m3/s
            0.001;   ! tolerance
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Baseboard Total Heating Rate [W]HVAC,Sum,Baseboard Total Heating Energy [J]
    HVAC,Sum,Baseboard Hot Water Energy [J]
    HVAC,Average,Baseboard Hot Water Mass Flow Rate [kg/s]
    HVAC,Average,Baseboard Air Mass Flow Rate [kg/s]
    HVAC,Average,Baseboard Air Inlet Temperature [C]
    HVAC,Average,Baseboard Air Outlet Temperature [C]
    HVAC,Average,Baseboard Water Inlet Temperature [C]
    HVAC,Average,Baseboard Water Outlet Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Baseboard Total Heating Rate [W]

This is the heat addition rate of the convective baseboard to the zone it is serving in Watts.

#### Baseboard Total Heating Energy [J]

This is the heat addition of the convective baseboard to the zone it is serving in Joules over the timestep being reported.

#### Baseboard Hot Water Energy [J]

This is the energy in the hot water used by the baseboard to heat the zone, in Joules.

#### Baseboard Hot Water Mass Flow Rate [kg/s]

This field reports the water mass flow rate at the inlet node of a baseboard unit.

#### Baseboard Air Mass Flow Rate [kg/s]

This field reports the air mass flow rate at the inlet node of a baseboard unit.

#### Baseboard Water Inlet Temperature [C]

This field reports the water temperatures at the inlet node of a baseboard unit.

#### Baseboard Water Outlet Temperature [C]

This field reports the water temperatures at the outlet node of a baseboard unit.

#### Baseboard Air Inlet Temperature [C]

This field reports the air temperatures at the inlet node of a baseboard unit.

#### Baseboard Air Outlet Temperature [C]

This field reports the air temperatures at the outlet node of a baseboard unit.

## ZoneHVAC:Baseboard:Convective:Electric

The electric baseboard heater is a component in the zone equipment simulation. The component is controlled to meet any remaining zone load not met by other equipment in the zone that have higher heating priority. The control is accomplished by taking the remaining zone load and dividing by the efficiency of the baseboard.

### Inputs

#### Field: Name

A unique user assigned name for an instance of a electric baseboard heater unit. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the electric baseboard heater unit can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Heating Design Capacity Method

Enter the method used to determine the heating design capacity or enter the method for scalable sizing of heating capacity of electric baseboard unit. Input allowed is either *HeatingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedHeatingCapacity*. If this input field is left blank or zero, then autosizing is assumed. *HeatingDesignCapacity* means user specifies the magnitude of maximum or nominal heating capacity or the program calculates the maximum or nominal design heating capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design heating capacity from user specified heating capacity per floor area and floor area of the zone served by the electric convective baseboard unit. *FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity. The default method is *HeatingDesignCapacity*.

#### Field: Heating Design Capacity {W}

This field is for the convective electric baseboard nominal heating capacity in watts. This field can be autosized by EnergyPlus. This input field is autosizable. Design day sizing run must be specified. This input field may be left blank.

#### Field: Heating Design Capacity Per Floor Area {W/m2}

Enter the heating capacity per unit floor area in m3/s-m2 of electric convective baseboard unit. This field is required field when the Heating Design Capacity Method is *CapacityPerFloorArea*. The program calculates the heating capacity from floor area of the zone served by the electric convective baseboard unit and the heating capacity per unit floor area value specified by the user. This field may be left blank.

#### Field: Fraction of Autosized Heating Design Capacity {-}

Enter the heating capacity as a fraction of the autosized heating capacity for convective electric baseboard unit. This input field is required when the Heating Design Capacity Method is *FractionOfAutosizedHeatingCapacity*. The program calculates the heating capacity from the design autosized heating capacity and user specified fraction. Design day sizing must be specified. This field may be left blank.  Default value is 1.0.

#### Field: Efficiency

This is the overall electrical efficiency of the electric baseboard. The zone load met by this unit is divided by the electrical efficiency to obtain the total electric energy used.

An example IDF for the electric convective baseboard is shown below.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:Baseboard:Convective:Electric,
        Zone1Baseboard,  !- Baseboard Name
        FanAndCoilAvailSched,  !- Availability Schedule Name
        HeatingDesignCapacity,   !- Heating Design Capacity Method
        5000,                    !- Heating Design Capacity{ W }
        ,                        !- Heating Design Capacity Per Floor Area{ W / m2 }
        ,                        !- Fraction of Autosized Heating Design Capacity{ -}
        0.97;  !- Efficiency of the BaseBoard
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Baseboard Total Heating Rate [W]
    HVAC,Sum,Baseboard Total Heating Energy [J]
    HVAC,Meter,Baseboard:EnergyTransfer [J]
    HVAC,Sum,Baseboard Electric Energy [J]
    HVAC,Average,Baseboard Electric Power[W]
    HVAC,Meter,Heating:Electricity[J]
~~~~~~~~~~~~~~~~~~~~

#### Baseboard Total Heating Rate [W]

This is the heat addition rate of the electric convective baseboard to the zone it is serving in Watts.

#### Baseboard Total Heating Energy [J]

This is the heat addition of the convective baseboard to the zone it is serving in Joules over the timestep being reported.

#### Baseboard Electric Energy [J]

This is the total electric consumption of the baseboard for the zone it is serving in Joules over the timestep being reported.

#### Baseboard Electric Power [W]

This is the electric power required by the baseboard to the zone it is serving in Watts.

## ZoneHVAC:LowTemperatureRadiant:VariableFlow

This low temperature radiant system (hydronic) is a component of zone equipment that is intended to model any "radiant system" where water is used to supply/remove energy to/from a building surface (wall, ceiling, or floor). The component is controlled to meet any remaining zone load not met by other equipment in the zone that have higher priority. The control is accomplished by throttling the hot or chilled water flow to the unit. Note that this system will only control based on the radiant system controls defined by this input syntax and not via a zone thermostat such as is used for forced air systems. Note also that because this unit does not require a thermostat that in cases where no other systems are serving the zone in which this system resides that it will use the heating equipment priority to determine which system will run first.  If the radiant system is serving a zone with forced air equipment, the radiant system will follow the priority order established by the zone thermostat but will still base its response on the controls defined by the user for the radiant system.

This model covers a wide range of low temperature radiant systems: heating and/or cooling, panel or embedded pipes, etc. It is not intended to simulate high temperature electric or gas radiant heaters. Those devices will be handled by a separate model and different input syntax. Low temperature radiant systems that use electric resistance heating should also be defined using separate input syntax (ref: [ZoneHVAC:LowTemperatureRadiant:Electric](#zonehvaclowtemperatureradiantelectric)).

### Inputs

#### Field: Name

This field is an unique user assigned name for an instance of a hydronic low temperature radiant system. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

This field is the name of the schedule (ref: Schedule) that denotes whether the hydronic low temperature radiant system can run during a given time period. A schedule value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for that time period. A value greater than 0 (usually 1 is used) denotes that the unit is available to operate during that time period. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Zone Name

This field is the name of the zone (Ref: [Zone](#zone)) in which the hydronic low temperature radiant system is principally located and intended to affect. A system that is between two zones will still act upon each zone; however, the zone name referenced here should be the zone that controls the radiant system response.

#### Field: Surface Name or Radiant Surface Group Name

This field is the name of the surface (Ref: BuildingSurface) or surface list (Ref: [ZoneHVAC:LowTemperatureRadiant:SurfaceGroup](#zonehvaclowtemperatureradiantsurfacegroup)) in which the hydronic tubing is embedded/contained. This specification attaches the source or sink from the radiant system to a particular surface and the contribution of the system to the heat balances of that surface. If this field is a surface list, then the source or sink is attached to all of the surfaces in the list with the radiant system surface group defining the breakdown of how flow rate is split between the various surfaces. Only base surfaces (e.g., [BuildingSurface:Detailed](#buildingsurfacedetailed)) are valid. Window/Door surfaces and Internal Mass are not valid surface types for embedded radiant systems.

#### Field: Hydronic Tubing Inside Diameter

This field is the inside diameter of the tubes through which water is circulated for the system being defined by this statement. The inside diameter should be recorded in meters and is used to determine the convective heat transfer from the water to the inside surface of the hydronic tubing.

#### Field: Hydronic Tubing Length

This field is the total length of pipe embedded in the surface named above in the surface name field. The length of the tube should be entered in meters and is used to determine the effectiveness of heat transfer from the fluid being circulated through the tubes and the tube/surface. Longer tubing lengths result in more heat will be transferred to/from the radiant surface to the circulating fluid. Note that if the user elects to autosize this field that a standard zone thermostat such as would be used for a forced air system must be defined as autosizing calculations are based on the zone thermostat value and not on the radiant system control values.

#### Field: Temperature Control Type

This field specifies along with the throttling range and setpoint schedules how the user wishes to control the hydronic radiant system. The temperature denoted in the setpoint schedule can refer to one of five different temperatures: the zone mean air temperature, the zone mean radiant temperature, the zone operative temperature, the outdoor dry-bulb temperature, or the outdoor wet-bulb temperature. The choice of temperature is controlled by the current field—temperature control type. The user must select from the following options:

~~~~~~~~~~~~~~~~~~~~

    MeanAirTemperature
    MeanRadiantTemperature
    OperativeTemperature
    OutdoorDryBulbTemperature
    OutdoorWetBulbTemperature
~~~~~~~~~~~~~~~~~~~~

Operative temperature for radiant system controls is the average of Mean Air Temperature and Mean Radiant Temperature. If the user does not select a control type, **MeanAirTemperature** control is assumed by EnergyPlus. See the throttling range and control temperature schedule fields below for more information.

#### Field: Heating Design Capacity Method

Enter the method used to determine the heating design capacity for scalable sizing. Input allowed is either *HeatingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedHeatingCapacity*. If this input field is left blank or zero, then autosizing is assumed. *HeatingDesignCapacity* means user specifies the magnitude of maximum or nominal heating capacity or the program calculates the design heating capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design heating capacity from user specified heating capacity per floor area and floor area of the zone served by the low temperature radiant variable flow unit. *FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity. The default method is *HeatingDesignCapacity*.

#### Field: Heating Design Capacity {W}

This field is for the low temperature radiant variable flow unit design heating capacity in watts. This field can be autosized by EnergyPlus. This input field is autosizable. When the Heating Design Capacity Method is *HeatingDesignCapacity*  and this input is is blank, autosizing is assumed. Design day sizing run must be specified if autosized.

#### Field: Heating Design Capacity Per Floor Area {W/m2}

Enter the heating capacity per unit floor area in m3/s-m2 of low temperature radiant variable flow unit. This field is required field when the Heating Design Capacity Method is *CapacityPerFloorArea*. The program calculates the heating capacity from floor area of the zone served by the low temperature radiant variable flow unit and the heating capacity per unit floor area value specified by the user. This field may be left blank.

#### Field: Fraction of Autosized Heating Design Capacity {-}

Enter the heating capacity as a fraction of the autosized heating capacity for low temperature radiant variable flow unit. This input field is required when the Heating Design Capacity Method is *FractionOfAutosizedHeatingCapacity*. The program calculates the heating capacity from the design autosized heat capacity and user specified fraction. Design day sizing run must be specified. This field may be left blank. Default value is 1.0.

#### Field: Maximum Hot Water Flow

This field is the maximum flow rate of hot water through the radiant system in m^3^/sec. The controls for the radiant system will vary the flow rate of hot water through the surface using zero flow and the maximum flow rate specified in this field as the lower and upper bounds, respectively. Note that if the user elects to autosize this field that a standard zone thermostat such as would be used for a forced air system must be defined as autosizing calculations are based on the zone thermostat value and not on the radiant system control values.

#### Field: Heating Water Inlet Node Name

This field contains the name of the hot water inlet node to the radiant system. Note that this node name must also show up in the branch description when defining the plant demand side network in a manner identical to defining a heating coil.

#### Field: Heating Water Outlet Node Name

This field contains the name of the hot water oulet node to the radiant system. Note that this node name must also show up in the branch description when defining the plant demand side network in a manner identical to defining a heating coil.

#### Field: Heating Control Throttling Range

This field specifies the range of temperature in degrees Celsuis over which the radiant system throttles from zero flow rate up to the maximum defined by the maximum hot water flow rate field described above. The throttling range parameter is used in conjunction with the control temperature to define the response of the system to various zone conditions. The heating control temperature schedule specifies the "setpoint" temperature where the flow rate to the system is at half of the maximum flow rate. For example, if the heating control temperature setpoint is currently 15C and the heating throttling range is 2C, the water flow rate to the radiant system will be zero when the controlling temperature (MAT, MRT, Operative Temperature, ODB, or OWB; see control type field above) is at or above 16C and the maximum flow rate when the controlling temperature is at or below 14C. This represents a throttling range of 2C around the setpoint of 15C. In between 14C and 16C, the flow rate to the radiant system is varied linearly.

#### Field: Heating Control Temperature Schedule Name

This field specifies the heating setpoint or control temperature for the radiant system in degrees Celsius. Used in conjunction with the previous field (heating control throttling range), it will define whether or not the system is running and the current flow rate. Water flow rate to the system is varied linearly around the setpoint temperature based on the throttling range and the maximum heating flow rate parameters (see above). It should be noted that this control schedule will allow different setpoint temperatures throughout the year for heating. The control of the radiant system is based solely on the heating control temperature schedule, the cooling control temperature schedule (see below), and the control temperature type listed above. The radiant system will not use any zone thermostat that might be used by other systems serving the zone in which the radiant system resides.

#### Field: Cooling Design Capacity Method

Enter the method used to determine the cooling design capacity for scalable sizing. Input allowed is either *CoolingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedCoolingCapacity*. If this input field is left blank or zero, then autosizing is assumed. *CoolingDesignCapacity* means user specifies the design cooling capacity or the program calculates the design cooling capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design cooling capacity from user specified heating capacity per floor area and floor area of the zone served by the low temperature radiant variable flow unit. *FractionOfAutosizedCoolingCapacity* means the program calculates the design cooling capacity from user specified fraction and the auto-sized design cooling capacity. The default method is *CoolingDesignCapacity*.

#### Field: Cooling Design Capacity {W}

This field is for the low temperature radiant variable flow unit design cooling capacity in watts. This field can be autosized by EnergyPlus. This input field is autosizable. When the Cooling Design Capacity Method is *CoolingDesignCapacity* and this input is is blank, autosizing is assumed. Design day sizing run must be specified if autosized.

#### Field: Cooling Design Capacity Per Floor Area {W/m2}

Enter the cooling capacity per unit floor area in m3/s-m2 of low temperature radiant variable flow unit. This field is required field when the Cooling Design Capacity Method is *CapacityPerFloorArea*. The program calculates the cooling capacity from floor area of the zone served by the low temperature radiant variable flow unit and the cooling capacity per unit floor area value specified by the user. This field may be left blank.

#### Field: Fraction of Autosized Cooling Design Capacity {-}

Enter the cooling capacity as a fraction of the autosized cooling capacity for low temperature radiant variable flow unit. This input field is required when the Cooling Design Capacity Method is *FractionOfAutosizedCoolingCapacity*. The program calculates the cooling capacity from the design autosized cooling capacity and user specified fraction. Design day sizing run must be specified. This field may be left blank. Default value is 1.0.

#### Field: Maximum Cold Water Flow

This field is the maximum flow rate of cold water through the radiant system in m^3^/sec. The controls for the radiant system will vary the flow rate of cold water through the surface using zero flow and the maximum flow rate specified in this field as the lower and upper bounds, respectively. Note that this field is optional and not required for a heating only system. Note also that if the user elects to autosize this field that a standard zone thermostat such as would be used for a forced air system must be defined as autosizing calculations are based on the zone thermostat value and not on the radiant system control values.

#### Field: Cooling Water Inlet Node Name

This field contains the name of the cold water inlet node to the radiant system. Note that this node name must also show up in the branch description when defining the plant demand side network in a manner identical to defining a cooling coil. As with the maximum cold water flow rate, this field is optional and not required for a heating only system.

#### Field: Cooling Water Outlet Node Name

This field contains the name of the cold water oulet node to the radiant system. Note that this node name must also show up in the branch description when defining the plant demand side network in a manner identical to defining a cooling coil. As with the maximum cold water flow rate, this field is optional and not required for a heating only system.

#### Field: Cooling Control Throttling Range

This field specifies the range of temperature in degrees Celsuis over which the radiant system throttles from zero flow rate up to the maximum defined by the maximum cold water flow rate field described above. The throttling range parameter is used in conjunction with the control temperature to define the response of the system to various zone conditions. The cooling control temperature schedule specifies the "setpoint" temperature where the flow rate to the system is at half of the maximum flow rate. For example, if the cooling control temperature setpoint is currently 25C and the cooling throttling range is 2C, the water flow rate to the radiant system will be zero when the controlling temperature (MAT, MRT, Operative Temperature, ODB, or OWB; see control type field above) is at or below 24C and the maximum flow rate when the controlling temperature is at or above 26C. This represents a throttling range of 2C around the setpoint of 25C. In between 24C and 26C, the flow rate to the radiant system is varied linearly.

#### Field: Cooling Control Temperature Schedule Name

This field specifies the cooling setpoint or control temperature for the radiant system in degrees Celsius. Used in conjunction with the previous field (cooling control throttling range), it will define whether or not the system is running and the current flow rate. Water flow rate to the system is varied linearly around the setpoint temperature based on the throttling range and the maximum cooling flow rate parameters (see above). It should be noted that this control schedule will allow different setpoint temperatures throughout the year for cooling. The control of the radiant system is based solely on the heating control temperature schedule listed above, the cooling control temperature schedule, and the control temperature type listed above. The radiant system will not use any zone thermostat that might be used by other systems serving the zone in which the radiant system resides.

#### Field: Condensation Control Type

When radiant systems do cooling, there is the possibility that condensation will occur on the surface that is being cooled.  This is due to the fact that the surface temperature may drop below the dew-point temperature of the space.  When this occurs, condensation on the surface will occur.  In EnergyPlus, users have several options for handling this situation including: Off and SimpleOff.  When the user chooses the Off option, EnergyPlus will not do anything other than produce a warning message when condensation is predicted to occur.  The program will simply continue on; no moisture will be removed from the zone air and there will be no adjustment of the surface temperature as a result of the condensation.  When the user chooses the SimpleOff option, the program will predict cases where condensation will occur and shut-off the radiant system to avoid this situation.  With this option, the users also have the opportunity to adjust when the system will shut down.  This is specified with the next parameter (field: condensation differential parameter). This parameter is optional and EnergyPlus will use the SimpleOff strategy when this parameter is not specified.

#### Field: Condensation Control Dewpoint Offset

This optional parameter is only valid with the SimpleOff condensation handling algorithm (see previous input parameter).  It establishes the difference between the calculated dew-point temperature of the space and the allowed surface temperature to which the surface can drop before the radiant system shuts down in degrees Celsius.  This parameter can be any positive, negative, or zero value.  When this parameter is zero, the radiant system will shut down when the surface temperature drops to the dew-point temperature or below.  When this parameter is positive, the radiant system will shut down when the surface is the number of degrees Celsius above the dew-point temperature.  This allows some extra safety to avoid condensation.  When this parameter is negative, the radiant system will shut down when the surface temperature is the number of degrees Celsius below the dew-point temperature.  While not recommended, this strategy allows the user to simulate a situation where small amounts of condensation are tolerable.

#### Field: Number of Circuits

This optional input allows the user to choose between modeling each surface in the radiant system as a single hydronic circuit or to allow the program to divide the surface into multiple parallel hydronic circuits based on the next input field *Circuit Length*. To model as a single circuit choose *OnePerSurface*. To model as multiple circuits choose *CalculateFromCircuitLength*. It is recommended that *CalculateFromCircuitLength* be chosen. The default is *OnePerSurface* for backward compatibility with older versions of EnergyPlus.

#### Field: Circuit Length

The length in meters of each parallel hydronic circuit in a surface. Used when the previous input field is set to *CalculateFromCircuitLength*. The default is 106.7 meters (350 feet), which is the maximum circuit length allowed in Title 24.

An example IDF with a hydronic low temperature radiant system is shown below.

~~~~~~~~~~~~~~~~~~~~

      ZoneHVAC:LowTemperatureRadiant:VariableFlow,
        SPACE1-1 Zone Radiant Floor,  !- Name
        RADIANTSYSAVAILSCHED,    !- Availability Schedule Name
        SPACE1-1,                !- Zone Name
        C1-1,                    !- Surface Name or Radiant Surface Group Name
        0.013,                   !- Hydronic Tubing Inside Diameter {m}
        autosize,                !- Hydronic Tubing Length {m}
        OperativeTemperature,    !- Temperature Control Type
        HeatingDesignCapacity,   !- Heating Design Capacity Method
        autosize,                !- Heating Design Capacity{ W }
        ,                        !- Heating Design Capacity Per Floor Area{ W / m2 }
        ,                        !- Fraction of Autosized Heating Design Capacity{ -}
        0.0004,                  !- Maximum Hot Water Flow {m3/s}
        SPACE1-1 Zone Radiant Water Inlet Node,  !- Heating Water Inlet Node Name
        SPACE1-1 Zone Radiant Water Outlet Node,  !- Heating Water Outlet Node Name
        2.0,                     !- Heating Control Throttling Range {deltaC}
        RADIANT HEATING SETPOINTS,  !- Heating Control Temperature Schedule Name
        CoolingDesignCapacity,   !- Cooling Design Capacity Method
        autosize,                !- Cooling Design Capacity{ W }
        ,                        !- Cooling Design Capacity Per Floor Area{ W / m2 }
        ,                        !- Fraction of Autosized Cooling Design Capacity{ -}
        autosize,                !- Maximum Cold Water Flow {m3/s}
        SPACE1-1 Cooling Water Inlet Node,  !- Cooling Water Inlet Node Name
        SPACE1-1 Cooling Water Outlet Node,  !- Cooling Water Outlet Node Name
        2.0,                     !- Cooling Control Throttling Range {deltaC}
        RADIANT COOLING SETPOINTS,  !- Cooling Control Temperature Schedule Name
        Off,                     !- Condensation Control Type
        1.0,                     !- Condensation Control Dewpoint Offset
        CalculateFromCircuitLength,   !- Number of Circuits
        106.7;                   !- Circuit Length
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Zone Radiant HVAC Heating Rate [W]
    HVAC,Sum,Zone Radiant HVAC Heating Energy [J]
    HVAC,Average,Zone Radiant HVAC Cooling Rate [W]
    HVAC,Sum,Zone Radiant HVAC Cooling Energy [J]
    HVAC,Average,Zone Radiant HVAC Mass Flow Rate [kg/s]
    HVAC,Average,Zone Radiant HVAC Inlet Temperature [C]
    HVAC,Average,Zone Radiant HVAC Outlet Temperature [C]
    HVAC,Sum,Zone Radiant HVAC Moisture Condensation Time[s]
    HVAC,Sum,Zone Radiant HVAC Heating Fluid Energy [J]
    HVAC,Sum,Zone Radiant HVAC Cooling Fluid Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Zone Radiant HVAC Heating Rate [W]

This field reports the heating input rate to the low temperature radiant system in Watts. This is the heat source to the surface that is defined as the radiant system. The heating rate is determined by the zone conditions and the control scheme defined in the user input.

#### Zone Radiant HVAC Heating Energy [J]

This field reports the heating input to the low temperature radiant system in Joules. This is the heat source to the surface that is defined as the radiant system. The heating rate is determined by the zone conditions, the control scheme defined in the user input, and the timestep.

#### Zone Radiant HVAC Cooling Rate [W]

This field reports the cooling input rate to the low temperature radiant system in Watts. This is the heat sink to the surface that is defined as the radiant system. The cooling rate is determined by the zone conditions and the control scheme defined in the user input.

#### Zone Radiant HVAC Cooling Energy [J]

This field reports the cooling input to the low temperature radiant system in Joules. This is the heat sink to the surface that is defined as the radiant system. The cooling rate is determined by the zone conditions, the control scheme defined in the user input, and the timestep.

#### Zone Radiant HVAC Mass Flow Rate Rate [kg/s]

This field reports the mass flow rate of water through the low temperature radiant system in kilograms per second.

#### Zone Radiant HVAC Inlet Temperature [C]

This field reports the temperature of water entering the low temperature radiant system in Celsius.

#### Zone Radiant HVAC Outlet Temperature [C]

This field reports the temperature of water leaving the low temperature radiant system in Celsius.

#### Zone Radiant HVAC Moisture Condensation Time[s]

This field reports the amount of time when condensation is occurring.  When using the Off condensation control, this simply reports the amount of time when condensation occurs.  When using the SimpleOff condensation control, this indicates the amount of time when the system has been shut off because of the potential danger of condensation.

#### Zone Radiant HVAC Heating Fluid Energy [J]

This is the demand placed on the hot fluid plant loop connection serving the low temperature radiant system, in Joules.

#### Zone Radiant HVAC Cooling Fluid Energy [J]

This is the demand placed on the cooling fluid plant loop connection serving the low temperature radiant system, in Joules.

## ZoneHVAC:LowTemperatureRadiant:ConstantFlow

This low temperature radiant system (hydronic) is a component of zone equipment that is intended to model any "radiant system" where water is used to supply/remove energy to/from a building surface (wall, ceiling, or floor). The component is controlled via control schedules as described in the syntax below and does not require a zone thermostat. Note that because this unit does not require a thermostat that in cases where no other systems are serving the zone in which this system resides that it will use the heating equipment priority to determine which system will run first.  If the radiant system is serving a zone with forced air equipment, the radiant system will follow the priority order established by the zone thermostat but will still base its response on the controls defined by the user for the radiant system.

The constant flow system differs from the hydronic system describe above in what it controls. The hydronic system varies the flow rate through the radiant system based on some control temperature. The constant flow system keeps flow rate constant via a local circulation pump and varies the water temperature that is sent to the radiant system. This is accomplished with a mixing valve that is controlled by a sensor. This model covers a wide range of low temperature radiant systems: heating and/or cooling, panel or embedded pipes, etc. It is not intended to simulate high temperature electric or gas radiant heaters. Those devices will be handled by a separate model and different input syntax. Low temperature radiant systems that use electric resistance heating should also be defined using separate input syntax (ref: [ZoneHVAC:LowTemperatureRadiant:Electric](#zonehvaclowtemperatureradiantelectric)). Low temperature radiant systems that vary the flow rate through the radiant system should also be defined using separate input syntax (ref: [ZoneHVAC:LowTemperatureRadiant:VariableFlow](#zonehvaclowtemperatureradiantvariableflow))

One of the other differences between this model and the variable flow hydronic radiant system is that the constant flow radiant system has a built-in local secondary loop. It will recirculate flow coming out of the system and mix this with flow from the loop to arrive at the desired inlet temperature to the radiant system (note that this model has the temperature sensor AFTER the pump to insure proper inlet temperature to the radiant system). The local loop also contains a pump which is assumed to be upstream of the radiant system and after the mixing valve. So, the local loop can have some recirculation. The flow from the main loop may also bypass the radiant system if more than enough flow is available and the main loop is also a constant flow system.

### Inputs

#### Field: Name

This field is an unique user assigned name for an instance of a constant flow low temperature radiant system. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

This field is the name of the schedule (ref: Schedule) that denotes whether the constant flow low temperature radiant system can run during a given time period. A schedule value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for that time period. A value greater than 0 (usually 1 is used) denotes that the unit is available to operate during that time period. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Zone Name

This field is the name of the zone (Ref: [Zone](#zone)) in which the constant flow low temperature radiant system is principally located and intended to affect. A system that is between two zones will still act upon each zone; however, the zone name referenced here should be the zone that controls the radiant system response.

#### Field: Surface Name or Radiant Surface Group Name

This field is the name of the surface (Ref: [BuildingSurface:Detailed](#buildingsurfacedetailed)) or surface list (Ref: [ZoneHVAC:LowTemperatureRadiant:SurfaceGroup](#zonehvaclowtemperatureradiantsurfacegroup)) in which the hydronic tubing is embedded/contained. This specification attaches the source or sink from the radiant system to a particular surface and the contribution of the system to the heat balances of that surface. If this field is a surface list, then the source or sink is attached to all of the surfaces in the list with the radiant system surface group defining the breakdown of how flow rate is split between the various surfaces. Only base surfaces ([BuildingSurface:Detailed](#buildingsurfacedetailed)) are valid. Window/Door surfaces and Internal Mass are not valid surface types for embedded radiant systems.

#### Field: Hydronic Tubing Inside Diameter

This field is the inside diameter of the tubes through which water is circulated for the system being defined by this statement. The inside diameter should be recorded in meters and is used to determine the convective heat transfer from the water to the inside surface of the hydronic tubing.

#### Field: Hydronic Tubing Length

This field is the total length of pipe embedded in the surface named above in the surface name field. The length of the tube should be entered in meters and is used to determine the effectiveness of heat transfer from the fluid being circulated through the tubes and the tube/surface. Longer tubing lengths result in more heat being transferred to/from the radiant surface to the circulating fluid. This field is autosizable.

#### Field: Temperature Control Type

This field specifies along with setpoint (control) and water schedules how the user wishes to control the constant flow radiant system. The temperature denoted in the setpoint schedule can refer to one of five different temperatures: the zone mean air temperature, the zone mean radiant temperature, the zone operative temperature, the outdoor dry-bulb temperature, or the outdoor wet-bulb temperature. The choice of temperature is controlled by the current field—temperature control type. The user must select from the following options:

~~~~~~~~~~~~~~~~~~~~

    MeanAirTemperature
    MeanRadiantTemperature
    OperativeTemperature
    OutdoorDryBulbTemperature
    OutdoorWetBulbTemperature
~~~~~~~~~~~~~~~~~~~~

Operative temperature for radiant system controls is the average of Mean Air Temperature and Mean Radiant Temperature. If the user does not select a control type, **MeanAirTemperature** control is assumed by EnergyPlus. See the throttling range and control temperature schedule fields below for more information.

#### Field: Rated Flow Rate

This field is the maximum flow rate of water through the radiant system in m^3^/sec. This flow rate is held constant by the local component pump, but the user has the option of varying this flow rate via a schedule (see next input field). The constant flow system will accept this flow rate and control the inlet temperature based on the control and water temperature schedules defined below. This field is autosizable.

#### Field: Pump Flow Rate Schedule Name

This field modifies the maximum flow rate of water through the radiant system in m^3^/sec. This input is "optional". If the user does not enter a schedule, the flow rate through the radiant system is assumed to be constant during all hours that it is operating based on the value entered in the previous input field. Note that the values for this schedule must be between zero and one. The values in this schedule are multipliers on the previous field – Rated Flow Rate.

#### Field: Rated Pump Head

This numeric field contains the pump's rated head in Pascals.

#### Field: Rated Power Consumption

This numeric field contains the pump's rated power consumption in Watts.

#### Field: Motor Efficiency

This numeric field contains the pump's efficiency in decimal form (0 = 0%, 1 = 100%).

#### Field: Fraction of Motor Inefficiencies to Fluid Stream

This numeric field contains the pump's fraction of power loss to the fluid.

#### Field: Heating Water Inlet Node Name

This field contains the name of the hot water inlet node to the radiant system. Note that this node name must also show up in the branch description when defining the plant demand side network in a manner identical to defining a heating coil.

#### Field: Heating Water Outlet Node Name

This field contains the name of the hot water outlet node to the radiant system. Note that this node name must also show up in the branch description when defining the plant demand side network in a manner identical to defining a heating coil.

#### Field: Heating High Water Temperature Schedule Name

This field specifies the high water temperature in degrees Celsius for the temperature control of a constant flow radiant heating system. Water and control temperatures for heating work together to provide a linear function that determines the water temperature sent to the radiant system. The current control temperature (see Temperature Control Type above) is compared to the high and low control temperatures at the current time. If the control temperature is above the high temperature, then the system will be turned off and the water mass flow rate will be zero. If the control temperature is below the low temperature, then the inlet water temperature is set to the high water temperature. If the control temperature is between the high and low value, then the inlet water temperature is linearly interpolated between the low and high water temperature values.  For more information and a graph of how the water and control schedules affect the system operation, please consult the Engineering Reference document.

#### Field: Heating Low Water Temperature Schedule Name

This field specifies the low water temperature in degrees Celsius for the temperature control of a constant flow heating radiant system. For more information on its interpretation, see Heating High Water Temperature Schedule above.

#### Field: Heating High Control Temperature Schedule Name

This field specifies the high control temperature in degrees Celsius for the temperature control of a constant flow heating radiant system. For more information on its interpretation, see Heating High Water Temperature Schedule above.

#### Field: Heating Low Control Temperature Schedule Name

This field specifies the low control temperature in degrees Celsius for the temperature control of a constant flow heating radiant system. For more information on its interpretation, see Heating High Water Temperature Schedule above.

#### Field: Cooling Water Inlet Node Name

This field contains the name of the cold water inlet node to the radiant system. Note that this node name must also show up in the branch description when defining the plant demand side network in a manner identical to defining a cooling coil. As with the maximum cold water flow rate, this field is optional and not required for a heating only system.

#### Field: Cooling Water Outlet Node Name

This field contains the name of the cold water outlet node to the radiant system. Note that this node name must also show up in the branch description when defining the plant demand side network in a manner identical to defining a cooling coil. As with the maximum cold water flow rate, this field is optional and not required for a heating only system.

#### Field: Cooling High Water Temperature Schedule Name

This field specifies the high water temperature in degrees Celsius for the temperature control of a constant flow radiant cooling system. Water and control temperatures for heating work together to provide a linear function that determines the water temperature sent to the radiant system. The current control temperature (see Temperature Control Type above) is compared to the high and low control temperatures at the current time. If the control temperature is above the high temperature, then the inlet water temperature is set to the low water temperature. If the control temperature is below the low temperature, then system will be turned off and the water mass flow rate will be zero. If the control temperature is between the high and low value, then the inlet water temperature is linearly interpolated between the low and high water temperature values.  For more information and a graph of how the water and control schedules affect the system operation, please consult the Engineering Reference document.

#### Field: Cooling Low Water Temperature Schedule Name

This field specifies the low water temperature in degrees Celsius for the temperature control of a constant flow cooling radiant system. For more information on its interpretation, see Cooling High Water Temperature Schedule above.

#### Field: Cooling High Control Temperature Schedule Name

This field specifies the high control temperature in degrees Celsius for the temperature control of a constant flow cooling radiant system. For more information on its interpretation, see Cooling High Water Temperature Schedule above.

#### Field: Cooling Low Control Temperature Schedule Name

This field specifies the low control temperature in degrees Celsius for the temperature control of a constant flow cooling radiant system. For more information on its interpretation, see Cooling High Water Temperature Schedule above.

#### Field: Condensation Control Type

When radiant systems do cooling, there is the possibility that condensation will occur on the surface that is being cooled.  This is due to the fact that the surface temperature may drop below the dew-point temperature of the space.  When this occurs, condensation on the surface will occur.  In EnergyPlus, users have several options for handling this situation including: Off and SimpleOff.  When the user chooses the Off option, EnergyPlus will not do anything other than produce a warning message when condensation is predicted to occur.  The program will simply continue on; no moisture will be removed from the zone air and there will be no adjustment of the surface temperature as a result of the condensation.  When the user chooses the SimpleOff option, the program will predict cases where condensation will occur and shut-off the radiant system to avoid this situation.  With this option, the users also have the opportunity to adjust when the system will shut down.  This is specified with the next parameter (field: condensation differential parameter). This parameter is optional and EnergyPlus will use the SimpleOff strategy when this parameter is not specified.

#### Field: Condensation Control Dewpoint Offset

This optional parameter is only valid with the SimpleOff condensation handling algorithm (see previous input parameter).  It establishes the difference between the calculated dew-point temperature of the space and the allowed surface temperature to which the surface can drop before the radiant system shuts down in degrees Celsius.  This parameter can be any positive, negative, or zero value.  When this parameter is zero, the radiant system will shut down when the surface temperature drops to the dew-point temperature or below.  When this parameter is positive, the radiant system will shut down when the surface is the number of degrees Celsius above the dew-point temperature.  This allows some extra safety to avoid condensation.  When this parameter is negative, the radiant system will shut down when the surface temperature is the number of degrees Celsius below the dew-point temperature.  While not recommended, this strategy allows the user to simulate a situation where small amounts of condensation are tolerable.

#### Field: Number of Circuits

This optional input allows the user to choose between modeling each surface in the radiant system as a single hydronic circuit or to allow the program to divide the surface into multiple parallel hydronic circuits based on the next input field *Circuit Length*. To model as a single circuit choose *OnePerSurface*. To model as multiple circuits choose *CalculateFromCircuitLength*. It is recommended that *CalculateFromCircuitLength* be chosen. The default is *OnePerSurface* for backward compatibility with older versions of EnergyPlus.

#### Field: Circuit Length

The length in meters of each parallel hydronic circuit in a surface. Used when the previous input field is set to *CalculateFromCircuitLength*. The default is 106.7 meters (350 feet), which is the maximum circuit length allowed in Title 24.

An example IDF with a constant flow low temperature radiant system is shown below.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:LowTemperatureRadiant:ConstantFlow,
        Resistive Zone Radiant Floor,  !- name of CONSTANT FLOW low temperature radiant system
        RadiantSysAvailSched,  !- availability schedule
        Resistive Zone,  !- Zone name
        Zn001:Flr001,  !- Surface name or group
        0.012,  !- Hydronic tubing inside diameter {m}
        400.0,  !- Hydronic tubing length {m}
        MeanAirTemperature,  !- temperature control type
        0.0004,  !- maximum water volumetric flow rate {m3/s}
        ,  !-schedule for flow rate (optional, non-existent means constant)
        30000, !-Rated Pump Head in Pa
        50,  !-Rated Power Consumption in W
        0.87,  !-Motor Efficiency
        0.1,  !-Fraction of Motor Inefficiencies to Fluid Stream
        Resistive Zone Radiant Water Inlet Node,  !- heating water inlet node
        Resistive Zone Radiant Water Outlet Node,  !- heating water outlet node
        RadHeatHighWaterTemp,  !-high water temperature schedule
        RadHeatLowWaterTemp,   !-low water temperature schedule
        RadHeatHighControlTemp, !-high control temperature schedule
        RadHeatLowControlTemp,  !-low control temperature schedule
        Zone 1 Cooling Water Inlet Node,  !- cooling water inlet node
        Zone 1 Cooling Water Outlet Node,  !- cooling water outlet node
        RadCoolHighWaterTemp, !-cooling high water temperature schedule
        RadCoolLowWaterTemp,  !-cooling low water temperature schedule
        RadCoolHighControlTemp, !- cooling high control temperature schedule
        RadCoolLowControlTemp,  !- cooling low control temperature schedule
        SimpleOff,              !- condensation control type
        0.5,                    !- condensation control dewpoint offset    CalculateFromCircuitLength,   !- Number of Circuits
        106.7;                   !- Circuit Length
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Zone Radiant HVAC Heating Rate [W]
    HVAC,Sum,Zone Radiant HVAC Heating Energy [J]
    HVAC,Average,Zone Radiant HVAC Cooling Rate [W]
    HVAC,Sum,Zone Radiant HVAC Cooling Energy [J]
    HVAC,Average,Zone Radiant HVAC Mass Flow Rate [kg/s]
    HVAC,Average,Zone Radiant HVAC Injection Mass Flow Rate [kg/s]
    HVAC,Average,Zone Radiant HVAC Recirculation Mass Flow Rate [kg/s]
    HVAC,Average,Zone Radiant HVAC Inlet Temperature [C]
    HVAC,Average,Zone Radiant HVAC Outlet Temperature [C]
    HVAC,Average,Zone Radiant HVAC Pump Inlet Temperature [C]
    HVAC,Average,Zone Radiant HVAC Pump Electric Power[W]
    HVAC,Sum,Zone Radiant HVAC Pump Electric Energy [J]
    HVAC,Average,Zone Radiant HVAC Pump Mass Flow Rate [kg/s]
    HVAC,Average,Zone Radiant HVAC Pump Fluid Heat Gain Rate [W]
    HVAC,Sum,Zone Radiant HVAC Pump Fluid Heat Gain Energy [J]
    HVAC,Sum,Zone Radiant HVAC Moisture Condensation Time [s]
    HVAC,Sum,Zone Radiant HVAC Cooling Fluid Heat Transfer Energy [J]
    HVAC,Sum,Zone Radiant HVAC Heating Fluid Heat Transfer Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Zone Radiant HVAC Heating Rate [W]

This field reports the heating input rate to the low temperature radiant system in Watts. This is the heat source to the surface that is defined as the radiant system. The heating rate is determined by the zone conditions and the control scheme defined in the user input.

#### Zone Radiant HVAC Heating Energy [J]

This field reports the heating input to the low temperature radiant system in Joules. This is the heat source to the surface that is defined as the radiant system. The heating rate is determined by the zone conditions, the control scheme defined in the user input, and the timestep.

#### Zone Radiant HVAC Cooling Rate [W]

This field reports the cooling input rate to the low temperature radiant system in Watts. This is the heat sink to the surface that is defined as the radiant system. The cooling rate is determined by the zone conditions and the control scheme defined in the user input.

#### Zone Radiant HVAC Cooling Energy [J]

This field reports the cooling input to the low temperature radiant system in Joules. This is the heat sink to the surface that is defined as the radiant system. The cooling rate is determined by the zone conditions, the control scheme defined in the user input, and the timestep.

#### Zone Radiant HVAC Mass Flow Rate [kg/s]

This field reports the mass flow rate of water through the low temperature radiant system in kilograms per second. This should be identical to the pump flow rate for the system.

#### Zone Radiant HVAC Injection Mass Flow Rate [kg/s]

This field reports the mass flow rate of water that is injected into the radiant system from the main loop. A valve will control the injection and recirculation mass flow rates (see next field) to match the temperature controls specified by the user and dictated by the current simulation conditions.

#### Zone Radiant HVAC Recirculation Mass Flow Rate [kg/s]

This field reports the mass flow rate of water that is recirculated from the radiant system outlet and mixed with the injection flow from the main loop. A valve will control the injection and recirculation mass flow rates (see next field) to match the temperature controls specified by the user and dictated by the current simulation conditions.

#### Zone Radiant HVAC Inlet Temperature [C]

This field reports the temperature of water entering the low temperature radiant system in Celsius. This may differ from the inlet node temperature for the component since this component has its own local secondary loop.

#### Zone Radiant HVAC Outlet Temperature [C]

This field reports the temperature of water leaving the low temperature radiant system in Celsius. This may differ from the outlet node temperature for the component since this component has its own local secondary loop.

#### Zone Radiant HVAC Pump Inlet Temperature [C]

This field reports the temperature of water entering the low temperature radiant system pump in Celsius. This may differ from the inlet node temperature for the component since this component has its own local secondary loop. It is assumed that the pump is upstream of the radiant system.

#### Zone Radiant HVAC Pump Electric Power [W]

This field reports the rate of electric power consumption for the pump which supplies flow to the constant flow radiant system in Watts.

#### Zone Radiant HVAC Pump Electric Energy [J]

This field reports the electric power consumption for the pump which supplies flow to the constant flow radiant system in Joules.

#### Zone Radiant HVAC Pump Mass Flow Rate [kg/s]

This field reports the mass flow rate of water through the low temperature radiant system pump in kilograms per second. This should be identical to the flow rate for the system.

#### Zone Radiant HVAC Pump Fluid Heat Gain Rate [W]

This field reports the rate at which heat is added to the fluid stream as it passes through the pump in Watts. This heat is reflected in the radiant system inlet temperature which will be different from the pump inlet temperature if this field has a non-zero value.

#### Zone Radiant HVAC Pump Fluid Heat Gain Energy [J]

This field reports the amount of heat energy added to the fluid stream as it passes through the pump in Joules. This heat is reflected in the radiant system inlet temperature which will be different from the pump inlet temperature if this field has a non-zero value.

#### Zone Radiant HVAC Moisture Condensation Time [s]

This field reports the amount of time when condensation is occurring.  When using the Off condensation control, this simply reports the amount of time when condensation occurs.  When using the SimpleOff condensation control, this indicates the amount of time when the system has been shut off because of the potential danger of condensation.

#### Zone Radiant HVAC Cooling Fluid Heat Transfer Energy [J]

The heat transfer energy for the cooling fluid connection, in Joules.

#### Zone Radiant HVAC Heating Fluid Heat Transfer Energy [J]

The heat transfer energy for the heating fluid connection, in Joules.

## ZoneHVAC:LowTemperatureRadiant:Electric

This low temperature radiant system (electric) is a component of zone equipment that is intended to model any "radiant system" where electric resistance heating is used to supply energy (heat) to a building surface (wall, ceiling, or floor). The component is controlled by the radiant system controls that are defined in the syntax below and this control does not require the use of a zone thermostat unless the unit is being autosized. Note also that because this unit does not require a thermostat that in cases where no other systems are serving the zone in which this system resides that it will use the heating equipment priority to determine which system will run first.  If the radiant system is serving a zone with forced air equipment, the radiant system will follow the priority order established by the zone thermostat but will still base its response on the controls defined by the user for the radiant system.

The control is accomplished by varying the electrical power supplied to the unit. This model covers either a radiant panel system or wires embedded in entire walls, floors, or ceilings. It is not intended to simulate high temperature electric or gas radiant heaters. Those devices will be handled by a separate model and different input syntax. Low temperature radiant systems that use water flowing through tubes to provide heat to the system should also be defined using separate input syntax (ref: Low Temp Radiant System:Hydronic).

### Inputs

#### Field: Name

This field is an unique user assigned name for an instance of an electric low temperature radiant system. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

This field is the name of the schedule (ref: Schedule) that denotes whether the electric low temperature radiant system can operate during a given time period. A schedule value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for that time period. A value greater than 0 (usually 1 is used) denotes that the unit is available to operate during that time period. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Zone Name

This field is the name of the zone (Ref: [Zone](#zone)) in which the electric low temperature radiant system is principally located and intended to affect. A system that is between two zones will still act upon each zone; however, the zone name referenced here should be the zone that controls the radiant system response.

#### Field: Surface Name or Radiant Surface Group Name

This field is the name of the surface (Ref: BuildingSurface) or surface list (Ref: [ZoneHVAC:LowTemperatureRadiant:SurfaceGroup](#zonehvaclowtemperatureradiantsurfacegroup)) in which the hydronic tubing is embedded/contained. This specification attaches the source or sink from the radiant system to a particular surface and the contribution of the system to the heat balances of that surface. If this field is a surface list, then the source or sink is attached to all of the surfaces in the list with the radiant system surface group defining the breakdown of how flow rate is split between the various surfaces. Only base surfaces (e.g. [BuildingSurface:Detailed](#buildingsurfacedetailed)) are valid. Window/Door surfaces and Internal Mass are not valid surface types for embedded radiant systems.

#### Field: Heating Design Capacity Method

Enter the method used to determine the maximum electrical power (heating design capacity ) or enter the method for scalable sizing the maximum electrical power of low temperature radiant electric unit. Input allowed is either *HeatingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedHeatingCapacity*. If this input field is left blank or zero, then autosizing is assumed. *HeatingDesignCapacity* means user specifies the magnitude of maximum or nominal heating capacity or the program calculates the maximum or nominal design heating capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design heating capacity from user specified heating capacity per floor area and floor area of the zone served by the low temperature radiant electric unit. *FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity. The default method is *HeatingDesignCapacity*.

#### Field: Heating Design Capacity {W}

This field is for the maximum amount of electric energy rate (electric power) converted into heat in low temperature radiant electric unit in watts. This input field is autosizable. The controls for the radiant system will vary the amount of power supplied to the surface between zero input and the maximum power specified in this field as the lower and upper bounds, respectively. Note that if the user elects to autosize this field that a standard zone thermostat such as would be used for a forced air system must be defined as autosizing calculations are based on the zone thermostat value and not on the low temperature radiant electric unit control values. The default input for this input field is autosize.

#### Field: Heating Design Capacity Per Floor Area {W/m2}

Enter the heating capacity per unit floor area in m3/s-m2 of low temperature radiant electric system. This field is required field when the Heating Design Capacity Method is *CapacityPerFloorArea*. The program calculates the heating capacity from floor area of the zone served by the low temperature radiant electric unit and the heating capacity per unit floor area value specified by the user. This field may be left blank.

#### Field: Fraction of Autosized Heating Design Capacity {-}

Enter the heating capacity as a fraction of the autosized heating capacity of low temperature radiant electric unit. This input field is required when the Heating Design Capacity Method is *FractionOfAutosizedHeatingCapacity*. The program calculates the heating capacity from the design autosized heating capacity and user specified fraction. Design day sizing must be specified. This field may be left blank. The default value is 1.0.

#### Field: Temperature Control Type

This field specifies along with the throttling range and setpoint schedules how the user wishes to control the low temperature electric radiant system. The temperature denoted in the setpoint schedule can refer to one of three different temperatures: the zone mean air temperature, the zone mean radiant temperature, the zone operative temperature, the outdoor dry-bulb temperature, or the outdoor wet-bulb temperature. The choice of temperature is controlled by the current field—temperature control type. The user must select from the following options:

~~~~~~~~~~~~~~~~~~~~

    MeanAirTemperature
    MeanRadiantTemperature
    OperativeTemperature
    OutdoorDryBulbTemperature
    OutdoorWetBulbTemperature
~~~~~~~~~~~~~~~~~~~~

Operative temperature for radiant system controls is the average of Mean Air Temperature and Mean Radiant Temperature. If the user does not select a control type, **MeanAirTemperature** control is assumed by EnergyPlus. See the throttling range and control temperature schedule fields below for more information.

#### Field: Heating Throttling Range

This field specifies the range of temperature in degrees Celsuis over which the radiant system throttles from zero heat input via the electric resistance wires up to the maximum defined by the maximum electrical power field described above. The throttling range parameter is used in conjunction with the control temperature (see below) to define the response of the system to various zone conditions. The heating control temperature schedule specifies the "setpoint" temperature where the power input to the system is at half of the maximum power input. For example, if the heating control temperature setpoint is currently 15C and the heating throttling range is 2C, the electrical power supplied to the radiant system will be zero when the controlling temperature (MAT, MRT, Operative Temperature, ODB, or OWB; see control type field above) is at or above 16C and the maximum power input when the controlling temperature is at or below 14C. This represents a throttling range of 2C around the setpoint of 15C. In between 14C and 16C, the power input to the radiant system is varied linearly.

#### Field: Heating Setpoint Temperature Schedule Name

This field specifies the heating setpoint or control temperature for the radiant system in degrees Celsius. Used in conjunction with the previous field (heating throttling range), it will define whether or not the system is running and the current power input to the radiant surface. Power input to the system is varied linearly around the setpoint temperature based on the throttling range and the maximum electrical power parameters (see above). It should be noted that this control schedule will allow different setpoint temperatures throughout the year for heating. In addition, this schedule may be different that the thermostatic control schedule defined for overall operation of components serving the zone in which the radiant system is located. The thermostatic control determines whether or not there is a heating or cooling load in the space and thus whether the systems should be operating. This field simply controls the flow rate to the radiant system.

An example IDF with an electric low temperature radiant system is shown below.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:LowTemperatureRadiant:Electric, Zone 2 Radiant Floor,
             RadiantPanelAvailSched ,    ! Availability schedule
             EAST ZONE ,                 ! Zone name (name of zone system is serving)
             Zn002:Flr001 ,              ! Surface name (name of surface system is embedded in)
             HeatingDesignCapacity,      !- Heating Design Capacity Method
             10000,                      !- Heating Design Capacity{ W }
             ,                           !- Heating Design Capacity Per Floor Area{ W / m2 }
             ,                           !- Fraction of Autosized Heating Design Capacity{ -}
             MeanAirTemperature,         ! control type (control on mean air temperature)
             2.0 ,                       ! heating throttling range (in C)
             Radiant Heating Setpoints ; ! heating setpoint temperatures
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Zone Radiant HVAC Electric Power[W]
    HVAC,Sum, Zone Radiant HVAC Electric Energy [J]
    HVAC,Sum,Zone Radiant HVAC Heating Energy [J]
    HVAC,Average, Zone Radiant HVAC Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Zone Radiant HVAC Electric Power [W]

This field reports the rate at which electric energy is "burned" in the low temperature radiant system in Watts. This is the heat source to the surface that is defined as the radiant system. The heating rate is determined by the zone conditions and the control scheme defined in the user input.

#### Zone Radiant HVAC Electric Energy [J]

This field reports the amount of electric energy "burned" in the low temperature radiant system in Joules. This is the heat source to the surface that is defined as the radiant system. The heating rate is determined by the zone conditions, the control scheme defined in the user input, and the timestep.

#### Zone Radiant HVAC Heating Energy [J]

#### Zone Radiant HVAC Heating Rate [W]

These outputs are the heating provided by the low temperature radiant system to the zone, in Watts or Joules.

## ZoneHVAC:LowTemperatureRadiant:SurfaceGroup

A low temperature radiant system (hydronic or electric) may consist of multiple active surfaces that are serving to condition the space. Surfaces that act serially can be specified as multiple radiant systems using the standard radiant system input described above. However, if the multiple surfaces act in parallet, the Radiant System Surface Group input line is used to specify which surfaces are acting in a coordinated fashion and how flow rate is split between the surfaces. This list of surfaces (the name it is assigned) replaces the name of a single surface in the radiant system input described above. Note that all of the surfaces within a single list must be a part of the same zone and that the zone of these surfaces must also match the zone the radiant system is attempting to condition.

### Inputs

#### Field: Name

This field is an unique user assigned name for the list of surfaces that are acting in coordination with one another. Any reference to this list by a radiant system will use this name.

#### Field Set: Surface Name, Flow Fraction to Surface

The pairs of Surface Name, Flow Fraction to Surface are used in several objects. There can be up to 10 specified. The object is extensible so more can be added.

#### Field: Surface <x> Name

This field is the name of a surface in the zone being conditioned by the radiant system. Only base surfaces (walls, roofs, floors) are valid. Window/Door surfaces and Internal Mass are not valid surface types for embedded radiant systems.

#### Field: Flow Fraction for Surface <x>

This field is the fraction of the total radiant system flow rate that is being sent to this particular surface. Note that the Surface Name/Surface Flow Fraction pair can be repeated up to 10 times. Flow rate fractions must sum to unity, otherwise the program will not accept the input as valid.

An example IDF with an electric low temperature radiant system is shown below.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:LowTemperatureRadiant:SurfaceGroup,
        Zone 1 Radiant Surfaces, !- name of surface list
        Zn001:Flr001,            !- Surface name 1
        0.75,                    !- Flow fraction for surface 1
        Zn001:Roof001,           !- Surface name 2
        0.25;                    !- Flow fraction for surface 2
~~~~~~~~~~~~~~~~~~~~

## ZoneHVAC:HighTemperatureRadiant

The high temperature radiant system (gas-fired or electric) is a component of zone equipment that is intended to model any "high temperature" or "high intensity" radiant system where electric resistance or gas-fired combustion heating is used to supply energy (heat) to a building occupants directly as well as the building surfaces (wall, ceiling, or floor). The component is controlled by the radiant system controls defined in the syntax below and this control does not require the use of a zone thermostat unless the unit is being autosized. Note also that because this unit does not require a thermostat that in cases where no other systems are serving the zone in which this system resides that it will use the heating equipment priority to determine which system will run first.  If the radiant system is serving a zone with forced air equipment, the radiant system will follow the priority order established by the zone thermostat but will still base its response on the controls defined by the user for the radiant system.

The control is accomplished by varying the electrical power supplied to or gas consumed by the unit. It is not intended to simulate low temperature electric or hydronic radiant systems. Those devices will be handled by a separate model and different input syntax (ref: the low temperature systems described elsewhere).

### Inputs

#### Field: Name

This field is an unique user assigned name for an instance of the high temperature radiant system. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

This field is the name of the schedule (ref: Schedule) that denotes whether the high temperature radiant system can operate during a given time period. A schedule value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for that time period. A value greater than 0 (usually 1 is used) denotes that the unit is available to operate during that time period. If this field is left blank, the schedule has a value of 1 for all time periods.

#### Field: Zone Name

This field is the name of the zone (Ref: [Zone](#zone)) in which the high temperature radiant system is principally located and intended to affect.

#### Field: Heating Design Capacity Method

Enter the method used to determine the maximum electrical power (heating design capacity ) or enter the method for scalable sizing the maximum electrical power of high temperature radiant system unit. Input allowed is either *HeatingDesignCapacity*, *CapacityPerFloorArea*, and *FractionOfAutosizedHeatingCapacity*. If this input field is left blank or zero, then autosizing is assumed. *HeatingDesignCapacity* means user specifies the magnitude of maximum or nominal heating capacity or the program calculates the maximum or nominal design heating capacity if autosize is specified. *CapacityPerFloorArea* means the program calculates the design heating capacity from user specified heating capacity per floor area and floor area of the zone served by the high temperature radiant electric unit. *FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity. The default method is *HeatingDesignCapacity*.

#### Field: Heating Design Capacity {W}

This field is for the maximum amount of electric energy rate (electric power) converted into heat in high temperature radiant electric unit in watts. This input field is autosizable. The controls for the radiant system will vary the amount of power supplied to the surface between zero input and the maximum power specified in this field as the lower and upper bounds, respectively. Note that if the user elects to autosize this field that a standard zone thermostat such as would be used for a forced air system must be defined as autosizing calculations are based on the zone thermostat value and not on the radiant system control values. The default input for this field is autosize.

#### Field: Heating Design Capacity Per Floor Area {W/m2}

Enter the heating capacity per unit floor area in m3/s-m2 of high temperature radiant electric unit. This field is required field when the Heating Design Capacity Method is *CapacityPerFloorArea*. The program calculates the heating capacity from floor area of the zone served by the high temperature radiant electric unit and the heating capacity per unit floor area value specified by the user. This field may be left blank.

#### Field: Fraction of Autosized Heating Design Capacity {-}

Enter the heating capacity as a fraction of the autosized heating capacity for high temperature radiant electric unit. This input field is required when the Heating Design Capacity Method is *FractionOfAutosizedHeatingCapacity*. The program calculates the heating capacity from the design autosized heating capacity and user specified fraction. Design day sizing run must be specified. This field may be left blank. The default value is 1.0.

#### Field: Fuel Type

This field denotes the type of high temperature radiant heater: gas or electric. The user must specify this by inputting either the keyword "**NaturalGas**" or "**Electricity**" for this field.

#### Field: Combustion Efficiency

This field is the combustion efficiency for a gas high temperature radiant heater. This value should be greater than 0 and less than or equal to 1. It is intended to take into account any potential inefficiencies in the combustion process inside the radiant heater. The heater gas consumption is the ratio of heater output over the combustion efficiency. This parameter has no meaning for an electric high temperature radiant heater and is ignored when the previous field is equal to "Electric".

#### Field: Fraction of Input Converted to Radiant Energy

This field specifies what fraction of the power input to the high temperature radiant heater is actually radiant heat transfer. The fraction should be between 0 and 1. In conjunction with the next two parameters, it defines the breakdown of how the power input to the heater is distributed to the rest of the zone. The sum of these fractions must be less than or equal to 1. If the fractions are less than one, the remaining energy is added to the zone as convective heat transfer. The radiant heat transfer from the heat is distributed to people and surfaces using the distribution fractions listed below.

#### Field: Fraction of Input Converted to Latent Energy

This field specifies the fraction of the power input to the high temperature radiant heater that is converted to a latent heat gain within the space. This may be appropriate when a combustion process produces moisture that is transferred into the space. The latent heat addition from a high temperature radiant heater is handled as any other latent heat gain within the space, affecting the moisture balance on the zone air.

#### Field: Fraction of Input that Is Lost

This field specifies the fraction of power input to the high temperature radiant heater that is "lost". This energy is a loss term, and this fraction of the input power has no effect on the zone heat balances.

#### Field: Temperature control type

This field specifies along with the throttling range and setpoint schedules how the user wishes to control the high temperature radiant system. The temperature denoted in the setpoint schedule can refer to one of three different temperatures: a mean air temperature, a mean radiant temperature, or an operative temperature. The choice of temperature is controlled by the current field—temperature control type. The user must select from the following options:

~~~~~~~~~~~~~~~~~~~~

    MeanAirTemperature
    MeanRadiantTemperature
    OperativeTemperature
    MeanAirTemperatureSetpoint
    MeanRadiantTemperatureSetpoint
    OperativeTemperatureSetpoint
~~~~~~~~~~~~~~~~~~~~

If the user does not select a control type, OperativeTemperature control is assumed by EnergyPlus. For the setpoint control strategies (those ending in Setpoint above), EnergyPlus will attempt to find the correct heater output to meet the heating setpoint temperature (see below) through iteration and interpolation. This will more closely match the operation of an actual thermostat, but it will require significantly longer execution times.  For more information on the standard piecewise linear control algorithm used by the Mean Air Temperature, Mean Radiant Temperature, and Operative Temperature control types (the "non-Setpoint" controls), see the throttling range and control temperature schedule fields below for more information.

#### Field: Heating Throttling Range

This field specifies the range of temperature in degrees Celsuis over which the radiant system throttles from zero heat input via the electric resistance wires up to the maximum defined by the maximum electrical power field described above. The throttling range parameter is used in conjunction with the control temperature (see below) to define the response of the system to various zone conditions. The heating control temperature schedule specifies the "setpoint" temperature where the power input to the system is at half of the maximum power input. For example, if the heating control temperature setpoint is currently 15C and the heating throttling range is 2C, the electrical power supplied to the radiant system will be zero when the controlling temperature (Mean Air Temperature, Mean Radiant Temperature, and Operative Temperature; see control type field above) is at or above 16C and the maximum power input when the controlling temperature is at or below 14C. This represents a throttling range of 2C around the setpoint of 15C. In between 14C and 16C, the power input to the radiant system is varied linearly.

#### Field: Heating Setpoint Temperature Schedule Name

This field specifies the heating setpoint or control temperature for the radiant system in degrees Celsius. Used in conjunction with the previous field (heating throttling range), it will define whether or not the system is running and the current power input to the radiant surface. Power input to the system is varied linearly around the setpoint temperature based on the throttling range and the maximum electrical power parameters (see above). It should be noted that this control schedule will allow different setpoint temperatures throughout the year for heating. In addition, this schedule may be different that the thermostatic control schedule defined for overall operation of components serving the zone in which the radiant system is located. The thermostatic control determines whether or not there is a heating or cooling load in the space and thus whether the systems should be operating. This field simply controls the flow rate to the radiant system.

#### Field: Fraction of Radiant Energy Incident on People

This field specifies the fraction of radiant heat transfer to the zone from the radiant heater that is incident directly on people within the space. This has an impact on the predicted thermal comfort of the zone occupants. Note that since the thermal comfort models only use the results of the zone heat balances this energy is accounted for within the heat balance as convective energy, directly affecting the zone air heat balance. The basic assumption here is that most radiant energy falling on people will most likely be convected to the surroundings. This is a simplification of reality, but it maintains the overall energy balance.

#### Field Set: Surface Name, Fraction of radiant energy to Surface

The pairs of Surface Name, Fraction of Radiant Energy to Surface are used in several objects. There can be up to 20 specified.

#### Field: Surface <x> Name

This field is the name of the first surface to which radiant heat transfer from the high temperature radiant heater is distributed. Used in conjunction with the next field, it helps to define the distribution of the radiant energy on the surfaces within the zone. Note that up to 20 pairs of surface names and corresponding fractions may be entered for a single radiant heater system.

#### Field: Fraction of Radiant Energy to Surface <x>

This field is paired with the preceding surface name (previous field) to define the fraction of radiant heat transfer leaving the high temperature radiant system that is incident on a particular surface. Users should take into account the directionality of high temperature radiant heaters and their location when defining the value for this input field. Note that the sum of all fractions plus the fraction of radiant energy incident on people must add up to less than or equal to 1.

An example IDF with a high temperature radiant system is shown below.

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:HighTemperatureRadiant, Zone 2 Radiant Heater,
             RadiantPanelAvailSched ,   ! Availability schedule
             EAST ZONE ,                ! Zone name (name of zone system is serving)
             10000,                     !- Heating Design Capacity{ W }
             ,                          !- Heating Design Capacity Per Floor Area{ W / m2 }
             ,                          !- Fraction of Autosized Heating Design Capacity{ -}
             Gas,                       ! type of heater (either gas or electric)
             0.8,                       ! combustion efficiency (ignored for electric radiant heaters)
             0.80,                      ! fraction radiant
             0.00,                      ! fraction latent
             0.00,                      ! fraction lost
             OperativeTemperature,      ! temperature control type (controls on operative temperature)
             2.0 ,                      ! heating throttling range (in C)
             Radiant Heating Setpoints, ! heating setpoint temperatures
             0.04,                      ! fraction of radiant energy that is incident directly on people
             Zn002:Flr001, 0.80,        ! fraction of radiant energy that is incident on the surface indicated
             Zn002:Wall001, 0.04,       ! fraction of radiant energy that is incident on the surface indicated
             Zn002:Wall002, 0.04,       ! fraction of radiant energy that is incident on the surface indicated
             Zn002:Wall003, 0.04,       ! fraction of radiant energy that is incident on the surface indicated
             Zn002:Wall004, 0.04;       ! fraction of radiant energy that is incident on the surface indicated
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Zone Radiant HVAC Heating Rate [W]
    HVAC,Sum, Zone Radiant HVAC Heating Energy [J]
    HVAC,Average, Zone Radiant HVAC Gas Rate [W]
    HVAC,Sum, Zone Radiant HVAC Gas Energy [J]
    HVAC,Average, Zone Radiant HVAC Electric Power[W]
    HVAC,Sum, Zone Radiant HVAC Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Zone Radiant HVAC Heating Rate [W]

This field reports the rate at which energy is transferred from the high temperature radiant system to the zone in Watts. This rate is determined by the zone conditions and the control scheme defined in the user input. Note that this amount takes into account any combustion inefficiency and thus could be different that the gas consumption rate in a high temperature gas radiant system.

#### Zone Radiant HVAC Heating Energy [J]

This field reports the amount of energy transferred from the high temperature gas radiant system to the zone in Joules. This amount is determined by the zone conditions, the control scheme defined in the user input, and the timestep. Note that this amount takes into account any combustion inefficiency and thus could be different that the gas consumption in a high temperature gas radiant system.

#### Zone Radiant HVAC Gas Rate [W]

This field reports the rate at which gas is "burned" in a high temperature gas radiant system in Watts. This rate is determined by the zone conditions and the control scheme defined in the user input.

#### Zone Radiant HVAC Gas Energy [J]

This field reports the amount of gas "burned" in a high temperature gas radiant system in Joules. This amount is determined by the zone conditions, the control scheme defined in the user input, and the timestep.

#### Zone Radiant HVAC Electric Power[W]

This field reports the rate at which electric energy is "burned" in a high temperature electric radiant system in Watts. This rate is determined by the zone conditions and the control scheme defined in the user input.

#### Zone Radiant HVAC Electric Energy [J]

This field reports the amount of electric energy "burned" in a high temperature electric radiant system in Joules. This amount is determined by the zone conditions, the control scheme defined in the user input, and the timestep.

## ZoneHVAC:VentilatedSlab

Ventilated slab systems in general use outdoor air to "precool" slabs with colder nighttime air.  This method of precooling the thermal mass of a space can be very effective when nighttime temperatures are low and the mass of the system is high enough to provide a significant amount of cooling potential during the day to counteract internal heat gains within a zone.  Nearly all ventilated slabs are simple systems such as that shown in the right side of Figure 128.  The fan is shown in a blow through position, but the model will allow either a blow or draw through configuration.

![Ventilated Slab model - basic system](media/ventilated-slab-model-basic-system.jpeg)


### Inputs

#### Field: Name 

This field is a unique user assigned name for an instance of a ventilated slab system. Any reference to this unit by another object will use this name. Other objects that use this ventilated slab system will reference it by this name.

#### Field: Availability Schedule Name

This field is the name of the schedule (ref: Schedule) that denotes whether the ventilated slab system can run during a given time period. A schedule value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for that time period. A value greater than 0 (usually 1 is used) denotes that the unit is available to operate during that time period. If this field is left blank, the schedule has a value of 1 for all time periods.

![Example operating schedule for Ventilated Slab](media/example-operating-schedule-for-ventilated.jpeg)


#### Field: Zone Name

This field is the name of the zone (Ref: [Zone](#zone)) in which the ventilated slab system is principally located and intended to affect. A system that is between two zones will still act upon each zone; however, the zone name referenced here should be the zone that controls the system response.

#### Field: Surface Name or Radiant Surface Group Name

This field is the name of the surface (Ref: Surface) or surface list (Ref: [ZoneHVAC:VentilatedSlab:SlabGroup](#zonehvacventilatedslabslabgroup)) in which the hollow cores are embedded/contained. This specification attaches the source or sinks from the radiant system to a particular surface and the contribution of the system to the heat balances of that surface. If this field is a surface list, then the source or sink is attached to all of the surfaces in the list with the radiant system surface group defining the breakdown of how flow rate is split between the various surfaces. Only base surfaces (Walls, Roofs, Floors) are valid. Window/Door surfaces and Internal Mass are not valid surface types for embedded radiant systems.

#### Field: Maximum Air Flow Rate

This field allows the user to enter the maximum volumetric flow rate of air through the ventilated slab system in m^3^/sec.  This parameter should be some real number greater than zero.

#### Field: Outdoor Air Control Type

This field allows the user to control how outdoor air is used in the ventilated slab system.  The ventilated slab system described by this syntax has its own outdoor air handler.  The three options for outdoor air control are "**VariablePercent**", "**FixedTemperature**" and "**FixedAmount**". Those keywords are the only allowed choices for this parameter.  In general, the variable percent control will attempt to vary the amount of outdoor air between some minimum and maximum schedules of fractions (see next two fields) to best meet the current heating or cooling load. The fixed temperature control will vary the amount of outdoor air between the minimum schedule (fraction of maximum, see next field) and 100% available outdoor air to come as close as possible to a desired mixed air temperature (see two fields down) that can be scheduled. The fixed amount control will fix the outdoor air flow rate as minimum outdoor air flow rate and schedule specified by the user and automatically set the maximum and minimum outside flow rate to be equal by ignoring the maximum outdoor air flow rate. More information on the controls and operation of the ventilated slab are given in the section above (preceding the IDF description).

#### Field: Minimum Outdoor Air Flow Rate

This field allows the user to enter the minimum volumetric flow rate of outdoor air (in m^3^/sec) that will be brought in to the ventilated slab. The actual minimum outdoor air flow rate will be this number multiplied by the schedule value from the minimum outdoor air schedule. If "FixedAmount" type is selected as the outdoor air control strategy, the outdoor air flow rate will be fixed at the value of this field and the ventilated slab will automatically set the maximum and minimum outside flow rate to be equal by ignoring the maximum outdoor air flow rate.

#### Field: Minimum Outdoor Air Schedule Name

This field contains a schedule name (ref: Schedule) that should contain values for the minimum outdoor air used by the ventilated slab system for IAQ or other reasons.  Note that if the ventilated slab is scheduled off or if there is no load sensed in the zone that the system will not operate even to achieve the minimum air fraction.  However, if the system is operating, it will always bring in at least this fraction of the minimum air flow rate (see minimum air flow rate field above). If "FixedAmount" type is selected as the outdoor air control strategy, the actual outdoor air flow rate will be this number multiplied by the minimum outdoor air flow rate in the field above. The ventilated slab will automatically set the maximum and minimum outdoor air schedule to be equal by ignoring the maximum outdoor air schedule.

#### Field: Maximum Outdoor Air Flow Rate

This field allows the user to enter the maximum volumetric flow rate of outdoor air that can be brought into the ventilated slab in m^3^/sec.  This parameter should be some real number greater than zero.  Note that the value for this parameter may be less than the maximum air flow rate of the ventilated slab and this may affect the maximum fraction of outdoor air within the control strategy defined above. This parameter is an absolute maximum and will supercede any scheduled fraction of the ventilated slab maximum airflow rate. If "FixedAmount" type is selected as the outdoor air control strategy, this field will be ignored and be automatically set to be equal to the minimum outdoor air flow rate specified in the field above.

#### Field: Maximum Outdoor Air Fraction or Temperature Schedule Name

This field can have one of two meanings depending the type of control selected in the outdoor air control type parameter above.  If "VariablePercent" or "FixedAmount" was selected, then this field is a schedule name (ref: Schedule) corresponding to a maximum air fraction schedule. Furthermore, if "FixedAmount" type is selected as the outdoor air control strategy, this field will be ignored and be automatically set to be equal to the minimum outdoor air fraction specified in the field below. Note that this is a fraction of the maximum airflow rate field (see parameter above) for the ventilated slab. If "FixedTemperature" control was selected, then this field is still a schedule name (ref: Schedule), but it corresponds to a schedule of mixed air temperatures that the outdoor air control will try to attain.

#### Field: System Configuration Type

This field allows the user to control how the air is circulated using the ventilated slab system.  The options for system configuration are

~~~~~~~~~~~~~~~~~~~~

    SlabOnly
    SlabAndZone
    SeriesSlabs
~~~~~~~~~~~~~~~~~~~~

In the **SlabOnly**, the ventilation air is sent to the slab only and does not enter the zone.  In the **SlabAndZone**, the air first enters the slab and then is delivered to the zone before returning to the system. With the **SeriesSlabs** option, the user specifies a list of slabs ([ZoneHVAC:VentilatedSlab:SlabGroup](#zonehvacventilatedslabslabgroup)). This list determines the order of slabs through which the air passes.  In this option, air is not delivered to any zone.

#### Field: Hollow Core Inside Diameter

This field is the inside diameter of the cores through which air is circulated for the system being defined by this statement.  The inside diameter should be recorded in meters and is used to determine the convective heat transfer from the circulated air to the inside surface of the ventilated slab.

#### Field: Hollow Core Length

This field is the length of core embedded in the surface named above in the surface name field.  In other words, this should be the distance that air travels as it through the slab.  The length of the hollow core in the slab should be entered in meters and is used to determine the effectiveness of heat transfer from the air being circulated through the cores and the core inside surface.  Longer core lengths result in more heat transferred to/from the radiant surface to the circulating fluid.

#### Field: Number of Cores

This field allows the user to specify how many cores there are in the ventilated slab.  Air flow will be divided equally among the different cores.

#### Field: Temperature Control Type

This field specifies along with the throttling range and setpoint schedules how the user wishes to control the ventilated slab system.  The temperature denoted in the set temperature schedule can refer to one of seven different temperatures: the zone mean air temperature, the zone mean radiant temperature, the zone operative temperature, the surface temperature of the ventilated slab, the outdoor dry-bulb temperature, the outdoor wet-bulb temperature, or the dewpoint temperature of zone mean air temperature.  The choice of temperature is controlled by the current field—temperature control type.  The user must select from the following options:

~~~~~~~~~~~~~~~~~~~~

    MeanAirTemperature
    MeanRadiantTemperature
    OperativeTemperature
    OutdoorDryBulbTemperature
    OutdoorWetBulbTemperature
    SurfaceTemperature
    ZoneAirDewPointTemperature
~~~~~~~~~~~~~~~~~~~~

If the user does not select a control type, **MeanAirTemperature** control is assumed by EnergyPlus. See the control temperature schedule fields below for more information.

#### Field: Heating High Air Temperature Schedule Name

This field specifies the high air temperature in degrees Celsius for the temperature control of a ventilated slab system. Air and control temperatures for heating work together to provide a linear function that determines the air temperature sent to the ventilated slab. The current control temperature (see Temperature Control Type above) is compared to the high and low control temperatures at the current time.      If the control temperature is above the high temperature, then the inlet air temperature is set to the low air temperature. If the control temperature is below the low temperature, then the inlet air temperature is set to the high air temperature. If the control temperature is between the high and low value, then the inlet air temperature is linearly interpolated between the low and high air temperature values.

#### Field: Heating Low Air Temperature Schedule Name

This field specifies the low air temperature in degrees Celsius for the temperature control of a ventilated slab. For more information on its interpretation, see Heating High Air Temperature Schedule above.

#### Field: Heating High Control Temperature Schedule Name

This field specifies the high control temperature in degrees Celsius for the temperature control of a ventilated slab. For more information on its interpretation, see Heating High Air Temperature Schedule above.

#### Field: Heating Low Control Temperature Schedule Name

This field specifies the low control temperature in degrees Celsius for the temperature control of a ventilated slab. For more information on its interpretation, see Heating High Air Temperature Schedule above.

#### Field: Cooling High Air Temperature Schedule Name

This field specifies the high air temperature in degrees Celsius for the temperature control of a ventilated slab system. Air and control temperatures for cooling work together to provide a linear function that determines the air temperature sent to the ventilated slab system. The current control temperature (see Temperature Control Type above) is compared to the high and low control temperatures at the current time. If the control temperature is above the high temperature, then the inlet air temperature is set to the low air temperature. If the control temperature is below the low temperature, then the inlet air temperature is set to the high air temperature. If the control temperature is between the high and low value, then the inlet air temperature is linearly interpolated between the low and high air temperature values.

#### Field: Cooling Low Air Temperature Schedule Name

This field specifies the low air temperature in degrees Celsius for the temperature control of a constant flow cooling radiant system. For more information on its interpretation, see Cooling High Air Temperature Schedule above.

#### Field: Cooling High Control Temperature Schedule Name

This field specifies the high control temperature in degrees Celsius for the temperature control of a constant flow cooling radiant system. For more information on its interpretation, see Cooling High Air Temperature Schedule above.

#### Field: Cooling Low Control Temperature Schedule Name

This field specifies the low control temperature in degrees Celsius for the temperature control of a ventilated slab system. For more information on its interpretation, see Cooling High Air Temperature Schedule above.

#### Field: Return Air Node Name

This field is a node name used to identify the node that serves as the zone return air inlet to the ventilated slab system. This node is one of the inlets to the outdoor air mixer which is implicit in the ventilated slab system model. For "SlabAndZone" configuration, the Return Air Node will typically be the same node as a zone exhaust node. For "SlabOnly" or "SeriesSlabs" configuration, this node name is required but will have zero flow.

#### Field: Slab In Node Name

This field is a node name used to identify the node that serves as the inlet of the ventilated slab or series of slabs, after the outdoor air mixer, fan, and optional coils.

#### Field: Zone Supply Air Node Name

This field is a node name used to identify the node that serves as the outlet from the ventilated slab system to the zone when using the "SlabAndZone" configuration. It is the node exiting the slab section of the system. This node will typically be the same node as a zone inlet node. In the case of "SlabOnly" or "SeriesSlabs" configuration, this field will be ignored and it should be left BLANK.

#### Field: Outdoor Air Node Name

This field is a node name used to identify the node associated with fresh outdoor air brought into the ventilated slab system outdoor air mixer.  This node should also be specified in an [OutdoorAir:Node](#outdoorairnode) or [OutdoorAir:NodeList](#outdoorairnodelist) object.

#### Field: Relief Air Node Name

This field is a node name used to identify the node associated with air exhausted out of the ventilated slab system to the outdoor environment.

#### Field: Outdoor Air Mixer Outlet Node Name

This field is a node name used to identify the node associated with the "mixed" air of the ventilated slab.  These conditions are post-"mixing box" since they are the conditions of the fraction of return air combined with the outdoor air.  Since this is a simple system, this can also be viewed as the conditions of the air being sent to the coils.

#### Field: Fan Outlet Node Name

This field is a node name used to identify the node that serves as the air outlet from the fan.

#### Field: Fan Name

This field is the name of a fan (ref: [Fan:ConstantVolume](#fanconstantvolume)) that is part of the ventilated slab system.  This name links the ventilated slab to particular fan data entered elsewhere in the input data file.  A fan name is required since it is the prime mover of air in the ventilated slab system.

#### Field: Coil Option Type

This field allows the user to specify the coil operating options as one of the following options:

~~~~~~~~~~~~~~~~~~~~

    None
    Heating
    Cooling
    HeatingAndCooling
~~~~~~~~~~~~~~~~~~~~

If **None** is selected, the ventilated slab does not have any coils, and any other input will be ignored. If either **Heating** or **Cooling** is selected, only a heating or cooling coil, respectively, is present.  Thus, only four more inputs will be expected. If **HeatingAndCooling** is selected, both heating and cooling coil input must be entered, and the ventilated slab will have both a heating and a cooling coil.

#### Field: Heating Coil Object Type

This field is the type of coil (ref: [Coil:Heating:Water](#coilheatingwater), [Coil:Heating:Electric](#coilheatingelectric), [Coil:Heating:Gas](#coilheatinggas), [Coil:Heating:Steam](#coilheatingsteam)) that is used for heating in the ventilated slab system. This field must be one of the following keywords: [Coil:Heating:Water](#coilheatingwater), [Coil:Heating:Electric](#coilheatingelectric), [Coil:Heating:Gas](#coilheatinggas), [Coil:Heating:Steam](#coilheatingsteam).  It is used in conjunction with the heating coil name (see next field) to specify the heating coil present within the system.

#### Field: Heating Coil Name

This field is the name of the heating coil that is part of the ventilated slab system.  It is assumed that there is always some sort of heating coil associated with a ventilated slab system.  This name links the ventilated slab to particular heating coil data entered elsewhere in the input data file.

#### Field: Hot Water or Steam Inlet Node Name

This field corresponds to the water inlet node to the heating coil for a water coil.  The water inlet node controls how a water heating coil operates.  This field is ignored/not needed for gas and electric heating coils.

#### Field: Cooling Coil Object Type

This field is the name of the cooling coil (ref: [Coil:Cooling:Water](#coilcoolingwater), [Coil:Cooling:Water:DetailedGeometry](#coilcoolingwaterdetailedgeometry), [CoilSystem:Cooling:Water:HeatExchangerAssisted](#coilsystemcoolingwaterheatexchangerassisted)) that is part of the ventilated slab system.  This name links the ventilated slab system to particular cooling coil data entered elsewhere in the input data file. If no cooling coil is present, the previous field may be followed by a semi-colon and the remaining parameters in this statement may be ignored.

#### Field: Cooling Coil Name

This field is the name of the cooling coil that is part of the ventilated slab system.  It is assumed that there is always some sort of cooling coil associated with a ventilated slab system.  This name links the ventilated slab to particular cooling coil data entered elsewhere in the input data file.

#### Field: Cold Water Inlet Node Name

This field corresponds to the water inlet node to the cooling coil.  The water inlet node controls how a water cooling coil operates and is required for the ventilated slab system that has a cooling coil associated with it to function properly.

#### Field: Availability Manager List Name

This optional input field is the name of an [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) object. An Availability Manager Assignment List is a list of Availability Managers giving both Availability Manager type and name. The availability managers in the list apply to this ventilated slab object's fan. If the ventilated slab is available (per the Availability Schedule Name input field above) and this input field has a valid availability manager assignment list name, then the availability managers in the list determine when and if the fan of this ventilated slab object should be on or off.

#### Field: Design Specification ZoneHVAC Sizing Object Name

This optional input field is the name of a DesignSpecificationZoneHVACSizing object. The name must correspond to unique name of a [DesignSpecification:ZoneHVAC:Sizing](#designspecificationzonehvacsizing) object. A Design Sepcification [Zone](#zone) HVAC Sizing object defines scalable sizing methods for sizing input fields such as Maximum Air Flow Rate in this Ventilated Slab zone HVAC object. The scaled Maximum Air Flow Rate in turn is used to size cooling and heating capacity of the coils.

An example IDF with a ventilated slab is shown below.

~~~~~~~~~~~~~~~~~~~~

      ZoneHVAC:VentilatedSlab,
        Zone4VentSlab,           !- Name
        VentSlabAvailability,    !- Availability Schedule Name
        SPACE4-1,                !- Zone Name
        F4-1,                    !- Surface Name or Radiant Surface Group Name
        0.84,                    !- Maximum Air Flow Rate {m3/s}
        VariablePercent,         !- Outdoor Air Control Type
        0.168,                   !- Minimum Outdoor Air Flow Rate {m3/s}
        U2MinOASched,            !- Minimum Outdoor Air Schedule Name
        0.84,                    !- Maximum Outdoor Air Flow Rate {m3/s}
        VentSlabMaxOA,           !- Maximum Outdoor Air Fraction or Temperature Schedule Name
        SlabAndZone,             !- System Configuration Type
        0.050,                   !- Hollow Core Inside Diameter {m}
        15.0,                    !- Hollow Core Length {m}
        50.0,                    !- Number of Cores
        MeanRadiantTemperature,  !- Temperature Control Type
        VentSlabHotHighAir,      !- Heating High Air Temperature Schedule Name
        VentSlabHotLowAir,       !- Heating Low Air Temperature Schedule Name
        VentSlabHotHighControl,  !- Heating High Control Temperature Schedule Name
        VentSlabHotLowControl,   !- Heating Low Control Temperature Schedule Name
        VentSlabCoolHighAir,     !- Cooling High Air Temperature Schedule Name
        VentSlabCoolLowAir,      !- Cooling Low Air Temperature Schedule Name
        VentSlabCoolHighControl, !- Cooling High Control Temperature Schedule Name
        VentSlabCoolLowControl,  !- Cooling Low Control Temperature Schedule Name
        Zone4VentSlabReturnAirNode,  !- Return Air Node Name
        Zone4VentslabSlabInNode, !- Slab In Node Name
        Zone4Inlets,             !- Zone Supply Air Node Name
        Zone4VentSlabOAInNode,   !- Outdoor Air Node Name
        Zone4VentSlabExhNode,    !- Relief Air Node Name
        Zone4VentSlabOAMixerOutletNode,  !- Outdoor Air Mixer Outlet Node Name
        Zone4VentSlabFanOutletNode,  !- Fan Outlet Node Name
        Zone4VentSlabFan,        !- Fan Name
        HeatingAndCooling,       !- Coil Option Type
        Coil:Heating:Electric,   !- Heating Coil Object Type
        Zone4VentSlabHeatingCoil,!- Heating Coil Name
        ,                        !- Hot Water or Steam Inlet Node Name
        Coil:Cooling:Water,      !- Cooling Coil Object Type
        Zone4VentSlabCoolingCoil,!- Cooling Coil Name
        Zone4VentSlabChWInletNode;  !- Cold Water Inlet Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Zone Ventilated Slab Radiant Heating Rate [W]
    HVAC,Sum,Zone Ventilated Slab Radiant Heating Energy [J]
    HVAC,Average,Zone Ventilated Slab Radiant Cooling Rate [W]
    HVAC,Sum,Zone Ventilated Slab Radiant Cooling Energy [J]
    HVAC,Average,Zone Ventilated Slab Coil Heating Rate [W]
    HVAC,Sum,Zone Ventilated Slab Coil Heating Energy [J]
    HVAC,Average,Zone Ventilated Slab Coil Total Cooling Rate [W]
    HVAC,Sum,Zone Ventilated Slab Coil Total Cooling Energy [J]
    HVAC,Average,Zone Ventilated Slab Coil Sensible Cooling Rate [W]
    HVAC,Sum,Zone Ventilated Slab Coil Sensible Cooling Energy [J]
    HVAC,Average,Zone Ventilated Slab Coil Latent Cooling Rate [W]
    HVAC,Sum,Zone Ventilated Slab Coil Latent Cooling Energy [J]
    HVAC,Average, Zone Ventilated Slab Air Mass Flow Rate [kg/s]
    HVAC,Average, Zone Ventilated Slab Fan Electric Power [W]
    HVAC,Sum,Zone Ventilated Slab Fan Electric Energy [J]
    HVAC,Average,Zone Ventilated Slab Inlet Air Temperature [C]
    HVAC,Average,Zone Ventilated Slab Outlet Air Temperature [C]
    HVAC,Average,Zone Ventilated Slab Zone Inlet Air Temperature [C]
    HVAC,Average,Zone Ventilated Slab Return Air Temperature [C]
    HVAC,Average,Zone Ventilated Slab Fan Outlet Air Temperature [C]
    HVAC,Average,Zone Ventilated Slab Fan Availability Status []
~~~~~~~~~~~~~~~~~~~~

#### Zone Ventilated Slab Radiant Heating Rate  [W]

This field reports the radiant heating input rate of the ventilated slab system to the zone it is serving in Watts.  This is determined by outlet and zone air conditions and the mass flow rate through the ventilated slab system.

#### Zone Ventilated Slab Radiant Heating Energy [J]

This field is the heating radiant input of the ventilated slab system to the zone it is serving in Joules over the timestep being reported.  This is determined by outlet and zone air conditions, the mass flow rate through the ventilated slab system, and the timestep.

#### Zone Ventilated Slab Radiant Cooling Rate [W]

This field reports the radiant cooling input rate to the ventilated slab system in Watts. This is the heat sink to the surface that is defined as the ventilated slab system. The cooling rate is determined by the zone conditions and the control scheme defined in the user input.

#### Zone Ventilated Slab Radiant Cooling Energy [J]

This field reports the radiant cooling input to the ventilated slab system in Joules. This is the heat sink to the surface that is defined as the radiant system. The cooling rate is determined by the zone conditions, the control scheme defined in the user input, and the timestep.

#### Zone Ventilated Slab Coil Heating Rate [W]

This field reports the heating input rate of the heating coil of ventilated slab system the zone it is serving in Watts.  This is determined by return air and zone air conditions and the mass flow rate through the ventilation slab system.

#### Zone Ventilated Slab Coil Heating Energy [J]

This field is the heating output of the heating coil of the ventilated slab system the zone it is serving in Joules over the timestep being reported.  This is determined by return air and zone air conditions, the mass flow rate through the ventilation slab system, and the timestep.

#### Zone Ventilated Slab Coil Total Cooling Rate [W]

This field reports the total cooling (sensible plus latent) output rate of the cooling coil of the ventilated slab system to the zone it is serving in Watts.  This is determined by outlet and zone air conditions and the mass flow rate through the ventilation slab system.

#### Zone Ventilated Slab Coil Total Cooling Energy [J]

This field is the total cooling (sensible plus latent) output of the cooling coil of the ventilated slab system to the zone it is serving in Joules over the timestep being reported.  This is determined by outlet and zone air conditions, the mass flow rate through the ventilation slab system, and the timestep.

#### Zone Ventilated Slab Coil Sensible Cooling Rate [W]

This field reports the sensible cooling output rate of the cooling coil of the ventilated slab system to the zone it is serving in Watts.  This is determined by outlet and zone air conditions and the mass flow rate through the ventilation slab system.

#### Zone Ventilated Slab Coil Sensible Cooling Energy [J]

This field is the sensible cooling output of the cooling coli of the ventilated slab system to the zone it is serving in Joules over the timestep being reported.  This is determined by outlet and zone air conditions, the mass flow rate through the ventilation slab system, and the timestep.

#### Zone Ventilated Slab Coil Latent Cooling Rate [W]

This field reports the latent cooling output rate of the cooling coil of the ventilated slab system to the zone it is serving in Watts.  This is determined by outlet and zone air conditions and the mass flow rate through the ventilation slab system.

#### Zone Ventilated Slab Coil Latent Cooling Energy [J]

This field is the latent cooling output of the cooling coli of the ventilated slab system to the zone it is serving in Joules over the timestep being reported.  This is determined by outlet and zone air conditions, the mass flow rate through the ventilation slab system, and the timestep.

#### Zone Ventilated Slab Air Mass Flow Rate [kg/s]

This field reports the mass flow rate of air through the ventilated slab system in kilograms per second.

#### Zone Ventilated Slab Fan Electric Power [W]

This field reports the electric power consumption rate of the fan of the ventilated slab system in Watts.

#### Zone Ventilated Slab Fan Electric Energy [J]

This field reports the electric power consumed by the fan of the ventilated slab system over the timestep in Joules.

#### Zone Ventilated Slab Inlet Air Temperature [C]

This field reports the temperature of air entering the ventilated slab system in Celsius.

#### Zone Ventilated Slab Outlet Air Temperature [C]

This field reports the temperature of air leaving the ventilated slab system in Celsius.

#### Zone Ventilated Slab Zone Inlet Air Temperature [C]

This field reports the temperature of air entering the zone in Celsius.

#### Zone Ventilated Slab Return Air Temperature [C]

This field reports the temperature of air leaving the zone in Celsius. When system does not circulate air to zone("SlabOnly" Configuration), the slab outlet temperature and return air temperature will be the same.

#### Zone Ventilated Slab Fan Outlet Air Temperature [C]

This field reports the fan outlet air temperature for the ventilated slab system in Celsius.

#### Zone Ventilated Slab Fan Availability Status []

This is the availability status of the ventilated slab fan. This status flag is a result of the calculations made by the Availability Manager(s) listed in an [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) object and/or calculations made by Hybrid Ventilation Manager object. The [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) is an optional input in the ventilated slab object. When a single availability manager is used in an Availability Manager Assignment List, this is also the availability status reported by the specific availability manager (Ref. AvailabilityManager:\* Outputs). For multiple availability managers in an Availability Manager Assignment List along with Hybrid Ventilation Manager, rules to determine fan availability status are described in the section ‘Group – System Availability Managers'. The control status outputs are represented using integers 0 through 3. These integers represent NoAction (0), ForceOff (1), CycleOn (2), and CycleOnZoneFansOnly (3). Since the status output is averaged, the output result may not correspond to the values described here when output variable frequencies other than detailed are used. Use the "detailed" reporting frequency (Ref. [Output:Variable](#outputvariable) object) to view the availability status at each simulation timestep.

## ZoneHVAC:VentilatedSlab:SlabGroup

A ventilated slab system may consist of multiple active slabs that are serving to condition the zone. Slabs that act serially can be specified as multiple radiant systems using the standard ventilated slab input described above. This list of surfaces (the name it is assigned) replcaces the name of a single surface in the ventilated slab system input described above.

### Inputs

#### Field : Name of Ventilated Slab Surface Group

This field is a unique user assigned name for the list of surfaces that are acting in coordination with one another. Any reference to this list by a ventilated slab system will use this name.

#### Field: Zone Name

This field is the name of the zone in which the surface is principally located and intended to affect.

#### Field: Surface Name

This field is the name of a surface in the zone being conditioned by the ventilated slab system. Only base surfaces like walls, floors and roofs are valid. Door/Window Surface and Internal Mass are not valid surface types for the ventilated slab system.

#### Field: Core Diameter

This field is the inside diameter of the cores through which air is circulated for the surface being defined by this statement.  The inside diameter should be recorded in meters and is used to determine the convective heat transfer from the circulated air to the inside surface of the ventilated slab.

#### Field: Core length

This field is the length of core embedded in the surface named above in the surface name field.  In other words, this should be the distance that air travels as it through the slab.  The length of the hollow core in the surface should be entered in meters and is used to determine the effectiveness of heat transfer from the air being circulated through the cores and the core inside surface.  Longer core lengths result in more heat transferred to/from the radiant surface to the circulating fluid.

#### Field: Number of Cores

This field allows the user to specify how many cores there are in the ventilated slab.  Air flow will be divided equally among the different cores.

#### Field: Slab Inlet Node Name

This field is a node name (character string) used to identify the node that serves as the inlet (air side) to the surface.  In EnergyPlus, nodes represent points between components or at various points in the loops.  While a node name may be referenced more than once in an input data file, each node must have a unique name.

#### Field: Slab Outlet Node Name

This field is a node name (character string) used to identify the node that serves as the outlet (air side) of the surface.  In EnergyPlus, nodes represent points between components or at various points in the loops.  While a node name may be referenced more than once in an input data file, each node must have a unique name.

An Example IDF with a ventilated slab system is shown below

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:VentilatedSlab:SlabGroup,
        Z125,                    !- Name
        SPACE1-1,                !- Zone 1 Name
        C1-1,                    !- Surface 1 Name
        0.05,                    !- Core Diameter for Surface 1
        30,                      !- Core Length for Surface 1
        20,                      !- Core Numbers for Surface 1
        Z1VentslabIn,            !- Slab In Node Name for Surface 1
        Z1VentSlabout,           !- Slab Outlet Node Name for Surface 1
        SPACE2-1,                !- Zone 2 Name
        C2-1,                    !- Surface 2 Name
        0.05,                    !- Core Diameter for Surface 2
        15,                      !- Core Length for Surface 2
        20,                      !- Core Numbers for Surface 2
        Z2VentSlabIn,            !- Slab In Node Name for Surface 2
        Z2VentSlabOut,           !- Slab Outlet Node Name for Surface 2
        SPACE5-1,                !- Zone 3 Name
        C5-1,                    !- Surface 3 Name
        0.05,                    !- Core Diameter for Surface 3
        30,                      !- Core Length for Surface 3
        20,                      !- Core Numbers for Surface 3
        Z5VentSlabIn,            !- Slab In Node Name for Surface 3
        Z5VentSlabOut;           !- Slab Outlet Node Name for Surface 3
~~~~~~~~~~~~~~~~~~~~