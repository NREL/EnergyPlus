# eplusout.mdd

This file (from the **Output:VariableDictionary,regular;** and **Output:VariableDictionary,IDF;** commands) shows all the report meters along with their "availability" for the current input file. A user must first run the simulation (at least semi-successfully) before the available output meters are known. This output file is available in two flavors: **regular** (listed as they are in the Input Output Reference) and **IDF** (ready to be copied and pasted into your Input File). Once this file is generated, the IDF editor can use them for Output:Meter and Output:CumulativeMeter keys.

Note that meters are always accumulated to the Zone timestep. They are always "summed" variables.

## Meter Variables – regular format

~~~~~~~~~~~~~~~~~~~~

    Program Version,EnergyPlus <version>, <date time of run>,IDD_Version <IDD Version>
    Var Type (reported timestep),Var Report Type,Variable Name [Units]
    Zone,Meter,Electricity:Facility [J]
    Zone,Meter,Electricity:Building [J]
    Zone,Meter,Electricity:Zone:SOUTHZONE_1STFLOOR [J]
    Zone,Meter,InteriorLights:Electricity [J]
    Zone,Meter,InteriorLights:Electricity:Zone:SOUTHZONE_1STFLOOR [J]
    Zone,Meter,GeneralLights:InteriorLights:Electricity [J]
    <reduced>
~~~~~~~~~~~~~~~~~~~~

Note that custom meters also appear on this file:

~~~~~~~~~~~~~~~~~~~~

    Zone,Meter,MYGENERALLIGHTS [J]
    Zone,Meter,MYBUILDINGELECTRIC [J]
    Zone,Meter,MYBUILDINGOTHER [J]
~~~~~~~~~~~~~~~~~~~~

## Meter Variables – regular format – sorted by name

The meter variable file can also be sorted by name. If the rdd is sorted by name, then the meter mdd file will be as well.

~~~~~~~~~~~~~~~~~~~~

    Program Version,EnergyPlus <version>, <date time of run>,IDD_Version <IDD Version>
    Var Type (reported time step),Var Report Type,Variable Name [Units]
    Zone,Meter,Carbon Equivalent:Facility [kg]
    Zone,Meter,CarbonEquivalentEmissions:Carbon Equivalent [kg]
    Zone,Meter,Cogeneration:ElectricityNet [J]
    Zone,Meter,Cogeneration:ElectricityPurchased [J]
    Zone,Meter,Cogeneration:ElectricitySurplusSold [J]
    <snip>
~~~~~~~~~~~~~~~~~~~~

## Meter Variables – IDF format

This report produces the variable names in a form that are ready to be merged into an input file. By design, hourly reporting is selected – replace this with your desired frequency. An example of the results:

~~~~~~~~~~~~~~~~~~~~

    ! Program Version, <version>, <date/time of run>, IDD_Version <IDD version>
    ! Output:Meter Objects (applicable to this run)
    Output:Meter,DistrictHeating:Facility,hourly; !- [J]
    Output:Meter:Cumulative,DistrictHeating:Facility,hourly; !- [J]
    Output:Meter,DistrictHeating:HVAC,hourly; !- [J]
    Output:Meter:Cumulative,DistrictHeating:HVAC,hourly; !- [J]
    Output:Meter,Heating:DistrictHeating,hourly; !- [J]
    Output:Meter:Cumulative,Heating:DistrictHeating,hourly; !- [J]
    Output:Meter,DistrictCooling:Facility,hourly; !- [J]
    Output:Meter:Cumulative,DistrictCooling:Facility,hourly; !- [J]
    Output:Meter,DistrictCooling:HVAC,hourly; !- [J]
    Output:Meter:Cumulative,DistrictCooling:HVAC,hourly; !- [J]
~~~~~~~~~~~~~~~~~~~~

## Meter Naming Convention

To repeat some of the detail described in the Input Output Reference document:

Meter names are of three forms:

- <ResourceType>:<name>

Or

- <EndUseType>:<ResourceType>:<name>

Or

- <EndUseSubcategory>:<EndUseType>:<ResourceType>:<name>.

The user requests reporting by either specifying the full name above (without Units) or a "wildcard" representation of that name (to obtain all meters of a type). For example, entering "Electricity:\*" for the meter name will report on all the Electricity: meters (Electricity:Facility, Electricity:Building, Electricity:Zone:Resistive Zone, Electricity:Zone:East Zone, Electricity:North Zone in the example above). Both the resource types and end-use types are set within the program by the developers. End-use subcategories are user-defined and are an input field for certain objects.

To summarize the previous paragraph, you could use:

~~~~~~~~~~~~~~~~~~~~

    Output:Meter,Electricity:*;
~~~~~~~~~~~~~~~~~~~~

To get the same outputs as if you had entered:

~~~~~~~~~~~~~~~~~~~~

    Output:Meter,Electricity:Facility;
    Output:Meter,Electricity:Building;
    Output:Meter,Electricity:Zone:South Zone;
    Output:Meter,Electricity:Zone:East Zone;
    Output:Meter,Electricity:North Zone;
~~~~~~~~~~~~~~~~~~~~

From a three zone file that had no HVAC electricity (i.e., had only electric equipment or lighting equipment in an uncontrolled three zone building).

Current resource types are shown in the table below:

Table: Table of Metered Resource  Types

**Utility/Fuel Types**
-----------------------------------
Electricity|Gas
Gasoline|Diesel
Coal|FuelOil#1
FuelOil#2|Propane
Water|Steam
DistrictCooling|DistrictHeating

Other Resource Types
--------------------
EnergyTransfer

The end use types are shown in the following table (note that certain end use types apply only to the EnergyTransfer resource):

The end use types are shown in the following table:

Table: End Use Category Types

**End Use Types**
------------------------------
InteriorLights|ExteriorLights
InteriorEquipment|ExteriorEquipment
Fans|Pumps
Heating|Cooling
HeatRejection|Humidifier
HeatRecovery|DHW
Cogeneration|Refrigeration
Miscellaneous|

**Additional End Use Types Only Used for EnergyTransfer**
----------------------------------------------------------------------
HeatingCoils|CoolingCoils
Chillers|Boilers
Baseboard|HeatRecoveryForCooling
HeatReoveryFor Heating|

Specific meter types are then used for grouping the fuel type meters:

Table: Overall Meter Types

Meters
------
Facility
Building
Zone
System
Plant

Facility meters contain all the energy of a fuel type. Building meters contain the sum of each zone's energy. System meters contain the energy from the system components. Plant meters contain the energy from the plant equipments.

Thus, the following relationships should be observed:

![](media/image14.png)\


![](media/image15.png)\


Custom Meters, (review Input Output Reference, objects: **Meter:Custom** and **Meter:CustomDecrement**) for further requirements, are reported in the same way as pre-defined meters.

For example, one might put the Building Infiltration Heat Loss & Heat Gain on a set of custom meters:

~~~~~~~~~~~~~~~~~~~~
      Meter:Custom,
        Building Infiltration Heat Loss,  !- Name
        Generic,             !- Fuel Type
        *,                       !- Key Name 1
        Zone Infiltration Total Heat Loss Energy;  !- Output Variable Name 1

      Meter:Custom,
        Building Infiltration Heat Gain,  !- Name
        Generic,             !- Fuel Type
        *,                       !- Key Name 1
        Zone Infiltration Total Heat Gain Energy;  !- Output Variable Name 1
~~~~~~~~~~~~~~~~~~~~

One can then report these values the same way one reports other standard meters.
