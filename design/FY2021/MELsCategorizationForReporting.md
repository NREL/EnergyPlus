MELs Categorization for Reporting
================

**Yujie Xu, Xuan Luo, Tianzhen Hong**

**Lawrence Berkeley National Laboratory**

 - Original Date: April 15, 2021
 - Modified Date: May 6, 2021

## Justification for New Feature ##
Energy use by miscellaneous electric loads (MELs) has become a growing portion of energy consumption in residential and commercial buildings in the U.S. and worldwide. Currently,  EnergyPlus reports aggregated plug loads (under General end-use subcategory) without a way to break it down into major subcategories such as set-top boxes, TVs, PCs, etc. reported in the National Energy Modeling System (NEMS) [1]. This new feature proposal will implement end-use subcategorization to enable detailed MELs energy calculations and reporting in EnergyPlus to support GEBs research and application of tools such as Scout. The MELs categorization will be standardized based on previous research, DOE appliances standards, building energy codes and standards, as well as their significance in total energy use.

## Overview ##
EnergyPlus, up to version 9.5, reports major energy uses as shown in Table 1 where MELs are aggregated into the Interior Equipment end-use category.

|                    | Electricity [GJ] | Natural Gas [GJ] |
|--------------------|------------------|------------------|
| Heating            | 0.00             | 5293.02          |
| Cooling            | 1842.85          | 0.00             |
| Interior Lighting  | 5636.33          | 0.00             |
| Exterior Lighting  | 805.32           | 0.00             |
| Interior Equipment | 8384.16          | 0.00             |
| Exterior Equipment | 1865.08          | 0.00             |
| Fans               | 615.79           | 0.00             |
| Pumps              | 896.10           | 0.00             |
| Heat Rejection     | 486.64           | 0.00             |
| Humidification     | 0.00             | 0.00             |
| Heat Recovery      | 0.00             | 0.00             |
| Water System       | 0.00             | 246.60           |
| Refrigeration      | 0.00             | 0.00             |
| Generators         | 0.00             | 0.00             |
| Total End Uses     | 20532.27         | 5539.62          |

<p style="text-align: center;">Table 1 Major end-uses reported in EnergyPlus version 9.5</p>

The default subcategory Interior Equipment | General (Table 2) does not provide a breakdown of MELs end uses.

| End-use Category   | End-use Subcategory      | Electricity [GJ] | Natural Gas [GJ] |
|--------------------|--------------------------|------------------|------------------|
| Heating            | General                  | 0.00             | 0.00             |
|                    | Boiler                   | 0.00             | 5293.02          |
|                    | Boiler Parasitic         | 0.00             | 0.00             |
| Cooling            | General                  | 1842.85          | 0.00             |
| Interior Lighting  | General                  | 5636.33          | 0.00             |
| Exterior Lighting  | Exterior Façade Lighting | 805.32           | 0.00             |
| Interior Equipment | General                  | 8384.16          | 0.00             |
| Exterior Equipment | Elevators                | 1865.08          | 0.00             |
| Fans               | Fan Energy               | 615.79           | 0.00             |
| Pumps              | General                  | 896.10           | 0.00             |
| Heat Rejection     | General                  | 486.64           | 0.00             |
| Humidification     | General                  | 0.00             | 0.00             |
| Heat Recovery      | General                  | 0.00             | 0.00             |
| Water Systems      | General                  | 0.00             | 0.00             |
|                    | Water Heater             | 0.00             | 246.60           |
| Refrigeration      | General                  | 0.00             | 0.00             |
| Generators         | General                  | 0.00             | 0.00             |

<p style="text-align: center;">Table 2 EnergyPlus report end-uses by categories and subcategories</p>

Based on the above justifications, we propose to add a categorization for MELs which cover major plug loads in residential and commercial buildings.

## Approach ##
To enable the reporting of MELs in EnergyPlus, the ElectricEquipment will be augmented with more refined end-use subcategories: the field End-Use Subcategory (A5) will change from alpha to choice types, including major residential and commercial building electrical equipment categories.

```
ElectricEquipment,
  \memo Sets internal gains for electric equipment in the zone.
  ...
  N6 , \field Fraction Lost
  A5 , \field End-Use Subcategory
       \type choice
       \key General
       \key CeilingFan 
       \key ClothesDryer 
       \key ClothesWasher
       \key CoffeeMaker 
       \key Cooktop
       \key DesktopPC
       \key DiscPlayer
       \key Elevator
       \key Escalator
       \key FaxMachine
       \key Fitness 
       \key Freezer
       \key GameConsole
       \key HomeTheater
       \key Kettle
       \key Laptop
       \key MedicalDevice 
       \key Microwave
       \key Monitor
       \key NetworkEquipment
       \key Oven
       \key PortableFan
       \key Printer
       \key Refrigerator
       \key RiceCooker
       \key SecuritySystem 
       \key SettopBox
       \key Telephone
       \key TV
       \key VaccumCleaner
       \key VideoDisplay
       \key WineCooler
       \key UserDefined
       \retaincase
       \default General
A6 ;   \field User-defined End-Use Subcategory
       \note when the End-Use Subcategory is UserDefined , A6 - User-defined End-Use Subcategory is required.
       \note Any text may be used here to categorize the end-uses in the ABUPS End Uses by Subcategory table.
       \type alpha
       \retaincase
```

## Testing/Validation/Data Source(s): ##
Two new example files built upon the existing files, *SingleFamilyHouse_HP_Slab.idf* and *RefBldgLargeOfficeNew2004_Chicago.idf*, will be created to demonstrate the use of the new feature. Simulation results will be manually checked.

## IDD Object (New): ##
N/A

## IDD Object(s) (Revised): ##
N/A

## Proposed additions to Meters: ##
N/A

## Proposed Report Variables: ##
N/A

## Email Discussion ##
The feature and implementation details will be discussed at one of the EnergyPlus Technicality calls.

## References ##
[1]	U.S. Energy Information Administration, "Analysis and Representation of Miscellaneous Electric Loads in NEMS," 2017. [Online]. Available: https://www.eia.gov/analysis/studies/demand/miscelectric/pdf/miscelectric.pdf.
