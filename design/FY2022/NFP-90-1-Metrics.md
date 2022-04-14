Update output reporting of mechanical equipment using 90.1-2019 metrics
================

**Tobias Maile, Richard See**

**Digital Alchemy**

 - Original Date: April 14, 2022
 - Updated Date: XXX, 2022

## Justification for New Feature ##

Many new metrics appear in the ASHRAE 90.1-2019 [1] standard. EnergyPlus needs to stay in sync with new metrics and thus needs to be updated. 

## Overview ##

We have identified the following commonly used metrics:

| Metric | Standard it is used in | Description | Components | URL |
|--|--|--|--|--|
|SEER2|ASHRAE 90.1-2019|new Seasonal Energy Efficiency Ratio|Air Conditioner Condensing Units<br>Heat Pumps<br>Single Packaged Units<br>Evaporator Coils<br>Gas Furnaces|[2]|
|HSPF2 (AHRI 210/240)|ASHRAE 90.1-2019|new Heating Seasonal Performance Factor|Unitary Air-source Heat Pump|[3]|
|IEER (AHRI 340/360)|ASHRAE 90.1-2019|Integrated Energy Efficiency Ratio|Commercial and Industrial Unitary Air-conditioning and Heat Pump|[4]|
|CEER (ANSI/AHAM RAC-1)|ASHRAE 90.1-2019|Combined Energy Efficiency Ratio|Household Room Air Conditioners|[5]|
|MRE (AHRI 910)|ASHRAE 90.1-2019|Moisture Removal Efficiency|Indoor Pool Dehumidifiers |[6]|
|ISMRE|ASHRAE 90.1-2019|Integrated Seasonal Moisture Removal Efficiency|DOAS Specification|[7]|
|ISCOP (AHRI 920)|ASHRAE 90.1-2019|Integrated Seasonal Coefficient of Performance|Direct Expansion-Dedicated Outdoor Air System Units|[link](www.ahrinet.org/App_Content/ahri/files/STANDARDS/AHRI/AHRI_Standard_920_I-P_2020.pdf&usg=AOvVaw2BiFZqovmIGDJ0tSxvJACo)|
|UEF (10 CFP 430 Appendix E)|ASHRAE 90.1-2019|Uniform Energy Factor|Water Heaters|[link](https://www.govinfo.gov/app/details/CFR-2016-title10-vol3/CFR-2016-title10-vol3-part430-subpartB-appE)|
|FEI (AMCA 208)|ASHRAE 90.1-2019|Fan Energy Index|Fan Energy|[link](www.amca.org/news/press-releases/ansi/amca-standard-208%2C-calculation-of-the-fan-energy-index%2C-available-for-free-download.html&usg=AOvVaw2eZxHxgdq183Y9WsZlgjTB)|

In the current workorder, we would focus on two metrics only and propose to focus on heat pump related metrics: HSPF2 and IEER.

1. HSPF2

2. IEER

## Approach ##

For each of the two metrics, we define EnergyPlus components that the metric applies to. Replicate the code patter of existing metrics for the new metric and write unit tests to ensure the calculation is correct. Once the metrics are calculated we can integrate them into the correpsonding report code of the equipment summary table. 

## Testing/Validation/Data Source(s) ##

Unit tests will confirm and test the calculation of the metrics to ensure the code produces correct results and does not fail. 
Two example files will be modified to demonstrate the use of the new metrics. Simulation results will be manually checked/benchmarked using excel spreadsheet with input and output from EnergyPlus runs.

## Input Output Reference Documentation ##

The InputOutput Documentation will be expanded to include the new metrics. There will be an update in two places, first on the related section of the EnergyPlus components for which the metric is calculated and second in the reports section, specifically the Equipment Summary report. 

## Input Description ##

There is no additional input needed. 

## Outputs Description ##

There will be an update to the reports to include the metrics as desribed above. 

## Engineering Reference ##

The EngeineeringReference will be updated to include the formulas used to calculate the two metrics and be referenced from the corresponding EnergyPlus components. 

## Example Files and Transition Changes ##

No transition change is required.

## E-mail and  Conference Call Conclusions ##

TBD

## Acknowledgments ##

TBD 

## References ##

[1]	ASHRAE, ANSI/ASHRAE/IES Standard 90.1-2019: Energy Standard for Buildings Except Low-Rise Residential Buildings. 2019.

[2]	Jonestone Supply: The SEER2 Guide. 2022. URL: https://seer2.com/

[3] AHRI: Air Conditioning, Heating & Refrigeration Institute. 2023 (2020) Standard for Performance Rating of Unitary Air-conditioning & Air-source Heat Pump 
Equipment. 2020. URL: https://www.ahrinet.org/Portals/_Appleseed/documents/Standards/AHRI%20Standard%20210.240-2023%20(2020).pdf

[4] AHRI: AHRI Standard 340/360-2022 (I-P): 2022 Standard for Performance Rating of Commercial and Industrial Unitary Air-conditioning and Heat Pump Equipment. 2019. URL: https://www.ahrinet.org/Portals/Standards/AHRI%20Standard%20340-360-2022%20(I-P).pdf

[5] ANSI/AHAM: ANSI/AHAM RAC-1-2015: Room Air Conditioners. 2015. https://webstore.ansi.org/standards/aham/ansiahamrac2015

[6] ANSI/AHAM: ANSI/AHRI 910-I-P-2014: Performance Rating of Indoor Pool Dehumidifiers. 2014. http://www.ahrinet.org/App_Content/ahri/files/STANDARDS/AHRI/AHRI_Standard_910_I-P_2014.pdf

[7] HPAC Engineering. Understanding AHRI 920 and ISMRE Ratings for Most Efficient DOAS Specification. 2018. https://www.hpac.com/columns/managing-facilities/article/20929562/understanding-ahri-920-and-ismre-ratings-for-most-efficient-doas-specification
