Update output reporting of mechanical equipment using 90.1-2019 metrics
================

**Tobias Maile, Richard See**

**Digital Alchemy**

 - Original Date: April 14, 2022
 - Updated Date: XXX, 2022

## Justification for New Feature ##

Many new metrics appear in the ASHRAE 90.1-2019 [1] standard. EnergyPlus needs to be updated to support the most commonly used metrics so they are in sync with component characteristics. 

## Overview ##

We have identified the following commonly used metrics:

| Metric | Standard it is used in | Description | Components | URL |
|--|--|--|--|--|
|SEER2|ASHRAE 90.1-2019|new Seasonal Energy Efficiency Ratio|DX Cooling Coils |[2]|
|HSPF2 (AHRI 210/240)|ASHRAE 90.1-2019|new Heating Seasonal Performance Factor|Unitary Air-conditioning & Air-source Heat Pump|[link](http://www.ahrinet.org/App_Content/ahri/files/STANDARDS/AHRI/AHRI_Standard_210-240_2023.pdf&usg=AOvVaw1nLQ-YsmNgVqlloIRv7qYh)|
|IEER (AHRI 340/360)|ASHRAE 90.1-2019|Integrated Energy Efficiency Ratio|Commercial and Industrial Unitary Air-conditioning and Heat Pump|[link](www.ahrinet.org/App_Content/ahri/files/STANDARDS/AHRI/AHRI_Standard_340-360_I-P_2019.pdf&usg=AOvVaw0n8T9FYjFj17R72x0KC-_2)|
|CEER (ANSI/AHAM RAC-1)|ASHRAE 90.1-2019|Combined Energy Efficiency Ratio|Household Room Air Conditioners|[link](https://webstore.ansi.org/standards/aham/ansiahamrac2015)|
|MRE (AHRI 910)|ASHRAE 90.1-2019|Moisture Removal Efficiency|Indoor Pool Dehumidifiers |[link](www.ahrinet.org/App_Content/ahri/files/STANDARDS/AHRI/AHRI_Standard_910_I-P_2014.pdf&usg=AOvVaw2VWgKdE6MWerggvPPkiidu)|
|ISMRE|ASHRAE 90.1-2019|Integrated Seasonal Moisture Removal Efficiency|DOAS Specification|[link](https://www.hpac.com/columns/managing-facilities/article/20929562/understanding-ahri-920-and-ismre-ratings-for-most-efficient-doas-specification)|
|ISCOP (AHRI 920)|ASHRAE 90.1-2019|Integrated Seasonal Coefficient of Performance|Direct Expansion-Dedicated Outdoor Air System Units|[link](www.ahrinet.org/App_Content/ahri/files/STANDARDS/AHRI/AHRI_Standard_920_I-P_2020.pdf&usg=AOvVaw2BiFZqovmIGDJ0tSxvJACo)|
|UEF (10 CFP 430 Appendix E)|ASHRAE 90.1-2019|Uniform Energy Factor|Water Heaters|[link](https://www.govinfo.gov/app/details/CFR-2016-title10-vol3/CFR-2016-title10-vol3-part430-subpartB-appE)|
|FEI (AMCA 208)|ASHRAE 90.1-2019|Fan Energy Index|Fan Energy|[link](www.amca.org/news/press-releases/ansi/amca-standard-208%2C-calculation-of-the-fan-energy-index%2C-available-for-free-download.html&usg=AOvVaw2eZxHxgdq183Y9WsZlgjTB)|

In the current workorder, we would focus on two metrics only and propose to focus on heat pump related metrics: HSPF2 and IEER.

1. HSPF2

2. IEER

## Approach ##

TBD

## Testing/Validation/Data Source(s) ##

Tow unit tests will confirm the cacluation of the metric is correct. 
Two example files will be modified to demonstrate the use of the new metrics. Simulation results will be manually checked/benchmarked using excel spreadsheet with input and output from EnergyPlus runs.

## Input Output Reference Documentation ##

To be developed.

## Input Description ##

TBD

## Outputs Description ##

TBD

## Engineering Reference ##

To be developed.

## Example Files and Transition Changes ##

No transition change is required.

## E-mail and  Conference Call Conclusions ##

TBD

## Acknowledgments ##

TBD 

## References ##

[1]	ASHRAE, ANSI/ASHRAE/IES Standard 90.1-2019: Energy Standard for Buildings Except Low-Rise Residential Buildings, 2019.

[2]	Jonestone Supply: The SEER2 Guide. 2022 URL: https://seer2.com/
