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
|ISCOP (AHRI 920)|ASHRAE 90.1-2019|Integrated Seasonal Coefficient of Performance|Direct Expansion-Dedicated Outdoor Air System Units|[8]|
|UEF (10 CFP 430 Appendix E)|ASHRAE 90.1-2019|Uniform Energy Factor|Water Heaters|[9]|
|FEI (AMCA 208)|ASHRAE 90.1-2019|Fan Energy Index|Fan Energy|[10]|

In the current workorder, we would focus on two metrics only and propose to focus on SEER2 and HSPF2 based on ANRI 210/240 2023. 
1. SEER2

SEER2 is calculated based on ANSI/AHRI Standard 210/240-2023: applies to: Unitary Air-conditioners and Unitary Air-source Heat Pumps with
capacities less than 65,000 Btu/h

SEER2 = PLF(0.5) * EER2 

![image](https://user-images.githubusercontent.com/49325382/165989369-12ff95cd-3e8d-4b4d-8bc5-d29a7c260687.png)

based on the following temperature bin values

![image](https://user-images.githubusercontent.com/49325382/165989393-bfe2b1f7-940c-4b6b-85dd-3ab1ea947fb0.png)


2. HSPF2

HSPF2 is calculated based on ANSI/AHRI Standard 210/240-2023: applies to: Unitary Air-conditioners and Unitary Air-source Heat Pumps with
capacities less than 65,000 Btu/h

Calculation is done as follows (more details in ANSI/AHRI 210/240:

![image](https://user-images.githubusercontent.com/49325382/165988448-6c29f39c-cb7b-447d-8942-0b8eeaf61b28.png)

based on the following temperature bin values

![image](https://user-images.githubusercontent.com/49325382/165988518-3237a3b7-8a1b-4a41-ae6f-38fc5ec5dc6a.png)


## Approach ##

For each of the two metrics, will identify a set of EnergyPlus components for which the metric applies. Replicate the code pattern of existing metrics for the new metric and write unit tests to ensure the calculation is correct. Once the metrics are calculated we can integrate them into the corresponding report code of the equipment summary table. For now we want to add those two new metrics (SEER2 and HSPF2) in addition to the existing ones (SEER and HSPF) and explain the difference in a footnote. 
- Extent the implementation in StandardRatings.hh & StandardRatings.cc
   - for SEER2 by adding code to the following functions
     - CalcDXCoilStandardRating
     - SingleSpeedDXCoolingCoilStandardRatings
     - DXCoolingCoilDataCenterStandardRatings
     - MultiSpeedDXCoolingCoilStandardRatings
     - ReportDXCoilRating
     - CheckCurveLimitsForStandardRatings
   - for HSPF2 by adding code to the following functions
     - CalcDXCoilStandardRating
     - SingleSpeedDXHeatingCoilStandardRatings
     - MultiSpeedDXHeatingCoilStandardRatings
     - ReportDXCoilRating
     - CheckCurveLimitsForStandardRatings
   - Add unit tests for both metrics into StandardRatings.unit.cc



## Testing/Validation/Data Source(s) ##

Unit tests will confirm and test the calculation of the metrics to ensure the code produces correct results and does not fail. 
Two example files will be modified to demonstrate the use of the new metrics. Simulation results will be manually checked/benchmarked using excel spreadsheet with input and output from EnergyPlus runs.

## Input Output Reference Documentation ##

The InputOutput Documentation will be expanded to include the new metrics. There will be an update in two places, first on the related section of the EnergyPlus components for which the metric is calculated and second in the reports section, specifically the Equipment Summary report. Add a displcaimer:
"It is not reasonable to expect AHRI ratings calculated from model inputs to match the actual ratings in the AHRI directory. This is largely because it is common practice for manufacturers to underrate their equipment for marketing reasons and/or to build in some safety margin in case DOE audits their ratings."

## Input Description ##

There is no additional input needed. 

## Outputs Description ##

There will be an update to the reports to include the metrics as described above. 

## Engineering Reference ##

The EngineeringReference will be updated to include the formulas used to calculate the two metrics and be referenced from the corresponding EnergyPlus components. 

## Example Files and Transition Changes ##

No transition change is required.

## E-mail and Conference Call Conclusions ##

from Neal Kruis (Slack): Add disclaimer that it is not "not reasonable to expect AHRI ratings calculated from model inputs to match the actual ratings in the AHRI directory. This is largely because it is common practice for manufacturers to underrate their equipment for marketing reasons and/or to build in some safety margin in case DOE audits their ratings."

from Jason Glazer (PR): "As you are adding these new metrics to the EquipmentSummary report, please leave the existing SEER and HSPF as well and clarify the source of both in footnotes to the tables."

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

[8] ANSI/AHAM: ANSI/AHRI 920-I-P-2015: 2015 Standard for Performance Rating of DX-Dedicated Outdoor Air System Units. 2015. https://www.ahrinet.org/App_Content/ahri/files/STANDARDS/ANSI/ANSI_AHRI_Standard_920_I-P_2015.pdf

[9] 10 CFR Appendix E to Subpart B of Part 430 - Uniform Test Method for Measuring the Energy Consumption of Water Heaters. 2016. https://www.govinfo.gov/app/details/CFR-2016-title10-vol3/CFR-2016-title10-vol3-part430-subpartB-appE

[10] ANSI/AMCA Standard 208, Calculation of the Fan Energy Index. 2019. https://www.amca.org/news/press-releases/ansi/amca-standard-208,-calculation-of-the-fan-energy-index,-available-for-free-download.html&usg=AOvVaw2eZxHxgdq183Y9WsZlgjTB
