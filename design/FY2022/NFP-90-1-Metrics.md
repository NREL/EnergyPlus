Update output reporting of mechanical equipment using 90.1-2019 metrics
================

**Tobias Maile, Richard See**

**Digital Alchemy**

 - Original Date: April 14, 2022
 - Updated Date: June 7, 2022

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

**1. SEER2**

SEER2 is calculated based on ANSI/AHRI Standard 210/240-2023: applies to: Unitary Air-conditioners and Unitary Air-source Heat Pumps with
capacities less than 65,000 Btu/h. 

*For Single Stage Systems*

|Single Stage Systems|
|--|
|Graphical representation of SEER2:<br>![grafik](https://user-images.githubusercontent.com/49325382/173094322-3b9a650f-cccc-4e0f-a9ae-559c60d860e6.png)|

|Variable|Formulas|Code|
|--|--|--|
|SEER2|![grafik](https://user-images.githubusercontent.com/49325382/173093830-56c75bd8-8e3b-49b5-b58f-0788f717e479.png)|TBD|
|PLF(0.5)|![grafik](https://user-images.githubusercontent.com/49325382/173094047-5a2d4c02-a8b6-4690-9bfc-b337637dd780.png)<br>![grafik](https://user-images.githubusercontent.com/49325382/173094092-27b80da1-3fd4-415a-963f-f0e4d11ccf64.png)|TBD|
|C<sub>D</sub><sup>C</sup><br>Default Cooling Degradation Coefficient|Default: 0.2<br>If Tests C and D are not performed or C<sub>D</sub><sup>C</sup> is greater than default use default value|TBD|

*For Two Stage Systems*

|Two Stage Systems|
|--|
|Graphical representation of SEER2:<br>![grafik](https://user-images.githubusercontent.com/49325382/173099074-caeb59f7-d3b8-45ab-b8a7-5367506ff7f6.png)|

|Variable|Formulas|Code|
|--|--|--|
|SEER2|![grafik](https://user-images.githubusercontent.com/49325382/173098299-1b89804d-d8a3-4a75-8db3-8ad3bbdc8e36.png)|TBD|
|q(t<sub>j</sub>) and E(t<sub>j</sub>) are calculated for the following temperature bins|![grafik](https://user-images.githubusercontent.com/49325382/173098780-28c8a2ef-b13f-46e3-956e-6540477912dc.png)|
|building load < Low Stage capacity|11.2.1.2.1||
|building load > Low Stage capacity && building load < Full Stage capacity|11.2.1.2.2 or ??||
|building load > Unit capacity|11.2.1.2.4||
|BL(t<sub>j</sub>)|![grafik](https://user-images.githubusercontent.com/49325382/173107798-68ba98a9-e5d8-4a07-9a18-da6b1145bf5b.png)||
|q<sub>LOW</sub>(t<sub>j</sub>)|![grafik](https://user-images.githubusercontent.com/49325382/173108139-86c76624-cf0a-4452-a0e9-3e7379898df4.png)||
|P<sub>LOW</sub>(t<sub>j</sub>)|![grafik](https://user-images.githubusercontent.com/49325382/173108210-583a6e5a-5b81-40cf-8d85-dcd31b0358a0.png)||
|q<sub>Full</sub>(t<sub>j</sub>)|![grafik](https://user-images.githubusercontent.com/49325382/173108349-8f3ffe8c-3960-4b1d-8d42-a649556b0e5c.png)||
|P<sub>Full</sub>(t<sub>j</sub>)|![grafik](https://user-images.githubusercontent.com/49325382/173108376-c5732907-dd1b-46fc-89a3-1dcaf3ca59a2.png)||
|Case 1<br>![grafik](https://user-images.githubusercontent.com/49325382/173108719-2512fa72-daf7-4ce3-a2d3-c54a9fee1b31.png)|Building load is less than Low Stage capacity||
||![grafik](https://user-images.githubusercontent.com/49325382/173108824-f8bee0dc-5ba6-48e0-a8f6-0e8719e28305.png)||
|C<sub>D</sub><sup>C</sup><br>Default Cooling Degradation Coefficient|Default: 0.2<br>If Tests C and D are not performed or C<sub>D</sub><sup>C</sup> is greater than default use default value||
|Case 2<br>![grafik](https://user-images.githubusercontent.com/49325382/173109144-f7d78157-6f28-496c-a148-e4262494bb20.png)|Building load is greater than Low Stage capacity, but less than Full Stage capacity and the unit cycles between Low Stage operation and Full Stage operation||
||![grafik](https://user-images.githubusercontent.com/49325382/173109198-c817551d-8507-44ea-8f0f-f070b5cf695e.png)||
|Case 3<br>![grafik](https://user-images.githubusercontent.com/49325382/173109144-f7d78157-6f28-496c-a148-e4262494bb20.png)|Building load is greater than Low Stage capacity, but less than Full Stage capacity and the unit cycles between off and Full Stage operation||
||![grafik](https://user-images.githubusercontent.com/49325382/173109574-80e66076-9575-413b-a465-416d82ea1db3.png)||
|C<sub>D</sub><sup>c,Full</sup><br>Default Cooling Degradation Coefficient|Default: 0.2<br>If Tests C and D are not conducted set C<sub>D</sub><sup>c,Full to the default value or use the following forumlar:<br>![grafik](https://user-images.githubusercontent.com/49325382/173117743-27b547d9-8b76-4056-9f91-7c655b387df5.png)<br>If the test is conducted use the following forumlar:<br>![grafik](https://user-images.githubusercontent.com/49325382/173117884-9f2f26f4-8ed4-496d-b6d2-4c30beca8b1d.png)<br>![grafik](https://user-images.githubusercontent.com/49325382/173117960-61d85889-c575-4b0c-b8a3-e32d591ca7d2.png)||
|Case 4<br>![grafik](https://user-images.githubusercontent.com/49325382/173118064-7b60a4a5-97f4-4527-9e30-f9f1362c2c76.png)![grafik](https://user-images.githubusercontent.com/49325382/173118087-143d1ad3-8563-44b9-a5c0-1ebb16791c6c.png)|Building load is greater than or equal to the unit capacity||
||![grafik](https://user-images.githubusercontent.com/49325382/173118193-28b2fc85-c7a9-437b-8998-32d7f8a10ae2.png)||
 
*For variable speed system*

![grafik](https://user-images.githubusercontent.com/49325382/172480279-14b2e39b-f23a-4be7-88d1-1429ff94812d.png)

![grafik](https://user-images.githubusercontent.com/49325382/172480347-a5b2974c-4fa7-4986-89dc-f74cc1a97c05.png)

![grafik](https://user-images.githubusercontent.com/49325382/172480383-a0029b39-fb23-498b-90c7-ce5ed6aa557c.png)

![grafik](https://user-images.githubusercontent.com/49325382/172480433-31e57201-4845-40ed-a543-e15703b277e6.png)

![grafik](https://user-images.githubusercontent.com/49325382/172480471-7fc56c8a-04c6-4b16-9532-2c562f79f894.png)

![grafik](https://user-images.githubusercontent.com/49325382/172480515-87f68ed2-93e0-4319-9d1b-06691b4392af.png)


based on the following temperature bin values

![grafik](https://user-images.githubusercontent.com/49325382/172480683-d9c856c8-552f-48ac-b28a-8c96f68f2d7d.png)



**2. HSPF2**

HSPF2 is calculated based on ANSI/AHRI Standard 210/240-2023: applies to: Unitary Air-conditioners and Unitary Air-source Heat Pumps with
capacities less than 65,000 Btu/h

Calculation is done as follows (more details in ANSI/AHRI 210/240):

![grafik](https://user-images.githubusercontent.com/49325382/172480799-8f0fadc9-30ff-4339-9fa3-6df32cd14611.png)

![grafik](https://user-images.githubusercontent.com/49325382/172480916-a795389b-2a03-45be-b07f-8abfbc55d2c1.png)

![grafik](https://user-images.githubusercontent.com/49325382/172482051-43528104-e044-419d-b3c1-e1f93856c6df.png)

based on the following temperature bin values

![grafik](https://user-images.githubusercontent.com/49325382/172480836-a70565ce-130e-43f0-ba11-10254a894054.png)


These temperature bin values changed in the newer version, thus we need to define two arrays one for the older and one for the newer version. 


## Approach ##

For each of the two metrics, we will identify a set of EnergyPlus components for which the metric applies. Replicate the code pattern of existing metrics for the new metric and write unit tests to ensure the calculation is correct. Once the metrics are calculated we can integrate them into the corresponding report code of the equipment summary table. For now we want to add those two new metrics (SEER2 and HSPF2) in addition to the existing ones (SEER and HSPF) and explain the difference in a footnote. 
- Extend the implementation in StandardRatings.hh & StandardRatings.cc
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

No new example files are needed.
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
