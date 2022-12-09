# Additional ASHRAE Metrics Reporting in EnergyPlus 23-1
**Digital Alchemy - Richard See & Brijendra Pratap Singh**

**Revisions**
-   New Feature Proposal -- 25-November-2022
-   New Feature Design -- (updated) 08-December-2022

## Justification for New Feature
Many new metrics appear in the ASHRAE 90.1-2019 \[1\] standard.
EnergyPlus should be updated to stay in-sync with these new metrics.

## Overview
We have identified and investigated the following commonly used
equipment metrics, defined by various industry organizations and used in
ASHRAE 90.1-2019:

|#| Metric <br/>(Defining Org) | Used in | Description | Equipment<br/> Measured | URl |
|--|--|--|--|--|--|
|1 |SEER2 <br/> [(AHRI 210/240-2023)](https://www.ahrinet.org/search-standards/ahri-210240-2023-2020-performance-rating-unitary-air-conditioning-air-source-heat) | ASHRAE 90.1-2019 | (new) Seasonal Energy Efficiency Ratio | Air Conditioner Condensing Units<br/> Heat Pumps<br/> Single Packaged Units<br/> Evaporator /Coils<br/> Gas Furnaces |	[[2]](https://www.ahrinet.org/search-standards/ahri-210240-2023-2020-performance-rating-unitary-air-conditioning-air-source-heat) |
|2	| HSPF2 <br/> [(AHRI 210/240-2023)](https://www.ahrinet.org/search-standards/ahri-210240-2023-2020-performance-rating-unitary-air-conditioning-air-source-heat) | ASHRAE 90.1-2019	|(new) Heating Seasonal Performance Factor|	Unitary Air-source Heat Pump	|[[3]](https://www.ahrinet.org/search-standards/ahri-210240-2023-2020-performance-rating-unitary-air-conditioning-air-source-heat)|
|3	| IEER <br/> [(AHRI 340/360-2022)](https://www.ahrinet.org/search-standards/ahri-340360-i-p2022-performance-rating-commercial-and-industrial-unitary-air) | ASHRAE 90.1-2019 | Integrated Energy Efficiency Ratio | Commercial and Industrial Unitary Air-conditioning and Heat Pump |[[4]](https://www.ahrinet.org/search-standards/ahri-340360-i-p2022-performance-rating-commercial-and-industrial-unitary-air) |
|4	|CEER <br/>[(ANSI/AHAM RAC-1-2020)](https://www.aham.org/ItemDetail?iProductCode=20010&Category=MADSTD) | ASHRAE 90.1-2019	|Combined Energy Efficiency Ratio	|Household Room Air Conditioners	|[[5]](https://www.aham.org/ItemDetail?iProductCode=20010&Category=MADSTD)|
|5	|MRE <br/> [(AHRI 910-2020)](https://www.ahrinet.org/search-standards/ahri-910-i-p2014-performance-rating-indoor-pool-dehumidifiers) | ASHRAE 90.1-2019|	Moisture Removal Efficiency	|Indoor Pool Dehumidifiers	|[[6]](https://www.ahrinet.org/search-standards/ahri-910-i-p2014-performance-rating-indoor-pool-dehumidifiers)|
|6	|ISMRE <br/>[(AHRI 920-2020)](https://global.ihs.com/doc_detail.cfm?document_name=AHRI%20920%20I%2DP&item_s_key=00662234) | ASHRAE 90.1-2019	|Integrated Seasonal Moisture Removal Efficiency|	DOAS Specification	|[[7]](https://global.ihs.com/doc_detail.cfm?document_name=AHRI%20920%20I%2DP&item_s_key=00662234)|
|7	|ISCOP <br/> [(AHRI 920-2020)](https://global.ihs.com/doc_detail.cfm?document_name=AHRI%20920%20I%2DP&item_s_key=00662234) | ASHRAE 90.1-2019 |Integrated Seasonal Coefficient Of Performance	|Direct Expansion-Dedicated Outdoor Air System Units (DX DOAS Units)	|[[8]](https://global.ihs.com/doc_detail.cfm?document_name=AHRI%20920%20I%2DP&item_s_key=00662234)|
|8	|UEF <br/> [(CFR-430-2016 Appendix E)](https://www.govinfo.gov/app/details/CFR-2016-title10-vol3/CFR-2016-title10-vol3-part430-subpartB-appE) | ASHRAE 90.1-2019|	Uniform Energy Factor|	Water Heaters	|[[9]](https://www.govinfo.gov/app/details/CFR-2016-title10-vol3/CFR-2016-title10-vol3-part430-subpartB-appE)|
|9	|FEI  <br/>[(AMCA 208-2018)](https://www.techstreet.com/amca/searches/35955048) | ASHRAE 90.1-2019 |	fan energy index	| Fan Energy |	[[10]](https://www.techstreet.com/amca/searches/35955048) |

In a previous Task Order, we implemented support for calculation and reporting of SEER2 and HSPF2.  This was done for SingleSpeed, TwoSpeed, MultiSpeed Cooling and Heating coils.  It was also done for CurveFit: Speed Cooling coils.
In this Task Order we propose to implement support for calculation and reporting of the 2022 version of the AHRI IEER metric.  If budget allows, we will also extend the range of coils reporting SEER2 and HSPF2 to include VariableSpeed Cool and heating Coils.

## IEER = Integrated Energy Efficiency Rating (2022)
The Integrated Energy Efficiency Ratio (IEER) will be calculated and reported as defined in the 2022 version of the AHRI 340/360 standard -- "Performance Rating of Commercial and Industrial Unitary Air Conditioning and Heat Pump Equipment." This metric will be calculated and reported for Air-cooled Unitary Air-conditioners and Unitary Heat Pumps with capacities greater than 65,000 Btu/h.  When compared to the previous (limited) implementation of the 2008 version of this standard, the Temperature time bins have changed and various test conditions for calculation of this metric have have changed & been expanded.
<br/><br/>
**General IEER Equations** <br/>
For units covered by this standard, the IEER shall be calculated using test derived 
data and the below Equation<br/><br/>
![image](https://user-images.githubusercontent.com/78803858/205349295-b53d2fe9-c227-49a6-b926-fe00737e3b8c.png)<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;Where:<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;**A** = EER, (Btu/h)/W, at 100% Capacity at AHRI Standard Rating Conditions (see Table 6)<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;**B** = EER, (Btu/h)/W, at 75% Capacity and reduced condenser temperature (see Table 9)<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;**C** = EER, (Btu/h)/W, at 50% Capacity and reduced condenser temperature (see Table 9)<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;**D** = EER, (Btu/h)/W, at 25% Capacity and reduced condenser temperature (see Table 9)<br/>
<br/>
The IEER rating requires that the unit efficiency be determined at 100, 75, 50, and 25 Percent Load at the 
conditions specified in Table 9 and at the part-load rated indoor airflow, if different than the Full Load Rated Indoor Airflow. <br/>
![image](https://user-images.githubusercontent.com/78803858/205340767-bb273a2b-c63a-4418-a0aa-585a8d3dad25.png)<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;Where:<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; ![image](https://user-images.githubusercontent.com/78803858/205343185-e847900f-03fd-465f-a64d-4d15f0b1371e.png) = Degradation coefficient, (Btu/h)/(Btu/h)

&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; ![image](https://user-images.githubusercontent.com/78803858/205343266-aa280bcd-d3a3-40fc-ae8b-4d348d1de68b.png) = Compressor power at the lowest machine unloading point operating at the applicable
part-load Rating Condition, W


&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; ![image](https://user-images.githubusercontent.com/78803858/205343337-a93882e0-3689-4c1d-aa27-9e2ac06e041a.png) = Condenser Section power, at the applicable part-load Rating Condition, W 

&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; ![image](https://user-images.githubusercontent.com/78803858/205343408-24ab70c9-c15a-4a20-b1d6-1871f583c62a.png) = Control circuit power and any auxiliary loads, W


&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; ![image](https://user-images.githubusercontent.com/78803858/205343463-eb84d186-c610-46d7-a28d-12b1173f5875.png) = Indoor Fan power, W

&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; ![image](https://user-images.githubusercontent.com/78803858/205343556-cb53ba19-7b0a-4214-bb90-f0137e959ed1.png) = Cooling Capacity at the lowest machine unloading point operating at the applicable
part-load Rating Condition, Btu/h<br/>

The degradation coefficient, to account for cycling of the compressor for capacity less than the 
minimum step of capacity, shall be calculated using the Equation below:<br/>
![image](https://user-images.githubusercontent.com/78803858/205344387-f104632c-23dc-4b88-8d35-37f6fbadea64.png)<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;Where:<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;LF = Fractional “on” time for last stage at the tested load point, noted in the Equation below<br/><br/>
![image](https://user-images.githubusercontent.com/78803858/205344573-eec5558d-0b6e-4449-b243-222f0eebf7bb.png)<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;Where:<br/>
&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;![image](https://user-images.githubusercontent.com/78803858/205347128-2051af9d-6b90-4cd9-9097-70b2e29bcc68.png)= Percent Load

&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;![image](https://user-images.githubusercontent.com/78803858/205347196-b69ff8d4-100b-4500-adcd-e8018ce23125.png)= Full load Net Capacity, Btu/h

&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;![image](https://user-images.githubusercontent.com/78803858/205347261-64d76cda-98b8-483d-b5ff-6fe3910dfe11.png)= Part load Net Capacity, Btu/h

## Test Conditions: <br/>
![image](https://user-images.githubusercontent.com/78803858/205347858-a130fb11-c23b-4a34-bdc4-2a32e7d8d263.png)<br/><br/>
![image](https://user-images.githubusercontent.com/78803858/205354762-fdce0568-e15d-47f0-8966-cbeeb88df9c4.png)<br/>

**Example Test Points Used for EER Rating Points**<br/>
![image](https://user-images.githubusercontent.com/78803858/205357978-5b1096e7-3b38-44b4-a870-8881c911296e.png)<br/>


## SEER2 = Seasonal Energy Efficiency Rating (2023)
Currently, there is at least one DX cooling coil for which SEER2 is not calculated & reported. We proposal to extend SEER2 calculation and reporting to include Coil:Cooling:DX:VariableSpeed. Calculation of the SEER2 value for these Variable Speed coils will be according to the following - extracted from the AHRI 340/360-2022 standard:<br/>
 |Variable Speed Systems|
|--|
|Graphical representation of SEER2:<br>![image](https://user-images.githubusercontent.com/78803858/205965470-1fb0f38d-25cf-41bd-af91-eea3c3f2cf15.png)|

|Variable|Formulas|Code|
|--|--|--|
|SEER2|!![image](https://user-images.githubusercontent.com/78803858/205966718-6e8d8c2f-4360-4801-a97f-6efdd3edefce.png)|TBD|
|q<sub>LOW</sub>(t<sub>j</sub>)| <img src="https://user-images.githubusercontent.com/49325382/173122263-ae5eecb5-1e19-47b0-94d1-4fa087e9a8bb.png" width="80%" height="auto" />||
|P<sub>LOW</sub>(t<sub>j</sub>)|<img src="https://user-images.githubusercontent.com/49325382/173122290-8dd0658a-acad-4137-8cfe-abfac6e49e4c.png" width="80%" height="auto" />||
|q<sub>Int-Bin</sub>(t<sub>j</sub>)|![image](https://user-images.githubusercontent.com/78803858/205980941-cf041fe5-e61e-41f7-ae88-dd2163163094.png)||
|E<sub>Int-Bin</sub>(t<sub>j</sub>)|![image](https://user-images.githubusercontent.com/78803858/205981227-bf639f72-fd63-426f-9a15-114c97e23de4.png)||
|q<sub>Int</sub>(t<sub>j</sub>)|![image](https://user-images.githubusercontent.com/78803858/205981460-9bdf08aa-9410-46d8-acc2-3a655a037c0e.png)<br><img src="https://user-images.githubusercontent.com/49325382/173122561-5ae042d0-28a6-4fd9-812d-f9486abe1066.png" width="80%" height="auto" />||
|q<sub>Int</sub>(87)|<img src="https://user-images.githubusercontent.com/49325382/173122633-20787666-fa61-4ca2-b960-92528f8cf9bc.png" width="80%" height="auto" />||
|P<sub>Int</sub>(t<sub>j</sub>)|<img src="https://user-images.githubusercontent.com/49325382/173122777-aa94c891-7248-4448-a89c-93cfcef07301.png" width="80%" height="auto" />||
|P<sub>LOW</sub>(87)|<img src="https://user-images.githubusercontent.com/49325382/173122879-6a4382de-bd4b-4ed4-8f17-880d1adb0dc4.png" width="80%" height="auto" />||
|**Case 1**<br>![image](https://user-images.githubusercontent.com/78803858/205981627-b61c1b44-88a3-450a-a811-e79ad4a777dc.png)<br>![image](https://user-images.githubusercontent.com/78803858/205981833-4149a5c1-27dd-4eaa-821d-7012e93a930c.png)|Building load is no greater than unit capacity at low speed||
||<img src="https://user-images.githubusercontent.com/49325382/173125098-d13579fb-6db4-4db1-bf89-54f217a378dd.png" width="80%" height="auto" />||
|C<sub>D</sub><sup>C,low</sup>|![image](https://user-images.githubusercontent.com/78803858/205982024-6ca58141-3598-45f0-8f2c-81f91fe8d286.png)<br>![image](https://user-images.githubusercontent.com/78803858/205982193-48229efc-4c10-4add-8178-ae4a71a8b630.png)<br>Substitute Test G and I for test C and D||
|C<sub>D</sub><sup>C</sup><br>Default Cooling Degradation Coefficient|Default: 0.25<br>If Tests G and I are not performed or C<sub>D</sub><sup>C</sup> is greater than default use default value||
|**Case 2**<br><img src="https://user-images.githubusercontent.com/49325382/173124209-db6b6044-efe8-4e7f-88f7-aae795049240.png" width="80%" height="auto" />|Building load can be matched by modulating the compressor speed between low speed and full speed||
|q<sub>Int-Bin</sub>(t<sub>j</sub>)|![image](https://user-images.githubusercontent.com/78803858/205982359-a03c9bd1-1c80-4faa-b8d3-d25a7fcf55f2.png)||
|E<sub>Int-Bin</sub>(t<sub>j</sub>)|![image](https://user-images.githubusercontent.com/78803858/205982506-a7430315-3b3f-409d-9057-7cbd28788432.png)||
||<img src="https://user-images.githubusercontent.com/49325382/173109198-c817551d-8507-44ea-8f0f-f070b5cf695e.png" width="80%" height="auto" />||
|EER<sub>Int-Bin</sub>(t<sub>j</sub>)|<img src="https://user-images.githubusercontent.com/49325382/173124302-997c297d-555b-4ed2-afd5-6351d53642ba.png" width="80%" height="auto" />||
|**Case 3**<br><img src="https://user-images.githubusercontent.com/49325382/173124546-69a9d1f7-570a-422a-9b02-05aff8282289.png" width="80%" height="auto" />|Building load is equal to or greater than unit capacity at full stage||
||![image](https://user-images.githubusercontent.com/78803858/205982718-516326d4-76f3-4ab8-adfd-bf91b90e19ce.png)>||

## HSPF2 = Heating Seasonal Performance Factor (2023)
Currently, there is at least one DX cooling coils for which HSPF2 is not calculated. We proposal to extend HSPF2 calculation and reporting to include Coil:Heating:DX:VariableSpeed. Calculation of the HSPF2 value for these Variable Speed coils will be according to the following - extracted from the AHRI 340/360-2022 standard:<br/>
|Variable Speed System|
|--|
|Graphical representation of HSPFs:<br>!![image](https://user-images.githubusercontent.com/78803858/205966121-28ec2388-32a7-4784-8885-f55416b6070e.png)|

|Variable|Formulas|Code|
|--|--|--|
|HSPF2|![image](https://user-images.githubusercontent.com/78803858/205983900-c0ab37e2-6a2f-42f1-b840-977fc594dc28.png)|TBD|
||<img src="https://user-images.githubusercontent.com/49325382/173148304-e6399e4d-2be1-4d94-9221-70a5d6e28c4a.png" width="80%" height="auto" />||
|**Case 1**<br>![image](https://user-images.githubusercontent.com/78803858/205984100-d83fba93-2508-409d-9fed-036ddcd8f3ec.png)|Building Load is less than the capacity of the unit at the Low Compressor Speed||
|**Case 2**<br><img src="https://user-images.githubusercontent.com/49325382/173150412-f1325086-f758-40c8-82c4-116db328732b.png" width="80%" height="auto" />|Building load can be matched by modulating the compressor speed between low speed and full speed||
||<img src="https://user-images.githubusercontent.com/49325382/173150460-8e50e95d-ba7a-45c6-98ab-4257ac2382f7.png" width="80%" height="auto" />||
|**Case 3**<br>![image](https://user-images.githubusercontent.com/78803858/205984189-47957557-9446-4276-baa7-45d3965c596e.png)|Building Load is greater than the capacity of the unit at the Full Compressor Speed||
||<img src="https://user-images.githubusercontent.com/49325382/173150028-3732a2c7-bff3-4616-abb9-e1e3d735cea5.png" width="80%" height="auto" />|

## Approach
For each metric, we will first identify a set of EnergyPlus components to which the metric applies. We will then locate sample files that contain these object types -- to be used in testing. Coils we currently belive will be included are shown in the outline below.

For IEER, we will update code written in 2010, for partial support of the 2008 version of IEER. We will also add new code to fully support the newest version of this rating, written in 2022. This will include support for the updated Time Bins and expanded test conditions. We will also update existing unit tests and possibly add one or more unit tests to test the new implementation.

For SEER2 and HSPF2, we will update code written in the E+ 22-2 cycle to include support for the variable speed DX coils. As before, this will require adding new fields to these coils -- to enable users to specify the 'Fan Power Per Volume Flow Rate {W/(m3/s)}' inputs to the SEER2 & HSPF2 calculations. 

Once the metrics are calculated we can integrate them into the corresponding report code of the equipment summary table.

This will all be done by extending the implementation in StandardRatings.hh & StandardRatings.cc.  Our initial estimate for the scope of this work includes the following:
-   For IEER
    -   SingleSpeedDXCoolingCoilStandardRatings -- used for the following E+ coils:
        -   Coil:Cooling:DX:SingleSpeed
        -   Coil:Cooling:WaterToAirHeatPump:EquationFit
        -   Coil:Cooling:DX:CurveFit (single-speed operating mode)

    -   MultiSpeedDXCoolingCoilStandardRatings -- used for the following E+ coils:
        -   Coil:Cooling:DX:TwoSpeed
        -   Coil:Cooling:DX:MultiSpeed
        -   Coil:Cooling:DX:VariableSpeed *(Note: standard formula/test conditions are limited to 4 > speeds)*
        -   Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit
        -   Coil:Cooling:DX:CurveFit (multi-speed operating mode)

    -   SingleSpeedDXHeatingCoilStandardRatings -- used for the
        following E+ coils:
        -   Coil: Heating:DX:SingleSpeed

    -   MultiSpeedDXHeatingCoilStandardRatings -- used for the following E+ coils:
        -   Coil: Heating:DX:TwoSpeed
        -   Coil: Heating:DX:MultiSpeed
        -   Coil: Heating:DX:VariableSpeed *(Note: standard formula/test conditions are limited to 4 > speeds)*

    -   ReportDXCoilRating used to report metrics for all the coils listed above.
    -   CheckCurveLimitsForStandardRatings
    -   Update existing unit tests and add new tests where new cases have been added

-   For SEER2
    -   MultiSpeedDXCoolingCoilStandardRatings -- add support for:
        -   Coil:Cooling:DX:VariableSpeed
    -   Update existing unit tests and add new tests where new cases have been added

-   For HSPF2
    -   MultiSpeedDXHeatingCoilStandardRatings -- add support for:
        -   Coil: Heating:DX:VariableSpeed
    -   Update existing unit tests and add new tests where new cases have been added

## Testing/Validation/Data Source(s)
Example files will be simulated to confirm calculation and reporting of the new metrics. Simulation results will be compared with values generated by a third-party implementation (assuming we can locate one).  Peer reviewer input on this is eagerly invited.
Unit tests for the Continuous Integration pipeline will also be updated/extended to confirm the calculation of the metrics and ensure the code produces correct results and does not fail when future code changes are merged.

## Input Output Reference Documentation
The Input Output Documentation will be expanded to include the new metrics. There will be an update in two places: first on the related section of the EnergyPlus components for which the metric is calculated and second in the reports section, specifically the Equipment Summary report. We will also add a disclaimer: \"It is not reasonable to expect AHRI ratings calculated from model inputs to match the actual ratings in the AHRI directory. This is largely because it is common practice for manufacturers to underrate their equipment for marketing reasons and/or to build in some safety margin in case DOE audits their ratings.\"

## Input Description
New fields will be added to the variable speed DX coils (both cooling and heating) to represent the static pressure on the fans (Power per flow rate).

## Outputs Description
There will be an update to the reports to include the metrics as described above.

## Engineering Reference
The Engineering Reference will be updated with a high-level description
of the calculations with links/references to Formulae (by number)
defined in the standards. formulas used to calculate the two metrics and
be referenced from the corresponding EnergyPlus components.

## Example Files and Transition Changes
Transition rules will be required to accommodate the new fields added to the VariableSpeed DX coils.
Example files that include the VariableSpeed DX coils will need to be updated.

-   For IEER: There are 225 example files that feature one or more of various DX coils listed above.  Testing will be done for some or all of these.  We will begin with one example for each of: SingleSpeed, TwoSpeed, MultiSpeed, VariableSpeed, and CurveFit:Speed.  For example:
    -   HeatPump.idf								                 – Single speed cooling (Condenser:EvapCooled)
    -   HeatPumpAuto.idf                     – Single speed cooling & heating (Condenser:AirCooled)
    -   RetailPackagedTESCoil.idf					       – Single speed cooling/thermalStorage (Condenser:EvapCooled)
    -   UnitarySystem_SingleSpeedDX.idf		    – Single speed CurveFit (Condenser:EvapCooled)
    -   5ZoneAutoDXVAV.idf					              – Two speed cooling (Condenser:EvapCooled)
    -   MultiSpeedHeatPump.idf				           – Mulitspeed cooling & heating (Condenser:AirCooled)
    -   PackagedTerminalHeatPumpVSAS.idf	    – Variable speed cooling & heating (Condenser:AirCooled)
    -   UnitarySystem_MultiSpeedDX			        – Mulitspeed cooling CurveFit (Condenser:AirCooled)
    -   HeatPumpWaterToAirEquationFit.idf 			– WaterToAirHeatPump:EquationFit	 (Condenser:WaterCooled)
    -   VSHeatPumpWaterToAirEquationFit.idf		– WaterToAirHeatPump:VariableSpeedEquationFit (Condenser:WaterCooled)

-   For SEER2/HSPF2: There are 11 example files that feature one or both of the variable speed DX coils.  Testing will be done with some of all of these.  We will start files which include variable speed DX coils for both heating and cooling.  For example:
    -   PackagedTerminalAirConditionerVSAS.idf	– Variable speed cooling & heating
    -   PackagedTerminalHeatPumpVSAS.idf		     – Variable speed cooling & heating

## Acknowledgments
TBD

## References
\[1\] ASHRAE, ANSI/ASHRAE/IES Standard 90.1-2019: Energy Standard for
Buildings Except Low-Rise Residential Buildings. 2019.

\[2\] Jonestone Supply: The SEER2 Guide. 2022. URL: [AHRI 210/240-202](https://seer2.com/)

\[3\] AHRI: Air Conditioning, Heating & Refrigeration Institute. 2023
(2020) Standard for Performance Rating of Unitary Air-conditioning &
Air-source Heat Pump Equipment. 2020. URL: [AHRI 210/240-2023](https://www.ahrinet.org/Portals/\_Appleseed/documents/Standards/AHRI%20Standard%20210.240-2023%20(2020).pdf)

\[4\] AHRI: AHRI Standard 340/360-2022 (I-P): 2022 Standard for
Performance Rating of Commercial and Industrial Unitary Air-conditioning
and Heat Pump Equipment. 2019. URL: [AHRI 340/360-2022](https://www.ahrinet.org/Portals/Standards/AHRI%20Standard%20340-360-2022%20(I-P).pdf)

\[5\] ANSI/AHAM: ANSI/AHAM RAC-1-2015: Room Air Conditioners. 2015. URL: [ANSI/AHAM RAC-1-2020](https://webstore.ansi.org/standards/aham/ansiahamrac2015)

\[6\] ANSI/AHAM: ANSI/AHRI 910-I-P-2014: Performance Rating of Indoor
Pool Dehumidifiers. 2014. URL: [AHRI 910-2020](http://www.ahrinet.org/App_Content/ahri/files/STANDARDS/AHRI/AHRI_Standard_910_I-P_2014.pdf)

\[7\] HPAC Engineering. Understanding AHRI 920 and ISMRE Ratings for
Most Efficient DOAS Specification. 2018. URL:[AHRI 920-2020](https://www.hpac.com/columns/managing-facilities/article/20929562/understanding-ahri-920-and-ismre-ratings-for-most-efficient-doas-specification)

\[8\] ANSI/AHAM: ANSI/AHRI 920-I-P-2015: 2015 Standard for Performance
Rating of DX-Dedicated Outdoor Air System Units. 2015.<br/>&ensp;&ensp; URL: [AHRI 920-2020](https://www.ahrinet.org/App_Content/ahri/files/STANDARDS/ANSI/ANSI_AHRI_Standard_920_I-P_2015.pdf)

\[9\] 10 CFR Appendix E to Subpart B of Part 430 - Uniform Test Method
for Measuring the Energy Consumption of Water Heaters. 2016.<br/>&ensp;&ensp; URL: [FR-430-2016 Appendix E](https://www.govinfo.gov/app/details/CFR-2016-title10-vol3/CFR-2016-title10-vol3-part430-subpartB-appE)

\[10\] ANSI/AMCA Standard 208, Calculation of the Fan Energy Index.
2019. URL: [AMCA 208-2018](https://www.amca.org/news/press-releases/ansi/amca-standard-208,-calculation-of-the-fan-energy-index,-available-for-free-download.html&usg=AOvVaw2eZxHxgdq183Y9WsZlgjTB)

\[11\] Differences between SEER and HSOPF and SEER2 and HSPF2 URL: [Presentation](https://www.ahrinet.org/Portals/AHRI%20CAC%20HP%20Appendix%20M1%20Presentation%20-%20AHR%20Expo.pdf)
