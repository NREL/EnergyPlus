# Additional ASHRAE Metrics Reporting in EnergyPlus 23-1
**Digital Alchemy - Richard See & Brijendra Pratap Singh**

**Revisions**
-   New Feature Proposal -- 25-November-2022
-   New Feature Design -- TBD

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
The Integrated Energy Efficiency Ratio (IEER) will be calculated and reported as defined in the 2022 version of the AHRI 340/360 standard -- "Performance Rating of Commercial and Industrial Unitary Air Conditioning and Heat Pump Equipment." When compared to the previous (limited) implementation of the 2008 version of this standard, the Temperature time bins have changed and various test conditions for calculation of this metric have have changed & been expanded.

## SEER2 = Seasonal Energy Efficiency Rating (2023)
Currently, there is at least one DX cooling coil for which SEER2 is not calculated & reported. We proposal to extend SEER2 calculation and reporting to include Coil:Cooling:DX:VariableSpeed.

## HSPF2 = Heating Seasonal Performance Factor (2023)
Currently, there is at least one DX cooling coils for which HSPF2 is not calculated. We proposal to extend HSPF2 calculation and reporting to include Coil:Heating:DX:VariableSpeed.

## Approach
For each metric, we will first identify a set of EnergyPlus components to which the metric applies. We will then locate sample files that contain these object types -- to be used in testing. Coils we currently belive will be included are shown in the outline below.

For IEER, we will update code written in 2010, for partial support of the 2008 version of IEER. We will also add new code to fully support the newest version of this rating, written in 2022. This will include support for the updated Time Bins and expanded test conditions. We will also update existing unit tests and possibly add one or more unit tests to test the new implementation.

For SEER2 and HSPF2, we will update code written in the E+ 22-2 cycle to include support for the variable speed DX coils. As before, this will require adding new fields to these coils -- to enable users to specify the 'Fan Power Per Volume Flow Rate {W/(m3/s)}' inputs to the SEER2 & HSPF2 calculations. 

Once the metrics are calculated we can integrate them into the corresponding report code of the equipment summary table.

This will all be done by extending the implementation in StandardRatings.hh & StandardRatings.cc.  Our initial estimate for the scope of this work includes the following:
-   For IEER
    -   SingleSpeedDXCoolingCoilStandardRatings -- used for the
        following E+ coils:
        -   Coil:Cooling:DX:SingleSpeed

    -   MultiSpeedDXCoolingCoilStandardRatings -- used for the following
        E+ coils:
        -   Coil:Cooling:DX:TwoSpeed
        -   Coil:Cooling:DX:MultiSpeed
        -   Coil:Cooling:DX:VariableSpeed *(formula/data limited to 4 > speeds)*
        -   Coil:Cooling:DX:CurveFit:Speed

    -   SingleSpeedDXHeatingCoilStandardRatings -- used for the
        following E+ coils:
        -   Coil: Heating:DX:SingleSpeed

    -   MultiSpeedDXHeatingCoilStandardRatings -- used for the following
        E+ coils:
        -   Coil: Heating:DX:TwoSpeed
        -   Coil: Heating:DX:MultiSpeed
        -   Coil: Heating:DX:VariableSpeed *(formula/data limited to 4 > speeds)*

    -   ReportDXCoilRating used to report metrics for all the coils
        listed above.
    -   CheckCurveLimitsForStandardRatings

-   For SEER2
    -   MultiSpeedDXCoolingCoilStandardRatings -- add support for:
        -   Coil:Cooling:DX:VariableSpeed

-   For HSPF2
    -   MultiSpeedDXHeatingCoilStandardRatings -- add support for:
        -   Coil: Heating:DX:VariableSpeed

## Testing/Validation/Data Source(s)
Example files will be simulated to confirm calculation and reporting of the new metrics. Simulation results will be compared with values generated by a third-party implementation (assuming we can locate one).  Peer reviewer input on this is eagerly invited.
Unit tests for the Continuous Integration pipeline will also be updated/extended to confirm the calculation of the metrics and ensure the code produces correct results and does not fail when future code changes are merged.

## Input Output Reference Documentation
The Input Output Documentation will be expanded to include the new metrics. There will be an update in two places: first on the related section of the EnergyPlus components for which the metric is calculated and second in the reports section, specifically the Equipment Summary report. We will also add a disclaimer: \"It is not reasonable to expect AHRI ratings calculated from model inputs to match the actual ratings in the AHRI directory. This is largely because it is common practice for manufacturers to underrate their equipment for marketing reasons and/or to build in some safety margin in case DOE audits their ratings.\"

## Input Description
New fields will be added to the variable speed DX coils (both cooling and heating).

## Outputs Description
There will be an update to the reports to include the metrics as
described above.

## Engineering Reference
The Engineering Reference will be updated with a high-level description
of the calculations with links/references to Formulae (by number)
defined in the standards. formulas used to calculate the two metrics and
be referenced from the corresponding EnergyPlus components.

## Example Files and Transition Changes
Transition rules will be required to accomodate the new fields added to the VariableSpeed DX coils.
Example files the include the VariableSpeed DX coils will need to be updated.
More detail to be added in design.

## Acknowledgments
TBD

## References
\[1\] ASHRAE, ANSI/ASHRAE/IES Standard 90.1-2019: Energy Standard for
Buildings Except Low-Rise Residential Buildings. 2019.

\[2\] Jonestone Supply: The SEER2 Guide. 2022. URL: https://seer2.com/

\[3\] AHRI: Air Conditioning, Heating & Refrigeration Institute. 2023
(2020) Standard for Performance Rating of Unitary Air-conditioning &
Air-source Heat Pump Equipment. 2020. URL:
https://www.ahrinet.org/Portals/\_Appleseed/documents/Standards/AHRI%20Standard%20210.240-2023%20(2020).pdf

\[4\] AHRI: AHRI Standard 340/360-2022 (I-P): 2022 Standard for
Performance Rating of Commercial and Industrial Unitary Air-conditioning
and Heat Pump Equipment. 2019. URL:
https://www.ahrinet.org/Portals/Standards/AHRI%20Standard%20340-360-2022%20(I-P).pdf

\[5\] ANSI/AHAM: ANSI/AHAM RAC-1-2015: Room Air Conditioners. 2015.
https://webstore.ansi.org/standards/aham/ansiahamrac2015

\[6\] ANSI/AHAM: ANSI/AHRI 910-I-P-2014: Performance Rating of Indoor
Pool Dehumidifiers. 2014.
http://www.ahrinet.org/App_Content/ahri/files/STANDARDS/AHRI/AHRI_Standard_910_I-P_2014.pdf

\[7\] HPAC Engineering. Understanding AHRI 920 and ISMRE Ratings for
Most Efficient DOAS Specification. 2018.
https://www.hpac.com/columns/managing-facilities/article/20929562/understanding-ahri-920-and-ismre-ratings-for-most-efficient-doas-specification

\[8\] ANSI/AHAM: ANSI/AHRI 920-I-P-2015: 2015 Standard for Performance
Rating of DX-Dedicated Outdoor Air System Units. 2015.
https://www.ahrinet.org/App_Content/ahri/files/STANDARDS/ANSI/ANSI_AHRI_Standard_920_I-P_2015.pdf

\[9\] 10 CFR Appendix E to Subpart B of Part 430 - Uniform Test Method
for Measuring the Energy Consumption of Water Heaters. 2016.
https://www.govinfo.gov/app/details/CFR-2016-title10-vol3/CFR-2016-title10-vol3-part430-subpartB-appE

\[10\] ANSI/AMCA Standard 208, Calculation of the Fan Energy Index.
2019.
https://www.amca.org/news/press-releases/ansi/amca-standard-208,-calculation-of-the-fan-energy-index,-available-for-free-download.html&usg=AOvVaw2eZxHxgdq183Y9WsZlgjTB

\[11\] Differences between SEER and HSOPF and SEER2 and HSPF2
https://www.ahrinet.org/Portals/AHRI%20CAC%20HP%20Appendix%20M1%20Presentation%20-%20AHR%20Expo.pdf
