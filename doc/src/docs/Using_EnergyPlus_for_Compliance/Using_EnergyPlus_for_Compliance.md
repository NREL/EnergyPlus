
![](media/ep.gif)

<br/>
<p><h1>EnergyPlus<sup>TM</sup> Documentation</h1></p>
<hr>
<h1>Using EnergyPlus for Compliance</h1>
<h2>Hints on using EnergyPlus for Compliance with Standards and Rating Systems</h2>
<br/>
<p><i>(stay inside the lines...)</i></p>
<br/>
<br/>
<br/>
<br/>
<br/>
<p><small>COPYRIGHT (c) 1996-2015 THE BOARD OF TRUSTEES OF THE UNIVERSITY OF ILLINOIS AND THE REGENTS OF THE UNIVERSITY OF CALIFORNIA THROUGH THE ERNEST ORLANDO LAWRENCE BERKELEY NATIONAL LABORATORY. ALL RIGHTS RESERVED. NO PART OF THIS MATERIAL MAY BE REPRODUCED OR TRANSMITTED IN ANY FORM OR BY ANY MEANS WITHOUT THE PRIOR WRITTEN PERMISSION OF THE UNIVERSITY OF ILLINOIS OR THE ERNEST ORLANDO LAWRENCE BERKELEY NATIONAL LABORATORY. ENERGYPLUS IS A TRADEMARK OF THE US DEPARTMENT OF ENERGY.</small></p>
<p style="page-break-after:always;"></p>
<div id="generated-toc"></div>
<p style="page-break-after:always;"></p>

Introduction
============

Background
----------

Energy Standards abound throughout the world. This document provides guidance on using EnergyPlus and its reports for compliance with energy standards.

The tabular reports from EnergyPlus were designed to help collate information into a form usually required for reporting on designs for energy use. The tabular reports are fully described in the Output Details document and will be judiciously reproduced here for clarity and reference. In addition, by using the Output:Table:Style object, one can obtain the reports in a variety of formats – ready for incorporating into your own style for submission or presentation.

First, we describe the features available in the tabular report that might be used for energy compliance reporting or presentation of energy designs to clients.

Compliance Objects
------------------

To facilitate using EnergyPlus for compliance reporting, a group of Compliance Objects is described in the Input Output Reference document. Currently, there is only one object in this group: **Compliance:Building** which, when fully populated, will describe various aspects that are needed specific to buildings. Other compliance objects will be added to this group when appropriate.

Tabular Summary Reports
-----------------------

Several predefined reports are available from the Output Table:Predefined object including the following.

- Annual Building Utility Performance Summary

- Input Verification and Results Summary

- Demand End Use Components Summary

- Source Energy End Use Components Summary

- Climatic Data Summary

- Equipment Summary

- Envelope Summary

- Surface Shadowing Summary

- Shading Summary

- Lighting Summary

- HVAC Sizing Summary

- System Summary

- Component Sizing Summary

- Outside Air Summary

- Object Count Summary

In addition, specifying “All Summary” will enable all the reports in this category (more than are shown here).

### Annual Building Utility Performance Summary

First and foremost, the Annual Building Utility Performance Summary gives a good overview of the energy use by category:



Report: **AnnualBuildingUtilityPerformanceSummary**

For: **Entire Facility**

Timestamp: **2009-09-17 17:30:22**

Values gathered over 8760.00 hours




Site and Source Energy

<table class="table table-striped">
<tr>
<th> </th>
<th>Total Energy [GJ]</th>
<th>Energy Per Total Building Area [MJ/m2]</th>
<th>Energy Per Conditioned Building Area [MJ/m2]</th>
</tr>
<tr>
<td>Total Site Energy</td>
<td>180.73</td>
<td>194.92</td>
<td>194.92</td>
</tr>
<tr>
<td>Net Site Energy</td>
<td>180.73</td>
<td>194.92</td>
<td>194.92</td>
</tr>
<tr>
<td>Total Source Energy</td>
<td>1141.64</td>
<td>1231.28</td>
<td>1231.28</td>
</tr>
<tr>
<td>Net Source Energy</td>
<td>1141.64</td>
<td>1231.28</td>
<td>1231.28</td>
</tr>
</table>



 Source to Site Energy Conversion Factors

<table class="table table-striped">
<tr>
<th> </th>
<th>Source=&gt;Site Conversion Factor</th>
</tr>
<tr>
<td>Electricity</td>
<td>0.293</td>
</tr>
<tr>
<td>Natural Gas</td>
<td>0.010</td>
</tr>
<tr>
<td>District Cooling</td>
<td>0.098</td>
</tr>
<tr>
<td>District Heating</td>
<td>0.033</td>
</tr>
<tr>
<td>Steam</td>
<td>0.250</td>
</tr>
<tr>
<td>Gasoline</td>
<td>1.050</td>
</tr>
<tr>
<td>Diesel</td>
<td>1.050</td>
</tr>
<tr>
<td>Coal</td>
<td>1.050</td>
</tr>
<tr>
<td>Fuel Oil #1</td>
<td>1.050</td>
</tr>
<tr>
<td>Fuel Oil #2</td>
<td>1.050</td>
</tr>
<tr>
<td>Propane</td>
<td>1.050</td>
</tr>
</table>



 Building Area

<table class="table table-striped">
<tr>
<th> </th>
<th>Area [m2]</th>
</tr>
<tr>
<td>Total Building Area</td>
<td>927.20</td>
</tr>
<tr>
<td>Net Conditioned Building Area</td>
<td>927.20</td>
</tr>
<tr>
<td>Unconditioned Building Area</td>
<td>0.00</td>
</tr>
</table>



 End Uses

<table class="table table-striped">
<tr>
<th> </th>
<th>Electricity [GJ]</th>
<th>Natural Gas [GJ]</th>
<th>Other Fuel [GJ]</th>
<th>District Cooling [GJ]</th>
<th>District Heating [GJ]</th>
<th>Water [m3]</th>
</tr>
<tr>
<td>Heating</td>
<td>0.00</td>
<td>28.16</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Cooling</td>
<td>16.56</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Lighting</td>
<td>81.24</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Lighting</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Equipment</td>
<td>47.70</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Equipment</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Fans</td>
<td>6.59</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Pumps</td>
<td>0.48</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Rejection</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Humidification</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Recovery</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Water Systems</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Refrigeration</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Generators</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
</tr>
<tr>
<td>Total End Uses</td>
<td>152.57</td>
<td>28.16</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
</table>

*Note: Natural gas appears to be the principal heating source based on energy usage.*

 End Uses By Subcategory

<table class="table table-striped">
<tr>
<th> </th>
<th>Subcategory</th>
<th>Electricity [GJ]</th>
<th>Natural Gas [GJ]</th>
<th>Other Fuel [GJ]</th>
<th>District Cooling [GJ]</th>
<th>District Heating [GJ]</th>
<th>Water [m3]</th>
</tr>
<tr>
<td>Heating</td>
<td>Boiler</td>
<td>0.00</td>
<td>28.16</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td> </td>
<td>Boiler Parasitic</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Cooling</td>
<td>General</td>
<td>16.56</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Lighting</td>
<td>GeneralLights</td>
<td>81.24</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Lighting</td>
<td>General</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Equipment</td>
<td>General</td>
<td>47.70</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Equipment</td>
<td>General</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Fans</td>
<td>General</td>
<td>6.59</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Pumps</td>
<td>General</td>
<td>0.48</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Rejection</td>
<td>General</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Humidification</td>
<td>General</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Recovery</td>
<td>General</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Water Systems</td>
<td>General</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Refrigeration</td>
<td>General</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Generators</td>
<td>General</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
</table>



 Normalized Metrics

 Utility Use Per Conditioned Floor Area

<table class="table table-striped">
<tr>
<th> </th>
<th>Electricity Intensity [MJ/m2]</th>
<th>Natural Gas Intensity [MJ/m2]</th>
<th>Other Fuel Intensity [MJ/m2]</th>
<th>District Cooling Intensity [MJ/m2]</th>
<th>District Heating Intensity [MJ/m2]</th>
<th>Water Intensity [m3/m2]</th>
</tr>
<tr>
<td>Lighting</td>
<td>87.62</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>HVAC</td>
<td>25.49</td>
<td>30.37</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Other</td>
<td>51.44</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Total</td>
<td>164.55</td>
<td>30.37</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
</table>



 Utility Use Per Total Floor Area

<table class="table table-striped">
<tr>
<th> </th>
<th>Electricity Intensity [MJ/m2]</th>
<th>Natural Gas Intensity [MJ/m2]</th>
<th>Other Fuel Intensity [MJ/m2]</th>
<th>District Cooling Intensity [MJ/m2]</th>
<th>District Heating Intensity [MJ/m2]</th>
<th>Water Intensity [m3/m2]</th>
</tr>
<tr>
<td>Lighting</td>
<td>87.62</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>HVAC</td>
<td>25.49</td>
<td>30.37</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Other</td>
<td>51.44</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Total</td>
<td>164.55</td>
<td>30.37</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
</table>



 Electric Loads Satisfied

<table class="table table-striped">
<tr>
<th> </th>
<th>Electricity [GJ]</th>
<th>Percent Electricity [%]</th>
</tr>
<tr>
<td>Fuel-Fired Power Generation</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>High Temperature Geothermal*</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Photovoltaic Power</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Wind Power*</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Net Decrease in On-Site Storage</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Total On-Site Electric Sources</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td> </td>
</tr>
<tr>
<td>Electricity Coming From Utility</td>
<td>152.57</td>
<td>100.00</td>
</tr>
<tr>
<td>Surplus Electricity Going To Utility</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Net Electricity From Utility</td>
<td>152.57</td>
<td>100.00</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td> </td>
</tr>
<tr>
<td>Total On-Site and Utility Electric Sources</td>
<td>152.57</td>
<td>100.00</td>
</tr>
<tr>
<td>Total Electricity End Uses</td>
<td>152.57</td>
<td>100.00</td>
</tr>
</table>



 On-Site Thermal Sources

<table class="table table-striped">
<tr>
<th> </th>
<th>Heat [GJ]</th>
<th>Percent Heat [%]</th>
</tr>
<tr>
<td>Water-Side Heat Recovery</td>
<td>0.00</td>
<td> </td>
</tr>
<tr>
<td>Air to Air Heat Recovery for Cooling</td>
<td>0.00</td>
<td> </td>
</tr>
<tr>
<td>Air to Air Heat Recovery for Heating</td>
<td>0.00</td>
<td> </td>
</tr>
<tr>
<td>High-Temperature Geothermal*</td>
<td>0.00</td>
<td> </td>
</tr>
<tr>
<td>Solar Water Thermal</td>
<td>0.00</td>
<td> </td>
</tr>
<tr>
<td>Solar Air Thermal</td>
<td>0.00</td>
<td> </td>
</tr>
<tr>
<td>Total On-Site Thermal Sources</td>
<td>0.00</td>
<td> </td>
</tr>
</table>



 Water Source Summary

<table class="table table-striped">
<tr>
<th> </th>
<th>Water [m3]</th>
<th>Percent Water [%]</th>
</tr>
<tr>
<td>Rainwater Collection</td>
<td>0.00</td>
<td>-</td>
</tr>
<tr>
<td>Condensate Collection</td>
<td>0.00</td>
<td>-</td>
</tr>
<tr>
<td>Groundwater Well</td>
<td>0.00</td>
<td>-</td>
</tr>
<tr>
<td>Total On Site Water Sources</td>
<td>0.00</td>
<td>-</td>
</tr>
<tr>
<td>-</td>
<td>-</td>
<td>-</td>
</tr>
<tr>
<td>Initial Storage</td>
<td>0.00</td>
<td>-</td>
</tr>
<tr>
<td>Final Storage</td>
<td>0.00</td>
<td>-</td>
</tr>
<tr>
<td>Change in Storage</td>
<td>0.00</td>
<td>-</td>
</tr>
<tr>
<td>-</td>
<td>-</td>
<td>-</td>
</tr>
<tr>
<td>Water Supplied by Utility</td>
<td>0.00</td>
<td>-</td>
</tr>
<tr>
<td>-</td>
<td>-</td>
<td>-</td>
</tr>
<tr>
<td>Total On Site, Change in Storage, and Utility Water Sources</td>
<td>0.00</td>
<td>-</td>
</tr>
<tr>
<td>Total Water End Uses</td>
<td>0.00</td>
<td>-</td>
</tr>
</table>



 Comfort and Setpoint Not Met Summary

<table class="table table-striped">
<tr>
<th> </th>
<th>Facility [Hours]</th>
</tr>
<tr>
<td>Time Set Point Not Met During Occupied Heating</td>
<td>0.00</td>
</tr>
<tr>
<td>Time Set Point Not Met During Occupied Cooling</td>
<td>213.25</td>
</tr>
<tr>
<td>Time Not Comfortable Based on Simple ASHRAE 55-2004</td>
<td>72.50</td>
</tr>
</table>



 Note 1: An asterisk (\*) indicates that the feature is not yet implemented.

### Input Verification Summary

The input verification summary gives a good overview of the facility being simulated:



Report: **InputVerificationandResultsSummary**

For: **Entire Facility**

**Timestamp: 2009-09-17 17:30:22**

General

<table class="table table-striped">
<tr>
<th> </th>
<th>Value</th>
</tr>
<tr>
<td>Program Version and Build</td>
<td>EnergyPlus 4.0.0.017, 9/17/2009 5:30 PM</td>
</tr>
<tr>
<td>Weather</td>
<td>Climate Zone 6 CA USA WYEC2-C-00006 WMO#=722970</td>
</tr>
<tr>
<td>Latitude [deg]</td>
<td>33.90</td>
</tr>
<tr>
<td>Longitude [deg]</td>
<td>-118.5</td>
</tr>
<tr>
<td>Elevation [m]</td>
<td>30.00</td>
</tr>
<tr>
<td>Time Zone</td>
<td>-8.0</td>
</tr>
<tr>
<td>North Axis Angle [deg]</td>
<td>30.00</td>
</tr>
<tr>
<td>Rotation for Appendix G [deg]</td>
<td>0.00</td>
</tr>
<tr>
<td>Hours Simulated [hrs]</td>
<td>8760.00</td>
</tr>
</table>



 ENVELOPE

 Window-Wall Ratio

<table class="table table-striped">
<tr>
<th> </th>
<th>Total</th>
<th>North (315 to 45 deg)</th>
<th>East (45 to 135 deg)</th>
<th>South (135 to 225 deg)</th>
<th>West (225 to 315 deg)</th>
</tr>
<tr>
<td>Gross Wall Area [m2]</td>
<td>274.20</td>
<td>91.50</td>
<td>45.60</td>
<td>91.50</td>
<td>45.60</td>
</tr>
<tr>
<td>Window Opening Area [m2]</td>
<td>60.90</td>
<td>20.85</td>
<td>9.12</td>
<td>21.81</td>
<td>9.12</td>
</tr>
<tr>
<td>Window-Wall Ratio [%]</td>
<td>22.21</td>
<td>22.79</td>
<td>20.00</td>
<td>23.84</td>
<td>20.00</td>
</tr>
</table>



 Skylight-Roof Ratio

<table class="table table-striped">
<tr>
<th> </th>
<th>Total</th>
</tr>
<tr>
<td>Gross Roof Area [m2]</td>
<td>463.60</td>
</tr>
<tr>
<td>Skylight Area [m2]</td>
<td>0.00</td>
</tr>
<tr>
<td>Skylight-Roof Ratio [%]</td>
<td>0.00</td>
</tr>
</table>



 PERFORMANCE

 Zone Summary

<table class="table table-striped">
<tr>
<th> </th>
<th>Area [m2]</th>
<th>Conditioned (Y/N)</th>
<th>Volume [m3]</th>
<th>Multipliers</th>
<th>Gross Wall Area [m2]</th>
<th>Window Glass Area [m2]</th>
<th>Lighting [W/m2]</th>
<th>People [m2] per person</th>
<th>Plug and Process [W/m2]</th>
</tr>
<tr>
<td>PLENUM-1</td>
<td>463.60</td>
<td>Yes</td>
<td>283.20</td>
<td>1.00</td>
<td>54.84</td>
<td>0.00</td>
<td>0.0000</td>
<td> </td>
<td>0.0000</td>
</tr>
<tr>
<td>SPACE1-1</td>
<td>99.16</td>
<td>Yes</td>
<td>239.25</td>
<td>1.00</td>
<td>73.20</td>
<td>21.81</td>
<td>15.9742</td>
<td>9.01</td>
<td>10.6495</td>
</tr>
<tr>
<td>SPACE2-1</td>
<td>42.73</td>
<td>Yes</td>
<td>103.31</td>
<td>1.00</td>
<td>36.48</td>
<td>9.12</td>
<td>16.0056</td>
<td>8.55</td>
<td>10.6704</td>
</tr>
<tr>
<td>SPACE3-1</td>
<td>96.48</td>
<td>Yes</td>
<td>239.25</td>
<td>1.00</td>
<td>73.20</td>
<td>20.85</td>
<td>16.4179</td>
<td>8.77</td>
<td>10.9453</td>
</tr>
<tr>
<td>SPACE4-1</td>
<td>42.73</td>
<td>Yes</td>
<td>103.31</td>
<td>1.00</td>
<td>36.48</td>
<td>9.12</td>
<td>16.0056</td>
<td>8.55</td>
<td>10.6704</td>
</tr>
<tr>
<td>SPACE5-1</td>
<td>182.49</td>
<td>Yes</td>
<td>447.68</td>
<td>1.00</td>
<td>0.00</td>
<td>0.00</td>
<td>16.2420</td>
<td>9.12</td>
<td>10.8280</td>
</tr>
<tr>
<td>Total</td>
<td>927.20</td>
<td> </td>
<td>1416.00</td>
<td> </td>
<td>274.20</td>
<td>60.90</td>
<td>8.0889</td>
<td>17.83</td>
<td>5.3926</td>
</tr>
<tr>
<td>Conditioned Total</td>
<td>927.20</td>
<td> </td>
<td>1416.00</td>
<td> </td>
<td>274.20</td>
<td>60.90</td>
<td>8.0889</td>
<td>17.83</td>
<td>5.3926</td>
</tr>
<tr>
<td>Unconditioned Total</td>
<td>0.00</td>
<td> </td>
<td>0.00</td>
<td> </td>
<td>0.00</td>
<td>0.00</td>
<td> </td>
<td> </td>
<td> </td>
</tr>
</table>



### Source Energy End Use Components Summary

The source energy end use components summary gives a good overview of the source energy consumption broken down by end uses for the facility being simulated:



Report: **SourceEnergyEndUseComponentsSummary**

For: **Entire Facility**

Timestamp: **2011-10-07 20:53:43**

**Values gathered over 8760.00 hours**

 **Source Energy End Use Components Summary**

<table class="table table-striped">
<tr>
<th></th>
<th>Source Electricity [GJ]</th>
<th>Source Natural Gas [GJ]</th>
<th>Source Other Fuel [GJ]</th>
<th>Source District Cooling [GJ]</th>
<th>Source District Heating [GJ]</th>
</tr>
<tr>
<td>Heating</td>
<td>0.00</td>
<td>140.03</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Cooling</td>
<td>167.65</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Lighting</td>
<td>569.39</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Lighting</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Equipment</td>
<td>325.49</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Equipment</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Fans</td>
<td>55.72</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Pumps</td>
<td>7.57</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Rejection</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Humidification</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Recovery</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Water Systems</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Refrigeration</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Generators</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Total Source Energy End Use Components</td>
<td>1125.82</td>
<td>140.03</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
</table>


 **Normalized Metrics**

 **Source Energy End Use Components Per Conditioned Floor Area**

<table class="table table-striped">
<tr>
<th></th>
<th>Source Electricity [MJ/m2]</th>
<th>Source Natural Gas [MJ/m2]</th>
<th>Source Other Fuel [MJ/m2]</th>
<th>Source District Cooling [MJ/m2]</th>
<th>Source District Heating [MJ/m2]</th>
</tr>
<tr>
<td>Heating</td>
<td>0.00</td>
<td>151.02</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Cooling</td>
<td>180.82</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Lighting</td>
<td>614.09</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Lighting</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Equipment</td>
<td>351.05</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Equipment</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Fans</td>
<td>60.10</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Pumps</td>
<td>8.16</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Rejection</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Humidification</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Recovery</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Water Systems</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Refrigeration</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Generators</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Total Source Energy End Use Components</td>
<td>1214.22</td>
<td>151.02</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
</table>


 **Source Energy End Use Components Per Total Floor Area**

<table class="table table-striped">
<tr>
<th></th>
<th>Source Electricity [MJ/m2]</th>
<th>Source Natural Gas [MJ/m2]</th>
<th>Source Other Fuel [MJ/m2]</th>
<th>Source District Cooling [MJ/m2]</th>
<th>Source District Heating [MJ/m2]</th>
</tr>
<tr>
<td>Heating</td>
<td>0.00</td>
<td>151.02</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Cooling</td>
<td>180.82</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Lighting</td>
<td>614.09</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Lighting</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Equipment</td>
<td>351.05</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Equipment</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Fans</td>
<td>60.10</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Pumps</td>
<td>8.16</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Rejection</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Humidification</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Recovery</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Water Systems</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Refrigeration</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Generators</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>Total Source Energy End Use Components</td>
<td>1214.22</td>
<td>151.02</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
</table>

### Other Summary Reports

The Climatic Data Summary, Envelope Summary and Equipment Summary reports might be quite important to some reporting or presentation.



Report: **ClimaticDataSummary**

For: **Entire Facility**

**Timestamp: 2009-09-17 17:30:22**

SizingPeriod:DesignDay

<table class="table table-striped">
<tr>
<th> </th>
<th>Maximum Dry Bulb [C]</th>
<th>Daily Temperature Range [C]</th>
<th>Humidity Value</th>
<th>Humidity Type</th>
<th>Wind Speed [m/s]</th>
<th>Wind Direction</th>
</tr>
<tr>
<td>LOS_ANGELES_CA_USA HEATING 99% CONDITIONS</td>
<td>7.40</td>
<td>0.00</td>
<td>7.40</td>
<td>0</td>
<td>2.80</td>
<td>70.00</td>
</tr>
<tr>
<td>LOS_ANGELES_CA_USA COOLING 1% CONDITIONS DB=&gt;MWB</td>
<td>27.00</td>
<td>6.10</td>
<td>17.60</td>
<td>0</td>
<td>4.40</td>
<td>250.00</td>
</tr>
</table>



Weather Statistics File

<table class="table table-striped">
<tr>
<th> </th>
<th>Value</th>
</tr>
<tr>
<td>Reference</td>
<td>CZ06RV2</td>
</tr>
<tr>
<td>Site:Location</td>
<td>Climate Zone 6 CA USA</td>
</tr>
<tr>
<td>Latitude</td>
<td>N 33° 54'</td>
</tr>
<tr>
<td>Longitude</td>
<td>W 118° 30'</td>
</tr>
<tr>
<td>Time Zone</td>
<td>GMT -8.0 Hours</td>
</tr>
<tr>
<td>Elevation</td>
<td>30m above sea level</td>
</tr>
<tr>
<td>Standard Pressure at Elevation</td>
<td>100965Pa</td>
</tr>
<tr>
<td>Data Source</td>
<td>WYEC2-C-00006</td>
</tr>
<tr>
<td>WMO Station</td>
<td>722970</td>
</tr>
<tr>
<td>Design Conditions</td>
<td>Climate Design Data 2005 ASHRAE Handbook</td>
</tr>
<tr>
<td>Heating Design Temperature 99.6% (C)</td>
<td>4.9</td>
</tr>
<tr>
<td>Heating Design Temperature 99% (C)</td>
<td>6.2</td>
</tr>
<tr>
<td>Cooling Design Temperature 0.4% (C)</td>
<td>33.1</td>
</tr>
<tr>
<td>Cooling Design Temperature 1% (C)</td>
<td>30.9</td>
</tr>
<tr>
<td>Cooling Design Temperature 2% (C)</td>
<td>29.2</td>
</tr>
<tr>
<td>Maximum Dry Bulb Temperature (C)</td>
<td>31.7°</td>
</tr>
<tr>
<td>Maximum Dry Bulb Occurs on</td>
<td>Sep 24</td>
</tr>
<tr>
<td>Minimum Dry Bulb Temperature (C)</td>
<td>3.5°</td>
</tr>
<tr>
<td>Minimum Dry Bulb Occurs on</td>
<td>Jan 8</td>
</tr>
<tr>
<td>Maximum Dew Point Temperature (C)</td>
<td>20.9</td>
</tr>
<tr>
<td>Maximum Dew Point Occurs on</td>
<td>Jun 20</td>
</tr>
<tr>
<td>Minimum Dew Point Temperature (C)</td>
<td>-21.7</td>
</tr>
<tr>
<td>Minimum Dew Point Occurs on</td>
<td>Dec 27</td>
</tr>
<tr>
<td>Heating Degree-Days (base 10°C)</td>
<td>2</td>
</tr>
<tr>
<td>Cooling Degree-Days (base 18°C)</td>
<td>216</td>
</tr>
<tr>
<td>Köppen Classification</td>
<td>Csb</td>
</tr>
<tr>
<td>Köppen Description</td>
<td>Mediterranean climate (dry warm summer, mild winter, lat. 30-45°N)</td>
</tr>
<tr>
<td>Köppen Recommendation</td>
<td> </td>
</tr>
<tr>
<td>ASHRAE Climate Zone</td>
<td>3C</td>
</tr>
<tr>
<td>ASHRAE Description</td>
<td>Warm-Marine</td>
</tr>
</table>





Report: **EnvelopeSummary**

For: **Entire Facility**

**Timestamp: 2009-09-17 17:30:22**

Opaque Exterior

<table class="table table-striped">
<tr>
<th> </th>
<th>Construction</th>
<th>Reflectance</th>
<th>U-Factor with Film [W/m2-K]</th>
<th>U-Factor no Film [W/m2-K]</th>
<th>Gross Area [m2]</th>
<th>Azimuth [deg]</th>
<th>Tilt [deg]</th>
<th>Cardinal Direction</th>
</tr>
<tr>
<td>WALL-1PF</td>
<td>WALL-1</td>
<td>0.22</td>
<td>0.384</td>
<td>0.408</td>
<td>18.30</td>
<td>210.00</td>
<td>90.00</td>
<td>S</td>
</tr>
<tr>
<td>WALL-1PR</td>
<td>WALL-1</td>
<td>0.22</td>
<td>0.384</td>
<td>0.408</td>
<td>9.12</td>
<td>120.00</td>
<td>90.00</td>
<td>E</td>
</tr>
<tr>
<td>WALL-1PB</td>
<td>WALL-1</td>
<td>0.22</td>
<td>0.384</td>
<td>0.408</td>
<td>18.30</td>
<td>30.00</td>
<td>90.00</td>
<td>N</td>
</tr>
<tr>
<td>WALL-1PL</td>
<td>WALL-1</td>
<td>0.22</td>
<td>0.384</td>
<td>0.408</td>
<td>9.12</td>
<td>300.00</td>
<td>90.00</td>
<td>W</td>
</tr>
<tr>
<td>TOP-1</td>
<td>ROOF-1</td>
<td>0.35</td>
<td>0.268</td>
<td>0.282</td>
<td>463.60</td>
<td>210.00</td>
<td>0.00</td>
<td> </td>
</tr>
<tr>
<td>FRONT-1</td>
<td>WALL-1</td>
<td>0.22</td>
<td>0.384</td>
<td>0.408</td>
<td>73.20</td>
<td>210.00</td>
<td>90.00</td>
<td>S</td>
</tr>
<tr>
<td>F1-1</td>
<td>FLOOR-SLAB-1</td>
<td>0.35</td>
<td>3.119</td>
<td>12.894</td>
<td>99.16</td>
<td>30.00</td>
<td>180.00</td>
<td> </td>
</tr>
<tr>
<td>RIGHT-1</td>
<td>WALL-1</td>
<td>0.22</td>
<td>0.384</td>
<td>0.408</td>
<td>36.48</td>
<td>120.00</td>
<td>90.00</td>
<td>E</td>
</tr>
<tr>
<td>F2-1</td>
<td>FLOOR-SLAB-1</td>
<td>0.35</td>
<td>3.119</td>
<td>12.894</td>
<td>42.73</td>
<td>300.00</td>
<td>180.00</td>
<td> </td>
</tr>
<tr>
<td>BACK-1</td>
<td>WALL-1</td>
<td>0.22</td>
<td>0.384</td>
<td>0.408</td>
<td>73.20</td>
<td>30.00</td>
<td>90.00</td>
<td>N</td>
</tr>
<tr>
<td>F3-1</td>
<td>FLOOR-SLAB-1</td>
<td>0.35</td>
<td>3.119</td>
<td>12.894</td>
<td>96.48</td>
<td>74.22</td>
<td>180.00</td>
<td> </td>
</tr>
<tr>
<td>LEFT-1</td>
<td>WALL-1</td>
<td>0.22</td>
<td>0.384</td>
<td>0.408</td>
<td>36.48</td>
<td>300.00</td>
<td>90.00</td>
<td>W</td>
</tr>
<tr>
<td>F4-1</td>
<td>FLOOR-SLAB-1</td>
<td>0.35</td>
<td>3.119</td>
<td>12.894</td>
<td>42.73</td>
<td>120.00</td>
<td>180.00</td>
<td> </td>
</tr>
<tr>
<td>F5-1</td>
<td>FLOOR-SLAB-1</td>
<td>0.35</td>
<td>3.119</td>
<td>12.894</td>
<td>182.49</td>
<td>30.00</td>
<td>180.00</td>
<td> </td>
</tr>
</table>



 Fenestration

<table class="table table-striped">
<tr>
<th> </th>
<th>Construction</th>
<th>Area of One Opening [m2]</th>
<th>Area of Openings [m2]</th>
<th>U-Factor [W/m2-K]</th>
<th>SHGC</th>
<th>Visible Transmittance</th>
<th>Shade Control</th>
<th>Parent Surface</th>
<th>Azimuth [deg]</th>
<th>Cardinal Direction</th>
</tr>
<tr>
<td>WF-1</td>
<td>DBL CLR 3MM/13MM AIR</td>
<td>16.56</td>
<td>16.56</td>
<td>2.720</td>
<td>0.764</td>
<td>0.812</td>
<td>No</td>
<td>FRONT-1</td>
<td>210.00</td>
<td>S</td>
</tr>
<tr>
<td>DF-1</td>
<td>SGL GREY 3MM</td>
<td>5.25</td>
<td>5.25</td>
<td>5.894</td>
<td>0.716</td>
<td>0.611</td>
<td>No</td>
<td>FRONT-1</td>
<td>210.00</td>
<td>S</td>
</tr>
<tr>
<td>WR-1</td>
<td>DBL CLR 3MM/13MM AIR</td>
<td>9.12</td>
<td>9.12</td>
<td>2.720</td>
<td>0.764</td>
<td>0.812</td>
<td>No</td>
<td>RIGHT-1</td>
<td>120.00</td>
<td>E</td>
</tr>
<tr>
<td>WB-1</td>
<td>DBL CLR 3MM/13MM AIR</td>
<td>16.44</td>
<td>16.44</td>
<td>2.720</td>
<td>0.764</td>
<td>0.812</td>
<td>No</td>
<td>BACK-1</td>
<td>30.00</td>
<td>N</td>
</tr>
<tr>
<td>DB-1</td>
<td>SGL GREY 3MM</td>
<td>4.41</td>
<td>4.41</td>
<td>5.894</td>
<td>0.716</td>
<td>0.611</td>
<td>No</td>
<td>BACK-1</td>
<td>30.00</td>
<td>N</td>
</tr>
<tr>
<td>WL-1</td>
<td>DBL CLR 3MM/13MM AIR</td>
<td>9.12</td>
<td>9.12</td>
<td>2.720</td>
<td>0.764</td>
<td>0.812</td>
<td>No</td>
<td>LEFT-1</td>
<td>300.00</td>
<td>W</td>
</tr>
<tr>
<td>Total or Average</td>
<td> </td>
<td> </td>
<td>60.90</td>
<td>3.22</td>
<td>0.756</td>
<td>0.780</td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
</tr>
<tr>
<td>North Total or Average</td>
<td> </td>
<td> </td>
<td>20.85</td>
<td>3.39</td>
<td>0.754</td>
<td>0.769</td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
</tr>
<tr>
<td>Non-North Total or Average</td>
<td> </td>
<td> </td>
<td>40.05</td>
<td>3.14</td>
<td>0.757</td>
<td>0.785</td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
</tr>
</table>





------------------------------------------------------------------------

Report: **ShadingSummary**

For: **Entire Facility**

**Timestamp: 2009-09-17 17:30:22**

Sunlit Fraction

<table class="table table-striped">
<tr>
<th> </th>
<th>March 21 9am</th>
<th>March 21 noon</th>
<th>March 21 3pm</th>
<th>June 21 9am</th>
<th>June 21 noon</th>
<th>June 21 3pm</th>
<th>December 21 9am</th>
<th>December 21 noon</th>
<th>December 21 3pm</th>
</tr>
<tr>
<td>WF-1</td>
<td>0.00</td>
<td>0.00</td>
<td>0.26</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.30</td>
<td>0.74</td>
</tr>
<tr>
<td>DF-1</td>
<td>0.75</td>
<td>0.13</td>
<td>0.59</td>
<td>0.00</td>
<td>0.00</td>
<td>0.35</td>
<td>0.92</td>
<td>0.62</td>
<td>0.94</td>
</tr>
<tr>
<td>WR-1</td>
<td>1.00</td>
<td>1.00</td>
<td>0.00</td>
<td>1.00</td>
<td>1.00</td>
<td>0.00</td>
<td>1.00</td>
<td>1.00</td>
<td>0.00</td>
</tr>
<tr>
<td>WB-1</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>1.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>DB-1</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>1.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>WL-1</td>
<td>0.00</td>
<td>0.00</td>
<td>1.00</td>
<td>0.00</td>
<td>0.00</td>
<td>1.00</td>
<td>0.00</td>
<td>0.00</td>
<td>1.00</td>
</tr>
</table>



 Window Control

<table class="table table-striped">
<tr>
<th> </th>
<th>Name</th>
<th>Type</th>
<th>Shaded Construction</th>
<th>Control</th>
<th>Glare Control</th>
</tr>
<tr>
<td>None</td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
</tr>
</table>





Report: **LightingSummary**

For: **Entire Facility**

**Timestamp: 2009-09-17 17:30:22**

Interior Lighting

<table class="table table-striped">
<tr>
<th> </th>
<th>Zone</th>
<th>Lighting Power Density [W/m2]</th>
<th>Zone Area [m2]</th>
<th>Total Power [W]</th>
<th>End Use Subcategory</th>
<th>Schedule Name</th>
<th>Average Hours/Week [hr]</th>
<th>Return Air Fraction</th>
<th>Conditioned (Y/N)</th>
</tr>
<tr>
<td>SPACE1-1 LIGHTS 1</td>
<td>SPACE1-1</td>
<td>15.9742</td>
<td>99.16</td>
<td>1584.00</td>
<td>GeneralLights</td>
<td>LIGHTS-1</td>
<td>0.00</td>
<td>0.2000</td>
<td>Y</td>
</tr>
<tr>
<td>SPACE2-1 LIGHTS 1</td>
<td>SPACE2-1</td>
<td>16.0056</td>
<td>42.73</td>
<td>684.00</td>
<td>GeneralLights</td>
<td>LIGHTS-1</td>
<td>0.00</td>
<td>0.2000</td>
<td>Y</td>
</tr>
<tr>
<td>SPACE3-1 LIGHTS 1</td>
<td>SPACE3-1</td>
<td>16.4179</td>
<td>96.48</td>
<td>1584.00</td>
<td>GeneralLights</td>
<td>LIGHTS-1</td>
<td>0.00</td>
<td>0.2000</td>
<td>Y</td>
</tr>
<tr>
<td>SPACE4-1 LIGHTS 1</td>
<td>SPACE4-1</td>
<td>16.0056</td>
<td>42.73</td>
<td>684.00</td>
<td>GeneralLights</td>
<td>LIGHTS-1</td>
<td>0.00</td>
<td>0.2000</td>
<td>Y</td>
</tr>
<tr>
<td>SPACE5-1 LIGHTS 1</td>
<td>SPACE5-1</td>
<td>16.2420</td>
<td>182.49</td>
<td>2964.00</td>
<td>GeneralLights</td>
<td>LIGHTS-1</td>
<td>0.00</td>
<td>0.2000</td>
<td>Y</td>
</tr>
<tr>
<td>Interior Lighting Total</td>
<td> </td>
<td>16.1777</td>
<td>463.60</td>
<td>7500.00</td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
</tr>
</table>



 Daylighting

<table class="table table-striped">
<tr>
<th> </th>
<th>Zone</th>
<th>Daylighting Type</th>
<th>Control Type</th>
<th>Fraction Controlled</th>
<th>Lighting Installed in Zone [W]</th>
<th>Lighting Controlled [W]</th>
</tr>
<tr>
<td>None</td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
</tr>
</table>



 Exterior Lighting

<table class="table table-striped">
<tr>
<th> </th>
<th>Total Watts</th>
<th>Astronomical Clock/Schedule</th>
<th>Schedule Name</th>
<th>Average Annual Hours/Week</th>
</tr>
<tr>
<td>Exterior Lighting Total</td>
<td>0.00</td>
<td> </td>
<td> </td>
<td> </td>
</tr>
</table>





Report: **EquipmentSummary**

For: **Entire Facility**

**Timestamp: 2009-09-17 17:30:22**

Central Plant

<table class="table table-striped">
<tr>
<th> </th>
<th>Type</th>
<th>Nominal Capacity [W]</th>
<th>Nominal Efficiency [W/W]</th>
</tr>
<tr>
<td>CENTRAL CHILLER</td>
<td>Chiller:Electric</td>
<td>25311.64</td>
<td>3.20</td>
</tr>
<tr>
<td>CENTRAL BOILER</td>
<td>Boiler:HotWater</td>
<td>56191.29</td>
<td>0.80</td>
</tr>
</table>



 Cooling Coils

<table class="table table-striped">
<tr>
<th> </th>
<th>Type</th>
<th>Nominal Total Capacity [W]</th>
<th>Nominal Sensible Capacity [W]</th>
<th>Nominal Latent Capacity [W]</th>
<th>Nominal Sensible Heat Ratio</th>
<th>Nominal Efficiency [W/W]</th>
</tr>
<tr>
<td>MAIN COOLING COIL 1</td>
<td>Coil:Cooling:Water</td>
<td>28159.12</td>
<td>19476.53</td>
<td>8682.60</td>
<td>0.69</td>
<td>-</td>
</tr>
</table>



 Heating Coils

<table class="table table-striped">
<tr>
<th> </th>
<th>Type</th>
<th>Nominal Total Capacity [W]</th>
<th>Nominal Efficiency [W/W]</th>
</tr>
<tr>
<td>SPACE1-1 ZONE COIL</td>
<td>Coil:Heating:Water</td>
<td>10384.85</td>
<td>-</td>
</tr>
<tr>
<td>SPACE2-1 ZONE COIL</td>
<td>Coil:Heating:Water</td>
<td>8470.48</td>
<td>-</td>
</tr>
<tr>
<td>SPACE3-1 ZONE COIL</td>
<td>Coil:Heating:Water</td>
<td>10477.88</td>
<td>-</td>
</tr>
<tr>
<td>SPACE4-1 ZONE COIL</td>
<td>Coil:Heating:Water</td>
<td>11334.29</td>
<td>-</td>
</tr>
<tr>
<td>SPACE5-1 ZONE COIL</td>
<td>Coil:Heating:Water</td>
<td>11391.33</td>
<td>-</td>
</tr>
<tr>
<td>MAIN HEATING COIL 1</td>
<td>Coil:Heating:Water</td>
<td>4061.83</td>
<td>-</td>
</tr>
</table>



 Fans

<table class="table table-striped">
<tr>
<th> </th>
<th>Type</th>
<th>Total Efficiency [W/W]</th>
<th>Delta Pressure [pa]</th>
<th>Max Flow Rate [m3/s]</th>
<th>Rated Power [W]</th>
<th>Motor Heat In Air Fraction</th>
<th>End Use</th>
</tr>
<tr>
<td>SUPPLY FAN 1</td>
<td>Fan:VariableVolume</td>
<td>0.70</td>
<td>600.00</td>
<td>1.28</td>
<td>1099.23</td>
<td>1.00</td>
<td>General</td>
</tr>
</table>



 Pumps

<table class="table table-striped">
<tr>
<th> </th>
<th>Type</th>
<th>Control</th>
<th>Head [pa]</th>
<th>Power [W]</th>
<th>Motor Efficiency [W/W]</th>
</tr>
<tr>
<td>HW CIRC PUMP</td>
<td>Pump:VariableSpeed</td>
<td>INTERMITTENT</td>
<td>179352.00</td>
<td>312.23</td>
<td>0.90</td>
</tr>
<tr>
<td>CW CIRC PUMP</td>
<td>Pump:VariableSpeed</td>
<td>INTERMITTENT</td>
<td>179352.00</td>
<td>231.95</td>
<td>0.90</td>
</tr>
</table>



 Service Water Heating

<table class="table table-striped">
<tr>
<th> </th>
<th>Type</th>
<th>Storage Volume [m3]</th>
<th>Input [W]</th>
<th>Thermal Efficiency [W/W]</th>
<th>Recovery Efficiency [W/W]</th>
<th>Energy Factor</th>
</tr>
<tr>
<td>None</td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
</tr>
</table>

Features Related to Appendix G and LEED
---------------------------------------

EnergyPlus has several built-in features to make demonstrating compliance with ASHRAE Standard 90.1 Appendix G easier. Appendix G is used in Energy and Atmosphere Credit 1 Optimize Energy Performance of the U.S. Green Building Council (USGBC) building certification system called Leadership in Energy and Environmental Design (LEED) Green Building Rating System™. The following sections describe the features.

### Baseline Building Rotations

Table G3.1 Section 5 Building Envelope of ASHRAE Standard 90.1 Appendix G requires that the baseline building be simulated facing four directions:

Orientation. The baseline building performance shall be generated by simulating the building with its actual orientation and again after rotating the entire building 90, 180, and 270 degrees, then averaging the results.

This provision intends to provide a baseline that is neutral to building orientation so that buildings purposely oriented to minimize energy use can realize a percent savings.

**Step 1.** The first step of performing the building rotations is to include a Compliance:Building object which contains the Building Rotation for Appendix G field. The value of the field should be different for four simulations, 0, 90, 180 and 270 (see Step 2 below). This is further documented in the Input Output Reference.

```idf
Compliance:Building,
    90;                 Building Rotation for Appendix G
```

Also make sure that the HTML summary reports will be generated by including the following:

```idf
OutputControl:Table:Style,
  HTML;                      !- type

Output:Table:SummaryReports,
  AllSummary;                !- type
```

**Step 2.** The ParametricPreprocessor automates the creation of the four simulation input files so that only a single simulation file is needs to be created by the user. By setting the file name suffixes (which are appended to the existing file name for each simulation), setting a variable that varies for the four simulations, and setting the Building Rotation for Appendix G to this variable, the user can automatically create the four files when using EnergyPlus. The following example shows what should be included in the EnergyPlus file. The Parametric objects are described further in the Input Output Reference and the ParametricPreprocessor is described in the Auxiliary Programs documentation.

```
Parametric:FileNameSuffix,
  Names,
  G000,
  G090,
  G180,
  G270;

Parametric:SetValueForRun,
  $appGAngle,            !- Parameter Name
  0.0,                   !- Value 1
  90.0,                  !- Value 2
  180.0,                 !- Value 3
  270.0;                 !- Value 4

Compliance:Building,
    =$appGAngle;        !- Building Rotation for Appendix G {deg}

OutputControl:Table:Style,
  HTML;                      !- type

Output:Table:SummaryReports,
  AllSummary;                !- type
```

**Step 3.** Run the simulation using EP-Launch. Using the “Single Input File” tab of EP-Launch select the input that contains the Compliance:Building, Parametric:FileNameSuffix, Parametric:SetValueForRun objects as shown above in Step 2 as well as the weather file. In EP-Launch, make sure under VIEW .. OPTIONS .. MISCELLANEOUS that the RUN PARAMETRICPREPROCESSOR option is checked.

**Step 4**. Review the results and revise the model inputs as needed. You can view the results of the simulation by using the EP-Launch and the History tab. At the bottom of the history list, four simulations that use the G000, G090, G180 and G270 file name suffixes should appear and the result files associated with each can be selected and opened.

**Step 5.** Use the AppGPostProcess to average results across the simulations. In EP-Launch under the UTILITIES tab, select AppGPostProcess utility and select one of the HTML files resulting from the multiple simulation runs (e.g., &lt;filename&gt;-G000.html). This will open all four files and generate a new output file with the file suffix GAVG for the HTML and other results files that can be opened on that tab.

### Completing LEED Forms from Tabular Reports

The U.S. Green Building Council building certification system called Leadership in Energy and Environmental Design (LEED) Green Building Rating System™ includes Energy and Atmosphere Credit 1 Optimize Energy Performance. Credit 1 includes an option that requires a series of building energy simulations that follow the procedures of ASHRAE Standard 90.1 Appendix G Performance Rating Method. The  LEED Summary report provides many of the simulation results required for the forms. The report can be produced by specifying LEEDSummary in Output:Table:SummaryReports which is also part of the AllSummary option.



Report: **LEED Summary**

For: **Entire Facility**

Timestamp: **2013-03-01 15:24:37**

**Sec1.1A-General Information**

<table class="table table-striped">
<tr>
<th></th>
<th>Data</th>
</tr>
<tr>
<td>Heating Degree Days</td>
<td>1748</td>
</tr>
<tr>
<td>Cooling Degree Days</td>
<td>506</td>
</tr>
<tr>
<td>Climate Zone</td>
<td>5A</td>
</tr>
<tr>
<td>Weather File</td>
<td>Chicago Ohare Intl Ap IL USA TMY3 WMO#=725300</td>
</tr>
<tr>
<td>HDD and CDD data source</td>
<td>Weather File Stat</td>
</tr>
<tr>
<td>Total gross floor area [m2]</td>
<td>927.20</td>
</tr>
<tr>
<td>Principal Heating Source</td>
<td>Natural Gas</td>
</tr>
</table>



 **EAp2-1. Space Usage Type**

<table class="table table-striped">
<tr>
<th></th>
<th>Space Area [m2]</th>
<th>Regularly Occupied Area [m2]</th>
<th>Unconditioned Area [m2]</th>
<th>Typical Hours/Week in Operation [hr/wk]</th>
</tr>
<tr>
<td>SPACE1-1</td>
<td>99.16</td>
<td>99.16</td>
<td>0.00</td>
<td>55.06</td>
</tr>
<tr>
<td>SPACE2-1</td>
<td>42.73</td>
<td>42.73</td>
<td>0.00</td>
<td>55.06</td>
</tr>
<tr>
<td>SPACE3-1</td>
<td>96.48</td>
<td>96.48</td>
<td>0.00</td>
<td>55.06</td>
</tr>
<tr>
<td>SPACE4-1</td>
<td>42.73</td>
<td>42.73</td>
<td>0.00</td>
<td>55.06</td>
</tr>
<tr>
<td>SPACE5-1</td>
<td>182.49</td>
<td>182.49</td>
<td>0.00</td>
<td>55.06</td>
</tr>
<tr>
<td>PLENUM-1</td>
<td>463.60</td>
<td>463.60</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Totals</td>
<td>927.20</td>
<td>927.20</td>
<td>0.00</td>
<td> </td>
</tr>
</table>



 **EAp2-2. Advisory Messages**

<table class="table table-striped">
<tr>
<th></th>
<th>Data</th>
</tr>
<tr>
<td>Number of hours heating loads not met</td>
<td>0.00</td>
</tr>
<tr>
<td>Number of hours cooling loads not met</td>
<td>10.75</td>
</tr>
<tr>
<td>Number of hours not met</td>
<td>10.75</td>
</tr>
</table>



 **EAp2-3. Energy Type Summary**

<table class="table table-striped">
<tr>
<th></th>
<th>Utility Rate</th>
<th>Virtual Rate [$/unit energy]</th>
<th>Units of Energy</th>
<th>Units of Demand</th>
</tr>
<tr>
<td>Electricity</td>
<td>EXAMPLEA EXAMPLEI-SELL</td>
<td>0.055</td>
<td>kWh</td>
<td>kW</td>
</tr>
<tr>
<td>Natural Gas</td>
<td>EXAMPLEA-GAS</td>
<td>0.569</td>
<td>Therm</td>
<td>Therm/Hr</td>
</tr>
<tr>
<td>Other</td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
</tr>
</table>



 **EAp2-4/5. Performance Rating Method Compliance**

<table class="table table-striped">
<tr>
<th></th>
<th>Electric Energy Use [GJ]</th>
<th>Electric Demand [W]</th>
<th>Natural Gas Energy Use [GJ]</th>
<th>Natural Gas Demand [W]</th>
<th>Other Energy Use [GJ]</th>
<th>Other Demand [W]</th>
</tr>
<tr>
<td>Interior Lighting</td>
<td>81.24</td>
<td>7125.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Exterior Lighting</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Space Heating</td>
<td>0.00</td>
<td>0.00</td>
<td>103.92</td>
<td>62499.99</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Space Cooling</td>
<td>17.63</td>
<td>9523.66</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Pumps</td>
<td>1.54</td>
<td>319.57</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Heat Rejection</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Fans-Interior</td>
<td>7.01</td>
<td>609.44</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Fans-Parking Garage</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Service Water Heating</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Receptacle Equipment</td>
<td>47.70</td>
<td>4500.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Interior Lighting (process)</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Refrigeration Equipment</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Cooking</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Industrial Process</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Elevators and Escalators</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Total Line</td>
<td>155.12</td>
<td> </td>
<td>274.01</td>
<td> </td>
<td>0.00</td>
<td> </td>
</tr>
</table>



 **EAp2-6. Energy Use Summary**

<table class="table table-striped">
<tr>
<th></th>
<th>Process Subtotal [GJ]</th>
<th>Total Energy Use [GJ]</th>
</tr>
<tr>
<td>Electricity</td>
<td>47.70</td>
<td>155.12</td>
</tr>
<tr>
<td>Natural Gas</td>
<td>0.00</td>
<td>274.01</td>
</tr>
<tr>
<td>Other</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Total</td>
<td>47.70</td>
<td>429.13</td>
</tr>
</table>



 **EAp2-7. Energy Cost Summary**

<table class="table table-striped">
<tr>
<th></th>
<th>Process Subtotal [$]</th>
<th>Total Energy Cost [$]</th>
</tr>
<tr>
<td>Electricity</td>
<td>552.55</td>
<td>1796.99</td>
</tr>
<tr>
<td>Natural Gas</td>
<td>0.00</td>
<td>1478.58</td>
</tr>
<tr>
<td>Other</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Total</td>
<td>552.55</td>
<td>3275.57</td>
</tr>
</table>

*Process energy cost based on ratio of process to total energy.*

 **L-1. Renewable Energy Source Summary**

<table class="table table-striped">
<tr>
<th></th>
<th>Rated Capacity [kW]</th>
<th>Annual Energy Generated [GJ]</th>
</tr>
<tr>
<td>Photovoltaic</td>
<td>0.00</td>
<td>0.00</td>
</tr>
<tr>
<td>Wind</td>
<td>0.00</td>
<td>0.00</td>
</tr>
</table>



 **EAp2-17a. Energy Use Intensity - Electricity**

<table class="table table-striped">
<tr>
<th></th>
<th>Electricty [MJ/m2]</th>
</tr>
<tr>
<td>Interior Lighting</td>
<td>87.62</td>
</tr>
<tr>
<td>Space Heating</td>
<td>0.00</td>
</tr>
<tr>
<td>Space Cooling</td>
<td>19.02</td>
</tr>
<tr>
<td>Fans-Interior</td>
<td>7.56</td>
</tr>
<tr>
<td>Service Water Heating</td>
<td>0.00</td>
</tr>
<tr>
<td>Receptacle Equipment</td>
<td>51.44</td>
</tr>
<tr>
<td>Miscellaneous</td>
<td>1.66</td>
</tr>
<tr>
<td>Subtotal</td>
<td>167.30</td>
</tr>
</table>



 **EAp2-17b. Energy Use Intensity - Natural Gas**

<table class="table table-striped">
<tr>
<th></th>
<th>Natural Gas [MJ/m2]</th>
</tr>
<tr>
<td>Space Heating</td>
<td>112.08</td>
</tr>
<tr>
<td>Service Water Heating</td>
<td>0.00</td>
</tr>
<tr>
<td>Miscellaneous</td>
<td>183.45</td>
</tr>
<tr>
<td>Subtotal</td>
<td>295.53</td>
</tr>
</table>



 **EAp2-17c. Energy Use Intensity - Other**

<table class="table table-striped">
<tr>
<th></th>
<th>Other [MJ/m2]</th>
</tr>
<tr>
<td>Miscellaneous</td>
<td>0.00</td>
</tr>
<tr>
<td>Subtotal</td>
<td>0.00</td>
</tr>
</table>



 **EAp2-18. End Use Percentage**

<table class="table table-striped">
<tr>
<th></th>
<th>Percent [%]</th>
</tr>
<tr>
<td>Interior Lighting</td>
<td>18.93</td>
</tr>
<tr>
<td>Space Heating</td>
<td>24.22</td>
</tr>
<tr>
<td>Space Cooling</td>
<td>4.11</td>
</tr>
<tr>
<td>Fans-Interior</td>
<td>1.63</td>
</tr>
<tr>
<td>Service Water Heating</td>
<td>0.00</td>
</tr>
<tr>
<td>Receptacle Equipment</td>
<td>11.11</td>
</tr>
<tr>
<td>Miscellaneous</td>
<td>39.99</td>
</tr>
</table>



#### LEED Form Section 1.4 – Comparison of Proposed Design Versus Baseline Design

Unlike other portions of the LEED forms, Section 1.4 Comparison of Proposed Design Versus Baseline Design is less structured with many possible “Model Input Parameters” to match with the “Proposed Design Input” and “Baseline Design Input”.

*1. Exterior wall, underground wall, roof, floor, and slab assemblies including framing type, assembly R-values, assembly U-factors, and roof reflectivity when modeling cool roofs. (Refer to ASHRAE 90.1 Appendix A)*

Use the values from the Envelope Summary table, Opaque Exterior subtable for Construction, Reflectance, U-Factor with Film, and U-Factor no Film.

*2. Fenestration types, assembly U-factors (including the impact of the frame on the assembly, SHGCs, and visual light transmittances, overall window-to-gross wall ratio, fixed shading devices, and automated movable shading devices*

Use the values from Input Verification and Results Summary table, Window-Wall Ratio table Window-Wall Ratio percentage as well as Skylight-Roof Ratio table. Also use the values from the Envelope Summary table, Fenestration subtable for Construction, Area of Openings, U-Factor, SHGC, Visible Transmittance, and Shade Control. For shading use the Shading Summary report including Sunlit Fraction and Window Control.

*3. Interior lighting power densities, exterior lighting power, process lighting power, and lighting controls modeled for credit.*

Use the values from the Lighting Summary report for Interior Lighting, Daylighting and Exterior Lighting including Lighting Power Density, Schedule Name, Average Hours/Week, Daylighting Type, Control Type, Fraction Controlled, Lighting Installed in Zone, Lighting Controlled.

*4. Receptacle equipment, elevators or escalators, refrigeration equipment and other process loads.*

Use the values from Input Verification and Results Summary table, Zone Summary subtable for Plug and Process.

*5. HVAC system information including types and efficiencies, exhaust heat recovery, pump power and controls, and other pertinent system information. (Include the ASRHAE 90.1-2004 Table G3.1.1B Baseline System Number)*

Use the Equipment Summary report, Central Plant subtable for Type, Nominal Capacity and Nominal Efficiency; Cooling Coils subtable for Nominal Total Capacity and Nominal Efficiency; Heating Coils subtable for Type, Nominal Total Capacity and Nominal Efficiency; Fans subtable for Type, Total Efficiency, Delta Pressure, Max Flow Rate, Rated Power, Motor Heat in Air Fraction; Pumps subtable for Type, Control, Head, Power and Motor Efficiency. Use the System Summary report, Economizer subtable for High Limit Shutoff Control and Minimum Outdoor Air; the Demand Controlled Ventilation using Controller:MechanicalVentilation subtable.

*6. Domestic hot water system type, efficiency and storage tank volume.*

Use the Equipment Summary report, Service Water Heating subtable for Type, Storage Volume, Input, Thermal Efficiency, Recovery Efficiency, and Energy Factor.

*7. General schedule information.*

Use the Lighting Summary report, Interior Lighting subtable Schedule Name and Average Hours/Week; Exterior Lighting subtable Schedule Name and Average Hours/Week. Use the System Summary report, Demand Controlled Ventilation using Controller:MechanicalVentilation subtable for the Air Distribution Effectiveness Schedule.

Features Related to California Title 24 Compliance
--------------------------------------------------

EnergyPlus has a reporting feature that is useful for obtaining Time Dependant Valuation (TDV) of source energy use for the entire facility being simulated. Hourly variations of fuel factors can be specified through the 'FuelFactors' input object and will be used to calculate TDV based source energy consumption broken down by end uses. The Source Energy End Use Components Summary report above provides these TDV based values. The source energy consumption is also reported based on area normalized metrics. Refer to 'Fuel Factors' and 'Output:Table:SummaryReports' in the Input-Output Reference manual for more details.

Also included is the California\_Title\_24-2008.idf dataset (described briefly in the Output Details document).


