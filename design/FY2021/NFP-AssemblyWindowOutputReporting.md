Assembly Window Output Reporting
================

**Jason Glazer, GARD Analytics**

 - April 16, 2021
 

## Justification for New Feature ##

Currently, EnergyPlus does not provide or allow output reporting for assembly 
U-factors for glazing systems. This task will enhance EnergyPlus output 
reporting and allow reporting for assembly U-factors for glazing systems. The 
glazing systems shall include windows and frames. 

In addition, [Issue #6530](https://github.com/NREL/EnergyPlus/issues/6530) says:

> Problem: EnergyPlus only reports U-factor, SHGC, and VT for center-of-glass. It 
> does not report those values for the whole window.
> 
> Rationale: It is very important for modelers to confirm that the window systems 
> (glass layers, gas layers, frames, dividers, shades, etc.) have been input 
> correctly and reporting standard rating values from EnergyPlus’s models is very
> helpful to close the loop.
> 
> Solution: Add content to the fenestration summary report for whole window 
> U/SHGC/VT. Calculate the whole-window metrics using procedures defined by NFRC 
> and consistent with LBNL Window program. Correct any problems with how frame and 
> edge-of-glass regions are input and modeled to make EnergyPlus’s whole-window 
> representation consistent with NFRC and LBNL Window program.
> 
> Context: Frame and edge-of-glass effects can be significant making whole-window 
> performance values very different from center-of-glass values. NFRC uses 
> whole-window U/SHGC/VT for performance ratings. EnergyPlus echoes out 
> center-of-glass U/SHGC/VT but that information is not complete when modeling 
> frames, dividers, etc.

## E-mail and  Conference Call Conclusions ##

None yet.

## Overview ##

Use the updated Windows Calculation Engine to compute the overall assembly 
U-factor, SHGC, and VT for fenestration.

## Approach ##

The Windows Calculation Engine (WCE) has recently been updated to include the 
calculation of the overall assembly U-factor, SHGC, and VT for fenestration that
includes frames. This will be getting merged into EnergyPlus and after that the
window.vt(), window.uValue(), window.SHGC() methods will be used for calculating
the overall window properties. Converting the current frame information into frame
information compatible with the WCE will need to be done.

The output will be new columns in the Exterior Fenestration and Interior Fenestration 
tables of the Envelope Summary report.

## Testing/Validation/Data Sources ##

Comparing results with the values from WINDOW program.

## Input Output Reference Documentation ##

The only change the input output reference is shown below with underlines:

7.4.1.1.5 Envelope Summary
The Envelope Summary report (key: EnvelopeSummary) produces a report that 
includes the following tables:

* Opaque which includes all opaque surfaces and includes the name of the 
construction, reflectance, U-Factor, gross area, azimuth, tilt, cardinal 
direction.

* Fenestration which includes all non-opaque surfaces and includes the name of 
the construction, areas (glass, frame, divider, single opening, multiplied 
openings), glass U-Factor, glass SHGC (the solar heat gain coeﬀicient based on 
summer conditions), glass visible transmittance, <ins>assembly U-Factor, assembly 
SHGC, assembly visible transmittance,</ins> conductance (frame, divider), indication 
of shade control, the name of the parent surface, azimuth, tilt, cardinal 
direction. <ins>The assembly result include the effect of the frame and divider.</ins>

## Input Description ##

No input changes are expected.

## Outputs Description ##

The current Envelope Summary report consists of six tables:

* Opaque Exterior
* Opaque Interior
* Exterior Fenestration
* Interior Fenestration
* Exterior Door
* Interior Door

Of these, the Exterior Fenestration and Interior Fenestration tables will be modified.

The current version of those tables is shown below:

<b>Exterior Fenestration</b><br><br>
<!-- FullName:Envelope Summary_Entire Facility_Exterior Fenestration-->
<table border="1" cellpadding="4" cellspacing="0">
  <tr><td></td>
    <td align="right">Construction</td>
    <td align="right">Glass Area [m2]</td>
    <td align="right">Frame Area [m2]</td>
    <td align="right">Divider Area [m2]</td>
    <td align="right">Area of One Opening [m2]</td>
    <td align="right">Area of Multiplied Openings [m2]</td>
    <td align="right">Glass U-Factor [W/m2-K]</td>
    <td align="right">Glass SHGC</td>
    <td align="right">Glass Visible Transmittance</td>
    <td align="right">Frame Conductance [W/m2-K]</td>
    <td align="right">Divider Conductance [W/m2-K]</td>
    <td align="right">Shade Control</td>
    <td align="right">Parent Surface</td>
    <td align="right">Azimuth [deg]</td>
    <td align="right">Tilt [deg]</td>
    <td align="right">Cardinal Direction</td>
  </tr>
  <tr>
    <td align="right">WF-1</td>
    <td align="right">DBL CLR 3MM/13MM AIR</td>
    <td align="right">       16.56</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">       16.56</td>
    <td align="right">       16.56</td>
    <td align="right">       2.720</td>
    <td align="right">       0.764</td>
    <td align="right">       0.812</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">FRONT-1</td>
    <td align="right">      210.00</td>
    <td align="right">       90.00</td>
    <td align="right">S</td>
  </tr>
  <tr>
    <td align="right">DF-1</td>
    <td align="right">SGL GREY 3MM</td>
    <td align="right">        5.25</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">        5.25</td>
    <td align="right">        5.25</td>
    <td align="right">       5.894</td>
    <td align="right">       0.716</td>
    <td align="right">       0.611</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">FRONT-1</td>
    <td align="right">      210.00</td>
    <td align="right">       90.00</td>
    <td align="right">S</td>
  </tr>
  <tr>
    <td align="right">WR-1</td>
    <td align="right">DBL CLR 3MM/13MM AIR</td>
    <td align="right">        9.12</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">        9.12</td>
    <td align="right">        9.12</td>
    <td align="right">       2.720</td>
    <td align="right">       0.764</td>
    <td align="right">       0.812</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">RIGHT-1</td>
    <td align="right">      120.00</td>
    <td align="right">       90.00</td>
    <td align="right">E</td>
  </tr>
  <tr>
    <td align="right">WB-1</td>
    <td align="right">DBL CLR 3MM/13MM AIR</td>
    <td align="right">       16.44</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">       16.44</td>
    <td align="right">       16.44</td>
    <td align="right">       2.720</td>
    <td align="right">       0.764</td>
    <td align="right">       0.812</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">BACK-1</td>
    <td align="right">       30.00</td>
    <td align="right">       90.00</td>
    <td align="right">N</td>
  </tr>
  <tr>
    <td align="right">DB-1</td>
    <td align="right">SGL GREY 3MM</td>
    <td align="right">        4.41</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">        4.41</td>
    <td align="right">        4.41</td>
    <td align="right">       5.894</td>
    <td align="right">       0.716</td>
    <td align="right">       0.611</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">BACK-1</td>
    <td align="right">       30.00</td>
    <td align="right">       90.00</td>
    <td align="right">N</td>
  </tr>
  <tr>
    <td align="right">WL-1</td>
    <td align="right">DBL CLR 3MM/13MM AIR</td>
    <td align="right">        9.12</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">        9.12</td>
    <td align="right">        9.12</td>
    <td align="right">       2.720</td>
    <td align="right">       0.764</td>
    <td align="right">       0.812</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">LEFT-1</td>
    <td align="right">      300.00</td>
    <td align="right">       90.00</td>
    <td align="right">W</td>
  </tr>
  <tr>
    <td align="right">Total or Average</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">       60.90</td>
    <td align="right">       3.223</td>
    <td align="right">       0.756</td>
    <td align="right">       0.780</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
  </tr>
  <tr>
    <td align="right">North Total or Average</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">       20.85</td>
    <td align="right">       3.391</td>
    <td align="right">       0.754</td>
    <td align="right">       0.769</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
  </tr>
  <tr>
    <td align="right">Non-North Total or Average</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">       40.05</td>
    <td align="right">       3.136</td>
    <td align="right">       0.757</td>
    <td align="right">       0.785</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
  </tr>
</table>
<br><br>
<b>Interior Fenestration</b><br><br>
<!-- FullName:Envelope Summary_Entire Facility_Interior Fenestration-->
<table border="1" cellpadding="4" cellspacing="0">
  <tr><td></td>
    <td align="right">Construction</td>
    <td align="right">Area of One Opening [m2]</td>
    <td align="right">Area of Openings [m2]</td>
    <td align="right">Glass U-Factor [W/m2-K]</td>
    <td align="right">Glass SHGC</td>
    <td align="right">Glass Visible Transmittance</td>
    <td align="right">Parent Surface</td>
  </tr>
  <tr>
    <td align="right">Total or Average</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">        0.00</td>
    <td align="right">-</td>
    <td align="right">-</td>
    <td align="right">-</td>
    <td align="right">&nbsp;</td>
  </tr>
</table>

The new columns are propsed to be added after the glass only u-factor, SHGC and VT:

<b>Exterior Fenestration</b><br><br>
<!-- FullName:Envelope Summary_Entire Facility_Exterior Fenestration-->
<table border="1" cellpadding="4" cellspacing="0">
  <tr><td></td>
    <td align="right">Construction</td>
    <td align="right">Glass Area [m2]</td>
    <td align="right">Frame Area [m2]</td>
    <td align="right">Divider Area [m2]</td>
    <td align="right">Area of One Opening [m2]</td>
    <td align="right">Area of Multiplied Openings [m2]</td>
    <td align="right">Glass U-Factor [W/m2-K]</td>
    <td align="right">Glass SHGC</td>
    <td align="right">Glass Visible Transmittance</td>
    <td align="right">Assembly U-Factor [W/m2-K]</td>
    <td align="right">Assembly SHGC</td>
    <td align="right">Assembly Visible Transmittance</td>
    <td align="right">Frame Conductance [W/m2-K]</td>
    <td align="right">Divider Conductance [W/m2-K]</td>
    <td align="right">Shade Control</td>
    <td align="right">Parent Surface</td>
    <td align="right">Azimuth [deg]</td>
    <td align="right">Tilt [deg]</td>
    <td align="right">Cardinal Direction</td>
  </tr>
  <tr>
    <td align="right">WF-1</td>
    <td align="right">DBL CLR 3MM/13MM AIR</td>
    <td align="right">       16.56</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">       16.56</td>
    <td align="right">       16.56</td>
    <td align="right">       2.720</td>
    <td align="right">       0.764</td>
    <td align="right">       0.812</td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">FRONT-1</td>
    <td align="right">      210.00</td>
    <td align="right">       90.00</td>
    <td align="right">S</td>
  </tr>
  <tr>
    <td align="right">DF-1</td>
    <td align="right">SGL GREY 3MM</td>
    <td align="right">        5.25</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">        5.25</td>
    <td align="right">        5.25</td>
    <td align="right">       5.894</td>
    <td align="right">       0.716</td>
    <td align="right">       0.611</td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">FRONT-1</td>
    <td align="right">      210.00</td>
    <td align="right">       90.00</td>
    <td align="right">S</td>
  </tr>
  <tr>
    <td align="right">WR-1</td>
    <td align="right">DBL CLR 3MM/13MM AIR</td>
    <td align="right">        9.12</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">        9.12</td>
    <td align="right">        9.12</td>
    <td align="right">       2.720</td>
    <td align="right">       0.764</td>
    <td align="right">       0.812</td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">RIGHT-1</td>
    <td align="right">      120.00</td>
    <td align="right">       90.00</td>
    <td align="right">E</td>
  </tr>
  <tr>
    <td align="right">WB-1</td>
    <td align="right">DBL CLR 3MM/13MM AIR</td>
    <td align="right">       16.44</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">       16.44</td>
    <td align="right">       16.44</td>
    <td align="right">       2.720</td>
    <td align="right">       0.764</td>
    <td align="right">       0.812</td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">BACK-1</td>
    <td align="right">       30.00</td>
    <td align="right">       90.00</td>
    <td align="right">N</td>
  </tr>
  <tr>
    <td align="right">DB-1</td>
    <td align="right">SGL GREY 3MM</td>
    <td align="right">        4.41</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">        4.41</td>
    <td align="right">        4.41</td>
    <td align="right">       5.894</td>
    <td align="right">       0.716</td>
    <td align="right">       0.611</td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">BACK-1</td>
    <td align="right">       30.00</td>
    <td align="right">       90.00</td>
    <td align="right">N</td>
  </tr>
  <tr>
    <td align="right">WL-1</td>
    <td align="right">DBL CLR 3MM/13MM AIR</td>
    <td align="right">        9.12</td>
    <td align="right">        0.00</td>
    <td align="right">        0.00</td>
    <td align="right">        9.12</td>
    <td align="right">        9.12</td>
    <td align="right">       2.720</td>
    <td align="right">       0.764</td>
    <td align="right">       0.812</td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">No</td>
    <td align="right">LEFT-1</td>
    <td align="right">      300.00</td>
    <td align="right">       90.00</td>
    <td align="right">W</td>
  </tr>
  <tr>
    <td align="right">Total or Average</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">       60.90</td>
    <td align="right">       3.223</td>
    <td align="right">       0.756</td>
    <td align="right">       0.780</td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
  </tr>
  <tr>
    <td align="right">North Total or Average</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">       20.85</td>
    <td align="right">       3.391</td>
    <td align="right">       0.754</td>
    <td align="right">       0.769</td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
  </tr>
  <tr>
    <td align="right">Non-North Total or Average</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">       40.05</td>
    <td align="right">       3.136</td>
    <td align="right">       0.757</td>
    <td align="right">       0.785</td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
  </tr>
</table>
<br><br>
<b>Interior Fenestration</b><br><br>
<!-- FullName:Envelope Summary_Entire Facility_Interior Fenestration-->
<table border="1" cellpadding="4" cellspacing="0">
  <tr><td></td>
    <td align="right">Construction</td>
    <td align="right">Area of One Opening [m2]</td>
    <td align="right">Area of Openings [m2]</td>
    <td align="right">Glass U-Factor [W/m2-K]</td>
    <td align="right">Glass SHGC</td>
    <td align="right">Glass Visible Transmittance</td>
    <td align="right">Assembly U-Factor [W/m2-K]</td>
    <td align="right">Assembly SHGC</td>
    <td align="right">Assembly Visible Transmittance</td>
    <td align="right">Parent Surface</td>
  </tr>
  <tr>
    <td align="right">Total or Average</td>
    <td align="right">&nbsp;</td>
    <td align="right">&nbsp;</td>
    <td align="right">        0.00</td>
    <td align="right">-</td>
    <td align="right">-</td>
    <td align="right">-</td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">       ---  </td>
    <td align="right">&nbsp;</td>
  </tr>
</table>

## Engineering Reference ##

No changes expected in the engineering reference.

## Example File and Transition Changes ##

No example file or input transition changes. Output changes to the tabular 
output files are as described above.

## References ##





