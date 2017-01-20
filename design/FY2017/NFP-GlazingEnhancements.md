<head>
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({
  extensions: ["tex2jax.js"],
  jax: ["input/TeX", "output/HTML-CSS"],
  tex2jax: {
   inlineMath: [ ['$','$'], ["\\(","\\)"] ],
   displayMath: [ ['$$','$$'], ["\\[","\\]"] ],
   processEscapes: true
  },
  "HTML-CSS": { availableFonts: ["TeX"] }
  });
</script>
  <script type="text/javascript"
   src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
  </script>
</head>
# Add a New Functionality in EnergyPlus to Allow Window Properties as a Function of Incident Angles #

================

**Lixing Gu**
**Florida Solar Energy Center**

 - Original: January 5, 2017
 - NA
 

## Justification for New Feature ##

The current EnergyPlus allows user-defined spectral dependent glazing properties. The MaterialProperty:GlazingSpectralData object allows users to input transmittance, front reflectance, and back reflectance as a function of wavelength. In reality, these glazing properties are also a function of incident angle. AGC Glass Europe would like to add this new functionality. FSEC will help AGC to add the new functionality in EnergyPlus for public release.

## E-mail and  Conference Call Conclusions ##

insert text

## Overview ##

Window glazing properties of transmittance, front reflectance, and back reflectance are a function of wavelength and incident angel. The existing EnergyPlus considers the glazing properties are a function of wavelength only. The WindowMaterial:Glazing object has a field of Optical Data Type to allow users to select a choice of Spectral and enter Window Glass Spectral Data Set Name as a data set to input glazing data under the MaterialProperty:GlazingSpectralData object. The AGC Glass Europe found the glazing properties are not only a function of wavelength, as well as a function of incident angle. These properties can be measured from testing. Therefore, the existing input structure can not meet the possible functionality. A new functionality was proposed to enhance glazing properties as a function of both wavelength and incident angle. The added new functionality will improve window heat transfer calculations with more accurate prediction. 

The code modification will be based on Window Calculation Engine (WCE) submitted by LBNL.   

## Approach ##

The proposed approach will revise the existing object of WindowMaterial:Glazing, and add 3 new objects to handle data sets of the glazing properties as a function of both wavelength and incident angle. 

### Revision of WindowMaterial:Glazing object ###

The WindowMaterial:Glazing object will be revised by adding an additional choice of the 
Optical Data Type field in the WindowMaterial:Glazing object as SpectralAndIncidentAngle, and 3 new fields of optical dataset names to represent transmittance, front reflectance, and back reflectance, respectively. 

It is possible to combine all optical properties as a single dataset name. However, the dataset will contain 5 columns to be listed as:

Wavelength 1, Incident Angle 1, Transmittance 1, Front Reflectance 1, and Back Reflectance 1 ....

Therefore, the dataset becomes very large and not readable easily. It is better to have 3 datasets to represent transmittance, front reflectance, and back reflectance, respectively. It also provides flexibility to allow each optical property may have values at different wavelengths and incident angles. Each dataset set up 2 independent variables (wavelength and incident angle) and 1 dependent variable (optical property). 

### New objects of glazing properties ###

We proposed 3 new objects to represent optical properties of transmittance, front reflectance, and back reflectance as a function of both wavelength and incident angle. Each object contains an optical property at different wavelengths and incident angles. 

### Calculation procedure ###

The calculation procedure is expanded based on the Spectral choice defined in the  WindowMaterial:Glazing object. The main difference is that the Spectral choice calculates the optical properties at a given incident angle using empirical functions (getPropertyAtAngle), while the new choice of SpectralAndIncidentAngle will calculate the optical properties at a given incident angle via linear interpolation from given inputs.

1. Read input objects

A new function will be created to read 3 new dataset objects as GetWindowGlassSpectralAndAngleData in the HeatBalanceManager module. Its functionality is similar to GetWindowGlassSpectralData.

2. Integrate system properties with respect to wavelength for each angle of incidence

For a particular angle of incidence, calculates system properties for a glass layer for each wavelength in the solar spectrum in a new function in WCE called by the WindowManager module using given solar spectral irradiance. In other words, the approach treats each property as SpectralAverage defined in the WindowMaterial:Glazing to provide equivalent property for each incident angle. Therefore, after calculation, each property is not a function of wavelength.

The next step will generate an angular array with size of all incident angles from all layers in a construction. In other words, a common size array will contain all input incident angles from all layers. If an angle from other layers is not available in this layer, the properties will be linearly interpolated from this particular angle.  

The difference is that the SpectralAverage provide a constant value for each incident angle, while the SpectralAndIncidentAngle after solar spectral integration is a function of incident angle.  

3. Interpolate angular properties

A new function, corresponding to the SpectralAndIncidentAngle choice, and similar to a WCE function of getPropertyAtAngle, will be generated in WCE called from the SolarShading module. Instead of calculating angular properties, the properties will be linearly interpolated between two nearest angles given in the material properties.   
 
## Testing/Validation/Data Sources ##

Optical properties from EnergyPlus will be compared with spread sheet calculation.

A possible validation for a single clear glass without coating will also be performed.

If input values of optical properties are provided with calculation results from the theory at different incident angles for a choice of SpectralAndIncidentAngle, the optical properties at different incident angles should be the same as ones based on a choice of Spectral, if the same theory is used to calculate optical properties on a choice of Spectral. 

## Input Output Reference Documentation ##

This section is divided into two sub-sections. The first sub-section provides documentation related to revised objects. Any revisions are highlighted in red. The second sub-section presents documentation of new objects.  

### Revised objects ###

The revised object is WindowMaterial:Glazing.

### WindowMaterial:Glazing

In the following, for exterior windows, “front side” is the side of the glass closest to the outside air and “back side” is the side closest to the zone the window is defined in. For interzone windows, “front side” is the side closest to the zone adjacent to the zone the window is defined in and “back side” is the side closest to the zone the window is defined in.

#### Field: Name

The name of the glass layer. It corresponds to a layer in a window construction.

#### Field: Optical Data Type

Valid values for this field are SpectralAverage, Spectral, <span style="color:red;">SpectralAndAngle</span>, BSDF.

If Optical Data Type = SpectralAverage, the values you enter for solar transmittance and reflectance are assumed to be averaged over the solar spectrum, and the values you enter for visible transmittance and reflectance are assumed to be averaged over the solar spectrum and weighted by the response of  the human eye. There is an EnergyPlus Reference Data Set for WindowMaterial:Glazing that contains spectral average properties for many different types of glass.

If Optical Data Type = Spectral, then, in the following field, you must enter the name of a spectral data set defined with the WindowGlassSpectralData object. In this case, the values of  solar and visible transmittance and reflectance in the fields below should be blank.

<span style="color:red;">If Optical Data Type = SpectralAndAngle, then, in the next 3 fields, you must enter the name of a spectral and angle data set defined with the WindowGlassSpectralAndAngleData object. In this case, the Window Glass Spectral Data Set Name should be blank, and the values of solar and visible transmittance and reflectance in the fields below should be blank.</span>

If Optical Data Type = BSDF, the Construction:ComplexFenestrationState object must be used to define the window construction layers. The Construction:ComplexFenestrationState object contains references to the BSDF files which contain the optical properties of the Complex Fenestration layers. In this case,

#### Field: Window Glass Spectral Data Set Name

If Optical Data Type = Spectral, this is the name of a spectral data set defined with a WindowGlassSpectralData object.

#### <span style="color:red;">Field: Window Glass Spectral and Incident Angle Transmittance Data Set Name</span>

<span style="color:red;">If Optical Data Type = SpectralAndAngle, this is the name of a spectral  and angle data set of transmittance defined with a TransmittanceSpectralAndAngleDataSet object.</span>

#### <span style="color:red;">Field: Window Glass Spectral and Incident Angle Front Reflectance Data Set Name</span>

<span style="color:red;">If Optical Data Type = SpectralAndAngle, this is the name of a spectral and angle data set of front reflectance defined with a FrontReflectanceSpectralAndAngleDataSet object.</span>

#### <span style="color:red;">Field: Window Glass Spectral and Incident Angle Back Reflectance Data Set Name</span>

<span style="color:red;">If Optical Data Type = SpectralAndAngle, this is the name of a spectral and angle data set of back reflectance defined with a BackReflectanceSpectralAndAngleDataSet object.</span>

#### Field: Thickness

The surface-to-surface thickness of the glass (m).

#### Field: Solar Transmittance at Normal Incidence

Transmittance at normal incidence averaged over the solar spectrum. Used only when Optical Data Type = SpectralAverage.

For uncoated glass, when alternative optical properties are available—such as thickness, solar index of refraction, and solar extinction coefficient—they can be converted to equivalent solar transmittance and reflectance values using the equations given in “Conversion from Alternative Specification of Glass Optical Properties.”)

#### Field: Front Side Solar Reflectance at Normal Incidence

Front-side reflectance at normal incidence averaged over the solar spectrum. Used only when Optical Data Type = SpectralAverage.

#### Field: Back Side Solar Reflectance at Normal Incidence

Back-side reflectance at normal incidence averaged over the solar spectrum. Used only when Optical Data Type = SpectralAverage.

#### Field: Visible Transmittance at Normal Incidence

Transmittance at normal incidence averaged over the solar spectrum and weighted by the response of the human eye. Used only when Optical Data Type = SpectralAverage.

For uncoated glass, when alternative optical properties are available—such as thickness, visible index of refraction, and visible extinction coefficient—they can be converted to equivalent visible transmittance and reflectance values using the equations given in “Conversion from Alternative Specification of Glass Optical Properties.”)

#### Field: Front Side Visible Reflectance at Normal Incidence

Front-side reflectance at normal incidence averaged over the solar spectrum and weighted by the response of the human eye. Used only when Optical Data Type = SpectralAverage.

#### Field: Back Side Visible Reflectance at Normal Incidence

Back-side reflectance at normal incidence averaged over the solar spectrum and weighted by the response of the human eye. Used only when Optical Data Type = SpectralAverage.

#### Field: Infrared Transmittance at Normal Incidence

Long-wave transmittance at normal incidence.

#### Field: Front Side Infrared Hemispherical Emissivity

Front-side long-wave emissivity.

#### Field: Back Side Infrared Hemispherical Emissivity

Back-side long-wave emissivity.

#### Field: Conductivity

Thermal conductivity (W/m-K).

#### Field: Dirt Correction Factor for Solar and Visible Transmittance

This is a factor that corrects for the presence of dirt on the glass. The program multiplies the fields “Solar Transmittance at Normal Incidence” and “Visible Transmittance at Normal Incidence” by this factor if the material is used as the outer glass layer of an exterior window or glass door.[[1]](#_ftn1) If the material is used as an inner glass layer (in double glazing, for example), the dirt correction factor is not applied because inner glass layers are assumed to be clean. Using a material with dirt correction factor &lt; 1.0 in the construction for an interior window will result in an error message.

Representative values of the dirt correction factor are shown in Table 7.

     Table 7. Dirt Correction Factors

<table class="table table-striped">
  <tr>
    <th rowspan="2">Type of Location</th>
    <th colspan="3">Angle of Glazing</th>
  </tr>
  <tr>
    <td>Vertical</td>
    <td>45</td>
    <td>Horizontal</td>
  </tr>
  <tr>
    <td>Non-industrial</td>
    <td>0.9</td>
    <td>0.8</td>
    <td>0.7</td>
  </tr>
  <tr>
    <td>Industrial</td>
    <td>0.7</td>
    <td>0.6</td>
    <td>0.5</td>
  </tr>
  <tr>
    <td>Very Dirty</td>
    <td>0.6</td>
    <td>0.5</td>
    <td>0.4</td>
  </tr>
</table>

From Appendix A, “Daylighting in Sports Halls, Report 2,” SportScotland, Nov. 2002
 (www.sportscotland.org.uk)

The default value of the dirt correction factor is 1.0, which means the glass is clean.

It is assumed that dirt, if present, has no effect on the IR properties of the glass.

#### Field: Solar Diffusing

Takes values No (the default) and Yes. If No, the glass is transparent and beam solar radiation incident on the glass is transmitted as beam radiation with no diffuse component. If Yes, the glass is translucent and beam solar radiation incident on the glass is transmitted as hemispherically diffuse radiation with no beam component.[[2]](#_ftn2) See Figure 10. Solar Diffusing = Yes should only be used on the *innermost* pane of glass in an exterior window; it does not apply to interior windows.

For both Solar Diffusing = No and Yes, beam is reflected as beam with no diffuse component (see Figure 10). Solar Diffusing cannot be used with Window Shading Control devices (except Switchable Glazing). When attempted, the window property will be set to No for Solar Diffusing. The Surface Details report will reflect the override.

If, in the Building object, Solar Distribution = FullInteriorAndExterior, use of Solar Diffusing = Yes for glass in an exterior window will change the distribution of interior solar radiation from the window. The result is that beam solar radiation that would be transmitted by a transparent window and get absorbed by particular interior surfaces will be diffused by a translucent window and be spread over more interior surfaces. This can change the time dependence of heating and cooling loads in the zone.

In a zone with Daylighting:Detailed, translucent glazing---which is often used in skylights---will provide a more uniform daylight illuminance over the zone and will avoid patches of sunlight on the floor.

![](media/image025.svg)

Figure 10. Comparison between transmittance properties of transparent glass (Solar Diffusing = No) and translucent glass (Solar Diffusing = Yes).

#### Field: Young’s modulus

A measure of the stiffness of an elastic material.  It is defined as the ratio of the unaxial stress over the uniaxial strain in the range of stress in which Hooke’s Law holds. It is used only with complex fenestration systems defined through the Construction:ComplexFenestrationState object. The default value for glass is 7.2e10 Pa.

#### Field: Poisson’s ratio

The ratio, when a sample object is stretched, of the contraction or transverse strain (perpendicular to the applied load), to the extension or axial strain (in the direction of the applied load). This value is used only with complex fenestration systems defined through the Construction:ComplexFenestrationState object. The default value for glass is 0.22.

IDF examples of Spectral average and using a Spectral data set:

```idf
MATERIAL:WINDOWGLASS, GLASS - CLEAR SHEET 1 / 8 IN,
  SpectralAverage, ! Optical data type
  ,                    !- Spectral Data name
  ,      !- Window Glass Spectral and Incident Angle Transmittance Data Set Name
  ,      !- Window Glass Spectral and Incident Angle Front Reflectance Data Set Name
  ,      !- Window Glass Spectral and Incident Angle Back Reflectance Data Set Name
  0.003, ! Thickness {m} 1/8"
  0.850, ! Solar transmittance at normal incidence
  0.075, ! Solar reflectance at normal incidence: front side
  0.075, ! Solar reflectance at normal incidence: back side
  0.901, ! Visible transmittance at normal incidence
  0.081, ! Visible reflectance at normal incidence: front side
  0.081, ! Visible reflectance at normal incidence: back side
  0.0,   ! IR transmittance at normal incidence
  0.84,  ! IR hemispherical emissivity: front side
  0.84,  ! IR hemispherical emissivity: back side
  0.9;   ! Conductivity {W/m-K}


WindowMaterial:Glazing ,SPECTRAL GLASS INNER PANE, ! Material name
    Spectral, ! Optical data type {SpectralAverage or Spectral}
    TestSpectralDataSet, ! Name of spectral data set
    ,      !- Window Glass Spectral and Incident Angle Transmittance Data Set Name
    ,      !- Window Glass Spectral and Incident Angle Front Reflectance Data Set Name
    ,      !- Window Glass Spectral and Incident Angle Back Reflectance Data Set Name
    0.0099, ! Thickness {m}
    ,  ! Solar transmittance at normal incidence
    ,  ! Solar reflectance at normal incidence: front side
    ,  ! Solar reflectance at normal incidence: back side
    ,  ! Visible transmittance at normal incidence
    ,  ! Visible reflectance at normal incidence: front side
    ,  ! Visible reflectance at normal incidence: back side
    0.0,   ! IR transmittance at normal incidence
    0.84,  ! IR emissivity: front side
    0.84,  ! IR emissivity: back side
    0.798; ! Conductivity {W/m-K}
```
IDF example using a SpectralAndAngle data set:

```idf
WindowMaterial:Glazing ,SPECTRAL AND ANGLE GLASS INNER PANE, ! Material name
    SpectralAndAngle, ! Optical data type {SpectralAverage or Spectral}
    , ! Name of spectral data set
    TranmittanceDataSet,  !- Window Glass Spectral and Incident Angle Transmittance Data Set Name
    FrontReflectanceDataSet,      !- Window Glass Spectral and Incident Angle Front Reflectance Data Set Name
    BackReflectanceDataSet,      !- Window Glass Spectral and Incident Angle Back Reflectance Data Set Name
    0.0099, ! Thickness {m}
    ,  ! Solar transmittance at normal incidence
    ,  ! Solar reflectance at normal incidence: front side
    ,  ! Solar reflectance at normal incidence: back side
    ,  ! Visible transmittance at normal incidence
    ,  ! Visible reflectance at normal incidence: front side
    ,  ! Visible reflectance at normal incidence: back side
    0.0,   ! IR transmittance at normal incidence
    0.84,  ! IR emissivity: front side
    0.84,  ! IR emissivity: back side
    0.798; ! Conductivity {W/m-K}
```

IDF example of Spectral Data Type = BSDF

```idf
WindowMaterial:Glazing,
  Glass_5012_Layer,        !- Layer name : CLEAR_6.PPG
  BSDF,                    !- Optical Data Type
  ,                        !- Spectral Data name
  ,      !- Window Glass Spectral and Incident Angle Transmittance Data Set Name
  ,      !- Window Glass Spectral and Incident Angle Front Reflectance Data Set Name
  ,      !- Window Glass Spectral and Incident Angle Back Reflectance Data Set Name
  0.005664,                !- Thickness
  ,                        !- Solar Transmittance
  ,                        !- Solar Front Reflectance
  ,                        !- Solar Back Reflectance
  ,                        !- Visible Transmittance
  ,                        !- Visible Front Reflectance
  ,                        !- Visible Back reflectance
  0.000000,                !- IR Transmittance
  0.840000,                !-Front Emissivity
  0.840000,                !-Back Emissivity
  1.000000,                !-Conductivity
  ,                        !-Dirt Correction Factor for Sol/Vis Transmittance
  ,                        !-Solar Diffusing
  7.2e10,                  !-Young’s modulus
  0.22;                    !-Poisson’s ratio
```

### New objects ###

There are 3 new objects to allow users to input optical data as a function of wavelength and incident angle.

### MaterialProperty:GlazingTransmittanceData

With the MaterialProperty:GlazingTransmittanceData object, you can specify transmittance of a glass material as a function of wavelength and angle of incidence. To determine the overall optical properties of a glazing system (solar and visible transmittance and solar absorptance), EnergyPlus first integrates transmittance and absorptance with respect to wavelength for each angle of incidence. The integration is weighted by a standard solar spectrum to get the solar transmittance and absorptance (for use in the solar heat gain calculations). The properties are further weighted by the response of the human eye to get the visible transmittance for each angle of incidence (for use in the daylighting calculation). 

MaterialProperty:GlazingTransmittanceData should be used for multi-pane windows when one or more of the glass layers is *spectrally and angled selective*, i.e., the transmittance depends strongly on wavelength and angle of incidence. An example is glass with a coating that gives high transmittance in the daylight part of the solar spectrum (roughly 0.4 to 0.7 microns) and low transmittance at longer wavelengths, thus providing better solar heat gain control than uncoated glass. If spectral data is not used in case, the overall optical properties of the glazing system that EnergyPlus calculates will not be correct.

You can input up to 1000 sets of values for wavelengths and angle of incidence covering the solar spectrum. Each set consists of {wavelength (microns), incident angle, transmittance}.

#### Field: Name

The name of the Transmittance data set as a function of wavelength and angle of incidence. It is referenced by WindowMaterial:Glazing when Optical Data Type = SpectralAndAngle.

#### Fields 1-3 (repeated up to 1000 times)

Sets of values for wavelengths covering the solar spectrum (from about 0.25 to 2.5 microns [10<sup>-6</sup> m]), angles of incidence between 0 and 90 degrees, and transmittance at corresponding wavelength and incident angle. Each set consists of

**{wavelength (microns), incident angle, transmittance}**

The wavelength and incident angle values must be in ascending order. 

An IDF example:

```idf
MaterialProperty:GlazingTransmittanceData,
      TransmittanceDataSet,
       ! { from WINDOW 4 library }
       ! { actual 9.91mm clear laminate: 15_mil PVB, ID:37966/50032-39-9 } 10.38
       ! { conductivity PVB adjusted, W/M/K  } 0.798
       ! { thermal IR transmittance, assumed } tir=0.00
       ! { thermal IR hemispherical emittance, assumed } emis= 0.84 0.84

       ! WL  IncAng Trans     
       .300, 0.000, 0.000, 
       .310, 0.000, 0.000, 
      <snip>
      2.450, 0.000, 0.200,
      2.500, 0.000, 0.214, 
      <snip>
       .300, 90.00, 0.000, 
       .310, 90.00, 0.000, 
      <snip>
      2.450, 90.00, 0.000,
      2.500, 90.00, 0.000; 
```
### MaterialProperty:GlazingFrontReflectanceData

With the MaterialProperty:GlazingFrontReflectanceData object, you can specify front reflectance of a glass material as a function of wavelength and angle of incidence. “Front reflectance” is the reflectance for radiation striking the glass from the outside, i.e., from the side opposite the zone in which the window is defined. To determine the overall optical properties of a glazing system, EnergyPlus first integrates front reflectance with respect to wavelength for each angle of incidence. The integration is weighted by a standard solar spectrum to get the front reflectance (for use in the solar heat gain calculations).  

MaterialProperty:GlazingFrontReflectanceData should be used for multi-pane windows when one or more of the glass layers is *spectrally and angled selective*, i.e., the front reflectance depends strongly on wavelength and angle of incidence. 

You can input up to 1000 sets of values for wavelengths and angle of incidence covering the solar spectrum. Each set consists of {wavelength (microns), incident angle, front reflectance}.

#### Field: Name

The name of the front reflectance data set as a function of wavelength and angle of incidence. It is referenced by WindowMaterial:Glazing when Optical Data Type = SpectralAndAngle.

#### Fields 1-3 (repeated up to 1000 times)

Sets of values for wavelengths covering the solar spectrum (from about 0.25 to 2.5 microns [10<sup>-6</sup> m]), angles of incidence between 0 and 90 degrees, and front reflectance. Each set consists of

**{wavelength (microns), incident angle, front reflectance}**

The wavelength and incident angle values must be in ascending order. 

An IDF example:

```idf
MaterialProperty:GlazingFrontReflectanceData,
      FrontReflectanceDataSet,
       ! WL  IncAng FRefl     
       .300, 0.000, 0.000, 
       .310, 0.000, 0.000, 
      <snip>
      2.450, 0.000, 0.200,
      2.500, 0.000, 0.214, 
      <snip>
       .300, 90.00, 0.000, 
       .310, 90.00, 0.000, 
      <snip>
      2.450, 90.00, 0.000,
      2.500, 90.00, 0.000; 
```

### MaterialProperty:GlazingBackReflectanceData

With the MaterialProperty:GlazingBackReflectanceData object, you can specify back reflectance of a glass material as a function of wavelength and angle of incidence. “Back reflectance” is the reflectance for radiation striking the glass from the inside, i.e., from the zone in which the window is defined. To determine the overall optical properties of a glazing system, EnergyPlus first integrates back reflectance with respect to wavelength for each angle of incidence. The integration is weighted by a standard solar spectrum to get the back reflectance (for use in the solar heat gain calculations).  

MaterialProperty:GlazingBackReflectanceData should be used for multi-pane windows when one or more of the glass layers is *spectrally and angled selective*, i.e., the front reflectance depends strongly on wavelength and angle of incidence. 

You can input up to 1000 sets of values for wavelengths and angle of incidence covering the solar spectrum. Each set consists of {wavelength (microns), incident angle, back reflectance}.

#### Field: Name

The name of the back reflectance data set as a function of wavelength and angle of incidence. It is referenced by WindowMaterial:Glazing when Optical Data Type = SpectralAndAngle.

#### Fields 1-3 (repeated up to 1000 times)

Sets of values for wavelengths covering the solar spectrum (from about 0.25 to 2.5 microns [10<sup>-6</sup> m]), angles of incidence between 0 and 90 degrees, and back reflectance. Each set consists of

**{wavelength (microns), incident angle, back reflectance}**

The wavelength and incident angle values must be in ascending order. 

An IDF example:

```idf
MaterialProperty:GlazingBackReflectanceData,
      BackReflectanceDataSet,
       ! WL  IncAng BRefl     
       .300, 0.000, 0.000, 
       .310, 0.000, 0.000, 
      <snip>
      2.450, 0.000, 0.200,
      2.500, 0.000, 0.214, 
      <snip>
       .300, 90.00, 0.000, 
       .310, 90.00, 0.000, 
      <snip>
      2.450, 90.00, 0.000,
      2.500, 90.00, 0.000; 
```

## Input Description ##

This section describes inputs of 3 new objects and a revised object

### Revised object  

The WindowMaterial:Glazing object will be revised.

Any new additions will be highlighted in red.

	WindowMaterial:Glazing,
    \min-fields 14
    \memo Glass material properties for Windows or Glass Doors
    \memo Transmittance/Reflectance input method.
  	A1 , \field Name
       \required-field
       \type alpha
       \reference MaterialName
       \reference GlazingMaterialName
       \reference CFSGlazingName
 	A2 , \field Optical Data Type
       \required-field
       \type choice
       \key SpectralAverage
       \key Spectral
       \key BSDF
 <span style="color:red;">**\key, SpectralAndAngle**</span>

  	A3 , \field Window Glass Spectral Data Set Name
       \note Used only when Optical Data Type = Spectral
       \type object-list
       \object-list SpectralDataSets
 <span style="color:red;">**A4 , \field Window Glass Spectral and Incident Angle Transmittance Data Set Name**</span>

       \note Used only when Optical Data Type = SpectralAndAngle
       \type object-list
       \object-list TransmittanceSpectralAndAngleDataSet
 <span style="color:red;">**A5 , \field Window Glass Spectral and Incident Angle Front Reflectance Data Set Name**</span>

       \note Used only when Optical Data Type = SpectralAndAngle
       \type object-list
       \object-list FrontReflectanceSpectralAndAngleDataSet
 <span style="color:red;">**A6 , \field Window Glass Spectral and Incident Angle Back Reflectance Data Set Name**</span>

       \note Used only when Optical Data Type = SpectralAndAngle
       \type object-list
       \object-list BackReflectanceSpectralAndAngleDataSet
  	N1 , \field Thickness
       \required-field
       \units m
       \type real
       \minimum> 0.0
       \ip-units in
  	N2 , \field Solar Transmittance at Normal Incidence
       \note Used only when Optical Data Type = SpectralAverage
       \type real
       \minimum 0.0
       \maximum 1.0
  	N3 , \field Front Side Solar Reflectance at Normal Incidence
       \note Used only when Optical Data Type = SpectralAverage
       \note Front Side is side closest to outdoor air
       \type real
       \minimum 0.0
       \maximum 1.0
  	N4 , \field Back Side Solar Reflectance at Normal Incidence
       \note Used only when Optical Data Type = SpectralAverage
       \note Back Side is side closest to zone air
       \type real
       \minimum 0.0
       \maximum 1.0
  	N5 , \field Visible Transmittance at Normal Incidence
       \note Used only when Optical Data Type = SpectralAverage
       \type real
       \minimum 0.0
       \maximum 1.0
  	N6 , \field Front Side Visible Reflectance at Normal Incidence
       \note Used only when Optical Data Type = SpectralAverage
       \type real
       \minimum 0.0
       \maximum 1.0
 	N7 , \field Back Side Visible Reflectance at Normal Incidence
       \note Used only when Optical Data Type = SpectralAverage
       \type real
       \minimum 0.0
       \maximum 1.0
  	N8 , \field Infrared Transmittance at Normal Incidence
       \type real
       \minimum 0.0
       \maximum 1.0
       \default 0.0
  	N9 , \field Front Side Infrared Hemispherical Emissivity
       \type real
       \minimum> 0.0
       \maximum< 1.0
       \default 0.84
  	N10, \field Back Side Infrared Hemispherical Emissivity
       \type real
       \minimum> 0.0
       \maximum< 1.0
       \default 0.84
  	N11, \field Conductivity
       \units W/m-K
       \type real
       \minimum> 0.0
       \default 0.9
  	N12, \field Dirt Correction Factor for Solar and Visible Transmittance
       \type real
       \minimum> 0.0
       \maximum 1.0
       \default 1.0 
   	A7 ,  \field Solar Diffusing
       \type choice
       \key No
       \key Yes
       \default No
  	N13, \field Young's modulus
       \note coefficient used for deflection calculations. Used only with complex
       \note fenestration when deflection model is set to TemperatureAndPressureInput
       \units Pa
       \type real
       \minimum> 0.0
       \default 7.2e10
  	N14; \field Poisson's ratio
       \note coefficient used for deflection calculations. Used only with complex
       \note fenestration when deflection model is set to TemperatureAndPressureInput
       \type real
       \minimum>  0.0
       \maximum<  1.0
       \default 0.22

### 3 New objects ###

There are 3 new objects to contain optical properties as a function of wavelength and incident angle: MaterialProperty:GlazingTransmittanceData, MaterialProperty:GlazingFrontReflectanceData, and MaterialProperty:GlazingBackReflectanceData.
 
	MaterialProperty:GlazingTransmittanceData,
       \memo Name is followed by up to 1000 sets of measured values of transmittance
       \memo [wavelength, incident angle, transmittance] for wavelengths and incident angle.
       \extensible:3 -- duplicate last set of data: [wavelength, incident angle, transmittance] 
       \ (last 3 fields), remembering to remove ; from "inner" fields.

	A1,  \field Name
       \required-field
       \reference TransmittanceSpectralAndAngleDataSet
	N1,  \field Wavelength 1
       \type real
       \units micron
  	N2,  \field IncidentAngle 1
       \type real
       \units degree
  	N3,  \field Transmittance 1
       \type real
       \units dimensionless
  	N4,  \field Wavelength 2
  	N5,  \field IncidentAngle 2
  	N6,  \field Transmittance 2
       \type real
       \units dimensionless
    ....

	MaterialProperty:GlazingFrontReflectanceData,
       \memo Name is followed by up to 1000 sets of measured values of front reflectance
       \memo [wavelength, incident angle, front reflectance] for wavelengths and incident angle.
       \extensible:3 -- duplicate last set of data: [wavelength, incident angle, front reflectance] 
       \ (last 3 fields), remembering to remove ; from "inner" fields.

	A1,  \field Name
       \required-field
       \reference FrontReflectanceSpectralAndAngleDataSet
	N1,  \field Wavelength 1
       \type real
       \units micron
  	N2,  \field Incident Angle 1
       \type real
       \units degree
  	N3,  \field Front Reflectance 1
       \type real
       \units dimensionless
  	N4,  \field Wavelength 2
  	N5,  \field Incident Angle 2
  	N6,  \field Front Reflectance 2
       \type real
       \units dimensionless
    ....

	MaterialProperty:GlazingBackReflectanceData,
       \memo Name is followed by up to 1000 sets of measured values of back reflectance
       \memo [wavelength, incident angle, back reflectance] for wavelengths and incident angle.
       \extensible:3 -- duplicate last set of data: [wavelength, incident angle, back reflectance] 
       \ (last 3 fields), remembering to remove ; from "inner" fields.

	A1,  \field Name
       \required-field
       \reference BackReflectanceSpectralAndAngleDataSet
	N1,  \field Wavelength 1
       \type real
       \units micron
  	N2,  \field Incident Angle 1
       \type real
       \units degree
  	N3,  \field Back Reflectance 1
       \type real
       \units dimensionless
  	N4,  \field Wavelength 2
  	N5,  \field Incident Angle 2
  	N6,  \field Back Reflectance 2
       \type real
       \units dimensionless
    ....

Questions:

How many fields are needed in idd? I assume 1000 sets.

## Outputs Description ##

insert text



## Engineering Reference ##

Revised section by adding description of optical properties as a function of wavelength and incident angle. The added portion is colored in red.

### Glazing System Properties

The optical properties of a glazing system consisting of *N* glass layers separated by nonabsorbing gas layers (Figure 77.  Schematic of transmission, reflection and absorption of solar radiation within a multi-layer glazing system.) are determined by solving the following recursion relations for *T<sub>i,j</sub>*, the transmittance through layers* i* to* j*; *R<sup>f</sup><sub>i,j</sub>* and *R<sup>b</sup><sub>i,j</sub>*, the front and back reflectance, respectively, from layers* i* to* j*; and *A<sub>j</sub>*, the absorption in layer* j*. Here layer 1 is the outermost layer and layer *N* is the innermost layer. These relations account for multiple internal reflections within the glazing system. Each of the variables is a function of wavelength.

<div>$${T_{i,j}} = \frac{{{T_{i,j - 1}}{T_{j,j}}}}{{1 - R_{j,j}^fR_{j - 1,i}^b}}$$</div>

<div>$$R_{i,j}^f = R_{i,j - 1}^f + \frac{{T_{i,j - 1}^2R_{j,j}^f}}{{1 - R_{j,j}^fR_{j - 1,i}^b}}$$</div>

<div>$$R_{j,i}^b = R_{j,j}^b + \frac{{T_{j,j}^2R_{j - 1,i}^b}}{{1 - R_{j - 1,i}^bR_{j,j}^f}}$$</div>

<div>$$A_j^f = \frac{{{T_{1,j - 1}}(1 - {T_{j,j}} - R_{j,j}^f)}}{{1 - R_{j,N}^fR_{j - 1,1}^b}} + \frac{{{T_{1,j}}R_{j + 1,N}^f(1 - {T_{j,j}} - R_{j,j}^b)}}{{1 - R_{j,N}^fR_{j - 1,1}^b}}$$</div>

In Eq. *T<sub>i,j</sub>* = 1 and *R<sub>i,j</sub>* = 0 if* i*&lt;0 or* j*&gt;*N*.

![](media/image956.png)

Figure 77.  Schematic of transmission, reflection and absorption of solar radiation within a multi-layer glazing system.

As an example, for double glazing (*N*=2) these equations reduce to

<div>$${T_{1,2}} = \frac{{{T_{1,1}}{T_{2,2}}}}{{1 - R_{2,2}^fR_{1,1}^b}}$$</div>

<div>$$R_{1,2}^f = R_{1,1}^f + \frac{{T_{1,1}^2R_{2,2}^f}}{{1 - R_{2,2}^fR_{1,1}^b}}$$</div>

<div>$$R_{2,1}^b = R_{2,2}^b + \frac{{T_{2,2}^2R_{1,1}^b}}{{1 - R_{1,1}^bR_{2,2}^f}}$$</div>

<div>$$A_1^f = (1 - {T_{1,1}} - R_{1,1}^f) + \frac{{{T_{1,1}}R_{2,2}^f(1 - {T_{1,1}} - R_{1,1}^b)}}{{1 - R_{2,2}^fR_{1,1}^b}}$$</div>

<div>$$A_2^f = \frac{{{T_{1,1}}(1 - {T_{2,2}} - R_{2,2}^f)}}{{1 - R_{2,2}^fR_{1,1}^b}}$$</div>

If the above transmittance and reflectance properties are input as a function of wavelength, EnergyPlus calculates “spectral average” values of the above glazing system properties by integrating over wavelength:

The spectral-average solar property is

<div>$${P_s} = \frac{{\int {P(\lambda ){E_s}(\lambda )d\lambda } }}{{\int {{E_s}(\lambda )d\lambda } }}$$</div>

The spectral-average visible property is

<div>$${P_v} = \frac{{\int {P(\lambda ){E_s}(\lambda )V(\lambda )d\lambda } }}{{\int {{E_s}(\lambda )V(\lambda )d\lambda } }}$$</div>

where <span>\({E_s}(\lambda )\)</span>is the solar spectral irradiance function and <span>\(V(\lambda )\)</span>is the photopic response function of the eye. The default functions are shown in Table 29 and Table 30. They can be overwritten by user defined solar and/or visible spectrum using the objects Site:SolarAndVisibleSpectrum and Site:SpectrumData. They are expressed as a set of values followed by the corresponding wavelengths for values.

<span style="color:red;">When a choice of Spectral is entered as optical data type, the correlations to store glazing system's angular performance are generated based on angular performance at 10 degree increments. When a choice of SpectralAndAngel is entered as optical data type, the correlations to store glazing system's angular performance are generated based on given incident angular values from inputs. An increment of 10 degree is not used for generation of the correlations.</span>
  
If a glazing layer has optical properties that are roughly constant with wavelength, the wavelength-dependent values of *T<sub>i,i</sub>* , *R<sup>f</sup><sub>i,i</sub>* and *R<sup>b</sup><sub>i,i</sub>* in Eqs. to can be replaced with constant values for that layer.

## Example File and Transition Changes ##

An example input file with 3 new objects will be delivered.

A transition by adding 3 new extra fields are needed.

## References ##

insert text

