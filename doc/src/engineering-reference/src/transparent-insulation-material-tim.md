# Transparent Insulation Material (TIM)

## Introduction

The input object "SurfaceControl:MovableInsulation" allows modeling Transparent Insulation Materials (TIM) that were originally designed for use in solar collector systems, where there was a need to increase the insulation in the solar collector without dramatically reducing solar energy transmittance.  Transparent Insulation provides both these properties, insulation from heat loss and transmittance of solar energy.  The combination of these properties is achieved, because Transparent Insulation is a transmitter of short wave radiation but a barrier to longwave radiation.  Therefore short wave solar radiation passes through the Transparent Insulation and longwave heat radiation is insulated by the transparent insulation. Incident solar energy falling on the transparent insulation is reflected and re-reflected within the material and eventually falls on the absorber.  In addition, transparent insulation materials also have increase thermal resistance due to conduction in comparison to standard glass.

Transparent Insulation is now used in the housing industry as a passive solar feature. It is attached to the walls of houses for insulation and solar energy gains are transmitted to the house during the right ambient conditions. The walls of the house act as a thermal mass, absorbing the sunlight at the surface and converting it into heat which is slowly transmitted to the inside of the house.

## Comparison of Opaque and Transparent Insulation

A qualitative comparison between the performance of Transparent Insulation and opaque insulation is shown diagrammatically in the figure below. The upper half of the figure represents approximate heat transfer through the wall cross-section for both transparent and opaque insulation cases.  The lower half of this figure shows representative temperature variations through the wall cross-sections for different solar conditions.

![Energy Flows of Opaquely and Transparently Insulated Walls (Wood and Jesch 1993).](media/energy-flows-of-opaquely-and-transparently.jpeg)


While both types of insulation reduce energy losses from the building via conduction through the building surfaces, transparent insulation allows solar radiation to penetrate deeper into the surface construction.  This increases the construction internal temperature and can result in heat being conducted into the building under the proper weather conditions.  This can be seen in the lower half of the above figure during a sunny day.  The temperature plot shows a maximum between the transparent insulation and the rest of the surface construction.  As a result, the temperature gradient results in heat transfer from this point into the interior space, causing a heating effect on the zone.  Thus, the advantage of transparent insulation is that, like opaque insulation, it reduces winter heat transfer losses during low or no solar conditions and has the possibility of providing heating during sunny winter days.  It should be noted that this same effect in summer could be detrimental to the cooling loads of a building since the introduction of solar radiation closer to the space will increase the solar heating within the zone.  Most systems counteract this with a shading device or with sophisticated transparent insulation systems.

## Types of Transparent Insulation Materials

Transparent insulation can be classified into four general categories:

Absorber Parallel Covers

Cavity Structures

Absorber Vertical Covers

Quasi-Homogeneous Structures

Cross-sections of each of these types is shown in the figure below.  The arrows in these diagrams indicate solar rays and the path these rays trace as they are transmitted through the transparent insulation layer.  The most advantageous set-up (see absorber-parallel below) would send most of the rays downward towards the interior of the building while minimizing the rays that are reflected back to the exterior environment.

![Geometrical Categories of Classification for Transparent Insulation Material (Wood and Jesch 1993).](media/geometrical-categories-of-classification-for.jpeg)


## TIM- Basic Mathematical Model

![Cross Section of TIM and wall, showing energy flow](media/cross-section-of-tim-and-wall-showing-energy.png)


Mathematical model to calculate amount of energy absorbed at the surface of moveable insulation (TIM) and at the Outside surface of the Wall.

![](media/image395.png)\


The total solar gain on any exterior surface is a combination of the absorption of direct and diffuse solar radiation given by

**![](media/image396.png)**

Where,

= solar absorptance of the surface

![](media/image397.png) = angle of incidence of the sun's rays

S= area of the surface

S~s~= sunlit area of the surface

I~b~= intensity of the beam (direct) radiation

I~s~= intensity of the sky diffuse radiation

I~g~= intensity of the beam (direct) radiation

F~ss~= angle factor between the surface and the sky

F~sg~= angle factor between the surface and the ground

Now,

![](media/image398.png)\


The model for TIM is simplified in that it assumes that absorption of solar radiation takes place at the inside and outside of the TIM only, not throughout the material.  In addition, the model assumes that the solar radiation absorbed during the first pass through the TIM affects the outside surface of the TIM while the solar radiation reflected at the outer wall surface that gets absorbed during the back reflection will affect the inside TIM surface (which is also the outside surface of the wall).  Thus, the heat absorbed at the outside of the TIM is as shown in Equation .

The heat absorbed at the inside of the TIM/outside of the wall includes two components.  The first component is the amount of solar that is transmitted through the TIM and absorbed at the inside of the wall.  This is characterized by the following equation:

![](media/image399.png)\


The amount of solar absorbed by the TIM and aggregated at the inside surface of the TIM (outside wall surface) is:

![](media/image400.png)\


The heat absorbed at the interface between the wall and the TIM includes both of these components.  Thus, QSO is equal to:

![](media/image401.png)\


Substituting the definition for QSM into this equation and rearranging results in:

![](media/image402.png)\


![](media/image403.png)\


Where,

*QSM*= Short wave radiant flux absorbed at surface of Movable Insulation

*QSO*= Short wave radiant flux absorbed at surface of Wall.

~TIM~= Absorptance of TIM

~TIM~= Transmittance of TIM.

~WALL~= Absorptance of Wall.

 ~WALL~= Reflectance of Wall surface

Following is the FORTRAN Code used in the HeatBalanceSurfaceManager module, to determine the short wave radiation absorbed on outside of movable insulation and the short wave radiation absorbed on outside of opaque surface of the wall.

~~~~~~~~~~~~~~~~~~~~

    IF (Surface(SurfNum)%MaterialMovInsulExt.GT.0)                                     &
        CALL EvalOutsideMovableInsulation(SurfNum,HMovInsul,RoughIndexMovInsul,AbsExt)
        IF (HMovInsul > 0) THEN    ! Movable outside insulation in place
          QRadSWOutMvIns(SurfNum) = QRadSWOutAbs(SurfNum)*AbsExt          &
                           /Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpSolar
    !  For Transparent Insulation
          QRadSWOutAbs(SurfNum) = Material(Surface(SurfNum)%MaterialMovInsulExt)%Trans  &
                 *QRadSWOutMvIns(SurfNum)*                              &
                     (  (Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpSolar/AbsExt)   &
                            +(1-Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpSolar)  )
~~~~~~~~~~~~~~~~~~~~

## Sample Test Run Cases: – Comparison

A series of test cases were run in EnergyPlus to test the TIM model.  The building was a very simple box with walls facing north, south, east, and west, all of which are exterior walls.  Transparent Insulation Material has been applied to the south wall (except as noted in the table below).  The program was run for this fictional 1 zone building located in Chanute AFB IL, for two design days, (21^st^ June and 21^st^ January).  The main purpose of these runs was to verify that the transparent insulation model was predicting results that were reasonable using a simple test case.  The winter design day was also modified in some runs to have a clearness of 1.0 so that the effect that solar radiation during winter-time conditions could be studied.

The Transparent Insulation material is conceived by applying a SurfaceControl:MoveableInsulation on the exterior.  In the test cases, the TIM had the following thermal properties:

    0.05, ! Thickness {m}

    0.90,  ! Solar transmittance at normal incidence

    0.031,  ! Solar reflectance at normal incidence: front side

    0.031,  ! Solar reflectance at normal incidence: back side

    0.90,  ! Visible transmittance at normal incidence

    0.05,  ! Visible reflectance at normal incidence: front side

    0.05,  ! Visible reflectance at normal incidence: back side

    0.0,   ! IR transmittance at normal incidence

    0.84,  ! IR emissivity: front side

    0.84,  ! IR emissivity: back side

    0.04;   ! Conductivity {W/m-K}

The Wall Construction is defined as an EXTWALL80 composed of 1" Stucco, 4" Common Brick and ¾" Plaster or Gypboard.

The following two tables shows data for two series of runs.  The first "summer table" illustrates the execution of a summer design day.  The second "winter table" shows winter conditions with clearness=0 (the typical default for a winter design day) and clearness=1 (to illustrate solar radiation with other winter conditions).  Test cases included no movable insulation, moveable opaque insulation, and TIM on the exterior (south wall unless otherwise noted).  Savings reported are heating and cooling loads for the design days only.  The results showed that the TIM model was performing reasonably well and was producing results that were within expectations.

Table: TIM with Summer Conditions

**Conductivity**
**Thick-ness.**
**Sensible**
**Energy Saved**

**EXTWALL80 Construction**
**

**Cooling Energy**
**

**[W/m-K]**
**[m]**
**[J]**
**[J]**

**Normal case**
**Without any Insulation**
0.000
0.000
3.37E+08
0.00E+00

**With Dense Insulation Present**
0.040
0.025
3.17E+08
2.05E+07

**With Dense Insulation Present**
0.040
0.050
3.09E+08
2.84E+07

**With Dense Insulation Present**
0.040
0.100
3.02E+08
3.53E+07

**With TIM Present**
0.040
0.025
4.27E+08
-9.01E+07

**With TIM Present**
0.040
0.050
4.63E+08
-1.26E+08

**With TIM Present**
0.040
0.100
4.89E+08
-1.52E+08

**With TIM Present -R value =**
**( 0.05m,0.04W/m-K)**
0.035
0.044
4.63E+08
-1.26E+08

**With TIM Present**
**(EAST WALL)**
0.040
0.050
5.49E+08
-2.12E+08

**With TIM Present**
**(NORTH WALL)**
0.040
0.050
3.63E+08
-2.57E+07

**With TIM Present**
**(WEST WALL)**
0.040
0.050
5.64E+08
-2.27E+08

Table: TIM with Winter Conditions

**Conduc-tivity**
**Thick-ness.**
**Sensible**
**Energy Saved**
**Sensible**
 **Energy Saved**

**EXTWALL80 Construction**
**

**Heating Energy**
**Winter Clear-ness=0**
**Heating Energy**
**Winter Clear-ness=1**

**[W/m-K]**
**[m]**
**[J]**
**[J]**
**[J]**
**[J]**

**Normal case**
**Without any Insulation**
0.000
0.000
1.47E+09
0.00E+00
1.05E+09
0.00E+00

**With Dense Insulation Present**
0.040
0.025
1.30E+09
1.70E+08
9.76E+08
7.40E+07

**With Dense Insulation Present**
0.040
0.050
1.26E+09
2.10E+08
9.73E+08
7.70E+07

**With Dense Insulation Present**
0.040
0.100
1.22E+09
2.50E+08
9.74E+08
7.60E+07

**With TIM Present**
0.040
0.025
1.30E+09
1.70E+08
5.66E+08
4.84E+08

**With TIM Present**
0.040
0.050
1.26E+09
2.10E+08
4.41E+08
6.09E+08

**With TIM Present**
0.040
0.100
1.22E+09
2.50E+08
3.57E+08
6.93E+08

**With TIM Present -R value =**
**( 0.05m,0.04W/m-K)**
0.035
0.044
1.26E+09
2.10E+08
4.40E+08
6.10E+08

**With TIM Present**
**(EAST WALL)**
0.040
0.050
1.26E+09
2.10E+08
7.36E+08
3.14E+08

**With TIM Present**
**(NORTH WALL)**
0.040
0.050
1.24E+09
2.30E+08
8.31E+08
2.19E+08

**With TIM Present**
**(WEST WALL)**
0.040
0.050
1.24E+09
2.30E+08
7.07E+08
3.43E+08

## References

P.O. Braun, A. Goetzberger, J. Schmid, and W.Stahl. Transparent Insulation of Building Facades- Steps from Research to Commercial applications, Fraunhofer Institute for Solar Energy Systems, Oltmannsstrasse 22, D-7800 Freiburg, Germany.

Thermotropic materials and Systems for Overheating Protection. http://www.ise.fhg.de/Projects/Solbuild/materials.html

Robert Hausner. Arbeitsgemeinschaft Erneuerbare energie, Transparent Insulation- Areas of Application, Society for Renewable Energy. http://www..aee.at/verz/english/tin.html

Werner J.Platzer. Transparent Insulation materials: a review, Fraunhofer Institute for Solar Energy Systems, Oltmannsstr. 5, D-79100 Freiburg, Germany.

Volker Wittwer. The use of Transparent Insulation Materials and Optical Switching Layers in Window Systems, Fraunhofer Institute for Solar Energy Systems, Oltmannsstr. 5, D-79100 Freiburg, Germany.

M. Wood and L.F. Jesch. 1993. Transparent insulation technology: a technical note, Ambient Press Limited.

Façade Modules with back-ventilated Transparent Insulation- Research and Development toward Series Application. http://www.ise.fhg.de/Projects/development99/art4.html

Two 0-Energy Houses, http://www.smartarch.nl/smartgrid/items/oo5_chur.htm

Advanced Building Technologies – Transparent Insulation Materials ( TIM ). http://www.enermodal.com/advancedtech/transp.html

Transparent Insulation, http://www.esv.or.at/service/info-material/diverse/twd/index_e.htm

G. Verbeeck, H. Hens. Transparent Insulation: an alternative solution for summer discomfort. Die neue Transparenz: Warmedamm-Verbund- system StoTherm Solar.

E.Lindauer, H.Leonhardt. Brauchwasservorerwarmmung mit transparent gedammten Bauteilen ( Hybridsystem ), Fraunhofer- Institut fur Bauphysik.