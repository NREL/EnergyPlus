New Feature Proposal Draft

# Fast and accurate g-function calculations for modeling ground heat exchangers


**Jeffrey D. Spitler and Jack C. Cook, OSU**

- Original Date 2021-04-09

## Justification for New Feature
In order for EnergyPlus to simulate a ground heat exchanger (GHE) used with a
ground-source heat pump (GSHP) system, a dimensionless response function,
called a g-function, is required.  The dimensionless response depends primarily
on geometry – locations of boreholes and depths – though the borehole thermal
resistance and flow rate can also have a minor effect.  Users of EnergyPlus
currently have three possibilities for obtaining the g-function:

- EnergyPlus can calculate the g-function with a boundary condition known as
“uniform heat flux” – that is the heat flux per unit length is assumed uniform
over the entire fields.  This assumption works reasonably well for small fields,
but causes significant errors as the number of boreholes increases.
(Malayappan and Spitler 2013)
- The g-function can be input by the end user after determining it for a
specific configuration with a 3rd party tool, GLHEPRO. (Spitler 2000, OSU 2016)
This has the disadvantage of requiring the user to use a commercially
available, non-free, software program.  GLHEPRO relies upon a library of
g-functions that were pre-calculated using the uniform borehole wall
temperature boundary condition, so is not able to calculate g-functions on the
fly for larger fields.  (GLHEPRO does have an implementation of the uniform
heat flux g-function calculation, but based on the findings in Malayappan
and Spitler (2013), limits its usage to a maximum of 36 boreholes. GLHEPRO
does write a snippet of IDF that can be pasted into the user’s IDF file.
- Another tool, pygfunction (Cimmino 2018b) could be used to calculate a
g-function for any configuration.  As shown by Spitler et al. (2020), this
feature can be a powerful aid in optimizing the system performance.  Pygfunction
requires the user to be familiar with Python programming, and to write the IDF
description from scratch.  For larger fields, the computational time can become
excessive (e.g. hours).  A more significant constraint is that the memory
requirements can vastly exceed what is available even on highly-resourced
workstations.

As each of the above methods has significant limitations, it would be highly
desirable to add the capability to calculate g-functions with a method that is
as accurate as that available in GLHEPRO and pygfunction, but which allows
user-specified (non-library) borehole configurations, without excessive
computational time or memory requirements.

## E-mail and Conference Call Conclusions

N/A

## Overview

Starting with the methodology utilized by pygfunction to calculate g-functions
with uniform borehole wall temperature, a C++ implementation
(Cook and Spitler 2021) referred to as “cpgfunction” has been developed.  
The solution has been considerably restructured to give an 8-fold reduction
in required memory.  The reduction in memory depends on the number of finite
line source segments, as shown in Figure 1. This reduction in memory comes at
the expense of some computational speed, but for irregular configurations, as
shown in Figure 2, cpgfunction increases speed by a factor of four when there
are more than about 5000 sources and by higher amounts with smaller amounts of
sources.

![F1_memory_comparison](images_cpgfunction/F1_memory_comparison.png)

Figure 1 Comparison of pygfunction and cpgfunction memory requirements

![F2_memory_comparison](images_cpgfunction/F2_timing_comparison.png)

Figure 2 Comparison of pygfunction and cpgfunction computational speed for
irregular borehole configurations

Beyond the improvements described by Cook and Spitler (2021), this new feature
will include an adaptive discretization algorithm to maximize speed and minimize
memory requirements while maintaining acceptable accuracy.

## Approach

The recently developed “cpgfunction” implementation (Cook and Spitler 2021) will
be adapted for use in EnergyPlus.  In this section, we give an overview of the
methodology.  Cpgfunction and pygfunction use a discretized representation of
the ground heat exchanger, with each borehole divided into multiple segments.  
The effect of each segment on every other segment is determined with an
analytical solution known as the finite line source (FLS).  When applied in
this way, with each borehole represented by multiple finite line sources, the
method is sometimes referred to as stacked finite line source (SFLS).

The algorithm has three major steps:

1. Discretization of the boreholes into finite line segments.  The actual
discretization is simple – boreholes are divided into equal length segments –
but specifying a minimal number of segments that achieve sufficient accuracy is
more difficult.  We are currently finalizing an algorithm that we call the
“adaptive discretization scheme” that ensures sufficient accuracy.  The
methodology for this is similar to a grid-independency study.  Past work in the
field has not included rigorous examination of this question.
2. Analysis of similarities and calculation of segment-to-segment responses.
For, say, a system with 200 boreholes and 12 segments per borehole, there are
24002 segment-to-segment responses to be calculated.  This can be quite
computationally time-consuming and, therefore, as proposed by (Cimmino 2018a),
an analysis of similarities is done first.  By similarities, we mean pairs of
segments with identical or near-identical horizontal and vertical offsets, such
that the segment-to-segment responses are identical or near-identical.  
Therefore, an analysis is done first, and segment-to-segment responses are only
calculated for the unique cases.
3. Superposition of the segment-to-segment responses.  This step may seem
simple, and it is simple if the uniform heat flux boundary condition is used
and all segments have a constant and uniform heat input.  However, as shown by
Malayappan and Spitler (2013), this choice gives inaccurate results for larger
borefields.  The uniform borehole wall temperature has been considered the
reference method for some years, has been widely used to size ground heat
exchangers and has had the benefit of some field validation.
(Cullin et al. 2015)  Arguably, the uniform inlet fluid temperature
method has even better physical justification.  Cpgfunction currently
implements the uniform borehole wall temperature boundary condition, and
as will be discussed below, this is advantageous.

The project team is currently finalizing research into what we call the
“adaptive discretization scheme”, and it takes advantage of the fact that using
the uniform borehole wall temperature approximation with a reduced number of
segments can give a quite good match to the uniform inlet fluid temperature
boundary condition.  This gives us the possibility of significant reduction
in both memory and time requirements while also obtaining a highly accurate
solution.  The memory reduction is illustrated in Figure 3 for a borefield with
about 18000 segments.  Cpgfunction using the same number of segments reduces the
memory required from 435 GB to 50 GB.  Applying the adaptive discretization
scheme then gives a further 10-fold reduction in memory requirements to 5 GB.  
The reduction in computation time depends on the amount of similarities in the
field.  With an irregularly shaped field with few similarities, we would expect
a 4-fold reduction in using cpgfunction with the same number of segments and a
further 20-fold reduction using the adaptive discretization scheme.

![F3_memory_reduction](images_cpgfunction/F3_memory_reduction.png)

Figure 3  Reduction in memory requirements. Downward vertical arrow corresponds
to reduction with cpgfunction using the same number of segments as pygfunction.  
Other arrow represents savings of adaptive discretization scheme.

## Testing/Validation

G-functions are specified as a series of points `(g, ln(t/t_s))`.  A proposed
testing algorithm would involve calculation of g-functions for several borehole
configurations and comparison of the results with pre-calculated g-functions.  
A sample C-shaped configuration is shown in Figure 4; the dimensions are in m.  
Figure 5 shows the g-function corresponding to a 96m depth with the 27 discrete
values marked as points.  The testing algorithm will compute the g-function,
also in 27 discrete values, and the RMSE between the EnergyPlus calculated
g-function and the pre-calculated g-function.  An RMSE of less than 0.1% should
indicate a “pass”.

![F4_C_configuration](images_cpgfunction/F4_C_configuration.png)

Figure 4 Sample C-shaped configuration

![F5_g_funtion_C_configuration](images_cpgfunction/F5_g_funtion_C_configuration.png)

Figure 5 Sample g-function showing discrete values

## I/O Reference Documentation

EnergyPlus can already calculate g-functions with the uniform heat flux boundary
conditions.  It would be possible to simply replace the algorithm for computing
g-functions and make no changes to the GroundHeatExchanger:System object.  
Borehole locations will be specified with GroundHeatExchanger:Vertical:Single
objects.
However, for purposes of testing, validation, and research it is desirable to
retain the uniform heat flux boundary condition calculation as a non-default
option.  Therefore, we propose a new key for the GroundHeatExchanger:System
object, the “g-function calculation model name”, with values of:

- UHFcalc for the existing uniform heat flux boundary condition calculation
- UBHWTcalc for the new calculation method that utilizes uniform borehole wall
temperature boundary conditions.

Sample IDF and IDD follow with changes marked in red:

```
!!! IDF example using GHE:Vertical:Array input !!!

GroundHeatExchanger:System,
  Vertical GHE 1x4 Std,    !- Name
  GLHE Inlet,              !- Inlet Node Name
  GLHE Outlet,             !- Outlet Node Name
  0.004,                   !- Design Flow Rate {m3/s}
  Site:GroundTemperature:Undisturbed:KusudaAchenbach,  !- Undisturbed Ground Temperature Model Type
  KATemps,                 !- Undisturbed Ground Temperature Model Name
  2.5,                     !- Ground Thermal Conductivity {W/m-K}
  2.5E+06,                 !- Ground Thermal Heat Capacity {J/m3-K}
  ,                        !- GHE:Vertical:ResponseFactors Object Name
  UHFcalc,                 !- g-function Calculation Model Name
  GHE-Array;               !- GHE:Vertical:Array Object Name
```
<span style="color:red">UHFcalc, !- g-function Calculation Model Name</span>

```
!!! IDF example using GHE:Vertical:Single input !!!

  GroundHeatExchanger:System,
    Vertical GHE 1x4 Std,    !- Name
    GLHE Inlet,              !- Inlet Node Name
    GLHE Outlet,             !- Outlet Node Name
    0.004,                   !- Design Flow Rate {m3/s}
    Site:GroundTemperature:Undisturbed:KusudaAchenbach,  !- Undisturbed Ground Temperature Model Type
    KATemps,                 !- Undisturbed Ground Temperature Model Name
    2.5,                     !- Ground Thermal Conductivity {W/m-K}
    2.5E+06,                 !- Ground Thermal Heat Capacity {J/m3-K}
    ,                        !- GHE:Vertical:ResponseFactors Object Name
    UHFcalc,                 !- g-function Calculation Model Name
    ,                        !- GHE:Vertical:Array Object Name
    BH1,                     !- GHE:Vertical:Single Object Name 1
    BH2;                     !- GHE:Vertical:Single Object Name 2
```
<span style="color:red">UHFcalc, !- g-function Calculation Model Name</span>

```
!!! IDD Modifications !!!

GroundHeatExchanger:System,
    \memo Models vertical ground heat exchangers systems using the response factor approach
    \memo developed by Eskilson. Response factors are calculated using a finite line source
    \memo model assuming uniform heat flux at the borehole wall if UHFcalc is specified,
    \memo or uniform borehole wall temperature if UBHWTcalc is specified.
    \extensible:1
    \min-fields 9
    A1,   \field Name

    ...

    A6, \field GHE:Vertical:ResponseFactors Object Name
        \type object-list
        \object-list GroundHeatExchangerVerticalResponseFactorNames
    A7, \field g-function Calculation Model Name
        \type choice
        \key UHFcalc
        \key UBHWTcalc
    A8, \field GHE:Vertical:Array Object Name
        \type object-list
        \object-list GroundHeatExchangerVerticalArrayNames
    ...
```
<span style="color:red">\memo model assuming uniform heat flux at the borehole
wall if UHFcalc is specified,</span>

<span style="color:red">\memo or uniform borehole wall temperature if UBHWTcalc
is specified.</span>

<span style="color:red">A7, \field g-function Calculation Model Name
        \type choice
        \key UHFcalc
        \key UBHWTcalc</span>

## Engineering Reference Documentation

The engineering reference documentation will be prepared and will reference a
conference paper (Cook and Spitler 2021, currently under review) and a journal
paper (in preparation.)

## References

Cimmino, M. (2018a). "Fast calculation of the g-functions of geothermal borehole
fields using similarities in the evaluation of the finite line source solution."
Journal of Building Performance Simulation 11(6): 655-668.

Cimmino, M. (2018b). pygfunction: an open-source toolbox for the evaluation of
thermal. eSim 2018, Montreál, IBPSA Canada.

Cook, J. C. and J. D. Spitler (2021). Faster computation of g-functions used for
modeling of ground heat exchangers with reduced memory consumption. Submitted to
Building Simulation 2021. Bruges, Belgium, IBPSA.

Cullin, J. R., J. D. Spitler, C. Montagud, F. Ruiz-Calvo, S. J. Rees, S. S.
Naicker, P. Konečný and L. E. Southard (2015). "Validation of vertical ground
heat exchanger design methodologies." Science and Technology for the Built
Environment 21(2): 137-149.

Malayappan, V. and J. D. Spitler (2013). Limitations of Using Uniform Heat Flux
Assumptions in Sizing Vertical Borehole Heat Exchanger Fields. Clima 2013.
Prague (Czech Republic).

OSU (2016). GLHEPro 5.0 for Windows - Users' Guide. Stillwater.

Spitler, J. D. (2000). GLHEPRO --  A Design Tool For Commercial Building Ground
Loop Heat Exchangers. Fourth International Heat Pumps in Cold Climates
Conference, Aylmer, Québec.

Spitler, J. D., J. C. Cook and X. Liu (2020). A Preliminary Investigation on the
Cost Reduction Potential of Optimizing Bore Fields for Commercial Ground Source
Heat Pump Systems. Proceedings, 45th Workshop on Geothermal Reservoir
Engineering. Stanford, California, Stanford University.
