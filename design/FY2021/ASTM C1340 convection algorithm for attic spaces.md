ASTM C1340 Convection Algorithm for Radiant Barriers
================

**Dareum Nam, NREL**

 - First draft 1/21/2021
 

## Justification for New Feature ##

Radiant barriers are a technology used in residential attics to reduce radiation heat transfer. It has been shown that EnergyPlus underestimates the savings of using these technologies compared to three other building models that specialized in attic models. The ASTM C1340 convection algorithm has been implemented in an EMS program to simulate radiant barriers and demonstrates a vast improvement in predicted performance. Introducing the ASTM C1340 algorithm into EnergyPlus would allow users to access the model more easily, reduce modeling time, and ensure proper implementation by users.

## E-mail and Conference Call Conclusions ##

insert text

## Overview ##

Overall, the task here is to implement the convection coefficients algorithm from ASTM C1340 standard. This convection algorithm for inside surfaces can be used for attics containing radiant barriers.

## Approach ##

Table 1 shows the correlations for convection coefficients from ASTM C1340 Standard.
![figure1](https://github.com/dareumnam/EnergyPlus/blob/RadiantBarrier/design/FY2021/table1.png)
ASTM C1340 Convection algorithm will be implemented for SurfaceConvectionAlgorithm:Inside. 
For radiant barriers, it can be selected Zone Inside Convection Algorithm of attic zone in Zone object.



## Testing/Validation/Data Sources ##

Standard unit tests will be used to verify that the convective coefficients are calculated properly.

## Input Output Reference Documentation ##


## Input Description ##

No new input descriptions are necessary for this work.

## Outputs Description ##

No new output descriptions are necessary for this work.

## Engineering Reference ##

insert text

## Example File and Transition Changes ##

N/A

## References ##

Fontanini, A. D., Aguilar, J. L. C., Mitchell, M. S., Kosny, J., Merket, N., DeGraw, J. W., & Lee, E. (2018). Predicting the performance of radiant technologies in attics: Reducing the discrepancies between attic specific and whole-building energy models. Energy and Buildings, 169, 69-83.

Standard Practice for Estimation of Heat Gain or Loss Through Ceilings Under Attics Containing Radiant Barriers by Use of a Computer Program


