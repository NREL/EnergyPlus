# Effective Moisture Penetration Depth (EMPD) Model

## Overview

Moisture has little effect on heating system performance, but a profound effect on the performance of air conditioning systems.  In order to accurately describe building performance during periods when cooling is needed, it is very important to know the moisture conditions of the building.  If one assumes that all building moisture is contained in the room air, then one ignores the fact that the materials that bound the room (e.g. wall surfaces, furnishings, linens, etc.) store and release moisture.  Thus, to assume that the only moisture that effects cooling system performance is contained in the room air is a false, and it can lead to significant error in the prediction of room moisture conditions and cooling system loads.

The EMPD (Effective Moisture Penetration Depth) model is a simplified, lumped approach to simulate surface moisture adsorption and desorption.

## EMPD Model Description

The EMPD concept assumes that a thin layer (δ~M~) close to the wall surface behaves dynamically and exchanges moisture with the air domain when exposed to cyclic air moisture pulses.  For short periods where the cyclic integral of the total moisture adsorption and desorption is near zero (i.e. there is no net moisture storage), the EMPD concept has been shown to be a reasonable approximation of reality (Kerestecioglu et al, 1989).  In other words, the following constraint must be met:

![](media/image226.png)\


where, τ~2~-τ~1~ denotes the finite time interval over which the equation holds.  The EMPD model assumes no spatial distribution of moisture content across the thickness (L) of the solid; rather, a thin layer (δ~M~) of uniform moisture content (U) is assumed to represent the total moisture content of the solid. This may be mathematically stated as:

![](media/image227.png)\


For most building materials, the equilibrium moisture sorption isotherm can be defined by the following general equation (Kerestecioglu et al. 1988):

![](media/image228.png)\


where

![](media/image229.png)\


and

![](media/image230.png)\


 Given that U=U(W^\*^,T^\*^), the moisture content may be differentiated with respect to time in the following manner:

![](media/image231.png)\


where A~T~ and B~ρ~ are the isothermal moisture capacity and thermo-gradient coefficient, respectively.  From Eqs. ,  and , they can be expressed as:

![](media/image232.png)\


and

![](media/image233.png)\


The lumped mass transfer equation for the i-th solid domain may be written as

![](media/image234.png)\


Using Eqs. , ,  and , one obtains the final equation needed for closure moisture transfer at internal surface.

![](media/image235.png)\


The energy equation for the envelope contains the surface temperature and is given by the conduction equation

![](media/image236.png)\


with the boundary conditions at interior surface

![](media/image237.png)\


A more detailed account of the numerical solution procedure can be found in Kerestecioglu et al. (1988).

## EMPD Value Determination

An effective moisture penetration depth may be determined from either experimental or detailed simulation data by using actual surface areas and moisture vapor diffusivity.  An empirical function derived from the detailed simulation may be used to determine the EMPD value (Kerestecioglu et al, 1989):

![](media/image238.png)\


where

![](media/image239.png)\


Figure 17 gives the EMPD values to be used for various vapor diffusivities evaluated at different ambient excitations.

![Limit of Effective Penetration Depth Values for Various Vapor Diffusivities at Different Ambient Excitations.](media/limit-of-effective-penetration-depth-values.png)


## EMPD Nomenclature

A= Area [m^2^]

A~T~= Isothermal moisture capacity [m^3^/kg]

B~ρ~= Thermo-gradient coefficient [kg/kg-K]

C~p~= Specific heat [J/kg.K]

h~M~= Convective mass transfer coeff. [kg/m^2^-s]

h~T~= Convective heat transfer coeff. [W/m^2^-K]

k= Thermal conductivity [W/m-K]

L= Length [m]

q"~T~= Imposed heat flux [W/m^2^]

R~v~= Ideal gas constant [461.52 J/kg-K]

T= Temperature [K]

U= Moisture content [kg/kg]

W= Humidity ratio [kg/kg]

### Greek letters

δ~M~= Effective penetration depth for moisture equation [m]

λ= Heat of vaporization [J/kg]

ρ= Density [kg/m^3^]

τ= Time [s]

φ= Relative humidity [0 to 1]

ξ= Ambient moisture excitation rate [1/h]

### Subscripts and superscripts

a= Air

b= Bulk

\*          = Surface

i= i-th surface

## References

Kerestecioglu, A., Swami, M., Dabir, R., Razzaq, N., and Fairey, P., 1988, "Theoretical and Computational Investigation of Algorithms for Simultaneous Heat and Moisture Transport in Buildings," FSEC-CR-191-88, Florida Solar Energy Center, Cape Canaveral, FL.

Kerestecioglu, A., M. Swami and A. Kamel, 1989, "Theoretical and Computational Investigation of Simultaneous Heat and Moisture Transfer in Buildings: Effective Penetration Depth Theory."  ASHRAE Winter Meeting, Atlanta, GA.

Kerestecioglu, A., M. V. Swami, P. Brahma, L. Gu, P. Fairey, and S. Chandra, 1989, "FSEC 1.1 User's Manual," Florida Solar Energy Center, Cape Canaveral, FL