Additional Source Term in the Exterior Surface Heat Balance
================
**Tianzhen Hong, Xuan Luo, LBNL**

**Original: November 4, 2017**

# Justification for New Feature 

In hot and humid climates, the frequent rain provides significant cooling effect (Jayamaha et al. 1997, Zhang et al. 2016) for exterior surfaces with porous materials. Currently EnergyPlus cannot consider this evaporative cooling effect in the exterior surface heat balance calculation. The proposed new feature will allow an additional source term to be included in the surface heat balance calculation.

# Team Discussion

Proposed and discussed briefly at the annual EnergyPlus team conference call.

# Overview 

Currently EnergyPlus does not consider the evaporative cooling effect of rain or the ice melting effect on exterior surfaces (e.g., roofs), which cannot be modeled by EMS either. These heat transfer processes can be complicated and not necessary the required weather parameters (e.g., rain, ice on roof) are available in the commonly used TMY weather files. Therefore, it is not the intent of this new feature to develop algorithms in EnergyPlus to calculate these heat transfer processes, rather to enable the consideration of these processes with a pre-calculated source term in the exterior surface heat balance calculation in EnergyPlus.

# Approach

We propose to add a new object SurfaceProperty:HeatSource which specifies an exterior surface with the additional heat source term described by a schedule of heat rates (can be positive or negative values) in W/m<sup>2</sup>. The heat rates are pre-calculated outside EnergyPlus.
	
	SurfaceProperty:HeatSource,
	       \memo Allows additional heat source term in the surface heat balance calculation
	       \memo For Advanced/Research Usage
	       \min-fields 2
	  A1 , \field Surface Name
	       \required-field
	       \type object-list
	       \object-list SurfaceNames
	  A2 ; \field Heat Source Schedule Name
	       \required-field
	       \type object-list
	       \object-list ScheduleNames
	       \note The schedule values are heat rate per surface area (W/m2)

# Testing/Validation/Data Sources

Manually inspect EnergyPlus outputs of the new example file. 

# Input Output Reference Documentation

Update the IORef with the proposed IDD changes.

# Engineering Reference

N/A

# Output Details and Examples

N/A

# Example Files and Transition Changes

No transition changes are envisioned as a result of the new feature. A new example file will be developed to demonstrate the new feature.

# References

S.E.G. Jayamaha, N.E. Wijeysundera, S.K. Chou. Effect of rain on the heat gain through building walls in tropical climates, Building and Environment, 32(5): 465-477, 1997.

L. Zhang, R. Zhang, Y. Zhang, T. Hong, Q. Meng, Y. Feng, The impact of evaporation from porous tile on roof thermal performance: A case study of Guangzhou's climatic conditions. Energy and Buildings, 2016.

