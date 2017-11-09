Disable Self-Shading Effect
================
**Tianzhen Hong, Xuan Luo, LBNL**

**Original: November 2, 2017**

# Justification for New Feature 

Building energy modeling for LEED certification requires to disable self-shading effect in the baseline model, which gives credit to the proposed building with self-shading. For example, an L-shape building that is oriented correctly can provide significant self-shading and ASHRAE Standard 90.1 wants to reward that design choice. The proposed new feature will allow users to disable self-shading effect in EnergyPlus.

# Team Discussion

Some feedback from Mike Witte and Jason Glazer.

# Overview 

Currently in EnergyPlus, if the Solar Distribution is not set to MinimalShadowing, shading from detached shading, wings, overhangs, and exterior surfaces of all zones are computed. To disable self-shading effect, we would bypass the shading by exterior surfaces of all zones, which are by default created in EnergyPlus: one shading surface for each exterior surface of all zones. This is straightforward, except when a single IDF file contains two or more detached buildings (which cannot be detected by EnergyPlus). In this case, the shading of one building’s exterior surfaces on other buildings should be counted even when self-shading is disabled. 

# Approaches 

We propose to add a new field “Disable Self-shading Effect” to the ShadowCalculation object, which will skip the self-shading calculations in EnergyPlus.

	ShadowCalculation,
	  A6 ; \field Disable Self-shading Effect
	       \note If Yes, the shading effect by all other exterior surfaces of all zones is disabled
	       \note If multiple buildings are defined in one IDF file, an BuildingComplex object is required when Disable Self-shading Effect = Yes.
	       \type choice
	       \key Yes
	       \key No
	       \default No       

To address the cases with multiple buildings in a single EnergyPlus model (IDF file), we propose to add a new object BuildingComplex to explicitly define the buildings and their zones/spaces. This object will enable EnergyPlus to skip self-shading from exterior surfaces of individual zones of a building on other zones of the same building, while considering the shading of exterior surfaces of all zones of a building on exterior surfaces of other buildings. This object is only needed when users need to disable self-shading for multiple buildings in their models.

	BuildingComplex,
	       \unique-object
	       \memo This object is used to define multiple buildings and their zones, currently it is only used when self-shading effect is to be disabled.
	       \extensible:2 - repeat last two fields, remembering to remove ; from "inner" fields.
	       \min-fields 5
	   A1, \field Name
	       \type alpha
	   A2, \field Building 1 Name 
	       \type alpha
	   A3, \field Building 1 ZoneList Name
	       \type alpha
	   A4, \field Building 2 Name 
	       \type alpha
	   A5, \field Building 2 ZoneList Name
	       \type alpha
	   …
	   ;


# Testing/Validation/Data Sources

Manually inspect EnergyPlus outputs of shading calculations. 

# Input Output Reference Documentation

Update the IORef with the proposed IDD changes.

# Engineering Reference

N/A

# Output Details and Examples

N/A

# Example Files and Transition Changes

No transition changes are envisioned as a result of the new feature. A new example file will be developed to demonstrate the new feature.

# References

N/A
