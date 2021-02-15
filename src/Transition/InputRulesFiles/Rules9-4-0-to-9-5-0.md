Input Changes version 9.4.0 to 9.5.0
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: Construction:AirBoundary

Summary: The fields for *Solar and Daylighting Method* and *Radiant Exchange Method* have been removed. All air boundaries will be modeled using the former "GroupedZones" option. The former options for "InteriorWindow" and "IRTSurface" are no longer available and will generate a transition warning.

Field 1 remains the same.
Fields 2 and 3 are deleted.
Fields 4-6 remain the same, shifting up to new fields 2-4.

See [8370](https://github.com/NREL/EnergyPlus/pull/8370)


# Object Change: ZoneAirMassFlowConservation

Summary: name of the first input field have been renamed, the two existing key choices (*Yes* and *No*) have been renamed and three more new choice keys have been added.  

Field 1 renamed from *Adjust Zone Mixing For Zone Air Mass Flow Balance* to *Adjust Zone Mixing and Return For Air Mass Flow Balance*.
      - Choice key *Yes* has been replaced with *AdjustMixingOnly*.
	  - Choice key *No* has been replaced with *None*.
	  - New choice key *AdjustReturnOnly* has been added.
      - New choice key *AdjustMixingThenReturn* has been added.
      - New choice key *AdjustReturnThenMixing* has been added.
      - Now there are five choice keys. 
Fields 2-3 remain the same.

See [pull request 8460] (https://github.com/NREL/EnergyPlus/pull/8460)
