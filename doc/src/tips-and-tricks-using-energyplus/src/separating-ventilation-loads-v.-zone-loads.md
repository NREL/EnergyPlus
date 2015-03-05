# Separating Ventilation Loads v. Zone Loads

*Can I determine the ventilation load for PAU in PAU  + FCUs system? If can, how to split the total cooling load into room load and ventilation load for PAU sizing in energyplus?*

*In the HTML report, "Nominal total capacity [W]" (EquipmentSummary) and "Design Load [W]" (HVACSizingSummary) can be found. Are they equal to "Total cooling load" and "Room load"? (i.e. Ventilation load = "nominal total capacity" - "Design Load")*

PAU – Primary Fresh Air Handling Unit or DOAS – Dedicated Outdoor Air Unit

FCU – Fan Coil Unit

There are several ways to split the total cooling load into room load and ventilation load for PAU sizing in EnergyPlus:

#. In the eio output, section, the heating and cooling loads reported there are the peak \*sensible\* loads for each zone, without any ventilation load. These are the same values reported as "Design Load" in the HVACSizingSummary table report.
#. In the EquipmentSummary table report, the component capacities reported there are the total (cooling, sensible for heating) output capacities include any ventilation load if it impacts that component.
#. If you have a central air loop that serves only the ventilation load, and zone equipment that serves only the zone load, there is an autosizing option in Sizing:System that should autosize the central system appropriately.

From example file 5ZoneCoolBeam.idf:

~~~~~~~~~~~~~~~~~~~~

    Sizing:System,VAV Sys 1, !- AirLoop NameVentilationRequirement, !- Type of Load to Size Onautosize, !- Design Outdoor Air Flow Rate {m3/s}1.0, !- Minimum System Air Flow Ratio
~~~~~~~~~~~~~~~~~~~~

When you run a simulation, if you want to report ventilation loads, the following Output:Variable names are available:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Zone Mechanical Ventilation No Load Heat Removal [J]HVAC,Sum,Zone Mechanical Ventilation Cooling Load Increase [J]HVAC,Sum,Zone Mech Ventilation Cooling Load Increase: OverHeating [J]HVAC,Sum,Zone Mechanical Ventilation Cooling Load Decrease [J]HVAC,Sum,Zone Mechanical Ventilation No Load Heat Addition [J]HVAC,Sum,Zone Mechanical Ventilation Heating Load Increase [J]HVAC,Sum,Zone Mech Ventilation Heating Load Increase: OverCooling [J]HVAC,Sum,Zone Mechanical Ventilation Heating Load Decrease [J]
~~~~~~~~~~~~~~~~~~~~