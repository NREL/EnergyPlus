# Using Multipliers (Zone and/or Window)

## Background and Study using Multipliers

Multipliers are used in EnergyPlus for convenience in modeling. Though window multipliers are useful for any size building when you have multiple windows on a façade, zone multipliers are more useful in large buildings with several to many stories.

Zone multipliers are designed as a "multiplier" for floor area, zone loads, and energy consumed by internal gains. It takes the calculated load for the zone and multiplies it, sending the multiplied load to the attached HVAC system. The HVAC system size is specified to meet the entire multiplied zone load and will report the amount of the load met in the Zone/Sys Sensible Heating or Cooling Energy/Rate report variable. Autosizing automatically accounts for multipliers. Metered energy consumption by internal gains objects such as Lights or Electric Equipment will be multiplied.

To illustrate the benefits (and comparison of results), the MultiStory.idf example file was used. The MultiStory file is a 9 zone, 10 story/floored building with heating (ZoneHVAC:Baseboard:Convective:Electric object) and cooling (ZoneHVAC:WindowAirConditioner object). The middle zone of each floor in the original represents 4 zones (multiplier=4) and the middle floor (ZoneGroup) represents 8 floors (ZoneGroup multiplier=8). Clone representations were made for comparisons:

![Original Multistory IDF](media/original-multistory-idf.png)


In the figure above, each "middle" zone represents 4 zones.  The middle "floor" represents 8 floors. Additionally, each of the windows has a multiplier of 4 – so each window represents 4 windows of the same size. For the Multistory file, the Zone object for the center zones has the multiplier of 4. And for the center floors, the ZoneList and ZoneGroup objects to collect the zones and apply multipliers. The top floor then uses the Zone object multiplier for the center zones. Specifically:

~~~~~~~~~~~~~~~~~~~~

    <snip>
      Zone,
        Gnd Center Zone,         !- Name
        0.0,                     !- Direction of Relative North {deg}
        8.0, 0.0, 0.0,           !- Origin [X,Y,Z] {m}
        1,                       !- Type
        4,                       !- Multiplier
        autocalculate,           !- Ceiling Height {m}
        autocalculate;           !- Volume {m3}
    <snip>
      ZoneGroup,
        Mid Floor,               !- Zone Group Name
        Mid Floor List,          !- Zone List Name
        8;                       !- Zone List Multiplier

      ZoneList,
        Mid Floor List,          !- Zone List Name
        Mid West Zone,           !- Zone 1 Name
        Mid Center Zone,         !- Zone 2 Name
        Mid East Zone;           !- Zone 3 Name
    <snip>
      Zone,
        Top Center Zone,         !- Name
        0.0,                     !- Direction of Relative North {deg}
        8.0,                     !- X Origin {m}
        0.0,                     !- Y Origin {m}
        22.5,                    !- Z Origin {m}
        1,                       !- Type
        4,                       !- Multiplier
        autocalculate,           !- Ceiling Height {m}
        autocalculate;           !- Volume {m3}
~~~~~~~~~~~~~~~~~~~~

For comparison purposes, clones of the middle zones were done.

![Multistory with cloned middle zones.](media/multistory-with-cloned-middle-zones..png)


And, finally, the entire building was created:

![Multistory building -- fully cloned.](media/multistory-building-fully-cloned..png)


The building is autosized. For convenience in comparison, the extreme summer and winter days were used for autosizing and the simulation was run for the 5 United States weather files that are included in the EnergyPlus release: Chicago IL; San Francisco CA; Golden CO; Tampa FL; and Washington DC.

Comparisons were done with the Zone Group Loads values (Zone Group Sensible Heating Energy and Zone Group Sensible Cooling Energy) as well as meter values for Electricity. Using the regression testing limits that are used during EnergyPlus development testing (i.e. small differences are within .001 or .5%; big differences are greater than those limits).

For the purposes of dicussion, the buildings will be called: Multistory 1 – the original 9 zone building (with multipliers and groups) ref: Figure 7; Multistory 2 – the building shown in Figure 8. Multistory with cloned middle zones.; Multistory 3 – the fully configured building – ref Figure 9.

The following table illustrates the regression testing for Multistory 2 and Multistory 3, group loads and meters versus Multistory 1 results.

Table: Multistory vs Multistory 2 and Multistory 3

Location|Multi-story 2 Loads|Multi-story 2 Meter|Multi-story 3 Loads|Multi-story 3 Meter
--------|-------------------|-------------------|-------------------|-------------------
USA_IL_Chicago-OHare.Intl.AP.725300_TMY3|Small Diffs|Equal|Big Diffs\* (76%)|Big Diffs\* (62%)
USA_CA_San.Francisco.Intl.AP.724940_TMY3|Big Diffs\* (2.43%)|Big Diffs\* (.6%)|Big Diffs\* (49%)|Big Diffs\* (41)%
USA_CO_GOLDEN-NREL.724666_TMY3|Small Diffs|Small Diffs|Big Diffs\* (26%)|Big Diffs\* (24%)
USA_FL_Tampa.Intl.AP.722110_TMY3|Small Diffs|Small Diffs|Big Diffs\* (6%)|Big Diffs\* (2%)
USA_VA_Sterling-Washington.Dulles.Intl.AP.724030_TMY3|Equal|Equal|Big Diffs\* (91%)|Big Diffs\* (72%)

\* Big Diffs maximum occur in monthly values whereas the runperiod values are much smaller.

To try to pare down the discrepancies shown here, the effects of height that are used in the calculations were removed (i.e., the Site:WeatherStation and Site:HeightVariation objects were entered as below to negate the effects of height on the environmental variables such as wind and temperature).  In addition the height effect was removed from the OutdoorAir:Node object.

~~~~~~~~~~~~~~~~~~~~

      Site:WeatherStation,
        ,          !- Wind Sensor Height Above Ground {m}
        ,          !- Wind Speed Profile Exponent
        ,          !- Wind Speed Profile Boundary Layer Thickness {m}
        0;         !- Air Temperature Sensor Height Above Ground {m}

      Site:HeightVariation,
        0,         !- Wind Speed Profile Exponent
        ,          !- Wind Speed Profile Boundary Layer Thickness {m}
        0;         !- Air Temperature Gradient Coefficient {K/m}
~~~~~~~~~~~~~~~~~~~~


With these included, the files were rerun with the following results:

Table: Multiplier Results with negated height variation.

Location|Multi-story 2 Loads|Multi-story 2 Meter|Multi-story 3 Loads|Multi-story 3 Meter
--------|-------------------|-------------------|-------------------|-------------------
USA_IL_Chicago-OHare.Intl.AP.725300_TMY3|Small diffs|Small diffs|Small diffs|Small diffs
USA_CA_San.Francisco.Intl.AP.724940_TMY3|Small diffs|Small diffs|Small diffs|Small diffs
USA_CO_GOLDEN-NREL.724666_TMY3|Small diffs|Small diffs|Small diffs|Small diffs
USA_FL_Tampa.Intl.AP.722110_TMY3|Small diffs|Small diffs|Small diffs|Small diffs
USA_VA_Sterling-Washington.Dulles.Intl.AP.724030_TMY3|Small diffs|Small diffs|Small diffs|Small diffs

To investigate if other systems might have different results, the Ideal Loads System was used as the system.  Similar results were found for the multipliers vs cloned results. However, it may also be noted that the results between the original systems (baseboard and window ac) vs the ideal loads were very similar.

The biggest difference really comes in calculation time. As shown in the following table,

Table: Runtimes for Multistory files (baseboard/window ac)

Location|Multi-story 1 (9 zones)|(mm:ss)|Multi-story 2  (18 zones)|(MM:SS)|Multi-story 3|(60 zones)|(MM:SS)
--------|-----------------------|-------|-------------------------|-------|-------------|----------|-------
USA_IL_Chicago-OHare.Intl.AP.725300_TMY3|1:05|2:14|13:15
USA_CA_San.Francisco.Intl.AP.724940_TMY3|1:04|2:05|13:20
USA_CO_GOLDEN-NREL.724666_TMY3|1:17|2:28|14:43
USA_FL_Tampa.Intl.AP.722110_TMY3|1:11|2:21|13:43
USA_VA_Sterling-Washington.Dulles.Intl.AP.724030_TMY3|1:05|2:15|13:18

Because the overall results were so similar, the run times for the Ideal Loads runs are included:

Table: Runtime for Multistory files (ideal loads)

Location|Multi-story 1|(9 zones)|(mm:ss)|Multi-story 2|(18 zones)|(MM:SS)|Multi-story 3|(60 zones)|(MM:SS)
--------|-------------|---------|-------|-------------|----------|-------|-------------|----------|-------
USA_IL_Chicago-OHare.Intl.AP.725300_TMY3|0:51|1:34|9:37
USA_CA_San.Francisco.Intl.AP.724940_TMY3|0:50|1:34|9:59
USA_CO_GOLDEN-NREL.724666_TMY3|0:51|1:40|10:31
USA_FL_Tampa.Intl.AP.722110_TMY3|0:51|1:36|10:05
USA_VA_Sterling-Washington.Dulles.Intl.AP.724030_TMY3|0:51|1:36|9:48

More zones (and, particularly more surfaces) make for longer run times.

## Guidelines for Using Multipliers and Groups

- If the basic zone geometry is identical, make one zone, copy & paste it as necessary, then change the Zone Origin field to locate each zone correctly.
- Do not use interzone surfaces between zones that are multiplied. Set the adjoining surfaces to be adiabatic, i.e. use the OtherZoneSurface exterior boundary condition with the other surface pointing back to itself.
- Locate the middle floor zones roughly halfway between top and ground because exterior convection coefficients change with height. Halfway should cause the differences to average out. If you have many stories (the example only has 10 stories), consider using more middle floor zones.
- Consider removing the effects of height variation for the simulation.
- Follow guidelines in HVACTemplate and other objects about sizing if you are mixing autosize fields with hard sized fields (recommended to "autosize" all fields rather than mix).
- All HVAC system sizes must be specified to meet the entire multiplied zone load.
- Autosizing automatically accounts for multipliers.