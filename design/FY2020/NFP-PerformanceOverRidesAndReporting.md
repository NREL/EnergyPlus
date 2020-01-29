Performance Overrides and Reporting
================

**Jason Glazer, GARD Analytics**

 - January 29, 2020
 

## Justification for New Feature ##

Provide some new controls to speed up the performance of EnergyPlus by overriding current defaults 
as well as providing reporting to make monitoring performance tuning of specific simulations easier.

## E-mail and  Conference Call Conclusions ##

None

## Overview ##

Add to the PerformancePrecisionTradeoffs object to override:

 - Zone Time Step
 - Zone Air Heat Balance Algorithm
 - Harwired and user set convergence parameters
 
Add reporting of the PerformancePrecisionTradeoffs object into the EIO file and Tabular Initialization 
Summary. Also include in the EIO report other performance oriented options that exist such as the 
recent sizing speed up work from TRANE [#7567](https://github.com/NREL/EnergyPlus/pull/7567) and 
the CarrollMRT method [#7534](https://github.com/NREL/EnergyPlus/pull/7567).

Add new output variables for:

- oscillating zone temperature during occupancy
- oscillating zone temperature while floating
- site energy consumption during oscillating

Add a new output file related to performance that is a log file and gets appended to after each 
simulation of the same file. The file would be comma delimitted and would show the EIO performance 
parameters described above as well as total energy by energy type, peak electrical demand, the oscillation 
variables. It would also include the total number of objects and fields to indicate if the input file
changed between simulations. The intention is that the user could review this after successive simulations 
when testing out the various performance parameters.
 

## Approach ##

Override the default inputs and hard wired convergence parameters with other values.

## Testing/Validation/Data Sources ##

insert text

## Input Output Reference Documentation ##

insert text

## Input Description ##

The current PerformancePrecisionTradeoffs object is shown below:

```
PerformancePrecisionTradeoffs,
      \unique-object
      \memo This object enables users to choose certain options that speed up EnergyPlus simulation,
      \memo but may lead to small decreases in accuracy of results.
  A1, \field Use Coil Direct Solutions
      \note If Yes, an analytical or empirical solution will be used to replace iterations in
      \note the coil performance calculations.
      \type choice
      \key Yes
      \key No
      \default No
  A2; \field Zone Radiant Exchange Algorithm
      \note Determines which algorithm will be used to solve long wave radiant exchange among surfaces within a zone.
      \type choice
      \key ScriptF
      \key CarrollMRT
      \default ScriptF
```

The revised version of the PerformancePrecisionTradeoffs as proposed is shown below:

```
PerformancePrecisionTradeoffs,
      \unique-object
      \memo This object enables users to choose certain options that speed up EnergyPlus simulation,
      \memo but may lead to small decreases in accuracy of results.
  A1, \field Use Coil Direct Solutions
      \note If Yes, an analytical or empirical solution will be used to replace iterations in
      \note the coil performance calculations.
      \type choice
      \key Yes
      \key No
      \default No
  A2, \field Zone Radiant Exchange Algorithm
      \note Determines which algorithm will be used to solve long wave radiant exchange among surfaces within a zone.
      \type choice
      \key ScriptF
      \key CarrollMRT
      \default ScriptF
  A3, \field Override Zone Time Step
      \note If Yes, the zone time step (TimeStep object) will be set to one timestep per hour
      \type choice
      \key Yes
      \key No
      \default No
  A4, \field Override Zone Air Heat Balance Algorithm
      \note If Yes, the zone air heat balance algorithm (ZoneAirHeatBalanceAlgorithm object) will be set to Euler
      \type choice
      \key Yes
      \key No
      \default No
  A5, \field Convergence Parameter Choice
      \note Normal uses the normal input values. Low medium and high provide three different sets of convergence 
      \note parameter overrides
      \type choice
      \key Normal
      \key Low
      \key Medium
      \key High
      \default Normal
  A4, \field Create Performance Log File
      \note If Yes, a log file with perflog extension is created that is appended to each 
      \note simulation with performance related settings and results
      \type choice
      \key Yes
      \key No
      \default No

```


## Outputs Description ##

The new output variables would be based on the current oscillation variable shown below:

```
Output:Variable,*,Zone Oscillating Temperatures Time,hourly; !- HVAC Sum [hr]
Output:Variable,*,Facility Any Zone Oscillating Temperatures Time,hourly; !- HVAC Sum [hr]
```

The new output variables would add:

```
Output:Variable,*,Zone Oscillating Temperatures During Occupancy Time,hourly; !- HVAC Sum [hr]
Output:Variable,*,Facility Any Zone Oscillating Temperatures During Occupancy Time,hourly; !- HVAC Sum [hr]
Output:Variable,*,Zone Oscillating Temperatures While Floating Time,hourly; !- HVAC Sum [hr]
Output:Variable,*,Facility Any Zone Oscillating Temperatures While Floating Time,hourly; !- HVAC Sum [hr]
Output:Variable,*,Site Energy When Any Zone Oscillating Temperatures,hourly; !- HVAC Sum [J]
```

The EIO file and Tabular Initialization Summary would be modified to show the values of the PerformancePrecisionTradeoffs 
object. It could also include other performance oriented options that exist such as the 
recent sizing speed up work from TRANE [#7567](https://github.com/NREL/EnergyPlus/pull/7567) and 
the CarrollMRT method [#7534](https://github.com/NREL/EnergyPlus/pull/7567).

A new output file with the extension .perflog would be created. This log file would consist of the new EIO 
output and some simple overall building energy and demand results as well as oscillation outputs. The perflog
file would be appended to instead of replaced each time to facilitate evaluation of the performance objects to 
the user determine which combination is best for their particular input file. It would include a timestamp
and some summary information about the input file like the number of objects and fields. A hash of all inputs 
besides the PerformancePrecisionTradeoffs would be good but it might be too time consuming to generate.


## Engineering Reference ##

None

## Example File and Transition Changes ##

Since the PerformancePrecisionTradeoffs object is being appended, no transition rules are anticipating. The new
EIO report and initialization report will impact the output files. 

## References ##

None



