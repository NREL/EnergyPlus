Enhancement for External Shading Calculation 
================
**Tianzhen Hong, Xuan Luo, LBNL**

**Original: May 30, 2017**

**Revised: June 27, 2017**

# Justification for New Feature 

External shading, i.e., shading of an exterior building surface, is closely connected with energy use in buildings for heating, cooling, and lighting and with the occupants’ visual and thermal comfort. Accurate calculation of the shading that building environment, building elements or shading devices may cast on the building envelope, plays a key role in thermal load calculations and building energy simulation. However, external shading calculation for a target building in an urban context can be considerably slow considering many surrounding surfaces may shade a target surface. Various external tools can be used to more efficiently pre-calculate the shading fractions for many target buildings in the urban context. We propose to add links to EnergyPlus to enable the import of external shading calculation results, and saving the EnergyPlus shading calculation results for reuse.  

# Team Discussion so far
Michael Witte and Stuart Mentzer provided suggestions on input details and external tool resources.

# Overview 
Solar shading affects energy use in a building by reducing the solar heat gains received by opaque surfaces or the transmitted and absorbed solar radiation of windows. External shading also influences daylighting level in a room and the view to the exterior. Accurate calculation of solar shading on building exterior surfaces is of great importance in whole building energy simulation. Modeling a building in the urban context may involve many shading surfaces from adjacent buildings for a target building, which can significantly slow down EnergyPlus simulations due to the currently implemented shading calculation algorithms. Furthermore, for urban building energy simulation which involves many buildings, there can be more efficient ways to do the shading calculations than the current way of doing shading calculations for every building separately in EnergyPlus. There is also the potential of using GPU and other parallel computing techniques for building shading calculations.

Various simulation tools, such as Radiance and Daysim, employing state-of-the-art ray-tracing simulation techniques, can be used to pre-calculate the shading fractions for each exterior building surface. However, in the current implementation, EnergyPlus doesn’t allow overriding exterior shading calculations with external data either by EMS or co-simulation interface. Considering a large amount of information and the complexity of calculating the dynamic shading in EnergyPlus, allowing this external input would enable a huge potential to speed up EnergyPlus in urban-scale simulations. Another benefit is to enable reusing the shading results for parametric runs which usually do not change external shading. Adding this feature to EnergyPlus would also allow OpenStudio to run radiance simulations before the EnergyPlus simulation and reusing the radiance calculations for shading. 


# Approaches 
We would add an object **Shading:CalculationMethod** listed as follows. The object contains a mandatory **Field: External Shading Calculation Method**, which has two options namely **UseSchedules** and **InternalCalculation**. The field indicates whether the external shading is pre-calculated and imported as a schedule. If **UseSchedules** is chosen, the schedule field can be linked to a Schedule object **Schedule:File**. The name of the schedule is defined in the **Field: Surfaces External Shaded Fraction Schedule Name**. The **Schedule:File** object points to a CSV text file that has external shaded fraction scheduled values for each external surface considered in the calculation. The schedule values are listed in each column, and the header of the column points to the name of the corresponding exterior surface that receives the shading. For each column, the text contains 8760-8784 hours of surface external shaded fraction data, which is pre-calculated and stored in the text file. The Schedule object would be set up in the initialization, and the results can be loaded at runtime or overwritten through co-simulation or EMS code. The data pairs listed as extensible variables map each building exterior surface to a column in the schedule CSV file. During initializing, all columns are loaded at one, and each is saved corresponding to an exterior surface, and the data can be used in run time.

	Shading:CalculationMethod,
          \memo Used to specify the external shading calculation method used.
          \extensible: -- duplicate last set of Surfaces – Schedule Mapping (last 2 fields), remembering to remove ; from "inner" fields.
      A1, \field External Shading Calculation Method
          \type choice
          \key UseSchedules  
          \key InternalCalculation
          \default InternalCalculation
          \note If UseSchedules is chosen, Surfaces External Shaded Fraction Schedule Name is required.
      A2, \field Surfaces External Shaded Fraction Schedule Name
          \type object-list
          \object-list ScheduleNames
          \note Used only if External Shading Calculation Method is UseSchedules.
          \note Schedule values should be between 0 and 1.
      N1, \field Number of Data Pairs of the Surface – Column in the schedule Mapping
          \required-field
          \type integer
          \minimum 1
          \maximum 500
          \note The data pairs map an exterior surface to a column in the schedule file
      A3, \field Surface 1 Name
          \required-field
          \type object-list
          \object-list SurfaceNames
      N2, \field Surface 1 Shading Fraction Column
          \required-field
          \type integer
      A4, \field Surface 2 Name
          \required-field
          \type object-list
          \object-list SurfaceNames
      N3, \field Surface 2 Shading Fraction Column
          \required-field
          \type integer
      …

# Testing/Validation/Data Sources

TBD

# Input Output Reference Documentation

TBD

# Engineering Reference

TBD

# Output Details and Examples

TBD

# Example Files and Transition Changes

No transition changes are envisioned as a result of the new features. New example files will be developed to demonstrate the new features.

# References

n/a

# E-mail comments

**Michael J Witte <mjwitte@gard.com> Mon, Jun 5, 2017** 

Scheduled surface shaded fractions - The proposed approach of specifying a shading schedule for every surface seems inefficient.  It creates an input burden on every surface object, a schedule:file object for every surface, and the added overhead of reading and processing the schedule data from the file(s).

I've always viewed external (or cached) shading calculations as an all-or-nothing proposition.  Let's say you specify a schedule for every surface but one, and want EnergyPlus to calculated the shading for that single surface, then the full shading calculation for all surfaces still has to be done in order to figure out the shading for that one surface.  Wouldn't it be better to have some global inputs to access all shading fractions from an external file?  This would also allow an option to save all shading fractions to an external file, allowing re-use of EnergyPlus' shading calcs.

> Thanks for the great advice. We adopted the suggested approach.

**Stuart Mentzer <Stuart_Mentzer@objexx.com> Thu, Jun 1, 2017**

External shading would greatly benefit EnergyPlus for single building modeling as well as this is a big performance/scalability bottleneck. If a preferred external tool can be integrated into the "normal" EnergyPlus in place of or as an optional alternative to the current shading approach I know a few cloud providers who would be very pleased. (Perhaps they would even contribute to this work.)  I'm reaching out. If I hear anything I'll let you know.
Is reflection also being considered for external treatment? If not the octree system I added for daylighting should be used for reflection to address scalability.

>Yes, the idea is to allow import of shading fractions calculated by external tools. Reflection is not considered in the proposed work as it is another big task which can be tackled in future.




