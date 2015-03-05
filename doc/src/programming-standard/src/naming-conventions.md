# Naming Conventions

The naming conventions shown in the next several sections apply to all code, whether reengineered or new.  Legacy code that is brought over "as is" will not be required to undergo name changes immediately.  However, if the code is to remain in EnergyPlus, it should conform to the EnergyPlus standards – naming conventions, readability and language use.  If one must retain links to the legacy code, one can change the legacy code to comments with the new statements in succeeding blocks.

In all of the naming conventions listed below, the F90/F95 conventions of 31 character names has been used.  In F2003, variable names can be up to 63 characters – some of the EnergyPlus code takes advantage of this. Spaces are not allowed as valid characters in any of the names.  Underscores ("_") should be avoided but may be useful in defining "constants" (aka Parameters).

Note that different naming conventions apply to the Data Dictionary and Report Variables – these guidelines are contained in the Module Developer's document.

## Subroutine Naming Convention

Subroutine names should be constructed using the verb-predicate rule.  Every subroutine models an action on some item.  Thus, the subroutine name should reflect this by including both the action and the item upon which the action is taken.  The verb should be the first part of the name followed by the predicate.  Below are some examples of the verb-predicate notation:

- CorrectZoneAirTemp
- CalcZoneMassBalance
- SimAirLoops
- ReportFan

Notice that the first letter of each word is capitalized to make the name easier to read.  In general, the use of longer names for subroutine names rather than abbreviations is encouraged because subroutine names will not appear often in the code.  However, abbreviations may be used if the subroutine name is readily understandable when abbreviated.

## Module and Source Code File Naming Convention

Since modules typically are associated with objects or data groupings, the name selected for a module should refer to the object or data grouping.  For example, a module that deals with pumps in the central plant should be called "PlantPumps".  Modules which consist of only variable declarations (data-only module, see section on Variable Declarations) should use a "Data" prefix for its name followed by a logical descriptive term or terms.  An example of a potential name for a data-only module is "DataGlobals".

Since each file containing source code must consist of a single program module*, source code files should use the name of the module as the base name* and a ".f90" as the file extension.  Thus, the examples listed in the preceding paragraph would be contained in the files PlantPumps.f90 and DataGlobals.f90. It should be noted that there are some limits on file names on certain machines.  Consequently, programmers should omit terms such as "algorithm" or "model" from module names.

## Variable Naming Convention

Variable names appear in code much more often than subroutine or module names.  As a result, using long variable names can become a burden for the programmer to enter.  It also makes the code more difficult to read as program statements may continue over several lines.  On the other hand, the use of short, cryptic names makes it difficult to understand the code without extensive documentation.

For variable names, logical abbreviations are thus encouraged.  Typically, lengthy words should be shortened to somewhere between three and five characters to make a logical yet concise name for the various program variables.  For example, the variable for the humidity ratio of the air entering the cooling coil might be named "InletAirHumRat".  In addition, plurals should not be used (i.e., use Zone instead of Zones, System instead of Systems, etc.) in variable names.

A list of approved abbreviations for use in EnergyPlus programming may be found in Appendix A.  In the cases where words are five letters or shorter in length or do not have a logical abbreviation, these should not be abbreviated.  As with all of the other items found in this standard, developers should use their judgment in implementing these abbreviations within code.  When in doubt, do not use an abbreviation.

It should be noted that an explicit order for items, modifiers, etc. for variable names cannot be defined using the verb-predicate rule for the subroutine names.  This, in part, is because variables may include more than one noun (InletAirHumRat) or several modifiers (NumSingleTempHeatCoolControls).  We recommend that the variable name gives preference to the most important or higher level elements.  As always, the developer should use common sense in applying these guidelines to program code.