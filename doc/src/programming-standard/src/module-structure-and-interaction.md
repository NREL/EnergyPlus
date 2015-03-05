# Module Structure and Interaction

## Module Usage in EnergyPlus

The module is the building block of the EnergyPlus program code.  Modules are used to bring together either related algorithms or related data.  In EnergyPlus, modules are used for grouping both data and algorithms because in many cases they are linked together.  The EnergyPlus Module Structure has four important goals:

- to promote uniformity of program code
- to simplify the process of adding additional modules to the code
- to enhance the testing capability of various program elements
- to replace the extremely confusing data structure with a more understandable, segmented data structure as defined in the previous section.

Modules are most consistently organized when a map of the various modules and their interaction form an inverted tree or pyramid shape.  EnergyPlus uses a modified form of this tree structure in that one main driver module accesses the heat balance module.  In turn, the heat balance module interacts with the main system module through the heat balance-system "interface".  In a like manner, the major blocks of the HVAC code also interact through defined "interfaces".  Each of these main program elements access routines and data contained in other related modules.

The transformation of the code from a distributed program environment into discrete modules requires careful planning.  The next several sections outline the potential elements of each module.  This information should serve as a guide to constructing a module either from new or legacy code.  Again, programmers should remember to take both data structure and algorithmic considerations into account when constructing a module for EnergyPlus.

## Driver Subroutines

Driver routines are subroutines contained within a module that are called from subroutines in other modules.  Access to the module and its data elements are only allowed through the driver routines.  These routines would be the only PUBLIC routines in the module (with the possible exception of some of the input routines) since they are accessed from outside of the module.  All other routines in this module are accessed from these main driver routines.

The main driver subroutines may contain any programming necessary to model the component or element on a macroscopic level.  Other details of the algorithm should be contained in one of the subroutine types described below.  Therefore, the driver routines will in most cases be a list of subroutine calls and possibly some control logic.  For convenience, the programmer may specify a sub-driver routine for each of the main subroutine sections described below to minimize the number of calls that appear in the main driver routine.

## Environment Flags

One set of variables that are candidates for elevation to "GLOBAL" status is the environment flags.  The environment flags serve to keep track of time during the simulation. There are environment variables for hour, time step, sub-time step, day, etc.  In addition, there are flags which are set to tell whether the current moment in the simulation is the beginning of a particular time frame (time step, hour, day, etc.) or at the end of the time frame.  This information is extremely important to the driver subroutine that controls the simulation of the component module.

Based on the values of the environment flags, the driver will decide what types of initializations are required, whether input data must be read, the record keeping that must be done, and if reporting is necessary.  In other words, the environment variables help the driver control all of the actions taken on the local and module variables except actual changes required by the model algorithms.

## User Data Interface Subroutines (Get routines)

Most, if not all, modules require some input from the user such as design values, locations, schedules, etc.  Consequently, these modules must have subroutines that interface with the user data.  These routines are called from the driver routine(s) of this module or possibly from an input reading driver routine.  It is also conceivable that this section might include some standard file operations such as "new", "open", "save", "save as", etc. that would relate to the external storage of input data for this module.  In short, these routines are responsible only for transferring information from the user-input file to the particular variables.

## Initialization Subroutine(s)

All routines that perform any required manipulations on the user data to obtain simulation ready information are considered initialization subroutines.  The routines in this section may include several routines that are called from the driver subroutines within this module based on the value of the global environment (status) flags.  For example, at the beginning of the simulation, one subroutine may be called to set up information that will be valid throughout the simulation such as processing of coil design data into a simulation-ready format.  There may also be monthly, daily, hour, and time step level initializations that must be performed.  Each of these levels could have a separate subroutine in this section dedicated to such initializations. Generally, though, it is preferred that all initializations be performed in one "Init" routine.

## Calculation Routines and Utility Subroutines and Functions

This section includes any calculations that are required to simulate the element represented by this module.  Thus, most of the details of the algorithm will be contained in this section. There should be one "Calc" routine and as many utility routines as are needed. Generally, no data movement will take place in the calculation section of the module.

## Update Routine(s)

This section normally consists of one routine that performs any data transfer or movement that is needed within EnergyPlus, after the actual calculation in the module has taken place.

## Reporting Subroutine(s)

Usually there is one reporting routine for each module. This routine will do any calculation that is needed strictly for output or reporting purposes and will contain the calls to the EnergyPlus reporting routines.

## USE Statements in EnergyPlus

Once the various modules have been constructed, the "blocks" of the "pyramid" must be assembled using the F90 "USE" statements.  The USE statement allows access from one module to another.  For example, in EnergyPlus, since the ManageSimulation subroutine calls the ManageHeatBalance subroutine, it must have access to the HeatBalanceModule.  Thus, the ManageSimulation subroutine or the SimulationManager module must have a USE HeatBalanceModule statement in it to allow this access.  The USE statement also allows access to the data-only modules.

One question that arises through the implementation of USE statements is: "Where does the USE statement belong -- at the module level or the subroutine level?"  At this point, a definite rule has not been established to answer this question.  However, the following guidelines seem reasonable and appropriate to the goals of this programming standard:

- USE statements that are required for access to a subroutine that is in another module probably belong at the subroutine level.  In the example above, if the call to ManageHeatBalance is the only access to HeatBalanceModule from within the SimulationManager module, then the USE statement should probably reside at the subroutine level (in subroutine ManageSimulation in this case).
- If there are numerous calls from various subroutines in one module to another module, then the programmer may wish to elevate the USE statement to the module level for the sake of convenience.  The number of calls threshold is left to the discretion of the programmer.
- Due to the definition of "global" data (i.e., global data should be available everywhere), any USE statements which are included for access to the global data (DataGlobals) must be placed at the module level.
- USE statements for access to "super-block" data-only modules can be placed at either the subroutine or the module level though it is recommended that these USE statements reside at the module level.  If data is only used in a single subroutine, the programmer may need to consider passing the data as subroutine arguments.

## Example of the EnergyPlus Module Structure

The code listed in the Module Developer's Guide is intended to give the reader a good example of how to implement the module described in the previous sections.

## Generic Subroutines and Functions

There is a good possibility that the EnergyPlus code may contain some "generic" mathematical functions or procedures that are used by several, disparate sections of the program.  Rather than repeat this code in each module that it is required, it might be appropriate to construct a generic subroutine/function module.  Then, subroutines that need that function can use this generic module.  Some examples of processes that might be included in a generic procedure module include array-zeroing subroutines, special functions not included in the F90 standard, etc.  Developers should note that F90 does include a variety of matrix manipulation intrinsic functions.  These standard functions should be used unless there are special conditions that require more specific programming.  Subroutines or functions that are only called from a single module should be included in the utilities section of that module.  Finally, subroutines or functions added to this generic process module should be written in the most general terms so that any routine which must call into this module will be able to do this without the assistance of extra programming or the definition of a new routine.

Several routines, such as "GetNumObject" or other "Get" routines as well as Psychrometric functions have already been defined for the EnergyPlus Developer.  To find out more about these, refer to the Module Developer's Guide.