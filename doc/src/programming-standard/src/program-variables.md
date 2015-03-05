# Program Variables

## Variable Declarations and Usage

One of the guiding philosophies for variable handling in EnergyPlus is that variables are only available and accessible where they are needed.  This is one of the benefits of modules, i.e., that information which is not needed by a particular routine can be hidden from it.  Limiting the scope of variables also makes the code easier to read, maintain, and test.  Another goal of this standard is to minimize the amount of information that must be transferred from one routine to another via a passed variable list and to eliminate the need for commons.  In order to ensure that this goal is realized, several restrictions on the availability of variables must be observed.

All EnergyPlus subroutines and functions must declare "IMPLICIT NONE" near the beginning of each routine or function.  Consequently, all variables must be declared using valid F90 syntax.  In F90, programmer has the ability to define certain variables as either **private** or **public**.  This specification has implications on the availability of a particular variable outside of that module.

Real data in EnergyPlus must be a minimum of 64 bit reals (often called double precision).  To facilitate this, a Data Only module called "DataPrecisionGlobals.f90" is included and should be USEd by each module.  The declaration for reals, is then:

REAL(r64) :: MyReal

And note that comparisons may imply that a single precision is used.  For example, .2 is not the same as .2d0.  So, a comparison should be:

IF (InputValue > .2d0) …

Only four methods of variable definition are allowed in EnergyPlus:

#. *Subroutine Level Variables.*  The first method of defining a variable is at the subroutine level as private.  This definition is consistent with the F77 local variable.  Subroutine level variables are not available anywhere outside the subroutine in which they are declared.  Thus, by definition, any variable declared at the subroutine level is "private".  Local subroutine variables should be the first choice for the method of declaration.
#. *Module Level Variables  --  Private.*  An alternative for defining a variable is private at the module level.  Variables defined in this way are available to all of the subroutines in the module but to no routines outside the module or other modules.  This definition is closely related to the use of common blocks in F77 code except that related data has now been grouped into a particular section of the code.  Declaring a variable as private at the module level avoids the need to pass the variable from one subroutine to another within the same module.  It should be noted, however, that it is inadvisable to declare loop counters or any other variable that could be declared as a subroutine local variable as module level variables.  While it may seem redundant to declare a variable in several subroutines, experience has shown that such an abuse of module level variables is poor programming and will inevitably lead to program bugs.
#. *Data Only Module  --  Public (Superblock).*  In most cases, the prudent selection of module routines will result in the need for only subroutine level and private module-level variables.  However, there will be some circumstances involving either legacy code or complex new code where the variables will need to be accessible in several related modules.  Such related modules are considered a superblock.  Variables that are used in several related modules should be separated and defined in a module that only includes public variable declarations.  Defining module level variables as "public" should only be done when variables must be shared in a significant number of modules in a superblock.  For example, material properties are used in several parts of the heat balance code that may be broken up into separate modules. Elevating variables to public status in data only modules should be a rare exception to the previous two rules and should be done with caution.  Variables declared as public should only be used within modules that are considered part of their superblock (e.g., waste heat should not be used in the heat balance).  Note that variables in modules that have subroutines should not be defined as public.
#. *Data Only Module  --  Public (Global).*  As in almost any other significant program, EnergyPlus will have certain variables that will be utilized in many modules.  When several superblocks require access to a variable, it must be elevated to "global" status.  As a rule, there should be very few global variables, and elevating a variable to global status should only be used as a last resort.  Variables which may end up being global variables include: physical and geometric constants such as , environment information variables (described in a later section) such as time, date, hour, time step, etc., and file unit numbers.  When Data Only modules are used, sometimes it is useful or expedient to include some functions that operate with the variables in these blocks – view DataEnvironment for an example. Likewise, some primarily data modules may include the "get" routines for the data as expedient in complex calling structures.

Another issue related to variable declaration addressed by this standard is the **static** vs. dynamic storage of local subroutine variables.  Due to the large number of legacy code variables that became part of the EnergyPlus program, it was originally necessary to specify the static storage of variables as a compiler flag.  Variables in new and reengineered code should take advantage of dynamic allocation when possible and be explicit about its need to save variable values between subroutine calls. The compiler flag for static "all" is no longer used for the release compiles.

Two features of F90/95 that this standard encourages but does not mandate are intent and derived types.  **Intent** allows the programmer to declare how a passed variable will be used within a subroutine: as in input, an output, or both.  The use of intent is another issue of the clarity of variable usage.  With the intent of a variable explicitly defined, bugs in the program will show up much earlier in the development process and will be easier to detect and eliminate.

**Derived types** offer a convenient way to group and, if necessary, pass related variables.  They are a method for constructing custom data structures within a program.  For example, all of the information about a cooling coil could be defined as derived type.  Consult any F90 book on the merit, advantages, and applications of derived types.  While derived types can be nested (one derived type becomes part of another, etc.), no derived type used with EnergyPlus may be nested more than three layers deep.

One feature of F77 that will be eliminated during the process of reengineering the legacy code is the use of **common blocks** (and the include files which contain them).  While common blocks are allowed in legacy code, their use in reengineered and new code will be limited to any temporary scaffolding that is needed to mesh between the old data system and the improved structure as outlined in this document.  Even with that, they should only be used until the developer is comfortable that the new/revised structure is performing as the old and then the common blocks should be removed.  Ideally, this will happen prior to being included in a version of official EnergyPlus code.

## Units in EnergyPlus

Since the scope of EnergyPlus is limited to building energy analysis and does not include pre- or post-processing interfaces, EnergyPlus will expect information in a single unit system (SI).  Thus, interface developers will be required to convert user inputs from those preferred by architects and engineers into the standard metric units of EnergyPlus.  Thus, EnergyPlus does perform any unit conversions and will not have any unit conversion routines.  Standard internal as well as program input and output units are as follows:

Table: **Standard Units for EnergyPlus.**

Variable Type|Units
-------------|-----
Area|m^2^
Conductivity|W/m-K
Density|kg/m^3^
Energy|J
Enthalpy|J/kg
Heat Content (Fuels)|J/kg
Length|m
Mass Flow|kg/s
Mass|kg
Power|W
Pressure|Pa
R-Value|m^2^-K/W
Specific Heat|J/kg-K
Speed|m/s
Temperature|C
Delta Temperature|C
U-Value|W/ m^2^-K
Volume|m^3^
Volume Flow|m^3^/s

Unless otherwise noted in program comments or in the output file description, all units must conform to the above list.

## Variable Initializations

Consistency in variable initialization has been a concern in most of the legacy code.  The problem existed because the methods of initialization were used interchangeably for different types of initialization.  Consequently, attempts to do parametric runs were limited to separate program launches rather than control of parametric cases within the program.

Several types of initializations might occur in a program:

#. Variable is set to some value and never changes throughout the rest of the program.  These variables should be defined as a parameter when they are declared regardless of whether the variable is a subroutine variable, a module level variable, or a data-only module variable.
#. Variable is set to some value defined by the user and never changes during the run.  These variables should be set in the "Get" routines based on the values read in from the user input file.
#. Variable is set to some initial value and then gets updated one or more times during the run.  In F90, the developer has the opportunity to initialize a variable as part of its declaration statement.  This method is preferred over the data statement because a data statement must be placed somewhere in the code, i.e. in a routine.  Code that is much easier to read will result if these variables are initialized at declaration.
#. Variable is set periodically during the simulation or would need to be reset during subsequent cases of a parametric run.  These variables must be reset within defined initialization subroutines as described in the initialization information provided in the next section, Module Structure and Interaction.
#. Variable is used within a subroutine – IVF compiler does not "pre-initialize" values nor hold those values from one call of the subroutine to the next.  Within the scope of the subroutine, one must always initialize this variable.  IF the value is to hold from one call of the subroutine to the next, it should be designated as a SAVE variable in the declaration.

Notice that block data statements must not be used for any of the initializations -- they are only allowed deprecated features in legacy code.  Using the definitions and methods described above, there is no longer any need for the confusing block data statements.

## Allocation/Deallocation Variable Arrays

A really nice feature of F90 and beyond is the dynamic allocation of arrays. This feature allows EnergyPlus to accept virtually any number of zones, surfaces, etc. without imposing limitations on the user.

However, if you decide to allocate/deallocate with every subroutine call, you are wasting time – allocation and deallocation consumes time.  This will not show up with many of our simple test files but will become painfully obvious to the large simulation with many "widgets" to allocate.

- Allocate during initial phases of the simulation (GetInput, Initialize) for module level variables
- For routine variable arrays, allocate to the maximum once (by using "one time" flags).

## Unused Variables

Unused variables are usually be flagged by compilers.  While some unused variables might be intended for later/future developments, these should be commented out until such time as they are needed.