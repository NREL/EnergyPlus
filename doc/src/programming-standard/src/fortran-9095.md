# Fortran 90/95

Because of the power, modular structure, and other strengths as a computational language, we chose Fortran90/95 as the language of choice for EnergyPlus.  All of the guidelines presented in this section ("Coding Standard") are based on the use of Fortran for all code in EnergyPlus.  The information in this section is not intended to be a complete description of the Fortran programming language but rather a supplement to the ANSI Standard. As we change compilers to later standards such as  Fortran 2000/2003, we evolve code as appropriate.  For those not familiar with Fortran90, the following books have proved useful to the development team:

- Ellis, T.M.R., Ivor R. Phillips, Thomas M. Lahey, 1994. *Fortran 90 Programming*, New York: Addison-Wesley.
- Redwine, Cooper. 1995. *Upgrading to Fortran 90.* New York: Springer Verlag.
- Chapman, Steven, McGraw Hill, 1998. *Fortran 90/95 for Scientists and Engineers*. New York: McGraw Hill.
- Metcalf, Michael, John Reid, Malcolm Cohen, 2004. *fortran 95/2003 explained.* New York: Oxford University Press.

However, there are many other Fortran 90/95 texts available. A useful book about software design is:

- McConnell, Steve C. 1993. *Code Complete: A Practical Handbook of Software Construction*, Redmond, Washington: Microsoft Press.

## Fortran 90 Code

For the EnergyPlus project, three types of code that may coexist in any version of the program:

- Legacy Code
- Reengineered Code
- New Code

While these types of code will coexist in the EnergyPlus source, different expectations on the relative "purity" of the code will be enforced.  All legacy code that is included in EnergyPlus must be at least F90 strict.  (Note that legacy code included from another source may need permission/granting clauses to be place in EnergyPlus – see the Module Developer's Guide for more on this). Mildly reengineered code (near legacy) that has not undergone any algorithm changes (only inclusion in a module, renaming of variables, etc.) will be allowed as long as it conforms to the F90 strict test.  Reengineered code that has been modified significantly and all new code will be required to conform to the F90/95 pure standard.

All code should be placed in "free-format" as opposed to the fixed format used by F77 and other versions of Fortran.  (Legacy code may be converted from fixed to free format using a utility available from team members.)  In addition, the following guidelines should be followed for all free-formatted code in EnergyPlus:

- Only ! is valid for indicating comments.
- In-line comments are allowed and encouraged.

Guidelines from Code Complete should be followed for inline comments.  Several suggestions are repeated here:

- *Avoid* endline (inline) comments that merely repeat the code.
- *Avoid* endline comments for multiple lines of code.  In other words, avoid using a comment at the end of one line that applies to several lines of code.
- *Use* endline comments to annotate data declarations.
- *Use* endline comments for maintenance notes (bug fixes, for example).
- *Use* endline comments to mark ends of blocks.

- Lines must not extend past 130 characters.  (Absolute limit is 132 characters!)
- We recommend that you limit most lines to 80 characters for readability.  This allows most code to be seen on a standard size screen and be printed without resorting to micro-fonts or landscape mode.
- Column 6 is not associated with a continuation line in free format.  To continue one line onto the next line, place an ampersand (&) at the end of the line to be continued.
- The main program should begin with a PROGRAM statement.
- To help visually distinguish between F90 syntax and other code elements such as comments and names, F90 syntax should be in all capital letters while other elements should be mixed case.
- Tab characters are not allowed in any source code file.  If the file editor allows tab characters, be certain to set it up so that the tab characters are converted to spaces upon saving the file.
- It is highly recommended that programmers not use generic loop counters such as i, j, k, etc.  This simply adds to the complexity of the code.  The source code will be much easier to read if programmers use logical names for loop counting such as SurfaceNum, SystemNum, IterationNum, etc.
- Though the F90/95 standard allows for multiple statements on a line, *do not use these.*

~~~~~~~~~~~~~~~~~~~~

    E.g. a=0; b=0
    instead  use:
    a=0
    b=0
~~~~~~~~~~~~~~~~~~~~

- Likewise while F90/F95 allows multiple initialization statements on a line, *do not use these*.

~~~~~~~~~~~~~~~~~~~~

    E.g. Integer, Save :: Var1=0, Var2=0
    Instead use
    Integer, Save :: Var1=0   ! should also have commenting on what var1 is
    Integer, Save :: Var2=0   ! Var2 definition
~~~~~~~~~~~~~~~~~~~~

- We require that you take advantage of the F95 standard for default initialization of derived type elements.  The following example illustrates this.  When CompData type is allocated (usually these are arrays), it is automatically filled with the indicated defaults.

~~~~~~~~~~~~~~~~~~~~

    TYPE CompData
      CHARACTER(len=MaxNameLength)   :: TypeOf=' '  ! identifying  component type
      CHARACTER(len=MaxNameLength)   :: Name=' '      !Component name
      INTEGER                        :: CompNum=0     !component ID number
      CHARACTER(len=MaxNameLength)   :: FlowCtrl=' '  !Component flow control
                                                   ! (ACTIVE/PASSIVE/BYPASS)
      LOGICAL                        :: ON=.false.       !When true, the
                   !    designated component or operation scheme is available
      CHARACTER(len=MaxNameLength)   :: NodeNameIn=' '  !Component inlet node
                                                        !  name
      CHARACTER(len=MaxNameLength)   :: NodeNameOut=' '   !Component outlet node
                                                          ! name
      INTEGER                        :: NodeNumIn=0     !Component inlet node
                                                        ! number
      INTEGER                        :: NodeNumOut=0    !Component outlet node
                                                        ! number
      REAL                           :: MyLoad=0.0        !Distributed Load
      REAL                           :: MaxLoad=0.0       !Maximum load
      REAL                           :: MinLoad=0.0       !Minimum Load
      REAL                           :: OptLoad=0.0       !Optimal Load
    END TYPE CompData
~~~~~~~~~~~~~~~~~~~~

Other F90 syntax that are either allowed or prohibited in various types of EnergyPlus code are listed below:

- EQUIVALENCE statements are **not** allowed in any code except legacy code.  Since EQUIVALENCE is a deprecated feature of F90, as many equivalence statements as possible should be eliminated during the reengineering process.  New code should not use EQUIVALENCE statements.  If you have code with EQUIVALENCE statements, the EnergyPlus development team will help you transition to newer forms.
- Do not use COMMON blocks and INCLUDE files, instead replace these with module level variables or data only modules (where allowed).  Commons and includes will be allowed in legacy and reengineered code until sufficient reengineering has taken place in that section of code to make other methods of variable declarations more appropriate.  Commons and include files are **not** allowed in new code except where necessary for interfacing with legacy or mildly reengineered code.
- Legacy code should be gone at this point.  One only uses legacy forms to make sure that transition to the F90/F95/F2003 has gone correctly.
- Avoid GOTO statements in both reengineered and new code.
- Do **not** use platform/machine specific features of extensions to F90 in any code.

## F90/95 Language Features for Use in EnergyPlus

There are numerous features in F90 that provide advantages to EnergyPlus programmers.  The following program elements must be used for all reengineered and new code:

- Modules
- USE statements (allows interaction of modules)

The following F90/95 features should be considered for use with all reengineered and new code in EnergyPlus:

- Derived types

- Variables in Derived Types must be "initialized" so that when allocated or originally presented, these values will be automatically assumed in the structure.

- Variable intent
- Interface blocks

## Fortran 90/95 Compilers

- The EnergyPlus development team uses Windows OS machines for most development.  Linux is used for some testing as well as creating the Linux install version. The primary compiler being used is Intel Visual Fortran (IVF), which operates within Microsoft Visual Studio. Other compilers such Lahey F95 and the open source g-95 is used in portability tests.  At times, the results from other compilers is compared to the IVF results. For detailed compiler switches for Intel Fortran, please contact the development team.

> It should also be noted that since tab characters are not allowed in EnergyPlus but are valid in SOME COMPILERS that all developers should configure ANY EDITOR to replace tabs with spaces.

## Beyond F90/95 Language Features for Use in EnergyPlus

The original programming standard was written before F95 had been adopted.  Though we don't yet (2007) have fully compliant compilers for F2000+, we are beginning to use these features. Notably:

- Allocatable structures in Derived Types -- if one has a variable length array that belongs to part of a Derived Type structure, F90 "allowed" these to be pointers.  This is not, in general, a stellar idea and since F2003 allows these to be "allocatable", EnergyPlus code should reflect that convention.
- Standard methods of determining command line arguments and defined environment variables. F2003 defines standard routines GET_COMMAND_ARGUMENT and GET_ENVIRONMENT_VARIABLE.