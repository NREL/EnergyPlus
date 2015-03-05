# Appendix A: Definitions and Notation

The following definitions will be applicable for this document:

- EnergyPlus  --  The name of the program chosen by team members to represent the best pieces of the DOE-2 and (I)BLAST programs.  EnergyPlus as defined by the team members is only responsible for performing building energy analysis.  It will not serve either as a user interface or as an output processor.  This will have some bearing on the topics detailed in this standard.
- Legacy Code  --  Program code from (I)BLAST and DOE-2 which will not be revised (no algorithm changes) for reasons of time constraints, testing considerations, etc.
- Reverse Engineering  --  The process of determining the data flow and algorithms used for various program codes.
- Reengineered Code  --  Code which has been reverse engineered and then modified to fit the proposed guidelines agreed upon by the team members.  The starting point for reengineered code is code from either (I)BLAST or DOE-2.
- New Code  --  Code which has been written from scratch, i.e., completely new code.
- Superblock  --  A grouping of modules with a common purpose.  All of the heat balance modules would be considered part of the heat balance superblock; system modules would be part of a system superblock, etc.
- Fortran 90/95 or F90/95  --  This refers to the full ANSI Fortran 90 language as defined in the American National Standard Programming Language Fortran 90, ANSI X3.198-1992 and International Standards Organization Programming Language Fortran, ISO/IEC 1539:1991(E).  F95 is a slightly revised standard – a bit later than F90.  F2000 and F2003 is a later standard than F95 and contains some substantial revisions.
- Fortran 90 Strict  --  Strict means that the code adheres to at least the Fortran 77 standard and includes new features of Fortran 90.
- Fortran 90 Pure  --  Pure means that the code does not contain any of the features that have been ruled obsolete by the Fortran 90 standard.
- Verb-Predicate Rule  --  A method for naming subroutines consistently and logically based on the functionality of the routine.  Every subroutine performs some action (the "verb") on a particular item or data set (the "predicate").  The subroutine name is thus constructed using the verb-predicate combination to arrive at a unique name for a particular algorithm.
- ANSI – American National Standards Institute, keeper of Fortran language standards.
- ISO – International Standards Organization, keeper of Fortran language standards since F90.