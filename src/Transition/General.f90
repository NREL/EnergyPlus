MODULE General

  ! Module containing routines for general use

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl, Linda Lawrie
  !       DATE WRITTEN   December 2001
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! contains routines (most likely numeric) that may be needed in several parts
  ! of EnergyPlus

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! This module should not contain variables in the module sense as it is
  ! intended strictly to provide "interfaces" to routines used by other
  ! parts of the simulation.

  ! MODULE PARAMETER DEFINITIONS
 INTEGER, PARAMETER :: r64=KIND(1.0D0)

  ! DERIVED TYPE DEFINITIONS
  ! na

  ! INTERFACE DEFINITIONS
INTERFACE RoundSigDigits
  MODULE PROCEDURE rRoundSigDigits, iRoundSigDigits, rdRoundSigDigits
END INTERFACE RoundSigDigits
INTERFACE TrimSigDigits
  MODULE PROCEDURE rTrimSigDigits, iTrimSigDigits, rdTrimSigDigits
END INTERFACE TrimSigDigits

  ! MODULE VARIABLE DECLARATIONS:
  ! na

  !SUBROUTINE SPECIFICATIONS FOR MODULE General
PUBLIC  TrimSigDigits ! used for better formatting of numeric variables
PUBLIC  RoundSigDigits ! used for better formatting of numeric variables
PUBLIC  SafeDivide

CONTAINS

FUNCTION rTrimSigDigits(RealValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN) :: RealValue
  INTEGER, INTENT(IN) :: SigDigits
  CHARACTER(len=30) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER DotPos               ! Position of decimal point in original string
  INTEGER EPos                 ! Position of E in original string format xxEyy
  INTEGER SLen                 ! Length of String (w/o E part)
  CHARACTER(len=30) String     ! Working string
  CHARACTER(len=10) EString    ! E string retained from original string
  LOGICAL IncludeDot           ! True when decimal point output

  IF (RealValue /= 0.0) THEN
    WRITE(String,*) RealValue
  ELSE
    String='0.000000000000000000000000000'
  ENDIF
  EPos=INDEX(String,'E')
  IF (EPos > 0) THEN
    EString=String(EPos:)
    String(EPos:)=' '
  ELSE
    EString=' '
  ENDIF
  DotPos=INDEX(String,'.')
  SLen=LEN_TRIM(String)
  IF (SigDigits > 0 .or. EString /= ' ') THEN
    IncludeDot=.true.
  ELSE
    IncludeDot=.false.
  ENDIF
  IF (IncludeDot) THEN
    String=String(1:MIN(DotPos+SigDigits,SLen))//EString
  ELSE
    String=String(1:DotPos-1)
  ENDIF
  IF (IsNAN(RealValue)) THEN
    String='NAN'
  ENDIF
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION rTrimSigDigits

FUNCTION rdTrimSigDigits(RealValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: RealValue
  INTEGER, INTENT(IN) :: SigDigits
  CHARACTER(len=30) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER DotPos               ! Position of decimal point in original string
  INTEGER EPos                 ! Position of E in original string format xxEyy
  INTEGER SLen                 ! Length of String (w/o E part)
  CHARACTER(len=30) String     ! Working string
  CHARACTER(len=10) EString    ! E string retained from original string
  LOGICAL IncludeDot           ! True when decimal point output

  IF (RealValue /= 0.0) THEN
    WRITE(String,*) RealValue
  ELSE
    String='0.000000000000000000000000000'
  ENDIF
  EPos=INDEX(String,'E')
  IF (EPos > 0) THEN
    EString=String(EPos:)
    String(EPos:)=' '
  ELSE
    EString=' '
  ENDIF
  DotPos=INDEX(String,'.')
  SLen=LEN_TRIM(String)
  IF (SigDigits > 0 .or. EString /= ' ') THEN
    IncludeDot=.true.
  ELSE
    IncludeDot=.false.
  ENDIF
  IF (IncludeDot) THEN
    String=String(1:MIN(DotPos+SigDigits,SLen))//EString
  ELSE
    String=String(1:DotPos-1)
  ENDIF
  IF (IsNAN(RealValue)) THEN
    String='NAN'
  ENDIF
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION rdTrimSigDigits

FUNCTION iTrimSigDigits(IntegerValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IntegerValue
  INTEGER, INTENT(IN), OPTIONAL :: SigDigits  ! ignored
  CHARACTER(len=30) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=30) String     ! Working string

  WRITE(String,*) IntegerValue
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION iTrimSigDigits

FUNCTION rRoundSigDigits(RealValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN) :: RealValue
  INTEGER, INTENT(IN) :: SigDigits
  CHARACTER(len=30) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=11) :: DigitChar='01234567890'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER DotPos               ! Position of decimal point in original string
  INTEGER EPos                 ! Position of E in original string format xxEyy
  INTEGER TPos                 ! Position of Testchar in Digit string
  INTEGER NPos                 ! Position of "next" char in Digit String
  INTEGER TPos1                ! Position of "next" char rounded in Digit string
  INTEGER SPos                 ! Actual string position being replaced
  INTEGER SLen                 ! Length of String (w/o E part)
  CHARACTER(len=30) String     ! Working string
  CHARACTER(len=10) EString    ! E string retained from original string
  CHARACTER(len=1) TestChar    ! Test character (digit) for rounding, if position in digit string > 5 (digit is 5 or greater)
                               ! then will round
  CHARACTER(len=1) Char2Rep    ! Character (digit) to be replaced
  LOGICAL IncludeDot           ! True when decimal point output

  IF (RealValue /= 0.0) THEN
    WRITE(String,*) RealValue
  ELSE
    String='0.000000000000000000000000000'
  ENDIF
  EPos=INDEX(String,'E')
  IF (EPos > 0) THEN
    EString=String(EPos:)
    String(Epos:)=' '
  ELSE
    EString=' '
  ENDIF

  DotPos=INDEX(String,'.')
  TestChar=String(DotPos+SigDigits+1:DotPos+SigDigits+1)
  TPos=INDEX(DigitChar,TestChar)

  IF (SigDigits == 0) THEN
    SPos=DotPos-1
  ELSE
    SPos=DotPos+SigDigits
  ENDIF

  IF (TPos > 5) THEN  ! Must round to next Digit
    Char2Rep=String(SPos:SPos)
    NPos=INDEX(DigitChar,Char2Rep)
    String(SPos:SPos)=DigitChar(NPos+1:NPos+1)
    DO WHILE (NPos == 10)
        ! Must change other char too
      IF (SigDigits == 1) THEN
        TestChar=String(SPos-2:SPos-2)
        IF (TestChar == '.') THEN
          TestChar=String(SPos-3:SPos-3)
          SPos=SPos-2
        ENDIF
        IF (TestChar == ' ') TestChar='0'  ! all 999s
        TPos1=INDEX(DigitChar,TestChar)
        String(SPos-2:SPos-2)=DigitChar(TPos1+1:TPos1+1)
      ELSE
        TestChar=String(SPos-1:SPos-1)
        IF (TestChar == '.') THEN
          TestChar=String(SPos-2:SPos-2)
          SPos=SPos-1
        ENDIF
        IF (TestChar == ' ') TestChar='0'  ! all 999s
        TPos1=INDEX(DigitChar,TestChar)
        String(SPos-1:SPos-1)=DigitChar(TPos1+1:TPos1+1)
      ENDIF
      SPos=SPos-1
      NPos=TPos1
    ENDDO
  ENDIF

  SLen=LEN_TRIM(String)
  IF (SigDigits > 0 .or. EString /= ' ') THEN
    IncludeDot=.true.
  ELSE
    IncludeDot=.false.
  ENDIF
  IF (IncludeDot) THEN
    String=String(1:MIN(DotPos+SigDigits,SLen))//EString
  ELSE
    String=String(1:DotPos-1)
  ENDIF
  IF (IsNAN(RealValue)) THEN
    String='NAN'
  ENDIF
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION rRoundSigDigits

FUNCTION rdRoundSigDigits(RealValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: RealValue
  INTEGER, INTENT(IN) :: SigDigits
  CHARACTER(len=30) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=11) :: DigitChar='01234567890'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER DotPos               ! Position of decimal point in original string
  INTEGER EPos                 ! Position of E in original string format xxEyy
  INTEGER TPos                 ! Position of Testchar in Digit string
  INTEGER NPos                 ! Position of "next" char in Digit String
  INTEGER TPos1                ! Position of "next" char rounded in Digit string
  INTEGER SPos                 ! Actual string position being replaced
  INTEGER SLen                 ! Length of String (w/o E part)
  CHARACTER(len=30) String     ! Working string
  CHARACTER(len=10) EString    ! E string retained from original string
  CHARACTER(len=1) TestChar    ! Test character (digit) for rounding, if position in digit string > 5 (digit is 5 or greater)
                               ! then will round
  CHARACTER(len=1) Char2Rep    ! Character (digit) to be replaced
  LOGICAL IncludeDot           ! True when decimal point output

  IF (RealValue /= 0.0) THEN
    WRITE(String,*) RealValue
  ELSE
    String='0.000000000000000000000000000'
  ENDIF
  EPos=INDEX(String,'E')
  IF (EPos > 0) THEN
    EString=String(EPos:)
    String(Epos:)=' '
  ELSE
    EString=' '
  ENDIF

  DotPos=INDEX(String,'.')
  TestChar=String(DotPos+SigDigits+1:DotPos+SigDigits+1)
  TPos=INDEX(DigitChar,TestChar)

  IF (SigDigits == 0) THEN
    SPos=DotPos-1
  ELSE
    SPos=DotPos+SigDigits
  ENDIF

  IF (TPos > 5) THEN  ! Must round to next Digit
    Char2Rep=String(SPos:SPos)
    NPos=INDEX(DigitChar,Char2Rep)
    String(SPos:SPos)=DigitChar(NPos+1:NPos+1)
    DO WHILE (NPos == 10)
        ! Must change other char too
      IF (SigDigits == 1) THEN
        TestChar=String(SPos-2:SPos-2)
        IF (TestChar == '.') THEN
          TestChar=String(SPos-3:SPos-3)
          SPos=SPos-2
        ENDIF
        IF (TestChar == ' ') TestChar='0'  ! all 999s
        TPos1=INDEX(DigitChar,TestChar)
        String(SPos-2:SPos-2)=DigitChar(TPos1+1:TPos1+1)
      ELSE
        TestChar=String(SPos-1:SPos-1)
        IF (TestChar == '.') THEN
          TestChar=String(SPos-2:SPos-2)
          SPos=SPos-1
        ENDIF
        IF (TestChar == ' ') TestChar='0'  ! all 999s
        TPos1=INDEX(DigitChar,TestChar)
        String(SPos-1:SPos-1)=DigitChar(TPos1+1:TPos1+1)
      ENDIF
      SPos=SPos-1
      NPos=TPos1
    ENDDO
  ENDIF

  SLen=LEN_TRIM(String)
  IF (SigDigits > 0 .or. EString /= ' ') THEN
    IncludeDot=.true.
  ELSE
    IncludeDot=.false.
  ENDIF
  IF (IncludeDot) THEN
    String=String(1:MIN(DotPos+SigDigits,SLen))//EString
  ELSE
    String=String(1:DotPos-1)
  ENDIF
  IF (IsNAN(RealValue)) THEN
    String='NAN'
  ENDIF
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION rdRoundSigDigits

FUNCTION iRoundSigDigits(IntegerValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IntegerValue
  INTEGER, INTENT(IN), OPTIONAL :: SigDigits  ! ignored
  CHARACTER(len=30) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=30) String     ! Working string

  WRITE(String,*) IntegerValue
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION iRoundSigDigits

FUNCTION SafeDivide(a, b) RESULT (c)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! returns a / b while preventing division by zero
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  REAL(8), INTENT(IN) :: a, b
  REAL(8) :: c

  REAL(8), PARAMETER :: SMALL=1.d-10

  IF (ABS(b) < SMALL) THEN
    c = a / SIGN(SMALL, b)
  ELSE
    c = a / b
  END IF
END FUNCTION

!     NOTICE
!
!     Copyright © 1996-2006 The Board of Trustees of the University of Illinois
!     and The Regents of the University of California through Ernest Orlando Lawrence
!     Berkeley National Laboratory.  All rights reserved.
!
!     Portions of the EnergyPlus software package have been developed and copyrighted
!     by other individuals, companies and institutions.  These portions have been
!     incorporated into the EnergyPlus software package under license.   For a complete
!     list of contributors, see "Notice" located in EnergyPlus.f90.
!
!     NOTICE: The U.S. Government is granted for itself and others acting on its
!     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
!     reproduce, prepare derivative works, and perform publicly and display publicly.
!     Beginning five (5) years after permission to assert copyright is granted,
!     subject to two possible five year renewals, the U.S. Government is granted for
!     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
!     worldwide license in this data to reproduce, prepare derivative works,
!     distribute copies to the public, perform publicly and display publicly, and to
!     permit others to do so.
!
!     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.
!

END MODULE General
