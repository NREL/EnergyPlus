module VCompareGlobalRoutines
USE DataGlobals, ONLY: MaxNameLength, ShowFatalError
USE DataVCompareGlobals
USE DataStringGlobals, ONLY: PrognameConversion
PUBLIC

CONTAINS

FUNCTION AddFieldNameToLine(Line,LString,FieldName) RESULT(LineOut)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function adds the field name to the output string.  (At "column" 30 or nearest after that
          ! as possible).  Helps to create a "regular" looking output file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Line
  CHARACTER(len=*), INTENT(IN) :: LString
  CHARACTER(len=*), INTENT(IN) :: FieldName
  CHARACTER(len=LEN(Line))     :: LineOut

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (LEN_TRIM(Line) > 29) THEN
    LineOut=TRIM(Line)//LString//TRIM(FieldName)
  ELSE
    LineOut=TRIM(Line)
    LineOut(30:)=TRIM(ADJUSTL(LString))//' '//TRIM(FieldName)
  ENDIF

  RETURN

END FUNCTION AddFieldNameToLine

FUNCTION AddUnitsToLine(Line,UnitsArgChr) RESULT(LineOut)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function adds the Units to the almost filled line.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Line
  CHARACTER(len=*), INTENT(IN) :: UnitsArgChr
  CHARACTER(len=LEN(Line))     :: LineOut

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  LineOut=TRIM(Line)//' {'//TRIM(UnitsArgChr)//'}'

  RETURN

END FUNCTION AddUnitsToLine

SUBROUTINE WriteOutIDFLines(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes out one IDF object to the indicated LFN.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: DifUnit
  CHARACTER(len=*), INTENT(IN) :: ObjectName
  INTEGER, INTENT(IN) :: CurArgs
  CHARACTER(len=*), DIMENSION(:) :: OutArgs
  CHARACTER(len=*), DIMENSION(:) :: FieldNames
  CHARACTER(len=*), DIMENSION(:) :: FieldUnits

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=2), PARAMETER :: CommaString=', '
  CHARACTER(len=2), PARAMETER :: SemiString='; '
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=2) LString1
    CHARACTER(len=5) :: LString2='  !- '
    INTEGER Arg
    CHARACTER(len=500) :: LineOut
    INTEGER :: MaxSize

    MaxSize=SIZE(FieldNames)
    WRITE(DifUnit,fmta) '  '//TRIM(ObjectName)//','
    DO Arg=1,CurArgs
      IF (Arg /= CurArgs) THEN
        LString1=CommaString
      ELSE
        LString1=SemiString
      ENDIF
      LineOut='    '//TRIM(OutArgs(Arg))//LString1
      IF (Arg <= MaxSize) THEN
        LineOut=AddFieldNameToLine(LineOut,LString2,FieldNames(Arg))
        IF (withUnits .and. FieldUnits(Arg) /= Blank) THEN
          LineOut=AddUnitsToLine(LineOut,FieldUnits(Arg))
        ENDIF
      ELSE
        LineOut=AddFieldNameToLine(LineOut,LString2,'Extended Field')
      ENDIF
      Write(DifUnit,fmta) TRIM(LineOut)
    ENDDO
    WRITE(DifUnit,fmta) ' '

  RETURN

END SUBROUTINE WriteOutIDFLines

SUBROUTINE WriteOutIDFLinesAsSingleLine(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes out one IDF object to the indicated LFN.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: DifUnit
  CHARACTER(len=*), INTENT(IN) :: ObjectName
  INTEGER, INTENT(IN) :: CurArgs
  CHARACTER(len=*), DIMENSION(:) :: OutArgs
  CHARACTER(len=*), DIMENSION(:), OPTIONAL :: FieldNames
  CHARACTER(len=*), DIMENSION(:), OPTIONAL :: FieldUnits

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=2), PARAMETER :: CommaString=', '
  CHARACTER(len=2), PARAMETER :: SemiString='; '
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=2) LString1
    CHARACTER(len=5) :: LString2='  !- '
    INTEGER Arg
    CHARACTER(len=500) :: LineOut
    INTEGER :: MaxSize

    MaxSize=SIZE(FieldNames)
    WRITE(DifUnit,fmta,ADVANCE='NO') '  '//TRIM(ObjectName)//','
    DO Arg=1,CurArgs
      IF (Arg /= CurArgs) THEN
        LString1=CommaString
      ELSE
        LString1=SemiString
      ENDIF
      LineOut=TRIM(OutArgs(Arg))//LString1
      IF (Arg /= CurArgs) THEN
        Write(DifUnit,fmta,ADVANCE='NO') TRIM(LineOut)
      ELSE
        Write(DifUnit,fmta) TRIM(LineOut)
      ENDIF
    ENDDO
    WRITE(DifUnit,fmta) ' '

  RETURN

END SUBROUTINE WriteOutIDFLinesAsSingleLine

SUBROUTINE WriteOutPartialIDFLines(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes out part of one IDF object to the indicated LFN.
          ! (no terminating ;) (no terminating blank line)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: DifUnit
  CHARACTER(len=*), INTENT(IN) :: ObjectName
  INTEGER, INTENT(IN) :: CurArgs
  CHARACTER(len=*), DIMENSION(:) :: OutArgs
  CHARACTER(len=*), DIMENSION(:) :: FieldNames
  CHARACTER(len=*), DIMENSION(:) :: FieldUnits

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=2), PARAMETER :: CommaString=', '
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=2) LString1
    CHARACTER(len=5) :: LString2='  !- '
    INTEGER Arg
    CHARACTER(len=500) :: LineOut
    INTEGER :: MaxSize

    MaxSize=SIZE(FieldNames)
    WRITE(DifUnit,fmta) '  '//TRIM(ObjectName)//','
    DO Arg=1,CurArgs
      LString1=CommaString
      LineOut='    '//TRIM(OutArgs(Arg))//LString1
      IF (Arg <= MaxSize) THEN
        LineOut=AddFieldNameToLine(LineOut,LString2,FieldNames(Arg))
        IF (withUnits .and. FieldUnits(Arg) /= Blank) THEN
          LineOut=AddUnitsToLine(LineOut,FieldUnits(Arg))
        ENDIF
      ELSE
        LineOut=AddFieldNameToLine(LineOut,LString2,'Extended Field')
      ENDIF
      Write(DifUnit,fmta) TRIM(LineOut)
    ENDDO

  RETURN

END SUBROUTINE WriteOutPartialIDFLines

SUBROUTINE WriteOutIDFLinesAsComments(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes out one IDF object to the indicated LFN.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: DifUnit
  CHARACTER(len=*), INTENT(IN) :: ObjectName
  INTEGER, INTENT(IN) :: CurArgs
  CHARACTER(len=*), DIMENSION(:) :: OutArgs
  CHARACTER(len=*), DIMENSION(:) :: FieldNames
  CHARACTER(len=*), DIMENSION(:) :: FieldUnits

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=2), PARAMETER :: CommaString=', '
  CHARACTER(len=2), PARAMETER :: SemiString='; '
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=2) LString1
    CHARACTER(len=5) :: LString2='  !- '
    INTEGER Arg
    CHARACTER(len=500) :: LineOut
    INTEGER :: MaxSize

    MaxSize=SIZE(FieldNames)
    WRITE(DifUnit,fmta) '!  '//TRIM(ObjectName)//','
    DO Arg=1,CurArgs
      IF (Arg /= CurArgs) THEN
        LString1=CommaString
      ELSE
        LString1=SemiString
      ENDIF
      LineOut='!    '//TRIM(OutArgs(Arg))//LString1
      IF (Arg <= MaxSize) THEN
        LineOut=AddFieldNameToLine(LineOut,LString2,FieldNames(Arg))
        IF (withUnits .and. FieldUnits(Arg) /= Blank) THEN
          LineOut=AddUnitsToLine(LineOut,FieldUnits(Arg))
        ENDIF
      ELSE
        LineOut=AddFieldNameToLine(LineOut,LString2,'Extended Field')
      ENDIF
      Write(DifUnit,fmta) TRIM(LineOut)
    ENDDO
    WRITE(DifUnit,fmta) ' '

  RETURN

END SUBROUTINE WriteOutIDFLinesAsComments

SUBROUTINE CheckSpecialObjects(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits,Written)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes out some special objects (possibly).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUPPERCase,SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: DifUnit
  CHARACTER(len=*), INTENT(IN) :: ObjectName
  INTEGER, INTENT(IN) :: CurArgs
  CHARACTER(len=*), DIMENSION(:) :: OutArgs
  CHARACTER(len=*), DIMENSION(:) :: FieldNames
  CHARACTER(len=*), DIMENSION(:) :: FieldUnits
  LOGICAL, INTENT(Out) :: Written

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=2), PARAMETER :: CommaString=', '
  CHARACTER(len=2), PARAMETER :: SemiString='; '
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  CHARACTER(len=*), PARAMETER :: Blank=' '
  CHARACTER(len=*), PARAMETER :: VertexString='X,Y,Z ==> Vertex'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=6) LString
    INTEGER Arg
    CHARACTER(len=500) :: LineOut
    INTEGER VArg
    INTEGER VArgS
    INTEGER VArgE
    CHARACTER(len=MaxNameLength) UCRepVarName
    INTEGER pos
    INTEGER pos2
    INTEGER NVert
    CHARACTER(len=4) :: VString=' '
    INTEGER iCurArgs
    LOGICAL compactwarning

    Written=.true.
    compactwarning=.false.

    IF (VersionNum < 3.0) THEN
      SELECT CASE (MakeUPPERCase(ObjectName))

        !  Changes for readability
        CASE ('BUILDING')
          IF (OutArgs(3) == '1') THEN
            OutArgs(3)='Country'
          ENDIF
          IF (OutArgs(3) == '2') THEN
            OutArgs(3)='Suburbs'
          ENDIF
          IF (OutArgs(3) == '3') THEN
            OutArgs(3)='City'
          ENDIF
          IF (OutArgs(6) == '-1') THEN
            OutArgs(6)='MinimalShadowing'
          ENDIF
          IF (OutArgs(6) == '0') THEN
            OutArgs(6)='FullExterior'
          ENDIF
          IF (OutArgs(6) == '1') THEN
            OutArgs(6)='FullInteriorAndExterior'
          ENDIF
          iCurArgs=CurArgs
          IF (iCurArgs == 8) THEN
            IF (MakeUPPERCase(OutArgs(8)) == 'YES') THEN
              OutArgs(6)=TRIM(OutArgs(6))//'WithReflections'
              OutArgs(8)=Blank
              iCurArgs=7
            ELSEIF (MakeUPPERCase(OutArgs(8)) == 'NO') THEN
              OutArgs(8)=Blank
              iCurArgs=7
            ENDIF
          ENDIF
          CALL WriteOutIDFLines(DifUnit,ObjectName,iCurArgs,OutArgs,FieldNames,FieldUnits)

        CASE ('SOLUTION ALGORITHM')
          IF (OutArgs(1) == '0') THEN
            OutArgs(1)='CTF'
          ENDIF
          IF (MakeUPPERCase(OutArgs(1)) == 'DEFAULT') THEN
            OutArgs(1)='CTF'
          ENDIF
          IF (OutArgs(1) == '2') THEN
            OutArgs(1)='EMPD'
          ENDIF
          IF (OutArgs(1) == '3') THEN
            OutArgs(1)='MTF'
          ENDIF
          CALL WriteOutIDFLinesAsSingleLine(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits)

        CASE ('OUTSIDE CONVECTION ALGORITHM')
          IF (OutArgs(1) == '0') THEN
            OutArgs(1)='Simple'
          ENDIF
          IF (OutArgs(1) == '1') THEN
            OutArgs(1)='Detailed'
          ENDIF
          CALL WriteOutIDFLinesAsSingleLine(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits)

        CASE ('INSIDE CONVECTION ALGORITHM')
          IF (OutArgs(1) == '0') THEN
            OutArgs(1)='Simple'
          ENDIF
          IF (OutArgs(1) == '1') THEN
            OutArgs(1)='Detailed'
          ENDIF
          CALL WriteOutIDFLinesAsSingleLine(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits)

        CASE ('REPORT VARIABLE')
          IF (OutArgs(1) == Blank) THEN
            OutArgs(1)='*'
          ENDIF
          CALL WriteOutIDFLinesAsSingleLine(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits)

        CASE('SURFACE:HEATTRANSFER','SURFACE:HEATTRANSFER:SUB')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,10,OutArgs,FieldNames,FieldUnits)
          IF (MakeUPPERCase(OutArgs(10)) == 'AUTOCALCULATE') THEN
            NVert=(CurArgs-10)/3
          ELSEIF (OutArgs(10) == '') THEN
            NVert=(CurArgs-10)/3
          ELSE
            READ(OutArgs(10),*) NVert
          ENDIF
          VArg=11
          DO Arg=1,NVert
            IF (Arg /= NVert) THEN
              LString=',  !- '
            ELSE
              LString=';  !- '
            ENDIF
            WRITE(VString,'(I4)') Arg
            VString=ADJUSTL(VString)
            IF (withUnits .and. FieldUnits(VArg) /= Blank) THEN
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)//' {'//TRIM(FieldUnits(VArg))//'}'
            ELSE
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)
            ENDIF
            VArg=VArg+3
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('SURFACE:SHADING:DETACHED','SURFACE:SHADING:DETACHED:FIXED','SURFACE:SHADING:DETACHED:BUILDING')
          IF (ObjectName == 'SURFACE:SHADING:DETACHED') THEN
            CALL WriteOutPartialIDFLines(DifUnit,'SURFACE:SHADING:DETACHED:FIXED',3,OutArgs,FieldNames,FieldUnits)
          ELSE
            CALL WriteOutPartialIDFLines(DifUnit,ObjectName,3,OutArgs,FieldNames,FieldUnits)
          ENDIF
          IF (MakeUPPERCase(OutArgs(3)) == 'AUTOCALCULATE') THEN
            NVert=(CurArgs-3)/3
          ELSEIF (OutArgs(3) == '') THEN
            NVert=(CurArgs-3)/3
          ELSE
            READ(OutArgs(3),*) NVert
          ENDIF
          VArg=4
          DO Arg=1,NVert
            IF (Arg /= NVert) THEN
              LString=',  !- '
            ELSE
              LString=';  !- '
            ENDIF
            WRITE(VString,'(I4)') Arg
            VString=ADJUSTL(VString)
            IF (withUnits .and. FieldUnits(VArg) /= Blank) THEN
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)//' {'//TRIM(FieldUnits(VArg))//'}'
            ELSE
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)
            ENDIF
            VArg=VArg+3
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('SURFACE:SHADING:ATTACHED')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,4,OutArgs,FieldNames,FieldUnits)
          IF (MakeUPPERCase(OutArgs(4)) == 'AUTOCALCULATE') THEN
            NVert=(CurArgs-4)/3
          ELSEIF (OutArgs(4) == '') THEN
            NVert=(CurArgs-4)/3
          ELSE
            READ(OutArgs(4),*) NVert
          ENDIF
          VArg=5
          DO Arg=1,NVert
            IF (Arg /= NVert) THEN
              LString=',  !- '
            ELSE
              LString=';  !- '
            ENDIF
            WRITE(VString,'(I4)') Arg
            VString=ADJUSTL(VString)
            IF (withUnits .and. FieldUnits(VArg) /= Blank) THEN
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)//' {'//TRIM(FieldUnits(VArg))//'}'
            ELSE
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)
            ENDIF
            VArg=VArg+3
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('WINDOWGLASSSPECTRALDATA')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,1,OutArgs,FieldNames,FieldUnits)
          DO Arg=2,CurArgs,4
            VargS=Arg
            VargE=Arg+3
            IF (VargS+3 > CurArgs) VargE=CurArgs
            IF (VArgE /= CurArgs) THEN
              LString=','
            ELSE
              LString=';'
            ENDIF
            LineOut='    '//TRIM(OutArgs(VArgS))
            DO VArg=VargS+1,VargE
              LineOut=TRIM(LineOut)//','//OutArgs(Varg)
            ENDDO
            LineOut=TRIM(LineOut)//LString
            WRITE(DifUnit,fmta) TRIM(LineOut)
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('FLUIDPROPERTYTEMPERATURES')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,1,OutArgs,FieldNames,FieldUnits)
          DO Arg=2,CurArgs,7
            VargS=Arg
            VargE=Arg+6
            IF (VargS+6 > CurArgs) VargE=CurArgs
            IF (VArgE /= CurArgs) THEN
              LString=','
            ELSE
              LString=';'
            ENDIF
            LineOut='    '//TRIM(OutArgs(VArgS))
            DO VArg=VargS+1,VargE
              LineOut=TRIM(LineOut)//','//OutArgs(Varg)
            ENDDO
            LineOut=TRIM(LineOut)//LString
            WRITE(DifUnit,fmta) TRIM(LineOut)
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('FLUIDPROPERTYSATURATED','FLUIDPROPERTYSUPERHEATED','FLUIDPROPERTYCONCENTRATION')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,4,OutArgs,FieldNames,FieldUnits)
          DO Arg=5,CurArgs,7
            VargS=Arg
            VargE=Arg+6
            IF (VargS+6 > CurArgs) VargE=CurArgs
            IF (VArgE /= CurArgs) THEN
              LString=','
            ELSE
              LString=';'
            ENDIF
            LineOut='    '//TRIM(OutArgs(VArgS))
            DO VArg=VargS+1,VargE
              LineOut=TRIM(LineOut)//','//OutArgs(Varg)
            ENDDO
            LineOut=TRIM(LineOut)//LString
            WRITE(DifUnit,fmta) TRIM(LineOut)
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE DEFAULT
          Written=.false.

      END SELECT

    ELSE
      SELECT CASE (MakeUPPERCase(ObjectName))

        ! Special single line formats

        CASE ('VERSION','SURFACECONVECTIONALGORITHM:INSIDE','SURFACECONVECTIONALGORITHM:OUTSIDE',  &
           'HEATBALANCEALGORITHM','ZONECAPACITANCEMULTIPLIER','TIMESTEP','SITE:GROUNDTEMPERATURE:BUILDINGSURFACE',  &
           'SITE:GROUNDTEMPERATURE:FCFACTORMETHOD','SITE:GROUNDTEMPERATURE:SHALLOW','SITE:GROUNDTEMPERATURE:DEEP',  &
           'SITE:GROUNDREFLECTANCE','OUTPUT:METER','OUTPUT:METER:METERFILEONLY','OUTPUT:METER:CUMULATIVE',  &
           'OUTPUT:METER:CUMULATIVE:METERFILEONLY','OUTPUT:DEBUGGINGDATA','OUTPUT:VARIABLEDICTIONARY','OUTPUT:SURFACES:LIST',  &
           'OUTPUT:SURFACES:DRAWING','OUTPUT:SCHEDULES','OUTPUT:CONSTRUCTIONS','SCHEDULE:CONSTANT')
          CALL WriteOutIDFLinesAsSingleLine(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits)

        CASE ('SCHEDULE:COMPACT')
          compactwarning=.false.
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,2,OutArgs,FieldNames,FieldUnits)
          Arg=3
          DO WHILE (Arg <= CurArgs)
            IF (SameString(OutArgs(Arg)(1:5),'Until')) THEN
              IF (Arg+1 /= CurArgs) THEN
                LString=','
              ELSE
                LString=';'
              ENDIF
              IF (Arg == CurArgs) THEN
                LString=';'  ! may throw error, is probably same workings as before
                ! need to write out error message?  Output:preprocessor?
                compactwarning=.true.
              ENDIF
              LineOut='    '//trim(OutArgs(Arg))//','//trim(OutArgs(Arg+1))//LString
              IF (Len_Trim(LineOut) > 29) THEN
                WRITE(DifUnit,fmta) TRIM(LineOut)//' !- '//trim(FieldNames(Arg))
              ELSE
                LineOut(30:)='!- '//Trim(FieldNames(Arg))
                WRITE(DifUnit,fmta) Trim(LineOut)
              ENDIF
              Arg=Arg+2
            ELSE
              IF (Arg /= CurArgs) THEN
                LString=','
              ELSE
                LString=';'
              ENDIF
              LineOut='    '//trim(OutArgs(Arg))//LString
              IF (Len_Trim(LineOut) > 29) THEN
                WRITE(DifUnit,fmta) TRIM(LineOut)//' !- '//trim(FieldNames(Arg))
              ELSE
                LineOut(30:)='!- '//Trim(FieldNames(Arg))
                WRITE(DifUnit,fmta) Trim(LineOut)
              ENDIF
              Arg=Arg+1
            ENDIF
          ENDDO
          WRITE(DifUnit,fmta) ' '
          if (compactwarning) then
            CALL writePreProcessorObject(DifUnit,ProgNameConversion,'Warning',  &
             'Compact Schedule object="'//trim(OutArgs(1))//'" terminated early.  Check for accuracy.')
          endif

        !  Changes for readability
        CASE ('OUTPUT:VARIABLE')
          IF (OutArgs(1) == Blank) THEN
            OutArgs(1)='*'
          ENDIF
          CALL WriteOutIDFLinesAsSingleLine(DifUnit,ObjectName,CurArgs,OutArgs,FieldNames,FieldUnits)

        CASE('BUILDINGSURFACE:DETAILED','FENESTRATIONSURFACE:DETAILED')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,10,OutArgs,FieldNames,FieldUnits)
          IF (MakeUPPERCase(OutArgs(10)) == 'AUTOCALCULATE') THEN
            NVert=(CurArgs-10)/3
          ELSEIF (OutArgs(10) == '') THEN
            NVert=(CurArgs-10)/3
          ELSE
            READ(OutArgs(10),*) NVert
          ENDIF
          VArg=11
          DO Arg=1,NVert
            IF (Arg /= NVert) THEN
              LString=',  !- '
            ELSE
              LString=';  !- '
            ENDIF
            WRITE(VString,'(I4)') Arg
            VString=ADJUSTL(VString)
            IF (withUnits .and. FieldUnits(VArg) /= Blank) THEN
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)//' {'//TRIM(FieldUnits(VArg))//'}'
            ELSE
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)
            ENDIF
            VArg=VArg+3
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('WALL:DETAILED','ROOFCEILING:DETAILED','FLOOR:DETAILED')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,9,OutArgs,FieldNames,FieldUnits)
          IF (MakeUPPERCase(OutArgs(9)) == 'AUTOCALCULATE') THEN
            NVert=(CurArgs-9)/3
          ELSEIF (OutArgs(9) == '') THEN
            NVert=(CurArgs-9)/3
          ELSE
            READ(OutArgs(9),*) NVert
          ENDIF
          VArg=10
          DO Arg=1,NVert
            IF (Arg /= NVert) THEN
              LString=',  !- '
            ELSE
              LString=';  !- '
            ENDIF
            WRITE(VString,'(I4)') Arg
            VString=ADJUSTL(VString)
            IF (withUnits .and. FieldUnits(VArg) /= Blank) THEN
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)//' {'//TRIM(FieldUnits(VArg))//'}'
            ELSE
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)
            ENDIF
            VArg=VArg+3
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('SHADING:SITE:DETAILED','SHADING:BUILDING:DETAILED')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,3,OutArgs,FieldNames,FieldUnits)
          IF (MakeUPPERCase(OutArgs(3)) == 'AUTOCALCULATE') THEN
            NVert=(CurArgs-3)/3
          ELSEIF (OutArgs(3) == '') THEN
            NVert=(CurArgs-3)/3
          ELSE
            READ(OutArgs(3),*) NVert
          ENDIF
          VArg=4
          DO Arg=1,NVert
            IF (Arg /= NVert) THEN
              LString=',  !- '
            ELSE
              LString=';  !- '
            ENDIF
            WRITE(VString,'(I4)') Arg
            VString=ADJUSTL(VString)
            IF (withUnits .and. FieldUnits(VArg) /= Blank) THEN
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)//' {'//TRIM(FieldUnits(VArg))//'}'
            ELSE
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)
            ENDIF
            VArg=VArg+3
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('SHADING:ZONE:DETAILED')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,4,OutArgs,FieldNames,FieldUnits)
          IF (MakeUPPERCase(OutArgs(4)) == 'AUTOCALCULATE') THEN
            NVert=(CurArgs-4)/3
          ELSEIF (OutArgs(3) == '') THEN
            NVert=(CurArgs-4)/3
          ELSE
            READ(OutArgs(4),*) NVert
          ENDIF
          VArg=5
          DO Arg=1,NVert
            IF (Arg /= NVert) THEN
              LString=',  !- '
            ELSE
              LString=';  !- '
            ENDIF
            WRITE(VString,'(I4)') Arg
            VString=ADJUSTL(VString)
            IF (withUnits .and. FieldUnits(VArg) /= Blank) THEN
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)//' {'//TRIM(FieldUnits(VArg))//'}'
            ELSE
              WRITE(DifUnit,fmta) '    '//TRIM(OutArgs(VArg))//','//TRIM(OutArgs(VArg+1))//','//TRIM(OutArgs(VArg+2))//  &
                                       LString//TRIM(VertexString)//' '//TRIM(VString)
            ENDIF
            VArg=VArg+3
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('MATERIALPROPERTY:GLAZINGSPECTRALDATA')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,1,OutArgs,FieldNames,FieldUnits)
          DO Arg=2,CurArgs,4
            VargS=Arg
            VargE=Arg+3
            IF (VargS+3 > CurArgs) VargE=CurArgs
            IF (VArgE /= CurArgs) THEN
              LString=','
            ELSE
              LString=';'
            ENDIF
            LineOut='    '//TRIM(OutArgs(VArgS))
            DO VArg=VargS+1,VargE
              LineOut=TRIM(LineOut)//','//OutArgs(Varg)
            ENDDO
            LineOut=TRIM(LineOut)//LString
            WRITE(DifUnit,fmta) TRIM(LineOut)
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('FLUIDPROPERTIES:TEMPERATURES')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,1,OutArgs,FieldNames,FieldUnits)
          DO Arg=2,CurArgs,7
            VargS=Arg
            VargE=Arg+6
            IF (VargS+6 > CurArgs) VargE=CurArgs
            IF (VArgE /= CurArgs) THEN
              LString=','
            ELSE
              LString=';'
            ENDIF
            LineOut='    '//TRIM(OutArgs(VArgS))
            DO VArg=VargS+1,VargE
              LineOut=TRIM(LineOut)//','//OutArgs(Varg)
            ENDDO
            LineOut=TRIM(LineOut)//LString
            WRITE(DifUnit,fmta) TRIM(LineOut)
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE('FLUIDPROPERTIES:SATURATED','FLUIDPROPERTIES:SUPERHEATED','FLUIDPROPERTIES:CONCENTRATION')
          CALL WriteOutPartialIDFLines(DifUnit,ObjectName,4,OutArgs,FieldNames,FieldUnits)
          DO Arg=5,CurArgs,7
            VargS=Arg
            VargE=Arg+6
            IF (VargS+6 > CurArgs) VargE=CurArgs
            IF (VArgE /= CurArgs) THEN
              LString=','
            ELSE
              LString=';'
            ENDIF
            LineOut='    '//TRIM(OutArgs(VArgS))
            DO VArg=VargS+1,VargE
              LineOut=TRIM(LineOut)//','//OutArgs(Varg)
            ENDDO
            LineOut=TRIM(LineOut)//LString
            WRITE(DifUnit,fmta) TRIM(LineOut)
          ENDDO
          WRITE(DifUnit,fmta) ' '

        CASE DEFAULT
          Written=.false.

      END SELECT

    ENDIF
  RETURN

END SUBROUTINE CheckSpecialObjects

SUBROUTINE ReadRenamedObjects(FileName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine reads in a file of renamed objects, sorted by "old name".
          ! The file is tab delimited, with tabs between the old and new names.
          ! No case changing is done on the object names.
          ! Names go into the global OldObjectName and NewObjectName, number of
          ! renamed objects is NumRenamedObjects,
          ! File format has the number of lines following in the first line.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: FileName

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=1), PARAMETER :: Tab=Char(9)
  CHARACTER(len=*), PARAMETER :: fmta="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, EXTERNAL :: GetNewUnitNumber
  INTEGER :: Unit
  LOGICAL :: FileExist
  CHARACTER(len=MaxNameLength*3) :: InputLine
  INTEGER :: ReadStatus
  INTEGER :: pos
  INTEGER :: curNum

  NumRenamedObjects=0
  curNum=0
  INQUIRE(FILE=FileName,EXIST=FileExist)
  IF (FileExist) THEN
    Unit=GetNewUnitNumber()
    OPEN(Unit,File=trim(FileName))
    READ(Unit,*) NumRenamedObjects
    ALLOCATE(OldObjectNames(NumRenamedObjects))
    ALLOCATE(NewObjectNames(NumRenamedObjects))
    OldObjectNames=' '
    NewObjectNames=' '
    ReadStatus=0
    DO WHILE(ReadStatus==0)
      READ(Unit,fmta,IOStat=ReadStatus) InputLine
      IF (ReadStatus /= 0) EXIT
      pos=INDEX(InputLine,Tab)
      if (pos > 0) then
        curNum=curNum+1
        OldObjectNames(curNum)=InputLine(1:pos-1)
        NewObjectNames(curNum)=InputLine(pos+1:)
      else
        write(Auditf,*) 'file=',trim(FileName),' blank line after ',curNum,' renamed objects read'
        exit
      endif
    ENDDO
    NumRenamedObjects=curNum
  ELSE
    write(Auditf,*) 'file=',trim(FileName),' not found'
    CALL ShowFatalError('File='//trim(FileName)//' not found.  Transition terminates without completing.')
  ENDIF

  RETURN

END SUBROUTINE ReadRenamedObjects

SUBROUTINE ReplaceRenamedObjectFields(OldName,NewName,ErrFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine attempts to find the old object name in a sorted
          ! list, replaces the new field/name with the replacement name.
          ! If not found, the oldname just replaces the new.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInSortedList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: OldName
  CHARACTER(len=*), INTENT(OUT) :: NewName
  LOGICAL, INTENT(INOUT)        :: ErrFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: FoundItem

  ErrFlag=.false.

  FoundItem=FindItemInSortedList(OldName,OldObjectNames,NumRenamedObjects)
  if (FoundItem > 0) THEN  ! found it
    NewName=NewObjectNames(FoundItem)
  else
    NewName=OldName
    ErrFlag=.true.
  endif

  RETURN

END SUBROUTINE ReplaceRenamedObjectFields

FUNCTION IsValidOutputUnits(String) RESULT (IsValid)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns whether the input string is valid for
          ! Energyplus Output Units.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String
  LOGICAL :: IsValid

          ! FUNCTION PARAMETER DEFINITIONS:
   INTEGER, PARAMETER :: NumOutputUnits=41
   CHARACTER(len=*), PARAMETER, DIMENSION(NumOutputUnits) :: ValidOutputUnits = &
    (/'%               ',  &
      'A               ',  &
      'ach             ',  &
      'Ah              ',  &
      'C               ',  &
      'cd/m2           ',  &
      'deg             ',  &
      'deltaC          ',  &
      'hr              ',  &
      'J               ',  &
      'J/kg            ',  &
      'J/kgWater       ',  &
      'J/m2            ',  &
      'K/m             ',  &
      'kg              ',  &
      'kg/kg           ',  &
      'kg/m3           ',  &
      'kg/s            ',  &
      'kgWater/kgDryAir',  &
      'kgWater/s       ',  &
      'kmol/s          ',  &
      'L               ',  &
      'lum/W           ',  &
      'lux             ',  &
      'm               ',  &
      'm/s             ',  &
      'm2              ',  &
      'm3              ',  &
      'm3/s            ',  &
      'Pa              ',  &
      'ppm             ',  &
      'rad             ',  &
      's               ',  &
      'units           ',  &
      'V               ',  &
      'W               ',  &
      'W/K             ',  &
      'W/m2            ',  &
      'W/m2-C          ',  &
      'W/m2-K          ',  &
      'W/W             '/)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop

  IsValid=.false.
  DO Loop=1,NumOutputUnits
    IF (.not. SameString(String,ValidOutputUnits(Loop))) CYCLE
    IsValid=.true.
    EXIT
  ENDDO

  RETURN

END FUNCTION IsValidOutputUnits

end module VCompareGlobalRoutines
