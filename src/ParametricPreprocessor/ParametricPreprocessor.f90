PROGRAM ParametricPreprocessor
! PROGRAM INFORMATION:
!   AUTHOR         Jason Glazer of GARD Analytics, Inc.
!   DATE WRITTEN   April 2009
!   MODIFIED       na
!   RE-ENGINEERED  na
!
! PURPOSE OF THIS PROGRAM:
!   Implement the preprocessor for simplified parameter substitution that
!   allows some limited parameter substitution and equation evaluation
!   for multiple simulations using a single input file. Any field in the
!   input file can start with an equals sign "=" as a flag for a field with
!   an expression and can contain a reference to a parameter such as $WIDTH.
!   Specific objects that are used to set parameters include:
!
!        Parametric:SetValueForRun
!        Parametric:Logic
!        Parametric:RunControl
!        Parametric:FileNameSuffix
!
!   Details on the functionality of these objects is located in the normal
!   EnergyPlus documentation.
!
!   The order of evaluation is:
!      first set values based on Parametric:SetValueForRun
!      second evaluate all lines of the Parametric:Logic object
!      third evaluate all embedded expressions in remainder of file
!
! METHODOLOGY EMPLOYED:
!
!   Scan the input file and create an intermediate file that contains references
!   to all embedded expressions and has none of the Parametric:* objects. The program
!   stores the embedded expressions and lines from the Parametric:Logic object into
!   an intermediate code based on reverse polish notation. Then for each case, the
!   intermediate file is opened, the computations are done by first taking the
!   SetValueForRun values for the case, then the Parametric:Logic lines are computed,
!   and the embedded expressions are evaluated. The resulting evaluated symbols are
!   substituted in for the case. This is repeated for each case with a new file each
!   time.
!
! REFERENCES:
!   None.
!
! OTHER NOTES:
!
!
IMPLICIT NONE

LOGICAL :: verboseDebug = .FALSE.  !turn on and off extra debugging output

CHARACTER(len=1), PARAMETER :: tabChar=CHAR(9)

INTEGER, PARAMETER ::  charNum = 1    ! 0123456789
INTEGER, PARAMETER ::  charPeriod = 2 ! .
INTEGER, PARAMETER ::  charE = 3      ! e and E
INTEGER, PARAMETER ::  charAZ = 4     ! ABCDFGHIJKLMNOPQRSTUVWXYZabcdfghijklmnopqrstuvwxyz E's are missing
INTEGER, PARAMETER ::  charPlus = 5
INTEGER, PARAMETER ::  charMinus = 6
INTEGER, PARAMETER ::  charMult = 7
INTEGER, PARAMETER ::  charDiv = 8
INTEGER, PARAMETER ::  charExp = 9
INTEGER, PARAMETER ::  charLeftParen = 10
INTEGER, PARAMETER ::  charRightParen = 11
INTEGER, PARAMETER ::  charDoubleQuote = 12
INTEGER, PARAMETER ::  charEqual = 13
INTEGER, PARAMETER ::  charGreat = 14
INTEGER, PARAMETER ::  charLess = 15
INTEGER, PARAMETER ::  charTilde = 16
INTEGER, PARAMETER ::  charSpace = 17
INTEGER, PARAMETER ::  charDollar = 18
INTEGER, PARAMETER ::  charAmpersand = 19
INTEGER, PARAMETER ::  charPipe = 20
INTEGER, PARAMETER ::  charUnderscore = 21
INTEGER, PARAMETER ::  charOther = 100
INTEGER, PARAMETER ::  charNone = 200

INTEGER, PARAMETER ::  StringLength =  200
INTEGER, PARAMETER ::  LongString = 2000

INTEGER, PARAMETER :: tokNum = -1         !numbers, can start with minus, one period, one E or e   -0.343E-01
INTEGER, PARAMETER :: tokStr = -2         !strings starting and ending with "                      "string with spaces"
INTEGER, PARAMETER :: tokID = -3          !name containing letters only starting with $            $HEIGHT
INTEGER, PARAMETER :: tokFunc = -4
INTEGER, PARAMETER :: tokPlus = -5
INTEGER, PARAMETER :: tokMinus = -6
INTEGER, PARAMETER :: tokTimes = -7
INTEGER, PARAMETER :: tokDiv = -8
INTEGER, PARAMETER :: tokExp = -9
INTEGER, PARAMETER :: tokRtParen = -10
INTEGER, PARAMETER :: tokLtParen = -11
INTEGER, PARAMETER :: tokGT = -12
INTEGER, PARAMETER :: tokEQ = -13     !==
INTEGER, PARAMETER :: tokLT = -14
INTEGER, PARAMETER :: tokGE = -15     !>= or =>
INTEGER, PARAMETER :: tokLE = -16     !<= or =<
INTEGER, PARAMETER :: tokNE = -17
INTEGER, PARAMETER :: tokAnd = -18    ! &&
INTEGER, PARAMETER :: tokOr = -19     ! ||
INTEGER, PARAMETER :: tokTilde = -20  !used a separator for functions since comma cannot be used
INTEGER, PARAMETER :: tokUnNeg = -21 !unary negation -$PAR for example
INTEGER, PARAMETER :: tokFuncABS = -22
INTEGER, PARAMETER :: tokFuncACOS = -23
INTEGER, PARAMETER :: tokFuncASIN = -24
INTEGER, PARAMETER :: tokFuncATAN = -25
INTEGER, PARAMETER :: tokFuncCOS = -26
INTEGER, PARAMETER :: tokFuncEXP = -27
INTEGER, PARAMETER :: tokFuncINT = -28
INTEGER, PARAMETER :: tokFuncLEN = -29
INTEGER, PARAMETER :: tokFuncLOG = -30
INTEGER, PARAMETER :: tokFuncMOD = -31
INTEGER, PARAMETER :: tokFuncNOT = -32
INTEGER, PARAMETER :: tokFuncSIN = -33
INTEGER, PARAMETER :: tokFuncSQRT = -34
INTEGER, PARAMETER :: tokFuncTAN = -35
INTEGER, PARAMETER :: tokNone = -98
INTEGER, PARAMETER :: tokINVALID = -99

INTEGER, PARAMETER :: opStackSize = 100
TYPE opStackType
  INTEGER :: tokOperator = 0
  INTEGER :: precedence = 0
  INTEGER :: funcStart = 0
  INTEGER :: funcEnd = 0
END TYPE
TYPE (opStackType),DIMENSION(opStackSize) :: opStack
INTEGER :: opStackTop = 0

INTEGER, PARAMETER :: precExp = 6
INTEGER, PARAMETER :: precMultDiv = 5
INTEGER, PARAMETER :: precAddSub = 4
INTEGER, PARAMETER :: precCompare = 3
INTEGER, PARAMETER :: precAndOr = 2
INTEGER, PARAMETER :: precLeftParen = 1
INTEGER, PARAMETER :: precNoItem = 0

CHARACTER(len=LongString)                            :: InputFilePathName =''
CHARACTER(len=LongString)                            :: FilePathOnly =''
CHARACTER(len=LongString)                            :: OutputFileNameRoot = ''
CHARACTER(len=LongString)                            :: OutputFileName = ''
CHARACTER(len=StringLength)                          :: IOErrorMsg =''

INTEGER,PARAMETER :: InFH   = 1 !input IDF file handle
INTEGER,PARAMETER :: tFH    = 2 !temporary 'intermediate' file handle
INTEGER,PARAMETER :: oFH    = 3 !output file handle
INTEGER,PARAMETER :: ErrFH  = 4 !error file name

LOGICAL :: errorCondition = .FALSE.
CHARACTER(len=LongString) :: errorContext

! Array that is used to hold error messages during execution so that they
! all get written at the end (not in the middle of any object)
INTEGER, PARAMETER :: MaxErrorLength = 300
TYPE ErrMsgType
  CHARACTER(len=MaxErrorLength)  :: msgText = ''
  INTEGER                        :: msgKind = 0
  INTEGER                        :: msgErrNum = 0
  CHARACTER(len=MaxErrorLength)  :: msgContext = ''
END TYPE
TYPE (ErrMsgType), ALLOCATABLE, DIMENSION(:)  :: ErrMsgs
TYPE (ErrMsgType), ALLOCATABLE, DIMENSION(:)  :: ErrMsgsCopy
INTEGER                                                   :: numErrMsgs = 0
INTEGER                                                   :: sizeErrMsgs = 100
INTEGER, PARAMETER :: msgError = 1
INTEGER, PARAMETER :: msgWarning = 2

! Array that holds the expressions found in fields of the IDF file
TYPE FoundExpressionType
  CHARACTER(len=LongString)    :: text = ''
  INTEGER                        :: line = 0
  INTEGER                        :: startIntCode = 0
  INTEGER                        :: endIntCode = 0
  CHARACTER(len=LongString)    :: expResult = ''
END TYPE
TYPE (FoundExpressionType), ALLOCATABLE, DIMENSION(:)  :: FoundExpression
TYPE (FoundExpressionType), ALLOCATABLE, DIMENSION(:)  :: FoundExpressionCopy
INTEGER                                                :: numFoundExpression = 0
INTEGER                                                :: sizeFoundExpression = 100

! objects that are not stored but are simply identified by the line numbers
TYPE ObjectLinesType
  CHARACTER(len=StringLength)    :: kindOfObj  = ''
  CHARACTER(len=StringLength)    :: nameOfObj  = '' !first field
  INTEGER                        :: firstLine = 0
  INTEGER                        :: lastLine = 0
  LOGICAL                        :: enabled = .TRUE.      !in parametric:logic either ENABLE or DISABLE for this object
END TYPE
TYPE (ObjectLinesType), ALLOCATABLE, DIMENSION(:)  :: ObjectLines
TYPE (ObjectLinesType), ALLOCATABLE, DIMENSION(:)  :: ObjectLinesCopy
INTEGER                                            :: numObjectLines = 0
INTEGER                                            :: sizeObjectLines = 500
LOGICAL,ALLOCATABLE,DIMENSION(:,:) :: EnableObjectInFile  ! EnableObjectInFile(numObjectLines,numCases)
INTEGER                                            :: activeDisabledObject = 0

INTEGER, PARAMETER :: reset = -999

! objects that are read and stored in memory
TYPE IdfObjectType
  INTEGER                        :: kindObj = 0
  INTEGER                        :: firstField = 0
  INTEGER                        :: lastField = 0
END TYPE
TYPE (IdfObjectType), ALLOCATABLE, DIMENSION(:)  :: IdfObject
TYPE (IdfObjectType), ALLOCATABLE, DIMENSION(:)  :: IdfObjectCopy
INTEGER                                          :: numIdfObject = 0
INTEGER                                          :: sizeIdfObject = 20
INTEGER, PARAMETER  :: koNone = 0
INTEGER, PARAMETER  :: koSetValueForRun = 1
INTEGER, PARAMETER  :: koLogic = 2
INTEGER, PARAMETER  :: koRunControl = 3
INTEGER, PARAMETER  :: koFileNameSuffix = 4

CHARACTER(len=LongString), ALLOCATABLE, DIMENSION(:) :: IdfField
CHARACTER(len=LongString), ALLOCATABLE, DIMENSION(:) :: IdfFieldCopy
INTEGER                     :: numIdfField = 0
INTEGER                     :: sizeIdfField = 200

TYPE CasesType
  LOGICAL                     :: active = .FALSE.
  LOGICAL                     :: dup = .false.
  LOGICAL                     :: dup2 = .false.
  CHARACTER(len=StringLength) :: suffix = ''
END TYPE
TYPE (CasesType), ALLOCATABLE, DIMENSION(:)  :: Cases
INTEGER :: numCases !number of parametric cases - each case is an output file and contains a set of substituted values

! valForRun(lastSymbolParameter,numCases) the values of the parameters in Symbol
CHARACTER(len=StringLength), ALLOCATABLE, DIMENSION(:,:) :: valForRun

! kind of statment
INTEGER,PARAMETER :: ksNone = -128
INTEGER,PARAMETER :: ksAssignment = -129
INTEGER,PARAMETER :: ksParameter = -130
INTEGER,PARAMETER :: ksIf = -131
INTEGER,PARAMETER :: ksElseIf = -132
INTEGER,PARAMETER :: ksElse = -133
INTEGER,PARAMETER :: ksEndIf = -134
INTEGER,PARAMETER :: ksSelect = -135
INTEGER,PARAMETER :: ksCase = -136
INTEGER,PARAMETER :: ksDefault = -137
INTEGER,PARAMETER :: ksEndSelect = -138
INTEGER,PARAMETER :: ksDisable = -139
INTEGER,PARAMETER :: ksEnable = -140
INTEGER,PARAMETER :: ksRemark = -141

! IntCode - Intermediate Code array
! used in Parametric:Logic and expressions in other objects
! the array that holds the sequences to commands and references to parameter values and
! references to constants is shown below
!   Ranges of values have specific meanings:
!     -1 to -127 are token constants
!     1 to lastNumSymbol are references to the SymbolTable
! The IntCode is a list of codes evaluated in a stack based (reverse polish notation)
! approach with statements and tokens acting as operators and references to the symbol
! table acting as operands.
INTEGER, ALLOCATABLE, DIMENSION(:) :: IntCode
INTEGER, ALLOCATABLE, DIMENSION(:) :: IntCodeCopy
INTEGER                     :: numIntCode = 0
INTEGER                     :: sizeIntCode = 1000

! The symbol table is referenced by the IntCode array and contains the active value
! of each parameter and all constants.
TYPE SymbolType
  CHARACTER(len=StringLength)    :: name = '' !blank if a constant
  CHARACTER(len=StringLength)    :: val = ''
  LOGICAL                        :: isRealNum = .FALSE.
  REAL(8)                        :: valAsReal = 0.0
  LOGICAL                        :: isSetValueForRun = .FALSE.
  LOGICAL                        :: isParameter = .FALSE.
END TYPE
TYPE(SymbolType),ALLOCATABLE,DIMENSION(:) :: Symbol
TYPE(SymbolType),ALLOCATABLE,DIMENSION(:) :: SymbolCopy
INTEGER                     :: numSymbol = 0
INTEGER                     :: lastSymbolSetValueForRun = 0
INTEGER                     :: lastSymbolParameter = 0
INTEGER                     :: sizeSymbol = 500

TYPE ParLogLineType
  INTEGER                        :: statementKind = 0
  INTEGER                        :: idfFieldNum = 0
  INTEGER                        :: startIntCode = 0
  INTEGER                        :: endIntCode = 0
  INTEGER                        :: assignParam = 0
  INTEGER                        :: symbolA = 0
  INTEGER                        :: symbolB = 0
  INTEGER                        :: objLine = 0
END TYPE
TYPE(ParLogLineType),ALLOCATABLE,DIMENSION(:) :: ParLogLine
INTEGER                           :: numParLogLine=0

TYPE StrucStackType
  INTEGER                        :: mode
  CHARACTER(len=StringLength)    :: match = '' !for select
END TYPE
TYPE (StrucStackType),ALLOCATABLE,DIMENSION(:) :: StrucStack
TYPE (StrucStackType),ALLOCATABLE,DIMENSION(:) :: StrucStackCopy
INTEGER                                        :: topStrucStack = 0
INTEGER                                        :: sizeStrucStack = 50
INTEGER,PARAMETER :: ssmNotInStruc = 1      ! not within an IF or SELECT structure
INTEGER,PARAMETER :: ssmDoThen = 2          ! within the THEN block because IF was true statement is true
INTEGER,PARAMETER :: ssmSkipToElse = 3      ! skip to the ELSE or ELSEIF because IF was false or ELSEIF was false
INTEGER,PARAMETER :: ssmDoElse = 4          ! within the ELSE block looking for the ENDIF
INTEGER,PARAMETER :: ssmSkipToEndIf = 5     ! within part of the IF structure not being executed looking for the ENDIF
INTEGER,PARAMETER :: ssmFindCase =  6       ! looking for matching CASE value within SELECT structure
INTEGER,PARAMETER :: ssmDoCase = 7          ! executing a matching CASE block within SELECT structure
INTEGER,PARAMETER :: ssmSkipToEndSelect = 8 ! within part of the SELECT structure not being executed looking for the ENDSELECT

TYPE EvalStackType
  CHARACTER(len=StringLength)    :: val = ''
  LOGICAL                        :: isRealNum = .FALSE.
  REAL(8)                        :: valAsReal = 0.0
END TYPE
TYPE(EvalStackType),ALLOCATABLE,DIMENSION(:) :: EvalStack
TYPE(EvalStackType),ALLOCATABLE,DIMENSION(:) :: EvalStackCopy
INTEGER                     :: topEvalStack = 0
INTEGER                     :: sizeEvalStack = 50

! For delimKind
INTEGER, PARAMETER :: dkComma = 1
INTEGER, PARAMETER :: dkSemi = 2
INTEGER, PARAMETER :: dkEqual = 3
INTEGER, PARAMETER :: dkExcl = 4
INTEGER, PARAMETER :: dkEOL = 5
CHARACTER(len=*), PARAMETER :: cDeveloperFlag='DeveloperFlag'
CHARACTER(len=*), PARAMETER :: Blank=' '


!----------------------------------------------------------------------------------
! Main routine
!----------------------------------------------------------------------------------
REAL :: startTime=0.0
REAL :: endTime=0.0
INTEGER :: iCases = 0
CHARACTER(1) :: keyIn
LOGICAL :: DeveloperFlag=.false.       ! TRUE if developer flag is turned on. (turns on more displays to console)
CHARACTER(len=10)  :: cEnvValue=' '
LOGICAL :: errflag=.false.

PRINT "(A)", "ParametricPreprocessor Started."
CALL CPU_TIME(startTime)
CALL GET_COMMAND_ARGUMENT(1,InputFilePathName)

cEnvValue=' '
CALL Get_Environment_Variable(cDeveloperFlag,cEnvValue)
IF (cEnvValue /= Blank) &
  DeveloperFlag = (cEnvValue(1:1)=='Y' .or. cEnvValue(1:1)=='y')
verboseDebug = DeveloperFlag

IF (LEN_TRIM(InputFilePathName) .EQ. 0) THEN
!  InputFilePathName = 'C:\Documents and Settings\Jason\My Documents\projects\EnergyPlusDev\ParametricInputs\dev 2009-04a\5ZoneAirCooledPar.idf'
!  InputFilePathName = 'C:\Documents and Settings\Jason\My Documents\projects\EnergyPlusDev\ParametricInputs\dev 2009-04a\Debug\1ZoneParameterAspect.idf'
!  InputFilePathName = 'C:\Documents and Settings\Jason\My Documents\projects\EnergyPlusDev\ParametricInputs\dev 2009-06a\Debug\Shade-jg.idf'
!  InputFilePathName = 'C:\Documents and Settings\Jason\My Documents\projects\EnergyPlusDev\ParametricInputs\dev 2009-06b\Debug\Shade-3-Changes-LogicDisablingOverhang-Try1A.idf'
!  InputFilePathName = 'C:\Documents and Settings\Jason\My Documents\projects\EnergyPlusDev\ParametricInputs\dev 2009-08a\Debug\Cond-1-Select-Logic-jg.idf'
  InputFilePathName = 'C:\Documents and Settings\Jason\My Documents\projects\EnergyPlusDev\ParametricInputs\dev 2009-08a\Debug\Cond-1-Parametric-LogicTestingIF.idf'
  IF (verboseDebug) PRINT '(A,A)',' InputFilePathName:',TRIM(InputFilePathName)
END IF
FilePathOnly = PathOnly(InputFilePathName)
OutputFileNameRoot = NoExtension(InputFilePathName)
CALL OpenErrorFile
CALL OpenFilesFirstPass
CALL InitialRead !read IDF and create intermediate and read parametric objects and fields
CALL CloseFilesFirstPass
CALL DetermineNumberOfCases
ALLOCATE (Cases(numCases))
CALL SetCases !interpret RunControl and FileNameSuffix
CALL GatherParameterSymbols !make list of all params
ALLOCATE (valForRun(lastSymbolParameter,numCases))
valForRun = '' !set entire parameter value array to blank values
CALL ReadSetValueObjects
CALL TranslateParametricLogic
CALL TranslateEmbeddedExpressions
DO iCases = 1, numCases
  IF (Cases(iCases)%active) THEN
    CALL SetValueForCase(iCases)
    CALL ComputeParametricLogic
    CALL ComputeEmbeddedExpressions
    CALL OpenFileSecondPass(iCases)
    CALL SubstituteValues !substitute values into intermediate files for all output files
    CALL CloseFileSecondPass
  END IF
END DO
CALL WriteErrorsAndCloseFile
CALL CPU_TIME(endTime)
IF (numErrMsgs .GT. 0) THEN
  PRINT "(A,F10.3)", "ParametricPreprocessor Finished with Error(s). Time:", endTime - startTime
ELSE
  PRINT "(A,F10.3)", "ParametricPreprocessor Finished. Time:", endTime - startTime
END IF
IF (verboseDebug) THEN
  PRINT "(A)", "Type any key and press ENTER to exit"
  READ *,keyIn
END IF
! The following line is just for debugging - it allows the Watch window to display the proper values

IF (verboseDebug) THEN
  IF (numParLogLine > 0) THEN
    PRINT '(A,3I6,A,A)',trim(symbol(2)%name),IntCode(1),opStack(1)%tokOperator, ParLogLine(1)%statementKind,trim(IdfField(1)),trim(objectLines(1)%nameOfObj)
  else
    PRINT '(A,3I6,A,A)',trim(symbol(2)%name),IntCode(1),opStack(1)%tokOperator, -999,trim(IdfField(1)),trim(objectLines(1)%nameOfObj)
  ENDIF
ENDIF
STOP

CONTAINS

!=====================================================================================
!=====================================================================================
!
!    ROUTINES CALLED DIRECTLY FROM MAIN PROGRAM
!
!=====================================================================================
!=====================================================================================

!----------------------------------------------------------------------------------
! Open the error file.
!----------------------------------------------------------------------------------
SUBROUTINE OpenErrorFile
IMPLICIT NONE
CHARACTER(len=LongString) :: ErrorFileWithPath=' '
INTEGER :: status
IF (verboseDebug) PRINT '(A)',' Started OpenErrorFile'
!old method ErrorFileWithPath = TRIM(FilePathOnly) // 'parametric.err'
ErrorFileWithPath = TRIM(OutputFileNameRoot) // '.err'
OPEN (unit=ErrFH, file=TRIM(ErrorFileWithPath), action='WRITE',iostat=status)
IF (status .NE. 0) PRINT '(A)','Cannot open error file.'
END SUBROUTINE OpenErrorFile

!----------------------------------------------------------------------------------
! Open the IDF file for reading and the intermediate file for writing
!----------------------------------------------------------------------------------
SUBROUTINE OpenFilesFirstPass
IMPLICIT NONE
CHARACTER(len=LongString) :: TempFileName=' '
LOGICAL :: InFileExist=.false.
INTEGER :: status
IF (verboseDebug) PRINT '(A)',' Started OpenFilesFirstPass'
IF (errorCondition) RETURN  !if an error exists do not run this routine
TempFileName = TRIM(FilePathOnly) // 'parametric.int'
INQUIRE(file=InputFilePathName,exist=InFileExist)
IF (InFileExist) THEN
  OPEN (unit=InFH, file=TRIM(InputFilePathName), action='READ',iostat=status)
  IF (status .NE. 0) CALL AddToErrMsg('error opening idf file',msgError,status)
  OPEN (unit=tFH, file=TRIM(TempFileName), action='WRITE',iostat=status)
  IF (status .NE. 0) CALL AddToErrMsg('error opening intermediate file named parametric.int',msgError,status)
ELSE
  call AddToErrMsg('error opening idf file - no file found',msgError,0)
END IF
END SUBROUTINE OpenFilesFirstPass

!----------------------------------------------------------------------------------
! Read IDF and create intermediate and read parametric objects and fields.
! The intermediate file will contain the same items as the IDF file but
! also includes references to expressions instead of the expressions
! themselves. Each expression is replaces with =@@000001 where the =@@
! is an indicator that the following number is a reference for an expression.
! The 000001 is a index for the array of expressions and will have the substituted
! value.
!
! The objects:
!        Parametric:SetValueForRun
!        Parametric:Logic
!        Parametric:RunControl
!        Parametric:FileNameSuffix
! are extracted from the IDF file and not written to the intermediate file.
!
! In addition, the object type and first field (usually the name of the object)
! along with the first line and last line number of the object are stored for
! possible disabling.
!----------------------------------------------------------------------------------
SUBROUTINE InitialRead
IMPLICIT NONE
INTEGER :: read_stat = 0
CHARACTER(len=LongString) :: LineIn
INTEGER :: lineInPos = 0
INTEGER :: lineInLen = 0
CHARACTER(len=LongString) :: LineOut
INTEGER :: lineCount = 0
INTEGER :: lineOutPos = 0
LOGICAL :: doOutputLine = .TRUE.
LOGICAL :: doOutputChar = .TRUE.
INTEGER :: objMode = 1
INTEGER,PARAMETER :: omOut = 1   !objMode is out of any object
INTEGER,PARAMETER :: omNorm = 2  !objMode is in normal object
INTEGER,PARAMETER :: omParam = 3 !objMode is in parametric object
INTEGER :: fldMode = 1
INTEGER,PARAMETER :: fmOut = 1   !fldMode is out of any field
INTEGER,PARAMETER :: fmCmt = 2   !fldMode is in a comment
INTEGER,PARAMETER :: fmNorm = 3  !fldMode is in a normal field
INTEGER,PARAMETER :: fmExpr = 4  !fldMode is in an expression field, starting with =$
INTEGER :: startOfField = 0
INTEGER :: curExpressionID = 0
CHARACTER(len=LongString) :: curField
CHARACTER(len=StringLength) :: kindOfObj
CHARACTER(len=StringLength) :: curNameOfObj
INTEGER :: fldCount = 0
INTEGER :: lineAtObjStart = 0
INTEGER :: pos

IF (verboseDebug) PRINT '(A)',' Started InitialRead'
IF (errorCondition) RETURN  !if an error exists do not run this routine
objMode = omOut
fldCount = 0
lineCount = 1
DO WHILE (read_stat == 0) ! not end of file
  READ(UNIT=InFH, FMT="(A)", IOSTAT=read_stat) LineIn
  IF (read_stat /= 0) EXIT
  pos=INDEX(LineIn,tabChar)
  DO WHILE (pos > 0)
    LineIn(pos:pos)=' '
    pos=INDEX(LineIn,tabChar)
  ENDDO
  LineInLen = LEN_TRIM(LineIn)
  lineOut = ''
  lineOutPos = 0
  doOutputLine = .TRUE.
  fldMode = fmOut
  IF (LineInLen .GE. 1) THEN
    DO lineInPos = 1, LineInLen
      doOutputChar = .TRUE.
      SELECT CASE (ICHAR(LineIn(lineInPos:lineInPos)))
        CASE (44) ! , comma
          IF ((fldMode .EQ. fmNorm) .OR. (fldMode .EQ. fmExpr)) THEN
            curField = LineIn(startOfField:(lineInPos-1))
            fldCount = fldCount + 1
          END IF
          SELECT CASE (fldMode)
            CASE (fmNorm)
              fldMode = fmOut
              SELECT CASE (fldCount)
                CASE (1) ! first field
                  kindOfObj = curField
                  IF (IsStrEq(kindOfObj,'Parametric:SetValueForRun')) THEN
                    CALL addNewObject(koSetValueForRun)
                    objMode = omParam
                  ELSEIF (IsStrEq(kindOfObj,'Parametric:Logic')) THEN
                    CALL addNewObject(koLogic)
                    objMode = omParam
                  ELSEIF (IsStrEq(kindOfObj,'Parametric:RunControl')) THEN
                    CALL addNewObject(koRunControl)
                    objMode = omParam
                  ELSEIF (IsStrEq(kindOfObj,'Parametric:FileNameSuffix')) THEN
                    CALL addNewObject(koFileNameSuffix)
                    objMode = omParam
                  ELSE
                    objMode = omNorm
                    lineAtObjStart = lineCount
                  END IF
                CASE (2) !second field
                  curNameOfObj = curField
                  IF (objMode .EQ. omParam) CALL addField(curField)
                CASE DEFAULT !all other fields
                  IF (objMode .EQ. omParam) CALL addField(curField)
              END SELECT
              IF (objMode .EQ. omParam) doOutputLine = .FALSE.
            CASE (fmExpr)
              fldMode = fmOut
              objMode = omNorm
              curExpressionID = addNewExpression(curField(2:),lineCount)
              LineOut(lineOutPos + 1:lineOutPos + 9) = '=@@' // IntToStr6(curExpressionID)
              lineOutPos = lineOutPos + 9 ! the format will be =@@123456 which is always nine characters
          END SELECT
        CASE (59) ! ; semicolon
          IF ((fldMode .EQ. fmNorm) .OR. (fldMode .EQ. fmExpr)) THEN
            curField = LineIn(startOfField:(lineInPos-1))
            fldCount = fldCount + 1
          ELSEIF (fldMode .EQ. fmOut) THEN  !if the last field is blank (just a semicolon) process it (fix related to CR8536)
            curField = ''
            fldCount = fldCount + 1
            fldMode = fmNorm
          END IF
          IF (objMode .EQ. omParam) CALL addField(curField)
          SELECT CASE (fldMode)
            CASE (fmNorm)
              IF (objMode .EQ. omParam) doOutputLine = .FALSE.
              fldMode = fmOut
              objMode = omOut
              CALL AddObjectLineReference(kindOfObj,curNameOfObj,lineAtObjStart,lineCount)
              fldCount = 0
            CASE (fmExpr)
              fldMode = fmOut
              objMode = omOut
              curExpressionID = addNewExpression(curField(2:),lineCount)
              LineOut(lineOutPos + 1:lineOutPos + 9) = '=@@' // IntToStr6(curExpressionID)
              lineOutPos = lineOutPos + 9 ! the format will be =@@123456 which is always nine characters
              fldCount = 0
          END SELECT
        CASE (33) ! ! exclamation point
          fldMode = fmCmt
        CASE (61) ! = equals sign
          IF ((fldMode .EQ. fmOut) .AND. (objMode .EQ. omNorm)) THEN
            fldMode = fmExpr
            startOfField = lineInPos
            doOutputChar = .FALSE.
          END IF
        CASE (32) !   space
          !do nothing
        CASE DEFAULT
          SELECT CASE (fldMode)
            CASE (fmOut)
              fldMode = fmNorm
              startOfField = lineInPos
            CASE (fmExpr)
              doOutputChar = .FALSE.
          END SELECT
      END SELECT
      IF (doOutputChar) THEN
        lineOutPos = lineOutPos + 1
        LineOut(lineOutPos:lineOutPos) = LineIn(lineInPos:lineInPos)
      END IF
    END DO
  END IF
  IF (doOutputLine) THEN
    lineCount = lineCount + 1
    WRITE(tFH,'(A)')  TRIM(LineOut)
  END IF
END DO
END SUBROUTINE InitialRead

!----------------------------------------------------------------------------------
! Close the IDF file and the intermediate file
!----------------------------------------------------------------------------------
SUBROUTINE CloseFilesFirstPass
IMPLICIT NONE
IF (verboseDebug) PRINT '(A)',' Started CloseFilesFirstPass'
CLOSE(InFH)
CLOSE(tFH)
END SUBROUTINE

!----------------------------------------------------------------------------------
! Determine the number of parametric cases
!----------------------------------------------------------------------------------
SUBROUTINE DetermineNumberOfCases
IMPLICIT NONE
INTEGER :: iObj
INTEGER :: curNumFields
INTEGER :: curNumPossCases

IF (verboseDebug) PRINT '(A)',' Started DetermineNumberOfCases'
IF (errorCondition) RETURN  !if an error exists do not run this routine
numCases = 0
DO iObj = 1,numIdfObject
  curNumFields = (IdfObject(iObj)%lastField - IdfObject(iObj)%firstField) + 1
  curNumPossCases = curNumFields - 1
  SELECT CASE (IdfObject(iObj)%kindObj)
    CASE (koSetValueForRun,koRunControl,koFileNameSuffix)
      IF (curNumPossCases .GT. numCases) numCases = curNumPossCases
    CASE (koLogic) !if ParametricLogic is present make at least one case
      IF (numCases .EQ. 0) numCases = 1
  END SELECT
END DO
! if just embedded expressions than make at least one case.
IF (numCases .EQ. 0) THEN
  IF (numFoundExpression .GE. 1) numCases = 1
END IF
END SUBROUTINE DetermineNumberOfCases

!----------------------------------------------------------------------------------
! Interpret the Parametric:RunControl and Parametric:FileNameSuffix
!----------------------------------------------------------------------------------
SUBROUTINE SetCases
IMPLICIT NONE
LOGICAL :: foundRunControl = .FALSE.
LOGICAL :: foundFileNameSuffix = .FALSE.
INTEGER :: numCurFields = 0
INTEGER :: curField
INTEGER :: iObj = 0
INTEGER :: jFld = 0
INTEGER :: kPar = 0
INTEGER :: jPar = 0
INTEGER :: dupcount

IF (verboseDebug) PRINT '(A)',' Started SetCases'
IF (errorCondition) RETURN  !if an error exists do not run this routine
!set the default values for Case
DO kPar = 1,numCases
  Cases(kPar)%active = .TRUE.
  Cases(kPar)%suffix = IntToStr6(kPar) !always six characters long
END DO
DO iObj = 1, numIdfObject
  SELECT CASE (IdfObject(iObj)%kindObj)
    CASE (koRunControl)
      IF (.NOT. foundRunControl) THEN
        foundRunControl = .TRUE. !set flag in case a second object is found
        numCurFields = (IdfObject(iObj)%lastField - IdfObject(iObj)%firstField) + 1
        DO jFld = 2, numCurFields !ignore the name of the Parametric:RunControl object so skip first field
          curField = IdfObject(iObj)%firstField + jFld - 1
          IF ((curField .GT. 0) .AND. (curField .LE. numIdfField)) THEN
            IF (IsStrEq(IdfField(curField),'yes')) THEN
              Cases(jFld - 1)%active = .TRUE.
            ELSEIF (IsStrEq(IdfField(curField),'no')) THEN
              Cases(jFld - 1)%active = .FALSE.
            ELSE
              CALL AddToErrMsg('Invalid field in Parametric:RunControl' // TRIM(IdfField(curField)),msgError,0)
            END IF
          END IF
        END DO
      ELSE
        CALL AddToErrMsg('More than one Parametric:RunControl object found in file.',msgError,0)
      END IF
    CASE (koFileNameSuffix)
      IF (.NOT. foundFileNameSuffix) THEN
        foundFileNameSuffix = .TRUE. !set flag in case a second object is found
        numCurFields = (IdfObject(iObj)%lastField - IdfObject(iObj)%firstField) + 1
        DO jFld = 2, numCurFields !ignore the name of the Parametric:RunControl object so skip first field
          curField = IdfObject(iObj)%firstField + jFld - 1
          IF ((curField .GT. 0) .AND. (curField .LE. numIdfField)) THEN
            Cases(jFld - 1)%suffix = IdfField(curField)
          END IF
        END DO
      ELSE
        CALL AddToErrMsg('More than one Parametric:FileNameSuffix object found in file.',msgError,0)
      END IF
  END SELECT
END DO
! Test for duplicates
DO kPar = 1,numCases-1
  IF (.not. Cases(kPar)%active) CYCLE
  IF (Cases(kPar)%dup2) CYCLE
  DO jPar = kPar+1,numCases
    if (Cases(kPar)%suffix == Cases(jPar)%suffix) then
      Cases(kPar)%dup=.true.
      Cases(jPar)%dup2=.true.
    endif
  ENDDO
ENDDO
IF (ANY(Cases%dup)) THEN
  errorContext = 'Parametric:FileNameSuffix'
  dupcount=0
  DO kPar = 1,numCases
    IF (Cases(kPar)%dup) dupcount=dupcount+1
  ENDDO
  CALL AddToErrMsg('Duplicate names entered in suffix list. Cannot have duplicate names.',msgError,dupcount)
  IF (verboseDebug) THEN
    PRINT '(A)',' Duplicate suffixes:'
    DO kPar = 1,numCases
      IF (.not. Cases(kPar)%dup) CYCLE
      PRINT '(1X,A,I6,1X,A)',' suffix# ',kpar,trim(Cases(kPar)%suffix)
    ENDDO
  ENDIF
ENDIF
END SUBROUTINE SetCases

!----------------------------------------------------------------------------------
! Scan the Parametric:Logic and Parametric:SetValueForRun for parameters
! in the form of $AAA
!----------------------------------------------------------------------------------
SUBROUTINE GatherParameterSymbols
IMPLICIT NONE
INTEGER :: curField = 0
LOGICAL :: curErr = .FALSE.
CHARACTER(len=StringLength) :: curFldVal
INTEGER :: iObj = 0
INTEGER :: jFld = 0

IF (verboseDebug) PRINT '(A)',' Started GatherParameterSymbols'
IF (errorCondition) RETURN  !if an error exists do not run this routine
DO iObj = 1, numIdfObject
  SELECT CASE (IdfObject(iObj)%kindObj)
    CASE (koSetValueForRun)
      curField = IdfObject(iObj)%firstField
      IF ((curField .GT. 0) .AND. (curField .LE. numIdfField)) THEN
        curErr = AddParameterSymbol(IdfField(curField),.TRUE.) !create symbol and link to SetValueForFun
        IF (verboseDebug) PRINT '(A,A)','GatherParameterSymbolsA: ',symbol(iObj)%name
        IF (.NOT. curErr) THEN
          CALL AddToErrMsg('Parameter name contains invalid characters. ' // TRIM(IdfField(curField)) ,msgError,0)
        END IF
      END IF
    CASE (koLogic)
      ! scan for PARAMETER statements
      IF (IdfObject(iobj)%lastField .GT. IdfObject(iObj)%firstField) THEN
        DO jFld = IdfObject(iObj)%firstField + 1,IdfObject(iobj)%lastField
          curFldVal = ADJUSTL(IdfField(jFld))
          IF (IsStrEq(curFldVal(1:10), 'PARAMETER ')) THEN
            curErr = AddParameterSymbol(curFldVal(11:))
            IF (.NOT. curErr) THEN
              CALL AddToErrMsg('Parameter name contains invalid characters. ' // TRIM(curFldVal) // &
                ' in ' // TRIM(IdfField(IdfObject(iObj)%firstField)),msgError,0)
            END IF
          END IF
        END DO
      END IF
  END SELECT
END DO
END SUBROUTINE GatherParameterSymbols

!----------------------------------------------------------------------------------
! Interpret the Parametric:SetValuesForRun
!----------------------------------------------------------------------------------
SUBROUTINE ReadSetValueObjects
IMPLICIT NONE
INTEGER :: nameField = 0
INTEGER :: iObj = 0
INTEGER :: curParam = 0
INTEGER :: numFld = 0
INTEGER :: curCase = 0
INTEGER :: jCase = 0

IF (verboseDebug) PRINT '(A)',' Started ReadSetValueObjects'
IF (errorCondition) RETURN  !if an error exists do not run this routine
DO iObj = 1, numIdfObject
  IF (IdfObject(iObj)%kindObj .EQ. koSetValueForRun) THEN
    nameField = IdfObject(iObj)%firstField
    IF ((nameField .GT. 0) .AND. (nameField .LE. numIdfField)) THEN
      curParam = LookupParameterSymbol(IdfField(nameField))
      IF (curParam .GT. 0) THEN
        IF (curParam .LE. lastSymbolSetValueForRun) THEN
          numFld = IdfObject(iObj)%lastField - IdfObject(iObj)%firstField
          IF (numFld .LE. numCases) THEN
            ! transfer the field value into the parameter value array
            DO jCase = 1, numFld
              valForRun(curParam,jCase) = IdfField(nameField + jCase)
            END DO
            ! if not enough items, fill the array with the last item
            IF (numFld .LT. numCases) THEN
              DO jCase = numFld + 1, numCases
                valForRun(curParam,jCase) = IdfField(nameField + numFld)
              END DO
            END IF
          END IF
        ELSE
          CALL AddToErrMsg('SetParameterForRun must contain at least one value. ' // TRIM(IdfField(nameField)) ,msgError,0)
        END IF
      ELSE
        CALL AddToErrMsg('Invalid parameter name. ' // TRIM(IdfField(nameField)) ,msgError,0)
      END IF
    END IF
  END IF
END DO
END SUBROUTINE ReadSetValueObjects

!----------------------------------------------------------------------------------
! Translate the "programming language" present in the Parametric:Logic object
! into intermediate code and a symbol table.  This is like compiling a programming
! language into a machine code but instead the code is a simple list that appears
! in the intermediate code array, IntCode.
!
! The possible lines include:
!   <id> = <expression>                        assignment
!   PARAMETER <id>                             define parameter
!   IF <expresson>
!   ELSEIF <expression>
!   ELSE
!   ENDIF
!   SELECT <expression>
!   CASE <constant>
!   DEFAULT                                    if no constant matches
!   ENDSELECT
!   ENABLE <constant>                          the constant is the object name
!   ENABLE <constant> <constant>               object name and object type
!   DISABLE <constant>                         the constant is the object name
!   DISABLE <constant> <constant>              object name and object type
!   REMARK <text to ignore>
!
! The logic is applied to each case on a line by line basis.
!----------------------------------------------------------------------------------
SUBROUTINE TranslateParametricLogic
IMPLICIT NONE
INTEGER :: iObj = 0
INTEGER :: jLine = 0
LOGICAL :: foundParametricLogic = .FALSE.
CHARACTER(len=StringLength) :: curLine
CHARACTER(len=StringLength) :: curExpression
INTEGER :: statementKind = 0
INTEGER :: afterCommand = 0
INTEGER :: parLogLineNum = 0
INTEGER :: assignedParameter = 0
INTEGER :: intCodeStart = 0
INTEGER :: intCodeEnd = 0
INTEGER :: notUsed = 0
LOGICAL :: blankAfterStatement = .FALSE.

IF (verboseDebug) PRINT '(A)',' Started TranslateParametricLogic'
IF (errorCondition) RETURN  !if an error exists do not run this routine
! find the Parametric:Logic object
DO iObj = 1, numIdfObject
  IF (IdfObject(iObj)%kindObj .EQ. koLogic) THEN
    IF (.NOT. foundParametricLogic) THEN
      foundParametricLogic = .TRUE. !set flag in case a second object is found
      numParLogLine = IdfObject(iObj)%lastField - IdfObject(iObj)%firstField
      ALLOCATE(ParLogLine(numParLogLine))
      DO jLine = 1,numParLogLine
        curLine = IdfField(IdfObject(iObj)%firstField+jLine)
        ParLogLine(jLine)%idfFieldNum = IdfObject(iObj)%firstField+jLine
        CALL determineStatement(curLine,statementKind,afterCommand)
        ParLogLine(jLine)%statementKind = statementKind
        curExpression = curLine((afterCommand+1):)
        blankAfterStatement = (LEN_TRIM(curExpression) .EQ. 0)
        SELECT CASE (statementKind)
          CASE(ksNone)
            CALL AddToErrMsg('Invalid statement. Line ignored: ' // TRIM(curLine) ,msgWarning,0)
          CASE(ksAssignment)
            !for assignments the afterCommand should be pointing to the equals sign
            assignedParameter = LookupParameterSymbol(curLine(1:(afterCommand-1)))
            IF (assignedParameter .GE. 1) THEN
              ParLogLine(jLine)%assignParam = assignedParameter
              CALL expressionToRPN(curExpression,ParLogLine(jLine)%startIntCode,ParLogLine(jLine)%endIntCode)
            ELSE
              CALL AddToErrMsg('Parameter being assigned was not found. Line ignored: ' // TRIM(curLine) ,msgWarning,0)
            ENDIF
          CASE(ksParameter)
            !  do nothing - these were scanned during the GatherParameterSymbols routine
          CASE(ksIf,ksElseIf,ksSelect)
            CALL expressionToRPN(curExpression,ParLogLine(jLine)%startIntCode,ParLogLine(jLine)%endIntCode)
          CASE(ksElse,ksEndIf,ksDefault,ksEndSelect)
            IF (.NOT. blankAfterStatement) THEN
              CALL AddToErrMsg('No text should appear after the statement. The extra text is ignored: ' // TRIM(curLine) ,msgWarning,0)
            END IF
          CASE(ksCase)
            CALL getConstants(curExpression,ParLogLine(jLine)%symbolA)
            IF (ParLogLine(jLine)%symbolA .EQ. 0) THEN
              CALL AddToErrMsg('Invalid expression - a constant was expected. The line is ignored: ' // TRIM(curLine) ,msgWarning,0)
            END IF
          CASE(ksDisable,ksEnable)
            CALL getConstants(curExpression,ParLogLine(jLine)%symbolA,ParLogLine(jLine)%symbolB)
            IF (ParLogLine(jLine)%symbolA .EQ. 0) THEN
              CALL AddToErrMsg('Invalid - an object name was expected. The line is ignored: ' // TRIM(curLine) ,msgWarning,0)
            END IF
            ParLogLine(jLine)%objLine = getObjectReference(ParLogLine(jLine)%symbolA,ParLogLine(jLine)%symbolB)
          CASE(ksRemark)
            !  do nothing - the remaining text is a remark
        END SELECT
      END DO
    ELSE
      CALL AddToErrMsg('Only one Parametric:Logic object can be used.',msgError,0)
    END IF
  END IF
END DO
END SUBROUTINE TranslateParametricLogic

!----------------------------------------------------------------------------------
! Translate all expressions that are embedded in objects
!----------------------------------------------------------------------------------
SUBROUTINE TranslateEmbeddedExpressions
IMPLICIT NONE
INTEGER :: iFndExp
IF (verboseDebug) PRINT '(A)',' Started TranslateEmbeddedExpressions'
IF (errorCondition) RETURN  !if an error exists do not run this routine
DO iFndExp = 1, numFoundExpression
  CALL expressionToRPN(FoundExpression(iFndExp)%text,FoundExpression(iFndExp)%startIntCode,FoundExpression(iFndExp)%endIntCode)
END DO
END SUBROUTINE TranslateEmbeddedExpressions

!----------------------------------------------------------------------------------
! Set the values from Parametric:SetValuesForRun to specific value for the specific
! run (or case)
!----------------------------------------------------------------------------------
SUBROUTINE SetValueForCase(caseIn)
IMPLICIT NONE
INTEGER, INTENT(IN) :: caseIn
INTEGER :: iSym

IF (verboseDebug) PRINT '(A,I4)',' Started SetValueForCase: ',caseIn
IF (errorCondition) RETURN  !if an error exists do not run this routine
DO iSym = 1, lastSymbolParameter
  Symbol(iSym)%val = valForRun(iSym,caseIn)
  IF (VERIFY(TRIM(valForRun(iSym,caseIn)),'-0123456789.DE+') .EQ. 0) THEN
    Symbol(iSym)%isRealNum = .TRUE.
    Symbol(iSym)%valAsReal = StringToReal(valForRun(iSym,caseIn))
  ELSE
    Symbol(iSym)%isRealNum = .FALSE.
  END IF
END DO
END SUBROUTINE SetValueForCase

!----------------------------------------------------------------------------------
! Execute the intermediate code for each line of the Parametric:Logic lines.
!----------------------------------------------------------------------------------
SUBROUTINE ComputeParametricLogic
IMPLICIT NONE
INTEGER :: iLine = 0
INTEGER :: curAssignParam = 0
CHARACTER(len=StringLength) :: resultString = ''
REAL(8) :: resultReal = 0.
LOGICAL :: resultIsReal = .FALSE.
INTEGER :: curStrucMode = 0
INTEGER :: curObjLine = 0
INTEGER :: junk = 0
INTEGER :: curSym = 0
INTEGER :: jObjLines = 0

IF (verboseDebug) PRINT '(A)',' Started ComputeParametricLogic'
IF (errorCondition) RETURN  !if an error exists do not run this routine
IF (.NOT. ALLOCATED(ParLogLine)) RETURN
! reset all object lines to be included
DO jObjLines = 1,numObjectLines
  ObjectLines(jObjLines)%enabled = .TRUE.
END DO
CALL PushStrucStack(ssmNotInStruc)
DO iLine = 1,numParLogLine
  curStrucMode = CurrentStrucStackTop()
  SELECT CASE (ParLogLine(iLine)%statementKind)
    CASE(ksNone,ksParameter,ksRemark)
      !skip these lines
    CASE(ksAssignment)
      SELECT CASE (curStrucMode)
        CASE (ssmNotInStruc,ssmDoThen,ssmDoElse,ssmDoCase)
          CALL EvaluateExpression(ParLogLine(iLine)%startIntCode,ParLogLine(iLine)%endIntCode,resultString,resultReal,resultIsReal)
          curAssignParam = ParLogLine(iLine)%assignParam
          Symbol(curAssignParam)%val = resultString
          IF (resultIsReal) THEN
            Symbol(curAssignParam)%valAsReal = resultReal
            Symbol(curAssignParam)%isRealNum = .TRUE.
          ENDIF
      END SELECT
    CASE(ksDisable)
      SELECT CASE (curStrucMode)
        CASE (ssmNotInStruc,ssmDoThen,ssmDoElse,ssmDoCase)
          curObjLine = ParLogLine(iLine)%objLine
          IF ((curObjLine .GE. 1) .AND. (curObjLine .LE. numObjectLines)) THEN
            ObjectLines(curObjLine)%enabled = .FALSE.
          ELSE
            CALL AddToErrMsg('Programming Error - invalid reference to ObjectLine (Disable).',msgWarning,0)
          END IF
      END SELECT
    CASE(ksEnable)
      SELECT CASE (curStrucMode)
        CASE (ssmNotInStruc,ssmDoThen,ssmDoElse,ssmDoCase)
          curObjLine = ParLogLine(iLine)%objLine
          IF ((curObjLine .GE. 1) .AND. (curObjLine .LE. numObjectLines)) THEN
            ObjectLines(curObjLine)%enabled = .TRUE.
          ELSE
            CALL AddToErrMsg('Programming Error - invalid reference to ObjectLine (Enable).',msgWarning,0)
          END IF
      END SELECT
! structure related
    CASE(ksIf)
      SELECT CASE (curStrucMode)
        CASE (ssmNotInStruc,ssmDoThen,ssmDoElse,ssmDoCase)
          CALL EvaluateExpression(ParLogLine(iLine)%startIntCode,ParLogLine(iLine)%endIntCode,resultString,resultReal,resultIsReal)
          IF (isResultTrue(resultString)) THEN
            CALL PushStrucStack(ssmDoThen)
          ELSE
            CALL PushStrucStack(ssmSkipToElse)
          END IF
        CASE (ssmSkipToElse,ssmSkipToEndIf,ssmSkipToEndSelect,ssmFindCase)
          CALL PushStrucStack(ssmSkipToEndIf)
      END SELECT
    CASE(ksElseIf)
      SELECT CASE (curStrucMode)
        CASE (ssmDoThen) !jump to end of 'if' block
          CALL ReplaceTopStrucStack(ssmSkipToEndIf)
        CASE (ssmSkipToElse)
          CALL EvaluateExpression(ParLogLine(iLine)%startIntCode,ParLogLine(iLine)%endIntCode,resultString,resultReal,resultIsReal)
          IF (isResultTrue(resultString)) THEN
            CALL ReplaceTopStrucStack(ssmDoThen)
          ELSE
            CALL ReplaceTopStrucStack(ssmSkipToElse) !not needed but better to show explicitly
          END IF
        CASE (ssmNotInStruc,ssmFindCase,ssmDoCase,ssmDoElse)
          CALL AddToErrMsg('Not expecting ElseIf statement.',msgWarning,0)
      END SELECT
    CASE(ksElse)
      SELECT CASE (curStrucMode)
        CASE (ssmDoThen) !jump to end of 'if' block
          CALL ReplaceTopStrucStack(ssmSkipToEndIf)
        CASE (ssmSkipToElse)
          CALL ReplaceTopStrucStack(ssmDoElse)
        CASE (ssmNotInStruc,ssmFindCase,ssmDoCase,ssmDoElse)
          CALL AddToErrMsg('Not expecting Else statement.',msgWarning,0)
      END SELECT
    CASE(ksEndIf)
      SELECT CASE (curStrucMode)
        CASE (ssmDoThen,ssmDoElse,ssmSkipToEndIf,ssmSkipToElse)
          junk = PopStrucStack()
        CASE (ssmNotInStruc,ssmFindCase,ssmDoCase,ssmSkipToEndSelect)
          CALL AddToErrMsg('Not expecting EndIf statement.',msgWarning,0)
      END SELECT
    CASE(ksSelect)
      SELECT CASE (curStrucMode)
        CASE (ssmNotInStruc,ssmDoThen,ssmDoElse,ssmDoCase)
          CALL EvaluateExpression(ParLogLine(iLine)%startIntCode,ParLogLine(iLine)%endIntCode,resultString,resultReal,resultIsReal)
          CALL PushStrucStack(ssmFindCase,resultString)
        CASE (ssmFindCase)
          CALL AddToErrMsg('Expecting case statement.',msgWarning,0)
        CASE (ssmSkipToElse,ssmSkipToEndIf,ssmSkipToEndSelect)
          CALL PushStrucStack(ssmSkipToEndSelect)
      END SELECT
    CASE(ksCase)
      SELECT CASE (curStrucMode)
        CASE (ssmFindCase)
          curSym = ParLogLine(iLine)%symbolA
          IF ((curSym .GE. 1) .AND. (curSym .LE. numSymbol)) THEN
            IF (DoesTopMatch(Symbol(curSym)%val)) THEN
              CALL ReplaceTopStrucStack(ssmDoCase)
            END IF
          END IF
        CASE (ssmDoCase)
          CALL ReplaceTopStrucStack(ssmSkipToEndSelect)
        CASE (ssmNotInStruc,ssmDoThen,ssmSkipToElse,ssmDoElse)
          CALL AddToErrMsg('Not expecting Case statement.',msgWarning,0)
      END SELECT
    CASE(ksDefault)
      SELECT CASE (curStrucMode)
        CASE (ssmFindCase)
          CALL ReplaceTopStrucStack(ssmDoCase)
        CASE (ssmDoCase)
          CALL ReplaceTopStrucStack(ssmSkipToEndSelect)
        CASE (ssmNotInStruc,ssmDoThen,ssmSkipToElse,ssmDoElse)
          CALL AddToErrMsg('Not expecting Default statement.',msgWarning,0)
      END SELECT
    CASE(ksEndSelect)
      SELECT CASE (curStrucMode)
        CASE (ssmFindCase,ssmDoCase,ssmSkipToEndSelect)
          junk = PopStrucStack()
        CASE (ssmNotInStruc,ssmDoThen,ssmDoElse,ssmSkipToEndIf,ssmSkipToElse)
          CALL AddToErrMsg('Not expecting EndSelect statement.',msgWarning,0)
      END SELECT
  END SELECT
END DO
END SUBROUTINE ComputeParametricLogic

!----------------------------------------------------------------------------------
! Execute the intermediate code for each embedding expression.
!----------------------------------------------------------------------------------
SUBROUTINE ComputeEmbeddedExpressions
IMPLICIT NONE
INTEGER :: iFndExp
REAL(8) :: resultReal = 0.
LOGICAL :: resultIsReal = .FALSE.
IF (verboseDebug) PRINT '(A)',' Started ComputeEmbeddedExpressions'
IF (errorCondition) RETURN  !if an error exists do not run this routine
DO iFndExp = 1, numFoundExpression
  CALL EvaluateExpression(FoundExpression(iFndExp)%startIntCode,FoundExpression(iFndExp)%endIntCode,&
                          FoundExpression(iFndExp)%expResult,resultReal,resultIsReal)
END DO
END SUBROUTINE ComputeEmbeddedExpressions

!----------------------------------------------------------------------------------
! Open the intermediate file for reading and one of the output files for writing
!----------------------------------------------------------------------------------
SUBROUTINE OpenFileSecondPass(caseIn)
IMPLICIT NONE
INTEGER, INTENT(IN) :: caseIn
CHARACTER(len=LongString) :: TempFileName=' '
LOGICAL :: TempFileExist=.false.
INTEGER :: status
IF (verboseDebug) PRINT '(A)',' Started OpenFileSecondPass'
IF (errorCondition) RETURN  !if an error exists do not run this routine
TempFileName = TRIM(FilePathOnly) // 'parametric.int'
OutputFileName = TRIM(OutputFileNameRoot) // '-' // TRIM(cases(caseIn)%suffix) // '.idf'
INQUIRE(file=TempFileName,exist=TempFileExist)
IF (TempFileExist) THEN
  OPEN (unit=tFH, file=TRIM(TempFileName),action='READ',iostat=status)
  IF (status .NE. 0) CALL AddToErrMsg('error opening to read intermediate file named parametric.int',msgError,status)
  OPEN (unit=oFH, file=TRIM(OutputFileName), action='WRITE',iostat=status)
  IF (status .NE. 0) CALL AddToErrMsg('error opening output file' // TRIM(OutputFileName),msgError,status)
  IF (verboseDebug) PRINT '(A,A)','   Output file:',TRIM(OutputFileName)
ELSE
  call AddToErrMsg('error opening intermediate file - no file found',msgError,0)
END IF
END SUBROUTINE OpenFileSecondPass


!----------------------------------------------------------------------------------
! Copy the intermediate file to the output file but substitute values for
! embedded expressions and remove disabled objects
!----------------------------------------------------------------------------------
SUBROUTINE SubstituteValues
IMPLICIT NONE
INTEGER :: read_stat = 0
CHARACTER(len=LongString) :: LineIn
CHARACTER(len=LongString) :: LineOut
INTEGER :: lineCount = 0
INTEGER :: curFoundExpression = 0
INTEGER :: curObjectLines = 0
INTEGER :: nextLineWithExpression = 0
INTEGER :: nextFirstLineDisabledObj = 0
INTEGER :: nextLastLineDisabledObj = 0
INTEGER :: nextSpecialLine = 0
LOGICAL :: writeLine = .TRUE.
INTEGER :: found = 0
INTEGER :: pos
IF (verboseDebug) PRINT '(A)',' Started SubstituteValues'
IF (errorCondition) RETURN  !if an error exists do not run this routine
lineCount = 0
read_stat = 0
! get the intial special line
CALL getNextFoundExpression(reset,curFoundExpression,nextLineWithExpression)
CALL getNextDisabledObjectLines(reset,curObjectLines,nextFirstLineDisabledObj,nextLastLineDisabledObj)
writeLine = .TRUE.
DO WHILE (read_stat == 0)  ! not end of file
  READ(UNIT=tFH, FMT="(A)", IOSTAT=read_stat) LineIn
  IF (read_stat /= 0) EXIT
  pos=INDEX(LineIn,tabChar)
  DO WHILE (pos > 0)
    LineIn(pos:pos)=' '
    pos=INDEX(LineIn,tabChar)
  ENDDO
  lineCount = lineCount + 1 !count of lines in intermediate file
  IF (verboseDebug) THEN
    IF (MOD(lineCount,500) .EQ. 1) PRINT '(4X,I6,2X,A)',lineCount,TRIM(LineIn)
  END IF
  IF (lineCount .EQ. nextLineWithExpression) THEN
    LineOut = LineIn
    DO
      found = INDEX(LineOut,'=@@' // IntToStr6(curFoundExpression))
      IF (found .GE. 1) THEN
        !substitute the result for the expression
        LineOut = LineOut(1:(found-1)) // TRIM(FoundExpression(curFoundExpression)%expResult) // LineOut((found + 9):)
      ELSE
        LineOut = LineIn !use input line without substitution
        CALL AddToErrMsg('Programming Error - Did not find correct expression reference.' // TRIM(LineIn),msgWarning,curFoundExpression)
      END IF
      CALL getNextFoundExpression(lineCount,curFoundExpression,nextLineWithExpression)
      IF (nextLineWithExpression .NE. lineCount) EXIT
    END DO
  ELSE
    LineOut = LineIn !for most lines just echo the input line
  END IF
  IF (lineCount .EQ. nextFirstLineDisabledObj) THEN
    writeLine = .FALSE.
  ELSEIF (lineCount .EQ. (nextLastLineDisabledObj + 1)) THEN
    writeLine = .TRUE.
    CALL getNextDisabledObjectLines(lineCount,curObjectLines,nextFirstLineDisabledObj,nextLastLineDisabledObj)
  END IF
  ! if object is disabled, then don't write the line
  IF (writeLine) THEN
    WRITE(oFH,'(A)')  TRIM(LineOut)
  END IF
END DO
IF (verboseDebug) PRINT '(A,I6,2X,I6,2X,I6)',' In SubstituteValues, done copying the intermediate file. tFH/oFH/Line: ', tFH,oFH,lineCount
END SUBROUTINE SubstituteValues

!----------------------------------------------------------------------------------
! Close the IDF file and the intermediate file
!----------------------------------------------------------------------------------
SUBROUTINE CloseFileSecondPass
IMPLICIT NONE
IF (verboseDebug) PRINT '(A)',' Started CloseFileSecondPass'
CLOSE(oFH)
CLOSE(tFH)
END SUBROUTINE

!----------------------------------------------------------------------------------
! Write to the error file and close it. If no errors are reported, delete the error file.
!----------------------------------------------------------------------------------
SUBROUTINE WriteErrorsAndCloseFile
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
INTEGER :: iError = 0
LOGICAL :: wroteErrorHeader = .FALSE.
LOGICAL :: wroteWarningHeader = .FALSE.
! write errors first
DO iError = 1,numErrMsgs
  IF (ErrMsgs(numErrMsgs)%msgKind .EQ. msgError) THEN
    IF (.NOT. wroteErrorHeader) THEN
      wroteErrorHeader = .TRUE.
      WRITE(ErrFH, '(A)') '==================='
      WRITE(ErrFH, '(A)') '   ERRORS          '
      WRITE(ErrFH, '(A)') '==================='
    END IF
    WRITE(ErrFH, '(A)') '-------------------------------------------------------------------------'
    WRITE(ErrFH, '(A,A)') '      ', TRIM(ErrMsgs(numErrMsgs)%msgText)
    WRITE(ErrFH, '(A,I4)') '      Number ',ErrMsgs(numErrMsgs)%msgErrNum
    WRITE(ErrFH, '(A,A)') '      Context ', TRIM(ErrMsgs(numErrMsgs)%msgContext)
  END IF
END DO
! now write warnings
DO iError = 1,numErrMsgs
  IF (ErrMsgs(numErrMsgs)%msgKind .EQ. msgWarning) THEN
    IF (.NOT. wroteWarningHeader) THEN
      wroteWarningHeader = .TRUE.
      WRITE(ErrFH, '(A)') '==================='
      WRITE(ErrFH, '(A)') '   WARNINGS        '
      WRITE(ErrFH, '(A)') '==================='
    END IF
    WRITE(ErrFH, '(A)') '-------------------------------------------------------------------------'
    WRITE(ErrFH, '(A,A)') '      ', TRIM(ErrMsgs(numErrMsgs)%msgText)
    WRITE(ErrFH, '(A,I4)') '      Number ',ErrMsgs(numErrMsgs)%msgErrNum
    WRITE(ErrFH, '(A,A)') '      Context ', TRIM(ErrMsgs(numErrMsgs)%msgContext)
  END IF
END DO
IF (numErrMsgs .GT. 0) THEN
  CLOSE(ErrFH)
ELSE
  CLOSE(ErrFH,STATUS='DELETE')  !get rid of error file if no errors found
END IF
END SUBROUTINE WriteErrorsAndCloseFile


!=====================================================================================
!=====================================================================================
!
!    NON-GENERIC SUPPORT ROUTINES
!
!=====================================================================================
!=====================================================================================

!----------------------------------------------------------------------------------
! Get the next disabled object and return the index to the ObjectLines array
! as well as the first and last line numbers of the object. The returned index is the first
! object after the lineCountIn line number that is disabled. If no other objects exist return
! zeros. A special lineCountIn value of "reset" sets the count to one.
!----------------------------------------------------------------------------------
SUBROUTINE getNextDisabledObjectLines(lineCountIn,foundObjectLinesOut,firstLineOut,lastLineOut)
INTEGER, INTENT(IN) :: lineCountIn
INTEGER, INTENT(OUT) :: foundObjectLinesOut
INTEGER, INTENT(OUT) :: firstLineOut
INTEGER, INTENT(OUT) :: lastLineOut
INTEGER, SAVE :: indexObjectLines = 1
INTEGER :: i
IF (numObjectLines .GE. 1) THEN
  IF (lineCountIn .EQ. reset) THEN
    indexObjectLines = 1
  !ELSE
  !  indexObjectLines = indexObjectLines + 1
  END IF
  DO i = 1,5000 !run away condition prevention
    indexObjectLines = indexObjectLines + 1
    IF (indexObjectLines .LE. numObjectLines) THEN
      IF (.NOT. ObjectLines(indexObjectLines)%enabled) THEN
        IF (ObjectLines(indexObjectLines)%firstLine .GT. lineCountIn) THEN
          foundObjectLinesOut = indexObjectLines
          firstLineOut = ObjectLines(indexObjectLines)%firstLine
          lastLineOut = ObjectLines(indexObjectLines)%lastLine
          EXIT
        END IF
      END IF
    ELSE
      foundObjectLinesOut = 0
      firstLineOut = ObjectLines(indexObjectLines)%firstLine
      lastLineOut = ObjectLines(indexObjectLines)%lastLine
      EXIT
    END IF
  END DO
ELSE
  foundObjectLinesOut = 0
  firstLineOut = 0
  lastLineOut = 0
END IF
END SUBROUTINE getNextDisabledObjectLines

!----------------------------------------------------------------------------------
! Get the next found expression and return the index to the foundExprssion array
! as well as the line number of the next expression. The returned index is the first
! expression after the lineCountIn line number. If no other expressions exist return
! zeros. A special lineCountIn value of "reset" sets the count to one.
!----------------------------------------------------------------------------------
SUBROUTINE getNextFoundExpression(lineCountIn,foundExpressionOut,lineNumberOut)
INTEGER, INTENT(IN) :: lineCountIn
INTEGER, INTENT(OUT) :: foundExpressionOut
INTEGER, INTENT(OUT) :: lineNumberOut
INTEGER, SAVE :: indexFoundExpression = 1
INTEGER :: i
IF (numFoundExpression .GE. 1) THEN
  IF (lineCountIn .EQ. reset) THEN
    indexFoundExpression = 1
    foundExpressionOut = indexFoundExpression
    lineNumberOut = FoundExpression(indexFoundExpression)%line
  ELSE
    DO i = 1,1000 !run away condition prevention
      indexFoundExpression = indexFoundExpression + 1
      IF (indexFoundExpression .LE. numFoundExpression) THEN
        IF (FoundExpression(indexFoundExpression)%line .GE. lineCountIn) THEN  !changed from .GT. to .GE. Sep 2010
          foundExpressionOut = indexFoundExpression
          lineNumberOut = FoundExpression(indexFoundExpression)%line
          EXIT
        END IF
      ELSE
        foundExpressionOut = 0
        lineNumberOut = 0
        EXIT
      END IF
    END DO
  END IF
ELSE
  foundExpressionOut = 0
  lineNumberOut = 0
END IF
END SUBROUTINE

!----------------------------------------------------------------------------------
! Evaluate an expression already in intermediate code and return result
!----------------------------------------------------------------------------------
SUBROUTINE EvaluateExpression(firstIntCodeIn,lastIntCodeIn,resultStringOut,resultRealOut,resultIsRealOut)
IMPLICIT NONE
INTEGER, INTENT(IN) :: firstIntCodeIn
INTEGER, INTENT(IN) :: lastIntCodeIn
CHARACTER(len=*),INTENT(OUT) :: resultStringOut
REAL(8), INTENT(OUT) :: resultRealOut
LOGICAL, INTENT(OUT) :: resultIsRealOut
INTEGER :: iIntCode = 0
REAL(8) :: realX = 0.
REAL(8) :: realY = 0.
CHARACTER(len=StringLength) :: stringX = ''
CHARACTER(len=StringLength) :: stringY = ''
LOGICAL :: booleanX = .FALSE.
LOGICAL :: booleanY = .FALSE.

IF ((firstIntCodeIn .GE. 1) .AND. (firstIntCodeIn .LE. numIntCode) .AND. &
   (lastIntCodeIn .GE. 1) .AND. (lastIntCodeIn .LE. numIntCode)) THEN
  DO iIntCode = firstIntCodeIn, lastIntCodeIn
    SELECT CASE (IntCode(iIntCode))
      CASE (1:) !all positive values are references to the symbol table
        CALL PushSymbolOnEvalStack(IntCode(iIntCode))
      CASE (tokNum,tokStr,tokID,tokRtParen,tokFunc,tokLtParen,tokTilde,tokNone,tokINVALID)
        CALL AddToErrMsg('Programming Error - invalid token found while evaluating expression.',msgWarning,0)
      CASE (tokPlus)
        realX = popEvalReal()
        realY = popEvalReal()
        CALL pushEvalReal(realY + realX)
      CASE (tokMinus)
        realX = popEvalReal()
        realY = popEvalReal()
        CALL pushEvalReal(realY - realX)
      CASE (tokTimes)
        realX = popEvalReal()
        realY = popEvalReal()
        CALL pushEvalReal(realY * realX)
      CASE (tokDiv)
        realX = popEvalReal()
        realY = popEvalReal()
        CALL pushEvalReal(realY / realX)
      CASE (tokExp)
        realX = popEvalReal()
        realY = popEvalReal()
        CALL pushEvalReal(realY ** realX)
      CASE (tokGT)
        IF (isEvalStackTopReal(2)) THEN !check if top two values are both real
          realX = popEvalReal()
          realY = popEvalReal()
          CALL pushEvalLogical(realY .GT. realX)
        ELSE
          stringX = popEvalString()
          stringY = popEvalString()
          CALL pushEvalLogical(stringY .GT. stringX)
        END IF
      CASE (tokEQ)
        IF (isEvalStackTopReal(2)) THEN !check if top two values are both real
          realX = popEvalReal()
          realY = popEvalReal()
          CALL pushEvalLogical(realY .EQ. realX)
        ELSE
          stringX = popEvalString()
          stringY = popEvalString()
          CALL pushEvalLogical(stringY .EQ. stringX)
        END IF
      CASE (tokLT)
        IF (isEvalStackTopReal(2)) THEN !check if top two values are both real
          realX = popEvalReal()
          realY = popEvalReal()
          CALL pushEvalLogical(realY .LT. realX)
        ELSE
          stringX = popEvalString()
          stringY = popEvalString()
          CALL pushEvalLogical(stringY .LT. stringX)
        END IF
      CASE (tokGE)
        IF (isEvalStackTopReal(2)) THEN !check if top two values are both real
          realX = popEvalReal()
          realY = popEvalReal()
          CALL pushEvalLogical(realY .GE. realX)
        ELSE
          stringX = popEvalString()
          stringY = popEvalString()
          CALL pushEvalLogical(stringY .GE. stringX)
        END IF
      CASE (tokLE)
        IF (isEvalStackTopReal(2)) THEN !check if top two values are both real
          realX = popEvalReal()
          realY = popEvalReal()
          CALL pushEvalLogical(realY .LE. realX)
        ELSE
          stringX = popEvalString()
          stringY = popEvalString()
          CALL pushEvalLogical(stringY .LE. stringX)
        END IF
      CASE (tokNE)
        IF (isEvalStackTopReal(2)) THEN !check if top two values are both real
          realX = popEvalReal()
          realY = popEvalReal()
          CALL pushEvalLogical(realY .GT. realX)
        ELSE
          stringX = popEvalString()
          stringY = popEvalString()
          CALL pushEvalLogical(stringY .GT. stringX)
        END IF
      CASE (tokAnd)
        booleanX = popEvalLogical()
        booleanY = popEvalLogical()
        CALL pushEvalLogical(booleanY .AND. booleanX)
      CASE (tokOr)
        booleanX = popEvalLogical()
        booleanY = popEvalLogical()
        CALL pushEvalLogical(booleanY .OR. booleanX)
      CASE (tokUnNeg)
        realX = popEvalReal()
        CALL pushEvalReal(-realX)
      CASE (tokFuncABS)
        realX = popEvalReal()
        CALL pushEvalReal(ABS(realX))
      CASE (tokFuncACOS)
        realX = popEvalReal()
        CALL pushEvalReal(ACOS(realX))
      CASE (tokFuncASIN)
        realX = popEvalReal()
        CALL pushEvalReal(ASIN(realX))
      CASE (tokFuncATAN)
        realX = popEvalReal()
        CALL pushEvalReal(ATAN(realX))
      CASE (tokFuncCOS)
        realX = popEvalReal()
        CALL pushEvalReal(COS(realX))
      CASE (tokFuncEXP)
        realX = popEvalReal()
        CALL pushEvalReal(EXP(realX))
      CASE (tokFuncINT)
        realX = popEvalReal()
        CALL pushEvalReal(REAL(INT(realX),8))
      CASE (tokFuncLEN)
        stringX = popEvalString()
        CALL pushEvalReal(REAL(LEN_TRIM(stringX),8))
      CASE (tokFuncLOG)
        realX = popEvalReal()
        CALL pushEvalReal(LOG(realX))
      CASE (tokFuncMOD)
        realX = popEvalReal()
        realY = popEvalReal()
        CALL pushEvalReal(MOD(realY,realX))
      CASE (tokFuncNOT)
        booleanX = popEvalLogical()
        CALL pushEvalLogical(.NOT. booleanX)
      CASE (tokFuncSIN)
        realX = popEvalReal()
        CALL pushEvalReal(SIN(realX))
      CASE (tokFuncSQRT)
        realX = popEvalReal()
        CALL pushEvalReal(SQRT(realX))
      CASE (tokFuncTAN)
        realX = popEvalReal()
        CALL pushEvalReal(TAN(realX))
      CASE DEFAULT
        CALL AddToErrMsg('Programming Error - token not recognized while evaluating expression.',msgWarning,0)
    END SELECT
  END DO
  IF (topEvalStack .GE. 1) THEN
    resultIsRealOut = EvalStack(topEvalStack)%isRealNum
    resultRealOut = EvalStack(topEvalStack)%valAsReal
    IF (resultIsRealOut) THEN
      resultStringOut = ADJUSTL(EvalStack(topEvalStack)%val)
    ELSE
      resultStringOut = EvalStack(topEvalStack)%val
    END IF
    topEvalStack = topEvalStack - 1
  ELSE
    resultStringOut = ''
    resultRealOut = 0.0
    resultIsRealOut = .FALSE.
    CALL AddToErrMsg('Programming Error - evaluation stack empty.',msgWarning,0)
  END IF
ELSE
  resultStringOut = ''
  resultRealOut = 0.0
  resultIsRealOut = .FALSE.
  CALL AddToErrMsg('Programming Error - token not recognized while evaluating expression.',msgWarning,0)
END IF
END SUBROUTINE EvaluateExpression

!----------------------------------------------------------------------------------
! Add one top location in the evaluation stack
! Used by the PushEval subroutines.
!----------------------------------------------------------------------------------
SUBROUTINE AddToEvalStack
IMPLICIT NONE
IF (.NOT. ALLOCATED(EvalStack)) THEN
  ALLOCATE(EvalStack(sizeEvalStack))
  topEvalStack = 1
ELSE
  topEvalStack = topEvalStack + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (topEvalStack .GT. sizeEvalStack) THEN
    ALLOCATE(EvalStackCopy(sizeEvalStack))
    EvalStackCopy = EvalStack
    DEALLOCATE(EvalStack)
    ! double the size of the array
    ALLOCATE(EvalStack(sizeEvalStack * 2))
    EvalStack(1:sizeEvalStack) = EvalStackCopy
    DEALLOCATE(EvalStackCopy)
    sizeEvalStack = sizeEvalStack * 2
  END IF
END IF
END SUBROUTINE AddToEvalStack

!----------------------------------------------------------------------------------
! Add the symbol value (parameter or constant) onto the evaluation stack
!----------------------------------------------------------------------------------
SUBROUTINE PushSymbolOnEvalStack(symbolRefIn)
IMPLICIT NONE
INTEGER, INTENT(IN) :: symbolRefIn
IF ((symbolRefIn .GE. 1) .AND. (symbolRefIn .LE. numSymbol)) THEN
  CALL AddToEvalStack
  EvalStack(topEvalStack)%val = Symbol(symbolRefIn)%val
  EvalStack(topEvalStack)%isRealNum = Symbol(symbolRefIn)%isRealNum
  EvalStack(topEvalStack)%valAsReal = Symbol(symbolRefIn)%valAsReal
ELSE
  CALL AddToErrMsg('Programming Error - invalid symbol reference.',msgWarning,0)
END IF
END SUBROUTINE

!----------------------------------------------------------------------------------
! Take the top value from the evaluation stack as a real (numeric) value
!----------------------------------------------------------------------------------
REAL(8) FUNCTION popEvalReal()
IMPLICIT NONE
IF (topEvalStack .GE. 1) THEN
  IF (EvalStack(topEvalStack)%isRealNum) THEN
    popEvalReal = EvalStack(topEvalStack)%valAsReal
  ELSE
    CALL AddToErrMsg('Real value expected but not found on evaluation stack.',msgWarning,0)
    popEvalReal = 0
  END IF
  topEvalStack = topEvalStack - 1
ELSE
  popEvalReal = 0
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Add a real (numeric) value onto the evaluation stack
!----------------------------------------------------------------------------------
SUBROUTINE pushEvalReal(realIn)
IMPLICIT NONE
REAL(8), INTENT(IN) :: realIn
CALL AddToEvalStack
EvalStack(topEvalStack)%val = RealToString(realIn)
EvalStack(topEvalStack)%isRealNum = .TRUE.
EvalStack(topEvalStack)%valAsReal = realIn
END SUBROUTINE

!----------------------------------------------------------------------------------
! Take the top value from the evaluation stack as a logical value (True or False)
!----------------------------------------------------------------------------------
LOGICAL FUNCTION popEvalLogical()
IMPLICIT NONE
IF (topEvalStack .GE. 1) THEN
  IF (EvalStack(topEvalStack)%isRealNum) THEN
    popEvalLogical = (EvalStack(topEvalStack)%valAsReal .NE. 0.0)
  ELSE
    popEvalLogical = IsStrEq(EvalStack(topEvalStack)%val(1:1),'T')
  END IF
  topEvalStack = topEvalStack - 1
ELSE
  CALL AddToErrMsg('Logical value expected but not found on evaluation stack.',msgWarning,0)
  popEvalLogical = .FALSE.
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Add logical value (True or False) onto the evaluation stack
!----------------------------------------------------------------------------------
SUBROUTINE pushEvalLogical(logicalIn)
IMPLICIT NONE
LOGICAL, INTENT(IN) :: logicalIn
CALL AddToEvalStack
IF (logicalIn) THEN
  EvalStack(topEvalStack)%val = 'True'
  EvalStack(topEvalStack)%isRealNum = .FALSE.
  EvalStack(topEvalStack)%valAsReal = 1.0
  IF (verboseDebug) PRINT '(A)','pushEvalLogical - TRUE'
ELSE
  EvalStack(topEvalStack)%val = 'False'
  EvalStack(topEvalStack)%isRealNum = .FALSE.
  EvalStack(topEvalStack)%valAsReal = 0.0
  IF (verboseDebug) PRINT '(A)','pushEvalLogical - FALSE'
END IF
END SUBROUTINE

!----------------------------------------------------------------------------------
! Take the top value from the evaluation stack as a character string value
!----------------------------------------------------------------------------------
FUNCTION popEvalString() RESULT (stringOut)
IMPLICIT NONE
CHARACTER(len=LongString) :: stringOut
IF (topEvalStack .GE. 1) THEN
  stringOut = EvalStack(topEvalStack)%val
  topEvalStack = topEvalStack - 1
ELSE
  stringOut = ''
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Test of the top values of the stack are real numeric values
!----------------------------------------------------------------------------------
LOGICAL FUNCTION isEvalStackTopReal(numValsIn)
IMPLICIT NONE
INTEGER, INTENT(IN) :: numValsIn
INTEGER :: iStack
INTEGER :: firstItem
IF ((numValsIn .GE. 1) .AND. (numValsIn .LE. topEvalStack)) THEN
  ! look through the top of the stack for non-real values
  firstItem = topEvalStack - numValsIn + 1
  isEvalStackTopReal = .TRUE.
  DO iStack = firstItem, topEvalStack
    IF (.NOT. EvalStack(topEvalStack)%isRealNum) THEN
      isEvalStackTopReal = .FALSE.
      EXIT
    END IF
  END DO
ELSE
  CALL AddToErrMsg('Programming Error - checking top of eval stack for reals failed.',msgWarning,0)
  isEvalStackTopReal = .FALSE.
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Test if the result from an evaulated expression is True or False
!----------------------------------------------------------------------------------
LOGICAL FUNCTION isResultTrue(resultIn)
IMPLICIT NONE
CHARACTER(len=*),INTENT(IN) :: resultIn
!any text starting with T is considered true, everything else is false
isResultTrue = IsStrEq(resultIn(1:1),'T')
END FUNCTION

!----------------------------------------------------------------------------------
! Push the current structure mode to the stack
!----------------------------------------------------------------------------------
SUBROUTINE PushStrucStack(modeIn,matchIn)
IMPLICIT NONE
INTEGER,INTENT(IN) :: modeIn
CHARACTER(len=*),INTENT(IN),OPTIONAL :: matchIn

IF (.NOT. ALLOCATED(StrucStack)) THEN
  ALLOCATE(StrucStack(sizeStrucStack))
  topStrucStack = 1
ELSE
  topStrucStack = topStrucStack + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (topStrucStack .GT. sizeStrucStack) THEN
    ALLOCATE(StrucStackCopy(sizeStrucStack))
    StrucStackCopy = StrucStack
    DEALLOCATE(StrucStack)
    ! double the size of the array
    ALLOCATE(StrucStack(sizeStrucStack * 2))
    StrucStack(1:sizeStrucStack) = StrucStackCopy
    DEALLOCATE(StrucStackCopy)
    sizeStrucStack = sizeStrucStack * 2
  END IF
END IF
StrucStack(topStrucStack)%mode = modeIn
IF (PRESENT(matchIn)) THEN
  StrucStack(topStrucStack)%match = matchIn
END IF
END SUBROUTINE PushStrucStack

!----------------------------------------------------------------------------------
! Pop the top mode of the structure stack
!----------------------------------------------------------------------------------
INTEGER FUNCTION PopStrucStack()
IMPLICIT NONE
IF (topStrucStack .GE. 1) THEN
  PopStrucStack = StrucStack(topStrucStack)%mode
  topStrucStack = topStrucStack - 1
ELSE
  PopStrucStack = 0
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Return the mode of the top value of the structure stack
!----------------------------------------------------------------------------------
INTEGER FUNCTION CurrentStrucStackTop()
IMPLICIT NONE
IF (topStrucStack .GE. 1) THEN
  CurrentStrucStackTop = StrucStack(topStrucStack)%mode
ELSE
  CurrentStrucStackTop = 0
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Return the mode of the top value of the structure stack
!----------------------------------------------------------------------------------
SUBROUTINE ReplaceTopStrucStack(modeIn,matchIn)
IMPLICIT NONE
INTEGER,INTENT(IN) :: modeIn
CHARACTER(len=*),INTENT(IN),OPTIONAL :: matchIn

IF (topStrucStack .GE. 1) THEN
  StrucStack(topStrucStack)%mode = modeIn
  IF (PRESENT(matchIn)) THEN
    StrucStack(topStrucStack)%match = matchIn
  END IF
END IF
END SUBROUTINE ReplaceTopStrucStack


!----------------------------------------------------------------------------------
! See if the stack Match string is the same as argument. This is used
! primarily for SELECT structure CASE statements
!----------------------------------------------------------------------------------
LOGICAL FUNCTION DoesTopMatch(argIn)
IMPLICIT NONE
CHARACTER(len=*),INTENT(IN)    :: argIn
IF (topStrucStack .GE. 1) THEN
  DoesTopMatch = IsStrEq(argIn,StrucStack(topStrucStack)%match)
ELSE
  DoesTopMatch = .FALSE.
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Get the reference to the ObjectLines array that contains the references to all of
! the objects in the file based on one or two symbol references provided. This
! is primarily used for ENABLE and DISABLE commands.
!----------------------------------------------------------------------------------
INTEGER FUNCTION getObjectReference(nameOfObjectSymbolRefIn,kindOfObjectSymbolRefIn)
INTEGER, INTENT(IN) :: nameOfObjectSymbolRefIn
INTEGER, INTENT(IN) :: kindOfObjectSymbolRefIn
CHARACTER(len=LongString) :: nameOfObject
CHARACTER(len=LongString) :: kindOfObject
INTEGER :: iObjectLines = 0
INTEGER :: found = 0
INTEGER :: countOfFound = 0
countOfFound = 0
found = 0
! set default value as error condition
getObjectReference = 0
! use strings for search
IF (nameOfObjectSymbolRefIn .GE. 1) THEN
  nameOfObject = Symbol(nameOfObjectSymbolRefIn)%val
END IF
IF (kindOfObjectSymbolRefIn .GE. 1) THEN
  kindOfObject = Symbol(kindOfObjectSymbolRefIn)%val
END IF
! if kind of object is provided then search for exact match
IF (kindOfObjectSymbolRefIn .EQ. 0) THEN
  DO iObjectLines= 1, numObjectLines
    IF (IsStrEq(nameOfObject,ObjectLines(iObjectLines)%nameofObj)) THEN
      found = iObjectLines
      countOfFound = countOfFound + 1
    END IF
  END DO
ELSE !find
  DO iObjectLines= 1, numObjectLines
    IF (verboseDebug) PRINT '(A)','getObjectReference: ' // TRIM(ObjectLines(iObjectLines)%nameofObj) //'::'// TRIM(ObjectLines(iObjectLines)%kindOfObj)
    IF (IsStrEq(nameOfObject,ObjectLines(iObjectLines)%nameofObj)) THEN
      IF (IsStrEq(kindOfObject,ObjectLines(iObjectLines)%kindOfObj)) THEN
        found = iObjectLines
        countOfFound = countOfFound + 1
      END IF
    END IF
  END DO
END IF
IF (countOfFound .EQ. 1) THEN
  getObjectReference = found !this is the expected result
ELSEIF (countOfFound .EQ. 0) THEN
  CALL AddToErrMsg('Reference to object name not found: ' // TRIM(nameOfObject) ,msgWarning,0)
ELSE
  CALL AddToErrMsg('Reference to object name not unique: ' // TRIM(nameOfObject) ,msgWarning,0)
END IF
END FUNCTION getObjectReference

!----------------------------------------------------------------------------------
! Uses the expressionToRPN to identify one or two constants and returns the
! references to them in the symbol array.
!----------------------------------------------------------------------------------
SUBROUTINE getConstants(stringIn,aSymbolOut,bSymbolOut)
CHARACTER(len=*),INTENT(IN) :: stringIn
INTEGER,INTENT(OUT) :: aSymbolOut
INTEGER,INTENT(OUT),OPTIONAL :: bSymbolOut
INTEGER :: startIntCode = 0
INTEGER :: endIntCode = 0
CALL expressionToRPN(stringIn,startIntCode,endIntCode)
IF ((startIntCode .GE. 1) .AND. (endIntCode .GE. startIntCode)) THEN
  ! see if one or two symbols are needed
  IF (PRESENT(bSymbolOut)) THEN
    ! only a is used although b is also returned
    IF (endIntCode .EQ. startIntCode) THEN
      aSymbolOut = IntCode(startIntCode)
      bSymbolOut = 0
    ELSEIF (endIntCode .EQ. (startIntCode + 1)) THEN
      aSymbolOut = IntCode(startIntCode)
      bSymbolOut = IntCode(endIntCode)
    END IF
    IF ((aSymbolOut .LT. 1) .OR. (aSymbolOut .GT. numSymbol)) aSymbolOut = 0
    IF ((bSymbolOut .LT. 1) .OR. (bSymbolOut .GT. numSymbol)) bSymbolOut = 0
  ELSE !just one constant needed
    ! should only be two intcodes long - one for each symbol reference
    IF (endIntCode .EQ. startIntCode) THEN
      aSymbolOut = IntCode(startIntCode)
      ! check if just a symbol
      IF ((aSymbolOut .LT. 1) .OR. (aSymbolOut .GT. numSymbol)) aSymbolOut = 0
    END IF
  END IF
ENDIF
END SUBROUTINE getConstants

!----------------------------------------------------------------------------------
! Convert an expression into a reverse polish notation representation
! Use Edsger W. Dijkstra's Shunting-yard algorithm
! from input string
!----------------------------------------------------------------------------------
SUBROUTINE expressionToRPN(expressionIn,firstIntCodeOut,lastIntCodeOut)
CHARACTER(len=*),INTENT(IN) :: expressionIn
INTEGER,INTENT(OUT) :: firstIntCodeOut
INTEGER,INTENT(OUT) :: lastIntCodeOut
INTEGER :: prevTokenEnd = 0
INTEGER :: newTokenStart = 0
INTEGER :: newTokenEnd = 0
CHARACTER(len=LongString) :: curTokenString = ''
INTEGER :: classToken = tokNone
INTEGER :: prevClassToken = tokNone
CHARACTER(len=LongString) :: outStr = ''
INTEGER :: curPrec = precNoItem
INTEGER :: oldPrec = precNoItem
INTEGER :: oldOp = 0
INTEGER :: i = 0
INTEGER :: curFuncStart = 0
INTEGER :: curFuncEnd = 0
INTEGER :: curID = 0
CHARACTER(len=4) :: opString

errorContext = TRIM(expressionIn)
prevClassToken = tokNone
prevTokenEnd = 0
IF (verboseDebug) PRINT '(A,A)','expressionToRPN expressionIn: ', TRIM(expressionIn)
firstIntCodeOut = numIntCode + 1
DO
  CALL GetNextToken(expressionIn,prevTokenEnd,prevClassToken,newTokenStart,newTokenEnd,classToken)
  IF (newTokenEnd .EQ. 0) EXIT
  curTokenString = expressionIn(newTokenStart:newTokenEnd)
  opString = opToken2String(classToken)
  IF (verboseDebug) PRINT '(A,A,A,A,A,3X,I4)', 'Token: {', TRIM(curTokenString),'} {',opString,'}',classToken
  curPrec = classifyPrecedence(classToken)
  SELECT CASE (classToken)
    CASE (tokNum)
      CALL AddConstantSymbol(curTokenString,StringToReal(curTokenString))
      CALL AddIntCode(numSymbol)  !add the newest symbol to the IntCode list
    CASE (tokStr)
      CALL AddConstantSymbol(curTokenString)
      CALL AddIntCode(numSymbol)  !add the newest symbol to the IntCode list
    CASE (tokID)
      curID = LookupParameterSymbol(curTokenString)
      IF (curID .GE. 1) THEN
        CALL AddIntCode(curID)
      ELSE
        CALL AddToErrMsg('Invalid parameter name: ' // TRIM(curTokenString),msgError,0)
      END IF
    CASE (tokFuncABS,tokFuncASIN,tokFuncATAN,tokFuncCOS,tokFuncEXP,tokFuncINT,tokFuncLEN,tokFuncLOG, &
          tokFuncMOD,tokFuncNOT,tokFuncSIN,tokFuncSQRT,tokFuncTAN)
      CALL pushOpStack(classToken,precLeftParen,newTokenStart,newTokenEnd) !not sure about precedence
    CASE (tokOr,tokAnd,tokGT,tokEQ,tokLT,tokGE,tokLE,tokNE,tokTimes,tokDiv,tokPlus,tokMinus,tokExp)
      DO
        CALL checkTopPrec(oldPrec)
        IF (curPrec .LE. oldPrec) THEN
          CALL popOpStack(oldOp,oldPrec,curFuncStart,curFuncEnd)
          CALL AddIntCode(oldOp)
        ELSE
          EXIT
        ENDIF
      END DO
      CALL pushOpStack(classToken,curPrec,newTokenStart,newTokenEnd)
    CASE (tokLtParen)
      CALL pushOpStack(classToken,precLeftParen,newTokenStart,newTokenEnd)
    CASE (tokRtParen)
       DO
         IF (opStackTop .EQ. 0) THEN
           CALL AddToErrMsg('Stack underflow. ',msgError,0)
         END IF
         CALL popOpStack(oldOp, oldPrec,curFuncStart,curFuncEnd)
         IF (oldPrec .EQ. precLeftParen) EXIT
         CALL AddIntCode(oldOp)
       END DO
       IF (checkTopIsFunc()) THEN
         CALL popOpStack(oldOp, oldPrec,curFuncStart,curFuncEnd)
         CALL AddIntCode(oldOp)
         ! note the old line was the following that used the curFuncStart and End
         ! outStr = TRIM(outStr) // ' ' // expressionIn(curFuncStart:curFuncEnd)
       END IF
!    CASE (tokTilde) !currently not supporing multiple value functions
  END SELECT
  prevTokenEnd = newTokenEnd
  prevClassToken =  classToken
END DO
! pop the rest of the operator stack
DO i = 1,opStackTop
  CALL popOpStack(oldOp, oldPrec,curFuncStart,curFuncEnd)
  CALL AddIntCode(oldOp)
END DO
lastIntCodeOut = numIntCode
errorContext = ''
!PRINT '(I5)',opStack(1)%tokOperator !for debugging purposes only
END SUBROUTINE expressionToRPN

!----------------------------------------------------------------------------------
! The routine returns the start and end positions in the string
! for the next token which can be an operator or a number or a
! function name.  The search starts each time at the position
! endPrevToken + 1 so that it can be called multiple times. If tokenEnd
! returns a zero, no more tokens were found. Normally endPrevToken
! is set to endNewToken from the previous call.
!----------------------------------------------------------------------------------
SUBROUTINE GetNextToken(inString,endPrevToken,prevTokenClass,startNewToken,endNewToken,newTokenClass)
CHARACTER(LEN=*), INTENT(IN)  :: inString
INTEGER, INTENT(IN)   :: endPrevToken
INTEGER, INTENT(IN)   :: prevTokenClass
INTEGER, INTENT(OUT)  :: startNewToken
INTEGER, INTENT(OUT) :: endNewToken
INTEGER, INTENT(OUT) :: newTokenClass
INTEGER :: curCharKind = 0
INTEGER :: prevCharKind = charNone
INTEGER :: lenIn = 0
INTEGER :: i
INTEGER :: state = 0
LOGICAL :: onePeriodInNumber = .FALSE.
LOGICAL :: oneEInNumber = .FALSE.
CHARACTER(len=StringLength) :: errDescription = ''
INTEGER, PARAMETER :: stNone = 1
INTEGER, PARAMETER :: stNum = 2
INTEGER, PARAMETER :: stStr = 3
INTEGER, PARAMETER :: stID = 4
INTEGER, PARAMETER :: stEqual = 5
INTEGER, PARAMETER :: stGreat = 6
INTEGER, PARAMETER :: stLess = 7
INTEGER, PARAMETER :: stFunc = 8
INTEGER, PARAMETER :: stMinus = 9
INTEGER, PARAMETER :: stAmpersand = 10
INTEGER, PARAMETER :: stPipe = 11
INTEGER, PARAMETER :: stDONE = 100
CHARACTER(10) :: curFuncString
INTEGER :: startScan = 0
startNewToken = 0
endNewToken = 0
newTokenClass = 0
lenIn = LEN_TRIM(inString)
IF (endPrevToken .GE. lenIn) RETURN
prevCharKind = charNone
onePeriodInNumber = .FALSE.
oneEInNumber = .FALSE.
state = stNone
IF (prevTokenClass .NE. tokStr) THEN
  startScan = endPrevToken + 1
ELSE
  startScan = endPrevToken + 2 ! skip an extra character for the trailing double quote after a string
END IF
DO i = startScan,lenIn
  curCharKind = classifyChar(inString(i:i))
  !PRINT '(A,3X,I4)','character: ' // TRIM(inString(i:i)),curCharKind
  SELECT CASE (state)
    CASE (stNone)
      SELECT CASE (curCharKind)
        CASE (charNum)
          state = stNum
          startNewToken = i
          endNewToken = i
          newTokenClass = tokNum
        CASE (charPeriod)
          onePeriodInNumber = .TRUE.
          state = stNum
          startNewToken = i
          endNewToken = i
        CASE (charE,charAZ,charUnderScore)
          state = stFunc
          startNewToken = i
          endNewToken = i
        CASE (charMinus)
          state = stMinus
          startNewToken = i
        CASE (charPlus)
          startNewToken = i
          endNewToken = i
          newTokenClass = tokPlus
          state = stDONE
        CASE (charMult)
          startNewToken = i
          endNewToken = i
          newTokenClass = tokTimes
          state = stDONE
        CASE (charDiv)
          startNewToken = i
          endNewToken = i
          newTokenClass = tokDiv
          state = stDONE
        CASE (charExp)
          startNewToken = i
          endNewToken = i
          newTokenClass = tokExp
          state = stDONE
        CASE (charLeftParen)
          startNewToken = i
          endNewToken = i
          newTokenClass = tokLtParen
          state = stDONE
        CASE (charRightParen)
          startNewToken = i
          endNewToken = i
          newTokenClass = tokRtParen
          state = stDONE
        CASE (charTilde)
          startNewToken = i
          endNewToken = i
          newTokenClass = tokTilde
          state = stDONE
        CASE (charDoubleQuote)
          state = stStr
          startNewToken = i
        CASE (charEqual)
          state = stEqual
          startNewToken = i
        CASE (charGreat)
          state = stGreat
          startNewToken = i
        CASE (charLess)
          state = stLess
          startNewToken = i
        CASE (charSpace)
          state = stNone
        CASE (charDollar)
          state = stID
          startNewToken = i
        CASE (charAmpersand)
          state = stAmpersand
          startNewToken = i
        CASE (charPipe)
          state = stPipe
          startNewToken = i
        CASE (charOther)
          newTokenClass = tokINVALID
          errDescription = 'Invalid character'
          state = stDONE
      END SELECT
    CASE (stNum)
      SELECT CASE (curCharKind)
        CASE (charNum)
          endNewToken = i
          newTokenClass = tokNum
        CASE (charPeriod)
          IF (onePeriodInNumber) THEN  !already have a period in this number
            newTokenClass = tokINVALID
            errDescription = 'Only one period allowed in a number'
            state = stDONE
          ELSE
            endNewToken = i
            onePeriodInNumber = .TRUE.
          END IF
        CASE (charE)
          IF (oneEInNumber) THEN  !already have an E in this number
            newTokenClass = tokINVALID
            errDescription = 'Only one E or D allowed in a number'
            state = stDONE
          ELSE
            endNewToken = i
            oneEInNumber = .TRUE.
          END IF
        CASE (charMinus) !accept a minus if just after the E for negative exponents.
          IF (prevCharKind .EQ. charE) THEN
            endNewToken = i
          ELSE
            newTokenClass = tokNum
            state = stDONE
          END IF
        CASE DEFAULT
          newTokenClass = tokNum
          state = stDONE
      END SELECT
    CASE (stStr)
      IF (curCharKind .EQ. charDoubleQuote) THEN
        endNewToken = i
        newTokenClass = tokStr
        state = stDONE
      END IF
    CASE (stID) ! a dollar sign followed by letters and numbers
      SELECT CASE (curCharKind)
        CASE (charNum,charE,charAZ,charUnderscore)
          endNewToken = i
          newTokenClass = tokID
        CASE DEFAULT
          newTokenClass = tokID
          state = stDONE
      END SELECT
    CASE (stEqual)
      SELECT CASE (curCharKind)
        CASE (charEqual)
          endNewToken = i
          newTokenClass = tokEQ
          state = stDONE
        CASE (charGreat)
          endNewToken = i
          newTokenClass = tokGE
          state = stDONE
        CASE (charLess)
          endNewToken = i
          newTokenClass = tokLE
          state = stDONE
        CASE DEFAULT
          newTokenClass = tokINVALID
          errDescription = 'An equals sign alone is invalid for expressions.'
          state = stDONE
      END SELECT
    CASE (stGreat)
      SELECT CASE (curCharKind)
        CASE (charEqual)
          endNewToken = i
          newTokenClass = tokGE
          state = stDONE
        CASE DEFAULT
          endNewToken = startNewToken
          newTokenClass = tokGT
          state = stDONE
      END SELECT
    CASE (stLess)
      SELECT CASE (curCharKind)
        CASE (charGreat)
          endNewToken = i
          newTokenClass = tokNE
          state = stDONE
        CASE (charEqual)
          endNewToken = i
          newTokenClass = tokLE
          state = stDONE
        CASE DEFAULT
          endNewToken = startNewToken
          newTokenClass = tokLT
          state = stDONE
      END SELECT
    CASE (stFunc) !functions are alphas only
      SELECT CASE (curCharKind)
        CASE (charE,charAZ,charUnderscore)
          endNewToken = i
        CASE DEFAULT
          newTokenClass = tokFunc
          state = stDONE
      END SELECT
    CASE (stMinus) !can be an operator, unary minus, or the beginning of a number
! Cases with minus sign
!    8-FUNC(2) operand
!    8-$VAR operand
!    8-4 operand
!    $VAR-2 operand
!
!    8*-2 negative
!    -2 negative
!    FUNC(-2) negative
!    FUNC(3~-2~8) negative (remember ~ is parameter separator instead of comma
!
!    8*-$VAR unary negation operand
!    -$VAR  unary negation operand
      SELECT CASE (curCharKind)
        CASE (charNum)
          SELECT CASE (prevTokenClass)
            CASE (tokNone,tokMinus,tokPlus,tokTimes,tokDiv,tokExp,tokLtParen)
              endNewToken = i
              state = stNum
            CASE DEFAULT
              endNewToken = startNewToken
              newTokenClass = tokMinus
              state = stDONE
          END SELECT
        CASE (charPeriod)
          SELECT CASE (prevTokenClass)
            CASE (tokNone,tokMinus,tokPlus,tokTimes,tokDiv,tokExp,tokLtParen)
              endNewToken = i
              onePeriodInNumber = .TRUE.
              state = stNum
            CASE DEFAULT
              endNewToken = startNewToken
              newTokenClass = tokMinus
              state = stDONE
          END SELECT
        CASE DEFAULT
          SELECT CASE (prevTokenClass)
            CASE (tokNone,tokMinus,tokPlus,tokTimes,tokDiv,tokExp,tokLtParen)
              endNewToken = i
              newTokenClass = tokUnNeg
              state = stDone
            CASE DEFAULT
              endNewToken = startNewToken
              newTokenClass = tokMinus
              state = stDONE
          END SELECT
      END SELECT
    CASE (stAmpersand)
      SELECT CASE (curCharKind)
        CASE (charAmpersand)
          endNewToken = i
          newTokenClass = tokAnd
          state = stDONE
        CASE DEFAULT
          newTokenClass = tokINVALID
          errDescription = 'A single ampersand & alone is invalid for expressions.'
          state = stDONE
      END SELECT
    CASE (stPipe)
      SELECT CASE (curCharKind)
        CASE (charPipe)
          endNewToken = i
          newTokenClass = tokOr
          state = stDONE
        CASE DEFAULT
          newTokenClass = tokINVALID
          errDescription = 'A single pipe | alone is invalid for expressions.'
          state = stDONE
      END SELECT
  END SELECT
  prevCharKind = curCharKind
  IF (state .EQ. stDONE) EXIT
END DO
! look up the specific function
IF (newTokenClass .EQ. tokFunc) THEN
  curFuncString = inString(startNewToken:endNewToken)
  IF (IsStrEq(curFuncString,'ABS')) THEN
    newTokenClass = tokFuncABS
  ELSEIF (IsStrEq(curFuncString,'ACOS')) THEN !arc cos
    newTokenClass = tokFuncACOS
  ELSEIF (IsStrEq(curFuncString,'ASIN')) THEN !arc sin
    newTokenClass = tokFuncASIN
  ELSEIF (IsStrEq(curFuncString,'ATAN')) THEN !arc tan
    newTokenClass = tokFuncATAN
  ELSEIF (IsStrEq(curFuncString,'COS')) THEN
    newTokenClass = tokFuncCOS
  ELSEIF (IsStrEq(curFuncString,'EXP')) THEN  !e to the x
    newTokenClass = tokFuncEXP
  ELSEIF (IsStrEq(curFuncString,'INT')) THEN
    newTokenClass = tokFuncINT
  ELSEIF (IsStrEq(curFuncString,'LEN')) THEN !length of string
    newTokenClass = tokFuncINT
  ELSEIF (IsStrEq(curFuncString,'LOG')) THEN
    newTokenClass = tokFuncLOG
  ELSEIF (IsStrEq(curFuncString,'MOD')) THEN !remainder of argument
    newTokenClass = tokFuncLOG
  ELSEIF (IsStrEq(curFuncString,'NOT')) THEN
    newTokenClass = tokFuncNOT
  ELSEIF (IsStrEq(curFuncString,'SIN')) THEN
    newTokenClass = tokFuncSIN
  ELSEIF (IsStrEq(curFuncString,'SQRT')) THEN
    newTokenClass = tokFuncSQRT
  ELSEIF (IsStrEq(curFuncString,'TAN')) THEN
    newTokenClass = tokFuncTAN
  ELSE
    errDescription = 'Function referenced not found.'
    newTokenClass = tokINVALID
  END IF
END IF
! for strings take off the first and last character which will be double quotes
IF (newTokenClass .EQ.  tokStr) THEN
  endNewToken = endNewToken -1
  startNewToken = startNewToken + 1
END IF
! check for invalid token
IF (newTokenClass .EQ. tokINVALID) THEN
  CALL AddToErrMsg('Invalid token found: ' // TRIM(errDescription) // ' ' // TRIM(inString)  ,msgWarning,startNewToken)
  PRINT '(A)','CONTEXT: ' // TRIM(inString)
  PRINT '(A)',REPEAT(' ',startNewToken + 8) // '^'
  PRINT '(A)',TRIM(errDescription)
END IF
END SUBROUTINE



!----------------------------------------------------------------------------------
! Classify the kind of statement in the given parametric:logic field and
! return a constand for the kind of statement (see constants at top of file)
! and return the character after the command
!----------------------------------------------------------------------------------
SUBROUTINE determineStatement(inString,outKind,outAfter)
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
CHARACTER(len=*), INTENT(IN) :: inString
INTEGER, INTENT(OUT) :: outKind
INTEGER, INTENT(OUT) :: outAfter
CHARACTER(len=LongString) :: curInput
INTEGER :: equalsPos

curInput = ADJUSTL(inString)
IF (IsStrEq(curInput(1:9),'PARAMETER')) THEN
  outKind = ksParameter
  outAfter = 10
ELSEIF (IsStrEq(curInput(1:2),'IF')) THEN
  outKind = ksIf
  outAfter = 3
ELSEIF (IsStrEq(curInput(1:6),'ELSEIF')) THEN
  outKind = ksElseIf
  outAfter = 7
ELSEIF (IsStrEq(curInput(1:4),'ELSE')) THEN
  outKind = ksElse
  outAfter = 5
ELSEIF (IsStrEq(curInput(1:5),'ENDIF')) THEN
  outKind = ksEndIf
  outAfter = 6
ELSEIF (IsStrEq(curInput(1:6),'SELECT')) THEN
  outKind = ksSelect
  outAfter = 7
ELSEIF (IsStrEq(curInput(1:4),'CASE')) THEN
  outKind = ksCase
  outAfter = 5
ELSEIF (IsStrEq(curInput(1:7),'DEFAULT')) THEN
  outKind = ksDefault
  outAfter = 8
ELSEIF (IsStrEq(curInput(1:9),'ENDSELECT')) THEN
  outKind = ksEndSelect
  outAfter = 10
ELSEIF (IsStrEq(curInput(1:7),'DISABLE')) THEN
  outKind = ksDisable
  outAfter = 8
ELSEIF (IsStrEq(curInput(1:6),'ENABLE')) THEN
  outKind = ksEnable
  outAfter = 7
ELSEIF (IsStrEq(curInput(1:6),'REMARK')) THEN
  outKind = ksRemark
  outAfter = 7
ELSEIF (curInput(1:1) .EQ. '$') THEN
  !confirm that it is an assignment by verifying that an equals sign follows the
  !euals sign - still need to do more to verify that the identifier is valid.
  equalsPos = INDEX(curInput,'=')
  IF (equalsPos .GE. 3) THEN
    outKind = ksAssignment
    outAfter = equalsPos
  ELSE
    outKind = ksNone
    outAfter = 1
  END IF
ELSE
  outKind = ksNone
  outAfter = 1
END IF
END SUBROUTINE

!----------------------------------------------------------------------------------
! Add one IntCode - intermediate code
!----------------------------------------------------------------------------------
SUBROUTINE AddIntCode(intCodeIn)
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
INTEGER, INTENT(IN) :: intCodeIn

IF (.NOT. ALLOCATED(IntCode)) THEN
  ALLOCATE(IntCode(sizeIntCode))
  numIntCode = 1
ELSE
  numIntCode = numIntCode + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numIntCode .GT. sizeIntCode) THEN
    ALLOCATE(IntCodeCopy(sizeIntCode))
    IntCodeCopy = IntCode
    DEALLOCATE(IntCode)
    ! double the size of the array
    ALLOCATE(IntCode(sizeIntCode * 2))
    IntCode(1:sizeIntCode) = IntCodeCopy
    DEALLOCATE(IntCodeCopy)
    sizeIntCode = sizeIntCode * 2
  END IF
END IF
IntCode(numIntCode) = intCodeIn
END SUBROUTINE AddIntCode


!----------------------------------------------------------------------------------
! Add to the list of line references for all objects in the input file
!----------------------------------------------------------------------------------
SUBROUTINE AddObjectLineReference(objKind,objName,lineStart,lineEnd)
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
CHARACTER(len=*), INTENT(IN) :: objKind
CHARACTER(len=*), INTENT(IN) :: objName
INTEGER, INTENT(IN) :: lineStart
INTEGER, INTENT(IN) :: lineEnd

IF (.NOT. ALLOCATED(ObjectLines)) THEN
  ALLOCATE(ObjectLines(sizeObjectLines))
  numObjectLines = 1
ELSE
  numObjectLines = numObjectLines + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numObjectLines .GT. sizeObjectLines) THEN
    ALLOCATE(ObjectLinesCopy(sizeObjectLines))
    ObjectLinesCopy = ObjectLines
    DEALLOCATE(ObjectLines)
    ! double the size of the array
    ALLOCATE(ObjectLines(sizeObjectLines * 2))
    ObjectLines(1:sizeObjectLines) = ObjectLinesCopy
    DEALLOCATE(ObjectLinesCopy)
    sizeObjectLines = sizeObjectLines * 2
  END IF
END IF
ObjectLines(numObjectLines)%kindOfObj = objKind
ObjectLines(numObjectLines)%nameOfObj = objName
ObjectLines(numObjectLines)%firstLine = lineStart
ObjectLines(numObjectLines)%lastLine = lineEnd
ObjectLines(numObjectLines)%enabled = .TRUE.
! for debugging:
IF (verboseDebug) PRINT '(A,I6,2X,A,2X,A,2X,I6,2X,I6)', 'AddObjectLineReference: ', numObjectLines, TRIM(objKind),TRIM(objName),lineStart,lineEnd
END SUBROUTINE AddObjectLineReference

!----------------------------------------------------------------------------------
! Find the current parameter index from a given name
!----------------------------------------------------------------------------------
INTEGER FUNCTION LookupParameterSymbol(symbolNameIn)
CHARACTER(len=*), INTENT(IN)  :: symbolNameIn
INTEGER :: iSymbol = 0

LookupParameterSymbol = 0 !default is zero meaning not found
DO iSymbol = 1,lastSymbolParameter
  IF (IsStrEq(symbolNameIn,Symbol(iSymbol)%name)) THEN
    LookupParameterSymbol = iSymbol
    EXIT
  END IF
END DO
END FUNCTION LookupParameterSymbol

!----------------------------------------------------------------------------------
! Add a new object to the list of objects - holds only the PARAMETRIC: objects
!----------------------------------------------------------------------------------
SUBROUTINE addNewObject(kindOfObject)
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
INTEGER, INTENT(IN) :: kindOfObject
IF (.NOT. ALLOCATED(IdfObject)) THEN
  ALLOCATE(IdfObject(sizeIdfObject))
  numIdfObject = 1
ELSE
  numIdfObject = numIdfObject + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numIdfObject .GT. sizeIdfObject) THEN
    ALLOCATE(IdfObjectCopy(sizeIdfObject))
    IdfObjectCopy = IdfObject
    DEALLOCATE(IdfObject)
    ! double the size of the array
    ALLOCATE(IdfObject(sizeIdfObject * 2))
    IdfObject(1:sizeIdfObject) = IdfObjectCopy
    DEALLOCATE(IdfObjectCopy)
    sizeIdfObject = sizeIdfObject * 2
  END IF
END IF
IdfObject(numIdfObject)%kindObj = kindOfObject
!assume that the next field is the first and last field of the object until
!additional fields are added.
IdfObject(numIdfObject)%firstField = numIdfField + 1
IdfObject(numIdfObject)%lastField = numIdfField + 1
END SUBROUTINE

!----------------------------------------------------------------------------------
! Add a new field to the current object - holds only the fields from PARAMETRIC: objects
!----------------------------------------------------------------------------------
SUBROUTINE addField(fieldIn)
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
CHARACTER(len=*), INTENT(IN)  :: fieldIn
IF (.NOT. ALLOCATED(IdfField)) THEN
  ALLOCATE(IdfField(sizeIdfField))
  numIdfField = 1
ELSE
  numIdfField = numIdfField + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numIdfField .GT. sizeIdfField) THEN
    ALLOCATE(IdfFieldCopy(sizeIdfField))
    IdfFieldCopy = IdfField
    DEALLOCATE(IdfField)
    ! double the size of the array
    ALLOCATE(IdfField(sizeIdfField * 2))
    IdfField(1:sizeIdfField) = IdfFieldCopy
    DEALLOCATE(IdfFieldCopy)
    sizeIdfField = sizeIdfField * 2
  END IF
END IF
IdfField(numIdfField) = ADJUSTL(fieldIn)
IdfObject(numIdfObject)%lastField = numIdfField
END SUBROUTINE

!----------------------------------------------------------------------------------
! Add a new parameter to the list of symbols.
! Return a false if an error occurred
!----------------------------------------------------------------------------------
LOGICAL FUNCTION AddParameterSymbol(parIn,ifSetValueForRunIn)
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
CHARACTER(len=*), INTENT(IN)  :: parIn
LOGICAL, INTENT(IN),OPTIONAL :: ifSetValueForRunIn
CHARACTER(len=LongString) :: curPar
INTEGER :: iChar = 0
curPar = TRIM(ADJUSTL(parIn))
! confirm that it leads with a $
IF (curPar(1:1) .NE. '$') THEN
  AddParameterSymbol = .FALSE.
  RETURN
END IF
! confirm that the parameter name is alphanumeric
DO iChar = 2, LEN_TRIM(curPar)
  SELECT CASE (classifyChar(curPar(iChar:iChar)))
    CASE (charNum,charAZ,charE,charUnderscore)
      ! do nothing
    CASE DEFAULT
      AddParameterSymbol = .FALSE.
      RETURN
  END SELECT
END DO
IF (.NOT. ALLOCATED(Symbol)) THEN
  ALLOCATE(Symbol(sizeSymbol))
  numSymbol = 1
ELSE
  numSymbol = numSymbol + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numSymbol .GT. sizeSymbol) THEN
    ALLOCATE(SymbolCopy(sizeSymbol))
    SymbolCopy = Symbol
    DEALLOCATE(Symbol)
    ! double the size of the array
    ALLOCATE(Symbol(sizeSymbol * 2))
    Symbol(1:sizeSymbol) = SymbolCopy
    DEALLOCATE(SymbolCopy)
    sizeSymbol = sizeSymbol * 2
  END IF
END IF
Symbol(numSymbol)%name = TRIM(curPar)
Symbol(numSymbol)%isParameter = .TRUE.
IF (PRESENT(ifSetValueForRunIn)) THEN
  IF (ifSetValueForRunIn) THEN
    lastSymbolSetValueForRun = numSymbol
    Symbol(numSymbol)%isSetValueForRun = .TRUE.
  END IF
END IF
IF (verboseDebug) PRINT '(A,A)','  Started AddParameterSymbol: ', TRIM(Symbol(numSymbol)%name)
lastSymbolParameter = numSymbol
AddParameterSymbol = .TRUE.
END FUNCTION

!----------------------------------------------------------------------------------
! Add a new constant to the list of symbols. If value is a number the numerical
! real value is an optional input. No verification that the string and number
! are equivalent is done here. Must be verified by calling routine.
!----------------------------------------------------------------------------------
SUBROUTINE AddConstantSymbol(constantIn,constantRealIn)
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
CHARACTER(len=*), INTENT(IN)  :: constantIn
REAL(8), INTENT(IN), OPTIONAL :: constantRealIn
IF (.NOT. ALLOCATED(Symbol)) THEN
  ALLOCATE(Symbol(sizeSymbol))
  numSymbol = 1
ELSE
  numSymbol = numSymbol + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numSymbol .GT. sizeSymbol) THEN
    ALLOCATE(SymbolCopy(sizeSymbol))
    SymbolCopy = Symbol
    DEALLOCATE(Symbol)
    ! double the size of the array
    ALLOCATE(Symbol(sizeSymbol * 2))
    Symbol(1:sizeSymbol) = SymbolCopy
    DEALLOCATE(SymbolCopy)
    sizeSymbol = sizeSymbol * 2
  END IF
END IF
Symbol(numSymbol)%name = '' !blank for constants
Symbol(numSymbol)%val = constantIn
IF (PRESENT(constantRealIn)) THEN
  Symbol(numSymbol)%valAsReal = constantRealIn
  Symbol(numSymbol)%isRealNum = .TRUE.
END IF
END SUBROUTINE

!----------------------------------------------------------------------------------
! Add the expressions found in the IDF file, not including in the parametric
! objects, to the list.
!----------------------------------------------------------------------------------
INTEGER FUNCTION addNewExpression(expString,lineOfFile)
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
CHARACTER(len=*), INTENT(IN)  :: expString
INTEGER,INTENT(IN)            :: lineOfFile
IF (.NOT. ALLOCATED(FoundExpression)) THEN
  ALLOCATE(FoundExpression(sizeFoundExpression))
  numFoundExpression = 1
ELSE
  numFoundExpression = numFoundExpression + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numFoundExpression .GT. sizeFoundExpression) THEN
    ALLOCATE(FoundExpressionCopy(sizeFoundExpression))
    FoundExpressionCopy = FoundExpression
    DEALLOCATE(FoundExpression)
    ! double the size of the array
    ALLOCATE(FoundExpression(sizeFoundExpression * 2))
    FoundExpression(1:sizeFoundExpression) = FoundExpressionCopy
    DEALLOCATE(FoundExpressionCopy)
    sizeFoundExpression = sizeFoundExpression * 2
  END IF
END IF
IF (verboseDebug) PRINT '(A,2X,I6)',TRIM(expString),lineOfFile
FoundExpression(numFoundExpression)%text = expString
FoundExpression(numFoundExpression)%line = lineOfFile
addNewExpression = numFoundExpression !return the counter
END FUNCTION

!----------------------------------------------------------------------------------
! Find the location in the string of the !;,= symbols
!----------------------------------------------------------------------------------
SUBROUTINE FindDelimiters(StringIn,posExcl,posSemi,posComma,posEqual)
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
CHARACTER(len=*), INTENT(IN) :: StringIn  ! string to search
INTEGER,INTENT(OUT)          :: posExcl   ! position of the exclamation point
INTEGER,INTENT(OUT)          :: posSemi   ! position of the semicolon
INTEGER,INTENT(OUT)          :: posComma  ! position of the comma
INTEGER,INTENT(OUT)          :: posEqual  ! position of the equals
posExcl = INDEX(StringIn, '!')  ! comment
posSemi = INDEX(StringIn,';')   ! object end
posComma = INDEX(StringIn, ',') ! field delimit
posEqual = INDEX(StringIn, '=') ! field delimit
! if the delimiter is after the comment character then ignore them.
IF (posExcl .GE. 1) THEN
  IF (posSemi .GT. posExcl) THEN
    posSemi = 0
  END IF
  IF (posComma .GT. posExcl) THEN
    posComma = 0
  END IF
  IF (posEqual .GT. posExcl) THEN
    posEqual = 0
  END IF
END IF
END SUBROUTINE

!----------------------------------------------------------------------------------
! Store the error messages and set the errorCondition flag so that messages
! are included in file at end.
!----------------------------------------------------------------------------------
SUBROUTINE AddToErrMsg(TextOfError,KindOfError,ErrorNumber)
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
CHARACTER(len=*), INTENT(IN)  :: TextOfError
INTEGER,INTENT(IN)            :: KindOfError
INTEGER,INTENT(IN)            :: ErrorNumber
IF (.NOT. ALLOCATED(ErrMsgs)) THEN
  ALLOCATE(ErrMsgs(sizeErrMsgs))
  numErrMsgs = 1
ELSE
  numErrMsgs = numErrMsgs + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numErrMsgs .GT. sizeErrMsgs) THEN
    ALLOCATE(ErrMsgsCopy(sizeErrMsgs))
    ErrMsgsCopy = ErrMsgs
    DEALLOCATE(ErrMsgs)
    ! double the size of the array
    ALLOCATE(ErrMsgs(sizeErrMsgs * 2))
    ErrMsgs(1:sizeErrMsgs) = ErrMsgsCopy
    DEALLOCATE(ErrMsgsCopy)
    sizeErrMsgs = sizeErrMsgs * 2
  END IF
END IF
ErrMsgs(numErrMsgs)%msgText = TRIM(TextOfError)
ErrMsgs(numErrMsgs)%msgKind = KindOfError
ErrMsgs(numErrMsgs)%msgErrNum = ErrorNumber
ErrMsgs(numErrMsgs)%msgContext = errorContext
PRINT '(A)','-------------------------------------------------------------------------'
IF (KindOfError .EQ. msgError) THEN
  PRINT '(A)','   ERROR:'
ELSE
  PRINT '(A)','   WARNING:'
END IF
PRINT '(A,A)','      ', TRIM(TextOfError)
PRINT '(A,I4)','      Number ',ErrorNumber
IF (LEN_TRIM(errorContext) .GT. 0) THEN
  PRINT '(A,A)','   CONTEXT:', TRIM(errorContext)
END IF
IF (KindOfError .EQ. msgError) errorCondition = .TRUE.
END SUBROUTINE

!----------------------------------------------------------------------------------
! Classify the given character into a category (see program constants)
!----------------------------------------------------------------------------------
INTEGER FUNCTION classifyChar(singleChar)
CHARACTER(LEN=1), INTENT(IN) :: singleChar
SELECT CASE (ICHAR(singleChar))
    CASE (48:57)     !0123456789
      classifyChar = charNum
    CASE (46) ! .
      classifyChar = charPeriod
    CASE (68,69,100,101) ! d D e E
      classifyChar = charE
    CASE (65:67,70:90,97:99,102:122) !ABCDFGHIJKLMNOPQRSTUVWXYZabcdfghijklmnopqrstuvwxyz NO E's
      classifyChar = charAZ
    CASE (43)
      classifyChar =  charPlus
    CASE (45)
      classifyChar =  charMinus
    CASE (42)
      classifyChar =  charMult
    CASE (47)
      classifyChar =  charDiv
    CASE (94)
      classifyChar =  charExp
    CASE (40)
      classifyChar =  charLeftParen
    CASE (41)
      classifyChar =  charRightParen
    CASE (34)
      classifyChar =  charDoubleQuote
    CASE (61)
      classifyChar =  charEqual
    CASE (62)
      classifyChar =  charGreat
    CASE (60)
      classifyChar =  charLess
    CASE (126)
      classifyChar =  charTilde
    CASE (32)
      classifyChar =  charSpace
    CASE (36)
      classifyChar =  charDollar
    CASE (38)
      classifyChar =  charAmpersand
    CASE (95)
      classifyChar =  charUnderscore
    CASE (124)
      classifyChar =  charPipe
    CASE DEFAULT
      classifyChar =  charOther
END SELECT
END FUNCTION

!----------------------------------------------------------------------------------
! Push an operator on to the operation stack for converting expressions into RPN
!----------------------------------------------------------------------------------
SUBROUTINE pushOpStack(opTok, prec, startFunc, endFunc)
INTEGER, INTENT(IN) :: opTok
INTEGER, INTENT(IN) :: prec
INTEGER, INTENT(IN) :: startFunc
INTEGER, INTENT(IN) :: endFunc
opStackTop = opStackTop + 1
opStack(opStackTop)%tokOperator = opTok
opStack(opStackTop)%precedence = prec
opStack(opStackTop)%funcStart = startFunc
opStack(opStackTop)%funcEnd = endFunc
END SUBROUTINE

!----------------------------------------------------------------------------------
! Pop an operator off the top of the operation stack for converting expressions into RPN
!----------------------------------------------------------------------------------
SUBROUTINE popOpStack(opTok, prec, startFunc, endFunc)
INTEGER, INTENT(OUT) :: opTok
INTEGER, INTENT(OUT) :: prec
INTEGER, INTENT(OUT) :: startFunc
INTEGER, INTENT(OUT) :: endFunc
opTok = opStack(opStackTop)%tokOperator
prec = opStack(opStackTop)%precedence
startFunc = opStack(opStackTop)%funcStart
endFunc = opStack(opStackTop)%funcEnd
opStackTop = opStackTop - 1
IF (opStackTop .LT. 0) Then
  CALL AddToErrMsg('Operator stack underflow.',msgWarning,0)
END IF
END SUBROUTINE

!----------------------------------------------------------------------------------
! Check the top item in the operation stack for it precendence. This is used
! for converting expressions into RPN
!----------------------------------------------------------------------------------
SUBROUTINE checkTopPrec(prec)
INTEGER,INTENT(OUT) :: prec
IF (opStackTop .GE. 1) THEN
  prec = opStack(opStackTop)%precedence
ELSE
  prec = precNoItem
END IF
END SUBROUTINE

!----------------------------------------------------------------------------------
! Check the top item in the operation stack if it is a function. This is used
! for converting expressions into RPN
!----------------------------------------------------------------------------------
LOGICAL FUNCTION checkTopIsFunc()
IF (opStackTop .GE. 1) THEN
  IF ((opStack(opStackTop)%tokOperator .LE. tokFuncABS) .AND. &
     (opStack(opStackTop)%tokOperator .GE. tokFuncTAN)) THEN
    checkTopIsFunc = .TRUE.
  ELSE
    checkTopIsFunc = .FALSE.
  END IF
ELSE
  checkTopIsFunc = .FALSE.
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Convert the operation token back into a string
!----------------------------------------------------------------------------------
FUNCTION opToken2String(tokenIn) RESULT (strOut)
INTEGER,INTENT(IN) :: tokenIn
CHARACTER(4) :: strOut
SELECT CASE (tokenIn)
!tokens with normal output
  CASE (tokPlus)
    strOut = '+ '
  CASE (tokMinus)
    strOut = '- '
  CASE (tokTimes)
    strOut = '* '
  CASE (tokDiv)
    strOut = '/ '
  CASE (tokExp)
    strOut = '^ '
  CASE (tokRtParen)
    strOut = ') '
  CASE (tokLtParen)
    strOut = '('
  CASE (tokGT)
    strOut = '> '
  CASE (tokEQ)
    strOut = '=='
  CASE (tokLT)
    strOut = '< '
  CASE (tokGE)
    strOut = '>='
  CASE (tokLE)
    strOut = '<='
  CASE (tokNE)
    strOut = '<>'
  CASE (tokTilde)
    strOut = '~ '
  CASE (tokAnd)
    strOut = '&&'
  CASE (tokOr)
    strOut = '||'
  CASE (tokFuncABS)
    strOut = 'ABS'
  CASE (tokFuncACOS)
    strOut = 'ACOS'
  CASE (tokFuncASIN)
    strOut = 'ASIN'
  CASE (tokFuncATAN)
    strOut = 'ATAN'
  CASE (tokFuncCOS)
    strOut = 'COS'
  CASE (tokFuncEXP)
    strOut = 'EXP'
  CASE (tokFuncINT)
    strOut = 'INT'
  CASE (tokFuncLEN)
    strOut = 'LEN'
  CASE (tokFuncLOG)
    strOut = 'LOG'
  CASE (tokFuncMOD)
    strOut = 'MOD'
  CASE (tokFuncNOT)
    strOut = 'NOT'
  CASE (tokFuncSIN)
    strOut = 'SIN'
  CASE (tokFuncSQRT)
    strOut = 'SQRT'
  CASE (tokFuncTAN)
    strOut = 'TAN'
!tokens with output describing the string
  CASE(tokNone)
    strOut = 'None'
  CASE(tokNum)
    strOut = 'Num'
  CASE(tokStr)
    strOut = 'Str'
  CASE(tokID)
    strOut = 'ID'
!  CASE(tokFunc)
!    strOut = 'Func'
  CASE(tokUnNeg)
    strOut = 'UnNg'
  CASE(tokINVALID)
    strOut = 'INVL'
  CASE DEFAULT
    strOut = '????'
    CALL AddToErrMsg('Unknown type of operator',msgWarning,tokenIn)
END SELECT
END FUNCTION

!----------------------------------------------------------------------------------
! Determine the precedence of the token provided for converting expressions
! into a reverse polish notation represetation.
!----------------------------------------------------------------------------------
FUNCTION classifyPrecedence(tokenIn) RESULT (precOut)
INTEGER,INTENT(IN) :: tokenIn
INTEGER :: precOut
SELECT CASE (tokenIn)
  CASE(tokExp)
    precOut = precExp
  CASE(tokTimes,tokDiv)
    precOut = precMultDiv
  CASE(tokPlus,tokMinus)
    precOut = precAddSub
  CASE(tokGT,tokEQ,tokLT,tokGE,tokLE,tokNE)
    precOut = precCompare
  CASE(tokAnd,tokOr)
    precOut = precAndOr
  CASE(tokRtParen)
    precOut = precLeftParen
  CASE DEFAULT
    precOut = precNoItem
END SELECT
END FUNCTION


!=====================================================================================
!=====================================================================================
!
!    GENERIC (NOT PROGRAM SPECIFIC) SUPPORT ROUTINES
!
!=====================================================================================
!=====================================================================================

!----------------------------------------------------------------------------------
! Return the string that only includes only the path including the trailing slash
!----------------------------------------------------------------------------------
FUNCTION PathOnly(fileWithPathIn) RESULT (pathOnlyOut)
IMPLICIT NONE
INTEGER :: slashPos
CHARACTER(len=*),INTENT(IN) :: fileWithPathIn
CHARACTER(len=LEN(fileWithPathIn)) :: pathOnlyOut
!first look for DOS/Windows style slash
slashPos = INDEX(fileWithPathIn, '\',BACK=.TRUE.) !finds last slash
!now look for Unix style slash
IF (slashPos .EQ. 0) THEN
  slashPos = INDEX(fileWithPathIn, '/',BACK=.TRUE.) !finds last slash
END IF
If (slashPos .GE. 1) Then
  pathOnlyOut = fileWithPathIn(1:slashPos)
Else
  pathOnlyOut = ''
End If
END FUNCTION PathOnly

!----------------------------------------------------------------------------------
REAL(8) FUNCTION StringToReal(stringIn)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   February 2005
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    To convert from a string to a real

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: stringIn

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS
          !    na

          ! DERIVED TYPE DEFINITIONS
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na

IF (LEN_TRIM(stringIn) .GE. 1) THEN
  IF (VERIFY(TRIM(stringIn),'-0123456789.E+') .EQ. 0) THEN
    READ(stringIn,*) StringToReal
  ELSE
    StringToReal = 0
  END IF
ELSE
  StringToReal = 0
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Convert an integer value to a string.
!   Abstract away the internal write concept
!----------------------------------------------------------------------------------
FUNCTION IntToStr(intIn) RESULT (stringOut)
IMPLICIT NONE
INTEGER, INTENT(IN)    :: intIn
CHARACTER(LEN=12)      :: stringOut
WRITE(FMT=*, UNIT=stringOut) intIn
END FUNCTION

!----------------------------------------------------------------------------------
! Convert an integer value to a string but produces values in the form of 000 to 999
! If outside that range just produce normal freeformat integer
!   Abstract away the internal write concept
!----------------------------------------------------------------------------------
FUNCTION IntToStr3(intIn) RESULT (stringOut)
IMPLICIT NONE
INTEGER, INTENT(IN)    :: intIn
CHARACTER(LEN=12)      :: stringOut
IF ((intIn .LE. 999) .AND. (intIn .GE. 0)) THEN
  WRITE(FMT='(I3.3)', UNIT=stringOut) intIn
ELSE
  WRITE(FMT=*, UNIT=stringOut) intIn
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Convert an integer value to a string but produces values in the form of 000000 to 999999
! If outside that range just produce normal freeformat integer
!   Abstract away the internal write concept
!----------------------------------------------------------------------------------
FUNCTION IntToStr6(intIn) RESULT (stringOut)
IMPLICIT NONE
INTEGER, INTENT(IN)    :: intIn
CHARACTER(LEN=6)       :: stringOut
IF ((intIn .LE. 999999) .AND. (intIn .GE. 0)) THEN
  WRITE(FMT='(I6.6)', UNIT=stringOut) intIn
ELSE
  stringOut='Invalid'
END IF
END FUNCTION


!-------------------------------------------------
! Compare two strings and ignore case
!      Jason Glazer - July 2007
!-------------------------------------------------
FUNCTION IsStrEq(arg1,arg2) RESULT (resultEqual)
IMPLICIT NONE
CHARACTER(len=*), INTENT(IN) :: arg1
CHARACTER(len=*), INTENT(IN) :: arg2
LOGICAL :: resultEqual
INTEGER :: lenArg1
INTEGER :: lenArg2
INTEGER :: charArg1
INTEGER :: charArg2
INTEGER :: i
lenArg1 = LEN_TRIM(arg1)
lenArg2 = LEN_TRIM(arg2)
IF (arg1 .EQ. arg2) THEN
  resultEqual = .TRUE.
ELSEIF (lenArg1 .EQ. lenArg2) THEN
  resultEqual = .TRUE.  ! assume it is true unless find an incorrect compare
  ! for each character compare them
  DO i = 1, lenArg1
    charArg1 = ICHAR(arg1(i:i))
    charArg2 = ICHAR(arg2(i:i))
    IF (charArg1 .NE. charArg2) THEN
      SELECT CASE (charArg1)
        CASE (65:90) !uppercase
          IF ((charArg1 + 32) .NE. charArg2) THEN
            resultEqual = .FALSE.
            EXIT
          END IF
        CASE (97:122)!lowercase
          IF ((charArg1 - 32) .NE. charArg2) THEN
            resultEqual = .FALSE.
            EXIT
          END IF
        CASE DEFAULT
          resultEqual = .FALSE.
          EXIT
      END SELECT
    END IF
  END DO
ELSE
  resultEqual = .FALSE.
END IF
END FUNCTION

!----------------------------------------------------------------------------------
! Return the minimum value of the INT arguments as long as they are not zero.
!----------------------------------------------------------------------------------
INTEGER FUNCTION MinIgnoreZero(arg1,arg2,arg3,arg4,arg5,arg6,arg7)
IMPLICIT NONE
INTEGER, INTENT(IN)          :: arg1
INTEGER, INTENT(IN),OPTIONAL :: arg2
INTEGER, INTENT(IN),OPTIONAL :: arg3
INTEGER, INTENT(IN),OPTIONAL :: arg4
INTEGER, INTENT(IN),OPTIONAL :: arg5
INTEGER, INTENT(IN),OPTIONAL :: arg6
INTEGER, INTENT(IN),OPTIONAL :: arg7
INTEGER :: curMin
! set the minimum to a large value
curMin = HUGE(curMin)
IF (arg1 .NE. 0) THEN
  IF (arg1 .LT. curMin) curMin = arg1
END IF
IF (PRESENT(arg2)) THEN
  IF (arg2 .NE. 0) THEN
    IF (arg2 .LT. curMin) curMin = arg2
  END IF
END IF
IF (PRESENT(arg3)) THEN
  IF (arg3 .NE. 0) THEN
    IF (arg3 .LT. curMin) curMin = arg3
  END IF
END IF
IF (PRESENT(arg4)) THEN
  IF (arg4 .NE. 0) THEN
    IF (arg4 .LT. curMin) curMin = arg4
  END IF
END IF
IF (PRESENT(arg5)) THEN
  IF (arg5 .NE. 0) THEN
    IF (arg5 .LT. curMin) curMin = arg5
  END IF
END IF
IF (PRESENT(arg6)) THEN
  IF (arg6 .NE. 0) THEN
    IF (arg6 .LT. curMin) curMin = arg6
  END IF
END IF
IF (PRESENT(arg7)) THEN
  IF (arg7 .NE. 0) THEN
    IF (arg7 .LT. curMin) curMin = arg7
  END IF
END IF
MinIgnoreZero = curMin
END FUNCTION

!----------------------------------------------------------------------------------
!   Abstract away the internal write concept
!----------------------------------------------------------------------------------
FUNCTION RealToString(RealIn) RESULT (stringOut)
IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
REAL(8), INTENT(IN)         :: RealIn
INTEGER :: digitsBefore
INTEGER :: digitsAfter
LOGICAL :: useExponent
CHARACTER(LEN=40) :: stringOut
INTEGER           :: width
CHARACTER(LEN=40) :: formatString

IF ((RealIn .LT. 0.00001 .AND. RealIn .GT. -0.00001) .OR. &
    (RealIn .GT. 100000 .OR. RealIn .LT. -100000)) THEN
  useExponent = .TRUE.
ELSE
  useExponent = .FALSE.
END IF
digitsBefore = 6
digitsAfter = 6
width = digitsBefore + digitsAfter + 1 !add one for the decimal point
IF (useExponent) THEN
  width = width + 5          !add 3 digit exponent plus sign plus letter E 'E-001'
  formatString = '(E' // TRIM(ADJUSTL(IntToStr(width))) //'.'// TRIM(ADJUSTL(IntToStr(digitsAfter))) // 'E3)'
ELSE
  formatString = '(F' // TRIM(ADJUSTL(IntToStr(width))) //'.'// TRIM(ADJUSTL(IntToStr(digitsAfter))) // ')'
END IF
!debug  stringOut = formatString
WRITE(FMT=formatString, UNIT=stringOut) RealIn
stringOut = ADJUSTL(stringOut)
END FUNCTION

!----------------------------------------------------------------------------------
! Remove the file extension
!----------------------------------------------------------------------------------
FUNCTION NoExtension(stringIn) RESULT (stringOut)
IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
CHARACTER(len=*), INTENT(IN) :: stringIn
CHARACTER(LEN=LongString) :: stringOut
INTEGER :: dotPos = 0
dotPos = INDEX(stringIn,'.',BACK=.TRUE.)
IF (dotPos .GE. 1) THEN
  stringOut = stringIn(1:(dotPos-1))
ELSE
  stringOut = stringIn
END IF
END FUNCTION


END PROGRAM ParametricPreprocessor
