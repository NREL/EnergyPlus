# Input Services

The module *InputProcessor* processes the input data files (IDFs).  It also reads and parses the IDD file.  The InputProcessor uses the definition lines in the IDD as directives on how to process each input object in the IDF.  The InputProcessor also turns all alpha strings into all UPPER CASE.  Currently, it does nothing else to those strings – so the number of blanks in a string must match what the calculational modules expect.  The InputProcessor processes all numeric strings into single precision real numbers.  Special characters, such as tabs, should *not* be included in the IDF.

The EnergyPlus module *InputProcessor* provides several routines - generically called the "get" routines – that enable the developer to readily access the data for a new module.  These routines are made available by including a "USE InputProcessor" statement in the module or in the routine that will use the "get" routines.  The GetFanInput subroutine in the example illustrates some of the uses of the "get" routines.

## InputProcessor

The following objects use public routines from the InputProcessor.  To access these, the code has:

~~~~~~~~~~~~~~~~~~~~

    Use InputProcessor, ONLY: <routine1>, <routine2>
~~~~~~~~~~~~~~~~~~~~

Where the <routine> is one or more of the following:

## GetNumObjectsFound

This function returns the number of objects in the input belonging to a particular class.  In other terms, it returns the number of instances in the input of a particular component.

~~~~~~~~~~~~~~~~~~~~

    Example:
    USE InputProcessor, ONLY: GetNumObjectsFound
    ---
    NumVAVSys = GetNumObjectsFound('SINGLE DUCT:VAV:REHEAT')

~~~~~~~~~~~~~~~~~~~~

Here NumVAVSys will contain the number of single duct VAV terminal units in the input data file (IDF). SINGLE DUCT:VAV:REHEAT is the class name or keyword defining VAV terminal unit input on the IDD file.

## GetObjectItem

This subroutine is used to obtain the actual alphanumeric and numeric data for a particular object.

Example:

~~~~~~~~~~~~~~~~~~~~

        USE InputProcessor
    ---
        INTEGER :: SysNum = 0     ! The Sys that you are currently loading input into
    ---
        CHARACTER(Len=MaxNameLength)  :: CurrentModuleObject
        CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas
        CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields
        CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields
        REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers
        LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks
        LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks
        INTEGER                              :: MaxNums=0
        INTEGER                              :: MaxAlphas=0
        INTEGER                              :: TotalArgs=0
    ---
        CALL GetObjectDefMaxArgs &
    ('AirTerminal:SingleDuct:VAV:Reheat', TotalArgs, NumAlphas, NumNums)
        MaxNums=MAX(MaxNums,NumNums)
        MaxAlphas=MAX(MaxAlphas,NumAlphas)
    ---
          !Start Loading the System Input
            CurrentModuleObject='AirTerminal:SingleDuct:VAV:Reheat'

          DO SysIndex = 1,  NumVAVSys

            CALL GetObjectItem & (TRIM(CurrentModuleObject),SysIndex,Alphas,NumAlphas,Numbers,NumNums, &
        IOSTAT,AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
        AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)

            SysNum = SysIndex
    ---     Check to make sure no duplicate names entered
            Sys(SysNum)%SysName     = Alphas(1)
            Sys(SysNum)%SysType     = TRIM(CurrentModuleObject)
            Sys(SysNum)%SysType_Num = SingleDuctVAVReheat
    ---
          END DO
~~~~~~~~~~~~~~~~~~~~

Here GetObjectItem is called with inputs ('AirTerminal:SingleDuct:VAV:Reheat')  – passed as CurrentModuleObject the class of object we want to input – and SysIndex – the index of the object on the input file. If SysIndex is 3, the call to GetObjectItem will get the data for the third VAV terminal unit on the input file. Output is returned in the remaining arguments. AlphArray contains in order all the alphanumeric data items for a single VAV terminal unit. NumArray contains all the numeric data items. NumAlphas is the number of alphanumeric items read; NumNums is the number of numeric data items read.  IOSTAT is a status flag: -1 means there was an error; +1 means the input was OK. AlphArray and NumArray should be dimensioned to handle the largest expected input for the item – which in this case is set from a call to **GetObjectDefMaxArgs**. NumBlank is an optional argument to the routine – it can be used to determine if a numeric field was entered as "blank" rather than the filled value of 0.0. Likewise for NumFields and the others.

These are used to make the potential error messages from the GetInput routine correspond more closely to the IDD nomenclature, but look a bit funny in use:

~~~~~~~~~~~~~~~~~~~~

    IF (Sys(SysNum)%SchedPtr == 0) THEN
       CALL ShowSevereError(  &
         RoutineName//trim(CurrentModuleObject)//'="'//  &
         TRIM(Sys(SysNum)%SysName)//'", invalid schedule.')
       CALL ShowContinueError(TRIM(cAlphaFields(2))//' = '//  &
         TRIM(Alphas(2))//' not found.')
       ErrorsFound=.true.
    ENDIF
~~~~~~~~~~~~~~~~~~~~

More information about standard error message formatting is contained in the Output Details and Examples document (for the user) and (for the developer) in this document section: Standard Message Format.

## GetObjectDefMaxArgs

### Extensible input techniques

While developers do their best to guess how many items are needed in an object, users will often want to extend that object with far more fields than were dreamed of.  Using Allocatable arrays in Fortran usually makes this feasible, the special \\extensible field makes it possible.

Example:

~~~~~~~~~~~~~~~~~~~~

    USE InputProcessor, ONLY: GetObjectDefMaxArgs
    ---
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas
    REAL, ALLOCATABLE, DIMENSION(:)                         :: Numbers

    ! You supply the object word, routine returns numargs, numalpha, numnumeric

    CALL GetObjectDefMaxArgs('DAYSCHEDULE:INTERVAL',NumArgs,NumAlpha,NumNumeric)

    ALLOCATE(Alphas(NumAlpha))
    ALLOCATE(Numbers(NumNumeric))

    !  Then, usual get calls…
~~~~~~~~~~~~~~~~~~~~

Thus, you can determine how many arguments that the IDD has defined as "maximum" for a given object.

## GetObjectItemNum

GetObjectItem, described above, requires the input file index of the desired object in order to get the object's data. Sometimes this index may be unknown, but the name of the object is known. GetObjectItemNum returns the input file index given the class name and object name.

~~~~~~~~~~~~~~~~~~~~

    Example:
    USE InputProcessor, ONLY: GetObjectItemNum
    ---
    ListNum = GetObjectItemNum('CONTROLLER LIST',ControllerListName)

~~~~~~~~~~~~~~~~~~~~

In the example, ListNum will contain the input file index of the 'CONTROLLER LIST' whose name is contained in the string variable ControllerListName.

## FindItemInList

This function looks up a string in a similar list of items and returns the index of the item in the list, if found. It is case sensitive.

~~~~~~~~~~~~~~~~~~~~

    Example:
    USE InputProcessor, ONLY: FindItemInList
    ---
    SysNum = FindItemInList(CompName,Sys%SysName,NumSys)

~~~~~~~~~~~~~~~~~~~~

CompName is the input string, Sys%SysName is the list of names to be searched, and NumSys is the size of the list.

## FindItem

Case insensitive version of the FindItemInList.

~~~~~~~~~~~~~~~~~~~~

    Example:
    USE InputProcessor, ONLY: FindItem
    ---
    SysNum = FindItem(CompName,Sys%SysName,NumSys)

~~~~~~~~~~~~~~~~~~~~

CompName is the input string, Sys%SysName is the list of names to be searched, and NumSys is the size of the list.

## FindItemInSortedList

This function looks up a string in a sorted list of items and returns the index of the item in the list, if found. It is case sensitive.

~~~~~~~~~~~~~~~~~~~~

    Example:
    USE InputProcessor, ONLY: FindItemInSortedList
    ---
    SysNum = FindItemInSortedList(CompName,Sys%SysName,NumSys)

~~~~~~~~~~~~~~~~~~~~

CompName is the input string, Sys%SysName is the list of names to be searched, and NumSys is the size of the list. See quick sort utility – most lists are NOT sorted in EnergyPlus.

## SameString

This function returns true if two strings are equal (case insensitively).

~~~~~~~~~~~~~~~~~~~~

    Example:
    USE InputProcessor, ONLY: SameString
    ---
    IF (SameString(InputRoughness,'VeryRough')) THEN
        Material(MaterNum)%Roughness=VeryRough
    ENDIF
~~~~~~~~~~~~~~~~~~~~

## VerifyName

This subroutine checks that an object name is unique; that is, it hasn't already been used for the same class of object and the name is not blank.

~~~~~~~~~~~~~~~~~~~~

    Example:
    USE InputProcessor, ONLY: VerifyName
    ---
    CALL VerifyName(AlphArray(1),Fan%FanName, &
      FanNum-1,IsNotOK,IsBlank,'FAN:SIMPLE:CONSTVOLUME Name')
~~~~~~~~~~~~~~~~~~~~

The first argument is the name to be checked, the second is the list of names to search, the third argument is the number of entries in the list, the 4^th^ argument is set to TRUE if verification fails, the 5^th^ argument is set to true if the name is blank, and the last argument is part of the error message written to the error file when verification fails.

## RangeCheck

The routine RangeCheck can be used to produce a reasonable error message to describe the situation in addition to setting the ErrorsFound variable to true.  Errors found can then be checked in the calling routine and the program terminated if desired.

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE RangeCheck(ErrorsFound,WhatFieldString,WhatObjectString,ErrorLevel,  &
                          LowerBoundString,LowerBoundCondition,UpperBoundString,UpperBoundCondition)
~~~~~~~~~~~~~~~~~~~~

It can be used in a variety of places when the \\minimum and \\maximum fields will not work (e.g. different min/max dependent on some other field).

~~~~~~~~~~~~~~~~~~~~

    USE InputProcessor, ONLY: RangeCheck
    ---
    ErrorsFound=.false.
    CALL RangeCheck(ErrorsFound,'DryBulb Temperature','WeatherFile',  &
                    'SEVERE','> -70',(Drybulb>-70.),'< 70',(DryBulb <70.))
    CALL RangeCheck(ErrorsFound,'DewPoint Temperature','WeatherFile',  &
                    'SEVERE','> -70',(Dewpoint>-70.),'< 70',(Dewpoint <70.))
    CALL RangeCheck(ErrorsFound,'Relative Humidity','WeatherFile',  &
                    'SEVERE','> 0',(RelHum>=0.),'<= 110',(RelHum<=110.))
~~~~~~~~~~~~~~~~~~~~

To examine one call:

The variable **DryBulb** is set to its value.  In this case, it is coming from the **Weather File**.  The **LowerBoundString** is '> - 70' and the **LowerBoundCondition** is (DryBulb>-70.) [this expression will yield true or false depending…]

The LowerBounds (**LowerBoundString**, **LowerBoundCondition**) are optional as are the UpperBounds (**UpperBoundString**, **UpperBoundCondition**).  If we were only testing one set of ranges, the call would look like:

~~~~~~~~~~~~~~~~~~~~

    Call RangeCheck(ErrorsFound,'DryBulb Temperature','WeatherFile','SEVERE',  &
      UpperBoundString='< 70', UpperBoundCondition=(DryBulb<70.))
~~~~~~~~~~~~~~~~~~~~

ErrorLevel can be one of the usual Error levels:

WARNING – would be a simple warning message – the calling routine might reset the value to be within bounds

SEVERE – a severe error.  Usually the program would terminate if this is in a "GetInput" routine.  If during execution, the calling program could reset the value but RangeCheck contains too many string comparisons to be called for an execution problem.

FATAL – not likely to be used.  You want to provide a context to the error and if really a fatal type error, you'd like to execute the RangeCheck call and then terminate from the calling program.

And the context for the message may be shown in the calling routine by checking the value of ErrorsFound:

~~~~~~~~~~~~~~~~~~~~

    ErrFound=.false.
    Call RangeCheck(ErrFound,'This field','SEVERE','<= 100',(Value<100.))
    IF (ErrFound) THEN
      CALL ShowContinueError('Occurs in routine xyz')
      ErrorsFound=.true.  ! for later termination
    ENDIF
~~~~~~~~~~~~~~~~~~~~

## MakeUPPERCase

This function can be used to make sure an upper case string is being used.  (Note this is not needed when using "SameString").  Parameter 1 to the function is the string to be upper cased:

~~~~~~~~~~~~~~~~~~~~

    USE InputProcessor, ONLY: MakeUPPERCase
    ---
    UCString=MakeUPPERCase('lower string')
~~~~~~~~~~~~~~~~~~~~