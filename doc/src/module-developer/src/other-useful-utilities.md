# Other Useful Utilities

## GetNewUnitNumber

Rather than attempt to keep track of all open files and distribute this list to everyone, we have chosen to use a routine that does this operation.  If you need to have a scratch file (perhaps when porting legacy code into EnergyPlus modules), you can use the GetNewUnitNumber function to determine a logical file number for the OPEN and READ/WRITE commands.  The function works by looking at all open assigned files and returning a number that isn't being used.  This implies that you will OPEN the unit immediately after calling the function (and you should!).

~~~~~~~~~~~~~~~~~~~~

    Example:
    INTEGER, EXTERNAL :: GetNewUnitNumber
    …
    myunit=GetNewUnitNumber()
    OPEN(Unit=myunit,File='myscratch')
~~~~~~~~~~~~~~~~~~~~

## FindUnitNumber

If you want to find out a unit number for a file you think is already open, you can use the FindUnitNumber function.  For example, rather than creating a new unit for debug output, you could latch onto the same unit as currently used for the "eplusout.dbg" file.

~~~~~~~~~~~~~~~~~~~~

    Example:
    INTEGER, EXTERNAL :: FindUnitNumber
    …
    myunit=FindUnitNumber('eplusout.dbg')
~~~~~~~~~~~~~~~~~~~~

If that file is already opened, it will get back the unit number it is currently assigned to.  If it is not opened or does not exist, it will go ahead, get a unit number, and OPEN the file.  (Should not be used for Direct Access or Binary files!)

## FindNumberinList

Sometimes you would like to find a number in a list.  This is applicable to integers only (e.g. Index numbers of some item).

~~~~~~~~~~~~~~~~~~~~

    Example:
    INTEGER, EXTERNAL :: FindNumberInList
    …
    MatchingCooledZoneNum =  &
       FindNumberinList(CtrlZoneNum,  &
         AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums,NumZonesCooled)
~~~~~~~~~~~~~~~~~~~~

The location/index in the array **AirToZoneNodeInfo%CoolCtrlZoneNums** will be returned if it finds the number in the array.  If 0 is returned, it did not find that number in the list.

## ValidateComponent

Many objects specify a component type as well as a component name.  Or, an object might have only a component name.  The ValidateComponent routine will allow for objects outside the scope of a current "GetInput" routine to verify that the specific component does exist in the input file.

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE ValidateComponent(CompType,CompName,IsNotOK,CallString)
~~~~~~~~~~~~~~~~~~~~

CompType, CompName are the typical nomenclature for "Component Type" (e.g. Fan:Simple:OnOff) and "Component Name" (e.g. "my fan" – user specified).  IsNotOk is a logical from the calling program that is set to true when the component is not on the input file.  CallString should specify the calling object – so that an appropriate error message can be issued.

~~~~~~~~~~~~~~~~~~~~

    Example:
      ! No USE needed – straightforward routine in GeneralRoutines
      CALL ValidateComponent(Furnace(FurnaceNum)%FanType,  &
                       Furnace(FurnaceNum)%FanName,IsNotOK,  &
                       'Furnace:BlowThru:HeatOnly')
      IF (IsNotOK) THEN
        CALL ShowContinueError('In Furnace='//  &
                             TRIM(Furnace(FurnaceNum)%Name))
        ErrorsFound=.true.
      ENDIF
~~~~~~~~~~~~~~~~~~~~

Note that in the example, the FanType is entered by the user.  This allows for ultimate flexibility though the example could also include appropriate fan types that are inherent to the code (an acceptable, if somewhat inflexible, practice).

## CheckComponent

This routine is exactly like ValidateComponent but doesn't generate an error message.  It could be used instead of ValidateComponent and you could use the "IsNoOK" to generate your own error message.  However, the intended use is for checking out different components when you don't have the component type as a field for the object.  Thus, you can easily check if there is an object (component type) with the name entered in your field.

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE CheckComponent(CompType,CompName,IsNotOK)
~~~~~~~~~~~~~~~~~~~~

CompType, CompName are the typical nomenclature for "Component Type" (e.g. Fan:OnOff) and "Component Name" (e.g. "my fan" – user specified).  IsNotOk is a logical from the calling program that is set to true when the component is not on the input file.

~~~~~~~~~~~~~~~~~~~~

    Example:
      ! No USE needed – straightforward routine in GeneralRoutines
      CALL CheckComponent('Furnace:BlowThru:HeatOnly',  &
                       FurnaceRefName,IsNotOK)
      IF (IsNotOK) THEN
        CALL CheckComponent('Furnace:BlowThru:HeatCool',  &
                       FurnaceRefName,IsNotOK)
       . . . more checks on IsNotOK
             ELSE
               FurnaceType='Furnace:BlowThru:HeatOnly'
      ENDIF
    . . .
~~~~~~~~~~~~~~~~~~~~

Note that in the example, the FurnaceRefName is entered by the user.  And this module knows what kind of components it might be.

## CreateSysTimeIntervalString

A very important part of EnergyPlus simulation is to be able to alert the user to problems during the simulation.  The CreateSysTimeIntervalString will help do that though a better use is the ShowContinueErrorTimeStamp routine.  The routine has no argument – a string is returned.  The example below also illustrates the preferred method of counting how many times an error is produced and not printing each occurrence.

~~~~~~~~~~~~~~~~~~~~

    Example:
    USE General, ONLY: CreateSysTimeInterval
    ---
    !The warning message will be suppressed during the warm up days.
    If (.NOT.WarmUpFlag) Then
      ErrCount = ErrCount + 1
      IF (ErrCount < 15) THEN
        CALL ShowWarningError('SimAirLoops: Max iterations exceeded for '// &
            TRIM(PrimaryAirSystem(AirLoopNum)%Name)//', at '//  &
            TRIM(EnvironmentName)//', '//TRIM(CurMnDy)//' '//   &
            TRIM(CreateSysTimeIntervalString()))
      ELSE
        IF (MOD(ErrCount,50) == 0) THEN
          WRITE(CharErrOut,*) ErrCount
          CharErrOut=ADJUSTL(CharErrOut)
          CALL ShowWarningError ('SimAirLoops: Exceeding max iterations'// &
                                ' continues...'//CharErrOut)
        ENDIF
      ENDIF
    End If
~~~~~~~~~~~~~~~~~~~~

## TrimSigDigits

Along with error messages to alert the user, oftentimes you'd like to include values that are in error.  You can use what some of the examples have shown – Write(string,\*) value but that will produce many digits in real numbers.  The TrimSigDigits routine will allow for easy modification to a set of digits.  Note that there are two flavors (INTERFACE statement in module General) so that you can easily get the string value of an integer.

~~~~~~~~~~~~~~~~~~~~

    FUNCTION TrimSigDigits(RealValue,SigDigits) RESULT(OutputString)
~~~~~~~~~~~~~~~~~~~~

And

~~~~~~~~~~~~~~~~~~~~

    FUNCTION TrimSigDigits(IntegerValue) RESULT(OutputString)
~~~~~~~~~~~~~~~~~~~~

As seen in the following example of use in code, a real value is passed in as argument 1 and the number of digits desired is passed in as argument 2.  Note that the routine will preserve any "E+xx" outputs when a value like .000000004 might be passed in.

~~~~~~~~~~~~~~~~~~~~

    USE General, ONLY: TrimSigDigits
    . . .
    CALL ShowWarningError('COIL:Water:DetailedFlatCooling in Coil ='//  &
                         TRIM(WaterCoil(coilNum)%Name))
    CALL ShowContinueError('Air Flow Rate Velocity has greatly exceeded '// &
                           'upper design guildelines of ~2.5 m/s')
    CALL ShowContinueError('Air MassFlowRate[kg/s]='//  &
                         TRIM(TrimSigDigits(AirMassFlow,6)))
           AirVelocity=AirMassFlow*AirDensity/WaterCoil(CoilNum)%MinAirFlowArea
    CALL ShowContinueError('Air Face Velocity[m/s]='//  &
                         TRIM(TrimSigDigits(AirVelocity,6)))
    CALL ShowContinueError('Approximate MassFlowRate limit for Face '// &
                           Area[kg/s]='//  &                              TRIM(TrimSigDigits(2.5*WaterCoil(CoilNum)%MinAirFlowArea/AirDensity,6)))
    CALL ShowContinueError('COIL:Water:DetailedFlatCooling could be '// &
                           'resized/autosized to handle capacity')
    CoilWarningOnceFlag(CoilNum) = .False.
~~~~~~~~~~~~~~~~~~~~

## RoundSigDigits

Similar to TrimSigDigits, the RoundSigDigits function may be used when you want to "round" the output string – perhaps for reporting and/or error messages.  Note that there are two flavors (INTERFACE statement in module General) so that you can easily get the string value of an integer.

~~~~~~~~~~~~~~~~~~~~

    FUNCTION RoundSigDigits(RealValue,SigDigits) RESULT(OutputString)
~~~~~~~~~~~~~~~~~~~~

And

~~~~~~~~~~~~~~~~~~~~

    FUNCTION RoundSigDigits(IntgerValue) RESULT(OutputString)
~~~~~~~~~~~~~~~~~~~~

As seen in the following example of use in code, a real value is passed in as argument 1 and the number of digits desired is passed in as argument 2.    Note that the routine will preserve any "E+xx" outputs when a value like .000000004 might be passed in.

~~~~~~~~~~~~~~~~~~~~

    USE General, ONLY: RoundSigDigits
    . . .
        LatOut=RoundSigDigits(Latitude,2)
        LongOut=RoundSigDigits(Longitude,2)
        TZOut=RoundSigDigits(TimeZoneNumber,2)
        NumOut=RoundSigDigits(Elevation,2)
        PressOut=RoundSigDigits(StdBaroPress,0)
        Write(OutputFileInits,LocFormat) Trim(LocationTitle),TRIM(LatOut),  &
                                         TRIM(LongOut),  &
                                         TRIM(TZOut),  &
                                         TRIM(NumOut),  &
                                         TRIM(PressOut)
~~~~~~~~~~~~~~~~~~~~

## SafeDivide

SafeDivide can be used when you might not be sure that the denominator in a divide will not be zero.

~~~~~~~~~~~~~~~~~~~~

    FUNCTION SafeDivide(a, b) RESULT (c)
    USE General, ONLY: SafeDivide
    . . .
    Result=SafeDivide(A,B)
~~~~~~~~~~~~~~~~~~~~

## SetupAndSort

SetupAndSort can be called to order/sort a character array.  A companion index array goes along with it so that one does not have to supply an entire derived type to be sorted.  This companion array is then used to point to the proper element of such structures.

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE SetupAndSort(CharacterList, iCharacterList)
    USE SortAndStringUtilities, ONLY: SetupAndSort
    . . .
    A use:
    ALLOCATE(iCharacterList(number of entries))
    Do item=1,number of entries
      iCharacterList(item)=item
    end do

    ! routine sorts this array and its companion
    CALL SetUpAndSort(CharacterList,iCharacterList)

    Do item=1,number of entries
      ! iCharacterList now points to actual structure
      Write(output,*) Structure(iCharacterList(item))%Name
    Enddo
~~~~~~~~~~~~~~~~~~~~