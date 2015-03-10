# Error Messages

Several error message routines are provided for the developer, indicating three different levels of error severity: Show**Fatal**Error, Show**Severe**Error, Show**Severe**Message, Show**Warning**Error and Show**Warning**Message. Each takes a string as an argument. The string is printed out as the message body on the file "eplusout.err". There are two additional optional arguments, which are file unit numbers on which the message will also be printed. In practice, most modules will not use either file unit number argument – but the Input Processor may use these to make sure some files contain a notice.  **ShowFatalError** causes the program to immediately abort.

Two other error messages can be used to help make the error file more readable: ShowContinueError and ShowContinueErrorTimeStamp.  Finally, another similar ShowMessage call can be used to display an informative string to the error file (eplusout.err).

As indicated, all of the "show" error calls look the same:

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE <ErrorMessageCall>(ErrorMessage,OutUnit1,OutUnit2)
~~~~~~~~~~~~~~~~~~~~

Or

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE ShowWarningError(ErrorMessage,OutUnit1,OutUnit2)
    SUBROUTINE ShowWarningMessage(ErrorMessage,OutUnit1,OutUnit2)
    SUBROUTINE ShowSevereError(ErrorMessage,OutUnit1,OutUnit2)
    SUBROUTINE ShowSevereMessage(ErrorMessage,OutUnit1,OutUnit2)
    SUBROUTINE ShowFatalError(ErrorMessage,OutUnit1,OutUnit2)
    SUBROUTINE ShowContinueError(ErrorMessage,OutUnit1,OutUnit2)
    SUBROUTINE ShowContinueErrorTimeStamp(ErrorMessage,OutUnit1,OutUnit2)
    SUBROUTINE ShowMessage(Message,OutUnit1,OutUnit2)
~~~~~~~~~~~~~~~~~~~~

As stated previously, you would likely never use either of the optional "OutUnit" arguments.  One use might be if you were, in addition to the normal EnergyPlus output files, writing your own output file that would be processed separately.

Format of the error messages should be such that it makes it easy for the developer or user to realize the context of the error.  Obviously, it is usually easier for the developer as he/she can search the code for the error string, but hard for many users.  Current suggested format is to include the Module Name and/or the Routine name.  (see section Standard Message Format for more details). This is particularly useful when two or more places in the code have the same main error string but may mean different things: where one might be in a Plant Loop context and the other in a Condenser Loop context, for example.

Due to the optional parameters, Interface statements are set in DataGlobals and you must enter USE statements defining which of the error calls you wish to use.

~~~~~~~~~~~~~~~~~~~~

    Example:
    USE DataGlobals, ONLY: ShowSevereError
    . . .
    IF (Construct(ConstrNum)%LayerPoint(Layer) == 0) THEN
       CALL ShowSevereError('Did not find matching material for construct ' &
                            //TRIM(Construct(ConstrNum)%Name)// &
                           ', missing material = ' &
                           //TRIM(ConstructAlphas(Layer)))
       ErrorsFound=.true.
    ENDIF
~~~~~~~~~~~~~~~~~~~~

This code segment will produce (with proper conditions) the message onto the error file:

~~~~~~~~~~~~~~~~~~~~

    ** Warning ** Did not find matching material for construct XYZ, missing material = ABC
~~~~~~~~~~~~~~~~~~~~

The ShowContinueError is used in conjunction with either ShowSevereError or ShowWarningError.  The "~~~" characters represent the continuation:

~~~~~~~~~~~~~~~~~~~~

    ** Warning ** The total number of floors, walls, roofs and internal mass surfaces in Zone ZONE ONE
    **   ~~~   ** is < 6. This may cause an inaccurate zone heat balance calculation.
    ** Warning ** No floor exists in Zone=ZONE ONE
    ** Warning ** Surfaces in Zone="ZONE ONE" do not define an enclosure.
    **   ~~~   ** Number of surfaces is <= 4 in this zone. View factor reciprocity forced
~~~~~~~~~~~~~~~~~~~~

The ShowContinueError is particularly useful with some of the previous routines that, in addition to signaling an error, produce their own error message.  For example, see the example code in the ValidateComponent excerpt above.  Note that no ShowContinueError should be used with the ShowFatalError as it immediately terminates the program.  Instead, a Severe-Continue-Fatal sequence should be used.

Each GetInput routine is responsible for verifying its input.  Rather than terminating with the first illegal value, however, it is better to have an "ErrorsFound" logical that gets set to true for error conditions during the main routine processing and terminates at the end of the GetInput routine.  Of course during simulation, conditions should also be checked and terminated if necessary.  Try to give the user as much information as possible with the set of error routine calls.

Quite a complex message can be constructed using concatenation. These routines can also be used to output numeric fields by writing the numeric variables to a string variable, although this isn't very convenient.

A good use of the ContinueErrorTimeStamp as well as "counting" errors is shown below:

~~~~~~~~~~~~~~~~~~~~

          IF(OutDryBulbTemp .LT. 0.0) THEN
            CINErrCount1=CINErrCount1+1
            IF (CINErrCount1 < 15) THEN
              CALL ShowWarningError('ElectricChillerModel:Air Cooled '// &
                    'Condenser Inlet Temperature below 0C')
              CALL ShowContinueErrorTimeStamp('OutDoor Dry Bulb='//  &
                          TRIM(RoundSigDigits(OutDryBulbTemp,2)//','))
            ELSE
              IF (MOD(CINErrCount1,50) == 0) THEN
                WRITE(CINCharErrOut,*) CINErrCount1
                CINCharErrOut=ADJUSTL(CINCharErrOut)
                  CALL ShowWarningError('ElectricChillerModel:Air Cooled'// &
                       ' Condenser Inlet Temperature below 0C continues...' &
                       //CINCharErrOut)
              ENDIF
            ENDIF
          ENDIF
~~~~~~~~~~~~~~~~~~~~

## ShowWarningError, ShowWarningMessage

Both of these calls produce messages onto the .err file that signal a warning:

~~~~~~~~~~~~~~~~~~~~

       ** Warning ** Processing Monthly Tabular Reports: PEAK SPACE GAINS
~~~~~~~~~~~~~~~~~~~~

The important difference between the two calls is that the "Error" call will incrase the "number of warnings" counter whereas the "Message" call does not incrase the counter.  The "Message" call can, therefore, be used ro "start" off a recurring sequence without disturbing the total warning count.  To do this, one would place the calls:

~~~~~~~~~~~~~~~~~~~~

    CALL ShowWarningMessage(xxx)
    <more messages that describe the basic problem>
    CALL ShowRecurringWarningErrorAtEnd(xxx,msgindex)
~~~~~~~~~~~~~~~~~~~~

As indicated, this first call can also show significantly more information about the situation than will be captured by using the Recurring error sequence.

## ShowSevereError, ShowSevereMessage

Both of these calls produce messages onto the .err file that signal a warning:

~~~~~~~~~~~~~~~~~~~~

       ** Severe  ** Node Connection Error, Node="SOFC AIR INLET NODE", ZoneExhaust node did not find a matching inlet node.
~~~~~~~~~~~~~~~~~~~~

The important difference between the two calls is that the "Error" call will incrase the "number of severe errors" counter whereas the "Message" call does not incrase the counter.  The "Message" call can, therefore, be used ro "start" off a recurring sequence without disturbing the total warning count.  To do this, one would place the calls:

~~~~~~~~~~~~~~~~~~~~

    CALL ShowSevereMessage(xxx)
    <more messages that describe the basic problem>
    CALL ShowRecurringSevereErrorAtEnd(xxx,msgindex)
~~~~~~~~~~~~~~~~~~~~

As indicated, this first call can also show significantly more information about the situation than will be captured by using the Recurring error sequence.

## ShowFatalError

This error terminates the program.

~~~~~~~~~~~~~~~~~~~~

       **  Fatal  ** EMS user program halted simulation with error code = 9001.30
~~~~~~~~~~~~~~~~~~~~

For clarity, the sequence ending in the fatal error, should start with a Severe error and give the user a good indication of the problem.  During execution, this Severe error may immediately preceed the Fata call. During get input, errors may be found previously in the input, interspersed with Warning errors.  The last Severe error is stored and displayed as the program terminates.

## ShowContinueError, ShowContinueErrorTimeStamp

Continue errors are shown after the initial condition and formatted in a fashion that a post processor could detect and string all the errors about a single condition together.

The basic format is simple:

~~~~~~~~~~~~~~~~~~~~

       **   ~~~   ** ..Location object=DENVER STAPLETON INTL ARPT CO USA WMO=724690
~~~~~~~~~~~~~~~~~~~~

The continue error with time stemp adds the time stamp information to the initial message. (It also might show if the error occurred during "warmup").

~~~~~~~~~~~~~~~~~~~~

       **   ~~~   **  CalcMultiSpeedDXCoil:lowspeedoutlet Occurrence info=Chicago Ohare Intl Ap IL USA TMY3 WMO#=725300, 04/24 15:45 - 16:00
~~~~~~~~~~~~~~~~~~~~

## ShowMessage

This call is strictly for informative messages that are displayed to the .err file. For example:

~~~~~~~~~~~~~~~~~~~~

       ************* Beginning Zone Sizing Calculations
       ************* Beginning System Sizing Calculations
       ************* Testing Individual Branch Integrity
       ************* All Branches passed integrity testing
~~~~~~~~~~~~~~~~~~~~

The indicated messages help establish context for other errors that may be shown.

## Recurring Error Handling

One method of showing recurring errors is shown in the initial error section description with the illustration of counting the number of times the error occurs, printing the first few times and then only printing every x times (e.g. 100) that it occurs after that.

In addition to that method, three routines will help you automate the task.  These routines rely on the error message being displayed and can also keep track of values (min/max/sum) (and units thereof). And an error message index (pointer to the message in the recurring error structure) that is stored in your data structure is used.

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE ShowRecurringSevereErrorAtEnd(Message,MsgIndex,ReportMaxOf,ReportMinOf,ReportSumOf,  &
                                               ReportMaxUnits,ReportMinUnits,ReportSumUnits)
    SUBROUTINE ShowRecurringWarningErrorAtEnd(Message,MsgIndex,ReportMaxOf,ReportMinOf,ReportSumOf, &
                                               ReportMaxUnits,ReportMinUnits,ReportSumUnits)
    SUBROUTINE ShowRecurringContinueErrorAtEnd(Message,MsgIndex,ReportMaxOf,ReportMinOf,ReportSumOf, &
                                               ReportMaxUnits,ReportMinUnits,ReportSumUnits)
~~~~~~~~~~~~~~~~~~~~

The first two parameters (Message, MsgIndex) are required.  The remaining six arguments (ReportMaxOf, ReportMinOf, ReportSumOf, ReportMaxUnits, ReportMinUnits, ReportSumUnits) are optional. To illustrate, we re-write the above call using the recurring error routines.  (Note that we still do the first few counted because we are using the TimeStamp routine – however a message buffer is set up in this instance.).

~~~~~~~~~~~~~~~~~~~~

    !   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
    !   Wait for next time step to print warnings. If simulation iterates, print out
    !   the warning for the last iteration only. Must wait for next time step to accomplish this.
    !   If a warning occurs and the simulation down shifts, the warning is not valid.
        IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
          IF(ElectricChiller(ChillNum)%PrintMessage)THEN
              ElectricChiller(ChillNum)%MsgErrorCount = &
                               ElectricChiller(ChillNum)%MsgErrorCount + 1
    !       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
            IF (ElectricChiller(ChillNum)%MsgErrorCount < 2) THEN
               CALL ShowWarningError(TRIM(ElectricChiller(ChillNum)%MsgBuffer1)//'.')
               CALL ShowContinueError(TRIM(ElectricChiller(ChillNum)%MsgBuffer2))
            ELSE
              CALL ShowRecurringWarningErrorAtEnd(TRIM(ElectricChiller(ChillNum)%MsgBuffer1)//' error continues.', &
                 ElectricChiller(ChillNum)%ErrCount1,ReportMaxOf=ElectricChiller(ChillNum)%MsgDataLast,  &
                 ReportMinOf=ElectricChiller(ChillNum)%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
            END IF
          END IF
        END IF
~~~~~~~~~~~~~~~~~~~~

Illustrations of use of these calls is seen in the Chiller modules, PurchasedAir modules, DXCoil modules and others.

Another example is seen in the Dessicant routines:

~~~~~~~~~~~~~~~~~~~~

      IF (Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate .NE. &
                   RegenAirMassFlowRate) THEN
        ! Initialize standard air density
        IF (MyOneTimeFlag) THEN
          RhoAirStdInit = PsyRhoAirFnPbTdbW(StdBaroPress,20.0,0.0)
        ENDIF
        CALL ShowRecurringSevereErrorAtEnd( &
          'Improper flow delivered by desiccant regen fan - RESULTS INVALID!
                 Check regen fan capacity and schedule.', &
               DesicDehum(DesicDehumNum)%RegenFanErrorIndex1)
        CALL ShowRecurringContinueErrorAtEnd( &
               TRIM(DesicDehum(DesicDehumNum)%DehumType)//'='//  &
                 TRIM(DesicDehum(DesicDehumNum)%Name), &
               DesicDehum(DesicDehumNum)%RegenFanErrorIndex2)
        RhoAirStdInit = PsyRhoAirFnPbTdbW(StdBaroPress,20.0,0.0)
        CALL ShowRecurringContinueErrorAtEnd( &
               TRIM('Flow requested [m3/s] from '//  &
                DesicDehum(DesicDehumNum)%RegenFanType)//'='// &
               TRIM(DesicDehum(DesicDehumNum)%RegenFanName), &
               DesicDehum(DesicDehumNum)%RegenFanErrorIndex3, &
               ReportMaxOf=(RegenAirMassFlowRate / RhoAirStdInit))
        CALL ShowRecurringContinueErrorAtEnd( &
               'Flow request varied from delivered by [m3/s]', &
               DesicDehum(DesicDehumNum)%RegenFanErrorIndex4, &
        ReportMaxOf=((RegenAirMassFlowRate - Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate)/ RhoAirStdInit), &
        ReportMinOf=((RegenAirMassFlowRate - Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate)/ RhoAirStdInit))
      ENDIF
~~~~~~~~~~~~~~~~~~~~

## Standard Message Format

With the advent of using the field names as provided from the Input Processor, some of the message formatting has gotten easier for the developer, but harder for the person who might be searching the code to determine a user's problem.

Suggestion is that the error messages contain the Routine Name. And, for get input routines, the initial portion contain the Object Name and the Name of the object.  For example,

~~~~~~~~~~~~~~~~~~~~

    ** Warning ** GetPlantLoopData/GetPlantAvailabilityManager: AvailabilityManagerAssignmentList=ALWAYS_ON not found in lists.  No availability will be used.
~~~~~~~~~~~~~~~~~~~~

Similarly, note that this is a summary message after surfaces have been gotten:

~~~~~~~~~~~~~~~~~~~~

    ** Warning ** GetSurfaceData:The total number of floors, walls, roofs and internal mass surfaces in Zone GARAGE ZONE
    **   ~~~   ** is < 6. This may cause an inaccurate zone heat balance calculation.
~~~~~~~~~~~~~~~~~~~~

Not all of the code is using this style yet.