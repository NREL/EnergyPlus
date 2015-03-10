# Simulation Manager 

The simulation manager of EnergyPlus is contained in a single module. The main subroutine is shown below. Flow within the entire program is managed using a series of flags. These paired flags, in order (from the highest to the lowest) are:

Table: Simulation Flags

-------------------|-----------------
BeginSimulationFlag|EndSimulationFlag
BeginEnvironmentFlag|EndEnvironmentFlag(one to many days)
BeginDayFlag|EndDayFlag
BeginHourFlag|EndHourFlag
BeginTimeStepFlag|EndTimeStepFlag

There is also a **WarmupFlag** to signal that the program is in warmup state. The operation of these flags can be seen in the following subroutine. The advantage of using the flag system is that any subroutine throughout the code can determine the exact state of the simulation by checking the status of the flags.

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE ManageSimulation     ! Main driver routine for this module
    BeginSimFlag = .TRUE.
      EndSimFlag = .FALSE.
      CALL OpenOutputFiles
      CALL GetProjectData
      CALL GetEnvironmentInfo                 ! Get the number and type of Environments
      DO Envrn = 1, NumOfEnvrn        ! Begin environment loop ...
        BeginEnvrnFlag = .TRUE.
        EndEnvrnFlag   = .FALSE.
        WarmupFlag     = .TRUE.
        DayOfSim       =  0
        DO WHILE ((DayOfSim.LT.NumOfDayInEnvrn).OR.(WarmupFlag))  ! Begin day loop ...
          DayOfSim     = DayOfSim + 1
          BeginDayFlag = .TRUE.
          EndDayFlag   = .FALSE.
          DO HourOfDay = 1, 24      ! Begin hour loop ...
            BeginHourFlag = .TRUE.
            EndHourFlag   = .FALSE.
              DO TimeStep = 1, NumOfTimeStepInHour  ! Begin time step (TINC) loop ...
              BeginTimeStepFlag = .TRUE.
              EndTimeStepFlag   = .FALSE.
              ! Set the End__Flag variables to true if necessary. Note that each flag builds on
              ! the previous level. EndDayFlag cannot be .true. unless EndHourFlag is also .true., etc.
              ! Note that the EndEnvrnFlag and the EndSimFlag cannot be set during warmup.
              ! Note also that BeginTimeStepFlag, EndTimeStepFlag, and the
              ! SubTimeStepFlags can/will be set/reset in the HVAC Manager.
              IF ((TimeStep.EQ.NumOfTimeStepInHour)) THEN
                EndHourFlag = .TRUE.
                IF (HourOfDay.EQ.24) THEN
                  EndDayFlag = .TRUE.
                  IF ((.NOT.WarmupFlag).AND.(DayOfSim.EQ.NumOfDayInEnvrn)) THEN
                    EndEnvrnFlag = .TRUE.
                    IF (Envrn.EQ.NumOfEnvrn) THEN
                      EndSimFlag = .TRUE.
                    END IF
                  END IF
                END IF
              END IF
              CALL ManageWeather
              CALL ManageHeatBalance
              BeginHourFlag  = .FALSE.
              BeginDayFlag   = .FALSE.
              BeginEnvrnFlag = .FALSE.
              BeginSimFlag   = .FALSE.
            END DO                              ! ... End time step (TINC) loop.
          END DO                    ! ... End hour loop.
        END DO                      ! ... End day loop.
      END DO                        ! ... End environment loop.
      CALL CloseOutputFiles
      RETURN
    END SUBROUTINE ManageSimulation
~~~~~~~~~~~~~~~~~~~~