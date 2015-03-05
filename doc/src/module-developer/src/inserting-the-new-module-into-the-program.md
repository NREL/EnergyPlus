# Inserting the New Module into the Program

Let us return to our example new module NewHVACComponent.  Since the module gets its own input and output, adding the NewHVACComponent model to the program simply means adding a call to the driver routine SimNewHVACComponent from the correct place in EnergyPlus.  In the simplest case, there is only one location from which the driver routine should be called. In some cases, though, more than one HVAC simulation block will need to use the new component model.  SimulateWaterCoilComponents, for instance, can be used in both zone equipment and air systems for heating, reheating and cooling coils. In the air system simulation it is called from two places: the main air system simulation, and the mixed air simulation – the outside air duct might contain a separate cooling coil.

Let us assume that the NewHVACComponent will be part of the air system – perhaps it is a solid desiccant wheel.  Examining the air system calling tree we see that SimAirLoopComponent is one routine that will invoke the new component, and - if we want the component to possibly be in the outside air stream – then SimOAComponent is the other routine that will need to call the new component simulation. Generally, all that is involved is adding a new CASE statement to a Fortran SELECT construct.  For instance in SimAirLoopComponent this would look like:

~~~~~~~~~~~~~~~~~~~~

    SELECT CASE(CompType_Num)

      CASE(OAMixer_Num)  ! 'OUTSIDE AIR SYSTEM'
        CALL ManageOutsideAirSystem( …)

    ! Fan Types for the air sys simulation
      CASE(Fan_Simple_CV)  ! 'FAN:SIMPLE:CONSTVOLUME'
        CALL SimulateFanComponents(…)

      CASE(Fan_Simple_VAV)  ! 'FAN:SIMPLE:VARIABLEVOLUME'
        CALL SimulateFanComponents(…)

    ! Coil Types for the air sys simulation
    !                         'COIL:DX:COOLINGHEATEXCHANGERASSISTED'
      CASE(DXCoil_CoolingHXAsst)
          CALL SimHXAssistedCoolingCoil(…)
    !                         'COIL:WATER:COOLINGHEATEXCHANGERASSISTED'  CASE(WaterCoil_CoolingHXAsst)
        CALL SimHXAssistedCoolingCoil(…)
      CASE(WaterCoil_SimpleHeat)  ! 'COIL:WATER:SIMPLEHEATING'
        CALL SimulateWaterCoilComponents(…)
      CASE(SteamCoil_AirHeat)  ! 'COIL:STEAM:AIRHEATING'
        CALL SimulateSteamCoilComponents(…)
      CASE(WaterCoil_DetailedCool)  ! 'COIL:WATER:DETAILEDFLATCOOLING'
        CALL SimulateWaterCoilComponents(…)
      CASE(WaterCoil_Cooling)  ! 'COIL:WATER:COOLING'
        CALL SimulateWaterCoilComponents(…)
      CASE(Coil_ElectricHeat)  ! 'COIL:ELECTRIC:HEATING'
        CALL SimulateHeatingCoilComponents(…)
      CASE(Coil_GasHeat)  ! 'COIL:GAS:HEATING'
        CALL SimulateHeatingCoilComponents(…)
    ! Heat reclaim
      CASE(Coil_DeSuperHeat)  ! 'COIL:DESUPERHEATER:HEATING'
        CALL SimulateHeatingCoilComponents(…)

      CASE(DXSystem)  ! 'DXSYSTEM:AIRLOOP'
        CALL SimDXCoolingSystem(…)

      CASE(Furnace_UnitarySys)  ! 'FURNACE:BLOWTHRU:HEATONLY',
                                ! 'FURNACE:BLOWTHRU:HEATCOOL',
                                ! 'UNITARYSYSTEM:BLOWTHRU:HEATONLY',
                                ! 'UNITARYSYSTEM:BLOWTHRU:HEATCOOL'
                                ! 'UNITARYSYSTEM:HEATPUMP:AIRTOAIR',
                                ! 'UNITARYSYSTEM:HEATPUMP:WATERTOAIR'
        CALL SimFurnace(…)

    ! Humidifier Types for the air system simulation
      CASE(Humidifier)  ! 'HUMIDIFIER:STEAM:ELECTRICAL'
        CALL SimHumidifier(…)

    ! Evap Cooler Types for the air system simulation
      CASE(EvapCooler)  ! 'EVAPCOOLER:DIRECT:CELDEKPAD',
                        ! 'EVAPCOOLER:INDIRECT:CELDEKPAD'
                        ! 'EVAPCOOLER:INDIRECT:WETCOIL',
                        ! 'EVAPCOOLER:INDIRECT:RDDSPECIAL'
        CALL SimEvapCooler(…)

    ! Desiccant Dehumidifier Types for the air system simulation
      CASE(Desiccant)  ! 'DESICCANT DEHUMIDIFIER:SOLID'
        CALL SimDesiccantDehumidifier(…)

    ! Heat recovery
      CASE(HeatXchngr)  ! 'HEAT EXCHANGER:AIR TO AIR:FLAT PLATE'
        CALL SimHeatRecovery(…)

    ! Ducts
      CASE(Duct)  ! 'DUCT'
        CALL SimDuct(…)

    ! New HVAC Component
      CASE (NewHVACCompNum) ! 'NEW HVAC COMPONENT'
        CALL SimNewHVACComponent(…)

      DEFAULT

    END SELECT
~~~~~~~~~~~~~~~~~~~~

The new code is italicized.  Do the same thing in SimOAComponent and you are done!  Note that "NEW HVAC COMPONENT" is the class name (keyword) for the new component in the IDD file.  The class names are converted to upper case in EnergyPlus, so the CASE statement must have the class name in upper case.  The actual class name on the IDD file would probably be "New HVAC Component".

If the new HVAC component is a piece of zone equipment – a cooled beam system, for instance – then the zone equipment calling tree indicates that the call to SimNewHVACComponent would be in SimZoneEquipment. If the new component is a gas fired absorption chiller, the call would be in SimPlantEquip.

In every case, since NewHVACComponent is a new module, a USE statement must be added to the calling subroutine. For instance in SimAirLoopComponent this would look like:

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE SimAirLoopComponent(CompType, CompName, FirstHVACIteration, LastSim)

              ! SUBROUTINE INFORMATION
              !             AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
              !       DATE WRITTEN:  Oct 1997
              !           MODIFIED:  Dec 1997 Fred Buhl
              !      RE-ENGINEERED:  This is new code, not reengineered

              ! PURPOSE OF THIS SUBROUTINE:
              ! Calls the individual air loop component simulation routines

              ! METHODOLOGY EMPLOYED: None

              ! REFERENCES: None

              ! USE Statements
      USE Fans,       Only:SimulateFanComponents
      USE WaterCoils, Only:SimulateWaterCoilComponents
      USE MixedAir,   Only:ManageOutsideAirSystem
      USE NewHVACComponent,    Only:SimNewHVACComponent
~~~~~~~~~~~~~~~~~~~~