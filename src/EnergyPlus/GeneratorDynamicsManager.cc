// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGenerators.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/GeneratorDynamicsManager.hh>
#include <EnergyPlus/MicroCHPElectricGenerator.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>

namespace EnergyPlus {

namespace GeneratorDynamicsManager {

    //_______________________________________________
    // Utility modules used by other generators.
    //
    // GeneratorDynamicsManager
    //   reused among some generators to on/off state, transient limits, control implications etc.

    // Module containing the routines dealing with the management of dynamic constraints on Generator response

    // MODULE INFORMATION:
    //       AUTHOR        B. Griffith
    //       DATE WRITTEN   July 2006

    // PURPOSE OF THIS MODULE:
    // collect routines for managing generator states
    // reused by different generator models
    //  determine response that generator is capable of providing
    //  given load request data
    //   models requiring calculations across timesteps

    using namespace DataGenerators;
    using DataGlobals::CurrentTime;
    void SetupGeneratorControlStateManager(int const GenNum) // index of generator to setup
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   July 2006

        // PURPOSE OF THIS SUBROUTINE:
        // sets up data structures

        // METHODOLOGY EMPLOYED:
        // like a get input routine but feeds from
        //  parent objects, could have its own input object someday


        // get the number of generators that might use this module
        NumGensWDynamics = MicroCHPElectricGenerator::NumMicroCHPs; // TODO  + NumFuelCellCGenerators

        if (!allocated(GeneratorDynamics)) {
            GeneratorDynamics.allocate(NumGensWDynamics);
        }

        // first populate with Micro CHP data

        GeneratorDynamics(GenNum).Name = MicroCHPElectricGenerator::MicroCHP(GenNum).Name;
        GeneratorDynamics(GenNum).PelMin = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.MinElecPower;
        GeneratorDynamics(GenNum).PelMax = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.MaxElecPower;
        GeneratorDynamics(GenNum).UpTranLimit = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.DeltaPelMax;
        GeneratorDynamics(GenNum).DownTranLimit = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.DeltaPelMax;
        GeneratorDynamics(GenNum).UpTranLimitFuel = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.DeltaFuelMdotMax;
        GeneratorDynamics(GenNum).DownTranLimitFuel = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.DeltaFuelMdotMax;
        GeneratorDynamics(GenNum).WarmUpByTimeDelay = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.WarmUpByTimeDelay;
        GeneratorDynamics(GenNum).WarmUpByEngineTemp = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.WarmUpByEngineTemp;
        GeneratorDynamics(GenNum).MandatoryFullCoolDown = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.MandatoryFullCoolDown;
        GeneratorDynamics(GenNum).WarmRestartOkay = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.WarmRestartOkay;
        GeneratorDynamics(GenNum).WarmUpDelay = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.WarmUpDelay;
        GeneratorDynamics(GenNum).CoolDownDelay = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.CoolDownDelay / DataGlobalConstants::SecInHour(); // seconds to hours
        GeneratorDynamics(GenNum).PcoolDown = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.PcoolDown;
        GeneratorDynamics(GenNum).Pstandby = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.Pstandby;
        GeneratorDynamics(GenNum).MCeng = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.MCeng;
        GeneratorDynamics(GenNum).MCcw = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.MCcw;
        GeneratorDynamics(GenNum).kf = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.kf;
        GeneratorDynamics(GenNum).TnomEngOp = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.TnomEngOp;
        GeneratorDynamics(GenNum).kp = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.kp;
        GeneratorDynamics(GenNum).AvailabilitySchedID = MicroCHPElectricGenerator::MicroCHP(GenNum).AvailabilitySchedID;
        GeneratorDynamics(GenNum).StartUpTimeDelay = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.WarmUpDelay / DataGlobalConstants::SecInHour(); // seconds to hours

        GeneratorDynamics(GenNum).ElectEffNom = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.ElecEff;
        GeneratorDynamics(GenNum).ThermEffNom = MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.ThermEff;
        GeneratorDynamics(GenNum).QdotHXMax =
            MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.ThermEff * MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.MaxElecPower / MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.ElecEff;
        GeneratorDynamics(GenNum).QdotHXMin =
            MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.ThermEff * MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.MinElecPower / MicroCHPElectricGenerator::MicroCHP(GenNum).A42Model.ElecEff;
        GeneratorDynamics(GenNum).QdotHXOpt = GeneratorDynamics(GenNum).QdotHXMax;
        MicroCHPElectricGenerator::MicroCHP(GenNum).DynamicsControlID = GenNum;
    }

    void ManageGeneratorControlState(EnergyPlusData &state,
                                     GeneratorType const GeneratorType,  // type of Generator
                                     std::string const &EP_UNUSED(GeneratorName), // user specified name of Generator
                                     int const GeneratorNum,                      // Generator number
                                     bool const RunFlagElectCenter,               // TRUE when Generator operating per electric load center request
                                     bool const RunFlagPlant,                     // TRUE when generator operating per Plant request (always false)
                                     Real64 const ElecLoadRequest,                // Generator Electrical power demand
                                     Real64 const ThermalLoadRequest,             // cogenerator Thermal power demand
                                     Real64 &ElecLoadProvided,                    // power allowed
                                     int &OperatingMode,                          // operating mode
                                     Real64 &PLRforSubtimestepStartUp,            // part load ratio for switch to normal from start up
                                     Real64 &PLRforSubtimestepShutDown,           // part load ratio for switch from cool down to other
                                     bool const EP_UNUSED(FirstHVACIteration)     // True is this is first HVAC iteration
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   February-March 2007  (replaced July 2006 attempt)
        //       MODIFIED       Dec 2009, check and constrain with flow available from plant
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // provide a service to other generators to make decisions, mostly temporal, or cross-timestep issues
        //  used to model internal controlling issues within an individual generator model
        //  This subroutine determines the current operating mode and returns the allowed power and
        // and part load ratio for certain sub-time step switching e.g. in and out of normal mode or cool down mode

        // METHODOLOGY EMPLOYED:
        // model controls-related issues, rules based algorithm
        // Control decision results include:
        //     -- electrical load allowed/resulting/provided
        //     -- new operating mode
        //     -- part load this timestep for shift to normal mode occuring midway in timestep
        //     -- part load this timestep for shift out of cool down mode

        // Input data used to make control decisions include:
        //     -- Electrical load request
        //     -- Thermal Load request
        //     -- RunFlagElectricCenter
        //     -- RunFlagPlant
        //     -- previous timestep operating mode
        //     -- previous timestep Power generated
        //     -- availability schedule (off if not available)
        //     -- Generator control parameter constants including
        //           ** Start Up Time Delay  (in hours)
        //           ** Cool-down time delay (in hours)
        //     -- Expected Plant flow rate
        //     -- minimum cooling water flow rate

        // Algorithm summary
        //   1.  examine calling run flags and refine electric load request to account for
        //       thermal load requests (not yet ready for prime time)
        //   2.  Determine states of various control inputs that change during simulation
        //   3.  enter case statement based on previous operating mode.
        //       --  decide on current operating mode
        //       --  calculate part loads

        //   4.  based on current operating mode determine allowed/provided electrical load
        //        a. set allowed elec load by mode
        //        b. set allowed elec load by constraints on rate of change
        //        c. set allowed elec load by min and max

        //   5.  Calculated part load ratios for special cases.
        // REFERENCES:
        // controls specifications in Annex 42 model specs.
        // Using/Aliasing
        using namespace DataGlobalConstants;
        using DataHVACGlobals::SysTimeElapsed;
        using DataHVACGlobals::TimeStepSys;
        using DataLoopNode::Node;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleIndex;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // this is the part in normal mode
        // this is the part in cool down mode.

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool RunFlag;                           // true if generator supposed to run
        int DynaCntrlNum(0);                    // index in GeneratorDynamics structure for this generator          ! na
        Real64 CurrentFractionalDay;            // working var, time in decimal days
        Real64 EndingFractionalDay;             // working var, time is decimal days
        Real64 LastSystemTimeStepFractionalDay; // working var, time is decimal days
        Real64 MaxPel;                          // working variable for max allowed by transient constraint
        Real64 MinPel;                          // working variabel for min allowed by transient constraint
        Real64 PelInput;                        // holds initial value of IN var
        Real64 Pel;
        int newOpMode(0);
        Real64 SchedVal;
        //  REAL(r64)    :: PelDiff
        Real64 ElectLoadForThermalRequest;
        bool ConstrainedMaxP;           // true if request was altered because of max power limit
        bool ConstrainedMinP;           // true if request was altered because of min power limit
        bool ConstrainedIncreasingPdot; // true if request was altered because of power rate of change up
        bool ConstrainedDecreasingPdot; // true if request was altered because of power rate of change down
        bool ConstrainedByPlant;        // true if request was altered because of cooling water problem
        bool PLRStartUp;                // true if subtimestep issue involving startup
        bool PLRShutDown;
        //  INTEGER :: OutletCWnode        = 0 ! cooling water outlet node ID
        static int InletCWnode(0); // cooling water inlet node ID
        static bool InternalFlowControl(false);
        static Real64 TcwIn(0.0);          // inlet cooling water temperature (C)
        static Real64 TrialMdotcw(0.0);    // test or estimate of what the plant flows are going to be (kg/s)
        static Real64 LimitMinMdotcw(0.0); // lower limit for cooling water flow for generatior operation (kg/s)

        // inits
        PLRforSubtimestepStartUp = 1.0;
        PLRforSubtimestepShutDown = 0.0;
        ConstrainedMaxP = false;
        ConstrainedMinP = false;
        ConstrainedIncreasingPdot = false;
        ConstrainedDecreasingPdot = false;
        ConstrainedByPlant = false;
        PLRStartUp = false;
        PLRShutDown = false;
        InternalFlowControl = false;

        // get index for this generator in dynamics control structure
        {
            auto const SELECT_CASE_var(GeneratorType);
            if (SELECT_CASE_var == GeneratorType::MicroCHP) {
                DynaCntrlNum = MicroCHPElectricGenerator::MicroCHP(GeneratorNum).DynamicsControlID;
                // OutletCWnode = MicroCHPElectricGenerator::MicroCHP(GeneratorNum)%PlantOutletNodeID
                InletCWnode = MicroCHPElectricGenerator::MicroCHP(GeneratorNum).PlantInletNodeID;
                TcwIn = Node(MicroCHPElectricGenerator::MicroCHP(GeneratorNum).PlantInletNodeID).Temp;
                if (MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.InternalFlowControl) {
                    InternalFlowControl = true;
                }
                LimitMinMdotcw = MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.MinWaterMdot;
            } else if (SELECT_CASE_var == GeneratorType::FuelCell) {
                // not yet
            } else {
            }
        }

        PelInput = ElecLoadRequest;
        ElectLoadForThermalRequest = 0.0;
        if ((ThermalLoadRequest > 0.0) && RunFlagPlant) { // deal with possible thermal load following
            // Modify electric load request based on thermal load following signal using nominal efficiencies
            ElectLoadForThermalRequest =
                GeneratorDynamics(DynaCntrlNum).ThermEffNom * ThermalLoadRequest / GeneratorDynamics(DynaCntrlNum).ElectEffNom;
            PelInput = max(PelInput, ElectLoadForThermalRequest);
        }

        if ((RunFlagElectCenter) || (RunFlagPlant)) {
            RunFlag = true;
        } else {
            RunFlag = false;
        }

        // check availability schedule
        SchedVal = GetCurrentScheduleValue(GeneratorDynamics(DynaCntrlNum).AvailabilitySchedID);

        Pel = PelInput;

        // get data to check if sufficient flow available from Plant
        if (InternalFlowControl && (SchedVal > 0.0)) {
            TrialMdotcw = FuncDetermineCWMdotForInternalFlowControl(state, GeneratorNum, Pel, TcwIn);
        } else {
            TrialMdotcw = Node(InletCWnode).MassFlowRate;
        }

        // determine current operating mode.
        {
            auto const SELECT_CASE_var(GeneratorDynamics(DynaCntrlNum).LastOpMode);

            if ((SELECT_CASE_var == OpModeOff) || (SELECT_CASE_var == OpModeStandby)) {
                // possible future states {Off, Standby, WarmUp,Normal }
                if (SchedVal == 0.0) {
                    newOpMode = OpModeOff;

                } else if (((SchedVal != 0.0) && (!RunFlag)) || (TrialMdotcw < LimitMinMdotcw)) {
                    newOpMode = OpModeStandby;
                } else if ((SchedVal != 0.0) && (RunFlag)) {

                    if (GeneratorDynamics(DynaCntrlNum).WarmUpByTimeDelay) {

                        if (GeneratorDynamics(DynaCntrlNum).StartUpTimeDelay == 0.0) {
                            newOpMode = OpModeNormal;

                            // is startUp time delay longer than timestep?
                        } else if (GeneratorDynamics(DynaCntrlNum).StartUpTimeDelay >= TimeStepSys) {
                            newOpMode = OpModeWarmUp;
                            // generator just started so set start time
                            GeneratorDynamics(DynaCntrlNum).FractionalDayofLastStartUp =
                                double(state.dataGlobal->DayOfSim) +
                                (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime) - TimeStepSys))) / DataGlobalConstants::HoursInDay();

                        } else { // warm up period is less than a single system time step
                            newOpMode = OpModeNormal;
                            PLRStartUp = true;
                            PLRforSubtimestepStartUp = (TimeStepSys - GeneratorDynamics(DynaCntrlNum).StartUpTimeDelay) / TimeStepSys;
                        }
                    }
                    if (GeneratorDynamics(DynaCntrlNum).WarmUpByEngineTemp) {
                        if (MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.Teng >= GeneratorDynamics(DynaCntrlNum).TnomEngOp) {
                            newOpMode = OpModeNormal;
                            // assume linear interpolation for PLR
                            PLRStartUp = true;
                            if ((MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.Teng - MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.TengLast) > 0.0) {
                                // protect divide by zero or neg
                                PLRforSubtimestepStartUp = (MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.Teng - GeneratorDynamics(DynaCntrlNum).TnomEngOp) /
                                                           (MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.Teng - MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.TengLast);
                            } else {
                                PLRforSubtimestepStartUp = 1.0;
                            }
                        } else {
                            newOpMode = OpModeWarmUp;
                        }
                    }
                }

            } else if (SELECT_CASE_var == OpModeWarmUp) {
                // possible Future states {OFF, WarmUp, Normal, CoolDown }
                // check availability manager
                if (SchedVal == 0.0) {
                    // to off unless cool down time period is needed
                    if (GeneratorDynamics(DynaCntrlNum).CoolDownDelay == 0.0) {
                        newOpMode = OpModeOff;
                    } else {
                        if (GeneratorDynamics(DynaCntrlNum).CoolDownDelay > TimeStepSys) {
                            newOpMode = OpModeCoolDown;
                            // need to reset time of last shut down here
                            GeneratorDynamics(DynaCntrlNum).FractionalDayofLastShutDown =
                                double(state.dataGlobal->DayOfSim) + (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime)))) / DataGlobalConstants::HoursInDay();
                        } else {
                            newOpMode = OpModeOff;
                        }
                    }
                } else if (((SchedVal != 0.0) && (!RunFlag)) || (TrialMdotcw < LimitMinMdotcw)) {
                    // to standby unless cool down time period is needed
                    if (GeneratorDynamics(DynaCntrlNum).CoolDownDelay == 0.0) {
                        newOpMode = OpModeStandby;
                    } else {
                        if (GeneratorDynamics(DynaCntrlNum).CoolDownDelay > TimeStepSys) {
                            newOpMode = OpModeCoolDown;
                            // need to reset time of last shut down here
                            GeneratorDynamics(DynaCntrlNum).FractionalDayofLastShutDown =
                                double(state.dataGlobal->DayOfSim) + (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime)))) / DataGlobalConstants::HoursInDay();

                        } else {
                            newOpMode = OpModeStandby;
                            // assuming no PLR situation unless engine made to normal operation.
                        }
                    }
                } else if ((SchedVal != 0.0) && (RunFlag)) {
                    // either warm up or normal
                    // check if warm up completed, depends on type of warm up control time delay or reach nominal temperature
                    if (GeneratorDynamics(DynaCntrlNum).WarmUpByTimeDelay) {
                        // compare current time to when warm up is over
                        // calculate time for end of warmup period
                        CurrentFractionalDay =
                            double(state.dataGlobal->DayOfSim) + (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime)))) / DataGlobalConstants::HoursInDay();
                        EndingFractionalDay = GeneratorDynamics(DynaCntrlNum).FractionalDayofLastStartUp +
                                              GeneratorDynamics(DynaCntrlNum).StartUpTimeDelay / DataGlobalConstants::HoursInDay();
                        if ((std::abs(CurrentFractionalDay - EndingFractionalDay) < 0.000001) || (CurrentFractionalDay > EndingFractionalDay)) {
                            newOpMode = OpModeNormal;
                            PLRStartUp = true;
                            LastSystemTimeStepFractionalDay = CurrentFractionalDay - (TimeStepSys / DataGlobalConstants::HoursInDay());
                            PLRforSubtimestepStartUp =
                                ((CurrentFractionalDay - EndingFractionalDay) / (CurrentFractionalDay - LastSystemTimeStepFractionalDay));
                        } else {
                            newOpMode = OpModeWarmUp;
                        }

                    } else if (GeneratorDynamics(DynaCntrlNum).WarmUpByEngineTemp) {
                        if (GeneratorType == GeneratorType::MicroCHP) {
                            // only change to normal if this is result from completed timestep, not just an interation
                            if (MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.TengLast >= GeneratorDynamics(DynaCntrlNum).TnomEngOp) {
                                newOpMode = OpModeNormal;
                                // assume linear interpolation for PLR
                                PLRStartUp = true;
                                if ((MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.Teng - MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.TengLast) > 0.0) {
                                    // protect divide by zero or neg
                                    PLRforSubtimestepStartUp = (MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.Teng - GeneratorDynamics(DynaCntrlNum).TnomEngOp) /
                                                               (MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.Teng - MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.TengLast);
                                } else {
                                    PLRforSubtimestepStartUp = 1.0;
                                }
                            } else {
                                newOpMode = OpModeWarmUp;
                            }
                        }
                    } else {
                        // shouldn't come here
                        // Write(*,*) 'problem with warm up type of control logical flags'
                    }
                }

            } else if (SELECT_CASE_var == OpModeNormal) {
                // possible Future states {CoolDown, standby, off}
                if (((SchedVal == 0.0) || (!RunFlag)) || (TrialMdotcw < LimitMinMdotcw)) {
                    // is cool down time delay longer than timestep?
                    if (GeneratorDynamics(DynaCntrlNum).CoolDownDelay == 0.0) {
                        if (SchedVal != 0.0) {
                            newOpMode = OpModeStandby;
                        } else {
                            newOpMode = OpModeOff;
                        }
                    } else if (GeneratorDynamics(DynaCntrlNum).CoolDownDelay >= TimeStepSys) {
                        newOpMode = OpModeCoolDown;
                        // also, generator just shut down so record shut down time
                        GeneratorDynamics(DynaCntrlNum).FractionalDayofLastShutDown =
                            double(state.dataGlobal->DayOfSim) + (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime)))) / DataGlobalConstants::HoursInDay();
                    } else { // cool down period is less than a single system time step
                        if (SchedVal != 0.0) {
                            newOpMode = OpModeStandby;
                        } else {
                            newOpMode = OpModeOff;
                        }
                        PLRShutDown = true;
                        PLRforSubtimestepShutDown = (GeneratorDynamics(DynaCntrlNum).CoolDownDelay) / TimeStepSys;

                        // also, generator just shut down so record shut down time
                        GeneratorDynamics(DynaCntrlNum).FractionalDayofLastShutDown =
                            double(state.dataGlobal->DayOfSim) + (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime)))) / DataGlobalConstants::HoursInDay();
                    }
                } else if ((SchedVal != 0.0) && (RunFlag)) {

                    newOpMode = OpModeNormal;
                }

            } else if (SELECT_CASE_var == OpModeCoolDown) {
                // possible Future States {Standby, OFF, WarmUp, Normal}

                if (SchedVal == 0.0) { // no longer available.
                    // probably goes to off but could be stuck in cool down for awhile
                    if (GeneratorDynamics(DynaCntrlNum).CoolDownDelay > 0.0) {
                        // calculate time for end of cool down period
                        CurrentFractionalDay =
                            double(state.dataGlobal->DayOfSim) + (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime)))) / DataGlobalConstants::HoursInDay();
                        EndingFractionalDay = GeneratorDynamics(DynaCntrlNum).FractionalDayofLastShutDown +
                                              GeneratorDynamics(DynaCntrlNum).CoolDownDelay / DataGlobalConstants::HoursInDay() - (TimeStepSys / DataGlobalConstants::HoursInDay());
                        if ((std::abs(CurrentFractionalDay - EndingFractionalDay) < 0.000001) ||
                            (CurrentFractionalDay > EndingFractionalDay)) { // CurrentFractionalDay == EndingFractionalDay
                            newOpMode = OpModeOff;
                            PLRShutDown = true;
                            LastSystemTimeStepFractionalDay = CurrentFractionalDay - (TimeStepSys / DataGlobalConstants::HoursInDay());
                            PLRforSubtimestepShutDown = (EndingFractionalDay - LastSystemTimeStepFractionalDay) * DataGlobalConstants::HoursInDay() / TimeStepSys;
                        } else { // CurrentFractionalDay > EndingFractionalDay
                            newOpMode = OpModeCoolDown;
                        }
                    } else {
                        newOpMode = OpModeOff;
                    }
                } else if (((SchedVal != 0.0) && (!RunFlag)) || (TrialMdotcw < LimitMinMdotcw)) {
                    // probably goes to standby but could be stuck in cool down for awhile
                    if (GeneratorDynamics(DynaCntrlNum).CoolDownDelay > 0.0) {
                        // calculate time for end of cool down period
                        CurrentFractionalDay =
                            double(state.dataGlobal->DayOfSim) + (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime)))) / DataGlobalConstants::HoursInDay();
                        EndingFractionalDay = GeneratorDynamics(DynaCntrlNum).FractionalDayofLastShutDown +
                                              GeneratorDynamics(DynaCntrlNum).CoolDownDelay / DataGlobalConstants::HoursInDay() - (TimeStepSys / DataGlobalConstants::HoursInDay());
                        if ((std::abs(CurrentFractionalDay - EndingFractionalDay) < 0.000001) ||
                            (CurrentFractionalDay > EndingFractionalDay)) { // CurrentFractionalDay == EndingFractionalDay
                            newOpMode = OpModeStandby;
                            PLRShutDown = true;
                            LastSystemTimeStepFractionalDay = CurrentFractionalDay - (TimeStepSys / DataGlobalConstants::HoursInDay());
                            PLRforSubtimestepShutDown = (EndingFractionalDay - LastSystemTimeStepFractionalDay) * DataGlobalConstants::HoursInDay() / TimeStepSys;
                        } else { // CurrentFractionalDay < EndingFractionalDay
                            newOpMode = OpModeCoolDown;
                        }
                    } else {
                        newOpMode = OpModeStandby;
                    }
                } else if ((SchedVal != 0.0) && (RunFlag)) {
                    // was in cool down mode but is now being asked to restart
                    // probably goes to warm up but could be stuck in cool down or jump to normal
                    if (GeneratorDynamics(DynaCntrlNum).MandatoryFullCoolDown) {
                        // is cool down done or not?
                        if (GeneratorDynamics(DynaCntrlNum).CoolDownDelay > 0.0) {
                            // calculate time for end of cool down period
                            CurrentFractionalDay =
                                double(state.dataGlobal->DayOfSim) + (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime)))) / DataGlobalConstants::HoursInDay();
                            EndingFractionalDay = GeneratorDynamics(DynaCntrlNum).FractionalDayofLastShutDown +
                                                  GeneratorDynamics(DynaCntrlNum).CoolDownDelay / DataGlobalConstants::HoursInDay() - (TimeStepSys / DataGlobalConstants::HoursInDay());
                            if ((std::abs(CurrentFractionalDay - EndingFractionalDay) < 0.000001) ||
                                (CurrentFractionalDay < EndingFractionalDay)) { // CurrentFractionalDay == EndingFractionalDay

                                newOpMode = OpModeCoolDown;
                            } else { // CurrentFractionalDay > EndingFractionalDay
                                // could go to warm up or normal now
                                PLRShutDown = true;
                                LastSystemTimeStepFractionalDay = CurrentFractionalDay - (TimeStepSys / DataGlobalConstants::HoursInDay());
                                PLRforSubtimestepShutDown = (EndingFractionalDay - LastSystemTimeStepFractionalDay) * DataGlobalConstants::HoursInDay() / TimeStepSys;
                                if (GeneratorDynamics(DynaCntrlNum).StartUpTimeDelay == 0.0) {
                                    newOpMode = OpModeNormal;
                                    // possible PLR on start up.
                                    PLRStartUp = true;
                                    PLRforSubtimestepStartUp =
                                        ((CurrentFractionalDay - EndingFractionalDay) / (CurrentFractionalDay - LastSystemTimeStepFractionalDay));

                                } else if (GeneratorDynamics(DynaCntrlNum).StartUpTimeDelay > 0.0) {
                                    // is remaining time enough?
                                    if ((CurrentFractionalDay - EndingFractionalDay) > GeneratorDynamics(DynaCntrlNum).StartUpTimeDelay) {
                                        newOpMode = OpModeNormal;
                                        // possible PLR on start up.
                                        PLRStartUp = true;
                                        PLRforSubtimestepStartUp =
                                            ((CurrentFractionalDay - EndingFractionalDay) / (CurrentFractionalDay - LastSystemTimeStepFractionalDay));
                                    } else {
                                        newOpMode = OpModeWarmUp;
                                        // generator just started so set start time
                                        GeneratorDynamics(DynaCntrlNum).FractionalDayofLastStartUp =
                                            double(state.dataGlobal->DayOfSim) +
                                            (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime) - TimeStepSys))) / DataGlobalConstants::HoursInDay();
                                    }
                                }
                            }
                        } else {

                            newOpMode = OpModeStandby;
                        }
                    } else { // not mandetory cool donw
                        // likely to go into warm up but if no warm up then back to normal
                        if (GeneratorDynamics(DynaCntrlNum).WarmUpByTimeDelay) {
                            if (GeneratorDynamics(DynaCntrlNum).StartUpTimeDelay == 0.0) {
                                newOpMode = OpModeNormal;

                            } else if (GeneratorDynamics(DynaCntrlNum).StartUpTimeDelay > 0.0) {
                                CurrentFractionalDay =
                                    double(state.dataGlobal->DayOfSim) + (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime)))) / DataGlobalConstants::HoursInDay();
                                EndingFractionalDay = GeneratorDynamics(DynaCntrlNum).FractionalDayofLastShutDown +
                                                      GeneratorDynamics(DynaCntrlNum).CoolDownDelay / DataGlobalConstants::HoursInDay();
                                if ((std::abs(CurrentFractionalDay - EndingFractionalDay) < 0.000001) ||
                                    (CurrentFractionalDay > EndingFractionalDay)) { // CurrentFractionalDay == EndingFractionalDay
                                    newOpMode = OpModeNormal;
                                    // possible PLR on start up.
                                    PLRStartUp = true;
                                    LastSystemTimeStepFractionalDay = CurrentFractionalDay - (TimeStepSys / DataGlobalConstants::HoursInDay());
                                    PLRforSubtimestepStartUp =
                                        ((CurrentFractionalDay - EndingFractionalDay) / (CurrentFractionalDay - LastSystemTimeStepFractionalDay));
                                } else {
                                    newOpMode = OpModeWarmUp;
                                    // set start up time
                                    // generator just started so set start time
                                    GeneratorDynamics(DynaCntrlNum).FractionalDayofLastStartUp =
                                        double(state.dataGlobal->DayOfSim) +
                                        (int(CurrentTime) + (SysTimeElapsed + (CurrentTime - int(CurrentTime) - TimeStepSys))) / DataGlobalConstants::HoursInDay();
                                }
                            }
                        }
                    }
                }
            }
        } // previous case

        if (PLRforSubtimestepStartUp < 0.0) PLRforSubtimestepStartUp = 0.0;
        if (PLRforSubtimestepStartUp > 1.0) PLRforSubtimestepStartUp = 1.0;

        if (PLRforSubtimestepShutDown < 0.0) PLRforSubtimestepShutDown = 0.0;
        if (PLRforSubtimestepShutDown > 1.0) PLRforSubtimestepShutDown = 1.0;

        if (newOpMode == OpModeWarmUp) {
            {
                auto const SELECT_CASE_var(GeneratorType);

                if (SELECT_CASE_var == GeneratorType::FuelCell) {
                    // constant power level during start up (modeling artifact)
                    //? hours or seconds here?
                    Pel = GeneratorDynamics(DynaCntrlNum).StartUpElectProd / GeneratorDynamics(DynaCntrlNum).StartUpTimeDelay;

                } else if (SELECT_CASE_var == GeneratorType::MicroCHP) {

                    Pel = PelInput * PLRforSubtimestepStartUp;
                }
            }
        }

        if (newOpMode == OpModeNormal) {
            // correct if switched to normal at sub timestep
            Pel *= PLRforSubtimestepStartUp;
            // unit may have constraints from transient limits or operating ranges.
            if (Pel > GeneratorDynamics(DynaCntrlNum).PelLastTimeStep) { // powering up
                MaxPel = GeneratorDynamics(DynaCntrlNum).PelLastTimeStep + GeneratorDynamics(DynaCntrlNum).UpTranLimit * TimeStepSys * DataGlobalConstants::SecInHour();
                if (MaxPel < Pel) {
                    Pel = MaxPel;
                }
            } else if (Pel < GeneratorDynamics(DynaCntrlNum).PelLastTimeStep) { // powering down
                MinPel = GeneratorDynamics(DynaCntrlNum).PelLastTimeStep - GeneratorDynamics(DynaCntrlNum).DownTranLimit * TimeStepSys * DataGlobalConstants::SecInHour();
                if (Pel < MinPel) {
                    Pel = MinPel;
                }
            }
        }

        if (newOpMode == OpModeCoolDown) {
            Pel = 0.0; // assumes no power generated during shut down
        }

        if (newOpMode == OpModeOff) {
            Pel = 0.0; // assumes no power generated during OFF mode
        }

        if (newOpMode == OpModeStandby) {
            Pel = 0.0; // assumes no power generated during standby mode
        }

        // Control step 3: adjust for max and min limits on Pel

        if (Pel < GeneratorDynamics(DynaCntrlNum).PelMin) {
            Pel = GeneratorDynamics(DynaCntrlNum).PelMin;
        }
        if (Pel > GeneratorDynamics(DynaCntrlNum).PelMax) {
            Pel = GeneratorDynamics(DynaCntrlNum).PelMax;
        }

        // now do record keeping for amount of time spent in various operating modes
        {
            auto const SELECT_CASE_var(GeneratorType);
            if (SELECT_CASE_var == GeneratorType::MicroCHP) {
                // first clear out values
                MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.OffModeTime = 0.0;
                MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.StandyByModeTime = 0.0;
                MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.WarmUpModeTime = 0.0;
                MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.NormalModeTime = 0.0;
                MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.CoolDownModeTime = 0.0;
                {
                    auto const SELECT_CASE_var1(newOpMode);

                    if (SELECT_CASE_var1 == OpModeOff) {
                        if (PLRforSubtimestepShutDown == 0.0) {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.OffModeTime = TimeStepSys * DataGlobalConstants::SecInHour();
                        } else if ((PLRforSubtimestepShutDown > 0.0) && (PLRforSubtimestepShutDown < 1.0)) {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.CoolDownModeTime = TimeStepSys * DataGlobalConstants::SecInHour() * (PLRforSubtimestepShutDown);
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.OffModeTime = TimeStepSys * DataGlobalConstants::SecInHour() * (1.0 - PLRforSubtimestepShutDown);
                        } else {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.OffModeTime = TimeStepSys * DataGlobalConstants::SecInHour();
                        }
                    } else if (SELECT_CASE_var1 == OpModeStandby) {
                        if (PLRforSubtimestepShutDown == 0.0) {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.StandyByModeTime = TimeStepSys * DataGlobalConstants::SecInHour();
                        } else if ((PLRforSubtimestepShutDown > 0.0) && (PLRforSubtimestepShutDown < 1.0)) {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.CoolDownModeTime = TimeStepSys * DataGlobalConstants::SecInHour() * (PLRforSubtimestepShutDown);
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.StandyByModeTime = TimeStepSys * DataGlobalConstants::SecInHour() * (1.0 - PLRforSubtimestepShutDown);
                        } else {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.StandyByModeTime = TimeStepSys * DataGlobalConstants::SecInHour();
                        }
                    } else if (SELECT_CASE_var1 == OpModeWarmUp) {
                        if (PLRforSubtimestepShutDown == 0.0) {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.WarmUpModeTime = TimeStepSys * DataGlobalConstants::SecInHour();
                        } else if ((PLRforSubtimestepShutDown > 0.0) && (PLRforSubtimestepShutDown < 1.0)) {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.CoolDownModeTime = TimeStepSys * DataGlobalConstants::SecInHour() * (PLRforSubtimestepShutDown);
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.WarmUpModeTime = TimeStepSys * DataGlobalConstants::SecInHour() * (1.0 - PLRforSubtimestepShutDown);
                        } else {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.WarmUpModeTime = TimeStepSys * DataGlobalConstants::SecInHour();
                        }

                    } else if (SELECT_CASE_var1 == OpModeNormal) {
                        if (PLRforSubtimestepStartUp == 0.0) {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.WarmUpModeTime = TimeStepSys * DataGlobalConstants::SecInHour();

                        } else if ((PLRforSubtimestepStartUp > 0.0) && (PLRforSubtimestepStartUp < 1.0)) {
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.WarmUpModeTime = TimeStepSys * DataGlobalConstants::SecInHour() * (1.0 - PLRforSubtimestepStartUp);
                            MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.NormalModeTime = TimeStepSys * DataGlobalConstants::SecInHour() * (PLRforSubtimestepStartUp);

                        } else {
                            if (PLRforSubtimestepShutDown == 0.0) {
                                MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.NormalModeTime = TimeStepSys * DataGlobalConstants::SecInHour();
                            } else if ((PLRforSubtimestepShutDown > 0.0) && (PLRforSubtimestepShutDown < 1.0)) {
                                MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.CoolDownModeTime = TimeStepSys * DataGlobalConstants::SecInHour() * (PLRforSubtimestepShutDown);
                                MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.NormalModeTime = TimeStepSys * DataGlobalConstants::SecInHour() * (1.0 - PLRforSubtimestepShutDown);
                            } else {
                                MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.NormalModeTime = TimeStepSys * DataGlobalConstants::SecInHour();
                            }
                        }

                    } else if (SELECT_CASE_var1 == OpModeCoolDown) {

                        MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.CoolDownModeTime = TimeStepSys * DataGlobalConstants::SecInHour();
                    }
                }

            } else if (SELECT_CASE_var == GeneratorType::FuelCell) {
                // not yet using this control manager
            } else {
            }
        }

        ElecLoadProvided = Pel;

        GeneratorDynamics(DynaCntrlNum).CurrentOpMode = newOpMode;
        OperatingMode = newOpMode;
    }

    void ManageGeneratorFuelFlow(GeneratorType const GeneratorType,                     // type of Generator
                                 std::string const &EP_UNUSED(GeneratorName), // user specified name of Generator
                                 int const GeneratorNum,                      // Generator number
                                 bool const EP_UNUSED(RunFlag),               // TRUE when Generator operating
                                 Real64 const FuelFlowRequest,                // Generator demand mdot kg/ s
                                 Real64 &FuelFlowProvided,                    // allowed after constraints kg/s
                                 bool &ConstrainedIncreasingMdot,             // true if request was altered because of fuel rate of change up
                                 bool &ConstrainedDecreasingMdot              // true if request was altered because of fuel rate of change down
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   July 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // check if change in fuel flow rate is okay

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na
        // Using/Aliasing
        using namespace DataGlobalConstants;
        using DataHVACGlobals::TimeStepSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MdotFuel;
        Real64 MaxMdot;
        Real64 MinMdot;
        int DynaCntrlNum(0);

        ConstrainedIncreasingMdot = false;
        ConstrainedDecreasingMdot = false;
        MdotFuel = FuelFlowRequest;

        // get index from GeneratorNum
        {
            auto const SELECT_CASE_var(GeneratorType);
            if (SELECT_CASE_var == GeneratorType::MicroCHP) {
                DynaCntrlNum = MicroCHPElectricGenerator::MicroCHP(GeneratorNum).DynamicsControlID;
            }
        }

        if (FuelFlowRequest > GeneratorDynamics(DynaCntrlNum).FuelMdotLastTimestep) { // fuel flow is up
            MaxMdot =
                GeneratorDynamics(DynaCntrlNum).FuelMdotLastTimestep + GeneratorDynamics(DynaCntrlNum).UpTranLimitFuel * TimeStepSys * DataGlobalConstants::SecInHour();
            if (MaxMdot < FuelFlowRequest) {
                MdotFuel = MaxMdot;
                ConstrainedIncreasingMdot = true;
            }

        } else if (FuelFlowRequest < GeneratorDynamics(DynaCntrlNum).FuelMdotLastTimestep) { // fuel flow is down
            MinMdot =
                GeneratorDynamics(DynaCntrlNum).FuelMdotLastTimestep - GeneratorDynamics(DynaCntrlNum).DownTranLimitFuel * TimeStepSys * DataGlobalConstants::SecInHour();
            if (FuelFlowRequest < MinMdot) {
                MdotFuel = MinMdot;
                ConstrainedDecreasingMdot = true;
            }
        } else {
            // do nothing
        }

        FuelFlowProvided = MdotFuel;
    }

    Real64 FuncDetermineCWMdotForInternalFlowControl(EnergyPlusData &state,
                                                     int const GeneratorNum, // ID of generator
                                                     Real64 const Pnetss,    // power net steady state
                                                     Real64 const TcwIn      // temperature of cooling water at inlet
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  B. Griffith, Sept 2010, plant upgrade

        // PURPOSE OF THIS FUNCTION:
        // common place to figure flow rates with internal flow control

        // METHODOLOGY EMPLOYED:
        // apply contraints imposed by plant according to flow lock, first HVAC iteration etc.

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;
        using DataLoopNode::Node;
        using PlantUtilities::SetComponentFlowRate;

        // Return value
        Real64 FuncDetermineCWMdotForInternalFlowControl;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 MdotCW;
        int InletNode;
        int OutletNode;

        InletNode = MicroCHPElectricGenerator::MicroCHP(GeneratorNum).PlantInletNodeID;
        OutletNode = MicroCHPElectricGenerator::MicroCHP(GeneratorNum).PlantOutletNodeID;

        // first evaluate curve
        MdotCW = CurveValue(state, MicroCHPElectricGenerator::MicroCHP(GeneratorNum).A42Model.WaterFlowCurveID, Pnetss, TcwIn);

        // now apply constraints
        MdotCW = max(0.0, MdotCW);

        // make sure plant can provide, utility call may change flow
        if (MicroCHPElectricGenerator::MicroCHP(GeneratorNum).CWLoopNum > 0) { // protect early calls
            SetComponentFlowRate(MdotCW,
                                 InletNode,
                                 OutletNode,
                                 MicroCHPElectricGenerator::MicroCHP(GeneratorNum).CWLoopNum,
                                 MicroCHPElectricGenerator::MicroCHP(GeneratorNum).CWLoopSideNum,
                                 MicroCHPElectricGenerator::MicroCHP(GeneratorNum).CWBranchNum,
                                 MicroCHPElectricGenerator::MicroCHP(GeneratorNum).CWCompNum);
        }

        FuncDetermineCWMdotForInternalFlowControl = MdotCW;
        return FuncDetermineCWMdotForInternalFlowControl;
    }

} // namespace GeneratorDynamicsManager

} // namespace EnergyPlus
