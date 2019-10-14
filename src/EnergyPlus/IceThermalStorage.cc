// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/IceThermalStorage.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace IceThermalStorage {

    // MODULE INFORMATION:
    //       AUTHOR         Pyeongchan Ihm
    //       DATE WRITTEN   April 2002
    //       MODIFIED       Modified Refined model, added Simple model, by Guo Zhou, Oct 2002
    //                      Remove chiller, make just a storage tank, Michael J. Witte, Sep 2005
    //                      Added detailed ice storage model, Rick Strand, Feb 2006
    //                      B. Griffith, Sept 2010, plant upgrades, fluid properties
    //                      Enhancements to detailed ice storage model, Rick Strand, Aug 2012
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates the performance of Ice Thermal Storage

    // METHODOLOGY EMPLOYED:
    // Once the PlantLoopManager determines that the Ice Thermal Storage
    // is available to meet a loop cooling demand, it calls SimIceStorage
    // which in turn calls the appropriate Ice Thermal Storage model.

    // REFERENCES: Dion J. King, ASHRAE Transactions v104, pt1, 1998.

    std::string const cIceStorageSimple("ThermalStorage:Ice:Simple");
    std::string const cIceStorageDetailed("ThermalStorage:Ice:Detailed");

    // ITS parameter
    Real64 const modFreezTemp(0.0);       // Water freezing Temperature, 0[C]
    Real64 const modFreezTempIP(32.0);    // Water freezing Temperature, 32[F]
    Real64 const modTimeInterval(3600.0); // Time Interval (1 hr) [s]

    // Conversion parameter
    Real64 const modEpsLimitForX(0.0);         // 0.02  ! See Dion's code as eps1
    Real64 const modEpsLimitForDisCharge(0.0); // 0.20  ! See Dion's code as eps2
    Real64 const modEpsLimitForCharge(0.0);    // 0.20  ! See Dion's code as eps3

    // Parameter used by the Detailed Ice Storage Model
    Real64 const modDeltaTofMin(0.5); // Minimum allowed outlet side temperature difference [C]
    // This is (Tout - Tfreezing)
    Real64 const modDeltaTifMin(1.0); // Minimum allowed inlet side temperature difference [C]
    // This is (Tin - Tfreezing)

    // ITS numbers and FoundOrNot
    int modNumIceStorages(0);
    int modNumDetIceStorages(0);
    int modTotalIceStorages(0);

    // Object Data
    Array1D<IceStorageSpecs> IceStorage;        // dimension to number of machines
    Array1D<ReportVars> IceStorageReport;       // dimension to number of machines
    Array1D<DetailedIceStorageData> DetIceStor; // Derived type for detailed ice storage model
    Array1D<IceStorageMapping> IceStorageTypeMap;

    //*************************************************************************

    // Functions
    void clear_state()
    {
        modNumIceStorages = 0;
        modNumDetIceStorages = 0;
        modTotalIceStorages = 0;
        IceStorage.deallocate();
        IceStorageReport.deallocate();
        DetIceStor.deallocate();
        IceStorageTypeMap.deallocate();
    }

    void SimIceStorage(std::string const &IceStorageType,
                       std::string const &IceStorageName,
                       int &CompIndex,
                       bool const RunFlag,
                       bool const FirstIteration,
                       bool const InitLoopEquip,
                       Real64 &MyLoad)
    {

        Real64 DemandMdot;
        Real64 TempIn;
        Real64 TempSetPt(0.0);
        Real64 MyLoad2;
        Real64 MaxCap;
        Real64 MinCap;
        Real64 OptCap;
        Real64 Cp; // local plant fluid specific heat

        static std::string const RoutineName("SimIceStorage");
        static bool firstTime(true);
        int IceStorageNum;

        //  Set initialization flags
        //  Allow ice to build up during warmup?
        //  IF( (BeginEnvrnFlag) .OR. (WarmupFlag) ) THEN

        if (firstTime) {
            GetIceStorageInput();
            firstTime = false;
        }

        // Find the correct Equipment
        if (CompIndex == 0) {
            IceStorageNum = UtilityRoutines::FindItemInList(IceStorageName, IceStorageTypeMap, modTotalIceStorages);
            if (IceStorageNum == 0) {
                ShowFatalError("SimIceStorage: Unit not found=" + IceStorageName);
            }
            CompIndex = IceStorageNum;
        } else {
            IceStorageNum = CompIndex;
            if (IceStorageNum > modTotalIceStorages || IceStorageNum < 1) {
                ShowFatalError("SimIceStorage:  Invalid CompIndex passed=" + General::TrimSigDigits(IceStorageNum) +
                               ", Number of Units=" + General::TrimSigDigits(modTotalIceStorages) + ", Entered Unit name=" + IceStorageName);
            }

            for (auto thisITS : IceStorage) {
                if (IceStorageName != thisITS.Name) {
                    ShowFatalError("SimIceStorage: Invalid CompIndex passed=" + General::TrimSigDigits(IceStorageNum) + ", Unit name=" + IceStorageName +
                                   ", stored Unit Name for that index=" + thisITS.Name);
                }
                thisITS.CheckEquipName = false;
            }

            for (auto thisITS : DetIceStor) {
                if (IceStorageName != thisITS.Name) {
                    ShowFatalError("SimIceStorage: Invalid CompIndex passed=" + General::TrimSigDigits(IceStorageNum) + ", Unit name=" + IceStorageName +
                                   ", stored Unit Name for that index=" + thisITS.Name);
                }
                thisITS.CheckEquipName = false;
            }
        }

        int iceNum = IceStorageTypeMap(IceStorageNum).LocalEqNum;

        {
            auto const SELECT_CASE_var(IceStorageTypeMap(IceStorageNum).StorageType_Num);

            if (SELECT_CASE_var == IceStorageType::Simple) {
                if (DataGlobals::BeginEnvrnFlag && IceStorage(iceNum).MyEnvrnFlag) {
                    IceStorage(iceNum).ResetXForITSFlag = true;
                    IceStorage(iceNum).MyEnvrnFlag = false;
                }

                if (!DataGlobals::BeginEnvrnFlag) {
                    IceStorage(iceNum).MyEnvrnFlag = true;
                }
            }  else if (SELECT_CASE_var == IceStorageType::Detailed) {
                if (DataGlobals::BeginEnvrnFlag && DetIceStor(iceNum).MyEnvrnFlag) {
                    DetIceStor(iceNum).ResetXForITSFlag = true;
                    DetIceStor(iceNum).MyEnvrnFlag = false;
                }

                if (!DataGlobals::BeginEnvrnFlag) {
                    DetIceStor(iceNum).MyEnvrnFlag = true;
                }
            }
        }

        {
            auto const SELECT_CASE_var(IceStorageTypeMap(IceStorageNum).StorageType_Num);

            if (SELECT_CASE_var == IceStorageType::Simple) {

                //------------------------------------------------------------------------
                // READING INPUT when first calling SimIceStorage
                //------------------------------------------------------------------------
                iceNum = IceStorageTypeMap(IceStorageNum).LocalEqNum;

                InitSimpleIceStorage(iceNum);

                if (InitLoopEquip) {
                    return;
                } // End Of InitLoopEquip

                //------------------------------------------------------------------------
                // FIRST PROCESS (MyLoad = 0.0 as IN)
                // At this moment as first calling of ITS, ITS provide ONLY MaxCap/OptCap/MinCap.
                //------------------------------------------------------------------------
                // First process is in subroutine CalcIceStorageCapacity(MaxCap,MinCap,OptCap) shown bellow.

                //------------------------------------------------------------------------
                // SECOND PROCESS (MyLoad is provided by E+ based on MaxCap/OptCap/MinCap)
                //------------------------------------------------------------------------
                // Below routines are starting when second calling.
                // After previous return, MyLoad is calculated based on MaxCap, OptCap, and MinCap.
                // Then PlandSupplySideManager provides MyLoad to simulate Ice Thermal Storage.
                // The process will be decided based on sign(+,-,0) of input U.

                // MJW 19 Sep 2005 - New approach - calculate MyLoad locally from inlet node temp
                //                   and outlet node setpoint until MyLoad that is passed in behaves well

                // DSU? can we now use MyLoad? lets not yet to try to avoid scope creep

                TempIn = DataLoopNode::Node(IceStorage(iceNum).PltInletNodeNum).Temp;
                {
                    auto const SELECT_CASE_var1(DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var1 == DataPlant::SingleSetPoint) {
                        TempSetPt = DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var1 == DataPlant::DualSetPointDeadBand) {
                        TempSetPt = DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).TempSetPointHi;
                    } else {
                        assert(false);
                    }
                }
                DemandMdot = IceStorage(iceNum).DesignMassFlowRate;

                Cp = FluidProperties::GetSpecificHeatGlycol(
                        DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).FluidName, TempIn, DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).FluidIndex, RoutineName);

                MyLoad2 = (DemandMdot * Cp * (TempIn - TempSetPt));
                MyLoad = MyLoad2;

                //     Set fraction of ice remaining in storage
                IceStorage(iceNum).XCurIceFrac = IceStorage(iceNum).IceFracRemain;

                //***** Dormant Process for ITS *****************************************
                //************************************************************************
                //        IF( U .EQ. 0.0 ) THEN
                if ((MyLoad2 == 0.0) || (DemandMdot == 0.0)) {
                    CalcIceStorageDormant(IceStorageType::Simple, iceNum);

                    //***** Charging Process for ITS *****************************************
                    //************************************************************************
                    //        ELSE IF( U .GT. 0.0 ) THEN
                } else if (MyLoad2 < 0.0) {

                    //             Call CalcIceStorageCapacity from here - MJW - 19 Sep 2005
                    CalcIceStorageCapacity(IceStorageType::Simple, MaxCap, MinCap, OptCap, iceNum);

                    CalcIceStorageCharge(IceStorageType::Simple, iceNum);

                    //***** Discharging Process for ITS *****************************************
                    //************************************************************************
                    //        ELSE IF( U .LT. 0.0 ) THEN
                } else if (MyLoad2 > 0.0) {
                    //             Call CalcIceStorageCapacity from here - MJW - 19 Sep 2005
                    CalcIceStorageCapacity(IceStorageType::Simple, MaxCap, MinCap, OptCap, iceNum);

                    CalcIceStorageDischarge(IceStorageType::Simple, iceNum, MyLoad, RunFlag, FirstIteration, MaxCap);
                } // Based on input of U value, deciding Dormant/Charge/Discharge process

                // Update Node properties: mdot and Temperature
                UpdateNode(MyLoad2, RunFlag, iceNum);

                // Update report variables.
                RecordOutput(iceNum, MyLoad2, RunFlag);
                //--------------------------------------------------------------------------
                //        Ali's TES modle   Itegrated by ZG  Oct. 2002
                //---------------------------------------------------------------------------

            } else if (SELECT_CASE_var == IceStorageType::Detailed) {

                // Read input when first calling SimIceStorage
                if (InitLoopEquip) {
                    return;
                } // End Of InitLoopEquip

                InitDetailedIceStorage(iceNum); // Initialize detailed ice storage

                SimDetailedIceStorage(iceNum); // Simulate detailed ice storage

                UpdateDetailedIceStorage(iceNum); // Update detailed ice storage

                ReportDetailedIceStorage(iceNum); // Report detailed ice storage

            } else {
                ShowFatalError("Specified IceStorage not found in SimIceStorage" + IceStorageType);
            }
        }
    }

    void SimDetailedIceStorage(int const iceNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is the main simulation subroutine for the detailed
        // ice storage model.

        // METHODOLOGY EMPLOYED:
        // Based on whether the unit is dormant, in charging mode, or in discharging
        // mode, the code either passes the flow through the bypass, through the tank,
        // or both.  This depends on the temperature relative to the setpoint temperature
        // and other features of the model.  The model itself is a LMTD model that uses
        // performance curve fits that are quadratic in fraction charged/discharged and
        // linear in LMTD for the calculation of Q.  The equations are actually non-
        // dimensionalized.

        // REFERENCES:
        // Ice Storage Component Model Proposal (Revised).doc by Rick Strand (Dec 2005/Jan 2006)

        int const MaxIterNum(100);                      // Maximum number of internal iterations for ice storage solution
        Real64 const SmallestLoad(0.1);                 // Smallest load to actually run the ice storage unit [Watts]
        Real64 const TankDischargeToler(0.001);         // Below this fraction, there is nothing left to discharge
        Real64 const TankChargeToler(0.999);            // Above this fraction, we don't have anything left to charge
        Real64 const TemperatureToler(0.1);             // Temperature difference between iterations that indicates convergence [C]
        Real64 const SIEquiv100GPMinMassFlowRate(6.31); // Used to non-dimensionalize flow rate for use in CubicLinear charging equation
                                                        // Flow rate divided by nominal 100GPM used to non-dimensionalize volume flow rate
                                                        // Assumes approximate density of 1000 kg/m3 to get an estimate for mass flow rate
        std::string const RoutineName("SimDetailedIceStorage");

        Real64 ActualLoad;     // Actual load on the ice storage unit [W]
        Real64 AvgFracCharged; // Average fraction charged for the current time step
        Real64 ChargeFrac;     // Fraction of tank to be charged in the current time step
        int IterNum;           // Iteration number
        Real64 LMTDstar;       // Non-dimensional log mean temperature difference of ice storage unit [non-dimensional]
        Real64 LocalLoad;      // Estimated load on the ice storage unit [W]
        int NodeNumIn;         // Plant loop inlet node number for component
        int NodeNumOut;        // Plant loop outlet node number for component
        Real64 Qstar;          // Current load on the ice storage unit [non-dimensional]
        Real64 TempIn;         // Inlet temperature to component (from plant loop) [C]
        Real64 TempSetPt(0.0); // Setpoint temperature defined by loop controls [C]
        Real64 ToutNew;        // Updated outlet temperature from the tank [C]
        Real64 ToutOld;        // Tank outlet temperature from the last iteration [C]
        Real64 Cp;             // local plant fluid specific heat
        Real64 mdot;           // local mass flow rate for plant connection
        Real64 MassFlowstar;   // non-dimensional mass flow rate for charge curve use [non-dimensional]

        NodeNumIn = DetIceStor(iceNum).PlantInNodeNum;
        NodeNumOut = DetIceStor(iceNum).PlantOutNodeNum;
        TempIn = DataLoopNode::Node(NodeNumIn).Temp;
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(DetIceStor(iceNum).PlantLoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                TempSetPt = DataLoopNode::Node(NodeNumOut).TempSetPoint;
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                TempSetPt = DataLoopNode::Node(NodeNumOut).TempSetPointHi;
            } else {
                assert(false);
            }
        }

        IterNum = 0;

        // Set derived type variables
        DetIceStor(iceNum).InletTemp = TempIn;
        DetIceStor(iceNum).MassFlowRate = DataLoopNode::Node(NodeNumIn).MassFlowRate;

        // if two-way common pipe and no mass flow and tank is not full, then use design flow rate
        if ((DataPlant::PlantLoop(DetIceStor(iceNum).PlantLoopNum).CommonPipeType == DataPlant::CommonPipe_TwoWay) &&
            (std::abs(DetIceStor(iceNum).MassFlowRate) < DataBranchAirLoopPlant::MassFlowTolerance) && (DetIceStor(iceNum).IceFracRemaining < TankChargeToler)) {
            DetIceStor(iceNum).MassFlowRate = DetIceStor(iceNum).DesignMassFlowRate;
        }

        // Calculate the current load on the ice storage unit
        Cp = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(DetIceStor(iceNum).PlantLoopNum).FluidName, TempIn, DataPlant::PlantLoop(DetIceStor(iceNum).PlantLoopNum).FluidIndex, RoutineName);

        LocalLoad = DetIceStor(iceNum).MassFlowRate * Cp * (TempIn - TempSetPt);

        // Determine what the status is regarding the ice storage unit and the loop level flow
        if ((std::abs(LocalLoad) <= SmallestLoad) || (ScheduleManager::GetCurrentScheduleValue(DetIceStor(iceNum).ScheduleIndex) <= 0)) {
            // No real load on the ice storage device or ice storage OFF--bypass all of the flow and leave the tank alone
            DetIceStor(iceNum).CompLoad = 0.0;
            DetIceStor(iceNum).OutletTemp = TempIn;
            DetIceStor(iceNum).TankOutletTemp = TempIn;
            mdot = 0.0;
            PlantUtilities::SetComponentFlowRate(mdot,
                                 DetIceStor(iceNum).PlantInNodeNum,
                                 DetIceStor(iceNum).PlantOutNodeNum,
                                 DetIceStor(iceNum).PlantLoopNum,
                                 DetIceStor(iceNum).PlantLoopSideNum,
                                 DetIceStor(iceNum).PlantBranchNum,
                                 DetIceStor(iceNum).PlantCompNum);

            DetIceStor(iceNum).BypassMassFlowRate = mdot;
            DetIceStor(iceNum).TankMassFlowRate = 0.0;
            DetIceStor(iceNum).MassFlowRate = mdot;

        } else if (LocalLoad < 0.0) {
            // The load is less than zero so we should be charging
            // Before we do anything, we should check to make sure that we will actually be charging the unit

            if ((TempIn > (DetIceStor(iceNum).FreezingTemp - modDeltaTifMin)) || (DetIceStor(iceNum).IceFracRemaining >= TankChargeToler)) {
                // If the inlet temperature is not below the freezing temperature of the
                // device, then we cannot actually do any charging.  Bypass all of the flow.
                // Also, if the tank is already sufficiently charged, we don't need to
                // do any further charging.  So, bypass all of the flow.
                DetIceStor(iceNum).CompLoad = 0.0;
                DetIceStor(iceNum).OutletTemp = TempIn;
                DetIceStor(iceNum).TankOutletTemp = TempIn;
                mdot = 0.0;
                PlantUtilities::SetComponentFlowRate(mdot,
                                     DetIceStor(iceNum).PlantInNodeNum,
                                     DetIceStor(iceNum).PlantOutNodeNum,
                                     DetIceStor(iceNum).PlantLoopNum,
                                     DetIceStor(iceNum).PlantLoopSideNum,
                                     DetIceStor(iceNum).PlantBranchNum,
                                     DetIceStor(iceNum).PlantCompNum);

                DetIceStor(iceNum).BypassMassFlowRate = mdot;
                DetIceStor(iceNum).TankMassFlowRate = 0.0;
                DetIceStor(iceNum).MassFlowRate = mdot;

            } else {
                // make flow request so tank will get flow
                mdot = DetIceStor(iceNum).DesignMassFlowRate;
                PlantUtilities::SetComponentFlowRate(mdot,
                                     DetIceStor(iceNum).PlantInNodeNum,
                                     DetIceStor(iceNum).PlantOutNodeNum,
                                     DetIceStor(iceNum).PlantLoopNum,
                                     DetIceStor(iceNum).PlantLoopSideNum,
                                     DetIceStor(iceNum).PlantBranchNum,
                                     DetIceStor(iceNum).PlantCompNum);

                // We are in charging mode, the temperatures are low enough to charge
                // the tank, and we have some charging left to do.
                // Make first guess at Qstar based on the current ice fraction remaining
                // and LMTDstar that is based on the freezing or TempSetPt temperature.
                if (TempSetPt > (DetIceStor(iceNum).FreezingTemp - modDeltaTofMin)) {
                    // Outlet temperature cannot be above the freezing temperature so set
                    // the outlet temperature to the freezing temperature and calculate
                    // LMTDstar based on that assumption.
                    TempSetPt = DetIceStor(iceNum).FreezingTemp - modDeltaTofMin;
                }

                ToutOld = TempSetPt;
                LMTDstar = CalcDetIceStorLMTDstar(TempIn, ToutOld, DetIceStor(iceNum).FreezingTemp);
                MassFlowstar = DetIceStor(iceNum).MassFlowRate / SIEquiv100GPMinMassFlowRate;

                // Find initial guess at average fraction charged during time step
                ChargeFrac = LocalLoad * DataHVACGlobals::TimeStepSys / DetIceStor(iceNum).NomCapacity;
                if ((DetIceStor(iceNum).IceFracRemaining + ChargeFrac) > 1.0) {
                    ChargeFrac = 1.0 - DetIceStor(iceNum).IceFracRemaining;
                }
                if (DetIceStor(iceNum).ThawProcessIndex == DetIce::InsideMelt) {
                    AvgFracCharged = DetIceStor(iceNum).IceFracOnCoil + (ChargeFrac / 2.0);
                } else { // (DetIceStor(IceNum)%ThawProcessIndex == DetIce::OutsideMelt)
                    AvgFracCharged = DetIceStor(iceNum).IceFracRemaining + (ChargeFrac / 2.0);
                }

                Qstar = std::abs(CalcQstar(DetIceStor(iceNum).ChargeCurveNum, DetIceStor(iceNum).ChargeCurveTypeNum, AvgFracCharged, LMTDstar, MassFlowstar));

                ActualLoad = Qstar * DetIceStor(iceNum).NomCapacity / DetIceStor(iceNum).CurveFitTimeStep;

                ToutNew = TempIn + (ActualLoad / (DetIceStor(iceNum).MassFlowRate * Cp));
                // Again, the outlet temperature cannot be above the freezing temperature (factoring in the tolerance)
                if (ToutNew > (DetIceStor(iceNum).FreezingTemp - modDeltaTofMin)) ToutNew = DetIceStor(iceNum).FreezingTemp - modDeltaTofMin;

                if (ActualLoad > std::abs(LocalLoad)) {
                    // We have more than enough capacity to meet the load so no need to iterate to find a solution
                    DetIceStor(iceNum).OutletTemp = TempSetPt;
                    DetIceStor(iceNum).TankOutletTemp = ToutNew;
                    DetIceStor(iceNum).CompLoad = DetIceStor(iceNum).MassFlowRate * Cp * std::abs(TempIn - TempSetPt);
                    DetIceStor(iceNum).TankMassFlowRate = DetIceStor(iceNum).CompLoad / Cp / std::abs(TempIn - ToutNew);
                    DetIceStor(iceNum).BypassMassFlowRate = DetIceStor(iceNum).MassFlowRate - DetIceStor(iceNum).TankMassFlowRate;

                } else {

                    while (IterNum < MaxIterNum) {
                        if (std::abs(ToutOld - ToutNew) > TemperatureToler) {
                            // Not converged yet so recalculated what is needed and keep iterating
                            // Calculate new values for LMTDstar and Qstar based on updated outlet temperature
                            ToutOld = ToutNew;
                            LMTDstar = CalcDetIceStorLMTDstar(TempIn, ToutOld, DetIceStor(iceNum).FreezingTemp);
                            MassFlowstar = DetIceStor(iceNum).MassFlowRate / SIEquiv100GPMinMassFlowRate;
                            Qstar = std::abs(CalcQstar(DetIceStor(iceNum).ChargeCurveNum, DetIceStor(iceNum).ChargeCurveTypeNum, AvgFracCharged, LMTDstar, MassFlowstar));

                            // Now make sure that we don't go above 100% charged and calculate the new average fraction
                            ChargeFrac = Qstar * (DataHVACGlobals::TimeStepSys / DetIceStor(iceNum).CurveFitTimeStep);
                            if ((DetIceStor(iceNum).IceFracRemaining + ChargeFrac) > 1.0) {
                                ChargeFrac = 1.0 - DetIceStor(iceNum).IceFracRemaining;
                                Qstar = ChargeFrac;
                            }
                            if (DetIceStor(iceNum).ThawProcessIndex == DetIce::InsideMelt) {
                                AvgFracCharged = DetIceStor(iceNum).IceFracOnCoil + (ChargeFrac / 2.0);
                            } else { // (DetIceStor(IceNum)%ThawProcessIndex == DetIce::OutsideMelt)
                                AvgFracCharged = DetIceStor(iceNum).IceFracRemaining + (ChargeFrac / 2.0);
                            }

                            // Finally, update the actual load and calculate the new outlet temperature; increment iteration counter
                            ActualLoad = Qstar * DetIceStor(iceNum).NomCapacity / DetIceStor(iceNum).CurveFitTimeStep;
                            ToutNew = TempIn + (ActualLoad / (DetIceStor(iceNum).MassFlowRate * Cp));
                            // Again, the outlet temperature cannot be above the freezing temperature (factoring in the tolerance)
                            if (ToutNew < (DetIceStor(iceNum).FreezingTemp - modDeltaTofMin)) ToutNew = DetIceStor(iceNum).FreezingTemp - modDeltaTofMin;
                            ++IterNum;

                        } else {
                            // Converged to acceptable tolerance so set output variables and exit DO WHILE loop
                            break;
                        }

                    } // ...loop iterating for the ice storage outlet temperature

                    // Keep track of times that the iterations got excessive and report if necessary
                    if (IterNum >= MaxIterNum) {
                        ++DetIceStor(iceNum).ChargeIterErrors;
                        if (DetIceStor(iceNum).ChargeIterErrors <= 25) {
                            ShowWarningError("Detailed Ice Storage model exceeded its internal charging maximum iteration limit");
                            ShowContinueError("Detailed Ice Storage System Name = " + DetIceStor(iceNum).Name);
                            ShowContinueErrorTimeStamp("");
                        } else {
                            ShowRecurringWarningErrorAtEnd("Detailed Ice Storage system [" + DetIceStor(iceNum).Name +
                                                           "]  charging maximum iteration limit exceeded occurrence continues.",
                                                           DetIceStor(iceNum).ChargeErrorCount);
                        }
                    }

                    // Set the values for the key outlet parameters
                    // Note that in REAL(r64)ity the tank will probably bypass some flow when it
                    // gets close to full charge.  This is a simplification that assumes
                    // all flow through the tank during charging and a lower delta T near
                    // the full charge level.  From an energy perspective, this is a reasonable
                    // approximation.
                    DetIceStor(iceNum).OutletTemp = ToutNew;
                    DetIceStor(iceNum).TankOutletTemp = ToutNew;
                    DetIceStor(iceNum).BypassMassFlowRate = 0.0;
                    DetIceStor(iceNum).TankMassFlowRate = DetIceStor(iceNum).MassFlowRate;
                    DetIceStor(iceNum).CompLoad = DetIceStor(iceNum).MassFlowRate * Cp * std::abs(TempIn - ToutNew);
                }
            }

        } else if (LocalLoad > 0.0) {
            // The load is greater than zero so we should be discharging
            // Before we do anything, we should check to make sure that we will actually be discharging the unit

            if ((DetIceStor(iceNum).InletTemp < (DetIceStor(iceNum).FreezingTemp + modDeltaTifMin)) ||
                (DetIceStor(iceNum).IceFracRemaining <= TankDischargeToler)) {
                // If the inlet temperature is below the freezing temperature of the
                // device, then we cannot actually do any discharging.  Bypass all of the flow.
                // Also, if the tank is already discharged, we can't to do any further
                // discharging.  So, bypass all of the flow.
                DetIceStor(iceNum).CompLoad = 0.0;
                DetIceStor(iceNum).OutletTemp = DetIceStor(iceNum).InletTemp;
                DetIceStor(iceNum).TankOutletTemp = DetIceStor(iceNum).InletTemp;
                mdot = 0.0;
                PlantUtilities::SetComponentFlowRate(mdot,
                                     DetIceStor(iceNum).PlantInNodeNum,
                                     DetIceStor(iceNum).PlantOutNodeNum,
                                     DetIceStor(iceNum).PlantLoopNum,
                                     DetIceStor(iceNum).PlantLoopSideNum,
                                     DetIceStor(iceNum).PlantBranchNum,
                                     DetIceStor(iceNum).PlantCompNum);

                DetIceStor(iceNum).BypassMassFlowRate = mdot;
                DetIceStor(iceNum).TankMassFlowRate = 0.0;
                DetIceStor(iceNum).MassFlowRate = mdot;

            } else {

                // make flow request so tank will get flow
                mdot = DetIceStor(iceNum).DesignMassFlowRate;
                PlantUtilities::SetComponentFlowRate(mdot,
                                     DetIceStor(iceNum).PlantInNodeNum,
                                     DetIceStor(iceNum).PlantOutNodeNum,
                                     DetIceStor(iceNum).PlantLoopNum,
                                     DetIceStor(iceNum).PlantLoopSideNum,
                                     DetIceStor(iceNum).PlantBranchNum,
                                     DetIceStor(iceNum).PlantCompNum);

                // We are in discharging mode, the temperatures are high enough to discharge
                // the tank, and we have some discharging left to do.
                if (TempSetPt < (DetIceStor(iceNum).FreezingTemp + modDeltaTofMin)) {
                    // Outlet temperature cannot be below the freezing temperature so set
                    // the outlet temperature to the freezing temperature and calculate
                    // LMTDstar based on that assumption.
                    TempSetPt = DetIceStor(iceNum).FreezingTemp + modDeltaTofMin;
                }

                ToutOld = TempSetPt;
                LMTDstar = CalcDetIceStorLMTDstar(TempIn, ToutOld, DetIceStor(iceNum).FreezingTemp);
                MassFlowstar = DetIceStor(iceNum).MassFlowRate / SIEquiv100GPMinMassFlowRate;

                // Find initial guess at average fraction charged during time step
                ChargeFrac = LocalLoad * DataHVACGlobals::TimeStepSys / DetIceStor(iceNum).NomCapacity;
                if ((DetIceStor(iceNum).IceFracRemaining - ChargeFrac) < 0.0) ChargeFrac = DetIceStor(iceNum).IceFracRemaining;
                AvgFracCharged = DetIceStor(iceNum).IceFracRemaining - (ChargeFrac / 2.0);

                Qstar = std::abs(CalcQstar(DetIceStor(iceNum).DischargeCurveNum, DetIceStor(iceNum).DischargeCurveTypeNum, AvgFracCharged, LMTDstar, MassFlowstar));

                ActualLoad = Qstar * DetIceStor(iceNum).NomCapacity / DetIceStor(iceNum).CurveFitTimeStep;

                ToutNew = TempIn - (ActualLoad / (DetIceStor(iceNum).MassFlowRate * Cp));
                // Again, the outlet temperature cannot be below the freezing temperature (factoring in the tolerance)
                if (ToutNew < (DetIceStor(iceNum).FreezingTemp + modDeltaTofMin)) ToutNew = DetIceStor(iceNum).FreezingTemp + modDeltaTofMin;

                if (ActualLoad > LocalLoad) {
                    // We have more than enough storage to meet the load so no need to iterate to find a solution
                    DetIceStor(iceNum).OutletTemp = TempSetPt;
                    DetIceStor(iceNum).TankOutletTemp = ToutNew;
                    DetIceStor(iceNum).CompLoad = DetIceStor(iceNum).MassFlowRate * Cp * std::abs(TempIn - TempSetPt);
                    DetIceStor(iceNum).TankMassFlowRate = DetIceStor(iceNum).CompLoad / Cp / std::abs(TempIn - ToutNew);
                    DetIceStor(iceNum).BypassMassFlowRate = DetIceStor(iceNum).MassFlowRate - DetIceStor(iceNum).TankMassFlowRate;

                } else {

                    while (IterNum < MaxIterNum) {
                        if (std::abs(ToutOld - ToutNew) > TemperatureToler) {
                            // Not converged yet so recalculated what is needed and keep iterating
                            // Calculate new values for LMTDstar and Qstar based on updated outlet temperature
                            ToutOld = ToutNew;
                            LMTDstar = CalcDetIceStorLMTDstar(TempIn, ToutOld, DetIceStor(iceNum).FreezingTemp);

                            Qstar = std::abs(CalcQstar(DetIceStor(iceNum).DischargeCurveNum, DetIceStor(iceNum).DischargeCurveTypeNum, AvgFracCharged, LMTDstar, MassFlowstar));

                            // Now make sure that we don't go below 100% discharged and calculate the new average fraction
                            ChargeFrac = Qstar * (DataHVACGlobals::TimeStepSys / DetIceStor(iceNum).CurveFitTimeStep);
                            if ((DetIceStor(iceNum).IceFracRemaining - ChargeFrac) < 0.0) {
                                ChargeFrac = DetIceStor(iceNum).IceFracRemaining;
                                Qstar = ChargeFrac;
                            }
                            AvgFracCharged = DetIceStor(iceNum).IceFracRemaining - (ChargeFrac / 2.0);

                            // Finally, update the actual load and calculate the new outlet temperature; increment iteration counter
                            ActualLoad = Qstar * DetIceStor(iceNum).NomCapacity / DetIceStor(iceNum).CurveFitTimeStep;
                            ToutNew = TempIn - (ActualLoad / (DetIceStor(iceNum).MassFlowRate * Cp));
                            // Again, the outlet temperature cannot be below the freezing temperature (factoring in the tolerance)
                            if (ToutNew < (DetIceStor(iceNum).FreezingTemp + modDeltaTofMin)) ToutNew = DetIceStor(iceNum).FreezingTemp + modDeltaTofMin;
                            ++IterNum;

                        } else {
                            // Converged to acceptable tolerance so set output variables and exit DO WHILE loop
                            break;
                        }

                    } // ...loop iterating for the ice storage outlet temperature

                    // Keep track of times that the iterations got excessive
                    if (IterNum >= MaxIterNum && (!DataGlobals::WarmupFlag)) {
                        ++DetIceStor(iceNum).DischargeIterErrors;
                        if (DetIceStor(iceNum).DischargeIterErrors <= 25) {
                            ShowWarningError("Detailed Ice Storage model exceeded its internal discharging maximum iteration limit");
                            ShowContinueError("Detailed Ice Storage System Name = " + DetIceStor(iceNum).Name);
                            ShowContinueErrorTimeStamp("");
                        } else {
                            ShowRecurringWarningErrorAtEnd("Detailed Ice Storage system [" + DetIceStor(iceNum).Name +
                                                           "]  discharging maximum iteration limit exceeded occurrence continues.",
                                                           DetIceStor(iceNum).DischargeErrorCount);
                        }
                    }

                    // We are now done finding the outlet temperature of the tank.  We need
                    // to compare the outlet temperature to the setpoint temperature again
                    // to see where we are at and then we can set the values for the key
                    // outlet parameters.  If outlet temperature is greater than or equal
                    // to the setpoint temperature, then send all flow through the tank.
                    // Otherwise, we have more capacity than needed so let's bypass some
                    // flow and meet the setpoint temperautre.
                    if (ToutNew >= TempSetPt) {
                        DetIceStor(iceNum).OutletTemp = ToutNew;
                        DetIceStor(iceNum).TankOutletTemp = ToutNew;
                        DetIceStor(iceNum).BypassMassFlowRate = 0.0;
                        DetIceStor(iceNum).TankMassFlowRate = DetIceStor(iceNum).MassFlowRate;
                        DetIceStor(iceNum).CompLoad = DetIceStor(iceNum).MassFlowRate * Cp * std::abs(TempIn - ToutNew);
                    } else {
                        DetIceStor(iceNum).OutletTemp = TempSetPt;
                        DetIceStor(iceNum).TankOutletTemp = ToutNew;
                        DetIceStor(iceNum).CompLoad = DetIceStor(iceNum).MassFlowRate * Cp * std::abs(TempIn - TempSetPt);
                        DetIceStor(iceNum).TankMassFlowRate = DetIceStor(iceNum).CompLoad / (Cp * std::abs(TempIn - ToutNew));
                        DetIceStor(iceNum).BypassMassFlowRate = DetIceStor(iceNum).MassFlowRate - DetIceStor(iceNum).TankMassFlowRate;
                    }
                }
            }

        } else { // Shouldn't get here ever (print error if we do)

            ShowFatalError("Detailed Ice Storage systemic code error--contact EnergyPlus support");
        }
    }

    void GetIceStorageInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:
        //       DATE WRITTEN:

        // PURPOSE OF THIS SUBROUTINE:!This routine will get the input
        // required by the PrimaryPlantLoopManager.  As such
        // it will interact with the Input Scanner to retrieve
        // information from the input file, count the number of
        // heating and cooling loops and begin to fill the
        // arrays associated with the type PlantLoopProps.

        // METHODOLOGY EMPLOYED: to be determined...

        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        bool ErrorsFound;

        ErrorsFound = false; // Always need to reset this since there are multiple types of ice storage systems

        // LOAD ARRAYS WITH IceStorage DATA
        modNumIceStorages = inputProcessor->getNumObjectsFound(cIceStorageSimple); // by ZG
        modNumDetIceStorages = inputProcessor->getNumObjectsFound(cIceStorageDetailed);

        IceStorageTypeMap.allocate(modNumIceStorages + modNumDetIceStorages);

        // Allocate IceStorage based on NumOfIceStorage
        IceStorage.allocate(modNumIceStorages);
        IceStorageReport.allocate(modNumIceStorages);

        DataIPShortCuts::cCurrentModuleObject = cIceStorageSimple;
        for (int iceNum = 1; iceNum <= modNumIceStorages; ++iceNum) {

            inputProcessor->getObjectItem(
                    DataIPShortCuts::cCurrentModuleObject, iceNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat, _, _, _, DataIPShortCuts::cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

            ++modTotalIceStorages;
            IceStorageTypeMap(modTotalIceStorages).StorageType = DataIPShortCuts::cCurrentModuleObject;
            IceStorageTypeMap(modTotalIceStorages).StorageType_Num = IceStorageType::Simple;
            IceStorageTypeMap(modTotalIceStorages).Name = DataIPShortCuts::cAlphaArgs(1);
            IceStorageTypeMap(modTotalIceStorages).LocalEqNum = iceNum;
            IceStorage(iceNum).MapNum = modTotalIceStorages;

            // ITS name
            IceStorage(iceNum).Name = DataIPShortCuts::cAlphaArgs(1);

            // Get Ice Thermal Storage Type
            IceStorage(iceNum).ITSType = DataIPShortCuts::cAlphaArgs(2);
            if (UtilityRoutines::SameString(IceStorage(iceNum).ITSType, "IceOnCoilInternal")) {
                IceStorage(iceNum).ITSType_Num = ITSType::IceOnCoilInternal;
            } else if (UtilityRoutines::SameString(IceStorage(iceNum).ITSType, "IceOnCoilExternal")) {
                IceStorage(iceNum).ITSType_Num = ITSType::IceOnCoilExternal;
            } else {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));
                ErrorsFound = true;
            }

            // Get and Verify ITS nominal Capacity (user input is in GJ, internal value in in J)
            IceStorage(iceNum).ITSNomCap = DataIPShortCuts::rNumericArgs(1) * 1.e+09;
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ErrorsFound = true;
            }

            // Get Plant Inlet Node Num
            IceStorage(iceNum).PltInletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);

            // Get Plant Outlet Node Num
            IceStorage(iceNum).PltOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);

            // Test InletNode and OutletNode
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(3), DataIPShortCuts::cAlphaArgs(4), "Chilled Water Nodes");

            // Initialize Report Variables
            IceStorage(iceNum).MyLoad = 0.0;
            IceStorage(iceNum).Urate = 0.0;
            IceStorage(iceNum).IceFracRemain = 1.0;
            IceStorageReport(iceNum).ITSCoolingRate = 0.0;
            IceStorageReport(iceNum).ITSCoolingEnergy = 0.0;
            IceStorage(iceNum).ITSChargingRate = 0.0;
            IceStorage(iceNum).ITSChargingEnergy = 0.0;
            IceStorage(iceNum).ITSmdot = 0.0;
            IceStorage(iceNum).ITSInletTemp = 0.0;
            IceStorage(iceNum).ITSOutletTemp = 0.0;

        } // IceNum

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }

        // Setup Output Variables to Report  CurrentModuleObject='ThermalStorage:Ice:Simple'
        //********************************************
        for (int iceNum = 1; iceNum <= modNumIceStorages; ++iceNum) {

            SetupOutputVariable("Ice Thermal Storage Requested Load",
                                OutputProcessor::Unit::W,
                                IceStorage(iceNum).MyLoad,
                                "System",
                                "Average",
                                IceStorage(iceNum).Name);

            // Ice fraction
            SetupOutputVariable("Ice Thermal Storage End Fraction",
                                OutputProcessor::Unit::None,
                                IceStorage(iceNum).IceFracRemain,
                                "Zone",
                                "Average",
                                IceStorage(iceNum).Name);

            // Discharge: ITS Information
            SetupOutputVariable("Ice Thermal Storage Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                IceStorage(iceNum).ITSmdot,
                                "System",
                                "Average",
                                IceStorage(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Inlet Temperature",
                                OutputProcessor::Unit::C,
                                IceStorage(iceNum).ITSInletTemp,
                                "System",
                                "Average",
                                IceStorage(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Outlet Temperature",
                                OutputProcessor::Unit::C,
                                IceStorage(iceNum).ITSOutletTemp,
                                "System",
                                "Average",
                                IceStorage(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Cooling Discharge Rate",
                                OutputProcessor::Unit::W,
                                IceStorageReport(iceNum).ITSCoolingRate,
                                "System",
                                "Average",
                                IceStorage(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Cooling Discharge Energy",
                                OutputProcessor::Unit::J,
                                IceStorageReport(iceNum).ITSCoolingEnergy,
                                "System",
                                "Sum",
                                IceStorage(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Cooling Charge Rate",
                                OutputProcessor::Unit::W,
                                IceStorage(iceNum).ITSChargingRate,
                                "System",
                                "Average",
                                IceStorage(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Cooling Charge Energy",
                                OutputProcessor::Unit::J,
                                IceStorage(iceNum).ITSChargingEnergy,
                                "System",
                                "Sum",
                                IceStorage(iceNum).Name);

        } // IceNum

        ErrorsFound = false; // Always need to reset this since there are multiple types of ice storage systems

        // Determine the number of detailed ice storage devices are in the input file and allocate appropriately
        DataIPShortCuts::cCurrentModuleObject = cIceStorageDetailed;

        DetIceStor.allocate(modNumDetIceStorages); // Allocate DetIceStorage based on NumDetIceStorages

        for (int iceNum = 1; iceNum <= modNumDetIceStorages; ++iceNum) {

            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          iceNum,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          _,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

            ++modTotalIceStorages;
            IceStorageTypeMap(modTotalIceStorages).StorageType = DataIPShortCuts::cCurrentModuleObject;
            IceStorageTypeMap(modTotalIceStorages).StorageType_Num = IceStorageType::Detailed;
            IceStorageTypeMap(modTotalIceStorages).Name = DataIPShortCuts::cAlphaArgs(1);
            IceStorageTypeMap(modTotalIceStorages).LocalEqNum = iceNum;

            DetIceStor(iceNum).MapNum = modTotalIceStorages;
            DetIceStor(iceNum).Name = DataIPShortCuts::cAlphaArgs(1); // Detailed ice storage name

            // Get and verify availability schedule
            DetIceStor(iceNum).ScheduleName = DataIPShortCuts::cAlphaArgs(2); // Detailed ice storage availability schedule name
            if (DataIPShortCuts::lAlphaFieldBlanks(2)) {
                DetIceStor(iceNum).ScheduleIndex = DataGlobals::ScheduleAlwaysOn;
            } else {
                DetIceStor(iceNum).ScheduleIndex = ScheduleManager::GetScheduleIndex(DetIceStor(iceNum).ScheduleName);
                if (DetIceStor(iceNum).ScheduleIndex == 0) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            // Get and Verify ITS nominal Capacity (user input is in GJ, internal value is in W-hr)
            // Convert GJ to J by multiplying by 10^9
            // Convert J to W-hr by dividing by number of seconds in an hour (3600)
            DetIceStor(iceNum).NomCapacity = DataIPShortCuts::rNumericArgs(1) * (1.e+09) / (DataGlobals::SecInHour);

            if (DataIPShortCuts::rNumericArgs(1) <= 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            // Get Plant Inlet Node Num
            DetIceStor(iceNum).PlantInNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);

            // Get Plant Outlet Node Num
            DetIceStor(iceNum).PlantOutNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);

            // Test InletNode and OutletNode
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(3), DataIPShortCuts::cAlphaArgs(4), "Chilled Water Nodes");

            // Obtain the Charging and Discharging Curve types and names
            DetIceStor(iceNum).DischargeCurveName = DataIPShortCuts::cAlphaArgs(6);
            DetIceStor(iceNum).DischargeCurveNum = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(6));
            if (DetIceStor(iceNum).DischargeCurveNum <= 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(6) + '=' + DataIPShortCuts::cAlphaArgs(6));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            int dischargeCurveDim = CurveManager::PerfCurve(DetIceStor(iceNum).DischargeCurveNum).NumDims;
            if (dischargeCurveDim != 2) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ": Discharge curve must have 2 independent variables");
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError(DataIPShortCuts::cAlphaArgs(6) + " does not have 2 independent variables and thus cannot be used for detailed ice storage");
                ErrorsFound = true;
            } else {
                if (DataIPShortCuts::cAlphaArgs(5) == "FRACTIONCHARGEDLMTD") {
                    DetIceStor(iceNum).DischargeCurveTypeNum = CurveVars::FracChargedLMTD;
                } else if (DataIPShortCuts::cAlphaArgs(5) == "FRACTIONDISCHARGEDLMTD") {
                    DetIceStor(iceNum).DischargeCurveTypeNum = CurveVars::FracDischargedLMTD;
                } else if (DataIPShortCuts::cAlphaArgs(5) == "LMTDMASSFLOW") {
                    DetIceStor(iceNum).DischargeCurveTypeNum = CurveVars::LMTDMassFlow;
                } else if (DataIPShortCuts::cAlphaArgs(5) == "LMTDFRACTIONCHARGED") {
                    DetIceStor(iceNum).DischargeCurveTypeNum = CurveVars::LMTDFracCharged;
                } else {
                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ": Discharge curve independent variable options not valid, option=" + DataIPShortCuts::cAlphaArgs(5));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("The valid options are: FractionChargedLMTD, FractionDischargedLMTD, LMTDMassFlow or LMTDFractionCharged");
                    ErrorsFound = true;
                }
            }

            ErrorsFound |= CurveManager::CheckCurveDims(
                DetIceStor(iceNum).DischargeCurveNum,   // Curve index
                {2},                            // Valid dimensions
                "GetIceStorageInput: ",         // Routine name
                DataIPShortCuts::cCurrentModuleObject,           // Object Type
                DetIceStor(iceNum).Name,        // Object Name
                DataIPShortCuts::cAlphaFieldNames(6));           // Field Name

            DetIceStor(iceNum).ChargeCurveName = DataIPShortCuts::cAlphaArgs(8);
            DetIceStor(iceNum).ChargeCurveNum = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(8));
            if (DetIceStor(iceNum).ChargeCurveNum <= 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(8) + '=' + DataIPShortCuts::cAlphaArgs(8));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            int chargeCurveDim = CurveManager::PerfCurve(DetIceStor(iceNum).ChargeCurveNum).NumDims;
            if (chargeCurveDim != 2) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ": Charge curve must have 2 independent variables");
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError(DataIPShortCuts::cAlphaArgs(8) + " does not have 2 independent variables and thus cannot be used for detailed ice storage");
                ErrorsFound = true;
            } else {
                if (DataIPShortCuts::cAlphaArgs(7) == "FRACTIONCHARGEDLMTD") {
                    DetIceStor(iceNum).ChargeCurveTypeNum = CurveVars::FracChargedLMTD;
                } else if (DataIPShortCuts::cAlphaArgs(7) == "FRACTIONDISCHARGEDLMTD") {
                    DetIceStor(iceNum).ChargeCurveTypeNum = CurveVars::FracDischargedLMTD;
                } else if (DataIPShortCuts::cAlphaArgs(7) == "LMTDMASSFLOW") {
                    DetIceStor(iceNum).ChargeCurveTypeNum = CurveVars::LMTDMassFlow;
                } else if (DataIPShortCuts::cAlphaArgs(7) == "LMTDFRACTIONCHARGED") {
                    DetIceStor(iceNum).ChargeCurveTypeNum = CurveVars::LMTDFracCharged;
                } else {
                    ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ": Charge curve independent variable options not valid, option=" + DataIPShortCuts::cAlphaArgs(7));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("The valid options are: FractionChargedLMTD, FractionDischargedLMTD, LMTDMassFlow or LMTDFractionCharged");
                    ErrorsFound = true;
                }
            }

            ErrorsFound |= CurveManager::CheckCurveDims(
                DetIceStor(iceNum).ChargeCurveNum,   // Curve index
                {2},                            // Valid dimensions
                "GetIceStorageInput: ",         // Routine name
                DataIPShortCuts::cCurrentModuleObject,           // Object Type
                DetIceStor(iceNum).Name,        // Object Name
                DataIPShortCuts::cAlphaFieldNames(8));           // Field Name

            DetIceStor(iceNum).CurveFitTimeStep = DataIPShortCuts::rNumericArgs(2);
            if ((DetIceStor(iceNum).CurveFitTimeStep <= 0.0) || (DetIceStor(iceNum).CurveFitTimeStep > 1.0)) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(2) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 3));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Curve fit time step invalid, less than zero or greater than 1 for " + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            DetIceStor(iceNum).ThawProcessIndicator = DataIPShortCuts::cAlphaArgs(9);
            if (UtilityRoutines::SameString(DetIceStor(iceNum).ThawProcessIndicator, "INSIDEMELT")) {
                DetIceStor(iceNum).ThawProcessIndex = DetIce::InsideMelt;
            } else if ((UtilityRoutines::SameString(DetIceStor(iceNum).ThawProcessIndicator, "OUTSIDEMELT")) ||
                       (DetIceStor(iceNum).ThawProcessIndicator.empty())) {
                DetIceStor(iceNum).ThawProcessIndex = DetIce::OutsideMelt;
            } else {
                ShowSevereError("Invalid thaw process indicator of " + DataIPShortCuts::cAlphaArgs(9) + " was entered");
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError(R"(Value should either be "InsideMelt" or "OutsideMelt")");
                DetIceStor(iceNum).ThawProcessIndex = DetIce::InsideMelt; // Severe error will end simulation, but just in case...
                ErrorsFound = true;
            }

            // Get the other ice storage parameters (electric, heat loss, freezing temperature) and stupidity check each one
            DetIceStor(iceNum).DischargeParaElecLoad = DataIPShortCuts::rNumericArgs(3);
            DetIceStor(iceNum).ChargeParaElecLoad = DataIPShortCuts::rNumericArgs(4);
            DetIceStor(iceNum).TankLossCoeff = DataIPShortCuts::rNumericArgs(5);
            DetIceStor(iceNum).FreezingTemp = DataIPShortCuts::rNumericArgs(6);

            if ((DetIceStor(iceNum).DischargeParaElecLoad < 0.0) || (DetIceStor(iceNum).DischargeParaElecLoad > 1.0)) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(3) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(3), 3));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value is either less than/equal to zero or greater than 1");
                ErrorsFound = true;
            }

            if ((DetIceStor(iceNum).ChargeParaElecLoad < 0.0) || (DetIceStor(iceNum).ChargeParaElecLoad > 1.0)) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(4) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(4), 3));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value is either less than/equal to zero or greater than 1");
                ErrorsFound = true;
            }

            if ((DetIceStor(iceNum).TankLossCoeff < 0.0) || (DetIceStor(iceNum).TankLossCoeff > 0.1)) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(5) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(5), 3));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value is either less than/equal to zero or greater than 0.1 (10%)");
                ErrorsFound = true;
            }

            if ((DetIceStor(iceNum).FreezingTemp < -10.0) || (DetIceStor(iceNum).FreezingTemp > 10.0)) {
                ShowWarningError("Potentially invalid " + DataIPShortCuts::cNumericFieldNames(6) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(6), 3));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Value is either less than -10.0C or greater than 10.0C");
                ShowContinueError("This value will be allowed but the user should verify that this temperature is correct");
            }

            // Initialize Report Variables
            DetIceStor(iceNum).CompLoad = 0.0;
            DetIceStor(iceNum).IceFracChange = 0.0;
            DetIceStor(iceNum).IceFracRemaining = 1.0;
            DetIceStor(iceNum).IceFracOnCoil = 1.0;
            DetIceStor(iceNum).DischargingRate = 0.0;
            DetIceStor(iceNum).DischargingEnergy = 0.0;
            DetIceStor(iceNum).ChargingRate = 0.0;
            DetIceStor(iceNum).ChargingEnergy = 0.0;
            DetIceStor(iceNum).MassFlowRate = 0.0;
            DetIceStor(iceNum).BypassMassFlowRate = 0.0;
            DetIceStor(iceNum).TankMassFlowRate = 0.0;
            DetIceStor(iceNum).InletTemp = 0.0;
            DetIceStor(iceNum).OutletTemp = 0.0;
            DetIceStor(iceNum).TankOutletTemp = 0.0;
            DetIceStor(iceNum).ParasiticElecRate = 0.0;
            DetIceStor(iceNum).ParasiticElecEnergy = 0.0;

        } // ...over detailed ice storage units

        if ((modNumIceStorages + modNumDetIceStorages) <= 0) {
            ShowSevereError("No Ice Storage Equipment found in GetIceStorage");
            ErrorsFound = true;
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }

        // Setup Output Variables to Report CurrentModuleObject='ThermalStorage:Ice:Detailed'
        //********************************************
        for (int iceNum = 1; iceNum <= modNumDetIceStorages; ++iceNum) {

            SetupOutputVariable("Ice Thermal Storage Cooling Rate",
                                OutputProcessor::Unit::W,
                                DetIceStor(iceNum).CompLoad,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);

            // Ice fraction
            SetupOutputVariable("Ice Thermal Storage Change Fraction",
                                OutputProcessor::Unit::None,
                                DetIceStor(iceNum).IceFracChange,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage End Fraction",
                                OutputProcessor::Unit::None,
                                DetIceStor(iceNum).IceFracRemaining,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage On Coil Fraction",
                                OutputProcessor::Unit::None,
                                DetIceStor(iceNum).IceFracOnCoil,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);

            // Discharge: ITS Information
            SetupOutputVariable("Ice Thermal Storage Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                DetIceStor(iceNum).MassFlowRate,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Bypass Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                DetIceStor(iceNum).BypassMassFlowRate,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Tank Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                DetIceStor(iceNum).TankMassFlowRate,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Fluid Inlet Temperature",
                                OutputProcessor::Unit::C,
                                DetIceStor(iceNum).InletTemp,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Blended Outlet Temperature",
                                OutputProcessor::Unit::C,
                                DetIceStor(iceNum).OutletTemp,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Tank Outlet Temperature",
                                OutputProcessor::Unit::C,
                                DetIceStor(iceNum).TankOutletTemp,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Cooling Discharge Rate",
                                OutputProcessor::Unit::W,
                                DetIceStor(iceNum).DischargingRate,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Cooling Discharge Energy",
                                OutputProcessor::Unit::J,
                                DetIceStor(iceNum).DischargingEnergy,
                                "System",
                                "Sum",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Cooling Charge Rate",
                                OutputProcessor::Unit::W,
                                DetIceStor(iceNum).ChargingRate,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Cooling Charge Energy",
                                OutputProcessor::Unit::J,
                                DetIceStor(iceNum).ChargingEnergy,
                                "System",
                                "Sum",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Ancillary Electric Power",
                                OutputProcessor::Unit::W,
                                DetIceStor(iceNum).ParasiticElecRate,
                                "System",
                                "Average",
                                DetIceStor(iceNum).Name);
            SetupOutputVariable("Ice Thermal Storage Ancillary Electric Energy",
                                OutputProcessor::Unit::J,
                                DetIceStor(iceNum).ParasiticElecEnergy,
                                "System",
                                "Sum",
                                DetIceStor(iceNum).Name,
                                _,
                                "ELECTRICITY",
                                _,
                                _,
                                "System");

        } // ...over detailed ice storage units
    }

    void InitDetailedIceStorage(int const iceNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes variables for the detailed ice storage model.

        // METHODOLOGY EMPLOYED:
        // Initializes parameters based on current status flag values.

        static bool MyOneTimeFlag(true);
        static Array1D_bool MyPlantScanFlag;
        static Array1D_bool MyEnvrnFlag;
        int CompNum; // local do loop index
        // FLOW:

        if (MyOneTimeFlag) {
            MyPlantScanFlag.allocate(modNumDetIceStorages);
            MyEnvrnFlag.allocate(modNumDetIceStorages);
            MyPlantScanFlag = true;
            MyEnvrnFlag = true;
            MyOneTimeFlag = false;
        }

        if (MyPlantScanFlag(iceNum)) {
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(DetIceStor(iceNum).Name,
                                    DataPlant::TypeOf_TS_IceDetailed,
                                    DetIceStor(iceNum).PlantLoopNum,
                                    DetIceStor(iceNum).PlantLoopSideNum,
                                    DetIceStor(iceNum).PlantBranchNum,
                                    DetIceStor(iceNum).PlantCompNum,
                                    errFlag);
            // if errFlag then do something...
            MyPlantScanFlag(iceNum) = false;
        }

        if (DataGlobals::BeginEnvrnFlag && MyEnvrnFlag(iceNum)) { // Beginning of environment initializations
            // Make sure all state variables are reset at the beginning of every environment to avoid problems.
            // The storage unit is assumed to be fully charged at the start of any environment.
            // The IceNum variable is a module level variable that is already set before this subroutine is called.
            DetIceStor(iceNum).IceFracChange = 0.0;
            DetIceStor(iceNum).IceFracRemaining = 1.0;
            DetIceStor(iceNum).IceFracOnCoil = 1.0;
            DetIceStor(iceNum).InletTemp = 0.0;
            DetIceStor(iceNum).OutletTemp = 0.0;
            DetIceStor(iceNum).TankOutletTemp = 0.0;
            DetIceStor(iceNum).DischargeIterErrors = 0;
            DetIceStor(iceNum).ChargeIterErrors = 0;
            DetIceStor(iceNum).DesignMassFlowRate = DataPlant::PlantLoop(DetIceStor(iceNum).PlantLoopNum).MaxMassFlowRate;
            // no design flow rates for model, assume min is zero and max is plant loop's max
            PlantUtilities::InitComponentNodes(0.0,
                               DetIceStor(iceNum).DesignMassFlowRate,
                               DetIceStor(iceNum).PlantInNodeNum,
                               DetIceStor(iceNum).PlantOutNodeNum,
                               DetIceStor(iceNum).PlantLoopNum,
                               DetIceStor(iceNum).PlantLoopSideNum,
                               DetIceStor(iceNum).PlantBranchNum,
                               DetIceStor(iceNum).PlantCompNum);

            if ((DataPlant::PlantLoop(DetIceStor(iceNum).PlantLoopNum).CommonPipeType == DataPlant::CommonPipe_TwoWay) &&
                (DetIceStor(iceNum).PlantLoopSideNum == DataPlant::SupplySide)) {
                // up flow priority of other components on the same branch as the Ice tank
                for (CompNum = 1;
                     CompNum <=
                     DataPlant::PlantLoop(DetIceStor(iceNum).PlantLoopNum).LoopSide(DataPlant::SupplySide).Branch(DetIceStor(iceNum).PlantBranchNum).TotalComponents;
                     ++CompNum) {
                    DataPlant::PlantLoop(DetIceStor(iceNum).PlantLoopNum)
                        .LoopSide(DataPlant::SupplySide)
                        .Branch(DetIceStor(iceNum).PlantBranchNum)
                        .Comp(CompNum)
                        .FlowPriority = DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;
                }
            }

            MyEnvrnFlag(iceNum) = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) MyEnvrnFlag(iceNum) = true;

        // Initializations that are done every iteration
        // Make sure all of the reporting variables are always reset at the start of any iteration
        DetIceStor(iceNum).CompLoad = 0.0;
        DetIceStor(iceNum).IceFracChange = 0.0;
        DetIceStor(iceNum).DischargingRate = 0.0;
        DetIceStor(iceNum).DischargingEnergy = 0.0;
        DetIceStor(iceNum).ChargingRate = 0.0;
        DetIceStor(iceNum).ChargingEnergy = 0.0;
        DetIceStor(iceNum).MassFlowRate = 0.0;
        DetIceStor(iceNum).BypassMassFlowRate = 0.0;
        DetIceStor(iceNum).TankMassFlowRate = 0.0;
        DetIceStor(iceNum).ParasiticElecRate = 0.0;
        DetIceStor(iceNum).ParasiticElecEnergy = 0.0;
    }

    void InitSimpleIceStorage(int const iceNum)
    {

        static Array1D_bool MyPlantScanFlag;
        static bool MyOneTimeFlag(true);
        static Array1D_bool MyEnvrnFlag;
        bool errFlag;
        int CompNum; // local do loop counter

        if (MyOneTimeFlag) {
            MyPlantScanFlag.allocate(modNumIceStorages);
            MyEnvrnFlag.allocate(modNumIceStorages);
            MyOneTimeFlag = false;
            MyPlantScanFlag = true;
            MyEnvrnFlag = true;
        }

        if (MyPlantScanFlag(iceNum)) {
            // Locate the storage on the plant loops for later usage
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(IceStorage(iceNum).Name,
                                    DataPlant::TypeOf_TS_IceSimple,
                                    IceStorage(iceNum).LoopNum,
                                    IceStorage(iceNum).LoopSideNum,
                                    IceStorage(iceNum).BranchNum,
                                    IceStorage(iceNum).CompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);
            if (errFlag) {
                ShowFatalError("InitSimpleIceStorage: Program terminated due to previous condition(s).");
            }
            MyPlantScanFlag(iceNum) = false;
        }

        if (DataGlobals::BeginEnvrnFlag && MyEnvrnFlag(iceNum)) {
            IceStorage(iceNum).DesignMassFlowRate = DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).MaxMassFlowRate;
            // no design flow rates for model, assume min is zero and max is plant loop's max
            PlantUtilities::InitComponentNodes(0.0,
                               IceStorage(iceNum).DesignMassFlowRate,
                               IceStorage(iceNum).PltInletNodeNum,
                               IceStorage(iceNum).PltOutletNodeNum,
                               IceStorage(iceNum).LoopNum,
                               IceStorage(iceNum).LoopSideNum,
                               IceStorage(iceNum).BranchNum,
                               IceStorage(iceNum).CompNum);
            if ((DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).CommonPipeType == DataPlant::CommonPipe_TwoWay) && (IceStorage(iceNum).LoopSideNum == DataPlant::SupplySide)) {
                // up flow priority of other components on the same branch as the Ice tank
                for (CompNum = 1;
                     CompNum <= DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).LoopSide(DataPlant::SupplySide).Branch(IceStorage(iceNum).BranchNum).TotalComponents;
                     ++CompNum) {
                    DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).LoopSide(DataPlant::SupplySide).Branch(IceStorage(iceNum).BranchNum).Comp(CompNum).FlowPriority =
                        DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;
                }
            }
            IceStorage(iceNum).MyLoad = 0.0;
            IceStorage(iceNum).Urate = 0.0;
            IceStorage(iceNum).IceFracRemain = 1.0;
            IceStorage(iceNum).ITSCoolingRate = 0.0;
            IceStorageReport(iceNum).ITSCoolingEnergy = 0.0;
            IceStorage(iceNum).ITSChargingRate = 0.0;
            IceStorage(iceNum).ITSChargingEnergy = 0.0;
            IceStorage(iceNum).ITSmdot = 0.0;
            IceStorage(iceNum).ITSInletTemp = 0.0;
            IceStorage(iceNum).ITSOutletTemp = 0.0;

            MyEnvrnFlag(iceNum) = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) MyEnvrnFlag(iceNum) = true;
    }

    //******************************************************************************

    void CalcIceStorageCapacity(int const IceStorageType, Real64 &MaxCap, Real64 &MinCap, Real64 &OptCap, int iceNum)
    {

        Real64 Umin(0.0); // Min Urate  [fraction]
        Real64 Uact(0.0); // Acting between Umax and Umin [fraction]
        Real64 ITSCoolingRateMax;
        Real64 ITSCoolingRateOpt;
        Real64 ITSCoolingRateMin;
        Real64 QiceMin;

        {
            auto const SELECT_CASE_var(IceStorageType);
            if (SELECT_CASE_var == IceStorageType::Simple) {

                //------------------------------------------------------------------------
                // FIRST PROCESS (MyLoad = 0.0 as IN)
                // At this moment as first calling of ITS, ITS provide ONLY MaxCap/OptCap/MinCap.
                //------------------------------------------------------------------------

                // Initialize Capacity
                MaxCap = 0.0;
                MinCap = 0.0;
                OptCap = 0.0;

                // XCurIceFrac is reset to 1.0 when first hour of day.
                // Starting full is assumed, because most ice systems are fully charged overnight
                if (IceStorage(iceNum).ResetXForITSFlag) {
                    IceStorage(iceNum).XCurIceFrac = 1.0;
                    IceStorage(iceNum).IceFracRemain = 1.0;
                    IceStorage(iceNum).Urate = 0.0;
                    IceStorage(iceNum).ResetXForITSFlag = false;
                }

                // Calculate UAIceDisch[W/C] and UAIceCh[W/F] based on ONLY XCurIceFrac
                CalcUAIce(iceNum, IceStorage(iceNum).XCurIceFrac, IceStorage(iceNum).UAIceCh, IceStorage(iceNum).UAIceDisCh, IceStorage(iceNum).HLoss);

                // Calculate QiceMin by UAIceDisCh*deltaTlm
                //   with UAIceDisCh(function of XCurIceFrac), ITSInletTemp and ITSOutletTemp(=Node(OutletNodeNum)%TempSetPoint by E+[C])
                // QiceMin is REAL(r64) ITS capacity.
                CalcQiceDischageMax(QiceMin, iceNum);

                // At the first call of ITS model, MyLoad is 0. After that proper MyLoad will be provided by E+.
                // Therefore, Umin is decided between input U and ITS REAL(r64) capacity.
                Umin = min(max((-(1.0 - modEpsLimitForDisCharge) * QiceMin * modTimeInterval / IceStorage(iceNum).ITSNomCap), (-IceStorage(iceNum).XCurIceFrac + modEpsLimitForX)), 0.0);

                // Calculate CoolingRate with Uact to provide E+.
                Uact = Umin;
                ITSCoolingRateMax = std::abs(Uact * IceStorage(iceNum).ITSNomCap / modTimeInterval);
                ITSCoolingRateOpt = ITSCoolingRateMax;
                ITSCoolingRateMin = 0.0;

                // Define MaxCap, OptCap, and MinCap
                MaxCap = ITSCoolingRateMax;
                OptCap = ITSCoolingRateOpt;
                MinCap = ITSCoolingRateMin;

            } else {
            }
        }
    }

    //******************************************************************************

    void CalcIceStorageDormant(int const IceStorageType, // BY ZG
                               int &iceNum)
    {
        {
            auto const SELECT_CASE_var(IceStorageType); // by ZG

            if (SELECT_CASE_var == IceStorageType::Simple) { // by ZG

                // Provide output results for ITS.
                IceStorage(iceNum).ITSMassFlowRate = 0.0; //[kg/s]

                PlantUtilities::SetComponentFlowRate(IceStorage(iceNum).ITSMassFlowRate,
                                                     IceStorage(iceNum).PltInletNodeNum,
                                                     IceStorage(iceNum).PltOutletNodeNum,
                                                     IceStorage(iceNum).LoopNum,
                                                     IceStorage(iceNum).LoopSideNum,
                                                     IceStorage(iceNum).BranchNum,
                                                     IceStorage(iceNum).CompNum);

                IceStorage(iceNum).ITSInletTemp = DataLoopNode::Node(IceStorage(iceNum).PltInletNodeNum).Temp; //[C]
                IceStorage(iceNum).ITSOutletTemp = IceStorage(iceNum).ITSInletTemp;           //[C]
                {
                    auto const SELECT_CASE_var1(DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var1 == DataPlant::SingleSetPoint) {
                        IceStorage(iceNum).ITSOutletSetPointTemp = DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var1 == DataPlant::DualSetPointDeadBand) {
                        IceStorage(iceNum).ITSOutletSetPointTemp = DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).TempSetPointHi;
                    }
                }
                IceStorage(iceNum).ITSCoolingRate = 0.0;   //[W]
                IceStorage(iceNum).ITSCoolingEnergy = 0.0; //[J]

                IceStorage(iceNum).Urate = 0.0; //[n/a]

            } else {
            }
        }
    }

    //******************************************************************************

    void CalcIceStorageCharge(int const IceStorageType, // BY ZG
                              int &iceNum)
    {
        Real64 Umax(0.0);        // Max Urate adjusted Urate based on Error protection (I) [fraction]
        Real64 Uact(0.0);        // Acting between Usys and UsysLow Urate adjusted Urate based on Error protection (I) [fraction]
        Real64 QiceMax;          // [W]
        Real64 QiceMaxByChiller; // [W]
        Real64 QiceMaxByITS;     // [W]
        Real64 Qice;             // [W]
        Real64 DeltaTemp;        // [C]

        {
            auto const SELECT_CASE_var(IceStorageType);
            if (SELECT_CASE_var == IceStorageType::Simple) {

                //--------------------------------------------------------
                // Initialize
                //--------------------------------------------------------
                // Below values for ITS are reported forCharging process.
                IceStorage(iceNum).ITSMassFlowRate = IceStorage(iceNum).DesignMassFlowRate; //[kg/s]

                PlantUtilities::SetComponentFlowRate(IceStorage(iceNum).ITSMassFlowRate,
                                                     IceStorage(iceNum).PltInletNodeNum,
                                                     IceStorage(iceNum).PltOutletNodeNum,
                                                     IceStorage(iceNum).LoopNum,
                                                     IceStorage(iceNum).LoopSideNum,
                                                     IceStorage(iceNum).BranchNum,
                                                     IceStorage(iceNum).CompNum);

                IceStorage(iceNum).ITSInletTemp = DataLoopNode::Node(IceStorage(iceNum).PltInletNodeNum).Temp; //[C]
                IceStorage(iceNum).ITSOutletTemp = IceStorage(iceNum).ITSInletTemp;           //[C]
                {
                    auto const SELECT_CASE_var1(DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var1 == DataPlant::SingleSetPoint) {
                        IceStorage(iceNum).ITSOutletSetPointTemp = DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var1 == DataPlant::DualSetPointDeadBand) {
                        IceStorage(iceNum).ITSOutletSetPointTemp = DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).TempSetPointHi;
                    }
                }
                IceStorage(iceNum).ITSCoolingRate = 0.0;   //[W]
                IceStorage(iceNum).ITSCoolingEnergy = 0.0; //[J]

                // Initialize processed U values
                IceStorage(iceNum).Urate = 0.0;

                // Calculate QiceMax which is REAL(r64) ITS capacity.
                // There are three possible to calculate QiceMax
                //   with ChillerCapacity(Chiller+ITS), ITS capacity(ITS), and QchillerMax(Chiller).
                //--------------------------------------------------------
                // Calcualte QiceMax with QiceMaxByChiller, QiceMaxByITS, QchillerMax
                //--------------------------------------------------------
                // Calculate Qice charge max by Chiller with Twb and UAIceCh
                CalcQiceChargeMaxByChiller(iceNum, QiceMaxByChiller); //[W]

                // Chiller is remote now, so chiller out is inlet node temp
                Real64 chillerOutletTemp = DataLoopNode::Node(IceStorage(iceNum).PltInletNodeNum).Temp;
                // Calculate Qice charge max by ITS with ChillerOutletTemp
                CalcQiceChargeMaxByITS(iceNum, chillerOutletTemp, QiceMaxByITS); //[W]

                // Select minimum as QiceMax
                // Because It is uncertain that QiceMax by chiller is same as QiceMax by ITS.
                QiceMax = min(QiceMaxByChiller, QiceMaxByITS);

                //--------------------------------------------------------
                // Calculate Umin,Umax,Uact
                //--------------------------------------------------------
                // Set Umin
                // Calculate Umax based on real ITS Max Capacity and remained XCurIceFrac.
                // Umax should be equal or larger than 0.02 for realistic purpose by Dion.
                Umax = max(min(((1.0 - modEpsLimitForCharge) * QiceMax * modTimeInterval / IceStorage(iceNum).ITSNomCap), (1.0 - IceStorage(iceNum).XCurIceFrac - modEpsLimitForX)), 0.0);

                // Cannot charge more than the fraction that is left uncharged
                Umax = min(Umax, (1.0 - IceStorage(iceNum).IceFracRemain) / DataHVACGlobals::TimeStepSys);
                // First, check input U value.
                // Based on Umax and Umin, if necessary to run E+, calculate proper Uact.
                if (Umax == 0.0) { //(No Capacity of ITS), ITS is OFF.
                    Uact = 0.0;

                } else { // Umax non-zero
                    Uact = Umax;
                } // Check Uact for Discharging Process

                //--------------------------------------------------------
                // Calcualte possible ITSChargingRate with Uact, Then error check
                //--------------------------------------------------------
                // Calculate possible ITSChargingRate with Uact
                Qice = Uact * IceStorage(iceNum).ITSNomCap / modTimeInterval; //[W]
                // If Qice is equal or less than 0.0, no need to calculate anymore.
                if (Qice <= 0.0) {
                    IceStorage(iceNum).Urate = 0.0; //[ratio]
                }

                // Calculate leaving water temperature
                if ((Qice <= 0.0) || (IceStorage(iceNum).XCurIceFrac >= 1.0)) {
                    IceStorage(iceNum).ITSOutletTemp = IceStorage(iceNum).ITSInletTemp;
                    Qice = 0.0;
                    Uact = 0.0;
                } else {
                    DeltaTemp = Qice / Psychrometrics::CPCW(IceStorage(iceNum).ITSInletTemp) / IceStorage(iceNum).ITSMassFlowRate;
                    IceStorage(iceNum).ITSOutletTemp = IceStorage(iceNum).ITSInletTemp + DeltaTemp;
                    // Limit leaving temp to be no greater than setpoint or freezing temp minus 1C
                    IceStorage(iceNum).ITSOutletTemp = min(IceStorage(iceNum).ITSOutletTemp, IceStorage(iceNum).ITSOutletSetPointTemp, (modFreezTemp - 1));
                    // Limit leaving temp to be no less than inlet temp
                    IceStorage(iceNum).ITSOutletTemp = max(IceStorage(iceNum).ITSOutletTemp, IceStorage(iceNum).ITSInletTemp);
                    DeltaTemp = IceStorage(iceNum).ITSOutletTemp - IceStorage(iceNum).ITSInletTemp;
                    Qice = DeltaTemp * Psychrometrics::CPCW(IceStorage(iceNum).ITSInletTemp) * IceStorage(iceNum).ITSMassFlowRate;
                    Uact = Qice / (IceStorage(iceNum).ITSNomCap / modTimeInterval);
                } // End of leaving temp checks

                IceStorage(iceNum).Urate = Uact;
                IceStorage(iceNum).ITSCoolingRate = -Qice;
                IceStorage(iceNum).ITSCoolingEnergy = IceStorage(iceNum).ITSCoolingRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

            } else {
            }
        }
    }

    //******************************************************************************

    void CalcQiceChargeMaxByChiller(int &iceNum, Real64 &QiceMaxByChiller)
    {
        // METHODOLOGY EMPLOYED:
        // Calculation inside is IP unit, then return QiceMaxByChiller as SI [W] unit.

        Real64 TchillerOut;

        // Chiller is remote now, so chiller out is inlet node temp
        TchillerOut = DataLoopNode::Node(IceStorage(iceNum).PltInletNodeNum).Temp;
        QiceMaxByChiller = IceStorage(iceNum).UAIceCh * (modFreezTemp - TchillerOut); //[W] = [W/degC]*[degC]

        // If it happened, it is occurred at the Discharging or Dormant process.
        if (QiceMaxByChiller <= 0.0) {
            QiceMaxByChiller = 0.0;
        }
    }

    void CalcQiceChargeMaxByITS(int iceNum,
                                Real64 const chillerOutletTemp, // [degC]
                                Real64 &QiceMaxByITS            // [W]
    )
    {
        Real64 Tfr;
        Real64 ChillerInletTemp;
        Real64 ChOutletTemp;
        Real64 LogTerm;

        // Qice is maximized when ChillerInletTemp and ChillerOutletTemp(input data) is almost same due to LMTD method.
        // Qice is minimized(=0) when ChillerInletTemp is almost same as FreezTemp(=0).

        // Initilize
        Tfr = modFreezTempIP;
        ChOutletTemp = TempSItoIP(chillerOutletTemp); //[degF] = ConvertSItoIP[degC]
        // Chiller outlet temp must be below freeze temp, or else no charge
        if (ChOutletTemp >= Tfr) {
            QiceMaxByITS = 0.0;
        } else {
            // Make ChillerInletTemp as almost same as ChillerOutletTemp(input data)
            ChillerInletTemp = ChOutletTemp + 0.01;
            // ChillerInletTemp cannot be greater than or equal to freeze temp
            if (ChillerInletTemp >= Tfr) {
                ChillerInletTemp = ChOutletTemp + (Tfr - ChOutletTemp) / 2;
            }

            LogTerm = (Tfr - ChOutletTemp) / (Tfr - ChillerInletTemp);
            // Need to protect this from LogTerm <= 0 - not sure what it should do then
            if (LogTerm <= 0.0) {
                ChillerInletTemp = ChOutletTemp;
                QiceMaxByITS = 0.0;
            }
            QiceMaxByITS = IceStorage(iceNum).UAIceCh * (TempIPtoSI(ChillerInletTemp) - TempIPtoSI(ChOutletTemp)) / std::log(LogTerm);
        }
    }

    void CalcIceStorageDischarge(int const IceStorageType,             // by ZG
                                 int const iceNum,                     // ice storage number
                                 Real64 const MyLoad,                  // operating load
                                 bool const RunFlag,                   // TRUE when ice storage operating
                                 bool const EP_UNUSED(FirstIteration), // TRUE when first iteration of timestep
                                 Real64 const MaxCap                   // Max possible discharge rate (positive value)
    )
    {
        std::string const RoutineName("CalcIceStorageDischarge");
        Real64 Umax(0.0); // Max Urate adjusted Urate based on Error protection (I) [fraction]
        Real64 Umin(0.0); // Min Urate adjusted Urate based on Error protection (I) [fraction]
        Real64 Uact(0.0); // Acting between Usys and UsysLow Urate adjusted Urate based on Error protection (I) [fraction]
        Real64 Umyload(0.0);
        Real64 Qice(0.0);
        Real64 DeltaTemp(0.0);

        int LoopNum;
        Real64 CpFluid; // local temporary for plant loop's fluid specific heat

        {
            auto const SELECT_CASE_var(IceStorageType);
            if (SELECT_CASE_var == IceStorageType::Simple) {

                // Initialize processed Rate and Energy
                IceStorage(iceNum).ITSMassFlowRate = 0.0;
                IceStorage(iceNum).ITSCoolingRate = 0.0;
                IceStorage(iceNum).ITSCoolingEnergy = 0.0;

                {
                    auto const SELECT_CASE_var1(DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var1 == DataPlant::SingleSetPoint) {
                        IceStorage(iceNum).ITSOutletSetPointTemp = DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var1 == DataPlant::DualSetPointDeadBand) {
                        IceStorage(iceNum).ITSOutletSetPointTemp = DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).TempSetPointHi;
                    }
                }

                // Initialize processed U values
                IceStorage(iceNum).Urate = 0.0;

                // If no component demand or ITS OFF, then RETURN.
                if (MyLoad == 0 || !RunFlag) {
                    IceStorage(iceNum).ITSMassFlowRate = 0.0;
                    IceStorage(iceNum).ITSInletTemp = DataLoopNode::Node(IceStorage(iceNum).PltInletNodeNum).Temp;
                    IceStorage(iceNum).ITSOutletTemp = IceStorage(iceNum).ITSInletTemp;
                    IceStorage(iceNum).ITSCoolingRate = 0.0;
                    IceStorage(iceNum).ITSCoolingEnergy = 0.0;
                    return;
                }

                // If FlowLock(provided by PlantSupplyManager) is False(=0), that is, MyLoad is not changed.
                // then based on MyLoad, new ITSMassFlowRate will be calculated.

                //----------------------------
                LoopNum = IceStorage(iceNum).LoopNum;

                CpFluid = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(LoopNum).FluidName, DataLoopNode::Node(IceStorage(iceNum).PltInletNodeNum).Temp, DataPlant::PlantLoop(LoopNum).FluidIndex, RoutineName);

                // Calculate Umyload based on MyLoad from E+
                Umyload = -MyLoad * modTimeInterval / IceStorage(iceNum).ITSNomCap;
                // Calculate Umax and Umin
                // Cannot discharge more than the fraction that is left
                Umax = -IceStorage(iceNum).IceFracRemain / DataHVACGlobals::TimeStepSys;
                // Calculate Umin based on returned MyLoad from E+.
                Umin = min(Umyload, 0.0);
                // Based on Umax and Umin, if necessary to run E+, calculate proper Uact
                // U is negative here.
                Uact = max(Umin, Umax);

                // Set ITSInletTemp provided by E+
                IceStorage(iceNum).ITSInletTemp = DataLoopNode::Node(IceStorage(iceNum).PltInletNodeNum).Temp;
                // The first thing is to set the ITSMassFlowRate
                IceStorage(iceNum).ITSMassFlowRate = IceStorage(iceNum).DesignMassFlowRate; //[kg/s]

                PlantUtilities::SetComponentFlowRate(IceStorage(iceNum).ITSMassFlowRate,
                                                     IceStorage(iceNum).PltInletNodeNum,
                                                     IceStorage(iceNum).PltOutletNodeNum,
                                                     IceStorage(iceNum).LoopNum,
                                                     IceStorage(iceNum).LoopSideNum,
                                                     IceStorage(iceNum).BranchNum,
                                                     IceStorage(iceNum).CompNum);

                // Qice is calculate input U which is within boundary between Umin and Umax.
                Qice = Uact * IceStorage(iceNum).ITSNomCap / modTimeInterval;
                // Qice cannot exceed MaxCap calulated by CalcIceStorageCapacity
                // Note Qice is negative here, MaxCap is positive
                Qice = max(Qice, -MaxCap);

                // Calculate leaving water temperature
                if ((Qice >= 0.0) || (IceStorage(iceNum).XCurIceFrac <= 0.0) || (IceStorage(iceNum).ITSMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance)) {
                    IceStorage(iceNum).ITSOutletTemp = IceStorage(iceNum).ITSInletTemp;
                    Qice = 0.0;
                    Uact = 0.0;
                } else {
                    DeltaTemp = Qice / CpFluid / IceStorage(iceNum).ITSMassFlowRate;
                    IceStorage(iceNum).ITSOutletTemp = IceStorage(iceNum).ITSInletTemp + DeltaTemp;
                    // Limit leaving temp to be no less than setpoint or freezing temp plus 1C
                    IceStorage(iceNum).ITSOutletTemp = max(IceStorage(iceNum).ITSOutletTemp, IceStorage(iceNum).ITSOutletSetPointTemp, (modFreezTemp + 1));
                    // Limit leaving temp to be no greater than inlet temp
                    IceStorage(iceNum).ITSOutletTemp = min(IceStorage(iceNum).ITSOutletTemp, IceStorage(iceNum).ITSInletTemp);
                    DeltaTemp = IceStorage(iceNum).ITSOutletTemp - IceStorage(iceNum).ITSInletTemp;
                    Qice = DeltaTemp * CpFluid * IceStorage(iceNum).ITSMassFlowRate;
                    Uact = Qice / (IceStorage(iceNum).ITSNomCap / modTimeInterval);
                } // End of leaving temp checks

                // Calculate reported U value
                IceStorage(iceNum).Urate = Uact;
                // Calculate ITSCoolingEnergy [J]
                IceStorage(iceNum).ITSCoolingRate = -Qice;
                IceStorage(iceNum).ITSCoolingEnergy = IceStorage(iceNum).ITSCoolingRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

            } else {
            }
        }
    }

    void CalcQiceDischageMax(Real64 &QiceMin, int iceNum)
    {
        Real64 ITSInletTemp_loc;
        Real64 ITSOutletTemp_loc(0.0);
        Real64 LogTerm;

        // Qice is minimized when ITSInletTemp and ITSOutletTemp is almost same due to LMTD method.
        // Qice is maximized(=0) when ITSOutletTemp is almost same as FreezTemp(=0).

        ITSInletTemp_loc = DataLoopNode::Node(IceStorage(iceNum).PltInletNodeNum).Temp;
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(IceStorage(iceNum).LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                ITSOutletTemp_loc = DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).TempSetPoint;
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                ITSOutletTemp_loc = DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).TempSetPointHi;
            } else {
                assert(false);
            }
        }

        LogTerm = (ITSInletTemp_loc - modFreezTemp) / (ITSOutletTemp_loc - modFreezTemp);

        if (LogTerm <= 1) {
            QiceMin = 0.0;
        } else {
            QiceMin = IceStorage(iceNum).UAIceDisCh * (ITSInletTemp_loc - ITSOutletTemp_loc) / std::log(LogTerm);
        }
    }

    void CalcUAIce(int const iceNum, Real64 const XCurIceFrac_loc, Real64 &UAIceCh_loc, Real64 &UAIceDisCh_loc, Real64 &HLoss_loc)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR
        //       DATE WRITTEN
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:

        // METHODOLOGY EMPLOYED:
        // This routine is function of XCurIceFrac, and UA value is based on 1 hour.

        Real64 y;
        {
            auto const SELECT_CASE_var(IceStorage(iceNum).ITSType_Num);
            if (SELECT_CASE_var == ITSType::IceOnCoilInternal) {
                y = XCurIceFrac_loc;
                UAIceCh_loc = (1.3879 - 7.6333 * y + 26.3423 * pow_2(y) - 47.6084 * pow_3(y) + 41.8498 * pow_4(y) - 14.2948 * pow_5(y)) *
                              IceStorage(iceNum).ITSNomCap / modTimeInterval / 10.0; // [W/C]
                y = 1.0 - XCurIceFrac_loc;
                UAIceDisCh_loc = (1.3879 - 7.6333 * y + 26.3423 * pow_2(y) - 47.6084 * pow_3(y) + 41.8498 * pow_4(y) - 14.2948 * pow_5(y)) *
                                 IceStorage(iceNum).ITSNomCap / modTimeInterval / 10.0; // [W/C]
                HLoss_loc = 0.0;
            } else if (SELECT_CASE_var == ITSType::IceOnCoilExternal) {
                y = XCurIceFrac_loc;
                UAIceCh_loc = (1.3879 - 7.6333 * y + 26.3423 * pow_2(y) - 47.6084 * pow_3(y) + 41.8498 * pow_4(y) - 14.2948 * pow_5(y)) *
                              IceStorage(iceNum).ITSNomCap / modTimeInterval / 10.0; // [W/C]
                y = 1.0 - XCurIceFrac_loc;
                UAIceDisCh_loc = (1.1756 - 5.3689 * y + 17.3602 * pow_2(y) - 30.1077 * pow_3(y) + 25.6387 * pow_4(y) - 8.5102 * pow_5(y)) *
                                 IceStorage(iceNum).ITSNomCap / modTimeInterval / 10.0; // [W/C]
                HLoss_loc = 0.0;
            }
        }
    }

    Real64 CalcDetIceStorLMTDstar(Real64 const Tin,  // ice storage unit inlet temperature
                                  Real64 const Tout, // ice storage unit outlet (setpoint) temperature
                                  Real64 const Tfr   // freezing temperature
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the log mean temperature difference for
        // the detailed ice storage unit.  The temperature difference is non-
        // dimensionalized using a nominal temperature difference of 10C.
        // This value must be used when obtaining the curve fit coefficients.

        // METHODOLOGY EMPLOYED:
        // Straight-forward calculation where:
        // LMTD* = LMTD/Tnom
        // LMTD = (Tin-Tout)/ln((Tin-Tfr)/(Tout-Tfr))

        Real64 CalcDetIceStorLMTDstar;
        Real64 const Tnom(10.0); // Nominal temperature difference across the ice storage unit [C]

        Real64 DeltaTio; // Inlet to outlet temperature difference
        Real64 DeltaTif; // Inlet to freezing temperature difference
        Real64 DeltaTof; // Outlet to freezing temperature difference

        // First set the temperature differences and avoid problems with the LOG
        // term by setting some reasonable minimums
        DeltaTio = std::abs(Tin - Tout);
        DeltaTif = std::abs(Tin - Tfr);
        DeltaTof = std::abs(Tout - Tfr);

        if (DeltaTif < modDeltaTifMin) DeltaTif = modDeltaTifMin;
        if (DeltaTof < modDeltaTofMin) DeltaTof = modDeltaTofMin;

        CalcDetIceStorLMTDstar = (DeltaTio / std::log(DeltaTif / DeltaTof)) / Tnom;

        return CalcDetIceStorLMTDstar;
    }

    Real64 CalcQstar(int const CurveIndex,      // curve index
                     int const CurveIndVarType, // independent variable type for ice storage
                     Real64 const FracCharged,  // fraction charged for ice storage unit
                     Real64 const LMTDstar,     // normalized log mean temperature difference across the ice storage unit
                     Real64 const MassFlowstar  // normalized mass flow rate through the ice storage unit
    )
    {

        Real64 CalcQstar;
        
        if (CurveIndVarType == CurveVars::FracChargedLMTD) {
            CalcQstar = std::abs(CurveManager::CurveValue(CurveIndex, FracCharged, LMTDstar));
        } else if (CurveIndVarType == CurveVars::FracDischargedLMTD) {
            CalcQstar = std::abs(CurveManager::CurveValue(CurveIndex, (1.0-FracCharged), LMTDstar));
        } else if (CurveIndVarType == CurveVars::LMTDMassFlow) {
            CalcQstar = std::abs(CurveManager::CurveValue(CurveIndex, LMTDstar, MassFlowstar));
        } else if (CurveIndVarType == CurveVars::LMTDFracCharged) {
            CalcQstar = std::abs(CurveManager::CurveValue(CurveIndex, LMTDstar, FracCharged));
        } else { // should never get here as this is checked on input
            CalcQstar = 0.0;
        }
        
        return CalcQstar;

    }

    Real64 TempSItoIP(Real64 const Temp)
    {
        return (Temp * 9.0 / 5.0) + 32.0;
    }

    Real64 TempIPtoSI(Real64 const Temp)
    {
        return (Temp - 32.0) * 5.0 / 9.0;
    }

    void UpdateNode(Real64 const MyLoad, bool const RunFlag, int const iceNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998

        // Update Node Inlet & Outlet MassFlowRat
        PlantUtilities::SafeCopyPlantNode(IceStorage(iceNum).PltInletNodeNum, IceStorage(iceNum).PltOutletNodeNum);
        if (MyLoad == 0 || !RunFlag) {
            // Update Outlet Conditions so that same as Inlet, so component can be bypassed if necessary
            DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).Temp = DataLoopNode::Node(IceStorage(iceNum).PltInletNodeNum).Temp;
        } else {
            DataLoopNode::Node(IceStorage(iceNum).PltOutletNodeNum).Temp = IceStorage(iceNum).ITSOutletTemp;
        }
    }

    void RecordOutput(int const iceNum, Real64 const MyLoad, bool const RunFlag)
    {
        if (MyLoad == 0 || !RunFlag) {
            IceStorage(iceNum).MyLoad = MyLoad;
            IceStorageReport(iceNum).ITSCoolingRate = 0.0;
            IceStorageReport(iceNum).ITSCoolingEnergy = 0.0;
            IceStorage(iceNum).ITSChargingRate = 0.0;
            IceStorage(iceNum).ITSChargingEnergy = 0.0;
            IceStorage(iceNum).ITSmdot = 0.0;

        } else {
            IceStorage(iceNum).MyLoad = MyLoad;
            if (IceStorage(iceNum).ITSCoolingRate > 0.0) {
                IceStorageReport(iceNum).ITSCoolingRate = IceStorage(iceNum).ITSCoolingRate;
                IceStorageReport(iceNum).ITSCoolingEnergy = IceStorage(iceNum).ITSCoolingEnergy;
                IceStorage(iceNum).ITSChargingRate = 0.0;
                IceStorage(iceNum).ITSChargingEnergy = 0.0;
            } else {
                IceStorageReport(iceNum).ITSCoolingRate = 0.0;
                IceStorageReport(iceNum).ITSCoolingEnergy = 0.0;
                IceStorage(iceNum).ITSChargingRate = -IceStorage(iceNum).ITSCoolingRate;
                IceStorage(iceNum).ITSChargingEnergy = -IceStorage(iceNum).ITSCoolingEnergy;
            }
            IceStorage(iceNum).ITSmdot = IceStorage(iceNum).ITSMassFlowRate;
        }
    }

    void UpdateIceFractions()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mike Witte
        //       DATE WRITTEN   September 2005
        //       MODIFIED       Rick Strand (Feb 2006, for detailed ice storage model)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Update all ice fractions at end of system time step.

        // METHODOLOGY EMPLOYED:
        // This is called from HVACManager once we have actually stepped forward
        // a system time step.

        int IceNum2;

        for (IceNum2 = 1; IceNum2 <= modNumIceStorages; ++IceNum2) {
            IceStorage(IceNum2).IceFracRemain += IceStorage(IceNum2).Urate * DataHVACGlobals::TimeStepSys;
            if (IceStorage(IceNum2).IceFracRemain <= 0.001) IceStorage(IceNum2).IceFracRemain = 0.0;
            if (IceStorage(IceNum2).IceFracRemain > 1.0) IceStorage(IceNum2).IceFracRemain = 1.0;
        }

        for (IceNum2 = 1; IceNum2 <= modNumDetIceStorages; ++IceNum2) {
            DetIceStor(IceNum2).IceFracRemaining += DetIceStor(IceNum2).IceFracChange - (DetIceStor(IceNum2).TankLossCoeff * DataHVACGlobals::TimeStepSys);
            if (DetIceStor(IceNum2).IceFracRemaining < 0.001) DetIceStor(IceNum2).IceFracRemaining = 0.0;
            if (DetIceStor(IceNum2).IceFracRemaining > 1.000) DetIceStor(IceNum2).IceFracRemaining = 1.0;
            // Reset the ice on the coil to zero for inside melt whenever discharging takes place.
            // This assumes that any remaining ice floats away from the coil and resettles perfectly.
            // While this is not exactly what happens and it is possible theoretically to have multiple
            // freeze thaw cycles that are not complete, this is the best we can do.
            if (DetIceStor(IceNum2).ThawProcessIndex == DetIce::InsideMelt) {
                if (DetIceStor(IceNum2).IceFracChange < 0.0) {
                    DetIceStor(IceNum2).IceFracOnCoil = 0.0;
                } else {
                    // Assume loss term does not impact ice on the coil but what is remaining
                    DetIceStor(IceNum2).IceFracOnCoil += DetIceStor(IceNum2).IceFracChange;
                    // If the ice remaining has run out because of tank losses, reset ice fraction on coil so that it keeps track of losses
                    if (DetIceStor(IceNum2).IceFracOnCoil > DetIceStor(IceNum2).IceFracRemaining)
                        DetIceStor(IceNum2).IceFracOnCoil = DetIceStor(IceNum2).IceFracRemaining;
                }
            } else { // Outside melt system so IceFracOnCoil is always the same as IceFracRemaining (needs to be done for reporting only)
                DetIceStor(IceNum2).IceFracOnCoil = DetIceStor(IceNum2).IceFracRemaining;
            }
        }
    }

    void UpdateDetailedIceStorage(int const iceNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine takes the necessary information from the local data
        // structure and moves it back to the loop node data structure.

        // METHODOLOGY EMPLOYED:
        // Not much mystery here--just move the data to the appropriate place
        // for the detailed ice storage system in question.

        int InNodeNum;  // Plant inlet node number for component
        int OutNodeNum; // Plant outlet node number for component

        // Set the temperature and flow rate for the component outlet node
        InNodeNum = DetIceStor(iceNum).PlantInNodeNum;
        OutNodeNum = DetIceStor(iceNum).PlantOutNodeNum;

        PlantUtilities::SafeCopyPlantNode(InNodeNum, OutNodeNum);

        DataLoopNode::Node(OutNodeNum).Temp = DetIceStor(iceNum).OutletTemp;
    }

    void ReportDetailedIceStorage(int const iceNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports all of the output necessary for the model.

        // METHODOLOGY EMPLOYED:
        // Just take what has already been calculated or calculate the appropriate
        // output value based on simulation data.

        Real64 const LowLoadLimit(0.1); // Load below which device can be assumed off [W]

        if (DetIceStor(iceNum).CompLoad < LowLoadLimit) { // No load condition

            DetIceStor(iceNum).IceFracChange = 0.0;
            DetIceStor(iceNum).DischargingRate = 0.0;
            DetIceStor(iceNum).DischargingEnergy = 0.0;
            DetIceStor(iceNum).ChargingRate = 0.0;
            DetIceStor(iceNum).ChargingEnergy = 0.0;
            DetIceStor(iceNum).ParasiticElecRate = 0.0;
            DetIceStor(iceNum).ParasiticElecEnergy = 0.0;

        } else { // There is a load, determine whether we are charging or discharging based on inlet and outlet temperature

            if (DetIceStor(iceNum).InletTemp < DetIceStor(iceNum).OutletTemp) { // Charging Mode

                DetIceStor(iceNum).ChargingRate = DetIceStor(iceNum).CompLoad;
                DetIceStor(iceNum).ChargingEnergy = DetIceStor(iceNum).CompLoad * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
                DetIceStor(iceNum).IceFracChange = DetIceStor(iceNum).CompLoad * DataHVACGlobals::TimeStepSys / DetIceStor(iceNum).NomCapacity;
                DetIceStor(iceNum).DischargingRate = 0.0;
                DetIceStor(iceNum).DischargingEnergy = 0.0;
                DetIceStor(iceNum).ParasiticElecRate = DetIceStor(iceNum).ChargeParaElecLoad * DetIceStor(iceNum).CompLoad;
                DetIceStor(iceNum).ParasiticElecEnergy = DetIceStor(iceNum).ChargeParaElecLoad * DetIceStor(iceNum).ChargingEnergy;

            } else { // (DetIceStor(IceNum)%InletTemp < DetIceStor(IceNum)%OutletTemp) Discharging Mode

                DetIceStor(iceNum).DischargingRate = DetIceStor(iceNum).CompLoad;
                DetIceStor(iceNum).DischargingEnergy = DetIceStor(iceNum).CompLoad * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
                DetIceStor(iceNum).IceFracChange = -DetIceStor(iceNum).CompLoad * DataHVACGlobals::TimeStepSys / DetIceStor(iceNum).NomCapacity;
                DetIceStor(iceNum).ChargingRate = 0.0;
                DetIceStor(iceNum).ChargingEnergy = 0.0;
                DetIceStor(iceNum).ParasiticElecRate = DetIceStor(iceNum).DischargeParaElecLoad * DetIceStor(iceNum).CompLoad;
                DetIceStor(iceNum).ParasiticElecEnergy = DetIceStor(iceNum).DischargeParaElecLoad * DetIceStor(iceNum).ChargingEnergy;
            }
        }
    }

} // namespace IceThermalStorage

} // namespace EnergyPlus
