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
#include <algorithm>
#include <cmath>
#include <set>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/AirflowNetwork/include/AirflowNetwork/Elements.hpp>
#include <EnergyPlus/AirflowNetwork/include/AirflowNetwork/Solver.hpp>
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RoomAirModelManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneDehumidifier.hh>


namespace EnergyPlus {

namespace AirflowNetworkBalanceManager {

    // MODULE INFORMATION:
    //       AUTHOR         Lixing Gu, Don Shirey, and Muthusamy V. Swami
    //       DATE WRITTEN   July 28, 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module is used to simulate performance of air distribution system with a single HVAC system and a constant
    // volume supply fan.

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using CurveManager::CurveValue;
    using CurveManager::GetCurveIndex;
    using DataAirLoop::AirToZoneNodeInfo;
    using DataContaminantBalance::CO2ZoneTimeMinus1;
    using DataContaminantBalance::Contaminant;
    using DataContaminantBalance::GCZoneTimeMinus1;
    using DataContaminantBalance::OutdoorCO2;
    using DataContaminantBalance::OutdoorGC;
    using DataContaminantBalance::ZoneAirCO2;
    using DataContaminantBalance::ZoneAirGC;
    using DataEnvironment::CurMnDy;
    using DataEnvironment::EnvironmentName;
    using DataEnvironment::OutAirDensity;
    using DataEnvironment::OutBaroPress;
    using DataEnvironment::OutDryBulbTempAt;
    using DataEnvironment::OutEnthalpy;
    using DataEnvironment::OutHumRat;
    using DataEnvironment::StdBaroPress;
    using DataEnvironment::StdRhoAir;
    using DataEnvironment::WindDir;
    using DataEnvironment::WindSpeedAt;
    using DataGlobals::BeginEnvrnFlag;
    using DataGlobals::CurrentTime;
    using DataGlobals::DayOfSim;
    using DataGlobals::DegToRadians;
    using DataGlobals::DisplayExtraWarnings;
    using DataGlobals::NumOfZones;
    using DataGlobals::OutputFileBNDetails;
    using DataGlobals::Pi;
    using DataGlobals::ScheduleAlwaysOn;
    using DataGlobals::SecInHour;
    using DataGlobals::TimeStepZone;
    using DataGlobals::WarmupFlag;
    using DataHeatBalance::TotCrossMixing;
    using DataHeatBalance::TotInfiltration;
    using DataHeatBalance::TotMixing;
    using DataHeatBalance::TotVentilation;
    using DataHeatBalance::TotZoneAirBalance;
    using DataHeatBalance::Zone;
    using DataHeatBalFanSys::MAT;
    using DataHeatBalFanSys::WZoneTimeMinus1;
    using DataHeatBalFanSys::XMAT;
    using DataHeatBalFanSys::ZoneAirHumRat;
    using DataHeatBalFanSys::ZoneAirHumRatAvg;
    using DataHVACGlobals::ContFanCycCoil;
    using DataHVACGlobals::CycFanCycCoil;
    using DataHVACGlobals::FanType_SimpleConstVolume;
    using DataHVACGlobals::FanType_SimpleOnOff;
    using DataHVACGlobals::FanType_SimpleVAV;
    using DataHVACGlobals::FanType_ZoneExhaust;
    using DataHVACGlobals::NumHybridVentSysAvailMgrs;
    using DataHVACGlobals::OnOffFanPartLoadFraction;
    using DataHVACGlobals::SysTimeElapsed;
    using DataLoopNode::Node;
    using DataLoopNode::NodeID;
    using DataLoopNode::NumOfNodes;
    using DataRoomAirModel::AirModel;
    using DataRoomAirModel::AirNode;
    using DataRoomAirModel::RoomAirflowNetworkZoneInfo;
    using DataRoomAirModel::RoomAirModel_AirflowNetwork;
    using DataRoomAirModel::TotNumOfAirNodes;
    using DataSurfaces::cExtBoundCondition;
    using DataSurfaces::ExternalEnvironment;
    using DataSurfaces::OtherSideCoefNoCalcExt;
    using DataSurfaces::Surface;
    using DataSurfaces::SurfaceClass_Door;
    using DataSurfaces::SurfaceClass_GlassDoor;
    using DataSurfaces::SurfaceClass_Window;
    using DataSurfaces::SurfaceWindow;
    using DataSurfaces::TotSurfaces;
    using DataSurfaces::WorldCoordSystem;
    using DataZoneEquipment::ZoneEquipConfig;
    using Fans::GetFanIndex;
    using Fans::GetFanInletNode;
    using Fans::GetFanOutletNode;
    using Fans::GetFanType;
    using Fans::GetFanVolFlow;
    using General::RoundSigDigits;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleIndex;
    using namespace AirflowNetwork;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    int const VentCtrNum_None(0);  // Wrong input
    int const VentCtrNum_Temp(1);  // Temperature venting control
    int const VentCtrNum_Enth(2);  // Enthalpy venting control
    int const VentCtrNum_Const(3); // Constant venting control
    int const VentCtrNum_ASH55(4);
    int const VentCtrNum_CEN15251(5);
    int const VentCtrNum_Novent(6);        // No venting
    int const VentCtrNum_ZoneLevel(7);     // ZoneLevel control for a heat transfer subsurface
    int const VentCtrNum_AdjTemp(8);       // Temperature venting control based on adjacent zone conditions
    int const VentCtrNum_AdjEnth(9);       // Enthalpy venting control based on adjacent zone conditions
    int const FreeOperation(0);            // Free operation
    int const MinCheckForceOpen(1);        // Force open when opening elapsed time is less than minimum opening time
    int const MinCheckForceClose(2);       // Force open when closing elapsed time is less than minimum closing time
    int const ProbNoAction(0);             // No action from probability check
    int const ProbForceChange(1);          // Force open or close from probability check
    int const ProbKeepStatus(2);           // Keep status at the previous time step from probability check
    int const NumOfVentCtrTypes(6);        // Number of zone level venting control types
    int const EquivRec_Height(1);          // Effective rectangle polygonal height selection
    int const EquivRec_BaseAspectRatio(2); // Effective rectangle base surface aspect ratio selection
    int const EquivRec_UserAspectRatio(3); // Effective rectangle user input aspect ratio selection
    static std::string const BlankString;

    // DERIVED TYPE DEFINITIONS:
    // Report variables

    // MODULE VARIABLE DECLARATIONS:

    namespace {
        // These are purposefully not in the header file as an extern variable. No one outside of this should
        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
        bool ValidateDistributionSystemFlag(true);
    } // namespace

    // Report variables
    Array1D<Real64> PZ;
    // Inverse matrix
    Array1D<Real64> MA;
    Array1D<Real64> MV;
    Array1D_int IVEC;
    Array1D_int SplitterNodeNumbers;

    bool AirflowNetworkGetInputFlag(true);
    int VentilationCtrl(0);  // Hybrid ventilation control type
    int NumOfExhaustFans(0); // Number of exhaust fans

    int NumAirflowNetwork(0);
    int AirflowNetworkNumOfDetOpenings(0);
    int AirflowNetworkNumOfSimOpenings(0);
    int AirflowNetworkNumOfHorOpenings(0);
    int AirflowNetworkNumOfSurCracks(0);
    int AirflowNetworkNumOfSurELA(0);
    int AirflowNetworkNumOfExtNode(0);
    int AirflowNetworkNumOfOutAirNode(0);
    int AirflowNetworkNumOfSingleSideZones; // Total number of zones with advanced single sided wind pressure coefficient calculation
    int DisSysNumOfNodes(0);
    int DisSysNumOfLeaks(0);
    int DisSysNumOfELRs(0);
    int DisSysNumOfDucts(0);
    int DisSysNumOfDuctViewFactors(0);
    int DisSysNumOfDampers(0);
    int DisSysNumOfCVFs(0);
    int DisSysNumOfDetFans(0);
    int DisSysNumOfCoils(0);
    int DisSysNumOfHXs(0);
    int DisSysNumOfCPDs(0);
    int DisSysNumOfTermUnits(0);
    int DisSysNumOfLinks(0);
    int NumOfExtNodes(0);
    int AirflowNetworkNumOfExtSurfaces(0);
    Real64 IncAng(0.0);                     // Wind incidence angle relative to facade normal (deg)
    Array1D<Real64> FacadeAng(5);           // Facade azimuth angle (for walls, angle of outward normal to facade measured clockwise from North) (deg)
    int WindDirNum;                         // Wind direction number
    Real64 WindAng;                         // Wind direction angle (degrees clockwise from North)
    int SupplyFanInletNode(0);              // Supply air fan inlet node number
    int SupplyFanOutletNode(0);             // Supply air fan outlet node number
    int SupplyFanType(0);                   // Supply air fan type
    Real64 OnOffFanRunTimeFraction(0.0);    // Run time fraction for an On/Off fan flow rate
    Real64 MaxOnOffFanRunTimeFraction(0.0); // max Run time fraction for an On/Off fan flow rate among airloops
    Real64 CurrentEndTime(0.0);             // Current end time
    Real64 CurrentEndTimeLast(0.0);         // last end time
    Real64 TimeStepSysLast(0.0);            // last system time step
    int AirflowNetworkNumOfOccuVentCtrls(0);
    int IntraZoneNumOfNodes(0);
    int IntraZoneNumOfLinks(0);
    int IntraZoneNumOfZones(0);

    int NumOfPressureControllers(0); // number of pressure controllers
    int NumOfOAFans(0);              // number of OutdoorAir fans
    int NumOfReliefFans(0);          // number of OutdoorAir relief fans

    Array1D<Real64> LoopPartLoadRatio;
    Array1D<Real64> LoopOnOffFanRunTimeFraction;
    Array1D<bool> LoopOnOffFlag;

    // SUBROUTINE SPECIFICATIONS FOR MODULE AirflowNetworkBalanceManager:
    // Name Public routines, optionally name Private routines within this module

    // Object Data
    Array1D<AirflowNetworkReportVars> AirflowNetworkZnRpt;
    std::unordered_map<std::string, std::string> UniqueAirflowNetworkSurfaceName;

    Array1D<OccupantVentilationControlProp> OccupantVentilationControl;

    // Functions

    void clear_state()
    {
        PZ.deallocate();
        MA.deallocate();
        MV.deallocate();
        IVEC.deallocate();
        SplitterNodeNumbers.deallocate();
        AirflowNetworkGetInputFlag = true;
        ValidateDistributionSystemFlag = true;
        VentilationCtrl = 0;
        NumOfExhaustFans = 0;
        NumAirflowNetwork = 0;
        AirflowNetworkNumOfDetOpenings = 0;
        AirflowNetworkNumOfSimOpenings = 0;
        AirflowNetworkNumOfHorOpenings = 0;
        AirflowNetworkNumOfSurCracks = 0;
        AirflowNetworkNumOfSurELA = 0;
        AirflowNetworkNumOfExtNode = 0;
        AirflowNetworkNumOfOutAirNode = 0;
        AirflowNetworkNumOfSingleSideZones = 0; // added default value
        DisSysNumOfNodes = 0;
        DisSysNumOfLeaks = 0;
        DisSysNumOfELRs = 0;
        DisSysNumOfDucts = 0;
        DisSysNumOfDuctViewFactors = 0;
        DisSysNumOfDampers = 0;
        DisSysNumOfCVFs = 0;
        DisSysNumOfDetFans = 0;
        DisSysNumOfCoils = 0;
        DisSysNumOfHXs = 0;
        DisSysNumOfCPDs = 0;
        DisSysNumOfTermUnits = 0;
        DisSysNumOfLinks = 0;
        NumOfExtNodes = 0;
        AirflowNetworkNumOfExtSurfaces = 0;
        IncAng = 0.0;
        FacadeAng = Array1D<Real64>(5);
        WindDirNum = 0; // added default value
        WindAng = 0;    // added default value
        SupplyFanInletNode = 0;
        SupplyFanOutletNode = 0;
        SupplyFanType = 0;
        OnOffFanRunTimeFraction = 0.0;
        CurrentEndTime = 0.0;
        CurrentEndTimeLast = 0.0;
        TimeStepSysLast = 0.0;
        AirflowNetworkNumOfOccuVentCtrls = 0;
        IntraZoneNumOfNodes = 0;
        IntraZoneNumOfLinks = 0;
        IntraZoneNumOfZones = 0;
        AirflowNetworkZnRpt.deallocate();
        OccupantVentilationControl.deallocate();
        LoopPartLoadRatio.deallocate();
        LoopOnOffFanRunTimeFraction.deallocate();
        LoopOnOffFlag.deallocate();
        UniqueAirflowNetworkSurfaceName.clear();
    }

    void ManageAirflowNetworkBalance(Optional_bool_const FirstHVACIteration, // True when solution technique on first iteration
                                     Optional_int_const Iter,                // Iteration number
                                     Optional_bool ResimulateAirZone         // True when solution technique on third iteration
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   July 28, 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs simulation of air distribution system.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataAirLoop::AirLoopAFNInfo;
        using DataAirSystems::PrimaryAirSystem;
        using DataHVACGlobals::TurnFansOn;
        using DataHVACGlobals::VerySmallMassFlow;

        // Locals
        int i;
        int AFNSupplyFanType = 0;

        if (AirflowNetworkGetInputFlag) {
            GetAirflowNetworkInput(OutputFiles::getSingleton());
            AirflowNetworkGetInputFlag = false;
            return;
        }

        if (present(ResimulateAirZone)) {
            ResimulateAirZone = false;
        }

        if (SimulateAirflowNetwork < AirflowNetworkControlMultizone) return;

        if (BeginEnvrnFlag) {
            TurnFansOn = false; // The FAN should be off when BeginEnvrnFlag = .True.
        }

        InitAirflowNetwork();

        NetworkNumOfNodes = NumOfNodesMultiZone;
        NetworkNumOfLinks = NumOfLinksMultiZone;

        AirflowNetworkFanActivated = false;

        if (present(FirstHVACIteration) && SimulateAirflowNetwork >= AirflowNetworkControlSimpleADS) {
            if (FirstHVACIteration) {
                if (allocated(AirLoopAFNInfo)) {
                    for (i = 1; i <= DisSysNumOfCVFs; i++) {
                        AirLoopAFNInfo(i).AFNLoopHeatingCoilMaxRTF = 0.0;
                        AirLoopAFNInfo(i).AFNLoopOnOffFanRTF = 0.0;
                        AirLoopAFNInfo(i).AFNLoopDXCoilRTF = 0.0;
                        AirLoopAFNInfo(i).LoopOnOffFanPartLoadRatio = 0.0;
                    }
                }
            }
            Real64 FanMassFlowRate = 0.0;
            int FanOperModeCyc = 0;
            AFNSupplyFanType = 0;

            for (i = 1; i <= DisSysNumOfCVFs; i++) {
                AFNSupplyFanType = DisSysCompCVFData(i).FanTypeNum;
                FanMassFlowRate = max(FanMassFlowRate, Node(DisSysCompCVFData(i).OutletNode).MassFlowRate);
                // VAV take high priority
                if (DisSysCompCVFData(i).FanTypeNum == FanType_SimpleVAV) {
                    AFNSupplyFanType = DisSysCompCVFData(i).FanTypeNum;
                    break;
                }
                if (FanMassFlowRate > VerySmallMassFlow && AirLoopAFNInfo(i).LoopFanOperationMode == CycFanCycCoil &&
                    AirLoopAFNInfo(i).LoopSystemOnMassFlowrate > 0.0) {
                    FanOperModeCyc = CycFanCycCoil;
                    AFNSupplyFanType = DisSysCompCVFData(i).FanTypeNum;
                    if (AFNSupplyFanType == FanType_SimpleOnOff) {
                        break;
                    }
                }
            }
            //			Revised to meet heat exchanger requirement
            if ((FanMassFlowRate > VerySmallMassFlow) && (!FirstHVACIteration)) {
                if (AFNSupplyFanType == FanType_SimpleOnOff && FanOperModeCyc == CycFanCycCoil) {
                    AirflowNetworkFanActivated = true;
                } else if (AFNSupplyFanType == FanType_SimpleVAV) {
                    if (present(Iter) && Iter > 1) AirflowNetworkFanActivated = true;
                } else if (AirflowNetworkUnitarySystem) {
                    if (present(Iter) && Iter > 1) AirflowNetworkFanActivated = true;
                } else {
                    AirflowNetworkFanActivated = true;
                }
            }
        }
        if (allocated(ZoneEquipConfig) && NumHybridVentSysAvailMgrs > 0 && allocated(PrimaryAirSystem)) HybridVentilationControl();
        if (VentilationCtrl == 1 && NumHybridVentSysAvailMgrs > 0) AirflowNetworkFanActivated = false;

        if (present(Iter) && present(ResimulateAirZone) && SimulateAirflowNetwork >= AirflowNetworkControlSimpleADS) {
            if (AirflowNetworkFanActivated && Iter < 3 && AFNSupplyFanType == FanType_SimpleOnOff) {
                ResimulateAirZone = true;
            }
            if (AFNSupplyFanType == FanType_SimpleVAV) {
                if (!AirflowNetworkFanActivated && Iter < 3) ResimulateAirZone = true;
            }
            if (AirflowNetworkUnitarySystem) {
                if (!AirflowNetworkFanActivated && Iter < 3) ResimulateAirZone = true;
            }
        }
        if (AirflowNetworkFanActivated && SimulateAirflowNetwork > AirflowNetworkControlMultizone) {
            NetworkNumOfNodes = AirflowNetworkNumOfNodes;
            NetworkNumOfLinks = AirflowNetworkNumOfLinks;
        }

        if (allocated(ZoneEquipConfig)) ValidateExhaustFanInput();

        // VAV terminal set only
        if (present(FirstHVACIteration) && FirstHVACIteration) VAVTerminalRatio = 0.0;

        if (AirflowNetworkFanActivated && SimulateAirflowNetwork > AirflowNetworkControlMultizone) {
            if (ValidateDistributionSystemFlag) {
                ValidateDistributionSystem();
                ValidateFanFlowRate();
                ValidateDistributionSystemFlag = false;
            }
        }
        CalcAirflowNetworkAirBalance();

        if (AirflowNetworkFanActivated && SimulateAirflowNetwork > AirflowNetworkControlMultizone) {

            LoopOnOffFlag = false;
            for (i = 1; i <= DisSysNumOfCVFs; i++) {
                if (DisSysCompCVFData(i).AirLoopNum > 0) {
                    if (Node(DisSysCompCVFData(i).InletNode).MassFlowRate > 0.0) {
                        LoopOnOffFlag(DisSysCompCVFData(i).AirLoopNum) = true;
                    }
                }
            }

            CalcAirflowNetworkHeatBalance();
            CalcAirflowNetworkMoisBalance();
            if (Contaminant.CO2Simulation) CalcAirflowNetworkCO2Balance();
            if (Contaminant.GenericContamSimulation) CalcAirflowNetworkGCBalance();
        }

        UpdateAirflowNetwork(FirstHVACIteration);
    }

    static bool getAirflowElementInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason DeGraw
        //       DATE WRITTEN   Oct. 2018
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reads airflow element inputs (eventually)

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        static std::string const RoutineName{"getAirflowElementInput"};
        std::string CurrentModuleObject;
        bool success{true};

        // *** Read AirflowNetwork simulation reference crack conditions
        std::unordered_map<std::string, ReferenceConditions> referenceConditions; // Map for lookups
        ReferenceConditions defaultReferenceConditions("Default");                // Defaulted conditions
        bool conditionsAreDefaulted(true);                                        // Conditions are defaulted?
        CurrentModuleObject = "AirflowNetwork:MultiZone:ReferenceCrackConditions";
        auto instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            // globalSolverObject.referenceConditions.clear();
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());

                Real64 temperature{fields.at("reference_temperature")};
                Real64 pressure(101325.0);
                if (fields.find("reference_barometric_pressure") != fields.end()) { // not required field, has default value
                    pressure = fields.at("reference_barometric_pressure");
                    if (std::abs((pressure - StdBaroPress) / StdBaroPress) > 0.1) { // 10% off
                        ShowWarningError(RoutineName + ": " + CurrentModuleObject + ": Pressure = " + RoundSigDigits(pressure, 0) +
                                         " differs by more than 10% from Standard Barometric Pressure = " + RoundSigDigits(StdBaroPress, 0) + '.');
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + thisObjectName);
                    }
                    if (pressure <= 31000.0) {
                        ShowSevereError(RoutineName + ": " + CurrentModuleObject + ": " + thisObjectName +
                                        ". Reference Barometric Pressure must be greater than 31000 Pa.");
                        success = false;
                    }
                }
                Real64 humidity{fields.at("reference_humidity_ratio")};
                // globalSolverObject.referenceConditions.emplace_back(thisObjectName, temperature, pressure, humidity);
                referenceConditions.emplace(std::piecewise_construct,
                                            std::forward_as_tuple(thisObjectName),
                                            std::forward_as_tuple(instance.key(), temperature, pressure, humidity));
            }
            // Check that there is more than one
            if (referenceConditions.size() == 1) {
                inputProcessor->markObjectAsUsed("AirflowNetwork:MultiZone:ReferenceCrackConditions", referenceConditions.begin()->second.name);
                defaultReferenceConditions = referenceConditions.begin()->second;

            } else {
                conditionsAreDefaulted = false;
            }
        }
        if (!success) {
            return false;
        }

        // *** Read AirflowNetwork simulation surface crack component
        CurrentModuleObject = "AirflowNetwork:MultiZone:Surface:Crack";
        AirflowNetworkNumOfSurCracks = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                                        // Temporary workaround
            MultizoneSurfaceCrackData.allocate(AirflowNetworkNumOfSurCracks); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient_at_reference_conditions")}; // Required field
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent") != fields.end()) { // not required field, has default value
                    expnt = fields.at("air_mass_flow_exponent");
                }
                Real64 refT = defaultReferenceConditions.temperature;
                Real64 refP = defaultReferenceConditions.pressure;
                Real64 refW = defaultReferenceConditions.humidityRatio;
                if (!conditionsAreDefaulted) {
                    if (fields.find("reference_crack_conditions") != fields.end()) { // not required field, *should* have default value
                        auto result = referenceConditions.find(fields.at("reference_crack_conditions"));
                        if (result == referenceConditions.end()) {
                            ShowSevereError(RoutineName + CurrentModuleObject + ": " + thisObjectName +
                                            ". Cannot find reference crack conditions object \"" +
                                            fields.at("reference_crack_conditions").get<std::string>() + "\".");
                            success = false;
                        } else {
                            refT = result->second.temperature;
                            refP = result->second.pressure;
                            refW = result->second.humidityRatio;
                            inputProcessor->markObjectAsUsed("AirflowNetwork:MultiZone:ReferenceCrackConditions", result->second.name);
                        }
                    }
                }
                // globalSolverObject.cracks[thisObjectName] = SurfaceCrack(coeff, expnt, refT, refP, refW);
                MultizoneSurfaceCrackData(i).Name = thisObjectName; // Name of surface crack component
                MultizoneSurfaceCrackData(i).FlowCoef = coeff;      // Air Mass Flow Coefficient
                MultizoneSurfaceCrackData(i).FlowExpo = expnt;      // Air Mass Flow exponent
                MultizoneSurfaceCrackData(i).StandardT = refT;
                MultizoneSurfaceCrackData(i).StandardP = refP;
                MultizoneSurfaceCrackData(i).StandardW = refW;
                ++i;
            }
        }

        // *** Read AirflowNetwork simulation zone exhaust fan component
        CurrentModuleObject = "AirflowNetwork:MultiZone:Component:ZoneExhaustFan";
        AirflowNetworkNumOfExhFan = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        NumOfExhaustFans = inputProcessor->getNumObjectsFound("Fan:ZoneExhaust");            // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                                       // Temporary workaround
            MultizoneCompExhaustFanData.allocate(AirflowNetworkNumOfExhFan); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient_when_the_zone_exhaust_fan_is_off_at_reference_conditions")}; // Required field
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_the_zone_exhaust_fan_is_off") != fields.end()) { // not required field, has default value
                    expnt = fields.at("air_mass_flow_exponent_when_the_zone_exhaust_fan_is_off");
                }

                // This breaks the component model, need to fix
                bool fanErrorFound = false;
                int fanIndex;
                GetFanIndex(thisObjectName, fanIndex, fanErrorFound);
                if (fanErrorFound) {
                    ShowSevereError(RoutineName + ": " + CurrentModuleObject + " = " + thisObjectName + " is not found in Fan:ZoneExhaust objects.");
                    success = false;
                }
                Real64 flowRate;

                GetFanVolFlow(fanIndex, flowRate);
                flowRate *= StdRhoAir;
                bool nodeErrorsFound{false};
                int inletNode = GetFanInletNode("Fan:ZoneExhaust", thisObjectName, nodeErrorsFound);
                int outletNode = GetFanOutletNode("Fan:ZoneExhaust", thisObjectName, nodeErrorsFound);
                if (nodeErrorsFound) {
                    success = false;
                }
                int fanType_Num;
                GetFanType(thisObjectName, fanType_Num, fanErrorFound);
                if (fanType_Num != FanType_ZoneExhaust) {
                    ShowSevereError(RoutineName + CurrentModuleObject + " = " + thisObjectName + ". The specified " + "Name" +
                                    " is not found as a valid Fan:ZoneExhaust object.");
                    success = false;
                }

                Real64 refT = defaultReferenceConditions.temperature;
                Real64 refP = defaultReferenceConditions.pressure;
                Real64 refW = defaultReferenceConditions.humidityRatio;
                if (!conditionsAreDefaulted) {
                    if (fields.find("reference_crack_conditions") != fields.end()) { // not required field, *should* have default value
                        auto result = referenceConditions.find(fields.at("reference_crack_conditions"));
                        if (result == referenceConditions.end()) {
                            ShowSevereError(RoutineName + CurrentModuleObject + ": " + thisObjectName +
                                            ". Cannot find reference crack conditions object \"" +
                                            fields.at("reference_crack_conditions").get<std::string>() + "\".");
                            success = false;
                        } else {
                            refT = result->second.temperature;
                            refP = result->second.pressure;
                            refW = result->second.humidityRatio;
                            inputProcessor->markObjectAsUsed("AirflowNetwork:MultiZone:ReferenceCrackConditions", result->second.name);
                        }
                    }
                }
                // auto fanpair = globalSolverObject.zoneExhaustFans.emplace(
                //    std::piecewise_construct, std::forward_as_tuple(thisObjectName), std::forward_as_tuple(coeff, expnt, refT, refP, refW));
                // if (!fanpair.second) {
                // Duplicate name, which can't really happen since the fan name is the key
                //}
                // fanpair.first->second.FlowRate = flowRate;
                // fanpair.first->second.InletNode = inletNode;
                // fanpair.first->second.OutletNode = outletNode;

                MultizoneCompExhaustFanData(i).Name = thisObjectName; // Name of zone exhaust fan component
                MultizoneCompExhaustFanData(i).FlowCoef = coeff;      // flow coefficient
                MultizoneCompExhaustFanData(i).FlowExpo = expnt;      // Flow exponent

                MultizoneCompExhaustFanData(i).FlowRate = flowRate;
                MultizoneCompExhaustFanData(i).InletNode = inletNode;
                MultizoneCompExhaustFanData(i).OutletNode = outletNode;

                MultizoneCompExhaustFanData(i).StandardT = refT;
                MultizoneCompExhaustFanData(i).StandardP = refP;
                MultizoneCompExhaustFanData(i).StandardW = refW;
                ++i;
            }
        }

        // Read Outdoor Airflow object
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:OutdoorAirFlow";
        NumOfOAFans = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                      // Temporary workaround
            DisSysCompOutdoorAirData.allocate(NumOfOAFans); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string mixer_name = UtilityRoutines::MakeUPPERCase(fields.at("outdoor_air_mixer_name"));
                Real64 coeff{fields.at("air_mass_flow_coefficient_when_no_outdoor_air_flow_at_reference_conditions")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_no_outdoor_air_flow") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent_when_no_outdoor_air_flow");
                }

                int OAMixerNum = MixedAir::GetOAMixerNumber(mixer_name);
                if (OAMixerNum == 0) {
                    ShowSevereError(RoutineName + ": " + CurrentModuleObject + " object " + thisObjectName + ". Invalid " + "Outdoor Air Mixer Name" +
                                    " \"" + mixer_name + "\" given.");
                    success = false;
                }

                Real64 refT = defaultReferenceConditions.temperature;
                Real64 refP = defaultReferenceConditions.pressure;
                Real64 refW = defaultReferenceConditions.humidityRatio;
                if (!conditionsAreDefaulted) {
                    if (fields.find("reference_crack_conditions") != fields.end()) { // not required field, *should* have default value
                        auto result = referenceConditions.find(fields.at("reference_crack_conditions"));
                        if (result == referenceConditions.end()) {
                            ShowSevereError(RoutineName + CurrentModuleObject + ": " + thisObjectName +
                                            ". Cannot find reference crack conditions object \"" +
                                            fields.at("reference_crack_conditions").get<std::string>() + "\".");
                            success = false;
                        } else {
                            refT = result->second.temperature;
                            refP = result->second.pressure;
                            refW = result->second.humidityRatio;
                            inputProcessor->markObjectAsUsed("AirflowNetwork:MultiZone:ReferenceCrackConditions", result->second.name);
                        }
                    }
                }

                DisSysCompOutdoorAirData(i).Name = thisObjectName; // Name of zone exhaust fan component
                DisSysCompOutdoorAirData(i).FlowCoef = coeff;      // flow coefficient
                DisSysCompOutdoorAirData(i).FlowExpo = expnt;      // Flow exponent

                DisSysCompOutdoorAirData(i).OAMixerNum = OAMixerNum;

                DisSysCompOutdoorAirData(i).StandardT = refT;
                DisSysCompOutdoorAirData(i).StandardP = refP;
                DisSysCompOutdoorAirData(i).StandardW = refW;
                ++i;
            }
        }

        // Read Relief Airflow object
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:ReliefAirFlow";
        NumOfReliefFans = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                         // Temporary workaround
            DisSysCompReliefAirData.allocate(NumOfReliefFans); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string mixer_name = UtilityRoutines::MakeUPPERCase(fields.at("outdoor_air_mixer_name"));
                Real64 coeff{fields.at("air_mass_flow_coefficient_when_no_outdoor_air_flow_at_reference_conditions")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_no_outdoor_air_flow") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent_when_no_outdoor_air_flow");
                }

                int OAMixerNum{MixedAir::GetOAMixerNumber(mixer_name)};
                if (OAMixerNum == 0) {
                    ShowSevereError(RoutineName + ": " + CurrentModuleObject + " object " + thisObjectName + ". Invalid " + "Outdoor Air Mixer Name" +
                                    " \"" + mixer_name + "\" given.");
                    success = false;
                }

                Real64 refT = defaultReferenceConditions.temperature;
                Real64 refP = defaultReferenceConditions.pressure;
                Real64 refW = defaultReferenceConditions.humidityRatio;
                if (!conditionsAreDefaulted) {
                    if (fields.find("reference_crack_conditions") != fields.end()) { // not required field, *should* have default value
                        auto result = referenceConditions.find(fields.at("reference_crack_conditions"));
                        if (result == referenceConditions.end()) {
                            ShowSevereError(RoutineName + CurrentModuleObject + ": " + thisObjectName +
                                            ". Cannot find reference crack conditions object \"" +
                                            fields.at("reference_crack_conditions").get<std::string>() + "\".");
                            success = false;
                        } else {
                            refT = result->second.temperature;
                            refP = result->second.pressure;
                            refW = result->second.humidityRatio;
                            inputProcessor->markObjectAsUsed("AirflowNetwork:MultiZone:ReferenceCrackConditions", result->second.name);
                        }
                    }
                }

                DisSysCompReliefAirData(i).Name = thisObjectName; // Name of zone exhaust fan component
                DisSysCompReliefAirData(i).FlowCoef = coeff;      // flow coefficient
                DisSysCompReliefAirData(i).FlowExpo = expnt;      // Flow exponent
                DisSysCompReliefAirData(i).OAMixerNum = OAMixerNum;
                DisSysCompReliefAirData(i).StandardT = refT;
                DisSysCompReliefAirData(i).StandardP = refP;
                DisSysCompReliefAirData(i).StandardW = refW;
                ++i;
            }
        }

        // Read AirflowNetwork simulation detailed openings
        CurrentModuleObject = "AirflowNetwork:MultiZone:Component:DetailedOpening";
        AirflowNetworkNumOfDetOpenings = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                                            // Temporary workaround
            MultizoneCompDetOpeningData.allocate(AirflowNetworkNumOfDetOpenings); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient_when_opening_is_closed")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_opening_is_closed") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent_when_opening_is_closed");
                }

                int LVOtype{1};
                std::string LVOstring;
                if (fields.find("type_of_rectangular_large_vertical_opening_lvo_") != fields.end()) {
                    LVOstring = fields.at("type_of_rectangular_large_vertical_opening_lvo_").get<std::string>();
                    if (UtilityRoutines::SameString(LVOstring, "NonPivoted") || UtilityRoutines::SameString(LVOstring, "1")) {
                        LVOtype = 1; // Large vertical opening type number
                    } else if (UtilityRoutines::SameString(LVOstring, "HorizontallyPivoted") || UtilityRoutines::SameString(LVOstring, "2")) {
                        LVOtype = 2; // Large vertical opening type number
                    } else {
                        ShowSevereError(RoutineName + "Invalid Type of Rectangular Large Vertical Opening (LVO) = " + LVOstring + "in " +
                                        CurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("Valid choices are NonPivoted and HorizontallyPivoted.");
                        success = false;
                    }
                }

                Real64 extra{0.0};
                if (fields.find("extra_crack_length_or_height_of_pivoting_axis") != fields.end()) {
                    extra = fields.at("extra_crack_length_or_height_of_pivoting_axis");
                }

                Real64 N{fields.at("number_of_sets_of_opening_factor_data")};

                std::vector<Real64> factors(N);
                std::vector<Real64> cds(N);
                std::vector<Real64> width_factors(N);
                std::vector<Real64> height_factors(N);
                std::vector<Real64> start_height_factors(N);

                // Real64 factor{0.0};
                // if (fields.find("opening_factor_1") != fields.end()) {
                //    factor = fields.at("opening_factor_1");
                //}
                Real64 cd{0.001};
                if (fields.find("discharge_coefficient_for_opening_factor_1") != fields.end()) {
                    cd = fields.at("discharge_coefficient_for_opening_factor_1");
                }
                Real64 width_factor{0.0};
                if (fields.find("width_factor_for_opening_factor_1") != fields.end()) {
                    width_factor = fields.at("width_factor_for_opening_factor_1");
                }
                Real64 height_factor{0.0};
                if (fields.find("height_factor_for_opening_factor_1") != fields.end()) {
                    height_factor = fields.at("height_factor_for_opening_factor_1");
                }
                Real64 start_height_factor{0.0};
                if (fields.find("start_height_factor_for_opening_factor_1") != fields.end()) {
                    start_height_factor = fields.at("start_height_factor_for_opening_factor_1");
                }

                factors[0] = 0.0; // factor; // This factor must be zero
                cds[0] = cd;
                width_factors[0] = width_factor;
                height_factors[0] = height_factor;
                start_height_factors[0] = start_height_factor;

                Real64 factor{fields.at("opening_factor_2")};
                cd = 1.0;
                if (fields.find("discharge_coefficient_for_opening_factor_2") != fields.end()) {
                    cd = fields.at("discharge_coefficient_for_opening_factor_2");
                }
                width_factor = 1.0;
                if (fields.find("width_factor_for_opening_factor_2") != fields.end()) {
                    width_factor = fields.at("width_factor_for_opening_factor_2");
                }
                height_factor = 1.0;
                if (fields.find("height_factor_for_opening_factor_2") != fields.end()) {
                    height_factor = fields.at("height_factor_for_opening_factor_2");
                }
                start_height_factor = 0.0;
                if (fields.find("start_height_factor_for_opening_factor_2") != fields.end()) {
                    start_height_factor = fields.at("start_height_factor_for_opening_factor_2");
                }

                factors[1] = factor;
                cds[1] = cd;
                width_factors[1] = width_factor;
                height_factors[1] = height_factor;
                start_height_factors[1] = start_height_factor;

                if (N >= 3) {
                    factor = fields.at("opening_factor_3");
                    cd = 0.0;
                    if (fields.find("discharge_coefficient_for_opening_factor_3") != fields.end()) {
                        cd = fields.at("discharge_coefficient_for_opening_factor_3");
                    }
                    width_factor = 0.0;
                    if (fields.find("width_factor_for_opening_factor_3") != fields.end()) {
                        width_factor = fields.at("width_factor_for_opening_factor_3");
                    }
                    height_factor = 0.0;
                    if (fields.find("height_factor_for_opening_factor_3") != fields.end()) {
                        height_factor = fields.at("height_factor_for_opening_factor_3");
                    }
                    start_height_factor = 0.0;
                    if (fields.find("start_height_factor_for_opening_factor_3") != fields.end()) {
                        start_height_factor = fields.at("start_height_factor_for_opening_factor_3");
                    }

                    factors[2] = factor;
                    cds[2] = cd;
                    width_factors[2] = width_factor;
                    height_factors[2] = height_factor;
                    start_height_factors[2] = start_height_factor;

                    if (N >= 4) {
                        factor = fields.at("opening_factor_4");
                        cd = 0.0;
                        if (fields.find("discharge_coefficient_for_opening_factor_4") != fields.end()) {
                            cd = fields.at("discharge_coefficient_for_opening_factor_4");
                        }
                        width_factor = 0.0;
                        if (fields.find("width_factor_for_opening_factor_4") != fields.end()) {
                            width_factor = fields.at("width_factor_for_opening_factor_4");
                        }
                        height_factor = 0.0;
                        if (fields.find("height_factor_for_opening_factor_4") != fields.end()) {
                            height_factor = fields.at("height_factor_for_opening_factor_4");
                        }
                        start_height_factor = 0.0;
                        if (fields.find("start_height_factor_for_opening_factor_4") != fields.end()) {
                            start_height_factor = fields.at("start_height_factor_for_opening_factor_4");
                        }

                        factors[3] = factor;
                        cds[3] = cd;
                        width_factors[3] = width_factor;
                        height_factors[3] = height_factor;
                        start_height_factors[3] = start_height_factor;
                    }
                }

                MultizoneCompDetOpeningData(i).Name = thisObjectName; // Name of large detailed opening component
                MultizoneCompDetOpeningData(i).FlowCoef = coeff;      // Air Mass Flow Coefficient When Window or Door Is Closed
                MultizoneCompDetOpeningData(i).FlowExpo = expnt;      // Air Mass Flow exponent When Window or Door Is Closed
                MultizoneCompDetOpeningData(i).TypeName = LVOstring;  // Large vertical opening type
                MultizoneCompDetOpeningData(i).LVOType = LVOtype;     // Large vertical opening type number
                MultizoneCompDetOpeningData(i).LVOValue = extra;      // Extra crack length for LVO type 1 with multiple openable parts,
                                                                      // or Height of pivoting axis for LVO type 2

                MultizoneCompDetOpeningData(i).NumFac = N; // Number of Opening Factor Values

                MultizoneCompDetOpeningData(i).OpenFac1 = factors[0];                // Opening factor #1
                MultizoneCompDetOpeningData(i).DischCoeff1 = cds[0];                 // Discharge coefficient for opening factor #1
                MultizoneCompDetOpeningData(i).WidthFac1 = width_factors[0];         // Width factor for for Opening factor #1
                MultizoneCompDetOpeningData(i).HeightFac1 = height_factors[0];       // Height factor for opening factor #1
                MultizoneCompDetOpeningData(i).StartHFac1 = start_height_factors[0]; // Start height factor for opening factor #1
                MultizoneCompDetOpeningData(i).OpenFac2 = factors[1];                // Opening factor #2
                MultizoneCompDetOpeningData(i).DischCoeff2 = cds[1];                 // Discharge coefficient for opening factor #2
                MultizoneCompDetOpeningData(i).WidthFac2 = width_factors[1];         // Width factor for for Opening factor #2
                MultizoneCompDetOpeningData(i).HeightFac2 = height_factors[1];       // Height factor for opening factor #2
                MultizoneCompDetOpeningData(i).StartHFac2 = start_height_factors[1]; // Start height factor for opening factor #2

                MultizoneCompDetOpeningData(i).OpenFac3 = 0.0;    // Opening factor #3
                MultizoneCompDetOpeningData(i).DischCoeff3 = 0.0; // Discharge coefficient for opening factor #3
                MultizoneCompDetOpeningData(i).WidthFac3 = 0.0;   // Width factor for for Opening factor #3
                MultizoneCompDetOpeningData(i).HeightFac3 = 0.0;  // Height factor for opening factor #3
                MultizoneCompDetOpeningData(i).StartHFac3 = 0.0;  // Start height factor for opening factor #3
                MultizoneCompDetOpeningData(i).OpenFac4 = 0.0;    // Opening factor #4
                MultizoneCompDetOpeningData(i).DischCoeff4 = 0.0; // Discharge coefficient for opening factor #4
                MultizoneCompDetOpeningData(i).WidthFac4 = 0.0;   // Width factor for for Opening factor #4
                MultizoneCompDetOpeningData(i).HeightFac4 = 0.0;  // Height factor for opening factor #4
                MultizoneCompDetOpeningData(i).StartHFac4 = 0.0;  // Start height factor for opening factor #4
                if (N == 2) {
                    if (factors[1] != 1.0) {
                        ShowWarningError(RoutineName + ": " + CurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError(
                            "..This object specifies that only 3 opening factors will be used. So, the value of Opening Factor #2 is set to 1.0.");
                        ShowContinueError("..Input value was " + RoundSigDigits(MultizoneCompDetOpeningData(i).OpenFac2, 2));
                        MultizoneCompDetOpeningData(i).OpenFac2 = 1.0;
                    }
                } else if (N >= 3) {
                    MultizoneCompDetOpeningData(i).OpenFac3 = factors[2];                // Opening factor #3
                    MultizoneCompDetOpeningData(i).DischCoeff3 = cds[2];                 // Discharge coefficient for opening factor #3
                    MultizoneCompDetOpeningData(i).WidthFac3 = width_factors[2];         // Width factor for for Opening factor #3
                    MultizoneCompDetOpeningData(i).HeightFac3 = height_factors[2];       // Height factor for opening factor #3
                    MultizoneCompDetOpeningData(i).StartHFac3 = start_height_factors[2]; // Start height factor for opening factor #3
                    if (N >= 4) {
                        MultizoneCompDetOpeningData(i).OpenFac4 = factors[3]; // Opening factor #4
                        if (factors[3] != 1.0) {
                            ShowWarningError(RoutineName + ": " + CurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("..This object specifies that 4 opening factors will be used. So, the value of Opening Factor #4 "
                                              "is set to 1.0.");
                            ShowContinueError("..Input value was " + RoundSigDigits(MultizoneCompDetOpeningData(i).OpenFac4, 2));
                            MultizoneCompDetOpeningData(i).OpenFac4 = 1.0;
                        }
                        MultizoneCompDetOpeningData(i).DischCoeff4 = cds[3];                 // Discharge coefficient for opening factor #4
                        MultizoneCompDetOpeningData(i).WidthFac4 = width_factors[3];         // Width factor for for Opening factor #4
                        MultizoneCompDetOpeningData(i).HeightFac4 = height_factors[3];       // Height factor for opening factor #4
                        MultizoneCompDetOpeningData(i).StartHFac4 = start_height_factors[3]; // Start height factor for opening factor #4
                    } else {
                        if (factors[2] != 1.0) {
                            ShowWarningError(RoutineName + ": " + CurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("..This object specifies that only 3 opening factors will be used. So, the value of Opening Factor #3 "
                                              "is set to 1.0.");
                            ShowContinueError("..Input value was " + RoundSigDigits(MultizoneCompDetOpeningData(i).OpenFac3, 2));
                            MultizoneCompDetOpeningData(i).OpenFac3 = 1.0;
                        }
                    }
                }

                // Sanity checks, check sum of Height Factor and the Start Height Factor
                if (MultizoneCompDetOpeningData(i).HeightFac1 + MultizoneCompDetOpeningData(i).StartHFac1 > 1.0) {
                    ShowSevereError(RoutineName + ": " + CurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError(
                        "..The sum of Height Factor for Opening Factor 1 and Start Height Factor for Opening Factor 1 is greater than 1.0");
                    success = false;
                }
                if (MultizoneCompDetOpeningData(i).HeightFac2 + MultizoneCompDetOpeningData(i).StartHFac2 > 1.0) {
                    ShowSevereError(RoutineName + ": " + CurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError(
                        "..The sum of Height Factor for Opening Factor 2 and Start Height Factor for Opening Factor 2 is greater than 1.0");
                    success = false;
                }
                if (MultizoneCompDetOpeningData(i).NumFac > 2) {
                    if (MultizoneCompDetOpeningData(i).OpenFac2 >= MultizoneCompDetOpeningData(i).OpenFac3) {
                        ShowSevereError(RoutineName + ": " + CurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("..The value of Opening Factor #2 >= the value of Opening Factor #3");
                        success = false;
                    }
                    if (MultizoneCompDetOpeningData(i).HeightFac3 + MultizoneCompDetOpeningData(i).StartHFac3 > 1.0) {
                        ShowSevereError(RoutineName + ": " + CurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError(
                            "..The sum of Height Factor for Opening Factor 3 and Start Height Factor for Opening Factor 3 is greater than 1.0");
                        success = false;
                    }
                    if (MultizoneCompDetOpeningData(i).NumFac == 4) {
                        if (MultizoneCompDetOpeningData(i).OpenFac3 >= MultizoneCompDetOpeningData(i).OpenFac4) {
                            ShowSevereError(RoutineName + ": " + CurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("..The value of Opening Factor #3 >= the value of Opening Factor #4");
                            success = false;
                        }
                        if (MultizoneCompDetOpeningData(i).HeightFac4 + MultizoneCompDetOpeningData(i).StartHFac4 > 1.0) {
                            ShowSevereError(RoutineName + ": " + CurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                "..The sum of Height Factor for Opening Factor 4 and Start Height Factor for Opening Factor 4 is greater than 1.0");
                            success = false;
                        }
                    }
                }
                ++i;
            }
        }

        // Read AirflowNetwork simulation simple openings
        CurrentModuleObject = "AirflowNetwork:MultiZone:Component:SimpleOpening";
        AirflowNetworkNumOfSimOpenings = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                                               // Temporary workaround
            MultizoneCompSimpleOpeningData.allocate(AirflowNetworkNumOfSimOpenings); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient_when_opening_is_closed")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_opening_is_closed") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent_when_opening_is_closed");
                }
                Real64 diff{fields.at("minimum_density_difference_for_two_way_flow")};
                Real64 dischargeCoeff{fields.at("discharge_coefficient")};

                MultizoneCompSimpleOpeningData(i).Name = thisObjectName;       // Name of large simple opening component
                MultizoneCompSimpleOpeningData(i).FlowCoef = coeff;            // Air Mass Flow Coefficient When Window or Door Is Closed
                MultizoneCompSimpleOpeningData(i).FlowExpo = expnt;            // Air Mass Flow exponent When Window or Door Is Closed
                MultizoneCompSimpleOpeningData(i).MinRhoDiff = diff;           // Minimum density difference for two-way flow
                MultizoneCompSimpleOpeningData(i).DischCoeff = dischargeCoeff; // Discharge coefficient at full opening
                ++i;
            }
        }

        // Read AirflowNetwork simulation horizontal openings
        CurrentModuleObject = "AirflowNetwork:MultiZone:Component:HorizontalOpening";
        AirflowNetworkNumOfHorOpenings = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                                            // Temporary workaround
            MultizoneCompHorOpeningData.allocate(AirflowNetworkNumOfHorOpenings); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient_when_opening_is_closed")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_opening_is_closed") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent_when_opening_is_closed");
                }
                Real64 angle{90.0};
                if (fields.find("sloping_plane_angle") != fields.end()) {
                    angle = fields.at("sloping_plane_angle");
                }
                Real64 dischargeCoeff{fields.at("discharge_coefficient")};

                MultizoneCompHorOpeningData(i).Name = thisObjectName;       // Name of large simple opening component
                MultizoneCompHorOpeningData(i).FlowCoef = coeff;            // Air Mass Flow Coefficient When Window or Door Is Closed
                MultizoneCompHorOpeningData(i).FlowExpo = expnt;            // Air Mass Flow exponent When Window or Door Is Closed
                MultizoneCompHorOpeningData(i).Slope = angle;               // Sloping plane angle
                MultizoneCompHorOpeningData(i).DischCoeff = dischargeCoeff; // Discharge coefficient at full opening
                ++i;
            }
        }

        // *** Read AirflowNetwork simulation surface effective leakage area component
        CurrentModuleObject = "AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea";
        AirflowNetworkNumOfSurELA = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                                   // Temporary workaround
            MultizoneSurfaceELAData.allocate(AirflowNetworkNumOfSurELA); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 ela{fields.at("effective_leakage_area")};
                Real64 cd{1.0};
                if (fields.find("discharge_coefficient") != fields.end()) {
                    cd = fields.at("discharge_coefficient");
                }
                Real64 dp{4.0};
                if (fields.find("reference_pressure_difference") != fields.end()) {
                    dp = fields.at("reference_pressure_difference");
                }
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent");
                }

                MultizoneSurfaceELAData(i).Name = thisObjectName; // Name of surface effective leakage area component
                MultizoneSurfaceELAData(i).ELA = ela;             // Effective leakage area
                MultizoneSurfaceELAData(i).DischCoeff = cd;       // Discharge coefficient
                MultizoneSurfaceELAData(i).RefDeltaP = dp;        // Reference pressure difference
                MultizoneSurfaceELAData(i).FlowExpo = expnt;      // Air Mass Flow exponent
                MultizoneSurfaceELAData(i).TestDeltaP = 0.0;      // Testing pressure difference
                MultizoneSurfaceELAData(i).TestDisCoef = 0.0;     // Testing Discharge coefficient
                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: duct leakage
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Leak";
        DisSysNumOfLeaks = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                     // Temporary workaround
            DisSysCompLeakData.allocate(DisSysNumOfLeaks); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent");
                }

                DisSysCompLeakData(i).Name = thisObjectName; // Name of duct leak component
                DisSysCompLeakData(i).FlowCoef = coeff;      // Air Mass Flow Coefficient
                DisSysCompLeakData(i).FlowExpo = expnt;      // Air Mass Flow exponent
                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: duct effective leakage ratio
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:LeakageRatio";
        DisSysNumOfELRs = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                   // Temporary workaround
            DisSysCompELRData.allocate(DisSysNumOfELRs); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 elr{fields.at("effective_leakage_ratio")};
                Real64 maxflow{fields.at("maximum_flow_rate")};
                Real64 dp{fields.at("reference_pressure_difference")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent");
                }

                DisSysCompELRData(i).Name = thisObjectName;          // Name of duct effective leakage ratio component
                DisSysCompELRData(i).ELR = elr;                      // Value of effective leakage ratio
                DisSysCompELRData(i).FlowRate = maxflow * StdRhoAir; // Maximum airflow rate
                DisSysCompELRData(i).RefPres = dp;                   // Reference pressure difference
                DisSysCompELRData(i).FlowExpo = expnt;               // Air Mass Flow exponent
                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: duct
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Duct";
        DisSysNumOfDucts = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                     // Temporary workaround
            DisSysCompDuctData.allocate(DisSysNumOfDucts); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 L{fields.at("duct_length")};
                Real64 D{fields.at("hydraulic_diameter")};
                Real64 A{fields.at("cross_section_area")};
                Real64 e{0.0009};
                if (fields.find("surface_roughness") != fields.end()) {
                    e = fields.at("surface_roughness");
                }
                Real64 dlc{0.0};
                if (fields.find("coefficient_for_local_dynamic_loss_due_to_fitting") != fields.end()) {
                    dlc = fields.at("coefficient_for_local_dynamic_loss_due_to_fitting");
                }
                Real64 U{0.943};
                if (fields.find("heat_transmittance_coefficient_u_factor_for_duct_wall_construction") != fields.end()) {
                    U = fields.at("heat_transmittance_coefficient_u_factor_for_duct_wall_construction");
                }
                Real64 Um{0.001};
                if (fields.find("overall_moisture_transmittance_coefficient_from_air_to_air") != fields.end()) {
                    Um = fields.at("overall_moisture_transmittance_coefficient_from_air_to_air");
                }
                Real64 hout{0.0};
                if (fields.find("outside_convection_coefficient") != fields.end()) {
                    hout = fields.at("outside_convection_coefficient");
                }
                Real64 hin{0.0};
                if (fields.find("inside_convection_coefficient") != fields.end()) {
                    hin = fields.at("inside_convection_coefficient");
                }

                DisSysCompDuctData(i).Name = thisObjectName;   // Name of duct effective leakage ratio component
                DisSysCompDuctData(i).L = L;                   // Duct length [m]
                DisSysCompDuctData(i).hydraulicDiameter = D;   // Hydraulic diameter [m]
                DisSysCompDuctData(i).A = A;                   // Cross section area [m2]
                DisSysCompDuctData(i).roughness = e;           // Surface roughness [m]
                DisSysCompDuctData(i).TurDynCoef = dlc;        // Turbulent dynamic loss coefficient
                DisSysCompDuctData(i).UThermConduct = U;       // Conduction heat transmittance [W/m2.K]
                DisSysCompDuctData(i).UMoisture = Um;          // Overall moisture transmittance [kg/m2]
                DisSysCompDuctData(i).OutsideConvCoeff = hout; // Outside convection coefficient [W/m2.K]
                DisSysCompDuctData(i).InsideConvCoeff = hin;   // Inside convection coefficient [W/m2.K]
                DisSysCompDuctData(i).MThermal = 0.0;          // Thermal capacity [J/K]
                DisSysCompDuctData(i).MMoisture = 0.0;         // Moisture capacity [kg]
                DisSysCompDuctData(i).LamDynCoef = 64.0;       // Laminar dynamic loss coefficient
                DisSysCompDuctData(i).LamFriCoef = dlc;        // Laminar friction loss coefficient
                DisSysCompDuctData(i).InitLamCoef = 128.0;     // Coefficient of linear initialization
                DisSysCompDuctData(i).RelRough = e / D;        // e/D: relative roughness
                DisSysCompDuctData(i).RelL = L / D;            // L/D: relative length
                DisSysCompDuctData(i).A1 = 1.14 - 0.868589 * std::log(DisSysCompDuctData(i).RelRough); // 1.14 - 0.868589*ln(e/D)
                DisSysCompDuctData(i).g = DisSysCompDuctData(i).A1;                                    // 1/sqrt(Darcy friction factor)
                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: constant volume fan
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Fan";
        DisSysNumOfCVFs = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (DisSysNumOfCVFs > 0 && DisSysNumOfCVFs != inputProcessor->getNumObjectsFound("AirLoopHVAC")) {
            ShowSevereError("The number of entered AirflowNetwork:Distribution:Component:Fan objects is " + RoundSigDigits(DisSysNumOfCVFs));
            ShowSevereError("The number of entered AirLoopHVAC objects is " + RoundSigDigits(inputProcessor->getNumObjectsFound("AirLoopHVAC")));
            ShowContinueError("Both numbers should be equal. Please check your inputs.");
            success = false;
        }

        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                   // Temporary workaround
            DisSysCompCVFData.allocate(DisSysNumOfCVFs); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string fan_name = UtilityRoutines::MakeUPPERCase(fields.at("fan_name"));
                std::string fan_type = fields.at("supply_fan_object_type");

                bool FanErrorFound = false;
                int fanIndex;
                Real64 flowRate = 0.0;
                int fanType_Num = 0;
                int inletNode;
                int outletNode;

                if (UtilityRoutines::SameString(UtilityRoutines::MakeUPPERCase(fan_type), "FAN:SYSTEMMODEL")) {
                    HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(fan_name));
                    fanIndex = HVACFan::getFanObjectVectorIndex(fan_name);
                    if (fanIndex < 0) {
                        ShowSevereError("...occurs in " + CurrentModuleObject + " = " + DisSysCompCVFData(i).Name);
                        success = false;
                    } else {
                        flowRate = HVACFan::fanObjs[fanIndex]->designAirVolFlowRate;
                        flowRate *= StdRhoAir;
                        DisSysCompCVFData(i).FanModelFlag = true;
                        inletNode = HVACFan::fanObjs[fanIndex]->inletNodeNum;
                        outletNode = HVACFan::fanObjs[fanIndex]->outletNodeNum;
                        if (HVACFan::fanObjs[fanIndex]->speedControl == HVACFan::FanSystem::SpeedControlMethod::Continuous) {
                            fanType_Num = FanType_SimpleVAV;
                            VAVSystem = true;
                        } else {
                            fanType_Num = FanType_SimpleOnOff;
                        }
                        SupplyFanType = fanType_Num;
                    }

                } else {

                    GetFanIndex(fan_name, fanIndex, FanErrorFound);

                    if (FanErrorFound) {
                        ShowSevereError("...occurs in " + CurrentModuleObject + " = " + DisSysCompCVFData(i).Name);
                        success = false;
                    }

                    GetFanVolFlow(fanIndex, flowRate);
                    flowRate *= StdRhoAir;

                    GetFanType(fan_name, fanType_Num, FanErrorFound);
                    SupplyFanType = fanType_Num;
                }

                if (!(fanType_Num == FanType_SimpleConstVolume || fanType_Num == FanType_SimpleOnOff || fanType_Num == FanType_SimpleVAV)) {
                    ShowSevereError(RoutineName + "The Supply Fan Object Type in " + CurrentModuleObject + " = " + thisObjectName +
                                    " is not a valid fan type.");
                    ShowContinueError("Valid fan types are  Fan:ConstantVolume, Fan:OnOff, Fan:VariableVolume, or Fan:SystemModel.");
                    success = false;
                } else {
                    if (UtilityRoutines::SameString(fan_type, "Fan:ConstantVolume") && fanType_Num == FanType_SimpleOnOff) {
                        ShowSevereError("The Supply Fan Object Type defined in " + CurrentModuleObject + " is " + fan_type);
                        ShowContinueError("The Supply Fan Object Type defined in an AirLoopHVAC is Fan:OnOff");
                        success = false;
                    }
                    if (UtilityRoutines::SameString(fan_type, "Fan:OnOff") && fanType_Num == FanType_SimpleConstVolume) {
                        ShowSevereError("The Supply Fan Object Type defined in " + CurrentModuleObject + " is " + fan_type);
                        ShowContinueError("The Supply Fan Object Type defined in an AirLoopHVAC is Fan:ConstantVolume");
                        success = false;
                    }
                }
                bool ErrorsFound{false};
                if (fanType_Num == FanType_SimpleConstVolume) {
                    inletNode = GetFanInletNode("Fan:ConstantVolume", fan_name, ErrorsFound);
                    outletNode = GetFanOutletNode("Fan:ConstantVolume", fan_name, ErrorsFound);
                }
                if (fanType_Num == FanType_SimpleOnOff && !DisSysCompCVFData(i).FanModelFlag) {
                    inletNode = GetFanInletNode("Fan:OnOff", fan_name, ErrorsFound);
                    outletNode = GetFanOutletNode("Fan:OnOff", fan_name, ErrorsFound);
                }
                if (fanType_Num == FanType_SimpleVAV && !DisSysCompCVFData(i).FanModelFlag) {
                    inletNode = GetFanInletNode("Fan:VariableVolume", fan_name, ErrorsFound);
                    outletNode = GetFanOutletNode("Fan:VariableVolume", fan_name, ErrorsFound);
                    VAVSystem = true;
                }
                SupplyFanInletNode = inletNode;
                SupplyFanOutletNode = outletNode;

                if (ErrorsFound) {
                    success = false;
                }

                DisSysCompCVFData(i).Name = fan_name; // Name of duct effective leakage ratio component
                DisSysCompCVFData(i).Ctrl = 1.0;      // Control ratio
                DisSysCompCVFData(i).FanIndex = fanIndex;
                DisSysCompCVFData(i).FlowRate = flowRate;
                DisSysCompCVFData(i).FanTypeNum = fanType_Num;
                DisSysCompCVFData(i).InletNode = inletNode;
                DisSysCompCVFData(i).OutletNode = outletNode;
                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: coil
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Coil";
        DisSysNumOfCoils = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                     // Temporary workaround
            DisSysCompCoilData.allocate(DisSysNumOfCoils); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                // auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string coil_name = fields.at("coil_name");
                std::string coil_type = fields.at("coil_object_type");
                Real64 L{fields.at("air_path_length")};
                Real64 D{fields.at("air_path_hydraulic_diameter")};

                DisSysCompCoilData(i).Name = UtilityRoutines::MakeUPPERCase(coil_name); // Name of associated EPlus coil component
                DisSysCompCoilData(i).EPlusType = coil_type;                            // coil type
                DisSysCompCoilData(i).L = L;                                            // Air path length
                DisSysCompCoilData(i).hydraulicDiameter = D;                            // Air path hydraulic diameter
                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: heat exchanger
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:HeatExchanger";
        DisSysNumOfHXs = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                 // Temporary workaround
            DisSysCompHXData.allocate(DisSysNumOfHXs); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                // auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string hx_name = fields.at("heatexchanger_name");
                std::string hx_type = fields.at("heatexchanger_object_type");
                Real64 L{fields.at("air_path_length")};
                Real64 D{fields.at("air_path_hydraulic_diameter")};

                DisSysCompHXData(i).Name = UtilityRoutines::MakeUPPERCase(hx_name); // Name of associated EPlus heat exchange component
                DisSysCompHXData(i).EPlusType = hx_type;                            // coil type
                DisSysCompHXData(i).L = L;                                          // Air path length
                DisSysCompHXData(i).hydraulicDiameter = D;                          // Air path hydraulic diameter
                DisSysCompHXData(i).CoilParentExists = HVACHXAssistedCoolingCoil::VerifyHeatExchangerParent(hx_type, hx_name);
                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: terminal unit
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:TerminalUnit";
        DisSysNumOfTermUnits = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                             // Temporary workaround
            DisSysCompTermUnitData.allocate(DisSysNumOfTermUnits); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                // auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string tu_name = fields.at("terminal_unit_name");
                std::string tu_type = fields.at("terminal_unit_object_type");
                Real64 L{fields.at("air_path_length")};
                Real64 D{fields.at("air_path_hydraulic_diameter")};

                DisSysCompTermUnitData(i).Name = UtilityRoutines::MakeUPPERCase(tu_name); // Name of associated EPlus coil component
                DisSysCompTermUnitData(i).EPlusType = tu_type;                            // Terminal unit type
                DisSysCompTermUnitData(i).L = L;                                          // Air path length
                DisSysCompTermUnitData(i).hydraulicDiameter = D;                          // Air path hydraulic diameter
                ++i;
            }
        }

        // Get input data of constant pressure drop component
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:ConstantPressureDrop";
        DisSysNumOfCPDs = inputProcessor->getNumObjectsFound(CurrentModuleObject); // Temporary workaround
        instances = inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != inputProcessor->epJSON.end()) {
            int i = 1;                                   // Temporary workaround
            DisSysCompCPDData.allocate(DisSysNumOfCPDs); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 dp{fields.at("pressure_difference_across_the_component")};

                DisSysCompCPDData(i).Name = thisObjectName; // Name of constant pressure drop component
                DisSysCompCPDData(i).A = 1.0;               // cross section area
                DisSysCompCPDData(i).DP = dp;               // Pressure difference across the component
                ++i;
            }
        }

        return success;
    }

    void GetAirflowNetworkInput(OutputFiles &outputFiles)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Aug. 2003
        //       MODIFIED       Aug. 2005
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reads inputs of air distribution system

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::GetCurveIndex;
        using DataGlobals::AnyLocalEnvironmentsInModel;
        using DataHeatBalance::People;
        using DataHeatBalance::TotPeople;
        using DataLoopNode::Node;
        using DataLoopNode::NodeConnectionType_Inlet;
        using DataLoopNode::NodeType_Air;
        using DataLoopNode::ObjectIsParent;
        using HVACHXAssistedCoolingCoil::VerifyHeatExchangerParent;
        using MixedAir::GetOAMixerNumber;
        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::SetOutAirNodes;
        using RoomAirModelManager::GetRAFNNodeNum;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetAirflowNetworkInput: "); // include trailing blank space

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int n;
        int j;
        int k;
        int m;
        int count;
        bool NodeFound;
        bool CompFound;
        bool ErrorsFound;
        bool found;
        bool NodeFound1;
        bool NodeFound2;
        int NumAPL;
        Array1D_string CompName(2);
        std::string SimAirNetworkKey;
        bool SimObjectError;
        std::string StringOut;
        int ZoneNum;
        int NodeNum;

        // Declare variables used in this subroutine for debug purpose
        bool AirflowNetworkInitFlag;
        Array1D_int ZoneCheck;
        Array1D_int ZoneBCCheck;
        bool SurfaceFound;

        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;   // Used in GetObjectItem
        std::string CurrentModuleObject;
        Array1D_string Alphas;         // Alpha input items for object
        Array1D_string cAlphaFields;   // Alpha field names
        Array1D_string cNumericFields; // Numeric field names
        Array1D<Real64> Numbers;       // Numeric input items for object
        Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.
        static int MaxNums(0);         // Maximum number of numeric input fields
        static int MaxAlphas(0);       // Maximum number of alpha input fields
        static int TotalArgs(0);       // Total number of alpha and numeric arguments (max) for a
        bool Errorfound1;
        Real64 minHeight;
        Real64 maxHeight;
        Real64 baseratio;

        // Formats
        static constexpr auto Format_110(
            "! <AirflowNetwork Model:Control>, No Multizone or Distribution/Multizone with Distribution/Multizone "
            "without Distribution/Multizone with Distribution only during Fan Operation\n");
        static constexpr auto Format_120("AirflowNetwork Model:Control,{}\n");

        // Set the maximum numbers of input fields
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:SimulationControl", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:Zone", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:Surface", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:ReferenceCrackConditions", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:Surface:Crack", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:Component:DetailedOpening", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:Component:SimpleOpening", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:Component:ZoneExhaustFan", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:ExternalNode", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:WindPressureCoefficientArray", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:WindPressureCoefficientValues", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Node", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Component:Leak", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Component:LeakageRatio", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Component:Duct", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:DuctViewFactors", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Component:Fan", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Component:Coil", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Component:TerminalUnit", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Component:ConstantPressureDrop", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Linkage", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:OccupantVentilationControl", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:IntraZone:Node", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:IntraZone:Linkage", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirflowNetwork:ZoneControl:PressureController", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Component:OutdoorAirFlow", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);
        // inputProcessor->getObjectDefMaxArgs("AirflowNetwork:Distribution:Component:ReliefAirFlow", TotalArgs, NumAlphas, NumNumbers);
        // MaxNums = max(MaxNums, NumNumbers);
        // MaxAlphas = max(MaxAlphas, NumAlphas);

        Alphas.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNums);
        Numbers.dimension(MaxNums, 0.0);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNums, true);

        ErrorsFound = false;
        AirflowNetworkInitFlag = false;

        // Read AirflowNetwork OccupantVentilationControl before reading other AirflowNetwork objects, so that this object can be called by other
        // simple ventilation objects
        CurrentModuleObject = "AirflowNetwork:OccupantVentilationControl";
        AirflowNetworkNumOfOccuVentCtrls = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (AirflowNetworkNumOfOccuVentCtrls > 0) {
            OccupantVentilationControl.allocate(AirflowNetworkNumOfOccuVentCtrls);
            for (i = 1; i <= AirflowNetworkNumOfOccuVentCtrls; ++i) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              i,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);
                UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
                OccupantVentilationControl(i).Name = Alphas(1); // Name of object
                OccupantVentilationControl(i).MinOpeningTime = Numbers(1);
                if (OccupantVentilationControl(i).MinOpeningTime < 0.0) {
                    ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(1) + " < 0.0");
                    ShowContinueError("..Input value = " + RoundSigDigits(OccupantVentilationControl(i).MinOpeningTime, 1) +
                                      ", Value will be reset to 0.0");
                    ShowContinueError("..for " + cAlphaFields(1) + " = \"" + OccupantVentilationControl(i).Name);
                    OccupantVentilationControl(i).MinOpeningTime = 0.0;
                }
                OccupantVentilationControl(i).MinClosingTime = Numbers(2);
                if (OccupantVentilationControl(i).MinClosingTime < 0.0) {
                    ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(2) + " < 0.0");
                    ShowContinueError("..Input value = " + RoundSigDigits(OccupantVentilationControl(i).MinClosingTime, 1) +
                                      ", Value will be reset to 0.0");
                    ShowContinueError("..for " + cAlphaFields(1) + " = \"" + OccupantVentilationControl(i).Name);
                    OccupantVentilationControl(i).MinClosingTime = 0.0;
                }
                if (NumAlphas == 1 && NumNumbers == 2) {
                    OccupantVentilationControl(i).MinTimeControlOnly = true;
                }
                if (!lAlphaBlanks(2)) {
                    OccupantVentilationControl(i).ComfortLowTempCurveName = Alphas(2);
                    OccupantVentilationControl(i).ComfortLowTempCurveNum = GetCurveIndex(Alphas(2)); // convert curve name to number
                    if (OccupantVentilationControl(i).ComfortLowTempCurveNum == 0) {
                        OccupantVentilationControl(i).MinTimeControlOnly = true;
                        ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(2) +
                                         " not found = " + OccupantVentilationControl(i).ComfortLowTempCurveName);
                        ShowContinueError("..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ShowContinueError(
                            "Thermal comfort will not be performed and minimum opening and closing times are checked only. Simulation continues.");
                    } else {
                        ErrorsFound |= CurveManager::CheckCurveDims(OccupantVentilationControl(i).ComfortLowTempCurveNum, // Curve index
                                                                    {1},                                                  // Valid dimensions
                                                                    RoutineName,                                          // Routine name
                                                                    CurrentModuleObject,                                  // Object Type
                                                                    OccupantVentilationControl(i).Name,                   // Object Name
                                                                    cAlphaFields(2));                                     // Field Name
                    }
                }
                if (!lAlphaBlanks(3)) {
                    OccupantVentilationControl(i).ComfortHighTempCurveName = Alphas(3);
                    OccupantVentilationControl(i).ComfortHighTempCurveNum = GetCurveIndex(Alphas(3)); // convert curve name to number
                    if (OccupantVentilationControl(i).ComfortHighTempCurveNum > 0) {
                        ErrorsFound |= CurveManager::CheckCurveDims(OccupantVentilationControl(i).ComfortHighTempCurveNum, // Curve index
                                                                    {1},                                                   // Valid dimensions
                                                                    RoutineName,                                           // Routine name
                                                                    CurrentModuleObject,                                   // Object Type
                                                                    OccupantVentilationControl(i).Name,                    // Object Name
                                                                    cAlphaFields(3));                                      // Field Name
                    } else {
                        ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(3) +
                                         " not found = " + OccupantVentilationControl(i).ComfortHighTempCurveName);
                        ShowContinueError("..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ShowContinueError("A single curve of thermal comfort low temperature is used only. Simulation continues.");
                    }
                }
                if (OccupantVentilationControl(i).ComfortHighTempCurveNum > 0) {
                    OccupantVentilationControl(i).ComfortBouPoint = Numbers(3);
                    if (OccupantVentilationControl(i).ComfortBouPoint < 0.0) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(3) + " < 0.0");
                        ShowContinueError("..Input value = " + RoundSigDigits(OccupantVentilationControl(i).ComfortBouPoint, 1) +
                                          ", Value will be reset to 10.0 as default");
                        ShowContinueError("..for " + cAlphaFields(1) + " = \"" + OccupantVentilationControl(i).Name);
                        OccupantVentilationControl(i).ComfortBouPoint = 10.0;
                    }
                }
                // Check continuity of both curves at boundary point
                if (OccupantVentilationControl(i).ComfortLowTempCurveNum > 0 && OccupantVentilationControl(i).ComfortHighTempCurveNum) {
                    if (std::abs(CurveValue(OccupantVentilationControl(i).ComfortLowTempCurveNum, Numbers(3)) -
                                 CurveValue(OccupantVentilationControl(i).ComfortHighTempCurveNum, Numbers(3))) > 0.1) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " object: The difference of both curve values at boundary point > 0.1");
                        ShowContinueError("Both curve names are = " + cAlphaFields(2) + " and " + cAlphaFields(3));
                        ShowContinueError("The input value of " + cNumericFields(3) + " = " +
                                          RoundSigDigits(OccupantVentilationControl(i).ComfortBouPoint, 1));
                        ErrorsFound = true;
                    }
                }
                if (!lNumericBlanks(4)) {
                    OccupantVentilationControl(i).MaxPPD = Numbers(4);
                    if (OccupantVentilationControl(i).MaxPPD < 0.0 || OccupantVentilationControl(i).MaxPPD > 100.0) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(4) + " beyond 0.0 and 100.0");
                        ShowContinueError("..Input value = " + RoundSigDigits(OccupantVentilationControl(i).MaxPPD, 1) +
                                          ", Value will be reset to 10.0 as default");
                        ShowContinueError("..for " + cAlphaFields(1) + " = \"" + OccupantVentilationControl(i).Name);
                        OccupantVentilationControl(i).MaxPPD = 10.0;
                    }
                }
                if (!lAlphaBlanks(4)) {
                    if (UtilityRoutines::SameString(Alphas(4), "Yes")) {
                        OccupantVentilationControl(i).OccupancyCheck = true;
                    } else if (UtilityRoutines::SameString(Alphas(4), "No")) {
                        OccupantVentilationControl(i).OccupancyCheck = false;
                    } else {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\" invalid " + cAlphaFields(2) + "=\"" + Alphas(2) +
                                        "\" illegal key.");
                        ShowContinueError("Valid keys are: Yes or No");
                        ErrorsFound = true;
                    }
                }
                if (!lAlphaBlanks(5)) {
                    OccupantVentilationControl(i).OpeningProbSchName = Alphas(5); // a schedule name for opening probability
                    OccupantVentilationControl(i).OpeningProbSchNum = GetScheduleIndex(OccupantVentilationControl(i).OpeningProbSchName);
                    if (OccupantVentilationControl(i).OpeningProbSchNum == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(5) +
                                        " not found = " + OccupantVentilationControl(i).OpeningProbSchName);
                        ShowContinueError("..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                }
                if (!lAlphaBlanks(6)) {
                    OccupantVentilationControl(i).ClosingProbSchName = Alphas(6); // a schedule name for closing probability
                    OccupantVentilationControl(i).ClosingProbSchNum = GetScheduleIndex(OccupantVentilationControl(i).ClosingProbSchName);
                    if (OccupantVentilationControl(i).OpeningProbSchNum == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(6) +
                                        " not found = " + OccupantVentilationControl(i).ClosingProbSchName);
                        ShowContinueError("..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found getting inputs. Previous error(s) cause program termination.");
        }

        // *** Read AirflowNetwork simulation parameters
        CurrentModuleObject = "AirflowNetwork:SimulationControl";
        NumAirflowNetwork = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (NumAirflowNetwork == 0) {
            SimulateAirflowNetwork = AirflowNetworkControlSimple;
            print(outputFiles.eio, Format_110);
            print(outputFiles.eio, Format_120, "NoMultizoneOrDistribution");
            return;
        }
        if (NumAirflowNetwork > 1) {
            ShowFatalError(RoutineName + "Only one (\"1\") " + CurrentModuleObject + " object per simulation is allowed.");
        }

        SimObjectError = false;
        inputProcessor->getObjectItem(CurrentModuleObject,
                                      NumAirflowNetwork,
                                      Alphas,
                                      NumAlphas,
                                      Numbers,
                                      NumNumbers,
                                      IOStatus,
                                      lNumericBlanks,
                                      lAlphaBlanks,
                                      cAlphaFields,
                                      cNumericFields);

        AirflowNetworkSimu.AirflowNetworkSimuName = Alphas(1);
        AirflowNetworkSimu.Control = Alphas(2);
        AirflowNetworkSimu.WPCCntr = Alphas(3);
        AirflowNetworkSimu.HeightOption = Alphas(4);
        AirflowNetworkSimu.BldgType = Alphas(5);

        // Find a flag for possible combination of vent and distribution system
        {
            auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(AirflowNetworkSimu.Control));
            if (SELECT_CASE_var == "NOMULTIZONEORDISTRIBUTION") {
                SimulateAirflowNetwork = AirflowNetworkControlSimple;
                SimAirNetworkKey = "NoMultizoneOrDistribution";
            } else if (SELECT_CASE_var == "MULTIZONEWITHOUTDISTRIBUTION") {
                SimulateAirflowNetwork = AirflowNetworkControlMultizone;
                SimAirNetworkKey = "MultizoneWithoutDistribution";
            } else if (SELECT_CASE_var == "MULTIZONEWITHDISTRIBUTIONONLYDURINGFANOPERATION") {
                SimulateAirflowNetwork = AirflowNetworkControlSimpleADS;
                SimAirNetworkKey = "MultizoneWithDistributionOnlyDuringFanOperation";
            } else if (SELECT_CASE_var == "MULTIZONEWITHDISTRIBUTION") {
                SimulateAirflowNetwork = AirflowNetworkControlMultiADS;
                SimAirNetworkKey = "MultizoneWithDistribution";
            } else { // Error
                ShowSevereError(RoutineName + CurrentModuleObject + " object, The entered choice for " + cAlphaFields(2) + " is not valid = \"" +
                                AirflowNetworkSimu.Control + "\"");
                ShowContinueError("Valid choices are \"NO MULTIZONE OR DISTRIBUTION\",\"MULTIZONE WITH DISTRIBUTION ONLY DURING FAN OPERATION\"");
                ShowContinueError("\"MULTIZONE WITH DISTRIBUTION\", or \"MULTIZONE WITHOUT DISTRIBUTION\"");
                ShowContinueError("..specified in " + CurrentModuleObject + ' ' + cAlphaFields(1) + " = " +
                                  AirflowNetworkSimu.AirflowNetworkSimuName);
                ErrorsFound = true;
            }
        }

        // Check the number of primary air loops
        if (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS || SimulateAirflowNetwork == AirflowNetworkControlMultiADS) {
            NumAPL = inputProcessor->getNumObjectsFound("AirLoopHVAC");
            if (NumAPL > 0) {
                LoopPartLoadRatio.allocate(NumAPL);
                LoopOnOffFanRunTimeFraction.allocate(NumAPL);
                LoopOnOffFlag.allocate(NumAPL);
                LoopPartLoadRatio = 0.0;
                LoopOnOffFanRunTimeFraction = 0.0;
                LoopOnOffFlag = false;
            }
        }
        print(outputFiles.eio, Format_110);
        print(outputFiles.eio, Format_120, SimAirNetworkKey);

        // Check whether there are any objects from infiltration, ventilation, mixing and cross mixing
        if (SimulateAirflowNetwork == AirflowNetworkControlSimple || SimulateAirflowNetwork == AirflowNetworkControlSimpleADS) {
            if (TotInfiltration + TotVentilation + TotMixing + TotCrossMixing + TotZoneAirBalance +
                    inputProcessor->getNumObjectsFound("ZoneEarthtube") + inputProcessor->getNumObjectsFound("ZoneThermalChimney") +
                    inputProcessor->getNumObjectsFound("ZoneCoolTower:Shower") ==
                0) {
                ShowWarningError(RoutineName + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\".");
                ShowContinueError(
                    "..but there are no Infiltration, Ventilation, Mixing, Cross Mixing or ZoneAirBalance objects. The simulation continues...");
            }
        }

        // Check whether a user wants to perform SIMPLE calculation only or not
        if (SimulateAirflowNetwork == AirflowNetworkControlSimple) return;

        if (SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS) {
            if (TotInfiltration > 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + " object, ");
                ShowContinueError("..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneInfiltration:* objects are present.");
                ShowContinueError("..ZoneInfiltration objects will not be simulated.");
            }
            if (TotVentilation > 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + " object, ");
                ShowContinueError("..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneVentilation:* objects are present.");
                ShowContinueError("..ZoneVentilation objects will not be simulated.");
            }
            if (TotMixing > 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + " object, ");
                ShowContinueError("..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneMixing objects are present.");
                ShowContinueError("..ZoneMixing objects will not be simulated.");
            }
            if (TotCrossMixing > 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + " object, ");
                ShowContinueError("..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneCrossMixing objects are present.");
                ShowContinueError("..ZoneCrossMixing objects will not be simulated.");
            }
            if (TotZoneAirBalance > 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + " object, ");
                ShowContinueError("..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey +
                                  "\" and ZoneAirBalance:OutdoorAir objects are present.");
                ShowContinueError("..ZoneAirBalance:OutdoorAir objects will not be simulated.");
            }
            if (inputProcessor->getNumObjectsFound("ZoneEarthtube") > 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + " object, ");
                ShowContinueError("..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneEarthtube objects are present.");
                ShowContinueError("..ZoneEarthtube objects will not be simulated.");
            }
            if (inputProcessor->getNumObjectsFound("ZoneThermalChimney") > 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + " object, ");
                ShowContinueError("..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneThermalChimney objects are present.");
                ShowContinueError("..ZoneThermalChimney objects will not be simulated.");
            }
            if (inputProcessor->getNumObjectsFound("ZoneCoolTower:Shower") > 0) {
                ShowWarningError(RoutineName + CurrentModuleObject + " object, ");
                ShowContinueError("..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneCoolTower:Shower objects are present.");
                ShowContinueError("..ZoneCoolTower:Shower objects will not be simulated.");
            }
        }

        SetOutAirNodes();

        if (UtilityRoutines::SameString(AirflowNetworkSimu.WPCCntr, "Input")) {
            AirflowNetworkSimu.iWPCCntr = iWPCCntr_Input;
            if (lAlphaBlanks(4)) {
                ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(3) + " = INPUT.");
                ShowContinueError(".." + cAlphaFields(4) + " was not entered.");
                ErrorsFound = true;
                SimObjectError = true;
            } else {
                if (!(UtilityRoutines::SameString(AirflowNetworkSimu.HeightOption, "ExternalNode") ||
                      UtilityRoutines::SameString(AirflowNetworkSimu.HeightOption, "OpeningHeight"))) {
                    ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(4) + " = " + Alphas(4) + " is invalid.");
                    ShowContinueError("Valid choices are ExternalNode or OpeningHeight. " + CurrentModuleObject + ": " + cAlphaFields(1) + " = " +
                                      AirflowNetworkSimu.AirflowNetworkSimuName);
                    ErrorsFound = true;
                    SimObjectError = true;
                }
            }
            //     if (AirflowNetworkSimu%BldgType /= ' ') then
            //        CALL ShowMessage('GetAirflowNetworkInput: AirflowNetwork Wind Pressure Coefficient Type = INPUT.'// &
            //         ' Building type = '//TRIM(AirflowNetworkSimu%BldgType)//' was entered but will not be used.')
            //        AirflowNetworkSimu%BldgType    = ' '
            //     end if
        } else if (UtilityRoutines::SameString(AirflowNetworkSimu.WPCCntr, "SurfaceAverageCalculation")) {
            AirflowNetworkSimu.iWPCCntr = iWPCCntr_SurfAvg;
            // if ( ! lAlphaBlanks( 4 ) ) {
            // AirflowNetworkSimu.CpArrayName = "";
            //        CALL ShowWarningError('GetAirflowNetworkInput: AirflowNetwork Wind Pressure Coefficient Type '// &
            //             '= SURFACE-AVERAGE CALCULATION.'// &
            //             ' CP ARRAY NAME was entered but will not be used. The simulation continues...')
            //}
            if (!(UtilityRoutines::SameString(AirflowNetworkSimu.BldgType, "LowRise") ||
                  UtilityRoutines::SameString(AirflowNetworkSimu.BldgType, "HighRise"))) {
                ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(5) + " = " + Alphas(5) + " is invalid.");
                ShowContinueError("Valid choices are LowRise or HighRise. " + CurrentModuleObject + ": " + cAlphaFields(1) + " = " +
                                  AirflowNetworkSimu.AirflowNetworkSimuName);
                ErrorsFound = true;
                SimObjectError = true;
            }
            for (k = 1; k <= NumOfNodes; ++k) {
                if (Node(k).IsLocalNode) {
                    ShowSevereError(RoutineName + "Invalid " + cAlphaFields(3) + "=" + Alphas(3));
                    ShowContinueError("A local air node is defined to INPUT the wind pressure coefficient curve, while Wind Pressure Coefficient "
                                      "Type is set to SurfaceAverageCalculation.");
                    ShowContinueError("It requires  the Wind Pressure Coefficient Type be set to INPUT to use the local air node.");
                    ErrorsFound = true;
                    SimObjectError = true;
                    break;
                }
            }

        } else {
            ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(3) + " = " + AirflowNetworkSimu.WPCCntr +
                            " is not valid.");
            ShowContinueError("Valid choices are Input or SurfaceAverageCalculation. " + CurrentModuleObject + " = " +
                              AirflowNetworkSimu.AirflowNetworkSimuName);
            ErrorsFound = true;
            SimObjectError = true;
        }

        AirflowNetworkSimu.InitType = Alphas(6);
        if (UtilityRoutines::SameString(AirflowNetworkSimu.InitType, "LinearInitializationMethod")) {
            AirflowNetworkSimu.InitFlag = 0;
        } else if (UtilityRoutines::SameString(AirflowNetworkSimu.InitType, "ZeroNodePressures")) {
            AirflowNetworkSimu.InitFlag = 1;
        } else if (UtilityRoutines::SameString(AirflowNetworkSimu.InitType, "0")) {
            AirflowNetworkSimu.InitFlag = 0;
        } else if (UtilityRoutines::SameString(AirflowNetworkSimu.InitType, "1")) {
            AirflowNetworkSimu.InitFlag = 1;
        } else {
            ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(6) + " = " + Alphas(6) + " is invalid.");
            ShowContinueError("Valid choices are LinearInitializationMethod or ZeroNodePressures. " + CurrentModuleObject + " = " +
                              AirflowNetworkSimu.AirflowNetworkSimuName);
            ErrorsFound = true;
            SimObjectError = true;
        }

        if (!lAlphaBlanks(7) && UtilityRoutines::SameString(Alphas(7), "Yes")) AirflowNetworkSimu.TExtHeightDep = true;

        if (lAlphaBlanks(8)) {
            AirflowNetworkSimu.solver = AirflowNetworkSimuProp::Solver::SkylineLU;
        } else if (UtilityRoutines::SameString(Alphas(8), "SkylineLU")) {
            AirflowNetworkSimu.solver = AirflowNetworkSimuProp::Solver::SkylineLU;
        } else if (UtilityRoutines::SameString(Alphas(8), "ConjugateGradient")) {
            AirflowNetworkSimu.solver = AirflowNetworkSimuProp::Solver::ConjugateGradient;
        } else {
            AirflowNetworkSimu.solver = AirflowNetworkSimuProp::Solver::SkylineLU;
            ShowWarningError(RoutineName + CurrentModuleObject + " object, ");
            ShowContinueError("..Specified " + cAlphaFields(8) + " = \"" + Alphas(8) + "\" is unrecognized.");
            ShowContinueError("..Default value \"SkylineLU\" will be used.");
        }

        if (SimObjectError) {
            ShowFatalError(RoutineName + "Errors found getting " + CurrentModuleObject + " object. Previous error(s) cause program termination.");
        }

        AirflowNetworkSimu.MaxIteration = Numbers(1);
        AirflowNetworkSimu.RelTol = Numbers(2);
        AirflowNetworkSimu.AbsTol = Numbers(3);
        AirflowNetworkSimu.ConvLimit = Numbers(4);
        AirflowNetworkSimu.Azimuth = Numbers(5);
        AirflowNetworkSimu.AspectRatio = Numbers(6);
        AirflowNetworkSimu.MaxPressure = 500.0; // Maximum pressure difference by default

        // *** Read AirflowNetwork simulation zone data
        CurrentModuleObject = "AirflowNetwork:MultiZone:Zone";
        AirflowNetworkNumOfZones = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (AirflowNetworkNumOfZones > 0) {
            MultizoneZoneData.allocate(AirflowNetworkNumOfZones);
            AirflowNetworkZoneFlag.dimension(NumOfZones, false); // AirflowNetwork zone flag
            for (i = 1; i <= AirflowNetworkNumOfZones; ++i) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              i,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);
                UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
                MultizoneZoneData(i).ZoneName = Alphas(1);                          // Name of Associated EnergyPlus Thermal Zone
                if (!lAlphaBlanks(2)) MultizoneZoneData(i).VentControl = Alphas(2); // Ventilation Control Mode: "Temperature", "Enthalpy",
                // "ASHRAE55ADAPTIVE", "CEN15251AdaptiveComfort,
                // "Constant", or "NoVent"
                MultizoneZoneData(i).VentSchName = Alphas(3); // Name of ventilation temperature control schedule
                MultizoneZoneData(i).OpenFactor = Numbers(1); // Limit Value on Multiplier for Modulating Venting Open Factor,
                // Not applicable if Vent Control Mode = CONSTANT or NOVENT
                MultizoneZoneData(i).LowValueTemp = Numbers(2); // Lower Value on Inside/Outside Temperature Difference
                // for Modulating the Venting Open Factor with temp control
                MultizoneZoneData(i).UpValueTemp = Numbers(3); // Upper Value on Inside/Outside Temperature Difference
                // for Modulating the Venting Open Factor with temp control
                MultizoneZoneData(i).LowValueEnth = Numbers(4); // Lower Value on Inside/Outside Temperature Difference
                // for Modulating the Venting Open Factor with Enthalpy control
                MultizoneZoneData(i).UpValueEnth = Numbers(5); // Upper Value on Inside/Outside Temperature Difference
                // for Modulating the Venting Open Factor with Enthalpy control
                MultizoneZoneData(i).VentCtrNum = VentCtrNum_None;
                MultizoneZoneData(i).SingleSidedCpType = Alphas(5);
                MultizoneZoneData(i).BuildWidth = Numbers(6);

                if (!lAlphaBlanks(6)) {
                    MultizoneZoneData(i).OccupantVentilationControlName = Alphas(6);
                    MultizoneZoneData(i).OccupantVentilationControlNum =
                        UtilityRoutines::FindItemInList(MultizoneZoneData(i).OccupantVentilationControlName, OccupantVentilationControl);
                    if (MultizoneZoneData(i).OccupantVentilationControlNum == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(6) +
                                        " not found = " + MultizoneZoneData(i).OccupantVentilationControlName);
                        ShowContinueError("..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                }
                if (UtilityRoutines::SameString(MultizoneZoneData(i).VentControl, "Temperature")) MultizoneZoneData(i).VentCtrNum = VentCtrNum_Temp;
                if (UtilityRoutines::SameString(MultizoneZoneData(i).VentControl, "Enthalpy")) MultizoneZoneData(i).VentCtrNum = VentCtrNum_Enth;
                if (UtilityRoutines::SameString(MultizoneZoneData(i).VentControl, "Constant")) MultizoneZoneData(i).VentCtrNum = VentCtrNum_Const;
                if (UtilityRoutines::SameString(MultizoneZoneData(i).VentControl, "ASHRAE55Adaptive"))
                    MultizoneZoneData(i).VentCtrNum = VentCtrNum_ASH55;
                if (UtilityRoutines::SameString(MultizoneZoneData(i).VentControl, "CEN15251Adaptive"))
                    MultizoneZoneData(i).VentCtrNum = VentCtrNum_CEN15251;
                if (UtilityRoutines::SameString(MultizoneZoneData(i).VentControl, "NoVent")) MultizoneZoneData(i).VentCtrNum = VentCtrNum_Novent;

                if (MultizoneZoneData(i).VentCtrNum < NumOfVentCtrTypes) {
                    if (NumAlphas >= 4 && (!lAlphaBlanks(4))) {
                        MultizoneZoneData(i).VentingSchName = Alphas(4);
                        MultizoneZoneData(i).VentingSchNum = GetScheduleIndex(MultizoneZoneData(i).VentingSchName);
                        if (MultizoneZoneData(i).VentingSchNum == 0) {
                            ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(4) +
                                            " not found = " + MultizoneZoneData(i).VentingSchName);
                            ShowContinueError("..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                            ErrorsFound = true;
                        }
                    }
                } else {
                    MultizoneZoneData(i).VentingSchName = BlankString;
                    MultizoneZoneData(i).VentingSchNum = 0;
                }
            }
        } else {
            ShowSevereError(RoutineName + "For an AirflowNetwork Simulation, at least one " + CurrentModuleObject +
                            " object is required but none were found.");
            ShowFatalError(RoutineName + "Errors found getting " + CurrentModuleObject + " object. Previous error(s) cause program termination.");
        }

        // ==> Zone data validation
        for (i = 1; i <= AirflowNetworkNumOfZones; ++i) {
            // Zone name validation
            MultizoneZoneData(i).ZoneNum = UtilityRoutines::FindItemInList(MultizoneZoneData(i).ZoneName, Zone);
            if (MultizoneZoneData(i).ZoneNum == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + " object, invalid " + cAlphaFields(1) + " given.");
                ShowContinueError("..invalid " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName + "\"");
                ErrorsFound = true;
            } else {
                AirflowNetworkZoneFlag(MultizoneZoneData(i).ZoneNum) = true;
                MultizoneZoneData(i).Height = Zone(MultizoneZoneData(i).ZoneNum).Centroid.z; // Nodal height
            }
            if (MultizoneZoneData(i).VentCtrNum == VentCtrNum_None) {
                ShowSevereError(RoutineName + CurrentModuleObject + " object, invalid " + cAlphaFields(2) + " = " + MultizoneZoneData(i).VentControl);
                ShowContinueError("Valid choices are Temperature, Enthalpy, Constant, or NoVent");
                ShowContinueError(".. in " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName + "\"");
                ErrorsFound = true;
            }
            if (UtilityRoutines::SameString(MultizoneZoneData(i).VentControl, "Temperature") ||
                UtilityRoutines::SameString(MultizoneZoneData(i).VentControl, "Enthalpy")) {
                // .or. &
                // UtilityRoutines::SameString(MultizoneZoneData(i)%VentControl,'ASHRAE55Adaptive') .or. &
                // UtilityRoutines::SameString(MultizoneZoneData(i)%VentControl,'CEN15251Adaptive')) then
                MultizoneZoneData(i).VentSchNum = GetScheduleIndex(MultizoneZoneData(i).VentSchName);
                if (MultizoneZoneData(i).VentSchName == BlankString) {
                    ShowSevereError(RoutineName + CurrentModuleObject + " object, No " + cAlphaFields(3) + " was found, but is required when " +
                                    cAlphaFields(2) + " is Temperature or Enthalpy.");
                    ShowContinueError("..for " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName + "\", with " + cAlphaFields(2) + " = \"" +
                                      MultizoneZoneData(i).VentControl + "\"");
                    ErrorsFound = true;
                } else if (MultizoneZoneData(i).VentSchNum == 0) {
                    ShowSevereError(RoutineName + CurrentModuleObject + " object, invalid " + cAlphaFields(3) + ", required when " + cAlphaFields(2) +
                                    " is Temperature or Enthalpy.");
                    ShowContinueError(".." + cAlphaFields(3) + " in error = " + MultizoneZoneData(i).VentSchName);
                    ShowContinueError("..for " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName + "\", with " + cAlphaFields(2) + " = \"" +
                                      MultizoneZoneData(i).VentControl + "\"");
                    ErrorsFound = true;
                }
            } else {
                MultizoneZoneData(i).VentSchNum = GetScheduleIndex(MultizoneZoneData(i).VentSchName);
                if (MultizoneZoneData(i).VentSchNum > 0) {
                    ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(3) + " not required, when " + cAlphaFields(2) +
                                     " is neither Temperature nor Enthalpy.");
                    ShowContinueError(".." + cAlphaFields(3) + " specified = " + MultizoneZoneData(i).VentSchName);
                    ShowContinueError("..for " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName + "\", with " + cAlphaFields(2) + " = \"" +
                                      MultizoneZoneData(i).VentControl + "\"");
                    MultizoneZoneData(i).VentSchNum = 0;
                    MultizoneZoneData(i).VentSchName = BlankString;
                }
            }
            if (MultizoneZoneData(i).OpenFactor > 1.0 || MultizoneZoneData(i).OpenFactor < 0.0) {
                ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(1) + " is out of range [0.0,1.0]");
                ShowContinueError("..Input value = " + RoundSigDigits(MultizoneZoneData(i).OpenFactor, 2) + ", Value will be set to 1.0");
                MultizoneZoneData(i).OpenFactor = 1.0;
            }

            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(MultizoneZoneData(i).VentControl));
                if (SELECT_CASE_var == "TEMPERATURE") { // checks on Temperature control
                    if (MultizoneZoneData(i).LowValueTemp < 0.0) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(2) + " < 0.0");
                        ShowContinueError("..Input value = " + RoundSigDigits(MultizoneZoneData(i).LowValueTemp, 1) + ", Value will be set to 0.0");
                        ShowContinueError("..for " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName);
                        MultizoneZoneData(i).LowValueTemp = 0.0;
                    }
                    if (MultizoneZoneData(i).LowValueTemp >= 100.0) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(2) + " >= 100.0");
                        ShowContinueError("..Input value = " + RoundSigDigits(MultizoneZoneData(i).LowValueTemp, 1) + ", Value will be reset to 0.0");
                        ShowContinueError("..for " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName);
                        MultizoneZoneData(i).LowValueTemp = 0.0;
                    }
                    if (MultizoneZoneData(i).UpValueTemp <= MultizoneZoneData(i).LowValueTemp) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(3) + " <= " + cNumericFields(2));
                        ShowContinueError("..Input value for " + cNumericFields(3) + " = " + RoundSigDigits(MultizoneZoneData(i).UpValueTemp, 1) +
                                          ", Value will be reset to 100.0");
                        ShowContinueError("..for " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName);
                        MultizoneZoneData(i).UpValueTemp = 100.0;
                    }

                } else if (SELECT_CASE_var == "ENTHALPY") { // checks for Enthalpy control
                    if (MultizoneZoneData(i).LowValueEnth < 0.0) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(4) + " < 0.0");
                        ShowContinueError("..Input value = " + RoundSigDigits(MultizoneZoneData(i).LowValueEnth, 1) + ", Value will be reset to 0.0");
                        ShowContinueError("..for " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName);
                        MultizoneZoneData(i).LowValueEnth = 0.0;
                    }
                    if (MultizoneZoneData(i).LowValueEnth >= 300000.0) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(4) + " >= 300000.0");
                        ShowContinueError("..Input value = " + RoundSigDigits(MultizoneZoneData(i).LowValueEnth, 1) +
                                          ", Value will be reset to 0.0.");
                        ShowContinueError("..for " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName);
                        MultizoneZoneData(i).LowValueEnth = 0.0;
                    }
                    if (MultizoneZoneData(i).UpValueEnth <= MultizoneZoneData(i).LowValueEnth) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " object, " + cNumericFields(5) + " <= " + cNumericFields(4));
                        ShowContinueError("..Input value for " + cNumericFields(5) + "= " + RoundSigDigits(MultizoneZoneData(i).UpValueEnth, 1) +
                                          ", Value will be reset to 300000.0");
                        ShowContinueError("..for " + cAlphaFields(1) + " = \"" + MultizoneZoneData(i).ZoneName);
                        MultizoneZoneData(i).UpValueEnth = 300000.0;
                    }
                } else if (SELECT_CASE_var == "ASHRAE55ADAPTIVE") {
                    // Check that for the given zone, there is a people object for which ASHRAE 55 calculations are carried out
                    ZoneNum = MultizoneZoneData(i).ZoneNum;
                    for (j = 1; j <= TotPeople; ++j) {
                        if (ZoneNum == People(j).ZonePtr && People(j).AdaptiveASH55) {
                            MultizoneZoneData(i).ASH55PeopleInd = j;
                        }
                    }
                    if (MultizoneZoneData(i).ASH55PeopleInd == 0) {
                        ShowFatalError("ASHRAE55 ventilation control for zone " + MultizoneZoneData(i).ZoneName +
                                       " requires a people object with respective model calculations.");
                    }
                } else if (SELECT_CASE_var == "CEN15251ADAPTIVE") {
                    // Check that for the given zone, there is a people object for which CEN-15251 calculations are carried out
                    ZoneNum = MultizoneZoneData(i).ZoneNum;
                    for (j = 1; j <= TotPeople; ++j) {
                        if (ZoneNum == People(j).ZonePtr && People(j).AdaptiveCEN15251) {
                            MultizoneZoneData(i).CEN15251PeopleInd = j;
                            break;
                        }
                    }
                    if (MultizoneZoneData(i).CEN15251PeopleInd == 0) {
                        ShowFatalError("CEN15251 ventilation control for zone " + MultizoneZoneData(i).ZoneName +
                                       " requires a people object with respective model calculations.");
                    }
                } else {
                }
            }
        }

        // *** Read AirflowNetwork external node
        if (AirflowNetworkSimu.iWPCCntr == iWPCCntr_Input) {
            // Wind coefficient == Surface-Average does not need inputs of external nodes
            AirflowNetworkNumOfExtNode = inputProcessor->getNumObjectsFound("AirflowNetwork:MultiZone:ExternalNode");
            if (AnyLocalEnvironmentsInModel) {
                AirflowNetworkNumOfOutAirNode = inputProcessor->getNumObjectsFound("OutdoorAir:Node");
                AirflowNetworkNumOfExtNode += AirflowNetworkNumOfOutAirNode;
            }

            if (AirflowNetworkNumOfExtNode > 0) {
                MultizoneExternalNodeData.allocate(AirflowNetworkNumOfExtNode);
                CurrentModuleObject = "AirflowNetwork:MultiZone:ExternalNode";
                for (i = 1; i <= AirflowNetworkNumOfExtNode - AirflowNetworkNumOfOutAirNode; ++i) {
                    inputProcessor->getObjectItem(CurrentModuleObject,
                                                  i,
                                                  Alphas,
                                                  NumAlphas,
                                                  Numbers,
                                                  NumNumbers,
                                                  IOStatus,
                                                  lNumericBlanks,
                                                  lAlphaBlanks,
                                                  cAlphaFields,
                                                  cNumericFields);
                    UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
                    MultizoneExternalNodeData(i).Name = Alphas(1);    // Name of external node
                    MultizoneExternalNodeData(i).height = Numbers(1); // Nodal height
                    if (UtilityRoutines::SameString(AirflowNetworkSimu.HeightOption, "ExternalNode") && lNumericBlanks(1)) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " object =" + Alphas(1) + ". The input of " + cNumericFields(1) +
                                         " is required, but a blank is found.");
                        ShowContinueError("The default value is assigned as " + RoundSigDigits(Numbers(1), 1));
                    }
                    MultizoneExternalNodeData(i).ExtNum = AirflowNetworkNumOfZones + i;          // External node number
                    MultizoneExternalNodeData(i).curve = CurveManager::GetCurveIndex(Alphas(2)); // Wind pressure curve
                    if (MultizoneExternalNodeData(i).curve == 0) {
                        ShowSevereError(RoutineName + "Invalid " + cAlphaFields(2) + "=" + Alphas(2));
                        ShowContinueError("Entered in " + CurrentModuleObject + '=' + Alphas(1));
                        ErrorsFound = true;
                    }
                    if (NumAlphas >= 3 && !lAlphaBlanks(3)) { // Symmetric curve
                        if (UtilityRoutines::SameString(Alphas(3), "Yes")) {
                            MultizoneExternalNodeData(i).symmetricCurve = true;
                        } else if (!UtilityRoutines::SameString(Alphas(3), "No")) {
                            ShowWarningError(RoutineName + CurrentModuleObject + " object, Invalid input " + cAlphaFields(3) + " = " + Alphas(3));
                            ShowContinueError("The default value is assigned as No.");
                        }
                    }
                    if (NumAlphas == 4 && !lAlphaBlanks(4)) { // Relative or absolute wind angle
                        if (UtilityRoutines::SameString(Alphas(4), "Relative")) {
                            MultizoneExternalNodeData(i).useRelativeAngle = true;
                        } else if (!UtilityRoutines::SameString(Alphas(4), "Absolute")) {
                            ShowWarningError(RoutineName + CurrentModuleObject + " object, Invalid input " + cAlphaFields(4) + " = " + Alphas(4));
                            ShowContinueError("The default value is assigned as Absolute.");
                        }
                    }
                }
                CurrentModuleObject = "OutdoorAir:Node";
                for (i = AirflowNetworkNumOfExtNode - AirflowNetworkNumOfOutAirNode + 1; i <= AirflowNetworkNumOfExtNode; ++i) {
                    inputProcessor->getObjectItem(CurrentModuleObject,
                                                  i - (AirflowNetworkNumOfExtNode - AirflowNetworkNumOfOutAirNode),
                                                  Alphas,
                                                  NumAlphas,
                                                  Numbers,
                                                  NumNumbers,
                                                  IOStatus,
                                                  lNumericBlanks,
                                                  lAlphaBlanks,
                                                  cAlphaFields,
                                                  cNumericFields);
                    UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
                    // HACK: Need to verify name is unique between "OutdoorAir:Node" and "AirflowNetwork:MultiZone:ExternalNode"

                    if (NumAlphas > 5 && !lAlphaBlanks(6)) { // Wind pressure curve
                        MultizoneExternalNodeData(i).curve = GetCurveIndex(Alphas(6));
                        if (MultizoneExternalNodeData(i).curve == 0) {
                            ShowSevereError(RoutineName + "Invalid " + cAlphaFields(6) + "=" + Alphas(6));
                            ShowContinueError("Entered in " + CurrentModuleObject + '=' + Alphas(1));
                            ErrorsFound = true;
                        }
                    }

                    if (NumAlphas > 6 && !lAlphaBlanks(7)) { // Symmetric curve
                        if (UtilityRoutines::SameString(Alphas(7), "Yes")) {
                            MultizoneExternalNodeData(i).symmetricCurve = true;
                        } else if (!UtilityRoutines::SameString(Alphas(7), "No")) {
                            ShowWarningError(RoutineName + CurrentModuleObject + " object, Invalid input " + cAlphaFields(7) + " = " + Alphas(7));
                            ShowContinueError("The default value is assigned as No.");
                        }
                    }

                    if (NumAlphas > 7 && !lAlphaBlanks(8)) { // Relative or absolute wind angle
                        if (UtilityRoutines::SameString(Alphas(8), "Relative")) {
                            MultizoneExternalNodeData(i).useRelativeAngle = true;
                        } else if (!UtilityRoutines::SameString(Alphas(8), "Absolute")) {
                            ShowWarningError(RoutineName + CurrentModuleObject + " object, Invalid input " + cAlphaFields(8) + " = " + Alphas(8));
                            ShowContinueError("The default value is assigned as Absolute.");
                        }
                    }

                    MultizoneExternalNodeData(i).Name = Alphas(1); // Name of external node
                    NodeNum = GetOnlySingleNode(Alphas(1),
                                                ErrorsFound,
                                                CurrentModuleObject,
                                                "AirflowNetwork:Multizone:Surface",
                                                NodeType_Air,
                                                NodeConnectionType_Inlet,
                                                1,
                                                ObjectIsParent);
                    MultizoneExternalNodeData(i).OutAirNodeNum = NodeNum;               // Name of outdoor air node
                    MultizoneExternalNodeData(i).height = Node(NodeNum).Height;         // Nodal height
                    MultizoneExternalNodeData(i).ExtNum = AirflowNetworkNumOfZones + i; // External node number
                }
            } else {
                ShowSevereError(RoutineName + "An " + CurrentModuleObject +
                                " object is required but not found when Wind Pressure Coefficient Type = Input.");
                ErrorsFound = true;
            }
        }

        // *** Read AirflowNetwork element data
        ErrorsFound = ErrorsFound || !getAirflowElementInput();

        // *** Read AirflowNetwork simulation surface data
        CurrentModuleObject = "AirflowNetwork:MultiZone:Surface";
        AirflowNetworkNumOfSurfaces = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (AirflowNetworkNumOfSurfaces > 0) {
            MultizoneSurfaceData.allocate(AirflowNetworkNumOfSurfaces);
            for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              i,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);
                UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
                MultizoneSurfaceData(i).SurfName = Alphas(1);    // Name of Associated EnergyPlus surface
                MultizoneSurfaceData(i).OpeningName = Alphas(2); // Name of crack or opening component,
                // either simple or detailed large opening, or crack
                MultizoneSurfaceData(i).ExternalNodeName = Alphas(3); // Name of external node, but not used at WPC="INPUT"
                if (UtilityRoutines::FindItemInList(Alphas(3), MultizoneExternalNodeData) &&
                    MultizoneExternalNodeData(UtilityRoutines::FindItemInList(Alphas(3), MultizoneExternalNodeData)).curve == 0) {
                    ShowSevereError(RoutineName + "Invalid " + cAlphaFields(3) + "=" + Alphas(3));
                    ShowContinueError(
                        "A valid wind pressure coefficient curve name is required but not found when Wind Pressure Coefficient Type = Input.");
                    ErrorsFound = true;
                }
                MultizoneSurfaceData(i).Factor = Numbers(1); // Crack Actual Value or Window Open Factor for Ventilation
                if (MultizoneSurfaceData(i).Factor > 1.0 || MultizoneSurfaceData(i).Factor <= 0.0) {
                    ShowWarningError(RoutineName + CurrentModuleObject + " object=" + MultizoneSurfaceData(i).SurfName + ", " + cNumericFields(1) +
                                     " is out of range (0.0,1.0]");
                    ShowContinueError("..Input value = " + RoundSigDigits(MultizoneSurfaceData(i).Factor, 2) + ", Value will be set to 1.0");
                    MultizoneSurfaceData(i).Factor = 1.0;
                }
                // Get input of ventilation control and associated data
                if (NumAlphas >= 4) {
                    // Ventilation Control Mode: "TEMPERATURE", "ENTHALPY",
                    //   "CONSTANT", "ZONELEVEL", "NOVENT", "ADJACENTTEMPERATURE",
                    //   or "ADJACENTENTHALPY"
                    if (!lAlphaBlanks(4)) MultizoneSurfaceData(i).VentControl = Alphas(4);
                    // Name of ventilation temperature control schedule
                    if (!lAlphaBlanks(5)) MultizoneSurfaceData(i).VentSchName = Alphas(5);
                    {
                        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(MultizoneSurfaceData(i).VentControl));
                        if (SELECT_CASE_var == "TEMPERATURE") {
                            MultizoneSurfaceData(i).VentSurfCtrNum = VentCtrNum_Temp;
                            MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "ENTHALPY") {
                            MultizoneSurfaceData(i).VentSurfCtrNum = VentCtrNum_Enth;
                            MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "CONSTANT") {
                            MultizoneSurfaceData(i).VentSurfCtrNum = VentCtrNum_Const;
                            MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "ASHRAE55ADAPTIVE") {
                            MultizoneSurfaceData(i).VentSurfCtrNum = VentCtrNum_ASH55;
                            MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "CEN15251ADAPTIVE") {
                            MultizoneSurfaceData(i).VentSurfCtrNum = VentCtrNum_CEN15251;
                            MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "NOVENT") {
                            MultizoneSurfaceData(i).VentSurfCtrNum = VentCtrNum_Novent;
                            MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "ZONELEVEL") {
                            MultizoneSurfaceData(i).VentSurfCtrNum = VentCtrNum_ZoneLevel;
                            MultizoneSurfaceData(i).IndVentControl = false;
                        } else if (SELECT_CASE_var == "ADJACENTTEMPERATURE") {
                            MultizoneSurfaceData(i).VentSurfCtrNum = VentCtrNum_AdjTemp;
                            MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "ADJACENTENTHALPY") {
                            MultizoneSurfaceData(i).VentSurfCtrNum = VentCtrNum_AdjEnth;
                            MultizoneSurfaceData(i).IndVentControl = true;
                        } else {
                            ShowSevereError(RoutineName + CurrentModuleObject + " object, Invalid " + cAlphaFields(4));
                            ShowContinueError(".." + cAlphaFields(1) + " = " + MultizoneSurfaceData(i).SurfName + ", Specified " + cAlphaFields(4) +
                                              " = " + Alphas(4));
                            ShowContinueError("..The valid choices are \"Temperature\", \"Enthalpy\", \"Constant\", \"NoVent\", \"ZoneLevel\", "
                                              "\"AdjancentTemperature\" or \"AdjacentEnthalpy\"");
                            ErrorsFound = true;
                        }
                    }
                }
                MultizoneSurfaceData(i).ModulateFactor = Numbers(2); // Limit Value on Multiplier for Modulating Venting Open Factor
                MultizoneSurfaceData(i).LowValueTemp = Numbers(3);   // Lower temperature value for modulation of temperature control
                MultizoneSurfaceData(i).UpValueTemp = Numbers(4);    // Upper temperature value for modulation of temperature control
                MultizoneSurfaceData(i).LowValueEnth = Numbers(5);   // Lower Enthalpy value for modulation of Enthalpy control
                MultizoneSurfaceData(i).UpValueEnth = Numbers(6);    // Lower Enthalpy value for modulation of Enthalpy control
                if (MultizoneSurfaceData(i).VentSurfCtrNum < 4 || MultizoneSurfaceData(i).VentSurfCtrNum == VentCtrNum_AdjTemp ||
                    MultizoneSurfaceData(i).VentSurfCtrNum == VentCtrNum_AdjEnth) {
                    if (!lAlphaBlanks(6)) {
                        MultizoneSurfaceData(i).VentingSchName = Alphas(6); // Name of ventilation availability schedule
                    }
                }
                if (!lAlphaBlanks(7)) {
                    MultizoneSurfaceData(i).OccupantVentilationControlName = Alphas(7);
                    MultizoneSurfaceData(i).OccupantVentilationControlNum =
                        UtilityRoutines::FindItemInList(MultizoneSurfaceData(i).OccupantVentilationControlName, OccupantVentilationControl);
                    if (MultizoneSurfaceData(i).OccupantVentilationControlNum == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(7) +
                                        " not found = " + MultizoneSurfaceData(i).OccupantVentilationControlName);
                        ShowContinueError("..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                }
                // Get data of polygonal surface
                if (!lAlphaBlanks(8)) {
                    if (Alphas(8) == "POLYGONHEIGHT") {
                        MultizoneSurfaceData(i).EquivRecMethod = EquivRec_Height;
                    } else if (Alphas(8) == "BASESURFACEASPECTRATIO") {
                        MultizoneSurfaceData(i).EquivRecMethod = EquivRec_BaseAspectRatio;
                    } else if (Alphas(8) == "USERDEFINEDASPECTRATIO") {
                        MultizoneSurfaceData(i).EquivRecMethod = EquivRec_UserAspectRatio;
                    } else {
                        ShowSevereError(RoutineName + CurrentModuleObject + " object, Invalid " + cAlphaFields(8));
                        ShowContinueError(".." + cAlphaFields(1) + " = " + MultizoneSurfaceData(i).SurfName + ", Specified " + cAlphaFields(8) +
                                          " = " + Alphas(8));
                        ShowContinueError("..The valid choices are \"PolygonHeight\", \"BaseSurfaceAspectRatio\", or \"UserDefinedAspectRatio\"");
                        ErrorsFound = true;
                    }
                } else {
                    MultizoneSurfaceData(i).EquivRecMethod = EquivRec_Height;
                }
                if (!lNumericBlanks(7)) {
                    MultizoneSurfaceData(i).EquivRecUserAspectRatio = Numbers(7);
                } else {
                    MultizoneSurfaceData(i).EquivRecUserAspectRatio = 1.0;
                }
            }
        } else {
            ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object is required but not found.");
            ErrorsFound = true;
        }

        // ==> Validate AirflowNetwork simulation surface data
        NumOfExtNodes = 0;
        for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
            // Check a valid surface defined earlier
            MultizoneSurfaceData(i).SurfNum = UtilityRoutines::FindItemInList(MultizoneSurfaceData(i).SurfName, Surface);
            if (MultizoneSurfaceData(i).SurfNum == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + " object, Invalid " + cAlphaFields(1) +
                                " given = " + MultizoneSurfaceData(i).SurfName);
                ErrorsFound = true;
                continue;
            }
            if (!Surface(MultizoneSurfaceData(i).SurfNum).HeatTransSurf) {
                ShowSevereError(RoutineName + CurrentModuleObject + " object");
                ShowContinueError("..The surface specified must be a heat transfer surface. Invalid " + cAlphaFields(1) + " = " +
                                  MultizoneSurfaceData(i).SurfName);
                ErrorsFound = true;
                continue;
            }
            // Ensure an interior surface does not face itself
            if (Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond >= 1) {
                // Check the surface is a subsurface or not
                if (Surface(MultizoneSurfaceData(i).SurfNum).BaseSurf == MultizoneSurfaceData(i).SurfNum) {
                    if (MultizoneSurfaceData(i).SurfNum == Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " object");
                        ShowContinueError("..The surface facing itself is not allowed. Invalid " + cAlphaFields(1) + " = " +
                                          MultizoneSurfaceData(i).SurfName);
                        ErrorsFound = true;
                    }
                } else {
                    if (Surface(MultizoneSurfaceData(i).SurfNum).BaseSurf ==
                        Surface(Surface(MultizoneSurfaceData(i).SurfNum).BaseSurf).ExtBoundCond) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " object");
                        ShowContinueError("..The base surface facing itself is not allowed. Invalid " + cAlphaFields(1) + " = " +
                                          MultizoneSurfaceData(i).SurfName);
                        ErrorsFound = true;
                    }
                }
            }
            // Ensure zones defined in inside and outside environment are used in the object of AIRFLOWNETWORK:MULTIZONE:ZONE
            found = false;
            n = Surface(MultizoneSurfaceData(i).SurfNum).Zone;
            for (j = 1; j <= AirflowNetworkNumOfZones; ++j) {
                if (MultizoneZoneData(j).ZoneNum == n) {
                    found = true;
                    break;
                }
            }
            // find a surface geometry
            MultizoneSurfaceData(i).Height = Surface(MultizoneSurfaceData(i).SurfNum).Height;
            MultizoneSurfaceData(i).Width = Surface(MultizoneSurfaceData(i).SurfNum).Width;
            MultizoneSurfaceData(i).CHeight = Surface(MultizoneSurfaceData(i).SurfNum).Centroid.z;
            if (found) {
                MultizoneSurfaceData(i).NodeNums[0] = j;
            } else {
                ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(1) + " = " + MultizoneSurfaceData(i).SurfName);
                ShowContinueError("..Zone for inside surface must be defined in a AirflowNetwork:MultiZone:Zone object.  Could not find Zone = " +
                                  Zone(Surface(MultizoneSurfaceData(i).SurfNum).Zone).Name);
                ShowFatalError(RoutineName + "Errors found getting inputs. Previous error(s) cause program termination.");
            }

            // Calculate equivalent width and height
            if (Surface(MultizoneSurfaceData(i).SurfNum).Sides != 4) {
                MultizoneSurfaceData(i).NonRectangular = true;
                if (MultizoneSurfaceData(i).EquivRecMethod == EquivRec_Height) {
                    if (Surface(MultizoneSurfaceData(i).SurfNum).Tilt < 1.0 ||
                        Surface(MultizoneSurfaceData(i).SurfNum).Tilt > 179.0) { // horizontal surface
                        // check base surface shape
                        if (Surface(Surface(MultizoneSurfaceData(i).SurfNum).BaseSurf).Sides == 4) {
                            baseratio = Surface(Surface(MultizoneSurfaceData(i).SurfNum).BaseSurf).Width /
                                        Surface(Surface(MultizoneSurfaceData(i).SurfNum).BaseSurf).Height;
                            MultizoneSurfaceData(i).Width = sqrt(Surface(MultizoneSurfaceData(i).SurfNum).Area * baseratio);
                            MultizoneSurfaceData(i).Height = Surface(MultizoneSurfaceData(i).SurfNum).Area / MultizoneSurfaceData(i).Width;
                            if (DisplayExtraWarnings) {
                                ShowWarningError(RoutineName + CurrentModuleObject + " object = " + MultizoneSurfaceData(i).SurfName);
                                ShowContinueError("The entered choice of Equivalent Rectangle Method is PolygonHeight. This choice is not valid for "
                                                  "a horizontal surface.");
                                ShowContinueError("The BaseSurfaceAspectRatio choice is used. Simulation continues.");
                            }
                        } else {
                            MultizoneSurfaceData(i).Width =
                                sqrt(Surface(MultizoneSurfaceData(i).SurfNum).Area * MultizoneSurfaceData(i).EquivRecUserAspectRatio);
                            MultizoneSurfaceData(i).Height = Surface(MultizoneSurfaceData(i).SurfNum).Area / MultizoneSurfaceData(i).Width;
                            // add warning
                            if (DisplayExtraWarnings) {
                                ShowWarningError(RoutineName + CurrentModuleObject + " object = " + MultizoneSurfaceData(i).SurfName);
                                ShowContinueError("The entered choice of Equivalent Rectangle Method is PolygonHeight. This choice is not valid for "
                                                  "a horizontal surface with a polygonal base surface.");
                                ShowContinueError("The default aspect ratio at 1 is used. Simulation continues.");
                            }
                        }
                    } else {
                        minHeight = min(Surface(MultizoneSurfaceData(i).SurfNum).Vertex(1).z, Surface(MultizoneSurfaceData(i).SurfNum).Vertex(2).z);
                        maxHeight = max(Surface(MultizoneSurfaceData(i).SurfNum).Vertex(1).z, Surface(MultizoneSurfaceData(i).SurfNum).Vertex(2).z);
                        for (j = 3; j <= Surface(MultizoneSurfaceData(i).SurfNum).Sides; ++j) {
                            minHeight = min(
                                minHeight,
                                min(Surface(MultizoneSurfaceData(i).SurfNum).Vertex(j - 1).z, Surface(MultizoneSurfaceData(i).SurfNum).Vertex(j).z));
                            maxHeight = max(
                                maxHeight,
                                max(Surface(MultizoneSurfaceData(i).SurfNum).Vertex(j - 1).z, Surface(MultizoneSurfaceData(i).SurfNum).Vertex(j).z));
                        }
                        if (maxHeight > minHeight) {
                            MultizoneSurfaceData(i).Height = maxHeight - minHeight;
                            MultizoneSurfaceData(i).Width = Surface(MultizoneSurfaceData(i).SurfNum).Area / (maxHeight - minHeight);
                        }
                    }
                }
                if (MultizoneSurfaceData(i).EquivRecMethod == EquivRec_BaseAspectRatio) {
                    if (Surface(Surface(MultizoneSurfaceData(i).SurfNum).BaseSurf).Sides == 4) {
                        baseratio = Surface(Surface(MultizoneSurfaceData(i).SurfNum).BaseSurf).Width /
                                    Surface(Surface(MultizoneSurfaceData(i).SurfNum).BaseSurf).Height;
                        MultizoneSurfaceData(i).Width = sqrt(Surface(MultizoneSurfaceData(i).SurfNum).Area * baseratio);
                        MultizoneSurfaceData(i).Height = Surface(MultizoneSurfaceData(i).SurfNum).Area / MultizoneSurfaceData(i).Width;
                    } else {
                        minHeight = min(Surface(MultizoneSurfaceData(i).SurfNum).Vertex(1).z, Surface(MultizoneSurfaceData(i).SurfNum).Vertex(2).z);
                        maxHeight = max(Surface(MultizoneSurfaceData(i).SurfNum).Vertex(1).z, Surface(MultizoneSurfaceData(i).SurfNum).Vertex(2).z);
                        for (j = 3; j <= Surface(MultizoneSurfaceData(i).SurfNum).Sides; ++j) {
                            minHeight = min(
                                minHeight,
                                min(Surface(MultizoneSurfaceData(i).SurfNum).Vertex(j - 1).z, Surface(MultizoneSurfaceData(i).SurfNum).Vertex(j).z));
                            maxHeight = max(
                                maxHeight,
                                max(Surface(MultizoneSurfaceData(i).SurfNum).Vertex(j - 1).z, Surface(MultizoneSurfaceData(i).SurfNum).Vertex(j).z));
                        }
                        if (maxHeight > minHeight) {
                            MultizoneSurfaceData(i).Height = maxHeight - minHeight;
                            MultizoneSurfaceData(i).Width = Surface(MultizoneSurfaceData(i).SurfNum).Area / (maxHeight - minHeight);
                            // add warning
                            if (DisplayExtraWarnings) {
                                ShowWarningError(RoutineName + CurrentModuleObject + " object = " + MultizoneSurfaceData(i).SurfName);
                                ShowContinueError("The entered choice of Equivalent Rectangle Method is BaseSurfaceAspectRatio. This choice is not "
                                                  "valid for a polygonal base surface.");
                                ShowContinueError("The PolygonHeight choice is used. Simulation continues.");
                            }
                        } else {
                            MultizoneSurfaceData(i).Width =
                                sqrt(Surface(MultizoneSurfaceData(i).SurfNum).Area * MultizoneSurfaceData(i).EquivRecUserAspectRatio);
                            MultizoneSurfaceData(i).Height = Surface(MultizoneSurfaceData(i).SurfNum).Area / MultizoneSurfaceData(i).Width;
                            // add warning
                            if (DisplayExtraWarnings) {
                                ShowWarningError(RoutineName + CurrentModuleObject + " object = " + MultizoneSurfaceData(i).SurfName);
                                ShowContinueError("The entered choice of Equivalent Rectangle Method is BaseSurfaceAspectRatio. This choice is not "
                                                  "valid for a horizontal surface with a polygonal base surface.");
                                ShowContinueError("The default aspect ratio at 1 is used. Simulation continues.");
                            }
                        }
                    }
                }
                if (MultizoneSurfaceData(i).EquivRecMethod == EquivRec_UserAspectRatio) {
                    MultizoneSurfaceData(i).Width =
                        sqrt(Surface(MultizoneSurfaceData(i).SurfNum).Area * MultizoneSurfaceData(i).EquivRecUserAspectRatio);
                    MultizoneSurfaceData(i).Height = Surface(MultizoneSurfaceData(i).SurfNum).Area / MultizoneSurfaceData(i).Width;
                }
            }

            // Get the number of external surfaces
            if (Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond == ExternalEnvironment ||
                (Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt &&
                 Surface(MultizoneSurfaceData(i).SurfNum).ExtWind)) {
                ++AirflowNetworkNumOfExtSurfaces;
            }

            // Outside face environment
            if (AirflowNetworkSimu.iWPCCntr == iWPCCntr_Input) {
                n = Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond;
                if (n == ExternalEnvironment || (n == OtherSideCoefNoCalcExt && Surface(MultizoneSurfaceData(i).SurfNum).ExtWind)) {
                    ++NumOfExtNodes;
                    if (AirflowNetworkNumOfExtNode > 0) {
                        found = false;
                        for (j = 1; j <= AirflowNetworkNumOfExtNode; ++j) {
                            if (UtilityRoutines::SameString(MultizoneSurfaceData(i).ExternalNodeName, MultizoneExternalNodeData(j).Name)) {
                                MultizoneSurfaceData(i).NodeNums[1] = MultizoneExternalNodeData(j).ExtNum;
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            ShowSevereError(RoutineName + CurrentModuleObject + ": Invalid " + cAlphaFields(3) + " = " +
                                            MultizoneSurfaceData(i).ExternalNodeName);
                            ShowContinueError("A valid " + cAlphaFields(3) + " is required when Wind Pressure Coefficient Type = Input");
                            ErrorsFound = true;
                        }
                    } else {
                        //          MultizoneSurfaceData(i)%NodeNums[1] = AirflowNetworkNumOfZones+NumOfExtNodes
                    }
                    continue;
                } else {
                    if (n < ExternalEnvironment && !(Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt &&
                                                     Surface(MultizoneSurfaceData(i).SurfNum).ExtWind)) {
                        ShowSevereError(RoutineName + CurrentModuleObject + ": Invalid " + cAlphaFields(1) + " = " +
                                        MultizoneSurfaceData(i).SurfName);
                        ShowContinueError("This type of surface (has ground, etc exposure) cannot be used in the AiflowNetwork model.");
                        ErrorsFound = true;
                    }
                }
                found = false;
                for (j = 1; j <= AirflowNetworkNumOfZones; ++j) {
                    if (MultizoneZoneData(j).ZoneNum == Surface(n).Zone) {
                        found = true;
                        break;
                    }
                }
                if (found) {
                    MultizoneSurfaceData(i).NodeNums[1] = j;
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(1) + " = " + MultizoneSurfaceData(i).SurfName);
                    ShowContinueError(
                        "..Zone for outside surface must be defined in a AirflowNetwork:MultiZone:Zone object.  Could not find Zone = " +
                        Zone(Surface(MultizoneSurfaceData(i).SurfNum).Zone).Name);
                    ErrorsFound = true;
                    continue;
                }
            }
            if (UtilityRoutines::SameString(AirflowNetworkSimu.WPCCntr, "SurfaceAverageCalculation")) {
                n = Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond;
                if (n >= 1) { // exterior boundary condition is a surface
                    found = false;
                    for (j = 1; j <= AirflowNetworkNumOfZones; ++j) {
                        if (MultizoneZoneData(j).ZoneNum == Surface(n).Zone) {
                            found = true;
                            break;
                        }
                    }
                    if (found) {
                        MultizoneSurfaceData(i).NodeNums[1] = j;
                    } else {
                        ShowSevereError(RoutineName + CurrentModuleObject + " = " + MultizoneSurfaceData(i).SurfName);
                        ShowContinueError("An adjacent zone = " + Zone(Surface(n).Zone).Name + " is not described in AIRFLOWNETWORK:MULTIZONE:ZONE");
                        ErrorsFound = true;
                        continue;
                    }
                }
            }
            if (!(Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond == -2 && Surface(MultizoneSurfaceData(i).SurfNum).ExtWind)) {
                if (MultizoneSurfaceData(i).NodeNums[1] == 0 && Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond < 0) {
                    ShowSevereError(RoutineName + CurrentModuleObject + " = " + MultizoneSurfaceData(i).SurfName);
                    ShowContinueError("Outside boundary condition and object are " +
                                      cExtBoundCondition(Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond) + " and " +
                                      Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCondName + ".");
                    ShowContinueError("The outside boundary condition must be exposed to either the outside or an adjacent zone.");
                    ErrorsFound = true;
                    continue;
                }
            }
        }

        // write outputs in eio file
        found = true;
        for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
            if (MultizoneSurfaceData(i).NonRectangular) {
                if (found) {
                    print(outputFiles.eio,
                          "! <AirflowNetwork Model:Equivalent Rectangle Surface>, Name, Equivalent Height {{m}}, Equivalent Width {{m}} AirflowNetwork "
                          "Model:Equivalent Rectangle\n");
                    found = false;
                }
                print(outputFiles.eio,
                      "AirflowNetwork Model:Equivalent Rectangle Surface, {}, {:.2R},{:.2R}\n",
                      MultizoneSurfaceData(i).SurfName,
                      MultizoneSurfaceData(i).Height,
                      MultizoneSurfaceData(i).Width);
            }
        }

        // Validate adjacent temperature and Enthalpy control for an interior surface only
        for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
            if (MultizoneSurfaceData(i).VentSurfCtrNum == VentCtrNum_AdjTemp) {
                if (!(Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond >= 1)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(1) + " = " + MultizoneSurfaceData(i).SurfName);
                    ShowContinueError("..AdjacentTemperature venting control must be defined for an interzone surface.");
                    ErrorsFound = true;
                }
            }
            if (MultizoneSurfaceData(i).VentSurfCtrNum == VentCtrNum_AdjEnth) {
                if (!(Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond >= 1)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + " object, " + cAlphaFields(1) + " = " + MultizoneSurfaceData(i).SurfName);
                    ShowContinueError("..AdjacentEnthalpy venting control must be defined for an interzone surface.");
                    ErrorsFound = true;
                }
            }
        }

        // Ensure the number of external node = the number of external surface with HeightOption choice = OpeningHeight
        if (UtilityRoutines::SameString(AirflowNetworkSimu.HeightOption, "OpeningHeight") && AirflowNetworkSimu.iWPCCntr == iWPCCntr_Input) {
            if (AirflowNetworkNumOfExtSurfaces != AirflowNetworkNumOfExtNode) {
                ShowSevereError(RoutineName +
                                "When the choice of Height Selection for Local Wind Speed Calculation is OpeningHeight, the number of external "
                                "surfaces defined in " +
                                CurrentModuleObject + " objects ");
                ShowContinueError("has to be equal to the number of AirflowNetwork:MultiZone:ExternalNode objects.");
                ShowContinueError("The entered number of external nodes is " + RoundSigDigits(AirflowNetworkNumOfExtNode) +
                                  ". The entered number of external surfaces is " + RoundSigDigits(AirflowNetworkNumOfExtSurfaces) + '.');
                ErrorsFound = true;
            }
        }

        // Read AirflowNetwork simulation detailed openings
        // Moved into getAirflowElementInput

        // Validate opening component and assign opening dimension
        if (AirflowNetworkNumOfDetOpenings > 0) {
            for (i = 1; i <= AirflowNetworkNumOfDetOpenings; ++i) {
                found = false;
                for (j = 1; j <= AirflowNetworkNumOfSurfaces; ++j) {
                    if (MultizoneCompDetOpeningData(i).Name == MultizoneSurfaceData(j).OpeningName) {
                        //           MultizoneCompDetOpeningData(i)%Width = Surface(MultizoneSurfaceData(j)%SurfNum)%Width
                        //           MultizoneCompDetOpeningData(i)%Height = Surface(MultizoneSurfaceData(j)%SurfNum)%Height
                        found = true;
                    }
                }
            }
        }

        // Read AirflowNetwork simulation simple openings
        // Moved into getAirflowElementInput

        // Read AirflowNetwork simulation horizontal openings
        // Moved into getAirflowElementInput

        // Check status of control level for each surface with an opening
        j = 0;
        CurrentModuleObject = "AirflowNetwork:MultiZone:Surface";
        for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
            if (MultizoneSurfaceData(i).SurfNum == 0) continue;
            if (AirflowNetworkNumOfDetOpenings > 0)
                j = UtilityRoutines::FindItemInList(MultizoneSurfaceData(i).OpeningName, MultizoneCompDetOpeningData);
            if (j == 0 && AirflowNetworkNumOfSimOpenings > 0)
                j = UtilityRoutines::FindItemInList(MultizoneSurfaceData(i).OpeningName, MultizoneCompSimpleOpeningData);
            // Obtain schedule number and check surface shape
            if (j > 0) {
                if (Surface(MultizoneSurfaceData(i).SurfNum).Sides == 3) {
                    ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + MultizoneSurfaceData(i).SurfName + "\".");
                    ShowContinueError(
                        "The opening is a Triangular subsurface. A rectangular subsurface will be used with equivalent width and height.");
                }
                if (!MultizoneSurfaceData(i).VentingSchName.empty()) {
                    MultizoneSurfaceData(i).VentingSchNum = GetScheduleIndex(MultizoneSurfaceData(i).VentingSchName);
                    if (MultizoneSurfaceData(i).VentingSchNum == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + MultizoneSurfaceData(i).SurfName + "\", invalid schedule.");
                        ShowContinueError("Venting Schedule not found=\"" + MultizoneSurfaceData(i).VentingSchName + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    MultizoneSurfaceData(i).VentingSchName = "";
                    MultizoneSurfaceData(i).VentingSchNum = 0;
                }
                {
                    auto const SELECT_CASE_var(MultizoneSurfaceData(i).VentSurfCtrNum);
                    if ((SELECT_CASE_var == VentCtrNum_Temp) || (SELECT_CASE_var == VentCtrNum_AdjTemp)) {
                        MultizoneSurfaceData(i).VentSchNum = GetScheduleIndex(MultizoneSurfaceData(i).VentSchName);
                        if (MultizoneSurfaceData(i).VentSchName == BlankString) {
                            ShowSevereError(RoutineName + CurrentModuleObject +
                                            " object, No Ventilation Schedule was found, but is required when ventilation control is Temperature.");
                            ShowContinueError("..for Surface = \"" + MultizoneSurfaceData(i).SurfName + "\"");
                            ErrorsFound = true;
                        } else if (MultizoneSurfaceData(i).VentSchNum == 0) {
                            ShowSevereError(RoutineName + CurrentModuleObject +
                                            " object, Invalid Ventilation Schedule, required when ventilation control is Temperature.");
                            ShowContinueError("..Schedule name in error = " + MultizoneSurfaceData(i).VentSchName);
                            ShowContinueError("..for Surface = \"" + MultizoneSurfaceData(i).SurfName + "\"");
                            ErrorsFound = true;
                        }
                        if (MultizoneSurfaceData(i).LowValueTemp < 0.0) {
                            ShowWarningError(RoutineName + CurrentModuleObject + " object, Low Temperature difference value < 0.0d0");
                            ShowContinueError("..Input value=" + RoundSigDigits(MultizoneSurfaceData(i).LowValueTemp, 1) +
                                              ", Value will be reset to 0.0.");
                            ShowContinueError("..for Surface = \"" + MultizoneSurfaceData(i).SurfName + "\"");
                            MultizoneSurfaceData(i).LowValueTemp = 0.0;
                        }
                        if (MultizoneSurfaceData(i).LowValueTemp >= 100.0) {
                            ShowWarningError(RoutineName + CurrentModuleObject + " object, Low Temperature difference value >= 100.0d0");
                            ShowContinueError("..Input value = " + RoundSigDigits(MultizoneSurfaceData(i).LowValueTemp, 1) +
                                              ", Value will be reset to 0.0");
                            ShowContinueError("..for Surface = \"" + MultizoneSurfaceData(i).SurfName + "\"");
                            MultizoneZoneData(i).LowValueTemp = 0.0;
                        }
                        if (MultizoneSurfaceData(i).UpValueTemp <= MultizoneSurfaceData(i).LowValueTemp) {
                            ShowWarningError(RoutineName + CurrentModuleObject + " object, Upper Temperature <= Lower Temperature difference value.");
                            ShowContinueError("..Input value = " + RoundSigDigits(MultizoneSurfaceData(i).UpValueTemp, 1) +
                                              ", Value will be reset to 100.0");
                            ShowContinueError("..for Surface = \"" + MultizoneSurfaceData(i).SurfName + "\"");
                            MultizoneSurfaceData(i).UpValueTemp = 100.0;
                        }

                    } else if ((SELECT_CASE_var == VentCtrNum_Enth) || (SELECT_CASE_var == VentCtrNum_AdjEnth)) {
                        MultizoneSurfaceData(i).VentSchNum = GetScheduleIndex(MultizoneSurfaceData(i).VentSchName);
                        if (MultizoneSurfaceData(i).VentSchName == BlankString) {
                            ShowSevereError(RoutineName + CurrentModuleObject +
                                            " object, No Ventilation Schedule was found, but is required when ventilation control is Enthalpy.");
                            ShowContinueError("..for Surface = \"" + MultizoneSurfaceData(i).SurfName + "\"");
                            ErrorsFound = true;
                        } else if (MultizoneSurfaceData(i).VentSchNum == 0) {
                            ShowSevereError(RoutineName + CurrentModuleObject +
                                            " object, Invalid Ventilation Schedule, required when ventilation control is Enthalpy.");
                            ShowContinueError("..Schedule name in error = " + MultizoneSurfaceData(i).VentSchName);
                            ShowContinueError("..for Surface = \"" + MultizoneSurfaceData(i).SurfName + "\"");
                            ErrorsFound = true;
                        }
                        if (MultizoneSurfaceData(i).LowValueEnth < 0.0) {
                            ShowWarningError(RoutineName + CurrentModuleObject + " object, Low Enthalpy difference value < 0.0d0");
                            ShowContinueError("..Input value = " + RoundSigDigits(MultizoneSurfaceData(i).LowValueEnth, 1) +
                                              ", Value will be reset to 0.0");
                            ShowContinueError("..for Surface = \"" + MultizoneSurfaceData(i).SurfName + "\"");
                            MultizoneSurfaceData(i).LowValueEnth = 0.0;
                        }
                        if (MultizoneSurfaceData(i).LowValueEnth >= 300000.0) {
                            ShowWarningError(RoutineName + CurrentModuleObject + " object, Low Enthalpy difference value >= 300000.0");
                            ShowContinueError("..Input value = " + RoundSigDigits(MultizoneSurfaceData(i).LowValueEnth, 1) +
                                              ", Value will be reset to 0.0");
                            ShowContinueError("..for Surface = \"" + MultizoneSurfaceData(i).SurfName + "\"");
                            MultizoneZoneData(i).LowValueEnth = 0.0;
                        }
                        if (MultizoneSurfaceData(i).UpValueEnth <= MultizoneSurfaceData(i).LowValueEnth) {
                            ShowWarningError(RoutineName + CurrentModuleObject + " object, Upper Enthalpy <= Lower Enthalpy difference value.");
                            ShowContinueError("..Input value = " + RoundSigDigits(MultizoneSurfaceData(i).UpValueEnth, 1) +
                                              ", Value will be set to 300000.0");
                            ShowContinueError("..for Surface = \"" + MultizoneSurfaceData(i).SurfName + "\"");
                            MultizoneSurfaceData(i).UpValueEnth = 300000.0;
                        }

                    } else if (SELECT_CASE_var == VentCtrNum_Const) {
                        MultizoneSurfaceData(i).VentSchNum = 0;
                        MultizoneSurfaceData(i).VentSchName = "";

                    } else if (SELECT_CASE_var == VentCtrNum_ASH55) {
                        MultizoneSurfaceData(i).VentSchNum = 0;
                        MultizoneSurfaceData(i).VentSchName = "";

                    } else if (SELECT_CASE_var == VentCtrNum_CEN15251) {
                        MultizoneSurfaceData(i).VentSchNum = 0;
                        MultizoneSurfaceData(i).VentSchName = "";

                    } else if (SELECT_CASE_var == VentCtrNum_Novent) {
                        MultizoneSurfaceData(i).VentSchNum = 0;
                        MultizoneSurfaceData(i).VentSchName = "";

                    } else if (SELECT_CASE_var == VentCtrNum_ZoneLevel) {
                        MultizoneSurfaceData(i).VentSchNum = 0;
                        MultizoneSurfaceData(i).VentSchName = "";

                    } else {
                    }
                }
            }
        }

        // Validate opening component and assign opening dimension
        if (AirflowNetworkNumOfSimOpenings > 0) {
            for (i = 1; i <= AirflowNetworkNumOfSimOpenings; ++i) {
                found = false;
                for (j = 1; j <= AirflowNetworkNumOfSurfaces; ++j) {
                    if (MultizoneCompSimpleOpeningData(i).Name == MultizoneSurfaceData(j).OpeningName) {
                        //           MultizoneCompSimpleOpeningData(i)%Width = Surface(MultizoneSurfaceData(j)%SurfNum)%Width
                        //           MultizoneCompSimpleOpeningData(i)%Height = Surface(MultizoneSurfaceData(j)%SurfNum)%Height
                        found = true;
                    }
                }
            }
        }

        // *** Read AirflowNetwork simulation reference crack conditions
        // Moved into getAirflowElementInput

        // *** Read AirflowNetwork simulation surface crack component
        // Moved into getAirflowElementInput

        // *** Read AirflowNetwork simulation surface effective leakage area component
        // Moved into getAirflowElementInput

        // *** Read AirflowNetwork simulation zone exhaust fan component
        // Moved into getAirflowElementInput

        // Calculate CP values
        if (UtilityRoutines::SameString(AirflowNetworkSimu.WPCCntr, "SurfaceAverageCalculation")) {
            CalcWindPressureCoeffs();
            // Ensure automatic generation is OK
            n = 0;
            for (j = 1; j <= 5; ++j) {
                found = false;
                for (i = 1; i <= AirflowNetworkNumOfExtNode; ++i) {
                    if (MultizoneExternalNodeData(i).facadeNum == j) {
                        found = true;
                        break;
                    }
                }
                if (found) ++n;
                if (j == 5 && (!found)) {
                    found = true;
                    if (DisplayExtraWarnings) {
                        ShowWarningError(RoutineName + "SurfaceAverageCalculation is entered for field = Wind Pressure Coefficient Type, but no roof "
                                                       "surface is defined using an AirflowNetwork:MultiZone:Surface object.");
                        ShowContinueError("Reconsider if this is your modeling intent. Simulation continues.");
                    }
                }
            }
            if (n < 5 && DisplayExtraWarnings) {
                ShowWarningError(RoutineName + "SurfaceAverageCalculation is entered for field = Wind Pressure Coefficient Type.");
                ShowContinueError(
                    "The AirflowNetwork model provides wind pressure coefficients for 4 vertical exterior orientations and 1 horizontal roof.");
                ShowContinueError(" There are only " + RoundSigDigits(n) +
                                  " exterior surface orientations defined in this input file using AirflowNetwork:MultiZone:Surface objects.");
                ShowContinueError("Reconsider if this is your modeling intent. Simulation continues.");
            }
        }

        // Assign external node height
        if (UtilityRoutines::SameString(AirflowNetworkSimu.WPCCntr, "SurfaceAverageCalculation") ||
            UtilityRoutines::SameString(AirflowNetworkSimu.HeightOption, "OpeningHeight")) {
            for (i = 1; i <= AirflowNetworkNumOfExtNode; ++i) {
                for (j = 1; j <= AirflowNetworkNumOfSurfaces; ++j) {
                    if (Surface(MultizoneSurfaceData(j).SurfNum).ExtBoundCond == ExternalEnvironment ||
                        (Surface(MultizoneSurfaceData(j).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt &&
                         Surface(MultizoneSurfaceData(j).SurfNum).ExtWind)) {
                        if (UtilityRoutines::SameString(MultizoneSurfaceData(j).ExternalNodeName, MultizoneExternalNodeData(i).Name)) {
                            MultizoneExternalNodeData(i).height = Surface(MultizoneSurfaceData(j).SurfNum).Centroid.z;
                            break;
                        }
                    }
                }
            }
        }

        // Assign external node azimuth, should consider combining this with the above to avoid the repeated search
        for (i = 1; i <= AirflowNetworkNumOfExtNode; ++i) {
            for (j = 1; j <= AirflowNetworkNumOfSurfaces; ++j) {
                if (Surface(MultizoneSurfaceData(j).SurfNum).ExtBoundCond == ExternalEnvironment ||
                    (Surface(MultizoneSurfaceData(j).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt &&
                     Surface(MultizoneSurfaceData(j).SurfNum).ExtWind)) {
                    if (UtilityRoutines::SameString(MultizoneSurfaceData(j).ExternalNodeName, MultizoneExternalNodeData(i).Name)) {
                        MultizoneExternalNodeData(i).azimuth = Surface(MultizoneSurfaceData(j).SurfNum).Azimuth;
                        break;
                    }
                }
            }
        }

        if (ErrorsFound) ShowFatalError(RoutineName + "Errors found getting inputs. Previous error(s) cause program termination.");

        // Write wind pressure coefficients in the EIO file
        print(outputFiles.eio, "! <AirflowNetwork Model:Wind Direction>, Wind Direction #1 to n (degree)\n");
        print(outputFiles.eio, "AirflowNetwork Model:Wind Direction, ");

        int numWinDirs = 11;
        Real64 angleDelta = 30.0;
        if (AirflowNetworkNumOfSingleSideZones > 0) {
            numWinDirs = 35;
            angleDelta = 10.0;
        }

        for (i = 0; i < numWinDirs; ++i) {
            print(outputFiles.eio, "{:.1R},", i * angleDelta);
        }
        print(outputFiles.eio, "{:.1R}\n", numWinDirs * angleDelta);

        print(outputFiles.eio, "! <AirflowNetwork Model:Wind Pressure Coefficients>, Name, Wind Pressure Coefficients #1 to n (dimensionless)\n");

        // The old version used to write info with single-sided natural ventilation specific labeling, this version no longer does that.
        std::set<int> curves;
        for (int i = 1; i <= AirflowNetworkNumOfExtNode; ++i) {
            curves.insert(MultizoneExternalNodeData(i).curve);
        }
        for (auto index : curves) {
            print(outputFiles.eio, "AirflowNetwork Model:Wind Pressure Coefficients, {}, ", CurveManager::GetCurveName(index));

            for (j = 0; j < numWinDirs; ++j) {
                print(outputFiles.eio, "{:.2R},", CurveManager::CurveValue(index, j * angleDelta));
            }
            print(outputFiles.eio, "{:.2R}\n", CurveManager::CurveValue(index, numWinDirs * angleDelta));
        }

        if (AirflowNetworkNumOfSingleSideZones > 0) {
            for (i = 1; i <= AirflowNetworkNumOfZones; ++i) {
                if (MultizoneZoneData(i).SingleSidedCpType == "ADVANCED") {
                    print(outputFiles.eio, "AirflowNetwork: Advanced Single-Sided Model: Difference in Opening Wind Pressure Coefficients (DeltaCP), ");
                    print(outputFiles.eio, "{}, ", MultizoneZoneData(i).ZoneName);
                    for (unsigned j = 1; j <= EPDeltaCP(i).WindDir.size() - 1; ++j) {
                        print(outputFiles.eio, "{:.2R},", EPDeltaCP(i).WindDir(j));
                    }
                    print(outputFiles.eio, "{:.2R}\n", EPDeltaCP(i).WindDir(EPDeltaCP(i).WindDir.size()));
                }
            }
        }

        // If no zone object, exit
        if (AirflowNetworkNumOfZones == 0) {
            ShowFatalError(RoutineName + "Errors found getting inputs. Previous error(s) cause program termination.");
        }
        // If zone node number =0, exit.
        for (j = 1; j <= AirflowNetworkNumOfSurfaces; ++j) {
            if (MultizoneSurfaceData(j).NodeNums[0] == 0 && ErrorsFound) {
                ShowFatalError(RoutineName + "Errors found getting inputs. Previous error(s) cause program termination.");
            }
            if (MultizoneSurfaceData(j).NodeNums[1] == 0 && ErrorsFound) {
                ShowFatalError(RoutineName + "Errors found getting inputs. Previous error(s) cause program termination.");
            }
        }

        // Ensure at least two surfaces are exposed to a zone
        ZoneCheck.allocate(AirflowNetworkNumOfZones);
        ZoneBCCheck.allocate(AirflowNetworkNumOfZones);
        ZoneCheck = 0;
        ZoneBCCheck = 0;
        CurrentModuleObject = "AirflowNetwork:MultiZone:Surface";
        for (j = 1; j <= AirflowNetworkNumOfSurfaces; ++j) {
            if (MultizoneSurfaceData(j).NodeNums[0] <= AirflowNetworkNumOfZones) {
                ++ZoneCheck(MultizoneSurfaceData(j).NodeNums[0]);
                ZoneBCCheck(MultizoneSurfaceData(j).NodeNums[0]) = MultizoneSurfaceData(j).NodeNums[1];
            }
            if (MultizoneSurfaceData(j).NodeNums[1] <= AirflowNetworkNumOfZones) {
                ++ZoneCheck(MultizoneSurfaceData(j).NodeNums[1]);
                ZoneBCCheck(MultizoneSurfaceData(j).NodeNums[1]) = MultizoneSurfaceData(j).NodeNums[0];
            }
        }
        for (i = 1; i <= AirflowNetworkNumOfZones; ++i) {
            if (ZoneCheck(i) == 0) {
                ShowSevereError(RoutineName + "AirflowNetwork:Multizone:Zone = " + MultizoneZoneData(i).ZoneName);
                ShowContinueError(" does not have any surfaces defined in " + CurrentModuleObject);
                ShowContinueError("Each zone should have at least two surfaces defined in " + CurrentModuleObject);
                ErrorsFound = true;
            }
            if (ZoneCheck(i) == 1) {
                ShowSevereError(RoutineName + "AirflowNetwork:Multizone:Zone = " + MultizoneZoneData(i).ZoneName);
                ShowContinueError(" has only one surface defined in " + CurrentModuleObject);
                ShowContinueError(" Each zone should have at least two surfaces defined in " + CurrentModuleObject);
                ErrorsFound = true;
            }
            if (ZoneCheck(i) > 1) {
                SurfaceFound = false;
                for (j = 1; j <= AirflowNetworkNumOfSurfaces; ++j) {
                    if (MultizoneSurfaceData(j).NodeNums[0] == i) {
                        if (ZoneBCCheck(i) != MultizoneSurfaceData(j).NodeNums[1]) {
                            SurfaceFound = true;
                            break;
                        }
                    }
                    if (MultizoneSurfaceData(j).NodeNums[1] == i) {
                        if (ZoneBCCheck(i) != MultizoneSurfaceData(j).NodeNums[0]) {
                            SurfaceFound = true;
                            break;
                        }
                    }
                }
                if (!SurfaceFound) {
                    ShowWarningError(RoutineName + "AirflowNetwork:Multizone:Zone = " + MultizoneZoneData(i).ZoneName);
                    ShowContinueError("has more than one surface defined in " + CurrentModuleObject + ", but has the same boundary conditions");
                    ShowContinueError("Please check inputs of " + CurrentModuleObject);
                }
            }
        }
        ZoneCheck.deallocate();
        ZoneBCCheck.deallocate();

        // Validate CP Value number
        if (AirflowNetworkSimu.iWPCCntr == iWPCCntr_Input) { // Surface-Average does not need inputs of external nodes
            // Ensure different curve is used to avoid a single side boundary condition
            found = false;
            bool differentAngle = false;
            for (j = 2; j <= AirflowNetworkNumOfExtNode; ++j) {
                if (MultizoneExternalNodeData(j - 1).curve != MultizoneExternalNodeData(j).curve) {
                    found = true;
                    break;
                } else {
                    // If the curves are the same, then check to see if the azimuths are different
                    if (MultizoneExternalNodeData(j - 1).azimuth != MultizoneExternalNodeData(j).azimuth) {
                        differentAngle = MultizoneExternalNodeData(j - 1).symmetricCurve || MultizoneExternalNodeData(j).symmetricCurve;
                    }
                }
            }
            if (!found && !differentAngle) {
                ShowSevereError("The same Wind Pressure Coefficient Curve name is used in all AirflowNetwork:MultiZone:ExternalNode objects.");
                ShowContinueError(
                    "Please input at least two different Wind Pressure Coefficient Curve names to avoid single side boundary condition.");
                ErrorsFound = true;
            }
        }

        // Assign occupant ventilation control number from zone to surface
        for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
            j = MultizoneSurfaceData(i).SurfNum;
            if (SurfaceWindow(j).OriginalClass == SurfaceClass_Window || SurfaceWindow(j).OriginalClass == SurfaceClass_Door ||
                SurfaceWindow(j).OriginalClass == SurfaceClass_GlassDoor) {
                for (n = 1; n <= AirflowNetworkNumOfZones; ++n) {
                    if (MultizoneZoneData(n).ZoneNum == Surface(j).Zone) {
                        if (MultizoneZoneData(n).OccupantVentilationControlNum > 0 && MultizoneSurfaceData(i).OccupantVentilationControlNum == 0) {
                            MultizoneSurfaceData(i).OccupantVentilationControlNum = MultizoneZoneData(n).OccupantVentilationControlNum;
                        }
                    }
                }
            }
        }

        // Read AirflowNetwork Intra zone node
        CurrentModuleObject = "AirflowNetwork:IntraZone:Node";
        IntraZoneNumOfNodes = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (IntraZoneNumOfNodes > 0) {
            IntraZoneNodeData.allocate(IntraZoneNumOfNodes);
            for (i = 1; i <= IntraZoneNumOfNodes; ++i) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              i,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);
                UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
                IntraZoneNodeData(i).Name = Alphas(1);         // Name of node
                IntraZoneNodeData(i).RAFNNodeName = Alphas(2); // Name of RoomAir node
                IntraZoneNodeData(i).Height = Numbers(1);      // Nodal height
                // verify RoomAir model node names(May be too early to check and move to another subroutine)
                GetRAFNNodeNum(IntraZoneNodeData(i).RAFNNodeName, IntraZoneNodeData(i).ZoneNum, IntraZoneNodeData(i).RAFNNodeNum, Errorfound1);
                if (Errorfound1) ErrorsFound = true;
                if (IntraZoneNodeData(i).RAFNNodeNum == 0) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "='" + Alphas(1) + "' invalid name " + cAlphaFields(2) + "='" + Alphas(2));
                    ErrorsFound = true;
                }
                IntraZoneNodeData(i).AFNZoneNum =
                    UtilityRoutines::FindItemInList(Alphas(3), MultizoneZoneData, &MultizoneZoneProp::ZoneName, AirflowNetworkNumOfZones);
                if (MultizoneZoneData(IntraZoneNodeData(i).AFNZoneNum).RAFNNodeNum == 0) {
                    GetRAFNNodeNum(MultizoneZoneData(IntraZoneNodeData(i).AFNZoneNum).ZoneName,
                                   IntraZoneNodeData(i).ZoneNum,
                                   MultizoneZoneData(IntraZoneNodeData(i).AFNZoneNum).RAFNNodeNum,
                                   Errorfound1);
                }
                if (IntraZoneNodeData(i).ZoneNum == 0) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "='" + Alphas(1) + "' the Zone is not defined for " + cAlphaFields(3) + "='" +
                                    Alphas(3));
                    ErrorsFound = true;
                }
            }
        }

        // check model compatibility
        if (IntraZoneNumOfNodes > 0) {
            if (!UtilityRoutines::SameString(SimAirNetworkKey, "MultizoneWithoutDistribution")) {
                ShowSevereError(RoutineName + CurrentModuleObject +
                                " model requires Simulation Control = MultizoneWithoutDistribution, while the input choice is " + SimAirNetworkKey +
                                ".");
                ErrorsFound = true;
                ShowFatalError(RoutineName + "Errors found getting " + CurrentModuleObject +
                               " object."
                               " Previous error(s) cause program termination.");
            }
        }

        NumOfNodesIntraZone = IntraZoneNumOfNodes;
        // check zone node
        IntraZoneNumOfZones = 0;
        for (i = 1; i <= AirflowNetworkNumOfZones; ++i) {
            if (MultizoneZoneData(i).RAFNNodeNum > 0) {
                IntraZoneNumOfZones += 1;
            }
        }

        // Override error check due to RoomAirNode for the time being

        // Read AirflowNetwork Intra linkage
        CurrentModuleObject = "AirflowNetwork:IntraZone:Linkage";
        IntraZoneNumOfLinks = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (IntraZoneNumOfLinks > 0) {
            IntraZoneLinkageData.allocate(IntraZoneNumOfLinks);
            UniqueAirflowNetworkSurfaceName.reserve(IntraZoneNumOfLinks);
            for (i = 1; i <= IntraZoneNumOfLinks; ++i) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              i,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);
                UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
                IntraZoneLinkageData(i).Name = Alphas(1); // Name of linkage
                IntraZoneLinkageData(i).NodeNames[0] = Alphas(2);
                IntraZoneLinkageData(i).NodeHeights[0] = 0.0;
                IntraZoneLinkageData(i).NodeNames[1] = Alphas(3);
                IntraZoneLinkageData(i).NodeHeights[1] = 0.0;
                IntraZoneLinkageData(i).CompName = Alphas(4);
                if (!lAlphaBlanks(5)) {
                    // Perform simple test first.The comprehensive input validation will occur later
                    // Check valid surface name
                    IntraZoneLinkageData(i).SurfaceName = Alphas(5);
                    IntraZoneLinkageData(i).LinkNum = UtilityRoutines::FindItemInList(
                        Alphas(5), MultizoneSurfaceData, &MultizoneSurfaceProp::SurfName, AirflowNetworkNumOfSurfaces);
                    if (IntraZoneLinkageData(i).LinkNum == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "='" + Alphas(1) + "': Invalid " + cAlphaFields(5) +
                                        " given = " + Alphas(5) + " in AirflowNetwork:MultiZone:Surface objects");
                        ErrorsFound = true;
                    }
                    GlobalNames::VerifyUniqueInterObjectName(
                        UniqueAirflowNetworkSurfaceName, Alphas(5), CurrentModuleObject, cAlphaFields(5), ErrorsFound);
                }
                if (UtilityRoutines::SameString(Alphas(2), Alphas(3))) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "='" + Alphas(1) + "': Invalid inputs of both node name with " + Alphas(2) +
                                    " = " + Alphas(3));
                    ErrorsFound = true;
                }
                // Check valid node names
                IntraZoneLinkageData(i).NodeNums[0] = UtilityRoutines::FindItemInList(Alphas(2), IntraZoneNodeData, IntraZoneNumOfNodes);
                if (IntraZoneLinkageData(i).NodeNums[0] == 0) {
                    IntraZoneLinkageData(i).NodeNums[0] =
                        UtilityRoutines::FindItemInList(Alphas(2), MultizoneZoneData, &MultizoneZoneProp::ZoneName, AirflowNetworkNumOfZones);
                    IntraZoneLinkageData(i).NodeHeights[0] = Zone(MultizoneZoneData(IntraZoneLinkageData(i).NodeNums[0]).ZoneNum).Centroid.z;
                    if (IntraZoneLinkageData(i).NodeNums[0] == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "='" + Alphas(1) + "': Invalid " + cAlphaFields(2) +
                                        " given = " + Alphas(2) + " in AirflowNetwork:IntraZone:Node and AirflowNetwork:MultiZone:Zone objects");
                        ErrorsFound = true;
                    }
                } else {
                    IntraZoneLinkageData(i).NodeHeights[0] = IntraZoneNodeData(IntraZoneLinkageData(i).NodeNums[0]).Height;
                    IntraZoneLinkageData(i).NodeNums[0] = IntraZoneLinkageData(i).NodeNums[0] + AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode;
                }
                IntraZoneLinkageData(i).NodeNums[1] = UtilityRoutines::FindItemInList(Alphas(3), IntraZoneNodeData, IntraZoneNumOfNodes);
                if (IntraZoneLinkageData(i).NodeNums[1] == 0) {
                    IntraZoneLinkageData(i).NodeNums[1] =
                        UtilityRoutines::FindItemInList(Alphas(3), MultizoneZoneData, &MultizoneZoneProp::ZoneName, AirflowNetworkNumOfZones);
                    if (IntraZoneLinkageData(i).NodeNums[1] > 0) {
                        IntraZoneLinkageData(i).NodeHeights[1] = Zone(MultizoneZoneData(IntraZoneLinkageData(i).NodeNums[1]).ZoneNum).Centroid.z;
                    } else {
                        if (AirflowNetworkSimu.iWPCCntr == iWPCCntr_Input) { // Surface-Average does not need inputs of external nodes
                            IntraZoneLinkageData(i).NodeNums[1] = MultizoneSurfaceData(IntraZoneLinkageData(i).LinkNum).NodeNums[1];
                            if (IntraZoneLinkageData(i).NodeNums[1] == 0) {
                                ShowSevereError(RoutineName + CurrentModuleObject + "='" + Alphas(1) + "': Invalid " + cAlphaFields(3) +
                                                " given = " + Alphas(3) +
                                                " in AirflowNetwork:IntraZone:Node or AirflowNetwork:MultiZone:Zone or "
                                                "AirflowNetwork:MultiZone:ExternalNode objects");
                                ErrorsFound = true;
                            }
                        }
                        if (AirflowNetworkSimu.iWPCCntr == iWPCCntr_SurfAvg) {
                            if (!lAlphaBlanks(3)) {
                                ShowWarningError(RoutineName + CurrentModuleObject + "='" + Alphas(1) + " The input of " + cAlphaFields(3) +
                                                 " is not needed, ");
                                ShowContinueError(" since AirflowNetwork Wind Pressure Coefficient Type = SURFACE-AVERAGE CALCULATION. The "
                                                  "simulation continues...");
                            }
                            IntraZoneLinkageData(i).NodeNums[1] = MultizoneSurfaceData(IntraZoneLinkageData(i).LinkNum).NodeNums[1];
                        }
                    }
                } else {
                    IntraZoneLinkageData(i).NodeHeights[1] = IntraZoneNodeData(IntraZoneLinkageData(i).NodeNums[1]).Height;
                    IntraZoneLinkageData(i).NodeNums[1] = IntraZoneLinkageData(i).NodeNums[1] + AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode;
                }
                // Ensure the both linked nodes for a surface are not zone nodes.One of nodes has to be an intrazone node
                if (IntraZoneLinkageData(i).NodeNums[1] <= AirflowNetworkNumOfZones &&
                    IntraZoneLinkageData(i).NodeNums[0] <= AirflowNetworkNumOfZones) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "='" + Alphas(1) + "': Invalid node inputs " + Alphas(2) + " and " +
                                    Alphas(3) + " are zone nodes");
                    ErrorsFound = true;
                }
                if (IntraZoneLinkageData(i).NodeNums[0] <= AirflowNetworkNumOfZones &&
                    IntraZoneLinkageData(i).NodeNums[1] > AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode && lAlphaBlanks(5)) {
                    if (IntraZoneLinkageData(i).NodeNums[0] !=
                        IntraZoneNodeData(IntraZoneLinkageData(i).NodeNums[1] - AirflowNetworkNumOfZones - AirflowNetworkNumOfExtNode).ZoneNum) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "='" + Alphas(1) + ": Invalid zone inputs between Node and Link " +
                                        Alphas(2) + " and " +
                                        MultizoneZoneData(IntraZoneNodeData(IntraZoneLinkageData(i).NodeNums[0]).ZoneNum).ZoneName);
                        ErrorsFound = true;
                    }
                }
                if (IntraZoneLinkageData(i).NodeNums[1] <= AirflowNetworkNumOfZones &&
                    IntraZoneLinkageData(i).NodeNums[0] > AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode && lAlphaBlanks(5)) {
                    if (IntraZoneLinkageData(i).NodeNums[1] !=
                        IntraZoneNodeData(IntraZoneLinkageData(i).NodeNums[0] - AirflowNetworkNumOfZones - AirflowNetworkNumOfExtNode).ZoneNum) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "='" + Alphas(1) + ": Invalid zone inputs between Node and Link " +
                                        Alphas(3) + " and " +
                                        MultizoneZoneData(IntraZoneNodeData(IntraZoneLinkageData(i).NodeNums[1]).ZoneNum).ZoneName);
                        ErrorsFound = true;
                    }
                }
            }

            // Reset the number of intrazone links for a given surface
            NumOfLinksIntraZone = IntraZoneNumOfLinks;
            for (i = 1; i <= IntraZoneNumOfLinks; ++i) {
                j = IntraZoneLinkageData(i).LinkNum;
                if (j > 0) {
                    // Revise data in multizone object
                    NumOfLinksIntraZone = NumOfLinksIntraZone - 1;
                    if (Surface(MultizoneSurfaceData(j).SurfNum).ExtBoundCond == 0) {
                        // Exterior surface NodeNums[1] should be equal
                        if (IntraZoneLinkageData(i).NodeNums[0] > AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode) {
                            MultizoneSurfaceData(j).RAFNflag = true;
                            MultizoneSurfaceData(j).ZonePtr = MultizoneSurfaceData(j).NodeNums[0];
                            MultizoneSurfaceData(j).NodeNums[0] = IntraZoneLinkageData(i).NodeNums[0];
                        } else if (IntraZoneLinkageData(i).NodeNums[1] > AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode) {
                            MultizoneSurfaceData(j).RAFNflag = true;
                            MultizoneSurfaceData(j).ZonePtr = MultizoneSurfaceData(j).NodeNums[0];
                            MultizoneSurfaceData(j).NodeNums[0] = IntraZoneLinkageData(i).NodeNums[1];
                        } else {
                            ShowSevereError(RoutineName + "The InterZone link is not found between AirflowNetwork:IntraZone:Linkage =" +
                                            IntraZoneLinkageData(i).Name +
                                            " and AirflowNetwork:Multizone:Surface = " + MultizoneSurfaceData(j).SurfName);
                            ErrorsFound = true;
                        }
                    } else {
                        // Interior surface
                        if (IntraZoneLinkageData(i).NodeNums[0] > AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode &&
                            IntraZoneLinkageData(i).NodeNums[1] > AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode) {
                            MultizoneSurfaceData(j).RAFNflag = true;
                            if (MultizoneZoneData(MultizoneSurfaceData(j).NodeNums[0]).ZoneNum ==
                                IntraZoneNodeData(IntraZoneLinkageData(i).NodeNums[0] - AirflowNetworkNumOfZones - AirflowNetworkNumOfExtNode)
                                    .ZoneNum) {
                                MultizoneSurfaceData(j).ZonePtr = MultizoneSurfaceData(j).NodeNums[0];
                                MultizoneSurfaceData(j).NodeNums[0] = IntraZoneLinkageData(i).NodeNums[0];
                                MultizoneSurfaceData(j).NodeNums[1] = IntraZoneLinkageData(i).NodeNums[1];
                            } else {
                                MultizoneSurfaceData(j).ZonePtr = MultizoneSurfaceData(j).NodeNums[0];
                                MultizoneSurfaceData(j).NodeNums[0] = IntraZoneLinkageData(i).NodeNums[1];
                                MultizoneSurfaceData(j).NodeNums[1] = IntraZoneLinkageData(i).NodeNums[0];
                            }
                        } else if (IntraZoneLinkageData(i).NodeNums[0] > AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode) {
                            MultizoneSurfaceData(j).RAFNflag = true;
                            if (IntraZoneLinkageData(i).NodeNums[1] == MultizoneSurfaceData(j).NodeNums[0]) {
                                MultizoneSurfaceData(j).NodeNums[1] = IntraZoneLinkageData(i).NodeNums[0];
                            } else if (IntraZoneLinkageData(i).NodeNums[1] == MultizoneSurfaceData(j).NodeNums[1]) {
                                MultizoneSurfaceData(j).ZonePtr = MultizoneSurfaceData(j).NodeNums[0];
                                MultizoneSurfaceData(j).NodeNums[0] = IntraZoneLinkageData(i).NodeNums[0];
                            } else {
                                ShowSevereError(RoutineName + "The InterZone link is not found between AirflowNetwork:IntraZone:Linkage =" +
                                                IntraZoneLinkageData(i).Name +
                                                " and AirflowNetwork:Multizone:Surface = " + MultizoneSurfaceData(j).SurfName);
                                ErrorsFound = true;
                            }
                        } else if (IntraZoneLinkageData(i).NodeNums[1] > AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode) {
                            MultizoneSurfaceData(j).RAFNflag = true;
                            if (IntraZoneLinkageData(i).NodeNums[0] == MultizoneSurfaceData(j).NodeNums[0]) {
                                MultizoneSurfaceData(j).NodeNums[1] = IntraZoneLinkageData(i).NodeNums[1];
                            } else if (IntraZoneLinkageData(i).NodeNums[0] == MultizoneSurfaceData(j).NodeNums[1]) {
                                MultizoneSurfaceData(j).ZonePtr = MultizoneSurfaceData(j).NodeNums[0];
                                MultizoneSurfaceData(j).NodeNums[0] = IntraZoneLinkageData(i).NodeNums[1];
                            } else {
                                ShowSevereError(RoutineName + "The InterZone link is not found between AirflowNetwork:IntraZone:Linkage =" +
                                                IntraZoneLinkageData(i).Name +
                                                " and AirflowNetwork:Multizone:Surface = " + MultizoneSurfaceData(j).SurfName);
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            }
            // Remove links with surface defined in Multizone : Surface objects
            i = 1;
            if (NumOfLinksIntraZone < IntraZoneNumOfLinks) {
                while (i <= NumOfLinksIntraZone) {
                    if (IntraZoneLinkageData(i).LinkNum > 0) {
                        if (DisplayExtraWarnings) {
                            ShowWarningError(RoutineName + CurrentModuleObject + "='" + IntraZoneLinkageData(i).Name +
                                             " is reomoved from the list due to the surface conncetion from Intrazone to Interzone.");
                        }
                        for (j = i; j <= IntraZoneNumOfLinks - 1; ++j) {
                            IntraZoneLinkageData(j) = IntraZoneLinkageData(j + 1);
                        }
                    }
                    if (IntraZoneLinkageData(i).LinkNum == 0) i = i + 1;
                }
                if (IntraZoneLinkageData(i).LinkNum > 0) {
                    if (DisplayExtraWarnings) {
                        ShowWarningError(RoutineName + CurrentModuleObject + "='" + IntraZoneLinkageData(i).Name +
                                         " is reomoved from the list due to the surface conncetion from Intrazone to Interzone.");
                    }
                }
            }
        }

        // Read AirflowNetwork Distribution system node
        CurrentModuleObject = "AirflowNetwork:Distribution:Node";
        DisSysNumOfNodes = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (DisSysNumOfNodes > 0) {
            DisSysNodeData.allocate(DisSysNumOfNodes);
            for (i = 1; i <= DisSysNumOfNodes; ++i) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              i,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);
                UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
                DisSysNodeData(i).Name = Alphas(1);      // Name of node
                DisSysNodeData(i).EPlusName = Alphas(2); // Name of associated EnergyPlus node
                DisSysNodeData(i).EPlusType = Alphas(3); // Name of associated EnergyPlus type
                DisSysNodeData(i).Height = Numbers(1);   // Nodal height
                DisSysNodeData(i).EPlusNodeNum = 0;      // EPlus node number
                // verify EnergyPlus object type
                if (UtilityRoutines::SameString(Alphas(3), "AirLoopHVAC:ZoneMixer") ||
                    UtilityRoutines::SameString(Alphas(3), "AirLoopHVAC:ZoneSplitter") ||
                    UtilityRoutines::SameString(Alphas(3), "AirLoopHVAC:OutdoorAirSystem") ||
                    UtilityRoutines::SameString(Alphas(3), "OAMixerOutdoorAirStreamNode") ||
                    UtilityRoutines::SameString(Alphas(3), "OutdoorAir:NodeList") || UtilityRoutines::SameString(Alphas(3), "OutdoorAir:Node") ||
                    UtilityRoutines::SameString(Alphas(3), "Other") || lAlphaBlanks(3)) {
                    continue;
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\" invalid " + cAlphaFields(3) + "=\"" + Alphas(3) +
                                    "\" illegal key.");
                    ShowContinueError("Valid keys are: AirLoopHVAC:ZoneMixer, AirLoopHVAC:ZoneSplitter, AirLoopHVAC:OutdoorAirSystem, "
                                      "OAMixerOutdoorAirStreamNode, OutdoorAir:NodeList, OutdoorAir:Node or Other.");
                    ErrorsFound = true;
                }
            }
        } else {
            if (SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
                ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object is required but not found.");
                ErrorsFound = true;
            }
        }

        // Read AirflowNetwork Distribution system component: duct leakage
        // Moved into getAirflowElementInput

        // Read AirflowNetwork Distribution system component: duct effective leakage ratio
        // Moved into getAirflowElementInput

        // Read AirflowNetwork Distribution system component: duct
        // Moved into getAirflowElementInput
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Duct";
        if (DisSysNumOfDucts == 0) {
            if (SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
                ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object is required but not found.");
                ErrorsFound = true;
            }
        }

        // Read AirflowNetwork distribution system component: DuctViewFactors
        CurrentModuleObject = "AirflowNetwork:Distribution:DuctViewFactors";
        DisSysNumOfDuctViewFactors = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (DisSysNumOfDuctViewFactors > 0) {
            AirflowNetworkLinkageViewFactorData.allocate(DisSysNumOfDuctViewFactors);
            for (i = 1; i <= DisSysNumOfDuctViewFactors; ++i) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              i,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);
                UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);

                auto &this_VF_object(AirflowNetworkLinkageViewFactorData(i));

                this_VF_object.LinkageName = Alphas(1); // Name of linkage

                // Surface exposure fraction
                if (Numbers(2) > 1) {
                    ShowContinueError("Duct surface exposure fraction greater than 1. Check input in: " + CurrentModuleObject + " " +
                                      this_VF_object.LinkageName);
                    ShowContinueError("Using value of 1 for surface exposure fraction");
                    this_VF_object.DuctExposureFraction = 1;
                } else if (Numbers(2) < 0) {
                    ShowContinueError("Surface exposure fraction less than 0. Check input in: " + CurrentModuleObject + " " +
                                      this_VF_object.LinkageName);
                    ShowContinueError("Using value of 0 for surface exposure fraction");
                    this_VF_object.DuctExposureFraction = 0;
                } else {
                    this_VF_object.DuctExposureFraction = Numbers(1);
                }

                // Duct surface emittance
                if (Numbers(2) > 1) {
                    ShowContinueError("Duct surface emittance greater than 1. Check input in: " + CurrentModuleObject + " " +
                                      this_VF_object.LinkageName);
                    ShowContinueError("Using value of 1 for surface emittance");
                    this_VF_object.DuctEmittance = 1;
                } else if (Numbers(2) < 0) {
                    ShowContinueError("Surface exposure fraction less than 0. Check input in: " + CurrentModuleObject + " " +
                                      this_VF_object.LinkageName);
                    ShowContinueError("Using value of 0 for surface exposure fraction");
                    this_VF_object.DuctEmittance = 0;
                } else {
                    this_VF_object.DuctEmittance = Numbers(2);
                }

                this_VF_object.ObjectNum = i;

                int numSurfaces = NumAlphas - 1;

                this_VF_object.LinkageSurfaceData.allocate(numSurfaces);

                for (int surfNum = 1; surfNum < NumAlphas; ++surfNum) {
                    this_VF_object.LinkageSurfaceData(surfNum).SurfaceName = Alphas(surfNum + 1); // Surface name
                    this_VF_object.LinkageSurfaceData(surfNum).SurfaceNum = UtilityRoutines::FindItemInList(Alphas(surfNum + 1), Surface);

                    if (this_VF_object.LinkageSurfaceData(surfNum).SurfaceNum == 0) {
                        ShowFatalError("Surface " + Alphas(surfNum + 1) + " not found. See: " + CurrentModuleObject + " " +
                                       this_VF_object.LinkageName);
                    }

                    // Surface view factor
                    if (Numbers(surfNum + 2) > 1) {
                        ShowContinueError("View factor for surface " + Alphas(surfNum + 1) +
                                          " greater than 1. Check input in: " + CurrentModuleObject + " " + this_VF_object.LinkageName);
                        ShowContinueError("Using value of 1 for view factor");
                        this_VF_object.LinkageSurfaceData(surfNum).ViewFactor = 1;
                    } else if (Numbers(surfNum + 2) < 0) {
                        ShowContinueError("View factor for surface " + Alphas(surfNum + 1) + " less than 0. Check input in: " + CurrentModuleObject +
                                          " " + this_VF_object.LinkageName);
                        ShowContinueError("Using value of 0 for view factor");
                        this_VF_object.LinkageSurfaceData(surfNum).ViewFactor = 0;
                    } else {
                        this_VF_object.LinkageSurfaceData(surfNum).ViewFactor = Numbers(surfNum + 2);
                    }
                }
            }
        }

        // Read AirflowNetwork Distribution system component: Damper
        //  CurrentModuleObject='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DAMPER'
        // Deleted on Aug. 13, 2008

        // Read AirflowNetwork Distribution system component: constant volume fan
        // Moved into getAirflowElementInput
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Fan";
        if (DisSysNumOfCVFs == 0) {
            if (SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
                ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object is required but not found.");
                ErrorsFound = true;
            }
        }

        // Read AirflowNetwork Distribution system component: Detailed fan
        //  CurrentModuleObject='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DETAILED FAN'
        // Deleted on Aug. 13, 2008

        // Read AirflowNetwork Distribution system component: coil
        // Moved into getAirflowElementInput

        // Read AirflowNetwork Distribution system component: heat exchanger
        // Moved into getAirflowElementInput

        // Read AirflowNetwork Distribution system component: terminal unit
        // Moved into getAirflowElementInput

        // Get input data of constant pressure drop component
        // Moved into getAirflowElementInput

        // Read Outdoor Airflow object
        // Moved into getAirflowElementInput

        // Read Relief Airflow object
        // Moved into getAirflowElementInput

        // Read PressureController
        CurrentModuleObject = "AirflowNetwork:ZoneControl:PressureController";
        NumOfPressureControllers = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (NumOfPressureControllers > 1) {
            ShowSevereError(RoutineName + "More " + CurrentModuleObject + " are found. Currently only one( \"1\") " + CurrentModuleObject +
                            " object per simulation is allowed when using AirflowNetwork Distribution Systems.");
            ShowFatalError(RoutineName + "Errors found getting " + CurrentModuleObject + " object. Previous error(s) cause program termination.");
        }

        if (NumOfPressureControllers > 0) {
            PressureControllerData.allocate(NumOfPressureControllers);
            for (i = 1; i <= NumOfPressureControllers; ++i) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              i,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);
                UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
                PressureControllerData(i).Name = Alphas(1);     // Object Name
                PressureControllerData(i).ZoneName = Alphas(2); // Zone name
                PressureControllerData(i).ZoneNum = UtilityRoutines::FindItemInList(Alphas(2), Zone);
                PressureControllerData(i).AFNNodeNum =
                    UtilityRoutines::FindItemInList(Alphas(2), MultizoneZoneData, &MultizoneZoneProp::ZoneName, AirflowNetworkNumOfZones);
                if (PressureControllerData(i).ZoneNum == 0) {
                    ShowSevereError(RoutineName + CurrentModuleObject + " object, invalid " + cAlphaFields(2) + " given.");
                    ShowContinueError("..invalid " + cAlphaFields(2) + " = \"" + PressureControllerData(i).ZoneName + "\"");
                    ErrorsFound = true;
                }

                PressureControllerData(i).ControlObjectType = Alphas(3); // Control Object Type
                PressureControllerData(i).ControlObjectName = Alphas(4); // Control Object Name

                {
                    auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(Alphas(3)));
                    if (SELECT_CASE_var == "AIRFLOWNETWORK:MULTIZONE:COMPONENT:ZONEEXHAUSTFAN") {
                        PressureControllerData(i).ControlTypeSet = PressureCtrlExhaust;
                    } else if (SELECT_CASE_var == "AIRFLOWNETWORK:DISTRIBUTION:COMPONENT:RELIEFAIRFLOW") {
                        PressureControllerData(i).ControlTypeSet = PressureCtrlRelief;
                    } else { // Error
                        ShowSevereError(RoutineName + CurrentModuleObject + " object, The entered choice for " + cAlphaFields(3) +
                                        " is not valid = \"" + PressureControllerData(i).Name + "\"");
                        ShowContinueError("Valid choices are "
                                          "\"AirflowNetwork:MultiZone:Component:ZoneExhaustFan\",\"AirflowNetwork:Distribution:Component:"
                                          "ReliefAirFlow\"");
                        ShowContinueError("The input choice is " + Alphas(3));
                        ErrorsFound = true;
                    }
                }

                if (PressureControllerData(i).ControlTypeSet == PressureCtrlExhaust) {
                    if (UtilityRoutines::FindItemInList(Alphas(4), MultizoneCompExhaustFanData) == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " object, an invalid name is given:");
                        ShowContinueError(".. invalid " + cAlphaFields(4) + " = \"" + Alphas(4) + "\".");
                        ErrorsFound = true;
                    }
                }
                if (PressureControllerData(i).ControlTypeSet == PressureCtrlRelief) {
                    if (UtilityRoutines::FindItemInList(Alphas(4), DisSysCompReliefAirData) == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " object, an invalid name is given:");
                        ShowContinueError(".. invalid " + cAlphaFields(4) + " = \"" + Alphas(4) + "\".");
                        ErrorsFound = true;
                    }
                }

                if (lAlphaBlanks(5)) {
                    PressureControllerData(i).AvailSchedPtr = ScheduleAlwaysOn;
                } else {
                    PressureControllerData(i).AvailSchedPtr = GetScheduleIndex(Alphas(5));
                    if (PressureControllerData(i).AvailSchedPtr == 0) {
                        ShowSevereError(CurrentModuleObject + ", \"" + PressureControllerData(i).Name + "\" " + cAlphaFields(5) +
                                        " not found: " + Alphas(5));
                        ErrorsFound = true;
                    }
                }
                PressureControllerData(i).PresSetpointSchedPtr = GetScheduleIndex(Alphas(6));
                if (PressureControllerData(i).PresSetpointSchedPtr == 0) {
                    ShowSevereError(CurrentModuleObject + ", \"" + PressureControllerData(i).Name + "\" " + cAlphaFields(6) +
                                    " not found: " + Alphas(6));
                    ErrorsFound = true;
                }
            }
        }

        // Assign numbers of nodes and linkages
        if (SimulateAirflowNetwork > AirflowNetworkControlSimple) {
            if (AirflowNetworkSimu.iWPCCntr == iWPCCntr_Input) {
                NumOfNodesMultiZone = AirflowNetworkNumOfZones + AirflowNetworkNumOfExtNode;
            } else {
                NumOfNodesMultiZone = AirflowNetworkNumOfZones + NumOfExtNodes;
            }
            NumOfLinksMultiZone = AirflowNetworkNumOfSurfaces;
            AirflowNetworkNumOfNodes = NumOfNodesMultiZone;
            if (NumOfNodesIntraZone > 0) AirflowNetworkNumOfNodes = AirflowNetworkNumOfNodes + NumOfNodesIntraZone;
            AirflowNetworkNumOfLinks = NumOfLinksMultiZone;
            if (NumOfLinksIntraZone > 0) AirflowNetworkNumOfLinks = AirflowNetworkNumOfLinks + NumOfLinksIntraZone;
        }
        if (SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
            AirflowNetworkNumOfNodes = NumOfNodesMultiZone + DisSysNumOfNodes + NumOfNodesIntraZone;
        }

        // Assign node data
        AirflowNetworkNodeData.allocate(AirflowNetworkNumOfNodes);
        // Zone node
        for (i = 1; i <= AirflowNetworkNumOfZones; ++i) {
            AirflowNetworkNodeData(i).Name = MultizoneZoneData(i).ZoneName;
            AirflowNetworkNodeData(i).NodeTypeNum = 0;
            AirflowNetworkNodeData(i).EPlusZoneNum = MultizoneZoneData(i).ZoneNum;
            AirflowNetworkNodeData(i).NodeHeight = MultizoneZoneData(i).Height;
        }
        // External node
        if (AirflowNetworkSimu.iWPCCntr == iWPCCntr_Input) {
            for (i = AirflowNetworkNumOfZones + 1; i <= NumOfNodesMultiZone; ++i) {
                AirflowNetworkNodeData(i).Name = MultizoneExternalNodeData(i - AirflowNetworkNumOfZones).Name;
                AirflowNetworkNodeData(i).NodeTypeNum = 1;
                AirflowNetworkNodeData(i).EPlusZoneNum = 0;
                AirflowNetworkNodeData(i).NodeHeight = MultizoneExternalNodeData(i - AirflowNetworkNumOfZones).height;
                AirflowNetworkNodeData(i).ExtNodeNum = i - AirflowNetworkNumOfZones;
                AirflowNetworkNodeData(i).OutAirNodeNum = MultizoneExternalNodeData(i - AirflowNetworkNumOfZones).OutAirNodeNum;
            }
        } else { // Surface-Average input
            for (i = AirflowNetworkNumOfZones + 1; i <= NumOfNodesMultiZone; ++i) {
                n = i - AirflowNetworkNumOfZones;
                AirflowNetworkNodeData(i).Name = MultizoneExternalNodeData(n).Name;
                AirflowNetworkNodeData(i).NodeTypeNum = 1;
                AirflowNetworkNodeData(i).EPlusZoneNum = 0;
                AirflowNetworkNodeData(i).ExtNodeNum = n;
            }
        }

        // Intrazone node
        if (NumOfNodesIntraZone > 0) {
            for (i = NumOfNodesMultiZone + 1; i <= NumOfNodesMultiZone + NumOfNodesIntraZone; ++i) {
                n = i - NumOfNodesMultiZone;
                AirflowNetworkNodeData(i).Name = IntraZoneNodeData(n).Name;
                AirflowNetworkNodeData(i).NodeTypeNum = 0;
                AirflowNetworkNodeData(i).EPlusZoneNum = IntraZoneNodeData(n).ZoneNum;
                AirflowNetworkNodeData(i).NodeHeight = IntraZoneNodeData(n).Height;
                AirflowNetworkNodeData(i).RAFNNodeNum = IntraZoneNodeData(n).RAFNNodeNum;
            }
            for (i = 1; i <= AirflowNetworkNumOfZones; ++i) {
                if (MultizoneZoneData(i).RAFNNodeNum > 0) {
                    AirflowNetworkNodeData(i).RAFNNodeNum = MultizoneZoneData(i).RAFNNodeNum;
                }
            }
        }
        NumOfNodesMultiZone = NumOfNodesMultiZone + NumOfNodesIntraZone;

        // Check whether Distribution system is simulated
        if (AirflowNetworkNumOfNodes > NumOfNodesMultiZone) {
            // Search node types: OAMixerOutdoorAirStreamNode, OutdoorAir:NodeList, and OutdoorAir:Node
            j = 0;
            for (i = NumOfNodesMultiZone + 1; i <= AirflowNetworkNumOfNodes; ++i) {
                if (UtilityRoutines::SameString(DisSysNodeData(i - NumOfNodesMultiZone).EPlusType, "OAMixerOutdoorAirStreamNode")) {
                    ++j;
                }
                if (UtilityRoutines::SameString(DisSysNodeData(i - NumOfNodesMultiZone).EPlusType, "OutdoorAir:NodeList")) {
                    ++j;
                }
                if (UtilityRoutines::SameString(DisSysNodeData(i - NumOfNodesMultiZone).EPlusType, "OutdoorAir:Node")) {
                    ++j;
                }
            }

            for (i = NumOfNodesMultiZone + 1; i <= AirflowNetworkNumOfNodes; ++i) {
                AirflowNetworkNodeData(i).Name = DisSysNodeData(i - NumOfNodesMultiZone).Name;
                AirflowNetworkNodeData(i).NodeTypeNum = 0;
                AirflowNetworkNodeData(i).EPlusZoneNum = 0;
                AirflowNetworkNodeData(i).NodeHeight = DisSysNodeData(i - NumOfNodesMultiZone).Height;
                AirflowNetworkNodeData(i).EPlusNodeNum = DisSysNodeData(i - NumOfNodesMultiZone).EPlusNodeNum;
                // Get mixer information
                if (UtilityRoutines::SameString(DisSysNodeData(i - NumOfNodesMultiZone).EPlusType, "AirLoopHVAC:ZoneMixer")) {
                    AirflowNetworkNodeData(i).EPlusTypeNum = EPlusTypeNum_MIX;
                }
                // Get splitter information
                if (UtilityRoutines::SameString(DisSysNodeData(i - NumOfNodesMultiZone).EPlusType, "AirLoopHVAC:ZoneSplitter")) {
                    AirflowNetworkNodeData(i).EPlusTypeNum = EPlusTypeNum_SPL;
                }
                // Get outside air system information
                if (UtilityRoutines::SameString(DisSysNodeData(i - NumOfNodesMultiZone).EPlusType, "AirLoopHVAC:OutdoorAirSystem")) {
                    AirflowNetworkNodeData(i).EPlusTypeNum = EPlusTypeNum_OAN;
                }
                // Get OA system inlet information 'OAMixerOutdoorAirStreamNode' was specified as an outdoor air node implicitly
                if (UtilityRoutines::SameString(DisSysNodeData(i - NumOfNodesMultiZone).EPlusType, "OAMixerOutdoorAirStreamNode")) {
                    AirflowNetworkNodeData(i).EPlusTypeNum = EPlusTypeNum_EXT;
                    AirflowNetworkNodeData(i).ExtNodeNum = AirflowNetworkNumOfExtNode + 1;
                    AirflowNetworkNodeData(i).NodeTypeNum = 1;
                }
                if (UtilityRoutines::SameString(DisSysNodeData(i - NumOfNodesMultiZone).EPlusType, "OutdoorAir:NodeList") ||
                    UtilityRoutines::SameString(DisSysNodeData(i - NumOfNodesMultiZone).EPlusType, "OutdoorAir:Node")) {
                    if (j > 1) {
                        AirflowNetworkNodeData(i).EPlusTypeNum = EPlusTypeNum_EXT;
                        AirflowNetworkNodeData(i).ExtNodeNum = AirflowNetworkNumOfExtNode + 1;
                        AirflowNetworkNodeData(i).NodeTypeNum = 1;
                    } else {
                        ShowSevereError(RoutineName + "AirflowNetwork:Distribution:Node: The outdoor air node is found at " +
                                        AirflowNetworkNodeData(i).Name);
                        ShowContinueError("The node with Component Object Type = OAMixerOutdoorAirStreamNode is not found. Please check inputs.");
                        ErrorsFound = true;
                    }
                }
            }
        }

        // Start to assembly AirflowNetwork Components
        AirflowNetworkNumOfComps = AirflowNetworkNumOfDetOpenings + AirflowNetworkNumOfSimOpenings + AirflowNetworkNumOfSurCracks +
                                   AirflowNetworkNumOfSurELA + DisSysNumOfLeaks + DisSysNumOfELRs + DisSysNumOfDucts + DisSysNumOfDampers +
                                   DisSysNumOfCVFs + DisSysNumOfDetFans + DisSysNumOfCPDs + DisSysNumOfCoils + DisSysNumOfTermUnits +
                                   AirflowNetworkNumOfExhFan + DisSysNumOfHXs + AirflowNetworkNumOfHorOpenings + NumOfOAFans + NumOfReliefFans;
        AirflowNetworkCompData.allocate(AirflowNetworkNumOfComps);

        for (i = 1; i <= AirflowNetworkNumOfDetOpenings; ++i) { // Detailed opening component
            AirflowNetworkCompData(i).Name = MultizoneCompDetOpeningData(i).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_DOP;
            AirflowNetworkCompData(i).TypeNum = i;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j = AirflowNetworkNumOfDetOpenings;
        for (i = 1 + j; i <= AirflowNetworkNumOfSimOpenings + j; ++i) { // Simple opening component
            n = i - j;
            AirflowNetworkCompData(i).Name = MultizoneCompSimpleOpeningData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_SOP;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += AirflowNetworkNumOfSimOpenings;
        for (i = 1 + j; i <= AirflowNetworkNumOfSurCracks + j; ++i) { // Surface crack component
            n = i - j;
            AirflowNetworkCompData(i).Name = MultizoneSurfaceCrackData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_SCR;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += AirflowNetworkNumOfSurCracks;
        for (i = 1 + j; i <= AirflowNetworkNumOfSurELA + j; ++i) { // Surface crack component
            n = i - j;
            AirflowNetworkCompData(i).Name = MultizoneSurfaceELAData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_SEL;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += AirflowNetworkNumOfSurELA;
        for (i = 1 + j; i <= AirflowNetworkNumOfExhFan + j; ++i) { // Zone exhaust fan component
            n = i - j;
            AirflowNetworkCompData(i).Name = MultizoneCompExhaustFanData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_EXF;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += AirflowNetworkNumOfExhFan;
        for (i = 1 + j; i <= AirflowNetworkNumOfHorOpenings + j; ++i) { // Distribution system crack component
            n = i - j;
            AirflowNetworkCompData(i).Name = MultizoneCompHorOpeningData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_HOP;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += AirflowNetworkNumOfHorOpenings;
        for (i = 1 + j; i <= DisSysNumOfLeaks + j; ++i) { // Distribution system crack component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompLeakData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_PLR;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += DisSysNumOfLeaks;
        for (i = 1 + j; i <= DisSysNumOfELRs + j; ++i) { // Distribution system effective leakage ratio component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompELRData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_ELR;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += DisSysNumOfELRs;
        for (i = 1 + j; i <= DisSysNumOfDucts + j; ++i) { // Distribution system effective leakage ratio component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompDuctData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_DWC;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += DisSysNumOfDucts;
        for (i = 1 + j; i <= DisSysNumOfDampers + j; ++i) { // Distribution system effective leakage ratio component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompDamperData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_DMP;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += DisSysNumOfDampers;
        for (i = 1 + j; i <= DisSysNumOfCVFs + j; ++i) { // Distribution system constant volume fan component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompCVFData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_CVF;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
            AirflowNetworkCompData(i).EPlusTypeNum = EPlusTypeNum_FAN;
        }

        j += DisSysNumOfCVFs;
        for (i = 1 + j; i <= DisSysNumOfDetFans + j; ++i) { // Distribution system fan component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompDetFanData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_FAN;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
            AirflowNetworkCompData(i).EPlusTypeNum = EPlusTypeNum_FAN;
        }

        j += DisSysNumOfDetFans;
        for (i = 1 + j; i <= DisSysNumOfCPDs + j; ++i) { // Distribution system constant pressure drop component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompCPDData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_CPD;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += DisSysNumOfCPDs;
        for (i = 1 + j; i <= DisSysNumOfCoils + j; ++i) { // Distribution system coil component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompCoilData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_COI;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
            AirflowNetworkCompData(i).EPlusTypeNum = EPlusTypeNum_COI;
        }

        j += DisSysNumOfCoils;
        for (i = 1 + j; i <= DisSysNumOfTermUnits + j; ++i) { // Terminal unit component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompTermUnitData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_TMU;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
            AirflowNetworkCompData(i).EPlusTypeNum = EPlusTypeNum_RHT;
        }

        j += DisSysNumOfTermUnits;
        for (i = 1 + j; i <= DisSysNumOfHXs + j; ++i) { // Distribution system heat exchanger component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompHXData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_HEX;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
            AirflowNetworkCompData(i).EPlusTypeNum = EPlusTypeNum_HEX;
        }

        j += DisSysNumOfHXs;
        for (i = 1 + j; i <= NumOfOAFans + j; ++i) { // OA fan component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompOutdoorAirData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_OAF;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        j += NumOfOAFans;
        for (i = 1 + j; i <= NumOfReliefFans + j; ++i) { // OA fan component
            n = i - j;
            AirflowNetworkCompData(i).Name = DisSysCompReliefAirData(n).Name;
            AirflowNetworkCompData(i).CompTypeNum = CompTypeNum_REL;
            AirflowNetworkCompData(i).TypeNum = n;
            AirflowNetworkCompData(i).EPlusName = "";
            AirflowNetworkCompData(i).EPlusCompName = "";
            AirflowNetworkCompData(i).EPlusType = "";
            AirflowNetworkCompData(i).CompNum = i;
        }

        // Assign linkage data

        // Read AirflowNetwork linkage data
        CurrentModuleObject = "AirflowNetwork:Distribution:Linkage";
        DisSysNumOfLinks = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        if (DisSysNumOfLinks > 0 && SimulateAirflowNetwork > AirflowNetworkControlMultizone) { // Multizone + Distribution
            AirflowNetworkNumOfLinks = NumOfLinksMultiZone + DisSysNumOfLinks;
            AirflowNetworkLinkageData.allocate(DisSysNumOfLinks + AirflowNetworkNumOfSurfaces);
        } else { // Multizone + IntraZone only
            //	AirflowNetworkLinkageData.allocate( AirflowNetworkNumOfSurfaces );
            AirflowNetworkLinkageData.allocate(AirflowNetworkNumOfLinks);
        }

        // Assign Multizone linkage based on surfaces, by assuming every surface has a crack or opening
        j = 0;
        for (count = 1; count <= AirflowNetworkNumOfSurfaces; ++count) {
            if (MultizoneSurfaceData(count).SurfNum == 0) continue;
            AirflowNetworkLinkageData(count).Name = MultizoneSurfaceData(count).SurfName;
            AirflowNetworkLinkageData(count).NodeNums[0] = MultizoneSurfaceData(count).NodeNums[0];
            AirflowNetworkLinkageData(count).NodeNums[1] = MultizoneSurfaceData(count).NodeNums[1];
            AirflowNetworkLinkageData(count).CompName = MultizoneSurfaceData(count).OpeningName;
            AirflowNetworkLinkageData(count).ZoneNum = 0;
            AirflowNetworkLinkageData(count).LinkNum = count;
            AirflowNetworkLinkageData(count).NodeHeights[0] = MultizoneSurfaceData(count).CHeight;
            AirflowNetworkLinkageData(count).NodeHeights[1] = MultizoneSurfaceData(count).CHeight;
            if (!WorldCoordSystem) {
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(count).NodeNums[0]).EPlusZoneNum > 0) {
                    AirflowNetworkLinkageData(count).NodeHeights[0] -=
                        Zone(AirflowNetworkNodeData(AirflowNetworkLinkageData(count).NodeNums[0]).EPlusZoneNum).OriginZ;
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(count).NodeNums[1]).EPlusZoneNum > 0) {
                    AirflowNetworkLinkageData(count).NodeHeights[1] -=
                        Zone(AirflowNetworkNodeData(AirflowNetworkLinkageData(count).NodeNums[1]).EPlusZoneNum).OriginZ;
                }
            }
            // Find component number
            found = false;
            for (i = 1; i <= AirflowNetworkNumOfComps; ++i) {
                if (AirflowNetworkLinkageData(count).CompName == AirflowNetworkCompData(i).Name) {
                    AirflowNetworkLinkageData(count).CompNum = i;
                    found = true;
                    if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_DOP) {
                        ++j;
                        AirflowNetworkLinkageData(count).DetOpenNum = j;
                        MultizoneSurfaceData(count).Multiplier = Surface(MultizoneSurfaceData(count).SurfNum).Multiplier;
                        if (Surface(MultizoneSurfaceData(count).SurfNum).Tilt < 10.0 || Surface(MultizoneSurfaceData(count).SurfNum).Tilt > 170.0) {
                            ShowWarningError("An AirflowNetwork:Multizone:Surface object has an air-flow opening corresponding to");
                            ShowContinueError("window or door = " + MultizoneSurfaceData(count).SurfName + ", which is within ");
                            ShowContinueError("10 deg of being horizontal. Airflows through large horizontal openings are poorly");
                            ShowContinueError("modeled in the AirflowNetwork model resulting in only one-way airflow.");
                        }
                        if (!(SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_Window ||
                              SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_GlassDoor ||
                              SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_Door)) {
                            ShowSevereError(RoutineName + "AirflowNetworkComponent: The opening must be assigned to a window, door or glassdoor at " +
                                            AirflowNetworkLinkageData(count).Name);
                            ErrorsFound = true;
                        }
                        if (SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_Door ||
                            SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_GlassDoor) {
                            if (MultizoneCompDetOpeningData(AirflowNetworkCompData(i).TypeNum).LVOType == 2) {
                                ShowSevereError(
                                    RoutineName +
                                    "AirflowNetworkComponent: The opening with horizontally pivoted type must be assigned to a window surface at " +
                                    AirflowNetworkLinkageData(count).Name);
                                ErrorsFound = true;
                            }
                        }
                    }
                    if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_SOP) {
                        MultizoneSurfaceData(count).Multiplier = Surface(MultizoneSurfaceData(count).SurfNum).Multiplier;
                        if (Surface(MultizoneSurfaceData(count).SurfNum).Tilt < 10.0 || Surface(MultizoneSurfaceData(count).SurfNum).Tilt > 170.0) {
                            ShowSevereError("An AirflowNetwork:Multizone:Surface object has an air-flow opening corresponding to");
                            ShowContinueError("window or door = " + MultizoneSurfaceData(count).SurfName + ", which is within");
                            ShowContinueError("10 deg of being horizontal. Airflows through horizontal openings are not allowed.");
                            ShowContinueError("AirflowNetwork:Multizone:Component:SimpleOpening = " + AirflowNetworkCompData(i).Name);
                            ErrorsFound = true;
                        }
                        if (!(SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_Window ||
                              SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_GlassDoor ||
                              SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_Door)) {
                            ShowSevereError(RoutineName + "AirflowNetworkComponent: The opening must be assigned to a window, door or glassdoor at " +
                                            AirflowNetworkLinkageData(count).Name);
                            ErrorsFound = true;
                        }
                    }
                    if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_HOP) {
                        MultizoneSurfaceData(count).Multiplier = Surface(MultizoneSurfaceData(count).SurfNum).Multiplier;
                        // Get linkage height from upper and lower zones
                        if (MultizoneZoneData(AirflowNetworkLinkageData(count).NodeNums[0]).ZoneNum > 0) {
                            AirflowNetworkLinkageData(count).NodeHeights[0] =
                                Zone(MultizoneZoneData(AirflowNetworkLinkageData(count).NodeNums[0]).ZoneNum).Centroid.z;
                        }
                        if (AirflowNetworkLinkageData(count).NodeNums[1] <= AirflowNetworkNumOfZones) {
                            if (MultizoneZoneData(AirflowNetworkLinkageData(count).NodeNums[1]).ZoneNum > 0) {
                                AirflowNetworkLinkageData(count).NodeHeights[1] =
                                    Zone(MultizoneZoneData(AirflowNetworkLinkageData(count).NodeNums[1]).ZoneNum).Centroid.z;
                            }
                        }
                        if (AirflowNetworkLinkageData(count).NodeNums[1] > AirflowNetworkNumOfZones) {
                            ShowSevereError(RoutineName +
                                            "AirflowNetworkComponent: The horizontal opening must be located between two thermal zones at " +
                                            AirflowNetworkLinkageData(count).Name);
                            ShowContinueError("This component is exposed to outdoors.");
                            ErrorsFound = true;
                        } else {
                            if (!(MultizoneZoneData(AirflowNetworkLinkageData(count).NodeNums[0]).ZoneNum > 0 &&
                                  MultizoneZoneData(AirflowNetworkLinkageData(count).NodeNums[1]).ZoneNum > 0)) {
                                ShowSevereError(RoutineName +
                                                "AirflowNetworkComponent: The horizontal opening must be located between two thermal zones at " +
                                                AirflowNetworkLinkageData(count).Name);
                                ErrorsFound = true;
                            }
                        }
                        if (!(Surface(MultizoneSurfaceData(count).SurfNum).Tilt > 170.0 &&
                              Surface(MultizoneSurfaceData(count).SurfNum).Tilt < 190.0) &&
                            !(Surface(MultizoneSurfaceData(count).SurfNum).Tilt > -10.0 &&
                              Surface(MultizoneSurfaceData(count).SurfNum).Tilt < 10.0)) {
                            ShowWarningError("An AirflowNetwork:Multizone:Surface object has an air-flow opening corresponding to");
                            ShowContinueError("window or door = " + MultizoneSurfaceData(count).SurfName + ", which is above");
                            ShowContinueError("10 deg of being horizontal. Airflows through non-horizontal openings are not modeled");
                            ShowContinueError("with the object of AirflowNetwork:Multizone:Component:HorizontalOpening = " +
                                              AirflowNetworkCompData(i).Name);
                        }
                        if (!(SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_Window ||
                              SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_GlassDoor ||
                              SurfaceWindow(MultizoneSurfaceData(count).SurfNum).OriginalClass == SurfaceClass_Door)) {
                            ShowSevereError(RoutineName + "AirflowNetworkComponent: The opening must be assigned to a window, door or glassdoor at " +
                                            AirflowNetworkLinkageData(count).Name);
                            ErrorsFound = true;
                        }
                    }
                    break;
                }
            }
            if (!found) {
                ShowSevereError(RoutineName + CurrentModuleObject + ": The component is not defined in " + AirflowNetworkLinkageData(count).Name);
                ErrorsFound = true;
            }
        }

        // Assign intrazone links
        for (count = 1 + AirflowNetworkNumOfSurfaces; count <= NumOfLinksIntraZone + AirflowNetworkNumOfSurfaces; ++count) {
            AirflowNetworkLinkageData(count).Name = IntraZoneLinkageData(count - AirflowNetworkNumOfSurfaces).Name;
            AirflowNetworkLinkageData(count).NodeNums[0] = IntraZoneLinkageData(count - AirflowNetworkNumOfSurfaces).NodeNums[0];
            AirflowNetworkLinkageData(count).NodeNums[1] = IntraZoneLinkageData(count - AirflowNetworkNumOfSurfaces).NodeNums[1];
            AirflowNetworkLinkageData(count).CompName = IntraZoneLinkageData(count - AirflowNetworkNumOfSurfaces).CompName;
            AirflowNetworkLinkageData(count).ZoneNum = 0;
            AirflowNetworkLinkageData(count).LinkNum = count;
            AirflowNetworkLinkageData(count).NodeHeights[0] = IntraZoneLinkageData(count - AirflowNetworkNumOfSurfaces).NodeHeights[0];
            AirflowNetworkLinkageData(count).NodeHeights[1] = IntraZoneLinkageData(count - AirflowNetworkNumOfSurfaces).NodeHeights[1];
            // Find component number
            found = false;
            for (i = 1; i <= AirflowNetworkNumOfComps; ++i) {
                if (AirflowNetworkLinkageData(count).CompName == AirflowNetworkCompData(i).Name) {
                    AirflowNetworkLinkageData(count).CompNum = i;
                    if (AirflowNetworkCompData(i).CompTypeNum != CompTypeNum_SCR && AirflowNetworkCompData(i).CompTypeNum != CompTypeNum_SEL) {
                        ShowSevereError(RoutineName + AirflowNetworkLinkageData(count).CompName + ": The component is not allowed in " +
                                        AirflowNetworkLinkageData(count).Name);
                        ShowContinueError("The allowed component type is either AirflowNetwork:MultiZone:Surface:Crack or "
                                          "AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea.");
                        ErrorsFound = true;
                    }
                    found = true;
                    break;
                }
            }
            if (!found) {
                ShowSevereError(RoutineName + AirflowNetworkLinkageData(count).CompName + ": The component is not defined in " +
                                AirflowNetworkLinkageData(count).Name);
                ErrorsFound = true;
            }
        }

        // Reset AirflowNetworkNumOfSurfaces by including NumOfLinksIntraZone
        AirflowNetworkNumOfSurfaces = AirflowNetworkNumOfSurfaces + NumOfLinksIntraZone;
        if (NumOfLinksIntraZone > 0) NumOfLinksMultiZone = AirflowNetworkNumOfSurfaces;

        // Assign AirflowNetwork info in RoomAirflowNetworkZoneInfo
        if (NumOfNodesIntraZone > 0) {
            for (i = 1; i <= NumOfNodesMultiZone; ++i) {
                n = AirflowNetworkNodeData(i).EPlusZoneNum;
                AirflowNetworkNodeData(i).NumOfLinks = 0;
                if (n > 0 && AirflowNetworkNodeData(i).RAFNNodeNum > 0) {
                    RoomAirflowNetworkZoneInfo(n).Node(AirflowNetworkNodeData(i).RAFNNodeNum).AirflowNetworkNodeID = i;
                    for (j = 1; j <= AirflowNetworkNumOfSurfaces; ++j) {
                        if (AirflowNetworkLinkageData(j).NodeNums[0] == i) {
                            AirflowNetworkNodeData(i).NumOfLinks = AirflowNetworkNodeData(i).NumOfLinks + 1;
                        } else if (AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                            AirflowNetworkNodeData(i).NumOfLinks = AirflowNetworkNodeData(i).NumOfLinks + 1;
                        } else {
                        }
                    }
                }
                if (AirflowNetworkNodeData(i).RAFNNodeNum > 0) {
                    for (j = 1; j <= RoomAirflowNetworkZoneInfo(n).NumOfAirNodes; ++j) {
                        if (RoomAirflowNetworkZoneInfo(n).Node(j).AirflowNetworkNodeID == i) {
                            RoomAirflowNetworkZoneInfo(n).Node(j).NumOfAirflowLinks = AirflowNetworkNodeData(i).NumOfLinks;
                            RoomAirflowNetworkZoneInfo(n).Node(j).Link.allocate(AirflowNetworkNodeData(i).NumOfLinks);
                            k = 1;
                            for (m = 1; m <= AirflowNetworkNumOfSurfaces; ++m) {
                                if (AirflowNetworkLinkageData(m).NodeNums[0] == i) {
                                    RoomAirflowNetworkZoneInfo(n).Node(j).Link(k).AirflowNetworkLinkSimuID = m;
                                    RoomAirflowNetworkZoneInfo(n).Node(j).Link(k).AirflowNetworkLinkageDataID = m;
                                    k = k + 1;
                                    if (k > AirflowNetworkNodeData(i).NumOfLinks) break;
                                }
                                if (AirflowNetworkLinkageData(m).NodeNums[1] == i) {
                                    RoomAirflowNetworkZoneInfo(n).Node(j).Link(k).AirflowNetworkLinkSimuID = m;
                                    RoomAirflowNetworkZoneInfo(n).Node(j).Link(k).AirflowNetworkLinkageDataID = m;
                                    k = k + 1;
                                    if (k > AirflowNetworkNodeData(i).NumOfLinks) break;
                                }
                            }
                        }
                    }
                }
            }
        }

        if (DisSysNumOfLinks > 0 && SimulateAirflowNetwork > AirflowNetworkControlMultizone) { // Distribution

            for (auto &e : AirflowNetworkLinkageData)
                e.ZoneNum = 0;

            for (count = AirflowNetworkNumOfSurfaces + 1; count <= AirflowNetworkNumOfLinks; ++count) {

                inputProcessor->getObjectItem(CurrentModuleObject,
                                              count - AirflowNetworkNumOfSurfaces,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);
                UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
                AirflowNetworkLinkageData(count).Name = Alphas(1);
                AirflowNetworkLinkageData(count).NodeNames[0] = Alphas(2);
                AirflowNetworkLinkageData(count).NodeHeights[0] = 0.0;
                AirflowNetworkLinkageData(count).NodeNames[1] = Alphas(3);
                AirflowNetworkLinkageData(count).NodeHeights[1] = 0.0;
                AirflowNetworkLinkageData(count).CompName = Alphas(4);
                AirflowNetworkLinkageData(count).ZoneName = Alphas(5);
                AirflowNetworkLinkageData(count).LinkNum = count;

                for (int i = 1; i <= DisSysNumOfDuctViewFactors; ++i) {
                    if (AirflowNetworkLinkageData(count).Name == AirflowNetworkLinkageViewFactorData(i).LinkageName) {
                        AirflowNetworkLinkageData(count).LinkageViewFactorObjectNum = AirflowNetworkLinkageViewFactorData(i).ObjectNum;
                        break;
                    }
                }

                if (!lAlphaBlanks(5)) {
                    AirflowNetworkLinkageData(count).ZoneNum = UtilityRoutines::FindItemInList(AirflowNetworkLinkageData(count).ZoneName, Zone);
                    if (AirflowNetworkLinkageData(count).ZoneNum == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + ": Invalid " + cAlphaFields(5) +
                                        " given = " + AirflowNetworkLinkageData(count).ZoneName);
                        ErrorsFound = true;
                    }
                }
                if (Alphas(2) == Alphas(3)) {
                    ShowSevereError(RoutineName + CurrentModuleObject + ", " + cAlphaFields(2) + " = " + cAlphaFields(3) + " in " +
                                    AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
                // Find component number
                found = false;
                for (i = 1; i <= AirflowNetworkNumOfComps; ++i) {
                    if (AirflowNetworkLinkageData(count).CompName == AirflowNetworkCompData(i).Name) {
                        AirflowNetworkLinkageData(count).CompNum = i;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    ShowSevereError(RoutineName + CurrentModuleObject + ": The " + cAlphaFields(4) + " is not defined in " +
                                    AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
                // Find Node number
                found = false;
                for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
                    if (AirflowNetworkLinkageData(count).NodeNames[0] == AirflowNetworkNodeData(i).Name) {
                        AirflowNetworkLinkageData(count).NodeNums[0] = i;
                        AirflowNetworkLinkageData(count).NodeHeights[0] += AirflowNetworkNodeData(i).NodeHeight;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    ShowSevereError(RoutineName + CurrentModuleObject + ": The " + cAlphaFields(2) + " is not found in the node data " +
                                    AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
                for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
                    if (AirflowNetworkLinkageData(count).NodeNames[1] == AirflowNetworkNodeData(i).Name) {
                        AirflowNetworkLinkageData(count).NodeNums[1] = i;
                        AirflowNetworkLinkageData(count).NodeHeights[1] += AirflowNetworkNodeData(i).NodeHeight;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    ShowSevereError(RoutineName + CurrentModuleObject + ": The " + cAlphaFields(3) + " is not found in the node data " +
                                    AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
            }

        } else {

            if (SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
                ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object is required but not found.");
                ErrorsFound = true;
            }
        }

        // Ensure no duplicated names in AirflowNetwork component objects
        for (i = 1; i <= AirflowNetworkNumOfComps; ++i) {
            for (j = i + 1; j <= AirflowNetworkNumOfComps; ++j) {
                if (UtilityRoutines::SameString(AirflowNetworkCompData(i).Name, AirflowNetworkCompData(j).Name)) {
                    // SurfaceAirflowLeakageNames
                    if (i <= 4 && j <= 4) {
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_DOP)
                            CompName(1) = "AirflowNetwork:MultiZone:Component:DetailedOpening";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_SOP)
                            CompName(1) = "AirflowNetwork:MultiZone:Component:SimpleOpening";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_SCR) CompName(1) = "AirflowNetwork:MultiZone:Surface:Crack";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_SEL)
                            CompName(1) = "AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_DOP)
                            CompName(2) = "AirflowNetwork:MultiZone:Component:DetailedOpening";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_SOP)
                            CompName(2) = "AirflowNetwork:MultiZone:Component:SimpleOpening";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_SCR) CompName(2) = "AirflowNetwork:MultiZone:Surface:Crack";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_SEL)
                            CompName(2) = "AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea";
                        ShowSevereError(RoutineName + "Duplicated component names are found = " + AirflowNetworkCompData(i).Name);
                        ShowContinueError("A unique component name is required in both objects " + CompName(1) + " and " + CompName(2));
                        ErrorsFound = true;
                    }
                    // Distribution component
                    if (i > 4 && j > 4) {
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_PLR) CompName(1) = "AirflowNetwork:Distribution:Component:Leak";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_DWC) CompName(1) = "AirflowNetwork:Distribution:Component:Duct";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_ELR)
                            CompName(1) = "AirflowNetwork:Distribution:Component:LeakageRatio";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_DMP) CompName(1) = "AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DAMPER";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_CVF) CompName(1) = "AirflowNetwork:Distribution:Component:Fan";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_CPD)
                            CompName(1) = "AirflowNetwork:Distribution:Component:ConstantPressureDrop";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_COI) CompName(1) = "AirflowNetwork:Distribution:Component:Coil";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_TMU)
                            CompName(1) = "AirflowNetwork:Distribution:Component:TerminalUnit";
                        if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_HEX)
                            CompName(1) = "AirflowNetwork:Distribution:Component:HeatExchanger";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_PLR) CompName(2) = "AirflowNetwork:Distribution:Component:Leak";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_DWC) CompName(2) = "AirflowNetwork:Distribution:Component:Duct";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_ELR)
                            CompName(2) = "AirflowNetwork:Distribution:Component:LeakageRatio";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_DMP) CompName(2) = "AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DAMPER";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_CVF) CompName(2) = "AirflowNetwork:Distribution:Component:Fan";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_CPD)
                            CompName(2) = "AirflowNetwork:Distribution:Component:ConstantPressureDrop";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_COI) CompName(2) = "AirflowNetwork:Distribution:Component:Coil";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_TMU)
                            CompName(2) = "AirflowNetwork:Distribution:Component:TerminalUnit";
                        if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_HEX)
                            CompName(2) = "AirflowNetwork:Distribution:Component:HeatExchanger";
                        ShowSevereError(RoutineName + "Duplicated component names are found = " + AirflowNetworkCompData(i).Name);
                        ShowContinueError("A unique component name is required in both objects " + CompName(1) + " and " + CompName(2));
                        ErrorsFound = true;
                    }
                }
            }
        }

        // Node and component validation
        for (count = 1; count <= AirflowNetworkNumOfLinks; ++count) {
            NodeFound = false;
            for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
                if (i == AirflowNetworkLinkageData(count).NodeNums[0]) {
                    NodeFound = true;
                    break;
                }
            }
            if (!NodeFound) {
                if (count <= AirflowNetworkNumOfSurfaces) {
                    ShowSevereError(RoutineName + AirflowNetworkLinkageData(count).NodeNames[0] +
                                    " in AIRFLOWNETWORK:MULTIZONE:SURFACE = " + AirflowNetworkLinkageData(count).Name + " is not found");
                    // MBA: Always false due to same boolean check but I don't know what the correct logic should be. 01/10/2016
                    // } else if ( count <= AirflowNetworkNumOfSurfaces ) {
                    // 	ShowSevereError( RoutineName + AirflowNetworkLinkageData( count ).NodeNames( 1 ) + " in AIRFLOWNETWORK:INTRAZONE:LINKAGE = " +
                    // AirflowNetworkLinkageData( count ).Name + " is not found" );
                } else {
                    ShowSevereError(RoutineName + AirflowNetworkLinkageData(count).NodeNames[0] + " in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = " +
                                    AirflowNetworkLinkageData(count).Name + " is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE objects.");
                }
                ErrorsFound = true;
            }
            NodeFound = false;
            for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
                if (i == AirflowNetworkLinkageData(count).NodeNums[1]) {
                    NodeFound = true;
                    break;
                }
            }
            if (!NodeFound) {
                if (count <= AirflowNetworkNumOfSurfaces) {
                    ShowSevereError(RoutineName + AirflowNetworkLinkageData(count).NodeNames[0] +
                                    " in AIRFLOWNETWORK:MULTIZONE:SURFACE = " + AirflowNetworkLinkageData(count).Name + " is not found");
                } else {
                    ShowSevereError(RoutineName + AirflowNetworkLinkageData(count).NodeNames[1] + " in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = " +
                                    AirflowNetworkLinkageData(count).Name + " is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE objects.");
                }
                ErrorsFound = true;
            }
            CompFound = false;
            for (i = 1; i <= AirflowNetworkNumOfComps; ++i) {
                if (i == AirflowNetworkLinkageData(count).CompNum) {
                    CompFound = true;
                }
            }
            if (!CompFound) {
                ShowSevereError(RoutineName + "Component = " + AirflowNetworkLinkageData(count).CompName +
                                " in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = " + AirflowNetworkLinkageData(count).Name +
                                " is not found in AirflowNetwork Component Data objects.");
                ErrorsFound = true;
            }
        }

        // Ensure every AirflowNetworkNode is used in AirflowNetworkLinkage
        for (count = 1; count <= AirflowNetworkNumOfNodes; ++count) {
            NodeFound1 = false;
            NodeFound2 = false;
            for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
                if (count == AirflowNetworkLinkageData(i).NodeNums[0]) {
                    NodeFound1 = true;
                }
                if (count == AirflowNetworkLinkageData(i).NodeNums[1]) {
                    NodeFound2 = true;
                }
            }
            if ((!NodeFound1) && count > NumOfNodesMultiZone && AirflowNetworkNodeData(count).ExtNodeNum == 0) {
                ShowSevereError(RoutineName + "AIRFLOWNETWORK:DISTRIBUTION:NODE = " + AirflowNetworkNodeData(count).Name +
                                " is not found as Node 1 Name in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ShowContinueError(
                    "Each non-external AIRFLOWNETWORK:DISTRIBUTION:NODE has to be defined as Node 1 once in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ErrorsFound = true;
            }
            if ((!NodeFound2) && count > NumOfNodesMultiZone && AirflowNetworkNodeData(count).ExtNodeNum == 0) {
                ShowSevereError(RoutineName + "AIRFLOWNETWORK:DISTRIBUTION:NODE = " + AirflowNetworkNodeData(count).Name +
                                " is not found as Node 2 Name in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ShowContinueError(
                    "Each non-external AIRFLOWNETWORK:DISTRIBUTION:NODE has to be defined as Node 2 once in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ErrorsFound = true;
            }
            if ((!NodeFound1) && (!NodeFound2) && count > NumOfNodesMultiZone && AirflowNetworkNodeData(count).ExtNodeNum > 0) {
                ShowSevereError(RoutineName + "AIRFLOWNETWORK:DISTRIBUTION:NODE = " + AirflowNetworkNodeData(count).Name +
                                " is not found as Node 1 Name or Node 2 Name in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ShowContinueError("This external AIRFLOWNETWORK:DISTRIBUTION:NODE has to be defined in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ErrorsFound = true;
            }
        }

        // Ensure there is at least one node defined as EXTERNAL node
        NodeFound = false;
        for (count = 1; count <= AirflowNetworkNumOfNodes; ++count) {
            if (AirflowNetworkNodeData(count).ExtNodeNum > 0) {
                NodeFound = true;
            }
        }
        if (!NodeFound) {
            ShowSevereError(RoutineName +
                            "No External Nodes found in AirflowNetwork:Multizone:ExternalNode. There must be at least 1 external node defined.");
            ErrorsFound = true;
        }

        if (AirflowNetworkSimu.iWPCCntr == iWPCCntr_Input) {
            for (count = 1; count <= AirflowNetworkNumOfSurfaces; ++count) {
                if (AirflowNetworkLinkageData(count).NodeNums[0] == 0) {
                    ShowSevereError("The surface is not found in AIRFLOWNETWORK:MULTIZONE:SURFACE = " + AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
                if (AirflowNetworkLinkageData(count).NodeNums[1] == 0) {
                    ShowSevereError("The external node is not found in AIRFLOWNETWORK:MULTIZONE:SURFACE = " + AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
            }
        }

        // Provide a warning when a door component is assigned as envelope leakage
        if (!ErrorsFound) {
            for (count = 1; count <= AirflowNetworkNumOfSurfaces; ++count) {
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(count).NodeNums[0]).ExtNodeNum > 0 &&
                    AirflowNetworkNodeData(AirflowNetworkLinkageData(count).NodeNums[1]).EPlusZoneNum > 0 &&
                    AirflowNetworkLinkageData(count).CompNum > 0) {
                    if (AirflowNetworkCompData(AirflowNetworkLinkageData(count).CompNum).CompTypeNum == CompTypeNum_SOP) {
                        //            CALL ShowWarningError('A door component is assigned between an external node and a thermal zone ' &
                        //                 //'in AirflowNetwork linkage data = '//TRIM(AirflowNetworkLinkageData(count)%Name))
                        //            CALL ShowContinueError('This represents a large opening between indoor and outdoors. You may want to ' &
                        //                                   //'reconsider your input.')
                    }
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(count).NodeNums[1]).ExtNodeNum > 0 &&
                    AirflowNetworkNodeData(AirflowNetworkLinkageData(count).NodeNums[0]).EPlusZoneNum > 0 &&
                    AirflowNetworkLinkageData(count).CompNum > 0) {
                    if (AirflowNetworkCompData(AirflowNetworkLinkageData(count).CompNum).CompTypeNum == CompTypeNum_SOP) {
                        //            CALL ShowWarningError('A door component is assigned between an external node and a thermal zone ' &
                        //                 //'in AirflowNetwork linkage data = '//TRIM(AirflowNetworkLinkageData(count)%Name))
                        //            CALL ShowContinueError('This represents a large opening between indoor and outdoors. You may want to ' &
                        //                                   //'reconsider your input.')
                    }
                }
            }
        }

        // Ensure the name of each heat exchanger is shown either once or twice in the field of
        if (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS || SimulateAirflowNetwork == AirflowNetworkControlMultiADS) {
            for (i = 1; i <= DisSysNumOfHXs; ++i) {
                count = 0;
                for (j = 1; j <= AirflowNetworkNumOfLinks; ++j) {
                    if (UtilityRoutines::SameString(AirflowNetworkLinkageData(j).CompName, DisSysCompHXData(i).Name)) {
                        ++count;
                    }
                }

                if (DisSysCompHXData(i).CoilParentExists && count != 2) {
                    ShowSevereError(RoutineName +
                                    "The inputs of component name field as a heat exchanger in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE is not correct");
                    ShowContinueError("The entered name of heat exchanger is " + DisSysCompHXData(i).Name +
                                      " in AirflowNetwork:Distribution:Component:HeatExchanger objects");
                    ShowContinueError("The correct appearance number is 2. The entered appearance number is " + RoundSigDigits(count));
                    ErrorsFound = true;
                }
                if ((!DisSysCompHXData(i).CoilParentExists) && count != 1) {
                    ShowSevereError(RoutineName +
                                    "The inputs of component name field as a heat exchanger in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE is not correct");
                    ShowContinueError("The entered name of heat exchanger is " + DisSysCompHXData(i).Name +
                                      " in AirflowNetwork:Distribution:Component:HeatExchanger objects");
                    ShowContinueError("The correct appearance number is 1. The entered appearance number is " + RoundSigDigits(count));
                    ErrorsFound = true;
                }
            }
        }

        // Check node assignments using AirflowNetwork:Distribution:Component:OutdoorAirFlow or AirflowNetwork:Distribution:Component:ReliefAirFlow
        for (count = AirflowNetworkNumOfSurfaces + 1; count <= AirflowNetworkNumOfLinks; ++count) {
            i = AirflowNetworkLinkageData(count).CompNum;
            j = AirflowNetworkLinkageData(count).NodeNums[0];
            k = AirflowNetworkLinkageData(count).NodeNums[1];

            if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_OAF) {
                if (!UtilityRoutines::SameString(DisSysNodeData(j - NumOfNodesMultiZone).EPlusType, "OAMixerOutdoorAirStreamNode")) {
                    ShowSevereError(
                        RoutineName +
                        "AirflowNetwork:Distribution:Linkage: When the component type is AirflowNetwork:Distribution:Component:OutdoorAirFlow at " +
                        AirflowNetworkNodeData(j).Name + ",");
                    ShowContinueError("the component type in the first node should be OAMixerOutdoorAirStreamNode at " +
                                      AirflowNetworkNodeData(j).Name);
                    ErrorsFound = true;
                }
                if (!UtilityRoutines::SameString(DisSysNodeData(k - NumOfNodesMultiZone).EPlusType, "AirLoopHVAC:OutdoorAirSystem")) {
                    ShowSevereError(
                        RoutineName +
                        "AirflowNetwork:Distribution:Linkage: When the component type is AirflowNetwork:Distribution:Component:OutdoorAirFlow at " +
                        AirflowNetworkNodeData(k).Name + ",");
                    ShowContinueError("the component object type in the second node should be AirLoopHVAC:OutdoorAirSystem at " +
                                      AirflowNetworkNodeData(k).Name);
                    ErrorsFound = true;
                }
            }

            if (AirflowNetworkCompData(i).CompTypeNum == CompTypeNum_REL) {
                if (!UtilityRoutines::SameString(DisSysNodeData(j - NumOfNodesMultiZone).EPlusType, "AirLoopHVAC:OutdoorAirSystem")) {
                    ShowSevereError(
                        RoutineName +
                        "AirflowNetwork:Distribution:Linkage: When the component type is AirflowNetwork:Distribution:Component:OutdoorAirFlow at " +
                        AirflowNetworkNodeData(j).Name + ",");
                    ShowContinueError("the component object type in the first node should be AirLoopHVAC:OutdoorAirSystem at " +
                                      AirflowNetworkNodeData(j).Name);
                    ErrorsFound = true;
                }
                if (!UtilityRoutines::SameString(DisSysNodeData(k - NumOfNodesMultiZone).EPlusType, "OAMixerOutdoorAirStreamNode")) {
                    ShowSevereError(
                        RoutineName +
                        "AirflowNetwork:Distribution:Linkage: When the component type is AirflowNetwork:Distribution:Component:OutdoorAirFlow at " +
                        AirflowNetworkNodeData(k).Name + ",");
                    ShowContinueError("the component type in the second node should be OAMixerOutdoorAirStreamNode at " +
                                      AirflowNetworkNodeData(k).Name);
                    ErrorsFound = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found getting inputs. Previous error(s) cause program termination.");
        }

        Alphas.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        Numbers.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (!ErrorsFound) {
            AllocateAndInitData();
        }
    }

    void InitAirflowNetwork()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Aug. 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes variables of additional zone loads caused by ADS.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        using DataHVACGlobals::TimeStepSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool OneTimeFlag(true);
        static bool MyEnvrnFlag(true);
        int i;
        int j;
        int ZoneNum;

        if (OneTimeFlag) {
            AirflowNetworkExchangeData.allocate(NumOfZones); // AirflowNetwork exchange data due to air-forced system
            for (i = 1; i <= DisSysNumOfCVFs; i++) {
                if (DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff) {
                    AirflowNetworkMultiExchangeData.allocate(NumOfZones);
                    break;
                }
            }

            OneTimeFlag = false;
            if (Contaminant.CO2Simulation) {
                for (i = 1; i <= NumOfZones; ++i) {
                    SetupOutputVariable("AFN Zone Outdoor Air Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        AirflowNetworkExchangeData(i).SumMHr,
                                        "System",
                                        "Average",
                                        Zone(i).Name);
                    SetupOutputVariable("AFN Zone Mixing Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        AirflowNetworkExchangeData(i).SumMMHr,
                                        "System",
                                        "Average",
                                        Zone(i).Name);
                    SetupOutputVariable("AFN Zone Outdoor Air CO2 Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        AirflowNetworkExchangeData(i).SumMHrCO,
                                        "System",
                                        "Average",
                                        Zone(i).Name);
                    SetupOutputVariable("AFN Zone Mixing CO2 Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        AirflowNetworkExchangeData(i).SumMMHrCO,
                                        "System",
                                        "Average",
                                        Zone(i).Name);
                    SetupOutputVariable("AFN Zone Total CO2 Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        AirflowNetworkExchangeData(i).TotalCO2,
                                        "System",
                                        "Average",
                                        Zone(i).Name);
                }
            }
            if (Contaminant.GenericContamSimulation) {
                for (i = 1; i <= NumOfZones; ++i) {
                    if (!Contaminant.CO2Simulation) {
                        SetupOutputVariable("AFN Zone Outdoor Air Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            AirflowNetworkExchangeData(i).SumMHr,
                                            "System",
                                            "Average",
                                            Zone(i).Name);
                        SetupOutputVariable("AFN Zone Mixing Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            AirflowNetworkExchangeData(i).SumMMHr,
                                            "System",
                                            "Average",
                                            Zone(i).Name);
                    }
                    SetupOutputVariable("AFN Zone Outdoor Air Generic Air Contaminant Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        AirflowNetworkExchangeData(i).SumMHrGC,
                                        "System",
                                        "Average",
                                        Zone(i).Name);
                    SetupOutputVariable("AFN Zone Mixing Generic Air Contaminant Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        AirflowNetworkExchangeData(i).SumMMHrGC,
                                        "System",
                                        "Average",
                                        Zone(i).Name);
                    SetupOutputVariable("AFN Zone Total Generic Air Contaminant Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        AirflowNetworkExchangeData(i).TotalGC,
                                        "System",
                                        "Average",
                                        Zone(i).Name);
                }
            }
        }

        if (BeginEnvrnFlag && MyEnvrnFlag) {
            // Assign node values
            for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
                AirflowNetworkNodeSimu(i).TZ = 23.0;
                AirflowNetworkNodeSimu(i).WZ = 0.00084;
                AirflowNetworkNodeSimu(i).PZ = 0.0;
                AirflowNetworkNodeSimu(i).TZlast = AirflowNetworkNodeSimu(i).TZ;
                AirflowNetworkNodeSimu(i).WZlast = AirflowNetworkNodeSimu(i).WZ;
                if (Contaminant.CO2Simulation) {
                    AirflowNetworkNodeSimu(i).CO2Z = OutdoorCO2;
                    AirflowNetworkNodeSimu(i).CO2Zlast = AirflowNetworkNodeSimu(i).CO2Z;
                }
                if (Contaminant.GenericContamSimulation) {
                    AirflowNetworkNodeSimu(i).GCZ = OutdoorGC;
                    AirflowNetworkNodeSimu(i).GCZlast = AirflowNetworkNodeSimu(i).GCZ;
                }
                if (AirflowNetworkNodeData(i).RAFNNodeNum > 0) {
                    ZoneNum = AirflowNetworkNodeData(i).EPlusZoneNum;
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(AirflowNetworkNodeData(i).RAFNNodeNum).AirTemp = 23.0;
                    RoomAirflowNetworkZoneInfo(ZoneNum).Node(AirflowNetworkNodeData(i).RAFNNodeNum).HumRat = 0.0;
                }
            }

            for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
                AirflowNetworkLinkSimu(i).FLOW = 0.0;
                AirflowNetworkLinkSimu(i).FLOW2 = 0.0;
            }

            for (i = 1; i <= NumOfZones; ++i) {
                ANZT(i) = MAT(i);
                ANZW(i) = ZoneAirHumRat(i);
                if (Contaminant.CO2Simulation) ANCO(i) = ZoneAirCO2(i);
                if (Contaminant.GenericContamSimulation) ANGC(i) = ZoneAirGC(i);
            }
            if (AirflowNetworkNumOfOccuVentCtrls > 0) {
                for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
                    if (MultizoneSurfaceData(i).OccupantVentilationControlNum > 0) {
                        MultizoneSurfaceData(i).PrevOpeningstatus = 0;
                        MultizoneSurfaceData(i).CloseElapsedTime = 0.0;
                        MultizoneSurfaceData(i).OpenElapsedTime = 0.0;
                        MultizoneSurfaceData(i).OpeningStatus = 0;
                        MultizoneSurfaceData(i).OpeningProbStatus = 0;
                        MultizoneSurfaceData(i).ClosingProbStatus = 0;
                    }
                }
            }

            MyEnvrnFlag = false;
        }
        if (!BeginEnvrnFlag) {
            MyEnvrnFlag = true;
            if (SimulateAirflowNetwork > AirflowNetworkControlSimple) {
                if (RollBackFlag) {
                    for (i = 1; i <= NumOfZones; ++i) {
                        ANZT(i) = XMAT(i);
                        ANZW(i) = WZoneTimeMinus1(i);
                        if (Contaminant.CO2Simulation) ANCO(i) = CO2ZoneTimeMinus1(i);
                        if (Contaminant.GenericContamSimulation) ANGC(i) = GCZoneTimeMinus1(i);
                    }
                } else {
                    for (i = 1; i <= NumOfZones; ++i) {
                        ANZT(i) = MAT(i);
                        ANZW(i) = ZoneAirHumRat(i);
                        if (Contaminant.CO2Simulation) ANCO(i) = ZoneAirCO2(i);
                        if (Contaminant.GenericContamSimulation) ANGC(i) = ZoneAirGC(i);
                    }
                }

                for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
                    if (AirflowNetworkNodeData(i).EPlusZoneNum > 0) {
                        AirflowNetworkNodeSimu(i).TZ = ANZT(AirflowNetworkNodeData(i).EPlusZoneNum);
                        AirflowNetworkNodeSimu(i).WZ = ANZW(AirflowNetworkNodeData(i).EPlusZoneNum);
                        if (Contaminant.CO2Simulation) AirflowNetworkNodeSimu(i).CO2Z = ANCO(AirflowNetworkNodeData(i).EPlusZoneNum);
                        if (Contaminant.GenericContamSimulation) AirflowNetworkNodeSimu(i).GCZ = ANGC(AirflowNetworkNodeData(i).EPlusZoneNum);
                    }
                    if (AirflowNetworkNodeData(i).ExtNodeNum > 0) {
                        if (AirflowNetworkNodeData(i).OutAirNodeNum > 0 && Node(AirflowNetworkNodeData(i).OutAirNodeNum).IsLocalNode) {
                            AirflowNetworkNodeSimu(i).TZ = Node(AirflowNetworkNodeData(i).OutAirNodeNum).OutAirDryBulb;
                            AirflowNetworkNodeSimu(i).WZ = Node(AirflowNetworkNodeData(i).OutAirNodeNum).HumRat;
                        } else {
                            AirflowNetworkNodeSimu(i).TZ = OutDryBulbTempAt(AirflowNetworkNodeData(i).NodeHeight);
                            AirflowNetworkNodeSimu(i).WZ = OutHumRat;
                        }

                        if (Contaminant.CO2Simulation) AirflowNetworkNodeSimu(i).CO2Z = OutdoorCO2;
                        if (Contaminant.GenericContamSimulation) AirflowNetworkNodeSimu(i).GCZ = OutdoorGC;
                    }

                    if (AirflowNetworkNodeData(i).RAFNNodeNum > 0) {
                        ZoneNum = AirflowNetworkNodeData(i).EPlusZoneNum;
                        if (RoomAirflowNetworkZoneInfo(ZoneNum).Node(AirflowNetworkNodeData(i).RAFNNodeNum).AirflowNetworkNodeID == i) {
                            AirflowNetworkNodeSimu(i).TZ = RoomAirflowNetworkZoneInfo(ZoneNum).Node(AirflowNetworkNodeData(i).RAFNNodeNum).AirTemp;
                            AirflowNetworkNodeSimu(i).WZ = RoomAirflowNetworkZoneInfo(ZoneNum).Node(AirflowNetworkNodeData(i).RAFNNodeNum).HumRat;
                        }
                    }
                }
            }
        }

        for (auto &e : AirflowNetworkExchangeData) {
            e.TotalSen = 0.0;
            e.TotalLat = 0.0;
            e.MultiZoneSen = 0.0;
            e.MultiZoneLat = 0.0;
            e.LeakSen = 0.0;
            e.LeakLat = 0.0;
            e.CondSen = 0.0;
            e.DiffLat = 0.0;
            e.RadGain = 0.0;
        }
        if (Contaminant.CO2Simulation)
            for (auto &e : AirflowNetworkExchangeData)
                e.TotalCO2 = 0.0;
        if (Contaminant.GenericContamSimulation)
            for (auto &e : AirflowNetworkExchangeData)
                e.TotalGC = 0.0;

        // Occupant ventilation control
        CurrentEndTime = CurrentTime + SysTimeElapsed;
        if (CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast) {
            for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
                if (i > AirflowNetworkNumOfSurfaces - NumOfLinksIntraZone) continue;
                if (MultizoneSurfaceData(i).OccupantVentilationControlNum > 0) {
                    MultizoneSurfaceData(i).PrevOpeningstatus = MultizoneSurfaceData(i).OpeningStatus;
                    MultizoneSurfaceData(i).OpenFactorLast = MultizoneSurfaceData(i).OpenFactor;
                    if (MultizoneSurfaceData(i).OpenFactor > 0.0) {
                        MultizoneSurfaceData(i).OpenElapsedTime += (CurrentEndTime - CurrentEndTimeLast) * 60.0;
                        MultizoneSurfaceData(i).CloseElapsedTime = 0.0;
                    } else {
                        MultizoneSurfaceData(i).OpenElapsedTime = 0.0;
                        MultizoneSurfaceData(i).CloseElapsedTime += (CurrentEndTime - CurrentEndTimeLast) * 60.0;
                    }
                    j = MultizoneSurfaceData(i).SurfNum;
                    OccupantVentilationControl(MultizoneSurfaceData(i).OccupantVentilationControlNum)
                        .calc(Surface(j).Zone,
                              j,
                              MultizoneSurfaceData(i).PrevOpeningstatus,
                              MultizoneSurfaceData(i).OpenElapsedTime,
                              MultizoneSurfaceData(i).CloseElapsedTime,
                              MultizoneSurfaceData(i).OpeningStatus,
                              MultizoneSurfaceData(i).OpeningProbStatus,
                              MultizoneSurfaceData(i).ClosingProbStatus);
                    if (MultizoneSurfaceData(i).OpeningStatus == MinCheckForceOpen) {
                        MultizoneSurfaceData(i).OpenFactor = MultizoneSurfaceData(i).OpenFactorLast;
                    }
                    if (MultizoneSurfaceData(i).OpeningStatus == MinCheckForceClose) {
                        MultizoneSurfaceData(i).OpenFactor = 0.0;
                    }
                }
            }
        }
        TimeStepSysLast = TimeStepSys;
        CurrentEndTimeLast = CurrentEndTime;
    }

    void AllocateAndInitData()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Aug. 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes variables and allocates dynamic arrays.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataGlobals::AnyEnergyManagementSystemInModel;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int ZoneNum;
        int n;
        int SurfNum;

        AirflowNetworkNodeSimu.allocate(AirflowNetworkNumOfNodes);   // Node simulation variable in air distribution system
        AirflowNetworkLinkSimu.allocate(AirflowNetworkNumOfLinks);   // Link simulation variable in air distribution system
        AirflowNetworkLinkReport.allocate(AirflowNetworkNumOfLinks); // Report link simulation variable in air distribution system

        for (i = 1; i <= DisSysNumOfCVFs; i++) {
            if (DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff) {
                AirflowNetworkNodeReport.allocate(AirflowNetworkNumOfZones);
                AirflowNetworkLinkReport1.allocate(AirflowNetworkNumOfSurfaces);
                break;
            }
        }

        MA.allocate(AirflowNetworkNumOfNodes * AirflowNetworkNumOfNodes);
        MV.allocate(AirflowNetworkNumOfNodes);
        IVEC.allocate(AirflowNetworkNumOfNodes + 20);

        AirflowNetworkReportData.allocate(NumOfZones); // Report variables
        AirflowNetworkZnRpt.allocate(NumOfZones);      // Report variables

        ANZT.allocate(NumOfZones);                                          // Local zone air temperature for rollback use
        ANZW.allocate(NumOfZones);                                          // Local zone humidity ratio for rollback use
        if (Contaminant.CO2Simulation) ANCO.allocate(NumOfZones);           // Local zone CO2 for rollback use
        if (Contaminant.GenericContamSimulation) ANGC.allocate(NumOfZones); // Local zone generic contaminant for rollback use

        AllocateAirflowNetworkData();

        bool OnOffFanFlag = false;
        for (i = 1; i <= DisSysNumOfCVFs; i++) {
            if (DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff) {
                OnOffFanFlag = true;
            }
        }

        // CurrentModuleObject='AirflowNetwork Simulations'
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            SetupOutputVariable(
                "AFN Node Temperature", OutputProcessor::Unit::C, AirflowNetworkNodeSimu(i).TZ, "System", "Average", AirflowNetworkNodeData(i).Name);
            SetupOutputVariable("AFN Node Humidity Ratio",
                                OutputProcessor::Unit::kgWater_kgDryAir,
                                AirflowNetworkNodeSimu(i).WZ,
                                "System",
                                "Average",
                                AirflowNetworkNodeData(i).Name);
            if (Contaminant.CO2Simulation) {
                SetupOutputVariable("AFN Node CO2 Concentration",
                                    OutputProcessor::Unit::ppm,
                                    AirflowNetworkNodeSimu(i).CO2Z,
                                    "System",
                                    "Average",
                                    AirflowNetworkNodeData(i).Name);
            }
            if (Contaminant.GenericContamSimulation) {
                SetupOutputVariable("AFN Node Generic Air Contaminant Concentration",
                                    OutputProcessor::Unit::ppm,
                                    AirflowNetworkNodeSimu(i).GCZ,
                                    "System",
                                    "Average",
                                    AirflowNetworkNodeData(i).Name);
            }
            if (!(SupplyFanType == FanType_SimpleOnOff && i <= AirflowNetworkNumOfZones)) {
                SetupOutputVariable("AFN Node Total Pressure",
                                    OutputProcessor::Unit::Pa,
                                    AirflowNetworkNodeSimu(i).PZ,
                                    "System",
                                    "Average",
                                    AirflowNetworkNodeData(i).Name);
            }
            if (AirflowNetworkNodeData(i).ExtNodeNum > 0) {
                SetupOutputVariable("AFN Node Wind Pressure",
                                    OutputProcessor::Unit::Pa,
                                    AirflowNetworkNodeSimu(i).PZ,
                                    "System",
                                    "Average",
                                    AirflowNetworkNodeData(i).Name);
            }
        }

        for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            if (!(SupplyFanType == FanType_SimpleOnOff && i <= AirflowNetworkNumOfSurfaces)) {
                SetupOutputVariable("AFN Linkage Node 1 to Node 2 Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    AirflowNetworkLinkReport(i).FLOW,
                                    "System",
                                    "Average",
                                    AirflowNetworkLinkageData(i).Name);
                SetupOutputVariable("AFN Linkage Node 2 to Node 1 Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    AirflowNetworkLinkReport(i).FLOW2,
                                    "System",
                                    "Average",
                                    AirflowNetworkLinkageData(i).Name);
                SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    AirflowNetworkLinkReport(i).VolFLOW,
                                    "System",
                                    "Average",
                                    AirflowNetworkLinkageData(i).Name);
                SetupOutputVariable("AFN Linkage Node 2 to Node 1 Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    AirflowNetworkLinkReport(i).VolFLOW2,
                                    "System",
                                    "Average",
                                    AirflowNetworkLinkageData(i).Name);
                SetupOutputVariable("AFN Linkage Node 1 to Node 2 Pressure Difference",
                                    OutputProcessor::Unit::Pa,
                                    AirflowNetworkLinkSimu(i).DP,
                                    "System",
                                    "Average",
                                    AirflowNetworkLinkageData(i).Name);
            }
        }

        for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
            n = AirflowNetworkLinkageData(i).CompNum;
            if (AirflowNetworkCompData(n).CompTypeNum == CompTypeNum_DOP || AirflowNetworkCompData(n).CompTypeNum == CompTypeNum_SOP ||
                AirflowNetworkCompData(n).CompTypeNum == CompTypeNum_HOP) {
                SurfNum = MultizoneSurfaceData(i).SurfNum;
                SetupOutputVariable("AFN Surface Venting Window or Door Opening Factor",
                                    OutputProcessor::Unit::None,
                                    MultizoneSurfaceData(i).OpenFactor,
                                    "System",
                                    "Average",
                                    MultizoneSurfaceData(i).SurfName);
                if (AnyEnergyManagementSystemInModel) {
                    SetupEMSActuator("AirFlow Network Window/Door Opening",
                                     MultizoneSurfaceData(i).SurfName,
                                     "Venting Opening Factor",
                                     "[Fraction]",
                                     MultizoneSurfaceData(i).EMSOpenFactorActuated,
                                     MultizoneSurfaceData(i).EMSOpenFactor);
                }
                SetupOutputVariable("AFN Surface Venting Window or Door Opening Modulation Multiplier",
                                    OutputProcessor::Unit::None,
                                    SurfaceWindow(SurfNum).VentingOpenFactorMultRep,
                                    "System",
                                    "Average",
                                    Surface(SurfNum).Name);
                SetupOutputVariable("AFN Surface Venting Inside Setpoint Temperature",
                                    OutputProcessor::Unit::C,
                                    SurfaceWindow(SurfNum).InsideTempForVentingRep,
                                    "System",
                                    "Average",
                                    Surface(SurfNum).Name);
                SetupOutputVariable("AFN Surface Venting Availability Status",
                                    OutputProcessor::Unit::None,
                                    SurfaceWindow(SurfNum).VentingAvailabilityRep,
                                    "System",
                                    "Average",
                                    Surface(SurfNum).Name);
                if (MultizoneSurfaceData(i).OccupantVentilationControlNum > 0) {
                    SetupOutputVariable("AFN Surface Venting Window or Door Opening Factor at Previous Time Step",
                                        OutputProcessor::Unit::None,
                                        MultizoneSurfaceData(i).OpenFactorLast,
                                        "System",
                                        "Average",
                                        MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable("AFN Surface Opening Elapsed Time",
                                        OutputProcessor::Unit::min,
                                        MultizoneSurfaceData(i).OpenElapsedTime,
                                        "System",
                                        "Average",
                                        MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable("AFN Surface Closing Elapsed Time",
                                        OutputProcessor::Unit::min,
                                        MultizoneSurfaceData(i).CloseElapsedTime,
                                        "System",
                                        "Average",
                                        MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable("AFN Surface Opening Status at Previous Time Step",
                                        OutputProcessor::Unit::None,
                                        MultizoneSurfaceData(i).PrevOpeningstatus,
                                        "System",
                                        "Average",
                                        MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable("AFN Surface Opening Status",
                                        OutputProcessor::Unit::None,
                                        MultizoneSurfaceData(i).OpeningStatus,
                                        "System",
                                        "Average",
                                        MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable("AFN Surface Opening Probability Status",
                                        OutputProcessor::Unit::None,
                                        MultizoneSurfaceData(i).OpeningProbStatus,
                                        "System",
                                        "Average",
                                        MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable("AFN Surface Closing Probability Status",
                                        OutputProcessor::Unit::None,
                                        MultizoneSurfaceData(i).ClosingProbStatus,
                                        "System",
                                        "Average",
                                        MultizoneSurfaceData(i).SurfName);
                }
            }
        }

        for (i = 1; i <= NumOfZones; ++i) {
            // Multizone losses due to force air systems
            SetupOutputVariable("AFN Zone Infiltration Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).MultiZoneInfiSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Infiltration Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).MultiZoneInfiSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Mixing Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).MultiZoneMixSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Mixing Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).MultiZoneMixSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Infiltration Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).MultiZoneInfiSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Infiltration Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).MultiZoneInfiSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Mixing Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).MultiZoneMixSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Mixing Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).MultiZoneMixSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Infiltration Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).MultiZoneInfiLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Infiltration Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).MultiZoneInfiLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Infiltration Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).MultiZoneInfiLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Infiltration Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).MultiZoneInfiLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Mixing Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).MultiZoneMixLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Mixing Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).MultiZoneMixLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Mixing Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).MultiZoneMixLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Mixing Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).MultiZoneInfiLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            // Supply leak losses due to force air systems
            SetupOutputVariable("AFN Zone Duct Leaked Air Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).LeakSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Leaked Air Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).LeakSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Leaked Air Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).LeakSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Leaked Air Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).LeakSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Leaked Air Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).LeakLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Leaked Air Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).LeakLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Leaked Air Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).LeakLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Leaked Air Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).LeakLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            // Conduction losses due to force air systems
            SetupOutputVariable("AFN Zone Duct Conduction Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).CondSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Conduction Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).CondSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Conduction Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).CondSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Conduction Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).CondSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Diffusion Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).DiffLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Diffusion Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).DiffLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Diffusion Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).DiffLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Diffusion Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).DiffLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            // Radiation losses due to forced air systems
            SetupOutputVariable("AFN Zone Duct Radiation Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).RadGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Radiation Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).RadGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Radiation Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).RadLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Duct Radiation Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).RadLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            // Total losses due to force air systems
            SetupOutputVariable("AFN Distribution Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).TotalSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Distribution Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).TotalSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Distribution Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).TotalSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Distribution Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).TotalSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Distribution Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).TotalLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Distribution Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).TotalLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable("AFN Distribution Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkReportData(i).TotalLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Distribution Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                AirflowNetworkReportData(i).TotalLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
        }

        for (i = 1; i <= NumOfZones; ++i) {
            SetupOutputVariable(
                "AFN Zone Infiltration Volume", OutputProcessor::Unit::m3, AirflowNetworkZnRpt(i).InfilVolume, "System", "Sum", Zone(i).Name);
            SetupOutputVariable(
                "AFN Zone Infiltration Mass", OutputProcessor::Unit::kg, AirflowNetworkZnRpt(i).InfilMass, "System", "Sum", Zone(i).Name);
            SetupOutputVariable("AFN Zone Infiltration Air Change Rate",
                                OutputProcessor::Unit::ach,
                                AirflowNetworkZnRpt(i).InfilAirChangeRate,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Mixing Volume", OutputProcessor::Unit::m3, AirflowNetworkZnRpt(i).MixVolume, "System", "Sum", Zone(i).Name);
            SetupOutputVariable("AFN Zone Mixing Mass", OutputProcessor::Unit::kg, AirflowNetworkZnRpt(i).MixMass, "System", "Sum", Zone(i).Name);

            SetupOutputVariable("AFN Zone Exfiltration Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkZnRpt(i).ExfilTotalLoss,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Exfiltration Sensible Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkZnRpt(i).ExfilSensiLoss,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable("AFN Zone Exfiltration Latent Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                AirflowNetworkZnRpt(i).ExfilLatentLoss,
                                "System",
                                "Average",
                                Zone(i).Name);
        }

        if (OnOffFanFlag) {
            for (i = 1; i <= AirflowNetworkNumOfZones; ++i) {
                SetupOutputVariable(
                    "AFN Zone Average Pressure", OutputProcessor::Unit::Pa, AirflowNetworkNodeReport(i).PZ, "System", "Average", Zone(i).Name);
                SetupOutputVariable(
                    "AFN Zone On Cycle Pressure", OutputProcessor::Unit::Pa, AirflowNetworkNodeReport(i).PZON, "System", "Average", Zone(i).Name);
                SetupOutputVariable(
                    "AFN Zone Off Cycle Pressure", OutputProcessor::Unit::Pa, AirflowNetworkNodeReport(i).PZOFF, "System", "Average", Zone(i).Name);
            }
            for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
                SetupOutputVariable("AFN Linkage Node 1 to 2 Average Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    AirflowNetworkLinkReport1(i).FLOW,
                                    "System",
                                    "Average",
                                    MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable("AFN Linkage Node 2 to 1 Average Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    AirflowNetworkLinkReport1(i).FLOW2,
                                    "System",
                                    "Average",
                                    MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable("AFN Linkage Node 1 to 2 Average Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    AirflowNetworkLinkReport1(i).VolFLOW,
                                    "System",
                                    "Average",
                                    MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable("AFN Linkage Node 2 to 1 Average Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    AirflowNetworkLinkReport1(i).VolFLOW2,
                                    "System",
                                    "Average",
                                    MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable("AFN Surface Average Pressure Difference",
                                    OutputProcessor::Unit::Pa,
                                    AirflowNetworkLinkReport1(i).DP,
                                    "System",
                                    "Average",
                                    MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable("AFN Surface On Cycle Pressure Difference",
                                    OutputProcessor::Unit::Pa,
                                    AirflowNetworkLinkReport1(i).DPON,
                                    "System",
                                    "Average",
                                    MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable("AFN Surface Off Cycle Pressure Difference",
                                    OutputProcessor::Unit::Pa,
                                    AirflowNetworkLinkReport1(i).DPOFF,
                                    "System",
                                    "Average",
                                    MultizoneSurfaceData(i).SurfName);
            }
        }

        // Assign node reference height
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            if (!AirflowNetworkSimu.TExtHeightDep) AirflowNetworkNodeData(i).NodeHeight = 0.0;
            ZoneNum = AirflowNetworkNodeData(i).EPlusZoneNum;
            if (ZoneNum > 0) {
                if (WorldCoordSystem) {
                    AirflowNetworkNodeData(i).NodeHeight = 0.0;
                } else {
                    AirflowNetworkNodeData(i).NodeHeight = Zone(ZoneNum).OriginZ;
                }
            }
        }
    }

    void CalcAirflowNetworkAirBalance()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs simulations of nodal pressures and linkage airflows.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataAirLoop::AirLoopAFNInfo;
        using DataGlobals::AnyLocalEnvironmentsInModel;
        using DataHVACGlobals::TurnFansOn;
        using DataHVACGlobals::VerySmallMassFlow;
        using General::SolveRoot;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const CycFanCycComp(1); // fan cycles with compressor operation

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int n;
        int NodeNum;
        static bool OneTimeFlag(true);
        static bool ErrorsFound(false);
        Real64 GlobalOpenFactor;
        Real64 ZonePressure1;
        Real64 ZonePressure2;
        Real64 PressureSet;
        Real64 LocalAzimuth;
        Real64 LocalWindSpeed;
        Real64 LocalWindDir;
        Real64 LocalHumRat;
        Real64 LocalDryBulb;
        Array1D<Real64> Par; // Pressure setpoint
        Real64 const ErrorToler(0.00001);
        int const MaxIte(20);
        int SolFla;
        static int ErrCountVar(0);
        static int ErrCountHighPre(0);
        static int ErrCountLowPre(0);
        static int ErrIndexHighPre(0);
        static int ErrIndexVar(0);
        static int ErrIndexLowPre(0);
        Real64 MinExhaustMassFlowrate;
        Real64 MaxExhaustMassFlowrate;
        Real64 MinReliefMassFlowrate;
        Real64 MaxReliefMassFlowrate;
        int AirLoopNum;

        // Validate supply and return connections
        if (OneTimeFlag) {
            OneTimeFlag = false;
            if (ErrorsFound) {
                ShowFatalError("GetAirflowNetworkInput: Program terminates for preceding reason(s).");
            }
        }

        for (n = 1; n <= NetworkNumOfNodes; ++n) {
            if (AirflowNetworkNodeData(n).NodeTypeNum == 0) {
                AirflowNetworkNodeSimu(n).PZ = 0.0;
            } else {
                // Assigning ambient conditions to external nodes
                i = AirflowNetworkNodeData(n).ExtNodeNum;
                if (i > 0) {
                    AirflowNetworkNodeSimu(n).TZ = OutDryBulbTempAt(AirflowNetworkNodeData(n).NodeHeight);
                    AirflowNetworkNodeSimu(n).WZ = OutHumRat;
                    if (i <= AirflowNetworkNumOfExtNode) {
                        if (MultizoneExternalNodeData(i).OutAirNodeNum == 0) {
                            LocalWindSpeed = WindSpeedAt(MultizoneExternalNodeData(i).height);
                            LocalDryBulb = OutDryBulbTempAt(AirflowNetworkNodeData(n).NodeHeight);
                            LocalAzimuth = MultizoneExternalNodeData(i).azimuth;
                            AirflowNetworkNodeSimu(n).PZ = CalcWindPressure(MultizoneExternalNodeData(i).curve,
                                                                            MultizoneExternalNodeData(i).symmetricCurve,
                                                                            MultizoneExternalNodeData(i).useRelativeAngle,
                                                                            LocalAzimuth,
                                                                            LocalWindSpeed,
                                                                            WindDir,
                                                                            LocalDryBulb,
                                                                            OutHumRat);
                        } else {
                            // If and outdoor air node object is defined as the External Node Name in AirflowNetwork:MultiZone:Surface,
                            // the node object requires to define the Wind Pressure Coefficient Curve Name.
                            NodeNum = MultizoneExternalNodeData(i).OutAirNodeNum;
                            LocalWindSpeed = Node((NodeNum)).OutAirWindSpeed;
                            LocalWindDir = Node((NodeNum)).OutAirWindDir;
                            LocalHumRat = Node((NodeNum)).HumRat;
                            LocalDryBulb = Node((NodeNum)).OutAirDryBulb;
                            LocalAzimuth = MultizoneExternalNodeData(i).azimuth;
                            AirflowNetworkNodeSimu(n).PZ = CalcWindPressure(MultizoneExternalNodeData(i).curve,
                                                                            MultizoneExternalNodeData(i).symmetricCurve,
                                                                            MultizoneExternalNodeData(i).useRelativeAngle,
                                                                            LocalAzimuth,
                                                                            LocalWindSpeed,
                                                                            LocalWindDir,
                                                                            LocalDryBulb,
                                                                            LocalHumRat);
                            AirflowNetworkNodeSimu(n).TZ = LocalDryBulb;
                            AirflowNetworkNodeSimu(n).WZ = LocalHumRat;
                        }
                    }

                } else {
                    ShowSevereError("GetAirflowNetworkInput: AIRFLOWNETWORK:DISTRIBUTION:NODE: Invalid external node = " +
                                    AirflowNetworkNodeData(n).Name);
                    ErrorsFound = true;
                }
            }
        }

        for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
            if (i > AirflowNetworkNumOfSurfaces - NumOfLinksIntraZone) continue;
            if (MultizoneSurfaceData(i).OccupantVentilationControlNum == 0) MultizoneSurfaceData(i).OpenFactor = 0.0;
            j = MultizoneSurfaceData(i).SurfNum;
            if (SurfaceWindow(j).OriginalClass == SurfaceClass_Window || SurfaceWindow(j).OriginalClass == SurfaceClass_Door ||
                SurfaceWindow(j).OriginalClass == SurfaceClass_GlassDoor) {
                if (MultizoneSurfaceData(i).OccupantVentilationControlNum > 0) {
                    if (MultizoneSurfaceData(i).OpeningStatus == FreeOperation) {
                        if (MultizoneSurfaceData(i).OpeningProbStatus == ProbForceChange) {
                            MultizoneSurfaceData(i).OpenFactor = MultizoneSurfaceData(i).Factor;
                        } else if (MultizoneSurfaceData(i).ClosingProbStatus == ProbForceChange) {
                            MultizoneSurfaceData(i).OpenFactor = 0.0;
                        } else if (MultizoneSurfaceData(i).ClosingProbStatus == ProbKeepStatus ||
                                   MultizoneSurfaceData(i).OpeningProbStatus == ProbKeepStatus) {
                            MultizoneSurfaceData(i).OpenFactor = MultizoneSurfaceData(i).OpenFactorLast;
                        } else {
                            AirflowNetworkVentingControl(i, MultizoneSurfaceData(i).OpenFactor);
                        }
                    }
                } else {
                    AirflowNetworkVentingControl(i, MultizoneSurfaceData(i).OpenFactor);
                }
                MultizoneSurfaceData(i).OpenFactor *= MultizoneSurfaceData(i).WindModifier;
                if (MultizoneSurfaceData(i).HybridVentClose) {
                    MultizoneSurfaceData(i).OpenFactor = 0.0;
                    if (SurfaceWindow(j).VentingOpenFactorMultRep > 0.0) SurfaceWindow(j).VentingOpenFactorMultRep = 0.0;
                }
                if (AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_DOP ||
                    AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_SOP ||
                    AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_HOP) {
                    if (AirflowNetworkFanActivated && (SimulateAirflowNetwork > AirflowNetworkControlMultizone) &&
                        MultizoneSurfaceData(i).OpenFactor > 0.0 &&
                        (Surface(j).ExtBoundCond == ExternalEnvironment ||
                         (Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt &&
                          Surface(MultizoneSurfaceData(i).SurfNum).ExtWind)) &&
                        !WarmupFlag) {
                        // Exterior Large opening only
                        ++MultizoneSurfaceData(i).ExtLargeOpeningErrCount;
                        if (MultizoneSurfaceData(i).ExtLargeOpeningErrCount < 2) {
                            ShowWarningError("AirflowNetwork: The window or door is open during HVAC system operation " +
                                             MultizoneSurfaceData(i).SurfName);
                            ShowContinueError("The window or door opening factor is " + RoundSigDigits(MultizoneSurfaceData(i).OpenFactor, 2));
                            ShowContinueErrorTimeStamp("");
                        } else {
                            ShowRecurringWarningErrorAtEnd("AirFlowNetwork: " + MultizoneSurfaceData(i).SurfName +
                                                               " The window or door is open during HVAC system operation error continues...",
                                                           MultizoneSurfaceData(i).ExtLargeOpeningErrIndex,
                                                           MultizoneSurfaceData(i).OpenFactor,
                                                           MultizoneSurfaceData(i).OpenFactor);
                        }
                    }
                }
                if (MultizoneSurfaceData(i).OpenFactor > 1.0) {
                    ++MultizoneSurfaceData(i).OpenFactorErrCount;
                    if (MultizoneSurfaceData(i).OpenFactorErrCount < 2) {
                        ShowWarningError("AirflowNetwork: The window or door opening factor is greater than 1.0 " + MultizoneSurfaceData(i).SurfName);
                        ShowContinueErrorTimeStamp("");
                    } else {
                        ShowRecurringWarningErrorAtEnd("AirFlowNetwork: " + MultizoneSurfaceData(i).SurfName +
                                                           " The window or door opening factor is greater than 1.0 error continues...",
                                                       MultizoneSurfaceData(i).OpenFactorErrIndex,
                                                       MultizoneSurfaceData(i).OpenFactor,
                                                       MultizoneSurfaceData(i).OpenFactor);
                    }
                }
            }
        }

        // Check if the global ventilation control is applied or not
        GlobalOpenFactor = -1.0;
        for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
            if (i > AirflowNetworkNumOfSurfaces - NumOfLinksIntraZone) continue;
            if (MultizoneSurfaceData(i).HybridCtrlMaster) {
                GlobalOpenFactor = MultizoneSurfaceData(i).OpenFactor;
                break;
            }
        }
        if (GlobalOpenFactor >= 0.0) {
            for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
                if (i > AirflowNetworkNumOfSurfaces - NumOfLinksIntraZone) continue;
                j = MultizoneSurfaceData(i).SurfNum;
                if (SurfaceWindow(j).OriginalClass == SurfaceClass_Window || SurfaceWindow(j).OriginalClass == SurfaceClass_Door ||
                    SurfaceWindow(j).OriginalClass == SurfaceClass_GlassDoor) {
                    if (MultizoneSurfaceData(i).HybridCtrlGlobal) {
                        MultizoneSurfaceData(i).OpenFactor = GlobalOpenFactor;
                    }
                }
            }
        }

        if (!Par.allocated()) {
            Par.allocate(1);
            Par = 0.0;
        }

        PressureSetFlag = 0;

        if (NumOfPressureControllers == 1) {
            if (PressureControllerData(1).AvailSchedPtr == ScheduleAlwaysOn) {
                PressureSetFlag = PressureControllerData(1).ControlTypeSet;
            } else {
                if (GetCurrentScheduleValue(PressureControllerData(1).AvailSchedPtr) > 0.0) {
                    PressureSetFlag = PressureControllerData(1).ControlTypeSet;
                }
            }
            if (PressureSetFlag > 0) {
                PressureSet = GetCurrentScheduleValue(PressureControllerData(1).PresSetpointSchedPtr);
            }
        }

        InitAirflowNetworkData();

        if (!(PressureSetFlag > 0 && AirflowNetworkFanActivated)) {
            AIRMOV();
        } else if (PressureSetFlag == PressureCtrlExhaust) {
            AirLoopNum = AirflowNetworkNodeData(PressureControllerData(1).AFNNodeNum).AirLoopNum;
            MinExhaustMassFlowrate = 2.0 * VerySmallMassFlow;
            MaxExhaustMassFlowrate = Node(PressureControllerData(1).OANodeNum).MassFlowRate;
            if (AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycComp && AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio > 0.0) {
                MaxExhaustMassFlowrate = MaxExhaustMassFlowrate / AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
            }
            ExhaustFanMassFlowRate = MinExhaustMassFlowrate;
            AIRMOV();
            ZonePressure1 = AirflowNetworkNodeSimu(PressureControllerData(1).AFNNodeNum).PZ;
            if (ZonePressure1 <= PressureSet) {
                // The highest pressure due to minimum flow rate could not reach Pressure set, bypass pressure set calculation
                if (!WarmupFlag) {
                    if (ErrCountLowPre == 0) {
                        ++ErrCountLowPre;
                        ShowWarningError("The calculated pressure with minimum exhaust fan rate is lower than the pressure setpoint. The pressure "
                                         "control is unable to perform.");
                        ShowContinueErrorTimeStamp("Calculated pressure = " + RoundSigDigits(ZonePressure1, 2) +
                                                   "[Pa], Pressure setpoint =" + RoundSigDigits(PressureSet, 2));
                    } else {
                        ++ErrCountLowPre;
                        ShowRecurringWarningErrorAtEnd(AirflowNetworkNodeData(PressureControllerData(1).AFNNodeNum).Name +
                                                           ": The AFN model continues not to perform pressure control due to lower zone pressure...",
                                                       ErrIndexLowPre,
                                                       ZonePressure1,
                                                       ZonePressure1);
                    }
                }
            } else {
                ExhaustFanMassFlowRate = MaxExhaustMassFlowrate;
                AIRMOV();
                ZonePressure2 = AirflowNetworkNodeSimu(PressureControllerData(1).AFNNodeNum).PZ;
                if (ZonePressure2 >= PressureSet) {
                    // The lowest pressure due to maximum flow rate is still higher than Pressure set, bypass pressure set calculation
                    if (!WarmupFlag) {
                        if (ErrCountHighPre == 0) {
                            ++ErrCountHighPre;
                            ShowWarningError("The calculated pressure with maximum exhaust fan rate is higher than the pressure setpoint. The "
                                             "pressure control is unable to perform.");
                            ShowContinueErrorTimeStamp("Calculated pressure = " + RoundSigDigits(ZonePressure2, 2) +
                                                       "[Pa], Pressure setpoint = " + RoundSigDigits(PressureSet, 2));
                        } else {
                            ++ErrCountHighPre;
                            ShowRecurringWarningErrorAtEnd(
                                AirflowNetworkNodeData(PressureControllerData(1).AFNNodeNum).Name +
                                    ": The AFN model continues not to perform pressure control due to higher zone pressure...",
                                ErrIndexHighPre,
                                ZonePressure2,
                                ZonePressure2);
                        }
                    }
                } else {
                    //	if ( ZonePressure1 > PressureSet && ZonePressure2 < PressureSet ) {
                    Par(1) = PressureSet;
                    SolveRoot(
                        ErrorToler, MaxIte, SolFla, ExhaustFanMassFlowRate, AFNPressureResidual, MinExhaustMassFlowrate, MaxExhaustMassFlowrate, Par);
                    if (SolFla == -1) {
                        if (!WarmupFlag) {
                            if (ErrCountVar == 0) {
                                ++ErrCountVar;
                                ShowWarningError("Iteration limit exceeded pressure setpoint using an exhaust fan. Simulation continues.");
                                ShowContinueErrorTimeStamp("Exhaust fan flow rate = " + RoundSigDigits(ExhaustFanMassFlowRate, 4));
                            } else {
                                ++ErrCountVar;
                                ShowRecurringWarningErrorAtEnd(PressureControllerData(1).Name +
                                                                   "\": Iteration limit warning exceeding pressure setpoint continues...",
                                                               ErrIndexVar,
                                                               ExhaustFanMassFlowRate,
                                                               ExhaustFanMassFlowRate);
                            }
                        }
                    } else if (SolFla == -2) {
                        ShowFatalError("Zone pressure control failed using an exhaust fan: no solution is reached, for " +
                                       PressureControllerData(1).Name);
                    }
                }
            }
        } else { // PressureCtrlRelief - Pressure control type is Relief Flow
            AirLoopNum = AirflowNetworkNodeData(PressureControllerData(1).AFNNodeNum).AirLoopNum;
            MinReliefMassFlowrate = 2.0 * VerySmallMassFlow;
            MaxReliefMassFlowrate = Node(PressureControllerData(1).OANodeNum).MassFlowRate;
            if (AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycComp && AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio > 0.0) {
                MaxReliefMassFlowrate = MaxReliefMassFlowrate / AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
            }
            ReliefMassFlowRate = MinReliefMassFlowrate;
            InitAirflowNetworkData();
            AIRMOV();
            ZonePressure1 = AirflowNetworkNodeSimu(PressureControllerData(1).AFNNodeNum).PZ;

            if (ZonePressure1 <= PressureSet) {
                // The highest pressure due to minimum flow rate could not reach Pressure set, bypass pressure set calculation
                if (!WarmupFlag) {
                    if (ErrCountLowPre == 0) {
                        ++ErrCountLowPre;
                        ShowWarningError("The calculated pressure with minimum relief air rate is lower than the pressure setpoint. The pressure "
                                         "control is unable to perform.");
                        ShowContinueErrorTimeStamp("Calculated pressure = " + RoundSigDigits(ZonePressure1, 2) +
                                                   "[Pa], Pressure setpoint =" + RoundSigDigits(PressureSet, 2));
                    } else {
                        ++ErrCountLowPre;
                        ShowRecurringWarningErrorAtEnd(AirflowNetworkNodeData(PressureControllerData(1).AFNNodeNum).Name +
                                                           ": The AFN model continues not to perform pressure control due to lower zone pressure...",
                                                       ErrIndexLowPre,
                                                       ZonePressure1,
                                                       ZonePressure1);
                    }
                }
            } else {
                ReliefMassFlowRate = MaxReliefMassFlowrate;
                InitAirflowNetworkData();
                AIRMOV();
                ZonePressure2 = AirflowNetworkNodeSimu(PressureControllerData(1).AFNNodeNum).PZ;
                if (ZonePressure2 >= PressureSet) {
                    // The lowest pressure due to maximum flow rate is still higher than Pressure set, bypass pressure set calculation
                    if (!WarmupFlag) {
                        if (ErrCountHighPre == 0) {
                            ++ErrCountHighPre;
                            ShowWarningError("The calculated pressure with maximum relief air rate is higher than the pressure setpoint. The "
                                             "pressure control is unable to perform.");
                            ShowContinueErrorTimeStamp("Calculated pressure = " + RoundSigDigits(ZonePressure2, 2) +
                                                       "[Pa], Pressure setpoint = " + RoundSigDigits(PressureSet, 2));
                        } else {
                            ++ErrCountHighPre;
                            ShowRecurringWarningErrorAtEnd(
                                AirflowNetworkNodeData(PressureControllerData(1).AFNNodeNum).Name +
                                    ": The AFN model continues not to perform pressure control due to higher zone pressure...",
                                ErrIndexHighPre,
                                ZonePressure2,
                                ZonePressure2);
                        }
                    }
                } else {
                    //	if ( ZonePressure1 > PressureSet && ZonePressure2 < PressureSet ) {
                    Par(1) = PressureSet;
                    SolveRoot(ErrorToler, MaxIte, SolFla, ReliefMassFlowRate, AFNPressureResidual, MinReliefMassFlowrate, MaxReliefMassFlowrate, Par);
                    if (SolFla == -1) {
                        if (!WarmupFlag) {
                            if (ErrCountVar == 0) {
                                ++ErrCountVar;
                                ShowWarningError("Iteration limit exceeded pressure setpoint using relief air. Simulation continues.");
                                ShowContinueErrorTimeStamp("Relief air flow rate = " + RoundSigDigits(ReliefMassFlowRate, 4));
                            } else {
                                ++ErrCountVar;
                                ShowRecurringWarningErrorAtEnd(PressureControllerData(1).Name +
                                                                   "\": Iteration limit warning exceeding pressure setpoint continues...",
                                                               ErrIndexVar,
                                                               ReliefMassFlowRate,
                                                               ReliefMassFlowRate);
                            }
                        }
                    } else if (SolFla == -2) {
                        ShowFatalError("Zone pressure control failed using relief air: no solution is reached, for " +
                                       PressureControllerData(1).Name);
                    }
                }
            }
        }
    }

    Real64 AFNPressureResidual(Real64 const ControllerMassFlowRate, // Pressure setpoint
                               Array1<Real64> const &Par            // par(1) = PressureSet
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   April 2016
        //       MODIFIED       NA
        //       RE-ENGINEERED  NA

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function ((ZonePressure - PressureSet)/PressureSet)

        // METHODOLOGY EMPLOYED:
        //  Calls AIRMOV to get the pressure in the controlled zone and calculates the residual as defined above

        // REFERENCES:

        // USE STATEMENTS:
        // na

        // Return value
        Real64 AFNPressureResidual;

        // Argument array dimensioning

        // Locals

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        //  na

        // INTERFACE BLOCK SPECIFICATIONS
        //  na

        // DERIVED TYPE DEFINITIONS
        //  na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 PressureSet;
        Real64 ZonePressure;

        PressureSet = Par(1);

        if (PressureSetFlag == PressureCtrlExhaust) {
            ExhaustFanMassFlowRate = ControllerMassFlowRate;
        }

        if (PressureSetFlag == PressureCtrlRelief) {
            ReliefMassFlowRate = ControllerMassFlowRate;
        }

        InitAirflowNetworkData();
        AIRMOV();

        ZonePressure = AirflowNetworkNodeSimu(PressureControllerData(1).AFNNodeNum).PZ;

        if (PressureSet != 0.0) {
            AFNPressureResidual = (ZonePressure - PressureSet) / PressureSet;
        } else {
            AFNPressureResidual = (ZonePressure - PressureSet);
        }
        return AFNPressureResidual;
    }

    static int makeTable(const std::string &name, const int gridIndex, const std::vector<Real64> &y)
    {
        // Add a new table and performance curve
        std::string contextString = "CalcWindPressureCoeffs: Creating table \"" + name + "\"";
        Btwxt::setMessageCallback(CurveManager::BtwxtMessageCallback, &contextString);

        int CurveNum = CurveManager::PerfCurve.size() + 1;
        CurveManager::PerfCurve.push_back(CurveManager::PerfomanceCurveData());

        CurveManager::PerfCurve(CurveNum).Name = name;
        CurveManager::PerfCurve(CurveNum).ObjectType = "Table:Lookup";
        CurveManager::PerfCurve(CurveNum).NumDims = 1;

        CurveManager::PerfCurve(CurveNum).InterpolationType = CurveManager::BtwxtMethod;

        CurveManager::PerfCurve(CurveNum).Var1Min = 0.0;
        CurveManager::PerfCurve(CurveNum).Var1MinPresent = true;
        CurveManager::PerfCurve(CurveNum).Var1Max = 360.0;
        CurveManager::PerfCurve(CurveNum).Var1MaxPresent = true;

        CurveManager::PerfCurve(CurveNum).TableIndex = gridIndex;
        CurveManager::PerfCurve(CurveNum).GridValueIndex = CurveManager::btwxtManager.addOutputValues(gridIndex, y);

        CurveManager::NumCurves += 1;
        return CurveNum;
    }

    void CalcWindPressureCoeffs()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   May 2003
        //       MODIFIED       Revised by L. Gu, Nov. 2005, to meet requirements of AirflowNetwork
        //       MODIFIED       Revised by L. Gu, Dec. 2008, to set the number of external nodes based on
        //                      the number of external surfaces
        //       MODIFIED       Revised by J. DeGraw, Feb. 2017, to use tables
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates surface-average wind pressure coefficients for
        // the walls and roof of a rectangular building.

        // METHODOLOGY EMPLOYED:
        // Interpolates correlations between surface-average wind pressure coefficient and wind direction based on
        // measurements (see REFERENCES). Applicable only to rectangular buildings.

        // REFERENCES:
        // For low-rise buildings: M.V. Swami and S. Chandra, Correlations for Pressure Distribution
        // on Buildings and Calculation of Natural-Ventilation Airflow. ASHRAE Transactions 94 (1): 243-266.
        // For high-rise buildings: 2001 ASHRAE Fundamentals Handbook, p. 16.5, Fig. 7, "Surface Averaged
        // Wall Pressure Coefficients for Tall Buildings" and p.16.6, Fig. 9, "Surface Averaged Roof Pressure
        // Coefficients for Tall Buildings; from R.E. Akins, J.A. Peterka, and J.E. Cermak. 1979.
        // Averaged Pressure Coefficients for Rectangular Buildings. Wind Engineering. Proc. Fifth
        // International Conference 7:369-80, Fort Collins, CO. Pergamon Press, NY.

        // Using/Aliasing
        using namespace DataSurfaces;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:na
        // SUBROUTINE PARAMETER DEFINITIONS
        //  index 1 is wind incidence angle (0,30,60,...,300,330 deg)
        //  index 2 is side ratio (0.25,1.0,4.0),
        static Array2D<Real64> const CPHighRiseWall(
            3,
            12,
            reshape2<Real64, int>({0.60, 0.54, 0.23,  -0.25, -0.61, -0.55, -0.51, -0.55, -0.61, -0.25, 0.23,  0.54,
                                   0.60, 0.48, 0.04,  -0.56, -0.56, -0.42, -0.37, -0.42, -0.56, -0.56, 0.04,  0.48,
                                   0.60, 0.44, -0.26, -0.70, -0.53, -0.32, -0.22, -0.32, -0.53, -0.70, -0.26, 0.44},
                                  {3, 12})); // Surface-averaged wind-pressure coefficient array for walls // Explicit reshape2 template args are
                                             // work-around for VC++2013 bug
        //  index 1 is wind incidence angle (0,30,60,...,300,330 deg)
        //  index 2 is side ratio (0.25,0.5,1.0),
        static Array2D<Real64> const CPHighRiseRoof(
            3,
            12,
            reshape2<Real64, int>({-0.28, -0.69, -0.72, -0.76, -0.72, -0.69, -0.28, -0.69, -0.72, -0.76, -0.72, -0.69,
                                   -0.47, -0.52, -0.70, -0.76, -0.70, -0.52, -0.47, -0.52, -0.70, -0.76, -0.70, -0.52,
                                   -0.70, -0.55, -0.55, -0.70, -0.55, -0.55, -0.70, -0.55, -0.55, -0.70, -0.55, -0.55},
                                  {3, 12})); // Surface-averaged wind-pressure coefficient array for roof // Explicit reshape2 template args are
                                             // work-around for VC++2013 bug

        // INTERFACE BLOCK SPECIFICATIONS:na
        // DERIVED TYPE DEFINITIONS:na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FacadeNum;         // Facade number
        int ExtNum;            // External number
        int AFNZnNum;          // Zone number
        Real64 SideRatio;      // For vertical facades, width of facade / width of adjacent facade
        Real64 SR;             // SideRatio restricted to 0.25 to 4.0 range
        Real64 SideRatioFac;   // LOG(SideRatio)
        Real64 IncRad;         // IncAng in radians
        int IAng;              // Incidence angle index; used in interpolation
        Real64 DelAng;         // Incidence angle difference; used in interpolation
        Real64 WtAng;          // Incidence angle weighting factor; used in interpolation
        int ISR;               // Side ratio index, for interpolation
        Real64 WtSR;           // Side ratio weighting factor; used in interpolation
        int SurfNum;           // Surface number
        int SurfDatNum;        // Surface data number
        Real64 SurfAng;        // Azimuth angle of surface normal (degrees clockwise from North)
        int FacadeNumThisSurf; // Facade number for a particular surface
        Real64 AngDiff;        // Angle difference between wind and surface direction (deg)
        Real64 AngDiffMin;     // Minimum angle difference between wind and surface direction (deg)
        std::string Name;      // External node name
        std::vector<int> curveIndex = {0, 0, 0, 0, 0};

        // Facade azimuth angle
        for (FacadeNum = 1; FacadeNum <= 4; ++FacadeNum) {
            FacadeAng(FacadeNum) = AirflowNetworkSimu.Azimuth + (FacadeNum - 1) * 90.0;
            if (FacadeAng(FacadeNum) >= 360.0) {
                FacadeAng(FacadeNum) -= 360.0;
            }
        }

        FacadeAng(5) = AirflowNetworkSimu.Azimuth + 90.0;

        // Create AirflowNetwork external node objects -- one for each of the external surfaces

        MultizoneExternalNodeData.allocate(AirflowNetworkNumOfExtSurfaces);
        AirflowNetworkNumOfExtNode = AirflowNetworkNumOfExtSurfaces;
        NumOfExtNodes = AirflowNetworkNumOfExtSurfaces;
        for (ExtNum = 1; ExtNum <= NumOfExtNodes; ++ExtNum) {
            MultizoneExternalNodeData(ExtNum).ExtNum = AirflowNetworkNumOfZones + ExtNum;
            ObjexxFCL::gio::write(Name, "('ExtNode',I4)") << ExtNum;
            MultizoneExternalNodeData(ExtNum).Name = stripped(Name);
        }

        // Associate each external node with SurfaceData

        ExtNum = 0;
        for (SurfDatNum = 1; SurfDatNum <= AirflowNetworkNumOfSurfaces; ++SurfDatNum) {
            if (SurfDatNum > AirflowNetworkNumOfSurfaces - NumOfLinksIntraZone) {
                continue;
            }
            SurfNum = MultizoneSurfaceData(SurfDatNum).SurfNum;
            if (SurfNum == 0) {
                continue; // Error caught earlier
            }
            if (Surface(SurfNum).ExtBoundCond == ExternalEnvironment ||
                (Surface(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt && Surface(SurfNum).ExtWind)) {
                ++ExtNum;
                if (Surface(SurfNum).Tilt >= 45.0) { // "Vertical" surface
                    SurfAng = Surface(SurfNum).Azimuth;
                    FacadeNumThisSurf = 1;
                    AngDiffMin = std::abs(SurfAng - FacadeAng(1));
                    if (AngDiffMin > 359.0) {
                        AngDiffMin = std::abs(AngDiffMin - 360.0);
                    }
                    for (FacadeNum = 2; FacadeNum <= 4; ++FacadeNum) {
                        AngDiff = std::abs(SurfAng - FacadeAng(FacadeNum));
                        if (AngDiff > 359.0) {
                            AngDiff = std::abs(AngDiff - 360.0);
                        }
                        if (AngDiff < AngDiffMin) {
                            AngDiffMin = AngDiff;
                            FacadeNumThisSurf = FacadeNum;
                        }
                    }
                    // ObjexxFCL::gio::write( Name, "('FacadeNum',I1)" ) << FacadeNumThisSurf;
                    // MultizoneExternalNodeData( ExtNum ).CPVNum = FacadeNumThisSurf;
                    // MultizoneExternalNodeData(ExtNum).curve = curveIndex[FacadeNumThisSurf - 1];
                    MultizoneExternalNodeData(ExtNum).facadeNum = FacadeNumThisSurf;
                } else { // "Roof" surface
                    // ObjexxFCL::gio::write(Name, "('FacadeNum',I1)") << 5;
                    // MultizoneExternalNodeData( ExtNum ).CPVNum = 5;
                    MultizoneExternalNodeData(ExtNum).facadeNum = 5;
                    // MultizoneExternalNodeData(ExtNum).curve = curveIndex[4];
                }
                MultizoneSurfaceData(SurfDatNum).NodeNums[1] = MultizoneExternalNodeData(ExtNum).ExtNum;
                MultizoneSurfaceData(SurfDatNum).ExternalNodeName = MultizoneExternalNodeData(ExtNum).Name;
            }
            // else { // Not an exterior surface
            //       MultizoneSurfaceData(SurfDatNum)%ExternalNodeName = ' '
            //}
        }

        // Check if using the advanced single sided model
        for (AFNZnNum = 1; AFNZnNum <= AirflowNetworkNumOfZones; ++AFNZnNum) {
            if (MultizoneZoneData(AFNZnNum).SingleSidedCpType == "ADVANCED") {
                ++AirflowNetworkNumOfSingleSideZones;
            }
        }

        std::vector<Real64> dirs30 = {0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360};
        std::vector<Btwxt::GridAxis> dirs30Axes;
        dirs30Axes.emplace_back(dirs30, Btwxt::Method::LINEAR, Btwxt::Method::LINEAR, std::pair<double, double>{0.0, 360.0});

        auto dirs30GridIndex = CurveManager::btwxtManager.addGrid("30 Degree Increments", Btwxt::GriddedData(dirs30Axes));

        if (AirflowNetworkNumOfSingleSideZones == 0) { // do the standard surface average coefficient calculation
            // Create the array of wind directions

            // Create a curve for each facade
            for (FacadeNum = 1; FacadeNum <= 5; ++FacadeNum) {
                if (FacadeNum == 1 || FacadeNum == 3 || FacadeNum == 5) {
                    SideRatio = AirflowNetworkSimu.AspectRatio;
                } else { // FacadeNum = 2 or 4
                    SideRatio = 1.0 / AirflowNetworkSimu.AspectRatio;
                }
                if (UtilityRoutines::SameString(AirflowNetworkSimu.BldgType, "HighRise") && FacadeNum != 5) {
                    SideRatio = 1.0 / SideRatio;
                }
                SideRatioFac = std::log(SideRatio);
                std::vector<Real64> vals(13);
                for (WindDirNum = 1; WindDirNum <= 12; ++WindDirNum) {
                    WindAng = (WindDirNum - 1) * 30.0;
                    IncAng = std::abs(WindAng - FacadeAng(FacadeNum));
                    if (IncAng > 180.0) IncAng = 360.0 - IncAng;
                    IAng = int(IncAng / 30.0) + 1;
                    DelAng = mod(IncAng, 30.0);
                    WtAng = 1.0 - DelAng / 30.0;

                    // Wind-pressure coefficients for vertical facades, low-rise building

                    if (UtilityRoutines::SameString(AirflowNetworkSimu.BldgType, "LowRise") && FacadeNum <= 4) {
                        IncRad = IncAng * DegToRadians;
                        Real64 const cos_IncRad_over_2(std::cos(IncRad / 2.0));
                        vals[WindDirNum - 1] = 0.6 * std::log(1.248 - 0.703 * std::sin(IncRad / 2.0) - 1.175 * pow_2(std::sin(IncRad)) +
                                                              0.131 * pow_3(std::sin(2.0 * IncRad * SideRatioFac)) + 0.769 * cos_IncRad_over_2 +
                                                              0.07 * pow_2(SideRatioFac * std::sin(IncRad / 2.0)) + 0.717 * pow_2(cos_IncRad_over_2));
                    }

                    // Wind-pressure coefficients for vertical facades, high-rise building

                    else if (UtilityRoutines::SameString(AirflowNetworkSimu.BldgType, "HighRise") && FacadeNum <= 4) {
                        SR = min(max(SideRatio, 0.25), 4.0);
                        if (SR >= 0.25 && SR < 1.0) {
                            ISR = 1;
                            WtSR = (1.0 - SR) / 0.75;
                        } else { // 1.0 <= SR <= 4.0
                            ISR = 2;
                            WtSR = (4.0 - SR) / 3.0;
                        }
                        vals[WindDirNum - 1] =
                            WtSR * (WtAng * CPHighRiseWall(ISR, IAng) + (1.0 - WtAng) * CPHighRiseWall(ISR, IAng + 1)) +
                            (1.0 - WtSR) * (WtAng * CPHighRiseWall(ISR + 1, IAng) + (1.0 - WtAng) * CPHighRiseWall(ISR + 1, IAng + 1));
                    }

                    // Wind-pressure coefficients for roof (assumed same for low-rise and high-rise buildings)

                    else if ((UtilityRoutines::SameString(AirflowNetworkSimu.BldgType, "HighRise") ||
                              UtilityRoutines::SameString(AirflowNetworkSimu.BldgType, "LowRise")) &&
                             FacadeNum == 5) {
                        SR = min(max(SideRatio, 0.25), 1.0);
                        if (SR >= 0.25 && SR < 0.5) {
                            ISR = 1;
                            WtSR = (0.5 - SR) / 0.25;
                        } else { // 0.5 <= SR <= 1.0
                            ISR = 2;
                            WtSR = (1.0 - SR) / 0.5;
                        }
                        vals[WindDirNum - 1] =
                            WtSR * (WtAng * CPHighRiseRoof(ISR, IAng) + (1.0 - WtAng) * CPHighRiseRoof(ISR, IAng + 1)) +
                            (1.0 - WtSR) * (WtAng * CPHighRiseRoof(ISR + 1, IAng) + (1.0 - WtAng) * CPHighRiseRoof(ISR + 1, IAng + 1));
                    }

                } // End of wind direction loop
                // Add new table
                vals[12] = vals[0]; // Enforce periodicity
                curveIndex[FacadeNum - 1] = makeTable("!WPCTABLE" + std::to_string(FacadeNum), dirs30GridIndex, vals);
            } // End of facade number loop

        } else { //-calculate the advanced single sided wind pressure coefficients

            // Calculate the wind pressure coefficients vs. wind direction for each external node
            // The wind pressure coeffients are stored temporarily in the "valsByFacade" vector and then
            // converted into a table near the end of this else. There will be at least seven profiles
            // (four sides plus one roof plus two for each pair of windows). The name is thus a little
            // misleading, as it isn't really the values by facade once you get beyond the first five.
            std::vector<std::vector<Real64>> valsByFacade(5);
            for (FacadeNum = 0; FacadeNum < 4; ++FacadeNum) {
                valsByFacade[FacadeNum] = std::vector<Real64>(36);
            }
            FacadeNum = 4;
            valsByFacade[FacadeNum] = std::vector<Real64>(12);
            for (FacadeNum = 1; FacadeNum <= 4; ++FacadeNum) {
                if (FacadeNum == 1 || FacadeNum == 3) {
                    SideRatio = AirflowNetworkSimu.AspectRatio;
                } else { // FacadeNum = 2 or 4
                    SideRatio = 1.0 / AirflowNetworkSimu.AspectRatio;
                }
                if (UtilityRoutines::SameString(AirflowNetworkSimu.BldgType, "HighRise") && FacadeNum != 5) {
                    SideRatio = 1.0 / SideRatio;
                }
                SideRatioFac = std::log(SideRatio);
                for (WindDirNum = 1; WindDirNum <= 36; ++WindDirNum) {
                    WindAng = (WindDirNum - 1) * 10.0;
                    IncAng = std::abs(WindAng - FacadeAng(FacadeNum));
                    if (IncAng > 180.0) IncAng = 360.0 - IncAng;
                    IAng = int(IncAng / 10.0) + 1;
                    DelAng = mod(IncAng, 10.0);
                    WtAng = 1.0 - DelAng / 10.0;
                    // Wind-pressure coefficients for vertical facades, low-rise building
                    IncRad = IncAng * DegToRadians;
                    valsByFacade[FacadeNum - 1][WindDirNum - 1] =
                        0.6 * std::log(1.248 - 0.703 * std::sin(IncRad / 2.0) - 1.175 * pow_2(std::sin(IncRad)) +
                                       0.131 * pow_3(std::sin(2.0 * IncRad * SideRatioFac)) + 0.769 * std::cos(IncRad / 2.0) +
                                       0.07 * pow_2(SideRatioFac * std::sin(IncRad / 2.0)) + 0.717 * pow_2(std::cos(IncRad / 2.0)));
                } // End of wind direction loop
            }     // End of facade number loop
            // Add a roof
            FacadeNum = 5;
            SR = min(max(SideRatio, 0.25), 1.0);
            if (SR >= 0.25 && SR < 0.5) {
                ISR = 1;
                WtSR = (0.5 - SR) / 0.25;
            } else { // 0.5 <= SR <= 1.0
                ISR = 2;
                WtSR = (1.0 - SR) / 0.5;
            }
            for (WindDirNum = 1; WindDirNum <= 12; ++WindDirNum) {
                WindAng = (WindDirNum - 1) * 30.0;
                IncAng = std::abs(WindAng - FacadeAng(FacadeNum));
                if (IncAng > 180.0) IncAng = 360.0 - IncAng;
                IAng = int(IncAng / 30.0) + 1;
                DelAng = mod(IncAng, 30.0);
                WtAng = 1.0 - DelAng / 30.0;
                // Wind-pressure coefficients for roof (assumed same for low-rise and high-rise buildings)
                valsByFacade[FacadeNum - 1][WindDirNum - 1] =
                    WtSR * (WtAng * CPHighRiseRoof(ISR, IAng) + (1.0 - WtAng) * CPHighRiseRoof(ISR, IAng + 1)) +
                    (1.0 - WtSR) * (WtAng * CPHighRiseRoof(ISR + 1, IAng) + (1.0 - WtAng) * CPHighRiseRoof(ISR + 1, IAng + 1));
            }
            CalcSingleSidedCps(valsByFacade); // run the advanced single sided subroutine if at least one zone calls for it
            // Resize the curve index array
            curveIndex.resize(valsByFacade.size());
            // Create the curves

            std::vector<Real64> dirs10 = {0,   10,  20,  30,  40,  50,  60,  70,  80,  90,  100, 110, 120, 130, 140, 150, 160, 170, 180,
                                          190, 200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360};

            std::vector<Btwxt::GridAxis> dirs10Axes;
            dirs10Axes.emplace_back(dirs10, Btwxt::Method::LINEAR, Btwxt::Method::LINEAR, std::pair<double, double>{0.0, 360.0});

            auto dirs10GridIndex = CurveManager::btwxtManager.addGrid("10 Degree Increments", Btwxt::GriddedData(dirs10Axes));

            for (FacadeNum = 1; FacadeNum <= 4; ++FacadeNum) {
                valsByFacade[FacadeNum - 1].push_back(valsByFacade[FacadeNum - 1][0]); // Enforce periodicity
                curveIndex[FacadeNum - 1] = makeTable("!SSWPCTABLEFACADE" + std::to_string(FacadeNum), dirs10GridIndex, valsByFacade[FacadeNum - 1]);
            }
            FacadeNum = 5;
            valsByFacade[FacadeNum - 1].push_back(valsByFacade[FacadeNum - 1][0]); // Enforce periodicity
            curveIndex[FacadeNum - 1] = makeTable("!SSWPCTABLEFACADE" + std::to_string(FacadeNum), dirs30GridIndex, valsByFacade[FacadeNum - 1]);
            for (unsigned facadeNum = 6; facadeNum <= valsByFacade.size(); ++facadeNum) {
                valsByFacade[facadeNum - 1].push_back(valsByFacade[facadeNum - 1][0]); // Enforce periodicity
                curveIndex[facadeNum - 1] = makeTable("!SSWPCTABLE" + std::to_string(facadeNum), dirs10GridIndex, valsByFacade[facadeNum - 1]);
            }
        }
        // Connect the external nodes to the new curves
        for (ExtNum = 1; ExtNum <= NumOfExtNodes; ++ExtNum) {
            MultizoneExternalNodeData(ExtNum).curve = curveIndex[MultizoneExternalNodeData(ExtNum).facadeNum - 1];
        }
    }

    Real64 CalcWindPressure(int const curve,           // Curve index, change this to pointer after curve refactor
                            bool const symmetricCurve, // True if the curve is symmetric (0 to 180)
                            bool const relativeAngle,  // True if the Cp curve angle is measured relative to the surface
                            Real64 const azimuth,      // Azimuthal angle of surface
                            Real64 const windSpeed,    // Wind velocity
                            Real64 const windDir,      // Wind direction
                            Real64 const dryBulbTemp,  // Air node dry bulb temperature
                            Real64 const humRat        // Air node humidity ratio
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       Jason DeGraw, Feb. 2017, modify to use curves
        //       MODIFIED       Xuan Luo, Aug. 2017, modify to use local air condition
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates surface wind pressure based on given CP values

        // REFERENCES:
        // COMIS Fundamentals

        // Return value is wind pressure[Pa]

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 angle(windDir);
        Real64 rho; // Outdoor air density
        Real64 Cp;  // Cp value at given wind direction

        // Calculate outdoor density
        rho = PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, dryBulbTemp, humRat);

        // Calculate pressure coefficient
        if (relativeAngle) {
            angle = angle - azimuth;
            if (angle < 0.0) {
                angle += 360.0;
            }
        }
        if (symmetricCurve) {
            if (angle > 180.0) {
                angle = 360.0 - angle;
            }
        }
        Cp = CurveManager::CurveValue(curve, angle);

        return Cp * 0.5 * rho * windSpeed * windSpeed;
    }

    Real64 CalcDuctInsideConvResist(Real64 const Tair, // Average air temperature
                                    Real64 const mdot, // Mass flow rate
                                    Real64 const Dh,   // Hydraulic diameter
                                    Real64 const hIn   // User defined convection coefficient
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Matt Mitchell, Tony Fontanini
        //       DATE WRITTEN   Feb. 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates duct inside convection coefficients

        // REFERENCES:
        // ASTM C1340
        // Jakob, F.E.,  Fischer, R.D., Flanigan, L.J. 1987. "Experimental Validation of the Duct Submodel for the SP43 Simulation Model." ASHRAE
        // Trans. pp 1499-1514.

        Real64 hIn_final = 0;

        if (hIn == 0) {

            Real64 Tair_IP = Tair * 1.8 + 32.0;     // Convert C to F
            Real64 mdot_IP = mdot * 2.20462 * 3600; // Convert kg/s to lb/hr
            Real64 Dh_IP = Dh * 3.28084;            // Convert m to ft
            Real64 Ai_IP = pow_2(Dh_IP) * Pi / 4;

            Real64 CorrelationCoeff = 0.00368 + 1.5e-6 * (Tair_IP - 80);
            Real64 MassFlux = mdot_IP / Ai_IP; // lb/hr-ft2

            Real64 DuctInsideConvCoeff_IP = CorrelationCoeff * pow(MassFlux, 0.8) / pow(Dh_IP, 0.2); // BTU/hr-ft2-F

            hIn_final = DuctInsideConvCoeff_IP * pow_2(3.28084) * 1.8 * 1055.06 / 3600; // Convert BTU/hr-ft2-F to W/m2-K

        } else {
            hIn_final = hIn;
        }

        if (hIn_final == 0) {
            return 0;
        } else {
            return 1 / hIn_final;
        }
    }

    Real64 CalcDuctOutsideConvResist(Real64 const Ts,      // Surface temperature
                                     Real64 const Tamb,    // Free air temperature
                                     Real64 const Wamb,    // Free air humidity ratio
                                     Real64 const Pamb,    // Free air barometric pressure
                                     Real64 const Dh,      // Hydraulic diameter
                                     Real64 const ZoneNum, // Zone number
                                     Real64 const hOut     // User defined convection coefficient
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Matt Mitchell, Tony Fontanini
        //       DATE WRITTEN   Feb. 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates duct outside convection coefficients

        // REFERENCES:
        // ASTM C1340

        using DataEnvironment::WindSpeed;
        using DataGlobals::GravityConstant;
        using DataGlobals::KelvinConv;

        Real64 k = airThermConductivity(Ts);

        Real64 hOut_final = 0;

        if (hOut == 0) {

            // Free convection
            Real64 Pr = airPrandtl((Ts + Tamb) / 2, Wamb, Pamb);
            Real64 KinVisc = airKinematicVisc((Ts + Tamb) / 2, Wamb, Pamb);
            Real64 Beta = 2.0 / ((Tamb + KelvinConv) + (Ts + KelvinConv));
            Real64 Gr = GravityConstant * Beta * std::abs(Ts - Tamb) * pow_3(Dh) / pow_2(KinVisc);
            Real64 Ra = Gr * Pr;
            Real64 Nu_free(0);

            if (Ra < 10e9) {
                Nu_free = 0.53 * pow(Ra, 0.25);
            } else {
                Nu_free = 0.13 * pow(Ra, 0.333);
            }

            Real64 V = 0;
            // Forced convection
            if (ZoneNum > 0) {
                Real64 ACH = GetZoneInfilAirChangeRate(ZoneNum); // Zone air change rate [1/hr]
                Real64 Vol = Zone(ZoneNum).Volume;               // Zone volume [m3]
                V = pow(Vol, 0.333) * ACH / 3600;                // Average air speed in zone [m/s]
            } else {
                V = WindSpeed;
            }

            Real64 Re = V * Dh / KinVisc; // Reynolds number
            Real64 c = 0;
            Real64 n = 0;

            if (Re <= 4) {
                c = 0.989;
                n = 0.33;
            } else if (4 < Re && Re <= 40) {
                c = 0.911;
                n = 0.385;
            } else if (40 < Re && Re <= 4000) {
                c = 0.683;
                n = 0.466;
            } else if (4000 < Re && Re <= 40000) {
                c = 0.193;
                n = 0.618;
            } else if (40000 < Re) {
                c = 0.0266;
                n = 0.805;
            }

            Real64 Nu_forced = c * pow(Re, n) * pow(Pr, 0.333);

            Real64 Nu_combined = pow(pow_3(Nu_free) + pow_3(Nu_forced), 0.333);
            hOut_final = Nu_combined * k / Dh;

        } else {
            hOut_final = hOut;
        }

        if (hOut_final == 0) {
            return 0;
        } else {
            return 1 / hOut_final;
        }
    }

    void CalcAirflowNetworkHeatBalance()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on Subroutine CalcADSHeatBalance

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs AirflowNetwork thermal simulations.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        using DataEnvironment::OutBaroPress;
        using DataEnvironment::OutHumRat;
        using DataGlobals::KelvinConv;
        using DataGlobals::StefanBoltzmann;
        using DataHeatBalance::Construct;
        using DataHeatBalFanSys::QRadSurfAFNDuct;
        using DataHeatBalSurface::TH;
        using DataHVACGlobals::TimeStepSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int LF;
        int LT;
        int CompNum;
        int NF;
        int NT;
        int CompTypeNum;
        int TypeNum;
        int ExtNodeNum;
        std::string CompName;
        Real64 Ei;
        Real64 DirSign;
        Real64 Tamb;
        Real64 Wamb;
        Real64 Pamb;
        Real64 CpAir;
        Real64 TZON;
        Real64 load;
        int ZoneNum;
        bool found;
        bool OANode;

        MA = 0.0;
        MV = 0.0;

        for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            CompNum = AirflowNetworkLinkageData(i).CompNum;
            CompTypeNum = AirflowNetworkCompData(CompNum).CompTypeNum;
            CompName = AirflowNetworkCompData(CompNum).EPlusName;
            CpAir = PsyCpAirFnW((AirflowNetworkNodeSimu(AirflowNetworkLinkageData(i).NodeNums[0]).WZ +
                                 AirflowNetworkNodeSimu(AirflowNetworkLinkageData(i).NodeNums[1]).WZ) /
                                2.0);
            // Calculate duct conduction loss
            if (CompTypeNum == CompTypeNum_DWC && CompName == BlankString) { // Duct element only
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                // Fatal error when return flow is opposite to the desired direction
                if (AirflowNetworkLinkSimu(i).FLOW == 0.0 && AirflowNetworkLinkSimu(i).FLOW2 > 0.0) {
                    if (LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum)) {
                        ShowSevereError("AirflowNetwork: The airflow direction is opposite to the intended direction (from node 1 to node 2) in "
                                        "AirflowNetwork:Distribution:Linkage = " +
                                        AirflowNetworkLinkageData(i).Name);
                        ShowContinueErrorTimeStamp("");
                        ShowContinueError(
                            "The sum of the airflows entering the zone is greater than the airflows leaving the zone (e.g., wind and stack effect).");
                        ShowContinueError("Please check wind speed or reduce values of \"Window/Door Opening Factor, or Crack Factor\" defined in "
                                          "AirflowNetwork:MultiZone:Surface objects.");
                        //					ShowFatalError( "AirflowNetwork: The previous error causes termination." );
                    }
                }

                if (AirflowNetworkLinkageData(i).ZoneNum < 0) {
                    ExtNodeNum = AirflowNetworkLinkageData(i).NodeNums[1];
                    if (AirflowNetworkNodeData(ExtNodeNum).OutAirNodeNum > 0 && Node(AirflowNetworkNodeData(ExtNodeNum).OutAirNodeNum).IsLocalNode) {
                        Tamb = Node(AirflowNetworkNodeData(ExtNodeNum).OutAirNodeNum).OutAirDryBulb;
                        Wamb = Node(AirflowNetworkNodeData(ExtNodeNum).OutAirNodeNum).HumRat;
                    } else {
                        Tamb = OutDryBulbTempAt(AirflowNetworkNodeData(ExtNodeNum).NodeHeight);
                        Wamb = OutHumRat;
                    }
                } else if (AirflowNetworkLinkageData(i).ZoneNum == 0) {
                    Tamb = AirflowNetworkNodeSimu(LT).TZ;
                    Wamb = AirflowNetworkNodeSimu(LT).WZ;
                } else {
                    Tamb = ANZT(AirflowNetworkLinkageData(i).ZoneNum);
                    Wamb = ANZW(AirflowNetworkLinkageData(i).ZoneNum);
                }

                Pamb = OutBaroPress;

                Real64 const tolerance = 0.001;
                Real64 UThermal(10); // Initialize. This will get updated.
                Real64 UThermal_iter = 0;
                Real64 Tsurr = Tamb;
                Real64 Tsurr_K = Tsurr + KelvinConv;
                Real64 Tin = AirflowNetworkNodeSimu(LF).TZ;
                Real64 TDuctSurf = (Tamb + Tin) / 2.0;
                Real64 TDuctSurf_K = TDuctSurf + KelvinConv;
                Real64 DuctSurfArea = DisSysCompDuctData(TypeNum).L * DisSysCompDuctData(TypeNum).hydraulicDiameter * Pi;

                // If user defined view factors not present, calculate air-to-air heat transfer
                if (AirflowNetworkLinkageData(i).LinkageViewFactorObjectNum == 0) {

                    // Calculate convection coefficient if one or both not present
                    if (DisSysCompDuctData(TypeNum).InsideConvCoeff == 0 && DisSysCompDuctData(TypeNum).OutsideConvCoeff == 0) {
                        while (std::abs(UThermal - UThermal_iter) > tolerance) {
                            UThermal_iter = UThermal;

                            Real64 RThermConvIn = CalcDuctInsideConvResist(Tin,
                                                                           AirflowNetworkLinkSimu(i).FLOW,
                                                                           DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                           DisSysCompDuctData(TypeNum).InsideConvCoeff);
                            Real64 RThermConvOut = CalcDuctOutsideConvResist(TDuctSurf,
                                                                             Tamb,
                                                                             Wamb,
                                                                             Pamb,
                                                                             DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                             AirflowNetworkLinkageData(i).ZoneNum,
                                                                             DisSysCompDuctData(TypeNum).OutsideConvCoeff);
                            Real64 RThermConduct = 1.0 / DisSysCompDuctData(TypeNum).UThermConduct;
                            Real64 RThermTotal = RThermConvIn + RThermConvOut + RThermConduct;
                            UThermal = pow(RThermTotal, -1);

                            // Duct conduction, assuming effectiveness = 1 - exp(-NTU)
                            Ei = General::epexp(-UThermal * DuctSurfArea / (DirSign * AirflowNetworkLinkSimu(i).FLOW * CpAir));
                            Real64 QCondDuct = std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir * (Tamb - Tin) * (1 - Ei);

                            TDuctSurf = Tamb - QCondDuct * RThermConvOut / DuctSurfArea;
                        }
                    } else { // Air-to-air only. U and h values are all known
                        Real64 RThermConvIn = CalcDuctInsideConvResist(Tin,
                                                                       AirflowNetworkLinkSimu(i).FLOW,
                                                                       DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                       DisSysCompDuctData(TypeNum).InsideConvCoeff);
                        Real64 RThermConvOut = CalcDuctOutsideConvResist(TDuctSurf,
                                                                         Tamb,
                                                                         Wamb,
                                                                         Pamb,
                                                                         DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                         AirflowNetworkLinkageData(i).ZoneNum,
                                                                         DisSysCompDuctData(TypeNum).OutsideConvCoeff);
                        Real64 RThermConduct = 1.0 / DisSysCompDuctData(TypeNum).UThermConduct;
                        Real64 RThermTotal = RThermConvIn + RThermConvOut + RThermConduct;
                        UThermal = pow(RThermTotal, -1);
                    }

                    Tsurr = Tamb;

                } else { // Air-to-air + radiation heat transfer

                    auto &VFObj(AirflowNetworkLinkageViewFactorData(AirflowNetworkLinkageData(i).LinkageViewFactorObjectNum));
                    VFObj.QRad = 0;
                    VFObj.QConv = 0;

                    Real64 Tin_ave = Tin;
                    Real64 hOut = 0;

                    while (std::abs(UThermal - UThermal_iter) > tolerance) {
                        UThermal_iter = UThermal;

                        Real64 RThermConvIn = CalcDuctInsideConvResist(Tin_ave,
                                                                       AirflowNetworkLinkSimu(i).FLOW,
                                                                       DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                       DisSysCompDuctData(TypeNum).InsideConvCoeff);
                        Real64 RThermConvOut = CalcDuctOutsideConvResist(TDuctSurf,
                                                                         Tamb,
                                                                         Wamb,
                                                                         Pamb,
                                                                         DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                         AirflowNetworkLinkageData(i).ZoneNum,
                                                                         DisSysCompDuctData(TypeNum).OutsideConvCoeff);

                        if (RThermConvOut > 0.0) {
                            hOut = 1 / RThermConvOut;
                        }

                        Real64 RThermConduct = 1.0 / DisSysCompDuctData(TypeNum).UThermConduct;

                        Real64 hrjTj_sum = 0;
                        Real64 hrj_sum = 0;

                        for (int j = 1; j <= VFObj.LinkageSurfaceData.u(); ++j) {

                            int ZoneSurfNum = VFObj.LinkageSurfaceData(j).SurfaceNum;

                            Real64 TSurfj = TH(1, 1, ZoneSurfNum);
                            Real64 TSurfj_K = TSurfj + KelvinConv;

                            Real64 ZoneSurfEmissivity = Construct(Surface(ZoneSurfNum).Construction).InsideAbsorpThermal;
                            Real64 ZoneSurfArea = Surface(ZoneSurfNum).Area;

                            Real64 DuctEmissivity = VFObj.DuctEmittance;
                            Real64 DuctExposureFrac = VFObj.DuctExposureFraction;
                            Real64 DuctToZoneSurfViewFactor = VFObj.LinkageSurfaceData(j).ViewFactor;

                            Real64 DuctSurfResistance = (1 - DuctEmissivity) / (DuctExposureFrac * DuctSurfArea * DuctEmissivity);
                            Real64 SpaceResistance = 1 / (DuctExposureFrac * DuctSurfArea * DuctToZoneSurfViewFactor);
                            Real64 ZoneSurfResistance = (1 - ZoneSurfEmissivity) / (ZoneSurfArea * ZoneSurfEmissivity);

                            VFObj.LinkageSurfaceData(j).SurfaceResistanceFactor =
                                StefanBoltzmann / (DuctSurfResistance + SpaceResistance + ZoneSurfResistance);

                            Real64 hrj = VFObj.LinkageSurfaceData(j).SurfaceResistanceFactor * (TDuctSurf_K + TSurfj_K) *
                                         (pow_2(TDuctSurf_K) + pow_2(TSurfj_K)) / DuctSurfArea;

                            hrjTj_sum += hrj * TSurfj;
                            hrj_sum += hrj;
                        }

                        Tsurr = (hOut * Tamb + hrjTj_sum) / (hOut + hrj_sum); // Surroundings temperature [C]
                        Tsurr_K = Tsurr + KelvinConv;

                        Real64 RThermTotal = RThermConvIn + RThermConduct + 1 / (hOut + hrj_sum);
                        UThermal = pow(RThermTotal, -1);

                        Real64 NTU = UThermal * DuctSurfArea / (DirSign * AirflowNetworkLinkSimu(i).FLOW * CpAir);
                        Tin_ave = Tsurr + (Tin - Tsurr) * (1 / NTU) * (1 - exp(-NTU));

                        TDuctSurf = Tin_ave - UThermal * (RThermConvIn + RThermConduct) * (Tin_ave - Tsurr);
                        TDuctSurf_K = TDuctSurf + KelvinConv;
                    }

                    for (int j = 1; j <= VFObj.LinkageSurfaceData.u(); ++j) {
                        int ZoneSurfNum = VFObj.LinkageSurfaceData(j).SurfaceNum;
                        Real64 TSurfj = TH(1, 1, ZoneSurfNum);
                        Real64 TSurfj_K = TSurfj + KelvinConv;
                        VFObj.LinkageSurfaceData(j).SurfaceRadLoad = VFObj.LinkageSurfaceData(j).SurfaceResistanceFactor *
                                                                     (pow_4(TDuctSurf_K) - pow_4(TSurfj_K)); // Radiant load for this surface [W]
                        int SurfNum = VFObj.LinkageSurfaceData(j).SurfaceNum;
                        Real64 ZoneSurfaceArea = Surface(SurfNum).Area;
                        QRadSurfAFNDuct(SurfNum) += VFObj.LinkageSurfaceData(j).SurfaceRadLoad * TimeStepSys * SecInHour /
                                                    ZoneSurfaceArea;              // Energy to each surface per unit area [J/m2]
                        VFObj.QRad += VFObj.LinkageSurfaceData(j).SurfaceRadLoad; // Total radiant load from all surfaces for this system timestep [W]
                    }

                    VFObj.QConv = hOut * DuctSurfArea * (TDuctSurf - Tamb);
                    UThermal = (VFObj.QRad + VFObj.QConv) / (DuctSurfArea * std::abs(Tsurr - Tin));
                }

                if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum) && AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    if (AirflowNetworkLinkSimu(i).FLOW2 > 0.0) {
                        Ei = General::epexp(-UThermal * DuctSurfArea / (AirflowNetworkLinkSimu(i).FLOW2 * CpAir));
                    } else {
                        Ei = 0.0;
                    }
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir * Ei;
                    MV(LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * Tsurr * (1.0 - Ei) * CpAir;
                } else {
                    if (AirflowNetworkLinkSimu(i).FLOW > 0.0) {
                        Ei = General::epexp(-UThermal * DuctSurfArea / (DirSign * AirflowNetworkLinkSimu(i).FLOW * CpAir));
                    } else {
                        Ei = 0.0;
                    }
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir * Ei;
                    MV(LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW) * Tsurr * (1.0 - Ei) * CpAir;
                }
            }
            if (CompTypeNum == CompTypeNum_TMU) { // Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                if (AirflowNetworkLinkSimu(i).FLOW == 0.0) {
                    Ei = 0.0;
                } else {
                    Ei = std::exp(-0.001 * DisSysCompTermUnitData(TypeNum).L * DisSysCompTermUnitData(TypeNum).hydraulicDiameter * Pi /
                                  (DirSign * AirflowNetworkLinkSimu(i).FLOW * CpAir));
                }
                Tamb = AirflowNetworkNodeSimu(LT).TZ;
                if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum) && AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    if (AirflowNetworkLinkSimu(i).FLOW2 == 0.0) {
                        Ei = 0.0;
                    } else {
                        Ei = General::epexp(-0.001 * DisSysCompTermUnitData(TypeNum).L * DisSysCompTermUnitData(TypeNum).hydraulicDiameter * Pi /
                                            (AirflowNetworkLinkSimu(i).FLOW2 * CpAir));
                    }
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir * Ei;
                    MV(LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * Tamb * (1.0 - Ei) * CpAir;
                } else {
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir * Ei;
                    MV(LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW) * Tamb * (1.0 - Ei) * CpAir;
                }
            }
            if (CompTypeNum == CompTypeNum_COI) { // heating or cooling coil
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                //        Ei = exp(-0.001*DisSysCompCoilData(TypeNum)%L*DisSysCompCoilData(TypeNum)%D*pi/ &
                //             (DirSign*AirflowNetworkLinkSimu(I)%FLOW*CpAir))
                //        Tamb = AirflowNetworkNodeSimu(LT)%TZ
                //        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                //                                                 ABS(AirflowNetworkLinkSimu(I)%FLOW)*CpAir
                //        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -ABS(AirflowNetworkLinkSimu(I)%FLOW)*CpAir*Ei
                //        MV(LT) = MV(LT)+ABS(AirflowNetworkLinkSimu(I)%FLOW)*Tamb*(1.0-Ei)*CpAir
            }
            // Calculate temp in a constant pressure drop element
            if (CompTypeNum == CompTypeNum_CPD && CompName == BlankString) { // constant pressure element only
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) {                  // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                }
                if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum) && AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                } else {
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                }
                MV(LT) = 0.0;
            }
            // Calculate return leak
            if ((CompTypeNum == CompTypeNum_PLR || CompTypeNum == CompTypeNum_ELR) && CompName == BlankString) {
                // Return leak element only
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).ExtNodeNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).ExtNodeNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                }
            }
            // Check reheat unit or coil
            if (AirflowNetworkCompData(CompNum).EPlusTypeNum == EPlusTypeNum_RHT && (!AirflowNetworkLinkageData(i).VAVTermDamper)) {
                NF = 0;
                NT = 0;
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusNodeNum > 0) {
                    NF = AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusNodeNum;
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusNodeNum > 0) {
                    NT = AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusNodeNum;
                }
                if ((NF == 0) || (NT == 0)) {
                    ShowFatalError("Node number in the primary air loop is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE = " +
                                   AirflowNetworkLinkageData(i).Name);
                }
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    load = Node(NT).Temp - Node(NF).Temp;
                } else {
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    load = Node(NF).Temp - Node(NT).Temp;
                }
                CpAir = PsyCpAirFnW(Node(NT).HumRat);
                MV(LT) += AirflowNetworkLinkSimu(i).FLOW * CpAir * load;
            }
        }

        // Prescribe temperature for EPlus nodes
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            found = false;
            OANode = false;
            for (j = 1; j <= AirflowNetworkNumOfLinks; ++j) {
                if (AirflowNetworkLinkageData(j).NodeNums[0] == i || AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                    CompNum = AirflowNetworkLinkageData(j).CompNum;
                    if (AirflowNetworkCompData(CompNum).EPlusTypeNum == EPlusTypeNum_RHT && (!AirflowNetworkLinkageData(j).VAVTermDamper)) {
                        found = true;
                        break;
                    }
                    // Overwrite fan outlet node
                    if (AirflowNetworkCompData(CompNum).EPlusTypeNum == EPlusTypeNum_FAN && AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                        found = false;
                        break;
                    }
                    // Overwrite return connection outlet
                    if (AirflowNetworkLinkageData(j).ConnectionFlag == EPlusTypeNum_RCN) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                    if (AirflowNetworkLinkageData(j).ConnectionFlag == EPlusTypeNum_SCN &&
                        AirflowNetworkLinkageData(j).NodeNums[1] == i) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                }
                if (AirflowNetworkLinkageData(j).NodeNums[1] == i &&
                    AirflowNetworkNodeData(AirflowNetworkLinkageData(j).NodeNums[0]).EPlusTypeNum == EPlusTypeNum_OAN) {
                    OANode = true;
                    break;
                }
            }
            if (found) continue;
            if (AirflowNetworkNodeData(i).EPlusZoneNum == 0 && AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_ZIN) continue;
            j = AirflowNetworkNodeData(i).EPlusNodeNum;

            if (j > 0 && (AirflowNetworkNodeData(i).EPlusZoneNum > 0 || AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_FOU ||
                          AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_COU || AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_HXO)) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).Temp * 1.0e10;
            }
            if (j > 0 && OANode) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).Temp * 1.0e10;
            }
            if (AirflowNetworkNodeData(i).EPlusZoneNum > 0 && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 0.9e10) {
                ZoneNum = AirflowNetworkNodeData(i).EPlusZoneNum;
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = ANZT(ZoneNum) * 1.0e10;
            }
            if (AirflowNetworkNodeData(i).ExtNodeNum > 0 && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 0.9e10) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                if (AirflowNetworkNodeData(i).OutAirNodeNum > 0) {
                    MV(i) = Node(AirflowNetworkNodeData(i).OutAirNodeNum).OutAirDryBulb * 1.0e10;
                } else {
                    MV(i) = OutDryBulbTempAt(AirflowNetworkNodeData(i).NodeHeight) * 1.0e10;
                }
            }
            if (AirflowNetworkNodeData(i).RAFNNodeNum > 0 && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 0.9e10) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                ZoneNum = AirflowNetworkNodeData(i).EPlusZoneNum;
                if (RoomAirflowNetworkZoneInfo(ZoneNum).Node(AirflowNetworkNodeData(i).RAFNNodeNum).AirflowNetworkNodeID == i) {
                    MV(i) = RoomAirflowNetworkZoneInfo(ZoneNum).Node(AirflowNetworkNodeData(i).RAFNNodeNum).AirTemp * 1.0e10;
                }
            }
        }

        // Assign node value to distribution nodes with fan off
        for (i = 1 + NumOfNodesMultiZone; i <= AirflowNetworkNumOfNodes; ++i) {
            j = AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && !LoopOnOffFlag(AirflowNetworkNodeData(i).AirLoopNum) && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 1.0e9) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).Temp * 1.0e10;
            }
            if (j == 0 && i > NumOfNodesMultiZone && !LoopOnOffFlag(AirflowNetworkNodeData(i).AirLoopNum)) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = AirflowNetworkNodeSimu(i).TZlast * 1.0e10;
            }
        }

        // Check singularity
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            if (MA((i - 1) * AirflowNetworkNumOfNodes + i) < 1.0e-6) {
                if (i > NumOfNodesMultiZone && !LoopOnOffFlag(AirflowNetworkNodeData(i).AirLoopNum)) {
                    MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                    MV(i) = AirflowNetworkNodeSimu(i).TZlast * 1.0e10;
                } else {
                    ShowFatalError("CalcAirflowNetworkHeatBalance: A diagonal entity is zero in AirflowNetwork matrix at node " +
                                   AirflowNetworkNodeData(i).Name);
                }
            }
        }

        // Get an inverse matrix
        MRXINV(AirflowNetworkNumOfNodes);

        // Calculate node temperatures
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            TZON = 0.0;
            for (j = 1; j <= AirflowNetworkNumOfNodes; ++j) {
                TZON += MA((i - 1) * AirflowNetworkNumOfNodes + j) * MV(j);
            }
            AirflowNetworkNodeSimu(i).TZ = TZON;
        }
    }

    void CalcAirflowNetworkMoisBalance()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on Subroutine CalcADSMoistureBalance

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs AirflowNetwork moisture simulations.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int LF;
        int LT;
        int CompNum;
        int NF;
        int NT;
        int CompTypeNum;
        int TypeNum;
        std::string CompName;
        Real64 Ei;
        Real64 DirSign;
        Real64 Wamb;
        Real64 WZON;
        Real64 load;
        int ZoneNum;
        bool found;
        bool OANode;

        MA = 0.0;
        MV = 0.0;
        for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            CompNum = AirflowNetworkLinkageData(i).CompNum;
            CompTypeNum = AirflowNetworkCompData(CompNum).CompTypeNum;
            CompName = AirflowNetworkCompData(CompNum).EPlusName;
            // Calculate duct moisture diffusion loss
            if (CompTypeNum == CompTypeNum_DWC && CompName == BlankString) { // Duct component only
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                if (AirflowNetworkLinkSimu(i).FLOW == 0.0) {
                    Ei = 0.0;
                } else {
                    Ei = General::epexp(-DisSysCompDuctData(TypeNum).UMoisture * DisSysCompDuctData(TypeNum).L *
                                        DisSysCompDuctData(TypeNum).hydraulicDiameter * Pi / (DirSign * AirflowNetworkLinkSimu(i).FLOW));
                }
                if (AirflowNetworkLinkageData(i).ZoneNum < 0) {
                    Wamb = OutHumRat;
                } else if (AirflowNetworkLinkageData(i).ZoneNum == 0) {
                    Wamb = AirflowNetworkNodeSimu(LT).WZ;
                } else {
                    Wamb = ANZW(AirflowNetworkLinkageData(i).ZoneNum);
                }
                if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum) && AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    if (AirflowNetworkLinkSimu(i).FLOW2 == 0.0) {
                        Ei = 0.0;
                    } else {
                        Ei = General::epexp(-DisSysCompDuctData(TypeNum).UMoisture * DisSysCompDuctData(TypeNum).L *
                                            DisSysCompDuctData(TypeNum).hydraulicDiameter * Pi / (AirflowNetworkLinkSimu(i).FLOW2));
                    }
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2) * Ei;
                    MV(LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * Wamb * (1.0 - Ei);
                } else {
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW) * Ei;
                    MV(LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW) * Wamb * (1.0 - Ei);
                }
            }
            if (CompTypeNum == CompTypeNum_TMU) { // Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                if (AirflowNetworkLinkSimu(i).FLOW == 0.0) {
                    Ei = 0.0;
                } else {
                    Ei = General::epexp(-0.0001 * DisSysCompTermUnitData(TypeNum).L * DisSysCompTermUnitData(TypeNum).hydraulicDiameter * Pi /
                                        (DirSign * AirflowNetworkLinkSimu(i).FLOW));
                }
                Wamb = AirflowNetworkNodeSimu(LT).WZ;
                if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum) && AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    if (AirflowNetworkLinkSimu(i).FLOW2 == 0.0) {
                        Ei = 0.0;
                    } else {
                        Ei = General::epexp(-0.0001 * DisSysCompTermUnitData(TypeNum).L * DisSysCompTermUnitData(TypeNum).hydraulicDiameter * Pi /
                                            (AirflowNetworkLinkSimu(i).FLOW2));
                    }
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2) * Ei;
                    MV(LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * Wamb * (1.0 - Ei);
                } else {
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW) * Ei;
                    MV(LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW) * Wamb * (1.0 - Ei);
                }
            }
            if (CompTypeNum == CompTypeNum_COI) { // heating or cooling coil
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
            }
            // Calculate temp in a constant pressure drop component
            if (CompTypeNum == CompTypeNum_CPD && CompName == BlankString) { // constant pressure element only
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) {                  // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                }
                if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum) && AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                } else {
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
                }
                MV(LT) = 0.0;
            }
            // Calculate return leak
            if ((CompTypeNum == CompTypeNum_PLR || CompTypeNum == CompTypeNum_ELR) && CompName == BlankString) {
                // Return leak component only
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).ExtNodeNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).ExtNodeNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                }
            }
            // Check reheat unit
            if (AirflowNetworkCompData(CompNum).EPlusTypeNum == EPlusTypeNum_RHT && (!AirflowNetworkLinkageData(i).VAVTermDamper)) {
                NF = 0;
                NT = 0;
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusNodeNum > 0) {
                    NF = AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusNodeNum;
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusNodeNum > 0) {
                    NT = AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusNodeNum;
                }
                if ((NF == 0) || (NT == 0)) {
                    ShowFatalError("Node number in the primary air loop is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE = " +
                                   AirflowNetworkLinkageData(i).Name);
                }
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    load = Node(NT).HumRat - Node(NF).HumRat;
                } else {
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    load = Node(NF).HumRat - Node(NT).HumRat;
                }
                MV(LT) += AirflowNetworkLinkSimu(i).FLOW * load;
            }
        }

        // Prescribe temperature for EPlus nodes
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            found = false;
            OANode = false;
            for (j = 1; j <= AirflowNetworkNumOfLinks; ++j) {
                if (AirflowNetworkLinkageData(j).NodeNums[0] == i || AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                    CompNum = AirflowNetworkLinkageData(j).CompNum;
                    if (AirflowNetworkCompData(CompNum).EPlusTypeNum == EPlusTypeNum_RHT && (!AirflowNetworkLinkageData(j).VAVTermDamper)) {
                        found = true;
                        break;
                    }
                    // Overwrite fan outlet node
                    if (AirflowNetworkCompData(CompNum).EPlusTypeNum == EPlusTypeNum_FAN && AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                        found = false;
                        break;
                    }
                    // Overwrite return connection outlet
                    if (AirflowNetworkLinkageData(j).ConnectionFlag == EPlusTypeNum_RCN) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                    if (AirflowNetworkLinkageData(j).ConnectionFlag == EPlusTypeNum_SCN &&
                        AirflowNetworkLinkageData(j).NodeNums[1] == i) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                }
                if (AirflowNetworkLinkageData(j).NodeNums[1] == i &&
                    AirflowNetworkNodeData(AirflowNetworkLinkageData(j).NodeNums[0]).EPlusTypeNum == EPlusTypeNum_OAN) {
                    OANode = true;
                    break;
                }
            }
            if (found) continue;
            if (AirflowNetworkNodeData(i).EPlusZoneNum == 0 && AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_ZIN) continue;
            j = AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && (AirflowNetworkNodeData(i).EPlusZoneNum > 0 || AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_FOU ||
                          AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_COU || AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_HXO)) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).HumRat * 1.0e10;
            }
            if (j > 0 && OANode) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).HumRat * 1.0e10;
            }
            if (AirflowNetworkNodeData(i).EPlusZoneNum > 0 && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 0.9e10) {
                ZoneNum = AirflowNetworkNodeData(i).EPlusZoneNum;
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = ANZW(ZoneNum) * 1.0e10;
            }
            if (AirflowNetworkNodeData(i).ExtNodeNum > 0) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = OutHumRat * 1.0e10;
            }
            if (AirflowNetworkNodeData(i).RAFNNodeNum > 0 && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 0.9e10) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                ZoneNum = AirflowNetworkNodeData(i).EPlusZoneNum;
                if (RoomAirflowNetworkZoneInfo(ZoneNum).Node(AirflowNetworkNodeData(i).RAFNNodeNum).AirflowNetworkNodeID == i) {
                    MV(i) = RoomAirflowNetworkZoneInfo(ZoneNum).Node(AirflowNetworkNodeData(i).RAFNNodeNum).HumRat * 1.0e10;
                }
            }
        }

        // Assign node value to distribution nodes with fan off
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            j = AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && !LoopOnOffFlag(AirflowNetworkNodeData(i).AirLoopNum) && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 1.0e9) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).HumRat * 1.0e10;
            }
            if (j == 0 && i > NumOfNodesMultiZone && !LoopOnOffFlag(AirflowNetworkNodeData(i).AirLoopNum)) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = AirflowNetworkNodeSimu(i).WZlast * 1.0e10;
            }
        }

        // Check singularity
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            if (MA((i - 1) * AirflowNetworkNumOfNodes + i) < 1.0e-8) {
                ShowFatalError("CalcAirflowNetworkMoisBalance: A diagonal entity is zero in AirflowNetwork matrix at node " +
                               AirflowNetworkNodeData(i).Name);
            }
        }

        // Get an inverse matrix
        MRXINV(AirflowNetworkNumOfNodes);

        // Calculate node temperatures
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            WZON = 0.0;
            for (j = 1; j <= AirflowNetworkNumOfNodes; ++j) {
                WZON += MA((i - 1) * AirflowNetworkNumOfNodes + j) * MV(j);
            }
            AirflowNetworkNodeSimu(i).WZ = WZON;
        }
    }

    void CalcAirflowNetworkCO2Balance()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   June. 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on Subroutine CalcAirflowNetworkMoisBalance

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs AirflowNetwork CO2 simulations.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int LF;
        int LT;
        int CompNum;
        int CompTypeNum;
        int TypeNum;
        std::string CompName;
        Real64 DirSign;
        Real64 COZN;
        int ZoneNum;
        bool found;
        bool OANode;

        MA = 0.0;
        MV = 0.0;
        for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            CompNum = AirflowNetworkLinkageData(i).CompNum;
            CompTypeNum = AirflowNetworkCompData(CompNum).CompTypeNum;
            CompName = AirflowNetworkCompData(CompNum).EPlusName;
            // Calculate duct moisture diffusion loss
            if (CompTypeNum == CompTypeNum_DWC && CompName == BlankString) { // Duct component only
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
            }
            if (CompTypeNum == CompTypeNum_TMU) { // Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
            }
            if (CompTypeNum == CompTypeNum_COI) { // heating or cooling coil
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
            }
            // Calculate temp in a constant pressure drop component
            if (CompTypeNum == CompTypeNum_CPD && CompName == BlankString) { // constant pressure element only
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) {                  // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                }
                MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
                MV(LT) = 0.0;
            }
            // Calculate return leak
            if ((CompTypeNum == CompTypeNum_PLR || CompTypeNum == CompTypeNum_ELR) && CompName == BlankString) {
                // Return leak component only
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).ExtNodeNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).ExtNodeNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                }
            }
        }

        // Prescribe temperature for EPlus nodes
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            found = false;
            OANode = false;
            for (j = 1; j <= AirflowNetworkNumOfLinks; ++j) {
                if (AirflowNetworkLinkageData(j).NodeNums[0] == i || AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                    CompNum = AirflowNetworkLinkageData(j).CompNum;
                    if (AirflowNetworkCompData(CompNum).EPlusTypeNum == EPlusTypeNum_RHT && (!AirflowNetworkLinkageData(j).VAVTermDamper)) {
                        found = true;
                        break;
                    }
                    // Overwrite fan outlet node
                    if (AirflowNetworkCompData(CompNum).EPlusTypeNum == EPlusTypeNum_FAN && AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                        found = false;
                        break;
                    }
                    // Overwrite return connection outlet
                    if (AirflowNetworkLinkageData(j).ConnectionFlag == EPlusTypeNum_RCN) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                    if (AirflowNetworkLinkageData(j).ConnectionFlag == EPlusTypeNum_SCN &&
                        AirflowNetworkLinkageData(j).NodeNums[1] == i) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                }
                if (AirflowNetworkLinkageData(j).NodeNums[1] == i &&
                    AirflowNetworkNodeData(AirflowNetworkLinkageData(j).NodeNums[0]).EPlusTypeNum == EPlusTypeNum_OAN) {
                    OANode = true;
                    break;
                }
            }
            if (found) continue;
            if (AirflowNetworkNodeData(i).EPlusZoneNum == 0 && AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_ZIN) continue;
            j = AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && (AirflowNetworkNodeData(i).EPlusZoneNum > 0 || AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_FOU ||
                          AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_COU || AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_HXO)) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).CO2 * 1.0e10;
            }
            if (j > 0 && OANode) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).CO2 * 1.0e10;
            }
            if (AirflowNetworkNodeData(i).EPlusZoneNum > 0 && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 0.9e10) {
                ZoneNum = AirflowNetworkNodeData(i).EPlusZoneNum;
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = ANCO(ZoneNum) * 1.0e10;
            }
            if (AirflowNetworkNodeData(i).ExtNodeNum > 0) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = OutdoorCO2 * 1.0e10;
            }
        }

        // Assign node value to distribution nodes with fan off
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            j = AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && !LoopOnOffFlag(AirflowNetworkNodeData(i).AirLoopNum) && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 1.0e9) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).CO2 * 1.0e10;
            }
            if (j == 0 && i > NumOfNodesMultiZone && !LoopOnOffFlag(AirflowNetworkNodeData(i).AirLoopNum)) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = AirflowNetworkNodeSimu(i).CO2Zlast * 1.0e10;
            }
        }

        // Check singularity
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            if (MA((i - 1) * AirflowNetworkNumOfNodes + i) < 1.0e-6) {
                ShowFatalError("CalcAirflowNetworkCO2Balance: A diagonal entity is zero in AirflowNetwork matrix at node " +
                               AirflowNetworkNodeData(i).Name);
            }
        }

        // Get an inverse matrix
        MRXINV(AirflowNetworkNumOfNodes);

        // Calculate node temperatures
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            COZN = 0.0;
            for (j = 1; j <= AirflowNetworkNumOfNodes; ++j) {
                COZN += MA((i - 1) * AirflowNetworkNumOfNodes + j) * MV(j);
            }
            AirflowNetworkNodeSimu(i).CO2Z = COZN;
        }
    }

    void CalcAirflowNetworkGCBalance()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Jan. 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on Subroutine CalcAirflowNetworkCO2Balance

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs AirflowNetwork generic contaminant simulations.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int LF;
        int LT;
        int CompNum;
        int CompTypeNum;
        int TypeNum;
        std::string CompName;
        Real64 DirSign;
        Real64 COZN;
        int ZoneNum;
        bool found;
        bool OANode;

        MA = 0.0;
        MV = 0.0;
        for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            CompNum = AirflowNetworkLinkageData(i).CompNum;
            CompTypeNum = AirflowNetworkCompData(CompNum).CompTypeNum;
            CompName = AirflowNetworkCompData(CompNum).EPlusName;
            // Calculate duct moisture diffusion loss
            if (CompTypeNum == CompTypeNum_DWC && CompName == BlankString) { // Duct component only
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
            }
            if (CompTypeNum == CompTypeNum_TMU) { // Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
            }
            if (CompTypeNum == CompTypeNum_COI) { // heating or cooling coil
                TypeNum = AirflowNetworkCompData(CompNum).TypeNum;
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
            }
            // Calculate temp in a constant pressure drop component
            if (CompTypeNum == CompTypeNum_CPD && CompName == BlankString) { // constant pressure element only
                if (AirflowNetworkLinkSimu(i).FLOW > 0.0) {                  // flow direction is the same as input from node 1 to node 2
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                }
                MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
                MV(LT) = 0.0;
            }
            // Calculate return leak
            if ((CompTypeNum == CompTypeNum_PLR || CompTypeNum == CompTypeNum_ELR) && CompName == BlankString) {
                // Return leak component only
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).ExtNodeNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = AirflowNetworkLinkageData(i).NodeNums[1];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                }
                if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).ExtNodeNum > 0) &&
                    (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum == 0) && (AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = AirflowNetworkLinkageData(i).NodeNums[0];
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LT) += std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                    MA((LT - 1) * AirflowNetworkNumOfNodes + LF) = -std::abs(AirflowNetworkLinkSimu(i).FLOW2);
                }
            }
        }

        // Prescribe temperature for EPlus nodes
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            found = false;
            OANode = false;
            for (j = 1; j <= AirflowNetworkNumOfLinks; ++j) {
                if (AirflowNetworkLinkageData(j).NodeNums[0] == i || AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                    CompNum = AirflowNetworkLinkageData(j).CompNum;
                    if (AirflowNetworkCompData(CompNum).EPlusTypeNum == EPlusTypeNum_RHT && (!AirflowNetworkLinkageData(j).VAVTermDamper)) {
                        found = true;
                        break;
                    }
                    // Overwrite fan outlet node
                    if (AirflowNetworkCompData(CompNum).EPlusTypeNum == EPlusTypeNum_FAN && AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                        found = false;
                        break;
                    }
                    // Overwrite return connection outlet
                    if (AirflowNetworkLinkageData(j).ConnectionFlag == EPlusTypeNum_RCN) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                    if (AirflowNetworkLinkageData(j).ConnectionFlag == EPlusTypeNum_SCN &&
                        AirflowNetworkLinkageData(j).NodeNums[1] == i) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                }
                if (AirflowNetworkLinkageData(j).NodeNums[1] == i &&
                    AirflowNetworkNodeData(AirflowNetworkLinkageData(j).NodeNums[0]).EPlusTypeNum == EPlusTypeNum_OAN) {
                    OANode = true;
                    break;
                }
            }
            if (found) continue;
            if (AirflowNetworkNodeData(i).EPlusZoneNum == 0 && AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_ZIN) continue;
            j = AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && (AirflowNetworkNodeData(i).EPlusZoneNum > 0 || AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_FOU ||
                          AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_COU || AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_HXO)) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).GenContam * 1.0e10;
            }
            if (j > 0 && OANode) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).GenContam * 1.0e10;
            }
            if (AirflowNetworkNodeData(i).EPlusZoneNum > 0 && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 0.9e10) {
                ZoneNum = AirflowNetworkNodeData(i).EPlusZoneNum;
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = ANGC(ZoneNum) * 1.0e10;
            }
            if (AirflowNetworkNodeData(i).ExtNodeNum > 0) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = OutdoorGC * 1.0e10;
            }
        }

        // Assign node value to distribution nodes with fan off
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            j = AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && !LoopOnOffFlag(AirflowNetworkNodeData(i).AirLoopNum) && MA((i - 1) * AirflowNetworkNumOfNodes + i) < 1.0e9) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = Node(j).GenContam * 1.0e10;
            }
            if (j == 0 && i > NumOfNodesMultiZone && !LoopOnOffFlag(AirflowNetworkNodeData(i).AirLoopNum)) {
                MA((i - 1) * AirflowNetworkNumOfNodes + i) = 1.0e10;
                MV(i) = AirflowNetworkNodeSimu(i).GCZlast * 1.0e10;
            }
        }

        // Check singularity
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            if (MA((i - 1) * AirflowNetworkNumOfNodes + i) < 1.0e-6) {
                ShowFatalError("CalcAirflowNetworkGCBalance: A diagonal entity is zero in AirflowNetwork matrix at node " +
                               AirflowNetworkNodeData(i).Name);
            }
        }

        // Get an inverse matrix
        MRXINV(AirflowNetworkNumOfNodes);

        // Calculate node temperatures
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            COZN = 0.0;
            for (j = 1; j <= AirflowNetworkNumOfNodes; ++j) {
                COZN += MA((i - 1) * AirflowNetworkNumOfNodes + j) * MV(j);
            }
            AirflowNetworkNodeSimu(i).GCZ = COZN;
        }
    }

    void MRXINV(int const NORDER)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on Subroutine ADSINV

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine inverses a matrix

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int K;
        int M;
        Real64 R1;
        Real64 S;
        //   ############################################## MATRIX INVERSION

        IVEC = 0;
        for (i = 1; i <= NORDER; ++i) {
            IVEC(i + 20) = i;
        }
        for (i = 1; i <= NORDER; ++i) {
            R1 = 0.0;
            M = i;
            for (j = i; j <= NORDER; ++j) {
                if (std::abs(R1) < std::abs(MA((i - 1) * NORDER + j))) {
                    M = j;
                    R1 = MA((i - 1) * NORDER + j);
                }
            }
            if (i != M) {
                K = IVEC(M + 20);
                IVEC(M + 20) = IVEC(i + 20);
                IVEC(i + 20) = K;
                for (j = 1; j <= NORDER; ++j) {
                    S = MA((j - 1) * NORDER + i);
                    MA((j - 1) * NORDER + i) = MA((j - 1) * NORDER + M);
                    MA((j - 1) * NORDER + M) = S;
                }
            }
            MA((i - 1) * NORDER + i) = 1.0;
            for (j = 1; j <= NORDER; ++j) {
                MA((i - 1) * NORDER + j) /= R1;
            }
            for (j = 1; j <= NORDER; ++j) {
                if (i == j) continue;
                R1 = MA((j - 1) * NORDER + i);
                if (std::abs(R1) <= 1.0E-20) continue;
                MA((j - 1) * NORDER + i) = 0.0;
                for (K = 1; K <= NORDER; ++K) {
                    MA((j - 1) * NORDER + K) -= R1 * MA((i - 1) * NORDER + K);
                }
            }
        }
        for (i = 1; i <= NORDER; ++i) {
            if (IVEC(i + 20) == i) continue;
            M = i;
            while (NORDER > M) {
                ++M;
                if (IVEC(M + 20) == i) break;
            }
            IVEC(M + 20) = IVEC(i + 20);
            for (j = 1; j <= NORDER; ++j) {
                R1 = MA((i - 1) * NORDER + j);
                MA((i - 1) * NORDER + j) = MA((M - 1) * NORDER + j);
                MA((M - 1) * NORDER + j) = R1;
            }
            IVEC(i + 20) = i;
        }
        return;
        //   ########################################################### END
    }

    void ReportAirflowNetwork()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   2/1/04
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports outputs of air distribution systems

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHeatBalance::MRT;
        using DataHeatBalance::ZonePreDefRep;
        using DataHeatBalance::ZoneTotalExfiltrationHeatLoss;
        using DataHVACGlobals::NumPrimaryAirSys;
        using DataHVACGlobals::TimeStepSys;
        using DataHVACGlobals::TurnFansOn;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const Lam(2.5e6); // Heat of vaporization (J/kg)

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int n;
        int M;
        int ZN1;
        int ZN2;
        Real64 AirDensity;
        Real64 CpAir;
        Real64 Tamb;
        Real64 hfg; // latent heat of vaporization
        Real64 ReportingConstant;
        Real64 ReportingFraction;
        int AirLoopNum;
        int FanNum;
        Real64 RepOnOffFanRunTimeFraction;
        bool static onetime = false;
        static Array1D<bool> onceZoneFlag;
        static Array1D<bool> onceSurfFlag;

        if (SimulateAirflowNetwork < AirflowNetworkControlMultizone) return;

        if (!onetime) {
            onceZoneFlag.dimension(NumOfZones, false);
            onceSurfFlag.dimension(AirflowNetworkNumOfLinks, false);
            onetime = true;
        }
        ReportingConstant = TimeStepSys * SecInHour;

        ZoneTotalExfiltrationHeatLoss = 0.0;

        for (auto &e : AirflowNetworkReportData) {
            e.MultiZoneInfiSenGainW = 0.0;
            e.MultiZoneInfiSenGainJ = 0.0;
            e.MultiZoneInfiSenLossW = 0.0;
            e.MultiZoneInfiSenLossJ = 0.0;
            e.MultiZoneInfiLatGainW = 0.0;
            e.MultiZoneInfiLatGainJ = 0.0;
            e.MultiZoneInfiLatLossW = 0.0;
            e.MultiZoneInfiLatLossJ = 0.0;
            e.MultiZoneMixSenGainW = 0.0;
            e.MultiZoneMixSenGainJ = 0.0;
            e.MultiZoneMixSenLossW = 0.0;
            e.MultiZoneMixSenLossJ = 0.0;
            e.MultiZoneMixLatGainW = 0.0;
            e.MultiZoneMixLatGainJ = 0.0;
            e.MultiZoneMixLatLossW = 0.0;
            e.MultiZoneMixLatLossJ = 0.0;
            e.LeakSenGainW = 0.0;
            e.LeakSenGainJ = 0.0;
            e.LeakSenLossW = 0.0;
            e.LeakSenLossJ = 0.0;
            e.LeakLatGainW = 0.0;
            e.LeakLatGainJ = 0.0;
            e.LeakLatLossW = 0.0;
            e.LeakLatLossJ = 0.0;
            e.CondSenGainW = 0.0;
            e.CondSenGainJ = 0.0;
            e.CondSenLossW = 0.0;
            e.CondSenLossJ = 0.0;
            e.DiffLatGainW = 0.0;
            e.DiffLatGainJ = 0.0;
            e.DiffLatLossW = 0.0;
            e.DiffLatLossJ = 0.0;
            e.RadGainW = 0.0;
            e.RadGainJ = 0.0;
            e.RadLossW = 0.0;
            e.RadLossJ = 0.0;
            e.TotalSenGainW = 0.0;
            e.TotalSenGainJ = 0.0;
            e.TotalSenLossW = 0.0;
            e.TotalSenLossJ = 0.0;
            e.TotalLatGainW = 0.0;
            e.TotalLatGainJ = 0.0;
            e.TotalLatLossW = 0.0;
            e.TotalLatLossJ = 0.0;
        }

        // Calculate sensible and latent loads in each zone from multizone airflows
        if (SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS ||
            (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS && AirflowNetworkFanActivated)) {
            for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) { // Multizone airflow energy
                n = AirflowNetworkLinkageData(i).NodeNums[0];
                M = AirflowNetworkLinkageData(i).NodeNums[1];
                ZN1 = AirflowNetworkNodeData(n).EPlusZoneNum;
                ZN2 = AirflowNetworkNodeData(M).EPlusZoneNum;
                // Find a linkage from a zone to outdoors
                if (ZN1 > 0 && ZN2 == 0) {
                    if (Surface(MultizoneSurfaceData(i).SurfNum).HasLinkedOutAirNode) {
                        Tamb = Surface(MultizoneSurfaceData(i).SurfNum).OutDryBulbTemp;
                        CpAir =
                            PsyCpAirFnW(Psychrometrics::PsyWFnTdbTwbPb(Tamb, Surface(MultizoneSurfaceData(i).SurfNum).OutWetBulbTemp, OutBaroPress));
                    } else {
                        Tamb = Zone(ZN1).OutDryBulbTemp;
                        CpAir = PsyCpAirFnW(OutHumRat);
                    }
                    hfg = Psychrometrics::PsyHfgAirFnWTdb(ZoneAirHumRat(ZN1), (MAT(ZN1) + Tamb) / 2.0);

                    if (Tamb > MAT(ZN1)) {
                        AirflowNetworkReportData(ZN1).MultiZoneInfiSenGainW += (AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (Tamb - MAT(ZN1)));
                        AirflowNetworkReportData(ZN1).MultiZoneInfiSenGainJ +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (Tamb - MAT(ZN1))) * ReportingConstant;
                    } else {
                        AirflowNetworkReportData(ZN1).MultiZoneInfiSenLossW += (AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (MAT(ZN1) - Tamb));
                        AirflowNetworkReportData(ZN1).MultiZoneInfiSenLossJ +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (MAT(ZN1) - Tamb)) * ReportingConstant;
                    }
                    if (OutHumRat > ZoneAirHumRat(ZN1)) {
                        AirflowNetworkReportData(ZN1).MultiZoneInfiLatGainW +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * (OutHumRat - ZoneAirHumRat(ZN1))) * hfg;
                        AirflowNetworkReportData(ZN1).MultiZoneInfiLatGainJ +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * (OutHumRat - ZoneAirHumRat(ZN1))) * hfg * ReportingConstant;
                    } else {
                        AirflowNetworkReportData(ZN1).MultiZoneInfiLatLossW +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * (ZoneAirHumRat(ZN1) - OutHumRat)) * hfg;
                        AirflowNetworkReportData(ZN1).MultiZoneInfiLatLossJ +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * (ZoneAirHumRat(ZN1) - OutHumRat)) * hfg * ReportingConstant;
                    }
                }
                if (ZN1 == 0 && ZN2 > 0) {
                    if (Surface(MultizoneSurfaceData(i).SurfNum).HasLinkedOutAirNode) {
                        Tamb = Surface(MultizoneSurfaceData(i).SurfNum).OutDryBulbTemp;
                        CpAir =
                            PsyCpAirFnW(Psychrometrics::PsyWFnTdbTwbPb(Tamb, Surface(MultizoneSurfaceData(i).SurfNum).OutWetBulbTemp, OutBaroPress));
                    } else {
                        Tamb = Zone(ZN2).OutDryBulbTemp;
                        CpAir = PsyCpAirFnW(OutHumRat);
                    }
                    hfg = Psychrometrics::PsyHfgAirFnWTdb(ZoneAirHumRat(ZN2), (MAT(ZN2) + Tamb) / 2.0);

                    if (Tamb > MAT(ZN2)) {
                        AirflowNetworkReportData(ZN2).MultiZoneInfiSenGainW += (AirflowNetworkLinkSimu(i).FLOW * CpAir * (Tamb - MAT(ZN2)));
                        AirflowNetworkReportData(ZN2).MultiZoneInfiSenGainJ +=
                            (AirflowNetworkLinkSimu(i).FLOW * CpAir * (Tamb - MAT(ZN2))) * ReportingConstant;
                    } else {
                        AirflowNetworkReportData(ZN2).MultiZoneInfiSenLossW += (AirflowNetworkLinkSimu(i).FLOW * CpAir * (MAT(ZN2) - Tamb));
                        AirflowNetworkReportData(ZN2).MultiZoneInfiSenLossJ +=
                            (AirflowNetworkLinkSimu(i).FLOW * CpAir * (MAT(ZN2) - Tamb)) * ReportingConstant;
                    }
                    if (OutHumRat > ZoneAirHumRat(ZN2)) {
                        AirflowNetworkReportData(ZN2).MultiZoneInfiLatGainW +=
                            (AirflowNetworkLinkSimu(i).FLOW * (OutHumRat - ZoneAirHumRat(ZN2))) * hfg;
                        AirflowNetworkReportData(ZN2).MultiZoneInfiLatGainJ +=
                            (AirflowNetworkLinkSimu(i).FLOW * (OutHumRat - ZoneAirHumRat(ZN2))) * hfg * ReportingConstant;
                    } else {
                        AirflowNetworkReportData(ZN2).MultiZoneInfiLatLossW +=
                            (AirflowNetworkLinkSimu(i).FLOW * (ZoneAirHumRat(ZN2) - OutHumRat)) * hfg;
                        AirflowNetworkReportData(ZN2).MultiZoneInfiLatLossJ +=
                            (AirflowNetworkLinkSimu(i).FLOW * (ZoneAirHumRat(ZN2) - OutHumRat)) * hfg * ReportingConstant;
                    }
                }

                if (ZN1 > 0 && ZN2 > 0) {
                    CpAir = PsyCpAirFnW(ZoneAirHumRat(ZN1));
                    hfg = Psychrometrics::PsyHfgAirFnWTdb((ZoneAirHumRat(ZN1) + ZoneAirHumRat(ZN2)) / 2.0, (MAT(ZN1) + MAT(ZN2)) / 2.0);
                    if (MAT(ZN1) > MAT(ZN2)) {
                        AirflowNetworkReportData(ZN2).MultiZoneMixSenGainW += (AirflowNetworkLinkSimu(i).FLOW * CpAir * (MAT(ZN1) - MAT(ZN2)));
                        AirflowNetworkReportData(ZN2).MultiZoneMixSenGainJ +=
                            (AirflowNetworkLinkSimu(i).FLOW * CpAir * (MAT(ZN1) - MAT(ZN2))) * ReportingConstant;
                    } else {
                        AirflowNetworkReportData(ZN2).MultiZoneMixSenLossW += (AirflowNetworkLinkSimu(i).FLOW * CpAir * (MAT(ZN2) - MAT(ZN1)));
                        AirflowNetworkReportData(ZN2).MultiZoneMixSenLossJ +=
                            (AirflowNetworkLinkSimu(i).FLOW * CpAir * (MAT(ZN2) - MAT(ZN1))) * ReportingConstant;
                    }
                    if (ZoneAirHumRat(ZN1) > ZoneAirHumRat(ZN2)) {
                        AirflowNetworkReportData(ZN2).MultiZoneMixLatGainW +=
                            (AirflowNetworkLinkSimu(i).FLOW * (ZoneAirHumRat(ZN1) - ZoneAirHumRat(ZN2))) * hfg;
                        AirflowNetworkReportData(ZN2).MultiZoneMixLatGainJ +=
                            (AirflowNetworkLinkSimu(i).FLOW * (ZoneAirHumRat(ZN1) - ZoneAirHumRat(ZN2))) * hfg * ReportingConstant;
                    } else {
                        AirflowNetworkReportData(ZN2).MultiZoneMixLatLossW +=
                            (AirflowNetworkLinkSimu(i).FLOW * (ZoneAirHumRat(ZN2) - ZoneAirHumRat(ZN1))) * hfg;
                        AirflowNetworkReportData(ZN2).MultiZoneMixLatLossJ +=
                            (AirflowNetworkLinkSimu(i).FLOW * (ZoneAirHumRat(ZN2) - ZoneAirHumRat(ZN1))) * hfg * ReportingConstant;
                    }
                    CpAir = PsyCpAirFnW(ZoneAirHumRat(ZN2));
                    if (MAT(ZN2) > MAT(ZN1)) {
                        AirflowNetworkReportData(ZN1).MultiZoneMixSenGainW += (AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (MAT(ZN2) - MAT(ZN1)));
                        AirflowNetworkReportData(ZN1).MultiZoneMixSenGainJ +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (MAT(ZN2) - MAT(ZN1))) * ReportingConstant;
                    } else {
                        AirflowNetworkReportData(ZN1).MultiZoneMixSenLossW += (AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (MAT(ZN1) - MAT(ZN2)));
                        AirflowNetworkReportData(ZN1).MultiZoneMixSenLossJ +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (MAT(ZN1) - MAT(ZN2))) * ReportingConstant;
                    }
                    if (ZoneAirHumRat(ZN2) > ZoneAirHumRat(ZN1)) {
                        AirflowNetworkReportData(ZN1).MultiZoneMixLatGainW +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * (ZoneAirHumRat(ZN2) - ZoneAirHumRat(ZN1))) * hfg;
                        AirflowNetworkReportData(ZN1).MultiZoneMixLatGainJ +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * (ZoneAirHumRat(ZN2) - ZoneAirHumRat(ZN1))) * hfg * ReportingConstant;
                    } else {
                        AirflowNetworkReportData(ZN1).MultiZoneMixLatLossW +=
                            std::abs(AirflowNetworkLinkSimu(i).FLOW2 * (ZoneAirHumRat(ZN1) - ZoneAirHumRat(ZN2))) * hfg;
                        AirflowNetworkReportData(ZN1).MultiZoneMixLatLossJ +=
                            (AirflowNetworkLinkSimu(i).FLOW2 * (ZoneAirHumRat(ZN1) - ZoneAirHumRat(ZN2))) * hfg * ReportingConstant;
                    }
                }
            }
        }

        // Assign data for report
        if (SimulateAirflowNetwork > AirflowNetworkControlMultizone) {
            for (i = 1; i <= NumOfZones; ++i) {
                if (AirflowNetworkExchangeData(i).LeakSen > 0.0) {
                    AirflowNetworkReportData(i).LeakSenGainW = AirflowNetworkExchangeData(i).LeakSen;
                    AirflowNetworkReportData(i).LeakSenGainJ = AirflowNetworkExchangeData(i).LeakSen * ReportingConstant;
                } else {
                    AirflowNetworkReportData(i).LeakSenLossW = -AirflowNetworkExchangeData(i).LeakSen;
                    AirflowNetworkReportData(i).LeakSenLossJ = -AirflowNetworkExchangeData(i).LeakSen * ReportingConstant;
                }
                if (AirflowNetworkExchangeData(i).LeakLat > 0.0) {
                    AirflowNetworkReportData(i).LeakLatGainW = AirflowNetworkExchangeData(i).LeakLat * Lam;
                    AirflowNetworkReportData(i).LeakLatGainJ = AirflowNetworkExchangeData(i).LeakLat * Lam * ReportingConstant;
                } else {
                    AirflowNetworkReportData(i).LeakLatLossW = -AirflowNetworkExchangeData(i).LeakLat * Lam;
                    AirflowNetworkReportData(i).LeakLatLossJ = -AirflowNetworkExchangeData(i).LeakLat * Lam * ReportingConstant;
                }
                if (AirflowNetworkExchangeData(i).CondSen > 0.0) {
                    AirflowNetworkReportData(i).CondSenGainW = AirflowNetworkExchangeData(i).CondSen;
                    AirflowNetworkReportData(i).CondSenGainJ = AirflowNetworkExchangeData(i).CondSen * ReportingConstant;
                } else {
                    AirflowNetworkReportData(i).CondSenLossW = -AirflowNetworkExchangeData(i).CondSen;
                    AirflowNetworkReportData(i).CondSenLossJ = -AirflowNetworkExchangeData(i).CondSen * ReportingConstant;
                }
                if (AirflowNetworkExchangeData(i).DiffLat > 0.0) {
                    AirflowNetworkReportData(i).DiffLatGainW = AirflowNetworkExchangeData(i).DiffLat * Lam;
                    AirflowNetworkReportData(i).DiffLatGainJ = AirflowNetworkExchangeData(i).DiffLat * Lam * ReportingConstant;
                } else {
                    AirflowNetworkReportData(i).DiffLatLossW = -AirflowNetworkExchangeData(i).DiffLat * Lam;
                    AirflowNetworkReportData(i).DiffLatLossJ = -AirflowNetworkExchangeData(i).DiffLat * Lam * ReportingConstant;
                }
                if (AirflowNetworkExchangeData(i).RadGain < 0.0) {
                    AirflowNetworkReportData(i).RadGainW = -AirflowNetworkExchangeData(i).RadGain;
                    AirflowNetworkReportData(i).RadGainJ = -AirflowNetworkExchangeData(i).RadGain * ReportingConstant;
                } else {
                    AirflowNetworkReportData(i).RadLossW = AirflowNetworkExchangeData(i).RadGain;
                    AirflowNetworkReportData(i).RadLossJ = AirflowNetworkExchangeData(i).RadGain * ReportingConstant;
                }
                if (AirflowNetworkExchangeData(i).TotalSen > 0.0) {
                    AirflowNetworkReportData(i).TotalSenGainW = AirflowNetworkExchangeData(i).TotalSen;
                    AirflowNetworkReportData(i).TotalSenGainJ = AirflowNetworkExchangeData(i).TotalSen * ReportingConstant;
                } else {
                    AirflowNetworkReportData(i).TotalSenLossW = -AirflowNetworkExchangeData(i).TotalSen;
                    AirflowNetworkReportData(i).TotalSenLossJ = -AirflowNetworkExchangeData(i).TotalSen * ReportingConstant;
                }
                if (AirflowNetworkExchangeData(i).TotalLat > 0.0) {
                    AirflowNetworkReportData(i).TotalLatGainW = AirflowNetworkExchangeData(i).TotalLat * Lam;
                    AirflowNetworkReportData(i).TotalLatGainJ = AirflowNetworkExchangeData(i).TotalLat * Lam * ReportingConstant;
                } else {
                    AirflowNetworkReportData(i).TotalLatLossW = -AirflowNetworkExchangeData(i).TotalLat * Lam;
                    AirflowNetworkReportData(i).TotalLatLossJ = -AirflowNetworkExchangeData(i).TotalLat * Lam * ReportingConstant;
                }
            }
        }

        // Zone report

        for (auto &e : AirflowNetworkZnRpt) {
            e.InfilVolume = 0.0;
            e.InfilMass = 0.0;
            e.InfilAirChangeRate = 0.0;
            e.MixVolume = 0.0;
            e.MixMass = 0.0;
        }

        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            if (DisSysNumOfCVFs == 0) continue;
            for (FanNum = 1; FanNum <= DisSysNumOfCVFs; ++FanNum) {
                if (DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) break;
            }
            if (DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff && LoopOnOffFanRunTimeFraction(AirLoopNum) < 1.0 &&
                LoopOnOffFanRunTimeFraction(AirLoopNum) > 0.0) {
                // ON Cycle calculation
                onceZoneFlag = false;
                for (i = 1; i <= NumOfZones; ++i) {
                    if (AirflowNetworkNodeData(i).AirLoopNum > 0 && AirflowNetworkNodeData(i).AirLoopNum != AirLoopNum) continue;
                    if (AirflowNetworkNodeData(i).AirLoopNum == AirLoopNum) {
                        RepOnOffFanRunTimeFraction = LoopOnOffFanRunTimeFraction(AirLoopNum);
                    }
                    if (AirflowNetworkNodeData(i).AirLoopNum == 0) {
                        RepOnOffFanRunTimeFraction = MaxOnOffFanRunTimeFraction;
                    }
                    if (AirflowNetworkNodeData(i).AirLoopNum == 0 && onceZoneFlag(i)) continue;
                    AirflowNetworkReportData(i).MultiZoneInfiSenGainW *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneInfiSenGainJ *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneInfiSenLossW *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneInfiSenLossJ *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneInfiLatGainW *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneInfiLatGainJ *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneInfiLatLossW *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneInfiLatLossJ *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneMixSenGainW *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneMixSenGainJ *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneMixSenLossW *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneMixSenLossJ *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneMixLatGainW *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneMixLatGainJ *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneMixLatLossW *= RepOnOffFanRunTimeFraction;
                    AirflowNetworkReportData(i).MultiZoneMixLatLossJ *= RepOnOffFanRunTimeFraction;
                    if (AirflowNetworkNodeData(i).AirLoopNum == 0) {
                        onceZoneFlag(i) = true;
                    }
                }
                // Off Cycle addon
                onceSurfFlag = false;
                for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) { // Multizone airflow energy
                    n = AirflowNetworkLinkageData(i).NodeNums[0];
                    M = AirflowNetworkLinkageData(i).NodeNums[1];
                    ZN1 = AirflowNetworkNodeData(n).EPlusZoneNum;
                    ZN2 = AirflowNetworkNodeData(M).EPlusZoneNum;
                    //					ReportingFraction = ( 1.0 - OnOffFanRunTimeFraction );
                    // Find a linkage from a zone to outdoors
                    if (ZN1 > 0 && ZN2 == 0) {
                        if (AirflowNetworkNodeData(n).AirLoopNum > 0 && AirflowNetworkNodeData(n).AirLoopNum != AirLoopNum) continue;
                        if (AirflowNetworkNodeData(n).AirLoopNum == AirLoopNum) {
                            RepOnOffFanRunTimeFraction = LoopOnOffFanRunTimeFraction(AirLoopNum);
                        }
                        if (AirflowNetworkNodeData(n).AirLoopNum == 0) {
                            RepOnOffFanRunTimeFraction = MaxOnOffFanRunTimeFraction;
                        }
                        if (AirflowNetworkNodeData(n).AirLoopNum == 0 && onceSurfFlag(i)) continue;
                        ReportingFraction = (1.0 - RepOnOffFanRunTimeFraction);
                        Tamb = Zone(ZN1).OutDryBulbTemp;
                        CpAir = PsyCpAirFnW(OutHumRat);
                        if (Tamb > MAT(ZN1)) {
                            AirflowNetworkReportData(ZN1).MultiZoneInfiSenGainW +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * CpAir * (Tamb - MAT(ZN1))) * (1.0 - RepOnOffFanRunTimeFraction);
                            AirflowNetworkReportData(ZN1).MultiZoneInfiSenGainJ +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * CpAir * (Tamb - MAT(ZN1))) * ReportingConstant * ReportingFraction;
                        } else {
                            AirflowNetworkReportData(ZN1).MultiZoneInfiSenLossW +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * CpAir * (MAT(ZN1) - Tamb)) * (1.0 - RepOnOffFanRunTimeFraction);
                            AirflowNetworkReportData(ZN1).MultiZoneInfiSenLossJ +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * CpAir * (MAT(ZN1) - Tamb)) * ReportingConstant * ReportingFraction;
                        }
                        if (OutHumRat > ZoneAirHumRat(ZN1)) {
                            AirflowNetworkReportData(ZN1).MultiZoneInfiLatGainW +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * (OutHumRat - ZoneAirHumRat(ZN1))) * ReportingFraction;
                            AirflowNetworkReportData(ZN1).MultiZoneInfiLatGainJ +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * (OutHumRat - ZoneAirHumRat(ZN1))) * ReportingConstant * ReportingFraction;
                        } else {
                            AirflowNetworkReportData(ZN1).MultiZoneInfiLatLossW +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * (ZoneAirHumRat(ZN1) - OutHumRat)) * ReportingFraction;
                            AirflowNetworkReportData(ZN1).MultiZoneInfiLatLossJ +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * (ZoneAirHumRat(ZN1) - OutHumRat)) * ReportingConstant * ReportingFraction;
                        }
                        if (AirflowNetworkNodeData(n).AirLoopNum == 0) {
                            onceSurfFlag(i) = true;
                        }
                    }
                    if (ZN1 == 0 && ZN2 > 0) {
                        if (AirflowNetworkNodeData(M).AirLoopNum > 0 && AirflowNetworkNodeData(M).AirLoopNum != AirLoopNum) continue;
                        if (AirflowNetworkNodeData(M).AirLoopNum == AirLoopNum) {
                            RepOnOffFanRunTimeFraction = LoopOnOffFanRunTimeFraction(AirLoopNum);
                        }
                        if (AirflowNetworkNodeData(M).AirLoopNum == 0) {
                            RepOnOffFanRunTimeFraction = MaxOnOffFanRunTimeFraction;
                        }
                        if (AirflowNetworkNodeData(M).AirLoopNum == 0 && onceSurfFlag(i)) continue;
                        ReportingFraction = (1.0 - RepOnOffFanRunTimeFraction);
                        Tamb = Zone(ZN2).OutDryBulbTemp;
                        CpAir = PsyCpAirFnW(OutHumRat);
                        if (Tamb > MAT(ZN2)) {
                            AirflowNetworkReportData(ZN2).MultiZoneInfiSenGainW +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * CpAir * (Tamb - MAT(ZN2))) * ReportingFraction;
                            AirflowNetworkReportData(ZN2).MultiZoneInfiSenGainJ +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * CpAir * (Tamb - MAT(ZN2))) * ReportingConstant * ReportingFraction;
                        } else {
                            AirflowNetworkReportData(ZN2).MultiZoneInfiSenLossW +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * CpAir * (MAT(ZN2) - Tamb)) * ReportingFraction;
                            AirflowNetworkReportData(ZN2).MultiZoneInfiSenLossJ +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * CpAir * (MAT(ZN2) - Tamb)) * ReportingConstant * ReportingFraction;
                        }
                        if (OutHumRat > ZoneAirHumRat(ZN2)) {
                            AirflowNetworkReportData(ZN2).MultiZoneInfiLatGainW +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * (OutHumRat - ZoneAirHumRat(ZN2))) * ReportingFraction;
                            AirflowNetworkReportData(ZN2).MultiZoneInfiLatGainJ +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * (OutHumRat - ZoneAirHumRat(ZN2))) * ReportingConstant * ReportingFraction;
                        } else {
                            AirflowNetworkReportData(ZN2).MultiZoneInfiLatLossW +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * (ZoneAirHumRat(ZN2) - OutHumRat)) * ReportingFraction;
                            AirflowNetworkReportData(ZN2).MultiZoneInfiLatLossJ +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * (ZoneAirHumRat(ZN2) - OutHumRat)) * ReportingConstant * ReportingFraction;
                        }
                        if (AirflowNetworkNodeData(M).AirLoopNum == 0) {
                            onceSurfFlag(i) = true;
                        }
                    }

                    if (ZN1 > 0 && ZN2 > 0) {
                        ReportingFraction = (1.0 - MaxOnOffFanRunTimeFraction);
                        CpAir = PsyCpAirFnW(ZoneAirHumRat(ZN1));
                        if (MAT(ZN1) > MAT(ZN2)) {
                            AirflowNetworkReportData(ZN2).MultiZoneMixSenGainW +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * CpAir * (MAT(ZN1) - MAT(ZN2))) * ReportingFraction;
                            AirflowNetworkReportData(ZN2).MultiZoneMixSenGainJ +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * CpAir * (MAT(ZN1) - MAT(ZN2))) * ReportingConstant * ReportingFraction;
                        } else {
                            AirflowNetworkReportData(ZN2).MultiZoneMixSenLossW +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * CpAir * (MAT(ZN2) - MAT(ZN1))) * ReportingFraction;
                            AirflowNetworkReportData(ZN2).MultiZoneMixSenLossJ +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * CpAir * (MAT(ZN2) - MAT(ZN1))) * ReportingConstant * ReportingFraction;
                        }
                        if (ZoneAirHumRat(ZN1) > ZoneAirHumRat(ZN2)) {
                            AirflowNetworkReportData(ZN2).MultiZoneMixLatGainW +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * (ZoneAirHumRat(ZN1) - ZoneAirHumRat(ZN2))) * ReportingFraction;
                            AirflowNetworkReportData(ZN2).MultiZoneMixLatGainJ +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * (ZoneAirHumRat(ZN1) - ZoneAirHumRat(ZN2))) * ReportingConstant *
                                ReportingFraction;
                        } else {
                            AirflowNetworkReportData(ZN2).MultiZoneMixLatLossW +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * (ZoneAirHumRat(ZN2) - ZoneAirHumRat(ZN1))) * ReportingFraction;
                            AirflowNetworkReportData(ZN2).MultiZoneMixLatLossJ +=
                                (AirflowNetworkLinkReport1(i).FLOWOFF * (ZoneAirHumRat(ZN2) - ZoneAirHumRat(ZN1))) * ReportingConstant *
                                ReportingFraction;
                        }
                        CpAir = PsyCpAirFnW(ZoneAirHumRat(ZN2));
                        if (MAT(ZN2) > MAT(ZN1)) {
                            AirflowNetworkReportData(ZN1).MultiZoneMixSenGainW +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * CpAir * (MAT(ZN2) - MAT(ZN1))) * ReportingFraction;
                            AirflowNetworkReportData(ZN1).MultiZoneMixSenGainJ +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * CpAir * (MAT(ZN2) - MAT(ZN1))) * ReportingConstant * ReportingFraction;
                        } else {
                            AirflowNetworkReportData(ZN1).MultiZoneMixSenLossW +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * CpAir * (MAT(ZN1) - MAT(ZN2))) * ReportingFraction;
                            AirflowNetworkReportData(ZN1).MultiZoneMixSenLossJ +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * CpAir * (MAT(ZN1) - MAT(ZN2))) * ReportingConstant * ReportingFraction;
                        }

                        if (ZoneAirHumRat(ZN2) > ZoneAirHumRat(ZN1)) {
                            AirflowNetworkReportData(ZN1).MultiZoneMixLatGainW +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * (ZoneAirHumRat(ZN2) - ZoneAirHumRat(ZN1))) * ReportingFraction;
                            AirflowNetworkReportData(ZN1).MultiZoneMixLatGainJ +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * (ZoneAirHumRat(ZN2) - ZoneAirHumRat(ZN1))) * ReportingConstant *
                                ReportingFraction;
                        } else {
                            AirflowNetworkReportData(ZN1).MultiZoneMixLatLossW +=
                                std::abs(AirflowNetworkLinkReport1(i).FLOW2OFF * (ZoneAirHumRat(ZN1) - ZoneAirHumRat(ZN2))) * ReportingFraction;
                            AirflowNetworkReportData(ZN1).MultiZoneMixLatLossJ +=
                                (AirflowNetworkLinkReport1(i).FLOW2OFF * (ZoneAirHumRat(ZN1) - ZoneAirHumRat(ZN2))) * ReportingConstant *
                                ReportingFraction;
                        }
                    }
                }
            }
        }

        if (!(SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS)) return;

        for (i = 1; i <= NumOfZones; ++i) { // Start of zone loads report variable update loop ...
            Tamb = Zone(i).OutDryBulbTemp;
            CpAir = PsyCpAirFnW(ZoneAirHumRatAvg(i));
            AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, MAT(i), ZoneAirHumRatAvg(i));

            AirflowNetworkZnRpt(i).InfilVolume = (AirflowNetworkExchangeData(i).SumMCp / CpAir / AirDensity) * ReportingConstant;
            AirflowNetworkZnRpt(i).InfilAirChangeRate = AirflowNetworkZnRpt(i).InfilVolume / (TimeStepSys * Zone(i).Volume);
            AirflowNetworkZnRpt(i).InfilMass = (AirflowNetworkExchangeData(i).SumMCp / CpAir) * ReportingConstant;
            AirflowNetworkZnRpt(i).MixVolume = (AirflowNetworkExchangeData(i).SumMMCp / CpAir / AirDensity) * ReportingConstant;
            AirflowNetworkZnRpt(i).MixMass = (AirflowNetworkExchangeData(i).SumMMCp / CpAir) * ReportingConstant;
            // save values for predefined report
            if (ZonePreDefRep(i).isOccupied) {
                ZonePreDefRep(i).AFNInfilVolTotal += AirflowNetworkZnRpt(i).InfilVolume * Zone(i).Multiplier * Zone(i).ListMultiplier;
                if (AirflowNetworkZnRpt(i).InfilVolume < ZonePreDefRep(i).AFNInfilVolMin) {
                    ZonePreDefRep(i).AFNInfilVolMin = AirflowNetworkZnRpt(i).InfilVolume * Zone(i).Multiplier * Zone(i).ListMultiplier;
                }
            }

            Real64 H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(OutHumRat, Zone(i).OutDryBulbTemp);
            AirflowNetworkZnRpt(i).InletMass = 0;
            AirflowNetworkZnRpt(i).OutletMass = 0;
            if (ZoneEquipConfig(i).IsControlled) {
                for (int j = 1; j <= ZoneEquipConfig(i).NumInletNodes; ++j) {
                    AirflowNetworkZnRpt(i).InletMass += Node(ZoneEquipConfig(i).InletNode(j)).MassFlowRate * ReportingConstant;
                }
                for (int j = 1; j <= ZoneEquipConfig(i).NumExhaustNodes; ++j) {
                    AirflowNetworkZnRpt(i).OutletMass += Node(ZoneEquipConfig(i).ExhaustNode(j)).MassFlowRate * ReportingConstant;
                }
                for (int j = 1; j <= ZoneEquipConfig(i).NumReturnNodes; ++j) {
                    AirflowNetworkZnRpt(i).OutletMass += Node(ZoneEquipConfig(i).ReturnNode(j)).MassFlowRate * ReportingConstant;
                }
            }
            AirflowNetworkZnRpt(i).ExfilMass = AirflowNetworkZnRpt(i).InfilMass + AirflowNetworkZnRpt(i).VentilMass + AirflowNetworkZnRpt(i).MixMass +
                                               AirflowNetworkZnRpt(i).InletMass - AirflowNetworkZnRpt(i).OutletMass;
            AirflowNetworkZnRpt(i).ExfilSensiLoss = AirflowNetworkZnRpt(i).ExfilMass / ReportingConstant * (MAT(i) - Tamb) * CpAir;
            AirflowNetworkZnRpt(i).ExfilLatentLoss =
                AirflowNetworkZnRpt(i).ExfilMass / ReportingConstant * (ZoneAirHumRat(i) - OutHumRat) * H2OHtOfVap;
            AirflowNetworkZnRpt(i).ExfilTotalLoss = AirflowNetworkZnRpt(i).ExfilSensiLoss + AirflowNetworkZnRpt(i).ExfilLatentLoss;

            ZoneTotalExfiltrationHeatLoss += AirflowNetworkZnRpt(i).ExfilTotalLoss * ReportingConstant;
        } // ... end of zone loads report variable update loop.

        // Rewrite AirflowNetwork airflow rate
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            if (DisSysNumOfCVFs == 0) continue;
            for (FanNum = 1; FanNum <= DisSysNumOfCVFs; ++FanNum) {
                if (DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) break;
            }
            onceSurfFlag = false;

            for (i = 1; i <= NumOfLinksMultiZone; ++i) {
                if (onceSurfFlag(i)) continue;
                if (DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) {
                    Tamb = OutDryBulbTempAt(AirflowNetworkLinkageData(i).NodeHeights[0]);
                    AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, Tamb, OutHumRat);
                    if (DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff && LoopOnOffFanRunTimeFraction(AirLoopNum) < 1.0 &&
                        LoopOnOffFanRunTimeFraction(AirLoopNum) > 0.0) {
                        AirflowNetworkLinkReport(i).VolFLOW = AirflowNetworkLinkReport1(i).FLOW / AirDensity;
                        AirflowNetworkLinkReport(i).VolFLOW2 = AirflowNetworkLinkReport1(i).FLOW2 / AirDensity;
                    } else {
                        AirflowNetworkLinkReport(i).VolFLOW = AirflowNetworkLinkReport(i).FLOW / AirDensity;
                        AirflowNetworkLinkReport(i).VolFLOW2 = AirflowNetworkLinkReport(i).FLOW2 / AirDensity;
                    }
                    onceSurfFlag(i) = true;
                }
            }

            if (AirflowNetworkNumOfLinks > NumOfLinksMultiZone) {
                for (i = NumOfLinksMultiZone + 1; i <= AirflowNetworkNumOfLinks; ++i) {
                    if (onceSurfFlag(i)) continue;
                    if (DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) {
                        n = AirflowNetworkLinkageData(i).NodeNums[0];
                        M = AirflowNetworkLinkageData(i).NodeNums[1];
                        AirDensity = PsyRhoAirFnPbTdbW((AirflowNetworkNodeSimu(n).PZ + AirflowNetworkNodeSimu(M).PZ) / 2.0 + OutBaroPress,
                                                       (AirflowNetworkNodeSimu(n).TZ + AirflowNetworkNodeSimu(M).TZ) / 2.0,
                                                       (AirflowNetworkNodeSimu(n).WZ + AirflowNetworkNodeSimu(M).WZ) / 2.0);
                        if (DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff && LoopOnOffFanRunTimeFraction(AirLoopNum) < 1.0 &&
                            LoopOnOffFanRunTimeFraction(AirLoopNum) > 0.0) {
                            AirflowNetworkLinkReport(i).VolFLOW =
                                AirflowNetworkLinkReport(i).FLOW / AirDensity * (1.0 - LoopOnOffFanRunTimeFraction(AirLoopNum));
                            AirflowNetworkLinkReport(i).VolFLOW2 =
                                AirflowNetworkLinkReport(i).FLOW2 / AirDensity * (1.0 - LoopOnOffFanRunTimeFraction(AirLoopNum));
                            onceSurfFlag(i) = true;
                        } else {
                            AirflowNetworkLinkReport(i).VolFLOW = AirflowNetworkLinkReport(i).FLOW / AirDensity;
                            AirflowNetworkLinkReport(i).VolFLOW2 = AirflowNetworkLinkReport(i).FLOW2 / AirDensity;
                        }
                    }
                }
            }
        }
    }

    void UpdateAirflowNetwork(Optional_bool_const FirstHVACIteration) // True when solution technique on first iteration
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   12/10/05
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine update variables used in the AirflowNetwork model.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataAirLoop::AirLoopAFNInfo;
        using DataHVACGlobals::NumPrimaryAirSys;
        using DataHVACGlobals::TimeStepSys;
        using DataHVACGlobals::TurnFansOn;
        using DataHVACGlobals::VerySmallMassFlow;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int n;
        int M;
        int ZN1;
        int ZN2;
        int Node1;
        int Node2;
        int Node3;
        Real64 CpAir;
        Real64 Qsen;
        Real64 Qlat;
        Real64 AirDensity;
        Real64 Tamb;
        Real64 PartLoadRatio;
        Real64 OnOffRatio;
        Real64 NodeMass;
        Real64 AFNMass;
        bool WriteFlag;
        static bool MyOneTimeFlag(true);
        static bool MyOneTimeFlag1(true);

        for (auto &e : AirflowNetworkExchangeData) {
            e.SumMCp = 0.0;
            e.SumMCpT = 0.0;
            e.SumMHr = 0.0;
            e.SumMHrW = 0.0;
            e.SumMMCp = 0.0;
            e.SumMMCpT = 0.0;
            e.SumMMHr = 0.0;
            e.SumMMHrW = 0.0;
        }
        if (Contaminant.CO2Simulation) {
            for (auto &e : AirflowNetworkExchangeData) {
                e.SumMHrCO = 0.0;
                e.SumMMHrCO = 0.0;
            }
        }
        if (Contaminant.GenericContamSimulation) {
            for (auto &e : AirflowNetworkExchangeData) {
                e.SumMHrGC = 0.0;
                e.SumMMHrGC = 0.0;
            }
        }

        // Calculate sensible and latent loads in each zone from multizone airflows
        if (SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS ||
            (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS && AirflowNetworkFanActivated)) {
            for (i = 1; i <= NumOfLinksMultiZone; ++i) { // Multizone airflow energy
                n = AirflowNetworkLinkageData(i).NodeNums[0];
                M = AirflowNetworkLinkageData(i).NodeNums[1];
                ZN1 = AirflowNetworkNodeData(n).EPlusZoneNum;
                ZN2 = AirflowNetworkNodeData(M).EPlusZoneNum;
                if (ZN1 > 0 && ZN2 == 0) {
                    // Find a linkage from outdoors to this zone
                    Tamb = Zone(ZN1).OutDryBulbTemp;
                    CpAir = PsyCpAirFnW(OutHumRat);
                    AirflowNetworkExchangeData(ZN1).SumMCp += AirflowNetworkLinkSimu(i).FLOW2 * CpAir;
                    AirflowNetworkExchangeData(ZN1).SumMCpT += AirflowNetworkLinkSimu(i).FLOW2 * CpAir * Tamb;
                    AirflowNetworkExchangeData(ZN1).SumMHr += AirflowNetworkLinkSimu(i).FLOW2;
                    AirflowNetworkExchangeData(ZN1).SumMHrW += AirflowNetworkLinkSimu(i).FLOW2 * OutHumRat;
                    if (Contaminant.CO2Simulation) {
                        AirflowNetworkExchangeData(ZN1).SumMHrCO += AirflowNetworkLinkSimu(i).FLOW2 * OutdoorCO2;
                    }
                    if (Contaminant.GenericContamSimulation) {
                        AirflowNetworkExchangeData(ZN1).SumMHrGC += AirflowNetworkLinkSimu(i).FLOW2 * OutdoorGC;
                    }
                }
                if (ZN1 == 0 && ZN2 > 0) {
                    // Find a linkage from outdoors to this zone
                    Tamb = Zone(ZN2).OutDryBulbTemp;
                    CpAir = PsyCpAirFnW(OutHumRat);
                    AirflowNetworkExchangeData(ZN2).SumMCp += AirflowNetworkLinkSimu(i).FLOW * CpAir;
                    AirflowNetworkExchangeData(ZN2).SumMCpT += AirflowNetworkLinkSimu(i).FLOW * CpAir * Tamb;
                    AirflowNetworkExchangeData(ZN2).SumMHr += AirflowNetworkLinkSimu(i).FLOW;
                    AirflowNetworkExchangeData(ZN2).SumMHrW += AirflowNetworkLinkSimu(i).FLOW * OutHumRat;
                    if (Contaminant.CO2Simulation) {
                        AirflowNetworkExchangeData(ZN2).SumMHrCO += AirflowNetworkLinkSimu(i).FLOW * OutdoorCO2;
                    }
                    if (Contaminant.GenericContamSimulation) {
                        AirflowNetworkExchangeData(ZN2).SumMHrGC += AirflowNetworkLinkSimu(i).FLOW * OutdoorGC;
                    }
                }
                if (ZN1 > 0 && ZN2 > 0) {
                    // Find a linkage from outdoors to this zone
                    CpAir = PsyCpAirFnW(ANZW(ZN1));
                    AirflowNetworkExchangeData(ZN2).SumMMCp += AirflowNetworkLinkSimu(i).FLOW * CpAir;
                    AirflowNetworkExchangeData(ZN2).SumMMCpT += AirflowNetworkLinkSimu(i).FLOW * CpAir * ANZT(ZN1);
                    AirflowNetworkExchangeData(ZN2).SumMMHr += AirflowNetworkLinkSimu(i).FLOW;
                    AirflowNetworkExchangeData(ZN2).SumMMHrW += AirflowNetworkLinkSimu(i).FLOW * ANZW(ZN1);
                    if (Contaminant.CO2Simulation) {
                        AirflowNetworkExchangeData(ZN2).SumMMHrCO += AirflowNetworkLinkSimu(i).FLOW * ANCO(ZN1);
                    }
                    if (Contaminant.GenericContamSimulation) {
                        AirflowNetworkExchangeData(ZN2).SumMMHrGC += AirflowNetworkLinkSimu(i).FLOW * ANGC(ZN1);
                    }
                    CpAir = PsyCpAirFnW(ANZW(ZN2));
                    AirflowNetworkExchangeData(ZN1).SumMMCp += AirflowNetworkLinkSimu(i).FLOW2 * CpAir;
                    AirflowNetworkExchangeData(ZN1).SumMMCpT += AirflowNetworkLinkSimu(i).FLOW2 * CpAir * ANZT(ZN2);
                    AirflowNetworkExchangeData(ZN1).SumMMHr += AirflowNetworkLinkSimu(i).FLOW2;
                    AirflowNetworkExchangeData(ZN1).SumMMHrW += AirflowNetworkLinkSimu(i).FLOW2 * ANZW(ZN2);
                    if (Contaminant.CO2Simulation) {
                        AirflowNetworkExchangeData(ZN1).SumMMHrCO += AirflowNetworkLinkSimu(i).FLOW2 * ANCO(ZN2);
                    }
                    if (Contaminant.GenericContamSimulation) {
                        AirflowNetworkExchangeData(ZN1).SumMMHrGC += AirflowNetworkLinkSimu(i).FLOW2 * ANGC(ZN2);
                    }
                }
            }
        }
        // End of update of multizone airflow calculations

        // Initialize these values
        for (auto &e : AirflowNetworkExchangeData) {
            e.LeakSen = 0.0;
            e.CondSen = 0.0;
            e.LeakLat = 0.0;
            e.DiffLat = 0.0;
            e.MultiZoneSen = 0.0;
            e.MultiZoneLat = 0.0;
            e.RadGain = 0.0;
        }

        // Rewrite AirflowNetwork airflow rate
        for (i = 1; i <= NumOfLinksMultiZone; ++i) {
            Tamb = OutDryBulbTempAt(AirflowNetworkLinkageData(i).NodeHeights[0]);
            AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, Tamb, OutHumRat);
            AirflowNetworkLinkSimu(i).VolFLOW = AirflowNetworkLinkSimu(i).FLOW / AirDensity;
            AirflowNetworkLinkSimu(i).VolFLOW2 = AirflowNetworkLinkSimu(i).FLOW2 / AirDensity;
        }

        for (std::size_t i = 0; i < AirflowNetworkLinkReport.size(); ++i) {
            auto &r(AirflowNetworkLinkReport[i]);
            auto &s(AirflowNetworkLinkSimu[i]);
            r.FLOW = s.FLOW;
            r.FLOW2 = s.FLOW2;
            r.VolFLOW = s.VolFLOW;
            r.VolFLOW2 = s.VolFLOW2;
        }

        // Save zone loads from multizone calculation for later summation
        bool OnOffFanFlag = false;
        for (i = 1; i <= DisSysNumOfCVFs; i++) {
            if (DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff) {
                OnOffFanFlag = true;
                break;
            }
        }
        if (present(FirstHVACIteration)) {
            if (FirstHVACIteration && OnOffFanFlag) {
                AirflowNetworkMultiExchangeData = AirflowNetworkExchangeData;
                for (i = 1; i <= AirflowNetworkNumOfZones; ++i) {
                    AirflowNetworkNodeReport(i).PZ = AirflowNetworkNodeSimu(i).PZ;
                    AirflowNetworkNodeReport(i).PZOFF = AirflowNetworkNodeSimu(i).PZ;
                    AirflowNetworkNodeReport(i).PZON = 0.0;
                }
                for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
                    AirflowNetworkLinkReport1(i).FLOW = AirflowNetworkLinkSimu(i).FLOW;
                    AirflowNetworkLinkReport1(i).FLOW2 = AirflowNetworkLinkSimu(i).FLOW2;
                    AirflowNetworkLinkReport1(i).VolFLOW = AirflowNetworkLinkSimu(i).VolFLOW;
                    AirflowNetworkLinkReport1(i).VolFLOW2 = AirflowNetworkLinkSimu(i).VolFLOW2;
                    AirflowNetworkLinkReport1(i).FLOWOFF = AirflowNetworkLinkSimu(i).FLOW;
                    AirflowNetworkLinkReport1(i).FLOW2OFF = AirflowNetworkLinkSimu(i).FLOW2;
                    AirflowNetworkLinkReport1(i).VolFLOWOFF = AirflowNetworkLinkSimu(i).VolFLOW;
                    AirflowNetworkLinkReport1(i).VolFLOW2OFF = AirflowNetworkLinkSimu(i).VolFLOW2;
                    AirflowNetworkLinkReport1(i).DP = AirflowNetworkLinkSimu(i).DP;
                    AirflowNetworkLinkReport1(i).DPOFF = AirflowNetworkLinkSimu(i).DP;
                    AirflowNetworkLinkReport1(i).DPON = 0.0;
                }
            }
        }

        if (!AirflowNetworkFanActivated && (SimulateAirflowNetwork > AirflowNetworkControlMultizone)) {
            for (i = NumOfNodesMultiZone + NumOfNodesIntraZone + 1; i <= AirflowNetworkNumOfNodes; ++i) {
                AirflowNetworkNodeSimu(i).PZ = 0.0;
            }
            for (i = AirflowNetworkNumOfSurfaces + 1; i <= AirflowNetworkNumOfLinks; ++i) {
                AirflowNetworkLinkSimu(i).DP = 0.0;
                AirflowNetworkLinkReport(i).FLOW = 0.0;
                AirflowNetworkLinkReport(i).FLOW2 = 0.0;
                AirflowNetworkLinkReport(i).VolFLOW = 0.0;
                AirflowNetworkLinkReport(i).VolFLOW2 = 0.0;
            }
        }

        if (!(AirflowNetworkFanActivated && SimulateAirflowNetwork > AirflowNetworkControlMultizone)) return;

        if (SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
            for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) { // Multizone airflow energy
                n = AirflowNetworkLinkageData(i).NodeNums[0];
                M = AirflowNetworkLinkageData(i).NodeNums[1];
                ZN1 = AirflowNetworkNodeData(n).EPlusZoneNum;
                ZN2 = AirflowNetworkNodeData(M).EPlusZoneNum;
                // Find a linkage from a zone to outdoors
                if (ZN1 > 0 && ZN2 == 0) {
                    Tamb = Zone(ZN1).OutDryBulbTemp;
                    CpAir = PsyCpAirFnW(OutHumRat);
                    AirflowNetworkExchangeData(ZN1).MultiZoneSen += AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (Tamb - ANZT(ZN1));
                    AirflowNetworkExchangeData(ZN1).MultiZoneLat += AirflowNetworkLinkSimu(i).FLOW2 * (OutHumRat - ANZW(ZN1));
                }
                if (ZN1 == 0 && ZN2 > 0) {
                    Tamb = Zone(ZN2).OutDryBulbTemp;
                    CpAir = PsyCpAirFnW(OutHumRat);
                    AirflowNetworkExchangeData(ZN2).MultiZoneSen += AirflowNetworkLinkSimu(i).FLOW * CpAir * (Tamb - ANZT(ZN2));
                    AirflowNetworkExchangeData(ZN2).MultiZoneLat += AirflowNetworkLinkSimu(i).FLOW * (OutHumRat - ANZW(ZN2));
                }

                if (ZN1 > 0 && ZN2 > 0) {
                    if (AirflowNetworkLinkSimu(i).FLOW > 0) { // Flow from ZN1 to ZN2
                        CpAir = PsyCpAirFnW(ANZW(ZN1));
                        AirflowNetworkExchangeData(ZN2).MultiZoneSen += AirflowNetworkLinkSimu(i).FLOW * CpAir * (ANZT(ZN1) - ANZT(ZN2));
                        AirflowNetworkExchangeData(ZN2).MultiZoneLat += AirflowNetworkLinkSimu(i).FLOW * (ANZW(ZN1) - ANZW(ZN2));
                        CpAir = PsyCpAirFnW(ANZW(ZN2));
                        AirflowNetworkExchangeData(ZN1).MultiZoneSen += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir * (ANZT(ZN2) - ANZT(ZN1));
                        AirflowNetworkExchangeData(ZN1).MultiZoneLat += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * (ANZW(ZN2) - ANZW(ZN1));
                    } else {
                        //          CpAir   = PsyCpAirFnW(ZoneAirHumRat(ZN2), MAT(ZN2))
                        //          AirflowNetworkExchangeData(ZN1)%MultiZoneSen = AirflowNetworkExchangeData(ZN1)%MultiZoneSen + &
                        //            AirflowNetworkLinkSimu(i)%FLOW*CpAir*(MAT(ZN1)-MAT(ZN2))
                        //          AirflowNetworkExchangeData(ZN1)%MultiZoneLat = AirflowNetworkExchangeData(ZN1)%MultiZoneLat + &
                        //            AirflowNetworkLinkSimu(i)%FLOW*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2))
                        //          CpAir   = PsyCpAirFnW(ZoneAirHumRat(ZN1), MAT(ZN1))
                        //          AirflowNetworkExchangeData(ZN2)%MultiZoneSen = AirflowNetworkExchangeData(ZN2)%MultiZoneSen + &
                        //            ABS(AirflowNetworkLinkSimu(i)%FLOW2)*CpAir*(MAT(ZN1)-MAT(ZN2))
                        //          AirflowNetworkExchangeData(ZN2)%MultiZoneLat = AirflowNetworkExchangeData(ZN2)%MultiZoneLat + &
                        //            ABS(AirflowNetworkLinkSimu(i)%FLOW2)*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2))
                        CpAir = PsyCpAirFnW(ANZW(ZN2));
                        AirflowNetworkExchangeData(ZN1).MultiZoneSen += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * CpAir * (ANZT(ZN2) - ANZT(ZN1));
                        AirflowNetworkExchangeData(ZN1).MultiZoneLat += std::abs(AirflowNetworkLinkSimu(i).FLOW2) * (ANZW(ZN2) - ANZW(ZN1));
                    }
                }
            }
        }

        int AirLoopNum;
        int FanNum;
        Real64 MaxPartLoadRatio = 0.0;
        MaxOnOffFanRunTimeFraction = 0.0;
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            MaxPartLoadRatio = max(MaxPartLoadRatio, AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio);
            MaxOnOffFanRunTimeFraction = max(MaxOnOffFanRunTimeFraction, LoopOnOffFanRunTimeFraction(AirLoopNum));
        }
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            for (FanNum = 1; FanNum <= DisSysNumOfCVFs; ++FanNum) {
                if (DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) break;
            }
            PartLoadRatio = 1.0;
            LoopPartLoadRatio(AirLoopNum) = 1.0;
            OnOffFanRunTimeFraction = 1.0;
            LoopOnOffFanRunTimeFraction(AirLoopNum) = 1.0;
            // Calculate the part load ratio, can't be greater than 1 for a simple ONOFF fan
            if (DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff &&
                Node(DisSysCompCVFData(FanNum).InletNode).MassFlowRate > VerySmallMassFlow &&
                AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycCoil) {
                // Hard code here
                PartLoadRatio = AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
                LoopPartLoadRatio(AirLoopNum) = AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
                OnOffFanRunTimeFraction = max(AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF,
                                              AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF,
                                              AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF);
                LoopOnOffFanRunTimeFraction(AirLoopNum) = max(AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF,
                                                              AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF,
                                                              AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF);
            }
            AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF = 0.0;

            if (DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff && LoopPartLoadRatio(AirLoopNum) < 1.0) {
                for (std::size_t i = 0; i < AirflowNetworkLinkReport.size(); ++i) {
                    auto &r(AirflowNetworkLinkReport[i]);
                    auto &s(AirflowNetworkLinkSimu[i]);
                    auto &t(AirflowNetworkLinkageData[i]);
                    if (t.AirLoopNum == AirLoopNum) {
                        r.FLOW = s.FLOW * LoopPartLoadRatio(AirLoopNum);
                        r.FLOW2 = s.FLOW2 * LoopPartLoadRatio(AirLoopNum);
                        r.VolFLOW = s.VolFLOW * LoopPartLoadRatio(AirLoopNum);
                        r.VolFLOW2 = s.VolFLOW2 * LoopPartLoadRatio(AirLoopNum);
                    }
                    if (t.AirLoopNum == 0) {
                        r.FLOW = s.FLOW * MaxPartLoadRatio;
                        r.FLOW2 = s.FLOW2 * MaxPartLoadRatio;
                        r.VolFLOW = s.VolFLOW * MaxPartLoadRatio;
                        r.VolFLOW2 = s.VolFLOW2 * MaxPartLoadRatio;
                    }
                }
            }
        }

        // One time warning
        if (MyOneTimeFlag) {
            for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
                for (FanNum = 1; FanNum <= DisSysNumOfCVFs; ++FanNum) {
                    if (DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) break;
                }
                if (DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff &&
                    AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == ContFanCycCoil) {
                    OnOffRatio =
                        std::abs((AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate - AirLoopAFNInfo(AirLoopNum).LoopSystemOffMassFlowrate) /
                                 AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate);
                    if (OnOffRatio > 0.1) {
                        ShowWarningError("The absolute percent difference of supply air mass flow rate between HVAC operation and No HVAC operation "
                                         "is above 10% with fan operation mode = ContFanCycCoil.");
                        ShowContinueError("The added zone loads using the AirflowNetwork model may not be accurate because the zone loads are "
                                          "calculated based on the mass flow rate during HVAC operation.");
                        ShowContinueError(
                            "The mass flow rate during HVAC operation = " + RoundSigDigits(AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate, 2) +
                            " The mass flow rate during no HVAC operation = " +
                            RoundSigDigits(AirLoopAFNInfo(AirLoopNum).LoopSystemOffMassFlowrate, 2));
                        MyOneTimeFlag = false;
                    }
                }
            }
        }

        // Check mass flow differences in the zone inlet zones and splitter nodes between node and AFN links
        if (MyOneTimeFlag1) {
            if ((!VAVSystem) && DisplayExtraWarnings) {
                WriteFlag = false;
                for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
                    Node1 = AirflowNetworkLinkageData(i).NodeNums[0];
                    Node2 = AirflowNetworkLinkageData(i).NodeNums[1];
                    if (AirflowNetworkNodeData(Node1).EPlusTypeNum == EPlusTypeNum_SPI ||
                        AirflowNetworkNodeData(Node2).EPlusTypeNum == EPlusTypeNum_SPO ||
                        AirflowNetworkNodeData(Node2).EPlusTypeNum == EPlusTypeNum_ZIN) {
                        if (AirflowNetworkNodeData(Node1).EPlusTypeNum == EPlusTypeNum_SPI) {
                            Node3 = Node1;
                        } else {
                            Node3 = Node2;
                        }
                        if (AirflowNetworkNodeData(Node2).EPlusTypeNum == EPlusTypeNum_ZIN) {
                            if (AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).EPlusTypeNum == 0) continue;
                        }
                        NodeMass = Node(AirflowNetworkNodeData(Node3).EPlusNodeNum).MassFlowRate;
                        AFNMass = AirflowNetworkLinkSimu(i).FLOW;
                        if (NodeMass > 0.0 && AFNMass > NodeMass + 0.01) {
                            ShowWarningError("The mass flow rate difference is found between System Node = '" +
                                             NodeID(AirflowNetworkNodeData(Node3).EPlusNodeNum) + "' and AFN Link = '" +
                                             AirflowNetworkLinkageData(i).Name + "'.");
                            ShowContinueError("The system node max mass flow rate = " + RoundSigDigits(NodeMass, 3) +
                                              " kg/s. The AFN node mass flow rate = " + RoundSigDigits(AFNMass, 3) + " kg.s.");
                            WriteFlag = true;
                        }
                    }
                }
                MyOneTimeFlag1 = false;
                if (WriteFlag) {
                    ShowWarningError("Please adjust the rate of Maximum Air Flow Rate field in the terminal objects or duct pressure resistance.");
                }
            } else {
                MyOneTimeFlag1 = false;
            }
        }

        // Assign airflows to EPLus nodes
        for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            if (AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_DWC ||
                AirflowNetworkLinkageData(i).VAVTermDamper) {
                // Exclude envelope leakage Crack element
                Node1 = AirflowNetworkLinkageData(i).NodeNums[0];
                Node2 = AirflowNetworkLinkageData(i).NodeNums[1];

                j = AirflowNetworkNodeData(Node1).EPlusNodeNum;
                if (j > 0 && AirflowNetworkNodeData(Node1).EPlusZoneNum == 0) {
                    Node(j).MassFlowRate = AirflowNetworkLinkSimu(i).FLOW * LoopPartLoadRatio(AirflowNetworkNodeData(Node1).AirLoopNum);
                    if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum)) Node(j).MassFlowRate = 0.0;
                    if (!(AirflowNetworkNodeData(Node1).EPlusTypeNum == EPlusTypeNum_DIN ||
                          AirflowNetworkNodeData(Node1).EPlusTypeNum == EPlusTypeNum_DOU)) {
                        Node(j).MassFlowRateMaxAvail = AirflowNetworkLinkSimu(i).FLOW * LoopPartLoadRatio(AirflowNetworkNodeData(Node1).AirLoopNum);
                        Node(j).MassFlowRateMax = AirflowNetworkLinkSimu(i).FLOW;
                    }
                }

                j = AirflowNetworkNodeData(Node2).EPlusNodeNum;
                if (j > 0) {
                    Node(j).MassFlowRate = AirflowNetworkLinkSimu(i).FLOW * LoopPartLoadRatio(AirflowNetworkNodeData(Node2).AirLoopNum);
                    if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum)) Node(j).MassFlowRate = 0.0;
                    if (!(AirflowNetworkNodeData(Node2).EPlusTypeNum == EPlusTypeNum_DIN ||
                          AirflowNetworkNodeData(Node2).EPlusTypeNum == EPlusTypeNum_DOU)) {
                        Node(j).MassFlowRateMaxAvail = AirflowNetworkLinkSimu(i).FLOW * LoopPartLoadRatio(AirflowNetworkNodeData(Node2).AirLoopNum);
                        Node(j).MassFlowRateMax = AirflowNetworkLinkSimu(i).FLOW;
                    }
                }
            }
        }

        // Assign AirflowNetwork nodal values to Node array
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            j = AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0) {
                Node(j).Enthalpy = PsyHFnTdbW(AirflowNetworkNodeSimu(i).TZ, AirflowNetworkNodeSimu(i).WZ);
                Node(j).Temp = AirflowNetworkNodeSimu(i).TZ;
                Node(j).HumRat = AirflowNetworkNodeSimu(i).WZ;
                if (Contaminant.CO2Simulation) {
                    Node(j).CO2 = AirflowNetworkNodeSimu(i).CO2Z;
                }
                if (Contaminant.GenericContamSimulation) {
                    Node(j).GenContam = AirflowNetworkNodeSimu(i).GCZ;
                }
            }
        }

        // Calculate sensible loads from forced air flow
        for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            Node1 = AirflowNetworkLinkageData(i).NodeNums[0];
            Node2 = AirflowNetworkLinkageData(i).NodeNums[1];
            CpAir = PsyCpAirFnW((AirflowNetworkNodeSimu(Node1).WZ + AirflowNetworkNodeSimu(Node2).WZ) / 2.0);
            // Calculate sensible loads from duct conduction losses and loads from duct radiation
            if (AirflowNetworkLinkageData(i).ZoneNum > 0 &&
                AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_DWC) {
                Qsen = AirflowNetworkLinkSimu(i).FLOW * CpAir * (AirflowNetworkNodeSimu(Node2).TZ - AirflowNetworkNodeSimu(Node1).TZ);
                if (AirflowNetworkLinkageData(i).LinkageViewFactorObjectNum != 0) {
                    auto &DuctRadObj(AirflowNetworkLinkageViewFactorData(AirflowNetworkLinkageData(i).LinkageViewFactorObjectNum));
                    Qsen -= DuctRadObj.QRad;
                    AirflowNetworkExchangeData(AirflowNetworkLinkageData(i).ZoneNum).RadGain -= DuctRadObj.QRad;
                }
                // When the Airloop is shut off, no duct sensible losses
                if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum)) Qsen = 0.0;
                AirflowNetworkExchangeData(AirflowNetworkLinkageData(i).ZoneNum).CondSen -= Qsen;
            }
            // Calculate sensible leakage losses
            if (AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_PLR ||
                AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_ELR) {
                // Calculate supply leak sensible losses
                if ((AirflowNetworkNodeData(Node2).EPlusZoneNum > 0) && (AirflowNetworkNodeData(Node1).EPlusNodeNum == 0) &&
                    (AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    ZN2 = AirflowNetworkNodeData(Node2).EPlusZoneNum;
                    Qsen = AirflowNetworkLinkSimu(i).FLOW * CpAir * (AirflowNetworkNodeSimu(Node1).TZ - AirflowNetworkNodeSimu(Node2).TZ);
                    if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum)) Qsen = 0.0;
                    AirflowNetworkExchangeData(ZN2).LeakSen += Qsen;
                }
                if ((AirflowNetworkNodeData(Node1).EPlusZoneNum > 0) && (AirflowNetworkNodeData(Node2).EPlusNodeNum == 0) &&
                    (AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    ZN1 = AirflowNetworkNodeData(Node1).EPlusZoneNum;
                    Qsen = AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (AirflowNetworkNodeSimu(Node2).TZ - AirflowNetworkNodeSimu(Node1).TZ);
                    if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum)) Qsen = 0.0;
                    AirflowNetworkExchangeData(ZN1).LeakSen += Qsen;
                }
            }
        }

        // Calculate latent loads from forced air flow
        for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            Node1 = AirflowNetworkLinkageData(i).NodeNums[0];
            Node2 = AirflowNetworkLinkageData(i).NodeNums[1];
            // Calculate latent loads from duct conduction losses
            if (AirflowNetworkLinkageData(i).ZoneNum > 0 &&
                AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_DWC) {
                Qlat = AirflowNetworkLinkSimu(i).FLOW * (AirflowNetworkNodeSimu(Node2).WZ - AirflowNetworkNodeSimu(Node1).WZ);
                if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum)) Qlat = 0.0;
                AirflowNetworkExchangeData(AirflowNetworkLinkageData(i).ZoneNum).DiffLat -= Qlat;
            }
            // Calculate latent leakage losses
            if (AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_PLR ||
                AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_ELR) {
                // Calculate supply leak latent losses
                if ((AirflowNetworkNodeData(Node2).EPlusZoneNum > 0) && (AirflowNetworkNodeData(Node1).EPlusNodeNum == 0) &&
                    (AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    ZN2 = AirflowNetworkNodeData(Node2).EPlusZoneNum;
                    Qlat = AirflowNetworkLinkSimu(i).FLOW * (AirflowNetworkNodeSimu(Node1).WZ - AirflowNetworkNodeSimu(Node2).WZ);
                    if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum)) Qlat = 0.0;
                    AirflowNetworkExchangeData(ZN2).LeakLat += Qlat;
                    if (Contaminant.CO2Simulation) {
                        AirflowNetworkExchangeData(ZN2).TotalCO2 +=
                            AirflowNetworkLinkSimu(i).FLOW * (AirflowNetworkNodeSimu(Node1).CO2Z - AirflowNetworkNodeSimu(Node2).CO2Z);
                    }
                    if (Contaminant.GenericContamSimulation) {
                        AirflowNetworkExchangeData(ZN2).TotalGC +=
                            AirflowNetworkLinkSimu(i).FLOW * (AirflowNetworkNodeSimu(Node1).GCZ - AirflowNetworkNodeSimu(Node2).GCZ);
                    }
                }
                if ((AirflowNetworkNodeData(Node1).EPlusZoneNum > 0) && (AirflowNetworkNodeData(Node2).EPlusNodeNum == 0) &&
                    (AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    ZN1 = AirflowNetworkNodeData(Node1).EPlusZoneNum;
                    Qlat = AirflowNetworkLinkSimu(i).FLOW2 * (AirflowNetworkNodeSimu(Node2).WZ - AirflowNetworkNodeSimu(Node1).WZ);
                    if (!LoopOnOffFlag(AirflowNetworkLinkageData(i).AirLoopNum)) Qlat = 0.0;
                    AirflowNetworkExchangeData(ZN1).LeakLat += Qlat;
                    if (Contaminant.CO2Simulation) {
                        AirflowNetworkExchangeData(ZN1).TotalCO2 +=
                            AirflowNetworkLinkSimu(i).FLOW2 * (AirflowNetworkNodeSimu(Node2).CO2Z - AirflowNetworkNodeSimu(Node1).CO2Z);
                    }
                    if (Contaminant.GenericContamSimulation) {
                        AirflowNetworkExchangeData(ZN1).TotalGC +=
                            AirflowNetworkLinkSimu(i).FLOW2 * (AirflowNetworkNodeSimu(Node2).GCZ - AirflowNetworkNodeSimu(Node1).GCZ);
                    }
                }
            }
        }

        // Sum all the loads
        for (i = 1; i <= NumOfZones; ++i) {
            AirflowNetworkExchangeData(i).TotalSen =
                AirflowNetworkExchangeData(i).LeakSen + AirflowNetworkExchangeData(i).CondSen + AirflowNetworkExchangeData(i).RadGain;
            AirflowNetworkExchangeData(i).TotalLat = AirflowNetworkExchangeData(i).LeakLat + AirflowNetworkExchangeData(i).DiffLat;
        }

        // Simple ONOFF fan
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            for (FanNum = 1; FanNum <= DisSysNumOfCVFs; ++FanNum) {
                if (DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) break;
            }
            if (DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff && OnOffFanRunTimeFraction < 1.0) {
                for (i = 1; i <= NumOfZones; ++i) {
                    AirflowNetworkExchangeData(i).MultiZoneSen *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).MultiZoneLat *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).LeakSen *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).LeakLat *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).CondSen *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).DiffLat *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).RadGain *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).TotalSen *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).TotalLat *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).SumMCp *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).SumMCpT *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).SumMHr *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).SumMHrW *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).SumMMCp *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).SumMMCpT *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).SumMMHr *= OnOffFanRunTimeFraction;
                    AirflowNetworkExchangeData(i).SumMMHrW *= OnOffFanRunTimeFraction;
                    if (Contaminant.CO2Simulation) {
                        AirflowNetworkExchangeData(i).SumMHrCO *= OnOffFanRunTimeFraction;
                        AirflowNetworkExchangeData(i).SumMMHrCO *= OnOffFanRunTimeFraction;
                    }
                    if (Contaminant.GenericContamSimulation) {
                        AirflowNetworkExchangeData(i).SumMHrGC *= OnOffFanRunTimeFraction;
                        AirflowNetworkExchangeData(i).SumMMHrGC *= OnOffFanRunTimeFraction;
                    }
                }
                if (AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycCoil) {
                    for (i = 1; i <= NumOfZones; ++i) {
                        AirflowNetworkExchangeData(i).SumMCp += AirflowNetworkMultiExchangeData(i).SumMCp * (1.0 - OnOffFanRunTimeFraction);
                        AirflowNetworkExchangeData(i).SumMCpT += AirflowNetworkMultiExchangeData(i).SumMCpT * (1.0 - OnOffFanRunTimeFraction);
                        AirflowNetworkExchangeData(i).SumMHr += AirflowNetworkMultiExchangeData(i).SumMHr * (1.0 - OnOffFanRunTimeFraction);
                        AirflowNetworkExchangeData(i).SumMHrW += AirflowNetworkMultiExchangeData(i).SumMHrW * (1.0 - OnOffFanRunTimeFraction);
                        AirflowNetworkExchangeData(i).SumMMCp += AirflowNetworkMultiExchangeData(i).SumMMCp * (1.0 - OnOffFanRunTimeFraction);
                        AirflowNetworkExchangeData(i).SumMMCpT += AirflowNetworkMultiExchangeData(i).SumMMCpT * (1.0 - OnOffFanRunTimeFraction);
                        AirflowNetworkExchangeData(i).SumMMHr += AirflowNetworkMultiExchangeData(i).SumMMHr * (1.0 - OnOffFanRunTimeFraction);
                        AirflowNetworkExchangeData(i).SumMMHrW += AirflowNetworkMultiExchangeData(i).SumMMHrW * (1.0 - OnOffFanRunTimeFraction);
                        if (Contaminant.CO2Simulation) {
                            AirflowNetworkExchangeData(i).SumMHrCO += AirflowNetworkMultiExchangeData(i).SumMHrCO * (1.0 - OnOffFanRunTimeFraction);
                            AirflowNetworkExchangeData(i).SumMMHrCO += AirflowNetworkMultiExchangeData(i).SumMMHrCO * (1.0 - OnOffFanRunTimeFraction);
                        }
                        if (Contaminant.GenericContamSimulation) {
                            AirflowNetworkExchangeData(i).SumMHrGC += AirflowNetworkMultiExchangeData(i).SumMHrGC * (1.0 - OnOffFanRunTimeFraction);
                            AirflowNetworkExchangeData(i).SumMMHrGC += AirflowNetworkMultiExchangeData(i).SumMMHrGC * (1.0 - OnOffFanRunTimeFraction);
                        }
                    }
                }
            }

            if (DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff) {
                for (i = 1; i <= AirflowNetworkNumOfZones; ++i) {
                    if (AirflowNetworkNodeData(i).AirLoopNum == AirLoopNum) {
                        AirflowNetworkNodeReport(i).PZ = AirflowNetworkNodeSimu(i).PZ * LoopPartLoadRatio(AirLoopNum) +
                                                         AirflowNetworkNodeReport(i).PZOFF * (1.0 - LoopPartLoadRatio(AirLoopNum));
                        AirflowNetworkNodeReport(i).PZON = AirflowNetworkNodeSimu(i).PZ;
                    }
                }
                for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
                    PartLoadRatio = MaxPartLoadRatio;
                    for (j = 1; j <= AirflowNetworkNumOfZones; ++j) {
                        if (MultizoneZoneData(j).ZoneNum == MultizoneSurfaceData(i).ZonePtr) {
                            if (AirflowNetworkNodeData(j).AirLoopNum == AirLoopNum) {
                                PartLoadRatio = LoopPartLoadRatio(AirLoopNum);
                                break;
                            }
                        }
                    }
                    AirflowNetworkLinkReport1(i).FLOW =
                        AirflowNetworkLinkSimu(i).FLOW * PartLoadRatio + AirflowNetworkLinkReport1(i).FLOWOFF * (1.0 - PartLoadRatio);
                    AirflowNetworkLinkReport1(i).FLOW2 =
                        AirflowNetworkLinkSimu(i).FLOW2 * PartLoadRatio + AirflowNetworkLinkReport1(i).FLOW2OFF * (1.0 - PartLoadRatio);
                    AirflowNetworkLinkReport1(i).VolFLOW =
                        AirflowNetworkLinkSimu(i).VolFLOW * PartLoadRatio + AirflowNetworkLinkReport1(i).VolFLOWOFF * (1.0 - PartLoadRatio);
                    AirflowNetworkLinkReport1(i).VolFLOW2 =
                        AirflowNetworkLinkSimu(i).VolFLOW2 * PartLoadRatio + AirflowNetworkLinkReport1(i).VolFLOW2OFF * (1.0 - PartLoadRatio);
                    AirflowNetworkLinkReport1(i).DP =
                        AirflowNetworkLinkSimu(i).DP * PartLoadRatio + AirflowNetworkLinkReport1(i).DPOFF * (1.0 - PartLoadRatio);
                    AirflowNetworkLinkReport1(i).DPON = AirflowNetworkLinkSimu(i).DP;
                }
            }
        }

        // Save values
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            AirflowNetworkNodeSimu(i).TZlast = AirflowNetworkNodeSimu(i).TZ;
            AirflowNetworkNodeSimu(i).WZlast = AirflowNetworkNodeSimu(i).WZ;
            if (Contaminant.CO2Simulation) {
                AirflowNetworkNodeSimu(i).CO2Zlast = AirflowNetworkNodeSimu(i).CO2Z;
            }
            if (Contaminant.GenericContamSimulation) {
                AirflowNetworkNodeSimu(i).GCZlast = AirflowNetworkNodeSimu(i).GCZ;
            }
        }
    }

    void AirflowNetworkVentingControl(int const i,       // AirflowNetwork surface number
                                      Real64 &OpenFactor // Window or door opening factor (used to calculate airflow)
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   April 2003
        //       MODIFIED       Feb 2004, FCW: allow venting control of interior window/door
        //       MODIFIED       Nov. 2005, LG: to fit the requirement for AirflowNetwork Model
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // Determines the venting opening factor for an exterior or interior window or door
        // as determined by the venting control method.

        // METHODOLOGY EMPLOYED:na
        // REFERENCES:na

        // Using/Aliasing
        using DataSurfaces::SurfaceWindow;
        using ScheduleManager::GetCurrentScheduleValue;
        using ThermalComfort::ThermalComfortData;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:na
        // INTERFACE BLOCK SPECIFICATIONS:na
        // DERIVED TYPE DEFINITIONS:na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 VentTemp;                // Venting temperature (C)
        Real64 ZoneAirEnthalpy;         // Enthalpy of zone air (J/kg)
        Real64 OpenFactorMult;          // Window/door opening modulation multiplier on venting open factor
        Real64 DelTemp;                 // Inside-outside air temperature difference (K)
        Real64 DelEnthal;               // Inside-outside air enthalpy difference (J/kg)
        int IZ;                         // AirflowNetwork zone number
        int ZoneNum;                    // EnergyPlus zone number
        int SurfNum;                    // Heat transfer surface number
        Real64 LimValVentOpenFacMult;   // Limiting value of venting opening factor multiplier
        Real64 LowerValInOutTempDiff;   // Lower value of inside/outside temperature difference for opening factor modulation
        Real64 UpperValInOutTempDiff;   // Upper value of inside/outside temperature difference for opening factor modulation
        Real64 LowerValInOutEnthalDiff; // Lower value of inside/outside enthalpy difference for opening factor modulation
        Real64 UpperValInOutEnthalDiff; // Upper value of inside/outside enthalpy difference for opening factor modulation
        bool VentingAllowed;            // True if venting schedule allows venting
        int VentCtrlNum;                // Venting control strategy 1: Temperature contro; 2: Enthalpy control
        Real64 VentingSchVal;           // Current time step value of venting schedule
        Real64 Tamb;                    // Outdoor dry bulb temperature at surface centroid height
        int PeopleInd;

        if (MultizoneSurfaceData(i).EMSOpenFactorActuated) { // EMS sets value to use
            OpenFactor = MultizoneSurfaceData(i).EMSOpenFactor;
            SurfNum = MultizoneSurfaceData(i).SurfNum;
            if (MultizoneSurfaceData(i).Factor > 0.0) {
                SurfaceWindow(SurfNum).VentingOpenFactorMultRep = OpenFactor / MultizoneSurfaceData(i).Factor;
            } else {
                SurfaceWindow(SurfNum).VentingOpenFactorMultRep = OpenFactor;
            }
            return;
        }

        SurfNum = MultizoneSurfaceData(i).SurfNum;

        SurfaceWindow(SurfNum).VentingOpenFactorMultRep = -1.0;

        // Get venting temperature and venting strategy for exterior window or door
        // and determine whether venting is allowed

        SurfaceWindow(SurfNum).VentingAvailabilityRep = 1.0;
        VentingAllowed = true;
        IZ = MultizoneSurfaceData(i).NodeNums[0];
        // Revise for RoomAirflowNetwork model
        if (MultizoneSurfaceData(i).RAFNflag) IZ = MultizoneSurfaceData(i).ZonePtr;
        ZoneNum = MultizoneZoneData(IZ).ZoneNum;

        // Note in the following that individual venting control for a window/door takes
        // precedence over zone-level control
        if (MultizoneSurfaceData(i).IndVentControl) {
            VentTemp = GetCurrentScheduleValue(MultizoneSurfaceData(i).VentSchNum);
            VentCtrlNum = MultizoneSurfaceData(i).VentSurfCtrNum;
            if (MultizoneSurfaceData(i).VentingSchNum > 0) {
                VentingSchVal = GetCurrentScheduleValue(MultizoneSurfaceData(i).VentingSchNum);
                if (VentingSchVal <= 0.0) {
                    VentingAllowed = false;
                    SurfaceWindow(SurfNum).VentingAvailabilityRep = 0.0;
                }
            }
        } else {
            // Zone level only by Gu on Nov. 8, 2005
            VentTemp = GetCurrentScheduleValue(MultizoneZoneData(IZ).VentSchNum);
            VentCtrlNum = MultizoneZoneData(IZ).VentCtrNum;
            if (MultizoneZoneData(IZ).VentingSchNum > 0) {
                VentingSchVal = GetCurrentScheduleValue(MultizoneZoneData(IZ).VentingSchNum);
                if (VentingSchVal <= 0.0) {
                    VentingAllowed = false;
                    SurfaceWindow(SurfNum).VentingAvailabilityRep = 0.0;
                }
            }
        }

        SurfaceWindow(SurfNum).InsideTempForVentingRep = VentTemp;
        OpenFactor = 0.0;

        // Venting based on inside-outside air temperature difference

        if ((VentCtrlNum == VentCtrNum_Temp || VentCtrlNum == VentCtrNum_AdjTemp) && VentingAllowed) {
            Tamb = Surface(SurfNum).OutDryBulbTemp;
            // Check whether this surface is an interior wall or not. If Yes, use adjacent zone conditions
            if (VentCtrlNum == VentCtrNum_AdjTemp && MultizoneSurfaceData(i).IndVentControl) {
                Tamb = ANZT(MultizoneZoneData(MultizoneSurfaceData(i).NodeNums[1]).ZoneNum);
            }
            if (ANZT(ZoneNum) > Tamb && ANZT(ZoneNum) > VentTemp) {
                OpenFactor = MultizoneSurfaceData(i).Factor;
                SurfaceWindow(SurfNum).VentingOpenFactorMultRep = 1.0;
                // Modulation of OpenFactor
                if (MultizoneSurfaceData(i).IndVentControl) {
                    LimValVentOpenFacMult = MultizoneSurfaceData(i).ModulateFactor;
                    LowerValInOutTempDiff = MultizoneSurfaceData(i).LowValueTemp;
                    UpperValInOutTempDiff = MultizoneSurfaceData(i).UpValueTemp;
                } else {
                    LimValVentOpenFacMult = MultizoneZoneData(IZ).OpenFactor;
                    LowerValInOutTempDiff = MultizoneZoneData(IZ).LowValueTemp;
                    UpperValInOutTempDiff = MultizoneZoneData(IZ).UpValueTemp;
                }
                if (LimValVentOpenFacMult != 1.0) {
                    DelTemp = ANZT(ZoneNum) - Tamb;
                    if (DelTemp <= LowerValInOutTempDiff) {
                        OpenFactorMult = 1.0;
                    } else if (DelTemp >= UpperValInOutTempDiff) {
                        OpenFactorMult = LimValVentOpenFacMult;
                    } else {
                        OpenFactorMult =
                            LimValVentOpenFacMult +
                            ((UpperValInOutTempDiff - DelTemp) / (UpperValInOutTempDiff - LowerValInOutTempDiff)) * (1 - LimValVentOpenFacMult);
                    }
                    OpenFactor *= OpenFactorMult;
                    SurfaceWindow(SurfNum).VentingOpenFactorMultRep = OpenFactorMult;
                }
            } else {
                OpenFactor = 0.0;
                SurfaceWindow(SurfNum).VentingOpenFactorMultRep = -1.0;
            }
        }

        // Venting based on inside-outside air enthalpy difference

        if ((VentCtrlNum == VentCtrNum_Enth || VentCtrlNum == VentCtrNum_AdjEnth) && VentingAllowed) {
            ZoneAirEnthalpy = PsyHFnTdbW(ANZT(ZoneNum), ANZW(ZoneNum));
            // Check whether this surface is an interior wall or not. If Yes, use adjacent zone conditions
            if (VentCtrlNum == VentCtrNum_AdjEnth && MultizoneSurfaceData(i).IndVentControl) {
                OutEnthalpy = PsyHFnTdbW(ANZT(MultizoneZoneData(MultizoneSurfaceData(i).NodeNums[1]).ZoneNum),
                                         ANZW(MultizoneZoneData(MultizoneSurfaceData(i).NodeNums[1]).ZoneNum));
            }
            if (ZoneAirEnthalpy > OutEnthalpy && ANZT(ZoneNum) > VentTemp) {
                OpenFactor = MultizoneSurfaceData(i).Factor;
                // Modulation of OpenFactor
                if (MultizoneSurfaceData(i).IndVentControl) {
                    LimValVentOpenFacMult = MultizoneSurfaceData(i).ModulateFactor;
                    LowerValInOutEnthalDiff = MultizoneSurfaceData(i).LowValueEnth;
                    UpperValInOutEnthalDiff = MultizoneSurfaceData(i).UpValueEnth;
                } else {
                    LimValVentOpenFacMult = MultizoneZoneData(IZ).OpenFactor;
                    LowerValInOutEnthalDiff = MultizoneZoneData(IZ).LowValueEnth;
                    UpperValInOutEnthalDiff = MultizoneZoneData(IZ).UpValueEnth;
                }
                SurfaceWindow(SurfNum).VentingOpenFactorMultRep = 1.0;

                if (LimValVentOpenFacMult != 1.0) {
                    DelEnthal = ZoneAirEnthalpy - OutEnthalpy;
                    if (DelEnthal <= LowerValInOutEnthalDiff) {
                        OpenFactorMult = 1.0;
                    } else if (DelEnthal >= UpperValInOutEnthalDiff) {
                        OpenFactorMult = LimValVentOpenFacMult;
                    } else {
                        OpenFactorMult =
                            LimValVentOpenFacMult + ((UpperValInOutEnthalDiff - DelEnthal) / (UpperValInOutEnthalDiff - LowerValInOutEnthalDiff)) *
                                                        (1 - LimValVentOpenFacMult);
                    }
                    OpenFactor *= OpenFactorMult;
                    SurfaceWindow(SurfNum).VentingOpenFactorMultRep = OpenFactorMult;
                }
            } else {
                OpenFactor = 0.0;
                SurfaceWindow(SurfNum).VentingOpenFactorMultRep = -1.0;
            }
        }

        // Constant venting (opening factor as specified in IDF) - C-PH - added by Philip Haves 3/8/01
        // subject to venting availability

        if (VentCtrlNum == VentCtrNum_Const && VentingAllowed) { // Constant
            OpenFactor = MultizoneSurfaceData(i).Factor;
            SurfaceWindow(SurfNum).VentingOpenFactorMultRep = 1.0;
        }

        if (VentCtrlNum == VentCtrNum_ASH55) {
            if (VentingAllowed && (!BeginEnvrnFlag) && (!WarmupFlag)) {
                PeopleInd = MultizoneZoneData(IZ).ASH55PeopleInd;
                if (PeopleInd > 0 && ThermalComfortData(PeopleInd).ThermalComfortAdaptiveASH5590 != -1) {
                    if (ThermalComfortData(PeopleInd).ThermalComfortOpTemp > ThermalComfortData(PeopleInd).TComfASH55) {
                        OpenFactor = MultizoneSurfaceData(i).Factor;
                        SurfaceWindow(SurfNum).VentingOpenFactorMultRep = 1.0;
                    } else {
                        OpenFactor = 0.0;
                    }
                } else {
                    OpenFactor = 0.0;
                }
            } else {
                OpenFactor = 0.0;
            }
        }

        if (VentCtrlNum == VentCtrNum_CEN15251) {
            if (VentingAllowed && (!BeginEnvrnFlag) && (!WarmupFlag)) {
                PeopleInd = MultizoneZoneData(IZ).CEN15251PeopleInd;
                if (PeopleInd > 0 && ThermalComfortData(PeopleInd).ThermalComfortAdaptiveCEN15251CatI != -1) {
                    if (ThermalComfortData(PeopleInd).ThermalComfortOpTemp > ThermalComfortData(PeopleInd).TComfCEN15251) {
                        OpenFactor = MultizoneSurfaceData(i).Factor;
                        SurfaceWindow(SurfNum).VentingOpenFactorMultRep = 1.0;
                    } else {
                        OpenFactor = 0.0;
                    }
                } else {
                    OpenFactor = 0.0;
                }
            } else {
                OpenFactor = 0.0;
            }
        }

        // No venting, i.e, window/door always closed - added YJH 8 Aug 02

        if (VentCtrlNum == VentCtrNum_Novent) { // Novent
            OpenFactor = 0.0;
            SurfaceWindow(SurfNum).VentingOpenFactorMultRep = -1.0;
        }
    }

    void ValidateDistributionSystem()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       L. Gu, Jan. 2009: allow a desuperheater coil and three heat exchangers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine validates the inputs of distribution system, since node data from a primary airloop
        // are not available in the first call during reading input data of airflownetwork objects.
        // Note: this routine shouldn't be called more than once

        // Using/Aliasing
        using BranchNodeConnections::GetNodeConnectionType;
        using DataAirLoop::AirToZoneNodeInfo;
        using DataAirSystems::PrimaryAirSystem;
        using DataZoneEquipment::ZoneEquipConfig;
        using MixedAir::GetNumOAMixers;
        using MixedAir::GetOAMixerInletNodeNumber;
        using MixedAir::GetOAMixerReliefNodeNumber;
        using SingleDuct::GetHVACSingleDuctSysIndex;
        using namespace DataLoopNode;
        using DataBranchNodeConnections::NodeConnections;
        using DataBranchNodeConnections::NumOfNodeConnections;
        using DataHVACGlobals::NumPrimaryAirSys;
        using DXCoils::SetDXCoilAirLoopNumber;
        using Fans::SetFanAirLoopNumber;
        using HeatingCoils::SetHeatingCoilAirLoopNumber;
        using SplitterComponent::GetSplitterNodeNumbers;
        using SplitterComponent::GetSplitterOutletNumber;
        using ZoneDehumidifier::GetZoneDehumidifierNodeNumber;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("ValidateDistributionSystem: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int k;
        int n;
        int S1;
        int S2;
        int R1;
        int R2;
        bool LocalError;
        Array1D_bool NodeFound;

        bool ErrorsFound(false);
        bool IsNotOK(false);
        bool errFlag(false);
        Array1D_int NodeConnectionType; // Specifies the type of node connection
        std::string CurrentModuleObject;

        // Validate supply and return connections
        NodeFound.dimension(NumOfNodes, false);
        // Validate inlet and outlet nodes for zone exhaust fans
        for (i = 1; i <= AirflowNetworkNumOfExhFan; ++i) {
            NodeFound(MultizoneCompExhaustFanData(i).InletNode) = true;
            NodeFound(MultizoneCompExhaustFanData(i).OutletNode) = true;
        }
        // Validate EPlus Node names and types
        for (i = 1; i <= DisSysNumOfNodes; ++i) {
            if (UtilityRoutines::SameString(DisSysNodeData(i).EPlusName, "") || UtilityRoutines::SameString(DisSysNodeData(i).EPlusName, "Other"))
                continue;
            LocalError = false;
            for (j = 1; j <= NumOfNodes; ++j) { // NodeID
                if (DisSysNodeData(i).EPlusName == NodeID(j)) {
                    DisSysNodeData(i).AirLoopNum = GetAirLoopNumber(j);
                    if (DisSysNodeData(i).AirLoopNum == 0) {
                        ShowSevereError(RoutineName + "The Node or Component Name defined in " + DisSysNodeData(i).Name +
                                        " is not found in the AirLoopHVAC.");
                        ShowContinueError("The entered name is " + DisSysNodeData(i).EPlusName + " in an AirflowNetwork:Distribution:Node object.");
                        ErrorsFound = true;
                    }
                    DisSysNodeData(i).EPlusNodeNum = j;
                    AirflowNetworkNodeData(NumOfNodesMultiZone + i).EPlusNodeNum = j;
                    AirflowNetworkNodeData(NumOfNodesMultiZone + i).AirLoopNum = DisSysNodeData(i).AirLoopNum;
                    NodeFound(j) = true;
                    LocalError = true;
                    break;
                }
            }
            // Check outdoor air node
            if (UtilityRoutines::SameString(DisSysNodeData(i).EPlusType, "OutdoorAir:NodeList") ||
                UtilityRoutines::SameString(DisSysNodeData(i).EPlusType, "OutdoorAir:Node")) {
                if (!LocalError) {
                    ShowSevereError(RoutineName + "The Node or Component Name defined in " + DisSysNodeData(i).Name + " is not found in the " +
                                    DisSysNodeData(i).EPlusType);
                    ShowContinueError("The entered name is " + DisSysNodeData(i).EPlusName + " in an AirflowNetwork:Distribution:Node object.");
                    ErrorsFound = true;
                }
            }
            if (DisSysNodeData(i).EPlusNodeNum == 0) {
                ShowSevereError(RoutineName + "Primary Air Loop Node is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE = " + DisSysNodeData(i).Name);
                ErrorsFound = true;
            }
        }

        // Determine node numbers for zone inlets
        for (i = 1; i <= NumOfZones; ++i) {
            if (!ZoneEquipConfig(i).IsControlled) continue;
            for (j = 1; j <= ZoneEquipConfig(i).NumInletNodes; ++j) {
                for (k = 1; k <= AirflowNetworkNumOfNodes; ++k) {
                    if (ZoneEquipConfig(i).InletNode(j) == AirflowNetworkNodeData(k).EPlusNodeNum) {
                        AirflowNetworkNodeData(k).EPlusTypeNum = EPlusTypeNum_ZIN;
                        break;
                    }
                }
            }
        }

        // Eliminate node not related to AirLoopHVAC
        for (k = 1; k <= NumOfNodeConnections; ++k) {
            if (NodeFound(NodeConnections(k).NodeNumber)) continue;
            if (NodeConnections(k).FluidStream == 2) {
                NodeFound(NodeConnections(k).NodeNumber) = true;
            }
        }

        // Eliminate nodes with fluidtype = water
        for (k = 1; k <= NumOfNodes; ++k) {
            if (NodeFound(k)) continue;
            if (Node(k).FluidType == 2) {
                NodeFound(k) = true;
            }
        }

        // Eliminate local external air node for network
        for (k = 1; k <= NumOfNodes; ++k) {
            if (NodeFound(k)) continue;
            if (Node(k).IsLocalNode) NodeFound(k) = true;
        }

        // Ensure all the nodes used in Eplus are a subset of AirflowNetwork Nodes
        for (i = 1; i <= NumOfNodes; ++i) {
            if (NodeFound(i)) continue;
            // Skip the inlet and outlet nodes of zone dehumidifiers
            if (GetZoneDehumidifierNodeNumber(i)) NodeFound(i) = true;

            for (j = 1; j <= NumOfZones; ++j) {
                if (!ZoneEquipConfig(j).IsControlled) continue;
                if (ZoneEquipConfig(j).ZoneNode == i) {
                    if (ZoneEquipConfig(j).ActualZoneNum > AirflowNetworkNumOfNodes) {
                        ShowSevereError(RoutineName + "'" + NodeID(i) + "' is not defined as an AirflowNetwork:Distribution:Node object.");
                        ShowContinueError("This Node is the zone air node for Zone '" + ZoneEquipConfig(j).ZoneName + "'.");
                        ErrorsFound = true;
                    } else {
                        NodeFound(i) = true;
                        AirflowNetworkNodeData(ZoneEquipConfig(j).ActualZoneNum).EPlusNodeNum = i;
                    }
                    break;
                }
            }

            //   skip nodes that are not part of an airflow network

            //     DX COIL CONDENSER NODE TEST:
            //     Outside air nodes are used for DX coil condenser inlet nodes, these are specified in an outside air node or
            //     OutdoorAir:NodeList object (and classified with NodeConnectionType as OutsideAir). In addition,
            //     this same node is specified in a Coil:DX:CoolingBypassFactorEmpirical object (and classified with
            //     NodeConnectionType as OutsideAirReference). In the NodeConnectionType structure, both of these nodes have a
            //     unique index but have the same node number. The Outside Air Node will usually be listed first. Search for all
            //     indexes with the same node number and check if it is classified as NodeConnectionType = OutsideAirReference.
            //     Mark this node as found since it is not used in an airflownetwork simulation.
            //     Example (using AirflowNetwork_MultiZone_SmallOffice.idf with a single OA Mixer):
            //             (the example shown below is identical to AirflowNetwork_SimpleHouse.idf with no OA Mixer except
            //              that the NodeConnections indexes are (7) and (31), respectively and the NodeNumber = 6)
            //   The GetNodeConnectionType CALL below returns NodeConnectionType_OutsideAir = 7 and NodeConnectionType_OutsideAirReference = 14.
            //     NodeConnections info from OUTSIDE AIR NODE object read:
            //     NodeConnections(9)NodeNumber      = 10
            //     NodeConnections(9)NodeName        = ACDXCOIL 1 CONDENSER NODE
            //     NodeConnections(9)ObjectType      = OUTSIDE AIR NODE
            //     NodeConnections(9)ObjectName      = OUTSIDE AIR NODE
            //     NodeConnections(9)ConnectionType  = OutsideAir
            //     NodeConnections info from Coil:DX:CoolingBypassFactorEmpirical object read:
            //     NodeConnections(64)NodeNumber     = 10
            //     NodeConnections(64)NodeName       = ACDXCOIL 1 CONDENSER NODE
            //     NodeConnections(64)ObjectType     = COIL:DX:COOLINGBYPASSFACTOREMPIRICAL
            //     NodeConnections(64)ObjectName     = ACDXCOIL 1
            //     NodeConnections(64)ConnectionType = OutsideAirReference

            errFlag = false;
            GetNodeConnectionType(i, NodeConnectionType, errFlag); // Gets all connection types for a given node number
            if (errFlag) {
                ShowContinueError("...occurs in Airflow Network simulation.");
            } else {
                //   skip nodes for air cooled condensers
                for (j = 1; j <= isize(NodeConnectionType); ++j) {
                    if (NodeConnectionType(j) == NodeConnectionType_OutsideAirReference) {
                        NodeFound(i) = true;
                    }
                }
            }

            if (!NodeFound(i)) {
                // Check if this node is the OA relief node. For the time being, OA relief node is not used
                if (GetNumOAMixers() > 1) {
                    //						ShowSevereError( RoutineName + "Only one OutdoorAir:Mixer is allowed in the
                    // AirflowNetwork model." ); 						ErrorsFound = true;
                    int OAFanNum;
                    int OARelNum;
                    int OAMixerNum;

                    for (OAFanNum = 1; OAFanNum <= NumOfOAFans; ++OAFanNum) {
                        DisSysCompOutdoorAirData(OAFanNum).InletNode = GetOAMixerInletNodeNumber(DisSysCompOutdoorAirData(OAFanNum).OAMixerNum);
                        //							NodeFound( DisSysCompOutdoorAirData( OAFanNum ).InletNode ) = true;
                    }
                    for (OARelNum = 1; OARelNum <= NumOfReliefFans; ++OARelNum) {
                        DisSysCompReliefAirData(OARelNum).OutletNode = GetOAMixerInletNodeNumber(DisSysCompReliefAirData(OARelNum).OAMixerNum);
                        //							NodeFound( DisSysCompOutdoorAirData( OAFanNum ).InletNode ) = true;
                    }
                    // Check NodeFound status
                    for (OAMixerNum = 1; OAMixerNum <= GetNumOAMixers(); ++OAMixerNum) {
                        if (i == GetOAMixerReliefNodeNumber(OAMixerNum)) {
                            NodeFound(i) = true;
                            break;
                        } else if (i == GetOAMixerInletNodeNumber(OAMixerNum)) {
                            NodeFound(i) = true;
                            break;
                        } else {
                            if (OAMixerNum == GetNumOAMixers()) {
                                ShowSevereError(RoutineName + "'" + NodeID(i) + "' is not defined as an AirflowNetwork:Distribution:Node object.");
                                ErrorsFound = true;
                            }
                        }
                    }
                } else if (GetNumOAMixers() == 0) {
                    ShowSevereError(RoutineName + "'" + NodeID(i) + "' is not defined as an AirflowNetwork:Distribution:Node object.");
                    ErrorsFound = true;
                } else {
                    // TODO: I fail to see how you could enter this block given than NumOAMixers (returned by GetNumOAMixers())
                    // is initialized to zero, and we check above if '> 0' or '== 0'
                    if (NumOfOAFans == 1 && DisSysCompOutdoorAirData(1).InletNode == 0) {
                        DisSysCompOutdoorAirData(1).InletNode = GetOAMixerInletNodeNumber(1);
                    }
                    if (NumOfReliefFans == 1 && DisSysCompReliefAirData(1).OutletNode == 0) {
                        DisSysCompReliefAirData(1).OutletNode = GetOAMixerInletNodeNumber(1);
                    }
                    if (i == GetOAMixerReliefNodeNumber(1)) {
                        NodeFound(i) = true;
                    } else if (i == GetOAMixerInletNodeNumber(1)) {
                        NodeFound(i) = true;
                    } else {
                        ShowSevereError(RoutineName + "'" + NodeID(i) + "' is not defined as an AirflowNetwork:Distribution:Node object.");
                        ErrorsFound = true;
                    }
                }
            }
        }
        NodeFound.deallocate();

        // Assign AirLoop Number to every node and linkage
        // Zone first
        for (i = 1; i <= AirflowNetworkNumOfZones; i++) {
            for (j = 1; j <= NumOfZones; j++) {
                if (!ZoneEquipConfig(j).IsControlled) continue;
                if ((MultizoneZoneData(i).ZoneNum == j) && (ZoneEquipConfig(j).NumInletNodes > 0)) {
                    // No multiple Airloop
                    AirflowNetworkNodeData(i).AirLoopNum = ZoneEquipConfig(j).InletNodeAirLoopNum(1);
                }
            }
        }
        // Air Distribution system
        for (i = AirflowNetworkNumOfSurfaces + 1; i <= AirflowNetworkNumOfLinks; ++i) {
            j = AirflowNetworkLinkageData(i).NodeNums[0];
            k = AirflowNetworkLinkageData(i).NodeNums[1];
            if (AirflowNetworkNodeData(j).AirLoopNum == 0 && AirflowNetworkNodeData(k).AirLoopNum == 0) {
                // Error messaage
                ShowSevereError("ValidateDistributionSystem: AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = " + AirflowNetworkLinkageData(i).Name +
                                " is not valid for AirLoopNum assignment");
                ShowContinueError("AirLoopNum is not found in both nodes for the linkage: " + AirflowNetworkLinkageData(i).NodeNames[0] + " and " +
                                  AirflowNetworkLinkageData(i).NodeNames[1]);
                ShowContinueError("Please ensure one of two AIRFLOWNETWORK:DISTRIBUTION:NODEs in the first AIRFLOWNETWORK:DISTRIBUTION:LINKAGE "
                                  "object should be defined as EnergyPlus NodeID.");
                ErrorsFound = true;
            }
            if (AirflowNetworkNodeData(j).AirLoopNum > 0 && AirflowNetworkNodeData(k).AirLoopNum == 0) {
                AirflowNetworkNodeData(k).AirLoopNum = AirflowNetworkNodeData(j).AirLoopNum;
            }
            if (AirflowNetworkNodeData(j).AirLoopNum == 0 && AirflowNetworkNodeData(k).AirLoopNum > 0) {
                AirflowNetworkNodeData(j).AirLoopNum = AirflowNetworkNodeData(k).AirLoopNum;
            }
            if (AirflowNetworkNodeData(j).AirLoopNum == AirflowNetworkNodeData(k).AirLoopNum) {
                AirflowNetworkLinkageData(i).AirLoopNum = AirflowNetworkNodeData(j).AirLoopNum;
            }
            if (AirflowNetworkNodeData(j).AirLoopNum != AirflowNetworkNodeData(k).AirLoopNum && AirflowNetworkNodeData(j).AirLoopNum > 0 &&
                AirflowNetworkNodeData(k).AirLoopNum > 0) {
                AirflowNetworkLinkageData(i).AirLoopNum = AirflowNetworkNodeData(j).AirLoopNum;
                ShowSevereError("The AirLoopNum defined in both AIRFLOWNETWORK:DISTRIBUTION:NODE objects in " + AirflowNetworkLinkageData(i).Name +
                                " are not the same. Please make sure both nodes should be listed in the same AirLoop as a valid linkage.");
                ShowContinueError("AirLoop defined in " + AirflowNetworkNodeData(j).Name + " is " +
                                  PrimaryAirSystem(AirflowNetworkNodeData(j).AirLoopNum).Name + ", and AirLoop defined in " +
                                  AirflowNetworkNodeData(k).Name + " is " + PrimaryAirSystem(AirflowNetworkNodeData(k).AirLoopNum).Name);
                ErrorsFound = true;
            }
            // Set AirLoopNum to fans and coils
            if (AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).EPlusTypeNum == EPlusTypeNum_FAN) {
                n = DisSysCompCVFData(AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).TypeNum).FanIndex;
                DisSysCompCVFData(AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).TypeNum).AirLoopNum =
                    AirflowNetworkLinkageData(i).AirLoopNum;
                if (DisSysCompCVFData(AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).TypeNum).FanModelFlag) {
                    HVACFan::fanObjs[DisSysCompCVFData(AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).TypeNum).FanIndex]->AirLoopNum =
                        AirflowNetworkLinkageData(i).AirLoopNum;
                } else {
                    SetFanAirLoopNumber(n, AirflowNetworkLinkageData(i).AirLoopNum);
                }
            }
            if (AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).EPlusTypeNum == EPlusTypeNum_COI) {
                DisSysCompCoilData(AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).TypeNum).AirLoopNum =
                    AirflowNetworkLinkageData(i).AirLoopNum;
            }
        }

        // Validate coil name and type
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Coil";
        MultiSpeedHPIndicator = 0;
        for (i = 1; i <= DisSysNumOfCoils; ++i) {
            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(DisSysCompCoilData(i).EPlusType));

                if (SELECT_CASE_var == "COIL:COOLING:DX:SINGLESPEED") {
                    ValidateComponent("Coil:Cooling:DX:SingleSpeed", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(DisSysCompCoilData(i).Name, DisSysCompCoilData(i).AirLoopNum);
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:DX:SINGLESPEED") {
                    ValidateComponent("Coil:Heating:DX:SingleSpeed", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(DisSysCompCoilData(i).Name, DisSysCompCoilData(i).AirLoopNum);
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:FUEL") {
                    ValidateComponent("Coil:Heating:Fuel", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetHeatingCoilAirLoopNumber(DisSysCompCoilData(i).Name, DisSysCompCoilData(i).AirLoopNum, ErrorsFound);
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:ELECTRIC") {
                    ValidateComponent("Coil:Heating:Electric", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetHeatingCoilAirLoopNumber(DisSysCompCoilData(i).Name, DisSysCompCoilData(i).AirLoopNum, ErrorsFound);
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:WATER") {
                    ValidateComponent("Coil:Cooling:Water", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:WATER") {
                    ValidateComponent("Coil:Heating:Water", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY") {
                    ValidateComponent("Coil:Cooling:Water:DetailedGeometry", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") {
                    ValidateComponent(
                        "Coil:Cooling:DX:TwoStageWithHumidityControlMode", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(DisSysCompCoilData(i).Name, DisSysCompCoilData(i).AirLoopNum);
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:DX:MULTISPEED") {
                    ValidateComponent("Coil:Cooling:DX:MultiSpeed", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    ++MultiSpeedHPIndicator;
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(DisSysCompCoilData(i).Name, DisSysCompCoilData(i).AirLoopNum);
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:DX:MULTISPEED") {
                    ValidateComponent("Coil:Heating:DX:MultiSpeed", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    ++MultiSpeedHPIndicator;
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(DisSysCompCoilData(i).Name, DisSysCompCoilData(i).AirLoopNum);
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:DESUPERHEATER") {
                    ValidateComponent("Coil:Heating:Desuperheater", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:DX:TWOSPEED") {
                    ValidateComponent("Coil:Cooling:DX:TwoSpeed", DisSysCompCoilData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(DisSysCompCoilData(i).Name, DisSysCompCoilData(i).AirLoopNum);
                    }

                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + " Invalid coil type = " + DisSysCompCoilData(i).Name);
                    ErrorsFound = true;
                }
            }
        }

        // Validate terminal unit name and type
        for (i = 1; i <= DisSysNumOfTermUnits; ++i) {
            if (UtilityRoutines::SameString(DisSysCompTermUnitData(i).EPlusType, "AirTerminal:SingleDuct:ConstantVolume:Reheat") ||
                UtilityRoutines::SameString(DisSysCompTermUnitData(i).EPlusType, "AirTerminal:SingleDuct:VAV:Reheat")) {
                LocalError = false;
                if (UtilityRoutines::SameString(DisSysCompTermUnitData(i).EPlusType, "AirTerminal:SingleDuct:ConstantVolume:Reheat"))
                    GetHVACSingleDuctSysIndex(DisSysCompTermUnitData(i).Name, n, LocalError, "AirflowNetwork:Distribution:Component:TerminalUnit");
                if (UtilityRoutines::SameString(DisSysCompTermUnitData(i).EPlusType, "AirTerminal:SingleDuct:VAV:Reheat"))
                    GetHVACSingleDuctSysIndex(DisSysCompTermUnitData(i).Name,
                                              n,
                                              LocalError,
                                              "AirflowNetwork:Distribution:Component:TerminalUnit",
                                              DisSysCompTermUnitData(i).DamperInletNode,
                                              DisSysCompTermUnitData(i).DamperOutletNode);
                if (LocalError) ErrorsFound = true;
                if (VAVSystem) {
                    for (j = 1; j <= DisSysNumOfCVFs; j++) {
                        if (DisSysCompCVFData(j).FanTypeNum == FanType_SimpleVAV) {
                            if (DisSysCompCVFData(j).AirLoopNum == DisSysCompTermUnitData(i).AirLoopNum &&
                                !UtilityRoutines::SameString(DisSysCompTermUnitData(i).EPlusType, "AirTerminal:SingleDuct:VAV:Reheat")) {
                                ShowSevereError(RoutineName + CurrentModuleObject +
                                                " Invalid terminal type for a VAV system = " + DisSysCompTermUnitData(i).Name);
                                ShowContinueError("The input type = " + DisSysCompTermUnitData(i).EPlusType);
                                ShowContinueError("A VAV system requires all terminal units with type = AirTerminal:SingleDuct:VAV:Reheat");
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            } else {
                ShowSevereError(RoutineName + "AIRFLOWNETWORK:DISTRIBUTION:COMPONENT TERMINAL UNIT: Invalid Terminal unit type = " +
                                DisSysCompTermUnitData(i).Name);
                ErrorsFound = true;
            }
        }

        // Validate heat exchanger name and type
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:HeatExchanger";
        for (i = 1; i <= DisSysNumOfHXs; ++i) {
            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(DisSysCompHXData(i).EPlusType));

                if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:FLATPLATE") {
                    ValidateComponent("HeatExchanger:AirToAir:FlatPlate", DisSysCompHXData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT") {
                    ValidateComponent(
                        "HeatExchanger:AirToAir:SensibleAndLatent", DisSysCompHXData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "HEATEXCHANGER:DESICCANT:BALANCEDFLOW") {
                    ValidateComponent("HeatExchanger:Desiccant:BalancedFlow", DisSysCompHXData(i).Name, IsNotOK, RoutineName + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + " Invalid heat exchanger type = " + DisSysCompHXData(i).EPlusType);
                    ErrorsFound = true;
                }
            }
        }

        // Assign supply and return connection
        for (j = 1; j <= NumPrimaryAirSys; ++j) {
            S1 = 0;
            S2 = 0;
            R1 = 0;
            R2 = 0;
            for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
                if (AirflowNetworkNodeData(i).EPlusNodeNum == AirToZoneNodeInfo(j).AirLoopSupplyNodeNum(1)) S1 = i;
                if (AirflowNetworkNodeData(i).EPlusNodeNum == AirToZoneNodeInfo(j).ZoneEquipSupplyNodeNum(1)) S2 = i;
                if (AirflowNetworkNodeData(i).EPlusNodeNum == AirToZoneNodeInfo(j).ZoneEquipReturnNodeNum(1)) R1 = i;
                if (AirflowNetworkNodeData(i).EPlusNodeNum == AirToZoneNodeInfo(j).AirLoopReturnNodeNum(1)) R2 = i;
            }
            for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
                if (AirflowNetworkLinkageData(i).NodeNums[0] == R1 && AirflowNetworkLinkageData(i).NodeNums[1] == R2) {
                    AirflowNetworkLinkageData(i).ConnectionFlag = EPlusTypeNum_RCN;
                }
                if (AirflowNetworkLinkageData(i).NodeNums[0] == S1 && AirflowNetworkLinkageData(i).NodeNums[1] == S2) {
                    AirflowNetworkLinkageData(i).ConnectionFlag = EPlusTypeNum_SCN;
                }
            }
        }

        // Assign fan inlet and outlet node, and coil outlet
        for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            j = AirflowNetworkLinkageData(i).CompNum;
            if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_CVF) {
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusTypeNum == 0)
                    AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusTypeNum = EPlusTypeNum_FIN;
                AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusTypeNum = EPlusTypeNum_FOU;
            }
            if (AirflowNetworkCompData(j).EPlusTypeNum == EPlusTypeNum_COI) {
                AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusTypeNum = EPlusTypeNum_COU;
            }
            if (AirflowNetworkCompData(j).EPlusTypeNum == EPlusTypeNum_HEX) {
                AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusTypeNum = EPlusTypeNum_HXO;
            }
            if (AirflowNetworkCompData(j).CompTypeNum == CompTypeNum_TMU) {
                if (DisSysCompTermUnitData(AirflowNetworkCompData(j).TypeNum).DamperInletNode > 0) {
                    if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusNodeNum ==
                            DisSysCompTermUnitData(AirflowNetworkCompData(j).TypeNum).DamperInletNode &&
                        AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusNodeNum ==
                            DisSysCompTermUnitData(AirflowNetworkCompData(j).TypeNum).DamperOutletNode) {
                        AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusTypeNum = EPlusTypeNum_DIN;
                        AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusTypeNum = EPlusTypeNum_DOU;
                        AirflowNetworkLinkageData(i).VAVTermDamper = true;
                    }
                }
            }
        }

        // Validate the position of constant pressure drop component
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:ConstantPressureDrop";
        for (i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            if (AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum == CompTypeNum_CPD) {
                for (j = 1; j <= AirflowNetworkNumOfLinks; ++j) {
                    if (AirflowNetworkLinkageData(i).NodeNums[0] == AirflowNetworkLinkageData(j).NodeNums[1]) {
                        if (AirflowNetworkCompData(AirflowNetworkLinkageData(j).CompNum).CompTypeNum != CompTypeNum_DWC) {
                            ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object (" + AirflowNetworkLinkageData(i).CompName + ')');
                            ShowContinueError("must connect a duct component upstream and not " + AirflowNetworkLinkageData(j).Name);
                            ErrorsFound = true;
                        }
                    }
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusTypeNum == EPlusTypeNum_SPL) {
                    ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object (" + AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError("does not allow a AirLoopHVAC:ZoneSplitter node = " +
                                      AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).Name);
                    ErrorsFound = true;
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusTypeNum == EPlusTypeNum_SPL) {
                    ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object (" + AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError("does not allow a AirLoopHVAC:ZoneSplitter node = " +
                                      AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).Name);
                    ErrorsFound = true;
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusTypeNum == EPlusTypeNum_MIX) {
                    ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object (" + AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError("does not allow a AirLoopHVAC:ZoneMixer node = " +
                                      AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).Name);
                    ErrorsFound = true;
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusTypeNum == EPlusTypeNum_MIX) {
                    ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object (" + AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError("does not allow a AirLoopHVAC:ZoneMixer node = " +
                                      AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).Name);
                    ErrorsFound = true;
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusNodeNum > 0) {
                    ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object (" + AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError("does not allow to connect an EnergyPlus node = " +
                                      AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).Name);
                    ErrorsFound = true;
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusNodeNum > 0) {
                    ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object (" + AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError("does not allow to connect an EnergyPlus node = " +
                                      AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).Name);
                    ErrorsFound = true;
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum > 0) {
                    ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object (" + AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError("does not allow to connect an EnergyPlus zone = " +
                                      AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).Name);
                    ErrorsFound = true;
                }
                if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).EPlusZoneNum > 0) {
                    ShowSevereError(RoutineName + "An " + CurrentModuleObject + " object (" + AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError("does not allow to connect an EnergyPlus zone = " +
                                      AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[1]).Name);
                    ErrorsFound = true;
                }
            }
        }

        for (i = NumOfNodesMultiZone + 1; i <= AirflowNetworkNumOfNodes; ++i) {
            if (AirflowNetworkNodeData(i).EPlusTypeNum == EPlusTypeNum_SPL) {
                LocalError = false;
                j = GetSplitterOutletNumber("", 1, LocalError);
                SplitterNodeNumbers.allocate(j + 2);
                SplitterNodeNumbers = GetSplitterNodeNumbers("", 1, LocalError);
                if (LocalError) ErrorsFound = true;
            }
        }

        // Assigning inlet and outlet nodes for a splitter
        for (i = 1; i <= AirflowNetworkNumOfNodes; ++i) {
            if (AirflowNetworkNodeData(i).EPlusNodeNum == SplitterNodeNumbers(1)) {
                if (AirflowNetworkNodeData(i).EPlusTypeNum == 0) AirflowNetworkNodeData(i).EPlusTypeNum = EPlusTypeNum_SPI;
            }
            for (j = 1; j <= SplitterNodeNumbers(2); ++j) {
                if (AirflowNetworkNodeData(i).EPlusNodeNum == SplitterNodeNumbers(j + 2)) {
                    if (AirflowNetworkNodeData(i).EPlusTypeNum == 0) AirflowNetworkNodeData(i).EPlusTypeNum = EPlusTypeNum_SPO;
                }
            }
        }

        // Add additional output variables
        if (DisSysNumOfCVFs > 1) {
            bool OnOffFanFlag = false;
            for (i = 1; i <= DisSysNumOfCVFs; i++) {
                if (DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff && !DisSysCompCVFData(i).FanModelFlag) {
                    OnOffFanFlag = true;
                    break;
                }
                if (DisSysCompCVFData(i).FanModelFlag && DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff) {
                    int fanIndex = HVACFan::getFanObjectVectorIndex(DisSysCompCVFData(i).Name);
                    if (HVACFan::fanObjs[fanIndex]->AirPathFlag) {
                        DisSysCompCVFData(i).FanTypeNum = FanType_SimpleConstVolume;
                    } else {
                        OnOffFanFlag = true;
                        break;
                    }
                }
            }
            if (OnOffFanFlag) {
                for (j = 1; j <= AirflowNetworkNumOfZones; ++j) {
                    if (!ZoneEquipConfig(AirflowNetworkNodeData(j).EPlusZoneNum).IsControlled) continue;
                    for (i = 1; i <= DisSysNumOfCVFs; i++) {
                        if (DisSysCompCVFData(i).AirLoopNum == AirflowNetworkNodeData(j).AirLoopNum &&
                            DisSysCompCVFData(i).FanTypeNum != FanType_SimpleOnOff) {
                            SetupOutputVariable("AFN Node Total Pressure",
                                                OutputProcessor::Unit::Pa,
                                                AirflowNetworkNodeSimu(j).PZ,
                                                "System",
                                                "Average",
                                                AirflowNetworkNodeData(j).Name);
                        }
                    }
                }
                for (i = 1; i <= NumOfLinksMultiZone; ++i) {
                    if (!ZoneEquipConfig(AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).EPlusZoneNum).IsControlled) continue;
                    for (j = 1; j <= DisSysNumOfCVFs; j++) {
                        if (DisSysCompCVFData(j).AirLoopNum == AirflowNetworkNodeData(AirflowNetworkLinkageData(i).NodeNums[0]).AirLoopNum &&
                            DisSysCompCVFData(j).FanTypeNum != FanType_SimpleOnOff) {
                            SetupOutputVariable("AFN Linkage Node 1 to Node 2 Mass Flow Rate",
                                                OutputProcessor::Unit::kg_s,
                                                AirflowNetworkLinkReport(i).FLOW,
                                                "System",
                                                "Average",
                                                AirflowNetworkLinkageData(i).Name);
                            SetupOutputVariable("AFN Linkage Node 2 to Node 1 Mass Flow Rate",
                                                OutputProcessor::Unit::kg_s,
                                                AirflowNetworkLinkReport(i).FLOW2,
                                                "System",
                                                "Average",
                                                AirflowNetworkLinkageData(i).Name);
                            SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                                                OutputProcessor::Unit::m3_s,
                                                AirflowNetworkLinkReport(i).VolFLOW,
                                                "System",
                                                "Average",
                                                AirflowNetworkLinkageData(i).Name);
                            SetupOutputVariable("AFN Linkage Node 2 to Node 1 Volume Flow Rate",
                                                OutputProcessor::Unit::m3_s,
                                                AirflowNetworkLinkReport(i).VolFLOW2,
                                                "System",
                                                "Average",
                                                AirflowNetworkLinkageData(i).Name);
                            SetupOutputVariable("AFN Linkage Node 1 to Node 2 Pressure Difference",
                                                OutputProcessor::Unit::Pa,
                                                AirflowNetworkLinkSimu(i).DP,
                                                "System",
                                                "Average",
                                                AirflowNetworkLinkageData(i).Name);
                        }
                    }
                }
            }
        }
        bool FanModelConstFlag = false;
        for (i = 1; i <= DisSysNumOfCVFs; i++) {
            if (DisSysCompCVFData(i).FanModelFlag) {
                int fanIndex = HVACFan::getFanObjectVectorIndex(DisSysCompCVFData(i).Name);
                if (DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff && HVACFan::fanObjs[fanIndex]->AirPathFlag) {
                    DisSysCompCVFData(i).FanTypeNum = FanType_SimpleConstVolume;
                    SupplyFanType = FanType_SimpleConstVolume;
                    FanModelConstFlag = true;
                    break;
                }
            }
        }
        if (FanModelConstFlag) {
            for (i = 1; i <= AirflowNetworkNumOfSurfaces; ++i) {
                if (SupplyFanType == FanType_SimpleConstVolume) {
                    SetupOutputVariable("AFN Linkage Node 1 to Node 2 Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        AirflowNetworkLinkReport(i).FLOW,
                                        "System",
                                        "Average",
                                        AirflowNetworkLinkageData(i).Name);
                    SetupOutputVariable("AFN Linkage Node 2 to Node 1 Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        AirflowNetworkLinkReport(i).FLOW2,
                                        "System",
                                        "Average",
                                        AirflowNetworkLinkageData(i).Name);
                    SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        AirflowNetworkLinkReport(i).VolFLOW,
                                        "System",
                                        "Average",
                                        AirflowNetworkLinkageData(i).Name);
                    SetupOutputVariable("AFN Linkage Node 2 to Node 1 Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        AirflowNetworkLinkReport(i).VolFLOW2,
                                        "System",
                                        "Average",
                                        AirflowNetworkLinkageData(i).Name);
                    SetupOutputVariable("AFN Linkage Node 1 to Node 2 Pressure Difference",
                                        OutputProcessor::Unit::Pa,
                                        AirflowNetworkLinkSimu(i).DP,
                                        "System",
                                        "Average",
                                        AirflowNetworkLinkageData(i).Name);
                }
            }
        }

        // Add AirLoopNum to pressure control object
        for (i = 1; i <= NumOfPressureControllers; ++i) {
            for (j = 1; j <= NumOfZones; ++j) {
                if (PressureControllerData(i).ZoneNum == j) {
                    for (k = 1; k <= ZoneEquipConfig(j).NumInletNodes; ++k) {
                        if (ZoneEquipConfig(j).InletNodeAirLoopNum(k) > 0) {
                            PressureControllerData(i).AirLoopNum = ZoneEquipConfig(j).InletNodeAirLoopNum(k);
                            if (PressureControllerData(i).ControlTypeSet == PressureCtrlRelief) {
                                PressureControllerData(i).OANodeNum = PrimaryAirSystem(PressureControllerData(i).AirLoopNum).OAMixOAInNodeNum;
                                for (n = 1; n <= NumOfReliefFans; ++n) {
                                    if (DisSysCompReliefAirData(n).OutletNode == PressureControllerData(i).OANodeNum) {
                                        DisSysCompReliefAirData(n).PressCtrlNum = i;
                                    }
                                }
                            }
                            if (PressureControllerData(i).ControlTypeSet == PressureCtrlExhaust) {
                                PressureControllerData(i).OANodeNum = ZoneEquipConfig(PressureControllerData(i).ZoneNum).ExhaustNode(1);
                                for (n = 1; n <= AirflowNetworkNumOfExhFan; ++n) {
                                    if (MultizoneCompExhaustFanData(n).EPlusZoneNum == PressureControllerData(i).ZoneNum) {
                                        MultizoneCompExhaustFanData(n).PressCtrlNum = i;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Check number of fans specified in an AirLoop #6748
        int BranchNum;
        int CompNum;
        int NumOfFans;
        std::string FanNames;
        for (BranchNum = 1; BranchNum <= PrimaryAirSystem(1).NumBranches; ++BranchNum) {
            NumOfFans = 0;
            FanNames = "";
            for (CompNum = 1; CompNum <= PrimaryAirSystem(1).Branch(BranchNum).TotalComponents; ++CompNum) {
                if (UtilityRoutines::SameString(PrimaryAirSystem(1).Branch(BranchNum).Comp(CompNum).TypeOf, "Fan:ConstantVolume") ||
                    UtilityRoutines::SameString(PrimaryAirSystem(1).Branch(BranchNum).Comp(CompNum).TypeOf, "Fan:OnOff") ||
                    UtilityRoutines::SameString(PrimaryAirSystem(1).Branch(BranchNum).Comp(CompNum).TypeOf, "Fan:VariableVolume")) {
                    NumOfFans++;
                    if (NumOfFans > 1) {
                        FanNames += PrimaryAirSystem(1).Branch(BranchNum).Comp(CompNum).Name;
                        break;
                    } else {
                        FanNames += PrimaryAirSystem(1).Branch(BranchNum).Comp(CompNum).Name + ",";
                    }
                }
            }
            if (NumOfFans > 1) break;
        }
        if (NumOfFans > 1) {
            ShowSevereError(RoutineName + "An AirLoop branch, " + PrimaryAirSystem(1).Branch(BranchNum).Name + ", has two or more fans: " + FanNames);
            ShowContinueError(
                "The AirflowNetwork model allows a single supply fan in an AirLoop only. Please make changes in the input file accordingly.");
            ErrorsFound = true;
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Program terminates for preceding reason(s).");
        }
    }

    void ValidateFanFlowRate()
    {

        // Catch a fan flow rate from EPlus input file and add a flag for VAV terminal damper
        for (int i = 1; i <= AirflowNetworkNumOfLinks; ++i) {
            {
                auto const SELECT_CASE_var(AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).CompTypeNum);
                if (SELECT_CASE_var == CompTypeNum_CVF) { // 'CVF'
                    int typeNum = AirflowNetworkCompData(AirflowNetworkLinkageData(i).CompNum).TypeNum;
                    if (DisSysCompCVFData(typeNum).FanTypeNum == FanType_SimpleVAV) {
                        if (DisSysCompCVFData(typeNum).FanModelFlag) {
                            DisSysCompCVFData(typeNum).MaxAirMassFlowRate =
                                HVACFan::fanObjs[DisSysCompCVFData(typeNum).FanIndex]->designAirVolFlowRate * StdRhoAir;
                        } else {
                            Real64 FanFlow; // Return type
                            GetFanVolFlow(DisSysCompCVFData(typeNum).FanIndex, FanFlow);
                            DisSysCompCVFData(typeNum).MaxAirMassFlowRate = FanFlow * StdRhoAir;
                        }
                    }
                } else if (SELECT_CASE_var == CompTypeNum_FAN) { //'FAN'
                                                                 // Check ventilation status for large openings
                } else if (SELECT_CASE_var == CompTypeNum_SOP) { //'Simple opening'
                } else if (SELECT_CASE_var == CompTypeNum_TMU) { // Terminal unit
                } else {
                }
            }
        }
    }

    void ValidateExhaustFanInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Dec. 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine validate zone exhaust fan and associated surface

        // Using/Aliasing
        using DataZoneEquipment::ZoneEquipList;
        using DataZoneEquipment::ZoneExhaustFan_Num;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("ValidateExhaustFanInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int k;
        static bool OneTimeFlag(true);
        static bool ErrorsFound(false);
        bool found;
        int EquipTypeNum; // Equipment type number
        std::string CurrentModuleObject;

        // Validate supply and return connections
        if (OneTimeFlag) {
            CurrentModuleObject = "AirflowNetwork:MultiZone:Component:ZoneExhaustFan";
            if (std::any_of(
                    ZoneEquipConfig.begin(), ZoneEquipConfig.end(), [](DataZoneEquipment::EquipConfiguration const &e) { return e.IsControlled; })) {
                AirflowNetworkZoneExhaustFan.dimension(NumOfZones, false);
            }
            // Ensure the number of exhaust fan defined in the AirflowNetwork model matches the number of Zone Exhaust Fan objects
            if (NumOfExhaustFans != AirflowNetworkNumOfExhFan) {
                ShowSevereError(RoutineName + "The number of " + CurrentModuleObject +
                                " is not equal to the number of Fan:ZoneExhaust fans defined in ZoneHVAC:EquipmentConnections");
                ShowContinueError("The number of " + CurrentModuleObject + " is " + RoundSigDigits(AirflowNetworkNumOfExhFan));
                ShowContinueError("The number of Zone exhaust fans defined in ZoneHVAC:EquipmentConnections is " + RoundSigDigits(NumOfExhaustFans));
                ErrorsFound = true;
            }

            for (i = 1; i <= AirflowNetworkNumOfExhFan; ++i) {
                // Get zone number
                for (j = 1; j <= NumOfZones; ++j) {
                    if (!ZoneEquipConfig(j).IsControlled) continue;
                    for (k = 1; k <= ZoneEquipConfig(j).NumExhaustNodes; ++k) {
                        if (ZoneEquipConfig(j).ExhaustNode(k) == MultizoneCompExhaustFanData(i).InletNode) {
                            MultizoneCompExhaustFanData(i).EPlusZoneNum = ZoneEquipConfig(j).ActualZoneNum;
                            break;
                        }
                    }
                }
                if (MultizoneCompExhaustFanData(i).EPlusZoneNum == 0) {
                    ShowSevereError(RoutineName + "Zone name in " + CurrentModuleObject + "  = " + MultizoneCompExhaustFanData(i).Name +
                                    " does not match the zone name in ZoneHVAC:EquipmentConnections");
                    ErrorsFound = true;
                }
                // Ensure a surface using zone exhaust fan to expose to the same zone
                found = false;
                for (j = 1; j <= AirflowNetworkNumOfSurfaces; ++j) {
                    if (UtilityRoutines::SameString(MultizoneSurfaceData(j).OpeningName, MultizoneCompExhaustFanData(i).Name)) {
                        found = true;
                        if (Surface(MultizoneSurfaceData(j).SurfNum).ExtBoundCond != ExternalEnvironment &&
                            !(Surface(MultizoneSurfaceData(i).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt &&
                              Surface(MultizoneSurfaceData(i).SurfNum).ExtWind)) {
                            ShowSevereError(RoutineName + "The surface using " + CurrentModuleObject +
                                            " is not an exterior surface: " + MultizoneSurfaceData(j).SurfName);
                            ErrorsFound = true;
                        }
                        break;
                    }
                }
                if (!found) {
                    ShowSevereError(CurrentModuleObject + "  = " + MultizoneCompExhaustFanData(i).Name + " is defined and never used.");
                    ErrorsFound = true;
                } else {
                    if (MultizoneCompExhaustFanData(i).EPlusZoneNum != Surface(MultizoneSurfaceData(j).SurfNum).Zone) {
                        ShowSevereError(RoutineName + "Zone name in " + CurrentModuleObject + "  = " + MultizoneCompExhaustFanData(i).Name +
                                        " does not match the zone name");
                        ShowContinueError("the surface is exposed to " + Surface(MultizoneSurfaceData(j).SurfNum).Name);
                        ErrorsFound = true;
                    } else {
                        AirflowNetworkZoneExhaustFan(MultizoneCompExhaustFanData(i).EPlusZoneNum) = true;
                    }
                }
            }

            // Ensure all zone exhaust fans are defined
            for (j = 1; j <= NumOfZones; ++j) {
                if (!ZoneEquipConfig(j).IsControlled) continue;
                for (EquipTypeNum = 1; EquipTypeNum <= ZoneEquipList(j).NumOfEquipTypes; ++EquipTypeNum) {
                    if (ZoneEquipList(j).EquipType_Num(EquipTypeNum) == ZoneExhaustFan_Num) {
                        found = false;
                        for (k = 1; k <= ZoneEquipConfig(j).NumExhaustNodes; ++k) {
                            for (i = 1; i <= AirflowNetworkNumOfExhFan; ++i) {
                                if (ZoneEquipConfig(j).ExhaustNode(k) == MultizoneCompExhaustFanData(i).InletNode) {
                                    MultizoneCompExhaustFanData(i).EPlusZoneNum = ZoneEquipConfig(j).ActualZoneNum;
                                    found = true;
                                }
                            }
                            if (!found) {
                                ShowSevereError(RoutineName + "Fan:ZoneExhaust is not defined in " + CurrentModuleObject);
                                ShowContinueError("Zone Air Exhaust Node in ZoneHVAC:EquipmentConnections =" +
                                                  NodeID(ZoneEquipConfig(j).ExhaustNode(k)));
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            }

            OneTimeFlag = false;
            if (ErrorsFound) {
                ShowFatalError(RoutineName + "Program terminates for preceding reason(s).");
            }
        } // End if OneTimeFlag
    }

    void HybridVentilationControl()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Dec. 2006
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone hybrid ventilation managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs hybrid ventilation control

        // Using/Aliasing
        using DataHVACGlobals::HybridVentSysAvailActualZoneNum;
        using DataHVACGlobals::HybridVentSysAvailAirLoopNum;
        using DataHVACGlobals::HybridVentSysAvailANCtrlStatus;
        using DataHVACGlobals::HybridVentSysAvailMaster;
        using DataHVACGlobals::HybridVentSysAvailVentCtrl;
        using DataHVACGlobals::HybridVentSysAvailWindModifier;
        using DataHVACGlobals::NumHybridVentSysAvailMgrs;
        using DataZoneEquipment::ZoneEquipConfig;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const HybridVentCtrl_Close(2);                                  // Open windows or doors
        int const IndividualCtrlType(0);                                    // Individual window or door control
        int const GlobalCtrlType(1);                                        // Global window or door control
        static std::string const RoutineName("HybridVentilationControl: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SysAvailNum;       // Hybrid ventilation control number
        int AirLoopNum;        // Airloop number
        int ControlledZoneNum; // Controlled zone number
        int ActualZoneNum;     // Actual zone number
        int ANSurfaceNum;      // AirflowNetwork Surface Number
        int SurfNum;           // Surface number
        int ControlType;       // Hybrid ventilation control type: 0 individual; 1 global
        bool Found;            // Logical to indicate whether a master surface is found or not
        static int HybridGlobalErrIndex(0);
        static int HybridGlobalErrCount(0);

        for (auto &e : MultizoneSurfaceData) {
            e.HybridVentClose = false;
            e.HybridCtrlGlobal = false;
            e.HybridCtrlMaster = false;
            e.WindModifier = 1.0;
        }
        ControlType = IndividualCtrlType;

        for (SysAvailNum = 1; SysAvailNum <= NumHybridVentSysAvailMgrs; ++SysAvailNum) {
            AirLoopNum = HybridVentSysAvailAirLoopNum(SysAvailNum);
            VentilationCtrl = HybridVentSysAvailVentCtrl(SysAvailNum);
            if (HybridVentSysAvailANCtrlStatus(SysAvailNum) > 0) {
                ControlType = GetCurrentScheduleValue(HybridVentSysAvailANCtrlStatus(SysAvailNum));
            }
            Found = false;
            ActualZoneNum = 0;
            for (ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum) {
                if (!ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
                // Ensure all the zones served by this AirLoopHVAC to be controlled by the hybrid ventilation
                for (int zoneInNode = 1; zoneInNode <= ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                    if (AirLoopNum > 0) {
                        if (AirLoopNum == ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode)) {
                            ActualZoneNum = ZoneEquipConfig(ControlledZoneNum).ActualZoneNum;
                            break;
                        }
                    } else {
                        if (HybridVentSysAvailActualZoneNum(SysAvailNum) == ZoneEquipConfig(ControlledZoneNum).ActualZoneNum) {
                            ActualZoneNum = HybridVentSysAvailActualZoneNum(SysAvailNum);
                        }
                    }
                }
                if (ActualZoneNum > 0) {
                    for (ANSurfaceNum = 1; ANSurfaceNum <= AirflowNetworkNumOfSurfaces; ++ANSurfaceNum) {
                        SurfNum = MultizoneSurfaceData(ANSurfaceNum).SurfNum;
                        if (Surface(SurfNum).Zone == ActualZoneNum) {
                            if (VentilationCtrl == HybridVentCtrl_Close) {
                                MultizoneSurfaceData(ANSurfaceNum).HybridVentClose = true;
                            } else {
                                if (HybridVentSysAvailWindModifier(SysAvailNum) >= 0) {
                                    MultizoneSurfaceData(ANSurfaceNum).WindModifier = HybridVentSysAvailWindModifier(SysAvailNum);
                                }
                                if (ControlType == GlobalCtrlType) {
                                    MultizoneSurfaceData(ANSurfaceNum).HybridCtrlGlobal = true;
                                    if (HybridVentSysAvailMaster(SysAvailNum) == ActualZoneNum) {
                                        if ((SurfaceWindow(SurfNum).OriginalClass == SurfaceClass_Window ||
                                             SurfaceWindow(SurfNum).OriginalClass == SurfaceClass_Door ||
                                             SurfaceWindow(SurfNum).OriginalClass == SurfaceClass_GlassDoor) &&
                                            Surface(SurfNum).ExtBoundCond == ExternalEnvironment) {
                                            MultizoneSurfaceData(ANSurfaceNum).HybridCtrlMaster = true;
                                            Found = true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (ControlType == GlobalCtrlType && !Found && !WarmupFlag && VentilationCtrl != HybridVentCtrl_Close) {
                ++HybridGlobalErrCount;
                if (HybridGlobalErrCount < 2) {
                    ShowWarningError(RoutineName +
                                     "The hybrid ventilation control schedule value indicates global control in the controlled zone = " +
                                     Zone(HybridVentSysAvailMaster(SysAvailNum)).Name);
                    ShowContinueError("The exterior surface containing an opening component in the controlled zone is not found.  No global control "
                                      "will not be modeled.");
                    ShowContinueError("The individual control is assumed.");
                    ShowContinueErrorTimeStamp("");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        RoutineName + "The hybrid ventilation control requires a global control. The individual control continues...",
                        HybridGlobalErrIndex,
                        double(ControlType),
                        double(ControlType));
                }
            }
        }
    }

    void CalcSingleSidedCps(std::vector<std::vector<Real64>> &valsByFacade, int numWindDir)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Sam Brunswick
        //       DATE WRITTEN   September 2013
        //       MODIFIED       Revised by J. DeGraw, May 2017, to use tables
        //       RE-ENGINEERED  n/a

        // PURPOSE OF THIS SUBROUTINE:
        // Modify the wind pressure coefficients for single sided ventilation.

        // Using/Aliasing
        using namespace DataEnvironment;
        using namespace DataIPShortCuts;

        // Locals
        int WindDirNum;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AFNZnNum; // counters
        int SrfNum;
        int ExtOpenNum;
        int ZnNum;
        int DetOpenNum; // row index of surface in MultizoneCompDetOpeningData
        int SimOpenNum; // row index of surface in MultizoneCompSimOpeningData
        int MZDZoneNum; // row index of surface zone in MultizoneZoneData
        Real64 X1;
        Real64 Y1;
        Real64 X2;
        Real64 Y2;
        Real64 ZoneAng1;
        Real64 ZoneAng2;
        Real64 ZoneAngDiff;
        Array1D<Real64> ZoneAng;           // Azimuth angle of the exterior wall of the zone
        Array1D<Real64> PiFormula;         // Formula for the mean pressure difference
        Array1D<Real64> SigmaFormula;      // Formula for the fluctuating pressure difference
        Array1D<Real64> Sprime;            // The dimensionless ratio of the window separation to the building width
        Array1D<Real64> CPV1;              // Wind pressure coefficient for the first opening in the zone
        Array1D<Real64> CPV2;              // Wind pressure coefficient for the second opening in the zone
        static int AFNNumOfExtOpenings(0); // Total number of external openings in the model
        static int OpenNuminZone(0);       // Counts which opening this is in the zone, 1 or 2
        std::string Name;                  // External node name
        Array1D_int NumofExtSurfInZone;    // List of the number of exterior openings in each zone

        struct AFNExtSurfacesProp // External opening information
        {
            // Members
            int SurfNum;          // row index of the external opening in the Surface array
            std::string SurfName; // Surface name
            int MSDNum;           // row index of the external opening in the MultizoneSurfaceData array
            int ZoneNum;          // EnergyPlus zone number
            int MZDZoneNum;       // row index of the zone in the MultizoneZoneData array
            int ExtNodeNum;       // External node number; = row index in MultizoneExternalNodeData array + AirflowNetworkNumOfZones
            std::string ZoneName; // EnergyPlus zone name
            int facadeNum;
            int curve;          // wind pressure coefficient curve index
            int CompTypeNum;    // Opening type (detailed, simple, etc.)
            Real64 NodeHeight;  // Elevation of the opening node
            Real64 OpeningArea; // Opening area (=Height*Width)
            Real64 Height;      // Opening height = MultizoneSurfaceData()%Height
            Real64 Width;       // Opening width  = MultizoneSurfaceData()%Width
            Real64 DischCoeff;  // Opening discharge coefficient

            // Default Constructor
            AFNExtSurfacesProp()
                : SurfNum(0), MSDNum(0), ZoneNum(0), MZDZoneNum(0), ExtNodeNum(0), facadeNum(0), curve(0), CompTypeNum(0), NodeHeight(0.0),
                  OpeningArea(0.0), Height(0.0), Width(0.0), DischCoeff(0.0)
            {
            }
        };

        // Object Data
        Array1D<AFNExtSurfacesProp> AFNExtSurfaces; // Surface numbers of all exterior openings

        // Count the total number of exterior simple and detailed openings and the number in each zone
        // Verify that each zone with "ADVANCED" single sided wind pressure coefficients has exactly two openings.
        // If it doesn't have two openings, change "ADVANCED" to "STANDARD"
        NumofExtSurfInZone.dimension(AirflowNetworkNumOfZones, 0);
        for (AFNZnNum = 1; AFNZnNum <= AirflowNetworkNumOfZones; ++AFNZnNum) {
            if (MultizoneZoneData(AFNZnNum).SingleSidedCpType == "ADVANCED") {
                for (SrfNum = 1; SrfNum <= AirflowNetworkNumOfSurfaces; ++SrfNum) {
                    if (Surface(MultizoneSurfaceData(SrfNum).SurfNum).ExtBoundCond == ExternalEnvironment) { // check if outdoor boundary condition
                        MZDZoneNum = UtilityRoutines::FindItemInList(
                            Surface(MultizoneSurfaceData(SrfNum).SurfNum).ZoneName, MultizoneZoneData, &MultizoneZoneProp::ZoneName);
                        if (MZDZoneNum == AFNZnNum) {
                            DetOpenNum = UtilityRoutines::FindItemInList(MultizoneSurfaceData(SrfNum).OpeningName, MultizoneCompDetOpeningData);
                            if (DetOpenNum > 0) {
                                ++AFNNumOfExtOpenings;
                                ++NumofExtSurfInZone(AFNZnNum);
                            } else {
                                SimOpenNum =
                                    UtilityRoutines::FindItemInList(MultizoneSurfaceData(SrfNum).OpeningName, MultizoneCompSimpleOpeningData);
                                if (SimOpenNum > 0) {
                                    ++AFNNumOfExtOpenings;
                                    ++NumofExtSurfInZone(AFNZnNum);
                                }
                            }
                        }
                    }
                }
                if (NumofExtSurfInZone(AFNZnNum) == 0) {
                    ShowWarningError("AirflowNetwork:Multizone:Zone = " + MultizoneZoneData(AFNZnNum).ZoneName +
                                     " has single side wind pressure coefficient type \"ADVANCED\", but has no exterior "
                                     "AirflowNetwork:MultiZone:Component:DetailedOpening and/or AirflowNetwork:MultiZone:Component:SimpleOpening "
                                     "objects.");
                    ShowContinueError("Zones must have exactly two exterior openings in order for the \"ADVANCED\" single sided wind pressure "
                                      "coefficient model to be used.");
                    ShowContinueError("The wind pressure coefficient model for this zone will be set to \"STANDARD\" and simulation continues.");
                    MultizoneZoneData(AFNZnNum).SingleSidedCpType = "STANDARD";
                } else if (NumofExtSurfInZone(AFNZnNum) == 1) {
                    ShowWarningError("AirflowNetwork:Multizone:Zone = " + MultizoneZoneData(AFNZnNum).ZoneName);
                    ShowContinueError(
                        "has single side wind pressure coefficient type \"ADVANCED\", but has only one exterior "
                        "AirflowNetwork:MultiZone:Component:DetailedOpening and/or AirflowNetwork:MultiZone:Component:SimpleOpening objects.");
                    ShowContinueError(
                        "Zones must have exactly two openings in order for the \"ADVANCED\" single side wind pressure coefficient model to be used.");
                    ShowContinueError("The wind pressure coefficient model for this zone will be set to \"STANDARD\" and simulation continues.");
                    MultizoneZoneData(AFNZnNum).SingleSidedCpType = "STANDARD";
                } else if (NumofExtSurfInZone(AFNZnNum) > 2) {
                    ShowWarningError("AirflowNetwork:Multizone:Zone = " + MultizoneZoneData(AFNZnNum).ZoneName +
                                     " has single side wind pressure coefficient type \"ADVANCED\", but has " +
                                     RoundSigDigits(NumofExtSurfInZone(AFNZnNum)) +
                                     " exterior AirflowNetwork:MultiZone:Component:DetailedOpening and/or "
                                     "AirflowNetwork:MultiZone:Component:SimpleOpening objects.");
                    ShowContinueError(
                        "Zones must have exactly two openings in order for the \"ADVANCED\" single side wind pressure coefficient model to be used.");
                    ShowContinueError("The wind pressure coefficient model for this zone will be set to \"STANDARD\" and simulation continues.");
                    MultizoneZoneData(AFNZnNum).SingleSidedCpType = "STANDARD";
                }
            }
        }
        if (AFNNumOfExtOpenings == 0) return;
        // Recount the number of single sided zones
        AirflowNetworkNumOfSingleSideZones = 0;
        for (AFNZnNum = 1; AFNZnNum <= AirflowNetworkNumOfZones; ++AFNZnNum) {
            if (MultizoneZoneData(AFNZnNum).SingleSidedCpType == "ADVANCED") {
                ++AirflowNetworkNumOfSingleSideZones;
            }
        }
        if (AirflowNetworkNumOfSingleSideZones == 0) return; // Bail if no zones call for the advanced single sided model.
        // Recount the number of detailed and simple exterior openings in zones with "ADVANCED" single sided wind pressure coefficients
        AFNNumOfExtOpenings = 0;
        for (SrfNum = 1; SrfNum <= AirflowNetworkNumOfSurfaces; ++SrfNum) {
            MZDZoneNum = UtilityRoutines::FindItemInList(
                Surface(MultizoneSurfaceData(SrfNum).SurfNum).ZoneName, MultizoneZoneData, &MultizoneZoneProp::ZoneName);
            if (MultizoneZoneData(MZDZoneNum).SingleSidedCpType == "ADVANCED") {
                if (Surface(MultizoneSurfaceData(SrfNum).SurfNum).ExtBoundCond == ExternalEnvironment) { // check if outdoor boundary condition
                    DetOpenNum = UtilityRoutines::FindItemInList(MultizoneSurfaceData(SrfNum).OpeningName, MultizoneCompDetOpeningData);
                    if (DetOpenNum > 0) {
                        ++AFNNumOfExtOpenings;
                    } else {
                        SimOpenNum = UtilityRoutines::FindItemInList(MultizoneSurfaceData(SrfNum).OpeningName, MultizoneCompSimpleOpeningData);
                        if (SimOpenNum > 0) {
                            ++AFNNumOfExtOpenings;
                        }
                    }
                }
            }
        }
        AFNExtSurfaces.allocate(AFNNumOfExtOpenings);
        // Create array of properties for all the exterior single sided openings
        ExtOpenNum = 1;
        for (SrfNum = 1; SrfNum <= AirflowNetworkNumOfSurfaces; ++SrfNum) {
            if (Surface(MultizoneSurfaceData(SrfNum).SurfNum).ExtBoundCond == ExternalEnvironment) {
                if (AirflowNetworkNumOfDetOpenings > 0) {
                    DetOpenNum = UtilityRoutines::FindItemInList(MultizoneSurfaceData(SrfNum).OpeningName, MultizoneCompDetOpeningData);
                    MZDZoneNum = UtilityRoutines::FindItemInList(
                        Surface(MultizoneSurfaceData(SrfNum).SurfNum).ZoneName, MultizoneZoneData, &MultizoneZoneProp::ZoneName);
                    if (MultizoneZoneData(MZDZoneNum).SingleSidedCpType == "ADVANCED") {
                        if (DetOpenNum > 0) {
                            AFNExtSurfaces(ExtOpenNum).MSDNum = SrfNum;
                            AFNExtSurfaces(ExtOpenNum).SurfNum = MultizoneSurfaceData(SrfNum).SurfNum;
                            AFNExtSurfaces(ExtOpenNum).NodeHeight = Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Centroid.z;
                            AFNExtSurfaces(ExtOpenNum).SurfName = Surface(MultizoneSurfaceData(SrfNum).SurfNum).Name;
                            AFNExtSurfaces(ExtOpenNum).ZoneNum = Surface(MultizoneSurfaceData(SrfNum).SurfNum).Zone;
                            AFNExtSurfaces(ExtOpenNum).ZoneName = Surface(MultizoneSurfaceData(SrfNum).SurfNum).ZoneName;
                            AFNExtSurfaces(ExtOpenNum).MZDZoneNum =
                                UtilityRoutines::FindItemInList(AFNExtSurfaces(ExtOpenNum).ZoneName, MultizoneZoneData, &MultizoneZoneProp::ZoneName);
                            AFNExtSurfaces(ExtOpenNum).CompTypeNum = CompTypeNum_DOP;
                            AFNExtSurfaces(ExtOpenNum).Height = MultizoneSurfaceData(SrfNum).Height;
                            AFNExtSurfaces(ExtOpenNum).Width = MultizoneSurfaceData(SrfNum).Width;
                            AFNExtSurfaces(ExtOpenNum).OpeningArea =
                                MultizoneSurfaceData(SrfNum).Width * MultizoneSurfaceData(SrfNum).Height * MultizoneSurfaceData(SrfNum).OpenFactor;
                            AFNExtSurfaces(ExtOpenNum).ExtNodeNum = MultizoneSurfaceData(ExtOpenNum).NodeNums[1];
                            AFNExtSurfaces(ExtOpenNum).facadeNum =
                                MultizoneExternalNodeData(AFNExtSurfaces(ExtOpenNum).ExtNodeNum - AirflowNetworkNumOfZones).facadeNum;
                            AFNExtSurfaces(ExtOpenNum).curve =
                                MultizoneExternalNodeData(AFNExtSurfaces(ExtOpenNum).ExtNodeNum - AirflowNetworkNumOfZones).curve;
                            AFNExtSurfaces(ExtOpenNum).DischCoeff = MultizoneCompDetOpeningData(DetOpenNum).DischCoeff2;
                            ++ExtOpenNum;
                        }
                    }
                } else if (AirflowNetworkNumOfSimOpenings > 0) {
                    SimOpenNum = UtilityRoutines::FindItemInList(MultizoneSurfaceData(SrfNum).OpeningName, MultizoneCompSimpleOpeningData);
                    if (SimOpenNum > 0) {
                        AFNExtSurfaces(ExtOpenNum).MSDNum = SrfNum;
                        AFNExtSurfaces(ExtOpenNum).SurfNum = MultizoneSurfaceData(SrfNum).SurfNum;
                        AFNExtSurfaces(ExtOpenNum).SurfName = Surface(MultizoneSurfaceData(SrfNum).SurfNum).Name;
                        AFNExtSurfaces(ExtOpenNum).ZoneNum = Surface(MultizoneSurfaceData(SrfNum).SurfNum).Zone;
                        AFNExtSurfaces(ExtOpenNum).ZoneName = Surface(MultizoneSurfaceData(SrfNum).SurfNum).ZoneName;
                        AFNExtSurfaces(ExtOpenNum).MZDZoneNum =
                            UtilityRoutines::FindItemInList(AFNExtSurfaces(ExtOpenNum).ZoneName, MultizoneZoneData, &MultizoneZoneProp::ZoneName);
                        AFNExtSurfaces(ExtOpenNum).CompTypeNum = CompTypeNum_SOP;
                        AFNExtSurfaces(ExtOpenNum).Height = MultizoneSurfaceData(SrfNum).Height;
                        AFNExtSurfaces(ExtOpenNum).Width = MultizoneSurfaceData(SrfNum).Width;
                        AFNExtSurfaces(ExtOpenNum).OpeningArea =
                            MultizoneSurfaceData(SrfNum).Width * MultizoneSurfaceData(SrfNum).Height * MultizoneSurfaceData(SrfNum).OpenFactor;
                        AFNExtSurfaces(ExtOpenNum).ExtNodeNum = MultizoneSurfaceData(ExtOpenNum).NodeNums[1];
                        AFNExtSurfaces(ExtOpenNum).curve =
                            MultizoneExternalNodeData(AFNExtSurfaces(ExtOpenNum).ExtNodeNum - AirflowNetworkNumOfZones).curve;
                        AFNExtSurfaces(ExtOpenNum).DischCoeff = MultizoneCompSimpleOpeningData(SimOpenNum).DischCoeff;
                        ++ExtOpenNum;
                    }
                }
            }
        }
        // Calculate the azimuth and the coordinates of the centroid of each opening.
        // Calculate Sprime and DeltaCp for each zone.
        PiFormula.allocate(numWindDir);
        SigmaFormula.allocate(numWindDir);
        DeltaCp.allocate(AirflowNetworkNumOfZones);
        EPDeltaCP.allocate(AirflowNetworkNumOfZones);
        Sprime.allocate(AirflowNetworkNumOfZones);
        ZoneAng.allocate(AirflowNetworkNumOfZones);
        for (ZnNum = 1; ZnNum <= AirflowNetworkNumOfZones; ++ZnNum) {
            DeltaCp(ZnNum).WindDir.allocate(numWindDir);
            EPDeltaCP(ZnNum).WindDir.allocate(numWindDir);
            for (WindDirNum = 1; WindDirNum <= numWindDir; ++WindDirNum) {
                DeltaCp(ZnNum).WindDir(WindDirNum) = 0.0;
                EPDeltaCP(ZnNum).WindDir(WindDirNum) = 0.0;
            }
        }
        Sprime = 0.0;
        ZoneAng = 0.0;
        for (ZnNum = 1; ZnNum <= AirflowNetworkNumOfZones; ++ZnNum) {
            if (MultizoneZoneData(ZnNum).SingleSidedCpType == "ADVANCED") {
                OpenNuminZone = 1;
                for (ExtOpenNum = 1; ExtOpenNum <= AFNNumOfExtOpenings; ++ExtOpenNum) {
                    if (OpenNuminZone > 2) break; // Tuned
                    if (AFNExtSurfaces(ExtOpenNum).MZDZoneNum == ZnNum) {
                        if (OpenNuminZone == 1) {
                            X1 = Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Centroid.x;
                            Y1 = Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Centroid.y;
                            ZoneAng1 = Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Azimuth;
                            ++OpenNuminZone;
                        } else if (OpenNuminZone == 2) {
                            X2 = Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Centroid.x;
                            Y2 = Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Centroid.y;
                            ZoneAng2 = Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Azimuth;
                            ++OpenNuminZone;
                        }
                    }
                }
                ZoneAngDiff = ZoneAng1 - ZoneAng2;
                if (ZoneAngDiff > 0.01) {
                    ShowWarningError("AirflowNetwork:Multizone:Zone = " + MultizoneZoneData(AFNZnNum).ZoneName +
                                     " has single side wind pressure coefficient type \"ADVANCED\", but has openings which are not coplanar.");
                    ShowContinueError("The openings should be coplanar for the model to be valid. Simulation Continues.");
                }
                ZoneAng(ZnNum) = ZoneAng1;
                Sprime(ZnNum) = std::sqrt(pow_2(X1 - X2) + pow_2(Y1 - Y2)) / MultizoneZoneData(ZnNum).BuildWidth;
                // Calculate DeltaCp for each wind direction for each zone
                for (WindDirNum = 1; WindDirNum <= numWindDir; ++WindDirNum) {
                    WindDir = (WindDirNum - 1) * 10.0;
                    WindAng = (WindDirNum - 1) * 10.0;
                    IncAng = std::abs(WindAng - ZoneAng(ZnNum));
                    if (std::abs(IncAng) > 180.0) IncAng -= 360.0;
                    if (UtilityRoutines::SameString(AirflowNetworkSimu.WPCCntr, "SurfaceAverageCalculation")) {
                        if (std::abs(IncAng) <= 67.5) {
                            PiFormula(WindDirNum) = 0.44 * sign(std::sin(2.67 * std::abs(IncAng) * Pi / 180.0), IncAng);
                        } else if (std::abs(IncAng) <= 180.0) {
                            PiFormula(WindDirNum) = -0.69 * sign(std::sin((288 - 1.6 * std::abs(IncAng)) * Pi / 180.0), IncAng);
                        }
                        SigmaFormula(WindDirNum) = 0.423 - 0.00163 * std::abs(IncAng);
                        DeltaCp(ZnNum).WindDir(WindDirNum) =
                            (0.02 + (0.346 * std::abs(PiFormula(WindDirNum)) + 0.084 * SigmaFormula(WindDirNum)) * Sprime(ZnNum));
                    }
                }
            }
        }
        // int numOfCPValue = ( 4 + 2 * AirflowNetworkNumOfSingleSideZones );

        // Calculate the single sided Cp arrays from DeltaCp for each single sided opening
        CPV1.allocate(numWindDir); // These two arrays should probably be removed
        CPV2.allocate(numWindDir);
        CPV1 = 0.0;
        CPV2 = 0.0;
        SrfNum = 6;
        for (ZnNum = 1; ZnNum <= AirflowNetworkNumOfZones; ++ZnNum) {
            if (MultizoneZoneData(ZnNum).SingleSidedCpType == "ADVANCED") {
                OpenNuminZone = 1;
                for (ExtOpenNum = 1; ExtOpenNum <= AFNNumOfExtOpenings; ++ExtOpenNum) {
                    if (OpenNuminZone > 2) break; // Tuned
                    if (AFNExtSurfaces(ExtOpenNum).MZDZoneNum == ZnNum) {
                        Real64 const VelRatio_2(std::pow(10.0 / AFNExtSurfaces(ExtOpenNum).NodeHeight, 2.0 * SiteWindExp));
                        Real64 const AFNEExtSurface_fac(0.5 * (1.0 / pow_2(AFNExtSurfaces(ExtOpenNum).DischCoeff)));
                        if (OpenNuminZone == 1) {
                            std::vector<Real64> cpvalues(numWindDir);
                            for (WindDirNum = 1; WindDirNum <= numWindDir; ++WindDirNum) {
                                Real64 unmodifiedValue = valsByFacade[AFNExtSurfaces(ExtOpenNum).facadeNum - 1][WindDirNum - 1] +
                                                         AFNEExtSurface_fac * DeltaCp(ZnNum).WindDir(WindDirNum);
                                cpvalues[WindDirNum - 1] = CPV1(WindDirNum) = VelRatio_2 * unmodifiedValue;
                            }
                            valsByFacade.push_back(cpvalues);
                            MultizoneExternalNodeData(AFNExtSurfaces(ExtOpenNum).ExtNodeNum - AirflowNetworkNumOfZones).facadeNum = SrfNum;
                            ++OpenNuminZone;
                            ++SrfNum;
                        } else if (OpenNuminZone == 2) {
                            std::vector<Real64> cpvalues(numWindDir);
                            for (WindDirNum = 1; WindDirNum <= numWindDir; ++WindDirNum) {
                                Real64 unmodifiedValue = valsByFacade[AFNExtSurfaces(ExtOpenNum).facadeNum - 1][WindDirNum - 1] -
                                                         AFNEExtSurface_fac * DeltaCp(ZnNum).WindDir(WindDirNum);
                                cpvalues[WindDirNum - 1] = CPV2(WindDirNum) = VelRatio_2 * unmodifiedValue;
                                EPDeltaCP(ZnNum).WindDir(WindDirNum) = std::abs(CPV2(WindDirNum) - CPV1(WindDirNum));
                            }
                            valsByFacade.push_back(cpvalues);
                            MultizoneExternalNodeData(AFNExtSurfaces(ExtOpenNum).ExtNodeNum - AirflowNetworkNumOfZones).facadeNum = SrfNum;
                            ++OpenNuminZone;
                            ++SrfNum;
                        }
                    }
                }
            }
        }
        // Rewrite the CPVNum for all nodes that correspond with a simple or detailed opening
        // Does this loop really do anything?
        for (ZnNum = 1; ZnNum <= AirflowNetworkNumOfZones; ++ZnNum) {
            OpenNuminZone = 1;
            for (ExtOpenNum = 1; ExtOpenNum <= AFNNumOfExtOpenings; ++ExtOpenNum) {
                if (AFNExtSurfaces(ExtOpenNum).MZDZoneNum == ZnNum) {
                    if (OpenNuminZone == 1) {
                        ++OpenNuminZone;
                    } else if (OpenNuminZone == 2) {
                        ++OpenNuminZone;
                    }
                }
            }
        }
    }

    Real64 GetZoneInfilAirChangeRate(int const ZoneNum) // hybrid ventilation system controlled zone number
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May. 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This function outputs air change per hour in a given zone

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHVACGlobals::TimeStepSys;

        // Return value
        Real64 ACH; // Zone air change rate [ACH]

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 InfilVolume; // Zone infiltration volume
        Real64 RhoAir;      // Zone air density [kg/m3]
        Real64 CpAir;       // Zone air specific heat

        CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));
        RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, MAT(ZoneNum), ZoneAirHumRat(ZoneNum));
        InfilVolume = (AirflowNetworkExchangeData(ZoneNum).SumMCp / CpAir / RhoAir) * TimeStepSys * SecInHour;
        ACH = InfilVolume / (TimeStepSys * Zone(ZoneNum).Volume);

        return ACH;
    }

    int GetAirLoopNumber(int const NodeNumber) // Get air loop number for each distribution node and linkage
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Feb. 2018

        // PURPOSE OF THIS SUBROUTINE:
        // This function outputs an AirLoopNum based on node number

        // Using/Aliasing
        using BranchNodeConnections::GetChildrenData;
        using BranchNodeConnections::GetNumChildren;
        using BranchNodeConnections::IsParentObject;
        using DataAirSystems::PrimaryAirSystem;
        using DataHVACGlobals::NumPrimaryAirSys;
        using DataZoneEquipment::NumReturnAirPaths;
        using DataZoneEquipment::NumSupplyAirPaths;
        using DataZoneEquipment::ReturnAirPath;
        using DataZoneEquipment::SupplyAirPath;
        using SingleDuct::GetHVACSingleDuctSysIndex;

        // Return value
        int AirLoopNumber = 0;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BranchNum;
        int NumOfNodes;
        int NodeNum;
        int OutNum;
        int SupAirPath;
        int SupAirPathOutNodeNum;
        int CtrlZoneNum;
        int ZoneInNum;
        int ZoneOutNum;
        int AirLoopNum;
        int TUNum = 0;
        int TermNum = 0;
        bool LocalError;
        int NumOfComp;
        int NumOfSubComp;
        bool ErrorsFound;
        std::string TypeOfComp;
        std::string NameOfComp;
        int NumOfSubSubComp;

        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            // Check OAMixer OA inlet node
            if (NodeNumber == PrimaryAirSystem(AirLoopNum).OAMixOAInNodeNum) {
                return AirLoopNum;
            }
            // Check branch
            for (BranchNum = 1; BranchNum <= PrimaryAirSystem(AirLoopNum).NumBranches; ++BranchNum) {
                NumOfNodes = PrimaryAirSystem(AirLoopNum).Branch(BranchNum).TotalNodes;
                for (NodeNum = 1; NodeNum <= NumOfNodes; ++NodeNum) {
                    if (NodeNumber == PrimaryAirSystem(AirLoopNum).Branch(BranchNum).NodeNum(NodeNum)) {
                        return AirLoopNum;
                    }
                }
                for (NumOfComp = 1; NumOfComp <= PrimaryAirSystem(AirLoopNum).Branch(BranchNum).TotalComponents; ++NumOfComp) {
                    if (NodeNumber == PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).NodeNumIn) {
                        return AirLoopNum;
                    }
                    if (NodeNumber == PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).NodeNumOut) {
                        return AirLoopNum;
                    }
                    if (PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).NumSubComps == 0) {
                        std::string TypeOfComp = PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).TypeOf;
                        std::string NameOfComp = PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).Name;
                        if (IsParentObject(TypeOfComp, NameOfComp)) {

                            int NumChildren = GetNumChildren(TypeOfComp, NameOfComp);
                            Array1D_string SubCompTypes;
                            Array1D_string SubCompNames;
                            Array1D_string InletNodeNames;
                            Array1D_int InletNodeNumbers;
                            Array1D_string OutletNodeNames;
                            Array1D_int OutletNodeNumbers;

                            SubCompTypes.allocate(NumChildren);
                            SubCompNames.allocate(NumChildren);
                            InletNodeNames.allocate(NumChildren);
                            InletNodeNumbers.allocate(NumChildren);
                            OutletNodeNames.allocate(NumChildren);
                            OutletNodeNumbers.allocate(NumChildren);

                            GetChildrenData(TypeOfComp,
                                            NameOfComp,
                                            NumChildren,
                                            SubCompTypes,
                                            SubCompNames,
                                            InletNodeNames,
                                            InletNodeNumbers,
                                            OutletNodeNames,
                                            OutletNodeNumbers,
                                            ErrorsFound);

                            for (NumOfSubComp = 1; NumOfSubComp <= NumChildren; ++NumOfSubComp) {
                                if (NodeNumber == InletNodeNumbers(NumOfSubComp)) {
                                    SubCompTypes.deallocate();
                                    SubCompNames.deallocate();
                                    InletNodeNames.deallocate();
                                    InletNodeNumbers.deallocate();
                                    OutletNodeNames.deallocate();
                                    OutletNodeNumbers.deallocate();
                                    return AirLoopNum;
                                }
                                if (NodeNumber == OutletNodeNumbers(NumOfSubComp)) {
                                    SubCompTypes.deallocate();
                                    SubCompNames.deallocate();
                                    InletNodeNames.deallocate();
                                    InletNodeNumbers.deallocate();
                                    OutletNodeNames.deallocate();
                                    OutletNodeNumbers.deallocate();
                                    return AirLoopNum;
                                }
                            }
                            for (NumOfSubComp = 1; NumOfSubComp <= NumChildren; ++NumOfSubComp) {
                                std::string TypeOfComp = SubCompTypes(NumOfSubComp);
                                std::string NameOfComp = SubCompNames(NumOfSubComp);
                                if (IsParentObject(TypeOfComp, NameOfComp)) {

                                    int NumGrandChildren = GetNumChildren(TypeOfComp, NameOfComp);
                                    Array1D_string SubSubCompTypes;
                                    Array1D_string SubSubCompNames;
                                    Array1D_string SubSubInletNodeNames;
                                    Array1D_int SubSubInletNodeNumbers;
                                    Array1D_string SubSubOutletNodeNames;
                                    Array1D_int SubSubOutletNodeNumbers;

                                    SubSubCompTypes.allocate(NumGrandChildren);
                                    SubSubCompNames.allocate(NumGrandChildren);
                                    SubSubInletNodeNames.allocate(NumGrandChildren);
                                    SubSubInletNodeNumbers.allocate(NumGrandChildren);
                                    SubSubOutletNodeNames.allocate(NumGrandChildren);
                                    SubSubOutletNodeNumbers.allocate(NumGrandChildren);

                                    GetChildrenData(TypeOfComp,
                                                    NameOfComp,
                                                    NumGrandChildren,
                                                    SubSubCompTypes,
                                                    SubSubCompNames,
                                                    SubSubInletNodeNames,
                                                    SubSubInletNodeNumbers,
                                                    SubSubOutletNodeNames,
                                                    SubSubOutletNodeNumbers,
                                                    ErrorsFound);
                                    for (int SubSubCompNum = 1; SubSubCompNum <= NumGrandChildren; ++SubSubCompNum) {
                                        if (NodeNumber == SubSubInletNodeNumbers(SubSubCompNum)) {
                                            SubSubCompTypes.deallocate();
                                            SubSubCompNames.deallocate();
                                            SubSubInletNodeNames.deallocate();
                                            SubSubInletNodeNumbers.deallocate();
                                            SubSubOutletNodeNames.deallocate();
                                            SubSubOutletNodeNumbers.deallocate();
                                            SubCompTypes.deallocate();
                                            SubCompNames.deallocate();
                                            InletNodeNames.deallocate();
                                            InletNodeNumbers.deallocate();
                                            OutletNodeNames.deallocate();
                                            OutletNodeNumbers.deallocate();
                                            return AirLoopNum;
                                        }
                                        if (NodeNumber == SubSubOutletNodeNumbers(SubSubCompNum)) {
                                            SubSubCompTypes.deallocate();
                                            SubSubCompNames.deallocate();
                                            SubSubInletNodeNames.deallocate();
                                            SubSubInletNodeNumbers.deallocate();
                                            SubSubOutletNodeNames.deallocate();
                                            SubSubOutletNodeNumbers.deallocate();
                                            SubCompTypes.deallocate();
                                            SubCompNames.deallocate();
                                            InletNodeNames.deallocate();
                                            InletNodeNumbers.deallocate();
                                            OutletNodeNames.deallocate();
                                            OutletNodeNumbers.deallocate();
                                            return AirLoopNum;
                                        }
                                    }
                                    SubSubCompTypes.deallocate();
                                    SubSubCompNames.deallocate();
                                    SubSubInletNodeNames.deallocate();
                                    SubSubInletNodeNumbers.deallocate();
                                    SubSubOutletNodeNames.deallocate();
                                    SubSubOutletNodeNumbers.deallocate();
                                }
                            }

                            SubCompTypes.deallocate();
                            SubCompNames.deallocate();
                            InletNodeNames.deallocate();
                            InletNodeNumbers.deallocate();
                            OutletNodeNames.deallocate();
                            OutletNodeNumbers.deallocate();
                        }
                    } else {
                        for (NumOfSubComp = 1; NumOfSubComp <= PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).NumSubComps;
                             ++NumOfSubComp) {
                            if (NodeNumber == PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).SubComp(NumOfSubComp).NodeNumIn) {
                                return AirLoopNum;
                            }
                            if (NodeNumber == PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).SubComp(NumOfSubComp).NodeNumOut) {
                                return AirLoopNum;
                            }
                            for (NumOfSubSubComp = 1;
                                 NumOfSubSubComp <=
                                 PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).SubComp(NumOfSubComp).NumSubSubComps;
                                 ++NumOfSubSubComp) {
                                if (NodeNumber == PrimaryAirSystem(AirLoopNum)
                                                      .Branch(BranchNum)
                                                      .Comp(NumOfComp)
                                                      .SubComp(NumOfSubComp)
                                                      .SubSubComp(NumOfSubSubComp)
                                                      .NodeNumIn) {
                                    return AirLoopNum;
                                }
                                if (NodeNumber == PrimaryAirSystem(AirLoopNum)
                                                      .Branch(BranchNum)
                                                      .Comp(NumOfComp)
                                                      .SubComp(NumOfSubComp)
                                                      .SubSubComp(NumOfSubSubComp)
                                                      .NodeNumOut) {
                                    return AirLoopNum;
                                }
                            }
                        }
                    }
                }
            }

            // Check connection between supply and demand
            for (OutNum = 1; OutNum <= AirToZoneNodeInfo(AirLoopNum).NumSupplyNodes; ++OutNum) {
                // AirLoop supply outlet node
                if (AirToZoneNodeInfo(AirLoopNum).AirLoopSupplyNodeNum(OutNum) == NodeNumber) {
                    return AirLoopNum;
                }
                if (AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(OutNum) == NodeNumber) {
                    return AirLoopNum;
                }
                // supply path
                for (SupAirPath = 1; SupAirPath <= NumSupplyAirPaths; ++SupAirPath) {
                    if (SupplyAirPath(SupAirPath).InletNodeNum == AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(OutNum)) {
                        for (SupAirPathOutNodeNum = 1; SupAirPathOutNodeNum <= SupplyAirPath(SupAirPath).NumOutletNodes; ++SupAirPathOutNodeNum) {
                            if (SupplyAirPath(SupAirPath).OutletNode(SupAirPathOutNodeNum) == NodeNumber) {
                                return AirLoopNum;
                            }
                            for (TUNum = 1; TUNum <= DisSysNumOfTermUnits; ++TUNum) {
                                if (UtilityRoutines::SameString(DisSysCompTermUnitData(TUNum).EPlusType, "AirTerminal:SingleDuct:VAV:Reheat")) {
                                    LocalError = false;
                                    GetHVACSingleDuctSysIndex(DisSysCompTermUnitData(TUNum).Name,
                                                              TermNum,
                                                              LocalError,
                                                              "AirflowNetwork:Distribution:Component:TerminalUnit",
                                                              DisSysCompTermUnitData(TUNum).DamperInletNode,
                                                              DisSysCompTermUnitData(TUNum).DamperOutletNode);
                                    if (SupplyAirPath(SupAirPath).OutletNode(SupAirPathOutNodeNum) == DisSysCompTermUnitData(TUNum).DamperInletNode) {
                                        if (DisSysCompTermUnitData(TUNum).DamperOutletNode == NodeNumber) {
                                            DisSysCompTermUnitData(TUNum).AirLoopNum = AirLoopNum;
                                            return AirLoopNum;
                                        }
                                    }
                                    if (LocalError) {
                                    }
                                }
                            }
                        }
                    }
                }
                // return path
                if (AirToZoneNodeInfo(AirLoopNum).AirLoopReturnNodeNum(OutNum) == NodeNumber) {
                    return AirLoopNum;
                }
                if (AirToZoneNodeInfo(AirLoopNum).ZoneEquipReturnNodeNum(OutNum) == NodeNumber) {
                    return AirLoopNum;
                }
                for (int retPathNum = 1; retPathNum <= NumReturnAirPaths; ++retPathNum) {
                    if (ReturnAirPath(retPathNum).OutletNodeNum == AirToZoneNodeInfo(AirLoopNum).ZoneEquipReturnNodeNum(1)) {
                        if (ReturnAirPath(retPathNum).OutletNodeNum == NodeNumber) {
                            return AirLoopNum;
                        }
                    }
                }
                // Supply inlet node

                // Terminal damper node
            }
        }

        for (CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum) {
            if (!ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            for (ZoneInNum = 1; ZoneInNum <= ZoneEquipConfig(CtrlZoneNum).NumInletNodes; ++ZoneInNum) {
                if (ZoneEquipConfig(CtrlZoneNum).InletNode(ZoneInNum) == NodeNumber) {
                    return ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInNum);
                }
            }
            for (ZoneOutNum = 1; ZoneOutNum <= ZoneEquipConfig(CtrlZoneNum).NumReturnNodes; ++ZoneOutNum) {
                if (ZoneEquipConfig(CtrlZoneNum).ReturnNode(ZoneOutNum) == NodeNumber) {
                    return ZoneEquipConfig(CtrlZoneNum).ReturnNodeAirLoopNum(ZoneOutNum);
                }
            }
        }

        return AirLoopNumber;
    }

    void OccupantVentilationControlProp::calc(int const ZoneNum,
                                              int const EP_UNUSED(SurfNum),
                                              int const EP_UNUSED(PrevOpeningstatus),
                                              Real64 const TimeOpenDuration,
                                              Real64 const TimeCloseDuration,
                                              int &OpeningStatus,
                                              int &OpeningProbStatus,
                                              int &ClosingProbStatus)
    {
        using DataHeatBalance::MRT;

        Real64 Tcomfort;    // Thermal comfort temperature
        Real64 ComfortBand; // Thermal comfort band
        Real64 Toperative;  // Operative temperature
        Real64 OutDryBulb;  // Outdoor dry-bulb temperature

        // flow

        if (TimeOpenDuration > 0) {
            if (TimeOpenDuration >= MinOpeningTime) {
                OpeningStatus = FreeOperation; // free operation
            } else {
                OpeningStatus = MinCheckForceOpen; // forced to open
            }
        }
        if (TimeCloseDuration > 0) {
            if (TimeCloseDuration >= MinClosingTime) {
                OpeningStatus = FreeOperation; // free operation
            } else {
                OpeningStatus = MinCheckForceClose; // forced to close
            }
        }

        if (MinTimeControlOnly) return;

        if (Zone(ZoneNum).HasLinkedOutAirNode) {
            OutDryBulb = Zone(ZoneNum).OutDryBulbTemp;
        } else {
            OutDryBulb = OutDryBulbTempAt(Zone(ZoneNum).Centroid.z);
        }

        if (OutDryBulb < ComfortBouPoint) {
            Tcomfort = CurveValue(ComfortLowTempCurveNum, OutDryBulb);
        } else {
            Tcomfort = CurveValue(ComfortHighTempCurveNum, OutDryBulb);
        }
        ComfortBand = -0.0028 * (100 - MaxPPD) * (100 - MaxPPD) + 0.3419 * (100 - MaxPPD) - 6.6275;
        Toperative = 0.5 * (MAT(ZoneNum) + MRT(ZoneNum));

        if (Toperative > (Tcomfort + ComfortBand)) {
            if (openingProbability(ZoneNum, TimeCloseDuration)) {
                OpeningProbStatus = ProbForceChange; // forced to open
            } else {
                OpeningProbStatus = ProbKeepStatus; // Keep previous status
            }
        } else {
            OpeningProbStatus = ProbNoAction; // free operation
        }

        if (Toperative < (Tcomfort - ComfortBand)) {
            if (closingProbability(TimeOpenDuration)) {
                ClosingProbStatus = ProbForceChange; // forced to close
            } else {
                ClosingProbStatus = ProbKeepStatus; // Keep previous status
            }
        } else {
            ClosingProbStatus = ProbNoAction; // free operation
        }
    }

    bool OccupantVentilationControlProp::openingProbability(int const ZoneNum,
                                                            Real64 const TimeCloseDuration) // function to perform calculations of opening probability
    {
        using DataHeatBalance::ZoneIntGain;
        using DataHeatBalFanSys::TempControlType;
        using DataHeatBalFanSys::ZoneThermostatSetPointHi;
        using DataHeatBalFanSys::ZoneThermostatSetPointLo;
        using DataHVACGlobals::DualSetPointWithDeadBand;
        using DataHVACGlobals::SingleCoolingSetPoint;
        using DataHVACGlobals::SingleHeatCoolSetPoint;
        using DataHVACGlobals::SingleHeatingSetPoint;

        Real64 SchValue;
        Real64 RandomValue;

        if (TimeCloseDuration < MinClosingTime) {
            return false;
        }
        if (OccupancyCheck) {
            if (ZoneIntGain(ZoneNum).NOFOCC <= 0.0) {
                return false;
            }
        }

        {
            auto const SELECT_CASE_var(TempControlType(ZoneNum)); // Check zone setpoints
            if (SELECT_CASE_var == 0) {                           // Uncontrolled

            } else if (SELECT_CASE_var == SingleHeatingSetPoint) {
                if (MAT(ZoneNum) <= ZoneThermostatSetPointLo(ZoneNum)) {
                    return false;
                }
            } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                if (MAT(ZoneNum) >= ZoneThermostatSetPointHi(ZoneNum)) {
                    return false;
                }
            } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                return false;
            } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                if (MAT(ZoneNum) < ZoneThermostatSetPointLo(ZoneNum) || MAT(ZoneNum) > ZoneThermostatSetPointHi(ZoneNum)) {
                    return false;
                }
            }
        }

        if (OpeningProbSchNum == 0) {
            return true;
        } else {
            SchValue = GetCurrentScheduleValue(OpeningProbSchNum);
            RandomValue = Real64(rand()) / RAND_MAX;
            if (SchValue > RandomValue) {
                return true;
            } else {
                return false;
            }
        }
    }

    bool OccupantVentilationControlProp::closingProbability(Real64 const TimeOpenDuration) // function to perform calculations of closing probability
    {
        Real64 SchValue;
        Real64 RandomValue;

        if (TimeOpenDuration < MinOpeningTime) {
            return false;
        }
        if (ClosingProbSchNum == 0) {
            return true;
        } else {
            SchValue = GetCurrentScheduleValue(ClosingProbSchNum);
            RandomValue = Real64(rand()) / RAND_MAX;
            if (SchValue > RandomValue) {
                return true;
            } else {
                return false;
            }
        }
    }

} // namespace AirflowNetworkBalanceManager

} // namespace EnergyPlus
