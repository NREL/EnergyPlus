// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACManager.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus::HeatBalanceAirManager {
// Module containing the air heat balance simulation routines
// calculation (initialization) routines

// MODULE INFORMATION:
//       AUTHOR         Richard J. Liesen
//       DATE WRITTEN   February 1998
//       MODIFIED       May-July 2000 Joe Huang for Comis Link
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to
// manage the air simluation heat balance on the building.

// METHODOLOGY EMPLOYED:

// REFERENCES:
// The heat balance method is outlined in the "Tarp Alogorithms Manual"
// The methods are also summarized in many BSO Theses and papers.

// OTHER NOTES:
// This module was created from IBLAST subroutines

// USE STATEMENTS:
// Use statements for data only modules
// Using/Aliasing
using namespace DataEnvironment;
using namespace DataHeatBalFanSys;
using namespace DataHeatBalance;
using namespace DataSurfaces;

// Use statements for access to subroutines in other modules
using Psychrometrics::PsyCpAirFnW;
using Psychrometrics::PsyHFnTdbW;
using Psychrometrics::PsyRhoAirFnPbTdbW;
using Psychrometrics::PsyTdbFnHW;

void ManageAirHeatBalance(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   February 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages the heat air balance method of calculating
    // building thermal loads.  It is called from the HeatBalanceManager
    // at the time step level.  This driver manages the calls to all of
    // the other drivers and simulation algorithms.

    // Obtains and Allocates heat balance related parameters from input file
    if (state.dataHeatBalAirMgr->ManageAirHeatBalanceGetInputFlag) {
        GetAirHeatBalanceInput(state);
        state.dataHeatBalAirMgr->ManageAirHeatBalanceGetInputFlag = false;
    }

    InitAirHeatBalance(state); // Initialize all heat balance related parameters

    // Solve the zone heat balance 'Detailed' solution
    // Call the air surface heat balances
    CalcHeatBalanceAir(state);

    ReportZoneMeanAirTemp(state);
}

void GetAirHeatBalanceInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   February 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is the main routine to call other input routines

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrorsFound(false);

    GetAirFlowFlag(state, ErrorsFound);

    SetZoneMassConservationFlag(state);

    // get input parameters for modeling of room air flow
    GetRoomAirModelParameters(state, ErrorsFound);

    if (ErrorsFound) {
        ShowFatalError(state, "GetAirHeatBalanceInput: Errors found in getting Air inputs");
    }
}

void GetAirFlowFlag(EnergyPlusData &state, bool &ErrorsFound) // Set to true if errors found
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Garrett Westmacott
    //       DATE WRITTEN   February 2000
    //       MODIFIED       Oct 2003, FCW: Change "Infiltration-Air Change Rate" from Sum to State
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calls the routine to get simple air flow input data.

    // METHODOLOGY EMPLOYED:
    // Modelled after 'Modual Example' in Guide for Module Developers

    // Using/Aliasing
    using ScheduleManager::GetScheduleIndex;

    state.dataHeatBal->AirFlowFlag = UseSimpleAirFlow;

    GetSimpleAirModelInputs(state, ErrorsFound);
    if (state.dataHeatBal->TotInfiltration + state.dataHeatBal->TotVentilation + state.dataHeatBal->TotMixing + state.dataHeatBal->TotCrossMixing +
            state.dataHeatBal->TotRefDoorMixing >
        0) {
        static constexpr auto Format_720("! <AirFlow Model>, Simple\n AirFlow Model, {}\n");
        print(state.files.eio, Format_720, "Simple");
    }
}

void SetZoneMassConservationFlag(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION :
    // AUTHOR         Bereket Nigusse, FSEC
    // DATE WRITTEN   February 2014
    // MODIFIED

    // PURPOSE OF THIS SUBROUTINE :
    // This subroutine sets the zone mass conservation flag to true.

    if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance &&
        state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment != DataHeatBalance::AdjustmentType::NoAdjustReturnAndMixing) {
        for (int Loop = 1; Loop <= state.dataHeatBal->TotMixing; ++Loop) {
            state.dataHeatBalFanSys->ZoneMassBalanceFlag(state.dataHeatBal->Mixing(Loop).ZonePtr) = true;
            state.dataHeatBalFanSys->ZoneMassBalanceFlag(state.dataHeatBal->Mixing(Loop).FromZone) = true;
        }
    }
}

void GetSimpleAirModelInputs(EnergyPlusData &state, bool &ErrorsFound) // IF errors found in input
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   July 2000
    //       MODIFIED       Oct 2003,FCW: change "Infiltration-Air Change Rate" from Sum to State
    //       MODIFIED       Jan 2008,LG: Allow multiple infiltration and ventilation objects per zone
    //                      May 2009, BG: added calls to setup for possible EMS override
    //                      August 2011, TKS: added refrigeration door mixing
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the input for the "simple" air flow model.

    // REFERENCES:
    // IDD Statements
    // INFILTRATION,A1 [Zone Name],A2 [SCHEDULE Name],N1 [Design level KW],
    //     N2 [Constant Term Coefficient], N3 [Temperature Term Coefficient],
    //     N4 [Velocity Term Coefficient], N5 [Velocity Squared Term Coefficient];
    // MIXING,A1 [Zone Name],A2 [SCHEDULE Name],N1 [Design Level], A3 [Source Zone Name],
    //     N2 [Delta Temperature delta C];
    // CROSS MIXING,A1 [Zone Name],A2 [SCHEDULE Name],N1 [Design Level],
    //     A3 [Source Zone Name], N2 [Delta Temperature delta C];
    // REFRIGERATION DOOR MIXING,A1 [Zone Name],A2 [Mate Zone Name],N1 [Design Level],
    //     A3 [Source Zone Name], N2 [Delta Temperature delta C];

    // Using/Aliasing
    using General::CheckCreatedZoneItemName;
    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;
    using ScheduleManager::GetScheduleMinValue;
    using ScheduleManager::GetScheduleName;
    using ScheduleManager::GetScheduleValuesForDay;
    using SystemAvailabilityManager::GetHybridVentilationControlStatus;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const VentilTempLimit(100.0);                               // degrees Celsius
    Real64 const MixingTempLimit(100.0);                               // degrees Celsius
    Real64 const VentilWSLimit(40.0);                                  // m/s
    static std::string const RoutineName("GetSimpleAirModelInputs: "); // include trailing blank space
    // Refrigeration Door Mixing Protection types, factors used to moderate mixing flow.
    Real64 const RefDoorNone(0.0);
    Real64 const RefDoorAirCurtain(0.5);
    Real64 const RefDoorStripCurtain(0.9);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Array2D<Real64> SVals1;
    Array2D<Real64> SVals2;
    int NumAlpha;  // Number of Alphas for each GetobjectItem call
    int NumNumber; // Number of Numbers for each GetobjectItem call
    int maxAlpha;  // max of Alphas for allocation
    int maxNumber; // max of Numbers for allocation
    int NumArgs;
    int IOStat;
    Array1D_string cAlphaFieldNames;
    Array1D_string cNumericFieldNames;
    Array1D_bool lNumericFieldBlanks;
    Array1D_bool lAlphaFieldBlanks;
    Array1D_string cAlphaArgs;
    Array1D<Real64> rNumericArgs;
    std::string cCurrentModuleObject;

    int i;
    int Loop;
    int Loop1;
    Array1D_bool RepVarSet;
    bool IsNotOK;

    int ZoneNum;
    std::string StringOut;
    std::string NameThisObject;
    int InfiltCount;
    int VentiCount;
    bool ControlFlag;
    int Item;
    int Item1;
    bool errFlag;
    int ZLItem;
    Array1D<Real64> TotInfilVentFlow;
    Array1D<Real64> TotMixingFlow;
    Array1D<Real64> ZoneMixingNum;
    int ConnectTest;
    int ConnectionNumber;
    int NumbNum;
    int AlphaNum;
    int Zone1Num;
    int Zone2Num;
    int ZoneNumA;
    int ZoneNumB;
    int SourceCount;
    int ReceivingCount;
    int IsSourceZone;

    // Formats
    static constexpr auto Format_720(" {} Airflow Stats Nominal, {},{},{},{:.2R},{:.1R},");
    static constexpr auto Format_721("! <{} Airflow Stats Nominal>,Name,Schedule Name,Zone Name, Zone Floor Area {{m2}}, # Zone Occupants,{}\n");
    static constexpr auto Format_722(" {}, {}\n");

    RepVarSet.dimension(state.dataGlobal->NumOfZones, true);

    // Following used for reporting
    state.dataHeatBal->ZnAirRpt.allocate(state.dataGlobal->NumOfZones);

    for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
        // CurrentModuleObject='Zone'
        SetupOutputVariable(state,
                            "Zone Mean Air Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHeatBal->ZnAirRpt(Loop).MeanAirTemp,
                            "Zone",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Operative Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHeatBal->ZnAirRpt(Loop).OperativeTemp,
                            "Zone",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Mean Air Dewpoint Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHeatBal->ZnAirRpt(Loop).MeanAirDewPointTemp,
                            "Zone",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Mean Air Humidity Ratio",
                            OutputProcessor::Unit::kgWater_kgDryAir,
                            state.dataHeatBal->ZnAirRpt(Loop).MeanAirHumRat,
                            "Zone",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Air Heat Balance Internal Convective Heat Gain Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).SumIntGains,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Air Heat Balance Surface Convection Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).SumHADTsurfs,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Air Heat Balance Interzone Air Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).SumMCpDTzones,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Air Heat Balance Outdoor Air Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).SumMCpDtInfil,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Air Heat Balance System Air Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).SumMCpDTsystem,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Air Heat Balance System Convective Heat Gain Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).SumNonAirSystem,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Air Heat Balance Air Energy Storage Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).CzdTdt,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        if (state.dataGlobal->DisplayAdvancedReportVariables) {
            SetupOutputVariable(state,
                                "Zone Phase Change Material Melting Enthalpy",
                                OutputProcessor::Unit::J_kg,
                                state.dataHeatBal->ZnAirRpt(Loop).SumEnthalpyM,
                                "Zone",
                                "Average",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Phase Change Material Freezing Enthalpy",
                                OutputProcessor::Unit::J_kg,
                                state.dataHeatBal->ZnAirRpt(Loop).SumEnthalpyH,
                                "Zone",
                                "Average",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Air Heat Balance Deviation Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnAirRpt(Loop).imBalance,
                                "System",
                                "Average",
                                state.dataHeatBal->Zone(Loop).Name);
        }

        SetupOutputVariable(state,
                            "Zone Exfiltration Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).ExfilTotalLoss,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Exfiltration Sensible Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).ExfilSensiLoss,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Exfiltration Latent Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).ExfilLatentLoss,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Exhaust Air Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).ExhTotalLoss,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Exhaust Air Sensible Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).ExhSensiLoss,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
        SetupOutputVariable(state,
                            "Zone Exhaust Air Latent Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHeatBal->ZnAirRpt(Loop).ExhLatentLoss,
                            "System",
                            "Average",
                            state.dataHeatBal->Zone(Loop).Name);
    }

    SetupOutputVariable(state,
                        "Site Total Zone Exfiltration Heat Loss",
                        OutputProcessor::Unit::J,
                        state.dataHeatBal->ZoneTotalExfiltrationHeatLoss,
                        "System",
                        "Sum",
                        "Environment");
    SetupOutputVariable(state,
                        "Site Total Zone Exhaust Air Heat Loss",
                        OutputProcessor::Unit::J,
                        state.dataHeatBal->ZoneTotalExhaustHeatLoss,
                        "System",
                        "Sum",
                        "Environment");

    cCurrentModuleObject = "ZoneAirBalance:OutdoorAir";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlpha, NumNumber);
    maxAlpha = NumAlpha;
    maxNumber = NumNumber;
    cCurrentModuleObject = "ZoneInfiltration:EffectiveLeakageArea";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlpha, NumNumber);
    maxAlpha = max(NumAlpha, maxAlpha);
    maxNumber = max(NumNumber, maxNumber);
    cCurrentModuleObject = "ZoneInfiltration:FlowCoefficient";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlpha, NumNumber);
    maxAlpha = max(NumAlpha, maxAlpha);
    maxNumber = max(NumNumber, maxNumber);
    cCurrentModuleObject = "ZoneInfiltration:DesignFlowRate";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlpha, NumNumber);
    maxAlpha = max(NumAlpha, maxAlpha);
    maxNumber = max(NumNumber, maxNumber);
    cCurrentModuleObject = "ZoneVentilation:DesignFlowRate";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlpha, NumNumber);
    maxAlpha = max(NumAlpha, maxAlpha);
    maxNumber = max(NumNumber, maxNumber);
    cCurrentModuleObject = "ZoneVentilation:WindandStackOpenArea";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlpha, NumNumber);
    maxAlpha = max(NumAlpha, maxAlpha);
    maxNumber = max(NumNumber, maxNumber);
    cCurrentModuleObject = "ZoneMixing";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlpha, NumNumber);
    maxAlpha = max(NumAlpha, maxAlpha);
    maxNumber = max(NumNumber, maxNumber);
    cCurrentModuleObject = "ZoneCrossMixing";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlpha, NumNumber);
    maxAlpha = max(NumAlpha, maxAlpha);
    maxNumber = max(NumNumber, maxNumber);
    cCurrentModuleObject = "ZoneRefrigerationDoorMixing";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumArgs, NumAlpha, NumNumber);
    maxAlpha = max(NumAlpha, maxAlpha);
    maxNumber = max(NumNumber, maxNumber);

    cAlphaArgs.allocate(maxAlpha);
    cAlphaFieldNames.allocate(maxAlpha);
    cNumericFieldNames.allocate(maxNumber);
    rNumericArgs.dimension(maxNumber, 0.0);
    lAlphaFieldBlanks.dimension(maxAlpha, true);
    lNumericFieldBlanks.dimension(maxNumber, true);

    cCurrentModuleObject = "ZoneAirBalance:OutdoorAir";
    state.dataHeatBal->TotZoneAirBalance = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    state.dataHeatBal->ZoneAirBalance.allocate(state.dataHeatBal->TotZoneAirBalance);

    for (Loop = 1; Loop <= state.dataHeatBal->TotZoneAirBalance; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Loop,
                                                                 cAlphaArgs,
                                                                 NumAlpha,
                                                                 rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        IsNotOK = false;
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        state.dataHeatBal->ZoneAirBalance(Loop).Name = cAlphaArgs(1);
        state.dataHeatBal->ZoneAirBalance(Loop).ZoneName = cAlphaArgs(2);
        state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        if (state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr == 0) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2) + "\".");
            ErrorsFound = true;
        }
        GlobalNames::IntraObjUniquenessCheck(
            state, cAlphaArgs(2), cCurrentModuleObject, cAlphaFieldNames(2), state.dataHeatBalAirMgr->UniqueZoneNames, IsNotOK);
        if (IsNotOK) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", a duplicated object " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2) + "\" is found.");
            ShowContinueError(state, "A zone can only have one " + cCurrentModuleObject + " object.");
            ErrorsFound = true;
        }

        {
            auto const SELECT_CASE_var(cAlphaArgs(3)); // Aie balance method type character input-->convert to integer
            if (SELECT_CASE_var == "QUADRATURE") {
                state.dataHeatBal->ZoneAirBalance(Loop).BalanceMethod = AirBalanceQuadrature;
            } else if (SELECT_CASE_var == "NONE") {
                state.dataHeatBal->ZoneAirBalance(Loop).BalanceMethod = AirBalanceNone;
            } else {
                state.dataHeatBal->ZoneAirBalance(Loop).BalanceMethod = AirBalanceNone;
                ShowWarningError(state,
                                 RoutineName + cAlphaFieldNames(3) + " = " + cAlphaArgs(3) + " not valid choice for " + cCurrentModuleObject + '=' +
                                     cAlphaArgs(1));
                ShowContinueError(state, "The default choice \"NONE\" is assigned");
            }
        }

        state.dataHeatBal->ZoneAirBalance(Loop).InducedAirRate = rNumericArgs(1);
        if (rNumericArgs(1) < 0.0) {
            ShowSevereError(state,
                            format("{}{}=\"{}\", invalid Induced Outdoor Air Due to Duct Leakage Unbalance specification [<0.0]={:.3R}",
                                   RoutineName,
                                   cCurrentModuleObject,
                                   cAlphaArgs(1),
                                   rNumericArgs(1)));
            ErrorsFound = true;
        }

        state.dataHeatBal->ZoneAirBalance(Loop).InducedAirSchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
        if (state.dataHeatBal->ZoneAirBalance(Loop).InducedAirSchedPtr == 0) {
            if (lAlphaFieldBlanks(4)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(4) +
                                    " is required but field is blank.");
            } else {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(4) +
                                    "=\"" + cAlphaArgs(4) + "\".");
            }
            ErrorsFound = true;
        }
        if (!CheckScheduleValueMinMax(state, state.dataHeatBal->ZoneAirBalance(Loop).InducedAirSchedPtr, ">=", 0.0, "<=", 1.0)) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = " + state.dataHeatBal->ZoneAirBalance(Loop).Name + ":  Error found in " + cAlphaFieldNames(4) +
                                " = " + cAlphaArgs(4));
            ShowContinueError(state, "Schedule values must be (>=0., <=1.)");
            ErrorsFound = true;
        }

        // Check whether this zone is also controleld by hybrid ventilation object with ventilation control option or not
        ControlFlag = GetHybridVentilationControlStatus(state, state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr);
        if (ControlFlag && state.dataHeatBal->ZoneAirBalance(Loop).BalanceMethod == AirBalanceQuadrature) {
            state.dataHeatBal->ZoneAirBalance(Loop).BalanceMethod = AirBalanceNone;
            ShowWarningError(state,
                             cCurrentModuleObject + " = " + state.dataHeatBal->ZoneAirBalance(Loop).Name + ": This Zone (" + cAlphaArgs(2) +
                                 ") is controlled by AvailabilityManager:HybridVentilation with Simple Airflow Control Type option.");
            ShowContinueError(state,
                              "Air balance method type QUADRATURE and Simple Airflow Control Type cannot co-exist. The NONE method is assigned");
        }

        if (state.dataHeatBal->ZoneAirBalance(Loop).BalanceMethod == AirBalanceQuadrature) {
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceHeatLoss,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceHeatGain,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceLatentLoss,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceLatentGain,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Total Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceTotalLoss,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Total Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceTotalGain,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Current Density Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceVdotCurDensity,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Standard Density Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceVdotStdDensity,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Current Density Volume",
                                OutputProcessor::Unit::m3,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceVolumeCurDensity,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Standard Density Volume",
                                OutputProcessor::Unit::m3,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceVolumeStdDensity,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Mass",
                                OutputProcessor::Unit::kg,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceMass,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceMdot,
                                "System",
                                "Average",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Changes per Hour",
                                OutputProcessor::Unit::ach,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceAirChangeRate,
                                "System",
                                "Average",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
            SetupOutputVariable(state,
                                "Zone Combined Outdoor Air Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnAirRpt(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).OABalanceFanElec,
                                "System",
                                "Sum",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name,
                                _,
                                "Electricity",
                                "Fans",
                                "Ventilation (simple)",
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneAirBalance(Loop).ZonePtr).Name);
        }
    }

    cCurrentModuleObject = "ZoneInfiltration:EffectiveLeakageArea";
    state.dataHeatBal->TotShermGrimsInfiltration = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    cCurrentModuleObject = "ZoneInfiltration:FlowCoefficient";
    state.dataHeatBal->TotAIM2Infiltration = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    cCurrentModuleObject = "ZoneInfiltration:DesignFlowRate";
    state.dataHeatBal->NumInfiltrationStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    state.dataHeatBal->InfiltrationObjects.allocate(state.dataHeatBal->NumInfiltrationStatements);

    state.dataHeatBal->TotDesignFlowInfiltration = 0;
    errFlag = false;
    for (Item = 1; Item <= state.dataHeatBal->NumInfiltrationStatements; ++Item) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Item,
                                                                 cAlphaArgs,
                                                                 NumAlpha,
                                                                 rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataHeatBal->InfiltrationObjects(Item).Name = cAlphaArgs(1);
        Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        ZLItem = 0;
        if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
        if (Item1 > 0) {
            state.dataHeatBal->InfiltrationObjects(Item).StartPtr = state.dataHeatBal->TotDesignFlowInfiltration + 1;
            ++state.dataHeatBal->TotDesignFlowInfiltration;
            state.dataHeatBal->InfiltrationObjects(Item).NumOfZones = 1;
            state.dataHeatBal->InfiltrationObjects(Item).ZoneListActive = false;
            state.dataHeatBal->InfiltrationObjects(Item).ZoneOrZoneListPtr = Item1;
        } else if (ZLItem > 0) {
            state.dataHeatBal->InfiltrationObjects(Item).StartPtr = state.dataHeatBal->TotDesignFlowInfiltration + 1;
            state.dataHeatBal->TotDesignFlowInfiltration += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
            state.dataHeatBal->InfiltrationObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
            state.dataHeatBal->InfiltrationObjects(Item).ZoneListActive = true;
            state.dataHeatBal->InfiltrationObjects(Item).ZoneOrZoneListPtr = ZLItem;
        } else {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
            errFlag = true;
        }
    }

    if (errFlag) {
        ShowSevereError(state, RoutineName + "Errors with invalid names in " + cCurrentModuleObject + " objects.");
        ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
        state.dataHeatBal->TotDesignFlowInfiltration = 0;
    }

    state.dataHeatBal->TotInfiltration =
        state.dataHeatBal->TotDesignFlowInfiltration + state.dataHeatBal->TotShermGrimsInfiltration + state.dataHeatBal->TotAIM2Infiltration;

    state.dataHeatBal->Infiltration.allocate(state.dataHeatBal->TotInfiltration);
    state.dataHeatBalAirMgr->UniqueInfiltrationNames.reserve(static_cast<unsigned>(state.dataHeatBal->TotInfiltration));

    if (state.dataHeatBal->TotDesignFlowInfiltration > 0) {
        Loop = 0;
        cCurrentModuleObject = "ZoneInfiltration:DesignFlowRate";
        for (Item = 1; Item <= state.dataHeatBal->NumInfiltrationStatements; ++Item) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Item,
                                                                     cAlphaArgs,
                                                                     NumAlpha,
                                                                     rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            for (Item1 = 1; Item1 <= state.dataHeatBal->InfiltrationObjects(Item).NumOfZones; ++Item1) {
                ++Loop;
                if (!state.dataHeatBal->InfiltrationObjects(Item).ZoneListActive) {
                    state.dataHeatBal->Infiltration(Loop).Name = cAlphaArgs(1);
                    state.dataHeatBal->Infiltration(Loop).ZonePtr = state.dataHeatBal->InfiltrationObjects(Item).ZoneOrZoneListPtr;
                } else {
                    CheckCreatedZoneItemName(
                        state,
                        RoutineName,
                        cCurrentModuleObject,
                        state.dataHeatBal
                            ->Zone(state.dataHeatBal->ZoneList(state.dataHeatBal->InfiltrationObjects(Item).ZoneOrZoneListPtr).Zone(Item1))
                            .Name,
                        state.dataHeatBal->ZoneList(state.dataHeatBal->InfiltrationObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                        state.dataHeatBal->InfiltrationObjects(Item).Name,
                        state.dataHeatBal->Infiltration,
                        Loop - 1,
                        state.dataHeatBal->Infiltration(Loop).Name,
                        errFlag);
                    state.dataHeatBal->Infiltration(Loop).ZonePtr =
                        state.dataHeatBal->ZoneList(state.dataHeatBal->InfiltrationObjects(Item).ZoneOrZoneListPtr).Zone(Item1);
                    if (errFlag) ErrorsFound = true;
                }

                state.dataHeatBal->Infiltration(Loop).ModelType = InfiltrationDesignFlowRate;
                state.dataHeatBal->Infiltration(Loop).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataHeatBal->Infiltration(Loop).SchedPtr == 0) {
                    if (Item1 == 1) {
                        if (lAlphaFieldBlanks(3)) {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(3) +
                                                " is required but field is blank.");
                        } else {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " +
                                                cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                        }
                        ErrorsFound = true;
                    }
                }

                // setup a flag if the outdoor air balance method is applied
                if (state.dataHeatBal->Infiltration(Loop).ZonePtr > 0 && state.dataHeatBal->TotZoneAirBalance > 0) {
                    for (i = 1; i <= state.dataHeatBal->TotZoneAirBalance; ++i) {
                        if (state.dataHeatBal->Infiltration(Loop).ZonePtr == state.dataHeatBal->ZoneAirBalance(i).ZonePtr) {
                            if (state.dataHeatBal->ZoneAirBalance(i).BalanceMethod == AirBalanceQuadrature) {
                                state.dataHeatBal->Infiltration(Loop).QuadratureSum = true;
                                state.dataHeatBal->Infiltration(Loop).OABalancePtr = i;
                                break;
                            }
                        }
                    }
                }

                // Infiltration equipment design level calculation method.
                {
                    auto const SELECT_CASE_var(cAlphaArgs(4));
                    if ((SELECT_CASE_var == "FLOW") || (SELECT_CASE_var == "FLOW/ZONE")) {
                        state.dataHeatBal->Infiltration(Loop).DesignLevel = rNumericArgs(1);
                        if (lAlphaFieldBlanks(1)) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Infiltration(Loop).Name + "\", " +
                                                 cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(1) +
                                                 ", but that field is blank.  0 Infiltration will result.");
                        }

                    } else if (SELECT_CASE_var == "FLOW/AREA") {
                        if (state.dataHeatBal->Infiltration(Loop).ZonePtr != 0) {
                            if (rNumericArgs(2) >= 0.0) {
                                state.dataHeatBal->Infiltration(Loop).DesignLevel =
                                    rNumericArgs(2) * state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).FloorArea;
                                if (state.dataHeatBal->Infiltration(Loop).ZonePtr > 0) {
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).FloorArea <= 0.0) {
                                        ShowWarningError(state,
                                                         RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Infiltration(Loop).Name +
                                                             "\", " + cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(2) +
                                                             ", but Zone Floor Area = 0.  0 Infiltration will result.");
                                    }
                                }
                            } else {
                                ShowSevereError(state,
                                                format("{}{}=\"{}\", invalid flow/area specification [<0.0]={:.3R}",
                                                       RoutineName,
                                                       cCurrentModuleObject,
                                                       state.dataHeatBal->Infiltration(Loop).Name,
                                                       rNumericArgs(2)));
                                ErrorsFound = true;
                            }
                        }
                        if (lAlphaFieldBlanks(2)) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Infiltration(Loop).Name + "\", " +
                                                 cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(2) +
                                                 ", but that field is blank.  0 Infiltration will result.");
                        }

                    } else if (SELECT_CASE_var == "FLOW/EXTERIORAREA") {
                        if (state.dataHeatBal->Infiltration(Loop).ZonePtr != 0) {
                            if (rNumericArgs(3) >= 0.0) {
                                state.dataHeatBal->Infiltration(Loop).DesignLevel =
                                    rNumericArgs(3) * state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).ExteriorTotalSurfArea;
                                if (state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).ExteriorTotalSurfArea <= 0.0) {
                                    ShowWarningError(state,
                                                     RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Infiltration(Loop).Name +
                                                         "\", " + cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(3) +
                                                         ", but Exterior Surface Area = 0.  0 Infiltration will result.");
                                }
                            } else {
                                ShowSevereError(state,
                                                format("{}{} = \"{}\", invalid flow/exteriorarea specification [<0.0]={:.3R}",
                                                       RoutineName,
                                                       cCurrentModuleObject,
                                                       state.dataHeatBal->Infiltration(Loop).Name,
                                                       rNumericArgs(3)));
                                ErrorsFound = true;
                            }
                        }
                        if (lAlphaFieldBlanks(3)) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Infiltration(Loop).Name + "\", " +
                                                 cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(3) +
                                                 ", but that field is blank.  0 Infiltration will result.");
                        }
                    } else if (SELECT_CASE_var == "FLOW/EXTERIORWALLAREA") {
                        if (state.dataHeatBal->Infiltration(Loop).ZonePtr != 0) {
                            if (rNumericArgs(3) >= 0.0) {
                                state.dataHeatBal->Infiltration(Loop).DesignLevel =
                                    rNumericArgs(3) * state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).ExtGrossWallArea;
                                if (state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).ExtGrossWallArea <= 0.0) {
                                    ShowWarningError(state,
                                                     RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Infiltration(Loop).Name +
                                                         "\", " + cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(3) +
                                                         ", but Exterior Wall Area = 0.  0 Infiltration will result.");
                                }
                            } else {
                                ShowSevereError(state,
                                                format("{}{} = \"{}\", invalid flow/exteriorwallarea specification [<0.0]={:.3R}",
                                                       RoutineName,
                                                       cCurrentModuleObject,
                                                       state.dataHeatBal->Infiltration(Loop).Name,
                                                       rNumericArgs(3)));
                                ErrorsFound = true;
                            }
                        }
                        if (lAlphaFieldBlanks(3)) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Infiltration(Loop).Name + "\", " +
                                                 cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(3) +
                                                 ", but that field is blank.  0 Infiltration will result.");
                        }
                    } else if (SELECT_CASE_var == "AIRCHANGES/HOUR") {
                        if (state.dataHeatBal->Infiltration(Loop).ZonePtr != 0) {
                            if (rNumericArgs(4) >= 0.0) {
                                state.dataHeatBal->Infiltration(Loop).DesignLevel =
                                    rNumericArgs(4) * state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Volume /
                                    DataGlobalConstants::SecInHour;
                                if (state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Volume <= 0.0) {
                                    ShowWarningError(state,
                                                     RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Infiltration(Loop).Name +
                                                         "\", " + cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(4) +
                                                         ", but Zone Volume = 0.  0 Infiltration will result.");
                                }
                            } else {
                                ShowSevereError(state,
                                                format("{}In {} = \"{}\", invalid ACH (air changes per hour) specification [<0.0]={:.3R}",
                                                       RoutineName,
                                                       cCurrentModuleObject,
                                                       state.dataHeatBal->Infiltration(Loop).Name,
                                                       rNumericArgs(4)));
                                ErrorsFound = true;
                            }
                        }
                        if (lAlphaFieldBlanks(4)) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Infiltration(Loop).Name + "\", " +
                                                 cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(4) +
                                                 ", but that field is blank.  0 Infiltration will result.");
                        }

                    } else {
                        if (Item1 == 1) {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                                "\", invalid calculation method=" + cAlphaArgs(4));
                            ErrorsFound = true;
                        }
                    }
                }

                if (!lNumericFieldBlanks(5)) {
                    state.dataHeatBal->Infiltration(Loop).ConstantTermCoef = rNumericArgs(5);
                } else {
                    state.dataHeatBal->Infiltration(Loop).ConstantTermCoef = 1.0;
                }
                if (!lNumericFieldBlanks(6)) {
                    state.dataHeatBal->Infiltration(Loop).TemperatureTermCoef = rNumericArgs(6);
                } else {
                    state.dataHeatBal->Infiltration(Loop).TemperatureTermCoef = 0.0;
                }
                if (!lNumericFieldBlanks(7)) {
                    state.dataHeatBal->Infiltration(Loop).VelocityTermCoef = rNumericArgs(7);
                } else {
                    state.dataHeatBal->Infiltration(Loop).VelocityTermCoef = 0.0;
                }
                if (!lNumericFieldBlanks(8)) {
                    state.dataHeatBal->Infiltration(Loop).VelocitySQTermCoef = rNumericArgs(8);
                } else {
                    state.dataHeatBal->Infiltration(Loop).VelocitySQTermCoef = 0.0;
                }

                if (state.dataHeatBal->Infiltration(Loop).ConstantTermCoef == 0.0 &&
                    state.dataHeatBal->Infiltration(Loop).TemperatureTermCoef == 0.0 &&
                    state.dataHeatBal->Infiltration(Loop).VelocityTermCoef == 0.0 &&
                    state.dataHeatBal->Infiltration(Loop).VelocitySQTermCoef == 0.0) {
                    if (Item1 == 1) {
                        ShowWarningError(state,
                                         RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", in " + cAlphaFieldNames(2) + "=\"" +
                                             cAlphaArgs(2) + "\".");
                        ShowContinueError(state, "Infiltration Coefficients are all zero.  No Infiltration will be reported.");
                    }
                }
            }
        }
    }

    cCurrentModuleObject = "ZoneInfiltration:EffectiveLeakageArea";
    InfiltCount = state.dataHeatBal->TotDesignFlowInfiltration;
    for (Loop = 1; Loop <= state.dataHeatBal->TotShermGrimsInfiltration; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Loop,
                                                                 cAlphaArgs,
                                                                 NumAlpha,
                                                                 rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        ++InfiltCount;
        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataHeatBalAirMgr->UniqueInfiltrationNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
        state.dataHeatBal->Infiltration(InfiltCount).Name = cAlphaArgs(1);
        state.dataHeatBal->Infiltration(InfiltCount).ModelType = InfiltrationShermanGrimsrud;
        state.dataHeatBal->Infiltration(InfiltCount).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        if (state.dataHeatBal->Infiltration(InfiltCount).ZonePtr == 0) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2) + "\".");
            ErrorsFound = true;
        }

        // setup a flag if the outdoor air balance method is applied
        if (state.dataHeatBal->Infiltration(Loop).ZonePtr > 0 && state.dataHeatBal->TotZoneAirBalance > 0) {
            for (i = 1; i <= state.dataHeatBal->TotZoneAirBalance; ++i) {
                if (state.dataHeatBal->Infiltration(Loop).ZonePtr == state.dataHeatBal->ZoneAirBalance(i).ZonePtr) {
                    if (state.dataHeatBal->ZoneAirBalance(i).BalanceMethod == AirBalanceQuadrature) {
                        state.dataHeatBal->Infiltration(Loop).QuadratureSum = true;
                        state.dataHeatBal->Infiltration(Loop).OABalancePtr = i;
                        break;
                    }
                }
            }
        }

        state.dataHeatBal->Infiltration(InfiltCount).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
        if (state.dataHeatBal->Infiltration(InfiltCount).SchedPtr == 0) {
            if (lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(3) +
                                    " is required but field is blank.");
            } else {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(3) +
                                    "=\"" + cAlphaArgs(3) + "\".");
            }
            ErrorsFound = true;
        }
        state.dataHeatBal->Infiltration(InfiltCount).LeakageArea = rNumericArgs(1);
        state.dataHeatBal->Infiltration(InfiltCount).BasicStackCoefficient = rNumericArgs(2);
        state.dataHeatBal->Infiltration(InfiltCount).BasicWindCoefficient = rNumericArgs(3);

        // check if zone has exterior surfaces
        if (state.dataHeatBal->Infiltration(InfiltCount).ZonePtr > 0) {
            if (state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(InfiltCount).ZonePtr).ExteriorTotalSurfArea <= 0.0) {
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                     "\" does not have surfaces exposed to outdoors.");
                ShowContinueError(state, "Infiltration model is appropriate for exterior zones not interior zones, simulation continues.");
            }
        }
    }

    cCurrentModuleObject = "ZoneInfiltration:FlowCoefficient";
    for (Loop = 1; Loop <= state.dataHeatBal->TotAIM2Infiltration; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Loop,
                                                                 cAlphaArgs,
                                                                 NumAlpha,
                                                                 rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        ++InfiltCount;
        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataHeatBalAirMgr->UniqueInfiltrationNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
        state.dataHeatBal->Infiltration(InfiltCount).Name = cAlphaArgs(1);
        state.dataHeatBal->Infiltration(InfiltCount).ModelType = InfiltrationAIM2;
        state.dataHeatBal->Infiltration(InfiltCount).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        if (state.dataHeatBal->Infiltration(InfiltCount).ZonePtr == 0) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2) + "\".");
            ErrorsFound = true;
        }

        // setup a flag if the outdoor air balance method is applied
        if (state.dataHeatBal->Infiltration(Loop).ZonePtr > 0 && state.dataHeatBal->TotZoneAirBalance > 0) {
            for (i = 1; i <= state.dataHeatBal->TotZoneAirBalance; ++i) {
                if (state.dataHeatBal->Infiltration(Loop).ZonePtr == state.dataHeatBal->ZoneAirBalance(i).ZonePtr) {
                    if (state.dataHeatBal->ZoneAirBalance(i).BalanceMethod == AirBalanceQuadrature) {
                        state.dataHeatBal->Infiltration(Loop).QuadratureSum = true;
                        state.dataHeatBal->Infiltration(Loop).OABalancePtr = i;
                        break;
                    }
                }
            }
        }

        state.dataHeatBal->Infiltration(InfiltCount).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
        if (state.dataHeatBal->Infiltration(InfiltCount).SchedPtr == 0) {
            if (lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(3) +
                                    " is required but field is blank.");
            } else {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(3) +
                                    "=\"" + cAlphaArgs(3) + "\".");
            }
            ErrorsFound = true;
        }
        state.dataHeatBal->Infiltration(InfiltCount).FlowCoefficient = rNumericArgs(1);
        state.dataHeatBal->Infiltration(InfiltCount).AIM2StackCoefficient = rNumericArgs(2);
        state.dataHeatBal->Infiltration(InfiltCount).PressureExponent = rNumericArgs(3);
        state.dataHeatBal->Infiltration(InfiltCount).AIM2WindCoefficient = rNumericArgs(4);
        state.dataHeatBal->Infiltration(InfiltCount).ShelterFactor = rNumericArgs(5);

        // check if zone has exterior surfaces
        if (state.dataHeatBal->Infiltration(InfiltCount).ZonePtr > 0) {
            if (state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(InfiltCount).ZonePtr).ExteriorTotalSurfArea <= 0.0) {
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                     "\" does not have surfaces exposed to outdoors.");
                ShowContinueError(state, "Infiltration model is appropriate for exterior zones not interior zones, simulation continues.");
            }
        }
    }

    // setup zone-level infiltration reports
    for (Loop = 1; Loop <= state.dataHeatBal->TotInfiltration; ++Loop) {
        if (state.dataHeatBal->Infiltration(Loop).ZonePtr > 0 && !state.dataHeatBal->Infiltration(Loop).QuadratureSum) {
            if (RepVarSet(state.dataHeatBal->Infiltration(Loop).ZonePtr)) {
                RepVarSet(state.dataHeatBal->Infiltration(Loop).ZonePtr) = false;
                SetupOutputVariable(state,
                                    "Zone Infiltration Sensible Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilHeatLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Sensible Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilHeatGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Latent Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilLatentLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Latent Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilLatentGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Total Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilTotalLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Total Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilTotalGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Current Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilVdotCurDensity,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Standard Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilVdotStdDensity,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Current Density Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilVolumeCurDensity,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Standard Density Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilVolumeStdDensity,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Mass",
                                    OutputProcessor::Unit::kg,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilMass,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilMdot,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Infiltration Air Change Rate",
                                    OutputProcessor::Unit::ach,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Infiltration(Loop).ZonePtr).InfilAirChangeRate,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Infiltration(Loop).ZonePtr).Name);
            }
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSActuator(state,
                             "Zone Infiltration",
                             state.dataHeatBal->Infiltration(Loop).Name,
                             "Air Exchange Flow Rate",
                             "[m3/s]",
                             state.dataHeatBal->Infiltration(Loop).EMSOverrideOn,
                             state.dataHeatBal->Infiltration(Loop).EMSAirFlowRateValue);
        }
    }
    // VENTILATION Section: The following section is responsible for obtaining the simple ventilation
    // from the user's input file.
    RepVarSet = true;

    cCurrentModuleObject = "ZoneVentilation:DesignFlowRate";
    state.dataHeatBal->NumVentilationStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    cCurrentModuleObject = "ZoneVentilation:WindandStackOpenArea";
    state.dataHeatBal->TotWindAndStackVentilation = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    state.dataHeatBal->VentilationObjects.allocate(state.dataHeatBal->NumVentilationStatements);

    state.dataHeatBal->TotDesignFlowVentilation = 0;
    errFlag = false;
    cCurrentModuleObject = "ZoneVentilation:DesignFlowRate";
    for (Item = 1; Item <= state.dataHeatBal->NumVentilationStatements; ++Item) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Item,
                                                                 cAlphaArgs,
                                                                 NumAlpha,
                                                                 rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        errFlag = ErrorsFound;

        state.dataHeatBal->VentilationObjects(Item).Name = cAlphaArgs(1);

        Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        ZLItem = 0;
        if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->ZoneList);
        if (Item1 > 0) {
            state.dataHeatBal->VentilationObjects(Item).StartPtr = state.dataHeatBal->TotDesignFlowVentilation + 1;
            ++state.dataHeatBal->TotDesignFlowVentilation;
            state.dataHeatBal->VentilationObjects(Item).NumOfZones = 1;
            state.dataHeatBal->VentilationObjects(Item).ZoneListActive = false;
            state.dataHeatBal->VentilationObjects(Item).ZoneOrZoneListPtr = Item1;
        } else if (ZLItem > 0) {
            state.dataHeatBal->VentilationObjects(Item).StartPtr = state.dataHeatBal->TotDesignFlowVentilation + 1;
            state.dataHeatBal->TotDesignFlowVentilation += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
            state.dataHeatBal->VentilationObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
            state.dataHeatBal->VentilationObjects(Item).ZoneListActive = true;
            state.dataHeatBal->VentilationObjects(Item).ZoneOrZoneListPtr = ZLItem;
        } else {
            ShowSevereError(
                state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found.");
            ErrorsFound = true;
            errFlag = true;
        }
    }

    if (errFlag) {
        ShowSevereError(state, RoutineName + "Errors with invalid names in " + cCurrentModuleObject + " objects.");
        ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
        state.dataHeatBal->TotDesignFlowVentilation = 0;
    }

    state.dataHeatBal->TotVentilation = state.dataHeatBal->TotDesignFlowVentilation + state.dataHeatBal->TotWindAndStackVentilation;
    state.dataHeatBal->Ventilation.allocate(state.dataHeatBal->TotVentilation);

    if (state.dataHeatBal->TotDesignFlowVentilation > 0) {
        Loop = 0;
        cCurrentModuleObject = "ZoneVentilation:DesignFlowRate";
        for (Item = 1; Item <= state.dataHeatBal->NumVentilationStatements; ++Item) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Item,
                                                                     cAlphaArgs,
                                                                     NumAlpha,
                                                                     rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            for (Item1 = 1; Item1 <= state.dataHeatBal->VentilationObjects(Item).NumOfZones; ++Item1) {
                ++Loop;
                if (!state.dataHeatBal->VentilationObjects(Item).ZoneListActive) {
                    state.dataHeatBal->Ventilation(Loop).Name = cAlphaArgs(1);
                    state.dataHeatBal->Ventilation(Loop).ZonePtr = state.dataHeatBal->VentilationObjects(Item).ZoneOrZoneListPtr;
                } else {
                    CheckCreatedZoneItemName(
                        state,
                        RoutineName,
                        cCurrentModuleObject,
                        state.dataHeatBal
                            ->Zone(state.dataHeatBal->ZoneList(state.dataHeatBal->VentilationObjects(Item).ZoneOrZoneListPtr).Zone(Item1))
                            .Name,
                        state.dataHeatBal->ZoneList(state.dataHeatBal->VentilationObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                        state.dataHeatBal->VentilationObjects(Item).Name,
                        state.dataHeatBal->Ventilation,
                        Loop - 1,
                        state.dataHeatBal->Ventilation(Loop).Name,
                        errFlag);
                    state.dataHeatBal->Ventilation(Loop).ZonePtr =
                        state.dataHeatBal->ZoneList(state.dataHeatBal->VentilationObjects(Item).ZoneOrZoneListPtr).Zone(Item1);
                    if (errFlag) ErrorsFound = true;
                }

                // setup a flag if the outdoor air balance method is applied
                if (state.dataHeatBal->Ventilation(Loop).ZonePtr > 0 && state.dataHeatBal->TotZoneAirBalance > 0) {
                    for (i = 1; i <= state.dataHeatBal->TotZoneAirBalance; ++i) {
                        if (state.dataHeatBal->Ventilation(Loop).ZonePtr == state.dataHeatBal->ZoneAirBalance(i).ZonePtr) {
                            if (state.dataHeatBal->ZoneAirBalance(i).BalanceMethod == AirBalanceQuadrature) {
                                state.dataHeatBal->Ventilation(Loop).QuadratureSum = true;
                                state.dataHeatBal->Ventilation(Loop).OABalancePtr = i;
                                break;
                            }
                        }
                    }
                }

                state.dataHeatBal->Ventilation(Loop).ModelType = VentilationDesignFlowRate;
                state.dataHeatBal->Ventilation(Loop).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataHeatBal->Ventilation(Loop).SchedPtr == 0) {
                    if (Item1 == 1) {
                        if (lAlphaFieldBlanks(3)) {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(3) +
                                                " is required but field is blank.");
                        } else {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " +
                                                cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                        }
                    }
                    ErrorsFound = true;
                }

                // Ventilation equipment design level calculation method
                {
                    auto const SELECT_CASE_var(cAlphaArgs(4));
                    if ((SELECT_CASE_var == "FLOW") || (SELECT_CASE_var == "FLOW/ZONE")) {
                        state.dataHeatBal->Ventilation(Loop).DesignLevel = rNumericArgs(1);
                        if (lAlphaFieldBlanks(1)) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Ventilation(Loop).Name + "\", " +
                                                 cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(1) +
                                                 ", but that field is blank.  0 Ventilation will result.");
                        }

                    } else if (SELECT_CASE_var == "FLOW/AREA") {
                        if (state.dataHeatBal->Ventilation(Loop).ZonePtr != 0) {
                            if (rNumericArgs(2) >= 0.0) {
                                state.dataHeatBal->Ventilation(Loop).DesignLevel =
                                    rNumericArgs(2) * state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).FloorArea;
                                if (state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).FloorArea <= 0.0) {
                                    ShowWarningError(state,
                                                     RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Ventilation(Loop).Name + "\", " +
                                                         cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(2) +
                                                         ", but Zone Floor Area = 0.  0 Ventilation will result.");
                                }
                            } else {
                                ShowSevereError(state,
                                                format("{}{}=\"{}\", invalid flow/area specification [<0.0]={:.3R}",
                                                       RoutineName,
                                                       cCurrentModuleObject,
                                                       state.dataHeatBal->Ventilation(Loop).Name,
                                                       rNumericArgs(2)));
                                ErrorsFound = true;
                            }
                        }
                        if (lAlphaFieldBlanks(2)) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Ventilation(Loop).Name + "\", " +
                                                 cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(2) +
                                                 ", but that field is blank.  0 Ventilation will result.");
                        }

                    } else if (SELECT_CASE_var == "FLOW/PERSON") {
                        if (state.dataHeatBal->Ventilation(Loop).ZonePtr != 0) {
                            if (rNumericArgs(3) >= 0.0) {
                                state.dataHeatBal->Ventilation(Loop).DesignLevel =
                                    rNumericArgs(3) * state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).TotOccupants;
                                if (state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).TotOccupants <= 0.0) {
                                    ShowWarningError(state,
                                                     RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Ventilation(Loop).Name + "\", " +
                                                         cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(3) +
                                                         ", but Zone Total Occupants = 0.  0 Ventilation will result.");
                                }
                            } else {
                                ShowSevereError(state,
                                                format("{}{}=\"{}\", invalid flow/person specification [<0.0]={:.3R}",
                                                       RoutineName,
                                                       cCurrentModuleObject,
                                                       state.dataHeatBal->Ventilation(Loop).Name,
                                                       rNumericArgs(3)));
                                ErrorsFound = true;
                            }
                        }
                        if (lAlphaFieldBlanks(3)) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Ventilation(Loop).Name + "\", " +
                                                 cAlphaFieldNames(4) + "specifies " + cNumericFieldNames(3) +
                                                 ", but that field is blank.  0 Ventilation will result.");
                        }

                    } else if (SELECT_CASE_var == "AIRCHANGES/HOUR") {
                        if (state.dataHeatBal->Ventilation(Loop).ZonePtr != 0) {
                            if (rNumericArgs(4) >= 0.0) {
                                state.dataHeatBal->Ventilation(Loop).DesignLevel =
                                    rNumericArgs(4) * state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Volume /
                                    DataGlobalConstants::SecInHour;
                                if (state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Volume <= 0.0) {
                                    ShowWarningError(state,
                                                     RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Ventilation(Loop).Name + "\", " +
                                                         cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(4) +
                                                         ", but Zone Volume = 0.  0 Ventilation will result.");
                                }
                            } else {
                                ShowSevereError(state,
                                                format("{}{}=\"{}\", invalid ACH (air changes per hour) specification [<0.0]={:.3R}",
                                                       RoutineName,
                                                       cCurrentModuleObject,
                                                       state.dataHeatBal->Ventilation(Loop).Name,
                                                       rNumericArgs(5)));
                                ErrorsFound = true;
                            }
                        }
                        if (lAlphaFieldBlanks(4)) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Ventilation(Loop).Name + "\", " +
                                                 cAlphaFieldNames(4) + " specifies " + cNumericFieldNames(4) +
                                                 ", but that field is blank.  0 Ventilation will result.");
                        }

                    } else {
                        if (Item1 == 1) {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                                "\", invalid calculation method=" + cAlphaArgs(4));
                            ErrorsFound = true;
                        }
                    }
                }

                {
                    auto const SELECT_CASE_var(cAlphaArgs(5)); // Fan type character input-->convert to integer
                    if (SELECT_CASE_var == "EXHAUST") {
                        state.dataHeatBal->Ventilation(Loop).FanType = ExhaustVentilation;
                    } else if (SELECT_CASE_var == "INTAKE") {
                        state.dataHeatBal->Ventilation(Loop).FanType = IntakeVentilation;
                    } else if ((SELECT_CASE_var == "NATURAL") || (SELECT_CASE_var == "NONE") || (SELECT_CASE_var == std::string())) {
                        state.dataHeatBal->Ventilation(Loop).FanType = NaturalVentilation;
                    } else if (SELECT_CASE_var == "BALANCED") {
                        state.dataHeatBal->Ventilation(Loop).FanType = BalancedVentilation;
                    } else {
                        if (Item1 == 1) {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Ventilation(Loop).Name + "\". invalid " +
                                                cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                            ErrorsFound = true;
                        }
                    }
                }

                state.dataHeatBal->Ventilation(Loop).FanPressure = rNumericArgs(5);
                if (state.dataHeatBal->Ventilation(Loop).FanPressure < 0.0) {
                    if (Item1 == 1) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Ventilation(Loop).Name + "\", " +
                                            cNumericFieldNames(5) + " must be >=0");
                        ErrorsFound = true;
                    }
                }

                state.dataHeatBal->Ventilation(Loop).FanEfficiency = rNumericArgs(6);
                if ((state.dataHeatBal->Ventilation(Loop).FanEfficiency <= 0.0) || (state.dataHeatBal->Ventilation(Loop).FanEfficiency > 1.0)) {
                    if (Item1 == 1) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + state.dataHeatBal->Ventilation(Loop).Name + "\"," +
                                            cNumericFieldNames(6) + " must be in range >0 and <= 1");
                        ErrorsFound = true;
                    }
                }

                // Override any user input for cases where natural ventilation is being used
                if (state.dataHeatBal->Ventilation(Loop).FanType == NaturalVentilation) {
                    state.dataHeatBal->Ventilation(Loop).FanPressure = 0.0;
                    state.dataHeatBal->Ventilation(Loop).FanEfficiency = 1.0;
                }

                if (!lNumericFieldBlanks(7)) {
                    state.dataHeatBal->Ventilation(Loop).ConstantTermCoef = rNumericArgs(7);
                } else {
                    state.dataHeatBal->Ventilation(Loop).ConstantTermCoef = 1.0;
                }
                if (!lNumericFieldBlanks(8)) {
                    state.dataHeatBal->Ventilation(Loop).TemperatureTermCoef = rNumericArgs(8);
                } else {
                    state.dataHeatBal->Ventilation(Loop).TemperatureTermCoef = 0.0;
                }
                if (!lNumericFieldBlanks(9)) {
                    state.dataHeatBal->Ventilation(Loop).VelocityTermCoef = rNumericArgs(9);
                } else {
                    state.dataHeatBal->Ventilation(Loop).VelocityTermCoef = 0.0;
                }
                if (!lNumericFieldBlanks(10)) {
                    state.dataHeatBal->Ventilation(Loop).VelocitySQTermCoef = rNumericArgs(10);
                } else {
                    state.dataHeatBal->Ventilation(Loop).VelocitySQTermCoef = 0.0;
                }

                if (state.dataHeatBal->Ventilation(Loop).ConstantTermCoef == 0.0 && state.dataHeatBal->Ventilation(Loop).TemperatureTermCoef == 0.0 &&
                    state.dataHeatBal->Ventilation(Loop).VelocityTermCoef == 0.0 && state.dataHeatBal->Ventilation(Loop).VelocitySQTermCoef == 0.0) {
                    if (Item1 == 1) {
                        ShowWarningError(state,
                                         RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", in " + cAlphaFieldNames(2) + "=\"" +
                                             cAlphaArgs(2) + "\".");
                        ShowContinueError(state, "Ventilation Coefficients are all zero.  No Ventilation will be reported.");
                    }
                }

                if (!lNumericFieldBlanks(11)) {
                    state.dataHeatBal->Ventilation(Loop).MinIndoorTemperature = rNumericArgs(11);
                } else {
                    state.dataHeatBal->Ventilation(Loop).MinIndoorTemperature = -VentilTempLimit;
                }
                //    Ventilation(Loop)%MinIndoorTemperature = rNumericArgs(11)
                if ((state.dataHeatBal->Ventilation(Loop).MinIndoorTemperature < -VentilTempLimit) ||
                    (state.dataHeatBal->Ventilation(Loop).MinIndoorTemperature > VentilTempLimit)) {
                    if (Item1 == 1) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" must have " + cNumericFieldNames(11) +
                                            " between -100C and 100C.");
                        ShowContinueError(state, format("...value entered=[{:.2R}].", rNumericArgs(11)));
                        ErrorsFound = true;
                    }
                }

                state.dataHeatBal->Ventilation(Loop).MinIndoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(6));
                if (state.dataHeatBal->Ventilation(Loop).MinIndoorTempSchedPtr > 0) {
                    if (Item1 == 1) {
                        if (!lNumericFieldBlanks(11))
                            ShowWarningError(
                                state,
                                RoutineName +
                                    "The Minimum Indoor Temperature value and schedule are provided. The scheduled temperature will be used in the " +
                                    cCurrentModuleObject + " object = " + cAlphaArgs(1));
                        // Check min and max values in the schedule to ensure both values are within the range
                        if (!CheckScheduleValueMinMax(
                                state, state.dataHeatBal->Ventilation(Loop).MinIndoorTempSchedPtr, ">=", -VentilTempLimit, "<=", VentilTempLimit)) {
                            ShowSevereError(
                                state,
                                RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                    " must have a minimum indoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(6));
                            ErrorsFound = true;
                        }
                    }
                }
                if (state.dataHeatBal->Ventilation(Loop).MinIndoorTempSchedPtr == 0 && lNumericFieldBlanks(11) && (!lAlphaFieldBlanks(6))) {
                    if (Item1 == 1) {
                        ShowWarningError(
                            state,
                            format("{}{}: the value field is blank and schedule field is invalid. The default value will be used ({:.1R}) ",
                                   RoutineName,
                                   cNumericFieldNames(11),
                                   -VentilTempLimit));
                        ShowContinueError(state,
                                          "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
                    }
                }
                // Check Minimum indoor temperature value and schedule fields
                if (!lNumericFieldBlanks(11) && (!cAlphaArgs(6).empty() && state.dataHeatBal->Ventilation(Loop).MinIndoorTempSchedPtr == 0)) {
                    if (Item1 == 1) {
                        ShowWarningError(state,
                                         format("{}{} = {} is invalid. The constant value will be used at {:.1R} degrees C ",
                                                RoutineName,
                                                cAlphaFieldNames(6),
                                                cAlphaArgs(6),
                                                rNumericArgs(11)));
                        ShowContinueError(state,
                                          "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
                    }
                }

                if (!lNumericFieldBlanks(12)) {
                    state.dataHeatBal->Ventilation(Loop).MaxIndoorTemperature = rNumericArgs(12);
                } else {
                    state.dataHeatBal->Ventilation(Loop).MaxIndoorTemperature = VentilTempLimit;
                }
                if ((state.dataHeatBal->Ventilation(Loop).MaxIndoorTemperature < -VentilTempLimit) ||
                    (state.dataHeatBal->Ventilation(Loop).MaxIndoorTemperature > VentilTempLimit)) {
                    if (Item1 == 1) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            " must have a maximum indoor temperature between -100C and 100C");
                        ErrorsFound = true;
                    }
                }

                state.dataHeatBal->Ventilation(Loop).MaxIndoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(7));
                if (state.dataHeatBal->Ventilation(Loop).MaxIndoorTempSchedPtr > 0) {
                    if (Item1 == 1) {
                        if (!lNumericFieldBlanks(12))
                            ShowWarningError(
                                state,
                                RoutineName +
                                    "The Maximum Indoor Temperature value and schedule are provided. The scheduled temperature will be used in the " +
                                    cCurrentModuleObject + " object = " + cAlphaArgs(1));
                        // Check min and max values in the schedule to ensure both values are within the range
                        if (!CheckScheduleValueMinMax(
                                state, state.dataHeatBal->Ventilation(Loop).MaxIndoorTempSchedPtr, ">=", -VentilTempLimit, "<=", VentilTempLimit)) {
                            ShowSevereError(
                                state,
                                cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                    " must have a maximum indoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(7));
                            ErrorsFound = true;
                        }
                    }
                }
                if (state.dataHeatBal->Ventilation(Loop).MaxIndoorTempSchedPtr == 0 && lNumericFieldBlanks(12) && (!lAlphaFieldBlanks(7))) {
                    if (Item1 == 1) {
                        ShowWarningError(
                            state,
                            format("{}{}: the value field is blank and schedule field is invalid. The default value will be used ({:.1R}) ",
                                   RoutineName,
                                   cNumericFieldNames(12),
                                   VentilTempLimit));
                        ShowContinueError(state,
                                          "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
                    }
                }
                // Check Maximum indoor temperature value and schedule fields
                if (!lNumericFieldBlanks(12) && ((!lAlphaFieldBlanks(7)) && state.dataHeatBal->Ventilation(Loop).MaxIndoorTempSchedPtr == 0)) {
                    if (Item1 == 1) {
                        ShowWarningError(state,
                                         format("{}{} = {} is invalid. The constant value will be used at {:.1R} degrees C ",
                                                RoutineName,
                                                cAlphaFieldNames(7),
                                                cAlphaArgs(7),
                                                rNumericArgs(12)));
                        ShowContinueError(state,
                                          "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
                    }
                }

                if (!lNumericFieldBlanks(13)) {
                    state.dataHeatBal->Ventilation(Loop).DelTemperature = rNumericArgs(13);
                } else {
                    state.dataHeatBal->Ventilation(Loop).DelTemperature = -VentilTempLimit;
                }
                //    Ventilation(Loop)%DelTemperature = rNumericArgs(13)  !  3/12/03  Negative del temp now allowed COP

                state.dataHeatBal->Ventilation(Loop).DeltaTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(8));
                if (state.dataHeatBal->Ventilation(Loop).DeltaTempSchedPtr > 0) {
                    if (Item1 == 1) {
                        if (!lNumericFieldBlanks(13))
                            ShowWarningError(
                                state,
                                RoutineName +
                                    "The Delta Temperature value and schedule are provided. The scheduled temperature will be used in the " +
                                    cCurrentModuleObject + " object = " + cAlphaArgs(1));
                        // Check min value in the schedule to ensure both values are within the range
                        if (GetScheduleMinValue(state, state.dataHeatBal->Ventilation(Loop).DeltaTempSchedPtr) < -VentilTempLimit) {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                                " must have a delta temperature equal to or above -100C defined in the schedule = " + cAlphaArgs(8));
                            ErrorsFound = true;
                        }
                    }
                }
                if (state.dataHeatBal->Ventilation(Loop).DeltaTempSchedPtr == 0 && lNumericFieldBlanks(13) && (!lAlphaFieldBlanks(8))) {
                    if (Item1 == 1) {
                        ShowWarningError(
                            state,
                            format("{}{}: the value field is blank and schedule field is invalid. The default value will be used ({:.1R}) ",
                                   RoutineName,
                                   cNumericFieldNames(13),
                                   VentilTempLimit));
                        ShowContinueError(state,
                                          "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
                    }
                }

                // Check delta temperature value and schedule fields
                //    if (lAlphaFieldBlanks(13) .AND. cAlphaArgs(8) .EQ. BlankString) THEN
                //      CALL ShowWarningError(state, RoutineName//'Both the delta temperature value and delta schedule are blank. ')
                //      CALL ShowContinueError(state, 'Will set the temperature to a constant value of '//TRIM(format("{:.1R}", -VentilTempLimit))
                //      &
                //           //' degrees C ')
                //      CALL ShowContinueError(state, 'in the Ventilation object = '//TRIM(cAlphaArgs(1))//' and the simulation continues...')
                //    END IF
                if (!lNumericFieldBlanks(13) && ((!lAlphaFieldBlanks(8)) && state.dataHeatBal->Ventilation(Loop).DeltaTempSchedPtr == 0)) {
                    if (Item1 == 1) {
                        ShowWarningError(state,
                                         format("{}{} = {} is invalid. The constant value will be used at {:.1R} degrees C ",
                                                RoutineName,
                                                cAlphaFieldNames(8),
                                                cAlphaArgs(8),
                                                rNumericArgs(13)));
                        ShowContinueError(state,
                                          "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
                    }
                }

                if (!lNumericFieldBlanks(14)) {
                    state.dataHeatBal->Ventilation(Loop).MinOutdoorTemperature = rNumericArgs(14);
                } else {
                    state.dataHeatBal->Ventilation(Loop).MinOutdoorTemperature = -VentilTempLimit;
                }
                if ((state.dataHeatBal->Ventilation(Loop).MinOutdoorTemperature < -VentilTempLimit) ||
                    (state.dataHeatBal->Ventilation(Loop).MinOutdoorTemperature > VentilTempLimit)) {
                    if (Item1 == 1) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) + " must have " +
                                            cNumericFieldNames(14) + " between -100C and 100C");
                        ErrorsFound = true;
                    }
                }

                state.dataHeatBal->Ventilation(Loop).MinOutdoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(9));
                if (Item1 == 1) {
                    if (state.dataHeatBal->Ventilation(Loop).MinOutdoorTempSchedPtr > 0) {
                        if (!lNumericFieldBlanks(14))
                            ShowWarningError(state,
                                             RoutineName +
                                                 "The Minimum Outdoor Temperature value and schedule are provided. The scheduled temperature will be "
                                                 "used in the " +
                                                 cCurrentModuleObject + " object = " + cAlphaArgs(1));
                        // Check min and max values in the schedule to ensure both values are within the range
                        if (!CheckScheduleValueMinMax(
                                state, state.dataHeatBal->Ventilation(Loop).MinOutdoorTempSchedPtr, ">=", -VentilTempLimit, "<=", VentilTempLimit)) {
                            ShowSevereError(
                                state,
                                RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                    " must have a minimum outdoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(9));
                            ErrorsFound = true;
                        }
                    }
                    if (state.dataHeatBal->Ventilation(Loop).MinOutdoorTempSchedPtr == 0 && lNumericFieldBlanks(14) && (!lAlphaFieldBlanks(9))) {
                        ShowWarningError(state,
                                         format("{}Minimum Outdoor Temperature: the value field is blank and schedule field is invalid. The "
                                                "default value will be used ({:.1R}) ",
                                                RoutineName,
                                                -VentilTempLimit));
                        ShowContinueError(state,
                                          "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
                    }
                    // Check Minimum outdoor temperature value and schedule fields
                    if (!lNumericFieldBlanks(14) && ((!lAlphaFieldBlanks(9)) && state.dataHeatBal->Ventilation(Loop).MinOutdoorTempSchedPtr == 0)) {
                        ShowWarningError(state,
                                         format("{}{} = {} is invalid. The constant value will be used at {:.1R} degrees C ",
                                                RoutineName,
                                                cAlphaFieldNames(9),
                                                cAlphaArgs(9),
                                                rNumericArgs(14)));
                        ShowContinueError(state,
                                          "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
                    }
                }

                if (!lNumericFieldBlanks(15)) {
                    state.dataHeatBal->Ventilation(Loop).MaxOutdoorTemperature = rNumericArgs(15);
                } else {
                    state.dataHeatBal->Ventilation(Loop).MaxOutdoorTemperature = VentilTempLimit;
                }
                if (Item1 == 1) {
                    if ((state.dataHeatBal->Ventilation(Loop).MaxOutdoorTemperature < -VentilTempLimit) ||
                        (state.dataHeatBal->Ventilation(Loop).MaxOutdoorTemperature > VentilTempLimit)) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) + " must have a " +
                                            cNumericFieldNames(15) + " between -100C and 100C");
                        ErrorsFound = true;
                    }
                }

                state.dataHeatBal->Ventilation(Loop).MaxOutdoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(10));
                if (Item1 == 1) {
                    if (state.dataHeatBal->Ventilation(Loop).MaxOutdoorTempSchedPtr > 0) {
                        if (!lNumericFieldBlanks(15))
                            ShowWarningError(state,
                                             RoutineName +
                                                 "The Maximum Outdoor Temperature value and schedule are provided. The scheduled temperature will be "
                                                 "used in the " +
                                                 cCurrentModuleObject + " object = " + cAlphaArgs(1));
                        if (!CheckScheduleValueMinMax(
                                state, state.dataHeatBal->Ventilation(Loop).MaxOutdoorTempSchedPtr, ">=", -VentilTempLimit, "<=", VentilTempLimit)) {
                            ShowSevereError(
                                state,
                                RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                    " must have a maximum outdoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(10));
                            ErrorsFound = true;
                        }
                    }
                    if (state.dataHeatBal->Ventilation(Loop).MaxOutdoorTempSchedPtr == 0 && lNumericFieldBlanks(15) && (!lAlphaFieldBlanks(10))) {
                        ShowWarningError(
                            state,
                            format("{}{}: the value field is blank and schedule field is invalid. The default value will be used ({:.1R}) ",
                                   RoutineName,
                                   cNumericFieldNames(15),
                                   VentilTempLimit));
                        ShowContinueError(state,
                                          "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
                    }
                    // Check Maximum outdoor temperature value and schedule fields
                    if (!lNumericFieldBlanks(15) && ((!lAlphaFieldBlanks(10)) && state.dataHeatBal->Ventilation(Loop).MaxOutdoorTempSchedPtr == 0)) {
                        ShowWarningError(state,
                                         format("{}{} = {}is invalid. The constant value will be used at {:.1R} degrees C ",
                                                RoutineName,
                                                cAlphaFieldNames(10),
                                                cAlphaArgs(10),
                                                rNumericArgs(15)));
                        ShowContinueError(state,
                                          "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
                    }
                }

                if (!lNumericFieldBlanks(16)) {
                    state.dataHeatBal->Ventilation(Loop).MaxWindSpeed = rNumericArgs(16);
                } else {
                    state.dataHeatBal->Ventilation(Loop).MaxWindSpeed = VentilWSLimit;
                }
                if (Item1 == 1) {
                    if ((state.dataHeatBal->Ventilation(Loop).MaxWindSpeed < -VentilWSLimit) ||
                        (state.dataHeatBal->Ventilation(Loop).MaxWindSpeed > VentilWSLimit)) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                            " must have a maximum wind speed between -40 m/s and 40 m/s");
                        ErrorsFound = true;
                    }
                }

                // Report variables should be added for individual VENTILATION objects, in addition to zone totals below

                if (state.dataHeatBal->Ventilation(Loop).ZonePtr > 0) {
                    if (RepVarSet(state.dataHeatBal->Ventilation(Loop).ZonePtr) && !state.dataHeatBal->Ventilation(Loop).QuadratureSum) {
                        RepVarSet(state.dataHeatBal->Ventilation(Loop).ZonePtr) = false;
                        SetupOutputVariable(state,
                                            "Zone Ventilation Sensible Heat Loss Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilHeatLoss,
                                            "System",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Sensible Heat Gain Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilHeatGain,
                                            "System",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Latent Heat Loss Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilLatentLoss,
                                            "System",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Latent Heat Gain Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilLatentGain,
                                            "System",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Total Heat Loss Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilTotalLoss,
                                            "System",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Total Heat Gain Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilTotalGain,
                                            "System",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Current Density Volume Flow Rate",
                                            OutputProcessor::Unit::m3_s,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilVdotCurDensity,
                                            "System",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Standard Density Volume Flow Rate",
                                            OutputProcessor::Unit::m3_s,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilVdotStdDensity,
                                            "System",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Current Density Volume",
                                            OutputProcessor::Unit::m3,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilVolumeCurDensity,
                                            "System",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Standard Density Volume",
                                            OutputProcessor::Unit::m3,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilVolumeStdDensity,
                                            "System",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Mass",
                                            OutputProcessor::Unit::kg,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilMass,
                                            "System",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilMdot,
                                            "System",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Air Change Rate",
                                            OutputProcessor::Unit::ach,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilAirChangeRate,
                                            "System",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Fan Electricity Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilFanElec,
                                            "System",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name,
                                            _,
                                            "Electricity",
                                            "Fans",
                                            "Ventilation (simple)",
                                            "Building",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Ventilation Air Inlet Temperature",
                                            OutputProcessor::Unit::C,
                                            state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(Loop).ZonePtr).VentilAirTemp,
                                            "System",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(Loop).ZonePtr).Name);
                    }
                }

                if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                    SetupEMSActuator(state,
                                     "Zone Ventilation",
                                     state.dataHeatBal->Ventilation(Loop).Name,
                                     "Air Exchange Flow Rate",
                                     "[m3/s]",
                                     state.dataHeatBal->Ventilation(Loop).EMSSimpleVentOn,
                                     state.dataHeatBal->Ventilation(Loop).EMSimpleVentFlowRate);
                }
            }
        }
    }

    cCurrentModuleObject = "ZoneVentilation:WindandStackOpenArea";
    VentiCount = state.dataHeatBal->TotDesignFlowVentilation;
    for (Loop = 1; Loop <= state.dataHeatBal->TotWindAndStackVentilation; ++Loop) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Loop,
                                                                 cAlphaArgs,
                                                                 NumAlpha,
                                                                 rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);

        VentiCount = state.dataHeatBal->TotDesignFlowVentilation + Loop;
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataHeatBal->Ventilation(VentiCount).Name = cAlphaArgs(1);
        state.dataHeatBal->Ventilation(VentiCount).ModelType = VentilationWindAndStack;

        state.dataHeatBal->Ventilation(VentiCount).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        if (state.dataHeatBal->Ventilation(VentiCount).ZonePtr == 0) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2) + "\".");
            ErrorsFound = true;
        }

        // setup a flag if the outdoor air balance method is applied
        if (state.dataHeatBal->Ventilation(VentiCount).ZonePtr > 0 && state.dataHeatBal->TotZoneAirBalance > 0) {
            for (i = 1; i <= state.dataHeatBal->TotZoneAirBalance; ++i) {
                if (state.dataHeatBal->Ventilation(VentiCount).ZonePtr == state.dataHeatBal->ZoneAirBalance(i).ZonePtr) {
                    if (state.dataHeatBal->ZoneAirBalance(i).BalanceMethod == AirBalanceQuadrature) {
                        state.dataHeatBal->Ventilation(VentiCount).QuadratureSum = true;
                        state.dataHeatBal->Ventilation(VentiCount).OABalancePtr = i;
                        break;
                    }
                }
            }
        }

        state.dataHeatBal->Ventilation(VentiCount).OpenArea = rNumericArgs(1);
        if (state.dataHeatBal->Ventilation(VentiCount).OpenArea < 0.0) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(1) + " must be positive.");
            ErrorsFound = true;
        }

        state.dataHeatBal->Ventilation(VentiCount).OpenAreaSchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
        if (state.dataHeatBal->Ventilation(VentiCount).OpenAreaSchedPtr == 0) {
            if (lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(3) +
                                    " is required but field is blank.");
            } else {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(3) +
                                    "=\"" + cAlphaArgs(3) + "\".");
            }
            ErrorsFound = true;
        }

        state.dataHeatBal->Ventilation(VentiCount).OpenEff = rNumericArgs(2);
        if (state.dataHeatBal->Ventilation(VentiCount).OpenEff != DataGlobalConstants::AutoCalculate &&
            (state.dataHeatBal->Ventilation(VentiCount).OpenEff < 0.0 || state.dataHeatBal->Ventilation(VentiCount).OpenEff > 1.0)) {
            ShowSevereError(
                state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(2) + " must be between 0 and 1.");
            ErrorsFound = true;
        }

        state.dataHeatBal->Ventilation(VentiCount).EffAngle = rNumericArgs(3);
        if (state.dataHeatBal->Ventilation(VentiCount).EffAngle < 0.0 || state.dataHeatBal->Ventilation(VentiCount).EffAngle >= 360.0) {
            ShowSevereError(
                state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(3) + " must be between 0 and 360.");
            ErrorsFound = true;
        }

        state.dataHeatBal->Ventilation(VentiCount).DH = rNumericArgs(4);
        if (state.dataHeatBal->Ventilation(VentiCount).DH < 0.0) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(4) + " must be positive.");
            ErrorsFound = true;
        }

        state.dataHeatBal->Ventilation(VentiCount).DiscCoef = rNumericArgs(5);
        if (state.dataHeatBal->Ventilation(VentiCount).DiscCoef != DataGlobalConstants::AutoCalculate &&
            (state.dataHeatBal->Ventilation(VentiCount).DiscCoef < 0.0 || state.dataHeatBal->Ventilation(VentiCount).DiscCoef > 1.0)) {
            ShowSevereError(
                state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(5) + " must be between 0 and 1.");
            ErrorsFound = true;
        }

        if (!lNumericFieldBlanks(6)) {
            state.dataHeatBal->Ventilation(VentiCount).MinIndoorTemperature = rNumericArgs(6);
        } else {
            state.dataHeatBal->Ventilation(VentiCount).MinIndoorTemperature = -VentilTempLimit;
        }
        if ((state.dataHeatBal->Ventilation(VentiCount).MinIndoorTemperature < -VentilTempLimit) ||
            (state.dataHeatBal->Ventilation(VentiCount).MinIndoorTemperature > VentilTempLimit)) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) + " must have " + cNumericFieldNames(6) +
                                " between -100C and 100C");
            ErrorsFound = true;
        }

        state.dataHeatBal->Ventilation(VentiCount).MinIndoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
        if (state.dataHeatBal->Ventilation(VentiCount).MinIndoorTempSchedPtr > 0) {
            if (!lNumericFieldBlanks(6))
                ShowWarningError(
                    state,
                    RoutineName + "The Minimum Indoor Temperature value and schedule are provided. The scheduled temperature will be used in the " +
                        cCurrentModuleObject + " object = " + cAlphaArgs(1));
            // Check min and max values in the schedule to ensure both values are within the range
            if (!CheckScheduleValueMinMax(
                    state, state.dataHeatBal->Ventilation(VentiCount).MinIndoorTempSchedPtr, ">=", -VentilTempLimit, "<=", VentilTempLimit)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                    " must have a minimum indoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(4));
                ErrorsFound = true;
            }
        }
        if (state.dataHeatBal->Ventilation(VentiCount).MinIndoorTempSchedPtr == 0 && lNumericFieldBlanks(6) && (!lAlphaFieldBlanks(4))) {
            ShowWarningError(state,
                             format("{}{}: the value field is blank and schedule field is invalid. The default value will be used ({:.1R}) ",
                                    RoutineName,
                                    cNumericFieldNames(6),
                                    -VentilTempLimit));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }
        // Check Minimum indoor temperature value and schedule fields
        if (!lNumericFieldBlanks(6) && (!cAlphaArgs(4).empty() && state.dataHeatBal->Ventilation(VentiCount).MinIndoorTempSchedPtr == 0)) {
            ShowWarningError(state,
                             format("{}{} = {} is invalid. The constant value will be used at {:.1R} degrees C ",
                                    RoutineName,
                                    cAlphaFieldNames(4),
                                    cAlphaArgs(4),
                                    rNumericArgs(11)));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }

        if (!lNumericFieldBlanks(7)) {
            state.dataHeatBal->Ventilation(VentiCount).MaxIndoorTemperature = rNumericArgs(7);
        } else {
            state.dataHeatBal->Ventilation(VentiCount).MaxIndoorTemperature = VentilTempLimit;
        }
        if ((state.dataHeatBal->Ventilation(VentiCount).MaxIndoorTemperature < -VentilTempLimit) ||
            (state.dataHeatBal->Ventilation(VentiCount).MaxIndoorTemperature > VentilTempLimit)) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                "\" must have a maximum indoor temperature between -100C and 100C");
            ErrorsFound = true;
        }

        state.dataHeatBal->Ventilation(VentiCount).MaxIndoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(5));
        if (state.dataHeatBal->Ventilation(VentiCount).MaxIndoorTempSchedPtr > 0) {
            if (!lNumericFieldBlanks(7))
                ShowWarningError(
                    state,
                    RoutineName + "The Maximum Indoor Temperature value and schedule are provided. The scheduled temperature will be used in the " +
                        cCurrentModuleObject + " object = " + cAlphaArgs(1));
            // Check min and max values in the schedule to ensure both values are within the range
            if (!CheckScheduleValueMinMax(
                    state, state.dataHeatBal->Ventilation(VentiCount).MaxIndoorTempSchedPtr, ">=", -VentilTempLimit, "<=", VentilTempLimit)) {
                ShowSevereError(state,
                                cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                    " must have a maximum indoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(5));
                ErrorsFound = true;
            }
        }
        if (state.dataHeatBal->Ventilation(VentiCount).MaxIndoorTempSchedPtr == 0 && lNumericFieldBlanks(7) && (!lAlphaFieldBlanks(5))) {
            ShowWarningError(state,
                             format("{}{}: the value field is blank and schedule field is invalid. The default value will be used ({:.1R}) ",
                                    RoutineName,
                                    cNumericFieldNames(7),
                                    VentilTempLimit));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }
        // Check Maximum indoor temperature value and schedule fields
        if (!lNumericFieldBlanks(7) && ((!lAlphaFieldBlanks(5)) && state.dataHeatBal->Ventilation(VentiCount).MaxIndoorTempSchedPtr == 0)) {
            ShowWarningError(state,
                             format("{}{} = {} is invalid. The constant value will be used at {:.1R} degrees C ",
                                    RoutineName,
                                    cAlphaFieldNames(7),
                                    cAlphaArgs(5),
                                    rNumericArgs(7)));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }

        if (!lNumericFieldBlanks(8)) {
            state.dataHeatBal->Ventilation(VentiCount).DelTemperature = rNumericArgs(8);
        } else {
            state.dataHeatBal->Ventilation(VentiCount).DelTemperature = -VentilTempLimit;
        }

        state.dataHeatBal->Ventilation(VentiCount).DeltaTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(6));
        if (state.dataHeatBal->Ventilation(VentiCount).DeltaTempSchedPtr > 0) {
            if (!lNumericFieldBlanks(8))
                ShowWarningError(state,
                                 RoutineName +
                                     "The Delta Temperature value and schedule are provided. The scheduled temperature will be used in the " +
                                     cCurrentModuleObject + " object = " + cAlphaArgs(1));
            // Check min value in the schedule to ensure both values are within the range
            if (GetScheduleMinValue(state, state.dataHeatBal->Ventilation(VentiCount).DeltaTempSchedPtr) < -VentilTempLimit) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                    " must have a delta temperature equal to or above -100C defined in the schedule = " + cAlphaArgs(8));
                ErrorsFound = true;
            }
        }
        if (state.dataHeatBal->Ventilation(VentiCount).DeltaTempSchedPtr == 0 && lNumericFieldBlanks(8) && (!lAlphaFieldBlanks(6))) {
            ShowWarningError(state,
                             format("{}{}: the value field is blank and schedule field is invalid. The default value will be used ({:.1R}) ",
                                    RoutineName,
                                    cNumericFieldNames(8),
                                    VentilTempLimit));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }
        if (!lNumericFieldBlanks(8) && ((!lAlphaFieldBlanks(6)) && state.dataHeatBal->Ventilation(VentiCount).DeltaTempSchedPtr == 0)) {
            ShowWarningError(state,
                             format("{}{} = {} is invalid. The constant value will be used at {:.1R} degrees C ",
                                    RoutineName,
                                    cAlphaFieldNames(6),
                                    cAlphaArgs(6),
                                    rNumericArgs(8)));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }

        if (!lNumericFieldBlanks(9)) {
            state.dataHeatBal->Ventilation(VentiCount).MinOutdoorTemperature = rNumericArgs(9);
        } else {
            state.dataHeatBal->Ventilation(VentiCount).MinOutdoorTemperature = -VentilTempLimit;
        }
        if ((state.dataHeatBal->Ventilation(VentiCount).MinOutdoorTemperature < -VentilTempLimit) ||
            (state.dataHeatBal->Ventilation(VentiCount).MinOutdoorTemperature > VentilTempLimit)) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) + " must have " + cNumericFieldNames(9) +
                                " between -100C and 100C");
            ErrorsFound = true;
        }

        state.dataHeatBal->Ventilation(VentiCount).MinOutdoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(7));
        if (state.dataHeatBal->Ventilation(VentiCount).MinOutdoorTempSchedPtr > 0) {
            if (!lNumericFieldBlanks(9))
                ShowWarningError(
                    state,
                    RoutineName + "The Minimum Outdoor Temperature value and schedule are provided. The scheduled temperature will be used in the " +
                        cCurrentModuleObject + " object = " + cAlphaArgs(1));
            // Check min and max values in the schedule to ensure both values are within the range
            if (!CheckScheduleValueMinMax(
                    state, state.dataHeatBal->Ventilation(VentiCount).MinOutdoorTempSchedPtr, ">=", -VentilTempLimit, "<=", VentilTempLimit)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                    " must have a minimum outdoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(7));
                ErrorsFound = true;
            }
        }
        if (state.dataHeatBal->Ventilation(VentiCount).MinOutdoorTempSchedPtr == 0 && lNumericFieldBlanks(9) && (!lAlphaFieldBlanks(7))) {
            ShowWarningError(state,
                             format("{}Minimum Outdoor Temperature: the value field is blank and schedule field is invalid. The default value "
                                    "will be used ({:.1R}) ",
                                    RoutineName,
                                    -VentilTempLimit));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }
        // Check Minimum outdoor temperature value and schedule fields
        if (!lNumericFieldBlanks(9) && ((!lAlphaFieldBlanks(7)) && state.dataHeatBal->Ventilation(VentiCount).MinOutdoorTempSchedPtr == 0)) {
            ShowWarningError(state,
                             format("{}{} = {} is invalid. The constant value will be used at {:.1R} degrees C ",
                                    RoutineName,
                                    cAlphaFieldNames(7),
                                    cAlphaArgs(7),
                                    rNumericArgs(14)));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }

        if (!lNumericFieldBlanks(10)) {
            state.dataHeatBal->Ventilation(VentiCount).MaxOutdoorTemperature = rNumericArgs(10);
        } else {
            state.dataHeatBal->Ventilation(VentiCount).MaxOutdoorTemperature = VentilTempLimit;
        }
        if ((state.dataHeatBal->Ventilation(VentiCount).MaxOutdoorTemperature < -VentilTempLimit) ||
            (state.dataHeatBal->Ventilation(VentiCount).MaxOutdoorTemperature > VentilTempLimit)) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) + " must have a " + cNumericFieldNames(10) +
                                " between -100C and 100C");
            ErrorsFound = true;
        }

        state.dataHeatBal->Ventilation(VentiCount).MaxOutdoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(8));
        if (state.dataHeatBal->Ventilation(VentiCount).MaxOutdoorTempSchedPtr > 0) {
            if (!lNumericFieldBlanks(10))
                ShowWarningError(
                    state,
                    RoutineName + "The Maximum Outdoor Temperature value and schedule are provided. The scheduled temperature will be used in the " +
                        cCurrentModuleObject + " object = " + cAlphaArgs(1));
            if (!CheckScheduleValueMinMax(
                    state, state.dataHeatBal->Ventilation(VentiCount).MaxOutdoorTempSchedPtr, ">=", -VentilTempLimit, "<=", VentilTempLimit)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                    " must have a maximum outdoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(8));
                ErrorsFound = true;
            }
        }
        if (state.dataHeatBal->Ventilation(VentiCount).MaxOutdoorTempSchedPtr == 0 && lNumericFieldBlanks(10) && (!lAlphaFieldBlanks(8))) {
            ShowWarningError(state,
                             format("{}{}: the value field is blank and schedule field is invalid. The default value will be used ({:.1R}) ",
                                    RoutineName,
                                    cNumericFieldNames(10),
                                    VentilTempLimit));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }
        // Check Maximum outdoor temperature value and schedule fields
        if (!lNumericFieldBlanks(10) && ((!lAlphaFieldBlanks(8)) && state.dataHeatBal->Ventilation(VentiCount).MaxOutdoorTempSchedPtr == 0)) {
            ShowWarningError(state,
                             format("{}{} = {}is invalid. The constant value will be used at {:.1R} degrees C ",
                                    RoutineName,
                                    cAlphaFieldNames(8),
                                    cAlphaArgs(8),
                                    rNumericArgs(10)));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }

        if (!lNumericFieldBlanks(11)) {
            state.dataHeatBal->Ventilation(VentiCount).MaxWindSpeed = rNumericArgs(11);
        } else {
            state.dataHeatBal->Ventilation(VentiCount).MaxWindSpeed = VentilWSLimit;
        }
        if ((state.dataHeatBal->Ventilation(VentiCount).MaxWindSpeed < -VentilWSLimit) ||
            (state.dataHeatBal->Ventilation(VentiCount).MaxWindSpeed > VentilWSLimit)) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                " must have a maximum wind speed between 0 m/s and 40 m/s");
            ErrorsFound = true;
        }

        // Report variables should be added for individual VENTILATION objects, in addition to zone totals below

        if (state.dataHeatBal->Ventilation(VentiCount).ZonePtr > 0) {
            if (RepVarSet(state.dataHeatBal->Ventilation(VentiCount).ZonePtr) && !state.dataHeatBal->Ventilation(Loop).QuadratureSum) {
                RepVarSet(state.dataHeatBal->Ventilation(VentiCount).ZonePtr) = false;
                SetupOutputVariable(state,
                                    "Zone Ventilation Sensible Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilHeatLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Sensible Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilHeatGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Latent Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilLatentLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Latent Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilLatentGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Total Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilTotalLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Total Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilTotalGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Current Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilVdotCurDensity,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Standard Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilVdotStdDensity,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Current Density Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilVolumeCurDensity,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Standard Density Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilVolumeStdDensity,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Mass",
                                    OutputProcessor::Unit::kg,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilMass,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilMdot,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Air Change Rate",
                                    OutputProcessor::Unit::ach,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilAirChangeRate,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Fan Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilFanElec,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name,
                                    _,
                                    "Electricity",
                                    "Fans",
                                    "Ventilation (simple)",
                                    "Building",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Ventilation Air Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).VentilAirTemp,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Ventilation(VentiCount).ZonePtr).Name);
            }
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSActuator(state,
                             "Zone Ventilation",
                             state.dataHeatBal->Ventilation(VentiCount).Name,
                             "Air Exchange Flow Rate",
                             "[m3/s]",
                             state.dataHeatBal->Ventilation(VentiCount).EMSSimpleVentOn,
                             state.dataHeatBal->Ventilation(VentiCount).EMSimpleVentFlowRate);
        }
    }

    RepVarSet = true;

    cCurrentModuleObject = "ZoneMixing";
    state.dataHeatBal->TotMixing = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataHeatBal->Mixing.allocate(state.dataHeatBal->TotMixing);

    for (Loop = 1; Loop <= state.dataHeatBal->TotMixing; ++Loop) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Loop,
                                                                 cAlphaArgs,
                                                                 NumAlpha,
                                                                 rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 lNumericFieldBlanks,
                                                                 lAlphaFieldBlanks,
                                                                 cAlphaFieldNames,
                                                                 cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        state.dataHeatBal->Mixing(Loop).Name = cAlphaArgs(1);

        state.dataHeatBal->Mixing(Loop).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
        if (state.dataHeatBal->Mixing(Loop).ZonePtr == 0) {
            ShowSevereError(state,
                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2) + "\".");
            ErrorsFound = true;
        }

        state.dataHeatBal->Mixing(Loop).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));

        if (state.dataHeatBal->Mixing(Loop).SchedPtr == 0) {
            if (lAlphaFieldBlanks(3)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(3) +
                                    " is required but field is blank.");
            } else {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(3) +
                                    "=\"" + cAlphaArgs(3) + "\".");
            }
            ErrorsFound = true;
        }

        // Mixing equipment design level calculation method
        {
            auto const SELECT_CASE_var(cAlphaArgs(4));
            if ((SELECT_CASE_var == "FLOW/ZONE") || (SELECT_CASE_var == "FLOW")) {
                state.dataHeatBal->Mixing(Loop).DesignLevel = rNumericArgs(1);
                if (lAlphaFieldBlanks(1)) {
                    ShowWarningError(state,
                                     RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) + " specifies " +
                                         cNumericFieldNames(1) + ", but that field is blank.  0 Mixing will result.");
                }

            } else if (SELECT_CASE_var == "FLOW/AREA") {
                if (state.dataHeatBal->Mixing(Loop).ZonePtr != 0) {
                    if (rNumericArgs(2) >= 0.0) {
                        state.dataHeatBal->Mixing(Loop).DesignLevel =
                            rNumericArgs(2) * state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).FloorArea;
                        if (state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).FloorArea <= 0.0) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) +
                                                 " specifies " + cNumericFieldNames(2) + ", but Zone Floor Area = 0.  0 Mixing will result.");
                        }
                    } else {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", invalid flow/person specification [<0.0]={:.3R}",
                                               RoutineName,
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               rNumericArgs(2)));
                        ErrorsFound = true;
                    }
                }
                if (lAlphaFieldBlanks(2)) {
                    ShowWarningError(state,
                                     RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) + " specifies " +
                                         cNumericFieldNames(2) + ", but that field is blank.  0 Mixing will result.");
                }

            } else if (SELECT_CASE_var == "FLOW/PERSON") {
                if (state.dataHeatBal->Mixing(Loop).ZonePtr != 0) {
                    if (rNumericArgs(3) >= 0.0) {
                        state.dataHeatBal->Mixing(Loop).DesignLevel =
                            rNumericArgs(3) * state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).TotOccupants;
                        if (state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).TotOccupants <= 0.0) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) +
                                                 " specifies " + cNumericFieldNames(3) + ", but Zone Total Occupants = 0.  0 Mixing will result.");
                        }
                    } else {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", invalid flow/person specification [<0.0]={:.3R}",
                                               RoutineName,
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               rNumericArgs(3)));
                        ErrorsFound = true;
                    }
                }
                if (lAlphaFieldBlanks(3)) {
                    ShowWarningError(state,
                                     RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) + " specifies " +
                                         cNumericFieldNames(3) + ", but that field is blank.  0 Mixing will result.");
                }

            } else if (SELECT_CASE_var == "AIRCHANGES/HOUR") {
                if (state.dataHeatBal->Mixing(Loop).ZonePtr != 0) {
                    if (rNumericArgs(4) >= 0.0) {
                        state.dataHeatBal->Mixing(Loop).DesignLevel = rNumericArgs(4) *
                                                                      state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Volume /
                                                                      DataGlobalConstants::SecInHour;
                        if (state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Volume <= 0.0) {
                            ShowWarningError(state,
                                             RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) +
                                                 " specifies " + cNumericFieldNames(4) + ", but Zone Volume = 0.  0 Mixing will result.");
                        }
                    } else {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", invalid flow/person specification [<0.0]={:.3R}",
                                               RoutineName,
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               rNumericArgs(4)));
                        ErrorsFound = true;
                    }
                }
                if (lAlphaFieldBlanks(4)) {
                    ShowWarningError(state,
                                     RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) + " specifies " +
                                         cNumericFieldNames(4) + ", but that field is blank.  0 Mixing will result.");
                }

            } else {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid calculation method=" + cAlphaArgs(4));
                ErrorsFound = true;
            }
        }

        state.dataHeatBal->Mixing(Loop).FromZone = UtilityRoutines::FindItemInList(cAlphaArgs(5), state.dataHeatBal->Zone);
        if (state.dataHeatBal->Mixing(Loop).FromZone == 0) {
            ShowSevereError(state,
                            RoutineName + cAlphaFieldNames(5) + " not found=" + cAlphaArgs(5) + " for " + cCurrentModuleObject + '=' + cAlphaArgs(1));
            ErrorsFound = true;
        }
        state.dataHeatBal->Mixing(Loop).DeltaTemperature = rNumericArgs(5);

        if (NumAlpha > 5) {
            state.dataHeatBal->Mixing(Loop).DeltaTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(6));
            if (state.dataHeatBal->Mixing(Loop).DeltaTempSchedPtr > 0) {
                if (!lNumericFieldBlanks(5))
                    ShowWarningError(state,
                                     RoutineName +
                                         "The Delta Temperature value and schedule are provided. The scheduled temperature will be used in the " +
                                         cCurrentModuleObject + " object = " + cAlphaArgs(1));
                if (GetScheduleMinValue(state, state.dataHeatBal->Mixing(Loop).DeltaTempSchedPtr) < -MixingTempLimit) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                        " must have a delta temperature equal to or above -100C defined in the schedule = " + cAlphaArgs(6));
                    ErrorsFound = true;
                }
            }
        }
        if (state.dataHeatBal->Mixing(Loop).DeltaTempSchedPtr == 0 && lNumericFieldBlanks(5) && (!lAlphaFieldBlanks(6))) {
            ShowWarningError(state,
                             format("{}{}: the value field is blank and schedule field is invalid. The default value will be used ({:.1R}) ",
                                    RoutineName,
                                    cNumericFieldNames(5),
                                    rNumericArgs(5)));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }
        if (!lNumericFieldBlanks(5) && ((!lAlphaFieldBlanks(6)) && state.dataHeatBal->Mixing(Loop).DeltaTempSchedPtr == 0)) {
            ShowWarningError(state,
                             format("{}{} = {} is invalid. The constant value will be used at {:.1R} degrees C ",
                                    RoutineName,
                                    cAlphaFieldNames(6),
                                    cAlphaArgs(6),
                                    rNumericArgs(5)));
            ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
        }

        if (NumAlpha > 6) {
            state.dataHeatBal->Mixing(Loop).MinIndoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(7));
            if (state.dataHeatBal->Mixing(Loop).MinIndoorTempSchedPtr == 0) {
                if ((!lAlphaFieldBlanks(7))) {
                    ShowSevereError(state,
                                    RoutineName + cAlphaFieldNames(7) + " not found=" + cAlphaArgs(7) + " for " + cCurrentModuleObject + '=' +
                                        cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }
            if (state.dataHeatBal->Mixing(Loop).MinIndoorTempSchedPtr > 0) {
                // Check min and max values in the schedule to ensure both values are within the range
                if (!CheckScheduleValueMinMax(
                        state, state.dataHeatBal->Mixing(Loop).MinIndoorTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + " statement = " + cAlphaArgs(1) +
                                        " must have a minimum zone temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(7));
                    ErrorsFound = true;
                }
            }
        }

        if (NumAlpha > 7) {
            state.dataHeatBal->Mixing(Loop).MaxIndoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(8));
            if (state.dataHeatBal->Mixing(Loop).MaxIndoorTempSchedPtr == 0) {
                if ((!lAlphaFieldBlanks(8))) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(8) + " not found=\"" +
                                        cAlphaArgs(8) + "\".");
                    ErrorsFound = true;
                }
            }
            if (state.dataHeatBal->Mixing(Loop).MaxIndoorTempSchedPtr > 0) {
                // Check min and max values in the schedule to ensure both values are within the range
                if (!CheckScheduleValueMinMax(
                        state, state.dataHeatBal->Mixing(Loop).MaxIndoorTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                        "\" must have a maximum zone temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(8));
                    ErrorsFound = true;
                }
            }
        }

        if (NumAlpha > 8) {
            state.dataHeatBal->Mixing(Loop).MinSourceTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(9));
            if (state.dataHeatBal->Mixing(Loop).MinSourceTempSchedPtr == 0) {
                if ((!lAlphaFieldBlanks(9))) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(9) + " not found=\"" +
                                        cAlphaArgs(9) + "\".");
                    ErrorsFound = true;
                }
            }
            if (state.dataHeatBal->Mixing(Loop).MinSourceTempSchedPtr > 0) {
                // Check min and max values in the schedule to ensure both values are within the range
                if (!CheckScheduleValueMinMax(
                        state, state.dataHeatBal->Mixing(Loop).MinSourceTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                    ShowSevereError(
                        state,
                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                            "\" must have a minimum source temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(9));
                    ErrorsFound = true;
                }
            }
        }

        if (NumAlpha > 9) {
            state.dataHeatBal->Mixing(Loop).MaxSourceTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(10));
            if (state.dataHeatBal->Mixing(Loop).MaxSourceTempSchedPtr == 0) {
                if ((!lAlphaFieldBlanks(10))) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(10) + " not found=\"" +
                                        cAlphaArgs(10) + "\".");
                    ErrorsFound = true;
                }
            }
            if (state.dataHeatBal->Mixing(Loop).MaxSourceTempSchedPtr > 0) {
                // Check min and max values in the schedule to ensure both values are within the range
                if (!CheckScheduleValueMinMax(
                        state, state.dataHeatBal->Mixing(Loop).MaxSourceTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                    ShowSevereError(
                        state,
                        RoutineName + cCurrentModuleObject + " statement =\"" + cAlphaArgs(1) +
                            "\" must have a maximum source temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(10));
                    ErrorsFound = true;
                }
            }
        }

        if (NumAlpha > 10) {
            state.dataHeatBal->Mixing(Loop).MinOutdoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(11));
            if (state.dataHeatBal->Mixing(Loop).MinOutdoorTempSchedPtr == 0) {
                if ((!lAlphaFieldBlanks(11))) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(11) + " not found=\"" +
                                        cAlphaArgs(11) + "\".");
                    ErrorsFound = true;
                }
            }
            if (state.dataHeatBal->Mixing(Loop).MinOutdoorTempSchedPtr > 0) {
                // Check min and max values in the schedule to ensure both values are within the range
                if (!CheckScheduleValueMinMax(
                        state, state.dataHeatBal->Mixing(Loop).MinOutdoorTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                    ShowSevereError(
                        state,
                        RoutineName + cCurrentModuleObject + " =\"" + cAlphaArgs(1) +
                            "\" must have a minimum outdoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(11));
                    ErrorsFound = true;
                }
            }
        }

        if (NumAlpha > 11) {
            state.dataHeatBal->Mixing(Loop).MaxOutdoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(12));
            if (state.dataHeatBal->Mixing(Loop).MaxOutdoorTempSchedPtr == 0) {
                if ((!lAlphaFieldBlanks(12))) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(12) + " not found=\"" +
                                        cAlphaArgs(12) + "\".");
                    ErrorsFound = true;
                }
            }
            if (state.dataHeatBal->Mixing(Loop).MaxOutdoorTempSchedPtr > 0) {
                // Check min and max values in the schedule to ensure both values are within the range
                if (!CheckScheduleValueMinMax(
                        state, state.dataHeatBal->Mixing(Loop).MaxOutdoorTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                    ShowSevereError(
                        state,
                        RoutineName + cCurrentModuleObject + " =\"" + cAlphaArgs(1) +
                            "\" must have a maximum outdoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(12));
                    ErrorsFound = true;
                }
            }
        }

        if (state.dataHeatBal->Mixing(Loop).ZonePtr > 0) {
            if (RepVarSet(state.dataHeatBal->Mixing(Loop).ZonePtr)) {
                RepVarSet(state.dataHeatBal->Mixing(Loop).ZonePtr) = false;
                SetupOutputVariable(state,
                                    "Zone Mixing Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixVolume,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Current Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixVdotCurDensity,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Standard Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixVdotStdDensity,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Mass",
                                    OutputProcessor::Unit::kg,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixMass,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixMdot,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Sensible Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixHeatLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Sensible Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixHeatGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Latent Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixLatentLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Latent Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixLatentGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Total Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixTotalLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Total Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->Mixing(Loop).ZonePtr).MixTotalGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).ZonePtr).Name);
            }
        }
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSActuator(state,
                             "ZoneMixing",
                             state.dataHeatBal->Mixing(Loop).Name,
                             "Air Exchange Flow Rate",
                             "[m3/s]",
                             state.dataHeatBal->Mixing(Loop).EMSSimpleMixingOn,
                             state.dataHeatBal->Mixing(Loop).EMSimpleMixingFlowRate);
        }
    }

    // allocate MassConservation
    state.dataHeatBal->MassConservation.allocate(state.dataGlobal->NumOfZones);

    // added by BAN, 02/14
    if (state.dataHeatBal->TotMixing > 0) {
        ZoneMixingNum.allocate(state.dataHeatBal->TotMixing);
        // get source zones mixing objects index
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            SourceCount = 0;
            for (Loop = 1; Loop <= state.dataHeatBal->TotMixing; ++Loop) {
                if (ZoneNum == state.dataHeatBal->Mixing(Loop).FromZone) {
                    SourceCount += 1;
                    ZoneMixingNum(SourceCount) = Loop;
                }
            }
            // save mixing objects index for zones which serve as a source zone
            state.dataHeatBal->MassConservation(ZoneNum).NumSourceZonesMixingObject = SourceCount;
            if (SourceCount > 0) {
                state.dataHeatBal->MassConservation(ZoneNum).ZoneMixingSourcesPtr.allocate(SourceCount);
                for (Loop = 1; Loop <= SourceCount; ++Loop) {
                    state.dataHeatBal->MassConservation(ZoneNum).ZoneMixingSourcesPtr(Loop) = ZoneMixingNum(Loop);
                }
            }
        }

        // check zones which are used only as a source zones
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            IsSourceZone = false;
            for (Loop = 1; Loop <= state.dataHeatBal->TotMixing; ++Loop) {
                if (ZoneNum != state.dataHeatBal->Mixing(Loop).FromZone) continue;
                state.dataHeatBal->MassConservation(ZoneNum).IsOnlySourceZone = true;
                for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotMixing; ++Loop1) {
                    if (ZoneNum == state.dataHeatBal->Mixing(Loop1).ZonePtr) {
                        state.dataHeatBal->MassConservation(ZoneNum).IsOnlySourceZone = false;
                        break;
                    }
                }
            }
        }
        // get receiving zones mixing objects index
        ZoneMixingNum = 0;
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            ReceivingCount = 0;
            for (Loop = 1; Loop <= state.dataHeatBal->TotMixing; ++Loop) {
                if (ZoneNum == state.dataHeatBal->Mixing(Loop).ZonePtr) {
                    ReceivingCount += 1;
                    ZoneMixingNum(ReceivingCount) = Loop;
                }
            }
            // save mixing objects index for zones which serve as a receiving zone
            state.dataHeatBal->MassConservation(ZoneNum).NumReceivingZonesMixingObject = ReceivingCount;
            if (ReceivingCount > 0) {
                state.dataHeatBal->MassConservation(ZoneNum).ZoneMixingReceivingPtr.allocate(ReceivingCount);
                state.dataHeatBal->MassConservation(ZoneNum).ZoneMixingReceivingFr.allocate(ReceivingCount);
                for (Loop = 1; Loop <= ReceivingCount; ++Loop) {
                    state.dataHeatBal->MassConservation(ZoneNum).ZoneMixingReceivingPtr(Loop) = ZoneMixingNum(Loop);
                }
            }
            // flag zones used as both source and receiving zone
            if (state.dataHeatBal->MassConservation(ZoneNum).NumSourceZonesMixingObject > 0 &&
                state.dataHeatBal->MassConservation(ZoneNum).NumReceivingZonesMixingObject > 0) {
                state.dataHeatBal->MassConservation(ZoneNum).IsSourceAndReceivingZone = true;
            }
        }
        if (allocated(ZoneMixingNum)) ZoneMixingNum.deallocate();
    }

    // zone mass conservation calculation order starts with receiving zones
    // and then proceeds to source zones
    Loop = 0;
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (!state.dataHeatBal->MassConservation(ZoneNum).IsOnlySourceZone &&
            !state.dataHeatBal->MassConservation(ZoneNum).IsSourceAndReceivingZone) {
            Loop += 1;
            state.dataHeatBalFanSys->ZoneReOrder(Loop) = ZoneNum;
        }
    }
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (state.dataHeatBal->MassConservation(ZoneNum).IsSourceAndReceivingZone) {
            Loop += 1;
            state.dataHeatBalFanSys->ZoneReOrder(Loop) = ZoneNum;
        }
    }
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (state.dataHeatBal->MassConservation(ZoneNum).IsOnlySourceZone) {
            Loop += 1;
            state.dataHeatBalFanSys->ZoneReOrder(Loop) = ZoneNum;
        }
    }
    cCurrentModuleObject = "ZoneCrossMixing";
    int inputCrossMixing = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataHeatBal->TotCrossMixing = inputCrossMixing + state.dataHeatBal->NumAirBoundaryMixing;
    state.dataHeatBal->CrossMixing.allocate(state.dataHeatBal->TotCrossMixing);

    for (Loop = 1; Loop <= state.dataHeatBal->TotCrossMixing; ++Loop) {

        if (Loop > inputCrossMixing) {
            // Create CrossMixing object from air boundary info
            int airBoundaryIndex = Loop - inputCrossMixing - 1; // zero-based
            int zone1 = state.dataHeatBal->AirBoundaryMixingZone1[airBoundaryIndex];
            int zone2 = state.dataHeatBal->AirBoundaryMixingZone2[airBoundaryIndex];
            state.dataHeatBal->CrossMixing(Loop).Name = fmt::format("Air Boundary Mixing Zones {} and {}", zone1, zone2);
            state.dataHeatBal->CrossMixing(Loop).ZonePtr = zone1;
            state.dataHeatBal->CrossMixing(Loop).SchedPtr = state.dataHeatBal->AirBoundaryMixingSched[airBoundaryIndex];
            state.dataHeatBal->CrossMixing(Loop).DesignLevel = state.dataHeatBal->AirBoundaryMixingVol[airBoundaryIndex];
            state.dataHeatBal->CrossMixing(Loop).FromZone = zone2;
        } else {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Loop,
                                                                     cAlphaArgs,
                                                                     NumAlpha,
                                                                     rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            state.dataHeatBal->CrossMixing(Loop).Name = cAlphaArgs(1);

            state.dataHeatBal->CrossMixing(Loop).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataHeatBal->Zone);
            if (state.dataHeatBal->CrossMixing(Loop).ZonePtr == 0) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(2) +
                                    "=\"" + cAlphaArgs(2) + "\".");
                ErrorsFound = true;
            }

            state.dataHeatBal->CrossMixing(Loop).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
            if (state.dataHeatBal->CrossMixing(Loop).SchedPtr == 0) {
                if (lAlphaFieldBlanks(3)) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(3) +
                                        " is required but field is blank.");
                } else {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(3) +
                                        "=\"" + cAlphaArgs(3) + "\".");
                }
                ErrorsFound = true;
            }

            // Mixing equipment design level calculation method.
            {
                auto const SELECT_CASE_var(cAlphaArgs(4));
                if ((SELECT_CASE_var == "FLOW/ZONE") || (SELECT_CASE_var == "FLOW")) {
                    state.dataHeatBal->CrossMixing(Loop).DesignLevel = rNumericArgs(1);
                    if (lAlphaFieldBlanks(1)) {
                        ShowWarningError(state,
                                         RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) + " specifies " +
                                             cNumericFieldNames(1) + ", but that field is blank.  0 Cross Mixing will result.");
                    }

                } else if (SELECT_CASE_var == "FLOW/AREA") {
                    if (state.dataHeatBal->CrossMixing(Loop).ZonePtr != 0) {
                        if (rNumericArgs(2) >= 0.0) {
                            state.dataHeatBal->CrossMixing(Loop).DesignLevel =
                                rNumericArgs(2) * state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).FloorArea;
                            if (state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).FloorArea <= 0.0) {
                                ShowWarningError(state,
                                                 RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) +
                                                     " specifies " + cNumericFieldNames(2) +
                                                     ", but Zone Floor Area = 0.  0 Cross Mixing will result.");
                            }
                        } else {
                            ShowSevereError(state,
                                            format("{}{}=\"{}\", invalid flow/person specification [<0.0]={:.3R}",
                                                   RoutineName,
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   rNumericArgs(2)));
                            ErrorsFound = true;
                        }
                    }
                    if (lAlphaFieldBlanks(2)) {
                        ShowWarningError(state,
                                         RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) + " specifies " +
                                             cNumericFieldNames(2) + ", but that field is blank.  0 Cross Mixing will result.");
                    }

                } else if (SELECT_CASE_var == "FLOW/PERSON") {
                    if (state.dataHeatBal->CrossMixing(Loop).ZonePtr != 0) {
                        if (rNumericArgs(3) >= 0.0) {
                            state.dataHeatBal->CrossMixing(Loop).DesignLevel =
                                rNumericArgs(3) * state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).TotOccupants;
                            if (state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).TotOccupants <= 0.0) {
                                ShowWarningError(state,
                                                 RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) +
                                                     " specifies " + cNumericFieldNames(3) +
                                                     ", but Zone Total Occupants = 0.  0 Cross Mixing will result.");
                            }
                        } else {
                            ShowSevereError(state,
                                            format("{}{}=\"{}\", invalid flow/person specification [<0.0]={:.3R}",
                                                   RoutineName,
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   rNumericArgs(3)));
                            ErrorsFound = true;
                        }
                    }
                    if (lAlphaFieldBlanks(3)) {
                        ShowWarningError(state,
                                         RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) + " specifies " +
                                             cNumericFieldNames(3) + ", but that field is blank.  0 Cross Mixing will result.");
                    }

                } else if (SELECT_CASE_var == "AIRCHANGES/HOUR") {
                    if (state.dataHeatBal->CrossMixing(Loop).ZonePtr != 0) {
                        if (rNumericArgs(4) >= 0.0) {
                            state.dataHeatBal->CrossMixing(Loop).DesignLevel =
                                rNumericArgs(4) * state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Volume /
                                DataGlobalConstants::SecInHour;
                            if (state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Volume <= 0.0) {
                                ShowWarningError(state,
                                                 RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) +
                                                     " specifies " + cNumericFieldNames(4) + ", but Zone Volume = 0.  0 Cross Mixing will result.");
                            }
                        } else {
                            ShowSevereError(state,
                                            format("{}{}=\"{}\", invalid flow/person specification [<0.0]={:.3R}",
                                                   RoutineName,
                                                   cCurrentModuleObject,
                                                   cAlphaArgs(1),
                                                   rNumericArgs(4)));
                            ErrorsFound = true;
                        }
                    }
                    if (lAlphaFieldBlanks(4)) {
                        ShowWarningError(state,
                                         RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(4) + " specifies " +
                                             cNumericFieldNames(4) + ", but that field is blank.  0 Cross Mixing will result.");
                    }

                } else {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid calculation method=" + cAlphaArgs(4));
                    ErrorsFound = true;
                }
            }

            state.dataHeatBal->CrossMixing(Loop).FromZone = UtilityRoutines::FindItemInList(cAlphaArgs(5), state.dataHeatBal->Zone);
            if (state.dataHeatBal->CrossMixing(Loop).FromZone == 0) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(5) +
                                    "=\"" + cAlphaArgs(5) + "\".");
                ErrorsFound = true;
            }
            state.dataHeatBal->CrossMixing(Loop).DeltaTemperature = rNumericArgs(5);

            if (NumAlpha > 5) {
                state.dataHeatBal->CrossMixing(Loop).DeltaTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(6));
                if (state.dataHeatBal->CrossMixing(Loop).DeltaTempSchedPtr > 0) {
                    if (!lNumericFieldBlanks(5))
                        ShowWarningError(state,
                                         RoutineName +
                                             "The Delta Temperature value and schedule are provided. The scheduled temperature will be used in the " +
                                             cCurrentModuleObject + " object = " + cAlphaArgs(1));
                    if (GetScheduleMinValue(state, state.dataHeatBal->CrossMixing(Loop).DeltaTempSchedPtr) < 0.0) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            " must have a delta temperature equal to or above 0 C defined in the schedule = " + cAlphaArgs(6));
                        ErrorsFound = true;
                    }
                }
            }
            if (state.dataHeatBal->CrossMixing(Loop).DeltaTempSchedPtr == 0 && lNumericFieldBlanks(5) && (!lAlphaFieldBlanks(6))) {
                ShowWarningError(state,
                                 format("{}{}: the value field is blank and schedule field is invalid. The default value will be used ({:.1R}) ",
                                        RoutineName,
                                        cNumericFieldNames(5),
                                        rNumericArgs(5)));
                ShowContinueError(state, "in " + cCurrentModuleObject + " = " + cAlphaArgs(1) + " and the simulation continues...");
            }
            if (!lNumericFieldBlanks(5) && ((!lAlphaFieldBlanks(6)) && state.dataHeatBal->CrossMixing(Loop).DeltaTempSchedPtr == 0)) {
                ShowWarningError(state,
                                 format("{}{} = {} is invalid. The constant value will be used at {:.1R} degrees C ",
                                        RoutineName,
                                        cAlphaFieldNames(6),
                                        cAlphaArgs(6),
                                        rNumericArgs(5)));
                ShowContinueError(state, "in the " + cCurrentModuleObject + " object = " + cAlphaArgs(1) + " and the simulation continues...");
            }

            if (NumAlpha > 6) {
                state.dataHeatBal->CrossMixing(Loop).MinIndoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(7));
                if (state.dataHeatBal->CrossMixing(Loop).MinIndoorTempSchedPtr == 0) {
                    if ((!lAlphaFieldBlanks(7))) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(7) +
                                            " not found=" + cAlphaArgs(7) + "\".");
                        ErrorsFound = true;
                    }
                }
                if (state.dataHeatBal->CrossMixing(Loop).MinIndoorTempSchedPtr > 0) {
                    // Check min and max values in the schedule to ensure both values are within the range
                    if (!CheckScheduleValueMinMax(
                            state, state.dataHeatBal->CrossMixing(Loop).MinIndoorTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                        ShowSevereError(
                            state,
                            RoutineName + cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                " must have a minimum zone temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(7));
                        ErrorsFound = true;
                    }
                }
            }

            if (NumAlpha > 7) {
                state.dataHeatBal->CrossMixing(Loop).MaxIndoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(8));
                if (state.dataHeatBal->CrossMixing(Loop).MaxIndoorTempSchedPtr == 0) {
                    if ((!lAlphaFieldBlanks(8))) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(8) + " not found=\"" +
                                            cAlphaArgs(8) + "\".");
                        ErrorsFound = true;
                    }
                }
                if (state.dataHeatBal->CrossMixing(Loop).MaxIndoorTempSchedPtr > 0) {
                    // Check min and max values in the schedule to ensure both values are within the range
                    if (!CheckScheduleValueMinMax(
                            state, state.dataHeatBal->CrossMixing(Loop).MaxIndoorTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                        ShowSevereError(
                            state,
                            RoutineName + cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                " must have a maximum zone temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(8));
                        ErrorsFound = true;
                    }
                }
            }

            if (NumAlpha > 8) {
                state.dataHeatBal->CrossMixing(Loop).MinSourceTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(9));
                if (state.dataHeatBal->CrossMixing(Loop).MinSourceTempSchedPtr == 0) {
                    if ((!lAlphaFieldBlanks(9))) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(9) + " not found=\"" +
                                            cAlphaArgs(9) + "\".");
                        ErrorsFound = true;
                    }
                }
                if (state.dataHeatBal->CrossMixing(Loop).MinSourceTempSchedPtr > 0) {
                    // Check min and max values in the schedule to ensure both values are within the range
                    if (!CheckScheduleValueMinMax(
                            state, state.dataHeatBal->CrossMixing(Loop).MinSourceTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                        ShowSevereError(
                            state,
                            RoutineName + cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                " must have a minimum source temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(9));
                        ErrorsFound = true;
                    }
                }
            }

            if (NumAlpha > 9) {
                state.dataHeatBal->CrossMixing(Loop).MaxSourceTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(10));
                if (state.dataHeatBal->CrossMixing(Loop).MaxSourceTempSchedPtr == 0) {
                    if ((!lAlphaFieldBlanks(10))) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(10) + " not found=\"" +
                                            cAlphaArgs(9) + "\".");
                        ErrorsFound = true;
                    }
                }
                if (state.dataHeatBal->CrossMixing(Loop).MaxSourceTempSchedPtr > 0) {
                    // Check min and max values in the schedule to ensure both values are within the range
                    if (!CheckScheduleValueMinMax(
                            state, state.dataHeatBal->CrossMixing(Loop).MaxSourceTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                        ShowSevereError(
                            state,
                            RoutineName + cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                " must have a maximum source temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(10));
                        ErrorsFound = true;
                    }
                }
            }

            if (NumAlpha > 10) {
                state.dataHeatBal->CrossMixing(Loop).MinOutdoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(11));
                if (state.dataHeatBal->CrossMixing(Loop).MinOutdoorTempSchedPtr == 0) {
                    if ((!lAlphaFieldBlanks(11))) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(11) + " not found=\"" +
                                            cAlphaArgs(9) + "\".");
                        ErrorsFound = true;
                    }
                }
                if (state.dataHeatBal->CrossMixing(Loop).MinOutdoorTempSchedPtr > 0) {
                    // Check min and max values in the schedule to ensure both values are within the range
                    if (!CheckScheduleValueMinMax(
                            state, state.dataHeatBal->CrossMixing(Loop).MinOutdoorTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                        ShowSevereError(
                            state,
                            RoutineName + cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                " must have a minimum outdoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(11));
                        ErrorsFound = true;
                    }
                }
            }

            if (NumAlpha > 11) {
                state.dataHeatBal->CrossMixing(Loop).MaxOutdoorTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(12));
                if (state.dataHeatBal->CrossMixing(Loop).MaxOutdoorTempSchedPtr == 0) {
                    if ((!lAlphaFieldBlanks(12))) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(12) + " not found=\"" +
                                            cAlphaArgs(9) + "\".");
                        ErrorsFound = true;
                    }
                }
                if (state.dataHeatBal->CrossMixing(Loop).MaxOutdoorTempSchedPtr > 0) {
                    // Check min and max values in the schedule to ensure both values are within the range
                    if (!CheckScheduleValueMinMax(
                            state, state.dataHeatBal->CrossMixing(Loop).MaxOutdoorTempSchedPtr, ">=", -MixingTempLimit, "<=", MixingTempLimit)) {
                        ShowSevereError(
                            state,
                            RoutineName + cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                " must have a maximum outdoor temperature between -100C and 100C defined in the schedule = " + cAlphaArgs(12));
                        ErrorsFound = true;
                    }
                }
            }
        }

        if (state.dataHeatBal->CrossMixing(Loop).ZonePtr > 0) {
            if (RepVarSet(state.dataHeatBal->CrossMixing(Loop).ZonePtr)) {
                RepVarSet(state.dataHeatBal->CrossMixing(Loop).ZonePtr) = false;
                SetupOutputVariable(state,
                                    "Zone Mixing Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixVolume,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Current Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixVdotCurDensity,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Standard Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixVdotStdDensity,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Mass",
                                    OutputProcessor::Unit::kg,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixMass,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixMdot,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Sensible Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixHeatLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Sensible Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixHeatGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Latent Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixLatentLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Latent Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixLatentGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Total Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixTotalLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Total Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).ZonePtr).MixTotalGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).ZonePtr).Name);
            }
        }
        if (state.dataHeatBal->CrossMixing(Loop).FromZone > 0) {
            if (RepVarSet(state.dataHeatBal->CrossMixing(Loop).FromZone)) {
                RepVarSet(state.dataHeatBal->CrossMixing(Loop).FromZone) = false;
                SetupOutputVariable(state,
                                    "Zone Mixing Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixVolume,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Current Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixVdotCurDensity,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Standard Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixVdotStdDensity,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Mass",
                                    OutputProcessor::Unit::kg,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixMass,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixMdot,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Sensible Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixHeatLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Sensible Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixHeatGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Latent Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixLatentLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Latent Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixLatentGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Total Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixTotalLoss,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
                SetupOutputVariable(state,
                                    "Zone Mixing Total Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnAirRpt(state.dataHeatBal->CrossMixing(Loop).FromZone).MixTotalGain,
                                    "System",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
            }
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSActuator(state,
                             "ZoneCrossMixing",
                             state.dataHeatBal->CrossMixing(Loop).Name,
                             "Air Exchange Flow Rate",
                             "[m3/s]",
                             state.dataHeatBal->CrossMixing(Loop).EMSSimpleMixingOn,
                             state.dataHeatBal->CrossMixing(Loop).EMSimpleMixingFlowRate);
        }
    }

    cCurrentModuleObject = "ZoneRefrigerationDoorMixing";
    state.dataHeatBal->TotRefDoorMixing = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    if (state.dataHeatBal->TotRefDoorMixing > 0) {
        state.dataHeatBal->RefDoorMixing.allocate(state.dataGlobal->NumOfZones);
        for (auto &e : state.dataHeatBal->RefDoorMixing)
            e.NumRefDoorConnections = 0;

        for (Loop = 1; Loop <= state.dataHeatBal->TotRefDoorMixing; ++Loop) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Loop,
                                                                     cAlphaArgs,
                                                                     NumAlpha,
                                                                     rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            NameThisObject = cAlphaArgs(1);

            AlphaNum = 2;
            Zone1Num = UtilityRoutines::FindItemInList(cAlphaArgs(AlphaNum), state.dataHeatBal->Zone);
            if (Zone1Num == 0) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(AlphaNum) +
                                    "=\"" + cAlphaArgs(AlphaNum) + "\".");
                ErrorsFound = true;
            }

            ++AlphaNum; // 3
            Zone2Num = UtilityRoutines::FindItemInList(cAlphaArgs(AlphaNum), state.dataHeatBal->Zone);
            if (Zone2Num == 0) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " + cAlphaFieldNames(AlphaNum) +
                                    "=\"" + cAlphaArgs(AlphaNum) + "\".");
                ErrorsFound = true;
            }
            if (Zone1Num == Zone2Num) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                    "\", The same zone name has been entered for both sides of a refrigerated door " + cAlphaFieldNames(AlphaNum) +
                                    "=\"" + cAlphaArgs(AlphaNum) + "\".");
                ErrorsFound = true;
            } else if (Zone1Num < Zone2Num) { // zone 1 will come first in soln loop, id zone 2 as mate zone
                ZoneNumA = Zone1Num;
                ZoneNumB = Zone2Num;
            } else if (Zone2Num < Zone1Num) { // zone 2 will come first in soln loop, id zone 1 as mate zone
                ZoneNumA = Zone2Num;
                ZoneNumB = Zone1Num;
            }

            if (!allocated(state.dataHeatBal->RefDoorMixing(ZoneNumA).OpenSchedPtr)) {
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorMixingObjectName.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumA).OpenSchedPtr.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorHeight.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorArea.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumA).Protection.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumA).MateZonePtr.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumA).EMSRefDoorMixingOn.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumA).EMSRefDoorFlowRate.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumA).VolRefDoorFlowRate.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorProtTypeName.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorMixingObjectName = "";
                state.dataHeatBal->RefDoorMixing(ZoneNumA).OpenSchedPtr = 0;
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorHeight = 0.0;
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorArea = 0.0;
                state.dataHeatBal->RefDoorMixing(ZoneNumA).Protection = RefDoorNone;
                state.dataHeatBal->RefDoorMixing(ZoneNumA).MateZonePtr = 0;
                state.dataHeatBal->RefDoorMixing(ZoneNumA).EMSRefDoorMixingOn = false;
                state.dataHeatBal->RefDoorMixing(ZoneNumA).EMSRefDoorFlowRate = 0.0;
                state.dataHeatBal->RefDoorMixing(ZoneNumA).VolRefDoorFlowRate = 0.0;
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorProtTypeName = "";
            } // First refrigeration mixing in this zone

            if (!allocated(state.dataHeatBal->RefDoorMixing(ZoneNumB).OpenSchedPtr)) {
                state.dataHeatBal->RefDoorMixing(ZoneNumB).DoorMixingObjectName.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumB).OpenSchedPtr.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumB).DoorHeight.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumB).DoorArea.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumB).Protection.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumB).MateZonePtr.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumB).EMSRefDoorMixingOn.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumB).EMSRefDoorFlowRate.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumB).VolRefDoorFlowRate.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumB).DoorProtTypeName.allocate(state.dataGlobal->NumOfZones);
                state.dataHeatBal->RefDoorMixing(ZoneNumB).DoorMixingObjectName = "";
                state.dataHeatBal->RefDoorMixing(ZoneNumB).OpenSchedPtr = 0;
                state.dataHeatBal->RefDoorMixing(ZoneNumB).DoorHeight = 0.0;
                state.dataHeatBal->RefDoorMixing(ZoneNumB).DoorArea = 0.0;
                state.dataHeatBal->RefDoorMixing(ZoneNumB).Protection = RefDoorNone;
                state.dataHeatBal->RefDoorMixing(ZoneNumB).MateZonePtr = 0;
                state.dataHeatBal->RefDoorMixing(ZoneNumB).EMSRefDoorMixingOn = false;
                state.dataHeatBal->RefDoorMixing(ZoneNumB).EMSRefDoorFlowRate = 0.0;
                state.dataHeatBal->RefDoorMixing(ZoneNumB).VolRefDoorFlowRate = 0.0;
                state.dataHeatBal->RefDoorMixing(ZoneNumB).DoorProtTypeName = "";
            } // First refrigeration mixing in this zone

            ConnectionNumber = state.dataHeatBal->RefDoorMixing(ZoneNumA).NumRefDoorConnections + 1;
            state.dataHeatBal->RefDoorMixing(ZoneNumA).NumRefDoorConnections = ConnectionNumber;
            state.dataHeatBal->RefDoorMixing(ZoneNumA).ZonePtr = ZoneNumA;
            state.dataHeatBal->RefDoorMixing(ZoneNumA).MateZonePtr(ConnectionNumber) = ZoneNumB;
            state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorMixingObjectName(ConnectionNumber) = NameThisObject;
            // need to make sure same pair of zones is only entered once.
            if (state.dataHeatBal->RefDoorMixing(ZoneNumA).RefDoorMixFlag && state.dataHeatBal->RefDoorMixing(ZoneNumB).RefDoorMixFlag) {
                if (state.dataHeatBal->RefDoorMixing(ZoneNumA).NumRefDoorConnections > 1) {
                    for (ConnectTest = 1; ConnectTest <= (ConnectionNumber - 1); ++ConnectTest) {
                        if (state.dataHeatBal->RefDoorMixing(ZoneNumA).MateZonePtr(ConnectTest) !=
                            state.dataHeatBal->RefDoorMixing(ZoneNumA).MateZonePtr(ConnectionNumber))
                            continue;
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", and " +
                                            state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorMixingObjectName(ConnectTest));
                        ShowContinueError(state,
                                          " Share same pair of zones: \"" + state.dataHeatBal->Zone(ZoneNumA).Name + "\" and \"" +
                                              state.dataHeatBal->Zone(ZoneNumB).Name +
                                              "\". Only one RefrigerationDoorMixing object is allowed for any unique pair of zones.");
                        ErrorsFound = true;
                    } // ConnectTest
                }     // NumRefDoorconnections > 1
            } else {  // Both zones need to be flagged with ref doors
                state.dataHeatBal->RefDoorMixing(ZoneNumA).RefDoorMixFlag = true;
                state.dataHeatBal->RefDoorMixing(ZoneNumB).RefDoorMixFlag = true;
            } // Both zones already flagged with ref doors

            ++AlphaNum; // 4
            if (lAlphaFieldBlanks(AlphaNum)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(AlphaNum) +
                                    " is required but field is blank.");
                ErrorsFound = true;
            } else { //(lAlphaFieldBlanks(AlphaNum)) THEN
                state.dataHeatBal->RefDoorMixing(ZoneNumA).OpenSchedPtr(ConnectionNumber) = GetScheduleIndex(state, cAlphaArgs(AlphaNum));
                if (state.dataHeatBal->RefDoorMixing(ZoneNumA).OpenSchedPtr(ConnectionNumber) == 0) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (not found) " +
                                        cAlphaFieldNames(AlphaNum) + "=\"" + cAlphaArgs(AlphaNum) + "\".");
                    ErrorsFound = true;
                } else { // OpenSchedPtr(ConnectionNumber) ne 0)
                    if (!CheckScheduleValueMinMax(
                            state, state.dataHeatBal->RefDoorMixing(ZoneNumA).OpenSchedPtr(ConnectionNumber), ">=", 0.0, "<=", 1.0)) {
                        ShowSevereError(state,
                                        RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"," + cAlphaFieldNames(AlphaNum) + "=\"" +
                                            cAlphaArgs(AlphaNum) + "\" has schedule values < 0 or > 1.");
                        ErrorsFound = true;
                    } // check door opening schedule values between 0 and 1
                }     // OpenSchedPtr(ConnectionNumber) == 0)
            }         //(lAlphaFieldBlanks(AlphaNum)) THEN

            NumbNum = 1;
            if (lAlphaFieldBlanks(NumbNum)) {
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorHeight(ConnectionNumber) = 3.0; // default height of 3 meters
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + cNumericFieldNames(NumbNum) +
                                     " is blank and the default value of 3.0 will be used.");
            } else {
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorHeight(ConnectionNumber) = rNumericArgs(NumbNum);
                if ((state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorHeight(ConnectionNumber) < 0) ||
                    (state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorHeight(ConnectionNumber) > 50.0)) {
                    ShowSevereError(
                        state, RoutineName + cCurrentModuleObject + " = " + cAlphaArgs(1) + " must have a door height between 0 and 50 meters. ");
                    ErrorsFound = true;
                }
            }

            ++NumbNum; // 2
            if (lAlphaFieldBlanks(NumbNum)) {
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorArea(ConnectionNumber) = 9.0; // default area of 9 m2
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + cNumericFieldNames(NumbNum) +
                                     " is blank and the default value of 9 m2 will be used.");
            } else {
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorArea(ConnectionNumber) = rNumericArgs(NumbNum);
                if ((state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorArea(ConnectionNumber) < 0) ||
                    (state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorArea(ConnectionNumber) > 400.0)) {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                        " must have a door height between 0 and 400 square meters. ");
                    ErrorsFound = true;
                }
            }

            ++AlphaNum; // 5
            // Door protection type.
            if (lAlphaFieldBlanks(AlphaNum)) {
                state.dataHeatBal->RefDoorMixing(ZoneNumA).Protection(ConnectionNumber) = RefDoorNone;  // Default
                state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorProtTypeName(ConnectionNumber) = "None"; // Default
                ShowWarningError(state,
                                 RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"  " + cAlphaFieldNames(AlphaNum) +
                                     " is blank. Default of no door protection will be used");
            } else {
                if (cAlphaArgs(AlphaNum) == "NONE") {
                    state.dataHeatBal->RefDoorMixing(ZoneNumA).Protection(ConnectionNumber) = RefDoorNone;
                    state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorProtTypeName(ConnectionNumber) = "None";
                } else if (cAlphaArgs(AlphaNum) == "AIRCURTAIN") {
                    state.dataHeatBal->RefDoorMixing(ZoneNumA).Protection(ConnectionNumber) = RefDoorAirCurtain;
                    state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorProtTypeName(ConnectionNumber) = "AirCurtain";
                } else if (cAlphaArgs(AlphaNum) == "STRIPCURTAIN") {
                    state.dataHeatBal->RefDoorMixing(ZoneNumA).Protection(ConnectionNumber) = RefDoorStripCurtain;
                    state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorProtTypeName(ConnectionNumber) = "StripCurtain";
                } else {
                    ShowSevereError(state,
                                    RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                        "\", invalid calculation method=" + cAlphaArgs(AlphaNum) + " with alphanum of 5: " + cAlphaArgs(5));
                    ErrorsFound = true;
                } // =none, etc.
            }     // Blank

            if (ZoneNumA > 0) {
                if (RepVarSet(ZoneNumA)) {
                    RepVarSet(ZoneNumA) = false;
                    SetupOutputVariable(state,
                                        "Zone Mixing Volume",
                                        OutputProcessor::Unit::m3,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixVolume,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Current Density Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixVdotCurDensity,
                                        "System",
                                        "Average",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Standard Density Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixVdotStdDensity,
                                        "System",
                                        "Average",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Mass",
                                        OutputProcessor::Unit::kg,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixMass,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixMdot,
                                        "System",
                                        "Average",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Sensible Heat Loss Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixHeatLoss,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Sensible Heat Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixHeatGain,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Latent Heat Loss Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixLatentLoss,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Latent Heat Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixLatentGain,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Total Heat Loss Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixTotalLoss,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Total Heat Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumA).MixTotalGain,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumA).Name);
                }
            }
            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state,
                                 "ZoneRefDoorMixing",
                                 state.dataHeatBal->RefDoorMixing(ZoneNumA).Name,
                                 "Air Exchange Flow Rate",
                                 "[m3/s]",
                                 state.dataHeatBal->RefDoorMixing(ZoneNumA).EMSRefDoorMixingOn(ConnectionNumber),
                                 state.dataHeatBal->RefDoorMixing(ZoneNumA).EMSRefDoorFlowRate(ConnectionNumber));
            }

            if (ZoneNumB > 0) {
                if (RepVarSet(ZoneNumB)) {
                    RepVarSet(ZoneNumB) = false;
                    SetupOutputVariable(state,
                                        "Zone Mixing Volume",
                                        OutputProcessor::Unit::m3,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixVolume,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Current Density Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixVdotCurDensity,
                                        "System",
                                        "Average",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Standard Density Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixVdotStdDensity,
                                        "System",
                                        "Average",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Mass",
                                        OutputProcessor::Unit::kg,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixMass,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixMdot,
                                        "System",
                                        "Average",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Sensible Heat Loss Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixHeatLoss,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Sensible Heat Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixHeatGain,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Latent Heat Loss Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixLatentLoss,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Latent Heat Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixLatentGain,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Total Heat Loss Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixTotalLoss,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                    SetupOutputVariable(state,
                                        "Zone Mixing Total Heat Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnAirRpt(ZoneNumB).MixTotalGain,
                                        "System",
                                        "Sum",
                                        state.dataHeatBal->Zone(ZoneNumB).Name);
                }
            }
            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state,
                                 "ZoneRefDoorMixing",
                                 state.dataHeatBal->RefDoorMixing(ZoneNumB).Name,
                                 "Air Exchange Flow Rate",
                                 "[m3/s]",
                                 state.dataHeatBal->RefDoorMixing(ZoneNumA).EMSRefDoorMixingOn(ConnectionNumber),
                                 state.dataHeatBal->RefDoorMixing(ZoneNumA).EMSRefDoorFlowRate(ConnectionNumber));
            }

        } // DO Loop=1,TotRefDoorMixing
    }     // TotRefDoorMixing > 0)

    RepVarSet.deallocate();
    cAlphaArgs.deallocate();
    cAlphaFieldNames.deallocate();
    cNumericFieldNames.deallocate();
    rNumericArgs.deallocate();
    lAlphaFieldBlanks.deallocate();
    lNumericFieldBlanks.deallocate();

    TotInfilVentFlow.dimension(state.dataGlobal->NumOfZones, 0.0);

    auto divide_and_print_if_greater_than_zero = [&](const Real64 denominator, const Real64 numerator) {
        if (denominator > 0.0) {
            print(state.files.eio, "{:.3R},", numerator / denominator);
        } else {
            print(state.files.eio, "N/A,");
        }
    };

    for (Loop = 1; Loop <= state.dataHeatBal->TotInfiltration; ++Loop) {
        if (Loop == 1)
            print(state.files.eio,
                  Format_721,
                  "ZoneInfiltration",
                  "Design Volume Flow Rate {m3/s},Volume Flow Rate/Floor Area {m3/s-m2},Volume Flow Rate/Exterior Surface Area {m3/s-m2},ACH - "
                  "Air Changes per Hour,Equation A - Constant Term Coefficient {},Equation B - Temperature Term Coefficient {1/C},Equation C - "
                  "Velocity Term Coefficient {s/m}, Equation D - Velocity Squared Term Coefficient {s2/m2}");

        ZoneNum = state.dataHeatBal->Infiltration(Loop).ZonePtr;
        if (ZoneNum == 0) {
            print(state.files.eio, Format_722, "Infiltration-Illegal Zone specified", state.dataHeatBal->Infiltration(Loop).Name);
            continue;
        }
        TotInfilVentFlow(ZoneNum) += state.dataHeatBal->Infiltration(Loop).DesignLevel;
        print(state.files.eio,
              Format_720,
              "ZoneInfiltration",
              state.dataHeatBal->Infiltration(Loop).Name,
              GetScheduleName(state, state.dataHeatBal->Infiltration(Loop).SchedPtr),
              state.dataHeatBal->Zone(ZoneNum).Name,
              state.dataHeatBal->Zone(ZoneNum).FloorArea,
              state.dataHeatBal->Zone(ZoneNum).TotOccupants);
        print(state.files.eio, "{:.3R},", state.dataHeatBal->Infiltration(Loop).DesignLevel);

        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).FloorArea, state.dataHeatBal->Infiltration(Loop).DesignLevel);
        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).ExteriorTotalSurfArea,
                                              state.dataHeatBal->Infiltration(Loop).DesignLevel);
        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).Volume,
                                              state.dataHeatBal->Infiltration(Loop).DesignLevel * DataGlobalConstants::SecInHour);

        print(state.files.eio, "{:.3R},", state.dataHeatBal->Infiltration(Loop).ConstantTermCoef);
        print(state.files.eio, "{:.3R},", state.dataHeatBal->Infiltration(Loop).TemperatureTermCoef);
        print(state.files.eio, "{:.3R},", state.dataHeatBal->Infiltration(Loop).VelocityTermCoef);
        print(state.files.eio, "{:.3R}\n", state.dataHeatBal->Infiltration(Loop).VelocitySQTermCoef);
    }

    if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
        for (Loop = 1; Loop <= state.dataHeatBal->TotInfiltration; ++Loop) {
            ZoneNum = state.dataHeatBal->Infiltration(Loop).ZonePtr;
            state.dataHeatBal->MassConservation(ZoneNum).InfiltrationPtr = Loop;
        }
    }

    for (Loop = 1; Loop <= state.dataHeatBal->TotVentilation; ++Loop) {
        if (Loop == 1) {
            print(state.files.eio,
                  Format_721,
                  "ZoneVentilation",
                  "Design Volume Flow Rate {m3/s},Volume Flow Rate/Floor Area {m3/s-m2},Volume Flow Rate/person Area {m3/s-person},ACH - Air "
                  "Changes per Hour,Fan Type {Exhaust;Intake;Natural},Fan Pressure Rise {Pa},Fan Efficiency {},Equation A - Constant Term "
                  "Coefficient {},Equation B - Temperature Term Coefficient {1/C},Equation C - Velocity Term Coefficient {s/m}, Equation D - "
                  "Velocity Squared Term Coefficient {s2/m2},Minimum Indoor Temperature{C}/Schedule,Maximum Indoor "
                  "Temperature{C}/Schedule,Delta Temperature{C}/Schedule,Minimum Outdoor Temperature{C}/Schedule,Maximum Outdoor "
                  "Temperature{C}/Schedule,Maximum WindSpeed{m/s}");
        }

        ZoneNum = state.dataHeatBal->Ventilation(Loop).ZonePtr;
        if (ZoneNum == 0) {
            print(state.files.eio, Format_722, "Ventilation-Illegal Zone specified", state.dataHeatBal->Ventilation(Loop).Name);
            continue;
        }
        TotInfilVentFlow(ZoneNum) += state.dataHeatBal->Ventilation(Loop).DesignLevel;
        print(state.files.eio,
              Format_720,
              "ZoneVentilation",
              state.dataHeatBal->Ventilation(Loop).Name,
              GetScheduleName(state, state.dataHeatBal->Ventilation(Loop).SchedPtr),
              state.dataHeatBal->Zone(ZoneNum).Name,
              state.dataHeatBal->Zone(ZoneNum).FloorArea,
              state.dataHeatBal->Zone(ZoneNum).TotOccupants);

        print(state.files.eio, "{:.3R},", state.dataHeatBal->Ventilation(Loop).DesignLevel);

        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).FloorArea, state.dataHeatBal->Ventilation(Loop).DesignLevel);
        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).TotOccupants, state.dataHeatBal->Ventilation(Loop).DesignLevel);
        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).Volume,
                                              state.dataHeatBal->Ventilation(Loop).DesignLevel * DataGlobalConstants::SecInHour);

        if (state.dataHeatBal->Ventilation(Loop).FanType == ExhaustVentilation) {
            print(state.files.eio, "Exhaust,");
        } else if (state.dataHeatBal->Ventilation(Loop).FanType == IntakeVentilation) {
            print(state.files.eio, "Intake,");
        } else if (state.dataHeatBal->Ventilation(Loop).FanType == NaturalVentilation) {
            print(state.files.eio, "Natural,");
        } else if (state.dataHeatBal->Ventilation(Loop).FanType == BalancedVentilation) {
            print(state.files.eio, "Balanced,");
        } else {
            print(state.files.eio, "UNKNOWN,");
        }
        print(state.files.eio, "{:.3R},", state.dataHeatBal->Ventilation(Loop).FanPressure);
        print(state.files.eio, "{:.1R},", state.dataHeatBal->Ventilation(Loop).FanEfficiency);
        print(state.files.eio, "{:.3R},", state.dataHeatBal->Ventilation(Loop).ConstantTermCoef);
        print(state.files.eio, "{:.3R},", state.dataHeatBal->Ventilation(Loop).TemperatureTermCoef);
        print(state.files.eio, "{:.3R},", state.dataHeatBal->Ventilation(Loop).VelocityTermCoef);
        print(state.files.eio, "{:.3R},", state.dataHeatBal->Ventilation(Loop).VelocitySQTermCoef);

        // TODO Should this also be prefixed with "Schedule: " like the following ones are?
        if (state.dataHeatBal->Ventilation(Loop).MinIndoorTempSchedPtr > 0) {
            print(state.files.eio, "{},", GetScheduleName(state, state.dataHeatBal->Ventilation(Loop).MinIndoorTempSchedPtr));
        } else {
            print(state.files.eio, "{:.2R},", state.dataHeatBal->Ventilation(Loop).MinIndoorTemperature);
        }

        const auto print_temperature = [&](const int ptr, const Real64 value) {
            if (ptr > 0) {
                print(state.files.eio, "Schedule: {},", GetScheduleName(state, ptr));
            } else {
                print(state.files.eio, "{:.2R},", value);
            }
        };

        print_temperature(state.dataHeatBal->Ventilation(Loop).MaxIndoorTempSchedPtr, state.dataHeatBal->Ventilation(Loop).MaxIndoorTemperature);
        print_temperature(state.dataHeatBal->Ventilation(Loop).DeltaTempSchedPtr, state.dataHeatBal->Ventilation(Loop).DelTemperature);
        print_temperature(state.dataHeatBal->Ventilation(Loop).MinOutdoorTempSchedPtr, state.dataHeatBal->Ventilation(Loop).MinOutdoorTemperature);
        print_temperature(state.dataHeatBal->Ventilation(Loop).MaxOutdoorTempSchedPtr, state.dataHeatBal->Ventilation(Loop).MaxOutdoorTemperature);

        print(state.files.eio, "{:.2R}\n", state.dataHeatBal->Ventilation(Loop).MaxWindSpeed);
    }

    TotMixingFlow.dimension(state.dataGlobal->NumOfZones, 0.0);
    for (Loop = 1; Loop <= state.dataHeatBal->TotMixing; ++Loop) {
        if (Loop == 1)
            print(state.files.eio,
                  Format_721,
                  "Mixing",
                  "Design Volume Flow Rate {m3/s},Volume Flow Rate/Floor Area {m3/s-m2},Volume Flow Rate/person Area {m3/s-person},ACH - Air "
                  "Changes per Hour,From/Source Zone,Delta Temperature {C}");

        ZoneNum = state.dataHeatBal->Mixing(Loop).ZonePtr;
        if (ZoneNum == 0) {
            print(state.files.eio, Format_722, "Mixing-Illegal Zone specified", state.dataHeatBal->Mixing(Loop).Name);
            continue;
        }
        TotMixingFlow(ZoneNum) += state.dataHeatBal->Mixing(Loop).DesignLevel;
        print(state.files.eio,
              Format_720,
              "Mixing",
              state.dataHeatBal->Mixing(Loop).Name,
              GetScheduleName(state, state.dataHeatBal->Mixing(Loop).SchedPtr),
              state.dataHeatBal->Zone(ZoneNum).Name,
              state.dataHeatBal->Zone(ZoneNum).FloorArea,
              state.dataHeatBal->Zone(ZoneNum).TotOccupants);
        print(state.files.eio, "{:.3R},", state.dataHeatBal->Mixing(Loop).DesignLevel);
        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).FloorArea, state.dataHeatBal->Mixing(Loop).DesignLevel);
        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).TotOccupants, state.dataHeatBal->Mixing(Loop).DesignLevel);
        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).Volume,
                                              state.dataHeatBal->Mixing(Loop).DesignLevel * DataGlobalConstants::SecInHour);

        print(state.files.eio, "{},", state.dataHeatBal->Zone(state.dataHeatBal->Mixing(Loop).FromZone).Name);
        print(state.files.eio, "{:.2R}\n", state.dataHeatBal->Mixing(Loop).DeltaTemperature);
    }

    for (Loop = 1; Loop <= state.dataHeatBal->TotCrossMixing; ++Loop) {
        if (Loop == 1) {
            print(state.files.eio,
                  Format_721,
                  "CrossMixing",
                  "Design Volume Flow Rate {m3/s},Volume Flow Rate/Floor Area {m3/s-m2},Volume Flow Rate/person Area {m3/s-person},ACH - Air "
                  "Changes per Hour,From/Source Zone,Delta Temperature {C}");
        }

        ZoneNum = state.dataHeatBal->CrossMixing(Loop).ZonePtr;
        if (ZoneNum == 0) {
            print(state.files.eio, Format_722, "CrossMixing-Illegal Zone specified", state.dataHeatBal->CrossMixing(Loop).Name);
            continue;
        }
        TotMixingFlow(ZoneNum) += state.dataHeatBal->CrossMixing(Loop).DesignLevel;
        print(state.files.eio,
              Format_720,
              "CrossMixing",
              state.dataHeatBal->CrossMixing(Loop).Name,
              GetScheduleName(state, state.dataHeatBal->CrossMixing(Loop).SchedPtr),
              state.dataHeatBal->Zone(ZoneNum).Name,
              state.dataHeatBal->Zone(ZoneNum).FloorArea,
              state.dataHeatBal->Zone(ZoneNum).TotOccupants);

        print(state.files.eio, "{:.3R},", state.dataHeatBal->CrossMixing(Loop).DesignLevel);

        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).FloorArea, state.dataHeatBal->CrossMixing(Loop).DesignLevel);
        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).TotOccupants, state.dataHeatBal->CrossMixing(Loop).DesignLevel);
        divide_and_print_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).Volume,
                                              state.dataHeatBal->CrossMixing(Loop).DesignLevel * DataGlobalConstants::SecInHour);

        print(state.files.eio, "{},", state.dataHeatBal->Zone(state.dataHeatBal->CrossMixing(Loop).FromZone).Name);
        print(state.files.eio, "{:.2R}\n", state.dataHeatBal->CrossMixing(Loop).DeltaTemperature);
    }

    if (state.dataHeatBal->TotRefDoorMixing > 0) {
        static constexpr auto Format_724("! <{} Airflow Stats Nominal>, {}\n");
        print(state.files.eio,
              Format_724,
              "RefrigerationDoorMixing ",
              "Name, Zone 1 Name,Zone 2 Name,Door Opening Schedule Name,Door Height {m},Door Area {m2},Door Protection Type");
        for (ZoneNumA = 1; ZoneNumA <= (state.dataGlobal->NumOfZones - 1); ++ZoneNumA) {
            if (!state.dataHeatBal->RefDoorMixing(ZoneNumA).RefDoorMixFlag) continue;
            for (ConnectionNumber = 1; ConnectionNumber <= state.dataHeatBal->RefDoorMixing(ZoneNumA).NumRefDoorConnections; ++ConnectionNumber) {
                ZoneNumB = state.dataHeatBal->RefDoorMixing(ZoneNumA).MateZonePtr(ConnectionNumber);
                // TotMixingFlow(ZoneNum)=TotMixingFlow(ZoneNum)+RefDoorMixing(Loop)%!DesignLevel
                static constexpr auto Format_723(" {} Airflow Stats Nominal, {},{},{},{},{:.3R},{:.3R},{}\n");
                print(state.files.eio,
                      Format_723,
                      "RefrigerationDoorMixing",
                      state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorMixingObjectName(ConnectionNumber),
                      state.dataHeatBal->Zone(ZoneNumA).Name,
                      state.dataHeatBal->Zone(ZoneNumB).Name,
                      GetScheduleName(state, state.dataHeatBal->RefDoorMixing(ZoneNumA).OpenSchedPtr(ConnectionNumber)),
                      state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorHeight(ConnectionNumber),
                      state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorArea(ConnectionNumber),
                      state.dataHeatBal->RefDoorMixing(ZoneNumA).DoorProtTypeName(ConnectionNumber));
            } // ConnectionNumber
        }     // ZoneNumA
    }         //(TotRefDoorMixing .GT. 0)

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        state.dataHeatBal->Zone(ZoneNum).NominalInfilVent = TotInfilVentFlow(ZoneNum);
        state.dataHeatBal->Zone(ZoneNum).NominalMixing = TotMixingFlow(ZoneNum);
    }

    if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
        // Check for infiltration in zone which are only a mixing source zone
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if ((state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment != DataHeatBalance::AdjustmentType::NoAdjustReturnAndMixing &&
                 state.dataHeatBal->MassConservation(ZoneNum).IsOnlySourceZone) &&
                (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment != DataHeatBalance::NoInfiltrationFlow)) {
                if (state.dataHeatBal->MassConservation(ZoneNum).InfiltrationPtr == 0) {
                    ShowSevereError(state, RoutineName + ": Infiltration object is not defined for zone = " + state.dataHeatBal->Zone(ZoneNum).Name);
                    ShowContinueError(state, "Zone air mass flow balance requires infiltration object for source zones of mixing objects");
                }
            }
        }
        // Set up zone air mass balance output variables
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            SetupOutputVariable(state,
                                "Zone Air Mass Balance Supply Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataHeatBal->MassConservation(ZoneNum).InMassFlowRate,
                                "System",
                                "Average",
                                state.dataHeatBal->Zone(ZoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Air Mass Balance Exhaust Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataHeatBal->MassConservation(ZoneNum).ExhMassFlowRate,
                                "System",
                                "Average",
                                state.dataHeatBal->Zone(ZoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Air Mass Balance Return Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataHeatBal->MassConservation(ZoneNum).RetMassFlowRate,
                                "System",
                                "Average",
                                state.dataHeatBal->Zone(ZoneNum).Name);
            if ((state.dataHeatBal->ZoneAirMassFlow.ZoneFlowAdjustment != DataHeatBalance::AdjustmentType::NoAdjustReturnAndMixing) &&
                ((state.dataHeatBal->MassConservation(ZoneNum).NumSourceZonesMixingObject +
                  state.dataHeatBal->MassConservation(ZoneNum).NumReceivingZonesMixingObject) > 0)) {
                SetupOutputVariable(state,
                                    "Zone Air Mass Balance Mixing Receiving Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataHeatBal->MassConservation(ZoneNum).MixingMassFlowRate,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(ZoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Air Mass Balance Mixing Source Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataHeatBal->MassConservation(ZoneNum).MixingSourceMassFlowRate,
                                    "System",
                                    "Average",
                                    state.dataHeatBal->Zone(ZoneNum).Name);
            }
            if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationTreatment != NoInfiltrationFlow) {
                if (state.dataHeatBal->ZoneAirMassFlow.InfiltrationZoneType == AllZones ||
                    (state.dataHeatBal->MassConservation(ZoneNum).NumSourceZonesMixingObject > 0)) {
                    if (state.dataHeatBal->MassConservation(ZoneNum).InfiltrationPtr > 0) {
                        SetupOutputVariable(state,
                                            "Zone Air Mass Balance Infiltration Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            state.dataHeatBal->MassConservation(ZoneNum).InfiltrationMassFlowRate,
                                            "System",
                                            "Average",
                                            state.dataHeatBal->Zone(ZoneNum).Name);
                        SetupOutputVariable(state,
                                            "Zone Air Mass Balance Infiltration Status",
                                            OutputProcessor::Unit::None,
                                            state.dataHeatBal->MassConservation(ZoneNum).IncludeInfilToZoneMassBal,
                                            "System",
                                            "Average",
                                            state.dataHeatBal->Zone(ZoneNum).Name);
                    }
                }
            }
        }
    }

    TotInfilVentFlow.deallocate();
    TotMixingFlow.deallocate();
    //           ' Area per Occupant {m2/person}, Occupant per Area {person/m2}, Interior Lighting {W/m2}, ',  &
    //           'Electric Load {W/m2}, Gas Load {W/m2}, Other Load {W/m2}, Hot Water Eq {W/m2}, Outdoor Controlled Baseboard Heat')
}

void GetRoomAirModelParameters(EnergyPlusData &state, bool &errFlag) // True if errors found during this input routine
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   August 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  April 2003, Weixiu Kong
    //                      December 2003, CC

    // PURPOSE OF THIS SUBROUTINE:
    //     Get room air model parameters for all zones at once

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas; // States which alpha value to read from a
    // "Number" line
    int NumNumbers; // Number of numbers encountered
    int Status;     // Notes if there was an error in processing the input
    int AirModelNum;
    int NumOfAirModels;
    int ZoneNum;
    bool ErrorsFound;
    bool IsNotOK;

    // Initialize default values for air model parameters
    state.dataRoomAirMod->AirModel.allocate(state.dataGlobal->NumOfZones);

    ErrorsFound = false;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

    cCurrentModuleObject = "RoomAirModelType";
    NumOfAirModels = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    if (NumOfAirModels > state.dataGlobal->NumOfZones) {
        ShowSevereError(state, "Too many " + cCurrentModuleObject + ".  Cannot exceed the number of Zones.");
        ErrorsFound = true;
    }

    for (AirModelNum = 1; AirModelNum <= NumOfAirModels; ++AirModelNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 AirModelNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 Status,
                                                                 _,
                                                                 _,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        ZoneNum = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataHeatBal->Zone);
        if (ZoneNum != 0) {
            if (!state.dataRoomAirMod->AirModel(ZoneNum).AirModelName.empty()) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Duplicate zone name, only one type of roomair model is allowed per zone");
                ShowContinueError(state,
                                  "Zone " + state.dataIPShortCut->cAlphaArgs(2) + " was already assigned a roomair model by " + cCurrentModuleObject +
                                      " = " + state.dataRoomAirMod->AirModel(ZoneNum).AirModelName);
                ShowContinueError(state,
                                  format("Air Model Type for zone already set to {}",
                                         DataRoomAirModel::ChAirModel[static_cast<int>(state.dataRoomAirMod->AirModel(ZoneNum).AirModelType)]));
                ShowContinueError(state, "Trying to overwrite with model type = " + state.dataIPShortCut->cAlphaArgs(3));
                ErrorsFound = true;
            }
            state.dataRoomAirMod->AirModel(ZoneNum).AirModelName = state.dataIPShortCut->cAlphaArgs(1);
            state.dataRoomAirMod->AirModel(ZoneNum).ZoneName = state.dataIPShortCut->cAlphaArgs(2);

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(3));
                if (SELECT_CASE_var == "MIXING") {
                    state.dataRoomAirMod->AirModel(ZoneNum).AirModelType = DataRoomAirModel::RoomAirModel::Mixing;
                } else if (SELECT_CASE_var == "ONENODEDISPLACEMENTVENTILATION") {
                    state.dataRoomAirMod->AirModel(ZoneNum).AirModelType = DataRoomAirModel::RoomAirModel::Mundt;
                    state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
                    state.dataRoomAirMod->MundtModelUsed = true;
                    IsNotOK = false;
                    ValidateComponent(state,
                                      "RoomAirSettings:OneNodeDisplacementVentilation",
                                      "zone_name",
                                      state.dataIPShortCut->cAlphaArgs(2),
                                      IsNotOK,
                                      "GetRoomAirModelParameters");
                    if (IsNotOK) {
                        ShowContinueError(state, "In " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1) + '.');
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "THREENODEDISPLACEMENTVENTILATION") {
                    state.dataRoomAirMod->AirModel(ZoneNum).AirModelType = DataRoomAirModel::RoomAirModel::UCSDDV;
                    state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
                    state.dataRoomAirMod->UCSDModelUsed = true;
                    IsNotOK = false;
                    ValidateComponent(state,
                                      "RoomAirSettings:ThreeNodeDisplacementVentilation",
                                      "zone_name",
                                      state.dataIPShortCut->cAlphaArgs(2),
                                      IsNotOK,
                                      "GetRoomAirModelParameters");
                    if (IsNotOK) {
                        ShowContinueError(state, "In " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1) + '.');
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "CROSSVENTILATION") {
                    state.dataRoomAirMod->AirModel(ZoneNum).AirModelType = DataRoomAirModel::RoomAirModel::UCSDCV;
                    state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
                    state.dataRoomAirMod->UCSDModelUsed = true;
                    IsNotOK = false;
                    ValidateComponent(state,
                                      "RoomAirSettings:CrossVentilation",
                                      "zone_name",
                                      state.dataIPShortCut->cAlphaArgs(2),
                                      IsNotOK,
                                      "GetRoomAirModelParameters");
                    if (IsNotOK) {
                        ShowContinueError(state, "In " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1) + '.');
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "UNDERFLOORAIRDISTRIBUTIONINTERIOR") {
                    state.dataRoomAirMod->AirModel(ZoneNum).AirModelType = DataRoomAirModel::RoomAirModel::UCSDUFI;
                    state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
                    state.dataRoomAirMod->UCSDModelUsed = true;
                    ValidateComponent(state,
                                      "RoomAirSettings:UnderFloorAirDistributionInterior",
                                      "zone_name",
                                      state.dataIPShortCut->cAlphaArgs(2),
                                      IsNotOK,
                                      "GetRoomAirModelParameters");
                    if (IsNotOK) {
                        ShowContinueError(state, "In " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1) + '.');
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "UNDERFLOORAIRDISTRIBUTIONEXTERIOR") {
                    state.dataRoomAirMod->AirModel(ZoneNum).AirModelType = DataRoomAirModel::RoomAirModel::UCSDUFE;
                    state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
                    state.dataRoomAirMod->UCSDModelUsed = true;
                    ValidateComponent(state,
                                      "RoomAirSettings:UnderFloorAirDistributionExterior",
                                      "zone_name",
                                      state.dataIPShortCut->cAlphaArgs(2),
                                      IsNotOK,
                                      "GetRoomAirModelParameters");
                    if (IsNotOK) {
                        ShowContinueError(state, "In " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1) + '.');
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "USERDEFINED") {
                    state.dataRoomAirMod->AirModel(ZoneNum).AirModelType = DataRoomAirModel::RoomAirModel::UserDefined;
                    state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
                    state.dataRoomAirMod->UserDefinedUsed = true;
                } else if (SELECT_CASE_var == "AIRFLOWNETWORK") {
                    state.dataRoomAirMod->AirModel(ZoneNum).AirModelType = DataRoomAirModel::RoomAirModel::AirflowNetwork;
                    state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
                    if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirflowNetwork:SimulationControl") == 0) {
                        ShowSevereError(state,
                                        "In " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) + ": " +
                                            state.dataIPShortCut->cAlphaFieldNames(3) + " = AIRFLOWNETWORK.");
                        ShowContinueError(state,
                                          "This model requires AirflowNetwork:* objects to form a complete network, including "
                                          "AirflowNetwork:Intrazone:Node and AirflowNetwork:Intrazone:Linkage.");
                        ShowContinueError(state, "AirflowNetwork:SimulationControl not found.");
                        ErrorsFound = true;
                    }
                } else {
                    ShowWarningError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "The mixing air model will be used for Zone =" + state.dataIPShortCut->cAlphaArgs(2));
                    state.dataRoomAirMod->AirModel(ZoneNum).AirModelType = DataRoomAirModel::RoomAirModel::Mixing;
                }
            }

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(4));
                if (SELECT_CASE_var == "DIRECT") {
                    state.dataRoomAirMod->AirModel(ZoneNum).TempCoupleScheme = DataRoomAirModel::CouplingScheme::Direct;
                } else if (SELECT_CASE_var == "INDIRECT") {
                    state.dataRoomAirMod->AirModel(ZoneNum).TempCoupleScheme = DataRoomAirModel::CouplingScheme::Indirect;
                } else {
                    ShowWarningError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + state.dataIPShortCut->cAlphaArgs(4));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "The direct coupling scheme will be used for Zone =" + state.dataIPShortCut->cAlphaArgs(2));
                    state.dataRoomAirMod->AirModel(ZoneNum).TempCoupleScheme = DataRoomAirModel::CouplingScheme::Direct;
                }
            }
        } else { // Zone Not Found
            ShowSevereError(state, cCurrentModuleObject + ", Zone not found=" + state.dataIPShortCut->cAlphaArgs(2));
            ShowContinueError(state, "occurs in " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ErrorsFound = true;
        }
    } // AirModel_Param_Loop

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        if (NumOfAirModels == 0) {
            state.dataRoomAirMod->AirModel(ZoneNum).AirModelName = "MIXING AIR MODEL FOR " + state.dataHeatBal->Zone(ZoneNum).Name;
            state.dataRoomAirMod->AirModel(ZoneNum).ZoneName = state.dataHeatBal->Zone(ZoneNum).Name;
        } else if (state.dataRoomAirMod->AirModel(ZoneNum).ZoneName == std::string()) {
            // no 'select air model' object for this zone so the mixing model is used for this zone
            state.dataRoomAirMod->AirModel(ZoneNum).AirModelName = "MIXING AIR MODEL FOR " + state.dataHeatBal->Zone(ZoneNum).Name;
            state.dataRoomAirMod->AirModel(ZoneNum).ZoneName = state.dataHeatBal->Zone(ZoneNum).Name;
        }
    }

    // Write RoomAir Model details onto EIO file
    static constexpr auto RoomAirHeader("! <RoomAir Model>, Zone Name, Mixing/Mundt/UCSDDV/UCSDCV/UCSDUFI/UCSDUFE/User Defined\n");
    print(state.files.eio, RoomAirHeader);
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        {
            static constexpr auto RoomAirZoneFmt("RoomAir Model,{},{}\n");

            auto const SELECT_CASE_var(state.dataRoomAirMod->AirModel(ZoneNum).AirModelType);
            if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::Mixing) {
                print(state.files.eio, RoomAirZoneFmt, state.dataHeatBal->Zone(ZoneNum).Name, "Mixing/Well-Stirred");
            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::Mundt) {
                print(state.files.eio, RoomAirZoneFmt, state.dataHeatBal->Zone(ZoneNum).Name, "OneNodeDisplacementVentilation");
            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UCSDDV) {
                print(state.files.eio, RoomAirZoneFmt, state.dataHeatBal->Zone(ZoneNum).Name, "ThreeNodeDisplacementVentilation");
            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UCSDCV) {
                print(state.files.eio, RoomAirZoneFmt, state.dataHeatBal->Zone(ZoneNum).Name, "CrossVentilation");
            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UCSDUFI) {
                print(state.files.eio, RoomAirZoneFmt, state.dataHeatBal->Zone(ZoneNum).Name, "UnderFloorAirDistributionInterior");
            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UCSDUFE) {
                print(state.files.eio, RoomAirZoneFmt, state.dataHeatBal->Zone(ZoneNum).Name, "UnderFloorAirDistributionExterior");
            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UserDefined) {
                print(state.files.eio, RoomAirZoneFmt, state.dataHeatBal->Zone(ZoneNum).Name, "UserDefined");
            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                print(state.files.eio, RoomAirZoneFmt, state.dataHeatBal->Zone(ZoneNum).Name, "AirflowNetwork");
            }
        }
    }

    if (ErrorsFound) {
        ShowSevereError(state, "Errors found in processing input for " + cCurrentModuleObject);
        errFlag = true;
    }
}

void InitAirHeatBalance(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   February 1998

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for  initializations within the
    // air heat balance.

    // Do the Begin Day initializations
    if (state.dataGlobal->BeginDayFlag) {
    }

    // Do the following initializations (every time step):
    InitSimpleMixingConvectiveHeatGains(state);
}

void InitSimpleMixingConvectiveHeatGains(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   February 1998
    //       MODIFIED       March 2003, FCW: allow individual window/door venting control
    //       DATE MODIFIED  April 2000
    //                      May 2009, Brent Griffith added EMS override to mixing and cross mixing flows
    //                      renamed routine and did some cleanup
    //                      August 2011, Therese Stovall added refrigeration door mixing flows
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets up the mixing and cross mixing flows

    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleIndex;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop; // local loop index
    int NZ;   // local index for zone number
    int J;    // local index for second zone in refrig door pair

    int ZoneNum;              // zone counter
    Real64 ZoneMixingFlowSum; // sum of zone mixing flows for a zone
    int NumOfMixingObjects;   // number of mixing objects for a receiving zone

    // Select type of airflow calculation

    {
        auto const SELECT_CASE_var(state.dataHeatBal->AirFlowFlag);

        if (SELECT_CASE_var == UseSimpleAirFlow) { // Simplified airflow calculation
            // Process the scheduled Mixing for air heat balance
            for (Loop = 1; Loop <= state.dataHeatBal->TotMixing; ++Loop) {
                NZ = state.dataHeatBal->Mixing(Loop).ZonePtr;
                state.dataHeatBal->Mixing(Loop).DesiredAirFlowRate =
                    state.dataHeatBal->Mixing(Loop).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->Mixing(Loop).SchedPtr);
                if (state.dataHeatBal->Mixing(Loop).EMSSimpleMixingOn)
                    state.dataHeatBal->Mixing(Loop).DesiredAirFlowRate = state.dataHeatBal->Mixing(Loop).EMSimpleMixingFlowRate;
                state.dataHeatBal->Mixing(Loop).DesiredAirFlowRateSaved = state.dataHeatBal->Mixing(Loop).DesiredAirFlowRate;
            }

            // if zone air mass flow balance enforced calculate the fraction of
            // contribution of each mixing object to a zone mixed flow rate, BAN Feb 2014
            if (state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
                for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                    ZoneMixingFlowSum = 0.0;
                    NumOfMixingObjects = state.dataHeatBal->MassConservation(ZoneNum).NumReceivingZonesMixingObject;
                    for (Loop = 1; Loop <= NumOfMixingObjects; ++Loop) {
                        ZoneMixingFlowSum = ZoneMixingFlowSum + state.dataHeatBal->Mixing(Loop).DesignLevel;
                    }
                    if (ZoneMixingFlowSum > 0.0) {
                        for (Loop = 1; Loop <= NumOfMixingObjects; ++Loop) {
                            state.dataHeatBal->MassConservation(ZoneNum).ZoneMixingReceivingFr(Loop) =
                                state.dataHeatBal->Mixing(Loop).DesignLevel / ZoneMixingFlowSum;
                        }
                    }
                }
            }

            // Process the scheduled CrossMixing for air heat balance
            for (Loop = 1; Loop <= state.dataHeatBal->TotCrossMixing; ++Loop) {
                NZ = state.dataHeatBal->CrossMixing(Loop).ZonePtr;
                state.dataHeatBal->CrossMixing(Loop).DesiredAirFlowRate =
                    state.dataHeatBal->CrossMixing(Loop).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->CrossMixing(Loop).SchedPtr);
                if (state.dataHeatBal->CrossMixing(Loop).EMSSimpleMixingOn)
                    state.dataHeatBal->CrossMixing(Loop).DesiredAirFlowRate = state.dataHeatBal->CrossMixing(Loop).EMSimpleMixingFlowRate;
            }

            // Note - do each Pair a Single time, so must do increment reports for both zones
            //       Can't have a pair that has ZoneA zone number = NumOfZones because organized
            //       in input with lowest zone # first no matter how input in idf

            // Process the scheduled Refrigeration Door mixing for air heat balance
            if (state.dataHeatBal->TotRefDoorMixing > 0) {
                for (NZ = 1; NZ <= (state.dataGlobal->NumOfZones - 1);
                     ++NZ) { // Can't have %ZonePtr==NumOfZones because lesser zone # of pair placed in ZonePtr in input
                    if (!state.dataHeatBal->RefDoorMixing(NZ).RefDoorMixFlag) continue;
                    if (state.dataHeatBal->RefDoorMixing(NZ).ZonePtr == NZ) {
                        for (J = 1; J <= state.dataHeatBal->RefDoorMixing(NZ).NumRefDoorConnections; ++J) {
                            state.dataHeatBal->RefDoorMixing(NZ).VolRefDoorFlowRate(J) = 0.0;
                            if (state.dataHeatBal->RefDoorMixing(NZ).EMSRefDoorMixingOn(J))
                                state.dataHeatBal->RefDoorMixing(NZ).VolRefDoorFlowRate(J) =
                                    state.dataHeatBal->RefDoorMixing(NZ).EMSRefDoorFlowRate(J);
                        }
                    }
                }
            } // TotRefDoorMixing

            // Infiltration and ventilation calculations have been moved to a subroutine of CalcAirFlowSimple in HVAC Manager

        } else {
        }
    }
}

void CalcHeatBalanceAir(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Legacy Code
    //       DATE WRITTEN   na
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the air component of the heat balance.

    // Using/Aliasing
    using HVACManager::ManageHVAC;

    if (state.dataGlobal->externalHVACManager) {
        if (!state.dataGlobal->externalHVACManagerInitialized) {
            initializeForExternalHVACManager(state);
        }
        state.dataGlobal->externalHVACManager(&state);
    } else {
        ManageHVAC(state);
    }

    // Do Final Temperature Calculations for Heat Balance before next Time step
    state.dataHeatBalFanSys->SumHmAW = 0.0;
    state.dataHeatBalFanSys->SumHmARa = 0.0;
    state.dataHeatBalFanSys->SumHmARaW = 0.0;
}

// END Algorithm Section of the Module

void initializeForExternalHVACManager(EnergyPlusData &state)
{
    // this function will ultimately provide a nice series of calls that initialize all the hvac stuff needed
    // to allow an external hvac manager to play nice with E+
    EnergyPlus::ZoneTempPredictorCorrector::InitZoneAirSetPoints(state);
    if (!state.dataZoneEquip->ZoneEquipInputsFilled) {
        EnergyPlus::DataZoneEquipment::GetZoneEquipmentData(state);
        state.dataZoneEquip->ZoneEquipInputsFilled = true;
    }
}

void ReportZoneMeanAirTemp(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   July 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the report variables for the AirHeatBalance.

    // Using/Aliasing
    using Psychrometrics::PsyTdpFnWPb;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneLoop;             // Counter for the # of zones (nz)
    int TempControlledZoneID; // index for zone in TempConrolled Zone structure
    Real64 thisMRTFraction;   // temp working value for radiative fraction/weight

    for (ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) {
        // The mean air temperature is actually ZTAV which is the average
        // temperature of the air temperatures at the system time step for the
        // entire zone time step.
        state.dataHeatBal->ZnAirRpt(ZoneLoop).MeanAirTemp = state.dataHeatBalFanSys->ZTAV(ZoneLoop);
        state.dataHeatBal->ZnAirRpt(ZoneLoop).MeanAirHumRat = state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneLoop);
        state.dataHeatBal->ZnAirRpt(ZoneLoop).OperativeTemp = 0.5 * (state.dataHeatBalFanSys->ZTAV(ZoneLoop) + state.dataHeatBal->ZoneMRT(ZoneLoop));
        state.dataHeatBal->ZnAirRpt(ZoneLoop).MeanAirDewPointTemp =
            PsyTdpFnWPb(state, state.dataHeatBal->ZnAirRpt(ZoneLoop).MeanAirHumRat, state.dataEnvrn->OutBaroPress);

        // if operative temperature control is being used, then radiative fraction/weighting
        //  might be defined by user to be something different than 0.5, even scheduled over simulation period
        if (state.dataZoneCtrls->AnyOpTempControl) { // dig further...
            // find TempControlledZoneID from ZoneLoop index
            TempControlledZoneID = state.dataHeatBal->Zone(ZoneLoop).TempControlledZoneIndex;
            if (state.dataHeatBal->Zone(ZoneLoop).IsControlled) {
                if ((state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).OperativeTempControl)) {
                    // is operative temp radiative fraction scheduled or fixed?
                    if (state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).OpTempCntrlModeScheduled) {
                        thisMRTFraction = GetCurrentScheduleValue(
                            state, state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).OpTempRadiativeFractionSched);
                    } else {
                        thisMRTFraction = state.dataZoneCtrls->TempControlledZone(TempControlledZoneID).FixedRadiativeFraction;
                    }
                    state.dataHeatBal->ZnAirRpt(ZoneLoop).ThermOperativeTemp =
                        (1.0 - thisMRTFraction) * state.dataHeatBalFanSys->ZTAV(ZoneLoop) + thisMRTFraction * state.dataHeatBal->ZoneMRT(ZoneLoop);
                }
            }
        }
    }
}

} // namespace EnergyPlus::HeatBalanceAirManager
