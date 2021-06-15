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
#include <cmath>
#include <map>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DaylightingDevices.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/FuelCellElectricGenerator.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/HybridModel.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/MicroCHPElectricGenerator.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/PipeHeatTransfer.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RefrigeratedCase.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/WaterUse.hh>
#include <EnergyPlus/ZonePlenum.hh>

namespace EnergyPlus {

namespace InternalHeatGains {
    // Module containing the routines dealing with the internal heat gains

    // MODULE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   August 2000
    //       MODIFIED       Aug 2005, PGE (Added object names and report variables)
    //                      Feb 2006, PGE (Added end-use subcategories)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Part of the heat balance modularization/re-engineering.  Purpose of this
    // module is to contain the internal heat gain routines in a single location.

    // METHODOLOGY EMPLOYED:
    // Routines are called as subroutines to supply the data-only module structures
    // with the proper values.

    // REFERENCES:
    // Legacy BLAST code

    // OTHER NOTES: none

    // Using/Aliasing
    using namespace DataEnvironment;
    using namespace DataHeatBalance;
    using namespace DataSurfaces;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    int const ITEClassNone(0);
    int const ITEClassA1(1);
    int const ITEClassA2(2);
    int const ITEClassA3(3);
    int const ITEClassA4(4);
    int const ITEClassB(5);
    int const ITEClassC(6);
    int const ITEInletAdjustedSupply(0);
    int const ITEInletZoneAirNode(1);
    int const ITEInletRoomAirModel(2);

    static constexpr std::string_view BlankString;

    void ManageInternalHeatGains(EnergyPlusData &state,
                                 Optional_bool_const InitOnly) // when true, just calls the get input, if appropriate and returns.
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Mar 2004, FCW: move call to DayltgElecLightingControl from InitSurfaceHeatBalance
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This is the main driver subroutine for the internal heat gains.

        if (state.dataInternalHeatGains->GetInternalHeatGainsInputFlag) {
            GetInternalHeatGainsInput(state);
            state.dataInternalHeatGains->GetInternalHeatGainsInputFlag = false;
        }

        if (present(InitOnly)) {
            if (InitOnly) return;
        }

        InitInternalHeatGains(state);

        ReportInternalHeatGains(state);

        CheckReturnAirHeatGain(state);

        // for the load component report, gather the load components for each timestep but not when doing pulse
        if (state.dataGlobal->ZoneSizingCalc) GatherComponentLoadsIntGain(state);
    }

    void GetInternalHeatGainsInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       September 1998, FW
        //                      May 2009, BG: added calls to setup for possible EMS override
        //       RE-ENGINEERED  August 2000, RKS

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the Internal Heat Gain Data for the Zones.
        // Sets up the various data that will be used later with the
        // schedulemanager to determine the actual values.

        // METHODOLOGY EMPLOYED:
        // The GetObjectItem routines are employed to retrieve the data.

        // REFERENCES:
        // IDD Objects:
        // People
        // Lights
        // ElectricEquipment
        // GasEquipment
        // SteamEquipment
        // HotWaterEquipment
        // OtherEquipment
        // ElectricEquipment:ITE:AirCooled
        // ZoneBaseboard:OutdoorTemperatureControlled

        // Using/Aliasing
        using namespace ScheduleManager;
        using General::CheckCreatedZoneItemName;

        using namespace OutputReportPredefined;
        using namespace DataLoopNode;
        using CurveManager::GetCurveIndex;
        using NodeInputManager::GetOnlySingleNode;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetInternalHeatGains: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string AlphaName;
        Array1D<Real64> IHGNumbers;
        int IOStat;
        int Loop;
        int NumAlpha;
        int NumNumber;
        int MaxAlpha;
        int MaxNumber;
        int OptionNum(0); // Autodesk:Init Initialization added to elim poss use uninitialized
        int lastOption;
        Array1D_bool RepVarSet;
        //   Variables for reporting nominal internal gains
        Real64 LightTot;       // Total Lights for calculating lights per square meter
        Real64 ElecTot;        // Total Electric Load for calculating electric per square meter
        Real64 GasTot;         // Total Gas load for calculating gas per square meter
        Real64 OthTot;         // Total Other load for calculating other load per square meter
        Real64 HWETot;         // Total Hot Water Equipment for calculating HWE per square meter
        Real64 StmTot;         // Total Steam for calculating Steam per square meter
        std::string BBHeatInd; // Yes if BBHeat in zone, no if not.
        int Loop1;
        Real64 SchMin;
        Real64 SchMax;
        std::string liteName;
        int zonePt;
        Real64 mult;
        int ZoneNum;
        Real64 maxOccupLoad;
        std::string CurrentModuleObject;
        bool errFlag;
        int Item;
        int ZLItem;
        int Item1;

        // Formats
        static constexpr fmt::string_view Format_720(" Zone Internal Gains Nominal, {},{:.2R},{:.1R},");
        static constexpr fmt::string_view Format_722(" {} Internal Gains Nominal, {},{},{},{:.2R},{:.1R},");
        static constexpr fmt::string_view Format_723("! <{} Internal Gains Nominal>,Name,Schedule Name,Zone Name,Zone Floor Area {{m2}},# Zone Occupants,{}");
        static constexpr fmt::string_view Format_724(" {}, {}\n");

        auto print_and_divide_if_greater_than_zero = [&](const Real64 numerator, const Real64 denominator) {
            if (denominator > 0.0) {
                print(state.files.eio, "{:.3R},", numerator / denominator);
            } else {
                print(state.files.eio, "N/A,");
            }
        };

        auto &ErrorsFound(state.dataInternalHeatGains->ErrorsFound);

        state.dataHeatBal->ZoneIntGain.allocate(state.dataGlobal->NumOfZones);
        state.dataHeatBal->ZnRpt.allocate(state.dataGlobal->NumOfZones);
        state.dataHeatBal->ZoneIntEEuse.allocate(state.dataGlobal->NumOfZones);
        state.dataHeatBal->RefrigCaseCredit.allocate(state.dataGlobal->NumOfZones);

        RepVarSet.dimension(state.dataGlobal->NumOfZones, true);

        // Determine argument length of objects gotten by this routine
        MaxAlpha = -100;
        MaxNumber = -100;
        CurrentModuleObject = "People";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "Lights";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "ElectricEquipment";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "GasEquipment";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "HotWaterEquipment";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "SteamEquipment";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "OtherEquipment";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "ElectricEquipment:ITE:AirCooled";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "ZoneBaseboard:OutdoorTemperatureControlled";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);
        CurrentModuleObject = "ZoneContaminantSourceAndSink:CarbonDioxide";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, Loop, NumAlpha, NumNumber);
        MaxAlpha = max(MaxAlpha, NumAlpha);
        MaxNumber = max(MaxNumber, NumNumber);

        IHGNumbers.allocate(MaxNumber);
        AlphaName.allocate(MaxAlpha);
        IHGNumbers = 0.0;
        AlphaName = "";

        // CurrentModuleObject='Zone'
        for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
            // Overall Zone Variables
            SetupOutputVariable(state,
                                "Zone Total Internal Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnRpt(Loop).TotRadiantGain,
                                "Zone",
                                "Sum",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnRpt(Loop).TotRadiantGainRate,
                                "Zone",
                                "Average",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Visible Radiation Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnRpt(Loop).TotVisHeatGain,
                                "Zone",
                                "Sum",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Visible Radiation Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnRpt(Loop).TotVisHeatGainRate,
                                "Zone",
                                "Average",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnRpt(Loop).TotConvectiveGain,
                                "Zone",
                                "Sum",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnRpt(Loop).TotConvectiveGainRate,
                                "Zone",
                                "Average",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnRpt(Loop).TotLatentGain,
                                "Zone",
                                "Sum",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnRpt(Loop).TotLatentGainRate,
                                "Zone",
                                "Average",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnRpt(Loop).TotTotalHeatGain,
                                "Zone",
                                "Sum",
                                state.dataHeatBal->Zone(Loop).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnRpt(Loop).TotTotalHeatGainRate,
                                "Zone",
                                "Average",
                                state.dataHeatBal->Zone(Loop).Name);
        }

        // PEOPLE: Includes both information related to the heat balance and thermal comfort
        // First, allocate and initialize the People derived type
        CurrentModuleObject = "People";
        state.dataHeatBal->NumPeopleStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataHeatBal->PeopleObjects.allocate(state.dataHeatBal->NumPeopleStatements);

        state.dataHeatBal->TotPeople = 0;
        errFlag = false;
        for (Item = 1; Item <= state.dataHeatBal->NumPeopleStatements; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Item,
                                                                     AlphaName,
                                                                     NumAlpha,
                                                                     IHGNumbers,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            errFlag = UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);

            state.dataHeatBal->PeopleObjects(Item).Name = AlphaName(1);

            Item1 = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
            ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0)
                ZLItem = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                state.dataHeatBal->PeopleObjects(Item).StartPtr = state.dataHeatBal->TotPeople + 1;
                ++state.dataHeatBal->TotPeople;
                state.dataHeatBal->PeopleObjects(Item).NumOfZones = 1;
                state.dataHeatBal->PeopleObjects(Item).ZoneListActive = false;
                state.dataHeatBal->PeopleObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                state.dataHeatBal->PeopleObjects(Item).StartPtr = state.dataHeatBal->TotPeople + 1;
                state.dataHeatBal->TotPeople += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->PeopleObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->PeopleObjects(Item).ZoneListActive = true;
                state.dataHeatBal->PeopleObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + AlphaName(1) + "\" invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" +
                                    AlphaName(2) + "\" not found.");
                ErrorsFound = true;
                errFlag = true;
            }
        }

        if (errFlag) {
            ShowSevereError(state, std::string{RoutineName} + "Errors with invalid names in " + CurrentModuleObject + " objects.");
            ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
            state.dataHeatBal->TotPeople = 0;
        }

        state.dataHeatBal->People.allocate(state.dataHeatBal->TotPeople);

        if (state.dataHeatBal->TotPeople > 0) {
            Loop = 0;
            for (Item = 1; Item <= state.dataHeatBal->NumPeopleStatements; ++Item) {
                AlphaName = std::string{};
                IHGNumbers = 0.0;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         Item,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                for (Item1 = 1; Item1 <= state.dataHeatBal->PeopleObjects(Item).NumOfZones; ++Item1) {
                    ++Loop;
                    if (!state.dataHeatBal->PeopleObjects(Item).ZoneListActive) {
                        state.dataHeatBal->People(Loop).Name = AlphaName(1);
                        state.dataHeatBal->People(Loop).ZonePtr = state.dataHeatBal->PeopleObjects(Item).ZoneOrZoneListPtr;
                    } else {
                        CheckCreatedZoneItemName(
                            state,
                            RoutineName,
                            CurrentModuleObject,
                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneList(state.dataHeatBal->PeopleObjects(Item).ZoneOrZoneListPtr).Zone(Item1))
                                .Name,
                            state.dataHeatBal->ZoneList(state.dataHeatBal->PeopleObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                            state.dataHeatBal->PeopleObjects(Item).Name,
                            state.dataHeatBal->People,
                            Loop - 1,
                            state.dataHeatBal->People(Loop).Name,
                            errFlag);
                        state.dataHeatBal->People(Loop).ZonePtr =
                            state.dataHeatBal->ZoneList(state.dataHeatBal->PeopleObjects(Item).ZoneOrZoneListPtr).Zone(Item1);
                        if (errFlag) ErrorsFound = true;
                    }

                    state.dataHeatBal->People(Loop).NumberOfPeoplePtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (state.dataHeatBal->People(Loop).NumberOfPeoplePtr == 0) {
                        if (Item1 == 1) { // only show error on first one
                            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                            } else {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                            }
                            ErrorsFound = true;
                        }
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, state.dataHeatBal->People(Loop).NumberOfPeoplePtr);
                        SchMax = GetScheduleMaxValue(state, state.dataHeatBal->People(Loop).NumberOfPeoplePtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (Item1 == 1) {
                                if (SchMin < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                    ErrorsFound = true;
                                }
                            }
                            if (Item1 == 1) {
                                if (SchMax < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }

                    // Number of people calculation method.
                    {
                        auto const peopleMethod(AlphaName(4));
                        if (peopleMethod == "PEOPLE") {
                            state.dataHeatBal->People(Loop).NumberOfPeople = IHGNumbers(1);
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->People(Loop).Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 People will result.");
                            }

                        } else if (peopleMethod == "PEOPLE/AREA") {
                            if (state.dataHeatBal->People(Loop).ZonePtr != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    state.dataHeatBal->People(Loop).NumberOfPeople =
                                        IHGNumbers(2) * state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).FloorArea;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).FloorArea <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->People(Loop).Name +
                                                             "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Zone Floor Area = 0.  0 People will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           state.dataHeatBal->People(Loop).Name,
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->People(Loop).Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 People will result.");
                            }

                        } else if (peopleMethod == "AREA/PERSON") {
                            if (state.dataHeatBal->People(Loop).ZonePtr != 0) {
                                if (IHGNumbers(3) > 0.0) {
                                    state.dataHeatBal->People(Loop).NumberOfPeople =
                                        state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).FloorArea / IHGNumbers(3);
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).FloorArea <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->People(Loop).Name +
                                                             "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Zone Floor Area = 0.  0 People will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           state.dataHeatBal->People(Loop).Name,
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->People(Loop).Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 People will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"People\", \"People/Area\", \"Area/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max people
                    state.dataHeatBal->People(Loop).NomMinNumberPeople = state.dataHeatBal->People(Loop).NumberOfPeople * SchMin;
                    state.dataHeatBal->People(Loop).NomMaxNumberPeople = state.dataHeatBal->People(Loop).NumberOfPeople * SchMax;

                    if (state.dataHeatBal->People(Loop).ZonePtr > 0) {
                        state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).TotOccupants +=
                            state.dataHeatBal->People(Loop).NumberOfPeople;
                    }

                    state.dataHeatBal->People(Loop).FractionRadiant = IHGNumbers(4);
                    state.dataHeatBal->People(Loop).FractionConvected = 1.0 - state.dataHeatBal->People(Loop).FractionRadiant;
                    if (Item1 == 1) {
                        if (state.dataHeatBal->People(Loop).FractionConvected < 0.0) {
                            ShowSevereError(state,
                                            format("{}{}=\"{}\", {} < 0.0, value ={:.2R}",
                                                   RoutineName,
                                                   CurrentModuleObject,
                                                   AlphaName(1),
                                                   state.dataIPShortCut->cNumericFieldNames(4),
                                                   IHGNumbers(4)));
                            ErrorsFound = true;
                        }
                    }

                    if (NumNumber >= 5 && !state.dataIPShortCut->lNumericFieldBlanks(5)) {
                        state.dataHeatBal->People(Loop).UserSpecSensFrac = IHGNumbers(5);
                    } else {
                        state.dataHeatBal->People(Loop).UserSpecSensFrac = DataGlobalConstants::AutoCalculate;
                    }

                    if (NumNumber == 6 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                        state.dataHeatBal->People(Loop).CO2RateFactor = IHGNumbers(6);
                    } else {
                        state.dataHeatBal->People(Loop).CO2RateFactor = 3.82e-8; // m3/s-W
                    }
                    if (state.dataHeatBal->People(Loop).CO2RateFactor < 0.0) {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", {} < 0.0, value ={:.2R}",
                                               RoutineName,
                                               CurrentModuleObject,
                                               AlphaName(1),
                                               state.dataIPShortCut->cNumericFieldNames(6),
                                               IHGNumbers(6)));
                        ErrorsFound = true;
                    }

                    state.dataHeatBal->People(Loop).ActivityLevelPtr = GetScheduleIndex(state, AlphaName(5));
                    if (state.dataHeatBal->People(Loop).ActivityLevelPtr == 0) {
                        if (Item1 == 1) {
                            if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(5) + " is required.");
                            } else {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(5) + " entered=" + AlphaName(5));
                            }
                            ErrorsFound = true;
                        }
                    } else { // Check values in Schedule
                        SchMin = GetScheduleMinValue(state, state.dataHeatBal->People(Loop).ActivityLevelPtr);
                        SchMax = GetScheduleMaxValue(state, state.dataHeatBal->People(Loop).ActivityLevelPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (Item1 == 1) {
                                if (SchMin < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(5) + " minimum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(5), SchMin));
                                    ErrorsFound = true;
                                }
                            }
                            if (Item1 == 1) {
                                if (SchMax < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(5) + " maximum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(5), SchMax));
                                    ErrorsFound = true;
                                }
                            }
                        } else if (SchMin < 70.0 || SchMax > 1000.0) {
                            if (Item1 == 1) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                     state.dataIPShortCut->cAlphaFieldNames(5) + " values");
                                ShowContinueError(state, "fall outside typical range [70,1000] W/person for Thermal Comfort Reporting.");
                                ShowContinueError(state, "Odd comfort values may result; Schedule=\"" + AlphaName(5) + "\".");
                                ShowContinueError(state, format("Entered min/max range=[{:.1R},] W/person.{:.1R}", SchMin, SchMax));
                            }
                        }
                    }

                    // Following is an optional parameter (ASHRAE 55 warnings
                    if (NumAlpha >= 6) {
                        if (UtilityRoutines::SameString(AlphaName(6), "Yes")) {
                            state.dataHeatBal->People(Loop).Show55Warning = true;
                        } else if (!UtilityRoutines::SameString(AlphaName(6), "No") && !state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(6) + " field should be Yes or No");
                                ShowContinueError(state, "...Field value=\"" + AlphaName(6) + "\" is invalid.");
                                ErrorsFound = true;
                            }
                        }
                    }

                    if (NumAlpha > 6) { // Optional parameters present--thermal comfort data follows...
                        state.dataInternalHeatGains->UsingThermalComfort = false;
                        if (NumAlpha > 20) {
                            lastOption = 20;
                        } else {
                            lastOption = NumAlpha;
                        }

                        // check to see if the user has specified schedules for air velocity, clothing insulation, and/or work efficiency
                        // but have NOT made a selection for a thermal comfort model.  If so, then the schedules are reported as unused
                        // which could cause confusion.  The solution is for the user to either remove those schedules or pick a thermal
                        // comfort model.
                        int const NumFirstTCModel = 14;
                        if (NumAlpha < NumFirstTCModel) {
                            bool NoTCModelSelectedWithSchedules = false;
                            NoTCModelSelectedWithSchedules = CheckThermalComfortSchedules(state.dataIPShortCut->lAlphaFieldBlanks(9),
                                                                                          state.dataIPShortCut->lAlphaFieldBlanks(12),
                                                                                          state.dataIPShortCut->lAlphaFieldBlanks(13));
                            if (NoTCModelSelectedWithSchedules) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) +
                                                     "\" has comfort related schedules but no thermal comfort model selected.");
                                ShowContinueError(state,
                                                  "If schedules are specified for air velocity, clothing insulation, and/or work efficiency but no "
                                                  "thermal comfort");
                                ShowContinueError(
                                    state, "thermal comfort model is selected, the schedules will be listed as unused schedules in the .err file.");
                                ShowContinueError(
                                    state,
                                    "To avoid these errors, select a valid thermal comfort model or eliminate these schedules in the PEOPLE input.");
                            }
                        }

                        for (OptionNum = NumFirstTCModel; OptionNum <= lastOption; ++OptionNum) {

                            {
                                auto const thermalComfortType(AlphaName(OptionNum));

                                if (thermalComfortType == "FANGER") {
                                    state.dataHeatBal->People(Loop).Fanger = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "PIERCE") {
                                    state.dataHeatBal->People(Loop).Pierce = true;
                                    state.dataHeatBal->AnyThermalComfortPierceModel = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "KSU") {
                                    state.dataHeatBal->People(Loop).KSU = true;
                                    state.dataHeatBal->AnyThermalComfortKSUModel = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "ADAPTIVEASH55") {
                                    state.dataHeatBal->People(Loop).AdaptiveASH55 = true;
                                    state.dataHeatBal->AdaptiveComfortRequested_ASH55 = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "ADAPTIVECEN15251") {
                                    state.dataHeatBal->People(Loop).AdaptiveCEN15251 = true;
                                    state.dataHeatBal->AdaptiveComfortRequested_CEN15251 = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "COOLINGEFFECTASH55") {
                                    state.dataHeatBal->People(Loop).CoolingEffectASH55 = true;
                                    state.dataHeatBal->AnyThermalComfortCoolingEffectModel = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "ANKLEDRAFTASH55") {
                                    state.dataHeatBal->People(Loop).AnkleDraftASH55 = true;
                                    state.dataHeatBal->AnyThermalComfortAnkleDraftModel = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "") { // Blank input field--just ignore this

                                } else { // An invalid keyword was entered--warn but ignore
                                    if (Item1 == 1) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                             state.dataIPShortCut->cAlphaFieldNames(OptionNum) + " Option=" + AlphaName(OptionNum));
                                        ShowContinueError(state,
                                                          "Valid Values are \"Fanger\", \"Pierce\", \"KSU\", \"AdaptiveASH55\", "
                                                          "\"AdaptiveCEN15251\", \"CoolingEffectASH55\", \"AnkleDraftASH55\"");
                                    }
                                }
                            }
                        }

                        if (state.dataInternalHeatGains->UsingThermalComfort) {

                            // Set the default value of MRTCalcType as 'ZoneAveraged'
                            state.dataHeatBal->People(Loop).MRTCalcType = ZoneAveraged;

                            bool ModelWithAdditionalInputs = state.dataHeatBal->People(Loop).Fanger || state.dataHeatBal->People(Loop).Pierce ||
                                                             state.dataHeatBal->People(Loop).KSU ||
                                                             state.dataHeatBal->People(Loop).CoolingEffectASH55 ||
                                                             state.dataHeatBal->People(Loop).AnkleDraftASH55;

                            // MRT Calculation Type and Surface Name
                            {
                                auto const mrtType(AlphaName(7));

                                if (mrtType == "ZONEAVERAGED") {
                                    state.dataHeatBal->People(Loop).MRTCalcType = ZoneAveraged;

                                } else if (mrtType == "SURFACEWEIGHTED") {
                                    state.dataHeatBal->People(Loop).MRTCalcType = SurfaceWeighted;
                                    state.dataHeatBal->People(Loop).SurfacePtr =
                                        UtilityRoutines::FindItemInList(AlphaName(8), state.dataSurface->Surface);
                                    if (state.dataHeatBal->People(Loop).SurfacePtr == 0 && ModelWithAdditionalInputs) {
                                        if (Item1 == 1) {
                                            ShowSevereError(state,
                                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                                state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphaName(7) +
                                                                " invalid Surface Name=" + AlphaName(8));
                                            ErrorsFound = true;
                                        }
                                    } else if (state.dataSurface->Surface(state.dataHeatBal->People(Loop).SurfacePtr).Zone !=
                                                   state.dataHeatBal->People(Loop).ZonePtr &&
                                               ModelWithAdditionalInputs) {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", Surface referenced in " +
                                                            state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphaName(7) + " in different zone.");
                                        ShowContinueError(
                                            state,
                                            "Surface is in Zone=" +
                                                state.dataHeatBal->Zone(state.dataSurface->Surface(state.dataHeatBal->People(Loop).SurfacePtr).Zone)
                                                    .Name +
                                                " and " + CurrentModuleObject + " is in Zone=" + AlphaName(2));
                                        ErrorsFound = true;
                                    }

                                } else if (mrtType == "ANGLEFACTOR") {
                                    state.dataHeatBal->People(Loop).MRTCalcType = AngleFactor;
                                    state.dataHeatBal->People(Loop).AngleFactorListName = AlphaName(8);

                                } else if (mrtType == "") { // Blank input field--just ignore this
                                    if (Item1 == 1 && ModelWithAdditionalInputs)
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", blank " +
                                                             state.dataIPShortCut->cAlphaFieldNames(7));

                                } else { // An invalid keyword was entered--warn but ignore
                                    if (Item1 == 1 && ModelWithAdditionalInputs) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                             state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphaName(7));
                                        ShowContinueError(state, "...Valid values are \"ZoneAveraged\", \"SurfaceWeighted\", \"AngleFactor\".");
                                    }
                                }
                            }

                            if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                                state.dataHeatBal->People(Loop).WorkEffPtr = GetScheduleIndex(state, AlphaName(9));
                                if (state.dataHeatBal->People(Loop).WorkEffPtr == 0) {
                                    if (Item1 == 1) {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                            state.dataIPShortCut->cAlphaFieldNames(9) + " entered=" + AlphaName(9));
                                        ErrorsFound = true;
                                    }
                                } else { // check min/max on schedule
                                    SchMin = GetScheduleMinValue(state, state.dataHeatBal->People(Loop).WorkEffPtr);
                                    SchMax = GetScheduleMaxValue(state, state.dataHeatBal->People(Loop).WorkEffPtr);
                                    if (SchMin < 0.0 || SchMax < 0.0) {
                                        if (SchMin < 0.0) {
                                            if (Item1 == 1) {
                                                ShowSevereError(state,
                                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                                    state.dataIPShortCut->cAlphaFieldNames(9) + ", minimum is < 0.0");
                                                ShowContinueError(
                                                    state,
                                                    format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(9), SchMin));
                                                ErrorsFound = true;
                                            }
                                        }
                                        if (SchMax < 0.0) {
                                            if (Item1 == 1) {
                                                ShowSevereError(state,
                                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                                    state.dataIPShortCut->cAlphaFieldNames(9) + ", maximum is < 0.0");
                                                ShowContinueError(
                                                    state,
                                                    format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(9), SchMax));
                                                ErrorsFound = true;
                                            }
                                        }
                                    }
                                    if (SchMax > 1.0) {
                                        if (Item1 == 1) {
                                            ShowWarningError(state,
                                                             std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                                 state.dataIPShortCut->cAlphaFieldNames(9) + ", maximum is > 1.0");
                                            ShowContinueError(state,
                                                              format("Schedule=\"{}\"; Entered min/max range=[{:.1R},{:.1R}] Work Efficiency.",
                                                                     AlphaName(9),
                                                                     SchMin,
                                                                     SchMax));
                                        }
                                    }
                                }
                            } else if (ModelWithAdditionalInputs) {
                                if (Item1 == 1) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", blank " +
                                                        state.dataIPShortCut->cAlphaFieldNames(9) + ". " + state.dataIPShortCut->cAlphaFieldNames(9) +
                                                        " is required when Thermal Comfort Model Type is one of "
                                                        "\"Fanger\", \"Pierce\", \"KSU\", \"CoolingEffectASH55\" or \"AnkleDraftASH55\"");
                                    ErrorsFound = true;
                                }
                            }

                            if (!state.dataIPShortCut->lAlphaFieldBlanks(10) || AlphaName(10) != "") {
                                {
                                    auto const clothingType(AlphaName(10));
                                    if (clothingType == "CLOTHINGINSULATIONSCHEDULE") {
                                        state.dataHeatBal->People(Loop).ClothingType = 1;
                                        state.dataHeatBal->People(Loop).ClothingPtr = GetScheduleIndex(state, AlphaName(12));
                                        if (state.dataHeatBal->People(Loop).ClothingPtr == 0 && ModelWithAdditionalInputs) {
                                            if (Item1 == 1) {
                                                ShowSevereError(state,
                                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                                    state.dataIPShortCut->cAlphaFieldNames(12) + " entered=\"" + AlphaName(12) +
                                                                    "\".");
                                                ErrorsFound = true;
                                            }
                                        } else { // check min/max on schedule
                                            SchMin = GetScheduleMinValue(state, state.dataHeatBal->People(Loop).ClothingPtr);
                                            SchMax = GetScheduleMaxValue(state, state.dataHeatBal->People(Loop).ClothingPtr);
                                            if (SchMin < 0.0 || SchMax < 0.0) {
                                                if (SchMin < 0.0) {
                                                    if (Item1 == 1) {
                                                        ShowSevereError(state,
                                                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                                            state.dataIPShortCut->cAlphaFieldNames(12) + ", minimum is < 0.0");
                                                        ShowContinueError(state,
                                                                          format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.",
                                                                                 AlphaName(12),
                                                                                 SchMin));
                                                        ErrorsFound = true;
                                                    }
                                                }
                                                if (SchMax < 0.0) {
                                                    if (Item1 == 1) {
                                                        ShowSevereError(state,
                                                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                                            state.dataIPShortCut->cAlphaFieldNames(12) + ", maximum is < 0.0");
                                                        ShowContinueError(state,
                                                                          format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.",
                                                                                 AlphaName(12),
                                                                                 SchMax));
                                                        ErrorsFound = true;
                                                    }
                                                }
                                            }
                                            if (SchMax > 2.0) {
                                                if (Item1 == 1) {
                                                    ShowWarningError(state,
                                                                     std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                                         state.dataIPShortCut->cAlphaFieldNames(12) + ", maximum is > 2.0");
                                                    ShowContinueError(state,
                                                                      format("Schedule=\"{}\"; Entered min/max range=[{:.1R},{:.1R}] Clothing.",
                                                                             AlphaName(12),
                                                                             SchMin,
                                                                             SchMax));
                                                }
                                            }
                                        }

                                    } else if (clothingType == "DYNAMICCLOTHINGMODELASHRAE55") {
                                        state.dataHeatBal->People(Loop).ClothingType = 2;

                                    } else if (clothingType == "CALCULATIONMETHODSCHEDULE") {
                                        state.dataHeatBal->People(Loop).ClothingType = 3;
                                        state.dataHeatBal->People(Loop).ClothingMethodPtr = GetScheduleIndex(state, AlphaName(11));
                                        if (state.dataHeatBal->People(Loop).ClothingMethodPtr == 0) {
                                            if (Item1 == 1) {
                                                ShowSevereError(state,
                                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                                    state.dataIPShortCut->cAlphaFieldNames(11) + " entered=\"" + AlphaName(11) +
                                                                    "\".");
                                                ErrorsFound = true;
                                            }
                                        }
                                        if (CheckScheduleValue(state, state.dataHeatBal->People(Loop).ClothingMethodPtr, 1)) {
                                            state.dataHeatBal->People(Loop).ClothingPtr = GetScheduleIndex(state, AlphaName(12));
                                            if (state.dataHeatBal->People(Loop).ClothingPtr == 0) {
                                                if (Item1 == 1) {
                                                    ShowSevereError(state,
                                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                                        state.dataIPShortCut->cAlphaFieldNames(12) + " entered=\"" + AlphaName(12) +
                                                                        "\".");
                                                    ErrorsFound = true;
                                                }
                                            }
                                        }

                                    } else {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->People(Loop).Name +
                                                            "\", invalid " + state.dataIPShortCut->cAlphaFieldNames(10) +
                                                            ", value  =" + AlphaName(10));
                                        ShowContinueError(state,
                                                          "...Valid values are \"ClothingInsulationSchedule\",\"DynamicClothingModelASHRAE55a\", "
                                                          "\"CalculationMethodSchedule\".");
                                        ErrorsFound = true;
                                    }
                                }
                            }

                            if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                                state.dataHeatBal->People(Loop).AirVelocityPtr = GetScheduleIndex(state, AlphaName(13));
                                if (state.dataHeatBal->People(Loop).AirVelocityPtr == 0) {
                                    if (Item1 == 1) {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                            state.dataIPShortCut->cAlphaFieldNames(13) + " entered=\"" + AlphaName(13) + "\".");
                                        ErrorsFound = true;
                                    }
                                } else { // check min/max on schedule
                                    SchMin = GetScheduleMinValue(state, state.dataHeatBal->People(Loop).AirVelocityPtr);
                                    SchMax = GetScheduleMaxValue(state, state.dataHeatBal->People(Loop).AirVelocityPtr);
                                    if (SchMin < 0.0 || SchMax < 0.0) {
                                        if (SchMin < 0.0) {
                                            if (Item1 == 1) {
                                                ShowSevereError(state,
                                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                                    state.dataIPShortCut->cAlphaFieldNames(13) + ", minimum is < 0.0");
                                                ShowContinueError(
                                                    state,
                                                    format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(13), SchMin));
                                                ErrorsFound = true;
                                            }
                                        }
                                        if (SchMax < 0.0) {
                                            if (Item1 == 1) {
                                                ShowSevereError(state,
                                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                                    state.dataIPShortCut->cAlphaFieldNames(13) + ", maximum is < 0.0");
                                                ShowContinueError(
                                                    state,
                                                    format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(13), SchMax));
                                                ErrorsFound = true;
                                            }
                                        }
                                    }
                                }
                            } else if (ModelWithAdditionalInputs) {
                                if (Item1 == 1) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", blank " +
                                                        state.dataIPShortCut->cAlphaFieldNames(13) + ". " +
                                                        state.dataIPShortCut->cAlphaFieldNames(13) +
                                                        " is required when Thermal Comfort Model Type is one of "
                                                        "\"Fanger\", \"Pierce\", \"KSU\", \"CoolingEffectASH55\" or \"AnkleDraftASH55\"");
                                    ErrorsFound = true;
                                }
                            }

                            int indexAnkleAirVelPtr = 21;
                            if (!state.dataIPShortCut->lAlphaFieldBlanks(indexAnkleAirVelPtr) || AlphaName(indexAnkleAirVelPtr) != "") {
                                state.dataHeatBal->People(Loop).AnkleAirVelocityPtr = GetScheduleIndex(state, AlphaName(indexAnkleAirVelPtr));
                                if (state.dataHeatBal->People(Loop).AnkleAirVelocityPtr == 0) {
                                    if (Item1 == 1) {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                            state.dataIPShortCut->cAlphaFieldNames(indexAnkleAirVelPtr) + " entered=\"" +
                                                            AlphaName(indexAnkleAirVelPtr) + "\".");
                                        ErrorsFound = true;
                                    }
                                }
                            } else if (state.dataHeatBal->People(Loop).AnkleDraftASH55) {
                                if (Item1 == 1) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", blank " +
                                                        state.dataIPShortCut->cAlphaFieldNames(indexAnkleAirVelPtr) + ". " +
                                                        state.dataIPShortCut->cAlphaFieldNames(indexAnkleAirVelPtr) +
                                                        " is required when Thermal Comfort Model Type is one of "
                                                        "\"Fanger\", \"Pierce\", \"KSU\", \"CoolingEffectASH55\" or \"AnkleDraftASH55\"");
                                    ErrorsFound = true;
                                }
                            }

                        } // usingthermalcomfort block

                    } // ...end of thermal comfort data IF-THEN block  (NumAlphas > 6)

                    if (state.dataHeatBal->People(Loop).ZonePtr <= 0) continue; // Error, will be caught and terminated later

                    // Object report variables
                    SetupOutputVariable(state,
                                        "People Occupant Count",
                                        OutputProcessor::Unit::None,
                                        state.dataHeatBal->People(Loop).NumOcc,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Radiant Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->People(Loop).RadGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Radiant Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->People(Loop).RadGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Convective Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->People(Loop).ConGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Convective Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->People(Loop).ConGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Sensible Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->People(Loop).SenGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Sensible Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->People(Loop).SenGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Latent Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->People(Loop).LatGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Latent Gain Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->People(Loop).LatGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Total Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->People(Loop).TotGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Total Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->People(Loop).TotGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Air Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataHeatBal->People(Loop).TemperatureInZone,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->People(Loop).Name);
                    SetupOutputVariable(state,
                                        "People Air Relative Humidity",
                                        OutputProcessor::Unit::Perc,
                                        state.dataHeatBal->People(Loop).RelativeHumidityInZone,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->People(Loop).Name);

                    // Zone total report variables
                    if (RepVarSet(state.dataHeatBal->People(Loop).ZonePtr)) {
                        RepVarSet(state.dataHeatBal->People(Loop).ZonePtr) = false;
                        SetupOutputVariable(state,
                                            "Zone People Occupant Count",
                                            OutputProcessor::Unit::None,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleNumOcc,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone People Radiant Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleRadGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone People Radiant Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleRadGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone People Convective Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleConGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone People Convective Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleConGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone People Sensible Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleSenGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone People Sensible Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleSenGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone People Latent Gain Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleLatGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone People Latent Gain Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleLatGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone People Total Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleTotGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone People Total Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->People(Loop).ZonePtr).PeopleTotGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->People(Loop).ZonePtr).Name);
                    }

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "People",
                                         state.dataHeatBal->People(Loop).Name,
                                         "Number of People",
                                         "[each]",
                                         state.dataHeatBal->People(Loop).EMSPeopleOn,
                                         state.dataHeatBal->People(Loop).EMSNumberOfPeople);
                        SetupEMSInternalVariable(state,
                                                 "People Count Design Level",
                                                 state.dataHeatBal->People(Loop).Name,
                                                 "[each]",
                                                 state.dataHeatBal->People(Loop).NumberOfPeople);
                    }

                    // setup internal gains
                    if (!ErrorsFound)
                        SetupZoneInternalGain(state,
                                              state.dataHeatBal->People(Loop).ZonePtr,
                                              "People",
                                              state.dataHeatBal->People(Loop).Name,
                                              IntGainTypeOf_People,
                                              &state.dataHeatBal->People(Loop).ConGainRate,
                                              nullptr,
                                              &state.dataHeatBal->People(Loop).RadGainRate,
                                              &state.dataHeatBal->People(Loop).LatGainRate,
                                              nullptr,
                                              &state.dataHeatBal->People(Loop).CO2GainRate);

                } // Item1 - number of zones
            }     // Item - number of people statements
        }         // TotPeople > 0

        // transfer the nominal number of people in a zone to the tabular reporting
        for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
            if (state.dataHeatBal->Zone(Loop).TotOccupants > 0.0) {
                if (state.dataHeatBal->Zone(Loop).FloorArea > 0.0 &&
                    state.dataHeatBal->Zone(Loop).FloorArea / state.dataHeatBal->Zone(Loop).TotOccupants < 0.1) {
                    ShowWarningError(state, std::string{RoutineName} + "Zone=\"" + state.dataHeatBal->Zone(Loop).Name + "\" occupant density is extremely high.");
                    if (state.dataHeatBal->Zone(Loop).FloorArea > 0.0) {
                        ShowContinueError(state,
                                          format("Occupant Density=[{:.0R}] person/m2.",
                                                 state.dataHeatBal->Zone(Loop).TotOccupants / state.dataHeatBal->Zone(Loop).FloorArea));
                    }
                    ShowContinueError(state,
                                      format("Occupant Density=[{:.3R}] m2/person. Problems in Temperature Out of Bounds may result.",
                                             state.dataHeatBal->Zone(Loop).FloorArea / state.dataHeatBal->Zone(Loop).TotOccupants));
                }
                maxOccupLoad = 0.0;
                for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotPeople; ++Loop1) {
                    if (state.dataHeatBal->People(Loop1).ZonePtr != Loop) continue;
                    if (maxOccupLoad < GetScheduleMaxValue(state, state.dataHeatBal->People(Loop1).NumberOfPeoplePtr) *
                                           state.dataHeatBal->People(Loop1).NumberOfPeople) {
                        maxOccupLoad = GetScheduleMaxValue(state, state.dataHeatBal->People(Loop1).NumberOfPeoplePtr) *
                                       state.dataHeatBal->People(Loop1).NumberOfPeople;
                        MaxNumber = state.dataHeatBal->People(Loop1).NumberOfPeoplePtr;
                        OptionNum = Loop1;
                    }
                }
                if (maxOccupLoad > state.dataHeatBal->Zone(Loop).TotOccupants) {
                    if (state.dataHeatBal->Zone(Loop).FloorArea > 0.0 && state.dataHeatBal->Zone(Loop).FloorArea / maxOccupLoad < 0.1) {
                        ShowWarningError(state,
                                         std::string{RoutineName} + "Zone=\"" + state.dataHeatBal->Zone(Loop).Name +
                                             "\" occupant density at a maximum schedule value is extremely high.");
                        if (state.dataHeatBal->Zone(Loop).FloorArea > 0.0) {
                            ShowContinueError(state,
                                              format("Occupant Density=[{:.0R}] person/m2.", maxOccupLoad / state.dataHeatBal->Zone(Loop).FloorArea));
                        }
                        ShowContinueError(state,
                                          format("Occupant Density=[{:.3R}] m2/person. Problems in Temperature Out of Bounds may result.",
                                                 state.dataHeatBal->Zone(Loop).FloorArea / maxOccupLoad));
                        ShowContinueError(state,
                                          "Check values in People=" + state.dataHeatBal->People(OptionNum).Name +
                                              ", Number of People Schedule=" + GetScheduleName(state, MaxNumber));
                    }
                }
            }

            if (state.dataHeatBal->Zone(Loop).isNominalControlled) { // conditioned zones only
                if (state.dataHeatBal->Zone(Loop).TotOccupants > 0.0) {
                    state.dataHeatBal->Zone(Loop).isNominalOccupied = true;
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchOaoNomNumOcc1,
                                     state.dataHeatBal->Zone(Loop).Name,
                                     state.dataHeatBal->Zone(Loop).TotOccupants);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchOaoNomNumOcc2,
                                     state.dataHeatBal->Zone(Loop).Name,
                                     state.dataHeatBal->Zone(Loop).TotOccupants);
                }
            }
        }

        RepVarSet = true;
        CurrentModuleObject = "Lights";
        state.dataHeatBal->NumLightsStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataHeatBal->LightsObjects.allocate(state.dataHeatBal->NumLightsStatements);

        state.dataHeatBal->TotLights = 0;
        errFlag = false;
        for (Item = 1; Item <= state.dataHeatBal->NumLightsStatements; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Item,
                                                                     AlphaName,
                                                                     NumAlpha,
                                                                     IHGNumbers,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            errFlag = UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);

            state.dataHeatBal->LightsObjects(Item).Name = AlphaName(1);

            Item1 = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
            ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0)
                ZLItem = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                state.dataHeatBal->LightsObjects(Item).StartPtr = state.dataHeatBal->TotLights + 1;
                ++state.dataHeatBal->TotLights;
                state.dataHeatBal->LightsObjects(Item).NumOfZones = 1;
                state.dataHeatBal->LightsObjects(Item).ZoneListActive = false;
                state.dataHeatBal->LightsObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                state.dataHeatBal->LightsObjects(Item).StartPtr = state.dataHeatBal->TotLights + 1;
                state.dataHeatBal->TotLights += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->LightsObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->LightsObjects(Item).ZoneListActive = true;
                state.dataHeatBal->LightsObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + AlphaName(1) + "\" invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" +
                                    AlphaName(2) + "\" not found.");
                ErrorsFound = true;
                errFlag = true;
            }
        }

        if (errFlag) {
            ShowSevereError(state, std::string{RoutineName} + "Errors with invalid names in " + CurrentModuleObject + " objects.");
            ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
            state.dataHeatBal->TotLights = 0;
        }

        state.dataHeatBal->Lights.allocate(state.dataHeatBal->TotLights);

        if (state.dataHeatBal->TotLights > 0) {
            Loop = 0;
            for (Item = 1; Item <= state.dataHeatBal->NumLightsStatements; ++Item) {
                AlphaName = std::string{};
                IHGNumbers = 0.0;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         Item,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                for (Item1 = 1; Item1 <= state.dataHeatBal->LightsObjects(Item).NumOfZones; ++Item1) {
                    ++Loop;
                    if (!state.dataHeatBal->LightsObjects(Item).ZoneListActive) {
                        state.dataHeatBal->Lights(Loop).Name = AlphaName(1);
                        state.dataHeatBal->Lights(Loop).ZonePtr = state.dataHeatBal->LightsObjects(Item).ZoneOrZoneListPtr;
                    } else {
                        CheckCreatedZoneItemName(
                            state,
                            RoutineName,
                            CurrentModuleObject,
                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneList(state.dataHeatBal->LightsObjects(Item).ZoneOrZoneListPtr).Zone(Item1))
                                .Name,
                            state.dataHeatBal->ZoneList(state.dataHeatBal->LightsObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                            state.dataHeatBal->LightsObjects(Item).Name,
                            state.dataHeatBal->Lights,
                            Loop - 1,
                            state.dataHeatBal->Lights(Loop).Name,
                            errFlag);
                        state.dataHeatBal->Lights(Loop).ZonePtr =
                            state.dataHeatBal->ZoneList(state.dataHeatBal->LightsObjects(Item).ZoneOrZoneListPtr).Zone(Item1);
                        if (errFlag) ErrorsFound = true;
                    }

                    state.dataHeatBal->Lights(Loop).SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (state.dataHeatBal->Lights(Loop).SchedPtr == 0) {
                        if (Item1 == 1) {
                            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                            } else {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                            }
                            ErrorsFound = true;
                        }
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, state.dataHeatBal->Lights(Loop).SchedPtr);
                        SchMax = GetScheduleMaxValue(state, state.dataHeatBal->Lights(Loop).SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (Item1 == 1) {
                                if (SchMin < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                    ErrorsFound = true;
                                }
                            }
                            if (Item1 == 1) {
                                if (SchMax < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }

                    // Lights Design Level calculation method.
                    {
                        auto const lightingLevel(AlphaName(4));
                        if (lightingLevel == "LIGHTINGLEVEL") {
                            state.dataHeatBal->Lights(Loop).DesignLevel = IHGNumbers(1);
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->Lights(Loop).Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 Lights will result.");
                            }

                        } else if (lightingLevel == "WATTS/AREA") {
                            if (state.dataHeatBal->Lights(Loop).ZonePtr != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    state.dataHeatBal->Lights(Loop).DesignLevel =
                                        IHGNumbers(2) * state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).FloorArea;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).FloorArea <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->Lights(Loop).Name +
                                                             "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Zone Floor Area = 0.  0 Lights will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           state.dataHeatBal->Lights(Loop).Name,
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->Lights(Loop).Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 Lights will result.");
                            }

                        } else if (lightingLevel == "WATTS/PERSON") {
                            if (state.dataHeatBal->Lights(Loop).ZonePtr != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    state.dataHeatBal->Lights(Loop).DesignLevel =
                                        IHGNumbers(3) * state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).TotOccupants;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).TotOccupants <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->Lights(Loop).Name +
                                                             "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Total Occupants = 0.  0 Lights will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           state.dataHeatBal->Lights(Loop).Name,
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->Lights(Loop).Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 Lights will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"LightingLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max lighting level
                    state.dataHeatBal->Lights(Loop).NomMinDesignLevel = state.dataHeatBal->Lights(Loop).DesignLevel * SchMin;
                    state.dataHeatBal->Lights(Loop).NomMaxDesignLevel = state.dataHeatBal->Lights(Loop).DesignLevel * SchMax;

                    state.dataHeatBal->Lights(Loop).FractionReturnAir = IHGNumbers(4);
                    state.dataHeatBal->Lights(Loop).FractionRadiant = IHGNumbers(5);
                    state.dataHeatBal->Lights(Loop).FractionShortWave = IHGNumbers(6);
                    state.dataHeatBal->Lights(Loop).FractionReplaceable = IHGNumbers(7);
                    state.dataHeatBal->Lights(Loop).FractionReturnAirPlenTempCoeff1 = IHGNumbers(8);
                    state.dataHeatBal->Lights(Loop).FractionReturnAirPlenTempCoeff2 = IHGNumbers(9);

                    state.dataHeatBal->Lights(Loop).FractionConvected =
                        1.0 - (state.dataHeatBal->Lights(Loop).FractionReturnAir + state.dataHeatBal->Lights(Loop).FractionRadiant +
                               state.dataHeatBal->Lights(Loop).FractionShortWave);
                    if (std::abs(state.dataHeatBal->Lights(Loop).FractionConvected) <= 0.001) state.dataHeatBal->Lights(Loop).FractionConvected = 0.0;
                    if (state.dataHeatBal->Lights(Loop).FractionConvected < 0.0) {
                        if (Item1 == 1) {
                            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", Sum of Fractions > 1.0");
                            ErrorsFound = true;
                        }
                    }

                    // Note: if FractionReturnAirIsCalculated = Yes and there is a return-air plenum:
                    // (1) The input values of FractionReturnAir, FractionRadiant and FractionShortWave, and the
                    // value of FractionConvected calculated from these are used in the zone sizing calculations;
                    // (2) in the regular calculation, FractionReturnAir is calculated each time step in
                    // Subr. InitInternalHeatGains as a function of the zone's return plenum air temperature
                    // using FractionReturnAirPlenTempCoeff1 and FractionReturnAirPlenTempCoeff2; then
                    // FractionRadiant and FractionConvected are adjusted from their input values such that
                    // FractionReturnAir + FractionRadiant + FractionShortWave + FractionConvected = 1.0, assuming
                    // FractionShortWave is constant and equal to its input value.

                    if (NumAlpha > 4) {
                        state.dataHeatBal->Lights(Loop).EndUseSubcategory = AlphaName(5);
                    } else {
                        state.dataHeatBal->Lights(Loop).EndUseSubcategory = "General";
                    }

                    if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                        state.dataHeatBal->Lights(Loop).FractionReturnAirIsCalculated = false;
                    } else if (AlphaName(6) != "YES" && AlphaName(6) != "NO") {
                        if (Item1 == 1) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                 state.dataIPShortCut->cAlphaFieldNames(6) + ", value  =" + AlphaName(6));
                            ShowContinueError(state, ".. Return Air Fraction from Plenum will NOT be calculated.");
                        }
                        state.dataHeatBal->Lights(Loop).FractionReturnAirIsCalculated = false;
                    } else {
                        state.dataHeatBal->Lights(Loop).FractionReturnAirIsCalculated = (AlphaName(6) == "YES");
                    }

                    // Set return air node number
                    state.dataHeatBal->Lights(Loop).ZoneReturnNum = 0;
                    std::string retNodeName = "";
                    if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                        if (state.dataHeatBal->LightsObjects(Item).ZoneListActive) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->Lights(Loop).Name +
                                                "\": " + state.dataIPShortCut->cAlphaFieldNames(7) + " must be blank when using a ZoneList.");
                            ErrorsFound = true;
                        } else {
                            retNodeName = AlphaName(7);
                        }
                    }
                    if (state.dataHeatBal->Lights(Loop).ZonePtr > 0) {
                        state.dataHeatBal->Lights(Loop).ZoneReturnNum = DataZoneEquipment::GetReturnNumForZone(
                            state, state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name, retNodeName);
                    }

                    if ((state.dataHeatBal->Lights(Loop).ZoneReturnNum == 0) && (state.dataHeatBal->Lights(Loop).FractionReturnAir > 0.0) &&
                        (!state.dataIPShortCut->lAlphaFieldBlanks(7))) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                            state.dataIPShortCut->cAlphaFieldNames(7) + " =" + AlphaName(7));
                        ShowContinueError(state, "No matching Zone Return Air Node found.");
                        ErrorsFound = true;
                    }
                    if (state.dataHeatBal->Lights(Loop).ZonePtr <= 0) continue; // Error, will be caught and terminated later

                    // Object report variables
                    SetupOutputVariable(state,
                                        "Lights Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->Lights(Loop).Power,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Lights(Loop).Name);

                    SetupOutputVariable(state,
                                        "Lights Radiant Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->Lights(Loop).RadGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Lights(Loop).Name);
                    SetupOutputVariable(state,
                                        "Lights Radiant Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->Lights(Loop).RadGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Lights(Loop).Name);
                    SetupOutputVariable(state,
                                        "Lights Visible Radiation Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->Lights(Loop).VisGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Lights(Loop).Name);

                    SetupOutputVariable(state,
                                        "Lights Visible Radiation Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->Lights(Loop).VisGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Lights(Loop).Name);
                    SetupOutputVariable(state,
                                        "Lights Convective Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->Lights(Loop).ConGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Lights(Loop).Name);
                    SetupOutputVariable(state,
                                        "Lights Convective Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->Lights(Loop).ConGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Lights(Loop).Name);
                    SetupOutputVariable(state,
                                        "Lights Return Air Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->Lights(Loop).RetAirGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Lights(Loop).Name);
                    SetupOutputVariable(state,
                                        "Lights Return Air Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->Lights(Loop).RetAirGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Lights(Loop).Name);
                    SetupOutputVariable(state,
                                        "Lights Total Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->Lights(Loop).TotGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Lights(Loop).Name);
                    SetupOutputVariable(state,
                                        "Lights Total Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->Lights(Loop).TotGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Lights(Loop).Name);
                    SetupOutputVariable(state,
                                        "Lights Electricity Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->Lights(Loop).Consumption,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Lights(Loop).Name,
                                        _,
                                        "Electricity",
                                        "InteriorLights",
                                        state.dataHeatBal->Lights(Loop).EndUseSubcategory,
                                        "Building",
                                        state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name,
                                        state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Multiplier,
                                        state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).ListMultiplier);

                    // Zone total report variables
                    if (RepVarSet(state.dataHeatBal->Lights(Loop).ZonePtr)) {
                        RepVarSet(state.dataHeatBal->Lights(Loop).ZonePtr) = false;
                        SetupOutputVariable(state,
                                            "Zone Lights Electricity Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsPower,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Electricity Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsElecConsump,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Radiant Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsRadGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Radiant Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsRadGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Visible Radiation Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsVisGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Visible Radiation Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsVisGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Convective Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsConGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Convective Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsConGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Return Air Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsRetAirGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Return Air Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsRetAirGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Total Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsTotGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Lights Total Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->Lights(Loop).ZonePtr).LtsTotGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).Name);
                    }

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "Lights",
                                         state.dataHeatBal->Lights(Loop).Name,
                                         "Electricity Rate",
                                         "[W]",
                                         state.dataHeatBal->Lights(Loop).EMSLightsOn,
                                         state.dataHeatBal->Lights(Loop).EMSLightingPower);
                        SetupEMSInternalVariable(state,
                                                 "Lighting Power Design Level",
                                                 state.dataHeatBal->Lights(Loop).Name,
                                                 "[W]",
                                                 state.dataHeatBal->Lights(Loop).DesignLevel);
                    } // EMS
                    // setup internal gains
                    int returnNodeNum = 0;
                    if ((state.dataHeatBal->Lights(Loop).ZoneReturnNum > 0) &&
                        (state.dataHeatBal->Lights(Loop).ZoneReturnNum <=
                         state.dataZoneEquip->ZoneEquipConfig(state.dataHeatBal->Lights(Loop).ZonePtr).NumReturnNodes)) {
                        returnNodeNum = state.dataZoneEquip->ZoneEquipConfig(state.dataHeatBal->Lights(Loop).ZonePtr)
                                            .ReturnNode(state.dataHeatBal->Lights(Loop).ZoneReturnNum);
                    }
                    if (!ErrorsFound)
                        SetupZoneInternalGain(state,
                                              state.dataHeatBal->Lights(Loop).ZonePtr,
                                              "Lights",
                                              state.dataHeatBal->Lights(Loop).Name,
                                              IntGainTypeOf_Lights,
                                              &state.dataHeatBal->Lights(Loop).ConGainRate,
                                              &state.dataHeatBal->Lights(Loop).RetAirGainRate,
                                              &state.dataHeatBal->Lights(Loop).RadGainRate,
                                              nullptr,
                                              nullptr,
                                              nullptr,
                                              nullptr,
                                              returnNodeNum);

                    if (state.dataHeatBal->Lights(Loop).FractionReturnAir > 0)
                        state.dataHeatBal->Zone(state.dataHeatBal->Lights(Loop).ZonePtr).HasLtsRetAirGain = true;
                    // send values to predefined lighting summary report
                    liteName = state.dataHeatBal->Lights(Loop).Name;
                    zonePt = state.dataHeatBal->Lights(Loop).ZonePtr;
                    mult = state.dataHeatBal->Zone(zonePt).Multiplier * state.dataHeatBal->Zone(zonePt).ListMultiplier;
                    state.dataInternalHeatGains->sumArea += state.dataHeatBal->Zone(zonePt).FloorArea * mult;
                    state.dataInternalHeatGains->sumPower += state.dataHeatBal->Lights(Loop).DesignLevel * mult;
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtZone, liteName, state.dataHeatBal->Zone(zonePt).Name);
                    if (state.dataHeatBal->Zone(zonePt).FloorArea > 0.0) {
                        PreDefTableEntry(state,
                                         state.dataOutRptPredefined->pdchInLtDens,
                                         liteName,
                                         state.dataHeatBal->Lights(Loop).DesignLevel / state.dataHeatBal->Zone(zonePt).FloorArea,
                                         4);
                    } else {
                        PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtDens, liteName, DataPrecisionGlobals::constant_zero, 4);
                    }
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtArea, liteName, state.dataHeatBal->Zone(zonePt).FloorArea * mult);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtPower, liteName, state.dataHeatBal->Lights(Loop).DesignLevel * mult);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtEndUse, liteName, state.dataHeatBal->Lights(Loop).EndUseSubcategory);
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchInLtSchd, liteName, GetScheduleName(state, state.dataHeatBal->Lights(Loop).SchedPtr));
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchInLtRetAir, liteName, state.dataHeatBal->Lights(Loop).FractionReturnAir, 4);
                } // Item1 - zones
            }     // Item = Number of Lights Objects
        }         // TotLights > 0 check
        // add total line to lighting summary table
        if (state.dataInternalHeatGains->sumArea > 0.0) {
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchInLtDens,
                             "Interior Lighting Total",
                             state.dataInternalHeatGains->sumPower / state.dataInternalHeatGains->sumArea,
                             4); //** line 792
        } else {
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtDens, "Interior Lighting Total", DataPrecisionGlobals::constant_zero, 4);
        }
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtArea, "Interior Lighting Total", state.dataInternalHeatGains->sumArea);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtPower, "Interior Lighting Total", state.dataInternalHeatGains->sumPower);

        RepVarSet = true;
        CurrentModuleObject = "ElectricEquipment";
        state.dataHeatBal->NumZoneElectricStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataHeatBal->ZoneElectricObjects.allocate(state.dataHeatBal->NumZoneElectricStatements);

        state.dataHeatBal->TotElecEquip = 0;
        errFlag = false;
        for (Item = 1; Item <= state.dataHeatBal->NumZoneElectricStatements; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Item,
                                                                     AlphaName,
                                                                     NumAlpha,
                                                                     IHGNumbers,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            errFlag = UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);

            state.dataHeatBal->ZoneElectricObjects(Item).Name = AlphaName(1);

            Item1 = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
            ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0)
                ZLItem = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                state.dataHeatBal->ZoneElectricObjects(Item).StartPtr = state.dataHeatBal->TotElecEquip + 1;
                ++state.dataHeatBal->TotElecEquip;
                state.dataHeatBal->ZoneElectricObjects(Item).NumOfZones = 1;
                state.dataHeatBal->ZoneElectricObjects(Item).ZoneListActive = false;
                state.dataHeatBal->ZoneElectricObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                state.dataHeatBal->ZoneElectricObjects(Item).StartPtr = state.dataHeatBal->TotElecEquip + 1;
                state.dataHeatBal->TotElecEquip += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->ZoneElectricObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->ZoneElectricObjects(Item).ZoneListActive = true;
                state.dataHeatBal->ZoneElectricObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + AlphaName(1) + "\" invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" +
                                    AlphaName(2) + "\" not found.");
                ErrorsFound = true;
                errFlag = true;
            }
        }

        if (errFlag) {
            ShowSevereError(state, std::string{RoutineName} + "Errors with invalid names in " + CurrentModuleObject + " objects.");
            ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
            state.dataHeatBal->TotElecEquip = 0;
        }

        state.dataHeatBal->ZoneElectric.allocate(state.dataHeatBal->TotElecEquip);

        if (state.dataHeatBal->TotElecEquip > 0) {
            Loop = 0;
            for (Item = 1; Item <= state.dataHeatBal->NumZoneElectricStatements; ++Item) {
                AlphaName = std::string{};
                IHGNumbers = 0.0;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         Item,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                for (Item1 = 1; Item1 <= state.dataHeatBal->ZoneElectricObjects(Item).NumOfZones; ++Item1) {
                    ++Loop;
                    if (!state.dataHeatBal->ZoneElectricObjects(Item).ZoneListActive) {
                        state.dataHeatBal->ZoneElectric(Loop).Name = AlphaName(1);
                        state.dataHeatBal->ZoneElectric(Loop).ZonePtr = state.dataHeatBal->ZoneElectricObjects(Item).ZoneOrZoneListPtr;
                    } else {
                        CheckCreatedZoneItemName(
                            state,
                            RoutineName,
                            CurrentModuleObject,
                            state.dataHeatBal
                                ->Zone(state.dataHeatBal->ZoneList(state.dataHeatBal->ZoneElectricObjects(Item).ZoneOrZoneListPtr).Zone(Item1))
                                .Name,
                            state.dataHeatBal->ZoneList(state.dataHeatBal->ZoneElectricObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                            state.dataHeatBal->ZoneElectricObjects(Item).Name,
                            state.dataHeatBal->ZoneElectric,
                            Loop - 1,
                            state.dataHeatBal->ZoneElectric(Loop).Name,
                            errFlag);
                        state.dataHeatBal->ZoneElectric(Loop).ZonePtr =
                            state.dataHeatBal->ZoneList(state.dataHeatBal->ZoneElectricObjects(Item).ZoneOrZoneListPtr).Zone(Item1);
                        if (errFlag) ErrorsFound = true;
                    }

                    state.dataHeatBal->ZoneElectric(Loop).SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (state.dataHeatBal->ZoneElectric(Loop).SchedPtr == 0) {
                        if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                        }
                        ErrorsFound = true;
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, state.dataHeatBal->ZoneElectric(Loop).SchedPtr);
                        SchMax = GetScheduleMaxValue(state, state.dataHeatBal->ZoneElectric(Loop).SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (SchMin < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                ErrorsFound = true;
                            }
                            if (SchMax < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Electric equipment design level calculation method.
                    {
                        auto const equipmentLevel(AlphaName(4));
                        if (equipmentLevel == "EQUIPMENTLEVEL") {
                            state.dataHeatBal->ZoneElectric(Loop).DesignLevel = IHGNumbers(1);
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 Electric Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/AREA") {
                            if (state.dataHeatBal->ZoneElectric(Loop).ZonePtr != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    state.dataHeatBal->ZoneElectric(Loop).DesignLevel =
                                        IHGNumbers(2) * state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).FloorArea;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).FloorArea <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Zone Floor Area = 0.  0 Electric Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           AlphaName(1),
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 Electric Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/PERSON") {
                            if (state.dataHeatBal->ZoneElectric(Loop).ZonePtr != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    state.dataHeatBal->ZoneElectric(Loop).DesignLevel =
                                        IHGNumbers(3) * state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).TotOccupants;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).TotOccupants <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Total Occupants = 0.  0 Electric Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           AlphaName(1),
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 Electric Equipment will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max equipment level
                    state.dataHeatBal->ZoneElectric(Loop).NomMinDesignLevel = state.dataHeatBal->ZoneElectric(Loop).DesignLevel * SchMin;
                    state.dataHeatBal->ZoneElectric(Loop).NomMaxDesignLevel = state.dataHeatBal->ZoneElectric(Loop).DesignLevel * SchMax;

                    state.dataHeatBal->ZoneElectric(Loop).FractionLatent = IHGNumbers(4);
                    state.dataHeatBal->ZoneElectric(Loop).FractionRadiant = IHGNumbers(5);
                    state.dataHeatBal->ZoneElectric(Loop).FractionLost = IHGNumbers(6);
                    // FractionConvected is a calculated field
                    state.dataHeatBal->ZoneElectric(Loop).FractionConvected =
                        1.0 - (state.dataHeatBal->ZoneElectric(Loop).FractionLatent + state.dataHeatBal->ZoneElectric(Loop).FractionRadiant +
                               state.dataHeatBal->ZoneElectric(Loop).FractionLost);
                    if (std::abs(state.dataHeatBal->ZoneElectric(Loop).FractionConvected) <= 0.001)
                        state.dataHeatBal->ZoneElectric(Loop).FractionConvected = 0.0;
                    if (state.dataHeatBal->ZoneElectric(Loop).FractionConvected < 0.0) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", Sum of Fractions > 1.0");
                        ErrorsFound = true;
                    }

                    if (NumAlpha > 4) {
                        state.dataHeatBal->ZoneElectric(Loop).EndUseSubcategory = AlphaName(5);
                    } else {
                        state.dataHeatBal->ZoneElectric(Loop).EndUseSubcategory = "General";
                    }

                    if (state.dataHeatBal->ZoneElectric(Loop).ZonePtr <= 0) continue; // Error, will be caught and terminated later

                    // Object report variables
                    SetupOutputVariable(state,
                                        "Electric Equipment Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneElectric(Loop).Power,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);
                    SetupOutputVariable(state,
                                        "Electric Equipment Electricity Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneElectric(Loop).Consumption,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneElectric(Loop).Name,
                                        _,
                                        "Electricity",
                                        "InteriorEquipment",
                                        state.dataHeatBal->ZoneElectric(Loop).EndUseSubcategory,
                                        "Building",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name,
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Multiplier,
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ListMultiplier);

                    SetupOutputVariable(state,
                                        "Electric Equipment Radiant Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneElectric(Loop).RadGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);
                    SetupOutputVariable(state,
                                        "Electric Equipment Radiant Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneElectric(Loop).RadGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);
                    SetupOutputVariable(state,
                                        "Electric Equipment Convective Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneElectric(Loop).ConGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);
                    SetupOutputVariable(state,
                                        "Electric Equipment Convective Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneElectric(Loop).ConGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);
                    SetupOutputVariable(state,
                                        "Electric Equipment Latent Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneElectric(Loop).LatGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);
                    SetupOutputVariable(state,
                                        "Electric Equipment Latent Gain Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneElectric(Loop).LatGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);
                    SetupOutputVariable(state,
                                        "Electric Equipment Lost Heat Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneElectric(Loop).LostEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);
                    SetupOutputVariable(state,
                                        "Electric Equipment Lost Heat Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneElectric(Loop).LostRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);
                    SetupOutputVariable(state,
                                        "Electric Equipment Total Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneElectric(Loop).TotGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);
                    SetupOutputVariable(state,
                                        "Electric Equipment Total Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneElectric(Loop).TotGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneElectric(Loop).Name);

                    // Zone total report variables
                    if (RepVarSet(state.dataHeatBal->ZoneElectric(Loop).ZonePtr)) {
                        RepVarSet(state.dataHeatBal->ZoneElectric(Loop).ZonePtr) = false;
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Electricity Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecPower,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Electricity Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecConsump,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);

                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Radiant Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecRadGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Radiant Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecRadGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Convective Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecConGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Convective Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecConGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Latent Gain Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecLatGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Latent Gain Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecLatGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Lost Heat Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecLost,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Lost Heat Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecLostRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Total Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecTotGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Electric Equipment Total Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).ElecTotGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(Loop).ZonePtr).Name);
                    }

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "ElectricEquipment",
                                         state.dataHeatBal->ZoneElectric(Loop).Name,
                                         "Electricity Rate",
                                         "[W]",
                                         state.dataHeatBal->ZoneElectric(Loop).EMSZoneEquipOverrideOn,
                                         state.dataHeatBal->ZoneElectric(Loop).EMSEquipPower);
                        SetupEMSInternalVariable(state,
                                                 "Plug and Process Power Design Level",
                                                 state.dataHeatBal->ZoneElectric(Loop).Name,
                                                 "[W]",
                                                 state.dataHeatBal->ZoneElectric(Loop).DesignLevel);
                    } // EMS

                    if (!ErrorsFound)
                        SetupZoneInternalGain(state,
                                              state.dataHeatBal->ZoneElectric(Loop).ZonePtr,
                                              "ElectricEquipment",
                                              state.dataHeatBal->ZoneElectric(Loop).Name,
                                              IntGainTypeOf_ElectricEquipment,
                                              &state.dataHeatBal->ZoneElectric(Loop).ConGainRate,
                                              nullptr,
                                              &state.dataHeatBal->ZoneElectric(Loop).RadGainRate,
                                              &state.dataHeatBal->ZoneElectric(Loop).LatGainRate);

                } // Item1
            }     // Item - Number of ZoneElectric objects
        }         // Check on number of ZoneElectric

        RepVarSet = true;
        CurrentModuleObject = "GasEquipment";
        state.dataHeatBal->NumZoneGasStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataHeatBal->ZoneGasObjects.allocate(state.dataHeatBal->NumZoneGasStatements);

        state.dataHeatBal->TotGasEquip = 0;
        errFlag = false;
        for (Item = 1; Item <= state.dataHeatBal->NumZoneGasStatements; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Item,
                                                                     AlphaName,
                                                                     NumAlpha,
                                                                     IHGNumbers,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            errFlag = UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);

            state.dataHeatBal->ZoneGasObjects(Item).Name = AlphaName(1);

            Item1 = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
            ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0)
                ZLItem = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                state.dataHeatBal->ZoneGasObjects(Item).StartPtr = state.dataHeatBal->TotGasEquip + 1;
                ++state.dataHeatBal->TotGasEquip;
                state.dataHeatBal->ZoneGasObjects(Item).NumOfZones = 1;
                state.dataHeatBal->ZoneGasObjects(Item).ZoneListActive = false;
                state.dataHeatBal->ZoneGasObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                state.dataHeatBal->ZoneGasObjects(Item).StartPtr = state.dataHeatBal->TotGasEquip + 1;
                state.dataHeatBal->TotGasEquip += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->ZoneGasObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->ZoneGasObjects(Item).ZoneListActive = true;
                state.dataHeatBal->ZoneGasObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + AlphaName(1) + "\" invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" +
                                    AlphaName(2) + "\" not found.");
                ErrorsFound = true;
                errFlag = true;
            }
        }

        if (errFlag) {
            ShowSevereError(state, std::string{RoutineName} + "Errors with invalid names in " + CurrentModuleObject + " objects.");
            ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
            state.dataHeatBal->TotGasEquip = 0;
        }

        state.dataHeatBal->ZoneGas.allocate(state.dataHeatBal->TotGasEquip);

        if (state.dataHeatBal->TotGasEquip > 0) {
            Loop = 0;
            for (Item = 1; Item <= state.dataHeatBal->NumZoneGasStatements; ++Item) {
                AlphaName = std::string{};
                IHGNumbers = 0.0;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         Item,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                for (Item1 = 1; Item1 <= state.dataHeatBal->ZoneGasObjects(Item).NumOfZones; ++Item1) {
                    ++Loop;
                    if (!state.dataHeatBal->ZoneGasObjects(Item).ZoneListActive) {
                        state.dataHeatBal->ZoneGas(Loop).Name = AlphaName(1);
                        state.dataHeatBal->ZoneGas(Loop).ZonePtr = state.dataHeatBal->ZoneGasObjects(Item).ZoneOrZoneListPtr;
                    } else {
                        CheckCreatedZoneItemName(
                            state,
                            RoutineName,
                            CurrentModuleObject,
                            state.dataHeatBal
                                ->Zone(state.dataHeatBal->ZoneList(state.dataHeatBal->ZoneGasObjects(Item).ZoneOrZoneListPtr).Zone(Item1))
                                .Name,
                            state.dataHeatBal->ZoneList(state.dataHeatBal->ZoneGasObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                            state.dataHeatBal->ZoneGasObjects(Item).Name,
                            state.dataHeatBal->ZoneGas,
                            Loop - 1,
                            state.dataHeatBal->ZoneGas(Loop).Name,
                            errFlag);
                        state.dataHeatBal->ZoneGas(Loop).ZonePtr =
                            state.dataHeatBal->ZoneList(state.dataHeatBal->ZoneGasObjects(Item).ZoneOrZoneListPtr).Zone(Item1);
                        if (errFlag) ErrorsFound = true;
                    }

                    state.dataHeatBal->ZoneGas(Loop).SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (state.dataHeatBal->ZoneGas(Loop).SchedPtr == 0) {
                        if (Item1 == 1) {
                            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                            } else {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                            }
                            ErrorsFound = true;
                        }
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, state.dataHeatBal->ZoneGas(Loop).SchedPtr);
                        SchMax = GetScheduleMaxValue(state, state.dataHeatBal->ZoneGas(Loop).SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (Item1 == 1) {
                                if (SchMin < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                    ErrorsFound = true;
                                }
                            }
                            if (Item1 == 1) {
                                if (SchMax < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }

                    // equipment design level calculation method.
                    {
                        auto const equipmentLevel(AlphaName(4));
                        if (equipmentLevel == "EQUIPMENTLEVEL") {
                            state.dataHeatBal->ZoneGas(Loop).DesignLevel = IHGNumbers(1);
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->ZoneGas(Loop).Name +
                                                     "\", specifies " + state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 Gas Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA") {
                            if (state.dataHeatBal->ZoneGas(Loop).ZonePtr != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    state.dataHeatBal->ZoneGas(Loop).DesignLevel =
                                        IHGNumbers(2) * state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).FloorArea;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).FloorArea <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->ZoneGas(Loop).Name +
                                                             "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Zone Floor Area = 0.  0 Gas Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           state.dataHeatBal->ZoneGas(Loop).Name,
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->ZoneGas(Loop).Name +
                                                     "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 Gas Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON") {
                            if (state.dataHeatBal->ZoneGas(Loop).ZonePtr != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    state.dataHeatBal->ZoneGas(Loop).DesignLevel =
                                        IHGNumbers(3) * state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).TotOccupants;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).TotOccupants <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->ZoneGas(Loop).Name +
                                                             "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Total Occupants = 0.  0 Gas Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           state.dataHeatBal->ZoneGas(Loop).Name,
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataHeatBal->ZoneGas(Loop).Name +
                                                     "\", specifies " + state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 Gas Equipment will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max equipment level
                    state.dataHeatBal->ZoneGas(Loop).NomMinDesignLevel = state.dataHeatBal->ZoneGas(Loop).DesignLevel * SchMin;
                    state.dataHeatBal->ZoneGas(Loop).NomMaxDesignLevel = state.dataHeatBal->ZoneGas(Loop).DesignLevel * SchMax;

                    state.dataHeatBal->ZoneGas(Loop).FractionLatent = IHGNumbers(4);
                    state.dataHeatBal->ZoneGas(Loop).FractionRadiant = IHGNumbers(5);
                    state.dataHeatBal->ZoneGas(Loop).FractionLost = IHGNumbers(6);

                    if ((NumNumber == 7) || (!state.dataIPShortCut->lNumericFieldBlanks(7))) {
                        state.dataHeatBal->ZoneGas(Loop).CO2RateFactor = IHGNumbers(7);
                    }
                    if (state.dataHeatBal->ZoneGas(Loop).CO2RateFactor < 0.0) {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", {} < 0.0, value ={:.2R}",
                                               RoutineName,
                                               CurrentModuleObject,
                                               AlphaName(1),
                                               state.dataIPShortCut->cNumericFieldNames(7),
                                               IHGNumbers(7)));
                        ErrorsFound = true;
                    }
                    if (state.dataHeatBal->ZoneGas(Loop).CO2RateFactor > 4.0e-7) {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", {} > 4.0E-7, value ={:.2R}",
                                               RoutineName,
                                               CurrentModuleObject,
                                               AlphaName(1),
                                               state.dataIPShortCut->cNumericFieldNames(7),
                                               IHGNumbers(7)));
                        ErrorsFound = true;
                    }
                    // FractionConvected is a calculated field
                    state.dataHeatBal->ZoneGas(Loop).FractionConvected =
                        1.0 - (state.dataHeatBal->ZoneGas(Loop).FractionLatent + state.dataHeatBal->ZoneGas(Loop).FractionRadiant +
                               state.dataHeatBal->ZoneGas(Loop).FractionLost);
                    if (std::abs(state.dataHeatBal->ZoneGas(Loop).FractionConvected) <= 0.001)
                        state.dataHeatBal->ZoneGas(Loop).FractionConvected = 0.0;
                    if (state.dataHeatBal->ZoneGas(Loop).FractionConvected < 0.0) {
                        if (Item1 == 1) {
                            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", Sum of Fractions > 1.0");
                            ErrorsFound = true;
                        }
                    }

                    if (NumAlpha > 4) {
                        state.dataHeatBal->ZoneGas(Loop).EndUseSubcategory = AlphaName(5);
                    } else {
                        state.dataHeatBal->ZoneGas(Loop).EndUseSubcategory = "General";
                    }

                    if (state.dataHeatBal->ZoneGas(Loop).ZonePtr <= 0) continue; // Error, will be caught and terminated later

                    // Object report variables
                    SetupOutputVariable(state,
                                        "Gas Equipment NaturalGas Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneGas(Loop).Power,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneGas(Loop).Name);
                    SetupOutputVariable(state,
                                        "Gas Equipment NaturalGas Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneGas(Loop).Consumption,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneGas(Loop).Name,
                                        _,
                                        "NaturalGas",
                                        "InteriorEquipment",
                                        state.dataHeatBal->ZoneGas(Loop).EndUseSubcategory,
                                        "Building",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name,
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Multiplier,
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).ListMultiplier);

                    SetupOutputVariable(state,
                                        "Gas Equipment Radiant Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneGas(Loop).RadGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneGas(Loop).Name);
                    SetupOutputVariable(state,
                                        "Gas Equipment Convective Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneGas(Loop).ConGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneGas(Loop).Name);
                    SetupOutputVariable(state,
                                        "Gas Equipment Latent Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneGas(Loop).LatGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneGas(Loop).Name);
                    SetupOutputVariable(state,
                                        "Gas Equipment Lost Heat Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneGas(Loop).LostEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneGas(Loop).Name);
                    SetupOutputVariable(state,
                                        "Gas Equipment Total Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneGas(Loop).TotGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneGas(Loop).Name);
                    SetupOutputVariable(state,
                                        "Gas Equipment Radiant Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneGas(Loop).RadGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneGas(Loop).Name);
                    SetupOutputVariable(state,
                                        "Gas Equipment Convective Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneGas(Loop).ConGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneGas(Loop).Name);
                    SetupOutputVariable(state,
                                        "Gas Equipment Latent Gain Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneGas(Loop).LatGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneGas(Loop).Name);
                    SetupOutputVariable(state,
                                        "Gas Equipment Lost Heat Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneGas(Loop).LostRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneGas(Loop).Name);
                    SetupOutputVariable(state,
                                        "Gas Equipment Total Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneGas(Loop).TotGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneGas(Loop).Name);

                    // Zone total report variables
                    if (RepVarSet(state.dataHeatBal->ZoneGas(Loop).ZonePtr)) {
                        RepVarSet(state.dataHeatBal->ZoneGas(Loop).ZonePtr) = false;

                        SetupOutputVariable(state,
                                            "Zone Gas Equipment NaturalGas Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasPower,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Gas Equipment NaturalGas Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasConsump,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);

                        SetupOutputVariable(state,
                                            "Zone Gas Equipment Radiant Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasRadGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Gas Equipment Radiant Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasRadGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Gas Equipment Convective Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasConGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Gas Equipment Convective Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasConGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Gas Equipment Latent Gain Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasLatGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Gas Equipment Latent Gain Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasLatGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Gas Equipment Lost Heat Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasLost,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Gas Equipment Lost Heat Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasLostRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Gas Equipment Total Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasTotGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Gas Equipment Total Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneGas(Loop).ZonePtr).GasTotGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(Loop).ZonePtr).Name);
                    }

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "GasEquipment",
                                         state.dataHeatBal->ZoneGas(Loop).Name,
                                         "NaturalGas Rate",
                                         "[W]",
                                         state.dataHeatBal->ZoneGas(Loop).EMSZoneEquipOverrideOn,
                                         state.dataHeatBal->ZoneGas(Loop).EMSEquipPower);
                        SetupEMSInternalVariable(state,
                                                 "Gas Process Power Design Level",
                                                 state.dataHeatBal->ZoneGas(Loop).Name,
                                                 "[W]",
                                                 state.dataHeatBal->ZoneGas(Loop).DesignLevel);
                    } // EMS

                    if (!ErrorsFound)
                        SetupZoneInternalGain(state,
                                              state.dataHeatBal->ZoneGas(Loop).ZonePtr,
                                              "GasEquipment",
                                              state.dataHeatBal->ZoneGas(Loop).Name,
                                              IntGainTypeOf_GasEquipment,
                                              &state.dataHeatBal->ZoneGas(Loop).ConGainRate,
                                              nullptr,
                                              &state.dataHeatBal->ZoneGas(Loop).RadGainRate,
                                              &state.dataHeatBal->ZoneGas(Loop).LatGainRate,
                                              nullptr,
                                              &state.dataHeatBal->ZoneGas(Loop).CO2GainRate);

                } // Item1
            }     // Item - number of gas statements
        }         // check for number of gas statements

        RepVarSet = true;
        CurrentModuleObject = "HotWaterEquipment";
        state.dataHeatBal->NumHotWaterEqStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataHeatBal->HotWaterEqObjects.allocate(state.dataHeatBal->NumHotWaterEqStatements);

        state.dataHeatBal->TotHWEquip = 0;
        errFlag = false;
        for (Item = 1; Item <= state.dataHeatBal->NumHotWaterEqStatements; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Item,
                                                                     AlphaName,
                                                                     NumAlpha,
                                                                     IHGNumbers,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            errFlag = UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);

            state.dataHeatBal->HotWaterEqObjects(Item).Name = AlphaName(1);

            Item1 = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
            ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0)
                ZLItem = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                state.dataHeatBal->HotWaterEqObjects(Item).StartPtr = state.dataHeatBal->TotHWEquip + 1;
                ++state.dataHeatBal->TotHWEquip;
                state.dataHeatBal->HotWaterEqObjects(Item).NumOfZones = 1;
                state.dataHeatBal->HotWaterEqObjects(Item).ZoneListActive = false;
                state.dataHeatBal->HotWaterEqObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                state.dataHeatBal->HotWaterEqObjects(Item).StartPtr = state.dataHeatBal->TotHWEquip + 1;
                state.dataHeatBal->TotHWEquip += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->HotWaterEqObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->HotWaterEqObjects(Item).ZoneListActive = true;
                state.dataHeatBal->HotWaterEqObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + AlphaName(1) + "\" invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" +
                                    AlphaName(2) + "\" not found.");
                ErrorsFound = true;
                errFlag = true;
            }
        }

        if (errFlag) {
            ShowSevereError(state, std::string{RoutineName} + "Errors with invalid names in " + CurrentModuleObject + " objects.");
            ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
            state.dataHeatBal->TotHWEquip = 0;
        }

        state.dataHeatBal->ZoneHWEq.allocate(state.dataHeatBal->TotHWEquip);

        if (state.dataHeatBal->TotHWEquip > 0) {
            Loop = 0;
            for (Item = 1; Item <= state.dataHeatBal->NumHotWaterEqStatements; ++Item) {
                AlphaName = std::string{};
                IHGNumbers = 0.0;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         Item,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                for (Item1 = 1; Item1 <= state.dataHeatBal->HotWaterEqObjects(Item).NumOfZones; ++Item1) {
                    ++Loop;
                    if (!state.dataHeatBal->HotWaterEqObjects(Item).ZoneListActive) {
                        state.dataHeatBal->ZoneHWEq(Loop).Name = AlphaName(1);
                        state.dataHeatBal->ZoneHWEq(Loop).ZonePtr = state.dataHeatBal->HotWaterEqObjects(Item).ZoneOrZoneListPtr;
                    } else {
                        CheckCreatedZoneItemName(
                            state,
                            RoutineName,
                            CurrentModuleObject,
                            state.dataHeatBal
                                ->Zone(state.dataHeatBal->ZoneList(state.dataHeatBal->HotWaterEqObjects(Item).ZoneOrZoneListPtr).Zone(Item1))
                                .Name,
                            state.dataHeatBal->ZoneList(state.dataHeatBal->HotWaterEqObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                            state.dataHeatBal->HotWaterEqObjects(Item).Name,
                            state.dataHeatBal->ZoneHWEq,
                            Loop - 1,
                            state.dataHeatBal->ZoneHWEq(Loop).Name,
                            errFlag);
                        state.dataHeatBal->ZoneHWEq(Loop).ZonePtr =
                            state.dataHeatBal->ZoneList(state.dataHeatBal->HotWaterEqObjects(Item).ZoneOrZoneListPtr).Zone(Item1);
                        if (errFlag) ErrorsFound = true;
                    }

                    state.dataHeatBal->ZoneHWEq(Loop).SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (state.dataHeatBal->ZoneHWEq(Loop).SchedPtr == 0) {
                        if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                        }
                        ErrorsFound = true;
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, state.dataHeatBal->ZoneHWEq(Loop).SchedPtr);
                        SchMax = GetScheduleMaxValue(state, state.dataHeatBal->ZoneHWEq(Loop).SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (SchMin < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                ErrorsFound = true;
                            }
                            if (SchMax < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Hot Water equipment design level calculation method.
                    {
                        auto const equipmentLevel(AlphaName(4));
                        if (equipmentLevel == "EQUIPMENTLEVEL") {
                            state.dataHeatBal->ZoneHWEq(Loop).DesignLevel = IHGNumbers(1);
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 Hot Water Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA") {
                            if (state.dataHeatBal->ZoneHWEq(Loop).ZonePtr != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    state.dataHeatBal->ZoneHWEq(Loop).DesignLevel =
                                        IHGNumbers(2) * state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).FloorArea;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).FloorArea <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Zone Floor Area = 0.  0 Hot Water Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           AlphaName(1),
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 Hot Water Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON") {
                            if (state.dataHeatBal->ZoneHWEq(Loop).ZonePtr != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    state.dataHeatBal->ZoneHWEq(Loop).DesignLevel =
                                        IHGNumbers(3) * state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).TotOccupants;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).TotOccupants <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Total Occupants = 0.  0 Hot Water Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           AlphaName(1),
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 Hot Water Equipment will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max equipment level
                    state.dataHeatBal->ZoneHWEq(Loop).NomMinDesignLevel = state.dataHeatBal->ZoneHWEq(Loop).DesignLevel * SchMin;
                    state.dataHeatBal->ZoneHWEq(Loop).NomMaxDesignLevel = state.dataHeatBal->ZoneHWEq(Loop).DesignLevel * SchMax;

                    state.dataHeatBal->ZoneHWEq(Loop).FractionLatent = IHGNumbers(4);
                    state.dataHeatBal->ZoneHWEq(Loop).FractionRadiant = IHGNumbers(5);
                    state.dataHeatBal->ZoneHWEq(Loop).FractionLost = IHGNumbers(6);
                    // FractionConvected is a calculated field
                    state.dataHeatBal->ZoneHWEq(Loop).FractionConvected =
                        1.0 - (state.dataHeatBal->ZoneHWEq(Loop).FractionLatent + state.dataHeatBal->ZoneHWEq(Loop).FractionRadiant +
                               state.dataHeatBal->ZoneHWEq(Loop).FractionLost);
                    if (std::abs(state.dataHeatBal->ZoneHWEq(Loop).FractionConvected) <= 0.001)
                        state.dataHeatBal->ZoneHWEq(Loop).FractionConvected = 0.0;
                    if (state.dataHeatBal->ZoneHWEq(Loop).FractionConvected < 0.0) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", Sum of Fractions > 1.0");
                        ErrorsFound = true;
                    }

                    if (NumAlpha > 4) {
                        state.dataHeatBal->ZoneHWEq(Loop).EndUseSubcategory = AlphaName(5);
                    } else {
                        state.dataHeatBal->ZoneHWEq(Loop).EndUseSubcategory = "General";
                    }

                    if (state.dataHeatBal->ZoneHWEq(Loop).ZonePtr <= 0) continue; // Error, will be caught and terminated later

                    // Object report variables
                    SetupOutputVariable(state,
                                        "Hot Water Equipment District Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneHWEq(Loop).Power,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Hot Water Equipment District Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneHWEq(Loop).Consumption,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name,
                                        _,
                                        "DistrictHeating",
                                        "InteriorEquipment",
                                        state.dataHeatBal->ZoneHWEq(Loop).EndUseSubcategory,
                                        "Building",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name,
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Multiplier,
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).ListMultiplier);

                    SetupOutputVariable(state,
                                        "Hot Water Equipment Radiant Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneHWEq(Loop).RadGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Hot Water Equipment Radiant Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneHWEq(Loop).RadGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Hot Water Equipment Convective Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneHWEq(Loop).ConGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Hot Water Equipment Convective Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneHWEq(Loop).ConGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Hot Water Equipment Latent Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneHWEq(Loop).LatGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Hot Water Equipment Latent Gain Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneHWEq(Loop).LatGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Hot Water Equipment Lost Heat Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneHWEq(Loop).LostEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Hot Water Equipment Lost Heat Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneHWEq(Loop).LostRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Hot Water Equipment Total Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneHWEq(Loop).TotGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Hot Water Equipment Total Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneHWEq(Loop).TotGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneHWEq(Loop).Name);

                    // Zone total report variables
                    if (RepVarSet(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr)) {
                        RepVarSet(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr) = false;
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment District Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWPower,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment District Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWConsump,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);

                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment Radiant Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWRadGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment Radiant Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWRadGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment Convective Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWConGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment Convective Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWConGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment Latent Gain Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWLatGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment Latent Gain Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWLatGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment Lost Heat Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWLost,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment Lost Heat Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWLostRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment Total Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWTotGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Hot Water Equipment Total Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).HWTotGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(Loop).ZonePtr).Name);
                    }

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "HotWaterEquipment",
                                         state.dataHeatBal->ZoneHWEq(Loop).Name,
                                         "District Heating Power Level",
                                         "[W]",
                                         state.dataHeatBal->ZoneHWEq(Loop).EMSZoneEquipOverrideOn,
                                         state.dataHeatBal->ZoneHWEq(Loop).EMSEquipPower);
                        SetupEMSInternalVariable(state,
                                                 "Process District Heat Design Level",
                                                 state.dataHeatBal->ZoneHWEq(Loop).Name,
                                                 "[W]",
                                                 state.dataHeatBal->ZoneHWEq(Loop).DesignLevel);
                    } // EMS

                    if (!ErrorsFound)
                        SetupZoneInternalGain(state,
                                              state.dataHeatBal->ZoneHWEq(Loop).ZonePtr,
                                              "HotWaterEquipment",
                                              state.dataHeatBal->ZoneHWEq(Loop).Name,
                                              IntGainTypeOf_HotWaterEquipment,
                                              &state.dataHeatBal->ZoneHWEq(Loop).ConGainRate,
                                              nullptr,
                                              &state.dataHeatBal->ZoneHWEq(Loop).RadGainRate,
                                              &state.dataHeatBal->ZoneHWEq(Loop).LatGainRate);

                } // Item1
            }     // Item - number of hot water statements
        }

        RepVarSet = true;
        CurrentModuleObject = "SteamEquipment";
        state.dataHeatBal->NumSteamEqStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataHeatBal->SteamEqObjects.allocate(state.dataHeatBal->NumSteamEqStatements);

        state.dataHeatBal->TotStmEquip = 0;
        errFlag = false;
        for (Item = 1; Item <= state.dataHeatBal->NumSteamEqStatements; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Item,
                                                                     AlphaName,
                                                                     NumAlpha,
                                                                     IHGNumbers,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            errFlag = UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);

            state.dataHeatBal->SteamEqObjects(Item).Name = AlphaName(1);

            Item1 = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
            ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0)
                ZLItem = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                state.dataHeatBal->SteamEqObjects(Item).StartPtr = state.dataHeatBal->TotStmEquip + 1;
                ++state.dataHeatBal->TotStmEquip;
                state.dataHeatBal->SteamEqObjects(Item).NumOfZones = 1;
                state.dataHeatBal->SteamEqObjects(Item).ZoneListActive = false;
                state.dataHeatBal->SteamEqObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                state.dataHeatBal->SteamEqObjects(Item).StartPtr = state.dataHeatBal->TotStmEquip + 1;
                state.dataHeatBal->TotStmEquip += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->SteamEqObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->SteamEqObjects(Item).ZoneListActive = true;
                state.dataHeatBal->SteamEqObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + AlphaName(1) + "\" invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" +
                                    AlphaName(2) + "\" not found.");
                ErrorsFound = true;
                errFlag = true;
            }
        }

        if (errFlag) {
            ShowSevereError(state, std::string{RoutineName} + "Errors with invalid names in " + CurrentModuleObject + " objects.");
            ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
            state.dataHeatBal->TotStmEquip = 0;
        }

        state.dataHeatBal->ZoneSteamEq.allocate(state.dataHeatBal->TotStmEquip);

        if (state.dataHeatBal->TotStmEquip > 0) {
            Loop = 0;
            for (Item = 1; Item <= state.dataHeatBal->NumSteamEqStatements; ++Item) {
                AlphaName = std::string{};
                IHGNumbers = 0.0;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         Item,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                for (Item1 = 1; Item1 <= state.dataHeatBal->SteamEqObjects(Item).NumOfZones; ++Item1) {
                    ++Loop;
                    if (!state.dataHeatBal->SteamEqObjects(Item).ZoneListActive) {
                        state.dataHeatBal->ZoneSteamEq(Loop).Name = AlphaName(1);
                        state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr = state.dataHeatBal->SteamEqObjects(Item).ZoneOrZoneListPtr;
                    } else {
                        CheckCreatedZoneItemName(
                            state,
                            RoutineName,
                            CurrentModuleObject,
                            state.dataHeatBal
                                ->Zone(state.dataHeatBal->ZoneList(state.dataHeatBal->SteamEqObjects(Item).ZoneOrZoneListPtr).Zone(Item1))
                                .Name,
                            state.dataHeatBal->ZoneList(state.dataHeatBal->SteamEqObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                            state.dataHeatBal->SteamEqObjects(Item).Name,
                            state.dataHeatBal->ZoneSteamEq,
                            Loop - 1,
                            state.dataHeatBal->ZoneSteamEq(Loop).Name,
                            errFlag);
                        state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr =
                            state.dataHeatBal->ZoneList(state.dataHeatBal->SteamEqObjects(Item).ZoneOrZoneListPtr).Zone(Item1);
                        if (errFlag) ErrorsFound = true;
                    }

                    state.dataHeatBal->ZoneSteamEq(Loop).SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (state.dataHeatBal->ZoneSteamEq(Loop).SchedPtr == 0) {
                        if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                        }
                        ErrorsFound = true;
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, state.dataHeatBal->ZoneSteamEq(Loop).SchedPtr);
                        SchMax = GetScheduleMaxValue(state, state.dataHeatBal->ZoneSteamEq(Loop).SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (SchMin < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                ErrorsFound = true;
                            }
                            if (SchMax < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Hot Water equipment design level calculation method.
                    {
                        auto const equipmentLevel(AlphaName(4));
                        if (equipmentLevel == "EQUIPMENTLEVEL") {
                            state.dataHeatBal->ZoneSteamEq(Loop).DesignLevel = IHGNumbers(1);
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 Hot Water Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA") {
                            if (state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    state.dataHeatBal->ZoneSteamEq(Loop).DesignLevel =
                                        IHGNumbers(2) * state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).FloorArea;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).FloorArea <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Zone Floor Area = 0.  0 Hot Water Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           AlphaName(1),
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 Hot Water Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON") {
                            if (state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    state.dataHeatBal->ZoneSteamEq(Loop).DesignLevel =
                                        IHGNumbers(3) * state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).TotOccupants;
                                    if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).TotOccupants <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Total Occupants = 0.  0 Hot Water Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           CurrentModuleObject,
                                                           AlphaName(1),
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 Hot Water Equipment will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max equipment level
                    state.dataHeatBal->ZoneSteamEq(Loop).NomMinDesignLevel = state.dataHeatBal->ZoneSteamEq(Loop).DesignLevel * SchMin;
                    state.dataHeatBal->ZoneSteamEq(Loop).NomMaxDesignLevel = state.dataHeatBal->ZoneSteamEq(Loop).DesignLevel * SchMax;

                    state.dataHeatBal->ZoneSteamEq(Loop).FractionLatent = IHGNumbers(4);
                    state.dataHeatBal->ZoneSteamEq(Loop).FractionRadiant = IHGNumbers(5);
                    state.dataHeatBal->ZoneSteamEq(Loop).FractionLost = IHGNumbers(6);
                    // FractionConvected is a calculated field
                    state.dataHeatBal->ZoneSteamEq(Loop).FractionConvected =
                        1.0 - (state.dataHeatBal->ZoneSteamEq(Loop).FractionLatent + state.dataHeatBal->ZoneSteamEq(Loop).FractionRadiant +
                               state.dataHeatBal->ZoneSteamEq(Loop).FractionLost);
                    if (std::abs(state.dataHeatBal->ZoneSteamEq(Loop).FractionConvected) <= 0.001)
                        state.dataHeatBal->ZoneSteamEq(Loop).FractionConvected = 0.0;
                    if (state.dataHeatBal->ZoneSteamEq(Loop).FractionConvected < 0.0) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", Sum of Fractions > 1.0");
                        ErrorsFound = true;
                    }

                    if (NumAlpha > 4) {
                        state.dataHeatBal->ZoneSteamEq(Loop).EndUseSubcategory = AlphaName(5);
                    } else {
                        state.dataHeatBal->ZoneSteamEq(Loop).EndUseSubcategory = "General";
                    }

                    if (state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr <= 0) continue; // Error, will be caught and terminated later

                    // Object report variables
                    SetupOutputVariable(state,
                                        "Steam Equipment District Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneSteamEq(Loop).Power,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Steam Equipment District Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneSteamEq(Loop).Consumption,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name,
                                        _,
                                        "DistrictHeating",
                                        "InteriorEquipment",
                                        state.dataHeatBal->ZoneSteamEq(Loop).EndUseSubcategory,
                                        "Building",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name,
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Multiplier,
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).ListMultiplier);

                    SetupOutputVariable(state,
                                        "Steam Equipment Radiant Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneSteamEq(Loop).RadGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Steam Equipment Radiant Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneSteamEq(Loop).RadGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Steam Equipment Convective Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneSteamEq(Loop).ConGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Steam Equipment Convective Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneSteamEq(Loop).ConGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Steam Equipment Latent Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneSteamEq(Loop).LatGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Steam Equipment Latent Gain Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneSteamEq(Loop).LatGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Steam Equipment Lost Heat Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneSteamEq(Loop).LostEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Steam Equipment Lost Heat Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneSteamEq(Loop).LostRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Steam Equipment Total Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneSteamEq(Loop).TotGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Steam Equipment Total Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneSteamEq(Loop).TotGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneSteamEq(Loop).Name);

                    // Zone total report variables
                    if (RepVarSet(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr)) {
                        RepVarSet(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr) = false;
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment District Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamPower,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment District Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamConsump,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);

                        SetupOutputVariable(state,
                                            "Zone Steam Equipment Radiant Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamRadGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment Radiant Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamRadGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment Convective Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamConGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment Convective Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamConGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment Latent Gain Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamLatGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment Latent Gain Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamLatGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment Lost Heat Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamLost,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment Lost Heat Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamLostRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment Total Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamTotGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Steam Equipment Total Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).SteamTotGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr).Name);
                    }

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "SteamEquipment",
                                         state.dataHeatBal->ZoneSteamEq(Loop).Name,
                                         "District Heating Power Level",
                                         "[W]",
                                         state.dataHeatBal->ZoneSteamEq(Loop).EMSZoneEquipOverrideOn,
                                         state.dataHeatBal->ZoneSteamEq(Loop).EMSEquipPower);
                        SetupEMSInternalVariable(state,
                                                 "Process Steam District Heat Design Level",
                                                 state.dataHeatBal->ZoneSteamEq(Loop).Name,
                                                 "[W]",
                                                 state.dataHeatBal->ZoneSteamEq(Loop).DesignLevel);
                    } // EMS

                    if (!ErrorsFound)
                        SetupZoneInternalGain(state,
                                              state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr,
                                              "SteamEquipment",
                                              state.dataHeatBal->ZoneSteamEq(Loop).Name,
                                              IntGainTypeOf_SteamEquipment,
                                              &state.dataHeatBal->ZoneSteamEq(Loop).ConGainRate,
                                              nullptr,
                                              &state.dataHeatBal->ZoneSteamEq(Loop).RadGainRate,
                                              &state.dataHeatBal->ZoneSteamEq(Loop).LatGainRate);

                } // Item1
            }     // Item - number of hot water statements
        }

        RepVarSet = true;
        CurrentModuleObject = "OtherEquipment";
        state.dataHeatBal->NumOtherEqStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataHeatBal->OtherEqObjects.allocate(state.dataHeatBal->NumOtherEqStatements);

        state.dataHeatBal->TotOthEquip = 0;
        errFlag = false;
        for (Item = 1; Item <= state.dataHeatBal->NumOtherEqStatements; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Item,
                                                                     AlphaName,
                                                                     NumAlpha,
                                                                     IHGNumbers,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            errFlag = UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);

            state.dataHeatBal->OtherEqObjects(Item).Name = AlphaName(1);

            Item1 = UtilityRoutines::FindItemInList(AlphaName(3), state.dataHeatBal->Zone);
            ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0)
                ZLItem = UtilityRoutines::FindItemInList(AlphaName(3), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                state.dataHeatBal->OtherEqObjects(Item).StartPtr = state.dataHeatBal->TotOthEquip + 1;
                ++state.dataHeatBal->TotOthEquip;
                state.dataHeatBal->OtherEqObjects(Item).NumOfZones = 1;
                state.dataHeatBal->OtherEqObjects(Item).ZoneListActive = false;
                state.dataHeatBal->OtherEqObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                state.dataHeatBal->OtherEqObjects(Item).StartPtr = state.dataHeatBal->TotOthEquip + 1;
                state.dataHeatBal->TotOthEquip += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->OtherEqObjects(Item).NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                state.dataHeatBal->OtherEqObjects(Item).ZoneListActive = true;
                state.dataHeatBal->OtherEqObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + AlphaName(1) + "\" invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" +
                                    AlphaName(3) + "\" not found.");
                ErrorsFound = true;
                errFlag = true;
            }
        }

        if (errFlag) {
            ShowSevereError(state, std::string{RoutineName} + "Errors with invalid names in " + CurrentModuleObject + " objects.");
            ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
            state.dataHeatBal->TotOthEquip = 0;
        }

        state.dataHeatBal->ZoneOtherEq.allocate(state.dataHeatBal->TotOthEquip);

        if (state.dataHeatBal->TotOthEquip > 0) {
            Loop = 0;
            for (Item = 1; Item <= state.dataHeatBal->NumOtherEqStatements; ++Item) {
                AlphaName = std::string{};
                IHGNumbers = 0.0;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         Item,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                for (Item1 = 1; Item1 <= state.dataHeatBal->OtherEqObjects(Item).NumOfZones; ++Item1) {
                    ++Loop;
                    if (!state.dataHeatBal->OtherEqObjects(Item).ZoneListActive) {
                        state.dataHeatBal->ZoneOtherEq(Loop).Name = AlphaName(1);
                        state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr = state.dataHeatBal->OtherEqObjects(Item).ZoneOrZoneListPtr;
                    } else {
                        CheckCreatedZoneItemName(
                            state,
                            RoutineName,
                            CurrentModuleObject,
                            state.dataHeatBal
                                ->Zone(state.dataHeatBal->ZoneList(state.dataHeatBal->OtherEqObjects(Item).ZoneOrZoneListPtr).Zone(Item1))
                                .Name,
                            state.dataHeatBal->ZoneList(state.dataHeatBal->OtherEqObjects(Item).ZoneOrZoneListPtr).MaxZoneNameLength,
                            state.dataHeatBal->OtherEqObjects(Item).Name,
                            state.dataHeatBal->ZoneOtherEq,
                            Loop - 1,
                            state.dataHeatBal->ZoneOtherEq(Loop).Name,
                            errFlag);
                        state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr =
                            state.dataHeatBal->ZoneList(state.dataHeatBal->OtherEqObjects(Item).ZoneOrZoneListPtr).Zone(Item1);
                        if (errFlag) ErrorsFound = true;
                    }

                    std::string FuelTypeString("");
                    if (AlphaName(2) == "NONE") {
                        state.dataHeatBal->ZoneOtherEq(Loop).OtherEquipFuelType = ExteriorEnergyUse::ExteriorFuelUsage::Unknown;
                        FuelTypeString = AlphaName(2);
                    } else {
                        ExteriorEnergyUse::ValidateFuelType(state,
                                                            state.dataHeatBal->ZoneOtherEq(Loop).OtherEquipFuelType,
                                                            AlphaName(2),
                                                            FuelTypeString,
                                                            CurrentModuleObject,
                                                            state.dataIPShortCut->cAlphaFieldNames(2),
                                                            AlphaName(2));
                        if (state.dataHeatBal->ZoneOtherEq(Loop).OtherEquipFuelType == ExteriorEnergyUse::ExteriorFuelUsage::Unknown ||
                            state.dataHeatBal->ZoneOtherEq(Loop).OtherEquipFuelType == ExteriorEnergyUse::ExteriorFuelUsage::WaterUse) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                                " entered=" + AlphaName(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                                AlphaName(1));
                            ErrorsFound = true;
                        }
                    }

                    state.dataHeatBal->ZoneOtherEq(Loop).SchedPtr = GetScheduleIndex(state, AlphaName(4));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (state.dataHeatBal->ZoneOtherEq(Loop).SchedPtr == 0) {
                        if (state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(4) + " is required.");
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(4) + " entered=" + AlphaName(4));
                        }
                        ErrorsFound = true;
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, state.dataHeatBal->ZoneOtherEq(Loop).SchedPtr);
                        SchMax = GetScheduleMaxValue(state, state.dataHeatBal->ZoneOtherEq(Loop).SchedPtr);
                    }

                    // equipment design level calculation method.
                    unsigned int DesignLevelFieldNumber;
                    {
                        auto const equipmentLevel(AlphaName(5));
                        if (equipmentLevel == "EQUIPMENTLEVEL") {
                            DesignLevelFieldNumber = 1;
                            state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel = IHGNumbers(DesignLevelFieldNumber);
                            if (state.dataIPShortCut->lNumericFieldBlanks(DesignLevelFieldNumber)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) +
                                                     ", but that field is blank.  0 Other Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA") {
                            DesignLevelFieldNumber = 2;
                            if (state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr != 0) {
                                state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel =
                                    IHGNumbers(DesignLevelFieldNumber) *
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).FloorArea;
                                if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).FloorArea <= 0.0) {
                                    ShowWarningError(state,
                                                     std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                         state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) +
                                                         ", but Zone Floor Area = 0.  0 Other Equipment will result.");
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(DesignLevelFieldNumber)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) +
                                                     ", but that field is blank.  0 Other Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON") {
                            DesignLevelFieldNumber = 3;
                            if (state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr != 0) {
                                state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel =
                                    IHGNumbers(3) * state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).TotOccupants;
                                if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).TotOccupants <= 0.0) {
                                    ShowWarningError(state,
                                                     std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                         state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) +
                                                         ", but Total Occupants = 0.  0 Other Equipment will result.");
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(DesignLevelFieldNumber)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) +
                                                     ", but that field is blank.  0 Other Equipment will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(5) + ", value  =" + AlphaName(5));
                                ShowContinueError(state, "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Throw an error if the design level is negative and we have a fuel type
                    if (state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel < 0.0 &&
                        state.dataHeatBal->ZoneOtherEq(Loop).OtherEquipFuelType != ExteriorEnergyUse::ExteriorFuelUsage::Unknown) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                            state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) + " is not allowed to be negative");
                        ShowContinueError(state, "... when a fuel type of " + FuelTypeString + " is specified.");
                        ErrorsFound = true;
                    }

                    // Calculate nominal min/max equipment level
                    state.dataHeatBal->ZoneOtherEq(Loop).NomMinDesignLevel = state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel * SchMin;
                    state.dataHeatBal->ZoneOtherEq(Loop).NomMaxDesignLevel = state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel * SchMax;

                    state.dataHeatBal->ZoneOtherEq(Loop).FractionLatent = IHGNumbers(4);
                    state.dataHeatBal->ZoneOtherEq(Loop).FractionRadiant = IHGNumbers(5);
                    state.dataHeatBal->ZoneOtherEq(Loop).FractionLost = IHGNumbers(6);

                    if ((NumNumber == 7) || (!state.dataIPShortCut->lNumericFieldBlanks(7))) {
                        state.dataHeatBal->ZoneOtherEq(Loop).CO2RateFactor = IHGNumbers(7);
                    }
                    if (state.dataHeatBal->ZoneOtherEq(Loop).CO2RateFactor < 0.0) {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", {} < 0.0, value ={:.2R}",
                                               RoutineName,
                                               CurrentModuleObject,
                                               AlphaName(1),
                                               state.dataIPShortCut->cNumericFieldNames(7),
                                               IHGNumbers(7)));
                        ErrorsFound = true;
                    }
                    if (state.dataHeatBal->ZoneOtherEq(Loop).CO2RateFactor > 4.0e-7) {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", {} > 4.0E-7, value ={:.2R}",
                                               RoutineName,
                                               CurrentModuleObject,
                                               AlphaName(1),
                                               state.dataIPShortCut->cNumericFieldNames(7),
                                               IHGNumbers(7)));
                        ErrorsFound = true;
                    }

                    // FractionConvected is a calculated field
                    state.dataHeatBal->ZoneOtherEq(Loop).FractionConvected =
                        1.0 - (state.dataHeatBal->ZoneOtherEq(Loop).FractionLatent + state.dataHeatBal->ZoneOtherEq(Loop).FractionRadiant +
                               state.dataHeatBal->ZoneOtherEq(Loop).FractionLost);
                    if (std::abs(state.dataHeatBal->ZoneOtherEq(Loop).FractionConvected) <= 0.001)
                        state.dataHeatBal->ZoneOtherEq(Loop).FractionConvected = 0.0;
                    if (state.dataHeatBal->ZoneOtherEq(Loop).FractionConvected < 0.0) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", Sum of Fractions > 1.0");
                        ErrorsFound = true;
                    }

                    if (NumAlpha > 5) {
                        state.dataHeatBal->ZoneOtherEq(Loop).EndUseSubcategory = AlphaName(6);
                    } else {
                        state.dataHeatBal->ZoneOtherEq(Loop).EndUseSubcategory = "General";
                    }

                    if (state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr <= 0) continue; // Error, will be caught and terminated later

                    // Object report variables
                    if (state.dataHeatBal->ZoneOtherEq(Loop).OtherEquipFuelType != ExteriorEnergyUse::ExteriorFuelUsage::Unknown) {
                        SetupOutputVariable(state,
                                            "Other Equipment " + FuelTypeString + " Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZoneOtherEq(Loop).Power,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->ZoneOtherEq(Loop).Name);
                        SetupOutputVariable(state,
                                            "Other Equipment " + FuelTypeString + " Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZoneOtherEq(Loop).Consumption,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->ZoneOtherEq(Loop).Name,
                                            _,
                                            FuelTypeString,
                                            "InteriorEquipment",
                                            state.dataHeatBal->ZoneOtherEq(Loop).EndUseSubcategory,
                                            "Building",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name,
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Multiplier,
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).ListMultiplier);
                    }

                    SetupOutputVariable(state,
                                        "Other Equipment Radiant Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneOtherEq(Loop).RadGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneOtherEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Other Equipment Radiant Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneOtherEq(Loop).RadGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneOtherEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Other Equipment Convective Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneOtherEq(Loop).ConGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneOtherEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Other Equipment Convective Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneOtherEq(Loop).ConGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneOtherEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Other Equipment Latent Gain Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneOtherEq(Loop).LatGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneOtherEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Other Equipment Latent Gain Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneOtherEq(Loop).LatGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneOtherEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Other Equipment Lost Heat Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneOtherEq(Loop).LostEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneOtherEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Other Equipment Lost Heat Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneOtherEq(Loop).LostRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneOtherEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Other Equipment Total Heating Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZoneOtherEq(Loop).TotGainEnergy,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->ZoneOtherEq(Loop).Name);
                    SetupOutputVariable(state,
                                        "Other Equipment Total Heating Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZoneOtherEq(Loop).TotGainRate,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneOtherEq(Loop).Name);

                    // Zone total report variables
                    if (RepVarSet(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr)) {
                        RepVarSet(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr) = false;

                        if (state.dataHeatBal->ZoneOtherEq(Loop).OtherEquipFuelType != ExteriorEnergyUse::ExteriorFuelUsage::Unknown) {
                            SetupOutputVariable(state,
                                                "Zone Other Equipment " + FuelTypeString + " Rate",
                                                OutputProcessor::Unit::W,
                                                state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherPower,
                                                "Zone",
                                                "Average",
                                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                            SetupOutputVariable(state,
                                                "Zone Other Equipment " + FuelTypeString + " Energy",
                                                OutputProcessor::Unit::J,
                                                state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherConsump,
                                                "Zone",
                                                "Sum",
                                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                        }

                        SetupOutputVariable(state,
                                            "Zone Other Equipment Radiant Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherRadGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Other Equipment Radiant Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherRadGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Other Equipment Convective Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherConGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Other Equipment Convective Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherConGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Other Equipment Latent Gain Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherLatGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Other Equipment Latent Gain Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherLatGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Other Equipment Lost Heat Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherLost,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Other Equipment Lost Heat Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherLostRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Other Equipment Total Heating Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherTotGain,
                                            "Zone",
                                            "Sum",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                        SetupOutputVariable(state,
                                            "Zone Other Equipment Total Heating Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).OtherTotGainRate,
                                            "Zone",
                                            "Average",
                                            state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr).Name);
                    }
                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "OtherEquipment",
                                         state.dataHeatBal->ZoneOtherEq(Loop).Name,
                                         "Power Level",
                                         "[W]",
                                         state.dataHeatBal->ZoneOtherEq(Loop).EMSZoneEquipOverrideOn,
                                         state.dataHeatBal->ZoneOtherEq(Loop).EMSEquipPower);
                        SetupEMSInternalVariable(state,
                                                 "Other Equipment Design Level",
                                                 state.dataHeatBal->ZoneOtherEq(Loop).Name,
                                                 "[W]",
                                                 state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel);
                    } // EMS

                    if (!ErrorsFound)
                        SetupZoneInternalGain(state,
                                              state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr,
                                              "OtherEquipment",
                                              state.dataHeatBal->ZoneOtherEq(Loop).Name,
                                              IntGainTypeOf_OtherEquipment,
                                              &state.dataHeatBal->ZoneOtherEq(Loop).ConGainRate,
                                              nullptr,
                                              &state.dataHeatBal->ZoneOtherEq(Loop).RadGainRate,
                                              &state.dataHeatBal->ZoneOtherEq(Loop).LatGainRate);

                } // Item1
            }     // Item - number of other equipment statements
        }

        RepVarSet = true;
        CurrentModuleObject = "ElectricEquipment:ITE:AirCooled";
        state.dataHeatBal->NumZoneITEqStatements = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        errFlag = false;

        // Note that this object type does not support ZoneList due to node names in input fields
        state.dataHeatBal->ZoneITEq.allocate(state.dataHeatBal->NumZoneITEqStatements);

        if (state.dataHeatBal->NumZoneITEqStatements > 0) {
            Loop = 0;
            for (Loop = 1; Loop <= state.dataHeatBal->NumZoneITEqStatements; ++Loop) {
                AlphaName = std::string{};
                IHGNumbers = 0.0;

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         Loop,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                state.dataHeatBal->ZoneITEq(Loop).Name = AlphaName(1);
                state.dataHeatBal->ZoneITEq(Loop).ZonePtr = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);

                // IT equipment design level calculation method.
                if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps = false;
                } else {
                    if (UtilityRoutines::SameString(AlphaName(3), "FlowFromSystem")) {
                        state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps = false;
                    } else if (UtilityRoutines::SameString(AlphaName(3), "FlowControlWithApproachTemperatures")) {
                        state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps = true;
                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).HasAdjustedReturnTempByITE = true;
                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).NoHeatToReturnAir = false;
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\": invalid calculation method: " + AlphaName(3));
                        ErrorsFound = true;
                    }
                }

                {
                    auto const equipmentLevel(AlphaName(4));
                    if (equipmentLevel == "WATTS/UNIT") {
                        state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower = IHGNumbers(1) * IHGNumbers(2);
                        if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                 state.dataIPShortCut->cNumericFieldNames(1) +
                                                 ", but that field is blank.  0 IT Equipment will result.");
                        }
                        if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                 state.dataIPShortCut->cNumericFieldNames(2) +
                                                 ", but that field is blank.  0 IT Equipment will result.");
                        }

                    } else if (equipmentLevel == "WATTS/AREA") {
                        if (state.dataHeatBal->ZoneITEq(Loop).ZonePtr != 0) {
                            if (IHGNumbers(3) >= 0.0) {
                                state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower =
                                    IHGNumbers(3) * state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).FloorArea;
                                if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).FloorArea <= 0.0) {
                                    ShowWarningError(state,
                                                     std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                         state.dataIPShortCut->cNumericFieldNames(3) +
                                                         ", but Zone Floor Area = 0.  0 IT Equipment will result.");
                                }
                            } else {
                                ShowSevereError(state,
                                                format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                       RoutineName,
                                                       CurrentModuleObject,
                                                       AlphaName(1),
                                                       state.dataIPShortCut->cNumericFieldNames(3),
                                                       IHGNumbers(3)));
                                ErrorsFound = true;
                            }
                        }
                        if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                 state.dataIPShortCut->cNumericFieldNames(3) +
                                                 ", but that field is blank.  0 IT Equipment will result.");
                        }

                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                            state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                        ShowContinueError(state, "...Valid values are \"Watts/Unit\" or \"Watts/Area\".");
                        ErrorsFound = true;
                    }
                }

                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    state.dataHeatBal->ZoneITEq(Loop).OperSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
                } else {
                    state.dataHeatBal->ZoneITEq(Loop).OperSchedPtr = GetScheduleIndex(state, AlphaName(5));
                }
                SchMin = 0.0;
                SchMax = 0.0;
                if (state.dataHeatBal->ZoneITEq(Loop).OperSchedPtr == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(5) + " entered=" + AlphaName(5));
                    ErrorsFound = true;
                } else { // check min/max on schedule
                    SchMin = GetScheduleMinValue(state, state.dataHeatBal->ZoneITEq(Loop).OperSchedPtr);
                    SchMax = GetScheduleMaxValue(state, state.dataHeatBal->ZoneITEq(Loop).OperSchedPtr);
                    if (SchMin < 0.0 || SchMax < 0.0) {
                        if (SchMin < 0.0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(5) + ", minimum is < 0.0");
                            ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(5), SchMin));
                            ErrorsFound = true;
                        }
                        if (SchMax < 0.0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(5) + ", maximum is < 0.0");
                            ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(5), SchMax));
                            ErrorsFound = true;
                        }
                    }
                }

                if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    state.dataHeatBal->ZoneITEq(Loop).CPULoadSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
                } else {
                    state.dataHeatBal->ZoneITEq(Loop).CPULoadSchedPtr = GetScheduleIndex(state, AlphaName(6));
                }
                SchMin = 0.0;
                SchMax = 0.0;
                if (state.dataHeatBal->ZoneITEq(Loop).CPULoadSchedPtr == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(6) + " entered=" + AlphaName(6));
                    ErrorsFound = true;
                } else { // check min/max on schedule
                    SchMin = GetScheduleMinValue(state, state.dataHeatBal->ZoneITEq(Loop).CPULoadSchedPtr);
                    SchMax = GetScheduleMaxValue(state, state.dataHeatBal->ZoneITEq(Loop).CPULoadSchedPtr);
                    if (SchMin < 0.0 || SchMax < 0.0) {
                        if (SchMin < 0.0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(6) + ", minimum is < 0.0");
                            ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(6), SchMin));
                            ErrorsFound = true;
                        }
                        if (SchMax < 0.0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(6) + ", maximum is < 0.0");
                            ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(6), SchMax));
                            ErrorsFound = true;
                        }
                    }
                }

                // Calculate nominal min/max equipment level
                state.dataHeatBal->ZoneITEq(Loop).NomMinDesignLevel = state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower * SchMin;
                state.dataHeatBal->ZoneITEq(Loop).NomMaxDesignLevel = state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower * SchMax;

                state.dataHeatBal->ZoneITEq(Loop).DesignFanPowerFrac = IHGNumbers(4);
                state.dataHeatBal->ZoneITEq(Loop).DesignFanPower =
                    state.dataHeatBal->ZoneITEq(Loop).DesignFanPowerFrac * state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower;
                state.dataHeatBal->ZoneITEq(Loop).DesignCPUPower =
                    (1.0 - state.dataHeatBal->ZoneITEq(Loop).DesignFanPowerFrac) * state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower;
                state.dataHeatBal->ZoneITEq(Loop).DesignAirVolFlowRate = IHGNumbers(5) * state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower;
                state.dataHeatBal->ZoneITEq(Loop).DesignTAirIn = IHGNumbers(6);
                state.dataHeatBal->ZoneITEq(Loop).DesignRecircFrac = IHGNumbers(7);
                state.dataHeatBal->ZoneITEq(Loop).DesignUPSEfficiency = IHGNumbers(8);
                state.dataHeatBal->ZoneITEq(Loop).UPSLossToZoneFrac = IHGNumbers(9);
                state.dataHeatBal->ZoneITEq(Loop).SupplyApproachTemp = IHGNumbers(10);
                state.dataHeatBal->ZoneITEq(Loop).ReturnApproachTemp = IHGNumbers(11);

                bool hasSupplyApproachTemp = !state.dataIPShortCut->lNumericFieldBlanks(10);
                bool hasReturnApproachTemp = !state.dataIPShortCut->lNumericFieldBlanks(11);

                // Performance curves
                state.dataHeatBal->ZoneITEq(Loop).CPUPowerFLTCurve = GetCurveIndex(state, AlphaName(7));
                if (state.dataHeatBal->ZoneITEq(Loop).CPUPowerFLTCurve == 0) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " \"" + AlphaName(1) + "\"");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphaName(7));
                    ErrorsFound = true;
                }

                state.dataHeatBal->ZoneITEq(Loop).AirFlowFLTCurve = GetCurveIndex(state, AlphaName(8));
                if (state.dataHeatBal->ZoneITEq(Loop).AirFlowFLTCurve == 0) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " \"" + AlphaName(1) + "\"");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + AlphaName(8));
                    ErrorsFound = true;
                }

                state.dataHeatBal->ZoneITEq(Loop).FanPowerFFCurve = GetCurveIndex(state, AlphaName(9));
                if (state.dataHeatBal->ZoneITEq(Loop).FanPowerFFCurve == 0) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " \"" + AlphaName(1) + "\"");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(9) + '=' + AlphaName(9));
                    ErrorsFound = true;
                }

                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    // If this field isn't blank, it must point to a valid curve
                    state.dataHeatBal->ZoneITEq(Loop).RecircFLTCurve = GetCurveIndex(state, AlphaName(15));
                    if (state.dataHeatBal->ZoneITEq(Loop).RecircFLTCurve == 0) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " \"" + AlphaName(1) + "\"");
                        ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(15) + '=' + AlphaName(15));
                        ErrorsFound = true;
                    }
                } else {
                    // If this curve is left blank, then the curve is assumed to always equal 1.0.
                    state.dataHeatBal->ZoneITEq(Loop).RecircFLTCurve = 0;
                }

                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    // If this field isn't blank, it must point to a valid curve
                    state.dataHeatBal->ZoneITEq(Loop).UPSEfficFPLRCurve = GetCurveIndex(state, AlphaName(16));
                    if (state.dataHeatBal->ZoneITEq(Loop).UPSEfficFPLRCurve == 0) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " \"" + AlphaName(1) + "\"");
                        ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(16) + '=' + AlphaName(16));
                        ErrorsFound = true;
                    }
                } else {
                    // If this curve is left blank, then the curve is assumed to always equal 1.0.
                    state.dataHeatBal->ZoneITEq(Loop).UPSEfficFPLRCurve = 0;
                }

                // Environmental class
                if (UtilityRoutines::SameString(AlphaName(10), "None")) {
                    state.dataHeatBal->ZoneITEq(Loop).Class = ITEClassNone;
                } else if (UtilityRoutines::SameString(AlphaName(10), "A1")) {
                    state.dataHeatBal->ZoneITEq(Loop).Class = ITEClassA1;
                } else if (UtilityRoutines::SameString(AlphaName(10), "A2")) {
                    state.dataHeatBal->ZoneITEq(Loop).Class = ITEClassA2;
                } else if (UtilityRoutines::SameString(AlphaName(10), "A3")) {
                    state.dataHeatBal->ZoneITEq(Loop).Class = ITEClassA3;
                } else if (UtilityRoutines::SameString(AlphaName(10), "A4")) {
                    state.dataHeatBal->ZoneITEq(Loop).Class = ITEClassA4;
                } else if (UtilityRoutines::SameString(AlphaName(10), "B")) {
                    state.dataHeatBal->ZoneITEq(Loop).Class = ITEClassB;
                } else if (UtilityRoutines::SameString(AlphaName(10), "C")) {
                    state.dataHeatBal->ZoneITEq(Loop).Class = ITEClassC;
                } else {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + ": " + AlphaName(1));
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(10) + '=' + AlphaName(10));
                    ShowContinueError(state, "Valid entries are None, A1, A2, A3, A4, B or C.");
                    ErrorsFound = true;
                }

                // Air and supply inlet connections
                if (UtilityRoutines::SameString(AlphaName(11), "AdjustedSupply")) {
                    state.dataHeatBal->ZoneITEq(Loop).AirConnectionType = ITEInletAdjustedSupply;
                } else if (UtilityRoutines::SameString(AlphaName(11), "ZoneAirNode")) {
                    state.dataHeatBal->ZoneITEq(Loop).AirConnectionType = ITEInletZoneAirNode;
                } else if (UtilityRoutines::SameString(AlphaName(11), "RoomAirModel")) {
                    // ZoneITEq( Loop ).AirConnectionType = ITEInletRoomAirModel;
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) +
                                         "Air Inlet Connection Type = RoomAirModel is not implemented yet, using ZoneAirNode");
                    state.dataHeatBal->ZoneITEq(Loop).AirConnectionType = ITEInletZoneAirNode;
                } else {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + ": " + AlphaName(1));
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(11) + '=' + AlphaName(11));
                    ShowContinueError(state, "Valid entries are AdjustedSupply, ZoneAirNode, or RoomAirModel.");
                    ErrorsFound = true;
                }
                if (state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    if (state.dataHeatBal->ZoneITEq(Loop).AirConnectionType == ITEInletAdjustedSupply) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + ": " + AlphaName(1));
                        ShowContinueError(state,
                                          "For " + state.dataIPShortCut->cAlphaFieldNames(11) + "= AdjustedSupply, " +
                                              state.dataIPShortCut->cAlphaFieldNames(14) + " is required, but this field is blank.");
                        ErrorsFound = true;
                    } else if (state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + ": " + AlphaName(1));
                        ShowContinueError(state,
                                          "For " + state.dataIPShortCut->cAlphaFieldNames(3) + "= FlowControlWithApproachTemperatures, " +
                                              state.dataIPShortCut->cAlphaFieldNames(14) + " is required, but this field is blank.");
                        ErrorsFound = true;
                    }
                } else {
                    state.dataHeatBal->ZoneITEq(Loop).SupplyAirNodeNum = GetOnlySingleNode(state,
                                                                                           AlphaName(14),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject,
                                                                                           AlphaName(1),
                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                           DataLoopNode::NodeConnectionType::Sensor,
                                                                                           1,
                                                                                           ObjectIsNotParent);
                }

                // check supply air node for matches with zone equipment supply air node
                int zoneEqIndex =
                    DataZoneEquipment::GetControlledZoneIndex(state, state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                auto itStart = state.dataZoneEquip->ZoneEquipConfig(zoneEqIndex).InletNode.begin();
                auto itEnd = state.dataZoneEquip->ZoneEquipConfig(zoneEqIndex).InletNode.end();
                auto key = state.dataHeatBal->ZoneITEq(Loop).SupplyAirNodeNum;
                bool supplyNodeFound = false;
                if (std::find(itStart, itEnd, key) != itEnd) {
                    supplyNodeFound = true;
                }

                if (state.dataHeatBal->ZoneITEq(Loop).AirConnectionType == ITEInletAdjustedSupply && !supplyNodeFound) {
                    // supply air node must match zone equipment supply air node for these conditions
                    ShowSevereError(state, std::string{RoutineName} + ": ElectricEquipment:ITE:AirCooled " + state.dataHeatBal->ZoneITEq(Loop).Name);
                    ShowContinueError(state, "Air Inlet Connection Type = AdjustedSupply but no Supply Air Node is specified.");
                    ErrorsFound = true;
                } else if (state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps && !supplyNodeFound) {
                    // supply air node must match zone equipment supply air node for these conditions
                    ShowSevereError(state, std::string{RoutineName} + ": ElectricEquipment:ITE:AirCooled " + state.dataHeatBal->ZoneITEq(Loop).Name);
                    ShowContinueError(state, "Air Inlet Connection Type = AdjustedSupply but no Supply Air Node is specified.");
                    ErrorsFound = true;
                } else if (state.dataHeatBal->ZoneITEq(Loop).SupplyAirNodeNum != 0 && !supplyNodeFound) {
                    // the given supply air node does not match any zone equipment supply air nodes
                    ShowWarningError(state,
                                     CurrentModuleObject + "name: '" + AlphaName(1) + ". " + "Supply Air Node Name '" + AlphaName(14) +
                                         "' does not match any ZoneHVAC:EquipmentConnections objects.");
                }

                // End-Use subcategories
                if (NumAlpha > 16) {
                    state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryCPU = AlphaName(17);
                } else {
                    state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryCPU = "ITE-CPU";
                }

                if (NumAlpha > 17) {
                    state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryFan = AlphaName(18);
                } else {
                    state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryFan = "ITE-Fans";
                }
                if (state.dataHeatBal->ZoneITEq(Loop).ZonePtr <= 0) continue; // Error, will be caught and terminated later

                if (NumAlpha > 18) {
                    state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryUPS = AlphaName(19);
                } else {
                    state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryUPS = "ITE-UPS";
                }
                if (state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps) {
                    if (!state.dataIPShortCut->lAlphaFieldBlanks(20)) {
                        state.dataHeatBal->ZoneITEq(Loop).SupplyApproachTempSch = GetScheduleIndex(state, AlphaName(20));
                        if (state.dataHeatBal->ZoneITEq(Loop).SupplyApproachTempSch == 0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(20) + " entered=" + AlphaName(20));
                            ErrorsFound = true;
                        }
                    } else {
                        if (!hasSupplyApproachTemp) {
                            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " \"" + AlphaName(1) + "\"");
                            ShowContinueError(state,
                                              "For " + state.dataIPShortCut->cAlphaFieldNames(3) + "= FlowControlWithApproachTemperatures, either " +
                                                  state.dataIPShortCut->cNumericFieldNames(10) + " or " + state.dataIPShortCut->cAlphaFieldNames(20) +
                                                  " is required, but both are left blank.");
                            ErrorsFound = true;
                        }
                    }

                    if (!state.dataIPShortCut->lAlphaFieldBlanks(21)) {
                        state.dataHeatBal->ZoneITEq(Loop).ReturnApproachTempSch = GetScheduleIndex(state, AlphaName(21));
                        if (state.dataHeatBal->ZoneITEq(Loop).ReturnApproachTempSch == 0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(20) + " entered=" + AlphaName(20));
                            ErrorsFound = true;
                        }
                    } else {
                        if (!hasReturnApproachTemp) {
                            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " \"" + AlphaName(1) + "\"");
                            ShowContinueError(state,
                                              "For " + state.dataIPShortCut->cAlphaFieldNames(3) + "= FlowControlWithApproachTemperatures, either " +
                                                  state.dataIPShortCut->cNumericFieldNames(11) + " or " + state.dataIPShortCut->cAlphaFieldNames(21) +
                                                  " is required, but both are left blank.");
                            ErrorsFound = true;
                        }
                    }
                }

                if (state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps) {
                    Real64 TAirInSizing = 0.0;
                    // Set the TAirInSizing to the maximun setpoint value to do sizing based on the maximum fan and cpu power of the ite object
                    SetPointManager::GetSetPointManagerInputData(state, ErrorsFound);
                    for (int SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZClSetPtMgrs; ++SetPtMgrNum) {
                        if (state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ControlZoneNum == Loop) {
                            TAirInSizing = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).MaxSetTemp;
                        }
                    }

                    state.dataHeatBal->ZoneITEq(Loop).SizingTAirIn = max(TAirInSizing, state.dataHeatBal->ZoneITEq(Loop).DesignTAirIn);
                }

                // Object report variables
                SetupOutputVariable(state,
                                    "ITE CPU Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZoneITEq(Loop).CPUPower,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Fan Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZoneITEq(Loop).FanPower,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE UPS Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZoneITEq(Loop).UPSPower,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE CPU Electricity Rate at Design Inlet Conditions",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZoneITEq(Loop).CPUPowerAtDesign,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Fan Electricity Rate at Design Inlet Conditions",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZoneITEq(Loop).FanPowerAtDesign,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE UPS Heat Gain to Zone Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZoneITEq(Loop).UPSGainRateToZone,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Total Heat Gain to Zone Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZoneITEq(Loop).ConGainRateToZone,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);

                SetupOutputVariable(state,
                                    "ITE CPU Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZoneITEq(Loop).CPUConsumption,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name,
                                    _,
                                    "Electricity",
                                    "InteriorEquipment",
                                    state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryCPU,
                                    "Building",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name,
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Multiplier,
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ListMultiplier);
                SetupOutputVariable(state,
                                    "ITE Fan Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZoneITEq(Loop).FanConsumption,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name,
                                    _,
                                    "Electricity",
                                    "InteriorEquipment",
                                    state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryFan,
                                    "Building",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name,
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Multiplier,
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ListMultiplier);
                SetupOutputVariable(state,
                                    "ITE UPS Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZoneITEq(Loop).UPSConsumption,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name,
                                    _,
                                    "Electricity",
                                    "InteriorEquipment",
                                    state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryUPS,
                                    "Building",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name,
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Multiplier,
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ListMultiplier);
                SetupOutputVariable(state,
                                    "ITE CPU Electricity Energy at Design Inlet Conditions",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZoneITEq(Loop).CPUEnergyAtDesign,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Fan Electricity Energy at Design Inlet Conditions",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZoneITEq(Loop).FanEnergyAtDesign,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE UPS Heat Gain to Zone Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZoneITEq(Loop).UPSGainEnergyToZone,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Total Heat Gain to Zone Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZoneITEq(Loop).ConGainEnergyToZone,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);

                SetupOutputVariable(state,
                                    "ITE Standard Density Air Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZoneITEq(Loop).AirVolFlowStdDensity,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Current Density Air Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZoneITEq(Loop).AirVolFlowCurDensity,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataHeatBal->ZoneITEq(Loop).AirMassFlow,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Dry-Bulb Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataHeatBal->ZoneITEq(Loop).AirInletDryBulbT,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Dewpoint Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataHeatBal->ZoneITEq(Loop).AirInletDewpointT,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Relative Humidity",
                                    OutputProcessor::Unit::Perc,
                                    state.dataHeatBal->ZoneITEq(Loop).AirInletRelHum,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Outlet Dry-Bulb Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataHeatBal->ZoneITEq(Loop).AirOutletDryBulbT,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                if (state.dataHeatBal->ZoneITEq(Loop).SupplyAirNodeNum != 0) {
                    SetupOutputVariable(state,
                                        "ITE Supply Heat Index",
                                        OutputProcessor::Unit::None,
                                        state.dataHeatBal->ZoneITEq(Loop).SHI,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->ZoneITEq(Loop).Name);
                }
                SetupOutputVariable(state,
                                    "ITE Air Inlet Operating Range Exceeded Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZoneITEq(Loop).TimeAboveDryBulbT,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZoneITEq(Loop).TimeBelowDryBulbT,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Dewpoint Temperature Above Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZoneITEq(Loop).TimeAboveDewpointT,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Dewpoint Temperature Below Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZoneITEq(Loop).TimeBelowDewpointT,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Relative Humidity Above Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZoneITEq(Loop).TimeAboveRH,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Relative Humidity Below Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZoneITEq(Loop).TimeBelowRH,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Dry-Bulb Temperature Difference Above Operating Range",
                                    OutputProcessor::Unit::deltaC,
                                    state.dataHeatBal->ZoneITEq(Loop).DryBulbTAboveDeltaT,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Dry-Bulb Temperature Difference Below Operating Range",
                                    OutputProcessor::Unit::deltaC,
                                    state.dataHeatBal->ZoneITEq(Loop).DryBulbTBelowDeltaT,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Dewpoint Temperature Difference Above Operating Range",
                                    OutputProcessor::Unit::deltaC,
                                    state.dataHeatBal->ZoneITEq(Loop).DewpointTAboveDeltaT,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Dewpoint Temperature Difference Below Operating Range",
                                    OutputProcessor::Unit::deltaC,
                                    state.dataHeatBal->ZoneITEq(Loop).DewpointTBelowDeltaT,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Relative Humidity Difference Above Operating Range",
                                    OutputProcessor::Unit::Perc,
                                    state.dataHeatBal->ZoneITEq(Loop).RHAboveDeltaRH,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);
                SetupOutputVariable(state,
                                    "ITE Air Inlet Relative Humidity Difference Below Operating Range",
                                    OutputProcessor::Unit::Perc,
                                    state.dataHeatBal->ZoneITEq(Loop).RHBelowDeltaRH,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->ZoneITEq(Loop).Name);

                // Zone total report variables
                if (RepVarSet(state.dataHeatBal->ZoneITEq(Loop).ZonePtr)) {
                    RepVarSet(state.dataHeatBal->ZoneITEq(Loop).ZonePtr) = false;
                    SetupOutputVariable(state,
                                        "Zone ITE CPU Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqCPUPower,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Fan Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqFanPower,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE UPS Electricity Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqUPSPower,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE CPU Electricity Rate at Design Inlet Conditions",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqCPUPowerAtDesign,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Fan Electricity Rate at Design Inlet Conditions",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqFanPowerAtDesign,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE UPS Heat Gain to Zone Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqUPSGainRateToZone,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Total Heat Gain to Zone Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqConGainRateToZone,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Adjusted Return Air Temperature",
                                        OutputProcessor::Unit::W,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEAdjReturnTemp,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);

                    SetupOutputVariable(state,
                                        "Zone ITE CPU Electricity Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqCPUConsumption,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Fan Electricity Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqFanConsumption,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE UPS Electricity Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqUPSConsumption,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE CPU Electricity Energy at Design Inlet Conditions",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqCPUEnergyAtDesign,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Fan Electricity Energy at Design Inlet Conditions",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqFanEnergyAtDesign,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE UPS Heat Gain to Zone Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqUPSGainEnergyToZone,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Total Heat Gain to Zone Energy",
                                        OutputProcessor::Unit::J,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqConGainEnergyToZone,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);

                    SetupOutputVariable(state,
                                        "Zone ITE Standard Density Air Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqAirVolFlowStdDensity,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Air Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqAirMassFlow,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Average Supply Heat Index",
                                        OutputProcessor::Unit::None,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqSHI,
                                        "Zone",
                                        "Average",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Any Air Inlet Operating Range Exceeded Time",
                                        OutputProcessor::Unit::hr,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqTimeOutOfOperRange,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Any Air Inlet Dry-Bulb Temperature Above Operating Range Time",
                                        OutputProcessor::Unit::hr,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqTimeAboveDryBulbT,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Any Air Inlet Dry-Bulb Temperature Below Operating Range Time",
                                        OutputProcessor::Unit::hr,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqTimeBelowDryBulbT,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Any Air Inlet Dewpoint Temperature Above Operating Range Time",
                                        OutputProcessor::Unit::hr,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqTimeAboveDewpointT,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Any Air Inlet Dewpoint Temperature Below Operating Range Time",
                                        OutputProcessor::Unit::hr,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqTimeBelowDewpointT,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Any Air Inlet Relative Humidity Above Operating Range Time",
                                        OutputProcessor::Unit::hr,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqTimeAboveRH,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                    SetupOutputVariable(state,
                                        "Zone ITE Any Air Inlet Relative Humidity Below Operating Range Time",
                                        OutputProcessor::Unit::hr,
                                        state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).ITEqTimeBelowRH,
                                        "Zone",
                                        "Sum",
                                        state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).Name);
                }

                // MJW - EMS Not in place yet
                // if ( AnyEnergyManagementSystemInModel ) {
                // SetupEMSActuator( "ElectricEquipment", ZoneITEq( Loop ).Name, "Electric Power Level", "[W]", ZoneITEq( Loop
                // ).EMSZoneEquipOverrideOn, ZoneITEq( Loop ).EMSEquipPower ); SetupEMSInternalVariable( "Plug and Process Power Design Level",
                // ZoneITEq( Loop ).Name, "[W]", ZoneITEq( Loop ).DesignTotalPower ); } // EMS

                if (!ErrorsFound)
                    SetupZoneInternalGain(state,
                                          state.dataHeatBal->ZoneITEq(Loop).ZonePtr,
                                          "ElectricEquipment:ITE:AirCooled",
                                          state.dataHeatBal->ZoneITEq(Loop).Name,
                                          IntGainTypeOf_ElectricEquipmentITEAirCooled,
                                          &state.dataHeatBal->ZoneITEq(Loop).ConGainRateToZone);

            } // Item - Number of ZoneITEq objects
            for (Loop = 1; Loop <= state.dataHeatBal->NumZoneITEqStatements; ++Loop) {
                if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).HasAdjustedReturnTempByITE &&
                    (!state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\": invalid calculation method " + AlphaName(3) +
                                        " for Zone: " + AlphaName(2));
                    ShowContinueError(state, "...Multiple flow control methods apply to one zone. ");
                    ErrorsFound = true;
                }
            }
        } // Check on number of ZoneITEq

        RepVarSet = true;
        CurrentModuleObject = "ZoneBaseboard:OutdoorTemperatureControlled";
        state.dataHeatBal->TotBBHeat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataHeatBal->ZoneBBHeat.allocate(state.dataHeatBal->TotBBHeat);

        for (Loop = 1; Loop <= state.dataHeatBal->TotBBHeat; ++Loop) {
            AlphaName = "";
            IHGNumbers = 0.0;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Loop,
                                                                     AlphaName,
                                                                     NumAlpha,
                                                                     IHGNumbers,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);

            state.dataHeatBal->ZoneBBHeat(Loop).Name = AlphaName(1);

            state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
            if (state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(2) + " entered=" + AlphaName(2));
                ErrorsFound = true;
            }

            state.dataHeatBal->ZoneBBHeat(Loop).SchedPtr = GetScheduleIndex(state, AlphaName(3));
            if (state.dataHeatBal->ZoneBBHeat(Loop).SchedPtr == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + state.dataIPShortCut->cAlphaFieldNames(3) +
                                        " is required.");
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(state, state.dataHeatBal->ZoneBBHeat(Loop).SchedPtr);
                SchMax = GetScheduleMaxValue(state, state.dataHeatBal->ZoneBBHeat(Loop).SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                            state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                        ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                            state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                        ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                        ErrorsFound = true;
                    }
                }
            }

            if (NumAlpha > 3) {
                state.dataHeatBal->ZoneBBHeat(Loop).EndUseSubcategory = AlphaName(4);
            } else {
                state.dataHeatBal->ZoneBBHeat(Loop).EndUseSubcategory = "General";
            }

            state.dataHeatBal->ZoneBBHeat(Loop).CapatLowTemperature = IHGNumbers(1);
            state.dataHeatBal->ZoneBBHeat(Loop).LowTemperature = IHGNumbers(2);
            state.dataHeatBal->ZoneBBHeat(Loop).CapatHighTemperature = IHGNumbers(3);
            state.dataHeatBal->ZoneBBHeat(Loop).HighTemperature = IHGNumbers(4);
            state.dataHeatBal->ZoneBBHeat(Loop).FractionRadiant = IHGNumbers(5);
            state.dataHeatBal->ZoneBBHeat(Loop).FractionConvected = 1.0 - state.dataHeatBal->ZoneBBHeat(Loop).FractionRadiant;
            if (state.dataHeatBal->ZoneBBHeat(Loop).FractionConvected < 0.0) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", Sum of Fractions > 1.0");
                ErrorsFound = true;
            }

            if (state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr <= 0) continue; // Error, will be caught and terminated later

            // Object report variables
            SetupOutputVariable(state,
                                "Baseboard Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneBBHeat(Loop).Power,
                                "Zone",
                                "Average",
                                state.dataHeatBal->ZoneBBHeat(Loop).Name);
            SetupOutputVariable(state,
                                "Baseboard Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneBBHeat(Loop).Consumption,
                                "Zone",
                                "Sum",
                                state.dataHeatBal->ZoneBBHeat(Loop).Name,
                                _,
                                "Electricity",
                                "InteriorEquipment",
                                state.dataHeatBal->ZoneBBHeat(Loop).EndUseSubcategory,
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).Name,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).Multiplier,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).ListMultiplier);

            SetupOutputVariable(state,
                                "Baseboard Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneBBHeat(Loop).RadGainEnergy,
                                "Zone",
                                "Sum",
                                state.dataHeatBal->ZoneBBHeat(Loop).Name);
            SetupOutputVariable(state,
                                "Baseboard Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneBBHeat(Loop).RadGainRate,
                                "Zone",
                                "Average",
                                state.dataHeatBal->ZoneBBHeat(Loop).Name);
            SetupOutputVariable(state,
                                "Baseboard Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneBBHeat(Loop).ConGainEnergy,
                                "Zone",
                                "Sum",
                                state.dataHeatBal->ZoneBBHeat(Loop).Name);
            SetupOutputVariable(state,
                                "Baseboard Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneBBHeat(Loop).ConGainRate,
                                "Zone",
                                "Average",
                                state.dataHeatBal->ZoneBBHeat(Loop).Name);
            SetupOutputVariable(state,
                                "Baseboard Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneBBHeat(Loop).TotGainEnergy,
                                "Zone",
                                "Sum",
                                state.dataHeatBal->ZoneBBHeat(Loop).Name);
            SetupOutputVariable(state,
                                "Baseboard Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneBBHeat(Loop).TotGainRate,
                                "Zone",
                                "Average",
                                state.dataHeatBal->ZoneBBHeat(Loop).Name);

            // Zone total report variables
            if (RepVarSet(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr)) {
                RepVarSet(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr) = false;
                SetupOutputVariable(state,
                                    "Zone Baseboard Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).BaseHeatPower,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).BaseHeatElecCons,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).Name);

                SetupOutputVariable(state,
                                    "Zone Baseboard Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).BaseHeatRadGain,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).BaseHeatRadGainRate,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).BaseHeatConGain,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).BaseHeatConGainRate,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).BaseHeatTotGain,
                                    "Zone",
                                    "Sum",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).BaseHeatTotGainRate,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr).Name);
            }

            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state,
                                 "ZoneBaseboard:OutdoorTemperatureControlled",
                                 state.dataHeatBal->ZoneBBHeat(Loop).Name,
                                 "Power Level",
                                 "[W]",
                                 state.dataHeatBal->ZoneBBHeat(Loop).EMSZoneBaseboardOverrideOn,
                                 state.dataHeatBal->ZoneBBHeat(Loop).EMSZoneBaseboardPower);
                SetupEMSInternalVariable(state,
                                         "Simple Zone Baseboard Capacity At Low Temperature",
                                         state.dataHeatBal->ZoneBBHeat(Loop).Name,
                                         "[W]",
                                         state.dataHeatBal->ZoneBBHeat(Loop).CapatLowTemperature);
                SetupEMSInternalVariable(state,
                                         "Simple Zone Baseboard Capacity At High Temperature",
                                         state.dataHeatBal->ZoneBBHeat(Loop).Name,
                                         "[W]",
                                         state.dataHeatBal->ZoneBBHeat(Loop).CapatHighTemperature);
            } // EMS

            SetupZoneInternalGain(state,
                                  state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr,
                                  "ZoneBaseboard:OutdoorTemperatureControlled",
                                  state.dataHeatBal->ZoneBBHeat(Loop).Name,
                                  IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled,
                                  &state.dataHeatBal->ZoneBBHeat(Loop).ConGainRate,
                                  nullptr,
                                  &state.dataHeatBal->ZoneBBHeat(Loop).RadGainRate);
        }

        RepVarSet = true;
        CurrentModuleObject = "ZoneContaminantSourceAndSink:CarbonDioxide";
        state.dataHeatBal->TotCO2Gen = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataHeatBal->ZoneCO2Gen.allocate(state.dataHeatBal->TotCO2Gen);

        for (Loop = 1; Loop <= state.dataHeatBal->TotCO2Gen; ++Loop) {
            AlphaName = "";
            IHGNumbers = 0.0;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Loop,
                                                                     AlphaName,
                                                                     NumAlpha,
                                                                     IHGNumbers,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, AlphaName(1), CurrentModuleObject, ErrorsFound);

            state.dataHeatBal->ZoneCO2Gen(Loop).Name = AlphaName(1);

            state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
            if (state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(2) + " entered=" + AlphaName(2));
                ErrorsFound = true;
            }

            state.dataHeatBal->ZoneCO2Gen(Loop).SchedPtr = GetScheduleIndex(state, AlphaName(3));
            if (state.dataHeatBal->ZoneCO2Gen(Loop).SchedPtr == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " + state.dataIPShortCut->cAlphaFieldNames(3) +
                                        " is required.");
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(state, state.dataHeatBal->ZoneCO2Gen(Loop).SchedPtr);
                SchMax = GetScheduleMaxValue(state, state.dataHeatBal->ZoneCO2Gen(Loop).SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                            state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                        ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphaName(1) + "\", " +
                                            state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                        ShowContinueError(state, format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                        ErrorsFound = true;
                    }
                }
            }

            state.dataHeatBal->ZoneCO2Gen(Loop).CO2DesignRate = IHGNumbers(1);

            if (state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr <= 0) continue; // Error, will be caught and terminated later

            // Object report variables
            SetupOutputVariable(state,
                                "Contaminant Source or Sink CO2 Gain Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZoneCO2Gen(Loop).CO2GainRate,
                                "Zone",
                                "Average",
                                state.dataHeatBal->ZoneCO2Gen(Loop).Name);

            // Zone total report variables
            if (RepVarSet(state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr)) {
                RepVarSet(state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr) = false;

                SetupOutputVariable(state,
                                    "Zone Contaminant Source or Sink CO2 Gain Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr).CO2Rate,
                                    "Zone",
                                    "Average",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr).Name);
            }

            SetupZoneInternalGain(state,
                                  state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr,
                                  "ZoneContaminantSourceAndSink:CarbonDioxide",
                                  state.dataHeatBal->ZoneCO2Gen(Loop).Name,
                                  IntGainTypeOf_ZoneContaminantSourceAndSinkCarbonDioxide,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  nullptr,
                                  &state.dataHeatBal->ZoneCO2Gen(Loop).CO2GainRate);
        }

        RepVarSet.deallocate();
        IHGNumbers.deallocate();
        AlphaName.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found in Getting Internal Gains Input, Program Stopped");
        }

        static constexpr fmt::string_view Format_721(
            "! <Zone Internal Gains Nominal>,Zone Name, Floor Area {{m2}},# Occupants,Area per Occupant "
            "{{m2/person}},Occupant per Area {{person/m2}},Interior Lighting {{W/m2}},Electric Load {{W/m2}},Gas Load {{W/m2}},Other "
            "Load {{W/m2}},Hot Water Eq {{W/m2}},Steam Equipment {{W/m2}},Sum Loads per Area {{W/m2}},Outdoor Controlled Baseboard "
            "Heat\n");

        print(state.files.eio, Format_721);
        for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
            LightTot = 0.0;
            ElecTot = 0.0;
            GasTot = 0.0;
            OthTot = 0.0;
            HWETot = 0.0;
            StmTot = 0.0;
            BBHeatInd = "No";
            for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotLights; ++Loop1) {
                if (state.dataHeatBal->Lights(Loop1).ZonePtr != Loop) continue;
                LightTot += state.dataHeatBal->Lights(Loop1).DesignLevel;
            }
            for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotElecEquip; ++Loop1) {
                if (state.dataHeatBal->ZoneElectric(Loop1).ZonePtr != Loop) continue;
                ElecTot += state.dataHeatBal->ZoneElectric(Loop1).DesignLevel;
            }
            for (Loop1 = 1; Loop1 <= state.dataHeatBal->NumZoneITEqStatements; ++Loop1) {
                if (state.dataHeatBal->ZoneITEq(Loop1).ZonePtr != Loop) continue;
                ElecTot += state.dataHeatBal->ZoneITEq(Loop1).DesignTotalPower;
            }
            for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotGasEquip; ++Loop1) {
                if (state.dataHeatBal->ZoneGas(Loop1).ZonePtr != Loop) continue;
                GasTot += state.dataHeatBal->ZoneGas(Loop1).DesignLevel;
            }
            for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotOthEquip; ++Loop1) {
                if (state.dataHeatBal->ZoneOtherEq(Loop1).ZonePtr != Loop) continue;
                OthTot += state.dataHeatBal->ZoneOtherEq(Loop1).DesignLevel;
            }
            for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotStmEquip; ++Loop1) {
                if (state.dataHeatBal->ZoneSteamEq(Loop1).ZonePtr != Loop) continue;
                StmTot += state.dataHeatBal->ZoneSteamEq(Loop1).DesignLevel;
            }
            for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotHWEquip; ++Loop1) {
                if (state.dataHeatBal->ZoneHWEq(Loop1).ZonePtr != Loop) continue;
                HWETot += state.dataHeatBal->ZoneHWEq(Loop1).DesignLevel;
            }
            for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotBBHeat; ++Loop1) {
                if (state.dataHeatBal->ZoneBBHeat(Loop1).ZonePtr != Loop) continue;
                BBHeatInd = "Yes";
            }
            state.dataHeatBal->Zone(Loop).InternalHeatGains = LightTot + ElecTot + GasTot + OthTot + HWETot + StmTot;
            if (state.dataHeatBal->Zone(Loop).FloorArea > 0.0) {
                print(state.files.eio,
                      Format_720,
                      state.dataHeatBal->Zone(Loop).Name,
                      state.dataHeatBal->Zone(Loop).FloorArea,
                      state.dataHeatBal->Zone(Loop).TotOccupants);
                print_and_divide_if_greater_than_zero(state.dataHeatBal->Zone(Loop).FloorArea, state.dataHeatBal->Zone(Loop).TotOccupants);
                print(state.files.eio, "{:.3R},", state.dataHeatBal->Zone(Loop).TotOccupants / state.dataHeatBal->Zone(Loop).FloorArea);
                print(state.files.eio, "{:.3R},", LightTot / state.dataHeatBal->Zone(Loop).FloorArea);
                print(state.files.eio, "{:.3R},", ElecTot / state.dataHeatBal->Zone(Loop).FloorArea);
                print(state.files.eio, "{:.3R},", GasTot / state.dataHeatBal->Zone(Loop).FloorArea);
                print(state.files.eio, "{:.3R},", OthTot / state.dataHeatBal->Zone(Loop).FloorArea);
                print(state.files.eio, "{:.3R},", HWETot / state.dataHeatBal->Zone(Loop).FloorArea);
                print(state.files.eio, "{:.3R},", StmTot / state.dataHeatBal->Zone(Loop).FloorArea);
                print(state.files.eio,
                      "{:.3R},{}\n",
                      state.dataHeatBal->Zone(Loop).InternalHeatGains / state.dataHeatBal->Zone(Loop).FloorArea,
                      BBHeatInd);
            } else {
                print(state.files.eio,
                      Format_720,
                      state.dataHeatBal->Zone(Loop).Name,
                      state.dataHeatBal->Zone(Loop).FloorArea,
                      state.dataHeatBal->Zone(Loop).TotOccupants);
                print(state.files.eio, "0.0,N/A,N/A,N/A,N/A,N/A,N/A,N/A,N/A,{}\n", BBHeatInd);
            }
        }
        for (Loop = 1; Loop <= state.dataHeatBal->TotPeople; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "People",
                      "Number of People {},People/Floor Area {person/m2},Floor Area per person {m2/person},Fraction Radiant,Fraction "
                      "Convected,Sensible Fraction Calculation,Activity level,ASHRAE 55 Warnings,Carbon Dioxide Generation Rate,Nominal Minimum "
                      "Number of People,Nominal Maximum Number of People");
                if (state.dataHeatBal->People(Loop).Fanger || state.dataHeatBal->People(Loop).Pierce || state.dataHeatBal->People(Loop).KSU ||
                    state.dataHeatBal->People(Loop).CoolingEffectASH55 || state.dataHeatBal->People(Loop).AnkleDraftASH55) {
                    print(state.files.eio,
                          ",MRT Calculation Type,Work Efficiency, Clothing Insulation Calculation Method,Clothing "
                          "Insulation Calculation Method Schedule,Clothing,Air Velocity,Fanger Calculation,Pierce "
                          "Calculation,KSU Calculation,Cooling Effect Calculation,Ankle Draft Calculation\n");
                } else {
                    print(state.files.eio, "\n");
                }
            }

            ZoneNum = state.dataHeatBal->People(Loop).ZonePtr;

            if (ZoneNum == 0) {
                print(state.files.eio, Format_724, "People-Illegal Zone specified", state.dataHeatBal->People(Loop).Name);
                continue;
            }

            print(state.files.eio,
                  Format_722,
                  "People",
                  state.dataHeatBal->People(Loop).Name,
                  GetScheduleName(state, state.dataHeatBal->People(Loop).NumberOfPeoplePtr),
                  state.dataHeatBal->Zone(ZoneNum).Name,
                  state.dataHeatBal->Zone(ZoneNum).FloorArea,
                  state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.1R},", state.dataHeatBal->People(Loop).NumberOfPeople);

            print_and_divide_if_greater_than_zero(state.dataHeatBal->People(Loop).NumberOfPeople, state.dataHeatBal->Zone(ZoneNum).FloorArea);

            if (state.dataHeatBal->People(Loop).NumberOfPeople > 0.0) {
                print_and_divide_if_greater_than_zero(state.dataHeatBal->Zone(ZoneNum).FloorArea, state.dataHeatBal->People(Loop).NumberOfPeople);
            } else {
                print(state.files.eio, "N/A,");
            }

            print(state.files.eio, "{:.3R},", state.dataHeatBal->People(Loop).FractionRadiant);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->People(Loop).FractionConvected);
            if (state.dataHeatBal->People(Loop).UserSpecSensFrac == DataGlobalConstants::AutoCalculate) {
                print(state.files.eio, "AutoCalculate,");
            } else {
                print(state.files.eio, "{:.3R},", state.dataHeatBal->People(Loop).UserSpecSensFrac);
            }
            print(state.files.eio, "{},", GetScheduleName(state, state.dataHeatBal->People(Loop).ActivityLevelPtr));

            if (state.dataHeatBal->People(Loop).Show55Warning) {
                print(state.files.eio, "Yes,");
            } else {
                print(state.files.eio, "No,");
            }
            print(state.files.eio, "{:.4R},", state.dataHeatBal->People(Loop).CO2RateFactor);
            print(state.files.eio, "{:.0R},", state.dataHeatBal->People(Loop).NomMinNumberPeople);

            if (state.dataHeatBal->People(Loop).Fanger || state.dataHeatBal->People(Loop).Pierce || state.dataHeatBal->People(Loop).KSU ||
                state.dataHeatBal->People(Loop).CoolingEffectASH55 || state.dataHeatBal->People(Loop).AnkleDraftASH55) {
                print(state.files.eio, "{:.0R},", state.dataHeatBal->People(Loop).NomMaxNumberPeople);

                if (state.dataHeatBal->People(Loop).MRTCalcType == ZoneAveraged) {
                    print(state.files.eio, "Zone Averaged,");
                } else if (state.dataHeatBal->People(Loop).MRTCalcType == SurfaceWeighted) {
                    print(state.files.eio, "Surface Weighted,");
                } else if (state.dataHeatBal->People(Loop).MRTCalcType == AngleFactor) {
                    print(state.files.eio, "Angle Factor,");
                } else {
                    print(state.files.eio, "N/A,");
                }
                print(state.files.eio, "{},", GetScheduleName(state, state.dataHeatBal->People(Loop).WorkEffPtr));

                if (state.dataHeatBal->People(Loop).ClothingType == 1) {
                    print(state.files.eio, "Clothing Insulation Schedule,");
                } else if (state.dataHeatBal->People(Loop).ClothingType == 2) {
                    print(state.files.eio, "Dynamic Clothing Model ASHRAE55,");
                } else if (state.dataHeatBal->People(Loop).ClothingType == 3) {
                    print(state.files.eio, "Calculation Method Schedule,");
                } else {
                    print(state.files.eio, "N/A,");
                }

                if (state.dataHeatBal->People(Loop).ClothingType == 3) {
                    print(state.files.eio, "{},", GetScheduleName(state, state.dataHeatBal->People(Loop).ClothingMethodPtr));
                } else {
                    print(state.files.eio, "N/A,");
                }

                print(state.files.eio, "{},", GetScheduleName(state, state.dataHeatBal->People(Loop).ClothingPtr));
                print(state.files.eio, "{},", GetScheduleName(state, state.dataHeatBal->People(Loop).AirVelocityPtr));

                if (state.dataHeatBal->People(Loop).Fanger) {
                    print(state.files.eio, "Yes,");
                } else {
                    print(state.files.eio, "No,");
                }
                if (state.dataHeatBal->People(Loop).Pierce) {
                    print(state.files.eio, "Yes,");
                } else {
                    print(state.files.eio, "No,");
                }
                if (state.dataHeatBal->People(Loop).KSU) {
                    print(state.files.eio, "Yes,");
                } else {
                    print(state.files.eio, "No,");
                }
                if (state.dataHeatBal->People(Loop).CoolingEffectASH55) {
                    print(state.files.eio, "Yes,");
                } else {
                    print(state.files.eio, "No,");
                }
                if (state.dataHeatBal->People(Loop).AnkleDraftASH55) {
                    print(state.files.eio, "Yes\n");
                } else {
                    print(state.files.eio, "No\n");
                }
            } else {
                print(state.files.eio, "{:.0R}\n", state.dataHeatBal->People(Loop).NomMaxNumberPeople);
            }
        }
        for (Loop = 1; Loop <= state.dataHeatBal->TotLights; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "Lights",
                      "Lighting Level {W},Lights/Floor Area {W/m2},Lights per person {W/person},Fraction Return "
                      "Air,Fraction Radiant,Fraction Short Wave,Fraction Convected,Fraction Replaceable,End-Use "
                      "Category,Nominal Minimum Lighting Level {W},Nominal Maximum Lighting Level {W}\n");
            }

            ZoneNum = state.dataHeatBal->Lights(Loop).ZonePtr;

            if (ZoneNum == 0) {
                print(state.files.eio, "Lights-Illegal Zone specified", state.dataHeatBal->Lights(Loop).Name);
                continue;
            }
            print(state.files.eio,
                  Format_722,
                  "Lights",
                  state.dataHeatBal->Lights(Loop).Name,
                  GetScheduleName(state, state.dataHeatBal->Lights(Loop).SchedPtr),
                  state.dataHeatBal->Zone(ZoneNum).Name,
                  state.dataHeatBal->Zone(ZoneNum).FloorArea,
                  state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->Lights(Loop).DesignLevel);

            print_and_divide_if_greater_than_zero(state.dataHeatBal->Lights(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).FloorArea);
            print_and_divide_if_greater_than_zero(state.dataHeatBal->Lights(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->Lights(Loop).FractionReturnAir);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->Lights(Loop).FractionRadiant);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->Lights(Loop).FractionShortWave);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->Lights(Loop).FractionConvected);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->Lights(Loop).FractionReplaceable);
            print(state.files.eio, "{},", state.dataHeatBal->Lights(Loop).EndUseSubcategory);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->Lights(Loop).NomMinDesignLevel);
            print(state.files.eio, "{:.3R}\n", state.dataHeatBal->Lights(Loop).NomMaxDesignLevel);
        }
        for (Loop = 1; Loop <= state.dataHeatBal->TotElecEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "ElectricEquipment",
                      "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction "
                      "Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}\n");
            }

            ZoneNum = state.dataHeatBal->ZoneElectric(Loop).ZonePtr;

            if (ZoneNum == 0) {
                print(state.files.eio, Format_724, "Electric Equipment-Illegal Zone specified", state.dataHeatBal->ZoneElectric(Loop).Name);
                continue;
            }
            print(state.files.eio,
                  Format_722,
                  "ElectricEquipment",
                  state.dataHeatBal->ZoneElectric(Loop).Name,
                  GetScheduleName(state, state.dataHeatBal->ZoneElectric(Loop).SchedPtr),
                  state.dataHeatBal->Zone(ZoneNum).Name,
                  state.dataHeatBal->Zone(ZoneNum).FloorArea,
                  state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneElectric(Loop).DesignLevel);

            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneElectric(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).FloorArea);
            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneElectric(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneElectric(Loop).FractionLatent);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneElectric(Loop).FractionRadiant);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneElectric(Loop).FractionLost);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneElectric(Loop).FractionConvected);
            print(state.files.eio, "{},", state.dataHeatBal->ZoneElectric(Loop).EndUseSubcategory);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneElectric(Loop).NomMinDesignLevel);
            print(state.files.eio, "{:.3R}\n", state.dataHeatBal->ZoneElectric(Loop).NomMaxDesignLevel);
        }
        for (Loop = 1; Loop <= state.dataHeatBal->TotGasEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "GasEquipment",
                      "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction "
                      "Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}\n");
            }

            ZoneNum = state.dataHeatBal->ZoneGas(Loop).ZonePtr;

            if (ZoneNum == 0) {
                print(state.files.eio, Format_724, "Gas Equipment-Illegal Zone specified", state.dataHeatBal->ZoneGas(Loop).Name);
                continue;
            }

            print(state.files.eio,
                  Format_722,
                  "GasEquipment",
                  state.dataHeatBal->ZoneGas(Loop).Name,
                  GetScheduleName(state, state.dataHeatBal->ZoneGas(Loop).SchedPtr),
                  state.dataHeatBal->Zone(ZoneNum).Name,
                  state.dataHeatBal->Zone(ZoneNum).FloorArea,
                  state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneGas(Loop).DesignLevel);

            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneGas(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).FloorArea);
            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneGas(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneGas(Loop).FractionLatent);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneGas(Loop).FractionRadiant);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneGas(Loop).FractionLost);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneGas(Loop).FractionConvected);
            print(state.files.eio, "{},", state.dataHeatBal->ZoneGas(Loop).EndUseSubcategory);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneGas(Loop).NomMinDesignLevel);
            print(state.files.eio, "{:.3R}\n", state.dataHeatBal->ZoneGas(Loop).NomMaxDesignLevel);
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotHWEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "HotWaterEquipment",
                      "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction "
                      "Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}\n");
            }

            ZoneNum = state.dataHeatBal->ZoneHWEq(Loop).ZonePtr;

            if (ZoneNum == 0) {
                print(state.files.eio, Format_724, "Hot Water Equipment-Illegal Zone specified", state.dataHeatBal->ZoneHWEq(Loop).Name);
                continue;
            }

            print(state.files.eio,
                  Format_722,
                  "HotWaterEquipment",
                  state.dataHeatBal->ZoneHWEq(Loop).Name,
                  GetScheduleName(state, state.dataHeatBal->ZoneHWEq(Loop).SchedPtr),
                  state.dataHeatBal->Zone(ZoneNum).Name,
                  state.dataHeatBal->Zone(ZoneNum).FloorArea,
                  state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneHWEq(Loop).DesignLevel);

            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneHWEq(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).FloorArea);
            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneHWEq(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneHWEq(Loop).FractionLatent);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneHWEq(Loop).FractionRadiant);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneHWEq(Loop).FractionLost);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneHWEq(Loop).FractionConvected);
            print(state.files.eio, "{},", state.dataHeatBal->ZoneHWEq(Loop).EndUseSubcategory);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneHWEq(Loop).NomMinDesignLevel);
            print(state.files.eio, "{:.3R}\n", state.dataHeatBal->ZoneHWEq(Loop).NomMaxDesignLevel);
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotStmEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "SteamEquipment",
                      "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction "
                      "Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}\n");
            }

            ZoneNum = state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr;

            if (ZoneNum == 0) {
                print(state.files.eio, Format_724, "Steam Equipment-Illegal Zone specified", state.dataHeatBal->ZoneSteamEq(Loop).Name);
                continue;
            }

            print(state.files.eio,
                  Format_722,
                  "SteamEquipment",
                  state.dataHeatBal->ZoneSteamEq(Loop).Name,
                  GetScheduleName(state, state.dataHeatBal->ZoneSteamEq(Loop).SchedPtr),
                  state.dataHeatBal->Zone(ZoneNum).Name,
                  state.dataHeatBal->Zone(ZoneNum).FloorArea,
                  state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneSteamEq(Loop).DesignLevel);

            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneSteamEq(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).FloorArea);
            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneSteamEq(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneSteamEq(Loop).FractionLatent);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneSteamEq(Loop).FractionRadiant);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneSteamEq(Loop).FractionLost);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneSteamEq(Loop).FractionConvected);
            print(state.files.eio, "{},", state.dataHeatBal->ZoneSteamEq(Loop).EndUseSubcategory);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneSteamEq(Loop).NomMinDesignLevel);
            print(state.files.eio, "{:.3R}\n", state.dataHeatBal->ZoneSteamEq(Loop).NomMaxDesignLevel);
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotOthEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "OtherEquipment",
                      "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction "
                      "Lost,Fraction Convected,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}\n");
            }

            ZoneNum = state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr;

            if (ZoneNum == 0) {
                print(state.files.eio, Format_724, "Other Equipment-Illegal Zone specified", state.dataHeatBal->ZoneOtherEq(Loop).Name);
                continue;
            }

            print(state.files.eio,
                  Format_722,
                  "OtherEquipment",
                  state.dataHeatBal->ZoneOtherEq(Loop).Name,
                  GetScheduleName(state, state.dataHeatBal->ZoneOtherEq(Loop).SchedPtr),
                  state.dataHeatBal->Zone(ZoneNum).Name,
                  state.dataHeatBal->Zone(ZoneNum).FloorArea,
                  state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel);

            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).FloorArea);
            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel, state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneOtherEq(Loop).FractionLatent);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneOtherEq(Loop).FractionRadiant);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneOtherEq(Loop).FractionLost);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneOtherEq(Loop).FractionConvected);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneOtherEq(Loop).NomMinDesignLevel);
            print(state.files.eio, "{:.3R}\n", state.dataHeatBal->ZoneOtherEq(Loop).NomMaxDesignLevel);
        }

        for (Loop = 1; Loop <= state.dataHeatBal->NumZoneITEqStatements; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "ElectricEquipment:ITE:AirCooled",
                      "Equipment Level {W},"
                      "Equipment/Floor Area {W/m2},Equipment per person {W/person},"
                      "Fraction Convected,CPU End-Use SubCategory,Fan End-Use SubCategory,UPS End-Use SubCategory,"
                      "Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}, Design Air Volume Flow Rate {m3/s}\n");
            }

            ZoneNum = state.dataHeatBal->ZoneITEq(Loop).ZonePtr;

            if (ZoneNum == 0) {
                print(state.files.eio, Format_724, "ElectricEquipment:ITE:AirCooled-Illegal Zone specified", state.dataHeatBal->ZoneITEq(Loop).Name);
                continue;
            }
            print(state.files.eio,
                  Format_722,
                  "ElectricEquipment:ITE:AirCooled",
                  state.dataHeatBal->ZoneITEq(Loop).Name,
                  GetScheduleName(state, state.dataHeatBal->ZoneITEq(Loop).OperSchedPtr),
                  state.dataHeatBal->Zone(ZoneNum).Name,
                  state.dataHeatBal->Zone(ZoneNum).FloorArea,
                  state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower);

            print_and_divide_if_greater_than_zero(state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower, state.dataHeatBal->Zone(ZoneNum).FloorArea);

            // ElectricEquipment:ITE:AirCooled is 100% convective
            print(state.files.eio, "1.0,");

            print(state.files.eio, "{},", state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryCPU);
            print(state.files.eio, "{},", state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryFan);
            print(state.files.eio, "{},", state.dataHeatBal->ZoneITEq(Loop).EndUseSubcategoryUPS);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneITEq(Loop).NomMinDesignLevel);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneITEq(Loop).NomMaxDesignLevel);
            print(state.files.eio, "{:.10R}\n", state.dataHeatBal->ZoneITEq(Loop).DesignAirVolFlowRate);
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotBBHeat; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "Outdoor Controlled Baseboard Heat",
                      "Capacity at Low Temperature {W},Low Temperature {C},Capacity at High Temperature "
                      "{W},High Temperature {C},Fraction Radiant,Fraction Convected,End-Use Subcategory\n");
            }

            ZoneNum = state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr;

            if (ZoneNum == 0) {
                print(state.files.eio,
                      Format_724,
                      "Outdoor Controlled Baseboard Heat-Illegal Zone specified",
                      state.dataHeatBal->ZoneBBHeat(Loop).Name);
                continue;
            }
            print(state.files.eio,
                  Format_722,
                  "Outdoor Controlled Baseboard Heat",
                  state.dataHeatBal->ZoneBBHeat(Loop).Name,
                  GetScheduleName(state, state.dataHeatBal->ZoneBBHeat(Loop).SchedPtr),
                  state.dataHeatBal->Zone(ZoneNum).Name,
                  state.dataHeatBal->Zone(ZoneNum).FloorArea,
                  state.dataHeatBal->Zone(ZoneNum).TotOccupants);

            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneBBHeat(Loop).CapatLowTemperature);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneBBHeat(Loop).LowTemperature);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneBBHeat(Loop).CapatHighTemperature);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneBBHeat(Loop).HighTemperature);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneBBHeat(Loop).FractionRadiant);
            print(state.files.eio, "{:.3R},", state.dataHeatBal->ZoneBBHeat(Loop).FractionConvected);
            print(state.files.eio, "{}\n", state.dataHeatBal->ZoneBBHeat(Loop).EndUseSubcategory);
        }
    }

    void InitInternalHeatGains(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       November 1998, FW: add adjustment to elec lights for dayltg controls
        //                      August 2003, FCW: add optional calculation of light-to-return fraction
        //                       as a function of return plenum air temperature.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sets up the zone internal heat gains
        // that are independent of the zone air temperature.

        // Using/Aliasing
        using namespace ScheduleManager;
        using DaylightingDevices::FigureTDDZoneGains;
        using FuelCellElectricGenerator::FigureFuelCellZoneGains;
        using MicroCHPElectricGenerator::FigureMicroCHPZoneGains;
        using OutputReportTabular::AllocateLoadComponentArrays;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using RefrigeratedCase::FigureRefrigerationZoneGains;
        using WaterThermalTanks::CalcWaterThermalTankZoneGains;
        using WaterUse::CalcWaterUseZoneGains;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D<Real64> const C(
            9, {6.4611027, 0.946892, 0.0000255737, 7.139322, -0.0627909, 0.0000589271, -0.198550, 0.000940018, -0.00000149532});
        static ZoneCatEUseData const zeroZoneCatEUse; // For initialization

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ActivityLevel_WperPerson; // Units on Activity Level (Schedule)
        Real64 NumberOccupants;          // Number of occupants
        int Loop;
        Real64 Q;                  // , QR
        Real64 TotalPeopleGain;    // Total heat gain from people (intermediate calculational variable)
        Real64 SensiblePeopleGain; // Sensible heat gain from people (intermediate calculational variable)
        Real64 FractionConvected;  // For general lighting, fraction of heat from lights convected to zone air
        Real64 FractionReturnAir;  // For general lighting, fraction of heat from lights convected to zone's return air
        Real64 FractionRadiant;    // For general lighting, fraction of heat from lights to zone that is long wave
        Real64 ReturnPlenumTemp;   // Air temperature of a zone's return air plenum (C)
        Real64 pulseMultipler;     // use to create a pulse for the load component report computations

        //  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: QSA

        //  IF (.NOT. ALLOCATED(QSA)) ALLOCATE(QSA(NumOfZones))

        //  Zero out time step variables
        for (auto &e : state.dataHeatBal->ZoneIntGain) {
            e.NOFOCC = 0.0;
            e.QOCTOT = 0.0;
            e.QOCSEN = 0.0;
            e.QOCLAT = 0.0;
            e.QOCRAD = 0.0;
            e.QOCCON = 0.0;
            e.QLTSW = 0.0;
            e.QLTCRA = 0.0;
            e.QLTRAD = 0.0;
            e.QLTCON = 0.0;
            e.QLTTOT = 0.0;

            e.QEELAT = 0.0;
            e.QEERAD = 0.0;
            e.QEECON = 0.0;
            e.QEELost = 0.0;
            e.QGELAT = 0.0;
            e.QGERAD = 0.0;
            e.QGECON = 0.0;
            e.QGELost = 0.0;
            e.QBBRAD = 0.0;
            e.QBBCON = 0.0;
            e.QOELAT = 0.0;
            e.QOERAD = 0.0;
            e.QOECON = 0.0;
            e.QOELost = 0.0;
            e.QHWLAT = 0.0;
            e.QHWRAD = 0.0;
            e.QHWCON = 0.0;
            e.QHWLost = 0.0;
            e.QSELAT = 0.0;
            e.QSERAD = 0.0;
            e.QSECON = 0.0;
            e.QSELost = 0.0;
        }

        state.dataHeatBal->ZoneIntEEuse = zeroZoneCatEUse; // Set all member arrays to zeros

        for (auto &e : state.dataHeatBal->ZnRpt) {
            e.LtsPower = 0.0;
            e.ElecPower = 0.0;
            e.GasPower = 0.0;
            e.HWPower = 0.0;
            e.SteamPower = 0.0;
            e.BaseHeatPower = 0.0;
            e.CO2Rate = 0.0;
        }

        for (auto &e : state.dataHeatBal->ZonePreDefRep) {
            e.NumOcc = 0.0;
        }

        //  QSA = 0.0

        // Process Internal Heat Gains, People done below
        // Occupant Stuff
        //   METHOD:
        //       The function is based on a curve fit to data presented in
        //       Table 48 'Heat Gain From People' of Chapter 1 of the 'Carrier
        //       Handbook of Air Conditioning System Design', 1965.  Values of
        //       Sensible gain were obtained from the table at average adjusted
        //       metabolic rates 350, 400, 450, 500, 750, 850, 1000, and
        //       1450 Btu/hr each at temperatures 82, 80, 78, 75, and 70F.
        //       Sensible gains of 0.0 at 96F and equal to the metabolic rate
        //       at 30F were assumed in order to give reasonable values beyond
        //       The reported temperature range.
        for (Loop = 1; Loop <= state.dataHeatBal->TotPeople; ++Loop) {
            int NZ = state.dataHeatBal->People(Loop).ZonePtr;
            NumberOccupants =
                state.dataHeatBal->People(Loop).NumberOfPeople * GetCurrentScheduleValue(state, state.dataHeatBal->People(Loop).NumberOfPeoplePtr);
            if (state.dataHeatBal->People(Loop).EMSPeopleOn) NumberOccupants = state.dataHeatBal->People(Loop).EMSNumberOfPeople;

            TotalPeopleGain = 0.0;
            SensiblePeopleGain = 0.0;

            if (NumberOccupants > 0.0) {
                ActivityLevel_WperPerson = GetCurrentScheduleValue(state, state.dataHeatBal->People(Loop).ActivityLevelPtr);
                TotalPeopleGain = NumberOccupants * ActivityLevel_WperPerson;
                // if the user did not specify a sensible fraction, calculate the sensible heat gain
                if (state.dataHeatBal->People(Loop).UserSpecSensFrac == DataGlobalConstants::AutoCalculate) {
                    if (!(state.dataRoomAirMod->IsZoneDV(NZ) || state.dataRoomAirMod->IsZoneUI(NZ))) {
                        SensiblePeopleGain =
                            NumberOccupants *
                            (C(1) + ActivityLevel_WperPerson * (C(2) + ActivityLevel_WperPerson * C(3)) +
                             state.dataHeatBalFanSys->MAT(NZ) *
                                 ((C(4) + ActivityLevel_WperPerson * (C(5) + ActivityLevel_WperPerson * C(6))) +
                                  state.dataHeatBalFanSys->MAT(NZ) * (C(7) + ActivityLevel_WperPerson * (C(8) + ActivityLevel_WperPerson * C(9)))));
                    } else { // UCSD - DV or UI
                        SensiblePeopleGain =
                            NumberOccupants *
                            (C(1) + ActivityLevel_WperPerson * (C(2) + ActivityLevel_WperPerson * C(3)) +
                             state.dataRoomAirMod->TCMF(NZ) *
                                 ((C(4) + ActivityLevel_WperPerson * (C(5) + ActivityLevel_WperPerson * C(6))) +
                                  state.dataRoomAirMod->TCMF(NZ) * (C(7) + ActivityLevel_WperPerson * (C(8) + ActivityLevel_WperPerson * C(9)))));
                    }
                } else { // if the user did specify a sensible fraction, use it
                    SensiblePeopleGain = TotalPeopleGain * state.dataHeatBal->People(Loop).UserSpecSensFrac;
                }

                if (SensiblePeopleGain > TotalPeopleGain) SensiblePeopleGain = TotalPeopleGain;
                if (SensiblePeopleGain < 0.0) SensiblePeopleGain = 0.0;

                // For predefined tabular reports related to outside air ventilation
                state.dataHeatBal->ZonePreDefRep(NZ).isOccupied = true; // set flag to occupied to be used in tabular reporting for ventilation
                state.dataHeatBal->ZonePreDefRep(NZ).NumOcc += NumberOccupants;
                state.dataHeatBal->ZonePreDefRep(NZ).NumOccAccum += NumberOccupants * state.dataGlobal->TimeStepZone;
                state.dataHeatBal->ZonePreDefRep(NZ).NumOccAccumTime += state.dataGlobal->TimeStepZone;
            } else {
                state.dataHeatBal->ZonePreDefRep(NZ).isOccupied = false; // set flag to occupied to be used in tabular reporting for ventilation
            }

            state.dataHeatBal->People(Loop).NumOcc = NumberOccupants;
            state.dataHeatBal->People(Loop).RadGainRate = SensiblePeopleGain * state.dataHeatBal->People(Loop).FractionRadiant;
            state.dataHeatBal->People(Loop).ConGainRate = SensiblePeopleGain * state.dataHeatBal->People(Loop).FractionConvected;
            state.dataHeatBal->People(Loop).SenGainRate = SensiblePeopleGain;
            state.dataHeatBal->People(Loop).LatGainRate = TotalPeopleGain - SensiblePeopleGain;
            state.dataHeatBal->People(Loop).TotGainRate = TotalPeopleGain;
            state.dataHeatBal->People(Loop).CO2GainRate = TotalPeopleGain * state.dataHeatBal->People(Loop).CO2RateFactor;

            state.dataHeatBal->ZoneIntGain(NZ).NOFOCC += state.dataHeatBal->People(Loop).NumOcc;
            state.dataHeatBal->ZoneIntGain(NZ).QOCRAD += state.dataHeatBal->People(Loop).RadGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QOCCON += state.dataHeatBal->People(Loop).ConGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QOCSEN += state.dataHeatBal->People(Loop).SenGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QOCLAT += state.dataHeatBal->People(Loop).LatGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QOCTOT += state.dataHeatBal->People(Loop).TotGainRate;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotLights; ++Loop) {
            int NZ = state.dataHeatBal->Lights(Loop).ZonePtr;
            Q = state.dataHeatBal->Lights(Loop).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->Lights(Loop).SchedPtr);

            if (state.dataDaylightingData->ZoneDaylight(NZ).DaylightMethod == DataDaylighting::iDaylightingMethod::SplitFluxDaylighting ||
                state.dataDaylightingData->ZoneDaylight(NZ).DaylightMethod == DataDaylighting::iDaylightingMethod::DElightDaylighting) {

                if (state.dataHeatBal->Lights(Loop).FractionReplaceable > 0.0) { // FractionReplaceable can only be 0 or 1 for these models
                    Q *= state.dataDaylightingData->ZoneDaylight(NZ).ZonePowerReductionFactor;
                }
            }

            // Reduce lighting power due to demand limiting
            if (state.dataHeatBal->Lights(Loop).ManageDemand && (Q > state.dataHeatBal->Lights(Loop).DemandLimit))
                Q = state.dataHeatBal->Lights(Loop).DemandLimit;

            // Set Q to EMS override if being called for by EMs
            if (state.dataHeatBal->Lights(Loop).EMSLightsOn) Q = state.dataHeatBal->Lights(Loop).EMSLightingPower;

            FractionConvected = state.dataHeatBal->Lights(Loop).FractionConvected;
            FractionReturnAir = state.dataHeatBal->Lights(Loop).FractionReturnAir;
            FractionRadiant = state.dataHeatBal->Lights(Loop).FractionRadiant;
            if (state.dataHeatBal->Lights(Loop).FractionReturnAirIsCalculated && !state.dataGlobal->ZoneSizingCalc &&
                state.dataGlobal->SimTimeSteps > 1) {
                // Calculate FractionReturnAir based on conditions in the zone's return air plenum, if there is one.
                if (state.dataHeatBal->Zone(NZ).IsControlled) {
                    int retNum = state.dataHeatBal->Lights(Loop).ZoneReturnNum;
                    int ReturnZonePlenumCondNum = state.dataZoneEquip->ZoneEquipConfig(NZ).ReturnNodePlenumNum(retNum);
                    if (ReturnZonePlenumCondNum > 0) {
                        ReturnPlenumTemp = state.dataZonePlenum->ZoneRetPlenCond(ReturnZonePlenumCondNum).ZoneTemp;
                        FractionReturnAir = state.dataHeatBal->Lights(Loop).FractionReturnAirPlenTempCoeff1 -
                                            state.dataHeatBal->Lights(Loop).FractionReturnAirPlenTempCoeff2 * ReturnPlenumTemp;
                        FractionReturnAir = max(0.0, min(1.0, FractionReturnAir));
                        if (FractionReturnAir >= (1.0 - state.dataHeatBal->Lights(Loop).FractionShortWave)) {
                            FractionReturnAir = 1.0 - state.dataHeatBal->Lights(Loop).FractionShortWave;
                            FractionRadiant = 0.0;
                            FractionConvected = 0.0;
                        } else {
                            FractionRadiant =
                                ((1.0 - FractionReturnAir - state.dataHeatBal->Lights(Loop).FractionShortWave) /
                                 (state.dataHeatBal->Lights(Loop).FractionRadiant + state.dataHeatBal->Lights(Loop).FractionConvected)) *
                                state.dataHeatBal->Lights(Loop).FractionRadiant;
                            FractionConvected = 1.0 - (FractionReturnAir + FractionRadiant + state.dataHeatBal->Lights(Loop).FractionShortWave);
                        }
                    }
                }
            }

            state.dataHeatBal->Lights(Loop).Power = Q;
            state.dataHeatBal->Lights(Loop).RadGainRate = Q * FractionRadiant;
            state.dataHeatBal->Lights(Loop).VisGainRate = Q * state.dataHeatBal->Lights(Loop).FractionShortWave;
            state.dataHeatBal->Lights(Loop).ConGainRate = Q * FractionConvected;
            state.dataHeatBal->Lights(Loop).RetAirGainRate = Q * FractionReturnAir;
            state.dataHeatBal->Lights(Loop).TotGainRate = Q;

            state.dataHeatBal->ZnRpt(NZ).LtsPower += state.dataHeatBal->Lights(Loop).Power;
            state.dataHeatBal->ZoneIntGain(NZ).QLTRAD += state.dataHeatBal->Lights(Loop).RadGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QLTSW += state.dataHeatBal->Lights(Loop).VisGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QLTCON += state.dataHeatBal->Lights(Loop).ConGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QLTCRA += state.dataHeatBal->Lights(Loop).RetAirGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QLTTOT += state.dataHeatBal->Lights(Loop).TotGainRate;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotElecEquip; ++Loop) {
            Q = state.dataHeatBal->ZoneElectric(Loop).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->ZoneElectric(Loop).SchedPtr);

            // Reduce equipment power due to demand limiting
            if (state.dataHeatBal->ZoneElectric(Loop).ManageDemand && (Q > state.dataHeatBal->ZoneElectric(Loop).DemandLimit))
                Q = state.dataHeatBal->ZoneElectric(Loop).DemandLimit;

            // Set Q to EMS override if being called for by EMs
            if (state.dataHeatBal->ZoneElectric(Loop).EMSZoneEquipOverrideOn) Q = state.dataHeatBal->ZoneElectric(Loop).EMSEquipPower;

            state.dataHeatBal->ZoneElectric(Loop).Power = Q;
            state.dataHeatBal->ZoneElectric(Loop).RadGainRate = Q * state.dataHeatBal->ZoneElectric(Loop).FractionRadiant;
            state.dataHeatBal->ZoneElectric(Loop).ConGainRate = Q * state.dataHeatBal->ZoneElectric(Loop).FractionConvected;
            state.dataHeatBal->ZoneElectric(Loop).LatGainRate = Q * state.dataHeatBal->ZoneElectric(Loop).FractionLatent;
            state.dataHeatBal->ZoneElectric(Loop).LostRate = Q * state.dataHeatBal->ZoneElectric(Loop).FractionLost;
            state.dataHeatBal->ZoneElectric(Loop).TotGainRate = Q - state.dataHeatBal->ZoneElectric(Loop).LostRate;

            int NZ = state.dataHeatBal->ZoneElectric(Loop).ZonePtr;
            state.dataHeatBal->ZnRpt(NZ).ElecPower += state.dataHeatBal->ZoneElectric(Loop).Power;
            state.dataHeatBal->ZoneIntGain(NZ).QEERAD += state.dataHeatBal->ZoneElectric(Loop).RadGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QEECON += state.dataHeatBal->ZoneElectric(Loop).ConGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QEELAT += state.dataHeatBal->ZoneElectric(Loop).LatGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QEELost += state.dataHeatBal->ZoneElectric(Loop).LostRate;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotGasEquip; ++Loop) {
            Q = state.dataHeatBal->ZoneGas(Loop).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->ZoneGas(Loop).SchedPtr);

            // Set Q to EMS override if being called for by EMs
            if (state.dataHeatBal->ZoneGas(Loop).EMSZoneEquipOverrideOn) Q = state.dataHeatBal->ZoneGas(Loop).EMSEquipPower;

            state.dataHeatBal->ZoneGas(Loop).Power = Q;
            state.dataHeatBal->ZoneGas(Loop).RadGainRate = Q * state.dataHeatBal->ZoneGas(Loop).FractionRadiant;
            state.dataHeatBal->ZoneGas(Loop).ConGainRate = Q * state.dataHeatBal->ZoneGas(Loop).FractionConvected;
            state.dataHeatBal->ZoneGas(Loop).LatGainRate = Q * state.dataHeatBal->ZoneGas(Loop).FractionLatent;
            state.dataHeatBal->ZoneGas(Loop).LostRate = Q * state.dataHeatBal->ZoneGas(Loop).FractionLost;
            state.dataHeatBal->ZoneGas(Loop).TotGainRate = Q - state.dataHeatBal->ZoneGas(Loop).LostRate;
            state.dataHeatBal->ZoneGas(Loop).CO2GainRate = Q * state.dataHeatBal->ZoneGas(Loop).CO2RateFactor;

            int NZ = state.dataHeatBal->ZoneGas(Loop).ZonePtr;
            state.dataHeatBal->ZnRpt(NZ).GasPower += state.dataHeatBal->ZoneGas(Loop).Power;
            state.dataHeatBal->ZoneIntGain(NZ).QGERAD += state.dataHeatBal->ZoneGas(Loop).RadGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QGECON += state.dataHeatBal->ZoneGas(Loop).ConGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QGELAT += state.dataHeatBal->ZoneGas(Loop).LatGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QGELost += state.dataHeatBal->ZoneGas(Loop).LostRate;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotOthEquip; ++Loop) {
            Q = state.dataHeatBal->ZoneOtherEq(Loop).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->ZoneOtherEq(Loop).SchedPtr);

            // Set Q to EMS override if being called for by EMs
            if (state.dataHeatBal->ZoneOtherEq(Loop).EMSZoneEquipOverrideOn) Q = state.dataHeatBal->ZoneOtherEq(Loop).EMSEquipPower;

            state.dataHeatBal->ZoneOtherEq(Loop).Power = Q;
            state.dataHeatBal->ZoneOtherEq(Loop).RadGainRate = Q * state.dataHeatBal->ZoneOtherEq(Loop).FractionRadiant;
            state.dataHeatBal->ZoneOtherEq(Loop).ConGainRate = Q * state.dataHeatBal->ZoneOtherEq(Loop).FractionConvected;
            state.dataHeatBal->ZoneOtherEq(Loop).LatGainRate = Q * state.dataHeatBal->ZoneOtherEq(Loop).FractionLatent;
            state.dataHeatBal->ZoneOtherEq(Loop).LostRate = Q * state.dataHeatBal->ZoneOtherEq(Loop).FractionLost;
            state.dataHeatBal->ZoneOtherEq(Loop).TotGainRate = Q - state.dataHeatBal->ZoneOtherEq(Loop).LostRate;

            int NZ = state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr;
            state.dataHeatBal->ZoneIntGain(NZ).QOERAD += state.dataHeatBal->ZoneOtherEq(Loop).RadGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QOECON += state.dataHeatBal->ZoneOtherEq(Loop).ConGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QOELAT += state.dataHeatBal->ZoneOtherEq(Loop).LatGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QOELost += state.dataHeatBal->ZoneOtherEq(Loop).LostRate;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotHWEquip; ++Loop) {
            Q = state.dataHeatBal->ZoneHWEq(Loop).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->ZoneHWEq(Loop).SchedPtr);

            // Set Q to EMS override if being called for by EMs
            if (state.dataHeatBal->ZoneHWEq(Loop).EMSZoneEquipOverrideOn) Q = state.dataHeatBal->ZoneHWEq(Loop).EMSEquipPower;

            state.dataHeatBal->ZoneHWEq(Loop).Power = Q;
            state.dataHeatBal->ZoneHWEq(Loop).RadGainRate = Q * state.dataHeatBal->ZoneHWEq(Loop).FractionRadiant;
            state.dataHeatBal->ZoneHWEq(Loop).ConGainRate = Q * state.dataHeatBal->ZoneHWEq(Loop).FractionConvected;
            state.dataHeatBal->ZoneHWEq(Loop).LatGainRate = Q * state.dataHeatBal->ZoneHWEq(Loop).FractionLatent;
            state.dataHeatBal->ZoneHWEq(Loop).LostRate = Q * state.dataHeatBal->ZoneHWEq(Loop).FractionLost;
            state.dataHeatBal->ZoneHWEq(Loop).TotGainRate = Q - state.dataHeatBal->ZoneHWEq(Loop).LostRate;

            int NZ = state.dataHeatBal->ZoneHWEq(Loop).ZonePtr;
            state.dataHeatBal->ZnRpt(NZ).HWPower += state.dataHeatBal->ZoneHWEq(Loop).Power;
            state.dataHeatBal->ZoneIntGain(NZ).QHWRAD += state.dataHeatBal->ZoneHWEq(Loop).RadGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QHWCON += state.dataHeatBal->ZoneHWEq(Loop).ConGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QHWLAT += state.dataHeatBal->ZoneHWEq(Loop).LatGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QHWLost += state.dataHeatBal->ZoneHWEq(Loop).LostRate;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotStmEquip; ++Loop) {
            Q = state.dataHeatBal->ZoneSteamEq(Loop).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->ZoneSteamEq(Loop).SchedPtr);

            // Set Q to EMS override if being called for by EMs
            if (state.dataHeatBal->ZoneSteamEq(Loop).EMSZoneEquipOverrideOn) Q = state.dataHeatBal->ZoneSteamEq(Loop).EMSEquipPower;

            state.dataHeatBal->ZoneSteamEq(Loop).Power = Q;
            state.dataHeatBal->ZoneSteamEq(Loop).RadGainRate = Q * state.dataHeatBal->ZoneSteamEq(Loop).FractionRadiant;
            state.dataHeatBal->ZoneSteamEq(Loop).ConGainRate = Q * state.dataHeatBal->ZoneSteamEq(Loop).FractionConvected;
            state.dataHeatBal->ZoneSteamEq(Loop).LatGainRate = Q * state.dataHeatBal->ZoneSteamEq(Loop).FractionLatent;
            state.dataHeatBal->ZoneSteamEq(Loop).LostRate = Q * state.dataHeatBal->ZoneSteamEq(Loop).FractionLost;
            state.dataHeatBal->ZoneSteamEq(Loop).TotGainRate = Q - state.dataHeatBal->ZoneSteamEq(Loop).LostRate;

            int NZ = state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr;
            state.dataHeatBal->ZnRpt(NZ).SteamPower += state.dataHeatBal->ZoneSteamEq(Loop).Power;
            state.dataHeatBal->ZoneIntGain(NZ).QSERAD += state.dataHeatBal->ZoneSteamEq(Loop).RadGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QSECON += state.dataHeatBal->ZoneSteamEq(Loop).ConGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QSELAT += state.dataHeatBal->ZoneSteamEq(Loop).LatGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QSELost += state.dataHeatBal->ZoneSteamEq(Loop).LostRate;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotBBHeat; ++Loop) {
            int NZ = state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr;
            if (state.dataHeatBal->Zone(NZ).OutDryBulbTemp >= state.dataHeatBal->ZoneBBHeat(Loop).HighTemperature) {
                Q = 0.0;
            } else if (state.dataHeatBal->Zone(NZ).OutDryBulbTemp > state.dataHeatBal->ZoneBBHeat(Loop).LowTemperature) {
                Q = (state.dataHeatBal->Zone(NZ).OutDryBulbTemp - state.dataHeatBal->ZoneBBHeat(Loop).LowTemperature) *
                        (state.dataHeatBal->ZoneBBHeat(Loop).CapatHighTemperature - state.dataHeatBal->ZoneBBHeat(Loop).CapatLowTemperature) /
                        (state.dataHeatBal->ZoneBBHeat(Loop).HighTemperature - state.dataHeatBal->ZoneBBHeat(Loop).LowTemperature) +
                    state.dataHeatBal->ZoneBBHeat(Loop).CapatLowTemperature;
            } else {
                Q = state.dataHeatBal->ZoneBBHeat(Loop).CapatLowTemperature;
            }
            Q *= GetCurrentScheduleValue(state, state.dataHeatBal->ZoneBBHeat(Loop).SchedPtr);

            // set with EMS value if being called for.
            if (state.dataHeatBal->ZoneBBHeat(Loop).EMSZoneBaseboardOverrideOn) Q = state.dataHeatBal->ZoneBBHeat(Loop).EMSZoneBaseboardPower;

            state.dataHeatBal->ZoneBBHeat(Loop).Power = Q;
            state.dataHeatBal->ZoneBBHeat(Loop).RadGainRate = Q * state.dataHeatBal->ZoneBBHeat(Loop).FractionRadiant;
            state.dataHeatBal->ZoneBBHeat(Loop).ConGainRate = Q * state.dataHeatBal->ZoneBBHeat(Loop).FractionConvected;
            state.dataHeatBal->ZoneBBHeat(Loop).TotGainRate = Q;

            NZ = state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr;
            state.dataHeatBal->ZnRpt(NZ).BaseHeatPower += state.dataHeatBal->ZoneBBHeat(Loop).Power;
            state.dataHeatBal->ZoneIntGain(NZ).QBBRAD += state.dataHeatBal->ZoneBBHeat(Loop).RadGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QBBCON += state.dataHeatBal->ZoneBBHeat(Loop).ConGainRate;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotCO2Gen; ++Loop) {
            int NZ = state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr;
            state.dataHeatBal->ZoneCO2Gen(Loop).CO2GainRate =
                state.dataHeatBal->ZoneCO2Gen(Loop).CO2DesignRate * GetCurrentScheduleValue(state, state.dataHeatBal->ZoneCO2Gen(Loop).SchedPtr);
            state.dataHeatBal->ZnRpt(NZ).CO2Rate += state.dataHeatBal->ZoneCO2Gen(Loop).CO2GainRate;
        }

        if (state.dataHeatBal->NumZoneITEqStatements > 0) CalcZoneITEq(state);

        CalcWaterThermalTankZoneGains(state);
        PipeHeatTransfer::PipeHTData::CalcZonePipesHeatGain(state);
        CalcWaterUseZoneGains(state);
        FigureFuelCellZoneGains(state);
        FigureMicroCHPZoneGains(state);
        initializeElectricPowerServiceZoneGains(state);
        FigureTDDZoneGains(state);
        FigureRefrigerationZoneGains(state);

        // store pointer values to hold generic internal gain values constant for entire timestep
        UpdateInternalGainValues(state);

        for (int NZ = 1; NZ <= state.dataGlobal->NumOfZones; ++NZ) {

            SumAllInternalLatentGains(state, NZ, state.dataHeatBalFanSys->ZoneLatentGain(NZ));
            // Added for hybrid model
            if (state.dataHybridModel->FlagHybridModel_PC) {
                SumAllInternalLatentGainsExceptPeople(state, NZ, state.dataHeatBalFanSys->ZoneLatentGainExceptPeople(NZ));
            }
        }

        // QL is per radiant enclosure (one or more zones if grouped by air boundaries)
        for (int enclosureNum = 1; enclosureNum <= state.dataViewFactor->NumOfRadiantEnclosures; ++enclosureNum) {
            auto &thisEnclosure(state.dataViewFactor->ZoneRadiantInfo(enclosureNum));
            state.dataHeatBal->EnclRadQThermalRad(enclosureNum) = 0.0;
            for (int const zoneNum : thisEnclosure.ZoneNums) {
                Real64 zoneQL;
                SumAllInternalRadiationGains(state, zoneNum, zoneQL);
                state.dataHeatBal->EnclRadQThermalRad(enclosureNum) += zoneQL;
            }
        }

        state.dataHeatBalFanSys->SumConvHTRadSys = 0.0;

        pulseMultipler = 0.01; // the W/sqft pulse for the zone
        if (state.dataGlobal->CompLoadReportIsReq) {
            AllocateLoadComponentArrays(state);
        }
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) { // Loop through all surfaces...
            int const firstSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceFirst;
            int const lastSurf = state.dataHeatBal->Zone(zoneNum).HTSurfaceLast;
            if (firstSurf <= 0) continue;
            for (int SurfNum = firstSurf; SurfNum <= lastSurf; ++SurfNum) {
                int const radEnclosureNum = state.dataHeatBal->Zone(zoneNum).RadiantEnclosureNum;
                if (!state.dataGlobal->doLoadComponentPulseNow) {
                    state.dataHeatBal->SurfQRadThermInAbs(SurfNum) = state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum) *
                                                                     state.dataHeatBal->TMULT(radEnclosureNum) * state.dataHeatBal->ITABSF(SurfNum);
                } else {
                    state.dataInternalHeatGains->curQL = state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum);
                    // for the loads component report during the special sizing run increase the radiant portion
                    // a small amount to create a "pulse" of heat that is used for the delayed loads
                    state.dataInternalHeatGains->adjQL =
                        state.dataInternalHeatGains->curQL + state.dataViewFactor->ZoneRadiantInfo(radEnclosureNum).FloorArea * pulseMultipler;
                    // ITABSF is the Inside Thermal Absorptance
                    // TMULT is a multiplier for each zone
                    // QRadThermInAbs is the thermal radiation absorbed on inside surfaces
                    state.dataHeatBal->SurfQRadThermInAbs(SurfNum) =
                        state.dataInternalHeatGains->adjQL * state.dataHeatBal->TMULT(radEnclosureNum) * state.dataHeatBal->ITABSF(SurfNum);
                    // store the magnitude and time of the pulse
                    state.dataOutRptTab->radiantPulseTimestep(state.dataSize->CurOverallSimDay, zoneNum) =
                        (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;
                    state.dataOutRptTab->radiantPulseReceived(state.dataSize->CurOverallSimDay, SurfNum) =
                        (state.dataInternalHeatGains->adjQL - state.dataInternalHeatGains->curQL) * state.dataHeatBal->TMULT(radEnclosureNum) *
                        state.dataHeatBal->ITABSF(SurfNum) * state.dataSurface->Surface(SurfNum).Area;
                }
            }
        }
    }

    void CheckReturnAirHeatGain(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Xuan Luo
        //       DATE WRITTEN   Jan 2018

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine currently creates the values for standard "zone loads" reporting
        // from the heat balance module.

        // Using/Aliasing

        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if (state.dataHeatBal->Zone(ZoneNum).HasAdjustedReturnTempByITE && state.dataHeatBal->Zone(ZoneNum).HasLtsRetAirGain) {
                ShowFatalError(state,
                               "Return air heat gains from lights are not allowed when Air Flow Calculation Method = "
                               "FlowControlWithApproachTemperatures in zones with ITE objects.");
            }
            if (state.dataHeatBal->Zone(ZoneNum).HasAdjustedReturnTempByITE && state.dataHeatBal->Zone(ZoneNum).HasAirFlowWindowReturn) {
                ShowFatalError(state,
                               "Return air heat gains from windows are not allowed when Air Flow Calculation Method = "
                               "FlowControlWithApproachTemperatures in zones with ITE objects.");
            }
        }
    }

    void CalcZoneITEq(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         M.J. Witte
        //       DATE WRITTEN   October 2014

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the gains and other results for ElectricEquipment:ITE:AirCooled.
        // This broken into a separate subroutine, because the calculations are more detailed than the other
        // types of internal gains.

        using ScheduleManager::GetCurrentScheduleValue;
        using namespace Psychrometrics;
        using CurveManager::CurveValue;
        using DataHVACGlobals::SmallAirVolFlow;
        using DataHVACGlobals::SmallTempDiff;

        // Operating Limits for environmental class: None, A1, A2, A3, A4, B, C
        // From ASHRAE 2011 Thermal Guidelines environmental classes for Air-Cooled ITE
        static Array1D<Real64> const DBMin(7, {-99.0, 15.0, 10.0, 5.0, 5.0, 5.0, 5.0});           // Minimum dry-bulb temperature [C]
        static Array1D<Real64> const DBMax(7, {99.0, 32.0, 35.0, 40.0, 45.0, 35.0, 40.0});        // Maximum dry-bulb temperature [C]
        static Array1D<Real64> const DPMax(7, {99.0, 17.0, 21.0, 24.0, 24.0, 28.0, 28.0});        // Maximum dewpoint temperature [C]
        static Array1D<Real64> const DPMin(7, {-99.0, -99.0, -99.0, -12.0, -12.0, -99.0, -99.0}); // Minimum dewpoint temperature [C]
        static Array1D<Real64> const RHMin(7, {0.0, 20.0, 20.0, 8.0, 8.0, 8.0, 8.0});             // Minimum relative humidity [%]
        static Array1D<Real64> const RHMax(7, {99.0, 80.0, 80.0, 85.0, 90.0, 80.0, 80.0});        // Maximum relative humidity [%]

        static constexpr std::string_view RoutineName("CalcZoneITEq");
        int Loop;
        int NZ;
        int SupplyNodeNum;            // Supply air node number (if zero, then not specified)
        Real64 OperSchedFrac;         // Operating schedule fraction
        Real64 CPULoadSchedFrac;      // CPU loading schedule fraction
        Real64 AirConnection;         // Air connection type
        Real64 TSupply(0.0);          // Supply air temperature [C]
        Real64 WSupply;               // Supply air humidity ratio [kgWater/kgDryAir]
        Real64 RecircFrac;            // Recirulation fraction - current
        Real64 TRecirc;               // Recirulation air temperature [C]
        Real64 WRecirc;               // Recirulation air humidity ratio [kgWater/kgDryAir]
        Real64 TAirIn;                // Entering air dry-bulb temperature [C]
        Real64 TAirInDesign;          // Design entering air dry-bulb temperature [C]
        Real64 WAirIn;                // Entering air humidity ratio [kgWater/kgDryAir]
        Real64 TDPAirIn;              // Entering air dewpoint temperature [C]
        Real64 RHAirIn;               // Entering air relative humidity [%]
        Real64 SupplyHeatIndex;       // Supply heat index
        Real64 TAirOut;               // Leaving air temperature [C]
        Real64 AirVolFlowFrac;        // Air volume flow fraction
        Real64 AirVolFlowFracDesignT; // Air volume flow fraction at design entering air temperature
        Real64 AirVolFlowRate;        // Air volume flow rate at current density [m3/s]
        Real64 AirMassFlowRate;       // Air mass flow rate [kg/s]
        Real64 CPUPower;              // CPU power input [W]
        Real64 FanPower;              // Fan power input [W]
        Real64 UPSPower;              // UPS new power input (losses) [W]
        Real64 UPSPartLoadRatio;      // UPS part load ratio (current total power input / design total power input)
        Real64 UPSHeatGain;           // UPS convective heat gain to zone [W]
        int EnvClass;                 // Index for environmental class (None=0, A1=1, A2=2, A3=3, A4=4, B=5, C=6)

        std::map<int, std::vector<int>> ZoneITEMap;

        //  Zero out time step variables
        // Object report variables
        for (Loop = 1; Loop <= state.dataHeatBal->NumZoneITEqStatements; ++Loop) {
            state.dataHeatBal->ZoneITEq(Loop).CPUPower = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).FanPower = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).UPSPower = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).CPUPowerAtDesign = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).FanPowerAtDesign = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).UPSGainRateToZone = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).ConGainRateToZone = 0.0;

            state.dataHeatBal->ZoneITEq(Loop).CPUConsumption = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).FanConsumption = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).UPSConsumption = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).CPUEnergyAtDesign = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).FanEnergyAtDesign = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).UPSGainEnergyToZone = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).ConGainEnergyToZone = 0.0;

            state.dataHeatBal->ZoneITEq(Loop).AirVolFlowStdDensity = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).AirVolFlowCurDensity = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).AirMassFlow = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).AirInletDryBulbT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).AirInletDewpointT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).AirInletRelHum = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).AirOutletDryBulbT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).SHI = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).TimeAboveDryBulbT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).TimeBelowDryBulbT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).TimeAboveDewpointT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).TimeBelowDewpointT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).TimeAboveRH = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).TimeBelowRH = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).DryBulbTAboveDeltaT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).DryBulbTBelowDeltaT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).DewpointTAboveDeltaT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).DewpointTBelowDeltaT = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).RHAboveDeltaRH = 0.0;
            state.dataHeatBal->ZoneITEq(Loop).RHBelowDeltaRH = 0.0;
        } // ZoneITEq init loop

        // Zone total report variables
        for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
            state.dataHeatBal->ZnRpt(Loop).ITEqCPUPower = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqFanPower = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqUPSPower = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqCPUPowerAtDesign = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqFanPowerAtDesign = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqUPSGainRateToZone = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqConGainRateToZone = 0.0;

            state.dataHeatBal->ZnRpt(Loop).ITEAdjReturnTemp = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqCPUConsumption = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqFanConsumption = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqUPSConsumption = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqCPUEnergyAtDesign = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqFanEnergyAtDesign = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqUPSGainEnergyToZone = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqConGainEnergyToZone = 0.0;

            state.dataHeatBal->ZnRpt(Loop).ITEqAirVolFlowStdDensity = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqAirMassFlow = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqSHI = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqTimeOutOfOperRange = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqTimeAboveDryBulbT = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqTimeBelowDryBulbT = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqTimeAboveDewpointT = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqTimeBelowDewpointT = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqTimeAboveRH = 0.0;
            state.dataHeatBal->ZnRpt(Loop).ITEqTimeBelowRH = 0.0;

            state.dataHeatBal->ZnRpt(Loop).SumTinMinusTSup = 0.0;
            state.dataHeatBal->ZnRpt(Loop).SumToutMinusTSup = 0.0;
        } // Zone init loop

        for (Loop = 1; Loop <= state.dataHeatBal->NumZoneITEqStatements; ++Loop) {
            // Get schedules
            NZ = state.dataHeatBal->ZoneITEq(Loop).ZonePtr;
            OperSchedFrac = GetCurrentScheduleValue(state, state.dataHeatBal->ZoneITEq(Loop).OperSchedPtr);
            CPULoadSchedFrac = GetCurrentScheduleValue(state, state.dataHeatBal->ZoneITEq(Loop).CPULoadSchedPtr);

            // Determine inlet air temperature and humidity
            AirConnection = state.dataHeatBal->ZoneITEq(Loop).AirConnectionType;
            RecircFrac = 0.0;
            SupplyNodeNum = state.dataHeatBal->ZoneITEq(Loop).SupplyAirNodeNum;
            if (state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps) {
                TSupply = state.dataLoopNodes->Node(SupplyNodeNum).Temp;
                WSupply = state.dataLoopNodes->Node(SupplyNodeNum).HumRat;
                if (state.dataHeatBal->ZoneITEq(Loop).SupplyApproachTempSch != 0) {
                    TAirIn = TSupply + GetCurrentScheduleValue(state, state.dataHeatBal->ZoneITEq(Loop).SupplyApproachTempSch);
                } else {
                    TAirIn = TSupply + state.dataHeatBal->ZoneITEq(Loop).SupplyApproachTemp;
                }
                WAirIn = state.dataLoopNodes->Node(SupplyNodeNum).HumRat;
            } else {
                if (AirConnection == ITEInletAdjustedSupply) {
                    TSupply = state.dataLoopNodes->Node(SupplyNodeNum).Temp;
                    WSupply = state.dataLoopNodes->Node(SupplyNodeNum).HumRat;
                    if (state.dataHeatBal->ZoneITEq(Loop).RecircFLTCurve != 0) {
                        RecircFrac = state.dataHeatBal->ZoneITEq(Loop).DesignRecircFrac *
                                     CurveValue(state, state.dataHeatBal->ZoneITEq(Loop).RecircFLTCurve, CPULoadSchedFrac, TSupply);
                    } else {
                        RecircFrac = state.dataHeatBal->ZoneITEq(Loop).DesignRecircFrac;
                    }
                    TRecirc = state.dataHeatBalFanSys->MAT(NZ);
                    WRecirc = state.dataHeatBalFanSys->ZoneAirHumRat(NZ);
                    TAirIn = TRecirc * RecircFrac + TSupply * (1.0 - RecircFrac);
                    WAirIn = WRecirc * RecircFrac + WSupply * (1.0 - RecircFrac);
                } else if (AirConnection == ITEInletRoomAirModel) {
                    // Room air model option: TAirIn=TAirZone, according to EngineeringRef 17.1.4
                    TAirIn = state.dataHeatBalFanSys->MAT(NZ);
                    TSupply = TAirIn;
                    WAirIn = state.dataHeatBalFanSys->ZoneAirHumRat(NZ);
                } else {
                    // TAirIn = TRoomAirNodeIn, according to EngineeringRef 17.1.4
                    int ZoneAirInletNode = state.dataZoneEquip->ZoneEquipConfig(NZ).InletNode(1);
                    TSupply = state.dataLoopNodes->Node(ZoneAirInletNode).Temp;
                    TAirIn = state.dataHeatBalFanSys->MAT(NZ);
                    WAirIn = state.dataHeatBalFanSys->ZoneAirHumRat(NZ);
                }
            }
            TDPAirIn = PsyTdpFnWPb(state, WAirIn, state.dataEnvrn->StdBaroPress, RoutineName);
            RHAirIn = 100.0 * PsyRhFnTdbWPb(state, TAirIn, WAirIn, state.dataEnvrn->StdBaroPress, RoutineName); // RHAirIn is %

            // Calculate power input and airflow
            TAirInDesign = state.dataHeatBal->ZoneITEq(Loop).DesignTAirIn;

            if (state.dataGlobal->DoingSizing && state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps) {

                TAirInDesign = state.dataHeatBal->ZoneITEq(Loop).SizingTAirIn;
                if (state.dataHeatBal->ZoneITEq(Loop).SupplyApproachTempSch != 0) {
                    TAirInDesign = TAirInDesign + GetCurrentScheduleValue(state, state.dataHeatBal->ZoneITEq(Loop).SupplyApproachTempSch);
                } else {
                    TAirInDesign = TAirInDesign + state.dataHeatBal->ZoneITEq(Loop).SupplyApproachTemp;
                }
                OperSchedFrac = GetCurrentScheduleValue(state, state.dataHeatBal->ZoneITEq(Loop).OperSchedPtr);
                CPULoadSchedFrac = GetCurrentScheduleValue(state, state.dataHeatBal->ZoneITEq(Loop).CPULoadSchedPtr);
            }

            CPUPower = max(state.dataHeatBal->ZoneITEq(Loop).DesignCPUPower * OperSchedFrac *
                               CurveValue(state, state.dataHeatBal->ZoneITEq(Loop).CPUPowerFLTCurve, CPULoadSchedFrac, TAirIn),
                           0.0);
            state.dataHeatBal->ZoneITEq(Loop).CPUPowerAtDesign =
                max(state.dataHeatBal->ZoneITEq(Loop).DesignCPUPower * OperSchedFrac *
                        CurveValue(state, state.dataHeatBal->ZoneITEq(Loop).CPUPowerFLTCurve, CPULoadSchedFrac, TAirInDesign),
                    0.0);

            AirVolFlowFrac = max(CurveValue(state, state.dataHeatBal->ZoneITEq(Loop).AirFlowFLTCurve, CPULoadSchedFrac, TAirIn), 0.0);
            AirVolFlowRate = state.dataHeatBal->ZoneITEq(Loop).DesignAirVolFlowRate * OperSchedFrac * AirVolFlowFrac;
            if (AirVolFlowRate < SmallAirVolFlow) {
                AirVolFlowRate = 0.0;
            }
            AirVolFlowFracDesignT = max(CurveValue(state, state.dataHeatBal->ZoneITEq(Loop).AirFlowFLTCurve, CPULoadSchedFrac, TAirInDesign), 0.0);

            FanPower = max(state.dataHeatBal->ZoneITEq(Loop).DesignFanPower * OperSchedFrac *
                               CurveValue(state, state.dataHeatBal->ZoneITEq(Loop).FanPowerFFCurve, AirVolFlowFrac),
                           0.0);
            state.dataHeatBal->ZoneITEq(Loop).FanPowerAtDesign =
                max(state.dataHeatBal->ZoneITEq(Loop).DesignFanPower * OperSchedFrac *
                        CurveValue(state, state.dataHeatBal->ZoneITEq(Loop).FanPowerFFCurve, AirVolFlowFracDesignT),
                    0.0);

            // Calculate UPS net power input (power in less power to ITEquip) and UPS heat gain to zone
            if (state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower > 0.0) {
                UPSPartLoadRatio = (CPUPower + FanPower) / state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower;
            } else {
                UPSPartLoadRatio = 0.0;
            }
            if (state.dataHeatBal->ZoneITEq(Loop).UPSEfficFPLRCurve != 0) {
                UPSPower =
                    (CPUPower + FanPower) * max((1.0 - state.dataHeatBal->ZoneITEq(Loop).DesignUPSEfficiency *
                                                           CurveValue(state, state.dataHeatBal->ZoneITEq(Loop).UPSEfficFPLRCurve, UPSPartLoadRatio)),
                                                0.0);
            } else {
                UPSPower = (CPUPower + FanPower) * max((1.0 - state.dataHeatBal->ZoneITEq(Loop).DesignUPSEfficiency), 0.0);
            }
            UPSHeatGain = UPSPower * state.dataHeatBal->ZoneITEq(Loop).UPSLossToZoneFrac;

            // Calculate air outlet conditions and convective heat gain to zone

            AirMassFlowRate = AirVolFlowRate * PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, TAirIn, WAirIn, RoutineName);
            if (AirMassFlowRate > 0.0) {
                TAirOut = TAirIn + (CPUPower + FanPower) / AirMassFlowRate / PsyCpAirFnW(WAirIn);
            } else {
                TAirOut = TAirIn;
            }

            if (std::abs(TAirOut - TSupply) < SmallTempDiff) {
                TAirOut = TSupply;
            }

            if ((SupplyNodeNum != 0) && (TAirOut != TSupply)) {
                SupplyHeatIndex = (TAirIn - TSupply) / (TAirOut - TSupply);
            } else {
                SupplyHeatIndex = 0.0;
            }

            if (AirConnection == ITEInletAdjustedSupply || AirConnection == ITEInletZoneAirNode) {
                // If not a room air model, then all ITEquip power input is a convective heat gain to the zone heat balance, plus UPS heat gain
                state.dataHeatBal->ZoneITEq(Loop).ConGainRateToZone = CPUPower + FanPower + UPSHeatGain;
            } else if (AirConnection == ITEInletRoomAirModel) {
                // Room air model option not implemented yet - set room air model outlet node conditions here
                // If a room air model, then the only convective heat gain to the zone heat balance is the UPS heat gain
                state.dataHeatBal->ZoneITEq(Loop).ConGainRateToZone = UPSHeatGain;
            }
            if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).HasAdjustedReturnTempByITE) {
                ZoneITEMap[state.dataHeatBal->ZoneITEq(Loop).ZonePtr].push_back(Loop);
            }
            if (state.dataGlobal->DoingSizing && state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps) {
                if (state.dataHeatBal->ZoneITEq(Loop).FanPowerAtDesign + state.dataHeatBal->ZoneITEq(Loop).CPUPowerAtDesign >
                    state.dataHeatBal->ZoneITEq(Loop).DesignTotalPower) {
                    state.dataHeatBal->ZoneITEq(Loop).ConGainRateToZone =
                        state.dataHeatBal->ZoneITEq(Loop).FanPowerAtDesign + state.dataHeatBal->ZoneITEq(Loop).CPUPowerAtDesign;
                }
            }
            // Object report variables
            state.dataHeatBal->ZoneITEq(Loop).CPUPower = CPUPower;
            state.dataHeatBal->ZoneITEq(Loop).FanPower = FanPower;
            state.dataHeatBal->ZoneITEq(Loop).UPSPower = UPSPower;
            // ZoneITEq( Loop ).CPUPowerAtDesign = set above
            // ZoneITEq( Loop ).FanPowerAtDesign = set above
            state.dataHeatBal->ZoneITEq(Loop).UPSGainRateToZone = UPSHeatGain;
            // ZoneITEq( Loop ).ConGainRateToZone = set above

            state.dataHeatBal->ZnRpt(NZ).ITEqCPUPower += state.dataHeatBal->ZoneITEq(Loop).CPUPower;
            state.dataHeatBal->ZnRpt(NZ).ITEqFanPower += state.dataHeatBal->ZoneITEq(Loop).FanPower;
            state.dataHeatBal->ZnRpt(NZ).ITEqUPSPower += state.dataHeatBal->ZoneITEq(Loop).UPSPower;
            state.dataHeatBal->ZnRpt(NZ).ITEqCPUPowerAtDesign += state.dataHeatBal->ZoneITEq(Loop).CPUPowerAtDesign;
            state.dataHeatBal->ZnRpt(NZ).ITEqFanPowerAtDesign += state.dataHeatBal->ZoneITEq(Loop).FanPowerAtDesign;
            state.dataHeatBal->ZnRpt(NZ).ITEqUPSGainRateToZone += state.dataHeatBal->ZoneITEq(Loop).UPSGainRateToZone;
            state.dataHeatBal->ZnRpt(NZ).ITEqConGainRateToZone += state.dataHeatBal->ZoneITEq(Loop).ConGainRateToZone;

            state.dataHeatBal->ZoneITEq(Loop).CPUConsumption = CPUPower * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneITEq(Loop).FanConsumption = FanPower * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneITEq(Loop).UPSConsumption = UPSPower * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneITEq(Loop).CPUEnergyAtDesign =
                state.dataHeatBal->ZoneITEq(Loop).CPUPowerAtDesign * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneITEq(Loop).FanEnergyAtDesign =
                state.dataHeatBal->ZoneITEq(Loop).FanPowerAtDesign * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneITEq(Loop).UPSGainEnergyToZone = UPSHeatGain * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneITEq(Loop).ConGainEnergyToZone =
                state.dataHeatBal->ZoneITEq(Loop).ConGainRateToZone * state.dataGlobal->TimeStepZoneSec;

            state.dataHeatBal->ZnRpt(NZ).ITEqCPUConsumption += state.dataHeatBal->ZoneITEq(Loop).CPUConsumption;
            state.dataHeatBal->ZnRpt(NZ).ITEqFanConsumption += state.dataHeatBal->ZoneITEq(Loop).FanConsumption;
            state.dataHeatBal->ZnRpt(NZ).ITEqUPSConsumption += state.dataHeatBal->ZoneITEq(Loop).UPSConsumption;
            state.dataHeatBal->ZnRpt(NZ).ITEqCPUEnergyAtDesign += state.dataHeatBal->ZoneITEq(Loop).CPUEnergyAtDesign;
            state.dataHeatBal->ZnRpt(NZ).ITEqFanEnergyAtDesign += state.dataHeatBal->ZoneITEq(Loop).FanEnergyAtDesign;
            state.dataHeatBal->ZnRpt(NZ).ITEqUPSGainEnergyToZone += state.dataHeatBal->ZoneITEq(Loop).UPSGainEnergyToZone;
            state.dataHeatBal->ZnRpt(NZ).ITEqConGainEnergyToZone += state.dataHeatBal->ZoneITEq(Loop).ConGainEnergyToZone;

            state.dataHeatBal->ZoneITEq(Loop).AirVolFlowStdDensity = AirMassFlowRate * state.dataEnvrn->StdRhoAir;
            state.dataHeatBal->ZoneITEq(Loop).AirVolFlowCurDensity = AirVolFlowRate;
            state.dataHeatBal->ZoneITEq(Loop).AirMassFlow = AirMassFlowRate;
            state.dataHeatBal->ZoneITEq(Loop).AirInletDryBulbT = TAirIn;
            state.dataHeatBal->ZoneITEq(Loop).AirInletDewpointT = TDPAirIn;
            state.dataHeatBal->ZoneITEq(Loop).AirInletRelHum = RHAirIn;
            state.dataHeatBal->ZoneITEq(Loop).AirOutletDryBulbT = TAirOut;
            state.dataHeatBal->ZoneITEq(Loop).SHI = SupplyHeatIndex;

            state.dataHeatBal->ZnRpt(NZ).ITEqAirVolFlowStdDensity += state.dataHeatBal->ZoneITEq(Loop).AirVolFlowStdDensity;
            state.dataHeatBal->ZnRpt(NZ).ITEqAirMassFlow += state.dataHeatBal->ZoneITEq(Loop).AirMassFlow;
            state.dataHeatBal->ZnRpt(NZ).SumTinMinusTSup += (TAirIn - TSupply) * AirVolFlowRate;
            state.dataHeatBal->ZnRpt(NZ).SumToutMinusTSup += (TAirOut - TSupply) * AirVolFlowRate;

            // Check environmental class operating range limits (defined as parameters in this subroutine)
            EnvClass = state.dataHeatBal->ZoneITEq(Loop).Class;
            if (EnvClass > 0) {
                if (TAirIn > DBMax(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeAboveDryBulbT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).DryBulbTAboveDeltaT = TAirIn - DBMax(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeAboveDryBulbT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
                if (TAirIn < DBMin(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeBelowDryBulbT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).DryBulbTBelowDeltaT = TAirIn - DBMin(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeBelowDryBulbT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
                if (TDPAirIn > DPMax(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeAboveDewpointT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).DewpointTAboveDeltaT = TDPAirIn - DPMax(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeAboveDewpointT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
                if (TDPAirIn < DPMin(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeBelowDewpointT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).DewpointTBelowDeltaT = TDPAirIn - DPMin(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeBelowDewpointT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
                if (RHAirIn > RHMax(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeAboveRH = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).RHAboveDeltaRH = RHAirIn - RHMax(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeAboveRH = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
                if (RHAirIn < RHMin(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeBelowRH = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).RHBelowDeltaRH = RHAirIn - RHMin(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeBelowRH = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
            }

        } // ZoneITEq calc loop

        // Zone-level sensible heat index
        for (Loop = 1; Loop <= state.dataHeatBal->NumZoneITEqStatements; ++Loop) {
            int ZN = state.dataHeatBal->ZoneITEq(Loop).ZonePtr;
            if (state.dataHeatBal->ZnRpt(NZ).SumToutMinusTSup != 0.0) {
                state.dataHeatBal->ZnRpt(ZN).ITEqSHI = state.dataHeatBal->ZnRpt(NZ).SumTinMinusTSup / state.dataHeatBal->ZnRpt(NZ).SumToutMinusTSup;
            }
        }

        std::map<int, std::vector<int>>::iterator it = ZoneITEMap.begin();
        Real64 totalGain;
        Real64 totalRate;
        Real64 TAirReturn;
        while (it != ZoneITEMap.end()) {
            if (state.dataHeatBal->Zone(it->first).HasAdjustedReturnTempByITE) {
                totalGain = 0;
                totalRate = 0;
                for (int i : it->second) {
                    if (state.dataHeatBal->ZoneITEq(i).ReturnApproachTempSch != 0) {
                        TAirReturn = state.dataHeatBal->ZoneITEq(i).AirOutletDryBulbT +
                                     GetCurrentScheduleValue(state, state.dataHeatBal->ZoneITEq(i).ReturnApproachTempSch);
                    } else {
                        TAirReturn = state.dataHeatBal->ZoneITEq(i).AirOutletDryBulbT + state.dataHeatBal->ZoneITEq(i).ReturnApproachTemp;
                    }
                    totalRate += state.dataHeatBal->ZoneITEq(i).AirMassFlow;
                    totalGain += state.dataHeatBal->ZoneITEq(i).AirMassFlow * TAirReturn;
                }
                if (totalRate != 0) {
                    state.dataHeatBal->Zone(it->first).AdjustedReturnTempByITE = totalGain / totalRate;
                    state.dataHeatBal->ZnRpt(it->first).ITEAdjReturnTemp = state.dataHeatBal->Zone(it->first).AdjustedReturnTempByITE;
                }
            }
            it++;
        }

    } // End CalcZoneITEq

    void ReportInternalHeatGains(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   June 1997
        //       MODIFIED       July 1997 RKS
        //       RE-ENGINEERED  December 1998 LKL

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine currently creates the values for standard "zone loads" reporting
        // from the heat balance module.

        // METHODOLOGY EMPLOYED:
        // The reporting methodology is described in the OutputDataStructure.doc
        // as the "modified modular" format.

        // REFERENCES:
        // OutputDataStructure.doc (EnergyPlus documentation)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int ZoneLoop; // Counter for the # of zones (nz)
        static const Array1D_int TradIntGainTypes(8,
                                                  {IntGainTypeOf_People,
                                                   IntGainTypeOf_Lights,
                                                   IntGainTypeOf_ElectricEquipment,
                                                   IntGainTypeOf_ElectricEquipmentITEAirCooled,
                                                   IntGainTypeOf_GasEquipment,
                                                   IntGainTypeOf_HotWaterEquipment,
                                                   IntGainTypeOf_SteamEquipment,
                                                   IntGainTypeOf_OtherEquipment});

        for (Loop = 1; Loop <= state.dataHeatBal->TotPeople; ++Loop) {
            state.dataHeatBal->People(Loop).RadGainEnergy = state.dataHeatBal->People(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->People(Loop).ConGainEnergy = state.dataHeatBal->People(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->People(Loop).SenGainEnergy = state.dataHeatBal->People(Loop).SenGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->People(Loop).LatGainEnergy = state.dataHeatBal->People(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->People(Loop).TotGainEnergy = state.dataHeatBal->People(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotLights; ++Loop) {
            state.dataHeatBal->Lights(Loop).Consumption = state.dataHeatBal->Lights(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->Lights(Loop).RadGainEnergy = state.dataHeatBal->Lights(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->Lights(Loop).VisGainEnergy = state.dataHeatBal->Lights(Loop).VisGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->Lights(Loop).ConGainEnergy = state.dataHeatBal->Lights(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->Lights(Loop).RetAirGainEnergy = state.dataHeatBal->Lights(Loop).RetAirGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->Lights(Loop).TotGainEnergy = state.dataHeatBal->Lights(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
            if (!state.dataGlobal->WarmupFlag) {
                if (state.dataGlobal->DoOutputReporting && state.dataOutRptTab->WriteTabularFiles &&
                    (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather)) { // for weather simulations only
                    // for tabular report, accumulate the total electricity used for each Light object
                    state.dataHeatBal->Lights(Loop).SumConsumption += state.dataHeatBal->Lights(Loop).Consumption;
                    // for tabular report, accumulate the time when each Light has consumption (using a very small threshold instead of zero)
                    if (state.dataHeatBal->Lights(Loop).Power > 0.01 * state.dataHeatBal->Lights(Loop).DesignLevel) {
                        state.dataHeatBal->Lights(Loop).SumTimeNotZeroCons += state.dataGlobal->TimeStepZone;
                    }
                }
            }
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotElecEquip; ++Loop) {
            state.dataHeatBal->ZoneElectric(Loop).Consumption = state.dataHeatBal->ZoneElectric(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneElectric(Loop).RadGainEnergy =
                state.dataHeatBal->ZoneElectric(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneElectric(Loop).ConGainEnergy =
                state.dataHeatBal->ZoneElectric(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneElectric(Loop).LatGainEnergy =
                state.dataHeatBal->ZoneElectric(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneElectric(Loop).LostEnergy = state.dataHeatBal->ZoneElectric(Loop).LostRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneElectric(Loop).TotGainEnergy =
                state.dataHeatBal->ZoneElectric(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotGasEquip; ++Loop) {
            state.dataHeatBal->ZoneGas(Loop).Consumption = state.dataHeatBal->ZoneGas(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneGas(Loop).RadGainEnergy = state.dataHeatBal->ZoneGas(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneGas(Loop).ConGainEnergy = state.dataHeatBal->ZoneGas(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneGas(Loop).LatGainEnergy = state.dataHeatBal->ZoneGas(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneGas(Loop).LostEnergy = state.dataHeatBal->ZoneGas(Loop).LostRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneGas(Loop).TotGainEnergy = state.dataHeatBal->ZoneGas(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotOthEquip; ++Loop) {
            state.dataHeatBal->ZoneOtherEq(Loop).Consumption = state.dataHeatBal->ZoneOtherEq(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOtherEq(Loop).RadGainEnergy = state.dataHeatBal->ZoneOtherEq(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOtherEq(Loop).ConGainEnergy = state.dataHeatBal->ZoneOtherEq(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOtherEq(Loop).LatGainEnergy = state.dataHeatBal->ZoneOtherEq(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOtherEq(Loop).LostEnergy = state.dataHeatBal->ZoneOtherEq(Loop).LostRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOtherEq(Loop).TotGainEnergy = state.dataHeatBal->ZoneOtherEq(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotHWEquip; ++Loop) {
            state.dataHeatBal->ZoneHWEq(Loop).Consumption = state.dataHeatBal->ZoneHWEq(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneHWEq(Loop).RadGainEnergy = state.dataHeatBal->ZoneHWEq(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneHWEq(Loop).ConGainEnergy = state.dataHeatBal->ZoneHWEq(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneHWEq(Loop).LatGainEnergy = state.dataHeatBal->ZoneHWEq(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneHWEq(Loop).LostEnergy = state.dataHeatBal->ZoneHWEq(Loop).LostRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneHWEq(Loop).TotGainEnergy = state.dataHeatBal->ZoneHWEq(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotStmEquip; ++Loop) {
            state.dataHeatBal->ZoneSteamEq(Loop).Consumption = state.dataHeatBal->ZoneSteamEq(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneSteamEq(Loop).RadGainEnergy = state.dataHeatBal->ZoneSteamEq(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneSteamEq(Loop).ConGainEnergy = state.dataHeatBal->ZoneSteamEq(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneSteamEq(Loop).LatGainEnergy = state.dataHeatBal->ZoneSteamEq(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneSteamEq(Loop).LostEnergy = state.dataHeatBal->ZoneSteamEq(Loop).LostRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneSteamEq(Loop).TotGainEnergy = state.dataHeatBal->ZoneSteamEq(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (Loop = 1; Loop <= state.dataHeatBal->TotBBHeat; ++Loop) {
            state.dataHeatBal->ZoneBBHeat(Loop).Consumption = state.dataHeatBal->ZoneBBHeat(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneBBHeat(Loop).RadGainEnergy = state.dataHeatBal->ZoneBBHeat(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneBBHeat(Loop).ConGainEnergy = state.dataHeatBal->ZoneBBHeat(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneBBHeat(Loop).TotGainEnergy = state.dataHeatBal->ZoneBBHeat(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) {
            // People
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleNumOcc = state.dataHeatBal->ZoneIntGain(ZoneLoop).NOFOCC;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleRadGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOCRAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleConGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOCCON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleSenGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOCSEN * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleLatGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOCLAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleTotGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOCTOT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleRadGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOCRAD;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleConGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOCCON;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleSenGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOCSEN;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleLatGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOCLAT;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleTotGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOCTOT;

            // General Lights
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsRetAirGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QLTCRA * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsRadGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QLTRAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsTotGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QLTTOT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsConGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QLTCON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsVisGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QLTSW * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsRetAirGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QLTCRA;
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsRadGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QLTRAD;
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsTotGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QLTTOT;
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsConGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QLTCON;
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsVisGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QLTSW;
            state.dataHeatBal->ZnRpt(ZoneLoop).LtsElecConsump = state.dataHeatBal->ZnRpt(ZoneLoop).LtsTotGain;

            // Electric Equipment
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecConGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QEECON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecRadGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QEERAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecLatGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QEELAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecLost = state.dataHeatBal->ZoneIntGain(ZoneLoop).QEELost * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecConGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QEECON;
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecRadGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QEERAD;
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecLatGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QEELAT;
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecLostRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QEELost;
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecConsump =
                state.dataHeatBal->ZnRpt(ZoneLoop).ElecConGain + state.dataHeatBal->ZnRpt(ZoneLoop).ElecRadGain +
                state.dataHeatBal->ZnRpt(ZoneLoop).ElecLatGain + state.dataHeatBal->ZnRpt(ZoneLoop).ElecLost;
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecTotGain = state.dataHeatBal->ZnRpt(ZoneLoop).ElecConGain +
                                                             state.dataHeatBal->ZnRpt(ZoneLoop).ElecRadGain +
                                                             state.dataHeatBal->ZnRpt(ZoneLoop).ElecLatGain;
            state.dataHeatBal->ZnRpt(ZoneLoop).ElecTotGainRate = state.dataHeatBal->ZnRpt(ZoneLoop).ElecConGainRate +
                                                                 state.dataHeatBal->ZnRpt(ZoneLoop).ElecRadGainRate +
                                                                 state.dataHeatBal->ZnRpt(ZoneLoop).ElecLatGainRate;

            // Gas Equipment
            state.dataHeatBal->ZnRpt(ZoneLoop).GasConGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QGECON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).GasRadGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QGERAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).GasLatGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QGELAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).GasLost = state.dataHeatBal->ZoneIntGain(ZoneLoop).QGELost * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).GasConGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QGECON;
            state.dataHeatBal->ZnRpt(ZoneLoop).GasRadGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QGERAD;
            state.dataHeatBal->ZnRpt(ZoneLoop).GasLatGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QGELAT;
            state.dataHeatBal->ZnRpt(ZoneLoop).GasLostRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QGELost;
            state.dataHeatBal->ZnRpt(ZoneLoop).GasConsump =
                state.dataHeatBal->ZnRpt(ZoneLoop).GasConGain + state.dataHeatBal->ZnRpt(ZoneLoop).GasRadGain +
                state.dataHeatBal->ZnRpt(ZoneLoop).GasLatGain + state.dataHeatBal->ZnRpt(ZoneLoop).GasLost;
            state.dataHeatBal->ZnRpt(ZoneLoop).GasTotGain = state.dataHeatBal->ZnRpt(ZoneLoop).GasConGain +
                                                            state.dataHeatBal->ZnRpt(ZoneLoop).GasRadGain +
                                                            state.dataHeatBal->ZnRpt(ZoneLoop).GasLatGain;
            state.dataHeatBal->ZnRpt(ZoneLoop).GasTotGainRate = state.dataHeatBal->ZnRpt(ZoneLoop).GasConGainRate +
                                                                state.dataHeatBal->ZnRpt(ZoneLoop).GasRadGainRate +
                                                                state.dataHeatBal->ZnRpt(ZoneLoop).GasLatGainRate;

            // Hot Water Equipment
            state.dataHeatBal->ZnRpt(ZoneLoop).HWConGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QHWCON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).HWRadGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QHWRAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).HWLatGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QHWLAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).HWLost = state.dataHeatBal->ZoneIntGain(ZoneLoop).QHWLost * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).HWConGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QHWCON;
            state.dataHeatBal->ZnRpt(ZoneLoop).HWRadGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QHWRAD;
            state.dataHeatBal->ZnRpt(ZoneLoop).HWLatGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QHWLAT;
            state.dataHeatBal->ZnRpt(ZoneLoop).HWLostRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QHWLost;
            state.dataHeatBal->ZnRpt(ZoneLoop).HWConsump = state.dataHeatBal->ZnRpt(ZoneLoop).HWConGain +
                                                           state.dataHeatBal->ZnRpt(ZoneLoop).HWRadGain +
                                                           state.dataHeatBal->ZnRpt(ZoneLoop).HWLatGain + state.dataHeatBal->ZnRpt(ZoneLoop).HWLost;
            state.dataHeatBal->ZnRpt(ZoneLoop).HWTotGain = state.dataHeatBal->ZnRpt(ZoneLoop).HWConGain +
                                                           state.dataHeatBal->ZnRpt(ZoneLoop).HWRadGain +
                                                           state.dataHeatBal->ZnRpt(ZoneLoop).HWLatGain;
            state.dataHeatBal->ZnRpt(ZoneLoop).HWTotGainRate = state.dataHeatBal->ZnRpt(ZoneLoop).HWConGainRate +
                                                               state.dataHeatBal->ZnRpt(ZoneLoop).HWRadGainRate +
                                                               state.dataHeatBal->ZnRpt(ZoneLoop).HWLatGainRate;

            // Steam Equipment
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamConGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QSECON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamRadGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QSERAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamLatGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QSELAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamLost = state.dataHeatBal->ZoneIntGain(ZoneLoop).QSELost * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamConGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QSECON;
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamRadGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QSERAD;
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamLatGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QSELAT;
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamLostRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QSELost;
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamConsump =
                state.dataHeatBal->ZnRpt(ZoneLoop).SteamConGain + state.dataHeatBal->ZnRpt(ZoneLoop).SteamRadGain +
                state.dataHeatBal->ZnRpt(ZoneLoop).SteamLatGain + state.dataHeatBal->ZnRpt(ZoneLoop).SteamLost;
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamTotGain = state.dataHeatBal->ZnRpt(ZoneLoop).SteamConGain +
                                                              state.dataHeatBal->ZnRpt(ZoneLoop).SteamRadGain +
                                                              state.dataHeatBal->ZnRpt(ZoneLoop).SteamLatGain;
            state.dataHeatBal->ZnRpt(ZoneLoop).SteamTotGainRate = state.dataHeatBal->ZnRpt(ZoneLoop).SteamConGainRate +
                                                                  state.dataHeatBal->ZnRpt(ZoneLoop).SteamRadGainRate +
                                                                  state.dataHeatBal->ZnRpt(ZoneLoop).SteamLatGainRate;

            // Other Equipment
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherConGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOECON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherRadGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOERAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherLatGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOELAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherLost = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOELost * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherConGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOECON;
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherRadGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOERAD;
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherLatGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOELAT;
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherLostRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QOELost;
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherConsump =
                state.dataHeatBal->ZnRpt(ZoneLoop).OtherConGain + state.dataHeatBal->ZnRpt(ZoneLoop).OtherRadGain +
                state.dataHeatBal->ZnRpt(ZoneLoop).OtherLatGain + state.dataHeatBal->ZnRpt(ZoneLoop).OtherLost;
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherTotGain = state.dataHeatBal->ZnRpt(ZoneLoop).OtherConGain +
                                                              state.dataHeatBal->ZnRpt(ZoneLoop).OtherRadGain +
                                                              state.dataHeatBal->ZnRpt(ZoneLoop).OtherLatGain;
            state.dataHeatBal->ZnRpt(ZoneLoop).OtherTotGainRate = state.dataHeatBal->ZnRpt(ZoneLoop).OtherConGainRate +
                                                                  state.dataHeatBal->ZnRpt(ZoneLoop).OtherRadGainRate +
                                                                  state.dataHeatBal->ZnRpt(ZoneLoop).OtherLatGainRate;

            // Baseboard Heat
            state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatConGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QBBCON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatRadGain = state.dataHeatBal->ZoneIntGain(ZoneLoop).QBBRAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatConGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QBBCON;
            state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatRadGainRate = state.dataHeatBal->ZoneIntGain(ZoneLoop).QBBRAD;
            state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatTotGain =
                state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatConGain + state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatRadGain;
            state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatTotGainRate =
                state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatConGainRate + state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatRadGainRate;
            state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatElecCons = state.dataHeatBal->ZnRpt(ZoneLoop).BaseHeatTotGain;

            // Overall Zone Variables

            // these overalls include component gains from devices like water heater, water use, and generators
            //   working vars QFCConv QGenConv QFCRad QGenRad  WaterUseLatentGain WaterThermalTankGain WaterUseSensibleGain

            state.dataHeatBal->ZnRpt(ZoneLoop).TotVisHeatGain = state.dataHeatBal->ZnRpt(ZoneLoop).LtsVisGain;
            state.dataHeatBal->ZnRpt(ZoneLoop).TotVisHeatGainRate = state.dataHeatBal->ZnRpt(ZoneLoop).LtsVisGainRate;

            SumInternalRadiationGainsByTypes(state, ZoneLoop, TradIntGainTypes, state.dataHeatBal->ZnRpt(ZoneLoop).TotRadiantGainRate);
            state.dataHeatBal->ZnRpt(ZoneLoop).TotRadiantGain =
                state.dataHeatBal->ZnRpt(ZoneLoop).TotRadiantGainRate * state.dataGlobal->TimeStepZoneSec;

            SumInternalConvectionGainsByTypes(state, ZoneLoop, TradIntGainTypes, state.dataHeatBal->ZnRpt(ZoneLoop).TotConvectiveGainRate);
            state.dataHeatBal->ZnRpt(ZoneLoop).TotConvectiveGain =
                state.dataHeatBal->ZnRpt(ZoneLoop).TotConvectiveGainRate * state.dataGlobal->TimeStepZoneSec;

            SumInternalLatentGainsByTypes(state, ZoneLoop, TradIntGainTypes, state.dataHeatBal->ZnRpt(ZoneLoop).TotLatentGainRate);
            state.dataHeatBal->ZnRpt(ZoneLoop).TotLatentGain =
                state.dataHeatBal->ZnRpt(ZoneLoop).TotLatentGainRate * state.dataGlobal->TimeStepZoneSec;

            state.dataHeatBal->ZnRpt(ZoneLoop).TotTotalHeatGainRate =
                state.dataHeatBal->ZnRpt(ZoneLoop).TotLatentGainRate + state.dataHeatBal->ZnRpt(ZoneLoop).TotRadiantGainRate +
                state.dataHeatBal->ZnRpt(ZoneLoop).TotConvectiveGainRate + state.dataHeatBal->ZnRpt(ZoneLoop).TotVisHeatGainRate;
            state.dataHeatBal->ZnRpt(ZoneLoop).TotTotalHeatGain =
                state.dataHeatBal->ZnRpt(ZoneLoop).TotTotalHeatGainRate * state.dataGlobal->TimeStepZoneSec;
        }
    }

    Real64 GetDesignLightingLevelForZone(EnergyPlusData &state, int const WhichZone) // name of zone
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   April 2007; January 2008 - moved to InternalGains
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This routine sums the Lighting Level for a zone.
        // Will issue a severe error for illegal zone.
        // Must be called after InternalHeatGains get input.

        // Using/Aliasing
        using namespace DataHeatBalance;
        // Return value
        Real64 DesignLightingLevelSum; // Sum of design lighting level for this zone

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Loop;

        if (state.dataInternalHeatGains->GetInternalHeatGainsInputFlag) {
            ShowFatalError(state, "GetDesignLightingLevelForZone: Function called prior to Getting Lights Input.");
        }

        DesignLightingLevelSum = 0.0;

        for (Loop = 1; Loop <= state.dataHeatBal->TotLights; ++Loop) {
            if (state.dataHeatBal->Lights(Loop).ZonePtr == WhichZone) {
                DesignLightingLevelSum += state.dataHeatBal->Lights(Loop).DesignLevel;
            }
        }

        return DesignLightingLevelSum;
    }

    bool CheckThermalComfortSchedules(bool const WorkEffSch, // Blank work efficiency schedule = true
                                      bool const CloInsSch,  // Blank clothing insulation schedule = true
                                      bool const AirVeloSch) // Blank air velocity schedule = true
    {
        bool TCSchedsPresent = false;

        if (!WorkEffSch || !CloInsSch || !AirVeloSch) {
            TCSchedsPresent = true;
        }

        return TCSchedsPresent;
    }

    void CheckLightsReplaceableMinMaxForZone(EnergyPlusData &state, int const WhichZone) // Zone Number
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   April 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Daylighting is not available unless Lights (replaceable) is 0.0 or 1.0.  No dimming will be done
        // unless the lights replaceable fraction is 1.0.  This is documented in the InputOutputReference but
        // not warned about.  Also, this will sum the Zone Design Lighting level, in case the calling routine
        // would like to have an error if the lights is zero and daylighting is requested.

        // METHODOLOGY EMPLOYED:
        // Traverse the LIGHTS structure and get fraction replaceable - min/max as well as lighting
        // level for a zone.

        // Using/Aliasing
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        Real64 LightsRepMin; // Minimum Lighting replacement fraction for any lights statement for this zone
        Real64 LightsRepMax; // Maximum Lighting replacement fraction for any lights statement for this zone
        int NumLights;       // Number of Lights statement for that zone.

        if (state.dataInternalHeatGains->GetInternalHeatGainsInputFlag) {
            ShowFatalError(state, "CheckLightsReplaceableMinMaxForZone: Function called prior to Getting Lights Input.");
        }

        LightsRepMin = 99999.0;
        LightsRepMax = -99999.0;
        NumLights = 0;

        for (Loop = 1; Loop <= state.dataHeatBal->TotLights; ++Loop) {
            if (state.dataHeatBal->Lights(Loop).ZonePtr != WhichZone) continue;
            LightsRepMin = min(LightsRepMin, state.dataHeatBal->Lights(Loop).FractionReplaceable);
            LightsRepMax = max(LightsRepMax, state.dataHeatBal->Lights(Loop).FractionReplaceable);
            ++NumLights;
            if ((state.dataDaylightingData->ZoneDaylight(state.dataHeatBal->Lights(Loop).ZonePtr).DaylightMethod ==
                     DataDaylighting::iDaylightingMethod::SplitFluxDaylighting ||
                 state.dataDaylightingData->ZoneDaylight(state.dataHeatBal->Lights(Loop).ZonePtr).DaylightMethod ==
                     DataDaylighting::iDaylightingMethod::DElightDaylighting) &&
                (state.dataHeatBal->Lights(Loop).FractionReplaceable > 0.0 && state.dataHeatBal->Lights(Loop).FractionReplaceable < 1.0)) {
                ShowWarningError(state, "CheckLightsReplaceableMinMaxForZone: Fraction Replaceable must be 0.0 or 1.0 if used with daylighting.");
                ShowContinueError(state,
                                  "..Lights=\"" + state.dataHeatBal->Lights(Loop).Name +
                                      "\", Fraction Replaceable will be reset to 1.0 to allow dimming controls");
                ShowContinueError(state, "..in Zone=" + state.dataHeatBal->Zone(WhichZone).Name);
                state.dataHeatBal->Lights(Loop).FractionReplaceable = 1.0;
            }
        }

        if (state.dataDaylightingData->ZoneDaylight(WhichZone).DaylightMethod == DataDaylighting::iDaylightingMethod::SplitFluxDaylighting) {
            if (LightsRepMax == 0.0) {
                ShowWarningError(state, "CheckLightsReplaceable: Zone \"" + state.dataHeatBal->Zone(WhichZone).Name + "\" has Daylighting:Controls.");
                ShowContinueError(state, "but all of the LIGHTS object in that zone have zero Fraction Replaceable.");
                ShowContinueError(state, "The daylighting controls will have no effect.");
            }
            if (NumLights == 0) {
                ShowWarningError(state, "CheckLightsReplaceable: Zone \"" + state.dataHeatBal->Zone(WhichZone).Name + "\" has Daylighting:Controls.");
                ShowContinueError(state, "but there are no LIGHTS objects in that zone.");
                ShowContinueError(state, "The daylighting controls will have no effect.");
            }
        } else if (state.dataDaylightingData->ZoneDaylight(WhichZone).DaylightMethod == DataDaylighting::iDaylightingMethod::DElightDaylighting) {
            if (LightsRepMax == 0.0) {
                ShowWarningError(state, "CheckLightsReplaceable: Zone \"" + state.dataHeatBal->Zone(WhichZone).Name + "\" has Daylighting:Controls.");
                ShowContinueError(state, "but all of the LIGHTS object in that zone have zero Fraction Replaceable.");
                ShowContinueError(state, "The daylighting controls will have no effect.");
            }
            if (NumLights == 0) {
                ShowWarningError(state, "CheckLightsReplaceable: Zone \"" + state.dataHeatBal->Zone(WhichZone).Name + "\" has Daylighting:Controls.");
                ShowContinueError(state, "but there are no LIGHTS objects in that zone.");
                ShowContinueError(state, "The daylighting controls will have no effect.");
            }
        }
    }

    void UpdateInternalGainValues(EnergyPlusData &state, Optional_bool_const SuppressRadiationUpdate, Optional_bool_const SumLatentGains)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec. 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int NZ;
        bool DoRadiationUpdate;
        bool ReSumLatentGains;

        DoRadiationUpdate = true;
        ReSumLatentGains = false;

        if (present(SuppressRadiationUpdate)) {
            if (SuppressRadiationUpdate) DoRadiationUpdate = false;
        }

        if (present(SumLatentGains)) {
            if (SumLatentGains) ReSumLatentGains = true;
        }

        // store pointer values to hold generic internal gain values constant for entire timestep
        for (NZ = 1; NZ <= state.dataGlobal->NumOfZones; ++NZ) {
            for (Loop = 1; Loop <= state.dataHeatBal->ZoneIntGain(NZ).NumberOfDevices; ++Loop) {
                state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).ConvectGainRate = *state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).PtrConvectGainRate;
                state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).ReturnAirConvGainRate =
                    *state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).PtrReturnAirConvGainRate;
                if (DoRadiationUpdate)
                    state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).RadiantGainRate =
                        *state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).PtrRadiantGainRate;
                state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).LatentGainRate = *state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).PtrLatentGainRate;
                state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).ReturnAirLatentGainRate =
                    *state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).PtrReturnAirLatentGainRate;
                state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).CarbonDioxideGainRate =
                    *state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).PtrCarbonDioxideGainRate;
                state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).GenericContamGainRate =
                    *state.dataHeatBal->ZoneIntGain(NZ).Device(Loop).PtrGenericContamGainRate;
            }
            if (ReSumLatentGains) {
                SumAllInternalLatentGains(state, NZ, state.dataHeatBalFanSys->ZoneLatentGain(NZ));
                // Added for the hybrid model
                if (state.dataHybridModel->FlagHybridModel_PC) {
                    SumAllInternalLatentGainsExceptPeople(state, NZ, state.dataHeatBalFanSys->ZoneLatentGainExceptPeople(NZ));
                }
            }
        }

        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation && allocated(state.dataContaminantBalance->ZoneGCGain)) {
            for (NZ = 1; NZ <= state.dataGlobal->NumOfZones; ++NZ) {
                SumAllInternalGenericContamGains(state, NZ, state.dataContaminantBalance->ZoneGCGain(NZ));
                state.dataHeatBal->ZnRpt(NZ).GCRate = state.dataContaminantBalance->ZoneGCGain(NZ);
            }
        }
    }

    void SumAllInternalConvectionGains(EnergyPlusData &state,
                                       int const ZoneNum, // zone index pointer for which zone to sum gains for
                                       Real64 &SumConvGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Nov. 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 tmpSumConvGainRate;
        int DeviceNum;

        tmpSumConvGainRate = 0.0;
        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumConvGainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            tmpSumConvGainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).ConvectGainRate;
        }

        SumConvGainRate = tmpSumConvGainRate;
    }

    // For HybridModel
    void SumAllInternalConvectionGainsExceptPeople(EnergyPlusData &state, int const ZoneNum, Real64 &SumConvGainRateExceptPeople)
    {
        Real64 tmpSumConvGainRateExceptPeople;
        int DeviceNum;
        std::string str_people = "PEOPLE";
        tmpSumConvGainRateExceptPeople = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumConvGainRateExceptPeople = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            if (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CompObjectType != str_people) {
                tmpSumConvGainRateExceptPeople += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).ConvectGainRate;
            }
        }

        SumConvGainRateExceptPeople = tmpSumConvGainRateExceptPeople;
    }

    void SumInternalConvectionGainsByTypes(EnergyPlusData &state,
                                           int const ZoneNum,              // zone index pointer for which zone to sum gains for
                                           const Array1D_int &GainTypeARR, // variable length 1-d array of integer valued gain types
                                           Real64 &SumConvGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Nov. 2011cl
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gain types

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumberOfTypes;
        Real64 tmpSumConvGainRate;
        int DeviceNum;
        int TypeNum;

        NumberOfTypes = size(GainTypeARR);
        tmpSumConvGainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumConvGainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            for (TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {

                if (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                    tmpSumConvGainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).ConvectGainRate;
                }
            }
        }

        SumConvGainRate = tmpSumConvGainRate;
    }

    void SumAllReturnAirConvectionGains(EnergyPlusData &state,
                                        int const ZoneNum, // zone index pointer for which zone to sum gains for
                                        Real64 &SumReturnAirGainRate,
                                        int const ReturnNodeNum // return air node number
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec. 2011

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        Real64 tmpSumRetAirGainRate;
        int DeviceNum;

        tmpSumRetAirGainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumReturnAirGainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            // If ReturnNodeNum is zero, sum for entire zone, otherwise sum only for specified ReturnNodeNum
            if ((ReturnNodeNum == 0) || (ReturnNodeNum == state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).ReturnAirNodeNum)) {
                tmpSumRetAirGainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).ReturnAirConvGainRate;
            }
        }

        SumReturnAirGainRate = tmpSumRetAirGainRate;
    }

    void SumReturnAirConvectionGainsByTypes(EnergyPlusData &state,
                                            int const ZoneNum,              // zone index pointer for which zone to sum gains for
                                            const Array1D_int &GainTypeARR, // variable length 1-d array of integer valued gain types
                                            Real64 &SumReturnAirGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Nov. 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gain types

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumberOfTypes;
        Real64 tmpSumRetAirConvGainRate;
        int DeviceNum;
        int TypeNum;

        NumberOfTypes = size(GainTypeARR);
        tmpSumRetAirConvGainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumReturnAirGainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            for (TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {

                if (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                    tmpSumRetAirConvGainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).ReturnAirConvGainRate;
                }
            }
        }

        SumReturnAirGainRate = tmpSumRetAirConvGainRate;
    }

    void SumAllInternalRadiationGains(EnergyPlusData &state,
                                      int const ZoneNum, // zone index pointer for which zone to sum gains for
                                      Real64 &SumRadGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Nov. 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 tmpSumRadGainRate;
        int DeviceNum;

        tmpSumRadGainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumRadGainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            tmpSumRadGainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).RadiantGainRate;
        }

        SumRadGainRate = tmpSumRadGainRate;
    }

    void SumInternalRadiationGainsByTypes(EnergyPlusData &state,
                                          int const ZoneNum,              // zone index pointer for which zone to sum gains for
                                          const Array1D_int &GainTypeARR, // variable length 1-d array of integer valued gain types
                                          Real64 &SumRadiationGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec. 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gain types

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumberOfTypes;
        Real64 tmpSumRadiationGainRate;
        int DeviceNum;
        int TypeNum;

        NumberOfTypes = size(GainTypeARR);
        tmpSumRadiationGainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumRadiationGainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            for (TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {

                if (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                    tmpSumRadiationGainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).RadiantGainRate;
                }
            }
        }

        SumRadiationGainRate = tmpSumRadiationGainRate;
    }

    void SumAllInternalLatentGains(EnergyPlusData &state,
                                   int const ZoneNum, // zone index pointer for which zone to sum gains for
                                   Real64 &SumLatentGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Nov. 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 tmpSumLatentGainRate;
        int DeviceNum;

        tmpSumLatentGainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumLatentGainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            tmpSumLatentGainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).LatentGainRate;
        }

        SumLatentGainRate = tmpSumLatentGainRate;
    }

    // Added for hybrid model -- calculate the latent gain from all sources except for people
    void SumAllInternalLatentGainsExceptPeople(EnergyPlusData &state,
                                               int const ZoneNum, // zone index pointer for which zone to sum gains for
                                               Real64 &SumLatentGainRateExceptPeople)
    {
        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumLatentGainRateExceptPeople = 0.0;
            return;
        }
        for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            if (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CompTypeOfNum != IntGainTypeOf_People) {
                SumLatentGainRateExceptPeople += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).LatentGainRate;
            }
        }
    }

    void SumInternalLatentGainsByTypes(EnergyPlusData &state,
                                       int const ZoneNum,              // zone index pointer for which zone to sum gains for
                                       const Array1D_int &GainTypeARR, // variable length 1-d array of integer valued gain types
                                       Real64 &SumLatentGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec. 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gain types

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumberOfTypes;
        Real64 tmpSumLatentGainRate;
        int DeviceNum;
        int TypeNum;

        NumberOfTypes = size(GainTypeARR);
        tmpSumLatentGainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumLatentGainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            for (TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {

                if (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                    tmpSumLatentGainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).LatentGainRate;
                }
            }
        }

        SumLatentGainRate = tmpSumLatentGainRate;
    }

    void SumAllReturnAirLatentGains(EnergyPlusData &state,
                                    int const ZoneNum, // zone index pointer for which zone to sum gains for
                                    Real64 &SumRetAirLatentGainRate,
                                    int const ReturnNodeNum // return air node number
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Nov. 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 tmpSumLatentGainRate;
        int DeviceNum;

        tmpSumLatentGainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumRetAirLatentGainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            // If ReturnNodeNum is zero, sum for entire zone, otherwise sum only for specified ReturnNodeNum
            if ((ReturnNodeNum == 0) || (ReturnNodeNum == state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).ReturnAirNodeNum)) {
                tmpSumLatentGainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).ReturnAirLatentGainRate;
            }
        }

        SumRetAirLatentGainRate = tmpSumLatentGainRate;
    }

    void SumAllInternalCO2Gains(EnergyPlusData &state,
                                int const ZoneNum, // zone index pointer for which zone to sum gains for
                                Real64 &SumCO2GainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec. 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 tmpSumCO2GainRate;
        int DeviceNum;

        tmpSumCO2GainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumCO2GainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            tmpSumCO2GainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CarbonDioxideGainRate;
        }

        SumCO2GainRate = tmpSumCO2GainRate;
    }

    // Added for hybrid model -- function for calculating CO2 gains except people
    void SumAllInternalCO2GainsExceptPeople(EnergyPlusData &state,
                                            int const ZoneNum, // zone index pointer for which zone to sum gains for
                                            Real64 &SumCO2GainRateExceptPeople)
    {
        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumCO2GainRateExceptPeople = 0.0;
            return;
        }

        for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            if (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CompTypeOfNum != IntGainTypeOf_People) {
                SumCO2GainRateExceptPeople += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CarbonDioxideGainRate;
            }
        }
    }

    void SumInternalCO2GainsByTypes(EnergyPlusData &state,
                                    int const ZoneNum,              // zone index pointer for which zone to sum gains for
                                    const Array1D_int &GainTypeARR, // variable length 1-d array of integer valued gain types
                                    Real64 &SumCO2GainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec. 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gain types

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumberOfTypes;
        Real64 tmpSumCO2GainRate;
        int DeviceNum;
        int TypeNum;

        NumberOfTypes = size(GainTypeARR);
        tmpSumCO2GainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumCO2GainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            for (TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {

                if (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                    tmpSumCO2GainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CarbonDioxideGainRate;
                }
            }
        }

        SumCO2GainRate = tmpSumCO2GainRate;
    }

    void SumAllInternalGenericContamGains(EnergyPlusData &state,
                                          int const ZoneNum, // zone index pointer for which zone to sum gains for
                                          Real64 &SumGCGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         L. Gu
        //       DATE WRITTEN   Feb. 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types based on the existing subrotine SumAllInternalCO2Gains

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 tmpSumGCGainRate;
        int DeviceNum;

        tmpSumGCGainRate = 0.0;

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumGCGainRate = 0.0;
            return;
        }

        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            tmpSumGCGainRate += state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).GenericContamGainRate;
        }

        SumGCGainRate = tmpSumGCGainRate;
    }

    void GatherComponentLoadsIntGain(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   September 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Gather values during sizing used for loads component report.

        // METHODOLOGY EMPLOYED:
        //   Save sequence of values for report during sizing.

        // Using/Aliasing
        using namespace DataHeatBalance;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static const Array1D_int IntGainTypesPeople(1, {IntGainTypeOf_People});
        static const Array1D_int IntGainTypesLight(1, {IntGainTypeOf_Lights});
        static const Array1D_int IntGainTypesEquip(6,
                                                   {IntGainTypeOf_ElectricEquipment,
                                                    IntGainTypeOf_ElectricEquipmentITEAirCooled,
                                                    IntGainTypeOf_GasEquipment,
                                                    IntGainTypeOf_HotWaterEquipment,
                                                    IntGainTypeOf_SteamEquipment,
                                                    IntGainTypeOf_OtherEquipment});
        static const Array1D_int IntGainTypesRefrig(10,
                                                    {IntGainTypeOf_RefrigerationCase,
                                                     IntGainTypeOf_RefrigerationCompressorRack,
                                                     IntGainTypeOf_RefrigerationSystemAirCooledCondenser,
                                                     IntGainTypeOf_RefrigerationSystemSuctionPipe,
                                                     IntGainTypeOf_RefrigerationSecondaryReceiver,
                                                     IntGainTypeOf_RefrigerationSecondaryPipe,
                                                     IntGainTypeOf_RefrigerationWalkIn,
                                                     IntGainTypeOf_RefrigerationTransSysAirCooledGasCooler,
                                                     IntGainTypeOf_RefrigerationTransSysSuctionPipeMT,
                                                     IntGainTypeOf_RefrigerationTransSysSuctionPipeLT});
        static const Array1D_int IntGainTypesWaterUse(
            3, {IntGainTypeOf_WaterUseEquipment, IntGainTypeOf_WaterHeaterMixed, IntGainTypeOf_WaterHeaterStratified});
        static const Array1D_int IntGainTypesHvacLoss(20,
                                                      {IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled,
                                                       IntGainTypeOf_ThermalStorageChilledWaterMixed,
                                                       IntGainTypeOf_ThermalStorageChilledWaterStratified,
                                                       IntGainTypeOf_PipeIndoor,
                                                       IntGainTypeOf_Pump_VarSpeed,
                                                       IntGainTypeOf_Pump_ConSpeed,
                                                       IntGainTypeOf_Pump_Cond,
                                                       IntGainTypeOf_PumpBank_VarSpeed,
                                                       IntGainTypeOf_PumpBank_ConSpeed,
                                                       IntGainTypeOf_PlantComponentUserDefined,
                                                       IntGainTypeOf_CoilUserDefined,
                                                       IntGainTypeOf_ZoneHVACForcedAirUserDefined,
                                                       IntGainTypeOf_AirTerminalUserDefined,
                                                       IntGainTypeOf_PackagedTESCoilTank,
                                                       IntGainTypeOf_FanSystemModel,
                                                       IntGainTypeOf_SecCoolingDXCoilSingleSpeed,
                                                       IntGainTypeOf_SecHeatingDXCoilSingleSpeed,
                                                       IntGainTypeOf_SecCoolingDXCoilTwoSpeed,
                                                       IntGainTypeOf_SecCoolingDXCoilMultiSpeed,
                                                       IntGainTypeOf_SecHeatingDXCoilMultiSpeed});
        static const Array1D_int IntGainTypesPowerGen(10,
                                                      {IntGainTypeOf_GeneratorFuelCell,
                                                       IntGainTypeOf_GeneratorMicroCHP,
                                                       IntGainTypeOf_ElectricLoadCenterTransformer,
                                                       IntGainTypeOf_ElectricLoadCenterInverterSimple,
                                                       IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower,
                                                       IntGainTypeOf_ElectricLoadCenterInverterLookUpTable,
                                                       IntGainTypeOf_ElectricLoadCenterStorageLiIonNmcBattery,
                                                       IntGainTypeOf_ElectricLoadCenterStorageBattery,
                                                       IntGainTypeOf_ElectricLoadCenterStorageSimple,
                                                       IntGainTypeOf_ElectricLoadCenterConverter});

        if (state.dataGlobal->CompLoadReportIsReq && !state.dataGlobal->isPulseZoneSizing) {
            int TimeStepInDay = (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;
            for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                SumInternalConvectionGainsByTypes(
                    state, iZone, IntGainTypesPeople, state.dataOutRptTab->peopleInstantSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumInternalLatentGainsByTypes(
                    state, iZone, IntGainTypesPeople, state.dataOutRptTab->peopleLatentSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumInternalRadiationGainsByTypes(
                    state, iZone, IntGainTypesPeople, state.dataOutRptTab->peopleRadSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));

                SumInternalConvectionGainsByTypes(
                    state, iZone, IntGainTypesLight, state.dataOutRptTab->lightInstantSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumReturnAirConvectionGainsByTypes(
                    state, iZone, IntGainTypesLight, state.dataOutRptTab->lightRetAirSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumInternalRadiationGainsByTypes(
                    state, iZone, IntGainTypesLight, state.dataOutRptTab->lightLWRadSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));

                SumInternalConvectionGainsByTypes(
                    state, iZone, IntGainTypesEquip, state.dataOutRptTab->equipInstantSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumInternalLatentGainsByTypes(
                    state, iZone, IntGainTypesEquip, state.dataOutRptTab->equipLatentSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumInternalRadiationGainsByTypes(
                    state, iZone, IntGainTypesEquip, state.dataOutRptTab->equipRadSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));

                SumInternalConvectionGainsByTypes(
                    state, iZone, IntGainTypesRefrig, state.dataOutRptTab->refrigInstantSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumReturnAirConvectionGainsByTypes(
                    state, iZone, IntGainTypesRefrig, state.dataOutRptTab->refrigRetAirSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumInternalLatentGainsByTypes(
                    state, iZone, IntGainTypesRefrig, state.dataOutRptTab->refrigLatentSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));

                SumInternalConvectionGainsByTypes(state,
                                                  iZone,
                                                  IntGainTypesWaterUse,
                                                  state.dataOutRptTab->waterUseInstantSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumInternalLatentGainsByTypes(state,
                                              iZone,
                                              IntGainTypesWaterUse,
                                              state.dataOutRptTab->waterUseLatentSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));

                SumInternalConvectionGainsByTypes(state,
                                                  iZone,
                                                  IntGainTypesHvacLoss,
                                                  state.dataOutRptTab->hvacLossInstantSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumInternalRadiationGainsByTypes(
                    state, iZone, IntGainTypesHvacLoss, state.dataOutRptTab->hvacLossRadSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));

                SumInternalConvectionGainsByTypes(state,
                                                  iZone,
                                                  IntGainTypesPowerGen,
                                                  state.dataOutRptTab->powerGenInstantSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
                SumInternalRadiationGainsByTypes(
                    state, iZone, IntGainTypesPowerGen, state.dataOutRptTab->powerGenRadSeq(state.dataSize->CurOverallSimDay, TimeStepInDay, iZone));
            }
        }
    }

    int GetInternalGainDeviceIndex(EnergyPlusData &state,
                                   int const ZoneNum,                   // zone index pointer for which zone to sum gains for
                                   int const IntGainTypeOfNum,          // zone internal gain type number
                                   std::string_view const &IntGainName) // Internal gain name
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // utility to retrieve index pointer to a specific internal gain
        // the subroutine returns the index of matched internal gain device or -1 if no match found.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DeviceNum;
        int DeviceIndex;
        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            DeviceIndex = -1;
            return DeviceIndex;
        }
        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices; ++DeviceNum) {
            if ((UtilityRoutines::SameString(state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CompObjectName, IntGainName.data())) &&
                (state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).CompTypeOfNum == IntGainTypeOfNum)) {
                DeviceIndex = DeviceNum;
                break;
            } else {
                DeviceIndex = -1;
            }
        }
        return DeviceIndex;
    }

    void SumInternalConvectionGainsByIndices(
        EnergyPlusData &state,
        int const ZoneNum,                  // zone index pointer for which zone to sum gains for
        const Array1D_int &DeviceIndexARR,  // variable length 1-d array of integer device index pointers to include in summation
        const Array1D<Real64> &FractionARR, // array of fractional multipliers to apply to devices
        Real64 &SumConvGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gains by index

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumberOfIndices;
        int NumberOfFractions;
        Real64 tmpSumConvGainRate;
        int loop;
        int DeviceNum;
        Real64 DeviceFraction;

        NumberOfIndices = isize(DeviceIndexARR);
        NumberOfFractions = isize(FractionARR);
        tmpSumConvGainRate = 0.0;

        // remove this next safety check after testing code
        if (NumberOfIndices != NumberOfFractions) { // throw error
            ShowSevereError(state, "SumInternalConvectionGainsByIndices: bad arguments, sizes do not match");
        }

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumConvGainRate = 0.0;
            return;
        }

        for (loop = 1; loop <= NumberOfIndices; ++loop) {
            DeviceNum = DeviceIndexARR(loop);
            DeviceFraction = FractionARR(loop);
            tmpSumConvGainRate = tmpSumConvGainRate + state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).ConvectGainRate * DeviceFraction;
        }
        SumConvGainRate = tmpSumConvGainRate;
    }

    void SumInternalLatentGainsByIndices(
        EnergyPlusData &state,
        int const ZoneNum,                  // zone index pointer for which zone to sum gains for
        const Array1D_int &DeviceIndexARR,  // variable length 1-d array of integer device index pointers to include in summation
        const Array1D<Real64> &FractionARR, // array of fractional multipliers to apply to devices
        Real64 &SumLatentGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gains by index

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumberOfIndices;
        int NumberOfFractions;
        Real64 tmpSumLatentGainRate;
        int loop;
        int DeviceNum;
        Real64 DeviceFraction;

        NumberOfIndices = isize(DeviceIndexARR);
        NumberOfFractions = isize(FractionARR);
        tmpSumLatentGainRate = 0.0;

        // remove this next safety check after testing code
        if (NumberOfIndices != NumberOfFractions) { // throw error
            ShowSevereError(state, "SumInternalLatentGainsByIndices: bad arguments, sizes do not match");
        }

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumLatentGainRate = 0.0;
            return;
        }

        for (loop = 1; loop <= NumberOfIndices; ++loop) {
            DeviceNum = DeviceIndexARR(loop);
            DeviceFraction = FractionARR(loop);
            tmpSumLatentGainRate = tmpSumLatentGainRate + state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).LatentGainRate * DeviceFraction;
        }
        SumLatentGainRate = tmpSumLatentGainRate;
    }

    void SumReturnAirConvectionGainsByIndices(
        EnergyPlusData &state,
        int const ZoneNum,                  // zone index pointer for which zone to sum gains for
        const Array1D_int &DeviceIndexARR,  // variable length 1-d array of integer device index pointers to include in summation
        const Array1D<Real64> &FractionARR, // array of fractional multipliers to apply to devices
        Real64 &SumReturnAirGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gains by index

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumberOfIndices;
        int NumberOfFractions;
        Real64 tmpSumReturnAirGainRate;
        int loop;
        int DeviceNum;
        Real64 DeviceFraction;

        NumberOfIndices = isize(DeviceIndexARR);
        NumberOfFractions = isize(FractionARR);
        tmpSumReturnAirGainRate = 0.0;

        // remove this next safety check after testing code
        if (NumberOfIndices != NumberOfFractions) { // throw error
            ShowSevereError(state, "SumReturnAirConvectionGainsByIndice: bad arguments, sizes do not match");
        }

        if (state.dataHeatBal->ZoneIntGain(ZoneNum).NumberOfDevices == 0) {
            SumReturnAirGainRate = 0.0;
            return;
        }

        for (loop = 1; loop <= NumberOfIndices; ++loop) {
            DeviceNum = DeviceIndexARR(loop);
            DeviceFraction = FractionARR(loop);
            tmpSumReturnAirGainRate =
                tmpSumReturnAirGainRate + state.dataHeatBal->ZoneIntGain(ZoneNum).Device(DeviceNum).ReturnAirConvGainRate * DeviceFraction;
        }
        SumReturnAirGainRate = tmpSumReturnAirGainRate;
    }

} // namespace InternalHeatGains

} // namespace EnergyPlus
