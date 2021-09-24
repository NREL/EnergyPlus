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
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
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
        int IOStat;
        int NumAlpha;
        int NumNumber;
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

        // Formats
        static constexpr std::string_view Format_720(" Zone Internal Gains Nominal, {},{:.2R},{:.1R},");
        static constexpr std::string_view Format_722(" {} Internal Gains Nominal, {},{},{},{:.2R},{:.1R},");
        static constexpr std::string_view Format_723(
            "! <{} Internal Gains Nominal>,Name,Schedule Name,Zone Name,Zone Floor Area {{m2}},# Zone Occupants,{}");
        static constexpr std::string_view Format_724(" {}, {}\n");

        auto print_and_divide_if_greater_than_zero = [&](const Real64 numerator, const Real64 denominator) {
            if (denominator > 0.0) {
                print(state.files.eio, "{:.3R},", numerator / denominator);
            } else {
                print(state.files.eio, "N/A,");
            }
        };

        auto &ErrorsFound(state.dataInternalHeatGains->ErrorsFound);

        // TODO MJW: Punt for now, sometimes unit test need these to be allocated in AllocateZoneHeatBalArrays, but simulations need them here
        if (!state.dataHeatBal->ZoneIntGain.allocated()) {
            state.dataHeatBal->ZoneIntGain.allocate(state.dataGlobal->NumOfZones);
            state.dataHeatBal->spaceIntGain.allocate(state.dataGlobal->numSpaces);
            state.dataHeatBal->spaceIntGainDevices.allocate(state.dataGlobal->numSpaces);
            state.dataDaylightingData->spacePowerReductionFactor.dimension(state.dataGlobal->numSpaces, 1.0);
        }
        state.dataHeatBal->ZnRpt.allocate(state.dataGlobal->NumOfZones);
        state.dataHeatBal->spaceRpt.allocate(state.dataGlobal->numSpaces);
        state.dataHeatBal->ZoneIntEEuse.allocate(state.dataGlobal->NumOfZones);
        state.dataHeatBal->RefrigCaseCredit.allocate(state.dataGlobal->NumOfZones);

        Array1D_bool RepVarSet;
        RepVarSet.allocate(state.dataGlobal->NumOfZones);
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            RepVarSet(zoneNum) = true;
        }

        const std::string peopleModuleObject = "People";
        const std::string lightsModuleObject = "Lights";
        const std::string elecEqModuleObject = "ElectricEquipment";
        const std::string gasEqModuleObject = "GasEquipment";
        const std::string hwEqModuleObject = "HotWaterEquipment";
        const std::string stmEqModuleObject = "SteamEquipment";
        const std::string othEqModuleObject = "OtherEquipment";
        const std::string itEqModuleObject = "ElectricEquipment:ITE:AirCooled";
        const std::string bbModuleObject = "ZoneBaseboard:OutdoorTemperatureControlled";
        const std::string contamSSModuleObject = "ZoneContaminantSourceAndSink:CarbonDioxide";

        auto &IHGNumbers = state.dataIPShortCut->rNumericArgs;
        auto &AlphaName = state.dataIPShortCut->cAlphaArgs;

        // PEOPLE: Includes both information related to the heat balance and thermal comfort
        setupIHGZonesAndSpaces(state,
                               peopleModuleObject,
                               state.dataHeatBal->PeopleObjects,
                               state.dataHeatBal->NumPeopleStatements,
                               state.dataHeatBal->TotPeople,
                               ErrorsFound);

        if (state.dataHeatBal->TotPeople > 0) {
            state.dataHeatBal->People.allocate(state.dataHeatBal->TotPeople);
            int peopleNum = 0;
            for (int peopleInputNum = 1; peopleInputNum <= state.dataHeatBal->NumPeopleStatements; ++peopleInputNum) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         peopleModuleObject,
                                                                         peopleInputNum,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                // Create one People instance for every space associated with this People input object
                auto &thisPeopleInput = state.dataHeatBal->PeopleObjects(peopleInputNum);
                for (int Item1 = 1; Item1 <= thisPeopleInput.numOfSpaces; ++Item1) {
                    ++peopleNum;
                    auto &thisPeople = state.dataHeatBal->People(peopleNum);
                    int const spaceNum = thisPeopleInput.spaceNums(Item1);
                    int const zoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
                    thisPeople.Name = thisPeopleInput.names(Item1);
                    thisPeople.spaceIndex = spaceNum;
                    thisPeople.ZonePtr = zoneNum;

                    thisPeople.NumberOfPeoplePtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (thisPeople.NumberOfPeoplePtr == 0) {
                        if (Item1 == 1) { // only show error on first one
                            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                            } else {
                                ShowSevereError(state,
                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                            }
                            ErrorsFound = true;
                        }
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, thisPeople.NumberOfPeoplePtr);
                        SchMax = GetScheduleMaxValue(state, thisPeople.NumberOfPeoplePtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (Item1 == 1) {
                                if (SchMin < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                    ErrorsFound = true;
                                }
                            }
                            if (Item1 == 1) {
                                if (SchMax < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                            // Set space load fraction
                            Real64 spaceFrac = 1.0;
                            if (thisPeopleInput.numOfSpaces > 1) {
                                Real64 const zoneArea = state.dataHeatBal->Zone(zoneNum).FloorArea;
                                if (zoneArea > 0.0) {
                                    spaceFrac = state.dataHeatBal->space(spaceNum).floorArea / zoneArea;
                                } else {
                                    ShowSevereError(state,
                                                    std::string(RoutineName) + "Zone floor area is zero when allocating People loads to Spaces.");
                                    ShowContinueError(state,
                                                      "Occurs for People object =" + thisPeopleInput.Name +
                                                          " in Zone=" + state.dataHeatBal->Zone(zoneNum).Name);
                                    ErrorsFound = true;
                                }
                            }
                            thisPeople.NumberOfPeople = IHGNumbers(1) * spaceFrac;
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + peopleModuleObject + "=\"" + thisPeople.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 People will result.");
                            }

                        } else if (peopleMethod == "PEOPLE/AREA") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    thisPeople.NumberOfPeople = IHGNumbers(2) * state.dataHeatBal->space(spaceNum).floorArea;
                                    if ((state.dataHeatBal->space(spaceNum).floorArea <= 0.0) &&
                                        !state.dataHeatBal->space(spaceNum).isRemainderSpace) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + peopleModuleObject + "=\"" + thisPeople.Name + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Space Floor Area = 0.  0 People will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           peopleModuleObject,
                                                           thisPeople.Name,
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + peopleModuleObject + "=\"" + thisPeople.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 People will result.");
                            }

                        } else if (peopleMethod == "AREA/PERSON") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(3) > 0.0) {
                                    thisPeople.NumberOfPeople = state.dataHeatBal->space(spaceNum).floorArea / IHGNumbers(3);
                                    if ((state.dataHeatBal->space(spaceNum).floorArea <= 0.0) &&
                                        !state.dataHeatBal->space(spaceNum).isRemainderSpace) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + peopleModuleObject + "=\"" + thisPeople.Name + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(3) +
                                                             ", but Space Floor Area = 0.  0 People will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           peopleModuleObject,
                                                           thisPeople.Name,
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + peopleModuleObject + "=\"" + thisPeople.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 People will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"People\", \"People/Area\", \"Area/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max people
                    thisPeople.NomMinNumberPeople = thisPeople.NumberOfPeople * SchMin;
                    thisPeople.NomMaxNumberPeople = thisPeople.NumberOfPeople * SchMax;

                    if (zoneNum > 0) {
                        state.dataHeatBal->Zone(zoneNum).TotOccupants += thisPeople.NumberOfPeople;
                        // Note that min/max occupants are non-coincident
                        state.dataHeatBal->Zone(zoneNum).minOccupants += thisPeople.NomMinNumberPeople;
                        state.dataHeatBal->Zone(zoneNum).maxOccupants += thisPeople.NomMaxNumberPeople;
                    }

                    if (spaceNum > 0) {
                        state.dataHeatBal->space(spaceNum).totOccupants += thisPeople.NumberOfPeople;
                        // Note that min/max occupants are non-coincident
                        state.dataHeatBal->space(spaceNum).minOccupants += thisPeople.NomMinNumberPeople;
                        state.dataHeatBal->space(spaceNum).maxOccupants += thisPeople.NomMaxNumberPeople;
                    }
                    thisPeople.FractionRadiant = IHGNumbers(4);
                    thisPeople.FractionConvected = 1.0 - thisPeople.FractionRadiant;
                    if (Item1 == 1) {
                        if (thisPeople.FractionConvected < 0.0) {
                            ShowSevereError(state,
                                            format("{}{}=\"{}\", {} < 0.0, value ={:.2R}",
                                                   RoutineName,
                                                   peopleModuleObject,
                                                   AlphaName(1),
                                                   state.dataIPShortCut->cNumericFieldNames(4),
                                                   IHGNumbers(4)));
                            ErrorsFound = true;
                        }
                    }

                    if (NumNumber >= 5 && !state.dataIPShortCut->lNumericFieldBlanks(5)) {
                        thisPeople.UserSpecSensFrac = IHGNumbers(5);
                    } else {
                        thisPeople.UserSpecSensFrac = DataGlobalConstants::AutoCalculate;
                    }

                    if (NumNumber == 6 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                        thisPeople.CO2RateFactor = IHGNumbers(6);
                    } else {
                        thisPeople.CO2RateFactor = 3.82e-8; // m3/s-W
                    }
                    if (thisPeople.CO2RateFactor < 0.0) {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", {} < 0.0, value ={:.2R}",
                                               RoutineName,
                                               peopleModuleObject,
                                               AlphaName(1),
                                               state.dataIPShortCut->cNumericFieldNames(6),
                                               IHGNumbers(6)));
                        ErrorsFound = true;
                    }

                    thisPeople.ActivityLevelPtr = GetScheduleIndex(state, AlphaName(5));
                    if (thisPeople.ActivityLevelPtr == 0) {
                        if (Item1 == 1) {
                            if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(5) + " is required.");
                            } else {
                                ShowSevereError(state,
                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(5) + " entered=" + AlphaName(5));
                            }
                            ErrorsFound = true;
                        }
                    } else { // Check values in Schedule
                        SchMin = GetScheduleMinValue(state, thisPeople.ActivityLevelPtr);
                        SchMax = GetScheduleMaxValue(state, thisPeople.ActivityLevelPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (Item1 == 1) {
                                if (SchMin < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(5) + " minimum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(5), SchMin));
                                    ErrorsFound = true;
                                }
                            }
                            if (Item1 == 1) {
                                if (SchMax < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(5) + " maximum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(5), SchMax));
                                    ErrorsFound = true;
                                }
                            }
                        } else if (SchMin < 70.0 || SchMax > 1000.0) {
                            if (Item1 == 1) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                            thisPeople.Show55Warning = true;
                        } else if (!UtilityRoutines::SameString(AlphaName(6), "No") && !state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(6) + " field should be Yes or No");
                                ShowContinueError(state, "...Field value=\"" + AlphaName(6) + "\" is invalid.");
                                ErrorsFound = true;
                            }
                        }
                    }

                    if (NumAlpha > 6) { // Optional parameters present--thermal comfort data follows...
                        int lastOption = 0;
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
                                                 std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) +
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

                        for (int OptionNum = NumFirstTCModel; OptionNum <= lastOption; ++OptionNum) {

                            {
                                auto const thermalComfortType(AlphaName(OptionNum));

                                if (thermalComfortType == "FANGER") {
                                    thisPeople.Fanger = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "PIERCE") {
                                    thisPeople.Pierce = true;
                                    state.dataHeatBal->AnyThermalComfortPierceModel = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "KSU") {
                                    thisPeople.KSU = true;
                                    state.dataHeatBal->AnyThermalComfortKSUModel = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "ADAPTIVEASH55") {
                                    thisPeople.AdaptiveASH55 = true;
                                    state.dataHeatBal->AdaptiveComfortRequested_ASH55 = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "ADAPTIVECEN15251") {
                                    thisPeople.AdaptiveCEN15251 = true;
                                    state.dataHeatBal->AdaptiveComfortRequested_CEN15251 = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "COOLINGEFFECTASH55") {
                                    thisPeople.CoolingEffectASH55 = true;
                                    state.dataHeatBal->AnyThermalComfortCoolingEffectModel = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "ANKLEDRAFTASH55") {
                                    thisPeople.AnkleDraftASH55 = true;
                                    state.dataHeatBal->AnyThermalComfortAnkleDraftModel = true;
                                    state.dataInternalHeatGains->UsingThermalComfort = true;

                                } else if (thermalComfortType == "") { // Blank input field--just ignore this

                                } else { // An invalid keyword was entered--warn but ignore
                                    if (Item1 == 1) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
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
                            thisPeople.MRTCalcType = DataHeatBalance::CalcMRT::ZoneAveraged;

                            bool ModelWithAdditionalInputs = thisPeople.Fanger || thisPeople.Pierce || thisPeople.KSU ||
                                                             thisPeople.CoolingEffectASH55 || thisPeople.AnkleDraftASH55;

                            // MRT Calculation Type and Surface Name
                            {
                                auto const mrtType(AlphaName(7));

                                if (mrtType == "ZONEAVERAGED") {
                                    thisPeople.MRTCalcType = DataHeatBalance::CalcMRT::ZoneAveraged;

                                } else if (mrtType == "SURFACEWEIGHTED") {
                                    thisPeople.MRTCalcType = DataHeatBalance::CalcMRT::SurfaceWeighted;
                                    thisPeople.SurfacePtr = UtilityRoutines::FindItemInList(AlphaName(8), state.dataSurface->Surface);
                                    if (thisPeople.SurfacePtr == 0 && ModelWithAdditionalInputs) {
                                        if (Item1 == 1) {
                                            ShowSevereError(state,
                                                            std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                                state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphaName(7) +
                                                                " invalid Surface Name=" + AlphaName(8));
                                            ErrorsFound = true;
                                        }
                                    } else if (state.dataSurface->Surface(thisPeople.SurfacePtr).Zone != thisPeople.ZonePtr &&
                                               ModelWithAdditionalInputs) {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) +
                                                            "\", Surface referenced in " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' +
                                                            AlphaName(7) + " in different zone.");
                                        ShowContinueError(state,
                                                          "Surface is in Zone=" +
                                                              state.dataHeatBal->Zone(state.dataSurface->Surface(thisPeople.SurfacePtr).Zone).Name +
                                                              " and " + peopleModuleObject + " is in Zone=" + AlphaName(2));
                                        ErrorsFound = true;
                                    }

                                } else if (mrtType == "ANGLEFACTOR") {
                                    thisPeople.MRTCalcType = DataHeatBalance::CalcMRT::AngleFactor;
                                    thisPeople.AngleFactorListName = AlphaName(8);

                                } else if (mrtType == "") { // Blank input field--just ignore this
                                    if (Item1 == 1 && ModelWithAdditionalInputs)
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", blank " +
                                                             state.dataIPShortCut->cAlphaFieldNames(7));

                                } else { // An invalid keyword was entered--warn but ignore
                                    if (Item1 == 1 && ModelWithAdditionalInputs) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                             state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphaName(7));
                                        ShowContinueError(state, "...Valid values are \"ZoneAveraged\", \"SurfaceWeighted\", \"AngleFactor\".");
                                    }
                                }
                            }

                            if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                                thisPeople.WorkEffPtr = GetScheduleIndex(state, AlphaName(9));
                                if (thisPeople.WorkEffPtr == 0) {
                                    if (Item1 == 1) {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                            state.dataIPShortCut->cAlphaFieldNames(9) + " entered=" + AlphaName(9));
                                        ErrorsFound = true;
                                    }
                                } else { // check min/max on schedule
                                    SchMin = GetScheduleMinValue(state, thisPeople.WorkEffPtr);
                                    SchMax = GetScheduleMaxValue(state, thisPeople.WorkEffPtr);
                                    if (SchMin < 0.0 || SchMax < 0.0) {
                                        if (SchMin < 0.0) {
                                            if (Item1 == 1) {
                                                ShowSevereError(state,
                                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                                                             std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                                                    std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", blank " +
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
                                        thisPeople.ClothingType = 1;
                                        thisPeople.ClothingPtr = GetScheduleIndex(state, AlphaName(12));
                                        if (thisPeople.ClothingPtr == 0 && ModelWithAdditionalInputs) {
                                            if (Item1 == 1) {
                                                ShowSevereError(state,
                                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) +
                                                                    "\", invalid " + state.dataIPShortCut->cAlphaFieldNames(12) + " entered=\"" +
                                                                    AlphaName(12) + "\".");
                                                ErrorsFound = true;
                                            }
                                        } else { // check min/max on schedule
                                            SchMin = GetScheduleMinValue(state, thisPeople.ClothingPtr);
                                            SchMax = GetScheduleMaxValue(state, thisPeople.ClothingPtr);
                                            if (SchMin < 0.0 || SchMax < 0.0) {
                                                if (SchMin < 0.0) {
                                                    if (Item1 == 1) {
                                                        ShowSevereError(state,
                                                                        std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) +
                                                                            "\", " + state.dataIPShortCut->cAlphaFieldNames(12) +
                                                                            ", minimum is < 0.0");
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
                                                                        std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) +
                                                                            "\", " + state.dataIPShortCut->cAlphaFieldNames(12) +
                                                                            ", maximum is < 0.0");
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
                                                                     std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                                        thisPeople.ClothingType = 2;

                                    } else if (clothingType == "CALCULATIONMETHODSCHEDULE") {
                                        thisPeople.ClothingType = 3;
                                        thisPeople.ClothingMethodPtr = GetScheduleIndex(state, AlphaName(11));
                                        if (thisPeople.ClothingMethodPtr == 0) {
                                            if (Item1 == 1) {
                                                ShowSevereError(state,
                                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) +
                                                                    "\", invalid " + state.dataIPShortCut->cAlphaFieldNames(11) + " entered=\"" +
                                                                    AlphaName(11) + "\".");
                                                ErrorsFound = true;
                                            }
                                        }
                                        if (CheckScheduleValue(state, thisPeople.ClothingMethodPtr, 1)) {
                                            thisPeople.ClothingPtr = GetScheduleIndex(state, AlphaName(12));
                                            if (thisPeople.ClothingPtr == 0) {
                                                if (Item1 == 1) {
                                                    ShowSevereError(state,
                                                                    std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) +
                                                                        "\", invalid " + state.dataIPShortCut->cAlphaFieldNames(12) + " entered=\"" +
                                                                        AlphaName(12) + "\".");
                                                    ErrorsFound = true;
                                                }
                                            }
                                        }

                                    } else {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} + peopleModuleObject + "=\"" + thisPeople.Name + "\", invalid " +
                                                            state.dataIPShortCut->cAlphaFieldNames(10) + ", value  =" + AlphaName(10));
                                        ShowContinueError(state,
                                                          "...Valid values are \"ClothingInsulationSchedule\",\"DynamicClothingModelASHRAE55a\", "
                                                          "\"CalculationMethodSchedule\".");
                                        ErrorsFound = true;
                                    }
                                }
                            }

                            if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                                thisPeople.AirVelocityPtr = GetScheduleIndex(state, AlphaName(13));
                                if (thisPeople.AirVelocityPtr == 0) {
                                    if (Item1 == 1) {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                            state.dataIPShortCut->cAlphaFieldNames(13) + " entered=\"" + AlphaName(13) + "\".");
                                        ErrorsFound = true;
                                    }
                                } else { // check min/max on schedule
                                    SchMin = GetScheduleMinValue(state, thisPeople.AirVelocityPtr);
                                    SchMax = GetScheduleMaxValue(state, thisPeople.AirVelocityPtr);
                                    if (SchMin < 0.0 || SchMax < 0.0) {
                                        if (SchMin < 0.0) {
                                            if (Item1 == 1) {
                                                ShowSevereError(state,
                                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                                                                std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                                                    std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", blank " +
                                                        state.dataIPShortCut->cAlphaFieldNames(13) + ". " +
                                                        state.dataIPShortCut->cAlphaFieldNames(13) +
                                                        " is required when Thermal Comfort Model Type is one of "
                                                        "\"Fanger\", \"Pierce\", \"KSU\", \"CoolingEffectASH55\" or \"AnkleDraftASH55\"");
                                    ErrorsFound = true;
                                }
                            }

                            int indexAnkleAirVelPtr = 21;
                            if (!state.dataIPShortCut->lAlphaFieldBlanks(indexAnkleAirVelPtr) || AlphaName(indexAnkleAirVelPtr) != "") {
                                thisPeople.AnkleAirVelocityPtr = GetScheduleIndex(state, AlphaName(indexAnkleAirVelPtr));
                                if (thisPeople.AnkleAirVelocityPtr == 0) {
                                    if (Item1 == 1) {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                            state.dataIPShortCut->cAlphaFieldNames(indexAnkleAirVelPtr) + " entered=\"" +
                                                            AlphaName(indexAnkleAirVelPtr) + "\".");
                                        ErrorsFound = true;
                                    }
                                }
                            } else if (thisPeople.AnkleDraftASH55) {
                                if (Item1 == 1) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + peopleModuleObject + "=\"" + AlphaName(1) + "\", blank " +
                                                        state.dataIPShortCut->cAlphaFieldNames(indexAnkleAirVelPtr) + ". " +
                                                        state.dataIPShortCut->cAlphaFieldNames(indexAnkleAirVelPtr) +
                                                        " is required when Thermal Comfort Model Type is one of "
                                                        "\"Fanger\", \"Pierce\", \"KSU\", \"CoolingEffectASH55\" or \"AnkleDraftASH55\"");
                                    ErrorsFound = true;
                                }
                            }

                        } // usingthermalcomfort block

                    } // ...end of thermal comfort data IF-THEN block  (NumAlphas > 6)

                    if (thisPeople.ZonePtr <= 0) continue; // Error, will be caught and terminated later
                }
            }

            for (int peopleNum = 1; peopleNum <= state.dataHeatBal->TotPeople; ++peopleNum) {
                if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                    SetupEMSActuator(state,
                                     "People",
                                     state.dataHeatBal->People(peopleNum).Name,
                                     "Number of People",
                                     "[each]",
                                     state.dataHeatBal->People(peopleNum).EMSPeopleOn,
                                     state.dataHeatBal->People(peopleNum).EMSNumberOfPeople);
                    SetupEMSInternalVariable(state,
                                             "People Count Design Level",
                                             state.dataHeatBal->People(peopleNum).Name,
                                             "[each]",
                                             state.dataHeatBal->People(peopleNum).NumberOfPeople);
                }

                // setup internal gains
                if (!ErrorsFound) {
                    SetupSpaceInternalGain(state,
                                           state.dataHeatBal->People(peopleNum).spaceIndex,
                                           1.0,
                                           "People",
                                           state.dataHeatBal->People(peopleNum).Name,
                                           IntGainTypeOf_People,
                                           &state.dataHeatBal->People(peopleNum).ConGainRate,
                                           nullptr,
                                           &state.dataHeatBal->People(peopleNum).RadGainRate,
                                           &state.dataHeatBal->People(peopleNum).LatGainRate,
                                           nullptr,
                                           &state.dataHeatBal->People(peopleNum).CO2GainRate);
                }
            }

            // transfer the nominal number of people in a zone to the tabular reporting
            for (int Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                if (state.dataHeatBal->Zone(Loop).TotOccupants > 0.0) {
                    if (state.dataHeatBal->Zone(Loop).FloorArea > 0.0 &&
                        state.dataHeatBal->Zone(Loop).FloorArea / state.dataHeatBal->Zone(Loop).TotOccupants < 0.1) {
                        ShowWarningError(state,
                                         std::string{RoutineName} + "Zone=\"" + state.dataHeatBal->Zone(Loop).Name +
                                             "\" occupant density is extremely high.");
                        if (state.dataHeatBal->Zone(Loop).FloorArea > 0.0) {
                            ShowContinueError(state,
                                              format("Occupant Density=[{:.0R}] person/m2.",
                                                     state.dataHeatBal->Zone(Loop).TotOccupants / state.dataHeatBal->Zone(Loop).FloorArea));
                        }
                        ShowContinueError(state,
                                          format("Occupant Density=[{:.3R}] m2/person. Problems in Temperature Out of Bounds may result.",
                                                 state.dataHeatBal->Zone(Loop).FloorArea / state.dataHeatBal->Zone(Loop).TotOccupants));
                    }
                    Real64 maxOccupLoad = 0.0;
                    int OptionNum = 0;
                    for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotPeople; ++Loop1) {
                        if (state.dataHeatBal->People(Loop1).ZonePtr != Loop) continue;
                        if (maxOccupLoad < GetScheduleMaxValue(state, state.dataHeatBal->People(Loop1).NumberOfPeoplePtr) *
                                               state.dataHeatBal->People(Loop1).NumberOfPeople) {
                            maxOccupLoad = GetScheduleMaxValue(state, state.dataHeatBal->People(Loop1).NumberOfPeoplePtr) *
                                           state.dataHeatBal->People(Loop1).NumberOfPeople;
                            OptionNum = Loop1;
                        }
                    }
                    if (maxOccupLoad > state.dataHeatBal->Zone(Loop).TotOccupants) {
                        if (state.dataHeatBal->Zone(Loop).FloorArea > 0.0 && state.dataHeatBal->Zone(Loop).FloorArea / maxOccupLoad < 0.1) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + "Zone=\"" + state.dataHeatBal->Zone(Loop).Name +
                                                 "\" occupant density at a maximum schedule value is extremely high.");
                            if (state.dataHeatBal->Zone(Loop).FloorArea > 0.0) {
                                ShowContinueError(
                                    state, format("Occupant Density=[{:.0R}] person/m2.", maxOccupLoad / state.dataHeatBal->Zone(Loop).FloorArea));
                            }
                            ShowContinueError(state,
                                              format("Occupant Density=[{:.3R}] m2/person. Problems in Temperature Out of Bounds may result.",
                                                     state.dataHeatBal->Zone(Loop).FloorArea / maxOccupLoad));
                            ShowContinueError(state,
                                              "Check values in People=" + state.dataHeatBal->People(OptionNum).Name + ", Number of People Schedule=" +
                                                  GetScheduleName(state, state.dataHeatBal->People(OptionNum).NumberOfPeoplePtr));
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
        } // TotPeople > 0

        setupIHGZonesAndSpaces(state,
                               lightsModuleObject,
                               state.dataHeatBal->LightsObjects,
                               state.dataHeatBal->NumLightsStatements,
                               state.dataHeatBal->TotLights,
                               ErrorsFound);

        if (state.dataHeatBal->TotLights > 0) {
            state.dataHeatBal->Lights.allocate(state.dataHeatBal->TotLights);
            bool CheckSharedExhaustFlag = false;
            int lightsNum = 0;
            for (int lightsInputNum = 1; lightsInputNum <= state.dataHeatBal->NumLightsStatements; ++lightsInputNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         lightsModuleObject,
                                                                         lightsInputNum,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                auto &thisLightsInput = state.dataHeatBal->LightsObjects(lightsInputNum);
                // Create one Lights instance for every space associated with this Lights input object
                for (int Item1 = 1; Item1 <= thisLightsInput.numOfSpaces; ++Item1) {
                    ++lightsNum;
                    auto &thisLights = state.dataHeatBal->Lights(lightsNum);
                    int const spaceNum = thisLightsInput.spaceNums(Item1);
                    int const zoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
                    thisLights.Name = thisLightsInput.names(Item1);
                    thisLights.spaceIndex = spaceNum;
                    thisLights.ZonePtr = zoneNum;

                    thisLights.SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (thisLights.SchedPtr == 0) {
                        if (Item1 == 1) {
                            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                            } else {
                                ShowSevereError(state,
                                                std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                            }
                            ErrorsFound = true;
                        }
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, thisLights.SchedPtr);
                        SchMax = GetScheduleMaxValue(state, thisLights.SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (Item1 == 1) {
                                if (SchMin < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                    ErrorsFound = true;
                                }
                            }
                            if (Item1 == 1) {
                                if (SchMax < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                        // Set space load fraction
                        auto const lightingLevel(AlphaName(4));
                        if (lightingLevel == "LIGHTINGLEVEL") {
                            Real64 spaceFrac = 1.0;
                            if (thisLightsInput.numOfSpaces > 1) {
                                Real64 const zoneArea = state.dataHeatBal->Zone(zoneNum).FloorArea;
                                if (zoneArea > 0.0) {
                                    spaceFrac = state.dataHeatBal->space(spaceNum).floorArea / zoneArea;
                                } else {
                                    ShowSevereError(state,
                                                    std::string(RoutineName) + "Zone floor area is zero when allocating Lights loads to Spaces.");
                                    ShowContinueError(
                                        state, "Occurs for Lights object =" + AlphaName(1) + " in Zone=" + state.dataHeatBal->Zone(zoneNum).Name);
                                    ErrorsFound = true;
                                }
                            }

                            thisLights.DesignLevel = IHGNumbers(1) * spaceFrac;
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 Lights will result.");
                            }
                        } else if (lightingLevel == "WATTS/AREA") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    thisLights.DesignLevel = IHGNumbers(2) * state.dataHeatBal->space(spaceNum).floorArea;
                                    if ((state.dataHeatBal->space(spaceNum).floorArea <= 0.0) &&
                                        !state.dataHeatBal->space(spaceNum).isRemainderSpace) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + lightsModuleObject + "=\"" + thisLights.Name + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Space Floor Area = 0.  0 Lights will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           lightsModuleObject,
                                                           thisLights.Name,
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 Lights will result.");
                            }
                        } else if (lightingLevel == "WATTS/PERSON") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    thisLights.DesignLevel = IHGNumbers(3) * state.dataHeatBal->space(spaceNum).totOccupants;
                                    if (state.dataHeatBal->space(spaceNum).totOccupants <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + lightsModuleObject + "=\"" + thisLights.Name + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Total Occupants = 0.  0 Lights will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           lightsModuleObject,
                                                           thisLights.Name,
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 Lights will result.");
                            }
                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"LightingLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max lighting level
                    thisLights.NomMinDesignLevel = thisLights.DesignLevel * SchMin;
                    thisLights.NomMaxDesignLevel = thisLights.DesignLevel * SchMax;

                    thisLights.FractionReturnAir = IHGNumbers(4);
                    thisLights.FractionRadiant = IHGNumbers(5);
                    thisLights.FractionShortWave = IHGNumbers(6);
                    thisLights.FractionReplaceable = IHGNumbers(7);
                    thisLights.FractionReturnAirPlenTempCoeff1 = IHGNumbers(8);
                    thisLights.FractionReturnAirPlenTempCoeff2 = IHGNumbers(9);

                    thisLights.FractionConvected = 1.0 - (thisLights.FractionReturnAir + thisLights.FractionRadiant + thisLights.FractionShortWave);
                    if (std::abs(thisLights.FractionConvected) <= 0.001) thisLights.FractionConvected = 0.0;
                    if (thisLights.FractionConvected < 0.0) {
                        if (Item1 == 1) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + lightsModuleObject + "=\"" + thisLights.Name + "\", Sum of Fractions > 1.0");
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
                        thisLights.EndUseSubcategory = AlphaName(5);
                    } else {
                        thisLights.EndUseSubcategory = "General";
                    }

                    if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                        thisLights.FractionReturnAirIsCalculated = false;
                    } else if (AlphaName(6) != "YES" && AlphaName(6) != "NO") {
                        if (Item1 == 1) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + lightsModuleObject + "=\"" + thisLightsInput.Name + "\", invalid " +
                                                 state.dataIPShortCut->cAlphaFieldNames(6) + ", value  =" + AlphaName(6));
                            ShowContinueError(state, ".. Return Air Fraction from Plenum will NOT be calculated.");
                        }
                        thisLights.FractionReturnAirIsCalculated = false;
                    } else {
                        thisLights.FractionReturnAirIsCalculated = (AlphaName(6) == "YES");
                    }

                    // Set return air node number
                    thisLights.ZoneReturnNum = 0;
                    thisLights.RetNodeName = "";
                    if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                        if (thisLightsInput.ZoneListActive) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + lightsModuleObject + "=\"" + thisLightsInput.Name +
                                                "\": " + state.dataIPShortCut->cAlphaFieldNames(7) + " must be blank when using a ZoneList.");
                            ErrorsFound = true;
                        } else {
                            thisLights.RetNodeName = AlphaName(7);
                        }
                    }
                    if (thisLights.ZonePtr > 0) {
                        thisLights.ZoneReturnNum =
                            DataZoneEquipment::GetReturnNumForZone(state, state.dataHeatBal->Zone(zoneNum).Name, thisLights.RetNodeName);
                    }

                    if ((thisLights.ZoneReturnNum == 0) && (thisLights.FractionReturnAir > 0.0) && (!state.dataIPShortCut->lAlphaFieldBlanks(7))) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                            state.dataIPShortCut->cAlphaFieldNames(7) + " =" + AlphaName(7));
                        ShowContinueError(state, "No matching Zone Return Air Node found.");
                        ErrorsFound = true;
                    }
                    // Set exhaust air node number
                    thisLights.ZoneExhaustNodeNum = 0;
                    if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                        if (thisLightsInput.ZoneListActive) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + lightsModuleObject + "=\"" + thisLightsInput.Name +
                                                "\": " + state.dataIPShortCut->cAlphaFieldNames(8) + " must be blank when using a ZoneList.");
                            ErrorsFound = true;
                        } else {
                            bool exhaustNodeError = false;
                            thisLights.ZoneExhaustNodeNum = GetOnlySingleNode(state,
                                                                              AlphaName(8),
                                                                              exhaustNodeError,
                                                                              lightsModuleObject,
                                                                              thisLights.Name,
                                                                              DataLoopNode::NodeFluidType::Air,
                                                                              DataLoopNode::NodeConnectionType::ZoneExhaust,
                                                                              NodeInputManager::compFluidStream::Primary,
                                                                              ObjectIsNotParent);
                            if (!exhaustNodeError) { // GetOnlySingleNode will throw error messages if this is a NodeList Name and for other issues
                                exhaustNodeError =
                                    DataZoneEquipment::VerifyLightsExhaustNodeForZone(state, thisLights.ZonePtr, thisLights.ZoneExhaustNodeNum);
                            }
                            if (exhaustNodeError) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(8) + " = " + AlphaName(8));
                                ShowContinueError(state, "No matching Zone Exhaust Air Node found.");
                                ErrorsFound = true;
                            } else {
                                if (thisLights.ZoneReturnNum > 0) {
                                    state.dataZoneEquip->ZoneEquipConfig(state.dataHeatBal->Zone(thisLights.ZonePtr).ZoneEqNum)
                                        .ReturnNodeExhaustNodeNum(thisLights.ZoneReturnNum) = thisLights.ZoneExhaustNodeNum;
                                    CheckSharedExhaustFlag = true;
                                } else {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + lightsModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(8) + " =" + AlphaName(8) + " is not used");
                                    ShowContinueError(
                                        state, "No matching Zone Return Air Node found. The Exhaust Node requires Return Node to work together");
                                    ErrorsFound = true;
                                }
                            }
                        }

                        if (thisLights.ZonePtr <= 0) continue; // Error, will be caught and terminated later
                    }
                }
            }
            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                for (int lightsNum = 1; lightsNum <= state.dataHeatBal->TotLights; ++lightsNum) {
                    SetupEMSActuator(state,
                                     "Lights",
                                     state.dataHeatBal->Lights(lightsNum).Name,
                                     "Electricity Rate",
                                     "[W]",
                                     state.dataHeatBal->Lights(lightsNum).EMSLightsOn,
                                     state.dataHeatBal->Lights(lightsNum).EMSLightingPower);
                    SetupEMSInternalVariable(state,
                                             "Lighting Power Design Level",
                                             state.dataHeatBal->Lights(lightsNum).Name,
                                             "[W]",
                                             state.dataHeatBal->Lights(lightsNum).DesignLevel);
                } // EMS
            }
            for (int lightsNum = 1; lightsNum <= state.dataHeatBal->TotLights; ++lightsNum) {
                int spaceNum = state.dataHeatBal->Lights(lightsNum).spaceIndex;
                int zoneNum = state.dataHeatBal->Lights(lightsNum).ZonePtr;
                // setup internal gains
                int returnNodeNum = 0;
                if ((state.dataHeatBal->Lights(lightsNum).ZoneReturnNum > 0) &&
                    (state.dataHeatBal->Lights(lightsNum).ZoneReturnNum <= state.dataZoneEquip->ZoneEquipConfig(zoneNum).NumReturnNodes)) {
                    returnNodeNum = state.dataZoneEquip->ZoneEquipConfig(zoneNum).ReturnNode(state.dataHeatBal->Lights(lightsNum).ZoneReturnNum);
                }
                if (!ErrorsFound) {
                    SetupSpaceInternalGain(state,
                                           state.dataHeatBal->Lights(lightsNum).spaceIndex,
                                           1.0,
                                           "Lights",
                                           state.dataHeatBal->Lights(lightsNum).Name,
                                           IntGainTypeOf_Lights,
                                           &state.dataHeatBal->Lights(lightsNum).ConGainRate,
                                           &state.dataHeatBal->Lights(lightsNum).RetAirGainRate,
                                           &state.dataHeatBal->Lights(lightsNum).RadGainRate,
                                           nullptr,
                                           nullptr,
                                           nullptr,
                                           nullptr,
                                           returnNodeNum);
                }

                if (state.dataHeatBal->Lights(lightsNum).FractionReturnAir > 0)
                    state.dataHeatBal->Zone(state.dataHeatBal->Lights(lightsNum).ZonePtr).HasLtsRetAirGain = true;
                // send values to predefined lighting summary report
                liteName = state.dataHeatBal->Lights(lightsNum).Name;
                Real64 mult = state.dataHeatBal->Zone(zoneNum).Multiplier * state.dataHeatBal->Zone(zoneNum).ListMultiplier;
                Real64 spaceArea = state.dataHeatBal->space(spaceNum).floorArea;
                state.dataInternalHeatGains->sumArea += spaceArea * mult;
                state.dataInternalHeatGains->sumPower += state.dataHeatBal->Lights(lightsNum).DesignLevel * mult;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtZone, liteName, state.dataHeatBal->Zone(zoneNum).Name);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtSpace, liteName, state.dataHeatBal->space(spaceNum).Name);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtSpaceType, liteName, state.dataHeatBal->space(spaceNum).spaceType);
                if (spaceArea > 0.0) {
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchInLtDens, liteName, state.dataHeatBal->Lights(lightsNum).DesignLevel / spaceArea, 4);
                } else {
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtDens, liteName, DataPrecisionGlobals::constant_zero, 4);
                }
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtArea, liteName, spaceArea * mult);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtPower, liteName, state.dataHeatBal->Lights(lightsNum).DesignLevel * mult);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchInLtEndUse, liteName, state.dataHeatBal->Lights(lightsNum).EndUseSubcategory);
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchInLtSchd, liteName, GetScheduleName(state, state.dataHeatBal->Lights(lightsNum).SchedPtr));
                PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchInLtRetAir, liteName, state.dataHeatBal->Lights(lightsNum).FractionReturnAir, 4);
            } // Item1 - Number of Lights instances
            if (CheckSharedExhaustFlag) {
                DataZoneEquipment::CheckSharedExhaust(state);
                Array1D_bool ReturnNodeShared; // zone supply air inlet nodes
                ReturnNodeShared.allocate(state.dataHeatBal->TotLights);
                ReturnNodeShared = false;
                for (int Loop = 1; Loop <= state.dataHeatBal->TotLights; ++Loop) {
                    int ZoneNum = state.dataHeatBal->Lights(Loop).ZonePtr;
                    int ReturnNum = state.dataHeatBal->Lights(Loop).ZoneReturnNum;
                    int ExhaustNodeNum = state.dataHeatBal->Lights(Loop).ZoneExhaustNodeNum;
                    if (ReturnNum == 0 || ExhaustNodeNum == 0) continue;
                    for (int Loop1 = Loop + 1; Loop1 <= state.dataHeatBal->TotLights; ++Loop1) {
                        if (ZoneNum != state.dataHeatBal->Lights(Loop1).ZonePtr) continue;
                        if (ReturnNodeShared(Loop1)) continue;
                        if (ReturnNum == state.dataHeatBal->Lights(Loop1).ZoneReturnNum &&
                            ExhaustNodeNum != state.dataHeatBal->Lights(Loop1).ZoneExhaustNodeNum) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + lightsModuleObject +
                                                ": Duplicated Return Air Node = " + state.dataHeatBal->Lights(Loop1).RetNodeName + " is found, ");
                            ShowContinueError(state,
                                              " in both Lights objects = " + state.dataHeatBal->Lights(Loop).Name + " and " +
                                                  state.dataHeatBal->Lights(Loop1).Name + ".");
                            ErrorsFound = true;
                            ReturnNodeShared(Loop1) = true;
                        }
                    }
                }
                ReturnNodeShared.deallocate();
            }
        } // TotLights > 0 check
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

        setupIHGZonesAndSpaces(state,
                               elecEqModuleObject,
                               state.dataHeatBal->ZoneElectricObjects,
                               state.dataHeatBal->NumZoneElectricStatements,
                               state.dataHeatBal->TotElecEquip,
                               ErrorsFound);

        if (state.dataHeatBal->TotElecEquip > 0) {
            state.dataHeatBal->ZoneElectric.allocate(state.dataHeatBal->TotElecEquip);
            int elecEqNum = 0;
            for (int elecEqInputNum = 1; elecEqInputNum <= state.dataHeatBal->NumZoneElectricStatements; ++elecEqInputNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         elecEqModuleObject,
                                                                         elecEqInputNum,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                auto &thisElecEqInput = state.dataHeatBal->ZoneElectricObjects(elecEqInputNum);
                for (int Item1 = 1; Item1 <= thisElecEqInput.numOfSpaces; ++Item1) {
                    ++elecEqNum;
                    auto &thisZoneElectric = state.dataHeatBal->ZoneElectric(elecEqNum);
                    int const spaceNum = thisElecEqInput.spaceNums(Item1);
                    int const zoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
                    thisZoneElectric.Name = thisElecEqInput.names(Item1);
                    thisZoneElectric.spaceIndex = spaceNum;
                    thisZoneElectric.ZonePtr = zoneNum;

                    thisZoneElectric.SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (thisZoneElectric.SchedPtr == 0) {
                        if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + elecEqModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + elecEqModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                        }
                        ErrorsFound = true;
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, thisZoneElectric.SchedPtr);
                        SchMax = GetScheduleMaxValue(state, thisZoneElectric.SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (SchMin < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + elecEqModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                ErrorsFound = true;
                            }
                            if (SchMax < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + elecEqModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                            Real64 spaceFrac = 1.0;
                            if (thisElecEqInput.numOfSpaces > 1) {
                                Real64 const zoneArea = state.dataHeatBal->Zone(zoneNum).FloorArea;
                                if (zoneArea > 0.0) {
                                    spaceFrac = state.dataHeatBal->space(spaceNum).floorArea / zoneArea;
                                } else {
                                    ShowSevereError(state,
                                                    std::string(RoutineName) +
                                                        "Zone floor area is zero when allocating ElectricEquipment loads to Spaces.");
                                    ShowContinueError(state,
                                                      "Occurs for ElectricEquipment object =" + thisElecEqInput.Name +
                                                          " in Zone=" + state.dataHeatBal->Zone(thisZoneElectric.ZonePtr).Name);
                                    ErrorsFound = true;
                                }
                            }
                            thisZoneElectric.DesignLevel = IHGNumbers(1) * spaceFrac;
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + elecEqModuleObject + "=\"" + thisElecEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 Electric Equipment will result.");
                            }
                        } else if (equipmentLevel == "WATTS/AREA") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    thisZoneElectric.DesignLevel = IHGNumbers(2) * state.dataHeatBal->space(spaceNum).floorArea;
                                    if ((state.dataHeatBal->space(spaceNum).floorArea <= 0.0) &&
                                        !state.dataHeatBal->space(spaceNum).isRemainderSpace) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + elecEqModuleObject + "=\"" + thisZoneElectric.Name +
                                                             "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Space Floor Area = 0.  0 Electric Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           elecEqModuleObject,
                                                           thisZoneElectric.Name,
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + elecEqModuleObject + "=\"" + thisElecEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 Electric Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/PERSON") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    thisZoneElectric.DesignLevel = IHGNumbers(3) * state.dataHeatBal->space(spaceNum).totOccupants;
                                    if (state.dataHeatBal->space(spaceNum).totOccupants <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + elecEqModuleObject + "=\"" + thisZoneElectric.Name +
                                                             "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Total Occupants = 0.  0 Electric Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           elecEqModuleObject,
                                                           thisZoneElectric.Name,
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + elecEqModuleObject + "=\"" + thisElecEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 Electric Equipment will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + elecEqModuleObject + "=\"" + thisElecEqInput.Name + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max equipment level
                    thisZoneElectric.NomMinDesignLevel = thisZoneElectric.DesignLevel * SchMin;
                    thisZoneElectric.NomMaxDesignLevel = thisZoneElectric.DesignLevel * SchMax;

                    thisZoneElectric.FractionLatent = IHGNumbers(4);
                    thisZoneElectric.FractionRadiant = IHGNumbers(5);
                    thisZoneElectric.FractionLost = IHGNumbers(6);
                    // FractionConvected is a calculated field
                    thisZoneElectric.FractionConvected =
                        1.0 - (thisZoneElectric.FractionLatent + thisZoneElectric.FractionRadiant + thisZoneElectric.FractionLost);
                    if (std::abs(thisZoneElectric.FractionConvected) <= 0.001) thisZoneElectric.FractionConvected = 0.0;
                    if (thisZoneElectric.FractionConvected < 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + elecEqModuleObject + "=\"" + thisElecEqInput.Name + "\", Sum of Fractions > 1.0");
                        ErrorsFound = true;
                    }

                    if (NumAlpha > 4) {
                        thisZoneElectric.EndUseSubcategory = AlphaName(5);
                    } else {
                        thisZoneElectric.EndUseSubcategory = "General";
                    }
                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "ElectricEquipment",
                                         thisZoneElectric.Name,
                                         "Electricity Rate",
                                         "[W]",
                                         thisZoneElectric.EMSZoneEquipOverrideOn,
                                         thisZoneElectric.EMSEquipPower);
                        SetupEMSInternalVariable(
                            state, "Plug and Process Power Design Level", thisZoneElectric.Name, "[W]", thisZoneElectric.DesignLevel);
                    } // EMS
                    if (!ErrorsFound) {
                        SetupSpaceInternalGain(state,
                                               thisZoneElectric.spaceIndex,
                                               1.0,
                                               "ElectricEquipment",
                                               thisZoneElectric.Name,
                                               IntGainTypeOf_ElectricEquipment,
                                               &thisZoneElectric.ConGainRate,
                                               nullptr,
                                               &thisZoneElectric.RadGainRate,
                                               &thisZoneElectric.LatGainRate);
                    }
                } // for elecEqInputNum.NumOfSpaces
            }     // for elecEqInputNum
        }         // TotElecEquip > 0

        setupIHGZonesAndSpaces(state,
                               gasEqModuleObject,
                               state.dataHeatBal->ZoneGasObjects,
                               state.dataHeatBal->NumZoneGasStatements,
                               state.dataHeatBal->TotGasEquip,
                               ErrorsFound);

        if (state.dataHeatBal->TotGasEquip > 0) {
            state.dataHeatBal->ZoneGas.allocate(state.dataHeatBal->TotGasEquip);
            int gasEqNum = 0;
            for (int gasEqInputNum = 1; gasEqInputNum <= state.dataHeatBal->NumZoneGasStatements; ++gasEqInputNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         gasEqModuleObject,
                                                                         gasEqInputNum,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                auto &thisGasEqInput = state.dataHeatBal->ZoneGasObjects(gasEqInputNum);
                for (int Item1 = 1; Item1 <= thisGasEqInput.numOfSpaces; ++Item1) {
                    ++gasEqNum;
                    auto &thisZoneGas = state.dataHeatBal->ZoneGas(gasEqNum);
                    int const spaceNum = thisGasEqInput.spaceNums(Item1);
                    int const zoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
                    thisZoneGas.Name = thisGasEqInput.names(Item1);
                    thisZoneGas.spaceIndex = spaceNum;
                    thisZoneGas.ZonePtr = zoneNum;

                    thisZoneGas.SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (thisZoneGas.SchedPtr == 0) {
                        if (Item1 == 1) {
                            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + gasEqModuleObject + "=\"" + thisGasEqInput.Name + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                            } else {
                                ShowSevereError(state,
                                                std::string{RoutineName} + gasEqModuleObject + "=\"" + thisGasEqInput.Name + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                            }
                            ErrorsFound = true;
                        }
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, thisZoneGas.SchedPtr);
                        SchMax = GetScheduleMaxValue(state, thisZoneGas.SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (Item1 == 1) {
                                if (SchMin < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + gasEqModuleObject + "=\"" + thisGasEqInput.Name + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                    ErrorsFound = true;
                                }
                            }
                            if (Item1 == 1) {
                                if (SchMax < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + gasEqModuleObject + "=\"" + thisGasEqInput.Name + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }

                    // Gas equipment design level calculation method.
                    {
                        auto const equipmentLevel(AlphaName(4));
                        if (equipmentLevel == "EQUIPMENTLEVEL") {
                            Real64 spaceFrac = 1.0;
                            if (thisGasEqInput.numOfSpaces > 1) {
                                Real64 const zoneArea = state.dataHeatBal->Zone(zoneNum).FloorArea;
                                if (zoneArea > 0.0) {
                                    spaceFrac = state.dataHeatBal->space(spaceNum).floorArea / zoneArea;
                                } else {
                                    ShowSevereError(
                                        state, std::string(RoutineName) + "Zone floor area is zero when allocating GasEquipment loads to Spaces.");
                                    ShowContinueError(state,
                                                      "Occurs for GasEquipment object =" + thisGasEqInput.Name +
                                                          " in Zone=" + state.dataHeatBal->Zone(thisZoneGas.ZonePtr).Name);
                                    ErrorsFound = true;
                                }
                            }
                            thisZoneGas.DesignLevel = IHGNumbers(1) * spaceFrac;
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + gasEqModuleObject + "=\"" + thisGasEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 Gas Equipment will result.");
                            }
                        } else if (equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    thisZoneGas.DesignLevel = IHGNumbers(2) * state.dataHeatBal->space(spaceNum).floorArea;
                                    if ((state.dataHeatBal->space(spaceNum).floorArea <= 0.0) &&
                                        !state.dataHeatBal->space(spaceNum).isRemainderSpace) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + gasEqModuleObject + "=\"" + thisZoneGas.Name + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Space Floor Area = 0.  0 Gas Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           gasEqModuleObject,
                                                           thisGasEqInput.Name,
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + gasEqModuleObject + "=\"" + thisGasEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 Gas Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    thisZoneGas.DesignLevel = IHGNumbers(3) * state.dataHeatBal->space(spaceNum).totOccupants;
                                    if (state.dataHeatBal->space(spaceNum).totOccupants <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + gasEqModuleObject + "=\"" + thisZoneGas.Name + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Total Occupants = 0.  0 Gas Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           gasEqModuleObject,
                                                           thisGasEqInput.Name,
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + gasEqModuleObject + "=\"" + thisGasEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 Gas Equipment will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + gasEqModuleObject + "=\"" + thisGasEqInput.Name + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max equipment level
                    thisZoneGas.NomMinDesignLevel = thisZoneGas.DesignLevel * SchMin;
                    thisZoneGas.NomMaxDesignLevel = thisZoneGas.DesignLevel * SchMax;

                    thisZoneGas.FractionLatent = IHGNumbers(4);
                    thisZoneGas.FractionRadiant = IHGNumbers(5);
                    thisZoneGas.FractionLost = IHGNumbers(6);

                    if ((NumNumber == 7) || (!state.dataIPShortCut->lNumericFieldBlanks(7))) {
                        thisZoneGas.CO2RateFactor = IHGNumbers(7);
                    }
                    if (thisZoneGas.CO2RateFactor < 0.0) {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", {} < 0.0, value ={:.2R}",
                                               RoutineName,
                                               gasEqModuleObject,
                                               thisGasEqInput.Name,
                                               state.dataIPShortCut->cNumericFieldNames(7),
                                               IHGNumbers(7)));
                        ErrorsFound = true;
                    }
                    if (thisZoneGas.CO2RateFactor > 4.0e-7) {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", {} > 4.0E-7, value ={:.2R}",
                                               RoutineName,
                                               gasEqModuleObject,
                                               thisGasEqInput.Name,
                                               state.dataIPShortCut->cNumericFieldNames(7),
                                               IHGNumbers(7)));
                        ErrorsFound = true;
                    }
                    // FractionConvected is a calculated field
                    thisZoneGas.FractionConvected = 1.0 - (thisZoneGas.FractionLatent + thisZoneGas.FractionRadiant + thisZoneGas.FractionLost);
                    if (std::abs(thisZoneGas.FractionConvected) <= 0.001) thisZoneGas.FractionConvected = 0.0;
                    if (thisZoneGas.FractionConvected < 0.0) {
                        if (Item1 == 1) {
                            ShowSevereError(
                                state, std::string{RoutineName} + gasEqModuleObject + "=\"" + thisGasEqInput.Name + "\", Sum of Fractions > 1.0");
                            ErrorsFound = true;
                        }
                    }

                    if (NumAlpha > 4) {
                        thisZoneGas.EndUseSubcategory = AlphaName(5);
                    } else {
                        thisZoneGas.EndUseSubcategory = "General";
                    }

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "GasEquipment",
                                         thisZoneGas.Name,
                                         "NaturalGas Rate",
                                         "[W]",
                                         thisZoneGas.EMSZoneEquipOverrideOn,
                                         thisZoneGas.EMSEquipPower);
                        SetupEMSInternalVariable(state, "Gas Process Power Design Level", thisZoneGas.Name, "[W]", thisZoneGas.DesignLevel);
                    } // EMS

                    if (!ErrorsFound)
                        SetupSpaceInternalGain(state,
                                               thisZoneGas.spaceIndex,
                                               1.0,
                                               "GasEquipment",
                                               thisZoneGas.Name,
                                               IntGainTypeOf_GasEquipment,
                                               &thisZoneGas.ConGainRate,
                                               nullptr,
                                               &thisZoneGas.RadGainRate,
                                               &thisZoneGas.LatGainRate,
                                               nullptr,
                                               &thisZoneGas.CO2GainRate);

                } // for gasEqInputNum.NumOfSpaces
            }     // for gasEqInputNum
        }         // TotGasEquip > 0

        setupIHGZonesAndSpaces(state,
                               hwEqModuleObject,
                               state.dataHeatBal->HotWaterEqObjects,
                               state.dataHeatBal->NumHotWaterEqStatements,
                               state.dataHeatBal->TotHWEquip,
                               ErrorsFound);

        if (state.dataHeatBal->TotHWEquip > 0) {
            state.dataHeatBal->ZoneHWEq.allocate(state.dataHeatBal->TotHWEquip);
            int hwEqNum = 0;
            for (int hwEqInputNum = 1; hwEqInputNum <= state.dataHeatBal->NumHotWaterEqStatements; ++hwEqInputNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         hwEqModuleObject,
                                                                         hwEqInputNum,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                auto &thisHWEqInput = state.dataHeatBal->HotWaterEqObjects(hwEqInputNum);
                for (int Item1 = 1; Item1 <= thisHWEqInput.numOfSpaces; ++Item1) {
                    ++hwEqNum;
                    auto &thisZoneHWEq = state.dataHeatBal->ZoneHWEq(hwEqNum);
                    int const spaceNum = thisHWEqInput.spaceNums(Item1);
                    int const zoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
                    thisZoneHWEq.Name = thisHWEqInput.names(Item1);
                    thisZoneHWEq.spaceIndex = spaceNum;
                    thisZoneHWEq.ZonePtr = zoneNum;

                    thisZoneHWEq.SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (thisZoneHWEq.SchedPtr == 0) {
                        if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + hwEqModuleObject + "=\"" + thisHWEqInput.Name + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + hwEqModuleObject + "=\"" + thisHWEqInput.Name + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                        }
                        ErrorsFound = true;
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, thisZoneHWEq.SchedPtr);
                        SchMax = GetScheduleMaxValue(state, thisZoneHWEq.SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (SchMin < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + hwEqModuleObject + "=\"" + thisHWEqInput.Name + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                ErrorsFound = true;
                            }
                            if (SchMax < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + hwEqModuleObject + "=\"" + thisHWEqInput.Name + "\", " +
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
                            Real64 spaceFrac = 1.0;
                            if (thisHWEqInput.numOfSpaces > 1) {
                                Real64 const zoneArea = state.dataHeatBal->Zone(zoneNum).FloorArea;
                                if (zoneArea > 0.0) {
                                    spaceFrac = state.dataHeatBal->space(spaceNum).floorArea / zoneArea;
                                } else {
                                    ShowSevereError(state,
                                                    std::string(RoutineName) +
                                                        "Zone floor area is zero when allocating HotWaterEquipment loads to Spaces.");
                                    ShowContinueError(state,
                                                      "Occurs for HotWaterEquipment object =" + thisHWEqInput.Name +
                                                          " in Zone=" + state.dataHeatBal->Zone(zoneNum).Name);
                                    ErrorsFound = true;
                                }
                            }
                            thisZoneHWEq.DesignLevel = IHGNumbers(1) * spaceFrac;
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + hwEqModuleObject + "=\"" + thisHWEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 Hot Water Equipment will result.");
                            }
                        } else if (equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    thisZoneHWEq.DesignLevel = IHGNumbers(2) * state.dataHeatBal->space(spaceNum).floorArea;
                                    if ((state.dataHeatBal->space(spaceNum).floorArea <= 0.0) &&
                                        !state.dataHeatBal->space(spaceNum).isRemainderSpace) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + hwEqModuleObject + "=\"" + thisZoneHWEq.Name + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Space Floor Area = 0.  0 Hot Water Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           hwEqModuleObject,
                                                           thisHWEqInput.Name,
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + hwEqModuleObject + "=\"" + thisHWEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 Hot Water Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    thisZoneHWEq.DesignLevel = IHGNumbers(3) * state.dataHeatBal->space(spaceNum).totOccupants;
                                    if (state.dataHeatBal->space(spaceNum).totOccupants <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + hwEqModuleObject + "=\"" + thisZoneHWEq.Name + "\", specifies " +
                                                             state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Total Occupants = 0.  0 Hot Water Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           hwEqModuleObject,
                                                           thisHWEqInput.Name,
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + hwEqModuleObject + "=\"" + thisHWEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 Hot Water Equipment will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + hwEqModuleObject + "=\"" + thisHWEqInput.Name + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max equipment level
                    thisZoneHWEq.NomMinDesignLevel = thisZoneHWEq.DesignLevel * SchMin;
                    thisZoneHWEq.NomMaxDesignLevel = thisZoneHWEq.DesignLevel * SchMax;

                    thisZoneHWEq.FractionLatent = IHGNumbers(4);
                    thisZoneHWEq.FractionRadiant = IHGNumbers(5);
                    thisZoneHWEq.FractionLost = IHGNumbers(6);
                    // FractionConvected is a calculated field
                    thisZoneHWEq.FractionConvected = 1.0 - (thisZoneHWEq.FractionLatent + thisZoneHWEq.FractionRadiant + thisZoneHWEq.FractionLost);
                    if (std::abs(thisZoneHWEq.FractionConvected) <= 0.001) thisZoneHWEq.FractionConvected = 0.0;
                    if (thisZoneHWEq.FractionConvected < 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + hwEqModuleObject + "=\"" + thisHWEqInput.Name + "\", Sum of Fractions > 1.0");
                        ErrorsFound = true;
                    }

                    if (NumAlpha > 4) {
                        thisZoneHWEq.EndUseSubcategory = AlphaName(5);
                    } else {
                        thisZoneHWEq.EndUseSubcategory = "General";
                    }

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "HotWaterEquipment",
                                         thisZoneHWEq.Name,
                                         "District Heating Power Level",
                                         "[W]",
                                         thisZoneHWEq.EMSZoneEquipOverrideOn,
                                         thisZoneHWEq.EMSEquipPower);
                        SetupEMSInternalVariable(state, "Process District Heat Design Level", thisZoneHWEq.Name, "[W]", thisZoneHWEq.DesignLevel);
                    } // EMS

                    if (!ErrorsFound)
                        SetupSpaceInternalGain(state,
                                               thisZoneHWEq.spaceIndex,
                                               1.0,
                                               "HotWaterEquipment",
                                               thisZoneHWEq.Name,
                                               IntGainTypeOf_HotWaterEquipment,
                                               &thisZoneHWEq.ConGainRate,
                                               nullptr,
                                               &thisZoneHWEq.RadGainRate,
                                               &thisZoneHWEq.LatGainRate);

                } // for hwEqInputNum.NumOfSpaces
            }     // for hwEqInputNum
        }         // TotHWEquip > 0

        setupIHGZonesAndSpaces(state,
                               stmEqModuleObject,
                               state.dataHeatBal->SteamEqObjects,
                               state.dataHeatBal->NumSteamEqStatements,
                               state.dataHeatBal->TotStmEquip,
                               ErrorsFound);

        if (state.dataHeatBal->TotStmEquip > 0) {
            state.dataHeatBal->ZoneSteamEq.allocate(state.dataHeatBal->TotStmEquip);
            int stmEqNum = 0;
            for (int stmEqInputNum = 1; stmEqInputNum <= state.dataHeatBal->NumSteamEqStatements; ++stmEqInputNum) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         stmEqModuleObject,
                                                                         stmEqInputNum,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                auto &thisStmEqInput = state.dataHeatBal->SteamEqObjects(stmEqInputNum);
                for (int Item1 = 1; Item1 <= thisStmEqInput.numOfSpaces; ++Item1) {
                    ++stmEqNum;
                    auto &thisZoneStmEq = state.dataHeatBal->ZoneSteamEq(stmEqNum);
                    int const spaceNum = thisStmEqInput.spaceNums(Item1);
                    int const zoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
                    thisZoneStmEq.Name = thisStmEqInput.names(Item1);
                    thisZoneStmEq.spaceIndex = spaceNum;
                    thisZoneStmEq.ZonePtr = zoneNum;

                    thisZoneStmEq.SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (thisZoneStmEq.SchedPtr == 0) {
                        if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + stmEqModuleObject + "=\"" + thisStmEqInput.Name + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + stmEqModuleObject + "=\"" + thisStmEqInput.Name + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                        }
                        ErrorsFound = true;
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, thisZoneStmEq.SchedPtr);
                        SchMax = GetScheduleMaxValue(state, thisZoneStmEq.SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (SchMin < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + stmEqModuleObject + "=\"" + thisStmEqInput.Name + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                ErrorsFound = true;
                            }
                            if (SchMax < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + stmEqModuleObject + "=\"" + thisStmEqInput.Name + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Steam equipment design level calculation method.
                    {
                        auto const equipmentLevel(AlphaName(4));
                        if (equipmentLevel == "EQUIPMENTLEVEL") {
                            Real64 spaceFrac = 1.0;
                            if (thisStmEqInput.numOfSpaces > 1) {
                                Real64 const zoneArea = state.dataHeatBal->Zone(zoneNum).FloorArea;
                                if (zoneArea > 0.0) {
                                    spaceFrac = state.dataHeatBal->space(spaceNum).floorArea / zoneArea;
                                } else {
                                    ShowSevereError(
                                        state, std::string(RoutineName) + "Zone floor area is zero when allocating SteamEquipment loads to Spaces.");
                                    ShowContinueError(state,
                                                      "Occurs for SteamEquipment object =" + thisStmEqInput.Name +
                                                          " in Zone=" + state.dataHeatBal->Zone(zoneNum).Name);
                                    ErrorsFound = true;
                                }
                            }
                            thisZoneStmEq.DesignLevel = IHGNumbers(1) * spaceFrac;
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + hwEqModuleObject + "=\"" + thisStmEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 Steam Equipment will result.");
                            }
                        } else if (equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA") {
                            if (spaceNum > 0) {
                                if (IHGNumbers(2) >= 0.0) {
                                    thisZoneStmEq.DesignLevel = IHGNumbers(2) * state.dataHeatBal->space(spaceNum).floorArea;
                                    if ((state.dataHeatBal->space(spaceNum).floorArea <= 0.0) &&
                                        !state.dataHeatBal->space(spaceNum).isRemainderSpace) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + stmEqModuleObject + "=\"" + thisZoneStmEq.Name +
                                                             "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Space Floor Area = 0.  0 Steam Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           stmEqModuleObject,
                                                           AlphaName(1),
                                                           state.dataIPShortCut->cNumericFieldNames(2),
                                                           IHGNumbers(2)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + stmEqModuleObject + "=\"" + thisStmEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 Steam Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON") {
                            if (spaceNum != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    thisZoneStmEq.DesignLevel = IHGNumbers(3) * state.dataHeatBal->space(spaceNum).totOccupants;
                                    if (state.dataHeatBal->space(spaceNum).totOccupants <= 0.0) {
                                        ShowWarningError(state,
                                                         std::string{RoutineName} + stmEqModuleObject + "=\"" + thisZoneStmEq.Name +
                                                             "\", specifies " + state.dataIPShortCut->cNumericFieldNames(2) +
                                                             ", but Total Occupants = 0.  0 Steam Equipment will result.");
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                           RoutineName,
                                                           stmEqModuleObject,
                                                           AlphaName(1),
                                                           state.dataIPShortCut->cNumericFieldNames(3),
                                                           IHGNumbers(3)));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + stmEqModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(3) +
                                                     ", but that field is blank.  0 Steam Equipment will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + stmEqModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Calculate nominal min/max equipment level
                    thisZoneStmEq.NomMinDesignLevel = thisZoneStmEq.DesignLevel * SchMin;
                    thisZoneStmEq.NomMaxDesignLevel = thisZoneStmEq.DesignLevel * SchMax;

                    thisZoneStmEq.FractionLatent = IHGNumbers(4);
                    thisZoneStmEq.FractionRadiant = IHGNumbers(5);
                    thisZoneStmEq.FractionLost = IHGNumbers(6);
                    // FractionConvected is a calculated field
                    thisZoneStmEq.FractionConvected =
                        1.0 - (thisZoneStmEq.FractionLatent + thisZoneStmEq.FractionRadiant + thisZoneStmEq.FractionLost);
                    if (std::abs(thisZoneStmEq.FractionConvected) <= 0.001) thisZoneStmEq.FractionConvected = 0.0;
                    if (thisZoneStmEq.FractionConvected < 0.0) {
                        ShowSevereError(state, std::string{RoutineName} + stmEqModuleObject + "=\"" + AlphaName(1) + "\", Sum of Fractions > 1.0");
                        ErrorsFound = true;
                    }

                    if (NumAlpha > 4) {
                        thisZoneStmEq.EndUseSubcategory = AlphaName(5);
                    } else {
                        thisZoneStmEq.EndUseSubcategory = "General";
                    }

                    if (thisZoneStmEq.ZonePtr <= 0) continue; // Error, will be caught and terminated later

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "SteamEquipment",
                                         thisZoneStmEq.Name,
                                         "District Heating Power Level",
                                         "[W]",
                                         thisZoneStmEq.EMSZoneEquipOverrideOn,
                                         thisZoneStmEq.EMSEquipPower);
                        SetupEMSInternalVariable(
                            state, "Process Steam District Heat Design Level", thisZoneStmEq.Name, "[W]", thisZoneStmEq.DesignLevel);
                    } // EMS

                    if (!ErrorsFound)
                        SetupSpaceInternalGain(state,
                                               thisZoneStmEq.spaceIndex,
                                               1.0,
                                               "SteamEquipment",
                                               thisZoneStmEq.Name,
                                               IntGainTypeOf_SteamEquipment,
                                               &thisZoneStmEq.ConGainRate,
                                               nullptr,
                                               &thisZoneStmEq.RadGainRate,
                                               &thisZoneStmEq.LatGainRate);

                } // for stmEqInputNum.NumOfSpaces
            }     // for stmEqInputNum
        }         // TotStmEquip > 0

        setupIHGZonesAndSpaces(state,
                               othEqModuleObject,
                               state.dataHeatBal->OtherEqObjects,
                               state.dataHeatBal->NumOtherEqStatements,
                               state.dataHeatBal->TotOthEquip,
                               ErrorsFound);

        if (state.dataHeatBal->TotOthEquip > 0) {
            state.dataHeatBal->ZoneOtherEq.allocate(state.dataHeatBal->TotOthEquip);
            int othEqNum = 0;
            for (int othEqInputNum = 1; othEqInputNum <= state.dataHeatBal->NumOtherEqStatements; ++othEqInputNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         othEqModuleObject,
                                                                         othEqInputNum,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                auto &thisOthEqInput = state.dataHeatBal->OtherEqObjects(othEqInputNum);
                for (int Item1 = 1; Item1 <= thisOthEqInput.numOfSpaces; ++Item1) {
                    ++othEqNum;
                    auto &thisZoneOthEq = state.dataHeatBal->ZoneOtherEq(othEqNum);
                    int const spaceNum = thisOthEqInput.spaceNums(Item1);
                    int const zoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
                    thisZoneOthEq.Name = thisOthEqInput.names(Item1);
                    thisZoneOthEq.spaceIndex = spaceNum;
                    thisZoneOthEq.ZonePtr = zoneNum;

                    std::string FuelTypeString("");
                    if (AlphaName(2) == "NONE") {
                        thisZoneOthEq.OtherEquipFuelType = ExteriorEnergyUse::ExteriorFuelUsage::Unknown;
                        FuelTypeString = AlphaName(2);
                    } else {
                        ExteriorEnergyUse::ValidateFuelType(state,
                                                            thisZoneOthEq.OtherEquipFuelType,
                                                            AlphaName(2),
                                                            FuelTypeString,
                                                            othEqModuleObject,
                                                            state.dataIPShortCut->cAlphaFieldNames(2),
                                                            AlphaName(2));
                        if (thisZoneOthEq.OtherEquipFuelType == ExteriorEnergyUse::ExteriorFuelUsage::Unknown ||
                            thisZoneOthEq.OtherEquipFuelType == ExteriorEnergyUse::ExteriorFuelUsage::WaterUse) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + othEqModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                                " entered=" + AlphaName(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                                thisOthEqInput.Name);
                            ErrorsFound = true;
                        }
                        thisZoneOthEq.otherEquipFuelTypeString = FuelTypeString; // Save for output variable setup later
                        // Build list of fuel types used in each zone and space (excluding None and Water)
                        bool found = false;
                        for (auto fuelType : state.dataHeatBal->Zone(zoneNum).otherEquipFuelTypeNums) {
                            if (thisZoneOthEq.OtherEquipFuelType == fuelType) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            state.dataHeatBal->Zone(zoneNum).otherEquipFuelTypeNums.emplace_back(thisZoneOthEq.OtherEquipFuelType);
                            state.dataHeatBal->Zone(zoneNum).otherEquipFuelTypeNames.emplace_back(FuelTypeString);
                        }
                        found = false;
                        for (auto fuelType : state.dataHeatBal->space(spaceNum).otherEquipFuelTypeNums) {
                            if (thisZoneOthEq.OtherEquipFuelType == fuelType) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            state.dataHeatBal->space(spaceNum).otherEquipFuelTypeNums.emplace_back(thisZoneOthEq.OtherEquipFuelType);
                            state.dataHeatBal->space(spaceNum).otherEquipFuelTypeNames.emplace_back(FuelTypeString);
                        }
                    }

                    thisZoneOthEq.SchedPtr = GetScheduleIndex(state, AlphaName(4));
                    SchMin = 0.0;
                    SchMax = 0.0;
                    if (thisZoneOthEq.SchedPtr == 0) {
                        if (state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + othEqModuleObject + "=\"" + thisOthEqInput.Name + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(4) + " is required.");
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + othEqModuleObject + "=\"" + thisOthEqInput.Name + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(4) + " entered=" + AlphaName(4));
                        }
                        ErrorsFound = true;
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, thisZoneOthEq.SchedPtr);
                        SchMax = GetScheduleMaxValue(state, thisZoneOthEq.SchedPtr);
                    }

                    // equipment design level calculation method.
                    unsigned int DesignLevelFieldNumber;
                    {
                        auto const equipmentLevel(AlphaName(5));
                        if (equipmentLevel == "EQUIPMENTLEVEL") {
                            DesignLevelFieldNumber = 1;
                            Real64 spaceFrac = 1.0;
                            if (thisOthEqInput.numOfSpaces > 1) {
                                Real64 const zoneArea = state.dataHeatBal->Zone(zoneNum).FloorArea;
                                if (zoneArea > 0.0) {
                                    spaceFrac = state.dataHeatBal->space(spaceNum).floorArea / zoneArea;
                                } else {
                                    ShowSevereError(
                                        state, std::string(RoutineName) + "Zone floor area is zero when allocating OtherEquipment loads to Spaces.");
                                    ShowContinueError(state,
                                                      "Occurs for OtherEquipment object =" + thisOthEqInput.Name +
                                                          " in Zone=" + state.dataHeatBal->Zone(zoneNum).Name);
                                    ErrorsFound = true;
                                }
                            }
                            thisZoneOthEq.DesignLevel = IHGNumbers(1) * spaceFrac;
                            if (state.dataIPShortCut->lNumericFieldBlanks(DesignLevelFieldNumber)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + othEqModuleObject + "=\"" + thisOthEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) +
                                                     ", but that field is blank.  0 Other Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA") {
                            DesignLevelFieldNumber = 2;
                            if (spaceNum > 0) {
                                thisZoneOthEq.DesignLevel = IHGNumbers(DesignLevelFieldNumber) * state.dataHeatBal->space(spaceNum).floorArea;
                                if ((state.dataHeatBal->space(spaceNum).floorArea <= 0.0) && !state.dataHeatBal->space(spaceNum).isRemainderSpace) {
                                    ShowWarningError(state,
                                                     std::string{RoutineName} + othEqModuleObject + "=\"" + thisZoneOthEq.Name + "\", specifies " +
                                                         state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) +
                                                         ", but Space Floor Area = 0.  0 Other Equipment will result.");
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(DesignLevelFieldNumber)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + othEqModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) +
                                                     ", but that field is blank.  0 Other Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON") {
                            DesignLevelFieldNumber = 3;
                            if (thisZoneOthEq.ZonePtr != 0) {
                                thisZoneOthEq.DesignLevel = IHGNumbers(3) * state.dataHeatBal->Zone(thisZoneOthEq.ZonePtr).TotOccupants;
                                if (state.dataHeatBal->Zone(thisZoneOthEq.ZonePtr).TotOccupants <= 0.0) {
                                    ShowWarningError(state,
                                                     std::string{RoutineName} + othEqModuleObject + "=\"" + thisZoneOthEq.Name + "\", specifies " +
                                                         state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) +
                                                         ", but Total Occupants = 0.  0 Other Equipment will result.");
                                }
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(DesignLevelFieldNumber)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + othEqModuleObject + "=\"" + thisOthEqInput.Name + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) +
                                                     ", but that field is blank.  0 Other Equipment will result.");
                            }

                        } else {
                            if (Item1 == 1) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + othEqModuleObject + "=\"" + thisOthEqInput.Name + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(5) + ", value  =" + AlphaName(5));
                                ShowContinueError(state, "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Throw an error if the design level is negative and we have a fuel type
                    if (thisZoneOthEq.DesignLevel < 0.0 && thisZoneOthEq.OtherEquipFuelType != ExteriorEnergyUse::ExteriorFuelUsage::Unknown) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + othEqModuleObject + "=\"" + thisOthEqInput.Name + "\", " +
                                            state.dataIPShortCut->cNumericFieldNames(DesignLevelFieldNumber) + " is not allowed to be negative");
                        ShowContinueError(state, "... when a fuel type of " + FuelTypeString + " is specified.");
                        ErrorsFound = true;
                    }

                    // Calculate nominal min/max equipment level
                    thisZoneOthEq.NomMinDesignLevel = thisZoneOthEq.DesignLevel * SchMin;
                    thisZoneOthEq.NomMaxDesignLevel = thisZoneOthEq.DesignLevel * SchMax;

                    thisZoneOthEq.FractionLatent = IHGNumbers(4);
                    thisZoneOthEq.FractionRadiant = IHGNumbers(5);
                    thisZoneOthEq.FractionLost = IHGNumbers(6);

                    if ((NumNumber == 7) || (!state.dataIPShortCut->lNumericFieldBlanks(7))) {
                        thisZoneOthEq.CO2RateFactor = IHGNumbers(7);
                    }
                    if (thisZoneOthEq.CO2RateFactor < 0.0) {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", {} < 0.0, value ={:.2R}",
                                               RoutineName,
                                               othEqModuleObject,
                                               thisOthEqInput.Name,
                                               state.dataIPShortCut->cNumericFieldNames(7),
                                               IHGNumbers(7)));
                        ErrorsFound = true;
                    }
                    if (thisZoneOthEq.CO2RateFactor > 4.0e-7) {
                        ShowSevereError(state,
                                        format("{}{}=\"{}\", {} > 4.0E-7, value ={:.2R}",
                                               RoutineName,
                                               othEqModuleObject,
                                               thisOthEqInput.Name,
                                               state.dataIPShortCut->cNumericFieldNames(7),
                                               IHGNumbers(7)));
                        ErrorsFound = true;
                    }

                    // FractionConvected is a calculated field
                    thisZoneOthEq.FractionConvected =
                        1.0 - (thisZoneOthEq.FractionLatent + thisZoneOthEq.FractionRadiant + thisZoneOthEq.FractionLost);
                    if (std::abs(thisZoneOthEq.FractionConvected) <= 0.001) thisZoneOthEq.FractionConvected = 0.0;
                    if (thisZoneOthEq.FractionConvected < 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + othEqModuleObject + "=\"" + thisOthEqInput.Name + "\", Sum of Fractions > 1.0");
                        ErrorsFound = true;
                    }

                    if (NumAlpha > 5) {
                        thisZoneOthEq.EndUseSubcategory = AlphaName(6);
                    } else {
                        thisZoneOthEq.EndUseSubcategory = "General";
                    }

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "OtherEquipment",
                                         thisZoneOthEq.Name,
                                         "Power Level",
                                         "[W]",
                                         thisZoneOthEq.EMSZoneEquipOverrideOn,
                                         thisZoneOthEq.EMSEquipPower);
                        SetupEMSInternalVariable(state, "Other Equipment Design Level", thisZoneOthEq.Name, "[W]", thisZoneOthEq.DesignLevel);
                    } // EMS

                    if (!ErrorsFound)
                        SetupSpaceInternalGain(state,
                                               thisZoneOthEq.spaceIndex,
                                               1.0,
                                               "OtherEquipment",
                                               thisZoneOthEq.Name,
                                               IntGainTypeOf_OtherEquipment,
                                               &thisZoneOthEq.ConGainRate,
                                               nullptr,
                                               &thisZoneOthEq.RadGainRate,
                                               &thisZoneOthEq.LatGainRate);

                } // for othEqInputNum.NumOfSpaces
            }     // for othEqInputNum
        }         // TotOtherEquip > 0

        // Note that this object type does not support ZoneList due to node names in input fields
        bool zoneListNotAllowed = true;
        setupIHGZonesAndSpaces(state,
                               itEqModuleObject,
                               state.dataHeatBal->ITEqObjects,
                               state.dataHeatBal->NumZoneITEqStatements,
                               state.dataHeatBal->TotITEquip,
                               ErrorsFound,
                               zoneListNotAllowed);

        if (state.dataHeatBal->TotITEquip > 0) {
            state.dataHeatBal->ZoneITEq.allocate(state.dataHeatBal->TotITEquip);
            int itEqNum = 0;
            for (int itEqInputNum = 1; itEqInputNum <= state.dataHeatBal->NumZoneITEqStatements; ++itEqInputNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         itEqModuleObject,
                                                                         itEqInputNum,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                auto &thisITEqInput = state.dataHeatBal->ITEqObjects(itEqInputNum);
                for (int Item1 = 1; Item1 <= thisITEqInput.numOfSpaces; ++Item1) {
                    ++itEqNum;
                    auto &thisZoneITEq = state.dataHeatBal->ZoneITEq(itEqNum);
                    int const spaceNum = thisITEqInput.spaceNums(Item1);
                    int const zoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
                    thisZoneITEq.Name = thisITEqInput.names(Item1);
                    thisZoneITEq.spaceIndex = spaceNum;
                    thisZoneITEq.ZonePtr = zoneNum;

                    // IT equipment design level calculation method.
                    if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                        thisZoneITEq.FlowControlWithApproachTemps = false;
                    } else {
                        if (UtilityRoutines::SameString(AlphaName(3), "FlowFromSystem")) {
                            thisZoneITEq.FlowControlWithApproachTemps = false;
                        } else if (UtilityRoutines::SameString(AlphaName(3), "FlowControlWithApproachTemperatures")) {
                            thisZoneITEq.FlowControlWithApproachTemps = true;
                            state.dataHeatBal->Zone(thisZoneITEq.ZonePtr).HasAdjustedReturnTempByITE = true;
                            state.dataHeatBal->Zone(thisZoneITEq.ZonePtr).NoHeatToReturnAir = false;
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) +
                                                "\": invalid calculation method: " + AlphaName(3));
                            ErrorsFound = true;
                        }
                    }

                    {
                        auto const equipmentLevel(AlphaName(4));
                        if (equipmentLevel == "WATTS/UNIT") {
                            Real64 spaceFrac = 1.0;
                            if (thisITEqInput.numOfSpaces > 1) {
                                Real64 const zoneArea = state.dataHeatBal->Zone(zoneNum).FloorArea;
                                if (zoneArea > 0.0) {
                                    spaceFrac = state.dataHeatBal->space(spaceNum).floorArea / zoneArea;
                                } else {
                                    ShowSevereError(state,
                                                    std::string(RoutineName) +
                                                        "Zone floor area is zero when allocating ElectricEquipment:ITE:AirCooled loads to Spaces.");
                                    ShowContinueError(state,
                                                      "Occurs for ElectricEquipment:ITE:AirCooled object =" + thisITEqInput.Name +
                                                          " in Zone=" + state.dataHeatBal->Zone(zoneNum).Name);
                                    ErrorsFound = true;
                                }
                            }
                            thisZoneITEq.DesignTotalPower = IHGNumbers(1) * IHGNumbers(2) * spaceFrac;
                            if (state.dataIPShortCut->lNumericFieldBlanks(1)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(1) +
                                                     ", but that field is blank.  0 IT Equipment will result.");
                            }
                            if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                     state.dataIPShortCut->cNumericFieldNames(2) +
                                                     ", but that field is blank.  0 IT Equipment will result.");
                            }

                        } else if (equipmentLevel == "WATTS/AREA") {
                            if (thisZoneITEq.ZonePtr != 0) {
                                if (IHGNumbers(3) >= 0.0) {
                                    if (spaceNum > 0) {
                                        thisZoneITEq.DesignTotalPower = IHGNumbers(3) * state.dataHeatBal->space(spaceNum).floorArea;
                                        if ((state.dataHeatBal->space(spaceNum).floorArea <= 0.0) &&
                                            !state.dataHeatBal->space(spaceNum).isRemainderSpace) {
                                            ShowWarningError(state,
                                                             std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                                 state.dataIPShortCut->cNumericFieldNames(3) +
                                                                 ", but Space Floor Area = 0.  0 IT Equipment will result.");
                                        }
                                    } else {
                                        ShowSevereError(state,
                                                        format("{}{}=\"{}\", invalid {}, value  [<0.0]={:.3R}",
                                                               RoutineName,
                                                               itEqModuleObject,
                                                               AlphaName(1),
                                                               state.dataIPShortCut->cNumericFieldNames(3),
                                                               IHGNumbers(3)));
                                        ErrorsFound = true;
                                    }
                                }
                                if (state.dataIPShortCut->lNumericFieldBlanks(3)) {
                                    ShowWarningError(state,
                                                     std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", specifies " +
                                                         state.dataIPShortCut->cNumericFieldNames(3) +
                                                         ", but that field is blank.  0 IT Equipment will result.");
                                }

                            } else {
                                ShowSevereError(state,
                                                std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                    state.dataIPShortCut->cAlphaFieldNames(4) + ", value  =" + AlphaName(4));
                                ShowContinueError(state, "...Valid values are \"Watts/Unit\" or \"Watts/Area\".");
                                ErrorsFound = true;
                            }
                        }

                        if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                            thisZoneITEq.OperSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
                        } else {
                            thisZoneITEq.OperSchedPtr = GetScheduleIndex(state, AlphaName(5));
                        }
                        SchMin = 0.0;
                        SchMax = 0.0;
                        if (thisZoneITEq.OperSchedPtr == 0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(5) + " entered=" + AlphaName(5));
                            ErrorsFound = true;
                        } else { // check min/max on schedule
                            SchMin = GetScheduleMinValue(state, thisZoneITEq.OperSchedPtr);
                            SchMax = GetScheduleMaxValue(state, thisZoneITEq.OperSchedPtr);
                            if (SchMin < 0.0 || SchMax < 0.0) {
                                if (SchMin < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(5) + ", minimum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(5), SchMin));
                                    ErrorsFound = true;
                                }
                                if (SchMax < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(5) + ", maximum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(5), SchMax));
                                    ErrorsFound = true;
                                }
                            }
                        }

                        if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                            thisZoneITEq.CPULoadSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
                        } else {
                            thisZoneITEq.CPULoadSchedPtr = GetScheduleIndex(state, AlphaName(6));
                        }
                        SchMin = 0.0;
                        SchMax = 0.0;
                        if (thisZoneITEq.CPULoadSchedPtr == 0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(6) + " entered=" + AlphaName(6));
                            ErrorsFound = true;
                        } else { // check min/max on schedule
                            SchMin = GetScheduleMinValue(state, thisZoneITEq.CPULoadSchedPtr);
                            SchMax = GetScheduleMaxValue(state, thisZoneITEq.CPULoadSchedPtr);
                            if (SchMin < 0.0 || SchMax < 0.0) {
                                if (SchMin < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(6) + ", minimum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(6), SchMin));
                                    ErrorsFound = true;
                                }
                                if (SchMax < 0.0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", " +
                                                        state.dataIPShortCut->cAlphaFieldNames(6) + ", maximum is < 0.0");
                                    ShowContinueError(state,
                                                      format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(6), SchMax));
                                    ErrorsFound = true;
                                }
                            }
                        }

                        // Calculate nominal min/max equipment level
                        thisZoneITEq.NomMinDesignLevel = thisZoneITEq.DesignTotalPower * SchMin;
                        thisZoneITEq.NomMaxDesignLevel = thisZoneITEq.DesignTotalPower * SchMax;

                        thisZoneITEq.DesignFanPowerFrac = IHGNumbers(4);
                        thisZoneITEq.DesignFanPower = thisZoneITEq.DesignFanPowerFrac * thisZoneITEq.DesignTotalPower;
                        thisZoneITEq.DesignCPUPower = (1.0 - thisZoneITEq.DesignFanPowerFrac) * thisZoneITEq.DesignTotalPower;
                        thisZoneITEq.DesignAirVolFlowRate = IHGNumbers(5) * thisZoneITEq.DesignTotalPower;
                        thisZoneITEq.DesignTAirIn = IHGNumbers(6);
                        thisZoneITEq.DesignRecircFrac = IHGNumbers(7);
                        thisZoneITEq.DesignUPSEfficiency = IHGNumbers(8);
                        thisZoneITEq.UPSLossToZoneFrac = IHGNumbers(9);
                        thisZoneITEq.SupplyApproachTemp = IHGNumbers(10);
                        thisZoneITEq.ReturnApproachTemp = IHGNumbers(11);

                        bool hasSupplyApproachTemp = !state.dataIPShortCut->lNumericFieldBlanks(10);
                        bool hasReturnApproachTemp = !state.dataIPShortCut->lNumericFieldBlanks(11);

                        // Performance curves
                        thisZoneITEq.CPUPowerFLTCurve = GetCurveIndex(state, AlphaName(7));
                        if (thisZoneITEq.CPUPowerFLTCurve == 0) {
                            ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + " \"" + AlphaName(1) + "\"");
                            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + AlphaName(7));
                            ErrorsFound = true;
                        }

                        thisZoneITEq.AirFlowFLTCurve = GetCurveIndex(state, AlphaName(8));
                        if (thisZoneITEq.AirFlowFLTCurve == 0) {
                            ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + " \"" + AlphaName(1) + "\"");
                            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + AlphaName(8));
                            ErrorsFound = true;
                        }

                        thisZoneITEq.FanPowerFFCurve = GetCurveIndex(state, AlphaName(9));
                        if (thisZoneITEq.FanPowerFFCurve == 0) {
                            ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + " \"" + AlphaName(1) + "\"");
                            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(9) + '=' + AlphaName(9));
                            ErrorsFound = true;
                        }

                        if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                            // If this field isn't blank, it must point to a valid curve
                            thisZoneITEq.RecircFLTCurve = GetCurveIndex(state, AlphaName(15));
                            if (thisZoneITEq.RecircFLTCurve == 0) {
                                ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + " \"" + AlphaName(1) + "\"");
                                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(15) + '=' + AlphaName(15));
                                ErrorsFound = true;
                            }
                        } else {
                            // If this curve is left blank, then the curve is assumed to always equal 1.0.
                            thisZoneITEq.RecircFLTCurve = 0;
                        }

                        if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                            // If this field isn't blank, it must point to a valid curve
                            thisZoneITEq.UPSEfficFPLRCurve = GetCurveIndex(state, AlphaName(16));
                            if (thisZoneITEq.UPSEfficFPLRCurve == 0) {
                                ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + " \"" + AlphaName(1) + "\"");
                                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(16) + '=' + AlphaName(16));
                                ErrorsFound = true;
                            }
                        } else {
                            // If this curve is left blank, then the curve is assumed to always equal 1.0.
                            thisZoneITEq.UPSEfficFPLRCurve = 0;
                        }

                        // Environmental class
                        if (UtilityRoutines::SameString(AlphaName(10), "None")) {
                            thisZoneITEq.Class = ITEClassNone;
                        } else if (UtilityRoutines::SameString(AlphaName(10), "A1")) {
                            thisZoneITEq.Class = ITEClassA1;
                        } else if (UtilityRoutines::SameString(AlphaName(10), "A2")) {
                            thisZoneITEq.Class = ITEClassA2;
                        } else if (UtilityRoutines::SameString(AlphaName(10), "A3")) {
                            thisZoneITEq.Class = ITEClassA3;
                        } else if (UtilityRoutines::SameString(AlphaName(10), "A4")) {
                            thisZoneITEq.Class = ITEClassA4;
                        } else if (UtilityRoutines::SameString(AlphaName(10), "B")) {
                            thisZoneITEq.Class = ITEClassB;
                        } else if (UtilityRoutines::SameString(AlphaName(10), "C")) {
                            thisZoneITEq.Class = ITEClassC;
                        } else {
                            ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + ": " + AlphaName(1));
                            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(10) + '=' + AlphaName(10));
                            ShowContinueError(state, "Valid entries are None, A1, A2, A3, A4, B or C.");
                            ErrorsFound = true;
                        }

                        // Air and supply inlet connections
                        if (UtilityRoutines::SameString(AlphaName(11), "AdjustedSupply")) {
                            thisZoneITEq.AirConnectionType = ITEInletAdjustedSupply;
                        } else if (UtilityRoutines::SameString(AlphaName(11), "ZoneAirNode")) {
                            thisZoneITEq.AirConnectionType = ITEInletZoneAirNode;
                        } else if (UtilityRoutines::SameString(AlphaName(11), "RoomAirModel")) {
                            // ZoneITEq( Loop ).AirConnectionType = ITEInletRoomAirModel;
                            ShowWarningError(state,
                                             std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) +
                                                 "Air Inlet Connection Type = RoomAirModel is not implemented yet, using ZoneAirNode");
                            thisZoneITEq.AirConnectionType = ITEInletZoneAirNode;
                        } else {
                            ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + ": " + AlphaName(1));
                            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(11) + '=' + AlphaName(11));
                            ShowContinueError(state, "Valid entries are AdjustedSupply, ZoneAirNode, or RoomAirModel.");
                            ErrorsFound = true;
                        }
                        if (state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                            if (thisZoneITEq.AirConnectionType == ITEInletAdjustedSupply) {
                                ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + ": " + AlphaName(1));
                                ShowContinueError(state,
                                                  "For " + state.dataIPShortCut->cAlphaFieldNames(11) + "= AdjustedSupply, " +
                                                      state.dataIPShortCut->cAlphaFieldNames(14) + " is required, but this field is blank.");
                                ErrorsFound = true;
                            } else if (thisZoneITEq.FlowControlWithApproachTemps) {
                                ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + ": " + AlphaName(1));
                                ShowContinueError(state,
                                                  "For " + state.dataIPShortCut->cAlphaFieldNames(3) + "= FlowControlWithApproachTemperatures, " +
                                                      state.dataIPShortCut->cAlphaFieldNames(14) + " is required, but this field is blank.");
                                ErrorsFound = true;
                            }
                        } else {
                            thisZoneITEq.SupplyAirNodeNum = GetOnlySingleNode(state,
                                                                              AlphaName(14),
                                                                              ErrorsFound,
                                                                              itEqModuleObject,
                                                                              AlphaName(1),
                                                                              DataLoopNode::NodeFluidType::Air,
                                                                              DataLoopNode::NodeConnectionType::Sensor,
                                                                              NodeInputManager::compFluidStream::Primary,
                                                                              ObjectIsNotParent);
                        }

                        // check supply air node for matches with zone equipment supply air node
                        int zoneEqIndex = DataZoneEquipment::GetControlledZoneIndex(state, state.dataHeatBal->Zone(thisZoneITEq.ZonePtr).Name);
                        thisZoneITEq.zoneEqIndex = zoneEqIndex;
                        if (zoneEqIndex > 0) { // zoneEqIndex could be zero in the case of an uncontrolled zone
                            auto itStart = state.dataZoneEquip->ZoneEquipConfig(zoneEqIndex).InletNode.begin();
                            auto itEnd = state.dataZoneEquip->ZoneEquipConfig(zoneEqIndex).InletNode.end();
                            auto key = thisZoneITEq.SupplyAirNodeNum;
                            bool supplyNodeFound = false;
                            if (std::find(itStart, itEnd, key) != itEnd) {
                                supplyNodeFound = true;
                            }

                            if (thisZoneITEq.AirConnectionType == ITEInletAdjustedSupply && !supplyNodeFound) {
                                // supply air node must match zone equipment supply air node for these conditions
                                ShowSevereError(state, std::string{RoutineName} + ": ElectricEquipment:ITE:AirCooled " + thisZoneITEq.Name);
                                ShowContinueError(state, "Air Inlet Connection Type = AdjustedSupply but no Supply Air Node is specified.");
                                ErrorsFound = true;
                            } else if (thisZoneITEq.FlowControlWithApproachTemps && !supplyNodeFound) {
                                // supply air node must match zone equipment supply air node for these conditions
                                ShowSevereError(state, std::string{RoutineName} + ": ElectricEquipment:ITE:AirCooled " + thisZoneITEq.Name);
                                ShowContinueError(state, "Air Inlet Connection Type = AdjustedSupply but no Supply Air Node is specified.");
                                ErrorsFound = true;
                            } else if (thisZoneITEq.SupplyAirNodeNum != 0 && !supplyNodeFound) {
                                // the given supply air node does not match any zone equipment supply air nodes
                                ShowWarningError(state,
                                                 itEqModuleObject + "name: '" + AlphaName(1) + ". " + "Supply Air Node Name '" + AlphaName(14) +
                                                     "' does not match any ZoneHVAC:EquipmentConnections objects.");
                            }
                        } // end of if block for zoneEqIndex > 0

                        // End-Use subcategories
                        if (NumAlpha > 16) {
                            thisZoneITEq.EndUseSubcategoryCPU = AlphaName(17);
                        } else {
                            thisZoneITEq.EndUseSubcategoryCPU = "ITE-CPU";
                        }

                        if (NumAlpha > 17) {
                            thisZoneITEq.EndUseSubcategoryFan = AlphaName(18);
                        } else {
                            thisZoneITEq.EndUseSubcategoryFan = "ITE-Fans";
                        }
                        if (thisZoneITEq.ZonePtr <= 0) continue; // Error, will be caught and terminated later

                        if (NumAlpha > 18) {
                            thisZoneITEq.EndUseSubcategoryUPS = AlphaName(19);
                        } else {
                            thisZoneITEq.EndUseSubcategoryUPS = "ITE-UPS";
                        }
                        if (thisZoneITEq.FlowControlWithApproachTemps) {
                            if (!state.dataIPShortCut->lAlphaFieldBlanks(20)) {
                                thisZoneITEq.SupplyApproachTempSch = GetScheduleIndex(state, AlphaName(20));
                                if (thisZoneITEq.SupplyApproachTempSch == 0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                        state.dataIPShortCut->cAlphaFieldNames(20) + " entered=" + AlphaName(20));
                                    ErrorsFound = true;
                                }
                            } else {
                                if (!hasSupplyApproachTemp) {
                                    ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + " \"" + AlphaName(1) + "\"");
                                    ShowContinueError(state,
                                                      "For " + state.dataIPShortCut->cAlphaFieldNames(3) +
                                                          "= FlowControlWithApproachTemperatures, either " +
                                                          state.dataIPShortCut->cNumericFieldNames(10) + " or " +
                                                          state.dataIPShortCut->cAlphaFieldNames(20) + " is required, but both are left blank.");
                                    ErrorsFound = true;
                                }
                            }

                            if (!state.dataIPShortCut->lAlphaFieldBlanks(21)) {
                                thisZoneITEq.ReturnApproachTempSch = GetScheduleIndex(state, AlphaName(21));
                                if (thisZoneITEq.ReturnApproachTempSch == 0) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                                        state.dataIPShortCut->cAlphaFieldNames(20) + " entered=" + AlphaName(20));
                                    ErrorsFound = true;
                                }
                            } else {
                                if (!hasReturnApproachTemp) {
                                    ShowSevereError(state, std::string{RoutineName} + itEqModuleObject + " \"" + AlphaName(1) + "\"");
                                    ShowContinueError(state,
                                                      "For " + state.dataIPShortCut->cAlphaFieldNames(3) +
                                                          "= FlowControlWithApproachTemperatures, either " +
                                                          state.dataIPShortCut->cNumericFieldNames(11) + " or " +
                                                          state.dataIPShortCut->cAlphaFieldNames(21) + " is required, but both are left blank.");
                                    ErrorsFound = true;
                                }
                            }
                        }

                        if (thisZoneITEq.FlowControlWithApproachTemps) {
                            Real64 TAirInSizing = 0.0;
                            // Set the TAirInSizing to the maximun setpoint value to do sizing based on the maximum fan and cpu power of the ite
                            // object
                            SetPointManager::GetSetPointManagerInputData(state, ErrorsFound);
                            for (int SetPtMgrNum = 1; SetPtMgrNum <= state.dataSetPointManager->NumSZClSetPtMgrs; ++SetPtMgrNum) {
                                if (state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).ControlZoneNum == zoneNum) {
                                    TAirInSizing = state.dataSetPointManager->SingZoneClSetPtMgr(SetPtMgrNum).MaxSetTemp;
                                }
                            }

                            thisZoneITEq.SizingTAirIn = max(TAirInSizing, thisZoneITEq.DesignTAirIn);
                        }

                        // MJW - EMS Not in place yet
                        // if ( AnyEnergyManagementSystemInModel ) {
                        // SetupEMSActuator( "ElectricEquipment", ZoneITEq( Loop ).Name, "Electric Power Level", "[W]", ZoneITEq( Loop
                        // ).EMSZoneEquipOverrideOn, ZoneITEq( Loop ).EMSEquipPower ); SetupEMSInternalVariable( "Plug and Process Power Design
                        // Level", ZoneITEq( Loop ).Name, "[W]", ZoneITEq( Loop ).DesignTotalPower ); } // EMS

                        if (!ErrorsFound)
                            SetupSpaceInternalGain(state,
                                                   thisZoneITEq.spaceIndex,
                                                   1.0,
                                                   "ElectricEquipment:ITE:AirCooled",
                                                   thisZoneITEq.Name,
                                                   IntGainTypeOf_ElectricEquipmentITEAirCooled,
                                                   &thisZoneITEq.ConGainRateToZone);
                    }
                } // for itEqInputNum.NumOfSpaces
            }     // for itEqInputNum
            for (int Loop = 1; Loop <= state.dataHeatBal->TotITEquip; ++Loop) {
                if (state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(Loop).ZonePtr).HasAdjustedReturnTempByITE &&
                    (!state.dataHeatBal->ZoneITEq(Loop).FlowControlWithApproachTemps)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + itEqModuleObject + "=\"" + AlphaName(1) + "\": invalid calculation method " +
                                        AlphaName(3) + " for Zone: " + AlphaName(2));
                    ShowContinueError(state, "...Multiple flow control methods apply to one zone. ");
                    ErrorsFound = true;
                }
            }
        } // TotITEquip > 0

        setupIHGZonesAndSpaces(state,
                               bbModuleObject,
                               state.dataHeatBal->ZoneBBHeatObjects,
                               state.dataHeatBal->NumZoneBBHeatStatements,
                               state.dataHeatBal->TotBBHeat,
                               ErrorsFound);

        if (state.dataHeatBal->TotBBHeat > 0) {
            state.dataHeatBal->ZoneBBHeat.allocate(state.dataHeatBal->TotBBHeat);
            int bbHeatNum = 0;
            for (int bbHeatInputNum = 1; bbHeatInputNum <= state.dataHeatBal->NumZoneBBHeatStatements; ++bbHeatInputNum) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         bbModuleObject,
                                                                         bbHeatInputNum,
                                                                         AlphaName,
                                                                         NumAlpha,
                                                                         IHGNumbers,
                                                                         NumNumber,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                auto &thisBBHeatInput = state.dataHeatBal->ZoneBBHeatObjects(bbHeatInputNum);
                for (int Item1 = 1; Item1 <= thisBBHeatInput.numOfSpaces; ++Item1) {
                    ++bbHeatNum;
                    auto &thisZoneBBHeat = state.dataHeatBal->ZoneBBHeat(bbHeatNum);
                    int const spaceNum = thisBBHeatInput.spaceNums(Item1);
                    int const zoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
                    thisZoneBBHeat.Name = thisBBHeatInput.names(Item1);
                    thisZoneBBHeat.spaceIndex = spaceNum;
                    thisZoneBBHeat.ZonePtr = zoneNum;

                    thisZoneBBHeat.SchedPtr = GetScheduleIndex(state, AlphaName(3));
                    if (thisZoneBBHeat.SchedPtr == 0) {
                        if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + bbModuleObject + "=\"" + thisBBHeatInput.Name + "\", " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + bbModuleObject + "=\"" + thisBBHeatInput.Name + "\", invalid " +
                                                state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                        }
                        ErrorsFound = true;
                    } else { // check min/max on schedule
                        SchMin = GetScheduleMinValue(state, thisZoneBBHeat.SchedPtr);
                        SchMax = GetScheduleMaxValue(state, thisZoneBBHeat.SchedPtr);
                        if (SchMin < 0.0 || SchMax < 0.0) {
                            if (SchMin < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + bbModuleObject + "=\"" + thisBBHeatInput.Name + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                                ErrorsFound = true;
                            }
                            if (SchMax < 0.0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + bbModuleObject + "=\"" + thisBBHeatInput.Name + "\", " +
                                                    state.dataIPShortCut->cAlphaFieldNames(3) + ", maximum is < 0.0");
                                ShowContinueError(state,
                                                  format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMax));
                                ErrorsFound = true;
                            }
                        }
                    }

                    if (NumAlpha > 3) {
                        thisZoneBBHeat.EndUseSubcategory = AlphaName(4);
                    } else {
                        thisZoneBBHeat.EndUseSubcategory = "General";
                    }

                    thisZoneBBHeat.CapatLowTemperature = IHGNumbers(1);
                    thisZoneBBHeat.LowTemperature = IHGNumbers(2);
                    thisZoneBBHeat.CapatHighTemperature = IHGNumbers(3);
                    thisZoneBBHeat.HighTemperature = IHGNumbers(4);
                    thisZoneBBHeat.FractionRadiant = IHGNumbers(5);
                    thisZoneBBHeat.FractionConvected = 1.0 - thisZoneBBHeat.FractionRadiant;
                    if (thisZoneBBHeat.FractionConvected < 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + bbModuleObject + "=\"" + thisBBHeatInput.Name + "\", Sum of Fractions > 1.0");
                        ErrorsFound = true;
                    }

                    if (thisZoneBBHeat.ZonePtr <= 0) continue; // Error, will be caught and terminated later

                    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        SetupEMSActuator(state,
                                         "ZoneBaseboard:OutdoorTemperatureControlled",
                                         thisZoneBBHeat.Name,
                                         "Power Level",
                                         "[W]",
                                         thisZoneBBHeat.EMSZoneBaseboardOverrideOn,
                                         thisZoneBBHeat.EMSZoneBaseboardPower);
                        SetupEMSInternalVariable(state,
                                                 "Simple Zone Baseboard Capacity At Low Temperature",
                                                 thisZoneBBHeat.Name,
                                                 "[W]",
                                                 thisZoneBBHeat.CapatLowTemperature);
                        SetupEMSInternalVariable(state,
                                                 "Simple Zone Baseboard Capacity At High Temperature",
                                                 thisZoneBBHeat.Name,
                                                 "[W]",
                                                 thisZoneBBHeat.CapatHighTemperature);
                    } // EMS

                    SetupSpaceInternalGain(state,
                                           thisZoneBBHeat.spaceIndex,
                                           1.0,
                                           "ZoneBaseboard:OutdoorTemperatureControlled",
                                           thisZoneBBHeat.Name,
                                           IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled,
                                           &thisZoneBBHeat.ConGainRate,
                                           nullptr,
                                           &thisZoneBBHeat.RadGainRate);
                } // for bbHeatInputNum.NumOfSpaces
            }     // for bbHeatInputNum
        }         // TotBBHeat > 0

        state.dataHeatBal->TotCO2Gen = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, contamSSModuleObject);
        state.dataHeatBal->ZoneCO2Gen.allocate(state.dataHeatBal->TotCO2Gen);

        for (int Loop = 1; Loop <= state.dataHeatBal->TotCO2Gen; ++Loop) {
            AlphaName = "";
            IHGNumbers = 0.0;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     contamSSModuleObject,
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
            UtilityRoutines::IsNameEmpty(state, AlphaName(1), contamSSModuleObject, ErrorsFound);

            state.dataHeatBal->ZoneCO2Gen(Loop).Name = AlphaName(1);

            state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr = UtilityRoutines::FindItemInList(AlphaName(2), state.dataHeatBal->Zone);
            if (state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + contamSSModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(2) + " entered=" + AlphaName(2));
                ErrorsFound = true;
            }

            state.dataHeatBal->ZoneCO2Gen(Loop).SchedPtr = GetScheduleIndex(state, AlphaName(3));
            if (state.dataHeatBal->ZoneCO2Gen(Loop).SchedPtr == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + contamSSModuleObject + "=\"" + AlphaName(1) + "\", " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + " is required.");
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + contamSSModuleObject + "=\"" + AlphaName(1) + "\", invalid " +
                                        state.dataIPShortCut->cAlphaFieldNames(3) + " entered=" + AlphaName(3));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = GetScheduleMinValue(state, state.dataHeatBal->ZoneCO2Gen(Loop).SchedPtr);
                SchMax = GetScheduleMaxValue(state, state.dataHeatBal->ZoneCO2Gen(Loop).SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + contamSSModuleObject + "=\"" + AlphaName(1) + "\", " +
                                            state.dataIPShortCut->cAlphaFieldNames(3) + ", minimum is < 0.0");
                        ShowContinueError(state, format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", AlphaName(3), SchMin));
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + contamSSModuleObject + "=\"" + AlphaName(1) + "\", " +
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
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneCO2Gen(Loop).Name);

            // Zone total report variables
            if (RepVarSet(state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr)) {
                RepVarSet(state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr) = false;

                SetupOutputVariable(state,
                                    "Zone Contaminant Source or Sink CO2 Gain Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnRpt(state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr).CO2Rate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
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

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found in Getting Internal Gains Input, Program Stopped");
        }

        setupIHGOutputs(state);

        static constexpr std::string_view Format_721(
            "! <Zone Internal Gains Nominal>,Zone Name, Floor Area {{m2}},# Occupants,Area per Occupant "
            "{{m2/person}},Occupant per Area {{person/m2}},Interior Lighting {{W/m2}},Electric Load {{W/m2}},Gas Load {{W/m2}},Other "
            "Load {{W/m2}},Hot Water Eq {{W/m2}},Steam Equipment {{W/m2}},Sum Loads per Area {{W/m2}},Outdoor Controlled Baseboard "
            "Heat\n");

        print(state.files.eio, Format_721);
        for (int Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
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
            for (Loop1 = 1; Loop1 <= state.dataHeatBal->TotITEquip; ++Loop1) {
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
        for (int Loop = 1; Loop <= state.dataHeatBal->TotPeople; ++Loop) {
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

            int ZoneNum = state.dataHeatBal->People(Loop).ZonePtr;

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

                if (state.dataHeatBal->People(Loop).MRTCalcType == DataHeatBalance::CalcMRT::ZoneAveraged) {
                    print(state.files.eio, "Zone Averaged,");
                } else if (state.dataHeatBal->People(Loop).MRTCalcType == DataHeatBalance::CalcMRT::SurfaceWeighted) {
                    print(state.files.eio, "Surface Weighted,");
                } else if (state.dataHeatBal->People(Loop).MRTCalcType == DataHeatBalance::CalcMRT::AngleFactor) {
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
        for (int Loop = 1; Loop <= state.dataHeatBal->TotLights; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "Lights",
                      "Lighting Level {W},Lights/Floor Area {W/m2},Lights per person {W/person},Fraction Return "
                      "Air,Fraction Radiant,Fraction Short Wave,Fraction Convected,Fraction Replaceable,End-Use "
                      "Category,Nominal Minimum Lighting Level {W},Nominal Maximum Lighting Level {W}\n");
            }

            int ZoneNum = state.dataHeatBal->Lights(Loop).ZonePtr;

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
        for (int Loop = 1; Loop <= state.dataHeatBal->TotElecEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "ElectricEquipment",
                      "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction "
                      "Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}\n");
            }

            int ZoneNum = state.dataHeatBal->ZoneElectric(Loop).ZonePtr;

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
        for (int Loop = 1; Loop <= state.dataHeatBal->TotGasEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "GasEquipment",
                      "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction "
                      "Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}\n");
            }

            int ZoneNum = state.dataHeatBal->ZoneGas(Loop).ZonePtr;

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

        for (int Loop = 1; Loop <= state.dataHeatBal->TotHWEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "HotWaterEquipment",
                      "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction "
                      "Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}\n");
            }

            int ZoneNum = state.dataHeatBal->ZoneHWEq(Loop).ZonePtr;

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

        for (int Loop = 1; Loop <= state.dataHeatBal->TotStmEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "SteamEquipment",
                      "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction "
                      "Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}\n");
            }

            int ZoneNum = state.dataHeatBal->ZoneSteamEq(Loop).ZonePtr;

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

        for (int Loop = 1; Loop <= state.dataHeatBal->TotOthEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "OtherEquipment",
                      "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction "
                      "Lost,Fraction Convected,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}\n");
            }

            int ZoneNum = state.dataHeatBal->ZoneOtherEq(Loop).ZonePtr;

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

        for (int Loop = 1; Loop <= state.dataHeatBal->TotITEquip; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "ElectricEquipment:ITE:AirCooled",
                      "Equipment Level {W},"
                      "Equipment/Floor Area {W/m2},Equipment per person {W/person},"
                      "Fraction Convected,CPU End-Use SubCategory,Fan End-Use SubCategory,UPS End-Use SubCategory,"
                      "Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}, Design Air Volume Flow Rate {m3/s}\n");
            }

            int ZoneNum = state.dataHeatBal->ZoneITEq(Loop).ZonePtr;

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

        for (int Loop = 1; Loop <= state.dataHeatBal->TotBBHeat; ++Loop) {
            if (Loop == 1) {
                print(state.files.eio,
                      Format_723,
                      "Outdoor Controlled Baseboard Heat",
                      "Capacity at Low Temperature {W},Low Temperature {C},Capacity at High Temperature "
                      "{W},High Temperature {C},Fraction Radiant,Fraction Convected,End-Use Subcategory\n");
            }

            int ZoneNum = state.dataHeatBal->ZoneBBHeat(Loop).ZonePtr;

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

    void setupIHGZonesAndSpaces(EnergyPlusData &state,
                                const std::string objectType,
                                EPVector<DataHeatBalance::GlobalInternalGainMiscObject> &inputObjects,
                                int &numInputObjects,
                                int &numGainInstances,
                                bool &errors,
                                const bool zoneListNotAllowed)
    {
        constexpr std::string_view routineName = "setupIHGZonesAndSpaces: ";
        bool localErrFlag = false;

        auto &ip = state.dataInputProcessing->inputProcessor;
        auto const instances = ip->epJSON.find(objectType);
        if (instances != ip->epJSON.end()) {
            auto const &objectSchemaProps = ip->getObjectSchemaProps(state, objectType);
            auto &instancesValue = instances.value();
            numInputObjects = int(instancesValue.size());
            inputObjects.allocate(numInputObjects);

            numGainInstances = 0;
            int counter = 0;
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &objectFields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                ip->markObjectAsUsed(objectType, instance.key());

                // For incoming idf, maintain object order
                ++counter;
                int objNum = ip->getIDFObjNum(state, objectType, counter);
                inputObjects(objNum).Name = thisObjectName;
                std::string areaFieldName;
                if (zoneListNotAllowed) {
                    areaFieldName = "zone_or_space_name";
                } else {
                    areaFieldName = "zone_or_zonelist_or_space_or_spacelist_name";
                }
                std::string areaName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, areaFieldName);

                int zoneNum = UtilityRoutines::FindItemInList(areaName, state.dataHeatBal->Zone);
                if (zoneNum > 0) {
                    inputObjects(objNum).StartPtr = numGainInstances + 1;
                    int numSpaces = state.dataHeatBal->Zone(zoneNum).numSpaces;
                    numGainInstances += numSpaces;
                    inputObjects(objNum).numOfSpaces = numSpaces;
                    inputObjects(objNum).NumOfZones = 1;
                    inputObjects(objNum).ZoneListActive = false;
                    inputObjects(objNum).ZoneOrZoneListPtr = zoneNum;
                    if (numSpaces == 1) {
                        inputObjects(objNum).spaceNums.emplace_back(state.dataHeatBal->Zone(zoneNum).spaceIndexes(1));
                        inputObjects(objNum).names.emplace_back(inputObjects(objNum).Name);
                    } else {
                        for (int const spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                            inputObjects(objNum).spaceNums.emplace_back(spaceNum);
                            inputObjects(objNum).names.emplace_back(state.dataHeatBal->space(spaceNum).Name + ' ' + inputObjects(objNum).Name);
                        }
                    }
                    continue;
                }
                int spaceNum = UtilityRoutines::FindItemInList(areaName, state.dataHeatBal->space);
                if (spaceNum > 0) {
                    inputObjects(objNum).StartPtr = numGainInstances + 1;
                    ++numGainInstances;
                    inputObjects(objNum).numOfSpaces = 1;
                    inputObjects(objNum).spaceListActive = false;
                    inputObjects(objNum).spaceOrSpaceListPtr = spaceNum;
                    inputObjects(objNum).spaceNums.emplace_back(spaceNum);
                    inputObjects(objNum).names.emplace_back(inputObjects(objNum).Name);
                    continue;
                }
                int zoneListNum = UtilityRoutines::FindItemInList(areaName, state.dataHeatBal->ZoneList);
                if (zoneListNum > 0) {
                    if (zoneListNotAllowed) {
                        ShowSevereError(
                            state, objectType + "=\"" + thisObjectName + "\" ZoneList Name=\"" + areaName + "\" not allowed for " + objectType + ".");
                        errors = true;
                        localErrFlag = true;
                    } else {

                        inputObjects(objNum).StartPtr = numGainInstances + 1;
                        int numSpaces = 0;
                        for (int const listZoneIdx : state.dataHeatBal->ZoneList(zoneListNum).Zone) {
                            numSpaces += state.dataHeatBal->Zone(listZoneIdx).numSpaces;
                            for (int const spaceNum : state.dataHeatBal->Zone(listZoneIdx).spaceIndexes) {
                                inputObjects(objNum).spaceNums.emplace_back(spaceNum);
                                inputObjects(objNum).names.emplace_back(state.dataHeatBal->space(spaceNum).Name + ' ' + inputObjects(objNum).Name);
                            }
                        }
                        numGainInstances += numSpaces;
                        inputObjects(objNum).numOfSpaces = numSpaces;
                        inputObjects(objNum).NumOfZones = state.dataHeatBal->ZoneList(zoneListNum).NumOfZones;
                        inputObjects(objNum).ZoneListActive = true;
                        inputObjects(objNum).ZoneOrZoneListPtr = zoneListNum;
                    }
                    continue;
                }
                int spaceListNum = UtilityRoutines::FindItemInList(areaName, state.dataHeatBal->spaceList);
                if (spaceListNum > 0) {
                    if (zoneListNotAllowed) {
                        ShowSevereError(state,
                                        objectType + "=\"" + thisObjectName + "\" SpaceList Name=\"" + areaName + "\" not allowed for " + objectType +
                                            ".");
                        errors = true;
                        localErrFlag = true;
                    } else {
                        inputObjects(objNum).StartPtr = numGainInstances + 1;
                        int numSpaces = state.dataHeatBal->spaceList(spaceListNum).numListSpaces;
                        numGainInstances += numSpaces;
                        inputObjects(objNum).numOfSpaces = numSpaces;
                        inputObjects(objNum).spaceListActive = true;
                        inputObjects(objNum).spaceOrSpaceListPtr = spaceListNum;
                        for (int const spaceNum : state.dataHeatBal->spaceList(spaceListNum).spaces) {
                            inputObjects(objNum).spaceNums.emplace_back(spaceNum);
                            inputObjects(objNum).names.emplace_back(state.dataHeatBal->space(spaceNum).Name + ' ' + inputObjects(objNum).Name);
                        }
                    }
                    continue;
                }
                ShowSevereError(state, objectType + "=\"" + thisObjectName + "\" invalid " + areaFieldName + "=\"" + areaName + "\" not found.");
                errors = true;
                localErrFlag = true;
            }
            if (localErrFlag) {
                ShowSevereError(state, std::string{routineName} + "Errors with invalid names in " + objectType + " objects.");
                ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
                numGainInstances = 0;
            }
        }
    }

    void setupIHGOutputs(EnergyPlusData &state)
    {
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            // Overall Zone Variables
            SetupOutputVariable(state,
                                "Zone Total Internal Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnRpt(zoneNum).TotRadiantGain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Zone(zoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnRpt(zoneNum).TotRadiantGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(zoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Visible Radiation Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnRpt(zoneNum).TotVisHeatGain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Zone(zoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Visible Radiation Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnRpt(zoneNum).TotVisHeatGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(zoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnRpt(zoneNum).TotConvectiveGain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Zone(zoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnRpt(zoneNum).TotConvectiveGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(zoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnRpt(zoneNum).TotLatentGain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Zone(zoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnRpt(zoneNum).TotLatentGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(zoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZnRpt(zoneNum).TotTotalHeatGain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Zone(zoneNum).Name);
            SetupOutputVariable(state,
                                "Zone Total Internal Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZnRpt(zoneNum).TotTotalHeatGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Zone(zoneNum).Name);
        }

        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            // Overall Space Variables
            SetupOutputVariable(state,
                                "Space Total Internal Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->spaceRpt(spaceNum).TotRadiantGain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->space(spaceNum).Name);
            SetupOutputVariable(state,
                                "Space Total Internal Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->spaceRpt(spaceNum).TotRadiantGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->space(spaceNum).Name);
            SetupOutputVariable(state,
                                "Space Total Internal Visible Radiation Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->spaceRpt(spaceNum).TotVisHeatGain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->space(spaceNum).Name);
            SetupOutputVariable(state,
                                "Space Total Internal Visible Radiation Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->spaceRpt(spaceNum).TotVisHeatGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->space(spaceNum).Name);
            SetupOutputVariable(state,
                                "Space Total Internal Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->spaceRpt(spaceNum).TotConvectiveGain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->space(spaceNum).Name);
            SetupOutputVariable(state,
                                "Space Total Internal Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->spaceRpt(spaceNum).TotConvectiveGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->space(spaceNum).Name);
            SetupOutputVariable(state,
                                "Space Total Internal Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->spaceRpt(spaceNum).TotLatentGain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->space(spaceNum).Name);
            SetupOutputVariable(state,
                                "Space Total Internal Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->spaceRpt(spaceNum).TotLatentGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->space(spaceNum).Name);
            SetupOutputVariable(state,
                                "Space Total Internal Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->spaceRpt(spaceNum).TotTotalHeatGain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->space(spaceNum).Name);
            SetupOutputVariable(state,
                                "Space Total Internal Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->spaceRpt(spaceNum).TotTotalHeatGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->space(spaceNum).Name);
        }

        // Add zone and space outputs only where the particular type of equipment is actually present
        Array1D_bool addZoneOutputs;
        addZoneOutputs.dimension(state.dataGlobal->NumOfZones, false);
        Array1D_bool addSpaceOutputs;
        addSpaceOutputs.dimension(state.dataGlobal->numSpaces, false);

        for (int peopleNum = 1; peopleNum <= state.dataHeatBal->TotPeople; ++peopleNum) {
            // Set flags for zone and space total report variables
            addZoneOutputs(state.dataHeatBal->People(peopleNum).ZonePtr) = true;
            addSpaceOutputs(state.dataHeatBal->People(peopleNum).spaceIndex) = true;
            // Object report variables
            SetupOutputVariable(state,
                                "People Occupant Count",
                                OutputProcessor::Unit::None,
                                state.dataHeatBal->People(peopleNum).NumOcc,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->People(peopleNum).RadGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->People(peopleNum).RadGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->People(peopleNum).ConGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->People(peopleNum).ConGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Sensible Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->People(peopleNum).SenGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Sensible Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->People(peopleNum).SenGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->People(peopleNum).LatGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->People(peopleNum).LatGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->People(peopleNum).TotGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->People(peopleNum).TotGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Air Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBal->People(peopleNum).TemperatureInZone,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->People(peopleNum).Name);
            SetupOutputVariable(state,
                                "People Air Relative Humidity",
                                OutputProcessor::Unit::Perc,
                                state.dataHeatBal->People(peopleNum).RelativeHumidityInZone,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->People(peopleNum).Name);
        }

        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            if (addZoneOutputs(zoneNum)) {
                // Zone total report variables
                SetupOutputVariable(state,
                                    "Zone People Occupant Count",
                                    OutputProcessor::Unit::None,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleNumOcc,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone People Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone People Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone People Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone People Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone People Sensible Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleSenGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone People Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleSenGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone People Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone People Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone People Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone People Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).PeopleTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
            }
            // Reset zone output flag
            addZoneOutputs(zoneNum) = false;
        }

        // Space total report variables
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (addSpaceOutputs(spaceNum)) {
                SetupOutputVariable(state,
                                    "Space People Occupant Count",
                                    OutputProcessor::Unit::None,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleNumOcc,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space People Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space People Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space People Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space People Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space People Sensible Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleSenGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space People Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleSenGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space People Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space People Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space People Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space People Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).PeopleTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
            }
            // Reset space output flag
            addSpaceOutputs(spaceNum) = false;
        }

        for (int lightsNum = 1; lightsNum <= state.dataHeatBal->TotLights; ++lightsNum) {
            // Set flags for zone and space total report variables
            addZoneOutputs(state.dataHeatBal->Lights(lightsNum).ZonePtr) = true;
            addSpaceOutputs(state.dataHeatBal->Lights(lightsNum).spaceIndex) = true;
            // Object report variables
            SetupOutputVariable(state,
                                "Lights Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->Lights(lightsNum).Power,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Lights(lightsNum).Name);

            SetupOutputVariable(state,
                                "Lights Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->Lights(lightsNum).RadGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Lights(lightsNum).Name);
            SetupOutputVariable(state,
                                "Lights Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->Lights(lightsNum).RadGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Lights(lightsNum).Name);
            SetupOutputVariable(state,
                                "Lights Visible Radiation Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->Lights(lightsNum).VisGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Lights(lightsNum).Name);

            SetupOutputVariable(state,
                                "Lights Visible Radiation Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->Lights(lightsNum).VisGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Lights(lightsNum).Name);
            SetupOutputVariable(state,
                                "Lights Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->Lights(lightsNum).ConGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Lights(lightsNum).Name);
            SetupOutputVariable(state,
                                "Lights Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->Lights(lightsNum).ConGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Lights(lightsNum).Name);
            SetupOutputVariable(state,
                                "Lights Return Air Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->Lights(lightsNum).RetAirGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Lights(lightsNum).Name);
            SetupOutputVariable(state,
                                "Lights Return Air Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->Lights(lightsNum).RetAirGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Lights(lightsNum).Name);
            SetupOutputVariable(state,
                                "Lights Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->Lights(lightsNum).TotGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Lights(lightsNum).Name);
            SetupOutputVariable(state,
                                "Lights Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->Lights(lightsNum).TotGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->Lights(lightsNum).Name);
            SetupOutputVariable(state,
                                "Lights Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->Lights(lightsNum).Consumption,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->Lights(lightsNum).Name,
                                _,
                                "Electricity",
                                "InteriorLights",
                                state.dataHeatBal->Lights(lightsNum).EndUseSubcategory,
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->Lights(lightsNum).ZonePtr).Name,
                                state.dataHeatBal->Zone(state.dataHeatBal->Lights(lightsNum).ZonePtr).Multiplier,
                                state.dataHeatBal->Zone(state.dataHeatBal->Lights(lightsNum).ZonePtr).ListMultiplier,
                                _,
                                _,
                                state.dataHeatBal->space(state.dataHeatBal->Lights(lightsNum).spaceIndex).spaceType);
        }

        // Zone total report variables
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            if (addZoneOutputs(zoneNum)) {
                SetupOutputVariable(state,
                                    "Zone Lights Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsElecConsump,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Visible Radiation Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsVisGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Visible Radiation Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsVisGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Return Air Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsRetAirGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Return Air Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsRetAirGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Lights Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).LtsTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
            }
            // Reset zone output flag
            addZoneOutputs(zoneNum) = false;
        }

        // Space total report variables
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (addSpaceOutputs(spaceNum)) {
                SetupOutputVariable(state,
                                    "Space Lights Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsElecConsump,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Visible Radiation Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsVisGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Visible Radiation Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsVisGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Return Air Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsRetAirGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Return Air Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsRetAirGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Lights Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).LtsTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
            }
            // Reset space output flag
            addSpaceOutputs(spaceNum) = false;
        }
        for (int elecEqNum = 1; elecEqNum <= state.dataHeatBal->TotElecEquip; ++elecEqNum) {
            // Set flags for zone and space total report variables
            addZoneOutputs(state.dataHeatBal->ZoneElectric(elecEqNum).ZonePtr) = true;
            addSpaceOutputs(state.dataHeatBal->ZoneElectric(elecEqNum).spaceIndex) = true;
            // Object report variables
            SetupOutputVariable(state,
                                "Electric Equipment Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Power,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
            SetupOutputVariable(state,
                                "Electric Equipment Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Consumption,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name,
                                _,
                                "Electricity",
                                "InteriorEquipment",
                                state.dataHeatBal->ZoneElectric(elecEqNum).EndUseSubcategory,
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(elecEqNum).ZonePtr).Name,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(elecEqNum).ZonePtr).Multiplier,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneElectric(elecEqNum).ZonePtr).ListMultiplier,
                                _,
                                _,
                                state.dataHeatBal->space(state.dataHeatBal->ZoneElectric(elecEqNum).spaceIndex).spaceType);

            SetupOutputVariable(state,
                                "Electric Equipment Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneElectric(elecEqNum).RadGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
            SetupOutputVariable(state,
                                "Electric Equipment Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneElectric(elecEqNum).RadGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
            SetupOutputVariable(state,
                                "Electric Equipment Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneElectric(elecEqNum).ConGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
            SetupOutputVariable(state,
                                "Electric Equipment Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneElectric(elecEqNum).ConGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
            SetupOutputVariable(state,
                                "Electric Equipment Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneElectric(elecEqNum).LatGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
            SetupOutputVariable(state,
                                "Electric Equipment Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneElectric(elecEqNum).LatGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
            SetupOutputVariable(state,
                                "Electric Equipment Lost Heat Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneElectric(elecEqNum).LostEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
            SetupOutputVariable(state,
                                "Electric Equipment Lost Heat Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneElectric(elecEqNum).LostRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
            SetupOutputVariable(state,
                                "Electric Equipment Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneElectric(elecEqNum).TotGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
            SetupOutputVariable(state,
                                "Electric Equipment Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneElectric(elecEqNum).TotGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneElectric(elecEqNum).Name);
        }

        // Zone total report variables
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            if (addZoneOutputs(zoneNum)) {
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecConsump,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);

                SetupOutputVariable(state,
                                    "Zone Electric Equipment Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Lost Heat Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecLost,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Lost Heat Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecLostRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Electric Equipment Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ElecTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
            }
            // Reset zone output flag
            addZoneOutputs(zoneNum) = false;
        }

        // space total report variables
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (addSpaceOutputs(spaceNum)) {
                SetupOutputVariable(state,
                                    "Space Electric Equipment Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Electric Equipment Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecConsump,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);

                SetupOutputVariable(state,
                                    "Space Electric Equipment Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Electric Equipment Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Electric Equipment Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Electric Equipment Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Electric Equipment Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Electric Equipment Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Electric Equipment Lost Heat Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecLost,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Electric Equipment Lost Heat Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecLostRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Electric Equipment Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Electric Equipment Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ElecTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
            }
            // Reset space output flag
            addSpaceOutputs(spaceNum) = false;
        }
        // Object report variables
        for (int gasEqNum = 1; gasEqNum <= state.dataHeatBal->TotGasEquip; ++gasEqNum) {
            // Set flags for zone and space total report variables
            addZoneOutputs(state.dataHeatBal->ZoneGas(gasEqNum).ZonePtr) = true;
            addSpaceOutputs(state.dataHeatBal->ZoneGas(gasEqNum).spaceIndex) = true;
            SetupOutputVariable(state,
                                "Gas Equipment NaturalGas Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneGas(gasEqNum).Power,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
            SetupOutputVariable(state,
                                "Gas Equipment NaturalGas Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneGas(gasEqNum).Consumption,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name,
                                _,
                                "NaturalGas",
                                "InteriorEquipment",
                                state.dataHeatBal->ZoneGas(gasEqNum).EndUseSubcategory,
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(gasEqNum).ZonePtr).Name,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(gasEqNum).ZonePtr).Multiplier,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneGas(gasEqNum).ZonePtr).ListMultiplier,
                                _,
                                _,
                                state.dataHeatBal->space(state.dataHeatBal->ZoneGas(gasEqNum).spaceIndex).spaceType);

            SetupOutputVariable(state,
                                "Gas Equipment Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneGas(gasEqNum).RadGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
            SetupOutputVariable(state,
                                "Gas Equipment Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneGas(gasEqNum).ConGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
            SetupOutputVariable(state,
                                "Gas Equipment Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneGas(gasEqNum).LatGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
            SetupOutputVariable(state,
                                "Gas Equipment Lost Heat Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneGas(gasEqNum).LostEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
            SetupOutputVariable(state,
                                "Gas Equipment Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneGas(gasEqNum).TotGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
            SetupOutputVariable(state,
                                "Gas Equipment Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneGas(gasEqNum).RadGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
            SetupOutputVariable(state,
                                "Gas Equipment Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneGas(gasEqNum).ConGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
            SetupOutputVariable(state,
                                "Gas Equipment Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneGas(gasEqNum).LatGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
            SetupOutputVariable(state,
                                "Gas Equipment Lost Heat Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneGas(gasEqNum).LostRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
            SetupOutputVariable(state,
                                "Gas Equipment Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneGas(gasEqNum).TotGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneGas(gasEqNum).Name);
        }

        // Zone total report variables
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            if (addZoneOutputs(zoneNum)) {

                SetupOutputVariable(state,
                                    "Zone Gas Equipment NaturalGas Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Gas Equipment NaturalGas Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasConsump,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);

                SetupOutputVariable(state,
                                    "Zone Gas Equipment Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Gas Equipment Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Gas Equipment Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Gas Equipment Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Gas Equipment Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Gas Equipment Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Gas Equipment Lost Heat Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasLost,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Gas Equipment Lost Heat Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasLostRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Gas Equipment Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Gas Equipment Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).GasTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
            }
            // Reset zone output flag
            addZoneOutputs(zoneNum) = false;
        }

        // Space total report variables
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (addSpaceOutputs(spaceNum)) {

                SetupOutputVariable(state,
                                    "Space Gas Equipment NaturalGas Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Gas Equipment NaturalGas Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasConsump,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);

                SetupOutputVariable(state,
                                    "Space Gas Equipment Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Gas Equipment Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Gas Equipment Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Gas Equipment Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Gas Equipment Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Gas Equipment Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Gas Equipment Lost Heat Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasLost,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Gas Equipment Lost Heat Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasLostRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Gas Equipment Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Gas Equipment Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).GasTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
            }
            // Reset space output flag
            addSpaceOutputs(spaceNum) = false;
        }

        // Object report variables
        for (int hwEqNum = 1; hwEqNum <= state.dataHeatBal->TotHWEquip; ++hwEqNum) {
            // Set flags for zone and space total report variables
            addZoneOutputs(state.dataHeatBal->ZoneHWEq(hwEqNum).ZonePtr) = true;
            addSpaceOutputs(state.dataHeatBal->ZoneHWEq(hwEqNum).spaceIndex) = true;
            SetupOutputVariable(state,
                                "Hot Water Equipment District Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Power,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
            SetupOutputVariable(state,
                                "Hot Water Equipment District Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Consumption,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name,
                                _,
                                "DistrictHeating",
                                "InteriorEquipment",
                                state.dataHeatBal->ZoneHWEq(hwEqNum).EndUseSubcategory,
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(hwEqNum).ZonePtr).Name,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(hwEqNum).ZonePtr).Multiplier,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneHWEq(hwEqNum).ZonePtr).ListMultiplier,
                                _,
                                _,
                                state.dataHeatBal->space(state.dataHeatBal->ZoneHWEq(hwEqNum).spaceIndex).spaceType);

            SetupOutputVariable(state,
                                "Hot Water Equipment Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).RadGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
            SetupOutputVariable(state,
                                "Hot Water Equipment Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).RadGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
            SetupOutputVariable(state,
                                "Hot Water Equipment Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).ConGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
            SetupOutputVariable(state,
                                "Hot Water Equipment Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).ConGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
            SetupOutputVariable(state,
                                "Hot Water Equipment Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).LatGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
            SetupOutputVariable(state,
                                "Hot Water Equipment Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).LatGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
            SetupOutputVariable(state,
                                "Hot Water Equipment Lost Heat Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).LostEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
            SetupOutputVariable(state,
                                "Hot Water Equipment Lost Heat Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).LostRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
            SetupOutputVariable(state,
                                "Hot Water Equipment Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).TotGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
            SetupOutputVariable(state,
                                "Hot Water Equipment Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).TotGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneHWEq(hwEqNum).Name);
        }

        // Zone total report variables
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            if (addZoneOutputs(zoneNum)) {
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment District Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment District Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWConsump,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);

                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment Lost Heat Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWLost,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment Lost Heat Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWLostRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Hot Water Equipment Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).HWTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
            }
            // Reset zone output flag
            addZoneOutputs(zoneNum) = false;
        }

        // Space total report variables
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (addSpaceOutputs(spaceNum)) {
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment District Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment District Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWConsump,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);

                SetupOutputVariable(state,
                                    "Space Hot Water Equipment Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment Lost Heat Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWLost,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment Lost Heat Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWLostRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Hot Water Equipment Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).HWTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
            }
            // Reset space output flag
            addSpaceOutputs(spaceNum) = false;
        }

        // Object report variables
        for (int stmEqNum = 1; stmEqNum <= state.dataHeatBal->TotStmEquip; ++stmEqNum) {
            // Set flags for zone and space total report variables
            addZoneOutputs(state.dataHeatBal->ZoneSteamEq(stmEqNum).ZonePtr) = true;
            addSpaceOutputs(state.dataHeatBal->ZoneSteamEq(stmEqNum).spaceIndex) = true;
            SetupOutputVariable(state,
                                "Steam Equipment District Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Power,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
            SetupOutputVariable(state,
                                "Steam Equipment District Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Consumption,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name,
                                _,
                                "DistrictHeating",
                                "InteriorEquipment",
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).EndUseSubcategory,
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(stmEqNum).ZonePtr).Name,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(stmEqNum).ZonePtr).Multiplier,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneSteamEq(stmEqNum).ZonePtr).ListMultiplier,
                                _,
                                _,
                                state.dataHeatBal->space(state.dataHeatBal->ZoneSteamEq(stmEqNum).spaceIndex).spaceType);

            SetupOutputVariable(state,
                                "Steam Equipment Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).RadGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
            SetupOutputVariable(state,
                                "Steam Equipment Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).RadGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
            SetupOutputVariable(state,
                                "Steam Equipment Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).ConGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
            SetupOutputVariable(state,
                                "Steam Equipment Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).ConGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
            SetupOutputVariable(state,
                                "Steam Equipment Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).LatGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
            SetupOutputVariable(state,
                                "Steam Equipment Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).LatGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
            SetupOutputVariable(state,
                                "Steam Equipment Lost Heat Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).LostEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
            SetupOutputVariable(state,
                                "Steam Equipment Lost Heat Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).LostRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
            SetupOutputVariable(state,
                                "Steam Equipment Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).TotGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
            SetupOutputVariable(state,
                                "Steam Equipment Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).TotGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneSteamEq(stmEqNum).Name);
        }

        // Zone total report variables
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            if (addZoneOutputs(zoneNum)) {
                SetupOutputVariable(state,
                                    "Zone Steam Equipment District Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Steam Equipment District Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamConsump,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);

                SetupOutputVariable(state,
                                    "Zone Steam Equipment Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Steam Equipment Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Steam Equipment Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Steam Equipment Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Steam Equipment Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Steam Equipment Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Steam Equipment Lost Heat Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamLost,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Steam Equipment Lost Heat Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamLostRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Steam Equipment Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Steam Equipment Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).SteamTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
            }
            // Reset zone output flag
            addZoneOutputs(zoneNum) = false;
        }

        // Space total report variables
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (addSpaceOutputs(spaceNum)) {
                SetupOutputVariable(state,
                                    "Space Steam Equipment District Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Steam Equipment District Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamConsump,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);

                SetupOutputVariable(state,
                                    "Space Steam Equipment Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Steam Equipment Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Steam Equipment Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Steam Equipment Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Steam Equipment Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Steam Equipment Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Steam Equipment Lost Heat Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamLost,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Steam Equipment Lost Heat Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamLostRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Steam Equipment Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Steam Equipment Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).SteamTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
            }
            // Reset space output flag
            addSpaceOutputs(spaceNum) = false;
        }

        // Object report variables
        for (int othEqNum = 1; othEqNum <= state.dataHeatBal->TotOthEquip; ++othEqNum) {
            // Set flags for zone and space total report variables
            addZoneOutputs(state.dataHeatBal->ZoneOtherEq(othEqNum).ZonePtr) = true;
            addSpaceOutputs(state.dataHeatBal->ZoneOtherEq(othEqNum).spaceIndex) = true;
            if (state.dataHeatBal->ZoneOtherEq(othEqNum).OtherEquipFuelType != ExteriorEnergyUse::ExteriorFuelUsage::Unknown) {
                std::string fuelTypeString = state.dataHeatBal->ZoneOtherEq(othEqNum).otherEquipFuelTypeString;
                SetupOutputVariable(state,
                                    "Other Equipment " + fuelTypeString + " Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZoneOtherEq(othEqNum).Power,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
                SetupOutputVariable(state,
                                    "Other Equipment " + fuelTypeString + " Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZoneOtherEq(othEqNum).Consumption,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->ZoneOtherEq(othEqNum).Name,
                                    _,
                                    fuelTypeString,
                                    "InteriorEquipment",
                                    state.dataHeatBal->ZoneOtherEq(othEqNum).EndUseSubcategory,
                                    "Building",
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(othEqNum).ZonePtr).Name,
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(othEqNum).ZonePtr).Multiplier,
                                    state.dataHeatBal->Zone(state.dataHeatBal->ZoneOtherEq(othEqNum).ZonePtr).ListMultiplier,
                                    _,
                                    _,
                                    state.dataHeatBal->space(state.dataHeatBal->ZoneOtherEq(othEqNum).spaceIndex).spaceType);
            }

            SetupOutputVariable(state,
                                "Other Equipment Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).RadGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
            SetupOutputVariable(state,
                                "Other Equipment Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).RadGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
            SetupOutputVariable(state,
                                "Other Equipment Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).ConGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
            SetupOutputVariable(state,
                                "Other Equipment Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).ConGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
            SetupOutputVariable(state,
                                "Other Equipment Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).LatGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
            SetupOutputVariable(state,
                                "Other Equipment Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).LatGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
            SetupOutputVariable(state,
                                "Other Equipment Lost Heat Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).LostEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
            SetupOutputVariable(state,
                                "Other Equipment Lost Heat Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).LostRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
            SetupOutputVariable(state,
                                "Other Equipment Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).TotGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
            SetupOutputVariable(state,
                                "Other Equipment Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).TotGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneOtherEq(othEqNum).Name);
        }

        // Zone total report variables
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            if (addZoneOutputs(zoneNum)) {
                bool firstFuelType = true;
                std::string firstFuel;
                for (std::string fuelTypeString : state.dataHeatBal->Zone(zoneNum).otherEquipFuelTypeNames) {
                    if (firstFuelType) {
                        firstFuel = fuelTypeString;
                        SetupOutputVariable(state,
                                            "Zone Other Equipment " + fuelTypeString + " Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->ZnRpt(zoneNum).OtherPower,
                                            OutputProcessor::SOVTimeStepType::Zone,
                                            OutputProcessor::SOVStoreType::Average,
                                            state.dataHeatBal->Zone(zoneNum).Name);
                        SetupOutputVariable(state,
                                            "Zone Other Equipment " + fuelTypeString + " Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->ZnRpt(zoneNum).OtherConsump,
                                            OutputProcessor::SOVTimeStepType::Zone,
                                            OutputProcessor::SOVStoreType::Summed,
                                            state.dataHeatBal->Zone(zoneNum).Name);
                        firstFuelType = false;
                    } else {
                        ShowWarningError(state,
                                         "setupIHGOutputs: Output variables=Zone Other Equipment " + fuelTypeString +
                                             " Rate and Energy are not available.");
                        ShowContinueError(state, "Only the first Other Equipment fuel type used in a zone is reported. (" + firstFuel + ")");
                    }
                }

                SetupOutputVariable(state,
                                    "Zone Other Equipment Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).OtherRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Other Equipment Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).OtherRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Other Equipment Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).OtherConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Other Equipment Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).OtherConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Other Equipment Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).OtherLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Other Equipment Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).OtherLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Other Equipment Lost Heat Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).OtherLost,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Other Equipment Lost Heat Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).OtherLostRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Other Equipment Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).OtherTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Other Equipment Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).OtherTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
            }
            // Reset zone output flag
            addZoneOutputs(zoneNum) = false;
        }

        // Space total report variables
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (addSpaceOutputs(spaceNum)) {
                bool firstFuelType = true;
                std::string firstFuel;
                for (std::string fuelTypeString : state.dataHeatBal->space(spaceNum).otherEquipFuelTypeNames) {
                    if (firstFuelType) {
                        firstFuel = fuelTypeString;
                        SetupOutputVariable(state,
                                            "Space Other Equipment " + fuelTypeString + " Rate",
                                            OutputProcessor::Unit::W,
                                            state.dataHeatBal->spaceRpt(spaceNum).OtherPower,
                                            OutputProcessor::SOVTimeStepType::Zone,
                                            OutputProcessor::SOVStoreType::Average,
                                            state.dataHeatBal->space(spaceNum).Name);
                        SetupOutputVariable(state,
                                            "Space Other Equipment " + fuelTypeString + " Energy",
                                            OutputProcessor::Unit::J,
                                            state.dataHeatBal->spaceRpt(spaceNum).OtherConsump,
                                            OutputProcessor::SOVTimeStepType::Zone,
                                            OutputProcessor::SOVStoreType::Summed,
                                            state.dataHeatBal->space(spaceNum).Name);
                        firstFuelType = false;
                    } else {
                        ShowWarningError(state,
                                         "setupIHGOutputs: Output variables=Space Other Equipment " + fuelTypeString +
                                             " Rate and Energy are not available.");
                        ShowContinueError(state, "Only the first Other Equipment fuel type used in a zone is reported. (" + firstFuel + ")");
                    }
                }

                SetupOutputVariable(state,
                                    "Space Other Equipment Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).OtherRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Other Equipment Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).OtherRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Other Equipment Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).OtherConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Other Equipment Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).OtherConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Other Equipment Latent Gain Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).OtherLatGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Other Equipment Latent Gain Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).OtherLatGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Other Equipment Lost Heat Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).OtherLost,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Other Equipment Lost Heat Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).OtherLostRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Other Equipment Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).OtherTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Other Equipment Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).OtherTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
            }
            // Reset space output flag
            addSpaceOutputs(spaceNum) = false;
        }
        // Object report variables
        for (int itEqNum = 1; itEqNum <= state.dataHeatBal->TotITEquip; ++itEqNum) {
            // Set flags for zone and space total report variables
            addZoneOutputs(state.dataHeatBal->ZoneITEq(itEqNum).ZonePtr) = true;
            addSpaceOutputs(state.dataHeatBal->ZoneITEq(itEqNum).spaceIndex) = true;
            SetupOutputVariable(state,
                                "ITE CPU Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneITEq(itEqNum).CPUPower,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Fan Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneITEq(itEqNum).FanPower,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE UPS Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneITEq(itEqNum).UPSPower,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE CPU Electricity Rate at Design Inlet Conditions",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneITEq(itEqNum).CPUPowerAtDesign,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Fan Electricity Rate at Design Inlet Conditions",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneITEq(itEqNum).FanPowerAtDesign,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE UPS Heat Gain to Zone Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneITEq(itEqNum).UPSGainRateToZone,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Total Heat Gain to Zone Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneITEq(itEqNum).ConGainRateToZone,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);

            SetupOutputVariable(state,
                                "ITE CPU Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneITEq(itEqNum).CPUConsumption,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name,
                                _,
                                "Electricity",
                                "InteriorEquipment",
                                state.dataHeatBal->ZoneITEq(itEqNum).EndUseSubcategoryCPU,
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(itEqNum).ZonePtr).Name,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(itEqNum).ZonePtr).Multiplier,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(itEqNum).ZonePtr).ListMultiplier,
                                _,
                                _,
                                state.dataHeatBal->space(state.dataHeatBal->ZoneITEq(itEqNum).spaceIndex).spaceType);

            SetupOutputVariable(state,
                                "ITE Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneITEq(itEqNum).FanConsumption,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name,
                                _,
                                "Electricity",
                                "InteriorEquipment",
                                state.dataHeatBal->ZoneITEq(itEqNum).EndUseSubcategoryFan,
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(itEqNum).ZonePtr).Name,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(itEqNum).ZonePtr).Multiplier,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(itEqNum).ZonePtr).ListMultiplier,
                                _,
                                _,
                                state.dataHeatBal->space(state.dataHeatBal->ZoneITEq(itEqNum).spaceIndex).spaceType);
            SetupOutputVariable(state,
                                "ITE UPS Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneITEq(itEqNum).UPSConsumption,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name,
                                _,
                                "Electricity",
                                "InteriorEquipment",
                                state.dataHeatBal->ZoneITEq(itEqNum).EndUseSubcategoryUPS,
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(itEqNum).ZonePtr).Name,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(itEqNum).ZonePtr).Multiplier,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneITEq(itEqNum).ZonePtr).ListMultiplier,
                                _,
                                _,
                                state.dataHeatBal->space(state.dataHeatBal->ZoneITEq(itEqNum).spaceIndex).spaceType);
            SetupOutputVariable(state,
                                "ITE CPU Electricity Energy at Design Inlet Conditions",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneITEq(itEqNum).CPUEnergyAtDesign,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Fan Electricity Energy at Design Inlet Conditions",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneITEq(itEqNum).FanEnergyAtDesign,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE UPS Heat Gain to Zone Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneITEq(itEqNum).UPSGainEnergyToZone,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Total Heat Gain to Zone Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneITEq(itEqNum).ConGainEnergyToZone,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);

            SetupOutputVariable(state,
                                "ITE Standard Density Air Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZoneITEq(itEqNum).AirVolFlowStdDensity,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Current Density Air Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataHeatBal->ZoneITEq(itEqNum).AirVolFlowCurDensity,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataHeatBal->ZoneITEq(itEqNum).AirMassFlow,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Dry-Bulb Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBal->ZoneITEq(itEqNum).AirInletDryBulbT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Dewpoint Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBal->ZoneITEq(itEqNum).AirInletDewpointT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Relative Humidity",
                                OutputProcessor::Unit::Perc,
                                state.dataHeatBal->ZoneITEq(itEqNum).AirInletRelHum,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Outlet Dry-Bulb Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBal->ZoneITEq(itEqNum).AirOutletDryBulbT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            if (state.dataHeatBal->ZoneITEq(itEqNum).SupplyAirNodeNum != 0) {
                SetupOutputVariable(state,
                                    "ITE Supply Heat Index",
                                    OutputProcessor::Unit::None,
                                    state.dataHeatBal->ZoneITEq(itEqNum).SHI,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->ZoneITEq(itEqNum).Name);
            }
            SetupOutputVariable(state,
                                "ITE Air Inlet Operating Range Exceeded Time",
                                OutputProcessor::Unit::hr,
                                state.dataHeatBal->ZoneITEq(itEqNum).TimeOutOfOperRange,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time",
                                OutputProcessor::Unit::hr,
                                state.dataHeatBal->ZoneITEq(itEqNum).TimeAboveDryBulbT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time",
                                OutputProcessor::Unit::hr,
                                state.dataHeatBal->ZoneITEq(itEqNum).TimeBelowDryBulbT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Dewpoint Temperature Above Operating Range Time",
                                OutputProcessor::Unit::hr,
                                state.dataHeatBal->ZoneITEq(itEqNum).TimeAboveDewpointT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Dewpoint Temperature Below Operating Range Time",
                                OutputProcessor::Unit::hr,
                                state.dataHeatBal->ZoneITEq(itEqNum).TimeBelowDewpointT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Relative Humidity Above Operating Range Time",
                                OutputProcessor::Unit::hr,
                                state.dataHeatBal->ZoneITEq(itEqNum).TimeAboveRH,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Relative Humidity Below Operating Range Time",
                                OutputProcessor::Unit::hr,
                                state.dataHeatBal->ZoneITEq(itEqNum).TimeBelowRH,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Dry-Bulb Temperature Difference Above Operating Range",
                                OutputProcessor::Unit::deltaC,
                                state.dataHeatBal->ZoneITEq(itEqNum).DryBulbTAboveDeltaT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Dry-Bulb Temperature Difference Below Operating Range",
                                OutputProcessor::Unit::deltaC,
                                state.dataHeatBal->ZoneITEq(itEqNum).DryBulbTBelowDeltaT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Dewpoint Temperature Difference Above Operating Range",
                                OutputProcessor::Unit::deltaC,
                                state.dataHeatBal->ZoneITEq(itEqNum).DewpointTAboveDeltaT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Dewpoint Temperature Difference Below Operating Range",
                                OutputProcessor::Unit::deltaC,
                                state.dataHeatBal->ZoneITEq(itEqNum).DewpointTBelowDeltaT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Relative Humidity Difference Above Operating Range",
                                OutputProcessor::Unit::Perc,
                                state.dataHeatBal->ZoneITEq(itEqNum).RHAboveDeltaRH,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
            SetupOutputVariable(state,
                                "ITE Air Inlet Relative Humidity Difference Below Operating Range",
                                OutputProcessor::Unit::Perc,
                                state.dataHeatBal->ZoneITEq(itEqNum).RHBelowDeltaRH,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneITEq(itEqNum).Name);
        }

        // Zone total report variables
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            if (addZoneOutputs(zoneNum)) {
                SetupOutputVariable(state,
                                    "Zone ITE CPU Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqCPUPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Fan Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqFanPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE UPS Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqUPSPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE CPU Electricity Rate at Design Inlet Conditions",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqCPUPowerAtDesign,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Fan Electricity Rate at Design Inlet Conditions",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqFanPowerAtDesign,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE UPS Heat Gain to Zone Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqUPSGainRateToZone,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Total Heat Gain to Zone Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqConGainRateToZone,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Adjusted Return Air Temperature",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEAdjReturnTemp,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);

                SetupOutputVariable(state,
                                    "Zone ITE CPU Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqCPUConsumption,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Fan Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqFanConsumption,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE UPS Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqUPSConsumption,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE CPU Electricity Energy at Design Inlet Conditions",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqCPUEnergyAtDesign,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Fan Electricity Energy at Design Inlet Conditions",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqFanEnergyAtDesign,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE UPS Heat Gain to Zone Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqUPSGainEnergyToZone,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Total Heat Gain to Zone Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqConGainEnergyToZone,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);

                SetupOutputVariable(state,
                                    "Zone ITE Standard Density Air Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqAirVolFlowStdDensity,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqAirMassFlow,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Average Supply Heat Index",
                                    OutputProcessor::Unit::None,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqSHI,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Any Air Inlet Operating Range Exceeded Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqTimeOutOfOperRange,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Any Air Inlet Dry-Bulb Temperature Above Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqTimeAboveDryBulbT,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Any Air Inlet Dry-Bulb Temperature Below Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqTimeBelowDryBulbT,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Any Air Inlet Dewpoint Temperature Above Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqTimeAboveDewpointT,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Any Air Inlet Dewpoint Temperature Below Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqTimeBelowDewpointT,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Any Air Inlet Relative Humidity Above Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqTimeAboveRH,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone ITE Any Air Inlet Relative Humidity Below Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->ZnRpt(zoneNum).ITEqTimeBelowRH,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
            }
            // Reset zone output flag
            addZoneOutputs(zoneNum) = false;
        }

        // Space total report variables
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (addSpaceOutputs(spaceNum)) {
                SetupOutputVariable(state,
                                    "Space ITE CPU Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Fan Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqFanPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE UPS Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE CPU Electricity Rate at Design Inlet Conditions",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUPowerAtDesign,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Fan Electricity Rate at Design Inlet Conditions",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqFanPowerAtDesign,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE UPS Heat Gain to Zone Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSGainRateToZone,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Total Heat Gain to Zone Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqConGainRateToZone,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                // Not applicable for space until space has it's own air temeratures
                // Setup Output Variable(state,
                //                    "Space ITE Adjusted Return Air Temperature",
                //                    OutputProcessor::Unit::W,
                //                    state.dataHeatBal->spaceRpt(spaceNum).ITEAdjReturnTemp,
                //                    OutputProcessor::SOVTimeStepType::Zone,
                //                    OutputProcessor::SOVStoreType::Average,
                //                    state.dataHeatBal->space(spaceNum).Name);

                SetupOutputVariable(state,
                                    "Space ITE CPU Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUConsumption,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Fan Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqFanConsumption,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE UPS Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSConsumption,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE CPU Electricity Energy at Design Inlet Conditions",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUEnergyAtDesign,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Fan Electricity Energy at Design Inlet Conditions",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqFanEnergyAtDesign,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE UPS Heat Gain to Zone Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSGainEnergyToZone,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Total Heat Gain to Zone Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqConGainEnergyToZone,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);

                SetupOutputVariable(state,
                                    "Space ITE Standard Density Air Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqAirVolFlowStdDensity,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqAirMassFlow,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Average Supply Heat Index",
                                    OutputProcessor::Unit::None,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqSHI,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Any Air Inlet Operating Range Exceeded Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeOutOfOperRange,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Any Air Inlet Dry-Bulb Temperature Above Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeAboveDryBulbT,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Any Air Inlet Dry-Bulb Temperature Below Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeBelowDryBulbT,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Any Air Inlet Dewpoint Temperature Above Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeAboveDewpointT,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Any Air Inlet Dewpoint Temperature Below Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeBelowDewpointT,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Any Air Inlet Relative Humidity Above Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeAboveRH,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space ITE Any Air Inlet Relative Humidity Below Operating Range Time",
                                    OutputProcessor::Unit::hr,
                                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeBelowRH,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
            }
            // Reset space output flag
            addSpaceOutputs(spaceNum) = false;
        }

        // Object report variables
        for (int bbHeatNum = 1; bbHeatNum <= state.dataHeatBal->TotBBHeat; ++bbHeatNum) {
            // Set flags for zone and space total report variables
            addZoneOutputs(state.dataHeatBal->ZoneBBHeat(bbHeatNum).ZonePtr) = true;
            addSpaceOutputs(state.dataHeatBal->ZoneBBHeat(bbHeatNum).spaceIndex) = true;
            SetupOutputVariable(state,
                                "Baseboard Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).Power,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).Name);
            SetupOutputVariable(state,
                                "Baseboard Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).Consumption,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).Name,
                                _,
                                "Electricity",
                                "InteriorEquipment",
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).EndUseSubcategory,
                                "Building",
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(bbHeatNum).ZonePtr).Name,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(bbHeatNum).ZonePtr).Multiplier,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneBBHeat(bbHeatNum).ZonePtr).ListMultiplier,
                                _,
                                _,
                                state.dataHeatBal->space(state.dataHeatBal->ZoneBBHeat(bbHeatNum).spaceIndex).spaceType);

            SetupOutputVariable(state,
                                "Baseboard Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).RadGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).Name);
            SetupOutputVariable(state,
                                "Baseboard Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).RadGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).Name);
            SetupOutputVariable(state,
                                "Baseboard Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).ConGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).Name);
            SetupOutputVariable(state,
                                "Baseboard Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).ConGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).Name);
            SetupOutputVariable(state,
                                "Baseboard Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).TotGainEnergy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).Name);
            SetupOutputVariable(state,
                                "Baseboard Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).TotGainRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataHeatBal->ZoneBBHeat(bbHeatNum).Name);
        }

        // Zone total report variables
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            if (addZoneOutputs(zoneNum)) {
                SetupOutputVariable(state,
                                    "Zone Baseboard Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).BaseHeatPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).BaseHeatElecCons,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);

                SetupOutputVariable(state,
                                    "Zone Baseboard Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).BaseHeatRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).BaseHeatRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).BaseHeatConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).BaseHeatConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->ZnRpt(zoneNum).BaseHeatTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->Zone(zoneNum).Name);
                SetupOutputVariable(state,
                                    "Zone Baseboard Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->ZnRpt(zoneNum).BaseHeatTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->Zone(zoneNum).Name);
            }
            // Reset zone output flag
            addZoneOutputs(zoneNum) = false;
        }

        // Space total report variables
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (addSpaceOutputs(spaceNum)) {
                SetupOutputVariable(state,
                                    "Space Baseboard Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).BaseHeatPower,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Baseboard Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).BaseHeatElecCons,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);

                SetupOutputVariable(state,
                                    "Space Baseboard Radiant Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).BaseHeatRadGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Baseboard Radiant Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).BaseHeatRadGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Baseboard Convective Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).BaseHeatConGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Baseboard Convective Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).BaseHeatConGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Baseboard Total Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataHeatBal->spaceRpt(spaceNum).BaseHeatTotGain,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    state.dataHeatBal->space(spaceNum).Name);
                SetupOutputVariable(state,
                                    "Space Baseboard Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataHeatBal->spaceRpt(spaceNum).BaseHeatTotGainRate,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    state.dataHeatBal->space(spaceNum).Name);
            }
            // Reset space output flag
            addSpaceOutputs(spaceNum) = false;
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
        Real64 Q;                        // , QR
        Real64 TotalPeopleGain;          // Total heat gain from people (intermediate calculational variable)
        Real64 SensiblePeopleGain;       // Sensible heat gain from people (intermediate calculational variable)
        Real64 FractionConvected;        // For general lighting, fraction of heat from lights convected to zone air
        Real64 FractionReturnAir;        // For general lighting, fraction of heat from lights convected to zone's return air
        Real64 FractionRadiant;          // For general lighting, fraction of heat from lights to zone that is long wave
        Real64 ReturnPlenumTemp;         // Air temperature of a zone's return air plenum (C)
        Real64 pulseMultipler;           // use to create a pulse for the load component report computations

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

        for (auto &e : state.dataHeatBal->spaceIntGain) {
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

        for (auto &e : state.dataHeatBal->spaceRpt) {
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
        for (int Loop = 1; Loop <= state.dataHeatBal->TotPeople; ++Loop) {
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

            int spaceNum = state.dataHeatBal->People(Loop).spaceIndex;
            state.dataHeatBal->spaceIntGain(spaceNum).NOFOCC += state.dataHeatBal->People(Loop).NumOcc;
            state.dataHeatBal->spaceIntGain(spaceNum).QOCRAD += state.dataHeatBal->People(Loop).RadGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QOCCON += state.dataHeatBal->People(Loop).ConGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QOCSEN += state.dataHeatBal->People(Loop).SenGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QOCLAT += state.dataHeatBal->People(Loop).LatGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QOCTOT += state.dataHeatBal->People(Loop).TotGainRate;
        }

        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            state.dataHeatBal->ZoneIntGain(zoneNum).NOFOCC = 0.0;
            state.dataHeatBal->ZoneIntGain(zoneNum).QOCRAD = 0.0;
            state.dataHeatBal->ZoneIntGain(zoneNum).QOCCON = 0.0;
            state.dataHeatBal->ZoneIntGain(zoneNum).QOCSEN = 0.0;
            state.dataHeatBal->ZoneIntGain(zoneNum).QOCLAT = 0.0;
            state.dataHeatBal->ZoneIntGain(zoneNum).QOCTOT = 0.0;
            for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                state.dataHeatBal->ZoneIntGain(zoneNum).NOFOCC += state.dataHeatBal->spaceIntGain(spaceNum).NOFOCC;
                state.dataHeatBal->ZoneIntGain(zoneNum).QOCRAD += state.dataHeatBal->spaceIntGain(spaceNum).QOCRAD;
                state.dataHeatBal->ZoneIntGain(zoneNum).QOCCON += state.dataHeatBal->spaceIntGain(spaceNum).QOCCON;
                state.dataHeatBal->ZoneIntGain(zoneNum).QOCSEN += state.dataHeatBal->spaceIntGain(spaceNum).QOCSEN;
                state.dataHeatBal->ZoneIntGain(zoneNum).QOCLAT += state.dataHeatBal->spaceIntGain(spaceNum).QOCLAT;
                state.dataHeatBal->ZoneIntGain(zoneNum).QOCTOT += state.dataHeatBal->spaceIntGain(spaceNum).QOCTOT;
            }
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotLights; ++Loop) {
            int NZ = state.dataHeatBal->Lights(Loop).ZonePtr;
            int spaceNum = state.dataHeatBal->Lights(Loop).spaceIndex;
            Q = state.dataHeatBal->Lights(Loop).DesignLevel * GetCurrentScheduleValue(state, state.dataHeatBal->Lights(Loop).SchedPtr);

            if (state.dataDaylightingData->ZoneDaylight(NZ).totRefPts > 0) {
                if (state.dataHeatBal->Lights(Loop).FractionReplaceable > 0.0) { // FractionReplaceable can only be 0 or 1 for these models
                    Q *= state.dataDaylightingData->spacePowerReductionFactor(spaceNum);
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

            state.dataHeatBal->spaceRpt(spaceNum).LtsPower += state.dataHeatBal->Lights(Loop).Power;
            state.dataHeatBal->spaceIntGain(spaceNum).QLTRAD += state.dataHeatBal->Lights(Loop).RadGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QLTSW += state.dataHeatBal->Lights(Loop).VisGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QLTCON += state.dataHeatBal->Lights(Loop).ConGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QLTCRA += state.dataHeatBal->Lights(Loop).RetAirGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QLTTOT += state.dataHeatBal->Lights(Loop).TotGainRate;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotElecEquip; ++Loop) {
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

            int spaceNum = state.dataHeatBal->ZoneElectric(Loop).spaceIndex;
            state.dataHeatBal->spaceRpt(spaceNum).ElecPower += state.dataHeatBal->ZoneElectric(Loop).Power;
            state.dataHeatBal->spaceIntGain(spaceNum).QEERAD += state.dataHeatBal->ZoneElectric(Loop).RadGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QEECON += state.dataHeatBal->ZoneElectric(Loop).ConGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QEELAT += state.dataHeatBal->ZoneElectric(Loop).LatGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QEELost += state.dataHeatBal->ZoneElectric(Loop).LostRate;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotGasEquip; ++Loop) {
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

            int spaceNum = state.dataHeatBal->ZoneGas(Loop).spaceIndex;
            state.dataHeatBal->spaceRpt(spaceNum).GasPower += state.dataHeatBal->ZoneGas(Loop).Power;
            state.dataHeatBal->spaceIntGain(spaceNum).QGERAD += state.dataHeatBal->ZoneGas(Loop).RadGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QGECON += state.dataHeatBal->ZoneGas(Loop).ConGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QGELAT += state.dataHeatBal->ZoneGas(Loop).LatGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QGELost += state.dataHeatBal->ZoneGas(Loop).LostRate;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotOthEquip; ++Loop) {
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
            state.dataHeatBal->ZnRpt(NZ).OtherPower += state.dataHeatBal->ZoneOtherEq(Loop).Power;
            state.dataHeatBal->ZoneIntGain(NZ).QOERAD += state.dataHeatBal->ZoneOtherEq(Loop).RadGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QOECON += state.dataHeatBal->ZoneOtherEq(Loop).ConGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QOELAT += state.dataHeatBal->ZoneOtherEq(Loop).LatGainRate;
            state.dataHeatBal->ZoneIntGain(NZ).QOELost += state.dataHeatBal->ZoneOtherEq(Loop).LostRate;

            int spaceNum = state.dataHeatBal->ZoneOtherEq(Loop).spaceIndex;
            state.dataHeatBal->spaceRpt(spaceNum).OtherPower += state.dataHeatBal->ZoneOtherEq(Loop).Power;
            state.dataHeatBal->spaceIntGain(spaceNum).QOERAD += state.dataHeatBal->ZoneOtherEq(Loop).RadGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QOECON += state.dataHeatBal->ZoneOtherEq(Loop).ConGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QOELAT += state.dataHeatBal->ZoneOtherEq(Loop).LatGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QOELost += state.dataHeatBal->ZoneOtherEq(Loop).LostRate;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotHWEquip; ++Loop) {
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

            int spaceNum = state.dataHeatBal->ZoneHWEq(Loop).spaceIndex;
            state.dataHeatBal->spaceRpt(spaceNum).HWPower += state.dataHeatBal->ZoneHWEq(Loop).Power;
            state.dataHeatBal->spaceIntGain(spaceNum).QHWRAD += state.dataHeatBal->ZoneHWEq(Loop).RadGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QHWCON += state.dataHeatBal->ZoneHWEq(Loop).ConGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QHWLAT += state.dataHeatBal->ZoneHWEq(Loop).LatGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QHWLost += state.dataHeatBal->ZoneHWEq(Loop).LostRate;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotStmEquip; ++Loop) {
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

            int spaceNum = state.dataHeatBal->ZoneSteamEq(Loop).spaceIndex;
            state.dataHeatBal->spaceRpt(spaceNum).SteamPower += state.dataHeatBal->ZoneSteamEq(Loop).Power;
            state.dataHeatBal->spaceIntGain(spaceNum).QSERAD += state.dataHeatBal->ZoneSteamEq(Loop).RadGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QSECON += state.dataHeatBal->ZoneSteamEq(Loop).ConGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QSELAT += state.dataHeatBal->ZoneSteamEq(Loop).LatGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QSELost += state.dataHeatBal->ZoneSteamEq(Loop).LostRate;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotBBHeat; ++Loop) {
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

            int spaceNum = state.dataHeatBal->ZoneBBHeat(Loop).spaceIndex;
            state.dataHeatBal->spaceRpt(spaceNum).BaseHeatPower += state.dataHeatBal->ZoneBBHeat(Loop).Power;
            state.dataHeatBal->spaceIntGain(spaceNum).QBBRAD += state.dataHeatBal->ZoneBBHeat(Loop).RadGainRate;
            state.dataHeatBal->spaceIntGain(spaceNum).QBBCON += state.dataHeatBal->ZoneBBHeat(Loop).ConGainRate;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotCO2Gen; ++Loop) {
            int NZ = state.dataHeatBal->ZoneCO2Gen(Loop).ZonePtr;
            state.dataHeatBal->ZoneCO2Gen(Loop).CO2GainRate =
                state.dataHeatBal->ZoneCO2Gen(Loop).CO2DesignRate * GetCurrentScheduleValue(state, state.dataHeatBal->ZoneCO2Gen(Loop).SchedPtr);
            state.dataHeatBal->ZnRpt(NZ).CO2Rate += state.dataHeatBal->ZoneCO2Gen(Loop).CO2GainRate;
        }

        if (state.dataHeatBal->TotITEquip > 0) CalcZoneITEq(state);

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

            InternalHeatGains::SumAllInternalLatentGains(state, NZ, state.dataHeatBalFanSys->ZoneLatentGain(NZ));
            // Added for hybrid model
            if (state.dataHybridModel->FlagHybridModel_PC) {
                InternalHeatGains::SumAllInternalLatentGainsExceptPeople(state, NZ, state.dataHeatBalFanSys->ZoneLatentGainExceptPeople(NZ));
            }
        }

        // QL is per radiant enclosure (one or more spaces if grouped by air boundaries)
        for (int enclosureNum = 1; enclosureNum <= state.dataViewFactor->NumOfRadiantEnclosures; ++enclosureNum) {
            auto &thisEnclosure(state.dataViewFactor->EnclRadInfo(enclosureNum));
            state.dataHeatBal->EnclRadQThermalRad(enclosureNum) = 0.0;
            for (int const spaceNum : thisEnclosure.spaceNums) {
                Real64 spaceQL;
                InternalHeatGains::SumAllSpaceInternalRadiationGains(state, spaceNum, spaceQL);
                state.dataHeatBal->EnclRadQThermalRad(enclosureNum) += spaceQL;
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
                int const radEnclosureNum = state.dataHeatBal->space(state.dataSurface->Surface(SurfNum).spaceNum).radiantEnclosureNum;
                if (!state.dataGlobal->doLoadComponentPulseNow) {
                    state.dataHeatBal->SurfQdotRadIntGainsInPerArea(SurfNum) = state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum) *
                                                                               state.dataHeatBal->EnclRadThermAbsMult(radEnclosureNum) *
                                                                               state.dataHeatBalSurf->SurfAbsThermalInt(SurfNum);
                } else {
                    state.dataInternalHeatGains->curQL = state.dataHeatBal->EnclRadQThermalRad(radEnclosureNum);
                    // for the loads component report during the special sizing run increase the radiant portion
                    // a small amount to create a "pulse" of heat that is used for the delayed loads
                    state.dataInternalHeatGains->adjQL =
                        state.dataInternalHeatGains->curQL + state.dataViewFactor->EnclRadInfo(radEnclosureNum).FloorArea * pulseMultipler;
                    // ITABSF is the Inside Thermal Absorptance
                    // EnclRadThermAbsMult is a multiplier for each zone
                    // SurfQdotRadIntGainsInPerArea is the thermal radiation absorbed on inside surfaces
                    state.dataHeatBal->SurfQdotRadIntGainsInPerArea(SurfNum) = state.dataInternalHeatGains->adjQL *
                                                                               state.dataHeatBal->EnclRadThermAbsMult(radEnclosureNum) *
                                                                               state.dataHeatBalSurf->SurfAbsThermalInt(SurfNum);
                    // store the magnitude and time of the pulse
                    state.dataOutRptTab->radiantPulseTimestep(state.dataSize->CurOverallSimDay, zoneNum) =
                        (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;
                    state.dataOutRptTab->radiantPulseReceived(state.dataSize->CurOverallSimDay, SurfNum) =
                        (state.dataInternalHeatGains->adjQL - state.dataInternalHeatGains->curQL) *
                        state.dataHeatBal->EnclRadThermAbsMult(radEnclosureNum) * state.dataHeatBalSurf->SurfAbsThermalInt(SurfNum) *
                        state.dataSurface->Surface(SurfNum).Area;
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
        for (Loop = 1; Loop <= state.dataHeatBal->TotITEquip; ++Loop) {
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

        // Space total report variables
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUPower = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqFanPower = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSPower = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUPowerAtDesign = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqFanPowerAtDesign = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSGainRateToZone = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqConGainRateToZone = 0.0;

            state.dataHeatBal->spaceRpt(spaceNum).ITEAdjReturnTemp = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUConsumption = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqFanConsumption = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSConsumption = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUEnergyAtDesign = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqFanEnergyAtDesign = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSGainEnergyToZone = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqConGainEnergyToZone = 0.0;

            state.dataHeatBal->spaceRpt(spaceNum).ITEqAirVolFlowStdDensity = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqAirMassFlow = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqSHI = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeOutOfOperRange = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeAboveDryBulbT = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeBelowDryBulbT = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeAboveDewpointT = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeBelowDewpointT = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeAboveRH = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeBelowRH = 0.0;

            state.dataHeatBal->spaceRpt(spaceNum).SumTinMinusTSup = 0.0;
            state.dataHeatBal->spaceRpt(spaceNum).SumToutMinusTSup = 0.0;
        } // Space init spaceNum

        for (Loop = 1; Loop <= state.dataHeatBal->TotITEquip; ++Loop) {
            // Get schedules
            NZ = state.dataHeatBal->ZoneITEq(Loop).ZonePtr;
            int spaceNum = state.dataHeatBal->ZoneITEq(Loop).spaceIndex;
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
                    if (state.dataHeatBal->ZoneITEq(Loop).zoneEqIndex > 0) {
                        int ZoneAirInletNode = state.dataZoneEquip->ZoneEquipConfig(NZ).InletNode(1);
                        TSupply = state.dataLoopNodes->Node(ZoneAirInletNode).Temp;
                    } else {
                        TSupply = state.dataHeatBalFanSys->MAT(NZ);
                    }
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

            state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUPower += state.dataHeatBal->ZoneITEq(Loop).CPUPower;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqFanPower += state.dataHeatBal->ZoneITEq(Loop).FanPower;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSPower += state.dataHeatBal->ZoneITEq(Loop).UPSPower;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUPowerAtDesign += state.dataHeatBal->ZoneITEq(Loop).CPUPowerAtDesign;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqFanPowerAtDesign += state.dataHeatBal->ZoneITEq(Loop).FanPowerAtDesign;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSGainRateToZone += state.dataHeatBal->ZoneITEq(Loop).UPSGainRateToZone;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqConGainRateToZone += state.dataHeatBal->ZoneITEq(Loop).ConGainRateToZone;

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

            state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUConsumption += state.dataHeatBal->ZoneITEq(Loop).CPUConsumption;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqFanConsumption += state.dataHeatBal->ZoneITEq(Loop).FanConsumption;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSConsumption += state.dataHeatBal->ZoneITEq(Loop).UPSConsumption;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqCPUEnergyAtDesign += state.dataHeatBal->ZoneITEq(Loop).CPUEnergyAtDesign;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqFanEnergyAtDesign += state.dataHeatBal->ZoneITEq(Loop).FanEnergyAtDesign;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqUPSGainEnergyToZone += state.dataHeatBal->ZoneITEq(Loop).UPSGainEnergyToZone;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqConGainEnergyToZone += state.dataHeatBal->ZoneITEq(Loop).ConGainEnergyToZone;

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

            state.dataHeatBal->spaceRpt(spaceNum).ITEqAirVolFlowStdDensity += state.dataHeatBal->ZoneITEq(Loop).AirVolFlowStdDensity;
            state.dataHeatBal->spaceRpt(spaceNum).ITEqAirMassFlow += state.dataHeatBal->ZoneITEq(Loop).AirMassFlow;
            state.dataHeatBal->spaceRpt(spaceNum).SumTinMinusTSup += (TAirIn - TSupply) * AirVolFlowRate;
            state.dataHeatBal->spaceRpt(spaceNum).SumToutMinusTSup += (TAirOut - TSupply) * AirVolFlowRate;

            // Check environmental class operating range limits (defined as parameters in this subroutine)
            EnvClass = state.dataHeatBal->ZoneITEq(Loop).Class;
            if (EnvClass > 0) {
                if (TAirIn > DBMax(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeAboveDryBulbT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).DryBulbTAboveDeltaT = TAirIn - DBMax(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeAboveDryBulbT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeAboveDryBulbT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
                if (TAirIn < DBMin(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeBelowDryBulbT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).DryBulbTBelowDeltaT = TAirIn - DBMin(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeBelowDryBulbT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeBelowDryBulbT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
                if (TDPAirIn > DPMax(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeAboveDewpointT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).DewpointTAboveDeltaT = TDPAirIn - DPMax(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeAboveDewpointT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeAboveDewpointT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
                if (TDPAirIn < DPMin(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeBelowDewpointT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).DewpointTBelowDeltaT = TDPAirIn - DPMin(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeBelowDewpointT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeBelowDewpointT = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
                if (RHAirIn > RHMax(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeAboveRH = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).RHAboveDeltaRH = RHAirIn - RHMax(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeAboveRH = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeAboveRH = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
                if (RHAirIn < RHMin(EnvClass)) {
                    state.dataHeatBal->ZoneITEq(Loop).TimeBelowRH = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).TimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZoneITEq(Loop).RHBelowDeltaRH = RHAirIn - RHMin(EnvClass);
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeBelowRH = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->ZnRpt(NZ).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeBelowRH = state.dataGlobal->TimeStepZone;
                    state.dataHeatBal->spaceRpt(spaceNum).ITEqTimeOutOfOperRange = state.dataGlobal->TimeStepZone;
                }
            }

        } // ZoneITEq calc loop

        // Zone and space-level sensible heat index
        for (Loop = 1; Loop <= state.dataHeatBal->TotITEquip; ++Loop) {
            int ZN = state.dataHeatBal->ZoneITEq(Loop).ZonePtr;
            int spaceNum = state.dataHeatBal->ZoneITEq(Loop).spaceIndex;
            if (state.dataHeatBal->ZnRpt(ZN).SumToutMinusTSup != 0.0) {
                state.dataHeatBal->ZnRpt(ZN).ITEqSHI = state.dataHeatBal->ZnRpt(ZN).SumTinMinusTSup / state.dataHeatBal->ZnRpt(ZN).SumToutMinusTSup;
            }
            if (state.dataHeatBal->spaceRpt(spaceNum).SumToutMinusTSup != 0.0) {
                state.dataHeatBal->spaceRpt(spaceNum).ITEqSHI =
                    state.dataHeatBal->spaceRpt(spaceNum).SumTinMinusTSup / state.dataHeatBal->spaceRpt(spaceNum).SumToutMinusTSup;
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
        static const Array1D_int TradIntGainTypes(8,
                                                  {IntGainTypeOf_People,
                                                   IntGainTypeOf_Lights,
                                                   IntGainTypeOf_ElectricEquipment,
                                                   IntGainTypeOf_ElectricEquipmentITEAirCooled,
                                                   IntGainTypeOf_GasEquipment,
                                                   IntGainTypeOf_HotWaterEquipment,
                                                   IntGainTypeOf_SteamEquipment,
                                                   IntGainTypeOf_OtherEquipment});

        for (int Loop = 1; Loop <= state.dataHeatBal->TotPeople; ++Loop) {
            state.dataHeatBal->People(Loop).RadGainEnergy = state.dataHeatBal->People(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->People(Loop).ConGainEnergy = state.dataHeatBal->People(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->People(Loop).SenGainEnergy = state.dataHeatBal->People(Loop).SenGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->People(Loop).LatGainEnergy = state.dataHeatBal->People(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->People(Loop).TotGainEnergy = state.dataHeatBal->People(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotLights; ++Loop) {
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

        for (int Loop = 1; Loop <= state.dataHeatBal->TotElecEquip; ++Loop) {
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

        for (int Loop = 1; Loop <= state.dataHeatBal->TotGasEquip; ++Loop) {
            state.dataHeatBal->ZoneGas(Loop).Consumption = state.dataHeatBal->ZoneGas(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneGas(Loop).RadGainEnergy = state.dataHeatBal->ZoneGas(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneGas(Loop).ConGainEnergy = state.dataHeatBal->ZoneGas(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneGas(Loop).LatGainEnergy = state.dataHeatBal->ZoneGas(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneGas(Loop).LostEnergy = state.dataHeatBal->ZoneGas(Loop).LostRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneGas(Loop).TotGainEnergy = state.dataHeatBal->ZoneGas(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotOthEquip; ++Loop) {
            state.dataHeatBal->ZoneOtherEq(Loop).Consumption = state.dataHeatBal->ZoneOtherEq(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOtherEq(Loop).RadGainEnergy = state.dataHeatBal->ZoneOtherEq(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOtherEq(Loop).ConGainEnergy = state.dataHeatBal->ZoneOtherEq(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOtherEq(Loop).LatGainEnergy = state.dataHeatBal->ZoneOtherEq(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOtherEq(Loop).LostEnergy = state.dataHeatBal->ZoneOtherEq(Loop).LostRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneOtherEq(Loop).TotGainEnergy = state.dataHeatBal->ZoneOtherEq(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotHWEquip; ++Loop) {
            state.dataHeatBal->ZoneHWEq(Loop).Consumption = state.dataHeatBal->ZoneHWEq(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneHWEq(Loop).RadGainEnergy = state.dataHeatBal->ZoneHWEq(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneHWEq(Loop).ConGainEnergy = state.dataHeatBal->ZoneHWEq(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneHWEq(Loop).LatGainEnergy = state.dataHeatBal->ZoneHWEq(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneHWEq(Loop).LostEnergy = state.dataHeatBal->ZoneHWEq(Loop).LostRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneHWEq(Loop).TotGainEnergy = state.dataHeatBal->ZoneHWEq(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotStmEquip; ++Loop) {
            state.dataHeatBal->ZoneSteamEq(Loop).Consumption = state.dataHeatBal->ZoneSteamEq(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneSteamEq(Loop).RadGainEnergy = state.dataHeatBal->ZoneSteamEq(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneSteamEq(Loop).ConGainEnergy = state.dataHeatBal->ZoneSteamEq(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneSteamEq(Loop).LatGainEnergy = state.dataHeatBal->ZoneSteamEq(Loop).LatGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneSteamEq(Loop).LostEnergy = state.dataHeatBal->ZoneSteamEq(Loop).LostRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneSteamEq(Loop).TotGainEnergy = state.dataHeatBal->ZoneSteamEq(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (int Loop = 1; Loop <= state.dataHeatBal->TotBBHeat; ++Loop) {
            state.dataHeatBal->ZoneBBHeat(Loop).Consumption = state.dataHeatBal->ZoneBBHeat(Loop).Power * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneBBHeat(Loop).RadGainEnergy = state.dataHeatBal->ZoneBBHeat(Loop).RadGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneBBHeat(Loop).ConGainEnergy = state.dataHeatBal->ZoneBBHeat(Loop).ConGainRate * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->ZoneBBHeat(Loop).TotGainEnergy = state.dataHeatBal->ZoneBBHeat(Loop).TotGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            // People
            state.dataHeatBal->spaceRpt(spaceNum).PeopleNumOcc = state.dataHeatBal->spaceIntGain(spaceNum).NOFOCC;
            state.dataHeatBal->spaceRpt(spaceNum).PeopleRadGain =
                state.dataHeatBal->spaceIntGain(spaceNum).QOCRAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).PeopleConGain =
                state.dataHeatBal->spaceIntGain(spaceNum).QOCCON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).PeopleSenGain =
                state.dataHeatBal->spaceIntGain(spaceNum).QOCSEN * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).PeopleLatGain =
                state.dataHeatBal->spaceIntGain(spaceNum).QOCLAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).PeopleTotGain =
                state.dataHeatBal->spaceIntGain(spaceNum).QOCTOT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).PeopleRadGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QOCRAD;
            state.dataHeatBal->spaceRpt(spaceNum).PeopleConGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QOCCON;
            state.dataHeatBal->spaceRpt(spaceNum).PeopleSenGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QOCSEN;
            state.dataHeatBal->spaceRpt(spaceNum).PeopleLatGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QOCLAT;
            state.dataHeatBal->spaceRpt(spaceNum).PeopleTotGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QOCTOT;

            // General Lights
            state.dataHeatBal->spaceRpt(spaceNum).LtsRetAirGain =
                state.dataHeatBal->spaceIntGain(spaceNum).QLTCRA * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).LtsRadGain = state.dataHeatBal->spaceIntGain(spaceNum).QLTRAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).LtsTotGain = state.dataHeatBal->spaceIntGain(spaceNum).QLTTOT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).LtsConGain = state.dataHeatBal->spaceIntGain(spaceNum).QLTCON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).LtsVisGain = state.dataHeatBal->spaceIntGain(spaceNum).QLTSW * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).LtsRetAirGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QLTCRA;
            state.dataHeatBal->spaceRpt(spaceNum).LtsRadGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QLTRAD;
            state.dataHeatBal->spaceRpt(spaceNum).LtsTotGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QLTTOT;
            state.dataHeatBal->spaceRpt(spaceNum).LtsConGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QLTCON;
            state.dataHeatBal->spaceRpt(spaceNum).LtsVisGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QLTSW;
            state.dataHeatBal->spaceRpt(spaceNum).LtsElecConsump = state.dataHeatBal->spaceRpt(spaceNum).LtsTotGain;

            // Electric Equipment
            state.dataHeatBal->spaceRpt(spaceNum).ElecConGain = state.dataHeatBal->spaceIntGain(spaceNum).QEECON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).ElecRadGain = state.dataHeatBal->spaceIntGain(spaceNum).QEERAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).ElecLatGain = state.dataHeatBal->spaceIntGain(spaceNum).QEELAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).ElecLost = state.dataHeatBal->spaceIntGain(spaceNum).QEELost * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).ElecConGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QEECON;
            state.dataHeatBal->spaceRpt(spaceNum).ElecRadGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QEERAD;
            state.dataHeatBal->spaceRpt(spaceNum).ElecLatGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QEELAT;
            state.dataHeatBal->spaceRpt(spaceNum).ElecLostRate = state.dataHeatBal->spaceIntGain(spaceNum).QEELost;
            state.dataHeatBal->spaceRpt(spaceNum).ElecConsump =
                state.dataHeatBal->spaceRpt(spaceNum).ElecConGain + state.dataHeatBal->spaceRpt(spaceNum).ElecRadGain +
                state.dataHeatBal->spaceRpt(spaceNum).ElecLatGain + state.dataHeatBal->spaceRpt(spaceNum).ElecLost;
            state.dataHeatBal->spaceRpt(spaceNum).ElecTotGain = state.dataHeatBal->spaceRpt(spaceNum).ElecConGain +
                                                                state.dataHeatBal->spaceRpt(spaceNum).ElecRadGain +
                                                                state.dataHeatBal->spaceRpt(spaceNum).ElecLatGain;
            state.dataHeatBal->spaceRpt(spaceNum).ElecTotGainRate = state.dataHeatBal->spaceRpt(spaceNum).ElecConGainRate +
                                                                    state.dataHeatBal->spaceRpt(spaceNum).ElecRadGainRate +
                                                                    state.dataHeatBal->spaceRpt(spaceNum).ElecLatGainRate;

            // Gas Equipment
            state.dataHeatBal->spaceRpt(spaceNum).GasConGain = state.dataHeatBal->spaceIntGain(spaceNum).QGECON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).GasRadGain = state.dataHeatBal->spaceIntGain(spaceNum).QGERAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).GasLatGain = state.dataHeatBal->spaceIntGain(spaceNum).QGELAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).GasLost = state.dataHeatBal->spaceIntGain(spaceNum).QGELost * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).GasConGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QGECON;
            state.dataHeatBal->spaceRpt(spaceNum).GasRadGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QGERAD;
            state.dataHeatBal->spaceRpt(spaceNum).GasLatGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QGELAT;
            state.dataHeatBal->spaceRpt(spaceNum).GasLostRate = state.dataHeatBal->spaceIntGain(spaceNum).QGELost;
            state.dataHeatBal->spaceRpt(spaceNum).GasConsump =
                state.dataHeatBal->spaceRpt(spaceNum).GasConGain + state.dataHeatBal->spaceRpt(spaceNum).GasRadGain +
                state.dataHeatBal->spaceRpt(spaceNum).GasLatGain + state.dataHeatBal->spaceRpt(spaceNum).GasLost;
            state.dataHeatBal->spaceRpt(spaceNum).GasTotGain = state.dataHeatBal->spaceRpt(spaceNum).GasConGain +
                                                               state.dataHeatBal->spaceRpt(spaceNum).GasRadGain +
                                                               state.dataHeatBal->spaceRpt(spaceNum).GasLatGain;
            state.dataHeatBal->spaceRpt(spaceNum).GasTotGainRate = state.dataHeatBal->spaceRpt(spaceNum).GasConGainRate +
                                                                   state.dataHeatBal->spaceRpt(spaceNum).GasRadGainRate +
                                                                   state.dataHeatBal->spaceRpt(spaceNum).GasLatGainRate;

            // Hot Water Equipment
            state.dataHeatBal->spaceRpt(spaceNum).HWConGain = state.dataHeatBal->spaceIntGain(spaceNum).QHWCON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).HWRadGain = state.dataHeatBal->spaceIntGain(spaceNum).QHWRAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).HWLatGain = state.dataHeatBal->spaceIntGain(spaceNum).QHWLAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).HWLost = state.dataHeatBal->spaceIntGain(spaceNum).QHWLost * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).HWConGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QHWCON;
            state.dataHeatBal->spaceRpt(spaceNum).HWRadGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QHWRAD;
            state.dataHeatBal->spaceRpt(spaceNum).HWLatGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QHWLAT;
            state.dataHeatBal->spaceRpt(spaceNum).HWLostRate = state.dataHeatBal->spaceIntGain(spaceNum).QHWLost;
            state.dataHeatBal->spaceRpt(spaceNum).HWConsump =
                state.dataHeatBal->spaceRpt(spaceNum).HWConGain + state.dataHeatBal->spaceRpt(spaceNum).HWRadGain +
                state.dataHeatBal->spaceRpt(spaceNum).HWLatGain + state.dataHeatBal->spaceRpt(spaceNum).HWLost;
            state.dataHeatBal->spaceRpt(spaceNum).HWTotGain = state.dataHeatBal->spaceRpt(spaceNum).HWConGain +
                                                              state.dataHeatBal->spaceRpt(spaceNum).HWRadGain +
                                                              state.dataHeatBal->spaceRpt(spaceNum).HWLatGain;
            state.dataHeatBal->spaceRpt(spaceNum).HWTotGainRate = state.dataHeatBal->spaceRpt(spaceNum).HWConGainRate +
                                                                  state.dataHeatBal->spaceRpt(spaceNum).HWRadGainRate +
                                                                  state.dataHeatBal->spaceRpt(spaceNum).HWLatGainRate;

            // Steam Equipment
            state.dataHeatBal->spaceRpt(spaceNum).SteamConGain = state.dataHeatBal->spaceIntGain(spaceNum).QSECON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).SteamRadGain = state.dataHeatBal->spaceIntGain(spaceNum).QSERAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).SteamLatGain = state.dataHeatBal->spaceIntGain(spaceNum).QSELAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).SteamLost = state.dataHeatBal->spaceIntGain(spaceNum).QSELost * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).SteamConGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QSECON;
            state.dataHeatBal->spaceRpt(spaceNum).SteamRadGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QSERAD;
            state.dataHeatBal->spaceRpt(spaceNum).SteamLatGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QSELAT;
            state.dataHeatBal->spaceRpt(spaceNum).SteamLostRate = state.dataHeatBal->spaceIntGain(spaceNum).QSELost;
            state.dataHeatBal->spaceRpt(spaceNum).SteamConsump =
                state.dataHeatBal->spaceRpt(spaceNum).SteamConGain + state.dataHeatBal->spaceRpt(spaceNum).SteamRadGain +
                state.dataHeatBal->spaceRpt(spaceNum).SteamLatGain + state.dataHeatBal->spaceRpt(spaceNum).SteamLost;
            state.dataHeatBal->spaceRpt(spaceNum).SteamTotGain = state.dataHeatBal->spaceRpt(spaceNum).SteamConGain +
                                                                 state.dataHeatBal->spaceRpt(spaceNum).SteamRadGain +
                                                                 state.dataHeatBal->spaceRpt(spaceNum).SteamLatGain;
            state.dataHeatBal->spaceRpt(spaceNum).SteamTotGainRate = state.dataHeatBal->spaceRpt(spaceNum).SteamConGainRate +
                                                                     state.dataHeatBal->spaceRpt(spaceNum).SteamRadGainRate +
                                                                     state.dataHeatBal->spaceRpt(spaceNum).SteamLatGainRate;

            // Other Equipment
            state.dataHeatBal->spaceRpt(spaceNum).OtherConGain = state.dataHeatBal->spaceIntGain(spaceNum).QOECON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).OtherRadGain = state.dataHeatBal->spaceIntGain(spaceNum).QOERAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).OtherLatGain = state.dataHeatBal->spaceIntGain(spaceNum).QOELAT * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).OtherLost = state.dataHeatBal->spaceIntGain(spaceNum).QOELost * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).OtherConGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QOECON;
            state.dataHeatBal->spaceRpt(spaceNum).OtherRadGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QOERAD;
            state.dataHeatBal->spaceRpt(spaceNum).OtherLatGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QOELAT;
            state.dataHeatBal->spaceRpt(spaceNum).OtherLostRate = state.dataHeatBal->spaceIntGain(spaceNum).QOELost;
            state.dataHeatBal->spaceRpt(spaceNum).OtherConsump =
                state.dataHeatBal->spaceRpt(spaceNum).OtherConGain + state.dataHeatBal->spaceRpt(spaceNum).OtherRadGain +
                state.dataHeatBal->spaceRpt(spaceNum).OtherLatGain + state.dataHeatBal->spaceRpt(spaceNum).OtherLost;
            state.dataHeatBal->spaceRpt(spaceNum).OtherTotGain = state.dataHeatBal->spaceRpt(spaceNum).OtherConGain +
                                                                 state.dataHeatBal->spaceRpt(spaceNum).OtherRadGain +
                                                                 state.dataHeatBal->spaceRpt(spaceNum).OtherLatGain;
            state.dataHeatBal->spaceRpt(spaceNum).OtherTotGainRate = state.dataHeatBal->spaceRpt(spaceNum).OtherConGainRate +
                                                                     state.dataHeatBal->spaceRpt(spaceNum).OtherRadGainRate +
                                                                     state.dataHeatBal->spaceRpt(spaceNum).OtherLatGainRate;

            // Baseboard Heat
            state.dataHeatBal->spaceRpt(spaceNum).BaseHeatConGain =
                state.dataHeatBal->spaceIntGain(spaceNum).QBBCON * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).BaseHeatRadGain =
                state.dataHeatBal->spaceIntGain(spaceNum).QBBRAD * state.dataGlobal->TimeStepZoneSec;
            state.dataHeatBal->spaceRpt(spaceNum).BaseHeatConGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QBBCON;
            state.dataHeatBal->spaceRpt(spaceNum).BaseHeatRadGainRate = state.dataHeatBal->spaceIntGain(spaceNum).QBBRAD;
            state.dataHeatBal->spaceRpt(spaceNum).BaseHeatTotGain =
                state.dataHeatBal->spaceRpt(spaceNum).BaseHeatConGain + state.dataHeatBal->spaceRpt(spaceNum).BaseHeatRadGain;
            state.dataHeatBal->spaceRpt(spaceNum).BaseHeatTotGainRate =
                state.dataHeatBal->spaceRpt(spaceNum).BaseHeatConGainRate + state.dataHeatBal->spaceRpt(spaceNum).BaseHeatRadGainRate;
            state.dataHeatBal->spaceRpt(spaceNum).BaseHeatElecCons = state.dataHeatBal->spaceRpt(spaceNum).BaseHeatTotGain;

            // Overall Space Variables

            // these overalls include component gains from devices like water heater, water use, and generators
            //   working vars QFCConv QGenConv QFCRad QGenRad  WaterUseLatentGain WaterThermalTankGain WaterUseSensibleGain

            state.dataHeatBal->spaceRpt(spaceNum).TotVisHeatGain = state.dataHeatBal->spaceRpt(spaceNum).LtsVisGain;
            state.dataHeatBal->spaceRpt(spaceNum).TotVisHeatGainRate = state.dataHeatBal->spaceRpt(spaceNum).LtsVisGainRate;

            int zoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
            SumInternalRadiationGainsByTypes(state, zoneNum, TradIntGainTypes, state.dataHeatBal->spaceRpt(spaceNum).TotRadiantGainRate, spaceNum);
            state.dataHeatBal->spaceRpt(spaceNum).TotRadiantGain =
                state.dataHeatBal->spaceRpt(spaceNum).TotRadiantGainRate * state.dataGlobal->TimeStepZoneSec;

            SumInternalConvectionGainsByTypes(
                state, zoneNum, TradIntGainTypes, state.dataHeatBal->spaceRpt(spaceNum).TotConvectiveGainRate, spaceNum);
            state.dataHeatBal->spaceRpt(spaceNum).TotConvectiveGain =
                state.dataHeatBal->spaceRpt(spaceNum).TotConvectiveGainRate * state.dataGlobal->TimeStepZoneSec;

            SumInternalLatentGainsByTypes(state, zoneNum, TradIntGainTypes, state.dataHeatBal->spaceRpt(spaceNum).TotLatentGainRate, spaceNum);
            state.dataHeatBal->spaceRpt(spaceNum).TotLatentGain =
                state.dataHeatBal->spaceRpt(spaceNum).TotLatentGainRate * state.dataGlobal->TimeStepZoneSec;

            state.dataHeatBal->spaceRpt(spaceNum).TotTotalHeatGainRate =
                state.dataHeatBal->spaceRpt(spaceNum).TotLatentGainRate + state.dataHeatBal->spaceRpt(spaceNum).TotRadiantGainRate +
                state.dataHeatBal->spaceRpt(spaceNum).TotConvectiveGainRate + state.dataHeatBal->spaceRpt(spaceNum).TotVisHeatGainRate;
            state.dataHeatBal->spaceRpt(spaceNum).TotTotalHeatGain =
                state.dataHeatBal->spaceRpt(spaceNum).TotTotalHeatGainRate * state.dataGlobal->TimeStepZoneSec;
        }

        for (int ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) {
            // People
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleNumOcc = 0.0;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleRadGain = 0.0;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleConGain = 0.0;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleSenGain = 0.0;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleLatGain = 0.0;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleTotGain = 0.0;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleRadGainRate = 0.0;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleConGainRate = 0.0;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleSenGainRate = 0.0;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleLatGainRate = 0.0;
            state.dataHeatBal->ZnRpt(ZoneLoop).PeopleTotGainRate = 0.0;

            for (int spaceNum : state.dataHeatBal->Zone(ZoneLoop).spaceIndexes) {
                // People
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleNumOcc += state.dataHeatBal->spaceRpt(spaceNum).PeopleNumOcc;
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleRadGain += state.dataHeatBal->spaceRpt(spaceNum).PeopleRadGain;
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleConGain += state.dataHeatBal->spaceRpt(spaceNum).PeopleConGain;
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleSenGain += state.dataHeatBal->spaceRpt(spaceNum).PeopleSenGain;
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleLatGain += state.dataHeatBal->spaceRpt(spaceNum).PeopleLatGain;
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleTotGain += state.dataHeatBal->spaceRpt(spaceNum).PeopleTotGain;
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleRadGainRate += state.dataHeatBal->spaceRpt(spaceNum).PeopleRadGainRate;
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleConGainRate += state.dataHeatBal->spaceRpt(spaceNum).PeopleConGainRate;
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleSenGainRate += state.dataHeatBal->spaceRpt(spaceNum).PeopleSenGainRate;
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleLatGainRate += state.dataHeatBal->spaceRpt(spaceNum).PeopleLatGainRate;
                state.dataHeatBal->ZnRpt(ZoneLoop).PeopleTotGainRate += state.dataHeatBal->spaceRpt(spaceNum).PeopleTotGainRate;
            }
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
            if ((state.dataDaylightingData->ZoneDaylight(WhichZone).totRefPts > 0) &&
                (state.dataHeatBal->Lights(Loop).FractionReplaceable > 0.0 && state.dataHeatBal->Lights(Loop).FractionReplaceable < 1.0)) {
                ShowWarningError(state, "CheckLightsReplaceableMinMaxForZone: Fraction Replaceable must be 0.0 or 1.0 if used with daylighting.");
                ShowContinueError(state,
                                  "..Lights=\"" + state.dataHeatBal->Lights(Loop).Name +
                                      "\", Fraction Replaceable will be reset to 1.0 to allow dimming controls");
                ShowContinueError(state, "..in Zone=" + state.dataHeatBal->Zone(WhichZone).Name);
                state.dataHeatBal->Lights(Loop).FractionReplaceable = 1.0;
            }
        }

        if (state.dataDaylightingData->ZoneDaylight(WhichZone).totRefPts > 0) {
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
        bool DoRadiationUpdate = true;
        bool ReSumLatentGains = false;

        if (present(SuppressRadiationUpdate)) {
            if (SuppressRadiationUpdate) DoRadiationUpdate = false;
        }

        if (present(SumLatentGains)) {
            if (SumLatentGains) ReSumLatentGains = true;
        }

        // store pointer values to hold generic internal gain values constant for entire timestep
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            auto &thisIntGain = state.dataHeatBal->spaceIntGainDevices(spaceNum);
            for (int Loop = 1; Loop <= thisIntGain.numberOfDevices; ++Loop) {
                thisIntGain.device(Loop).ConvectGainRate = *thisIntGain.device(Loop).PtrConvectGainRate * thisIntGain.device(Loop).spaceGainFrac;
                thisIntGain.device(Loop).ReturnAirConvGainRate =
                    *thisIntGain.device(Loop).PtrReturnAirConvGainRate * thisIntGain.device(Loop).spaceGainFrac;
                if (DoRadiationUpdate)
                    thisIntGain.device(Loop).RadiantGainRate = *thisIntGain.device(Loop).PtrRadiantGainRate * thisIntGain.device(Loop).spaceGainFrac;
                thisIntGain.device(Loop).LatentGainRate = *thisIntGain.device(Loop).PtrLatentGainRate * thisIntGain.device(Loop).spaceGainFrac;
                thisIntGain.device(Loop).ReturnAirLatentGainRate =
                    *thisIntGain.device(Loop).PtrReturnAirLatentGainRate * thisIntGain.device(Loop).spaceGainFrac;
                thisIntGain.device(Loop).CarbonDioxideGainRate =
                    *thisIntGain.device(Loop).PtrCarbonDioxideGainRate * thisIntGain.device(Loop).spaceGainFrac;
                thisIntGain.device(Loop).GenericContamGainRate =
                    *thisIntGain.device(Loop).PtrGenericContamGainRate * thisIntGain.device(Loop).spaceGainFrac;
            }
        }
        if (ReSumLatentGains) {
            for (int NZ = 1; NZ <= state.dataGlobal->NumOfZones; ++NZ) {
                InternalHeatGains::SumAllInternalLatentGains(state, NZ, state.dataHeatBalFanSys->ZoneLatentGain(NZ));
                // Added for the hybrid model
                if (state.dataHybridModel->FlagHybridModel_PC) {
                    InternalHeatGains::SumAllInternalLatentGainsExceptPeople(state, NZ, state.dataHeatBalFanSys->ZoneLatentGainExceptPeople(NZ));
                }
            }
        }

        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation && allocated(state.dataContaminantBalance->ZoneGCGain)) {
            for (int NZ = 1; NZ <= state.dataGlobal->NumOfZones; ++NZ) {
                InternalHeatGains::SumAllInternalGenericContamGains(state, NZ, state.dataContaminantBalance->ZoneGCGain(NZ));
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

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        Real64 tmpSumConvGainRate = 0.0;

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                tmpSumConvGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).ConvectGainRate;
            }
        }

        SumConvGainRate = tmpSumConvGainRate;
    }

    // For HybridModel
    void SumAllInternalConvectionGainsExceptPeople(EnergyPlusData &state, int const ZoneNum, Real64 &SumConvGainRateExceptPeople)
    {
        Real64 tmpSumConvGainRateExceptPeople = 0.0;
        std::string str_people = "PEOPLE";

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                if (state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CompObjectType != str_people) {
                    tmpSumConvGainRateExceptPeople += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).ConvectGainRate;
                }
            }
        }

        SumConvGainRateExceptPeople = tmpSumConvGainRateExceptPeople;
    }

    void SumInternalConvectionGainsByTypes(EnergyPlusData &state,
                                           int const ZoneNum,              // zone index pointer for which zone to sum gains for
                                           const Array1D_int &GainTypeARR, // variable length 1-d array of integer valued gain types
                                           Real64 &SumConvGainRate,
                                           int const spaceIndex) // space index pointer, sum gains only for this space
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Nov. 2011cl

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gain types

        int NumberOfTypes = size(GainTypeARR);
        Real64 tmpSumConvGainRate = 0.0;

        // TODO MJW: This could be refactored to avoid duplicate code, but for now . . . .
        if (spaceIndex > 0) {
            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceIndex).numberOfDevices; ++DeviceNum) {
                for (int TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {
                    if (state.dataHeatBal->spaceIntGainDevices(spaceIndex).device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                        tmpSumConvGainRate += state.dataHeatBal->spaceIntGainDevices(spaceIndex).device(DeviceNum).ConvectGainRate;
                    }
                }
            }
        } else {
            for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                    continue;
                }
                for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                    for (int TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {
                        if (state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                            tmpSumConvGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).ConvectGainRate;
                        }
                    }
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

        Real64 tmpSumRetAirGainRate = 0.0;

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                // If ReturnNodeNum is zero, sum for entire zone, otherwise sum only for specified ReturnNodeNum
                if ((ReturnNodeNum == 0) || (ReturnNodeNum == state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).ReturnAirNodeNum)) {
                    tmpSumRetAirGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).ReturnAirConvGainRate;
                }
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

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gain types

        int NumberOfTypes = size(GainTypeARR);
        Real64 tmpSumRetAirConvGainRate = 0.0;

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                for (int TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {

                    if (state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                        tmpSumRetAirConvGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).ReturnAirConvGainRate;
                    }
                }
            }
        }

        SumReturnAirGainRate = tmpSumRetAirConvGainRate;
    }

    void SumAllSpaceInternalRadiationGains(EnergyPlusData &state,
                                           int const spaceNum, // space index pointer for which space to sum gains for
                                           Real64 &sumRadGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Nov. 2011

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        Real64 tmpSumRadGainRate = 0.0;

        if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
            sumRadGainRate = 0.0;
            return;
        }

        for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
            tmpSumRadGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).RadiantGainRate;
        }

        sumRadGainRate = tmpSumRadGainRate;
    }

    void SumInternalRadiationGainsByTypes(EnergyPlusData &state,
                                          int const ZoneNum,              // zone index pointer for which zone to sum gains for
                                          const Array1D_int &GainTypeARR, // variable length 1-d array of integer valued gain types
                                          Real64 &SumRadiationGainRate,
                                          int const spaceIndex) // space index pointer, sum gains only for this space
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec. 2011

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gain types

        int NumberOfTypes = size(GainTypeARR);
        Real64 tmpSumRadiationGainRate = 0.0;

        // TODO MJW: This could be refactored to avoid duplicate code, but for now . . . .
        if (spaceIndex > 0) {
            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceIndex).numberOfDevices; ++DeviceNum) {
                for (int TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {
                    if (state.dataHeatBal->spaceIntGainDevices(spaceIndex).device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                        tmpSumRadiationGainRate += state.dataHeatBal->spaceIntGainDevices(spaceIndex).device(DeviceNum).RadiantGainRate;
                    }
                }
            }
        } else {
            for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                    continue;
                }
                for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                    for (int TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {
                        if (state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                            tmpSumRadiationGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).RadiantGainRate;
                        }
                    }
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

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        Real64 tmpSumLatentGainRate = 0.0;

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                tmpSumLatentGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).LatentGainRate;
            }

            SumLatentGainRate = tmpSumLatentGainRate;
        }
    }

    // Added for hybrid model -- calculate the latent gain from all sources except for people
    void SumAllInternalLatentGainsExceptPeople(EnergyPlusData &state,
                                               int const ZoneNum, // zone index pointer for which zone to sum gains for
                                               Real64 &SumLatentGainRateExceptPeople)
    {
        Real64 tmpSumLatentGainRateExceptPeople = 0.0;

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                if (state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CompTypeOfNum != IntGainTypeOf_People) {
                    tmpSumLatentGainRateExceptPeople += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).LatentGainRate;
                }
            }
        }

        SumLatentGainRateExceptPeople = tmpSumLatentGainRateExceptPeople;
    }

    void SumInternalLatentGainsByTypes(EnergyPlusData &state,
                                       int const ZoneNum,              // zone index pointer for which zone to sum gains for
                                       const Array1D_int &GainTypeARR, // variable length 1-d array of integer valued gain types
                                       Real64 &SumLatentGainRate,
                                       int const spaceIndex) // space index pointer, sum gains only for this space
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec. 2011

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gain types

        int NumberOfTypes = size(GainTypeARR);
        Real64 tmpSumLatentGainRate = 0.0;

        // TODO MJW: This could be refactored to avoid duplicate code, but for now . . . .
        if (spaceIndex > 0) {
            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceIndex).numberOfDevices; ++DeviceNum) {
                for (int TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {
                    if (state.dataHeatBal->spaceIntGainDevices(spaceIndex).device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                        tmpSumLatentGainRate += state.dataHeatBal->spaceIntGainDevices(spaceIndex).device(DeviceNum).LatentGainRate;
                    }
                }
            }
        } else {
            for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                    continue;
                }
                for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                    for (int TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {
                        if (state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                            tmpSumLatentGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).LatentGainRate;
                        }
                    }
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

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        Real64 tmpSumLatentGainRate = 0.0;

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                // If ReturnNodeNum is zero, sum for entire zone, otherwise sum only for specified ReturnNodeNum
                if ((ReturnNodeNum == 0) || (ReturnNodeNum == state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).ReturnAirNodeNum)) {
                    tmpSumLatentGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).ReturnAirLatentGainRate;
                }
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

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types

        Real64 tmpSumCO2GainRate = 0.0;

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                tmpSumCO2GainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CarbonDioxideGainRate;
            }
        }

        SumCO2GainRate = tmpSumCO2GainRate;
    }

    // Added for hybrid model -- function for calculating CO2 gains except people
    void SumAllInternalCO2GainsExceptPeople(EnergyPlusData &state,
                                            int const ZoneNum, // zone index pointer for which zone to sum gains for
                                            Real64 &SumCO2GainRateExceptPeople)
    {
        Real64 tmpSumCO2GainRateExceptPeople = 0.0;

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                if (state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CompTypeOfNum != IntGainTypeOf_People) {
                    tmpSumCO2GainRateExceptPeople += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CarbonDioxideGainRate;
                }
            }
        }

        SumCO2GainRateExceptPeople = tmpSumCO2GainRateExceptPeople;
    }

    void SumInternalCO2GainsByTypes(EnergyPlusData &state,
                                    int const ZoneNum,              // zone index pointer for which zone to sum gains for
                                    const Array1D_int &GainTypeARR, // variable length 1-d array of integer valued gain types
                                    Real64 &SumCO2GainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec. 2011

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gain types

        int NumberOfTypes = size(GainTypeARR);
        Real64 tmpSumCO2GainRate = 0.0;

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                for (int TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum) {

                    if (state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CompTypeOfNum == GainTypeARR(TypeNum)) {
                        tmpSumCO2GainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CarbonDioxideGainRate;
                    }
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

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing all the internal gain types based on the existing subrotine SumAllInternalCO2Gains

        Real64 tmpSumGCGainRate = 0.0;

        for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
            if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
                continue;
            }

            for (int DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
                tmpSumGCGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).GenericContamGainRate;
            }
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
                                   int const spaceNum,                  // space index pointer for which space to sum gains for
                                   int const intGainTypeOfNum,          // space internal gain type number
                                   std::string_view const &intGainName) // Internal gain name
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2012

        // PURPOSE OF THIS SUBROUTINE:
        // utility to retrieve index pointer to a specific internal gain
        // the subroutine returns the index of matched internal gain device or -1 if no match found.

        int DeviceNum;
        int DeviceIndex;
        if (state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices == 0) {
            DeviceIndex = -1;
            return DeviceIndex;
        }
        for (DeviceNum = 1; DeviceNum <= state.dataHeatBal->spaceIntGainDevices(spaceNum).numberOfDevices; ++DeviceNum) {
            if ((UtilityRoutines::SameString(state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CompObjectName,
                                             intGainName.data())) &&
                (state.dataHeatBal->spaceIntGainDevices(spaceNum).device(DeviceNum).CompTypeOfNum == intGainTypeOfNum)) {
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
        int const numGains,                 // number of device gains to sum
        const Array1D_int &deviceSpaceARR,  // variable length 1-d array of integer space index pointers to include in summation
        const Array1D_int &deviceIndexARR,  // variable length 1-d array of integer device index pointers to include in summation
        const Array1D<Real64> &fractionARR, // array of fractional multipliers to apply to devices
        Real64 &sumConvGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2012

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gains by index

        assert(numGains <= isize(deviceSpaceARR));
        assert(numGains <= isize(deviceIndexARR));
        assert(numGains <= isize(fractionARR));

        Real64 tmpSumConvGainRate = 0.0;

        for (int loop = 1; loop <= numGains; ++loop) {
            int spaceNum = deviceSpaceARR(loop);
            int deviceNum = deviceIndexARR(loop);
            Real64 deviceFraction = fractionARR(loop);
            tmpSumConvGainRate += state.dataHeatBal->spaceIntGainDevices(spaceNum).device(deviceNum).ConvectGainRate * deviceFraction;
        }
        sumConvGainRate = tmpSumConvGainRate;
    }

    void SumInternalLatentGainsByIndices(
        EnergyPlusData &state,
        int const numGains,                 // number of device gains to sum
        const Array1D_int &deviceSpaceARR,  // variable length 1-d array of integer space index pointers to include in summation
        const Array1D_int &deviceIndexARR,  // variable length 1-d array of integer device index pointers to include in summation
        const Array1D<Real64> &fractionARR, // array of fractional multipliers to apply to devices
        Real64 &sumLatentGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2012

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gains by index

        assert(numGains <= isize(deviceSpaceARR));
        assert(numGains <= isize(deviceIndexARR));
        assert(numGains <= isize(fractionARR));

        Real64 tmpSumLatentGainRate = 0.0;

        for (int loop = 1; loop <= numGains; ++loop) {
            int spaceNum = deviceSpaceARR(loop);
            int deviceNum = deviceIndexARR(loop);
            Real64 deviceFraction = fractionARR(loop);
            tmpSumLatentGainRate =
                tmpSumLatentGainRate + state.dataHeatBal->spaceIntGainDevices(spaceNum).device(deviceNum).LatentGainRate * deviceFraction;
        }
        sumLatentGainRate = tmpSumLatentGainRate;
    }

    void SumReturnAirConvectionGainsByIndices(
        EnergyPlusData &state,
        int const numGains,                 // number of device gains to sum
        const Array1D_int &deviceSpaceARR,  // variable length 1-d array of integer space index pointers to include in summation
        const Array1D_int &deviceIndexARR,  // variable length 1-d array of integer device index pointers to include in summation
        const Array1D<Real64> &fractionARR, // array of fractional multipliers to apply to devices
        Real64 &sumReturnAirGainRate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2012

        // PURPOSE OF THIS SUBROUTINE:
        // worker routine for summing a subset of the internal gains by index

        assert(numGains <= isize(deviceSpaceARR));
        assert(numGains <= isize(deviceIndexARR));
        assert(numGains <= isize(fractionARR));

        Real64 tmpSumReturnAirGainRate = 0.0;

        for (int loop = 1; loop <= numGains; ++loop) {
            int spaceNum = deviceSpaceARR(loop);
            int deviceNum = deviceIndexARR(loop);
            Real64 deviceFraction = fractionARR(loop);
            tmpSumReturnAirGainRate =
                tmpSumReturnAirGainRate + state.dataHeatBal->spaceIntGainDevices(spaceNum).device(deviceNum).ReturnAirConvGainRate * deviceFraction;
        }
        sumReturnAirGainRate = tmpSumReturnAirGainRate;
    }

} // namespace InternalHeatGains

} // namespace EnergyPlus
