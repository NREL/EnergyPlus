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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PackagedThermalStorageCoil.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterManager.hh>
#include <EnergyPlus/WaterThermalTanks.hh>

namespace EnergyPlus {

namespace PackagedThermalStorageCoil {

    // Module containing the routines dealing with the packaged thermal storage cooling

    // MODULE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // encapsulate the data and algorithms for modeling packaged thermals storage cooling coils

    // METHODOLOGY EMPLOYED:
    // <description>

    // REFERENCES:
    // na

    // OTHER NOTES:
    // na

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataLoopNode;
    using namespace DataGlobals;
    using namespace Psychrometrics;
    using DataEnvironment::CurMnDy;
    using DataEnvironment::EnvironmentName;
    using DataEnvironment::OutBaroPress;
    using DataEnvironment::OutDryBulbTemp;
    using DataEnvironment::OutHumRat;
    using DataEnvironment::OutWetBulbTemp;
    using DataEnvironment::StdBaroPress;
    using namespace CurveManager;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // control types
    int const ScheduledOpModes(1);   // control over TES modes is via local schedule
    int const EMSActuatedOpModes(2); // control over TES modes is via EMS

    // Control Modes
    int const OffMode(0);
    int const CoolingOnlyMode(1);
    int const CoolingAndChargeMode(2);
    int const CoolingAndDischargeMode(3);
    int const ChargeOnlyMode(4);
    int const DischargeOnlyMode(5);

    // storage media
    int const FluidBased(101);
    int const IceBased(102);
    // INTEGER, PARAMETER :: UserDefinedFluid = 103

    // Water Systems
    int const CondensateDiscarded(1001); // default mode where water is "lost"
    int const CondensateToTank(1002);    // collect coil condensate from air and store in water storage tank

    int const WaterSupplyFromMains(101);
    int const WaterSupplyFromTank(102);

    // Dehumidification control modes (DehumidControlMode)
    int const DehumidControl_CoolReheat(2);

    static std::string const BlankString;

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE DECLARATIONS:

    int NumTESCoils;
    Array1D_bool CheckEquipName;
    bool GetTESInputFlag(true);
    // SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

    // Object Data
    Array1D<PackagedTESCoolingCoilStruct> TESCoil;

    // Functions

    void SimTESCoil(std::string const &CompName, // name of the fan coil unit
                    int &CompIndex,
                    int const FanOpMode, // allows parent object to control fan mode
                    int &TESOpMode,
                    Optional<Real64 const> PartLoadRatio // part load ratio (for single speed cycling unit)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TESCoilNum;

        if (GetTESInputFlag) {
            GetTESCoilInput();
            GetTESInputFlag = false;
        }

        if (CompIndex == 0) {
            TESCoilNum = UtilityRoutines::FindItemInList(CompName, TESCoil);
            if (TESCoilNum == 0) {
                ShowFatalError("Thermal Energy Storage Cooling Coil not found=" + CompName);
            }
            CompIndex = TESCoilNum;
        } else {
            TESCoilNum = CompIndex;
            if (TESCoilNum > NumTESCoils || TESCoilNum < 1) {
                ShowFatalError("SimTESCoil: Invalid CompIndex passed=" + TrimSigDigits(TESCoilNum) +
                               ", Number of Thermal Energy Storage Cooling Coil Coils=" + TrimSigDigits(NumTESCoils) + ", Coil name=" + CompName);
            }
            if (CheckEquipName(TESCoilNum)) {
                if (!CompName.empty() && CompName != TESCoil(TESCoilNum).Name) {
                    ShowFatalError("SimTESCoil: Invalid CompIndex passed=" + TrimSigDigits(TESCoilNum) + ", Coil name=" + CompName +
                                   ", stored Coil Name for that index=" + TESCoil(TESCoilNum).Name);
                }
                CheckEquipName(TESCoilNum) = false;
            }
        }

        TESOpMode = 1;

        InitTESCoil(TESCoilNum);

        TESOpMode = TESCoil(TESCoilNum).CurControlMode;
        {
            auto const SELECT_CASE_var(TESOpMode);
            if (SELECT_CASE_var == OffMode) {
                CalcTESCoilOffMode(TESCoilNum);
            } else if (SELECT_CASE_var == CoolingOnlyMode) {
                CalcTESCoilCoolingOnlyMode(TESCoilNum, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == CoolingAndChargeMode) {
                CalcTESCoilCoolingAndChargeMode(TESCoilNum, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == CoolingAndDischargeMode) {
                CalcTESCoilCoolingAndDischargeMode(TESCoilNum, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == ChargeOnlyMode) {
                CalcTESCoilChargeOnlyMode(TESCoilNum);
            } else if (SELECT_CASE_var == DischargeOnlyMode) {
                CalcTESCoilDischargeOnlyMode(TESCoilNum, PartLoadRatio);
            }
        }
    }

    void GetTESCoilInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using BranchNodeConnections::TestCompSet;
        using DataHeatBalance::IntGainTypeOf_PackagedTESCoilTank;
        using DataZoneEquipment::FindControlledZoneIndexFromSystemNodeNumberForZone;
        using FluidProperties::CheckFluidPropertyName;
        using FluidProperties::FindGlycol;
        using FluidProperties::GetFluidDensityTemperatureLimits;
        using FluidProperties::GetFluidSpecificHeatTemperatureLimits;
        using GlobalNames::VerifyUniqueCoilName;
        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::CheckOutAirNodeNumber;
        using ScheduleManager::GetScheduleIndex;
        using WaterManager::SetupTankDemandComponent;
        using WaterManager::SetupTankSupplyComponent;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetTESCoilInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int item;                       // do loop counter
        int NumAlphas;                  // Number of alphas in input
        int NumNumbers;                 // Number of numeric items in input
        int IOStatus;                   // Input status returned from GetObjectItem
        static bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        Real64 TminRho;
        Real64 TmaxRho;
        Real64 TminCp;
        Real64 TmaxCp;
        int ZoneIndexTrial;

        cCurrentModuleObject = "Coil:Cooling:DX:SingleSpeed:ThermalStorage";
        NumTESCoils = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        TESCoil.allocate(NumTESCoils);
        CheckEquipName.dimension(NumTESCoils, true);

        for (item = 1; item <= NumTESCoils; ++item) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            VerifyUniqueCoilName(cCurrentModuleObject, cAlphaArgs(1), ErrorsFound, cCurrentModuleObject + " Name");

            TESCoil(item).Name = cAlphaArgs(1);
            if (lAlphaFieldBlanks(2)) {
                TESCoil(item).AvailSchedNum = ScheduleAlwaysOn;
            } else {
                TESCoil(item).AvailSchedNum = GetScheduleIndex(cAlphaArgs(2));
                if (TESCoil(item).AvailSchedNum == 0) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                }
            }
            {
                auto const SELECT_CASE_var(cAlphaArgs(3));
                if (SELECT_CASE_var == "SCHEDULEDMODES") {
                    TESCoil(item).ModeControlType = ScheduledOpModes;
                } else if (SELECT_CASE_var == "EMSCONTROLLED") {
                    TESCoil(item).ModeControlType = EMSActuatedOpModes;
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                    ShowContinueError("Available choices are ScheduledModes or EMSControlled");
                    ErrorsFound = true;
                }
            }
            if (lAlphaFieldBlanks(4)) {
                if (TESCoil(item).ModeControlType == ScheduledOpModes) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError(cAlphaFieldNames(4) + " is blank but a schedule is needed");
                    ErrorsFound = true;
                }
            } else {
                TESCoil(item).ControlModeSchedNum = GetScheduleIndex(cAlphaArgs(4));
                if (TESCoil(item).ControlModeSchedNum == 0 && TESCoil(item).ModeControlType == ScheduledOpModes) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
                    ErrorsFound = true;
                }
            }
            {
                auto const SELECT_CASE_var(cAlphaArgs(5));
                if (SELECT_CASE_var == "ICE") {
                    TESCoil(item).StorageMedia = IceBased;
                } else if (SELECT_CASE_var == "WATER") {
                    TESCoil(item).StorageMedia = FluidBased;
                    TESCoil(item).StorageFluidName = "WATER";
                    TESCoil(item).StorageFluidIndex = FindGlycol("WATER");
                } else if (SELECT_CASE_var == "USERDEFINEDFLUIDTYPE") {
                    TESCoil(item).StorageMedia = FluidBased;
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                    ShowContinueError("Available choices are Ice, Water, or UserDefindedFluidType");
                    ErrorsFound = true;
                }
            }

            if (UtilityRoutines::SameString(cAlphaArgs(5), "USERDEFINEDFLUIDTYPE")) {
                if (!(lAlphaFieldBlanks(6))) {
                    TESCoil(item).StorageFluidName = cAlphaArgs(6);
                    if (CheckFluidPropertyName(cAlphaArgs(6)) == 0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", missing fluid data");
                        ShowContinueError("Check that fluid property data have been input for fluid name = " + cAlphaArgs(6));
                        ErrorsFound = true;
                    } else {
                        TESCoil(item).StorageFluidIndex = FindGlycol(cAlphaArgs(6));
                        if (TESCoil(item).StorageFluidIndex == 0) {
                            ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid fluid data");
                            ShowContinueError("Check that correct fluid property data have been input for fluid name = " + cAlphaArgs(6));
                            ErrorsFound = true;
                        }
                    }

                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("Storage Type is set to UserDefinedFluidType but no name of fluid was entered.");
                    ErrorsFound = true;
                }
            }

            if ((TESCoil(item).StorageMedia == FluidBased) && (!lNumericFieldBlanks(1))) {
                TESCoil(item).FluidStorageVolume = rNumericArgs(1);
            } else if ((TESCoil(item).StorageMedia == FluidBased) && (lNumericFieldBlanks(1))) {
                ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                ShowContinueError(cNumericFieldNames(1) + " cannot be blank for Water storage type");
                ShowContinueError("Enter fluid storage tank volume in m3/s.");
                ErrorsFound = true;
            }

            if ((TESCoil(item).StorageMedia == IceBased) && (!lNumericFieldBlanks(2))) {
                if (rNumericArgs(2) == AutoCalculate) {
                    TESCoil(item).IceStorageCapacity = rNumericArgs(2);
                } else {
                    TESCoil(item).IceStorageCapacity = rNumericArgs(2) * 1.e+09; // input in giga joules, used as joules internally
                }
            } else if ((TESCoil(item).StorageMedia == IceBased) && (lNumericFieldBlanks(2))) {
                ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                ShowContinueError(cNumericFieldNames(2) + " cannot be blank for Ice storage type");
                ShowContinueError("Enter ice storage tank capacity in GJ.");
                ErrorsFound = true;
            }

            TESCoil(item).StorageCapacitySizingFactor = rNumericArgs(3);

            TESCoil(item).StorageAmbientNodeNum = GetOnlySingleNode(
                cAlphaArgs(7), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent);

            ZoneIndexTrial = FindControlledZoneIndexFromSystemNodeNumberForZone(TESCoil(item).StorageAmbientNodeNum);
            if (ZoneIndexTrial > 0) { // tank is inside a zone so setup internal gains
                SetupZoneInternalGain(ZoneIndexTrial,
                                      "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                      TESCoil(item).Name,
                                      IntGainTypeOf_PackagedTESCoilTank,
                                      TESCoil(item).QdotAmbient);
            }

            TESCoil(item).StorageUA = rNumericArgs(4);
            TESCoil(item).RatedFluidTankTemp = rNumericArgs(5);
            TESCoil(item).RatedEvapAirVolFlowRate = rNumericArgs(6);

            TESCoil(item).EvapAirInletNodeNum = GetOnlySingleNode(
                cAlphaArgs(8), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
            TESCoil(item).EvapAirOutletNodeNum = GetOnlySingleNode(
                cAlphaArgs(9), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(8), cAlphaArgs(9), "Air Nodes");

            {
                auto const SELECT_CASE_var(cAlphaArgs(10));
                if (SELECT_CASE_var == "YES") {
                    TESCoil(item).CoolingOnlyModeIsAvailable = true;
                } else if (SELECT_CASE_var == "NO") {
                    TESCoil(item).CoolingOnlyModeIsAvailable = false;
                } else {
                    TESCoil(item).CoolingOnlyModeIsAvailable = false;
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(10) + "=\"" + cAlphaArgs(10) + "\".");
                    ShowContinueError("Available choices are Yes or No.");
                    ErrorsFound = true;
                }
            }

            TESCoil(item).CoolingOnlyRatedTotCap = rNumericArgs(7);
            if (TESCoil(item).CoolingOnlyModeIsAvailable) { // get input data for this mode

                TESCoil(item).CoolingOnlyRatedSHR = rNumericArgs(8);
                TESCoil(item).CoolingOnlyRatedCOP = rNumericArgs(9);

                TESCoil(item).CoolingOnlyCapFTempCurve = GetCurveIndex(cAlphaArgs(11));
                if (TESCoil(item).CoolingOnlyCapFTempCurve == 0) {
                    if (lAlphaFieldBlanks(11)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(11) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(11) + "=\"" + cAlphaArgs(11) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                        TESCoil(item).CoolingOnlyCapFTempCurve,   // Curve index
                         {2},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(11));                   // Field Name
                }

                TESCoil(item).CoolingOnlyCapFFlowCurve = GetCurveIndex(cAlphaArgs(12));
                if (TESCoil(item).CoolingOnlyCapFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(12)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(12) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(12) + "=\"" + cAlphaArgs(12) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                        TESCoil(item).CoolingOnlyCapFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(12));                   // Field Name
                }

                TESCoil(item).CoolingOnlyEIRFTempCurve = GetCurveIndex(cAlphaArgs(13));
                if (TESCoil(item).CoolingOnlyEIRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(13)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(13) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(13) + "=\"" + cAlphaArgs(13) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                        TESCoil(item).CoolingOnlyEIRFTempCurve,   // Curve index
                         {2},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(13));                   // Field Name
                }

                TESCoil(item).CoolingOnlyEIRFFlowCurve = GetCurveIndex(cAlphaArgs(14));
                if (TESCoil(item).CoolingOnlyEIRFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(14)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(14) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(14) + "=\"" + cAlphaArgs(14) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                        TESCoil(item).CoolingOnlyEIRFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(14));                   // Field Name
                }

                TESCoil(item).CoolingOnlyPLFFPLRCurve = GetCurveIndex(cAlphaArgs(15));
                if (TESCoil(item).CoolingOnlyPLFFPLRCurve == 0) {
                    if (lAlphaFieldBlanks(15)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(15) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(15) + "=\"" + cAlphaArgs(15) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                        TESCoil(item).CoolingOnlyPLFFPLRCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(15));                   // Field Name
                }

                TESCoil(item).CoolingOnlySHRFTempCurve = GetCurveIndex(cAlphaArgs(16));
                if (TESCoil(item).CoolingOnlySHRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(16)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(16) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(16) + "=\"" + cAlphaArgs(16) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                        TESCoil(item).CoolingOnlySHRFTempCurve,   // Curve index
                         {2},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(16));                   // Field Name
                }

                TESCoil(item).CoolingOnlySHRFFlowCurve = GetCurveIndex(cAlphaArgs(17));
                if (TESCoil(item).CoolingOnlySHRFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(17)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(17) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(17) + "=\"" + cAlphaArgs(17) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                        TESCoil(item).CoolingOnlySHRFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(17));                   // Field Name
                }
            }

            {
                auto const SELECT_CASE_var(cAlphaArgs(18));
                if (SELECT_CASE_var == "YES") {
                    TESCoil(item).CoolingAndChargeModeAvailable = true;
                } else if (SELECT_CASE_var == "NO") {
                    TESCoil(item).CoolingAndChargeModeAvailable = false;
                } else {
                    TESCoil(item).CoolingAndChargeModeAvailable = false;
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(18) + "=\"" + cAlphaArgs(18) + "\".");
                    ShowContinueError("Available choices are Yes or No.");
                    ErrorsFound = true;
                }
            }

            if (TESCoil(item).CoolingAndChargeModeAvailable) {

                TESCoil(item).CoolingAndChargeRatedTotCap = rNumericArgs(10);                // gross total evaporator cooling capacity [W]
                TESCoil(item).CoolingAndChargeRatedTotCapSizingFactor = rNumericArgs(11);    // sizing factor for gross total evaporator [ ]
                TESCoil(item).CoolingAndChargeRatedChargeCap = rNumericArgs(12);             // net storage charging capacity at rating conditions [W]
                TESCoil(item).CoolingAndChargeRatedChargeCapSizingFactor = rNumericArgs(13); // sizing factor for charging capacity [ ]
                TESCoil(item).CoolingAndChargeRatedSHR = rNumericArgs(14);                   // Sensible heat ratio (sens cap/total cap)  [W/W]
                TESCoil(item).CoolingAndChargeCoolingRatedCOP = rNumericArgs(15);            // Coefficient of performance , for cooling [W/W]
                TESCoil(item).CoolingAndChargeChargingRatedCOP = rNumericArgs(16);           // Coefficient of performance , for charging [W/W]

                TESCoil(item).CoolingAndChargeCoolingCapFTempCurve = GetCurveIndex(cAlphaArgs(19));
                if (TESCoil(item).CoolingAndChargeCoolingCapFTempCurve == 0) {
                    if (lAlphaFieldBlanks(19)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(19) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(19) + "=\"" + cAlphaArgs(19) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeCoolingCapFTempCurve,   // Curve index
                         {3},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(19));                   // Field Name
                }

                TESCoil(item).CoolingAndChargeCoolingCapFFlowCurve = GetCurveIndex(cAlphaArgs(20));
                if (TESCoil(item).CoolingAndChargeCoolingCapFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(20)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(20) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(20) + "=\"" + cAlphaArgs(20) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeCoolingCapFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(20));                   // Field Name
                }
                TESCoil(item).CoolingAndChargeCoolingEIRFTempCurve = GetCurveIndex(cAlphaArgs(21));
                if (TESCoil(item).CoolingAndChargeCoolingEIRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(21)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(21) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(21) + "=\"" + cAlphaArgs(21) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeCoolingEIRFTempCurve,   // Curve index
                         {3},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(21));                   // Field Name
                }

                TESCoil(item).CoolingAndChargeCoolingEIRFFlowCurve = GetCurveIndex(cAlphaArgs(22));
                if (TESCoil(item).CoolingAndChargeCoolingEIRFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(22)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(22) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(22) + "=\"" + cAlphaArgs(22) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeCoolingEIRFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(22));                   // Field Name
                }

                TESCoil(item).CoolingAndChargeCoolingPLFFPLRCurve = GetCurveIndex(cAlphaArgs(23));
                if (TESCoil(item).CoolingAndChargeCoolingPLFFPLRCurve == 0) {
                    if (lAlphaFieldBlanks(23)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(23) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(23) + "=\"" + cAlphaArgs(23) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeCoolingPLFFPLRCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(23));                   // Field Name
                }

                TESCoil(item).CoolingAndChargeChargingCapFTempCurve = GetCurveIndex(cAlphaArgs(24));
                if (TESCoil(item).CoolingAndChargeChargingCapFTempCurve == 0) {
                    if (lAlphaFieldBlanks(24)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(24) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(24) + "=\"" + cAlphaArgs(24) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeChargingCapFTempCurve,   // Curve index
                         {3},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(24));                   // Field Name
                }

                TESCoil(item).CoolingAndChargeChargingCapFEvapPLRCurve = GetCurveIndex(cAlphaArgs(25));
                if (TESCoil(item).CoolingAndChargeChargingCapFEvapPLRCurve == 0) {
                    if (lAlphaFieldBlanks(25)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(25) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(25) + "=\"" + cAlphaArgs(25) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeChargingCapFEvapPLRCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(25));                   // Field Name
                }

                TESCoil(item).CoolingAndChargeChargingEIRFTempCurve = GetCurveIndex(cAlphaArgs(26));
                if (TESCoil(item).CoolingAndChargeChargingEIRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(26)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(26) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(26) + "=\"" + cAlphaArgs(26) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeChargingEIRFTempCurve,   // Curve index
                         {3},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(26));                   // Field Name
                }

                TESCoil(item).CoolingAndChargeChargingEIRFFLowCurve = GetCurveIndex(cAlphaArgs(27));
                if (TESCoil(item).CoolingAndChargeChargingEIRFFLowCurve == 0) {
                    if (lAlphaFieldBlanks(27)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(27) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(27) + "=\"" + cAlphaArgs(27) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeChargingEIRFFLowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(27));                   // Field Name
                }

                TESCoil(item).CoolingAndChargeChargingPLFFPLRCurve = GetCurveIndex(cAlphaArgs(28));
                if (TESCoil(item).CoolingAndChargeChargingPLFFPLRCurve == 0) {
                    if (lAlphaFieldBlanks(28)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(28) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(28) + "=\"" + cAlphaArgs(28) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeChargingPLFFPLRCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(28));                   // Field Name
                }

                TESCoil(item).CoolingAndChargeSHRFTempCurve = GetCurveIndex(cAlphaArgs(29));
                if (TESCoil(item).CoolingAndChargeSHRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(29)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(29) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(29) + "=\"" + cAlphaArgs(29) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeSHRFTempCurve,   // Curve index
                         {2, 3},                                  // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(29));                   // Field Name
                }

                TESCoil(item).CoolingAndChargeSHRFFlowCurve = GetCurveIndex(cAlphaArgs(30));
                if (TESCoil(item).CoolingAndChargeSHRFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(30)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(30) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(30) + "=\"" + cAlphaArgs(30) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndChargeSHRFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(30));                   // Field Name
                }

            } // Cooling and Charge Mode available

            {
                auto const SELECT_CASE_var(cAlphaArgs(31));
                if (SELECT_CASE_var == "YES") {
                    TESCoil(item).CoolingAndDischargeModeAvailable = true;
                } else if (SELECT_CASE_var == "NO") {
                    TESCoil(item).CoolingAndDischargeModeAvailable = false;
                } else {
                    TESCoil(item).CoolingAndDischargeModeAvailable = false;
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(31) + "=\"" + cAlphaArgs(31) + "\".");
                    ShowContinueError("Available choices are Yes or No.");
                    ErrorsFound = true;
                }
            }

            if (TESCoil(item).CoolingAndDischargeModeAvailable) {

                TESCoil(item).CoolingAndDischargeRatedTotCap = rNumericArgs(17);                   // gross total evaporator cooling capacity  [W]
                TESCoil(item).CoolingAndDischargeRatedTotCapSizingFactor = rNumericArgs(18);       // sizing factor gross total cooling capacity []
                TESCoil(item).CoolingAndDischargeRatedDischargeCap = rNumericArgs(19);             // net storage discharging capacity  [W]
                TESCoil(item).CoolingAndDischargeRatedDischargeCapSizingFactor = rNumericArgs(20); // sizing factor discharging capacity []
                TESCoil(item).CoolingAndDischargeRatedSHR = rNumericArgs(21);                      // Sensible heat ratio (sens cap/total cap) [W/W]
                TESCoil(item).CoolingAndDischargeCoolingRatedCOP = rNumericArgs(22);               // Coefficient of performance , for cooling [W/W]
                TESCoil(item).CoolingAndDischargeDischargingRatedCOP = rNumericArgs(23);           // Coefficient of performance , for charging [W/W]

                TESCoil(item).CoolingAndDischargeCoolingCapFTempCurve = GetCurveIndex(cAlphaArgs(32));
                if (TESCoil(item).CoolingAndDischargeCoolingCapFTempCurve == 0) {
                    if (lAlphaFieldBlanks(32)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(32) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(32) + "=\"" + cAlphaArgs(32) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeCoolingCapFTempCurve,   // Curve index
                         {3},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(32));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeCoolingCapFFlowCurve = GetCurveIndex(cAlphaArgs(33));
                if (TESCoil(item).CoolingAndDischargeCoolingCapFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(33)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(33) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(33) + "=\"" + cAlphaArgs(33) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeCoolingCapFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(33));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeCoolingEIRFTempCurve = GetCurveIndex(cAlphaArgs(34));
                if (TESCoil(item).CoolingAndDischargeCoolingEIRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(34)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(34) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(34) + "=\"" + cAlphaArgs(34) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeCoolingEIRFTempCurve,   // Curve index
                         {3},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(34));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeCoolingEIRFFlowCurve = GetCurveIndex(cAlphaArgs(35));
                if (TESCoil(item).CoolingAndDischargeCoolingEIRFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(35)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(35) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(35) + "=\"" + cAlphaArgs(35) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeCoolingEIRFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(35));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeCoolingPLFFPLRCurve = GetCurveIndex(cAlphaArgs(36));
                if (TESCoil(item).CoolingAndDischargeCoolingPLFFPLRCurve == 0) {
                    if (lAlphaFieldBlanks(36)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(36) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(36) + "=\"" + cAlphaArgs(36) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeCoolingPLFFPLRCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(36));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeDischargingCapFTempCurve = GetCurveIndex(cAlphaArgs(37));
                if (TESCoil(item).CoolingAndDischargeDischargingCapFTempCurve == 0) {
                    if (lAlphaFieldBlanks(37)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(37) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(37) + "=\"" + cAlphaArgs(37) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeDischargingCapFTempCurve,   // Curve index
                         {3},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(37));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeDischargingCapFFlowCurve = GetCurveIndex(cAlphaArgs(38));
                if (TESCoil(item).CoolingAndDischargeDischargingCapFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(38)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(38) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(38) + "=\"" + cAlphaArgs(38) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeDischargingCapFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(38));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeDischargingCapFEvapPLRCurve = GetCurveIndex(cAlphaArgs(39));
                if (TESCoil(item).CoolingAndDischargeDischargingCapFEvapPLRCurve == 0) {
                    if (lAlphaFieldBlanks(39)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(39) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(39) + "=\"" + cAlphaArgs(39) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeDischargingCapFEvapPLRCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(39));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeDischargingEIRFTempCurve = GetCurveIndex(cAlphaArgs(40));
                if (TESCoil(item).CoolingAndDischargeDischargingEIRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(40)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(40) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(40) + "=\"" + cAlphaArgs(40) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeDischargingEIRFTempCurve,   // Curve index
                         {3},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(40));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeDischargingEIRFFLowCurve = GetCurveIndex(cAlphaArgs(41));
                if (TESCoil(item).CoolingAndDischargeDischargingEIRFFLowCurve == 0) {
                    if (lAlphaFieldBlanks(41)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(41) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(41) + "=\"" + cAlphaArgs(41) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeDischargingEIRFFLowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(41));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeDischargingPLFFPLRCurve = GetCurveIndex(cAlphaArgs(42));
                if (TESCoil(item).CoolingAndDischargeDischargingPLFFPLRCurve == 0) {
                    if (lAlphaFieldBlanks(42)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(42) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(42) + "=\"" + cAlphaArgs(42) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeDischargingPLFFPLRCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(42));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeSHRFTempCurve = GetCurveIndex(cAlphaArgs(43));
                if (TESCoil(item).CoolingAndDischargeSHRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(43)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(43) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(43) + "=\"" + cAlphaArgs(43) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeSHRFTempCurve,   // Curve index
                         {2, 3},                                  // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(43));                   // Field Name
                }

                TESCoil(item).CoolingAndDischargeSHRFFlowCurve = GetCurveIndex(cAlphaArgs(44));
                if (TESCoil(item).CoolingAndDischargeSHRFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(44)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(44) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(44) + "=\"" + cAlphaArgs(44) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).CoolingAndDischargeSHRFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(44));                   // Field Name
                }

            } // cooling and discharge mode available

            {
                auto const SELECT_CASE_var(cAlphaArgs(45));
                if (SELECT_CASE_var == "YES") {
                    TESCoil(item).ChargeOnlyModeAvailable = true;
                } else if (SELECT_CASE_var == "NO") {
                    TESCoil(item).ChargeOnlyModeAvailable = false;
                } else {
                    TESCoil(item).ChargeOnlyModeAvailable = false;
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(45) + "=\"" + cAlphaArgs(45) + "\".");
                    ShowContinueError("Available choices are Yes or No.");
                    ErrorsFound = true;
                }
            }

            if (TESCoil(item).ChargeOnlyModeAvailable) {

                TESCoil(item).ChargeOnlyRatedCapacity = rNumericArgs(24);             // net storage charging capacity at rating conditions [W]
                TESCoil(item).ChargeOnlyRatedCapacitySizingFactor = rNumericArgs(25); // sizing factor for charging capacity []
                TESCoil(item).ChargeOnlyRatedCOP = rNumericArgs(26);                  // coefficient of performance at rating conditions [W/W]

                TESCoil(item).ChargeOnlyChargingCapFTempCurve = GetCurveIndex(cAlphaArgs(46));
                if (TESCoil(item).ChargeOnlyChargingCapFTempCurve == 0) {
                    if (lAlphaFieldBlanks(46)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(46) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(46) + "=\"" + cAlphaArgs(46) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).ChargeOnlyChargingCapFTempCurve,   // Curve index
                         {2},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(46));                   // Field Name
                }

                TESCoil(item).ChargeOnlyChargingEIRFTempCurve = GetCurveIndex(cAlphaArgs(47));
                if (TESCoil(item).ChargeOnlyChargingEIRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(47)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(47) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(47) + "=\"" + cAlphaArgs(47) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).ChargeOnlyChargingEIRFTempCurve,   // Curve index
                         {2},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(47));                   // Field Name
                }

            } // Charge only mode available

            {
                auto const SELECT_CASE_var(cAlphaArgs(48));
                if (SELECT_CASE_var == "YES") {
                    TESCoil(item).DischargeOnlyModeAvailable = true;
                } else if (SELECT_CASE_var == "NO") {
                    TESCoil(item).DischargeOnlyModeAvailable = false;
                } else {
                    TESCoil(item).DischargeOnlyModeAvailable = false;
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(48) + "=\"" + cAlphaArgs(48) + "\".");
                    ShowContinueError("Available choices are Yes or No.");
                    ErrorsFound = true;
                }
            }

            if (TESCoil(item).DischargeOnlyModeAvailable) {
                TESCoil(item).DischargeOnlyRatedDischargeCap = rNumericArgs(27);             // gross total evaporator cooling capacity  [W]
                TESCoil(item).DischargeOnlyRatedDischargeCapSizingFactor = rNumericArgs(28); // sizing factor for cooling capacity []
                TESCoil(item).DischargeOnlyRatedSHR = rNumericArgs(29);                      // sensible heat ratio (sens cap/total cap)
                TESCoil(item).DischargeOnlyRatedCOP = rNumericArgs(30);                      // coefficient of performance  for discharging [W/W]

                TESCoil(item).DischargeOnlyCapFTempCurve = GetCurveIndex(cAlphaArgs(49));
                if (TESCoil(item).DischargeOnlyCapFTempCurve == 0) {
                    if (lAlphaFieldBlanks(49)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(49) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(49) + "=\"" + cAlphaArgs(49) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).DischargeOnlyCapFTempCurve,   // Curve index
                         {2},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(49));                   // Field Name
                }

                TESCoil(item).DischargeOnlyCapFFlowCurve = GetCurveIndex(cAlphaArgs(50));
                if (TESCoil(item).DischargeOnlyCapFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(50)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(50) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(50) + "=\"" + cAlphaArgs(50) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).DischargeOnlyCapFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(50));                   // Field Name
                }

                TESCoil(item).DischargeOnlyEIRFTempCurve = GetCurveIndex(cAlphaArgs(51));
                if (TESCoil(item).DischargeOnlyEIRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(51)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(51) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(51) + "=\"" + cAlphaArgs(51) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).DischargeOnlyEIRFTempCurve,   // Curve index
                         {2},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(51));                   // Field Name
                }

                TESCoil(item).DischargeOnlyEIRFFlowCurve = GetCurveIndex(cAlphaArgs(52));
                if (TESCoil(item).DischargeOnlyEIRFFlowCurve == 0) {
                    if (lAlphaFieldBlanks(52)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(52) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(52) + "=\"" + cAlphaArgs(52) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).DischargeOnlyEIRFFlowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(52));                   // Field Name
                }

                TESCoil(item).DischargeOnlyPLFFPLRCurve = GetCurveIndex(cAlphaArgs(53));
                if (TESCoil(item).DischargeOnlyPLFFPLRCurve == 0) {
                    if (lAlphaFieldBlanks(53)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(53) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(53) + "=\"" + cAlphaArgs(53) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).DischargeOnlyPLFFPLRCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(53));                   // Field Name
                }

                TESCoil(item).DischargeOnlySHRFTempCurve = GetCurveIndex(cAlphaArgs(54));
                if (TESCoil(item).DischargeOnlySHRFTempCurve == 0) {
                    if (lAlphaFieldBlanks(54)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(54) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(54) + "=\"" + cAlphaArgs(54) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).DischargeOnlySHRFTempCurve,   // Curve index
                         {2, 3},                                  // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(54));                   // Field Name
                }

                TESCoil(item).DischargeOnlySHRFFLowCurve = GetCurveIndex(cAlphaArgs(55));
                if (TESCoil(item).DischargeOnlySHRFFLowCurve == 0) {
                    if (lAlphaFieldBlanks(55)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Required " + cAlphaFieldNames(55) + "is blank.");
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                        ShowContinueError("Not found " + cAlphaFieldNames(55) + "=\"" + cAlphaArgs(55) + "\".");
                    }
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, any curve with just x as single independent variable
                    ErrorsFound |= CurveManager::CheckCurveDims(
                         TESCoil(item).DischargeOnlySHRFFLowCurve,   // Curve index
                         {1},                                     // Valid dimensions
                         RoutineName,                             // Routine name
                         cCurrentModuleObject,                    // Object Type
                         TESCoil(item).Name,                      // Object Name
                         cAlphaFieldNames(55));                   // Field Name
                }

            } // Discharge Only mode available

            TESCoil(item).AncillaryControlsPower = rNumericArgs(31);
            TESCoil(item).ColdWeatherMinimumTempLimit = rNumericArgs(32);
            TESCoil(item).ColdWeatherAncillaryPower = rNumericArgs(33);
            TESCoil(item).CondAirInletNodeNum = GetOnlySingleNode(cAlphaArgs(56),
                                                                  ErrorsFound,
                                                                  cCurrentModuleObject,
                                                                  TESCoil(item).Name,
                                                                  NodeType_Air,
                                                                  NodeConnectionType_OutsideAirReference,
                                                                  1,
                                                                  ObjectIsNotParent);
            TESCoil(item).CondAirOutletNodeNum = GetOnlySingleNode(cAlphaArgs(57),
                                                                   ErrorsFound,
                                                                   cCurrentModuleObject,
                                                                   TESCoil(item).Name,
                                                                   NodeType_Air,
                                                                   NodeConnectionType_ReliefAir,
                                                                   1,
                                                                   ObjectIsNotParent);

            TESCoil(item).CondenserAirVolumeFlow = rNumericArgs(34);
            TESCoil(item).CondenserAirFlowSizingFactor = rNumericArgs(35);
            {
                auto const SELECT_CASE_var(cAlphaArgs(58));

                if (SELECT_CASE_var == "AIRCOOLED") {
                    TESCoil(item).CondenserType = AirCooled;
                } else if (SELECT_CASE_var == "EVAPORATIVELYCOOLED") {
                    TESCoil(item).CondenserType = EvapCooled;
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError(cAlphaFieldNames(58) + "=\"" + cAlphaArgs(58) + "\".");
                    ShowContinueError("Available choices are AirCooled or EvaporativelyCooled.");
                    ErrorsFound = true;
                }
            }
            TESCoil(item).EvapCondEffect = rNumericArgs(36);
            TESCoil(item).EvapCondPumpElecNomPower = rNumericArgs(37);
            TESCoil(item).BasinHeaterPowerFTempDiff = rNumericArgs(38);
            TESCoil(item).BasinHeaterSetpointTemp = rNumericArgs(39);

            if (lAlphaFieldBlanks(59)) {
                TESCoil(item).BasinHeaterAvailSchedNum = ScheduleAlwaysOn;
            } else {
                TESCoil(item).BasinHeaterAvailSchedNum = GetScheduleIndex(cAlphaArgs(59));
                if (TESCoil(item).BasinHeaterAvailSchedNum == 0) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(59) + "=\"" + cAlphaArgs(59) + "\".");
                    ErrorsFound = true;
                }
            }

            if (lAlphaFieldBlanks(60)) {
                TESCoil(item).EvapWaterSupplyMode = WaterSupplyFromMains;
            } else {
                TESCoil(item).EvapWaterSupplyName = cAlphaArgs(60);
                TESCoil(item).EvapWaterSupplyMode = WaterSupplyFromTank;
                SetupTankDemandComponent(TESCoil(item).Name,
                                         cCurrentModuleObject,
                                         TESCoil(item).EvapWaterSupplyName,
                                         ErrorsFound,
                                         TESCoil(item).EvapWaterSupTankID,
                                         TESCoil(item).EvapWaterTankDemandARRID);
            }

            if (lAlphaFieldBlanks(61)) {
                TESCoil(item).CondensateCollectMode = CondensateDiscarded;
            } else {
                TESCoil(item).CondensateCollectName = cAlphaArgs(61);
                TESCoil(item).CondensateCollectMode = CondensateToTank;
                SetupTankSupplyComponent(TESCoil(item).Name,
                                         cCurrentModuleObject,
                                         TESCoil(item).CondensateCollectName,
                                         ErrorsFound,
                                         TESCoil(item).CondensateTankID,
                                         TESCoil(item).CondensateTankSupplyARRID);
            }

            if (!lAlphaFieldBlanks(62)) {
                TESCoil(item).TESPlantInletNodeNum = GetOnlySingleNode(
                    cAlphaArgs(62), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent);

                TESCoil(item).TESPlantConnectionAvailable = true;
            } else {
                TESCoil(item).TESPlantConnectionAvailable = false;
            }
            if (!lAlphaFieldBlanks(63)) {
                TESCoil(item).TESPlantOutletNodeNum = GetOnlySingleNode(cAlphaArgs(63),
                                                                        ErrorsFound,
                                                                        cCurrentModuleObject,
                                                                        cAlphaArgs(1),
                                                                        NodeType_Water,
                                                                        NodeConnectionType_Outlet,
                                                                        2,
                                                                        ObjectIsNotParent);
            } else {
                if (TESCoil(item).TESPlantConnectionAvailable) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + TESCoil(item).Name + "\", invalid");
                    ShowContinueError("..." + cAlphaFieldNames(63) + " cannot be blank.");
                    ErrorsFound = true;
                }
            }
            if (TESCoil(item).TESPlantConnectionAvailable) {
                TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(62), cAlphaArgs(63), "Water Nodes");
            }

            if (!lNumericFieldBlanks(40)) {
                TESCoil(item).TESPlantDesignVolumeFlowRate = rNumericArgs(40);
            }
            if (!lNumericFieldBlanks(41)) {
                TESCoil(item).TESPlantEffectiveness = rNumericArgs(41);
            }
            if (TESCoil(item).StorageMedia == FluidBased) {
                if (!lNumericFieldBlanks(42)) {
                    TESCoil(item).MinimumFluidTankTempLimit = rNumericArgs(42);
                } else {

                    GetFluidDensityTemperatureLimits(TESCoil(item).StorageFluidIndex, TminRho, TmaxRho);
                    GetFluidSpecificHeatTemperatureLimits(TESCoil(item).StorageFluidIndex, TminCp, TmaxCp);
                    TESCoil(item).MinimumFluidTankTempLimit = max(TminRho, TminCp);
                }
                if (!lNumericFieldBlanks(43)) {
                    TESCoil(item).MaximumFluidTankTempLimit = rNumericArgs(43);
                } else {
                    GetFluidDensityTemperatureLimits(TESCoil(item).StorageFluidIndex, TminRho, TmaxRho);
                    GetFluidSpecificHeatTemperatureLimits(TESCoil(item).StorageFluidIndex, TminCp, TmaxCp);
                    TESCoil(item).MaximumFluidTankTempLimit = min(TmaxRho, TmaxCp);
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in getting " + cCurrentModuleObject + " input. Preceding condition(s) causes termination.");
        }

        // setup reporting
        for (item = 1; item <= NumTESCoils; ++item) {
            SetupOutputVariable("Cooling Coil Operating Mode Index",
                                OutputProcessor::Unit::None,
                                TESCoil(item).CurControlMode,
                                "System",
                                "Average",
                                TESCoil(item).Name);

            // cCurrentModuleObject = "Coil:Cooling:DX:SingleSpeed:ThermalStorage"
            SetupOutputVariable("Cooling Coil Total Cooling Rate",
                                OutputProcessor::Unit::W,
                                TESCoil(item).EvapTotCoolingRate,
                                "System",
                                "Average",
                                TESCoil(item).Name);
            SetupOutputVariable("Cooling Coil Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                TESCoil(item).EvapTotCoolingEnergy,
                                "System",
                                "Sum",
                                TESCoil(item).Name,
                                _,
                                "ENERGYTRANSFER",
                                "COOLINGCOILS",
                                _,
                                "System");
            SetupOutputVariable("Cooling Coil Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                TESCoil(item).EvapSensCoolingRate,
                                "System",
                                "Average",
                                TESCoil(item).Name);
            SetupOutputVariable("Cooling Coil Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                TESCoil(item).EvapSensCoolingEnergy,
                                "System",
                                "Sum",
                                TESCoil(item).Name);
            SetupOutputVariable("Cooling Coil Latent Cooling Rate",
                                OutputProcessor::Unit::W,
                                TESCoil(item).EvapLatCoolingRate,
                                "System",
                                "Average",
                                TESCoil(item).Name);
            SetupOutputVariable("Cooling Coil Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                TESCoil(item).EvapLatCoolingEnergy,
                                "System",
                                "Sum",
                                TESCoil(item).Name);
            SetupOutputVariable(
                "Cooling Coil Electric Power", OutputProcessor::Unit::W, TESCoil(item).ElecCoolingPower, "System", "Average", TESCoil(item).Name);
            SetupOutputVariable("Cooling Coil Electric Energy",
                                OutputProcessor::Unit::J,
                                TESCoil(item).ElecCoolingEnergy,
                                "System",
                                "Sum",
                                TESCoil(item).Name,
                                _,
                                "Electric",
                                "COOLING",
                                _,
                                "System");

            SetupOutputVariable(
                "Cooling Coil Runtime Fraction", OutputProcessor::Unit::None, TESCoil(item).RuntimeFraction, "System", "Average", TESCoil(item).Name);
            SetupOutputVariable("Cooling Coil Cold Weather Protection Electric Energy",
                                OutputProcessor::Unit::J,
                                TESCoil(item).ElectColdWeatherEnergy,
                                "System",
                                "Sum",
                                TESCoil(item).Name,
                                _,
                                "Electric",
                                "COOLING",
                                "Thermal Protection",
                                "System");
            SetupOutputVariable("Cooling Coil Cold Weather Protection Electric Power",
                                OutputProcessor::Unit::W,
                                TESCoil(item).ElectColdWeatherPower,
                                "System",
                                "Average",
                                TESCoil(item).Name);

            SetupOutputVariable("Cooling Coil Thermal Storage Mechanical Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                TESCoil(item).QdotTES,
                                "System",
                                "Average",
                                TESCoil(item).Name);

            SetupOutputVariable("Cooling Coil Thermal Storage Mechanical Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                TESCoil(item).Q_TES,
                                "System",
                                "Sum",
                                TESCoil(item).Name);

            SetupOutputVariable("Cooling Coil Thermal Storage Ambient Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                TESCoil(item).QdotAmbient,
                                "System",
                                "Average",
                                TESCoil(item).Name);

            SetupOutputVariable("Cooling Coil Thermal Storage Ambient Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                TESCoil(item).Q_Ambient,
                                "System",
                                "Sum",
                                TESCoil(item).Name);

            if (TESCoil(item).TESPlantConnectionAvailable) {
                SetupOutputVariable("Cooling Coil Thermal Storage Plant Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    TESCoil(item).QdotPlant,
                                    "System",
                                    "Average",
                                    TESCoil(item).Name);
                SetupOutputVariable("Cooling Coil Thermal Storage Plant Heat Transfer Energy",
                                    OutputProcessor::Unit::J,
                                    TESCoil(item).Q_Plant,
                                    "System",
                                    "Sum",
                                    TESCoil(item).Name);
            }

            if (TESCoil(item).CondenserType == EvapCooled) {
                SetupOutputVariable("Cooling Coil Condenser Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    TESCoil(item).CondInletTemp,
                                    "System",
                                    "Average",
                                    TESCoil(item).Name);

                if (TESCoil(item).EvapWaterSupplyMode == WaterSupplyFromMains) {
                    SetupOutputVariable("Cooling Coil Evaporative Condenser Water Volume",
                                        OutputProcessor::Unit::m3,
                                        TESCoil(item).EvapWaterConsump,
                                        "System",
                                        "Sum",
                                        TESCoil(item).Name,
                                        _,
                                        "Water",
                                        "Cooling",
                                        _,
                                        "System");
                    SetupOutputVariable("Cooling Coil Evaporative Condenser Mains Supply Water Volume",
                                        OutputProcessor::Unit::m3,
                                        TESCoil(item).EvapWaterConsump,
                                        "System",
                                        "Sum",
                                        TESCoil(item).Name,
                                        _,
                                        "MainsWater",
                                        "Cooling",
                                        _,
                                        "System");
                } else if (TESCoil(item).EvapWaterSupplyMode == WaterSupplyFromTank) {
                    SetupOutputVariable("Cooling Coil Evaporative Condenser Storage Tank Water Volume",
                                        OutputProcessor::Unit::m3,
                                        TESCoil(item).EvapWaterConsump,
                                        "System",
                                        "Sum",
                                        TESCoil(item).Name,
                                        _,
                                        "Water",
                                        "Cooling",
                                        _,
                                        "System");
                    SetupOutputVariable("Cooling Coil Evaporative Condenser Starved Water Volume",
                                        OutputProcessor::Unit::m3,
                                        TESCoil(item).EvapWaterStarvMakup,
                                        "System",
                                        "Sum",
                                        TESCoil(item).Name,
                                        _,
                                        "Water",
                                        "Cooling",
                                        _,
                                        "System");
                    SetupOutputVariable("Cooling Coil Evaporative Condenser Starved Mains Water Volume",
                                        OutputProcessor::Unit::m3,
                                        TESCoil(item).EvapWaterStarvMakup,
                                        "System",
                                        "Sum",
                                        TESCoil(item).Name,
                                        _,
                                        "MainsWater",
                                        "Cooling",
                                        _,
                                        "System");
                }

                SetupOutputVariable("Cooling Coil Evaporative Condenser Pump Electric Power",
                                    OutputProcessor::Unit::W,
                                    TESCoil(item).EvapCondPumpElecPower,
                                    "System",
                                    "Average",
                                    TESCoil(item).Name);
                SetupOutputVariable("Cooling Coil Evaporative Condenser Pump Electric Energy",
                                    OutputProcessor::Unit::J,
                                    TESCoil(item).EvapCondPumpElecConsumption,
                                    "System",
                                    "Sum",
                                    TESCoil(item).Name,
                                    _,
                                    "Electric",
                                    "COOLING",
                                    _,
                                    "System");

                SetupOutputVariable("Cooling Coil Basin Heater Electric Power",
                                    OutputProcessor::Unit::W,
                                    TESCoil(item).ElectEvapCondBasinHeaterPower,
                                    "System",
                                    "Average",
                                    TESCoil(item).Name);
                SetupOutputVariable("Cooling Coil Basin Heater Electric Energy",
                                    OutputProcessor::Unit::J,
                                    TESCoil(item).ElectEvapCondBasinHeaterEnergy,
                                    "System",
                                    "Sum",
                                    TESCoil(item).Name,
                                    _,
                                    "Electric",
                                    "COOLING",
                                    "Thermal Protection",
                                    "System");
            }

            if (TESCoil(item).StorageMedia == FluidBased) {
                SetupOutputVariable("Cooling Coil Fluid Thermal Storage End Temperature",
                                    OutputProcessor::Unit::C,
                                    TESCoil(item).FluidTankTempFinal,
                                    "System",
                                    "Average",
                                    TESCoil(item).Name);

            } else if (TESCoil(item).StorageMedia == IceBased) {
                SetupOutputVariable("Cooling Coil Ice Thermal Storage End Fraction",
                                    OutputProcessor::Unit::None,
                                    TESCoil(item).IceFracRemain,
                                    "System",
                                    "Average",
                                    TESCoil(item).Name);
            }
        }

        if (AnyEnergyManagementSystemInModel) {
            for (item = 1; item <= NumTESCoils; ++item) {
                // setup EMS actuator for control mode
                SetupEMSActuator("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                 TESCoil(item).Name,
                                 "Operating Mode",
                                 "[ ]",
                                 TESCoil(item).EMSControlModeOn,
                                 TESCoil(item).EMSControlModeValue);
            }
        }
    }

    void InitTESCoil(int &TESCoilNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataPlant::PlantLoop;
        using DataPlant::TypeOf_PackagedTESCoolingCoil;
        using General::RoundSigDigits;
        using PlantUtilities::ScanPlantLoopsForObject;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static Array1D_bool MyFlag;       // One time environment flag
        static Array1D_bool MySizeFlag;   // One time sizing flag
        static Array1D_bool MyEnvrnFlag;  // flag for init once at start of environment
        static Array1D_bool MyWarmupFlag; // flag for init after warmup complete
        static bool MyOneTimeFlag(true);  // One time flag used to allocate MyEnvrnFlag and MySizeFlag
        bool errFlag;
        int plloopnum;
        int lsnum;
        int brnum;
        int cpnum;
        Real64 tmpSchedValue;

        if (MyOneTimeFlag) {
            // initialize the environment and sizing flags
            MyFlag.allocate(NumTESCoils);
            MySizeFlag.allocate(NumTESCoils);
            MyEnvrnFlag.allocate(NumTESCoils);
            MyWarmupFlag.allocate(NumTESCoils);
            MyFlag = true;
            MySizeFlag = true;
            MyEnvrnFlag = true;
            MyOneTimeFlag = false;
            MyWarmupFlag = false;
        }

        if (MyFlag(TESCoilNum)) {

            if (TESCoil(TESCoilNum).TESPlantConnectionAvailable) {
                errFlag = false;
                ScanPlantLoopsForObject(TESCoil(TESCoilNum).Name, TypeOf_PackagedTESCoolingCoil, plloopnum, lsnum, brnum, cpnum, errFlag);

                // double check node names match
                if (errFlag) {
                    ShowFatalError("InitTESCoil: Program terminated due to previous condition(s).");
                }
                TESCoil(TESCoilNum).TESPlantLoopNum = plloopnum;
                TESCoil(TESCoilNum).TESPlantLoopSideNum = lsnum;
                TESCoil(TESCoilNum).TESPlantBranchNum = brnum;
                TESCoil(TESCoilNum).TESPlantCompNum = cpnum;

                if ((PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumIn != TESCoil(TESCoilNum).TESPlantInletNodeNum) ||
                    (PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumOut != TESCoil(TESCoilNum).TESPlantOutletNodeNum)) {
                    ShowSevereError("InitTESCoil: Coil:Cooling:DX:SingleSpeed:ThermalStorage =\"" + TESCoil(TESCoilNum).Name +
                                    "\", non-matching plant nodes.");
                    ShowContinueError("...in Branch=\"" +
                                      PlantLoop(TESCoil(TESCoilNum).TESPlantLoopNum)
                                          .LoopSide(TESCoil(TESCoilNum).TESPlantLoopSideNum)
                                          .Branch(TESCoil(TESCoilNum).TESPlantBranchNum)
                                          .Name +
                                      "\", Component referenced with:");
                    ShowContinueError("...Inlet Node=\"" + NodeID(PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumIn));
                    ShowContinueError("...Outlet Node=\"" + NodeID(PlantLoop(plloopnum).LoopSide(lsnum).Branch(brnum).Comp(cpnum).NodeNumOut));
                    ShowContinueError("...TES Inlet Node=\"" + NodeID(TESCoil(TESCoilNum).TESPlantInletNodeNum));
                    ShowContinueError("...TES Outlet Node=\"" + NodeID(TESCoil(TESCoilNum).TESPlantOutletNodeNum));
                    errFlag = true;
                }
                if (errFlag) {
                    ShowFatalError("InitTESCoil: Program terminated due to previous condition(s).");
                }

            } // any plant connection to TES
            MyFlag(TESCoilNum) = false;
        }

        if (MySizeFlag(TESCoilNum)) {

            SizeTESCoil(TESCoilNum);

            MySizeFlag(TESCoilNum) = false;
        }

        if (BeginEnvrnFlag && MyEnvrnFlag(TESCoilNum)) {
            TESCoil(TESCoilNum).CurControlMode = OffMode;
            TESCoil(TESCoilNum).QdotPlant = 0.0;
            TESCoil(TESCoilNum).Q_Plant = 0.0;
            TESCoil(TESCoilNum).QdotAmbient = 0.0;
            TESCoil(TESCoilNum).Q_Ambient = 0.0;
            TESCoil(TESCoilNum).QdotTES = 0.0;
            TESCoil(TESCoilNum).Q_TES = 0.0;
            TESCoil(TESCoilNum).TimeElapsed = 0.0;
            TESCoil(TESCoilNum).IceFracRemain = 0.0;
            TESCoil(TESCoilNum).IceFracRemainLastTimestep = 0.0;
            TESCoil(TESCoilNum).FluidTankTempFinal = TESCoil(TESCoilNum).RatedFluidTankTemp;
            TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep = TESCoil(TESCoilNum).RatedFluidTankTemp;
            TESCoil(TESCoilNum).ElecCoolingPower = 0.0;     // electric power for cooling [W]
            TESCoil(TESCoilNum).ElecCoolingEnergy = 0.0;    // electric energy for cooling [J], metered
            TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;   // evaporator coil total cooling rate [W]
            TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0; // evaporatory coil total cooling energy [J], metered
            TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
            TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;
            TESCoil(TESCoilNum).RuntimeFraction = 0.0;
            TESCoil(TESCoilNum).ElectColdWeatherPower = 0.0;  // electric power for cold weather protection [W]
            TESCoil(TESCoilNum).ElectColdWeatherEnergy = 0.0; // electric energy for cold weather protection [J], metered
            TESCoil(TESCoilNum).ElectEvapCondBasinHeaterPower = 0.0;
            TESCoil(TESCoilNum).ElectEvapCondBasinHeaterEnergy = 0.0;

            MyEnvrnFlag(TESCoilNum) = false;
        }

        if (!BeginEnvrnFlag) MyEnvrnFlag(TESCoilNum) = true;

        if (MyWarmupFlag(TESCoilNum) && (!WarmupFlag)) {
            // reset to initial condition once warm up is over.
            TESCoil(TESCoilNum).IceFracRemain = 0.0;
            TESCoil(TESCoilNum).IceFracRemainLastTimestep = 0.0;
            TESCoil(TESCoilNum).FluidTankTempFinal = TESCoil(TESCoilNum).RatedFluidTankTemp;
            TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep = TESCoil(TESCoilNum).RatedFluidTankTemp;
            MyWarmupFlag(TESCoilNum) = false;
        }

        if (WarmupFlag) MyWarmupFlag(TESCoilNum) = true;

        // determine control mode
        if (GetCurrentScheduleValue(TESCoil(TESCoilNum).AvailSchedNum) != 0.0) {
            if (TESCoil(TESCoilNum).ModeControlType == ScheduledOpModes) {
                tmpSchedValue = GetCurrentScheduleValue(TESCoil(TESCoilNum).ControlModeSchedNum);
                TESCoil(TESCoilNum).CurControlMode = int(tmpSchedValue);
                // check if value is valid
                {
                    auto const SELECT_CASE_var(TESCoil(TESCoilNum).CurControlMode);
                    if ((SELECT_CASE_var == OffMode) || (SELECT_CASE_var == CoolingOnlyMode) || (SELECT_CASE_var == CoolingAndChargeMode) ||
                        (SELECT_CASE_var == CoolingAndDischargeMode) || (SELECT_CASE_var == ChargeOnlyMode) ||
                        (SELECT_CASE_var == DischargeOnlyMode)) {
                        // do nothing, these are okay
                    } else {
                        TESCoil(TESCoilNum).CurControlMode = OffMode;
                        if (TESCoil(TESCoilNum).ControlModeErrorIndex == 0) {
                            ShowSevereMessage("InitTESCoil: Invalid control schedule value for operating mode");
                            ShowContinueError("Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " + TESCoil(TESCoilNum).Name);
                            ShowContinueError("Value returned from schedule =" + RoundSigDigits(tmpSchedValue, 8));
                            ShowContinueError("Operating mode will be set to Off, and the simulation continues");
                        }
                        ShowRecurringSevereErrorAtEnd("InitTESCoil: Invalid control schedule value for TES operating mode, set to Off",
                                                      TESCoil(TESCoilNum).ControlModeErrorIndex,
                                                      tmpSchedValue,
                                                      tmpSchedValue);
                    }
                }

            } else if (TESCoil(TESCoilNum).ModeControlType == EMSActuatedOpModes) {
                if (TESCoil(TESCoilNum).EMSControlModeOn) {
                    TESCoil(TESCoilNum).CurControlMode = std::floor(TESCoil(TESCoilNum).EMSControlModeValue);
                    // check if value is valid
                    {
                        auto const SELECT_CASE_var(TESCoil(TESCoilNum).CurControlMode);
                        if (SELECT_CASE_var == OffMode) {

                        } else if (SELECT_CASE_var == CoolingOnlyMode) {
                            if (!(TESCoil(TESCoilNum).CoolingOnlyModeIsAvailable)) {
                                ShowSevereMessage("InitTESCoil: Invalid control value for operating mode");
                                ShowContinueError("Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " + TESCoil(TESCoilNum).Name);
                                ShowContinueError("Value returned from EMS indicates Cooling Only Mode but that mode is not available.");
                                ShowContinueError("Operating mode will be set to Off, and the simulation continues");
                                TESCoil(TESCoilNum).CurControlMode = OffMode;
                            }
                        } else if (SELECT_CASE_var == CoolingAndChargeMode) {
                            if (!(TESCoil(TESCoilNum).CoolingAndChargeModeAvailable)) {
                                ShowSevereMessage("InitTESCoil: Invalid control value for operating mode");
                                ShowContinueError("Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " + TESCoil(TESCoilNum).Name);
                                ShowContinueError("Value returned from EMS indicates Cooling And Charge Mode but that mode is not available.");
                                ShowContinueError("Operating mode will be set to Off, and the simulation continues");
                                TESCoil(TESCoilNum).CurControlMode = OffMode;
                            }
                        } else if (SELECT_CASE_var == CoolingAndDischargeMode) {
                            if (!(TESCoil(TESCoilNum).CoolingAndDischargeModeAvailable)) {
                                ShowSevereMessage("InitTESCoil: Invalid control value for operating mode");
                                ShowContinueError("Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " + TESCoil(TESCoilNum).Name);
                                ShowContinueError("Value returned from EMS indicates Cooling And Discharge Mode but that mode is not available.");
                                ShowContinueError("Operating mode will be set to Off, and the simulation continues");
                                TESCoil(TESCoilNum).CurControlMode = OffMode;
                            }
                        } else if (SELECT_CASE_var == ChargeOnlyMode) {
                            if (!(TESCoil(TESCoilNum).ChargeOnlyModeAvailable)) {
                                ShowSevereMessage("InitTESCoil: Invalid control value for operating mode");
                                ShowContinueError("Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " + TESCoil(TESCoilNum).Name);
                                ShowContinueError("Value returned from EMS indicates Charge Only Mode but that mode is not available.");
                                ShowContinueError("Operating mode will be set to Off, and the simulation continues");
                                TESCoil(TESCoilNum).CurControlMode = OffMode;
                            }
                        } else if (SELECT_CASE_var == DischargeOnlyMode) {
                            if (!(TESCoil(TESCoilNum).DischargeOnlyModeAvailable)) {
                                ShowSevereMessage("InitTESCoil: Invalid control value for operating mode");
                                ShowContinueError("Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " + TESCoil(TESCoilNum).Name);
                                ShowContinueError("Value returned from EMS indicates Discharge Only Mode but that mode is not available.");
                                ShowContinueError("Operating mode will be set to Off, and the simulation continues");
                                TESCoil(TESCoilNum).CurControlMode = OffMode;
                            }
                        } else {
                            TESCoil(TESCoilNum).CurControlMode = OffMode;
                            if (TESCoil(TESCoilNum).ControlModeErrorIndex == 0) {
                                ShowSevereMessage("InitTESCoil: Invalid control value for operating mode");
                                ShowContinueError("Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = " + TESCoil(TESCoilNum).Name);
                                ShowContinueError("Value returned from EMS =" + RoundSigDigits(TESCoil(TESCoilNum).EMSControlModeValue, 8));
                                ShowContinueError("Operating mode will be set to Off, and the simulation continues");
                            }
                            ShowRecurringSevereErrorAtEnd("InitTESCoil: Invalid control schedule value for TES operating mode, set to Off",
                                                          TESCoil(TESCoilNum).ControlModeErrorIndex,
                                                          TESCoil(TESCoilNum).EMSControlModeValue,
                                                          TESCoil(TESCoilNum).EMSControlModeValue);
                        }
                    }
                } else {
                    TESCoil(TESCoilNum).CurControlMode = OffMode;
                }
            }
        } else {
            TESCoil(TESCoilNum).CurControlMode = OffMode;
        }

        TESCoil(TESCoilNum).QdotPlant = 0.0;   // heat exchange rate for plant connection to TES tank [W]
        TESCoil(TESCoilNum).Q_Plant = 0.0;     //  heat exchange energy for plant connection to TES tank [J]
        TESCoil(TESCoilNum).QdotAmbient = 0.0; // heat exchange rate for skin losses/gains for TES tank to surroundings [W]
        TESCoil(TESCoilNum).Q_Ambient = 0.0;   // heat exchange enegy for skin losses/gains for TES tank to surroundings [J]
        TESCoil(TESCoilNum).QdotTES = 0.0;     // heat exchange rate by mechanical systems to charge or discharge TES [W]
        TESCoil(TESCoilNum).Q_TES = 0.0;       // heat exchange energy by mechanical systems to charge or discharge TES [J]

        // dynamic calculated data
        TESCoil(TESCoilNum).ElecCoolingPower = 0.0;     // electric power for cooling [W]
        TESCoil(TESCoilNum).ElecCoolingEnergy = 0.0;    // electric energy for cooling [J], metered
        TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;   // evaporator coil total cooling rate [W]
        TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0; // evaporatory coil total cooling energy [J], metered
        TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
        TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
        TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
        TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;
        TESCoil(TESCoilNum).RuntimeFraction = 0.0;
        TESCoil(TESCoilNum).CondenserRuntimeFraction = 0.0;
        TESCoil(TESCoilNum).ElectColdWeatherPower = 0.0;  // electric power for cold weather protection [W]
        TESCoil(TESCoilNum).ElectColdWeatherEnergy = 0.0; // electric energy for cold weather protection [J], metered
        TESCoil(TESCoilNum).ElectEvapCondBasinHeaterPower = 0.0;
        TESCoil(TESCoilNum).ElectEvapCondBasinHeaterEnergy = 0.0;
    }

    void SizeTESCoil(int &TESCoilNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // Using/Aliasing
        using namespace DataSizing;
        using DataAirSystems::PrimaryAirSystem;
        using DataEnvironment::StdRhoAir;
        using ReportSizingManager::ReportSizingOutput;
        using namespace OutputReportPredefined;
        using CurveManager::CurveValue;
        using DataGlobals::SecInHour;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeTESCoil ");
        static std::string const calcTESWaterStorageTank("CalcTESWaterStorageTank");
        Real64 const FluidTankSizingDeltaT(10.0);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MixTemp;
        Real64 MixHumRat;
        Real64 MixEnth;
        Real64 MixWetBulb;
        Real64 SupTemp;
        Real64 SupHumRat;
        Real64 SupEnth;
        Real64 OutTemp;
        Real64 OutAirFrac;
        Real64 VolFlowRate;
        Real64 CoolCapAtPeak;
        Real64 TotCapTempModFac;
        int TimeStepNumAtMax;
        int DDNum;
        Real64 rhoair;
        Real64 rho;
        Real64 deltaT;
        Real64 Cp;

        if (TESCoil(TESCoilNum).RatedEvapAirVolFlowRate == AutoSize) {

            if (CurSysNum > 0) {
                CheckSysSizing("Coil:Cooling:DX:SingleSpeed:ThermalStorage", TESCoil(TESCoilNum).Name);
                if (CurOASysNum > 0) {
                    TESCoil(TESCoilNum).RatedEvapAirVolFlowRate = FinalSysSizing(CurSysNum).DesOutAirVolFlow;
                } else {
                    TESCoil(TESCoilNum).RatedEvapAirVolFlowRate = FinalSysSizing(CurSysNum).DesMainVolFlow;
                }
            } else if (CurZoneEqNum > 0) {
                CheckZoneSizing("Coil:Cooling:DX:SingleSpeed:ThermalStorage", TESCoil(TESCoilNum).Name);
                TESCoil(TESCoilNum).RatedEvapAirVolFlowRate =
                    max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow);
            }

            if (TESCoil(TESCoilNum).RatedEvapAirVolFlowRate < SmallAirVolFlow) {
                TESCoil(TESCoilNum).RatedEvapAirVolFlowRate = 0.0;
            }
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Rated Evaporator Air Flow Rate [m3/s]",
                               TESCoil(TESCoilNum).RatedEvapAirVolFlowRate);
        }

        TESCoil(TESCoilNum).RatedEvapAirMassFlowRate = StdRhoAir * TESCoil(TESCoilNum).RatedEvapAirVolFlowRate;

        if (TESCoil(TESCoilNum).CondenserAirVolumeFlow == AutoCalculate) {
            TESCoil(TESCoilNum).CondenserAirVolumeFlow =
                TESCoil(TESCoilNum).RatedEvapAirVolFlowRate * TESCoil(TESCoilNum).CondenserAirFlowSizingFactor;
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Condenser Air Flow Rate [m3/s]",
                               TESCoil(TESCoilNum).CondenserAirVolumeFlow);
        }

        TESCoil(TESCoilNum).CondenserAirMassFlow = StdRhoAir * TESCoil(TESCoilNum).CondenserAirVolumeFlow;

        if (TESCoil(TESCoilNum).CoolingOnlyRatedTotCap == AutoSize) {
            if (CurSysNum > 0) {
                CheckSysSizing("Coil:Cooling:DX:SingleSpeed:ThermalStorage", TESCoil(TESCoilNum).Name);
                VolFlowRate = TESCoil(TESCoilNum).RatedEvapAirVolFlowRate;
                if (VolFlowRate >= SmallAirVolFlow) {
                    if (CurOASysNum > 0) { // coil is in the OA stream
                        MixTemp = FinalSysSizing(CurSysNum).OutTempAtCoolPeak;
                        MixHumRat = FinalSysSizing(CurSysNum).OutHumRatAtCoolPeak;
                        SupTemp = FinalSysSizing(CurSysNum).PrecoolTemp;
                        SupHumRat = FinalSysSizing(CurSysNum).PrecoolHumRat;
                    } else { // coil is on the main air loop
                        //     MixTemp = FinalSysSizing(CurSysNum)%MixTempAtCoolPeak
                        //     MixHumRat = FinalSysSizing(CurSysNum)%MixHumRatAtCoolPeak
                        SupTemp = FinalSysSizing(CurSysNum).CoolSupTemp;
                        SupHumRat = FinalSysSizing(CurSysNum).CoolSupHumRat;
                        if (PrimaryAirSystem(CurSysNum).NumOACoolCoils == 0) { // there is no precooling of the OA stream
                            MixTemp = FinalSysSizing(CurSysNum).MixTempAtCoolPeak;
                            MixHumRat = FinalSysSizing(CurSysNum).MixHumRatAtCoolPeak;
                        } else { // there is precooling of OA stream
                            if (VolFlowRate > 0.0) {
                                OutAirFrac = FinalSysSizing(CurSysNum).DesOutAirVolFlow / VolFlowRate;
                            } else {
                                OutAirFrac = 1.0;
                            }
                            OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                            MixTemp =
                                OutAirFrac * FinalSysSizing(CurSysNum).PrecoolTemp + (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).RetTempAtCoolPeak;
                            MixHumRat = OutAirFrac * FinalSysSizing(CurSysNum).PrecoolHumRat +
                                        (1.0 - OutAirFrac) * FinalSysSizing(CurSysNum).RetHumRatAtCoolPeak;
                        }
                    }
                    OutTemp = FinalSysSizing(CurSysNum).OutTempAtCoolPeak;
                    rhoair = PsyRhoAirFnPbTdbW(StdBaroPress, MixTemp, MixHumRat, RoutineName);
                    MixEnth = PsyHFnTdbW(MixTemp, MixHumRat);
                    MixWetBulb = PsyTwbFnTdbWPb(MixTemp, MixHumRat, StdBaroPress, RoutineName);
                    SupEnth = PsyHFnTdbW(SupTemp, SupHumRat);
                    TotCapTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingOnlyCapFTempCurve, MixWetBulb, OutTemp);
                    CoolCapAtPeak = max(0.0, (rhoair * VolFlowRate * (MixEnth - SupEnth)));
                    if (TotCapTempModFac > 0.0) {
                        TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = CoolCapAtPeak / TotCapTempModFac;
                    } else {
                        TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = CoolCapAtPeak;
                    }

                } else {
                    TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = 0.0;
                }
            } else if (CurZoneEqNum > 0) {
                CheckZoneSizing("Coil:Cooling:DX:SingleSpeed:ThermalStorage", TESCoil(TESCoilNum).Name);
                VolFlowRate = TESCoil(TESCoilNum).RatedEvapAirVolFlowRate;
                if (VolFlowRate >= SmallAirVolFlow) {
                    if (ZoneEqDXCoil) {
                        if (ZoneEqSizing(CurZoneEqNum).OAVolFlow > 0.0) {
                            MixTemp = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp;
                            MixHumRat = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat;
                        } else {
                            MixTemp = FinalZoneSizing(CurZoneEqNum).ZoneRetTempAtCoolPeak;
                            MixHumRat = FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtCoolPeak;
                        }
                    } else {
                        MixTemp = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp;
                        MixHumRat = FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat;
                    }
                    SupTemp = FinalZoneSizing(CurZoneEqNum).CoolDesTemp;
                    SupHumRat = FinalZoneSizing(CurZoneEqNum).CoolDesHumRat;
                    TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax;
                    DDNum = FinalZoneSizing(CurZoneEqNum).CoolDDNum;
                    if (DDNum > 0 && TimeStepNumAtMax > 0) {
                        OutTemp = DesDayWeath(DDNum).Temp(TimeStepNumAtMax);
                    } else {
                        OutTemp = 0.0;
                    }
                    rhoair = PsyRhoAirFnPbTdbW(StdBaroPress, MixTemp, MixHumRat, RoutineName);
                    MixEnth = PsyHFnTdbW(MixTemp, MixHumRat);
                    MixWetBulb = PsyTwbFnTdbWPb(MixTemp, MixHumRat, StdBaroPress, RoutineName);
                    SupEnth = PsyHFnTdbW(SupTemp, SupHumRat);
                    TotCapTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingOnlyCapFTempCurve, MixWetBulb, OutTemp);
                    CoolCapAtPeak = max(0.0, (rhoair * VolFlowRate * (MixEnth - SupEnth)));
                    if (TotCapTempModFac > 0.0) {
                        TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = CoolCapAtPeak / TotCapTempModFac;
                    } else {
                        TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = CoolCapAtPeak;
                    }

                } else {
                    TESCoil(TESCoilNum).CoolingOnlyRatedTotCap = 0.0;
                }
            }

            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Cooling Only Mode Rated Total Evaporator Cooling Capacity [W]",
                               TESCoil(TESCoilNum).CoolingOnlyRatedTotCap);
        }

        if (TESCoil(TESCoilNum).CoolingAndChargeModeAvailable && (TESCoil(TESCoilNum).CoolingAndChargeRatedTotCap == AutoCalculate)) {
            TESCoil(TESCoilNum).CoolingAndChargeRatedTotCap =
                TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TESCoil(TESCoilNum).CoolingAndChargeRatedTotCapSizingFactor;
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Cooling And Charge Mode Rated Total Evaporator Cooling Capacity [W]",
                               TESCoil(TESCoilNum).CoolingAndChargeRatedTotCap);
        }

        if (TESCoil(TESCoilNum).CoolingAndChargeModeAvailable && (TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCap == AutoCalculate)) {
            TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCap =
                TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCapSizingFactor;
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Cooling And Charge Mode Rated Storage Charging Capacity [W]",
                               TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCap);
        }

        if (TESCoil(TESCoilNum).CoolingAndDischargeModeAvailable && (TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCap == AutoCalculate)) {
            TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCap =
                TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCapSizingFactor;
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Cooling And Discharge Mode Rated Total Evaporator Cooling Capacity [W]",
                               TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCap);
        }

        if (TESCoil(TESCoilNum).CoolingAndDischargeModeAvailable && (TESCoil(TESCoilNum).CoolingAndDischargeRatedDischargeCap == AutoCalculate)) {
            TESCoil(TESCoilNum).CoolingAndDischargeRatedDischargeCap =
                TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TESCoil(TESCoilNum).CoolingAndDischargeRatedDischargeCapSizingFactor;
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Cooling And Discharge Mode Rated Storage Discharging Capacity [W]",
                               TESCoil(TESCoilNum).CoolingAndDischargeRatedDischargeCap);
        }

        if (TESCoil(TESCoilNum).ChargeOnlyModeAvailable && (TESCoil(TESCoilNum).ChargeOnlyRatedCapacity == AutoCalculate)) {
            TESCoil(TESCoilNum).ChargeOnlyRatedCapacity =
                TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TESCoil(TESCoilNum).ChargeOnlyRatedCapacitySizingFactor;
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Charge Only Mode Rated Storage Charging Capacity [W]",
                               TESCoil(TESCoilNum).ChargeOnlyRatedCapacity);
        }

        if (TESCoil(TESCoilNum).DischargeOnlyModeAvailable && (TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap == AutoCalculate)) {
            TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap =
                TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCapSizingFactor;
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Discharge Only Mode Rated Storage Discharging Capacity [W]",
                               TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap);
        }

        if ((TESCoil(TESCoilNum).StorageMedia == FluidBased) && (TESCoil(TESCoilNum).FluidStorageVolume == AutoCalculate)) {
            // for fluid tanks, assume a 10C deltaT or diff between max and min, whichever is smaller
            deltaT = min(FluidTankSizingDeltaT, (TESCoil(TESCoilNum).MaximumFluidTankTempLimit - TESCoil(TESCoilNum).MinimumFluidTankTempLimit));

            rho = GetDensityGlycol(
                TESCoil(TESCoilNum).StorageFluidName, DataGlobals::CWInitConvTemp, TESCoil(TESCoilNum).StorageFluidIndex, calcTESWaterStorageTank);
            Cp = GetSpecificHeatGlycol(
                TESCoil(TESCoilNum).StorageFluidName, DataGlobals::CWInitConvTemp, TESCoil(TESCoilNum).StorageFluidIndex, calcTESWaterStorageTank);
            if (TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap > 0.0 && TESCoil(TESCoilNum).DischargeOnlyModeAvailable) {
                TESCoil(TESCoilNum).FluidStorageVolume =
                    (TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap * TESCoil(TESCoilNum).StorageCapacitySizingFactor * SecInHour) /
                    (rho * Cp * deltaT);
            } else {
                TESCoil(TESCoilNum).FluidStorageVolume =
                    (TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TESCoil(TESCoilNum).StorageCapacitySizingFactor * SecInHour) / (rho * Cp * deltaT);
            }
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Fluid Storage Volume [m3]",
                               TESCoil(TESCoilNum).FluidStorageVolume);
        }
        if ((TESCoil(TESCoilNum).StorageMedia == IceBased) && (TESCoil(TESCoilNum).IceStorageCapacity == AutoCalculate)) {

            if (TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap > 0.0 && TESCoil(TESCoilNum).DischargeOnlyModeAvailable) {
                TESCoil(TESCoilNum).IceStorageCapacity =
                    TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap * TESCoil(TESCoilNum).StorageCapacitySizingFactor * SecInHour;
            } else {
                TESCoil(TESCoilNum).IceStorageCapacity =
                    TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TESCoil(TESCoilNum).StorageCapacitySizingFactor * SecInHour;
            }
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Ice Storage Capacity [GJ]",
                               TESCoil(TESCoilNum).IceStorageCapacity / 1.e+09);
        }

        if ((TESCoil(TESCoilNum).CondenserType == EvapCooled) && (TESCoil(TESCoilNum).EvapCondPumpElecNomPower == AutoSize)) {
            TESCoil(TESCoilNum).EvapCondPumpElecNomPower = TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * 0.004266; // w/w (15 w/ton)
            ReportSizingOutput("Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                               TESCoil(TESCoilNum).Name,
                               "Evaporative Condenser Pump Rated Power Consumption [W]",
                               TESCoil(TESCoilNum).EvapCondPumpElecNomPower);
        }

        PreDefTableEntry(pdchCoolCoilType, TESCoil(TESCoilNum).Name, "Coil:Cooling:DX:SingleSpeed:ThermalStorage");

        PreDefTableEntry(pdchCoolCoilTotCap, TESCoil(TESCoilNum).Name, TESCoil(TESCoilNum).CoolingOnlyRatedTotCap);
        PreDefTableEntry(
            pdchCoolCoilSensCap, TESCoil(TESCoilNum).Name, TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TESCoil(TESCoilNum).CoolingOnlyRatedSHR);
        PreDefTableEntry(pdchCoolCoilLatCap,
                         TESCoil(TESCoilNum).Name,
                         TESCoil(TESCoilNum).CoolingOnlyRatedTotCap -
                             TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TESCoil(TESCoilNum).CoolingOnlyRatedSHR);
        PreDefTableEntry(pdchCoolCoilSHR, TESCoil(TESCoilNum).Name, TESCoil(TESCoilNum).CoolingOnlyRatedSHR);
        PreDefTableEntry(pdchCoolCoilNomEff, TESCoil(TESCoilNum).Name, TESCoil(TESCoilNum).CoolingOnlyRatedCOP);
    }

    void CalcTESCoilOffMode(int const TESCoilNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 StandbyAncillaryPower;

        // coil is off; just pass through conditions
        if (GetCurrentScheduleValue(TESCoil(TESCoilNum).AvailSchedNum) != 0.0) {
            StandbyAncillaryPower = TESCoil(TESCoilNum).AncillaryControlsPower;
        } else {
            StandbyAncillaryPower = 0.0;
        }

        TESCoil(TESCoilNum).ElecCoolingPower = StandbyAncillaryPower;
        TESCoil(TESCoilNum).ElecCoolingEnergy = StandbyAncillaryPower * TimeStepSys * SecInHour;

        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);

        Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
        Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
        Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
        Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
        Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
        TESCoil(TESCoilNum).RuntimeFraction = 0.0;
        TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
        TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
        TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
        TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
        TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
        TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;

        TESCoil(TESCoilNum).QdotTES = 0.0;
        TESCoil(TESCoilNum).Q_TES = 0.0;

        UpdateTEStorage(TESCoilNum);

        TESCoil(TESCoilNum).CondInletTemp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;

        UpdateColdWeatherProtection(TESCoilNum);

        if (TESCoil(TESCoilNum).CondenserType == EvapCooled) {
            UpdateEvaporativeCondenserBasinHeater(TESCoilNum);
        }
    }

    void CalcTESCoilCoolingOnlyMode(int const TESCoilNum, int const EP_UNUSED(FanOpMode), Real64 const PartLoadRatio)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;
        using DataHVACGlobals::TimeStepSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIter(30);
        Real64 const RelaxationFactor(0.4);
        Real64 const Tolerance(0.1);
        static std::string const RoutineName("CalcTESCoilCoolingOnlyMode");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
        // Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
        Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
        // For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
        Real64 CondAirMassFlow;           // Condenser air mass flow rate [kg/s]
        Real64 CondInletEnthalpy;         // condenser inlet enthalpy [J/kg]
        Real64 CondAirSidePressure;       // Outdoor barometric pressure at condenser (Pa)
        Real64 QdotCond;                  // condenser total heat rejection rate [W]
        Real64 CondOutletEnthalpy;        // condesner outlet enthalpy [J/kg]
        Real64 OutdoorDryBulb;            // outdoor air dry bulb local variable [C]
        Real64 OutdoorHumRat;             // outdoor air humidity ratio local [kg/kg]
        Real64 OutdoorWetBulb;            // outdoor air wetbulb local [C]
        Real64 EvapAirMassFlow;           // local for evaporator air mass flow [kg/s]
        Real64 EvapInletDryBulb;          // evaporator inlet air drybulb [C]
        Real64 EvapInletHumRat;           // evaporator inlet air humidity ratio [kg/kg]
        Real64 EvapInletWetBulb;          // evaporator inlet air wetbulb [C]
        Real64 EvapInletEnthalpy;         // evaporator inlet air enthalpy [J/kg]
        Real64 AirMassFlowRatio;          // evaporator inlet air mass flow divided by design mass flow [ ]
        Real64 TotCapTempModFac;          // total coolin capacity modification factor due to temps []
        Real64 TotCapFlowModFac;          // Total cooling capacity modification factor due to flow []
        Real64 TotCap;                    // total cooling capacity
        Real64 SHRTempFac;                // sensible heat ratio modification factor due to temps []
        Real64 SHRFlowFac;                // sensible heat ratio modification factor due to flow []
        Real64 SHR;                       // sensible heat ratio
        Real64 PLF;                       // part load factor
        Real64 RuntimeFraction;           // compressor running time divided by full time of timestep.
        Real64 FullLoadOutAirEnth;        // evaporator outlet full load enthalpy [J/kg]
        Real64 hTinwout;                  // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
        Real64 FullLoadOutAirHumRat;      // evaporator outlet humidity ratio at full load
        Real64 FullLoadOutAirTemp;        // evaporator outlet air temperature at full load [C]
        Real64 EvapOutletAirEnthalpy;     // evaporator outlet air enthalpy [J/kg]
        Real64 EvapOutletAirHumRat;       // evaporator outlet air humidity ratio [kg/kg]
        Real64 EvapOutletAirTemp;         // evaporator outlet drybulb [C]
        Real64 EIRTempModFac;             // energy input ratio modification factor due to temperatures []
        Real64 EIRFlowModFac;             // energy input ratio modification factor due to flow []
        Real64 EIR;                       // energy input ratio
        Real64 ElecCoolingPower;          // compressor electric power
        Real64 MinAirHumRat;              // minimum air humidity ratio
        Real64 PartLoadOutAirEnth;        // local leaving enthalpy at part load
        Real64 PartLoadDryCoilOutAirTemp; // local leaving drybulb if coil were dry
        bool CoilMightBeDry;
        int Counter;
        bool Converged;
        Real64 DryCoilTestEvapInletHumRat;
        Real64 DryCoilTestEvapInletWetBulb;
        Real64 hADP;
        Real64 tADP;
        Real64 wADP;
        Real64 hTinwADP;
        Real64 SHRadp;
        Real64 werror;

        // first deal with condenser
        if (TESCoil(TESCoilNum).CondenserType == AirCooled) {
            CondAirSidePressure = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
            if (CondAirSidePressure == DefaultNodeValues.Press) {
                CondInletTemp = OutDryBulbTemp;
                CondInletHumRat = OutHumRat;
                CondAirSidePressure = OutBaroPress;
            } else {
                CondInletTemp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
                CondInletHumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            }
            CondAirMassFlow = TESCoil(TESCoilNum).CondenserAirMassFlow;
        } else if (TESCoil(TESCoilNum).CondenserType == EvapCooled) {
            CondAirSidePressure = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
            if (CondAirSidePressure == DefaultNodeValues.Press) {
                OutdoorDryBulb = OutDryBulbTemp;
                OutdoorHumRat = OutHumRat;
                CondAirSidePressure = OutBaroPress;
                OutdoorWetBulb = OutWetBulbTemp;
            } else {
                OutdoorDryBulb = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
                OutdoorHumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
                OutdoorWetBulb = PsyTwbFnTdbWPb(OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, RoutineName);
            }
            CondAirMassFlow = TESCoil(TESCoilNum).CondenserAirMassFlow;
            // direct evap cool model
            CondInletTemp = OutdoorWetBulb + (OutdoorDryBulb - OutdoorWetBulb) * (1.0 - TESCoil(TESCoilNum).EvapCondEffect);
            CondInletHumRat = PsyWFnTdbTwbPb(CondInletTemp, OutdoorWetBulb, CondAirSidePressure, RoutineName);
        }

        EvapAirMassFlow = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
        EvapInletDryBulb = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
        EvapInletHumRat = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
        EvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, EvapInletHumRat, OutBaroPress, RoutineName);
        EvapInletEnthalpy = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Enthalpy;
        CoilMightBeDry = false;

        if ((EvapAirMassFlow > SmallMassFlow) && (PartLoadRatio > 0.0)) { // coil is running

            AirMassFlowRatio = EvapAirMassFlow / TESCoil(TESCoilNum).RatedEvapAirMassFlowRate;
            TotCapTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingOnlyCapFTempCurve, EvapInletWetBulb, CondInletTemp);
            TotCapTempModFac = max(0.0, TotCapTempModFac); // could warn if negative, DXcoil does
            TotCapFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingOnlyCapFFlowCurve, AirMassFlowRatio);
            TotCapFlowModFac = max(0.0, TotCapFlowModFac); // could warn if negative, DXcoil does
            TotCap = TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TotCapTempModFac * TotCapFlowModFac;
            // now see if coil might be running dry
            PartLoadOutAirEnth = EvapInletEnthalpy - (TotCap * PartLoadRatio) / EvapAirMassFlow;
            PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat);
            if (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(PartLoadOutAirEnth, OutBaroPress, RoutineName)) {
                CoilMightBeDry = true;
                // find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
                DryCoilTestEvapInletHumRat = EvapInletHumRat;
                DryCoilTestEvapInletWetBulb = EvapInletWetBulb;
                Counter = 0;
                Converged = false;
                while (!Converged) {
                    TotCapTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingOnlyCapFTempCurve, DryCoilTestEvapInletWetBulb, CondInletTemp);
                    TotCapTempModFac = max(0.0, TotCapTempModFac); // could warn if negative, DXcoil does
                    TotCapFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingOnlyCapFFlowCurve, AirMassFlowRatio);
                    TotCapFlowModFac = max(0.0, TotCapFlowModFac); // could warn if negative, DXcoil does
                    TotCap = TESCoil(TESCoilNum).CoolingOnlyRatedTotCap * TotCapTempModFac * TotCapFlowModFac;

                    // coil bypass factor = 0.0
                    hADP = EvapInletEnthalpy - (TotCap / EvapAirMassFlow);
                    tADP = PsyTsatFnHPb(hADP, OutBaroPress, RoutineName);
                    wADP = min(EvapInletHumRat, PsyWFnTdbH(tADP, hADP, RoutineName));
                    hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP);
                    if ((EvapInletEnthalpy - hADP) > 1.e-10) {
                        SHRadp = min((hTinwADP - hADP) / (EvapInletEnthalpy - hADP), 1.0);
                    } else {
                        SHRadp = 1.0;
                    }

                    if ((wADP > DryCoilTestEvapInletHumRat) || (Counter >= 1 && Counter < MaxIter)) {
                        if (DryCoilTestEvapInletHumRat <= 0.0) DryCoilTestEvapInletHumRat = 0.00001;
                        werror = (DryCoilTestEvapInletHumRat - wADP) / DryCoilTestEvapInletHumRat;

                        DryCoilTestEvapInletHumRat = RelaxationFactor * wADP + (1.0 - RelaxationFactor) * DryCoilTestEvapInletHumRat;
                        DryCoilTestEvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, DryCoilTestEvapInletHumRat, OutBaroPress, RoutineName);

                        ++Counter;
                        if (std::abs(werror) <= Tolerance) {
                            Converged = true;
                        } else {
                            Converged = false;
                        }
                    } else {
                        Converged = true;
                    }
                }
            }

            SHRTempFac = CurveValue(TESCoil(TESCoilNum).CoolingOnlySHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb);
            SHRFlowFac = CurveValue(TESCoil(TESCoilNum).CoolingOnlySHRFFlowCurve, AirMassFlowRatio);
            SHR = TESCoil(TESCoilNum).CoolingOnlyRatedSHR * SHRTempFac * SHRFlowFac;
            SHR = min(SHR, 1.0); // warn maybe
            SHR = max(SHR, 0.0); // warn maybe
            if (CoilMightBeDry) {
                if ((EvapInletHumRat < DryCoilTestEvapInletHumRat) && (SHRadp > SHR)) { // coil is dry for sure
                    SHR = 1.0;
                } else if (SHRadp > SHR) {
                    SHR = SHRadp;
                }
            }
            PLF = CurveValue(TESCoil(TESCoilNum).CoolingOnlyPLFFPLRCurve, PartLoadRatio);
            if (PLF >= PartLoadRatio && PLF > 0.0) {
                RuntimeFraction = PartLoadRatio / PLF;
            } else {
                RuntimeFraction = 1.0; // warn maybe
            }
            //  Calculate full load output conditions
            FullLoadOutAirEnth = EvapInletEnthalpy - TotCap / EvapAirMassFlow;

            hTinwout = EvapInletEnthalpy - (1.0 - SHR) * (TotCap / EvapAirMassFlow);
            // The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
            FullLoadOutAirHumRat = PsyWFnTdbH(EvapInletDryBulb, hTinwout, RoutineName, true);
            FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth, FullLoadOutAirHumRat);
            // Check for saturation error and modify temperature at constant enthalpy
            if (FullLoadOutAirTemp < PsyTsatFnHPb(FullLoadOutAirEnth, OutBaroPress, RoutineName)) {
                FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth, OutBaroPress, RoutineName);
                FullLoadOutAirHumRat = PsyWFnTdbH(FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName);
            }

            // Continuous fan, cycling compressor
            EvapOutletAirEnthalpy = ((PartLoadRatio)*FullLoadOutAirEnth + (1.0 - (PartLoadRatio)) * EvapInletEnthalpy);
            EvapOutletAirHumRat = ((PartLoadRatio)*FullLoadOutAirHumRat + (1.0 - (PartLoadRatio)) * EvapInletHumRat);
            EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy, EvapOutletAirHumRat);
            if (EvapOutletAirTemp < PsyTsatFnHPb(EvapOutletAirEnthalpy, OutBaroPress, RoutineName)) {
                EvapOutletAirTemp = PsyTsatFnHPb(EvapOutletAirEnthalpy, OutBaroPress, RoutineName);
                EvapOutletAirHumRat = PsyWFnTdbH(EvapOutletAirTemp, EvapOutletAirEnthalpy, RoutineName);
            }
            // Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
            EIRTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingOnlyEIRFTempCurve, EvapInletWetBulb, CondInletTemp);
            EIRTempModFac = max(EIRTempModFac, 0.0);
            EIRFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingOnlyEIRFFlowCurve, AirMassFlowRatio);
            EIRFlowModFac = max(EIRFlowModFac, 0.0);
            EIR = EIRTempModFac * EIRFlowModFac / TESCoil(TESCoilNum).CoolingOnlyRatedCOP;

            ElecCoolingPower = TotCap * EIR * RuntimeFraction;

            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = EvapOutletAirTemp;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = EvapOutletAirHumRat;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy = EvapOutletAirEnthalpy;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = EvapAirMassFlow;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;

            // determine condenser leaving conditions
            QdotCond = TotCap * RuntimeFraction + ElecCoolingPower;
            Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = TESCoil(TESCoilNum).CondenserAirMassFlow;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = TESCoil(TESCoilNum).CondenserAirMassFlow;
            CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
            CondOutletEnthalpy = CondInletEnthalpy + QdotCond / TESCoil(TESCoilNum).CondenserAirMassFlow;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = PsyTdbFnHW(CondOutletEnthalpy, CondInletHumRat);
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = CondInletHumRat;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy = CondOutletEnthalpy;

            TESCoil(TESCoilNum).ElecCoolingPower = ElecCoolingPower + TESCoil(TESCoilNum).AncillaryControlsPower;
            TESCoil(TESCoilNum).ElecCoolingEnergy = TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * SecInHour;

            TESCoil(TESCoilNum).RuntimeFraction = RuntimeFraction;
            TESCoil(TESCoilNum).CondenserRuntimeFraction = RuntimeFraction;
            TESCoil(TESCoilNum).EvapTotCoolingRate = TotCap * RuntimeFraction; // double check this
            TESCoil(TESCoilNum).EvapTotCoolingEnergy = TotCap * RuntimeFraction * TimeStepSys * SecInHour;
            MinAirHumRat = min(Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat, Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat);
            TESCoil(TESCoilNum).EvapSensCoolingRate =
                EvapAirMassFlow * (PsyHFnTdbW(EvapInletDryBulb, MinAirHumRat) - PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat));
            if (TESCoil(TESCoilNum).EvapSensCoolingRate > TESCoil(TESCoilNum).EvapTotCoolingRate) {
                TESCoil(TESCoilNum).EvapSensCoolingRate = TESCoil(TESCoilNum).EvapTotCoolingRate;
            }
            TESCoil(TESCoilNum).EvapSensCoolingEnergy = TESCoil(TESCoilNum).EvapSensCoolingRate * TimeStepSys * SecInHour;
            TESCoil(TESCoilNum).EvapLatCoolingRate = TESCoil(TESCoilNum).EvapTotCoolingRate - TESCoil(TESCoilNum).EvapSensCoolingRate;
            TESCoil(TESCoilNum).EvapLatCoolingEnergy = TESCoil(TESCoilNum).EvapLatCoolingRate * TimeStepSys * SecInHour;

        } else { // coil is off; just pass through conditions
            TESCoil(TESCoilNum).ElecCoolingPower = TESCoil(TESCoilNum).AncillaryControlsPower;
            TESCoil(TESCoilNum).ElecCoolingEnergy = TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * SecInHour;
            TESCoil(TESCoilNum).RuntimeFraction = 0.0;
            TESCoil(TESCoilNum).CondenserRuntimeFraction = 0.0;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
                PsyHFnTdbW(Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);

            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
                PsyHFnTdbW(Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
            TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
            TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
            TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;
        }

        TESCoil(TESCoilNum).QdotTES = 0.0;
        TESCoil(TESCoilNum).Q_TES = 0.0;

        UpdateTEStorage(TESCoilNum);

        TESCoil(TESCoilNum).CondInletTemp = CondInletTemp;

        UpdateColdWeatherProtection(TESCoilNum);

        if (TESCoil(TESCoilNum).CondenserType == EvapCooled) {
            UpdateEvaporativeCondenserBasinHeater(TESCoilNum);
            UpdateEvaporativeCondenserWaterUse(TESCoilNum, CondInletHumRat, TESCoil(TESCoilNum).CondAirInletNodeNum);
        }
    }

    void CalcTESCoilCoolingAndChargeMode(int const TESCoilNum, int const EP_UNUSED(FanOpMode), Real64 const PartLoadRatio)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;
        using DataHVACGlobals::TimeStepSys;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIter(30);
        Real64 const RelaxationFactor(0.4);
        Real64 const Tolerance(0.1);
        static std::string const RoutineName("CalcTESCoilCoolingAndChargeMode");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
        // Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
        Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
        // For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
        Real64 CondAirMassFlow;       // Condenser air mass flow rate [kg/s]
        Real64 CondInletEnthalpy;     // condenser inlet enthalpy [J/kg]
        Real64 CondAirSidePressure;   // Outdoor barometric pressure at condenser (Pa)
        Real64 QdotCond;              // condenser total heat rejection rate [W]
        Real64 CondOutletEnthalpy;    // condesner outlet enthalpy [J/kg]
        Real64 OutdoorDryBulb;        // outdoor air dry bulb local variable [C]
        Real64 OutdoorHumRat;         // outdoor air humidity ratio local [kg/kg]
        Real64 OutdoorWetBulb;        // outdoor air wetbulb local [C]
        Real64 EvapAirMassFlow;       // local for evaporator air mass flow [kg/s]
        Real64 EvapInletDryBulb;      // evaporator inlet air drybulb [C]
        Real64 EvapInletHumRat;       // evaporator inlet air humidity ratio [kg/kg]
        Real64 EvapInletWetBulb;      // evaporator inlet air wetbulb [C]
        Real64 EvapInletEnthalpy;     // evaporator inlet air enthalpy [J/kg]
        Real64 AirMassFlowRatio;      // evaporator inlet air mass flow divided by design mass flow [ ]
        Real64 EvapTotCapTempModFac;  // total coolin capacity modification factor due to temps []
        Real64 EvapTotCapFlowModFac;  // Total cooling capacity modification factor due to flow []
        Real64 EvapTotCap;            // total cooling capacity
        Real64 SHRTempFac(0.0);       // sensible heat ratio modification factor due to temps []
        Real64 SHRFlowFac;            // sensible heat ratio modification factor due to flow []
        Real64 SHR;                   // sensible heat ratio
        Real64 PLF;                   // part load factor
        Real64 EvapRuntimeFraction;   // compressor running time divided by full time of timestep.
        Real64 FullLoadOutAirEnth;    // evaporator outlet full load enthalpy [J/kg]
        Real64 hTinwout;              // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
        Real64 FullLoadOutAirHumRat;  // evaporator outlet humidity ratio at full load
        Real64 FullLoadOutAirTemp;    // evaporator outlet air temperature at full load [C]
        Real64 EvapOutletAirEnthalpy; // evaporator outlet air enthalpy [J/kg]
        Real64 EvapOutletAirHumRat;   // evaporator outlet air humidity ratio [kg/kg]
        Real64 EvapOutletAirTemp;     // evaporator outlet drybulb [C]
        Real64 EIRTempModFac;         // energy input ratio modification factor due to temperatures []
        Real64 EIRFlowModFac;         // energy input ratio modification factor due to flow []
        Real64 EIR;                   // energy input ratio
        Real64 EvapElecCoolingPower;  // compressor electric power
        Real64 MinAirHumRat;          // minimum air humidity ratio
        Real64 sTES;                  // stat of Thermal energy storage [C or fraction of ice]
        bool TESCanBeCharged;
        Real64 rho;
        Real64 TankMass;        // Mass of fluid in tank (kg)
        Real64 CpTank;          // Specific heat of water in tank (J/kg K)
        Real64 QdotChargeLimit; // limit for charge cooling power to hit limit of storage.
        Real64 ChargeCapModFac;
        Real64 ChargeCapPLRModFac;
        Real64 TotChargeCap;
        Real64 ChargeEIRTempModFac;
        Real64 ChargeEIRFlowModFac;
        Real64 ChargeEIR;
        Real64 ChargeElectricCoolingPower;
        Real64 ChargeRuntimeFraction;
        Real64 PartLoadOutAirEnth;        // local leaving enthalpy at part load
        Real64 PartLoadDryCoilOutAirTemp; // local leaving drybulb if coil were dry
        bool CoilMightBeDry;
        int Counter;
        bool Converged;
        Real64 DryCoilTestEvapInletHumRat;
        Real64 DryCoilTestEvapInletWetBulb;
        Real64 hADP;
        Real64 tADP;
        Real64 wADP;
        Real64 hTinwADP;
        Real64 SHRadp;
        Real64 werror;

        // first deal with condenser
        if (TESCoil(TESCoilNum).CondenserType == AirCooled) {
            CondAirSidePressure = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
            if (CondAirSidePressure == DefaultNodeValues.Press) {
                CondInletTemp = OutDryBulbTemp;
                CondInletHumRat = OutHumRat;
                CondAirSidePressure = OutBaroPress;
            } else {
                CondInletTemp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
                CondInletHumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            }
            CondAirMassFlow = TESCoil(TESCoilNum).CondenserAirMassFlow;
        } else if (TESCoil(TESCoilNum).CondenserType == EvapCooled) {
            CondAirSidePressure = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
            if (CondAirSidePressure == DefaultNodeValues.Press) {
                OutdoorDryBulb = OutDryBulbTemp;
                OutdoorHumRat = OutHumRat;
                CondAirSidePressure = OutBaroPress;
                OutdoorWetBulb = OutWetBulbTemp;
            } else {
                OutdoorDryBulb = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
                OutdoorHumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
                OutdoorWetBulb = PsyTwbFnTdbWPb(OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, RoutineName);
            }
            CondAirMassFlow = TESCoil(TESCoilNum).CondenserAirMassFlow;
            // direct evap cool model
            CondInletTemp = OutdoorWetBulb + (OutdoorDryBulb - OutdoorWetBulb) * (1.0 - TESCoil(TESCoilNum).EvapCondEffect);
            CondInletHumRat = PsyWFnTdbTwbPb(CondInletTemp, OutdoorWetBulb, CondAirSidePressure, RoutineName);
        }

        EvapAirMassFlow = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
        EvapInletDryBulb = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
        EvapInletHumRat = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
        EvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, EvapInletHumRat, OutBaroPress, RoutineName);
        EvapInletEnthalpy = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Enthalpy;
        CoilMightBeDry = false;

        if (TESCoil(TESCoilNum).StorageMedia == FluidBased) {
            sTES = TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
            if ((sTES > TESCoil(TESCoilNum).MinimumFluidTankTempLimit) && (sTES < TESCoil(TESCoilNum).MaximumFluidTankTempLimit)) {
                TESCanBeCharged = true;
                // find charge limit to reach limits
                rho = GetDensityGlycol(TESCoil(TESCoilNum).StorageFluidName, sTES, TESCoil(TESCoilNum).StorageFluidIndex, RoutineName);
                TankMass = rho * TESCoil(TESCoilNum).FluidStorageVolume;
                CpTank = GetSpecificHeatGlycol(TESCoil(TESCoilNum).StorageFluidName, sTES, TESCoil(TESCoilNum).StorageFluidIndex, RoutineName);
                // simple linear approximation of DT/Dt term in McpDT/Dt
                QdotChargeLimit = TankMass * CpTank * (sTES - TESCoil(TESCoilNum).MinimumFluidTankTempLimit) / (TimeStepSys * SecInHour);
            } else {
                TESCanBeCharged = false;
            }
        } else if (TESCoil(TESCoilNum).StorageMedia == IceBased) {
            sTES = TESCoil(TESCoilNum).IceFracRemainLastTimestep;
            if (sTES < 1.0) {
                TESCanBeCharged = true;
                // find charge limit to reach limit
                QdotChargeLimit = (1.0 - sTES) * TESCoil(TESCoilNum).IceStorageCapacity / (TimeStepSys * SecInHour);
            } else {
                TESCanBeCharged = false;
            }
        }

        if ((EvapAirMassFlow > SmallMassFlow) && (PartLoadRatio > 0.0)) { // coil is running

            AirMassFlowRatio = EvapAirMassFlow / TESCoil(TESCoilNum).RatedEvapAirMassFlowRate;
            EvapTotCapTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeCoolingCapFTempCurve, EvapInletWetBulb, CondInletTemp, sTES);
            EvapTotCapTempModFac = max(0.0, EvapTotCapTempModFac); // could warn if negative, DXcoil does
            EvapTotCapFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeCoolingCapFFlowCurve, AirMassFlowRatio);
            EvapTotCapFlowModFac = max(0.0, EvapTotCapFlowModFac); // could warn if negative, DXcoil does
            EvapTotCap = TESCoil(TESCoilNum).CoolingAndChargeRatedTotCap * EvapTotCapTempModFac * EvapTotCapFlowModFac;
            // now see if coil is running dry
            PartLoadOutAirEnth = EvapInletEnthalpy - (EvapTotCap * PartLoadRatio) / EvapAirMassFlow;
            PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat);
            if (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(PartLoadOutAirEnth, OutBaroPress, RoutineName)) {
                CoilMightBeDry = true;
                // find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
                DryCoilTestEvapInletHumRat = EvapInletHumRat;
                DryCoilTestEvapInletWetBulb = EvapInletWetBulb;
                Counter = 0;
                Converged = false;
                while (!Converged) {
                    EvapTotCapTempModFac =
                        CurveValue(TESCoil(TESCoilNum).CoolingAndChargeCoolingCapFTempCurve, DryCoilTestEvapInletWetBulb, CondInletTemp, sTES);
                    EvapTotCapTempModFac = max(0.0, EvapTotCapTempModFac); // could warn if negative, DXcoil does
                    EvapTotCapFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeCoolingCapFFlowCurve, AirMassFlowRatio);
                    EvapTotCapFlowModFac = max(0.0, EvapTotCapFlowModFac); // could warn if negative, DXcoil does
                    EvapTotCap = TESCoil(TESCoilNum).CoolingAndChargeRatedTotCap * EvapTotCapTempModFac * EvapTotCapFlowModFac;
                    // coil bypass factor = 0.0
                    hADP = EvapInletEnthalpy - (EvapTotCap / EvapAirMassFlow);
                    tADP = PsyTsatFnHPb(hADP, OutBaroPress, RoutineName);
                    wADP = min(EvapInletHumRat, PsyWFnTdbH(tADP, hADP, RoutineName));
                    hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP);
                    if ((EvapInletEnthalpy - hADP) > 1.e-10) {
                        SHRadp = min((hTinwADP - hADP) / (EvapInletEnthalpy - hADP), 1.0);
                    } else {
                        SHRadp = 1.0;
                    }

                    if ((wADP > DryCoilTestEvapInletHumRat) || (Counter >= 1 && Counter < MaxIter)) {
                        if (DryCoilTestEvapInletHumRat <= 0.0) DryCoilTestEvapInletHumRat = 0.00001;
                        werror = (DryCoilTestEvapInletHumRat - wADP) / DryCoilTestEvapInletHumRat;

                        DryCoilTestEvapInletHumRat = RelaxationFactor * wADP + (1.0 - RelaxationFactor) * DryCoilTestEvapInletHumRat;
                        DryCoilTestEvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, DryCoilTestEvapInletHumRat, OutBaroPress, RoutineName);

                        ++Counter;
                        if (std::abs(werror) <= Tolerance) {
                            Converged = true;
                        } else {
                            Converged = false;
                        }
                    } else {
                        Converged = true;
                    }
                }
            }
            {
                if (CurveManager::PerfCurve(TESCoil(TESCoilNum).CoolingAndChargeSHRFTempCurve).NumDims == 2) {
                    SHRTempFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeSHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb);
                } else {
                    SHRTempFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeSHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb, sTES);
                }
            }
            SHRFlowFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeSHRFFlowCurve, AirMassFlowRatio);
            SHR = TESCoil(TESCoilNum).CoolingAndChargeRatedSHR * SHRTempFac * SHRFlowFac;
            SHR = min(SHR, 1.0); // warn maybe
            SHR = max(SHR, 0.0); // warn maybe
            if (CoilMightBeDry) {
                if ((EvapInletHumRat < DryCoilTestEvapInletHumRat) && (SHRadp > SHR)) { // coil is dry for sure
                    SHR = 1.0;
                } else if (SHRadp > SHR) {
                    SHR = SHRadp;
                }
            }
            PLF = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeCoolingPLFFPLRCurve, PartLoadRatio);
            if (PLF >= PartLoadRatio && PLF > 0.0) {
                EvapRuntimeFraction = PartLoadRatio / PLF;
            } else {
                EvapRuntimeFraction = 1.0; // warn maybe
            }

            // Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
            EIRTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeCoolingEIRFTempCurve, EvapInletWetBulb, CondInletTemp, sTES);
            EIRTempModFac = max(EIRTempModFac, 0.0);
            EIRFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeCoolingEIRFFlowCurve, AirMassFlowRatio);
            EIRFlowModFac = max(EIRFlowModFac, 0.0);
            EIR = EIRTempModFac * EIRFlowModFac / TESCoil(TESCoilNum).CoolingAndChargeCoolingRatedCOP;

            EvapElecCoolingPower = EvapTotCap * EIR * EvapRuntimeFraction;

            if (TESCanBeCharged) {
                ChargeCapModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeChargingCapFTempCurve, EvapInletWetBulb, CondInletTemp, sTES);
                ChargeCapModFac = max(0.0, ChargeCapModFac);

                ChargeCapPLRModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeChargingCapFEvapPLRCurve, PartLoadRatio);
                ChargeCapPLRModFac = max(0.0, ChargeCapPLRModFac);

                TotChargeCap = TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCap * ChargeCapModFac * ChargeCapPLRModFac;
                if (TotChargeCap > QdotChargeLimit) {
                    ChargeRuntimeFraction = QdotChargeLimit / TotChargeCap;
                    TotChargeCap = min(TotChargeCap, QdotChargeLimit);
                } else {
                    ChargeRuntimeFraction = 1.0;
                }
                ChargeEIRTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeChargingEIRFTempCurve, EvapInletWetBulb, CondInletTemp, sTES);
                ChargeEIRTempModFac = max(0.0, ChargeEIRTempModFac);
                ChargeEIRFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeChargingEIRFFLowCurve, AirMassFlowRatio);
                ChargeEIRFlowModFac = max(0.0, ChargeEIRFlowModFac);
                ChargeEIR = (ChargeEIRTempModFac * ChargeEIRFlowModFac) / TESCoil(TESCoilNum).CoolingAndChargeChargingRatedCOP;
                ChargeElectricCoolingPower = TotChargeCap * ChargeEIR;
                TESCoil(TESCoilNum).QdotTES = -TotChargeCap;
            } else {
                TotChargeCap = 0.0;
                ChargeElectricCoolingPower = 0.0;
                TESCoil(TESCoilNum).QdotTES = 0.0;
                ChargeRuntimeFraction = 0.0;
            }

            //  Calculate full load output conditions
            FullLoadOutAirEnth = EvapInletEnthalpy - EvapTotCap / EvapAirMassFlow;

            hTinwout = EvapInletEnthalpy - (1.0 - SHR) * (EvapTotCap / EvapAirMassFlow);
            // The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
            FullLoadOutAirHumRat = PsyWFnTdbH(EvapInletDryBulb, hTinwout, RoutineName, true);
            FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth, FullLoadOutAirHumRat);
            // Check for saturation error and modify temperature at constant enthalpy
            if (FullLoadOutAirTemp < PsyTsatFnHPb(FullLoadOutAirEnth, OutBaroPress, RoutineName)) {
                FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth, OutBaroPress, RoutineName);
                FullLoadOutAirHumRat = PsyWFnTdbH(FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName);
            }

            // Continuous fan, cycling compressor
            EvapOutletAirEnthalpy = ((PartLoadRatio)*FullLoadOutAirEnth + (1.0 - (PartLoadRatio)) * EvapInletEnthalpy);
            EvapOutletAirHumRat = ((PartLoadRatio)*FullLoadOutAirHumRat + (1.0 - (PartLoadRatio)) * EvapInletHumRat);
            EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy, EvapOutletAirHumRat);
            if (EvapOutletAirTemp < PsyTsatFnHPb(EvapOutletAirEnthalpy, OutBaroPress, RoutineName)) {
                EvapOutletAirTemp = PsyTsatFnHPb(EvapOutletAirEnthalpy, OutBaroPress, RoutineName);
                EvapOutletAirHumRat = PsyWFnTdbH(EvapOutletAirTemp, EvapOutletAirEnthalpy, RoutineName);
            }

            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = EvapOutletAirTemp;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = EvapOutletAirHumRat;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy = EvapOutletAirEnthalpy;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = EvapAirMassFlow;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;

            // determine condenser leaving conditions
            QdotCond = EvapTotCap * EvapRuntimeFraction + EvapElecCoolingPower + TotChargeCap + ChargeElectricCoolingPower;
            Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = TESCoil(TESCoilNum).CondenserAirMassFlow;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = TESCoil(TESCoilNum).CondenserAirMassFlow;
            CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
            CondOutletEnthalpy = CondInletEnthalpy + QdotCond / TESCoil(TESCoilNum).CondenserAirMassFlow;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = PsyTdbFnHW(CondOutletEnthalpy, CondInletHumRat);
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = CondInletHumRat;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy = CondOutletEnthalpy;

            TESCoil(TESCoilNum).ElecCoolingPower = EvapElecCoolingPower + ChargeElectricCoolingPower + TESCoil(TESCoilNum).AncillaryControlsPower;
            TESCoil(TESCoilNum).ElecCoolingEnergy = TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * SecInHour;

            TESCoil(TESCoilNum).RuntimeFraction = EvapRuntimeFraction;
            if (ChargeRuntimeFraction > 0.0) {
                TESCoil(TESCoilNum).CondenserRuntimeFraction = max(ChargeRuntimeFraction, EvapRuntimeFraction);
            } else {
                TESCoil(TESCoilNum).CondenserRuntimeFraction = EvapRuntimeFraction;
            }

            TESCoil(TESCoilNum).EvapTotCoolingRate = EvapTotCap * EvapRuntimeFraction; // double check this
            TESCoil(TESCoilNum).EvapTotCoolingEnergy = EvapTotCap * EvapRuntimeFraction * TimeStepSys * SecInHour;
            MinAirHumRat = min(Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat, Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat);
            TESCoil(TESCoilNum).EvapSensCoolingRate =
                EvapAirMassFlow * (PsyHFnTdbW(EvapInletDryBulb, MinAirHumRat) - PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat));
            if (TESCoil(TESCoilNum).EvapSensCoolingRate > TESCoil(TESCoilNum).EvapTotCoolingRate) {
                TESCoil(TESCoilNum).EvapSensCoolingRate = TESCoil(TESCoilNum).EvapTotCoolingRate;
            }
            TESCoil(TESCoilNum).EvapSensCoolingEnergy = TESCoil(TESCoilNum).EvapSensCoolingRate * TimeStepSys * SecInHour;
            TESCoil(TESCoilNum).EvapLatCoolingRate = TESCoil(TESCoilNum).EvapTotCoolingRate - TESCoil(TESCoilNum).EvapSensCoolingRate;
            TESCoil(TESCoilNum).EvapLatCoolingEnergy = TESCoil(TESCoilNum).EvapLatCoolingRate * TimeStepSys * SecInHour;

        } else {                   // Evap off, but may still charge
            if (TESCanBeCharged) { // coil is running to charge but not to cool at evaporator
                AirMassFlowRatio = EvapAirMassFlow / TESCoil(TESCoilNum).RatedEvapAirMassFlowRate;
                ChargeCapModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeChargingCapFTempCurve, EvapInletWetBulb, CondInletTemp, sTES);
                ChargeCapModFac = max(0.0, ChargeCapModFac);

                ChargeCapPLRModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeChargingCapFEvapPLRCurve, PartLoadRatio);
                ChargeCapPLRModFac = max(0.0, ChargeCapPLRModFac);

                TotChargeCap = TESCoil(TESCoilNum).CoolingAndChargeRatedChargeCap * ChargeCapModFac * ChargeCapPLRModFac;
                if (TotChargeCap > QdotChargeLimit) {
                    ChargeRuntimeFraction = QdotChargeLimit / TotChargeCap;
                    TotChargeCap = min(TotChargeCap, QdotChargeLimit);
                } else {
                    ChargeRuntimeFraction = 1.0;
                }
                ChargeEIRTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeChargingEIRFTempCurve, EvapInletWetBulb, CondInletTemp, sTES);
                ChargeEIRTempModFac = max(0.0, ChargeEIRTempModFac);
                ChargeEIRFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndChargeChargingEIRFFLowCurve, AirMassFlowRatio);
                ChargeEIRFlowModFac = max(0.0, ChargeEIRFlowModFac);
                ChargeEIR = (ChargeEIRTempModFac * ChargeEIRFlowModFac) / TESCoil(TESCoilNum).CoolingAndChargeChargingRatedCOP;
                ChargeElectricCoolingPower = TotChargeCap * ChargeEIR;
                TESCoil(TESCoilNum).QdotTES = -TotChargeCap;
            } else {
                TotChargeCap = 0.0;
                ChargeElectricCoolingPower = 0.0;
                TESCoil(TESCoilNum).QdotTES = 0.0;
                ChargeRuntimeFraction = 0.0;
            }

            TESCoil(TESCoilNum).ElecCoolingPower = ChargeElectricCoolingPower + TESCoil(TESCoilNum).AncillaryControlsPower;
            TESCoil(TESCoilNum).ElecCoolingEnergy = TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * SecInHour;

            TESCoil(TESCoilNum).RuntimeFraction = 0.0;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
                PsyHFnTdbW(Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);

            TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
            TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
            TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;

            if (TotChargeCap == 0.0) {
                Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
                Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
                Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
                Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
                Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
                    PsyHFnTdbW(Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
                TESCoil(TESCoilNum).CondenserRuntimeFraction = 0.0;
            } else {

                // determine condenser leaving conditions
                QdotCond = TotChargeCap + ChargeElectricCoolingPower;
                Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = TESCoil(TESCoilNum).CondenserAirMassFlow;
                Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = TESCoil(TESCoilNum).CondenserAirMassFlow;
                CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
                CondOutletEnthalpy = CondInletEnthalpy + QdotCond / TESCoil(TESCoilNum).CondenserAirMassFlow;
                Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = PsyTdbFnHW(CondOutletEnthalpy, CondInletHumRat);
                Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = CondInletHumRat;
                Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy = CondOutletEnthalpy;
                TESCoil(TESCoilNum).CondenserRuntimeFraction = 1.0;
            }
        }

        TESCoil(TESCoilNum).QdotTES = -TotChargeCap;
        TESCoil(TESCoilNum).Q_TES = TESCoil(TESCoilNum).QdotTES * TimeStepSys * SecInHour;

        UpdateTEStorage(TESCoilNum);

        TESCoil(TESCoilNum).CondInletTemp = CondInletTemp;

        UpdateColdWeatherProtection(TESCoilNum);

        if (TESCoil(TESCoilNum).CondenserType == EvapCooled) {
            UpdateEvaporativeCondenserBasinHeater(TESCoilNum);
            UpdateEvaporativeCondenserWaterUse(TESCoilNum, CondInletHumRat, TESCoil(TESCoilNum).CondAirInletNodeNum);
        }
    }

    void CalcTESCoilCoolingAndDischargeMode(int const TESCoilNum, int const EP_UNUSED(FanOpMode), Real64 const PartLoadRatio)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;
        using DataHVACGlobals::TimeStepSys;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIter(30);
        Real64 const RelaxationFactor(0.4);
        Real64 const Tolerance(0.1);
        static std::string const RoutineName("CalcTESCoilCoolingAndDischargeMode");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
        // Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
        Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
        // For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
        Real64 CondAirMassFlow;     // Condenser air mass flow rate [kg/s]
        Real64 CondInletEnthalpy;   // condenser inlet enthalpy [J/kg]
        Real64 CondAirSidePressure; // Outdoor barometric pressure at condenser (Pa)
        Real64 CondOutletEnthalpy;  // condesner outlet enthalpy [J/kg]
        Real64 OutdoorDryBulb;      // outdoor air dry bulb local variable [C]
        Real64 OutdoorHumRat;       // outdoor air humidity ratio local [kg/kg]
        Real64 OutdoorWetBulb;      // outdoor air wetbulb local [C]
        Real64 EvapAirMassFlow;     // local for evaporator air mass flow [kg/s]
        Real64 EvapInletDryBulb;    // evaporator inlet air drybulb [C]
        Real64 EvapInletHumRat;     // evaporator inlet air humidity ratio [kg/kg]
        Real64 EvapInletWetBulb;    // evaporator inlet air wetbulb [C]
        Real64 EvapInletEnthalpy;   // evaporator inlet air enthalpy [J/kg]
        Real64 sTES;                // stat of Thermal energy storage [C or fraction of ice]
        bool TESHasSomeCharge;      // some charge available for discharge
        Real64 rho;
        Real64 TankMass;             // Mass of fluid in tank (kg)
        Real64 CpTank;               // Specific heat of water in tank (J/kg K)
        Real64 QdotDischargeLimit;   // limit for charge cooling power to hit limit of storage.
        Real64 AirMassFlowRatio;     // evaporator inlet air mass flow divided by design mass flow [ ]
        Real64 EvapTotCapTempModFac; // total coolin capacity modification factor due to temps []
        Real64 EvapTotCapFlowModFac; // Total cooling capacity modification factor due to flow []
        Real64 EvapTotCap;           // total cooling capacity
        Real64 SHRTempFac(0.0);      // sensible heat ratio modification factor due to temps []
        Real64 SHRFlowFac;           // sensible heat ratio modification factor due to flow []
        Real64 SHR;                  // sensible heat ratio
        Real64 PLF;                  // part load factor
        Real64 EvapRuntimeFraction;  // compressor running time divided by full time of timestep.
        Real64 EIRTempModFac;        // energy input ratio modification factor due to temperatures []
        Real64 EIRFlowModFac;        // energy input ratio modification factor due to flow []
        Real64 EIR;                  // energy input ratio
        Real64 DischargePLF;
        Real64 DischargeRuntimeFraction;
        Real64 TotDischargeCap;
        Real64 DischargeCapTempModFac;
        Real64 DischargeCapFlowModFac;
        Real64 DischargeEIRTempModFac;
        Real64 DischargeEIRFlowModFac;
        Real64 DischargeEIR;
        Real64 EvapElecCoolingPower; // compressor electric power
        Real64 DischargeElectricCoolingPower;
        Real64 TotCap;
        Real64 FullLoadOutAirEnth;        // evaporator outlet full load enthalpy [J/kg]
        Real64 hTinwout;                  // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
        Real64 FullLoadOutAirHumRat;      // evaporator outlet humidity ratio at full load
        Real64 FullLoadOutAirTemp;        // evaporator outlet air temperature at full load [C]
        Real64 EvapOutletAirEnthalpy;     // evaporator outlet air enthalpy [J/kg]
        Real64 EvapOutletAirHumRat;       // evaporator outlet air humidity ratio [kg/kg]
        Real64 EvapOutletAirTemp;         // evaporator outlet drybulb [C]
        Real64 QdotCond;                  // heat rejection rate at condenser [W]
        Real64 MinAirHumRat;              // minimum air humidity ratio
        Real64 PartLoadOutAirEnth;        // local leaving enthalpy at part load
        Real64 PartLoadDryCoilOutAirTemp; // local leaving drybulb if coil were dry
        bool CoilMightBeDry;
        int Counter;
        bool Converged;
        Real64 DryCoilTestEvapInletHumRat;
        Real64 DryCoilTestEvapInletWetBulb;
        Real64 hADP;
        Real64 tADP;
        Real64 wADP;
        Real64 hTinwADP;
        Real64 SHRadp;
        Real64 werror;

        // first deal with condenser
        if (TESCoil(TESCoilNum).CondenserType == AirCooled) {
            CondAirSidePressure = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
            if (CondAirSidePressure == DefaultNodeValues.Press) {
                CondInletTemp = OutDryBulbTemp;
                CondInletHumRat = OutHumRat;
                CondAirSidePressure = OutBaroPress;
            } else {
                CondInletTemp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
                CondInletHumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            }
            CondAirMassFlow = TESCoil(TESCoilNum).CondenserAirMassFlow;
        } else if (TESCoil(TESCoilNum).CondenserType == EvapCooled) {
            CondAirSidePressure = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
            if (CondAirSidePressure == DefaultNodeValues.Press) {
                OutdoorDryBulb = OutDryBulbTemp;
                OutdoorHumRat = OutHumRat;
                CondAirSidePressure = OutBaroPress;
                OutdoorWetBulb = OutWetBulbTemp;
            } else {
                OutdoorDryBulb = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
                OutdoorHumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
                OutdoorWetBulb = PsyTwbFnTdbWPb(OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, RoutineName);
            }
            CondAirMassFlow = TESCoil(TESCoilNum).CondenserAirMassFlow;
            // direct evap cool model
            CondInletTemp = OutdoorWetBulb + (OutdoorDryBulb - OutdoorWetBulb) * (1.0 - TESCoil(TESCoilNum).EvapCondEffect);
            CondInletHumRat = PsyWFnTdbTwbPb(CondInletTemp, OutdoorWetBulb, CondAirSidePressure, RoutineName);
        }
        EvapAirMassFlow = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
        EvapInletDryBulb = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
        EvapInletHumRat = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
        EvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, EvapInletHumRat, OutBaroPress, RoutineName);
        EvapInletEnthalpy = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Enthalpy;
        CoilMightBeDry = false;

        if (TESCoil(TESCoilNum).StorageMedia == FluidBased) {
            sTES = TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
            if ((sTES >= TESCoil(TESCoilNum).MinimumFluidTankTempLimit) && (sTES < TESCoil(TESCoilNum).MaximumFluidTankTempLimit)) {
                TESHasSomeCharge = true;
                rho = GetDensityGlycol(TESCoil(TESCoilNum).StorageFluidName, sTES, TESCoil(TESCoilNum).StorageFluidIndex, RoutineName);
                TankMass = rho * TESCoil(TESCoilNum).FluidStorageVolume;
                CpTank = GetSpecificHeatGlycol(TESCoil(TESCoilNum).StorageFluidName, sTES, TESCoil(TESCoilNum).StorageFluidIndex, RoutineName);
                // simple linear approximation of DT/Dt term in McpDT/Dt
                QdotDischargeLimit = TankMass * CpTank * (TESCoil(TESCoilNum).MaximumFluidTankTempLimit - sTES) / (TimeStepSys * SecInHour);
            } else {
                TESHasSomeCharge = false;
            }
        } else if (TESCoil(TESCoilNum).StorageMedia == IceBased) {
            sTES = TESCoil(TESCoilNum).IceFracRemainLastTimestep;
            if (sTES > 0.0) {
                TESHasSomeCharge = true;
                // discharge limit
                QdotDischargeLimit = (sTES)*TESCoil(TESCoilNum).IceStorageCapacity / (TimeStepSys * SecInHour);
            } else {
                TESHasSomeCharge = false;
            }
        }

        if ((EvapAirMassFlow > SmallMassFlow) && (PartLoadRatio > 0.0)) { // coil is running

            AirMassFlowRatio = EvapAirMassFlow / TESCoil(TESCoilNum).RatedEvapAirMassFlowRate;
            EvapTotCapTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeCoolingCapFTempCurve, EvapInletWetBulb, CondInletTemp, sTES);
            EvapTotCapTempModFac = max(0.0, EvapTotCapTempModFac); // could warn if negative, DXcoil does
            EvapTotCapFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeCoolingCapFFlowCurve, AirMassFlowRatio);
            EvapTotCapFlowModFac = max(0.0, EvapTotCapFlowModFac); // could warn if negative, DXcoil does
            EvapTotCap = TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCap * EvapTotCapTempModFac * EvapTotCapFlowModFac;
            // now see if coil is running dry
            PartLoadOutAirEnth = EvapInletEnthalpy - (EvapTotCap * PartLoadRatio) / EvapAirMassFlow;
            PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat);
            if (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(PartLoadOutAirEnth, OutBaroPress, RoutineName)) {
                CoilMightBeDry = true;
                // find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
                DryCoilTestEvapInletHumRat = EvapInletHumRat;
                DryCoilTestEvapInletWetBulb = EvapInletWetBulb;
                Counter = 0;
                Converged = false;
                while (!Converged) {
                    EvapTotCapTempModFac =
                        CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeCoolingCapFTempCurve, DryCoilTestEvapInletWetBulb, CondInletTemp, sTES);
                    EvapTotCapTempModFac = max(0.0, EvapTotCapTempModFac); // could warn if negative, DXcoil does
                    EvapTotCapFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeCoolingCapFFlowCurve, AirMassFlowRatio);
                    EvapTotCapFlowModFac = max(0.0, EvapTotCapFlowModFac); // could warn if negative, DXcoil does
                    EvapTotCap = TESCoil(TESCoilNum).CoolingAndDischargeRatedTotCap * EvapTotCapTempModFac * EvapTotCapFlowModFac;
                    // coil bypass factor = 0.0
                    hADP = EvapInletEnthalpy - (EvapTotCap / EvapAirMassFlow);
                    tADP = PsyTsatFnHPb(hADP, OutBaroPress, RoutineName);
                    wADP = min(EvapInletHumRat, PsyWFnTdbH(tADP, hADP, RoutineName));
                    hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP);
                    if ((EvapInletEnthalpy - hADP) > 1.e-10) {
                        SHRadp = min((hTinwADP - hADP) / (EvapInletEnthalpy - hADP), 1.0);
                    } else {
                        SHRadp = 1.0;
                    }

                    if ((wADP > DryCoilTestEvapInletHumRat) || (Counter >= 1 && Counter < MaxIter)) {
                        if (DryCoilTestEvapInletHumRat <= 0.0) DryCoilTestEvapInletHumRat = 0.00001;
                        werror = (DryCoilTestEvapInletHumRat - wADP) / DryCoilTestEvapInletHumRat;

                        DryCoilTestEvapInletHumRat = RelaxationFactor * wADP + (1.0 - RelaxationFactor) * DryCoilTestEvapInletHumRat;
                        DryCoilTestEvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, DryCoilTestEvapInletHumRat, OutBaroPress, RoutineName);

                        ++Counter;
                        if (std::abs(werror) <= Tolerance) {
                            Converged = true;
                        } else {
                            Converged = false;
                        }
                    } else {
                        Converged = true;
                    }
                }
            }
            {
                if (CurveManager::PerfCurve(TESCoil(TESCoilNum).CoolingAndDischargeSHRFTempCurve).NumDims == 2) {
                    SHRTempFac = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeSHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb);
                } else {
                    SHRTempFac = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeSHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb, sTES);
                }
            }
            SHRFlowFac = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeSHRFFlowCurve, AirMassFlowRatio);
            SHR = TESCoil(TESCoilNum).CoolingAndDischargeRatedSHR * SHRTempFac * SHRFlowFac;
            SHR = min(SHR, 1.0); // warn maybe
            SHR = max(SHR, 0.0); // warn maybe
            if (CoilMightBeDry) {
                if ((EvapInletHumRat < DryCoilTestEvapInletHumRat) && (SHRadp > SHR)) { // coil is dry for sure
                    SHR = 1.0;
                } else if (SHRadp > SHR) {
                    SHR = SHRadp;
                }
            }
            PLF = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeCoolingPLFFPLRCurve, PartLoadRatio);
            if (PLF >= PartLoadRatio && PLF > 0.0) {
                EvapRuntimeFraction = PartLoadRatio / PLF;
            } else {
                EvapRuntimeFraction = 1.0; // warn maybe
            }
            // Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
            EIRTempModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeCoolingEIRFTempCurve, EvapInletWetBulb, CondInletTemp, sTES);
            EIRTempModFac = max(EIRTempModFac, 0.0);
            EIRFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeCoolingEIRFFlowCurve, AirMassFlowRatio);
            EIRFlowModFac = max(EIRFlowModFac, 0.0);
            EIR = EIRTempModFac * EIRFlowModFac / TESCoil(TESCoilNum).CoolingAndDischargeCoolingRatedCOP;

            EvapElecCoolingPower = EvapTotCap * EIR * EvapRuntimeFraction;

            if (TESHasSomeCharge) {
                DischargeCapTempModFac =
                    CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeDischargingCapFTempCurve, EvapInletWetBulb, CondInletTemp, sTES);
                DischargeCapTempModFac = max(0.0, DischargeCapTempModFac);
                DischargeCapFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeDischargingCapFFlowCurve, AirMassFlowRatio);
                DischargeCapFlowModFac = max(0.0, DischargeCapFlowModFac);

                DischargePLF = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeDischargingCapFEvapPLRCurve, PartLoadRatio);
                if (DischargePLF >= PartLoadRatio && DischargePLF > 0.0) {
                    DischargeRuntimeFraction = PartLoadRatio / DischargePLF;
                } else {
                    DischargeRuntimeFraction = 1.0; // warn maybe
                }

                TotDischargeCap = TESCoil(TESCoilNum).CoolingAndDischargeRatedDischargeCap * DischargeCapTempModFac * DischargeCapFlowModFac *
                                  DischargeRuntimeFraction;
                if (TotDischargeCap > QdotDischargeLimit) {
                    TotDischargeCap = min(TotDischargeCap, QdotDischargeLimit);
                }
                DischargeEIRTempModFac =
                    CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeDischargingEIRFTempCurve, EvapInletWetBulb, CondInletTemp, sTES);
                DischargeEIRTempModFac = max(0.0, DischargeEIRTempModFac);
                DischargeEIRFlowModFac = CurveValue(TESCoil(TESCoilNum).CoolingAndDischargeDischargingEIRFFLowCurve, AirMassFlowRatio);
                DischargeEIRFlowModFac = max(0.0, DischargeEIRFlowModFac);

                DischargeEIR = (DischargeEIRTempModFac * DischargeEIRFlowModFac) / TESCoil(TESCoilNum).CoolingAndDischargeDischargingRatedCOP;
                DischargeElectricCoolingPower = TotDischargeCap * DischargeEIR * DischargeRuntimeFraction;
                TESCoil(TESCoilNum).QdotTES = TotDischargeCap;
            } else {
                TotDischargeCap = 0.0;
                DischargeRuntimeFraction = 0.0;
                DischargeElectricCoolingPower = 0.0;
                TESCoil(TESCoilNum).QdotTES = 0.0;
            }

            TotCap = EvapTotCap + TotDischargeCap;
            //  Calculate full load output conditions
            FullLoadOutAirEnth = EvapInletEnthalpy - TotCap / EvapAirMassFlow;

            hTinwout = EvapInletEnthalpy - (1.0 - SHR) * (TotCap / EvapAirMassFlow);
            // The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
            FullLoadOutAirHumRat = PsyWFnTdbH(EvapInletDryBulb, hTinwout, RoutineName, true);
            FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth, FullLoadOutAirHumRat);
            // Check for saturation error and modify temperature at constant enthalpy
            if (FullLoadOutAirTemp < PsyTsatFnHPb(FullLoadOutAirEnth, OutBaroPress, RoutineName)) {
                FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth, OutBaroPress, RoutineName);
                FullLoadOutAirHumRat = PsyWFnTdbH(FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName);
            }
            // Continuous fan, cycling compressor
            EvapOutletAirEnthalpy = ((PartLoadRatio)*FullLoadOutAirEnth + (1.0 - (PartLoadRatio)) * EvapInletEnthalpy);
            EvapOutletAirHumRat = ((PartLoadRatio)*FullLoadOutAirHumRat + (1.0 - (PartLoadRatio)) * EvapInletHumRat);
            EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy, EvapOutletAirHumRat);
            if (EvapOutletAirTemp < PsyTsatFnHPb(EvapOutletAirEnthalpy, OutBaroPress, RoutineName)) {
                EvapOutletAirTemp = PsyTsatFnHPb(EvapOutletAirEnthalpy, OutBaroPress, RoutineName);
                EvapOutletAirHumRat = PsyWFnTdbH(EvapOutletAirTemp, EvapOutletAirEnthalpy, RoutineName);
            }

            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = EvapOutletAirTemp;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = EvapOutletAirHumRat;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy = EvapOutletAirEnthalpy;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = EvapAirMassFlow;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;

            // determine condenser leaving conditions
            QdotCond = EvapTotCap * EvapRuntimeFraction + EvapElecCoolingPower;
            Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = TESCoil(TESCoilNum).CondenserAirMassFlow;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = TESCoil(TESCoilNum).CondenserAirMassFlow;
            CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
            CondOutletEnthalpy = CondInletEnthalpy + QdotCond / TESCoil(TESCoilNum).CondenserAirMassFlow;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = PsyTdbFnHW(CondOutletEnthalpy, CondInletHumRat);
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = CondInletHumRat;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy = CondOutletEnthalpy;

            TESCoil(TESCoilNum).ElecCoolingPower = EvapElecCoolingPower + DischargeElectricCoolingPower + TESCoil(TESCoilNum).AncillaryControlsPower;
            TESCoil(TESCoilNum).ElecCoolingEnergy = TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * SecInHour;
            TESCoil(TESCoilNum).RuntimeFraction =
                (EvapTotCap * EvapRuntimeFraction + TotDischargeCap * DischargeRuntimeFraction) / (EvapTotCap + TotDischargeCap);

            TESCoil(TESCoilNum).EvapTotCoolingRate = EvapTotCap * EvapRuntimeFraction + TotDischargeCap * DischargeRuntimeFraction;
            TESCoil(TESCoilNum).EvapTotCoolingEnergy = TESCoil(TESCoilNum).EvapTotCoolingRate * TimeStepSys * SecInHour;
            MinAirHumRat = min(Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat, Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat);
            TESCoil(TESCoilNum).EvapSensCoolingRate =
                EvapAirMassFlow * (PsyHFnTdbW(EvapInletDryBulb, MinAirHumRat) - PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat));
            if (TESCoil(TESCoilNum).EvapSensCoolingRate > TESCoil(TESCoilNum).EvapTotCoolingRate) {
                TESCoil(TESCoilNum).EvapSensCoolingRate = TESCoil(TESCoilNum).EvapTotCoolingRate;
            }
            TESCoil(TESCoilNum).EvapSensCoolingEnergy = TESCoil(TESCoilNum).EvapSensCoolingRate * TimeStepSys * SecInHour;
            TESCoil(TESCoilNum).EvapLatCoolingRate = TESCoil(TESCoilNum).EvapTotCoolingRate - TESCoil(TESCoilNum).EvapSensCoolingRate;
            TESCoil(TESCoilNum).EvapLatCoolingEnergy = TESCoil(TESCoilNum).EvapLatCoolingRate * TimeStepSys * SecInHour;

        } else { // coil is off; just pass through conditions
            TESCoil(TESCoilNum).QdotTES = 0.0;

            TESCoil(TESCoilNum).ElecCoolingPower = TESCoil(TESCoilNum).AncillaryControlsPower;
            TESCoil(TESCoilNum).ElecCoolingEnergy = TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * SecInHour;
            TESCoil(TESCoilNum).RuntimeFraction = 0.0;

            TESCoil(TESCoilNum).RuntimeFraction = 0.0;
            TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
            TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
            TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;

            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
                PsyHFnTdbW(Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);
            // nothing happens at condenser
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
                PsyHFnTdbW(Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
            TESCoil(TESCoilNum).CondInletTemp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
        }
        TESCoil(TESCoilNum).Q_TES = TESCoil(TESCoilNum).QdotTES * TimeStepSys * SecInHour;
        UpdateTEStorage(TESCoilNum);

        UpdateColdWeatherProtection(TESCoilNum);

        if (TESCoil(TESCoilNum).CondenserType == EvapCooled) {
            UpdateEvaporativeCondenserBasinHeater(TESCoilNum);
            UpdateEvaporativeCondenserWaterUse(TESCoilNum, CondInletHumRat, TESCoil(TESCoilNum).CondAirInletNodeNum);
        }
    }

    void CalcTESCoilChargeOnlyMode(int const TESCoilNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;
        using DataHVACGlobals::TimeStepSys;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcTESCoilChargeOnlyMode");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 sTES;          // local state of Thermal Energy Storage (C or ice fraction)
        Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
        // Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
        Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
        // For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
        Real64 CondAirMassFlow;      // Condenser air mass flow rate [kg/s]
        Real64 CondInletEnthalpy;    // condenser inlet enthalpy [J/kg]
        Real64 CondAirSidePressure;  // Outdoor barometric pressure at condenser (Pa)
        Real64 QdotCond;             // condenser total heat rejection rate [W]
        Real64 CondOutletEnthalpy;   // condesner outlet enthalpy [J/kg]
        Real64 OutdoorDryBulb;       // outdoor air dry bulb local variable [C]
        Real64 OutdoorHumRat;        // outdoor air humidity ratio local [kg/kg]
        Real64 OutdoorWetBulb;       // outdoor air wetbulb local [C]
        Real64 CapModFac;            // local capacity modifying factor
        Real64 TotCap;               // total cooling (charging) capacity
        Real64 EIRModFac;            // local energy input ratio modifying factor
        Real64 EIR;                  // energy input ratio
        Real64 ElecCoolingPower;     // compressor electric power
        bool TESCanBeCharged(false); // true if room for tank to be charged.
        Real64 QdotChargeLimit;      // limit for charge cooling power to hit limit of storage.
        Real64 rho;                  // density of fluid in tank (kg/m3)
        Real64 TankMass;             // Mass of fluid in tank (kg)
        Real64 CpTank;               // Specific heat of water in tank (J/kg K)

        // nothing happens at Evaporator
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
        Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);

        // first deal with condenser
        if (TESCoil(TESCoilNum).CondenserType == AirCooled) {
            CondAirSidePressure = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
            if (CondAirSidePressure == DefaultNodeValues.Press) {
                CondInletTemp = OutDryBulbTemp;
                CondInletHumRat = OutHumRat;
                CondAirSidePressure = OutBaroPress;
            } else {
                CondInletTemp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
                CondInletHumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            }
            CondAirMassFlow = TESCoil(TESCoilNum).CondenserAirMassFlow;
        } else if (TESCoil(TESCoilNum).CondenserType == EvapCooled) {
            CondAirSidePressure = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Press;
            if (CondAirSidePressure == DefaultNodeValues.Press) {
                OutdoorDryBulb = OutDryBulbTemp;
                OutdoorHumRat = OutHumRat;
                CondAirSidePressure = OutBaroPress;
                OutdoorWetBulb = OutWetBulbTemp;
            } else {
                OutdoorDryBulb = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
                OutdoorHumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
                OutdoorWetBulb = PsyTwbFnTdbWPb(OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, RoutineName);
            }
            CondAirMassFlow = TESCoil(TESCoilNum).CondenserAirMassFlow;
            // direct evap cool model
            CondInletTemp = OutdoorWetBulb + (OutdoorDryBulb - OutdoorWetBulb) * (1.0 - TESCoil(TESCoilNum).EvapCondEffect);
            CondInletHumRat = PsyWFnTdbTwbPb(CondInletTemp, OutdoorWetBulb, CondAirSidePressure, RoutineName);
        }

        if (TESCoil(TESCoilNum).StorageMedia == FluidBased) {
            sTES = TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
            if ((sTES > TESCoil(TESCoilNum).MinimumFluidTankTempLimit) && (sTES < TESCoil(TESCoilNum).MaximumFluidTankTempLimit)) {
                TESCanBeCharged = true;
                // find charge limit to reach limits
                rho = GetDensityGlycol(TESCoil(TESCoilNum).StorageFluidName, sTES, TESCoil(TESCoilNum).StorageFluidIndex, RoutineName);
                TankMass = rho * TESCoil(TESCoilNum).FluidStorageVolume;
                CpTank = GetSpecificHeatGlycol(TESCoil(TESCoilNum).StorageFluidName, sTES, TESCoil(TESCoilNum).StorageFluidIndex, RoutineName);
                // simple linear approximation of DT/Dt term in McpDT/Dt
                QdotChargeLimit = TankMass * CpTank * (sTES - TESCoil(TESCoilNum).MinimumFluidTankTempLimit) / (TimeStepSys * SecInHour);
            } else {
                TESCanBeCharged = false;
            }
        } else if (TESCoil(TESCoilNum).StorageMedia == IceBased) {
            sTES = TESCoil(TESCoilNum).IceFracRemainLastTimestep;
            if (sTES < 1.0) {
                TESCanBeCharged = true;
                // find charge limit to reach limit
                QdotChargeLimit = (1.0 - sTES) * TESCoil(TESCoilNum).IceStorageCapacity / (TimeStepSys * SecInHour);
            } else {
                TESCanBeCharged = false;
            }
        } else {
            assert(false);
        }

        if (TESCanBeCharged) { // coil is running
            CapModFac = CurveValue(TESCoil(TESCoilNum).ChargeOnlyChargingCapFTempCurve, CondInletTemp, sTES);
            CapModFac = max(0.0, CapModFac);
            TotCap = TESCoil(TESCoilNum).ChargeOnlyRatedCapacity * CapModFac;
            if (TotCap > QdotChargeLimit) {
                TESCoil(TESCoilNum).RuntimeFraction = QdotChargeLimit / TotCap;
                TotCap = min(TotCap, QdotChargeLimit);
            } else {
                TESCoil(TESCoilNum).RuntimeFraction = 1.0;
            }
            EIRModFac = CurveValue(TESCoil(TESCoilNum).ChargeOnlyChargingEIRFTempCurve, CondInletTemp, sTES);
            EIRModFac = max(0.0, EIRModFac);
            EIR = EIRModFac / TESCoil(TESCoilNum).ChargeOnlyRatedCOP;
            ElecCoolingPower = TotCap * EIR;
            QdotCond = TotCap + ElecCoolingPower;
            Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = TESCoil(TESCoilNum).CondenserAirMassFlow;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = TESCoil(TESCoilNum).CondenserAirMassFlow;
            CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat);
            CondOutletEnthalpy = CondInletEnthalpy + QdotCond / TESCoil(TESCoilNum).CondenserAirMassFlow;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = PsyTdbFnHW(CondOutletEnthalpy, CondInletHumRat);
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = CondInletHumRat;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy = CondOutletEnthalpy;

            TESCoil(TESCoilNum).ElecCoolingPower = ElecCoolingPower + TESCoil(TESCoilNum).AncillaryControlsPower;
            TESCoil(TESCoilNum).ElecCoolingEnergy = TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * SecInHour;

            TESCoil(TESCoilNum).QdotTES = -TotCap; // negative for cooling

        } else { // not running
            TESCoil(TESCoilNum).ElecCoolingPower = TESCoil(TESCoilNum).AncillaryControlsPower;
            TESCoil(TESCoilNum).ElecCoolingEnergy = TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * SecInHour;
            TESCoil(TESCoilNum).RuntimeFraction = 0.0;
            TESCoil(TESCoilNum).QdotTES = 0.0;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
            Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
            Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
                PsyHFnTdbW(Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
        }
        TESCoil(TESCoilNum).Q_TES = TESCoil(TESCoilNum).QdotTES * TimeStepSys * SecInHour;

        TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
        TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
        TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
        TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
        TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
        TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;

        UpdateTEStorage(TESCoilNum);

        UpdateColdWeatherProtection(TESCoilNum);

        if (TESCoil(TESCoilNum).CondenserType == EvapCooled) {
            UpdateEvaporativeCondenserBasinHeater(TESCoilNum);
            UpdateEvaporativeCondenserWaterUse(TESCoilNum, CondInletHumRat, TESCoil(TESCoilNum).CondAirInletNodeNum);
        }
    }

    void CalcTESCoilDischargeOnlyMode(int const TESCoilNum, Real64 const PartLoadRatio)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;
        using DataHVACGlobals::TimeStepSys;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIter(30);
        Real64 const RelaxationFactor(0.4);
        Real64 const Tolerance(0.1);
        static std::string const RoutineName("CalcTESCoilDischargeOnlyMode");
        static std::string const StorageTankName("CalcTESWaterStorageTank");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AirMassFlowRatio;      // evaporator inlet air mass flow divided by design mass flow [ ]
        Real64 EvapAirMassFlow;       // local for evaporator air mass flow [kg/s]
        Real64 EvapInletDryBulb;      // evaporator inlet air drybulb [C]
        Real64 EvapInletHumRat;       // evaporator inlet air humidity ratio [kg/kg]
        Real64 EvapInletWetBulb;      // evaporator inlet air wetbulb [C]
        Real64 EvapInletEnthalpy;     // evaporator inlet air enthalpy [J/kg]
        Real64 sTES;                  // state of charge of Thermal Energy Storage
        Real64 TotCapTempModFac;      // total coolin capacity modification factor due to temps []
        Real64 TotCapFlowModFac;      // Total cooling capacity modification factor due to flow []
        Real64 TotCap;                // total cooling capacity
        Real64 SHRTempFac(0.0);       // sensible heat ratio modification factor due to temps []
        Real64 SHRFlowFac;            // sensible heat ratio modification factor due to flow []
        Real64 SHR;                   // sensible heat ratio
        Real64 PLF;                   // part load factor
        Real64 PLR;                   // part load ratio
        Real64 RuntimeFraction;       // compressor running time divided by full time of timestep.
        Real64 FullLoadOutAirEnth;    // evaporator outlet full load enthalpy [J/kg]
        Real64 FullLoadOutAirHumRat;  // evaporator outlet humidity ratio at full load
        Real64 FullLoadOutAirTemp;    // evaporator outlet air temperature at full load [C]
        Real64 hTinwout;              // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
        Real64 EvapOutletAirEnthalpy; // evaporator outlet air enthalpy [J/kg]
        Real64 EvapOutletAirHumRat;   // evaporator outlet air humidity ratio [kg/kg]
        Real64 EvapOutletAirTemp;     // evaporator outlet drybulb [C]
        Real64 EIRTempModFac;         // energy input ratio modification factor due to temperatures []
        Real64 EIRFlowModFac;         // energy input ratio modification factor due to flow []
        Real64 EIR;                   // energy input ratio
        Real64 ElecCoolingPower;      // compressor electric power
        Real64 MinAirHumRat;          // minimum air humidity ratio
        bool TESHasSomeCharge;        // true when there is something avaiable in storage
        Real64 QdotDischargeLimit;    // limit for how much storage can be discharged without overshooting
        Real64 rho;                   // density of water in tank (kg/m3)
        Real64 TankMass;              // Mass of water in tank (kg)
        Real64 CpTank;                // Specific heat of water in tank (J/kg K)
        Real64 QdotTEStest;
        Real64 RuntimeFractionLimit;
        Real64 PartLoadOutAirEnth;        // local leaving enthalpy at part load
        Real64 PartLoadDryCoilOutAirTemp; // local leaving drybulb if coil were dry
        bool CoilMightBeDry;
        int Counter;
        bool Converged;
        Real64 DryCoilTestEvapInletHumRat;
        Real64 DryCoilTestEvapInletWetBulb;
        Real64 hADP;
        Real64 tADP;
        Real64 wADP;
        Real64 hTinwADP;
        Real64 SHRadp;
        Real64 werror;

        PLR = PartLoadRatio;

        EvapAirMassFlow = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
        EvapInletDryBulb = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
        EvapInletHumRat = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
        EvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, EvapInletHumRat, OutBaroPress, RoutineName);
        EvapInletEnthalpy = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Enthalpy;
        CoilMightBeDry = false;

        if (TESCoil(TESCoilNum).StorageMedia == FluidBased) {
            sTES = TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
            if ((sTES >= TESCoil(TESCoilNum).MinimumFluidTankTempLimit) && (sTES < TESCoil(TESCoilNum).MaximumFluidTankTempLimit)) {
                TESHasSomeCharge = true;
                rho = GetDensityGlycol(TESCoil(TESCoilNum).StorageFluidName, sTES, TESCoil(TESCoilNum).StorageFluidIndex, StorageTankName);
                TankMass = rho * TESCoil(TESCoilNum).FluidStorageVolume;
                CpTank = GetSpecificHeatGlycol(TESCoil(TESCoilNum).StorageFluidName, sTES, TESCoil(TESCoilNum).StorageFluidIndex, StorageTankName);
                // simple linear approximation of DT/Dt term in McpDT/Dt
                QdotDischargeLimit = TankMass * CpTank * (TESCoil(TESCoilNum).MaximumFluidTankTempLimit - sTES) / (TimeStepSys * SecInHour);
            } else {
                TESHasSomeCharge = false;
            }
        } else if (TESCoil(TESCoilNum).StorageMedia == IceBased) {
            sTES = TESCoil(TESCoilNum).IceFracRemainLastTimestep;
            if (sTES > 0.0) {
                TESHasSomeCharge = true;
                // discharge limit
                QdotDischargeLimit = (sTES)*TESCoil(TESCoilNum).IceStorageCapacity / (TimeStepSys * SecInHour);
            } else {
                TESHasSomeCharge = false;
            }
        }

        if ((EvapAirMassFlow > SmallMassFlow) && (PLR > 0.0) && TESHasSomeCharge) { // coil is running
            AirMassFlowRatio = EvapAirMassFlow / TESCoil(TESCoilNum).RatedEvapAirMassFlowRate;

            TotCapTempModFac = CurveValue(TESCoil(TESCoilNum).DischargeOnlyCapFTempCurve, EvapInletWetBulb, sTES);
            TotCapTempModFac = max(0.0, TotCapTempModFac);
            TotCapFlowModFac = CurveValue(TESCoil(TESCoilNum).DischargeOnlyCapFFlowCurve, AirMassFlowRatio);
            TotCapFlowModFac = max(0.0, TotCapFlowModFac);
            TotCap = TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap * TotCapTempModFac * TotCapFlowModFac;

            PLF = CurveValue(TESCoil(TESCoilNum).DischargeOnlyPLFFPLRCurve, PLR);
            if (PLF >= PLR && PLF > 0.0) {
                RuntimeFraction = PLR / PLF;
            } else {
                RuntimeFraction = 1.0; // warn maybe
            }
            // Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
            EIRTempModFac = CurveValue(TESCoil(TESCoilNum).DischargeOnlyEIRFTempCurve, EvapInletWetBulb, sTES);
            EIRTempModFac = max(EIRTempModFac, 0.0);
            EIRFlowModFac = CurveValue(TESCoil(TESCoilNum).DischargeOnlyEIRFFlowCurve, AirMassFlowRatio);
            EIRFlowModFac = max(EIRFlowModFac, 0.0);
            EIR = EIRTempModFac * EIRFlowModFac / TESCoil(TESCoilNum).DischargeOnlyRatedCOP;

            ElecCoolingPower = TotCap * EIR * RuntimeFraction;
            QdotTEStest = TotCap * RuntimeFraction + ElecCoolingPower;

            if (QdotTEStest > QdotDischargeLimit) {
                RuntimeFractionLimit = QdotDischargeLimit / (TotCap + TotCap * EIR);
                RuntimeFraction = min(RuntimeFraction, RuntimeFractionLimit);
                PLR = RuntimeFraction * PLF;
                ElecCoolingPower = TotCap * EIR * RuntimeFraction;
            }
            // now see if coil is running dry
            PartLoadOutAirEnth = EvapInletEnthalpy - (TotCap * PartLoadRatio) / EvapAirMassFlow;
            PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat);
            if (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(PartLoadOutAirEnth, OutBaroPress, RoutineName)) {
                CoilMightBeDry = true;
                // find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
                DryCoilTestEvapInletHumRat = EvapInletHumRat;
                DryCoilTestEvapInletWetBulb = EvapInletWetBulb;
                Counter = 0;
                Converged = false;
                while (!Converged) {
                    TotCapTempModFac = CurveValue(TESCoil(TESCoilNum).DischargeOnlyCapFTempCurve, DryCoilTestEvapInletWetBulb, sTES);
                    TotCapTempModFac = max(0.0, TotCapTempModFac);
                    TotCapFlowModFac = CurveValue(TESCoil(TESCoilNum).DischargeOnlyCapFFlowCurve, AirMassFlowRatio);
                    TotCapFlowModFac = max(0.0, TotCapFlowModFac);
                    TotCap = TESCoil(TESCoilNum).DischargeOnlyRatedDischargeCap * TotCapTempModFac * TotCapFlowModFac;
                    // coil bypass factor = 0.0
                    hADP = EvapInletEnthalpy - (TotCap / EvapAirMassFlow);
                    tADP = PsyTsatFnHPb(hADP, OutBaroPress, RoutineName);
                    wADP = min(EvapInletHumRat, PsyWFnTdbH(tADP, hADP, RoutineName));
                    hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP);
                    if ((EvapInletEnthalpy - hADP) > 1.e-10) {
                        SHRadp = min((hTinwADP - hADP) / (EvapInletEnthalpy - hADP), 1.0);
                    } else {
                        SHRadp = 1.0;
                    }

                    if ((wADP > DryCoilTestEvapInletHumRat) || (Counter >= 1 && Counter < MaxIter)) {
                        if (DryCoilTestEvapInletHumRat <= 0.0) DryCoilTestEvapInletHumRat = 0.00001;
                        werror = (DryCoilTestEvapInletHumRat - wADP) / DryCoilTestEvapInletHumRat;

                        DryCoilTestEvapInletHumRat = RelaxationFactor * wADP + (1.0 - RelaxationFactor) * DryCoilTestEvapInletHumRat;
                        DryCoilTestEvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, DryCoilTestEvapInletHumRat, OutBaroPress, RoutineName);

                        ++Counter;
                        if (std::abs(werror) <= Tolerance) {
                            Converged = true;
                        } else {
                            Converged = false;
                        }
                    } else {
                        Converged = true;
                    }
                }
            } // coil will be wet so use SHR curves
            {
                if (CurveManager::PerfCurve(TESCoil(TESCoilNum).DischargeOnlySHRFTempCurve).NumDims == 2) {
                    SHRTempFac = CurveValue(TESCoil(TESCoilNum).DischargeOnlySHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb);
                } else {
                    SHRTempFac = CurveValue(TESCoil(TESCoilNum).DischargeOnlySHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb, sTES);
                }
            }

            SHRFlowFac = CurveValue(TESCoil(TESCoilNum).DischargeOnlySHRFFLowCurve, AirMassFlowRatio);
            SHR = TESCoil(TESCoilNum).DischargeOnlyRatedSHR * SHRTempFac * SHRFlowFac;
            SHR = min(SHR, 1.0); // warn maybe
            SHR = max(SHR, 0.0); // warn maybe
            if (CoilMightBeDry) {
                if ((EvapInletHumRat < DryCoilTestEvapInletHumRat) && (SHRadp > SHR)) { // coil is dry for sure
                    SHR = 1.0;
                } else if (SHRadp > SHR) {
                    SHR = SHRadp;
                }
            }
            //  Calculate full load output conditions
            FullLoadOutAirEnth = EvapInletEnthalpy - TotCap / EvapAirMassFlow;

            hTinwout = EvapInletEnthalpy - (1.0 - SHR) * (TotCap / EvapAirMassFlow);
            // The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
            FullLoadOutAirHumRat = PsyWFnTdbH(EvapInletDryBulb, hTinwout, RoutineName, true);
            FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth, FullLoadOutAirHumRat);
            // Check for saturation error and modify temperature at constant enthalpy
            if (FullLoadOutAirTemp < PsyTsatFnHPb(FullLoadOutAirEnth, OutBaroPress, RoutineName)) {
                FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth, OutBaroPress, RoutineName);
                FullLoadOutAirHumRat = PsyWFnTdbH(FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName);
            }

            // Continuous fan, cycling compressor
            EvapOutletAirEnthalpy = ((PLR)*FullLoadOutAirEnth + (1.0 - (PLR)) * EvapInletEnthalpy);
            EvapOutletAirHumRat = ((PLR)*FullLoadOutAirHumRat + (1.0 - (PLR)) * EvapInletHumRat);
            EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy, EvapOutletAirHumRat);
            if (EvapOutletAirTemp < PsyTsatFnHPb(EvapOutletAirEnthalpy, OutBaroPress, RoutineName)) {
                EvapOutletAirTemp = PsyTsatFnHPb(EvapOutletAirEnthalpy, OutBaroPress, RoutineName);
                EvapOutletAirHumRat = PsyWFnTdbH(EvapOutletAirTemp, EvapOutletAirEnthalpy, RoutineName);
            }

            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = EvapOutletAirTemp;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = EvapOutletAirHumRat;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy = EvapOutletAirEnthalpy;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = EvapAirMassFlow;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
            TESCoil(TESCoilNum).ElecCoolingPower = ElecCoolingPower + TESCoil(TESCoilNum).AncillaryControlsPower;
            TESCoil(TESCoilNum).ElecCoolingEnergy = TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * SecInHour;
            TESCoil(TESCoilNum).RuntimeFraction = RuntimeFraction;
            TESCoil(TESCoilNum).EvapTotCoolingRate = TotCap * RuntimeFraction; // double check this
            TESCoil(TESCoilNum).EvapTotCoolingEnergy = TotCap * RuntimeFraction * TimeStepSys * SecInHour;
            MinAirHumRat = min(Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat, Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat);
            TESCoil(TESCoilNum).EvapSensCoolingRate =
                EvapAirMassFlow * (PsyHFnTdbW(EvapInletDryBulb, MinAirHumRat) - PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat));
            if (TESCoil(TESCoilNum).EvapSensCoolingRate > TESCoil(TESCoilNum).EvapTotCoolingRate) {
                TESCoil(TESCoilNum).EvapSensCoolingRate = TESCoil(TESCoilNum).EvapTotCoolingRate;
            }
            TESCoil(TESCoilNum).EvapSensCoolingEnergy = TESCoil(TESCoilNum).EvapSensCoolingRate * TimeStepSys * SecInHour;
            TESCoil(TESCoilNum).EvapLatCoolingRate = TESCoil(TESCoilNum).EvapTotCoolingRate - TESCoil(TESCoilNum).EvapSensCoolingRate;
            TESCoil(TESCoilNum).EvapLatCoolingEnergy = TESCoil(TESCoilNum).EvapLatCoolingRate * TimeStepSys * SecInHour;

            TESCoil(TESCoilNum).QdotTES = TotCap * RuntimeFraction + ElecCoolingPower; // all heat rejection into storage

        } else { // coil is off; just pass through conditions
            TESCoil(TESCoilNum).QdotTES = 0.0;

            TESCoil(TESCoilNum).ElecCoolingPower = TESCoil(TESCoilNum).AncillaryControlsPower;
            TESCoil(TESCoilNum).ElecCoolingEnergy = TESCoil(TESCoilNum).ElecCoolingPower * TimeStepSys * SecInHour;
            TESCoil(TESCoilNum).RuntimeFraction = 0.0;

            TESCoil(TESCoilNum).RuntimeFraction = 0.0;
            TESCoil(TESCoilNum).EvapTotCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapTotCoolingEnergy = 0.0;
            TESCoil(TESCoilNum).EvapSensCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapSensCoolingEnergy = 0.0;
            TESCoil(TESCoilNum).EvapLatCoolingRate = 0.0;
            TESCoil(TESCoilNum).EvapLatCoolingEnergy = 0.0;

            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).Temp;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).HumRat;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRate;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMinAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMinAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).MassFlowRateMaxAvail = Node(TESCoil(TESCoilNum).EvapAirInletNodeNum).MassFlowRateMaxAvail;
            Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Enthalpy =
                PsyHFnTdbW(Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).EvapAirOutletNodeNum).HumRat);
        }

        // nothing happens at condenser
        Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
        Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat;
        Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate = 0.0;
        Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).MassFlowRate = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).MassFlowRate;
        Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Enthalpy =
            PsyHFnTdbW(Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).Temp, Node(TESCoil(TESCoilNum).CondAirOutletNodeNum).HumRat);
        TESCoil(TESCoilNum).CondInletTemp = Node(TESCoil(TESCoilNum).CondAirInletNodeNum).Temp;
        TESCoil(TESCoilNum).Q_TES = TESCoil(TESCoilNum).QdotTES * TimeStepSys * SecInHour;
        UpdateTEStorage(TESCoilNum);

        UpdateColdWeatherProtection(TESCoilNum);

        if (TESCoil(TESCoilNum).CondenserType == EvapCooled) {
            UpdateEvaporativeCondenserBasinHeater(TESCoilNum);
            UpdateEvaporativeCondenserWaterUse(
                TESCoilNum, Node(TESCoil(TESCoilNum).CondAirInletNodeNum).HumRat, TESCoil(TESCoilNum).CondAirInletNodeNum);
        }
    }

    void ControlTESIceStorageTankCoil(std::string const &CoilName,      // child object coil name
                                      int CoilIndex,                    // child object coil index
                                      std::string SystemType,           // parent object system type
                                      int const FanOpMode,              // parent object fan operating mode
                                      Real64 const DesiredOutletTemp,   // desired outlet temperature [C]
                                      Real64 const DesiredOutletHumRat, // desired outlet humidity ratio [kg/kg]
                                      Real64 &PartLoadFrac, // value based on coil operation, if possible, as PLR required to meet T or w set point
                                      int &TESOpMode,       // value determined in InitTESCoil and passed back to parent for use in iteration routines
                                      int &ControlType,     // parent object dehumidification control type (e.g., None, Multimode, CoolReheat)
                                      int &SensPLRIter,     // iteration number of Sensible PLR Iteration warning message
                                      int &SensPLRIterIndex, // index to Sensible PLR Iteration warning message
                                      int &SensPLRFail,      // iteration number of Sensible PLR Iteration fail warning message
                                      int &SensPLRFailIndex, // index to Sensible PLR Iteration fail warning message
                                      int &LatPLRIter,       // iteration number of Latent PLR Iteration warning message
                                      int &LatPLRIterIndex,  // index to Latent PLR Iteration warning message
                                      int &LatPLRFail,       // iteration number of Latent PLR Iteration fail warning message
                                      int &LatPLRFailIndex   // index to Latent PLR Iteration fail warning message
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Raustad (based on HVACDXSystem code)
        //       DATE WRITTEN   July 13, 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Provides a common routine for parent objects. Parent objects will call this routine to determine the coil PLR.

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using General::RoundSigDigits;
        using General::SolveRoot;

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);         // Maximum number of iterations for solver
        Real64 const Acc(1.e-3);       // Accuracy of solver result
        Real64 const HumRatAcc(1.e-6); // Accuracy of solver result

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int InletNode;
        int OutletNode;
        Real64 NoOutput;
        Real64 NoLoadHumRatOut;
        Real64 FullOutput;
        Real64 FullLoadHumRatOut;
        Real64 ReqOutput;
        Real64 OutletHumRatDXCoil;
        int SolFlag;            // return flag from RegulaFalsi for sensible load
        Array1D<Real64> Par(5); // Parameter array passed to solver

        InletNode = TESCoil(CoilIndex).EvapAirInletNodeNum;
        OutletNode = TESCoil(CoilIndex).EvapAirOutletNodeNum;

        // First get the control mode that the child coil is in
        SimTESCoil(CoilName, CoilIndex, FanOpMode, TESOpMode, PartLoadFrac);
        if (TESOpMode == OffMode || TESOpMode == ChargeOnlyMode) { // cannot cool
            PartLoadFrac = 0.0;
        } else {
            // Get no load result
            PartLoadFrac = 0.0;
            SimTESCoil(CoilName, CoilIndex, FanOpMode, TESOpMode, PartLoadFrac);
            NoOutput = Node(InletNode).MassFlowRate *
                       (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
            NoLoadHumRatOut = Node(OutletNode).HumRat;

            // Get full load result
            PartLoadFrac = 1.0;
            SimTESCoil(CoilName, CoilIndex, FanOpMode, TESOpMode, PartLoadFrac);
            FullOutput = Node(InletNode).MassFlowRate *
                         (PsyHFnTdbW(Node(OutletNode).Temp, Node(OutletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
            FullLoadHumRatOut = Node(OutletNode).HumRat;

            ReqOutput = Node(InletNode).MassFlowRate *
                        (PsyHFnTdbW(DesiredOutletTemp, Node(OutletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(OutletNode).HumRat));
            //         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
            if ((NoOutput - ReqOutput) < Acc) {
                PartLoadFrac = 0.0;
                //         If the FullOutput is greater than (insufficient cooling) or very near the ReqOutput,
                //         run the compressor at PartLoadFrac = 1.
            } else if ((FullOutput - ReqOutput) > Acc) {
                PartLoadFrac = 1.0;
                //         Else find the PLR to meet the load
            } else {
                if (Node(OutletNode).Temp > DesiredOutletTemp) {
                    PartLoadFrac = 1.0;
                } else {
                    Par(1) = double(CoilIndex);
                    Par(2) = DesiredOutletTemp;
                    Par(3) = TESOpMode;
                    Par(4) = OutletNode;
                    Par(5) = double(FanOpMode);
                    SolveRoot(Acc, MaxIte, SolFlag, PartLoadFrac, TESCoilResidualFunction, 0.0, 1.0, Par);
                    if (SolFlag == -1) {
                        if (!WarmupFlag) {
                            if (SensPLRIter < 1) {
                                ++SensPLRIter;
                                ShowWarningError(SystemType +
                                                 " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + CoilName);
                                ShowContinueError("Estimated part-load ratio  = " + RoundSigDigits((ReqOutput / FullOutput), 3));
                                ShowContinueError("Calculated part-load ratio = " + RoundSigDigits(PartLoadFrac, 3));
                                ShowContinueErrorTimeStamp(
                                    "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                            }
                            ShowRecurringWarningErrorAtEnd(SystemType + " \"" + CoilName +
                                                               "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. "
                                                               "Sensible PLR statistics follow.",
                                                           SensPLRIterIndex,
                                                           PartLoadFrac,
                                                           PartLoadFrac);
                        }
                    } else if (SolFlag == -2) {
                        PartLoadFrac = ReqOutput / FullOutput;
                        if (!WarmupFlag) {
                            if (SensPLRFail < 1) {
                                ++SensPLRFail;
                                ShowWarningError(
                                    SystemType +
                                    " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " +
                                    CoilName);
                                ShowContinueError("Estimated part-load ratio = " + RoundSigDigits(PartLoadFrac, 3));
                                ShowContinueErrorTimeStamp(
                                    "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                            }
                            ShowRecurringWarningErrorAtEnd(
                                SystemType + " \"" + CoilName +
                                    "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.",
                                SensPLRFailIndex,
                                PartLoadFrac,
                                PartLoadFrac);
                        }
                    }
                }
                //         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
                //         else use operating humidity ratio to test against humidity setpoint
                if (PartLoadFrac == 0.0) {
                    OutletHumRatDXCoil = NoLoadHumRatOut;
                } else {
                    OutletHumRatDXCoil = Node(OutletNode).HumRat;
                }
                // If humidity setpoint is not satisfied and humidity control type is CoolReheat,
                // then overcool to meet moisture load

                if ((OutletHumRatDXCoil > DesiredOutletHumRat) && (PartLoadFrac < 1.0) && (ControlType == DehumidControl_CoolReheat)) {
                    //           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
                    //           do not run the compressor
                    if ((NoLoadHumRatOut - DesiredOutletHumRat) < HumRatAcc) {
                        // PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
                        //           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
                        //           run the compressor at PartLoadFrac = 1.
                    } else if ((DesiredOutletHumRat - FullLoadHumRatOut) < HumRatAcc) {
                        PartLoadFrac = 1.0;
                        //           Else find the PLR to meet the load
                    } else {
                        Par(1) = double(CoilIndex);
                        Par(2) = DesiredOutletHumRat;
                        Par(3) = TESOpMode;
                        Par(4) = OutletNode;
                        Par(5) = double(FanOpMode);
                        SolveRoot(HumRatAcc, MaxIte, SolFlag, PartLoadFrac, TESCoilHumRatResidualFunction, 0.0, 1.0, Par);
                        if (SolFlag == -1) {
                            if (!WarmupFlag) {
                                if (LatPLRIter < 1) {
                                    ++LatPLRIter;
                                    ShowWarningError(SystemType +
                                                     " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " + CoilName);
                                    ShowContinueError("Estimated part-load ratio   = " + RoundSigDigits((ReqOutput / FullOutput), 3));
                                    ShowContinueError("Calculated part-load ratio = " + RoundSigDigits(PartLoadFrac, 3));
                                    ShowContinueErrorTimeStamp(
                                        "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                }
                                ShowRecurringWarningErrorAtEnd(SystemType + " \"" + CoilName +
                                                                   "\" - Iteration limit exceeded calculating latent part-load ratio error "
                                                                   "continues. Latent PLR statistics follow.",
                                                               LatPLRIterIndex,
                                                               PartLoadFrac,
                                                               PartLoadFrac);
                            }
                        } else if (SolFlag == -2) {
                            //               RegulaFalsi returns PLR = minPLR when a solution cannot be found, recalculate PartLoadFrac.
                            if (NoLoadHumRatOut - FullLoadHumRatOut != 0.0) {
                                PartLoadFrac = (NoLoadHumRatOut - DesiredOutletHumRat) / (NoLoadHumRatOut - FullLoadHumRatOut);
                            } else {
                                PartLoadFrac = 1.0;
                            }
                            if (!WarmupFlag) {
                                if (LatPLRFail < 1) {
                                    ++LatPLRFail;
                                    ShowWarningError(
                                        SystemType +
                                        " - DX unit latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " +
                                        CoilName);
                                    ShowContinueError("Estimated part-load ratio = " + RoundSigDigits(PartLoadFrac, 3));
                                    ShowContinueErrorTimeStamp(
                                        "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                }
                                ShowRecurringWarningErrorAtEnd(
                                    SystemType + " \"" + CoilName +
                                        "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics follow.",
                                    LatPLRFailIndex,
                                    PartLoadFrac,
                                    PartLoadFrac);
                            }
                        }
                    }
                } // End if humidity ratio setpoint not met - CoolReheat humidity control

            } // operating mode can cool
            if (PartLoadFrac > 1.0) {
                PartLoadFrac = 1.0;
            } else if (PartLoadFrac < 0.0) {
                PartLoadFrac = 0.0;
            }
        }
    }

    Real64 TESCoilResidualFunction(Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                   Array1<Real64> const &Par   // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // TES Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls appropriate calculation routine depending on operating mode
        // to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        //		using PackagedThermalStorageCoil::CalcTESCoilCoolingOnlyMode;
        //		using PackagedThermalStorageCoil::CalcTESCoilCoolingAndChargeMode;
        //		using PackagedThermalStorageCoil::CalcTESCoilCoolingAndDischargeMode;
        //		using PackagedThermalStorageCoil::CalcTESCoilDischargeOnlyMode;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet temperature [C]
        // par(3) = TES coil operating mode
        // par(4) = outlet node number
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;        // index of this coil
        Real64 OutletAirTemp; // outlet air temperature [C]
        int FanOpMode;        // Supply air fan operating mode
        int TESOpMode;
        int OutletNodeNum;

        CoilIndex = int(Par(1));
        FanOpMode = int(Par(5));
        OutletNodeNum = int(Par(4));
        TESOpMode = int(Par(3));

        {
            auto const SELECT_CASE_var(TESOpMode);
            if (SELECT_CASE_var == CoolingOnlyMode) {
                CalcTESCoilCoolingOnlyMode(CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == CoolingAndChargeMode) {
                CalcTESCoilCoolingAndChargeMode(CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == CoolingAndDischargeMode) {
                CalcTESCoilCoolingAndDischargeMode(CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == DischargeOnlyMode) {
                CalcTESCoilDischargeOnlyMode(CoilIndex, PartLoadRatio);
            }
        }

        OutletAirTemp = Node(OutletNodeNum).Temp;
        Residuum = Par(2) - OutletAirTemp;

        return Residuum;
    }

    Real64 TESCoilHumRatResidualFunction(Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                         Array1<Real64> const &Par   // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet humrat - actual outlet humrat)
        // TES Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls appropriate calculation routine depending on operating mode
        // to get outlet hum rat at the given cycling ratio
        // and calculates the residual as defined above

        // REFERENCES:

        // Using/Aliasing
        //		using PackagedThermalStorageCoil::CalcTESCoilCoolingOnlyMode;
        //		using PackagedThermalStorageCoil::CalcTESCoilCoolingAndChargeMode;
        //		using PackagedThermalStorageCoil::CalcTESCoilCoolingAndDischargeMode;
        //		using PackagedThermalStorageCoil::CalcTESCoilDischargeOnlyMode;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = desired air outlet hum rat [kg_h20/kg_dryair]
        // par(3) = TES coil operating mode
        // par(4) = outlet node number
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;          // index of this coil
        Real64 OutletAirHumRat; // outlet air humidity ratio [kg_H20/Kg_dryair]
        int FanOpMode;          // Supply air fan operating mode
        int TESOpMode;
        int OutletNodeNum;

        CoilIndex = int(Par(1));
        FanOpMode = int(Par(5));
        OutletNodeNum = int(Par(4));
        TESOpMode = int(Par(3));

        {
            auto const SELECT_CASE_var(TESOpMode);
            if (SELECT_CASE_var == CoolingOnlyMode) {
                CalcTESCoilCoolingOnlyMode(CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == CoolingAndChargeMode) {
                CalcTESCoilCoolingAndChargeMode(CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == CoolingAndDischargeMode) {
                CalcTESCoilCoolingAndDischargeMode(CoilIndex, FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == DischargeOnlyMode) {
                CalcTESCoilDischargeOnlyMode(CoilIndex, PartLoadRatio);
            }
        }

        OutletAirHumRat = Node(OutletNodeNum).HumRat;
        Residuum = Par(2) - OutletAirHumRat;

        return Residuum;
    }

    void UpdateTEStorage(int const TESCoilNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

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
        // na
        if (TESCoil(TESCoilNum).StorageMedia == FluidBased) {
            CalcTESWaterStorageTank(TESCoilNum);
        } else if (TESCoil(TESCoilNum).StorageMedia == IceBased) {
            CalcTESIceStorageTank(TESCoilNum);
        }
    }

    void CalcTESWaterStorageTank(int const TESCoilNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataBranchAirLoopPlant::MassFlowTolerance;
        using DataGlobals::HourOfDay;
        using DataGlobals::TimeStep;
        using DataGlobals::TimeStepZone;
        using DataHVACGlobals::SysTimeElapsed;
        using DataHVACGlobals::TimeStepSys;
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using WaterThermalTanks::CalcTankTemp;
        using WaterThermalTanks::CalcTempIntegral;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcTESWaterStorageTank");
        static std::string const calcTESIceStorageTank("CalcTESIceStorageTank");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 TimeElapsed;        // Fraction of the current hour that has elapsed (h)
        Real64 AmbientTemp;        // Current ambient air temperature around tank (C)
        Real64 TankMass;           // Mass of water in tank (kg)
        Real64 LossCoeff;          // Loss coefficient to ambient environment (W/K)
        Real64 TankTemp;           // Instantaneous tank temperature (C)
        Real64 NewTankTemp;        // Predicted new tank temperature (C)
        Real64 CpTank;             // Specific heat of water in tank (J/kg K)
        Real64 UseInletTemp;       // Use side inlet temperature (C)
        Real64 UseMassFlowRate;    // Use side flow rate, including effectiveness factor (kg/s)
        Real64 SourceInletTemp;    // Source side inlet temperature (C)
        Real64 SourceMassFlowRate; // Source side flow rate, including effectiveness factor (kg/s)
        Real64 TimeRemaining;      // Time remaining in the current timestep (s)
        Real64 CpPlantConnection;  // Specific heat of fluid in plant connection (J/kg K)
        Real64 deltaTsum;          // Change in integrated tank temperature, dividing by time gives the average (C s)
        Real64 SecInTimeStep;      // Seconds in one timestep (s)
        Real64 rho;                // density of water in tank (kg/m3)
        Real64 QdotTES;            // heat exchange directly into tank from charging system [W]
        Real64 NewOutletTemp;      // calculated new tankoutlet temp (C)

        SecInTimeStep = TimeStepSys * SecInHour;
        TimeRemaining = SecInTimeStep;

        TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;

        if (TESCoil(TESCoilNum).TimeElapsed != TimeElapsed) {
            TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep = TESCoil(TESCoilNum).FluidTankTempFinal;
            TESCoil(TESCoilNum).TimeElapsed = TimeElapsed;
        }

        TankTemp = TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
        AmbientTemp = Node(TESCoil(TESCoilNum).StorageAmbientNodeNum).Temp;
        UseInletTemp = Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp;
        SourceInletTemp = TESCoil(TESCoilNum).FluidTankTempFinalLastTimestep;
        rho = GetDensityGlycol(TESCoil(TESCoilNum).StorageFluidName, TankTemp, TESCoil(TESCoilNum).StorageFluidIndex, RoutineName);
        TankMass = rho * TESCoil(TESCoilNum).FluidStorageVolume;
        CpTank = GetSpecificHeatGlycol(TESCoil(TESCoilNum).StorageFluidName, TankTemp, TESCoil(TESCoilNum).StorageFluidIndex, RoutineName);

        if (TESCoil(TESCoilNum).TESPlantConnectionAvailable) {
            UseMassFlowRate = Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate * TESCoil(TESCoilNum).TESPlantEffectiveness;
        } else {
            UseMassFlowRate = 0.0;
        }
        SourceMassFlowRate = 0.0;
        LossCoeff = TESCoil(TESCoilNum).StorageUA;
        QdotTES = TESCoil(TESCoilNum).QdotTES;

        NewTankTemp = CalcTankTemp(TankTemp,
                                   AmbientTemp,
                                   UseInletTemp,
                                   SourceInletTemp,
                                   TankMass,
                                   CpTank,
                                   UseMassFlowRate,
                                   SourceMassFlowRate,
                                   LossCoeff,
                                   QdotTES,
                                   TimeRemaining);

        TESCoil(TESCoilNum).FluidTankTempFinal = NewTankTemp;

        if (TESCoil(TESCoilNum).TESPlantConnectionAvailable) {
            CpPlantConnection = GetSpecificHeatGlycol(PlantLoop(TESCoil(TESCoilNum).TESPlantLoopNum).FluidName,
                                                      Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp,
                                                      PlantLoop(TESCoil(TESCoilNum).TESPlantLoopNum).FluidIndex,
                                                      calcTESIceStorageTank);

            TESCoil(TESCoilNum).QdotPlant = Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate * CpPlantConnection *
                                            TESCoil(TESCoilNum).TESPlantEffectiveness * (UseInletTemp - NewTankTemp);
            TESCoil(TESCoilNum).Q_Plant = TESCoil(TESCoilNum).QdotPlant * TimeStepSys * SecInHour;
            // now get correct outlet temp with actual massflow (not modified by effectiveness)
            if (Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate > MassFlowTolerance) {
                NewOutletTemp =
                    UseInletTemp - TESCoil(TESCoilNum).QdotPlant / (Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate * CpPlantConnection);
            } else {
                NewOutletTemp = UseInletTemp;
            }
            Node(TESCoil(TESCoilNum).TESPlantOutletNodeNum).Temp = NewOutletTemp;
        }

        deltaTsum = CalcTempIntegral(TankTemp,
                                     NewTankTemp,
                                     AmbientTemp,
                                     UseInletTemp,
                                     SourceInletTemp,
                                     TankMass,
                                     CpTank,
                                     UseMassFlowRate,
                                     SourceMassFlowRate,
                                     LossCoeff,
                                     QdotTES,
                                     TimeRemaining);
        TESCoil(TESCoilNum).QdotAmbient = (LossCoeff * (AmbientTemp * TimeRemaining - deltaTsum)) / SecInTimeStep;
        TESCoil(TESCoilNum).Q_Ambient = TESCoil(TESCoilNum).QdotAmbient * TimeStepSys * SecInHour;
    }

    void CalcTESIceStorageTank(int const TESCoilNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         <author>
        //       DATE WRITTEN   <date_written>
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataBranchAirLoopPlant::MassFlowTolerance;
        using DataGlobals::HourOfDay;
        using DataGlobals::TimeStep;
        using DataGlobals::TimeStepZone;
        using DataHVACGlobals::SysTimeElapsed;
        using DataHVACGlobals::TimeStepSys;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Real64 FreezingTemp(0.0); // zero degrees C
        static std::string const RoutineName("CalcTESIceStorageTank");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 Cp;            // local specific heat
        Real64 QdotIce;       // local rate of heat transfer to ice (negative cooling) [W]
        Real64 TimeElapsed;   // Fraction of the current hour that has elapsed (h)
        Real64 NewOutletTemp; // calculated new tankoutlet temp (C)

        TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;

        if (TESCoil(TESCoilNum).TimeElapsed != TimeElapsed) {
            TESCoil(TESCoilNum).IceFracRemainLastTimestep = TESCoil(TESCoilNum).IceFracRemain;
            TESCoil(TESCoilNum).TimeElapsed = TimeElapsed;
        }

        // update plant connection (if any)
        if (TESCoil(TESCoilNum).TESPlantConnectionAvailable) {
            Cp = GetSpecificHeatGlycol(PlantLoop(TESCoil(TESCoilNum).TESPlantLoopNum).FluidName,
                                       Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp,
                                       PlantLoop(TESCoil(TESCoilNum).TESPlantLoopNum).FluidIndex,
                                       RoutineName);

            TESCoil(TESCoilNum).QdotPlant = Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate * Cp *
                                            TESCoil(TESCoilNum).TESPlantEffectiveness *
                                            (Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp - FreezingTemp);
            TESCoil(TESCoilNum).Q_Plant = TESCoil(TESCoilNum).QdotPlant * TimeStepSys * SecInHour;
            // now get correct outlet temp with actual massflow (not modified by effectiveness)
            if (Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate > MassFlowTolerance) {
                NewOutletTemp = Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp +
                                TESCoil(TESCoilNum).QdotPlant / (Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).MassFlowRate * Cp);
            } else {
                NewOutletTemp = Node(TESCoil(TESCoilNum).TESPlantInletNodeNum).Temp;
            }
            Node(TESCoil(TESCoilNum).TESPlantOutletNodeNum).Temp = NewOutletTemp;
        } else {
            TESCoil(TESCoilNum).QdotPlant = 0.0;
            TESCoil(TESCoilNum).Q_Plant = 0.0;
        }

        // update ambient heat transfer

        TESCoil(TESCoilNum).QdotAmbient = TESCoil(TESCoilNum).StorageUA * (Node(TESCoil(TESCoilNum).StorageAmbientNodeNum).Temp - FreezingTemp);
        TESCoil(TESCoilNum).Q_Ambient = TESCoil(TESCoilNum).QdotAmbient * TimeStepSys * SecInHour;

        QdotIce = TESCoil(TESCoilNum).QdotPlant + TESCoil(TESCoilNum).QdotAmbient + TESCoil(TESCoilNum).QdotTES;

        if (QdotIce < 0.0) { // charging ice level
            TESCoil(TESCoilNum).IceFracRemain = TESCoil(TESCoilNum).IceFracRemainLastTimestep +
                                                std::abs(QdotIce) / (TESCoil(TESCoilNum).IceStorageCapacity / (TimeStepSys * SecInHour));
            if (TESCoil(TESCoilNum).IceFracRemain > 1.0) TESCoil(TESCoilNum).IceFracRemain = 1.0;
        } else { // not charging,but discharging
            TESCoil(TESCoilNum).IceFracRemain =
                TESCoil(TESCoilNum).IceFracRemainLastTimestep - QdotIce / (TESCoil(TESCoilNum).IceStorageCapacity / (TimeStepSys * SecInHour));
            if (TESCoil(TESCoilNum).IceFracRemain < 0.0) TESCoil(TESCoilNum).IceFracRemain = 0.0;
        }
    }

    void UpdateColdWeatherProtection(int const TESCoilNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        if ((Node(TESCoil(TESCoilNum).StorageAmbientNodeNum).Temp < TESCoil(TESCoilNum).ColdWeatherMinimumTempLimit) &&
            (GetCurrentScheduleValue(TESCoil(TESCoilNum).AvailSchedNum) != 0.0)) {
            TESCoil(TESCoilNum).ElectColdWeatherPower = TESCoil(TESCoilNum).ColdWeatherAncillaryPower;

        } else {
            TESCoil(TESCoilNum).ElectColdWeatherPower = 0.0;
        }
        TESCoil(TESCoilNum).ElectColdWeatherEnergy = TESCoil(TESCoilNum).ElectColdWeatherPower * TimeStepSys * SecInHour;
    }

    void UpdateEvaporativeCondenserBasinHeater(int const TESCoilNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   April 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // determine basin heater electrical power and energy

        // METHODOLOGY EMPLOYED:
        // call general worker routine

        // REFERENCES:
        // na

        // Using/Aliasing
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
        // na
        CalcBasinHeaterPower(TESCoil(TESCoilNum).BasinHeaterPowerFTempDiff,
                             TESCoil(TESCoilNum).BasinHeaterAvailSchedNum,
                             TESCoil(TESCoilNum).BasinHeaterSetpointTemp,
                             TESCoil(TESCoilNum).ElectEvapCondBasinHeaterPower);

        TESCoil(TESCoilNum).ElectEvapCondBasinHeaterEnergy = TESCoil(TESCoilNum).ElectEvapCondBasinHeaterPower * TimeStepSys * SecInHour;
    }

    void UpdateEvaporativeCondenserWaterUse(int const TESCoilNum, Real64 const HumRatAfterEvap, int const InletNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update and calculate water consumption for evaporatively cooled condensers

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataWater::WaterStorage;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AvailWaterRate;
        Real64 RhoWater;

        RhoWater = RhoH2O(Node(InletNodeNum).Temp);
        TESCoil(TESCoilNum).EvapWaterConsumpRate =
            (HumRatAfterEvap - Node(InletNodeNum).HumRat) * Node(InletNodeNum).MassFlowRate / RhoWater * TESCoil(TESCoilNum).CondenserRuntimeFraction;

        // Set the demand request for supply water from water storage tank (if needed)
        if (TESCoil(TESCoilNum).EvapWaterSupplyMode == WaterSupplyFromTank) {
            WaterStorage(TESCoil(TESCoilNum).EvapWaterSupTankID).VdotRequestDemand(TESCoil(TESCoilNum).EvapWaterTankDemandARRID) =
                TESCoil(TESCoilNum).EvapWaterConsumpRate;
        }

        // check if should be starved by restricted flow from tank
        if (TESCoil(TESCoilNum).EvapWaterSupplyMode == WaterSupplyFromTank) {
            AvailWaterRate = WaterStorage(TESCoil(TESCoilNum).EvapWaterSupTankID).VdotAvailDemand(TESCoil(TESCoilNum).EvapWaterTankDemandARRID);
            if (AvailWaterRate < TESCoil(TESCoilNum).EvapWaterConsumpRate) {
                TESCoil(TESCoilNum).EvapWaterStarvMakupRate = TESCoil(TESCoilNum).EvapWaterConsumpRate - AvailWaterRate;
                TESCoil(TESCoilNum).EvapWaterConsumpRate = AvailWaterRate;
            } else {
                TESCoil(TESCoilNum).EvapWaterStarvMakupRate = 0.0;
            }
        }

        TESCoil(TESCoilNum).EvapWaterConsump = TESCoil(TESCoilNum).EvapWaterConsumpRate * TimeStepSys * SecInHour;
        TESCoil(TESCoilNum).EvapWaterStarvMakup = TESCoil(TESCoilNum).EvapWaterStarvMakupRate * TimeStepSys * SecInHour;
        TESCoil(TESCoilNum).EvapCondPumpElecPower = TESCoil(TESCoilNum).EvapCondPumpElecNomPower * TESCoil(TESCoilNum).CondenserRuntimeFraction;
        TESCoil(TESCoilNum).EvapCondPumpElecConsumption = TESCoil(TESCoilNum).EvapCondPumpElecPower * TimeStepSys * SecInHour;
    }

    void GetTESCoilIndex(std::string const &CoilName, int &CoilIndex, bool &ErrorsFound, Optional_string_const CurrentModuleObject)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   August 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sets an index for a given TES Cooling Coil -- issues error message if that
        // coil is not a legal TES Cooling Coil.

        // Obtains and allocates TESCoil related parameters from input file
        if (GetTESInputFlag) { // First time subroutine has been called, get input data
            GetTESCoilInput();
            GetTESInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (NumTESCoils > 0) {
            CoilIndex = UtilityRoutines::FindItem(CoilName, TESCoil);
        } else {
            CoilIndex = 0;
        }

        if (CoilIndex == 0) {
            if (present(CurrentModuleObject)) {
                ShowSevereError(CurrentModuleObject() + ", GetTESCoilIndex: TES Cooling Coil not found=" + CoilName);
            } else {
                ShowSevereError("GetTESCoilIndex: TES Cooling Coil not found=" + CoilName);
            }
            ErrorsFound = true;
        }
    }

    void GetTESCoilAirInletNode(std::string const &CoilName, int &CoilAirInletNode, bool &ErrorsFound, std::string const &CurrentModuleObject)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   July 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets a given TES Cooling Coil's air inlet node -- issues error message if that
        // coil is not a legal TES Cooling Coil and sets air node to 0, otherwise, returns inlet air node number.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;

        // Obtains and allocates TESCoil related parameters from input file
        if (GetTESInputFlag) { // First time subroutine has been called, get input data
            GetTESCoilInput();
            GetTESInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (NumTESCoils > 0) {
            CoilIndex = UtilityRoutines::FindItem(CoilName, TESCoil, NumTESCoils);
        } else {
            CoilIndex = 0;
        }

        if (CoilIndex == 0) {
            ShowSevereError(CurrentModuleObject + ", GetTESCoilAirInletNode: TES Cooling Coil not found=" + CoilName);
            ErrorsFound = true;
            CoilAirInletNode = 0;
        } else {
            CoilAirInletNode = TESCoil(CoilIndex).EvapAirInletNodeNum;
        }
    }

    void GetTESCoilAirOutletNode(std::string const &CoilName, int &CoilAirOutletNode, bool &ErrorsFound, std::string const &CurrentModuleObject)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   July 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets a given TES Cooling Coil's air outlet node -- issues error message if that
        // coil is not a legal TES Cooling Coil and sets air node to 0, otherwise, returns outlet air node number.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;

        // Obtains and allocates TESCoil related parameters from input file
        if (GetTESInputFlag) { // First time subroutine has been called, get input data
            GetTESCoilInput();
            GetTESInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (NumTESCoils > 0) {
            CoilIndex = UtilityRoutines::FindItem(CoilName, TESCoil, NumTESCoils);
        } else {
            CoilIndex = 0;
        }

        if (CoilIndex == 0) {
            ShowSevereError(CurrentModuleObject + ", GetTESCoilAirOutletNode: TES Cooling Coil not found=" + CoilName);
            ErrorsFound = true;
            CoilAirOutletNode = 0;
        } else {
            CoilAirOutletNode = TESCoil(CoilIndex).EvapAirOutletNodeNum;
        }
    }

    void GetTESCoilCoolingCapacity(std::string const &CoilName, Real64 &CoilCoolCapacity, bool &ErrorsFound, std::string const &CurrentModuleObject)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   July 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets a given TES Cooling Coil's cooling only capacity -- issues error message if that
        // coil is not a legal TES Cooling Coil and sets capacity to 0, otherwise, returns cooling capacity.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;

        // Obtains and allocates TESCoil related parameters from input file
        if (GetTESInputFlag) { // First time subroutine has been called, get input data
            GetTESCoilInput();
            GetTESInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (NumTESCoils > 0) {
            CoilIndex = UtilityRoutines::FindItem(CoilName, TESCoil, NumTESCoils);
        } else {
            CoilIndex = 0;
        }

        if (CoilIndex == 0) {
            ShowSevereError(CurrentModuleObject + ", GetTESCoilCoolingCapacity: TES Cooling Coil not found=" + CoilName);
            ErrorsFound = true;
            CoilCoolCapacity = 0.0;
        } else {
            if (TESCoil(CoilIndex).CoolingOnlyModeIsAvailable) { // get input data for this mode
                CoilCoolCapacity = TESCoil(CoilIndex).CoolingOnlyRatedTotCap;
            } else if (TESCoil(CoilIndex).CoolingAndChargeModeAvailable) {
                CoilCoolCapacity = TESCoil(CoilIndex).CoolingAndChargeRatedTotCap;
            } else if (TESCoil(CoilIndex).CoolingAndDischargeModeAvailable) {
                CoilCoolCapacity = TESCoil(CoilIndex).CoolingAndDischargeRatedTotCap;
            } else {
                CoilCoolCapacity = 0.0;
            }
        }
    }

    void GetTESCoilCoolingAirFlowRate(std::string const &CoilName, Real64 &CoilCoolAirFlow, bool &ErrorsFound, std::string const &CurrentModuleObject)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   September 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets a given TES Cooling Coil's evaporator air flow rate -- issues error message if that
        // coil is not a legal TES Cooling Coil and sets air flow to 0, otherwise, returns cooling air flow rate.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;

        // Obtains and allocates TESCoil related parameters from input file
        if (GetTESInputFlag) { // First time subroutine has been called, get input data
            GetTESCoilInput();
            GetTESInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
        }

        if (NumTESCoils > 0) {
            CoilIndex = UtilityRoutines::FindItem(CoilName, TESCoil, NumTESCoils);
        } else {
            CoilIndex = 0;
        }

        if (CoilIndex == 0) {
            ShowSevereError(CurrentModuleObject + ", GetTESCoilCoolingCapacity: TES Cooling Coil not found=" + CoilName);
            ErrorsFound = true;
            CoilCoolAirFlow = 0.0;
        } else {
            CoilCoolAirFlow = TESCoil(CoilIndex).RatedEvapAirVolFlowRate;
        }
    }

} // namespace PackagedThermalStorageCoil

} // namespace EnergyPlus
