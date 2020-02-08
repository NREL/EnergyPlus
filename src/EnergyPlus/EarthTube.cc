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
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/EarthTube.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace EarthTube {
    // Module containing the data for Earth Tube system

    // MODULE INFORMATION:
    //       AUTHOR         Kwang Ho Lee
    //       DATE WRITTEN   November 2005

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithyms required to manage the EarthTube System Component

    // REFERENCES:
    // 1. M. Krarti, "Analytical Model to Predict Annual Soil Surface Temperature Variation",
    // Journal of Solar Energy Engineering 117, 1995, pp 91-99
    // 2. K. Labs In: J. Cook, editor, "Passive Cooling",
    // Cambridge Massachusetts, MIT Press, 1989, pp 206-212

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataGlobals;
    using namespace DataEnvironment;
    using namespace DataHeatBalFanSys;
    using namespace DataHeatBalance; // This is the heat balance super block data module
    using namespace DataSurfaces;
    using namespace Psychrometrics;

    // MODULE VARIABLES DECLARATIONS:
    static std::string const BlankString;

    int TotEarthTube(0); // Total EarthTube Statements in input
    // Parameters for Ventilation
    int const NaturalEarthTube(0);
    int const IntakeEarthTube(1);
    int const ExhaustEarthTube(2);

    // Object Data
    Array1D<EarthTubeData> EarthTubeSys;
    Array1D<EarthTubeZoneReportVars> ZnRptET;

    // MODULE SUBROUTINES:
    //*************************************************************************

    // Functions

    void clear_state()
    {
        TotEarthTube = 0;
        EarthTubeSys.deallocate();
        ZnRptET.deallocate();
    }

    void ManageEarthTube()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kwang Ho Lee
        //       DATE WRITTEN   November 2005

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages the simulation of EarthTube unit.
        // This driver manages the calls to all of
        // the other drivers and simulation algorithms.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool GetInputFlag(true);
        static bool ErrorsFound(false);

        // Obtains and Allocates heat balance related parameters from input file
        if (GetInputFlag) {
            GetEarthTube(ErrorsFound);
            GetInputFlag = false;
        }

        if (TotEarthTube == 0) return;

        CalcEarthTube();

        ReportEarthTube();
    }

    void GetEarthTube(bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kwang Ho Lee
        //       DATE WRITTEN   November 2005

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine obtains input data for EarthTube units and
        // stores it in the EarthTube data structure.

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using General::RoundSigDigits;
        using ScheduleManager::GetScheduleIndex;
        using ScheduleManager::GetScheduleValuesForDay;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const EarthTubeTempLimit(100.0); // degrees Celsius

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlpha;
        int NumNumber;
        int IOStat;
        int Loop;
        Array1D_bool RepVarSet;

        RepVarSet.dimension(NumOfZones, true);

        // Following used for reporting
        ZnRptET.allocate(NumOfZones);

        cCurrentModuleObject = "ZoneEarthtube";
        TotEarthTube = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        EarthTubeSys.allocate(TotEarthTube);

        for (Loop = 1; Loop <= TotEarthTube; ++Loop) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
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

            // First Alpha is Zone Name
            EarthTubeSys(Loop).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(1), Zone);
            if (EarthTubeSys(Loop).ZonePtr == 0) {
                ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + " not found=" + cAlphaArgs(1));
                ErrorsFound = true;
            }

            // Second Alpha is Schedule Name
            EarthTubeSys(Loop).SchedName = cAlphaArgs(2);
            EarthTubeSys(Loop).SchedPtr = GetScheduleIndex(cAlphaArgs(2));
            if (EarthTubeSys(Loop).SchedPtr == 0) {
                if (lAlphaFieldBlanks(2)) {
                    ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(2) + " is required, missing for " + cAlphaFieldNames(1) + '=' +
                                    cAlphaArgs(1));
                } else {
                    ShowSevereError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + " entered=" + cAlphaArgs(2) + " for " +
                                    cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                }
                ErrorsFound = true;
            }

            // Overall parameters and their limits
            EarthTubeSys(Loop).DesignLevel = rNumericArgs(1);

            EarthTubeSys(Loop).MinTemperature = rNumericArgs(2);
            if ((EarthTubeSys(Loop).MinTemperature < -EarthTubeTempLimit) || (EarthTubeSys(Loop).MinTemperature > EarthTubeTempLimit)) {
                ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) +
                                " must have a minimum temperature between -" + RoundSigDigits(EarthTubeTempLimit, 0) + "C and " +
                                RoundSigDigits(EarthTubeTempLimit, 0) + 'C');
                ShowContinueError("Entered value=" + RoundSigDigits(EarthTubeSys(Loop).MinTemperature, 0));
                ErrorsFound = true;
            }

            EarthTubeSys(Loop).MaxTemperature = rNumericArgs(3);
            if ((EarthTubeSys(Loop).MaxTemperature < -EarthTubeTempLimit) || (EarthTubeSys(Loop).MaxTemperature > EarthTubeTempLimit)) {
                ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) +
                                " must have a maximum temperature between -" + RoundSigDigits(EarthTubeTempLimit, 0) + "C and " +
                                RoundSigDigits(EarthTubeTempLimit, 0) + 'C');
                ShowContinueError("Entered value=" + RoundSigDigits(EarthTubeSys(Loop).MaxTemperature, 0));
                ErrorsFound = true;
            }

            EarthTubeSys(Loop).DelTemperature = rNumericArgs(4); //  3/12/03  Negative del temp now allowed COP

            {
                auto const SELECT_CASE_var(cAlphaArgs(3)); // Fan type character input-->convert to integer
                if (SELECT_CASE_var == "EXHAUST") {
                    EarthTubeSys(Loop).FanType = ExhaustEarthTube;
                } else if (SELECT_CASE_var == "INTAKE") {
                    EarthTubeSys(Loop).FanType = IntakeEarthTube;
                } else if ((SELECT_CASE_var == "NATURAL") || (SELECT_CASE_var == "NONE") || (SELECT_CASE_var == BlankString)) {
                    EarthTubeSys(Loop).FanType = NaturalEarthTube;
                } else {
                    ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + ", " + cAlphaFieldNames(3) +
                                    " invalid=" + cAlphaArgs(3));
                    ErrorsFound = true;
                }
            }

            EarthTubeSys(Loop).FanPressure = rNumericArgs(5);
            if (EarthTubeSys(Loop).FanPressure < 0.0) {
                ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + ", " + cNumericFieldNames(5) +
                                " must be positive, entered value=" + RoundSigDigits(EarthTubeSys(Loop).FanPressure, 2));
                ErrorsFound = true;
            }

            EarthTubeSys(Loop).FanEfficiency = rNumericArgs(6);
            if ((EarthTubeSys(Loop).FanEfficiency <= 0.0) || (EarthTubeSys(Loop).FanEfficiency > 1.0)) {
                ShowSevereError(
                    cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + ", " + cNumericFieldNames(6) +
                    " must be greater than zero and less than or equal to one, entered value=" + RoundSigDigits(EarthTubeSys(Loop).FanEfficiency, 2));
                ErrorsFound = true;
            }

            EarthTubeSys(Loop).r1 = rNumericArgs(7);
            if (EarthTubeSys(Loop).r1 <= 0.0) {
                ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + ", " + cNumericFieldNames(7) +
                                " must be positive, entered value=" + RoundSigDigits(EarthTubeSys(Loop).r1, 2));
                ErrorsFound = true;
            }

            EarthTubeSys(Loop).r2 = rNumericArgs(8);
            if (EarthTubeSys(Loop).r2 <= 0.0) {
                ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + ", " + cNumericFieldNames(8) +
                                " must be positive, entered value=" + RoundSigDigits(EarthTubeSys(Loop).r2, 2));
                ErrorsFound = true;
            }

            EarthTubeSys(Loop).r3 = 2.0 * EarthTubeSys(Loop).r1;

            EarthTubeSys(Loop).PipeLength = rNumericArgs(9);
            if (EarthTubeSys(Loop).PipeLength <= 0.0) {
                ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + ", " + cNumericFieldNames(9) +
                                " must be positive, entered value=" + RoundSigDigits(EarthTubeSys(Loop).PipeLength, 2));
                ErrorsFound = true;
            }

            EarthTubeSys(Loop).PipeThermCond = rNumericArgs(10);
            if (EarthTubeSys(Loop).PipeThermCond <= 0.0) {
                ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + ", " + cNumericFieldNames(10) +
                                " must be positive, entered value=" + RoundSigDigits(EarthTubeSys(Loop).PipeThermCond, 2));
                ErrorsFound = true;
            }

            EarthTubeSys(Loop).z = rNumericArgs(11);
            if (EarthTubeSys(Loop).z <= 0.0) {
                ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + ", " + cNumericFieldNames(11) +
                                " must be positive, entered value=" + RoundSigDigits(EarthTubeSys(Loop).z, 2));
                ErrorsFound = true;
            }
            if (EarthTubeSys(Loop).z <= (EarthTubeSys(Loop).r1 + EarthTubeSys(Loop).r2 + EarthTubeSys(Loop).r3)) {
                ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + ", " + cNumericFieldNames(11) +
                                " must be greater than 3*" + cNumericFieldNames(7) + " + " + cNumericFieldNames(8) +
                                " entered value=" + RoundSigDigits(EarthTubeSys(Loop).z, 2) +
                                " ref sum=" + RoundSigDigits(EarthTubeSys(Loop).r1 + EarthTubeSys(Loop).r2 + EarthTubeSys(Loop).r3, 2));
                ErrorsFound = true;
            }

            {
                auto const SELECT_CASE_var(cAlphaArgs(4)); // Soil type character input --> convert to number
                if (SELECT_CASE_var == "HEAVYANDSATURATED") {
                    EarthTubeSys(Loop).SoilThermDiff = 0.0781056;
                    EarthTubeSys(Loop).SoilThermCond = 2.42;
                } else if (SELECT_CASE_var == "HEAVYANDDAMP") {
                    EarthTubeSys(Loop).SoilThermDiff = 0.055728;
                    EarthTubeSys(Loop).SoilThermCond = 1.3;
                } else if (SELECT_CASE_var == "HEAVYANDDRY") {
                    EarthTubeSys(Loop).SoilThermDiff = 0.0445824;
                    EarthTubeSys(Loop).SoilThermCond = 0.865;
                } else if (SELECT_CASE_var == "LIGHTANDDRY") {
                    EarthTubeSys(Loop).SoilThermDiff = 0.024192;
                    EarthTubeSys(Loop).SoilThermCond = 0.346;
                } else {
                    ShowSevereError(cCurrentModuleObject + ": " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + ", " + cAlphaFieldNames(4) +
                                    " invalid=" + cAlphaArgs(4));
                    ErrorsFound = true;
                }
            }

            EarthTubeSys(Loop).AverSoilSurTemp = rNumericArgs(12);
            EarthTubeSys(Loop).ApmlSoilSurTemp = rNumericArgs(13);
            EarthTubeSys(Loop).SoilSurPhaseConst = int(rNumericArgs(14));

            // Override any user input for cases where natural ventilation is being used
            if (EarthTubeSys(Loop).FanType == NaturalEarthTube) {
                EarthTubeSys(Loop).FanPressure = 0.0;
                EarthTubeSys(Loop).FanEfficiency = 1.0;
            }

            EarthTubeSys(Loop).ConstantTermCoef = rNumericArgs(15);
            EarthTubeSys(Loop).TemperatureTermCoef = rNumericArgs(16);
            EarthTubeSys(Loop).VelocityTermCoef = rNumericArgs(17);
            EarthTubeSys(Loop).VelocitySQTermCoef = rNumericArgs(18);

            if (EarthTubeSys(Loop).ZonePtr > 0) {
                if (RepVarSet(EarthTubeSys(Loop).ZonePtr)) {
                    RepVarSet(EarthTubeSys(Loop).ZonePtr) = false;
                    SetupOutputVariable("Earth Tube Zone Sensible Cooling Energy",
                                        OutputProcessor::Unit::J,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeHeatLoss,
                                        "System",
                                        "NonState",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Zone Sensible Cooling Rate",
                                        OutputProcessor::Unit::W,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeHeatLossRate,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Zone Sensible Heating Energy",
                                        OutputProcessor::Unit::J,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeHeatGain,
                                        "System",
                                        "NonState",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Zone Sensible Heating Rate",
                                        OutputProcessor::Unit::W,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeHeatGainRate,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Air Flow Volume",
                                        OutputProcessor::Unit::m3,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeVolume,
                                        "System",
                                        "NonState",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Current Density Air Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeVolFlowRate,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Standard Density Air Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeVolFlowRateStd,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Air Flow Mass",
                                        OutputProcessor::Unit::kg,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeMass,
                                        "System",
                                        "NonState",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Air Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeMassFlowRate,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Water Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeWaterMassFlowRate,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Fan Electric Energy",
                                        OutputProcessor::Unit::J,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeFanElec,
                                        "System",
                                        "NonState",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name,
                                        _,
                                        "Electricity",
                                        _,
                                        _,
                                        "Building");
                    SetupOutputVariable("Earth Tube Fan Electric Power",
                                        OutputProcessor::Unit::W,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeFanElecPower,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Zone Inlet Air Temperature",
                                        OutputProcessor::Unit::C,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeAirTemp,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Ground Interface Temperature",
                                        OutputProcessor::Unit::C,
                                        EarthTubeSys(Loop).GroundTempz1z2t,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Outdoor Air Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeOATreatmentPower,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Zone Inlet Wet Bulb Temperature",
                                        OutputProcessor::Unit::C,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeWetBulbTemp,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                    SetupOutputVariable("Earth Tube Zone Inlet Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        ZnRptET(EarthTubeSys(Loop).ZonePtr).EarthTubeHumRat,
                                        "System",
                                        "State",
                                        Zone(EarthTubeSys(Loop).ZonePtr).Name);
                }
            }
        }

        CheckEarthTubesInZones(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

        if (ErrorsFound) {
            ShowFatalError(cCurrentModuleObject + ": Errors getting input.  Program terminates.");
        }
    }

    void CheckEarthTubesInZones(std::string const ZoneName,  // name of zone for error reporting
                                std::string const FieldName, // name of earth tube in input
                                bool &ErrorsFound            // Found a problem
    )
    {

        int Loop;
        int Loop1;

        // Check to make sure there is only one earth tube statement per zone
        for (Loop = 1; Loop <= TotEarthTube - 1; ++Loop) {
            for (Loop1 = Loop + 1; Loop1 <= TotEarthTube; ++Loop1) {
                if (EarthTubeSys(Loop).ZonePtr == EarthTubeSys(Loop1).ZonePtr) {
                    ShowSevereError(ZoneName + " has more than one " + FieldName + " associated with it.");
                    ShowContinueError("Only one " + FieldName + " is allowed per zone.  Check the definitions of " + FieldName);
                    ShowContinueError("in your input file and make sure that there is only one defined for each zone.");
                    ErrorsFound = true;
                }
            }
        }
    }

    void CalcEarthTube()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kwang Ho Lee
        //       DATE WRITTEN   November 2005

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the components making up the EarthTube unit.

        // Using/Aliasing
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int NZ;
        Real64 Process1;        // Variable Used in the Middle of the Calculation
        Real64 GroundTempz1z2t; // Average Ground Temperature between Depth z1 and z2 at time t

        Real64 AirThermCond;         // Thermal Conductivity of Air (W/mC)
        Real64 AirKinemVisco;        // Kinematic Viscosity of Air (m2/s)
        Real64 AirThermDiffus;       // Thermal Diffusivity of Air (m2/s)
        Real64 Re;                   // Reynolds Number for Flow Inside Pipe
        Real64 Pr;                   // Prandtl Number for Flow Inside Pipe
        Real64 Nu;                   // Nusselt Number for Flow Inside Pipe
        Real64 fa;                   // Friction Factor of Pipe
        Real64 PipeHeatTransCoef;    // Convective Heat Transfer Coefficient at Inner Pipe Surface
        Real64 Rc;                   // Thermal Resistance due to Convection between Air and Pipe Inner Surface
        Real64 Rp;                   // Thermal Resistance due to Conduction between Pipe Inner and Outer Surface
        Real64 Rs;                   // Thermal Resistance due to Conduction between Pipe Outer Surface and Soil
        Real64 Rt;                   // Total Thermal Resistance between Pipe Air and Soil
        Real64 OverallHeatTransCoef; // Overall Heat Transfer Coefficient of Earth Tube
        Real64 AverPipeAirVel;       // Average Pipe Air Velocity (m/s)
        Real64 AirMassFlowRate;      // Actual Mass Flow Rate of Air inside Pipe
        Real64 AirSpecHeat;          // Specific Heat of Air
        Real64 AirDensity;           // Density of Air
        static Array1D<Real64> EVF;  // DESIGN EARTHTUBE FLOW RATE (M**3/SEC)

        // Allocate the EVF array
        if (!allocated(EVF)) EVF.allocate(NumOfZones);

        EVF = 0.0;
        MCPTE = 0.0;
        MCPE = 0.0;
        EAMFL = 0.0;
        EAMFLxHumRat = 0.0;

        for (Loop = 1; Loop <= TotEarthTube; ++Loop) {

            NZ = EarthTubeSys(Loop).ZonePtr;
            EarthTubeSys(Loop).FanPower = 0.0;
            // Skip this if the zone is below the minimum temperature limit
            if (MAT(NZ) < EarthTubeSys(Loop).MinTemperature) continue;
            // Skip this if the zone is above the maximum temperature limit
            if (MAT(NZ) > EarthTubeSys(Loop).MaxTemperature) continue;
            // Skip if below the temperature difference limit
            if (std::abs(MAT(NZ) - OutDryBulbTemp) < EarthTubeSys(Loop).DelTemperature) continue;

            AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, OutDryBulbTemp, OutHumRat);
            AirSpecHeat = PsyCpAirFnW(OutHumRat);
            EVF(NZ) = EarthTubeSys(Loop).DesignLevel * GetCurrentScheduleValue(EarthTubeSys(Loop).SchedPtr);
            MCPE(NZ) = EVF(NZ) * AirDensity * AirSpecHeat *
                       (EarthTubeSys(Loop).ConstantTermCoef + std::abs(OutDryBulbTemp - MAT(NZ)) * EarthTubeSys(Loop).TemperatureTermCoef +
                        WindSpeed * (EarthTubeSys(Loop).VelocityTermCoef + WindSpeed * EarthTubeSys(Loop).VelocitySQTermCoef));

            EAMFL(NZ) = MCPE(NZ) / AirSpecHeat;
            if (EarthTubeSys(Loop).FanEfficiency > 0.0) {
                EarthTubeSys(Loop).FanPower = EAMFL(NZ) * EarthTubeSys(Loop).FanPressure / (EarthTubeSys(Loop).FanEfficiency * AirDensity);
            }

            AverPipeAirVel = EVF(NZ) / Pi / pow_2(EarthTubeSys(Loop).r1);
            AirMassFlowRate = EVF(NZ) * AirDensity;

            // Calculation of Average Ground Temperature between Depth z1 and z2 at time t
            GroundTempz1z2t = EarthTubeSys(Loop).AverSoilSurTemp -
                              EarthTubeSys(Loop).ApmlSoilSurTemp *
                                  std::exp(-EarthTubeSys(Loop).z * std::sqrt(Pi / 365.0 / EarthTubeSys(Loop).SoilThermDiff)) *
                                  std::cos(2.0 * Pi / 365.0 *
                                           (DayOfYear - EarthTubeSys(Loop).SoilSurPhaseConst -
                                            EarthTubeSys(Loop).z / 2.0 * std::sqrt(365.0 / Pi / EarthTubeSys(Loop).SoilThermDiff)));
            EarthTubeSys(Loop).GroundTempz1z2t = GroundTempz1z2t;

            // Calculation of Convective Heat Transfer Coefficient at Inner Pipe Surface
            AirThermCond = 0.02442 + 0.6992 * OutDryBulbTemp / 10000.0;
            AirKinemVisco = (0.1335 + 0.000925 * OutDryBulbTemp) / 10000.0;
            AirThermDiffus = (0.0014 * OutDryBulbTemp + 0.1872) / 10000.0;
            Re = 2.0 * EarthTubeSys(Loop).r1 * AverPipeAirVel / AirKinemVisco;
            Pr = AirKinemVisco / AirThermDiffus;
            if (Re <= 2300.0) {
                Nu = 3.66;
            } else if (Re <= 4000.0) {
                fa = std::pow(1.58 * std::log(Re) - 3.28, -2);
                Process1 = (fa / 2.0) * (Re - 1000.0) * Pr / (1.0 + 12.7 * std::sqrt(fa / 2.0) * (std::pow(Pr, 2.0 / 3.0) - 1.0));
                Nu = (Process1 - 3.66) / (1700.0) * Re + (4000.0 * 3.66 - 2300.0 * Process1) / 1700.0;
            } else {
                fa = std::pow(1.58 * std::log(Re) - 3.28, -2);
                Nu = (fa / 2.0) * (Re - 1000.0) * Pr / (1.0 + 12.7 * std::sqrt(fa / 2.0) * (std::pow(Pr, 2.0 / 3.0) - 1.0));
            }
            PipeHeatTransCoef = Nu * AirThermCond / 2.0 / EarthTubeSys(Loop).r1;

            // Claculation of Thermal Resistance and Overall Heat Transger Coefficient
            Rc = 1.0 / 2.0 / Pi / EarthTubeSys(Loop).r1 / PipeHeatTransCoef;
            Rp = std::log((EarthTubeSys(Loop).r1 + EarthTubeSys(Loop).r2) / EarthTubeSys(Loop).r1) / 2.0 / Pi / EarthTubeSys(Loop).PipeThermCond;
            Rs = std::log((EarthTubeSys(Loop).r1 + EarthTubeSys(Loop).r2 + EarthTubeSys(Loop).r3) / (EarthTubeSys(Loop).r1 + EarthTubeSys(Loop).r2)) /
                 2.0 / Pi / EarthTubeSys(Loop).SoilThermCond;
            Rt = Rc + Rp + Rs;
            OverallHeatTransCoef = 1.0 / Rt;

            if (AirMassFlowRate * AirSpecHeat == 0.0) {
                EarthTubeSys(Loop).InsideAirTemp = GroundTempz1z2t;

            } else {

                // Calculation of Pipe Outlet Air Temperature
                if (OutDryBulbTemp > GroundTempz1z2t) {
                    Process1 = (std::log(std::abs(OutDryBulbTemp - GroundTempz1z2t)) * AirMassFlowRate * AirSpecHeat -
                                OverallHeatTransCoef * EarthTubeSys(Loop).PipeLength) /
                               (AirMassFlowRate * AirSpecHeat);
                    EarthTubeSys(Loop).InsideAirTemp = std::exp(Process1) + GroundTempz1z2t;
                } else if (OutDryBulbTemp == GroundTempz1z2t) {
                    EarthTubeSys(Loop).InsideAirTemp = GroundTempz1z2t;
                } else {
                    Process1 = (std::log(std::abs(OutDryBulbTemp - GroundTempz1z2t)) * AirMassFlowRate * AirSpecHeat -
                                OverallHeatTransCoef * EarthTubeSys(Loop).PipeLength) /
                               (AirMassFlowRate * AirSpecHeat);
                    EarthTubeSys(Loop).InsideAirTemp = GroundTempz1z2t - std::exp(Process1);
                }
            }

            CalcEarthTubeHumRat(Loop, NZ);
        }
    }

    void CalcEarthTubeHumRat(int const Loop, // EarthTube number (index)
                             int const NZ    // Zone number (index)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kwang Ho Lee
        //       DATE WRITTEN   November 2005
        //       MODIFIED       Rick Strand, June 2017 (made this a separate subroutine)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines the leaving humidity ratio for the EarthTube
        // and calculates parameters associated with humidity ratio.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 InsideEnthalpy;
        Real64 OutletAirEnthalpy;
        Real64 InsideDewPointTemp;
        Real64 InsideHumRat;

        InsideDewPointTemp = PsyTdpFnWPb(OutHumRat, OutBaroPress);

        if (EarthTubeSys(Loop).InsideAirTemp >= InsideDewPointTemp) {
            InsideHumRat = OutHumRat;
            InsideEnthalpy = PsyHFnTdbW(EarthTubeSys(Loop).InsideAirTemp, OutHumRat);
            // Intake fans will add some heat to the air, raising the temperature for an intake fan...
            if (EarthTubeSys(Loop).FanType == IntakeEarthTube) {
                if (EAMFL(NZ) == 0.0) {
                    OutletAirEnthalpy = InsideEnthalpy;
                } else {
                    OutletAirEnthalpy = InsideEnthalpy + EarthTubeSys(Loop).FanPower / EAMFL(NZ);
                }
                EarthTubeSys(Loop).AirTemp = PsyTdbFnHW(OutletAirEnthalpy, OutHumRat);
            } else {
                EarthTubeSys(Loop).AirTemp = EarthTubeSys(Loop).InsideAirTemp;
            }
            MCPTE(NZ) = MCPE(NZ) * EarthTubeSys(Loop).AirTemp;

        } else {
            InsideHumRat = PsyWFnTdpPb(EarthTubeSys(Loop).InsideAirTemp, OutBaroPress);
            InsideEnthalpy = PsyHFnTdbW(EarthTubeSys(Loop).InsideAirTemp, InsideHumRat);
            // Intake fans will add some heat to the air, raising the temperature for an intake fan...
            if (EarthTubeSys(Loop).FanType == IntakeEarthTube) {
                if (EAMFL(NZ) == 0.0) {
                    OutletAirEnthalpy = InsideEnthalpy;
                } else {
                    OutletAirEnthalpy = InsideEnthalpy + EarthTubeSys(Loop).FanPower / EAMFL(NZ);
                }
                EarthTubeSys(Loop).AirTemp = PsyTdbFnHW(OutletAirEnthalpy, InsideHumRat);
            } else {
                EarthTubeSys(Loop).AirTemp = EarthTubeSys(Loop).InsideAirTemp;
            }
            MCPTE(NZ) = MCPE(NZ) * EarthTubeSys(Loop).AirTemp;
        }

        EarthTubeSys(Loop).HumRat = InsideHumRat;
        EarthTubeSys(Loop).WetBulbTemp = PsyTwbFnTdbWPb(EarthTubeSys(Loop).InsideAirTemp, InsideHumRat, OutBaroPress);
        EAMFLxHumRat(NZ) = EAMFL(NZ) * InsideHumRat;
    }

    void ReportEarthTube()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kwang Ho Lee
        //       DATE WRITTEN   November 2005
        //       MODIFIED       B. Griffith April 2010 added output reports

        // PURPOSE OF THIS SUBROUTINE: This subroutine fills remaining report variables.

        // Using/Aliasing
        using DataEnvironment::StdRhoAir;
        using DataHVACGlobals::TimeStepSys;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneLoop;     // Counter for the # of zones (nz)
        int EarthTubeNum; // Counter for EarthTube statements
        Real64 AirDensity;
        Real64 CpAir;
        Real64 ReportingConstant; // reporting constant for this module

        ReportingConstant = TimeStepSys * SecInHour;

        for (ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop) { // Start of zone loads report variable update loop ...

            // Break the infiltration load into heat gain and loss components.
            AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, OutDryBulbTemp, OutHumRat);
            CpAir = PsyCpAirFnW(OutHumRat);
            ZnRptET(ZoneLoop).EarthTubeVolume = (MCPE(ZoneLoop) / CpAir / AirDensity) * ReportingConstant;
            ZnRptET(ZoneLoop).EarthTubeMass = (MCPE(ZoneLoop) / CpAir) * ReportingConstant;
            ZnRptET(ZoneLoop).EarthTubeVolFlowRate = MCPE(ZoneLoop) / CpAir / AirDensity;
            ZnRptET(ZoneLoop).EarthTubeVolFlowRateStd = MCPE(ZoneLoop) / CpAir / StdRhoAir;
            ZnRptET(ZoneLoop).EarthTubeMassFlowRate = MCPE(ZoneLoop) / CpAir;
            ZnRptET(ZoneLoop).EarthTubeWaterMassFlowRate = EAMFLxHumRat(ZoneLoop);

            ZnRptET(ZoneLoop).EarthTubeFanElec = 0.0;
            ZnRptET(ZoneLoop).EarthTubeAirTemp = 0.0;
            for (EarthTubeNum = 1; EarthTubeNum <= TotEarthTube; ++EarthTubeNum) {
                if (EarthTubeSys(EarthTubeNum).ZonePtr == ZoneLoop) {
                    ZnRptET(ZoneLoop).EarthTubeFanElec = EarthTubeSys(EarthTubeNum).FanPower * ReportingConstant;
                    ZnRptET(ZoneLoop).EarthTubeFanElecPower = EarthTubeSys(EarthTubeNum).FanPower;

                    // Break the EarthTube load into heat gain and loss components.

                    if (ZT(ZoneLoop) > EarthTubeSys(EarthTubeNum).AirTemp) {

                        ZnRptET(ZoneLoop).EarthTubeHeatLoss =
                            MCPE(ZoneLoop) * (ZT(ZoneLoop) - EarthTubeSys(EarthTubeNum).AirTemp) * ReportingConstant;
                        ZnRptET(ZoneLoop).EarthTubeHeatLossRate = MCPE(ZoneLoop) * (ZT(ZoneLoop) - EarthTubeSys(EarthTubeNum).AirTemp);
                        ZnRptET(ZoneLoop).EarthTubeHeatGain = 0.0;
                        ZnRptET(ZoneLoop).EarthTubeHeatGainRate = 0.0;

                    } else if (ZT(ZoneLoop) <= EarthTubeSys(EarthTubeNum).AirTemp) {

                        ZnRptET(ZoneLoop).EarthTubeHeatGain =
                            MCPE(ZoneLoop) * (EarthTubeSys(EarthTubeNum).AirTemp - ZT(ZoneLoop)) * ReportingConstant;
                        ZnRptET(ZoneLoop).EarthTubeHeatGainRate = MCPE(ZoneLoop) * (EarthTubeSys(EarthTubeNum).AirTemp - ZT(ZoneLoop));
                        ZnRptET(ZoneLoop).EarthTubeHeatLoss = 0.0;
                        ZnRptET(ZoneLoop).EarthTubeHeatLossRate = 0.0;
                    }

                    ZnRptET(ZoneLoop).EarthTubeAirTemp = EarthTubeSys(EarthTubeNum).AirTemp;
                    ZnRptET(ZoneLoop).EarthTubeWetBulbTemp = EarthTubeSys(EarthTubeNum).WetBulbTemp;
                    ZnRptET(ZoneLoop).EarthTubeHumRat = EarthTubeSys(EarthTubeNum).HumRat;
                    ZnRptET(ZoneLoop).EarthTubeOATreatmentPower = MCPE(ZoneLoop) * (EarthTubeSys(EarthTubeNum).AirTemp - OutDryBulbTemp);
                    break; // DO loop
                }
            }

        } // ... end of zone loads report variable update loop.
    }

    //        End of Module Subroutines for EarthTube

    //*****************************************************************************************

} // namespace EarthTube

} // namespace EnergyPlus
