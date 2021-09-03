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

// ObjexxFCL Headers

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PollutionModule.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::PollutionModule {
// Module containing the pollution calculation routines

// MODULE INFORMATION:
//       AUTHOR         Richard J. Liesen (RJL)
//       DATE WRITTEN   August 2002
//       MODIFIED       January 17, 2004 - J Glazer - Added source energy support including schedules for source energy
//                      January 2008 - L Lawrie - implementing schedule fields for all emission factors.
//       RE-ENGINEERED  December 2003 RJL

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to
// calculate the pollution, and carbon eqiuvalent for the Energy consumed

// METHODOLOGY EMPLOYED:
// The methodology employed is to calculate the
// source pollution from building energy consumption.
//    PURPOSE:= Takes the Energy from the various sources and
//               calculates the Environmental Impact Factors.
//         STEP 1:  We begin with the output expressing the energy
//         STEP 2:  The energy used by types: must be converted back
//         to source fuel types (fossil or electricity) via User Input.
//         STEP 3:  All energy numbers have been converted to units of MJ's or 1x10^6 Joules.
//         STEP 4:  Environmental Impact Factors are calculated from Coefficients

// MODULE VARIABLE DECLARATIONS:
// Total for all of the Pollutants
// Total Carbon Equivalent Components
//  !Fuel Types
// Total Carbon Equivalent Coeffs
// Purchased Efficiencies

// Fuel Types used with the Pollution Factors
// Facility Meter Indexes
// Facility Meter Values used in Pollution Calcs

void CalculatePollution(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   August 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  December 2003 RJL

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is the main driver for the pollution calculation

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    if (!state.dataPollutionModule->PollutionReportSetup) return;

    //   Call the Routine to Read the Energy Values from the EnergyPlus Meters
    ReadEnergyMeters(state);

    //   Call the routine that takes the fuel data and calculates the
    //     Pollution for each fuel type.
    CalcPollution(state);
}

// Get Input Section of the Module
//******************************************************************************

void SetupPollutionCalculations(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   August 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  December 2003 RJL; August 2008 LKL - more standard getinput

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is the input routines and Get routines

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // Using/Aliasing
    using ScheduleManager::GetScheduleIndex;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumPolluteRpt;
    int NumAlphas;
    int NumNums;
    int Loop;
    int IOStat;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

    // First determine if the Pollution reporting has been triggered, and is not exit.
    cCurrentModuleObject = "Output:EnvironmentalImpactFactors";
    NumPolluteRpt = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataPollutionModule->PollutionReportSetup = true;

    for (Loop = 1; Loop <= NumPolluteRpt; ++Loop) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Loop,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        // Call this routine in the Output Processor to setup the correct Facility energy meters that are
        //  necessary to make sure that the Meter file is opened and written to by the OP so that time stamps
        //  and the like are happening as expected.
        if (!state.dataIPShortCut->lAlphaFieldBlanks(1)) {
            InitPollutionMeterReporting(state, state.dataIPShortCut->cAlphaArgs(1));
        } else {
            InitPollutionMeterReporting(state, "RunPeriod");
        }
    }
}

void GetPollutionFactorInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   August 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // SetupPollutionCalculation must be called after meters are initialized.  This caused a problem
    // in runs so have added this routine to allow central get for most inputs.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas;
    int NumNums;
    int Loop;
    int IOStat;
    bool ErrorsFound(false);
    auto &Pollution = state.dataPollutionModule->Pollution;
    auto &FuelType = state.dataPollutionModule->FuelType;
    if (!state.dataPollutionModule->GetInputFlagPollution) return; // Input already gotten
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    state.dataPollutionModule->GetInputFlagPollution = false;

    cCurrentModuleObject = "EnvironmentalImpactFactors";
    state.dataPollutionModule->NumEnvImpactFactors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    if (state.dataPollutionModule->NumEnvImpactFactors > 0) {
        // Now find and load all of the user inputs and factors.
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 1,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
    } else {
        if (state.dataPollutionModule->PollutionReportSetup)
            ShowWarningError(state, cCurrentModuleObject + ": not entered.  Values will be defaulted.");
    }

    Pollution.PurchHeatEffic = 0.3;
    Pollution.PurchCoolCOP = 3.0;
    Pollution.SteamConvEffic = 0.25;
    Pollution.CarbonEquivN2O = 0.0;
    Pollution.CarbonEquivCH4 = 0.0;
    Pollution.CarbonEquivCO2 = 0.0;

    if (state.dataPollutionModule->NumEnvImpactFactors > 0) {
        // If Heating Efficiency defined by the User is negative or zero then a default of 30% will be assigned.
        if (state.dataIPShortCut->rNumericArgs(1) > 0.0) {
            Pollution.PurchHeatEffic = state.dataIPShortCut->rNumericArgs(1);
        }

        // If COP defined by the User is negative or zero then a default of 3.0 will be assigned.
        if (state.dataIPShortCut->rNumericArgs(2) > 0.0) {
            Pollution.PurchCoolCOP = state.dataIPShortCut->rNumericArgs(2);
        }

        // If Steam Conversion Efficiency defined by the User is negative or zero then a default of 25% will be assigned.
        if (state.dataIPShortCut->rNumericArgs(1) > 0.0) {
            Pollution.SteamConvEffic = state.dataIPShortCut->rNumericArgs(3);
        }

        // Load the Total Carbon Equivalent Pollution Factor coefficients
        Pollution.CarbonEquivN2O = state.dataIPShortCut->rNumericArgs(4);
        Pollution.CarbonEquivCH4 = state.dataIPShortCut->rNumericArgs(5);
        Pollution.CarbonEquivCO2 = state.dataIPShortCut->rNumericArgs(6);
    }

    // Compare all of the Fuel Factors and compare to PollutionCalculationFactors List
    cCurrentModuleObject = "FuelFactors";
    state.dataPollutionModule->NumFuelFactors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    for (Loop = 1; Loop <= state.dataPollutionModule->NumFuelFactors; ++Loop) {
        // Now find and load all of the user inputs and factors.
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Loop,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        FuelType.FuelTypeNames(Loop) = state.dataIPShortCut->cAlphaArgs(1);

        {
            auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(FuelType.FuelTypeNames(Loop)));
            if (SELECT_CASE_var == "NATURALGAS") {
                if (Pollution.NatGasCoef.FuelFactorUsed) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": " + FuelType.FuelTypeNames(Loop) + " already entered. Previous entry will be used.");
                    continue;
                }
                Pollution.NatGasCoef.FuelFactorUsed = true;
                // Natural Gas Coeffs
                Pollution.NatGasCoef.Source = state.dataIPShortCut->rNumericArgs(2);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(3),
                                    state.dataIPShortCut->cAlphaArgs(3),
                                    Pollution.NatGasCoef.SourceSched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.CO2 = state.dataIPShortCut->rNumericArgs(3);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(4),
                                    state.dataIPShortCut->cAlphaArgs(4),
                                    Pollution.NatGasCoef.CO2Sched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.CO = state.dataIPShortCut->rNumericArgs(4);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(5),
                                    state.dataIPShortCut->cAlphaArgs(5),
                                    Pollution.NatGasCoef.COSched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.CH4 = state.dataIPShortCut->rNumericArgs(5);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(6),
                                    state.dataIPShortCut->cAlphaArgs(6),
                                    Pollution.NatGasCoef.CH4Sched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.NOx = state.dataIPShortCut->rNumericArgs(6);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(7),
                                    state.dataIPShortCut->cAlphaArgs(7),
                                    Pollution.NatGasCoef.NOxSched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.N2O = state.dataIPShortCut->rNumericArgs(7);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(8),
                                    state.dataIPShortCut->cAlphaArgs(8),
                                    Pollution.NatGasCoef.N2OSched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.SO2 = state.dataIPShortCut->rNumericArgs(8);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(9),
                                    state.dataIPShortCut->cAlphaArgs(9),
                                    Pollution.NatGasCoef.SO2Sched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.PM = state.dataIPShortCut->rNumericArgs(9);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(10),
                                    state.dataIPShortCut->cAlphaArgs(10),
                                    Pollution.NatGasCoef.PMSched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.PM10 = state.dataIPShortCut->rNumericArgs(10);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(11),
                                    state.dataIPShortCut->cAlphaArgs(11),
                                    Pollution.NatGasCoef.PM10Sched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.PM25 = state.dataIPShortCut->rNumericArgs(11);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(12),
                                    state.dataIPShortCut->cAlphaArgs(12),
                                    Pollution.NatGasCoef.PM25Sched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.NH3 = state.dataIPShortCut->rNumericArgs(12);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(13),
                                    state.dataIPShortCut->cAlphaArgs(13),
                                    Pollution.NatGasCoef.NH3Sched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.NMVOC = state.dataIPShortCut->rNumericArgs(13);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(14),
                                    state.dataIPShortCut->cAlphaArgs(14),
                                    Pollution.NatGasCoef.NMVOCSched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.Hg = state.dataIPShortCut->rNumericArgs(14);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(15),
                                    state.dataIPShortCut->cAlphaArgs(15),
                                    Pollution.NatGasCoef.HgSched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.Pb = state.dataIPShortCut->rNumericArgs(15);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(16),
                                    state.dataIPShortCut->cAlphaArgs(16),
                                    Pollution.NatGasCoef.PbSched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.Water = state.dataIPShortCut->rNumericArgs(16);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(17),
                                    state.dataIPShortCut->cAlphaArgs(17),
                                    Pollution.NatGasCoef.WaterSched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.NucHi = state.dataIPShortCut->rNumericArgs(17);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(18),
                                    state.dataIPShortCut->cAlphaArgs(18),
                                    Pollution.NatGasCoef.NucHiSched,
                                    ErrorsFound);
                }
                Pollution.NatGasCoef.NucLo = state.dataIPShortCut->rNumericArgs(18);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "NaturalGas",
                                    state.dataIPShortCut->cAlphaFieldNames(19),
                                    state.dataIPShortCut->cAlphaArgs(19),
                                    Pollution.NatGasCoef.NucLoSched,
                                    ErrorsFound);
                }

            } else if (SELECT_CASE_var == "FUELOILNO2") {
                if (Pollution.FuelOil2Coef.FuelFactorUsed) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": " + FuelType.FuelTypeNames(Loop) + " already entered. Previous entry will be used.");
                    continue;
                }
                Pollution.FuelOil2Coef.FuelFactorUsed = true;
                // FuelOilNo2 Coeffs
                Pollution.FuelOil2Coef.Source = state.dataIPShortCut->rNumericArgs(2);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(3),
                                    state.dataIPShortCut->cAlphaArgs(3),
                                    Pollution.FuelOil2Coef.SourceSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.CO2 = state.dataIPShortCut->rNumericArgs(3);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(4),
                                    state.dataIPShortCut->cAlphaArgs(4),
                                    Pollution.FuelOil2Coef.CO2Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.CO = state.dataIPShortCut->rNumericArgs(4);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(5),
                                    state.dataIPShortCut->cAlphaArgs(5),
                                    Pollution.FuelOil2Coef.COSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.CH4 = state.dataIPShortCut->rNumericArgs(5);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(6),
                                    state.dataIPShortCut->cAlphaArgs(6),
                                    Pollution.FuelOil2Coef.CH4Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.NOx = state.dataIPShortCut->rNumericArgs(6);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(7),
                                    state.dataIPShortCut->cAlphaArgs(7),
                                    Pollution.FuelOil2Coef.NOxSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.N2O = state.dataIPShortCut->rNumericArgs(7);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(8),
                                    state.dataIPShortCut->cAlphaArgs(8),
                                    Pollution.FuelOil2Coef.N2OSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.SO2 = state.dataIPShortCut->rNumericArgs(8);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(9),
                                    state.dataIPShortCut->cAlphaArgs(9),
                                    Pollution.FuelOil2Coef.SO2Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.PM = state.dataIPShortCut->rNumericArgs(9);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(10),
                                    state.dataIPShortCut->cAlphaArgs(10),
                                    Pollution.FuelOil2Coef.PMSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.PM10 = state.dataIPShortCut->rNumericArgs(10);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(11),
                                    state.dataIPShortCut->cAlphaArgs(11),
                                    Pollution.FuelOil2Coef.PM10Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.PM25 = state.dataIPShortCut->rNumericArgs(11);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(12),
                                    state.dataIPShortCut->cAlphaArgs(12),
                                    Pollution.FuelOil2Coef.PM25Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.NH3 = state.dataIPShortCut->rNumericArgs(12);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(13),
                                    state.dataIPShortCut->cAlphaArgs(13),
                                    Pollution.FuelOil2Coef.NH3Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.NMVOC = state.dataIPShortCut->rNumericArgs(13);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(14),
                                    state.dataIPShortCut->cAlphaArgs(14),
                                    Pollution.FuelOil2Coef.NMVOCSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.Hg = state.dataIPShortCut->rNumericArgs(14);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(15),
                                    state.dataIPShortCut->cAlphaArgs(15),
                                    Pollution.FuelOil2Coef.HgSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.Pb = state.dataIPShortCut->rNumericArgs(15);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(16),
                                    state.dataIPShortCut->cAlphaArgs(16),
                                    Pollution.FuelOil2Coef.PbSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.Water = state.dataIPShortCut->rNumericArgs(16);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(17),
                                    state.dataIPShortCut->cAlphaArgs(17),
                                    Pollution.FuelOil2Coef.WaterSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.NucHi = state.dataIPShortCut->rNumericArgs(17);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(18),
                                    state.dataIPShortCut->cAlphaArgs(18),
                                    Pollution.FuelOil2Coef.NucHiSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil2Coef.NucLo = state.dataIPShortCut->rNumericArgs(18);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#2",
                                    state.dataIPShortCut->cAlphaFieldNames(19),
                                    state.dataIPShortCut->cAlphaArgs(19),
                                    Pollution.FuelOil2Coef.NucLoSched,
                                    ErrorsFound);
                }

            } else if (SELECT_CASE_var == "FUELOILNO1") {
                if (Pollution.FuelOil1Coef.FuelFactorUsed) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": " + FuelType.FuelTypeNames(Loop) + " already entered. Previous entry will be used.");
                    continue;
                }
                Pollution.FuelOil1Coef.FuelFactorUsed = true;
                // FuelOilNo1 Coeffs
                Pollution.FuelOil1Coef.Source = state.dataIPShortCut->rNumericArgs(2);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(3),
                                    state.dataIPShortCut->cAlphaArgs(3),
                                    Pollution.FuelOil1Coef.SourceSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.CO2 = state.dataIPShortCut->rNumericArgs(3);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(4),
                                    state.dataIPShortCut->cAlphaArgs(4),
                                    Pollution.FuelOil1Coef.CO2Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.CO = state.dataIPShortCut->rNumericArgs(4);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(5),
                                    state.dataIPShortCut->cAlphaArgs(5),
                                    Pollution.FuelOil1Coef.COSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.CH4 = state.dataIPShortCut->rNumericArgs(5);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(6),
                                    state.dataIPShortCut->cAlphaArgs(6),
                                    Pollution.FuelOil1Coef.CH4Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.NOx = state.dataIPShortCut->rNumericArgs(6);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(7),
                                    state.dataIPShortCut->cAlphaArgs(7),
                                    Pollution.FuelOil1Coef.NOxSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.N2O = state.dataIPShortCut->rNumericArgs(7);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(8),
                                    state.dataIPShortCut->cAlphaArgs(8),
                                    Pollution.FuelOil1Coef.N2OSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.SO2 = state.dataIPShortCut->rNumericArgs(8);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(9),
                                    state.dataIPShortCut->cAlphaArgs(9),
                                    Pollution.FuelOil1Coef.SO2Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.PM = state.dataIPShortCut->rNumericArgs(9);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(10),
                                    state.dataIPShortCut->cAlphaArgs(10),
                                    Pollution.FuelOil1Coef.PMSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.PM10 = state.dataIPShortCut->rNumericArgs(10);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(11),
                                    state.dataIPShortCut->cAlphaArgs(11),
                                    Pollution.FuelOil1Coef.PM10Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.PM25 = state.dataIPShortCut->rNumericArgs(11);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(12),
                                    state.dataIPShortCut->cAlphaArgs(12),
                                    Pollution.FuelOil1Coef.PM25Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.NH3 = state.dataIPShortCut->rNumericArgs(12);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(13),
                                    state.dataIPShortCut->cAlphaArgs(13),
                                    Pollution.FuelOil1Coef.NH3Sched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.NMVOC = state.dataIPShortCut->rNumericArgs(13);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(14),
                                    state.dataIPShortCut->cAlphaArgs(14),
                                    Pollution.FuelOil1Coef.NMVOCSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.Hg = state.dataIPShortCut->rNumericArgs(14);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(15),
                                    state.dataIPShortCut->cAlphaArgs(15),
                                    Pollution.FuelOil1Coef.HgSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.Pb = state.dataIPShortCut->rNumericArgs(15);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(16),
                                    state.dataIPShortCut->cAlphaArgs(16),
                                    Pollution.FuelOil1Coef.PbSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.Water = state.dataIPShortCut->rNumericArgs(16);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(17),
                                    state.dataIPShortCut->cAlphaArgs(17),
                                    Pollution.FuelOil1Coef.WaterSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.NucHi = state.dataIPShortCut->rNumericArgs(17);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(18),
                                    state.dataIPShortCut->cAlphaArgs(18),
                                    Pollution.FuelOil1Coef.NucHiSched,
                                    ErrorsFound);
                }
                Pollution.FuelOil1Coef.NucLo = state.dataIPShortCut->rNumericArgs(18);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Fuel Oil#1",
                                    state.dataIPShortCut->cAlphaFieldNames(19),
                                    state.dataIPShortCut->cAlphaArgs(19),
                                    Pollution.FuelOil1Coef.NucLoSched,
                                    ErrorsFound);
                }

            } else if (SELECT_CASE_var == "COAL") {
                if (Pollution.CoalCoef.FuelFactorUsed) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": " + FuelType.FuelTypeNames(Loop) + " already entered. Previous entry will be used.");
                    continue;
                }
                Pollution.CoalCoef.FuelFactorUsed = true;
                // Coal
                Pollution.CoalCoef.Source = state.dataIPShortCut->rNumericArgs(2);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(3),
                                    state.dataIPShortCut->cAlphaArgs(3),
                                    Pollution.CoalCoef.SourceSched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.CO2 = state.dataIPShortCut->rNumericArgs(3);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(4),
                                    state.dataIPShortCut->cAlphaArgs(4),
                                    Pollution.CoalCoef.CO2Sched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.CO = state.dataIPShortCut->rNumericArgs(4);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(5),
                                    state.dataIPShortCut->cAlphaArgs(5),
                                    Pollution.CoalCoef.COSched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.CH4 = state.dataIPShortCut->rNumericArgs(5);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(6),
                                    state.dataIPShortCut->cAlphaArgs(6),
                                    Pollution.CoalCoef.CH4Sched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.NOx = state.dataIPShortCut->rNumericArgs(6);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(7),
                                    state.dataIPShortCut->cAlphaArgs(7),
                                    Pollution.CoalCoef.NOxSched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.N2O = state.dataIPShortCut->rNumericArgs(7);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(8),
                                    state.dataIPShortCut->cAlphaArgs(8),
                                    Pollution.CoalCoef.N2OSched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.SO2 = state.dataIPShortCut->rNumericArgs(8);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(9),
                                    state.dataIPShortCut->cAlphaArgs(9),
                                    Pollution.CoalCoef.SO2Sched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.PM = state.dataIPShortCut->rNumericArgs(9);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(10),
                                    state.dataIPShortCut->cAlphaArgs(10),
                                    Pollution.CoalCoef.PMSched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.PM10 = state.dataIPShortCut->rNumericArgs(10);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(11),
                                    state.dataIPShortCut->cAlphaArgs(11),
                                    Pollution.CoalCoef.PM10Sched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.PM25 = state.dataIPShortCut->rNumericArgs(11);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(12),
                                    state.dataIPShortCut->cAlphaArgs(12),
                                    Pollution.CoalCoef.PM25Sched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.NH3 = state.dataIPShortCut->rNumericArgs(12);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(13),
                                    state.dataIPShortCut->cAlphaArgs(13),
                                    Pollution.CoalCoef.NH3Sched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.NMVOC = state.dataIPShortCut->rNumericArgs(13);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(14),
                                    state.dataIPShortCut->cAlphaArgs(14),
                                    Pollution.CoalCoef.NMVOCSched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.Hg = state.dataIPShortCut->rNumericArgs(14);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(15),
                                    state.dataIPShortCut->cAlphaArgs(15),
                                    Pollution.CoalCoef.HgSched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.Pb = state.dataIPShortCut->rNumericArgs(15);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(16),
                                    state.dataIPShortCut->cAlphaArgs(16),
                                    Pollution.CoalCoef.PbSched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.Water = state.dataIPShortCut->rNumericArgs(16);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(17),
                                    state.dataIPShortCut->cAlphaArgs(17),
                                    Pollution.CoalCoef.WaterSched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.NucHi = state.dataIPShortCut->rNumericArgs(17);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(18),
                                    state.dataIPShortCut->cAlphaArgs(18),
                                    Pollution.CoalCoef.NucHiSched,
                                    ErrorsFound);
                }
                Pollution.CoalCoef.NucLo = state.dataIPShortCut->rNumericArgs(18);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Coal",
                                    state.dataIPShortCut->cAlphaFieldNames(19),
                                    state.dataIPShortCut->cAlphaArgs(19),
                                    Pollution.CoalCoef.NucLoSched,
                                    ErrorsFound);
                }

            } else if (SELECT_CASE_var == "ELECTRICITY") {
                if (Pollution.ElecCoef.FuelFactorUsed) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": " + FuelType.FuelTypeNames(Loop) + " already entered. Previous entry will be used.");
                    continue;
                }
                Pollution.ElecCoef.FuelFactorUsed = true;
                // Electric Coeffs
                Pollution.ElecCoef.Source = state.dataIPShortCut->rNumericArgs(2);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(3),
                                    state.dataIPShortCut->cAlphaArgs(3),
                                    Pollution.ElecCoef.SourceSched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.CO2 = state.dataIPShortCut->rNumericArgs(3);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(4),
                                    state.dataIPShortCut->cAlphaArgs(4),
                                    Pollution.ElecCoef.CO2Sched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.CO = state.dataIPShortCut->rNumericArgs(4);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(5),
                                    state.dataIPShortCut->cAlphaArgs(5),
                                    Pollution.ElecCoef.COSched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.CH4 = state.dataIPShortCut->rNumericArgs(5);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(6),
                                    state.dataIPShortCut->cAlphaArgs(6),
                                    Pollution.ElecCoef.CH4Sched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.NOx = state.dataIPShortCut->rNumericArgs(6);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(7),
                                    state.dataIPShortCut->cAlphaArgs(7),
                                    Pollution.ElecCoef.NOxSched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.N2O = state.dataIPShortCut->rNumericArgs(7);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(8),
                                    state.dataIPShortCut->cAlphaArgs(8),
                                    Pollution.ElecCoef.N2OSched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.SO2 = state.dataIPShortCut->rNumericArgs(8);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(9),
                                    state.dataIPShortCut->cAlphaArgs(9),
                                    Pollution.ElecCoef.SO2Sched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.PM = state.dataIPShortCut->rNumericArgs(9);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(10),
                                    state.dataIPShortCut->cAlphaArgs(10),
                                    Pollution.ElecCoef.PMSched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.PM10 = state.dataIPShortCut->rNumericArgs(10);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(11),
                                    state.dataIPShortCut->cAlphaArgs(11),
                                    Pollution.ElecCoef.PM10Sched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.PM25 = state.dataIPShortCut->rNumericArgs(11);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(12),
                                    state.dataIPShortCut->cAlphaArgs(12),
                                    Pollution.ElecCoef.PM25Sched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.NH3 = state.dataIPShortCut->rNumericArgs(12);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(13),
                                    state.dataIPShortCut->cAlphaArgs(13),
                                    Pollution.ElecCoef.NH3Sched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.NMVOC = state.dataIPShortCut->rNumericArgs(13);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(14),
                                    state.dataIPShortCut->cAlphaArgs(14),
                                    Pollution.ElecCoef.NMVOCSched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.Hg = state.dataIPShortCut->rNumericArgs(14);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(15),
                                    state.dataIPShortCut->cAlphaArgs(15),
                                    Pollution.ElecCoef.HgSched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.Pb = state.dataIPShortCut->rNumericArgs(15);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(16),
                                    state.dataIPShortCut->cAlphaArgs(16),
                                    Pollution.ElecCoef.PbSched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.Water = state.dataIPShortCut->rNumericArgs(16);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(17),
                                    state.dataIPShortCut->cAlphaArgs(17),
                                    Pollution.ElecCoef.WaterSched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.NucHi = state.dataIPShortCut->rNumericArgs(17);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(18),
                                    state.dataIPShortCut->cAlphaArgs(18),
                                    Pollution.ElecCoef.NucHiSched,
                                    ErrorsFound);
                }
                Pollution.ElecCoef.NucLo = state.dataIPShortCut->rNumericArgs(18);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Electricity",
                                    state.dataIPShortCut->cAlphaFieldNames(19),
                                    state.dataIPShortCut->cAlphaArgs(19),
                                    Pollution.ElecCoef.NucLoSched,
                                    ErrorsFound);
                }

            } else if (SELECT_CASE_var == "GASOLINE") {
                if (Pollution.GasolineCoef.FuelFactorUsed) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": " + FuelType.FuelTypeNames(Loop) + " already entered. Previous entry will be used.");
                    continue;
                }
                Pollution.GasolineCoef.FuelFactorUsed = true;
                // Gasoline Coeffs
                Pollution.GasolineCoef.Source = state.dataIPShortCut->rNumericArgs(2);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(3),
                                    state.dataIPShortCut->cAlphaArgs(3),
                                    Pollution.GasolineCoef.SourceSched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.CO2 = state.dataIPShortCut->rNumericArgs(3);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(4),
                                    state.dataIPShortCut->cAlphaArgs(4),
                                    Pollution.GasolineCoef.CO2Sched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.CO = state.dataIPShortCut->rNumericArgs(4);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(5),
                                    state.dataIPShortCut->cAlphaArgs(5),
                                    Pollution.GasolineCoef.COSched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.CH4 = state.dataIPShortCut->rNumericArgs(5);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(6),
                                    state.dataIPShortCut->cAlphaArgs(6),
                                    Pollution.GasolineCoef.CH4Sched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.NOx = state.dataIPShortCut->rNumericArgs(6);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(7),
                                    state.dataIPShortCut->cAlphaArgs(7),
                                    Pollution.GasolineCoef.NOxSched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.N2O = state.dataIPShortCut->rNumericArgs(7);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(8),
                                    state.dataIPShortCut->cAlphaArgs(8),
                                    Pollution.GasolineCoef.N2OSched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.SO2 = state.dataIPShortCut->rNumericArgs(8);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(9),
                                    state.dataIPShortCut->cAlphaArgs(9),
                                    Pollution.GasolineCoef.SO2Sched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.PM = state.dataIPShortCut->rNumericArgs(9);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(10),
                                    state.dataIPShortCut->cAlphaArgs(10),
                                    Pollution.GasolineCoef.PMSched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.PM10 = state.dataIPShortCut->rNumericArgs(10);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(11),
                                    state.dataIPShortCut->cAlphaArgs(11),
                                    Pollution.GasolineCoef.PM10Sched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.PM25 = state.dataIPShortCut->rNumericArgs(11);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(12),
                                    state.dataIPShortCut->cAlphaArgs(12),
                                    Pollution.GasolineCoef.PM25Sched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.NH3 = state.dataIPShortCut->rNumericArgs(12);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(13),
                                    state.dataIPShortCut->cAlphaArgs(13),
                                    Pollution.GasolineCoef.NH3Sched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.NMVOC = state.dataIPShortCut->rNumericArgs(13);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(14),
                                    state.dataIPShortCut->cAlphaArgs(14),
                                    Pollution.GasolineCoef.NMVOCSched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.Hg = state.dataIPShortCut->rNumericArgs(14);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(15),
                                    state.dataIPShortCut->cAlphaArgs(15),
                                    Pollution.GasolineCoef.HgSched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.Pb = state.dataIPShortCut->rNumericArgs(15);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(16),
                                    state.dataIPShortCut->cAlphaArgs(16),
                                    Pollution.GasolineCoef.PbSched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.Water = state.dataIPShortCut->rNumericArgs(16);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(17),
                                    state.dataIPShortCut->cAlphaArgs(17),
                                    Pollution.GasolineCoef.WaterSched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.NucHi = state.dataIPShortCut->rNumericArgs(17);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(18),
                                    state.dataIPShortCut->cAlphaArgs(18),
                                    Pollution.GasolineCoef.NucHiSched,
                                    ErrorsFound);
                }
                Pollution.GasolineCoef.NucLo = state.dataIPShortCut->rNumericArgs(18);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Gasoline",
                                    state.dataIPShortCut->cAlphaFieldNames(19),
                                    state.dataIPShortCut->cAlphaArgs(19),
                                    Pollution.GasolineCoef.NucLoSched,
                                    ErrorsFound);
                }

            } else if (SELECT_CASE_var == "PROPANE") {
                if (Pollution.PropaneCoef.FuelFactorUsed) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": " + FuelType.FuelTypeNames(Loop) + " already entered. Previous entry will be used.");
                    continue;
                }
                Pollution.PropaneCoef.FuelFactorUsed = true;
                // Propane Coeffs
                Pollution.PropaneCoef.Source = state.dataIPShortCut->rNumericArgs(2);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(3),
                                    state.dataIPShortCut->cAlphaArgs(3),
                                    Pollution.PropaneCoef.SourceSched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.CO2 = state.dataIPShortCut->rNumericArgs(3);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(4),
                                    state.dataIPShortCut->cAlphaArgs(4),
                                    Pollution.PropaneCoef.CO2Sched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.CO = state.dataIPShortCut->rNumericArgs(4);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(5),
                                    state.dataIPShortCut->cAlphaArgs(5),
                                    Pollution.PropaneCoef.COSched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.CH4 = state.dataIPShortCut->rNumericArgs(5);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(6),
                                    state.dataIPShortCut->cAlphaArgs(6),
                                    Pollution.PropaneCoef.CH4Sched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.NOx = state.dataIPShortCut->rNumericArgs(6);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(7),
                                    state.dataIPShortCut->cAlphaArgs(7),
                                    Pollution.PropaneCoef.NOxSched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.N2O = state.dataIPShortCut->rNumericArgs(7);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(8),
                                    state.dataIPShortCut->cAlphaArgs(8),
                                    Pollution.PropaneCoef.N2OSched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.SO2 = state.dataIPShortCut->rNumericArgs(8);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(9),
                                    state.dataIPShortCut->cAlphaArgs(9),
                                    Pollution.PropaneCoef.SO2Sched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.PM = state.dataIPShortCut->rNumericArgs(9);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(10),
                                    state.dataIPShortCut->cAlphaArgs(10),
                                    Pollution.PropaneCoef.PMSched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.PM10 = state.dataIPShortCut->rNumericArgs(10);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(11),
                                    state.dataIPShortCut->cAlphaArgs(11),
                                    Pollution.PropaneCoef.PM10Sched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.PM25 = state.dataIPShortCut->rNumericArgs(11);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(12),
                                    state.dataIPShortCut->cAlphaArgs(12),
                                    Pollution.PropaneCoef.PM25Sched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.NH3 = state.dataIPShortCut->rNumericArgs(12);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(13),
                                    state.dataIPShortCut->cAlphaArgs(13),
                                    Pollution.PropaneCoef.NH3Sched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.NMVOC = state.dataIPShortCut->rNumericArgs(13);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(14),
                                    state.dataIPShortCut->cAlphaArgs(14),
                                    Pollution.PropaneCoef.NMVOCSched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.Hg = state.dataIPShortCut->rNumericArgs(14);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(15),
                                    state.dataIPShortCut->cAlphaArgs(15),
                                    Pollution.PropaneCoef.HgSched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.Pb = state.dataIPShortCut->rNumericArgs(15);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(16),
                                    state.dataIPShortCut->cAlphaArgs(16),
                                    Pollution.PropaneCoef.PbSched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.Water = state.dataIPShortCut->rNumericArgs(16);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(17),
                                    state.dataIPShortCut->cAlphaArgs(17),
                                    Pollution.PropaneCoef.WaterSched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.NucHi = state.dataIPShortCut->rNumericArgs(17);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(18),
                                    state.dataIPShortCut->cAlphaArgs(18),
                                    Pollution.PropaneCoef.NucHiSched,
                                    ErrorsFound);
                }
                Pollution.PropaneCoef.NucLo = state.dataIPShortCut->rNumericArgs(18);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Propane",
                                    state.dataIPShortCut->cAlphaFieldNames(19),
                                    state.dataIPShortCut->cAlphaArgs(19),
                                    Pollution.PropaneCoef.NucLoSched,
                                    ErrorsFound);
                }

            } else if (SELECT_CASE_var == "DIESEL") {
                if (Pollution.DieselCoef.FuelFactorUsed) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": " + FuelType.FuelTypeNames(Loop) + " already entered. Previous entry will be used.");
                    continue;
                }
                Pollution.DieselCoef.FuelFactorUsed = true;
                // Diesel Coeffs
                Pollution.DieselCoef.Source = state.dataIPShortCut->rNumericArgs(2);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(3),
                                    state.dataIPShortCut->cAlphaArgs(3),
                                    Pollution.DieselCoef.SourceSched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.CO2 = state.dataIPShortCut->rNumericArgs(3);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(4),
                                    state.dataIPShortCut->cAlphaArgs(4),
                                    Pollution.DieselCoef.CO2Sched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.CO = state.dataIPShortCut->rNumericArgs(4);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(5),
                                    state.dataIPShortCut->cAlphaArgs(5),
                                    Pollution.DieselCoef.COSched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.CH4 = state.dataIPShortCut->rNumericArgs(5);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(6),
                                    state.dataIPShortCut->cAlphaArgs(6),
                                    Pollution.DieselCoef.CH4Sched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.NOx = state.dataIPShortCut->rNumericArgs(6);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(7),
                                    state.dataIPShortCut->cAlphaArgs(7),
                                    Pollution.DieselCoef.NOxSched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.N2O = state.dataIPShortCut->rNumericArgs(7);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(8),
                                    state.dataIPShortCut->cAlphaArgs(8),
                                    Pollution.DieselCoef.N2OSched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.SO2 = state.dataIPShortCut->rNumericArgs(8);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(9),
                                    state.dataIPShortCut->cAlphaArgs(9),
                                    Pollution.DieselCoef.SO2Sched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.PM = state.dataIPShortCut->rNumericArgs(9);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(10),
                                    state.dataIPShortCut->cAlphaArgs(10),
                                    Pollution.DieselCoef.PMSched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.PM10 = state.dataIPShortCut->rNumericArgs(10);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(11),
                                    state.dataIPShortCut->cAlphaArgs(11),
                                    Pollution.DieselCoef.PM10Sched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.PM25 = state.dataIPShortCut->rNumericArgs(11);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(12),
                                    state.dataIPShortCut->cAlphaArgs(12),
                                    Pollution.DieselCoef.PM25Sched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.NH3 = state.dataIPShortCut->rNumericArgs(12);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(13),
                                    state.dataIPShortCut->cAlphaArgs(13),
                                    Pollution.DieselCoef.NH3Sched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.NMVOC = state.dataIPShortCut->rNumericArgs(13);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(14),
                                    state.dataIPShortCut->cAlphaArgs(14),
                                    Pollution.DieselCoef.NMVOCSched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.Hg = state.dataIPShortCut->rNumericArgs(14);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(15),
                                    state.dataIPShortCut->cAlphaArgs(15),
                                    Pollution.DieselCoef.HgSched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.Pb = state.dataIPShortCut->rNumericArgs(15);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(16),
                                    state.dataIPShortCut->cAlphaArgs(16),
                                    Pollution.DieselCoef.PbSched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.Water = state.dataIPShortCut->rNumericArgs(16);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(17),
                                    state.dataIPShortCut->cAlphaArgs(17),
                                    Pollution.DieselCoef.WaterSched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.NucHi = state.dataIPShortCut->rNumericArgs(17);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(18),
                                    state.dataIPShortCut->cAlphaArgs(18),
                                    Pollution.DieselCoef.NucHiSched,
                                    ErrorsFound);
                }
                Pollution.DieselCoef.NucLo = state.dataIPShortCut->rNumericArgs(18);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "Diesel",
                                    state.dataIPShortCut->cAlphaFieldNames(19),
                                    state.dataIPShortCut->cAlphaArgs(19),
                                    Pollution.DieselCoef.NucLoSched,
                                    ErrorsFound);
                }

            } else if (SELECT_CASE_var == "OTHERFUEL1") {
                if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": " + FuelType.FuelTypeNames(Loop) + " already entered. Previous entry will be used.");
                    continue;
                }
                Pollution.OtherFuel1Coef.FuelFactorUsed = true;
                // OtherFuel1 Coeffs
                Pollution.OtherFuel1Coef.Source = state.dataIPShortCut->rNumericArgs(2);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(3),
                                    state.dataIPShortCut->cAlphaArgs(3),
                                    Pollution.OtherFuel1Coef.SourceSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.CO2 = state.dataIPShortCut->rNumericArgs(3);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(4),
                                    state.dataIPShortCut->cAlphaArgs(4),
                                    Pollution.OtherFuel1Coef.CO2Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.CO = state.dataIPShortCut->rNumericArgs(4);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(5),
                                    state.dataIPShortCut->cAlphaArgs(5),
                                    Pollution.OtherFuel1Coef.COSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.CH4 = state.dataIPShortCut->rNumericArgs(5);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(6),
                                    state.dataIPShortCut->cAlphaArgs(6),
                                    Pollution.OtherFuel1Coef.CH4Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.NOx = state.dataIPShortCut->rNumericArgs(6);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(7),
                                    state.dataIPShortCut->cAlphaArgs(7),
                                    Pollution.OtherFuel1Coef.NOxSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.N2O = state.dataIPShortCut->rNumericArgs(7);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(8),
                                    state.dataIPShortCut->cAlphaArgs(8),
                                    Pollution.OtherFuel1Coef.N2OSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.SO2 = state.dataIPShortCut->rNumericArgs(8);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(9),
                                    state.dataIPShortCut->cAlphaArgs(9),
                                    Pollution.OtherFuel1Coef.SO2Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.PM = state.dataIPShortCut->rNumericArgs(9);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(10),
                                    state.dataIPShortCut->cAlphaArgs(10),
                                    Pollution.OtherFuel1Coef.PMSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.PM10 = state.dataIPShortCut->rNumericArgs(10);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(11),
                                    state.dataIPShortCut->cAlphaArgs(11),
                                    Pollution.OtherFuel1Coef.PM10Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.PM25 = state.dataIPShortCut->rNumericArgs(11);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(12),
                                    state.dataIPShortCut->cAlphaArgs(12),
                                    Pollution.OtherFuel1Coef.PM25Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.NH3 = state.dataIPShortCut->rNumericArgs(12);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(13),
                                    state.dataIPShortCut->cAlphaArgs(13),
                                    Pollution.OtherFuel1Coef.NH3Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.NMVOC = state.dataIPShortCut->rNumericArgs(13);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(14),
                                    state.dataIPShortCut->cAlphaArgs(14),
                                    Pollution.OtherFuel1Coef.NMVOCSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.Hg = state.dataIPShortCut->rNumericArgs(14);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(15),
                                    state.dataIPShortCut->cAlphaArgs(15),
                                    Pollution.OtherFuel1Coef.HgSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.Pb = state.dataIPShortCut->rNumericArgs(15);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(16),
                                    state.dataIPShortCut->cAlphaArgs(16),
                                    Pollution.OtherFuel1Coef.PbSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.Water = state.dataIPShortCut->rNumericArgs(16);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(17),
                                    state.dataIPShortCut->cAlphaArgs(17),
                                    Pollution.OtherFuel1Coef.WaterSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.NucHi = state.dataIPShortCut->rNumericArgs(17);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(18),
                                    state.dataIPShortCut->cAlphaArgs(18),
                                    Pollution.OtherFuel1Coef.NucHiSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel1Coef.NucLo = state.dataIPShortCut->rNumericArgs(18);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel1",
                                    state.dataIPShortCut->cAlphaFieldNames(19),
                                    state.dataIPShortCut->cAlphaArgs(19),
                                    Pollution.OtherFuel1Coef.NucLoSched,
                                    ErrorsFound);
                }

            } else if (SELECT_CASE_var == "OTHERFUEL2") {
                if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": " + FuelType.FuelTypeNames(Loop) + " already entered. Previous entry will be used.");
                    continue;
                }
                Pollution.OtherFuel2Coef.FuelFactorUsed = true;
                // OtherFuel2 Coeffs
                Pollution.OtherFuel2Coef.Source = state.dataIPShortCut->rNumericArgs(2);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(3),
                                    state.dataIPShortCut->cAlphaArgs(3),
                                    Pollution.OtherFuel2Coef.SourceSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.CO2 = state.dataIPShortCut->rNumericArgs(3);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(4),
                                    state.dataIPShortCut->cAlphaArgs(4),
                                    Pollution.OtherFuel2Coef.CO2Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.CO = state.dataIPShortCut->rNumericArgs(4);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(5),
                                    state.dataIPShortCut->cAlphaArgs(5),
                                    Pollution.OtherFuel2Coef.COSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.CH4 = state.dataIPShortCut->rNumericArgs(5);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(6),
                                    state.dataIPShortCut->cAlphaArgs(6),
                                    Pollution.OtherFuel2Coef.CH4Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.NOx = state.dataIPShortCut->rNumericArgs(6);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(7),
                                    state.dataIPShortCut->cAlphaArgs(7),
                                    Pollution.OtherFuel2Coef.NOxSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.N2O = state.dataIPShortCut->rNumericArgs(7);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(8),
                                    state.dataIPShortCut->cAlphaArgs(8),
                                    Pollution.OtherFuel2Coef.N2OSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.SO2 = state.dataIPShortCut->rNumericArgs(8);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(9),
                                    state.dataIPShortCut->cAlphaArgs(9),
                                    Pollution.OtherFuel2Coef.SO2Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.PM = state.dataIPShortCut->rNumericArgs(9);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(10),
                                    state.dataIPShortCut->cAlphaArgs(10),
                                    Pollution.OtherFuel2Coef.PMSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.PM10 = state.dataIPShortCut->rNumericArgs(10);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(11),
                                    state.dataIPShortCut->cAlphaArgs(11),
                                    Pollution.OtherFuel2Coef.PM10Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.PM25 = state.dataIPShortCut->rNumericArgs(11);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(12),
                                    state.dataIPShortCut->cAlphaArgs(12),
                                    Pollution.OtherFuel2Coef.PM25Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.NH3 = state.dataIPShortCut->rNumericArgs(12);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(13),
                                    state.dataIPShortCut->cAlphaArgs(13),
                                    Pollution.OtherFuel2Coef.NH3Sched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.NMVOC = state.dataIPShortCut->rNumericArgs(13);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(14)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(14),
                                    state.dataIPShortCut->cAlphaArgs(14),
                                    Pollution.OtherFuel2Coef.NMVOCSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.Hg = state.dataIPShortCut->rNumericArgs(14);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(15)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(15),
                                    state.dataIPShortCut->cAlphaArgs(15),
                                    Pollution.OtherFuel2Coef.HgSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.Pb = state.dataIPShortCut->rNumericArgs(15);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(16),
                                    state.dataIPShortCut->cAlphaArgs(16),
                                    Pollution.OtherFuel2Coef.PbSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.Water = state.dataIPShortCut->rNumericArgs(16);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(17),
                                    state.dataIPShortCut->cAlphaArgs(17),
                                    Pollution.OtherFuel2Coef.WaterSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.NucHi = state.dataIPShortCut->rNumericArgs(17);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(18)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(18),
                                    state.dataIPShortCut->cAlphaArgs(18),
                                    Pollution.OtherFuel2Coef.NucHiSched,
                                    ErrorsFound);
                }
                Pollution.OtherFuel2Coef.NucLo = state.dataIPShortCut->rNumericArgs(18);
                if (!state.dataIPShortCut->lAlphaFieldBlanks(19)) {
                    CheckFFSchedule(state,
                                    cCurrentModuleObject,
                                    "OtherFuel2",
                                    state.dataIPShortCut->cAlphaFieldNames(19),
                                    state.dataIPShortCut->cAlphaArgs(19),
                                    Pollution.OtherFuel2Coef.NucLoSched,
                                    ErrorsFound);
                }

            } else {
                ShowSevereError(state, "Illegal FuelType for Pollution Calc Entered=" + FuelType.FuelTypeNames(Loop));
                ErrorsFound = true;
            }
        }

    } // End of the NumEnergyTypes Do Loop

    FuelType.ElecFacilityIndex = GetMeterIndex(state, "Electricity:Facility");
    FuelType.DieselFacilityIndex = GetMeterIndex(state, "Diesel:Facility");
    FuelType.PurchCoolFacilityIndex = GetMeterIndex(state, "DistrictCooling:Facility");
    FuelType.PurchHeatFacilityIndex = GetMeterIndex(state, "DistrictHeating:Facility");
    FuelType.NatGasFacilityIndex = GetMeterIndex(state, "NaturalGas:Facility");
    FuelType.GasolineFacilityIndex = GetMeterIndex(state, "Gasoline:Facility");
    FuelType.CoalFacilityIndex = GetMeterIndex(state, "Coal:Facility");
    FuelType.FuelOil1FacilityIndex = GetMeterIndex(state, "FuelOilNo1:Facility");
    FuelType.FuelOil2FacilityIndex = GetMeterIndex(state, "FuelOilNo2:Facility");
    FuelType.PropaneFacilityIndex = GetMeterIndex(state, "Propane:Facility");
    FuelType.OtherFuel1FacilityIndex = GetMeterIndex(state, "OtherFuel1:Facility");
    FuelType.OtherFuel2FacilityIndex = GetMeterIndex(state, "OtherFuel2:Facility");
    FuelType.ElecProducedFacilityIndex = GetMeterIndex(state, "ElectricityProduced:Facility");
    FuelType.SteamFacilityIndex = GetMeterIndex(state, "Steam:Facility");
    FuelType.ElecPurchasedFacilityIndex = GetMeterIndex(state, "ElectricityPurchased:Facility");
    FuelType.ElecSurplusSoldFacilityIndex = GetMeterIndex(state, "ElectricitySurplusSold:Facility");

    if (state.dataPollutionModule->PollutionReportSetup) { // only do this if reporting on the pollution
        // Need to go through all of the Fuel Types and make sure a Fuel Factor was found for each type of energy being simulated
        // Check for Electricity
        if (!Pollution.ElecCoef.FuelFactorUsed &&
            ((FuelType.ElecFacilityIndex > 0) || (FuelType.ElecProducedFacilityIndex > 0) || (FuelType.PurchCoolFacilityIndex > 0))) {
            ShowSevereError(state, cCurrentModuleObject + " Not Found or Fuel not specified For Pollution Calculation for ELECTRICITY");
            ErrorsFound = true;
        }
        // Check for Natural Gas
        if (!Pollution.NatGasCoef.FuelFactorUsed &&
            ((FuelType.NatGasFacilityIndex > 0) || (FuelType.PurchHeatFacilityIndex > 0) || (FuelType.SteamFacilityIndex > 0))) {
            ShowSevereError(state, cCurrentModuleObject + " Not Found or Fuel not specified For Pollution Calculation for NATURAL GAS");
            ErrorsFound = true;
        }
        // Check for FuelOilNo2 (Residual Oil)
        if (!Pollution.FuelOil2Coef.FuelFactorUsed && (FuelType.FuelOil2FacilityIndex > 0)) {
            ShowSevereError(state, cCurrentModuleObject + " Not Found or Fuel not specified For Pollution Calculation for FUEL OIL #2");
            ErrorsFound = true;
        }
        // Check for FuelOilNo1 (Distillate Oil)
        if (!Pollution.FuelOil1Coef.FuelFactorUsed && (FuelType.FuelOil1FacilityIndex > 0)) {
            ShowSevereError(state, cCurrentModuleObject + " Not Found or Fuel not specified For Pollution Calculation for FUEL OIL #1");
            ErrorsFound = true;
        }
        // Check for Coal
        if (!Pollution.CoalCoef.FuelFactorUsed && (FuelType.CoalFacilityIndex > 0)) {
            ShowSevereError(state, cCurrentModuleObject + " Not Found or Fuel not specified For Pollution Calculation for COAL");
            ErrorsFound = true;
        }
        // Check for Gasoline
        if (!Pollution.GasolineCoef.FuelFactorUsed && (FuelType.GasolineFacilityIndex > 0)) {
            ShowSevereError(state, cCurrentModuleObject + " Not Found or Fuel not specified For Pollution Calculation for GASOLINE");
            ErrorsFound = true;
        }
        // Check for Propane
        if (!Pollution.PropaneCoef.FuelFactorUsed && (FuelType.PropaneFacilityIndex > 0)) {
            ShowSevereError(state, cCurrentModuleObject + " Not Found or Fuel not specified For Pollution Calculation for PROPANE");
            ErrorsFound = true;
        }
        // Check for Diesel
        if (!Pollution.DieselCoef.FuelFactorUsed && (FuelType.DieselFacilityIndex > 0)) {
            ShowSevereError(state, cCurrentModuleObject + " Not Found or Fuel not specified For Pollution Calculation for DIESEL");
            ErrorsFound = true;
        }
        // Check for OtherFuel1
        if (!Pollution.OtherFuel1Coef.FuelFactorUsed && (FuelType.OtherFuel1FacilityIndex > 0)) {
            ShowSevereError(state, cCurrentModuleObject + " Not Found or Fuel not specified For Pollution Calculation for OTHERFUEL1");
            ErrorsFound = true;
        }
        // Check for OtherFuel2
        if (!Pollution.OtherFuel2Coef.FuelFactorUsed && (FuelType.OtherFuel2FacilityIndex > 0)) {
            ShowSevereError(state, cCurrentModuleObject + " Not Found or Fuel not specified For Pollution Calculation for OTHERFUEL2");
            ErrorsFound = true;
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Errors found in getting Pollution Calculation Reporting Input");
    }
}

void SetupPollutionMeterReporting(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   August 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  December 2003 RJL

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is the input routines and Get routines

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;

    if (state.dataPollutionModule->GetInputFlagPollution) {
        GetPollutionFactorInput(state);
        state.dataPollutionModule->GetInputFlagPollution = false;
    }
    auto &Pollution = state.dataPollutionModule->Pollution;
    auto &FuelType = state.dataPollutionModule->FuelType;
    for (Loop = 1; Loop <= static_cast<int>(PollFactor::NUM); ++Loop) {

        if (FuelType.FuelTypeNames(Loop).empty()) continue;

        {
            auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(FuelType.FuelTypeNames(Loop)));
            if (SELECT_CASE_var == "NATURALGAS") {
                // Pollutants from Natural Gas
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.NatGasComp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas CO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.CO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO2",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas CO Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.COPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas CH4 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.CH4Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CH4",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas NOx Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.NOxPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NOx",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas N2O Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.N2OPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "N2O",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas SO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.SO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "SO2",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas PM Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.PMPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas PM10 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.PM10Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM10",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas PM2.5 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.PM25Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM2.5",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas NH3 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.NH3Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NH3",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas NMVOC Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.NMVOCPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NMVOC",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas Hg Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.HgPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Hg",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas Pb Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.PbPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Pb",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas Water Consumption Volume",
                                    OutputProcessor::Unit::L,
                                    Pollution.NatGasComp.WaterPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "WaterEnvironmentalFactors",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas Nuclear High Level Waste Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.NatGasComp.NucHiPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear High",
                                    "NaturalGasEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact NaturalGas Nuclear Low Level Waste Volume",
                                    OutputProcessor::Unit::m3,
                                    Pollution.NatGasComp.NucLoPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear Low",
                                    "NaturalGasEmissions",
                                    _,
                                    "");

            } else if (SELECT_CASE_var == "FUELOILNO2") {
                // Pollutants from FuelOilNo2
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.FuelOil2Comp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 CO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.CO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO2",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 CO Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.COPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 CH4 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.CH4Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CH4",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 NOx Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.NOxPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NOx",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 N2O Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.N2OPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "N2O",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 SO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.SO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "SO2",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 PM Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.PMPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 PM10 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.PM10Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM10",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 PM2.5 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.PM25Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM2.5",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 NH3 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.NH3Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NH3",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 NMVOC Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.NMVOCPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NMVOC",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 Hg Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.HgPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Hg",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 Pb Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.PbPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Pb",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 Water Consumption Volume",
                                    OutputProcessor::Unit::L,
                                    Pollution.FuelOil2Comp.WaterPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "WaterEnvironmentalFactors",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 Nuclear High Level Waste Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil2Comp.NucHiPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear High",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo2 Nuclear Low Level Waste Volume",
                                    OutputProcessor::Unit::m3,
                                    Pollution.FuelOil2Comp.NucLoPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear Low",
                                    "FuelOilNo2Emissions",
                                    _,
                                    "");

            } else if (SELECT_CASE_var == "FUELOILNO1") {
                // Pollutants from FuelOilNo1
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.FuelOil1Comp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 CO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.CO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO2",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 CO Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.COPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 CH4 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.CH4Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CH4",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 NOx Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.NOxPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NOx",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 N2O Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.N2OPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "N2O",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 SO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.SO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "SO2",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 PM Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.PMPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 PM10 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.PM10Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM10",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 PM2.5 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.PM25Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM2.5",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 NH3 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.NH3Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NH3",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 NMVOC Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.NMVOCPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NMVOC",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 Hg Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.HgPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Hg",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 Pb Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.PbPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Pb",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 Water Consumption Volume",
                                    OutputProcessor::Unit::L,
                                    Pollution.FuelOil1Comp.WaterPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "WaterEnvironmentalFactors",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 Nuclear High Level Waste Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.FuelOil1Comp.NucHiPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear High",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact FuelOilNo1 Nuclear Low Level Waste Volume",
                                    OutputProcessor::Unit::m3,
                                    Pollution.FuelOil1Comp.NucLoPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear Low",
                                    "FuelOilNo1Emissions",
                                    _,
                                    "");

            } else if (SELECT_CASE_var == "COAL") {
                // Pollutants from Coal
                SetupOutputVariable(state,
                                    "Environmental Impact Coal Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.CoalComp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal CO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.CO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO2",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal CO Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.COPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal CH4 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.CH4Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CH4",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal NOx Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.NOxPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NOx",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal N2O Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.N2OPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "N2O",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal SO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.SO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "SO2",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal PM Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.PMPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal PM10 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.PM10Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM10",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal PM2.5 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.PM25Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM2.5",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal NH3 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.NH3Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NH3",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal NMVOC Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.NMVOCPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NMVOC",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal Hg Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.HgPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Hg",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal Pb Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.PbPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Pb",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal Water Consumption Volume",
                                    OutputProcessor::Unit::L,
                                    Pollution.CoalComp.WaterPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "WaterEnvironmentalFactors",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal Nuclear High Level Waste Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.CoalComp.NucHiPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear High",
                                    "CoalEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Coal Nuclear Low Level Waste Volume",
                                    OutputProcessor::Unit::m3,
                                    Pollution.CoalComp.NucLoPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear Low",
                                    "CoalEmissions",
                                    _,
                                    "");

            } else if (SELECT_CASE_var == "ELECTRICITY") {
                // Pollutants from Electricity
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.ElecComp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity CO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.CO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO2",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity CO Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.COPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity CH4 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.CH4Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CH4",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity NOx Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.NOxPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NOx",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity N2O Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.N2OPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "N2O",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity SO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.SO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "SO2",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity PM Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.PMPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity PM10 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.PM10Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM10",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity PM2.5 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.PM25Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM2.5",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity NH3 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.NH3Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NH3",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity NMVOC Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.NMVOCPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NMVOC",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity Hg Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.HgPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Hg",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity Pb Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.PbPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Pb",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity Water Consumption Volume",
                                    OutputProcessor::Unit::L,
                                    Pollution.ElecComp.WaterPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "WaterEnvironmentalFactors",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity Nuclear High Level Waste Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.ElecComp.NucHiPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear High",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Electricity Nuclear Low Level Waste Volume",
                                    OutputProcessor::Unit::m3,
                                    Pollution.ElecComp.NucLoPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear Low",
                                    "ElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Purchased Electricity Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.ElecPurchComp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "PurchasedElectricEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Surplus Sold Electricity Source",
                                    OutputProcessor::Unit::J,
                                    Pollution.ElecSurplusSoldComp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "SoldElectricEmissions",
                                    _,
                                    "");
            } else if (SELECT_CASE_var == "GASOLINE") {
                // Pollutants from Gasoline
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.GasolineComp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline CO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.CO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO2",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline CO Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.COPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline CH4 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.CH4Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CH4",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline NOx Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.NOxPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NOx",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline N2O Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.N2OPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "N2O",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline SO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.SO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "SO2",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline PM Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.PMPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline PM10 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.PM10Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM10",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline PM2.5 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.PM25Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM2.5",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline NH3 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.NH3Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NH3",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline NMVOC Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.NMVOCPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NMVOC",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline Hg Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.HgPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Hg",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline Pb Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.PbPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Pb",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline Water Consumption Volume",
                                    OutputProcessor::Unit::L,
                                    Pollution.GasolineComp.WaterPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "WaterEnvironmentalFactors",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline Nuclear High Level Waste Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.GasolineComp.NucHiPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear High",
                                    "GasolineEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Gasoline Nuclear Low Level Waste Volume",
                                    OutputProcessor::Unit::m3,
                                    Pollution.GasolineComp.NucLoPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear Low",
                                    "GasolineEmissions",
                                    _,
                                    "");

            } else if (SELECT_CASE_var == "PROPANE") {
                // Pollutants from Propane
                SetupOutputVariable(state,
                                    "Environmental Impact Propane Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.PropaneComp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane CO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.CO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO2",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane CO Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.COPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane CH4 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.CH4Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CH4",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane NOx Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.NOxPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NOx",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane N2O Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.N2OPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "N2O",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane SO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.SO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "SO2",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane PM Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.PMPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane PM10 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.PM10Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM10",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane PM2.5 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.PM25Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM2.5",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane NH3 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.NH3Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NH3",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane NMVOC Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.NMVOCPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NMVOC",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane Hg Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.HgPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Hg",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane Pb Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.PbPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Pb",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane Water Consumption Volume",
                                    OutputProcessor::Unit::L,
                                    Pollution.PropaneComp.WaterPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "WaterEnvironmentalFactors",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane Nuclear High Level Waste Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.PropaneComp.NucHiPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear High",
                                    "PropaneEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Propane Nuclear Low Level Waste Volume",
                                    OutputProcessor::Unit::m3,
                                    Pollution.PropaneComp.NucLoPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear Low",
                                    "PropaneEmissions",
                                    _,
                                    "");

            } else if (SELECT_CASE_var == "DIESEL") {
                // Pollutants from Diesel
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.DieselComp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel CO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.CO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO2",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel CO Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.COPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel CH4 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.CH4Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CH4",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel NOx Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.NOxPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NOx",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel N2O Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.N2OPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "N2O",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel SO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.SO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "SO2",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel PM Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.PMPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel PM10 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.PM10Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM10",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel PM2.5 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.PM25Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM2.5",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel NH3 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.NH3Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NH3",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel NMVOC Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.NMVOCPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NMVOC",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel Hg Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.HgPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Hg",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel Pb Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.PbPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Pb",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel Water Consumption Volume",
                                    OutputProcessor::Unit::L,
                                    Pollution.DieselComp.WaterPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "WaterEnvironmentalFactors",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel Nuclear High Level Waste Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.DieselComp.NucHiPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear High",
                                    "DieselEmissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact Diesel Nuclear Low Level Waste Volume",
                                    OutputProcessor::Unit::m3,
                                    Pollution.DieselComp.NucLoPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear Low",
                                    "DieselEmissions",
                                    _,
                                    "");

            } else if (SELECT_CASE_var == "OTHERFUEL1") {
                // Pollutants from OtherFuel1
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.OtherFuel1Comp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 CO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.CO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO2",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 CO Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.COPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 CH4 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.CH4Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CH4",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 NOx Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.NOxPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NOx",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 N2O Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.N2OPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "N2O",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 SO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.SO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "SO2",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 PM Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.PMPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 PM10 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.PM10Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM10",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 PM2.5 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.PM25Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM2.5",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 NH3 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.NH3Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NH3",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 NMVOC Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.NMVOCPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NMVOC",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 Hg Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.HgPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Hg",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 Pb Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.PbPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Pb",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 CO2 Water Consumption Volume",
                                    OutputProcessor::Unit::L,
                                    Pollution.OtherFuel1Comp.WaterPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "WaterEnvironmentalFactors",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 Nuclear High Level Waste Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel1Comp.NucHiPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear High",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel1 Nuclear Low Level Waste Volume",
                                    OutputProcessor::Unit::m3,
                                    Pollution.OtherFuel1Comp.NucLoPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear Low",
                                    "OtherFuel1Emissions",
                                    _,
                                    "");

            } else if (SELECT_CASE_var == "OTHERFUEL2") {
                // Pollutants from OtherFuel2
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 Source Energy",
                                    OutputProcessor::Unit::J,
                                    Pollution.OtherFuel2Comp.Source,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Source",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 CO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.CO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO2",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 CO Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.COPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CO",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 CH4 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.CH4Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "CH4",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 NOx Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.NOxPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NOx",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 N2O Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.N2OPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "N2O",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 SO2 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.SO2Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "SO2",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 PM Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.PMPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 PM10 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.PM10Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM10",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 PM2.5 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.PM25Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "PM2.5",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 NH3 Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.NH3Pollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NH3",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 NMVOC Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.NMVOCPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "NMVOC",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 Hg Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.HgPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Hg",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 Pb Emissions Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.PbPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Pb",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 CO2 Water Consumption Volume",
                                    OutputProcessor::Unit::L,
                                    Pollution.OtherFuel2Comp.WaterPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "WaterEnvironmentalFactors",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 Nuclear High Level Waste Mass",
                                    OutputProcessor::Unit::kg,
                                    Pollution.OtherFuel2Comp.NucHiPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear High",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
                SetupOutputVariable(state,
                                    "Environmental Impact OtherFuel2 Nuclear Low Level Waste Volume",
                                    OutputProcessor::Unit::m3,
                                    Pollution.OtherFuel2Comp.NucLoPollution,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    "Site",
                                    _,
                                    "Nuclear Low",
                                    "OtherFuel2Emissions",
                                    _,
                                    "");
            }
        }

    } // End of the NumEnergyTypes Do Loop

    // Always setup the Total Carbon Equivalent
    SetupOutputVariable(state,
                        "Environmental Impact Total N2O Emissions Carbon Equivalent Mass",
                        OutputProcessor::Unit::kg,
                        Pollution.TotCarbonEquivFromN2O,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        "Site",
                        _,
                        "Carbon Equivalent",
                        "CarbonEquivalentEmissions",
                        _,
                        "");
    SetupOutputVariable(state,
                        "Environmental Impact Total CH4 Emissions Carbon Equivalent Mass",
                        OutputProcessor::Unit::kg,
                        Pollution.TotCarbonEquivFromCH4,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        "Site",
                        _,
                        "Carbon Equivalent",
                        "CarbonEquivalentEmissions",
                        _,
                        "");
    SetupOutputVariable(state,
                        "Environmental Impact Total CO2 Emissions Carbon Equivalent Mass",
                        OutputProcessor::Unit::kg,
                        Pollution.TotCarbonEquivFromCO2,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        "Site",
                        _,
                        "Carbon Equivalent",
                        "CarbonEquivalentEmissions",
                        _,
                        "");
}

void CheckPollutionMeterReporting(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // <description>

    // in progress
    if (state.dataPollutionModule->NumFuelFactors == 0 || state.dataPollutionModule->NumEnvImpactFactors == 0) {
        if (ReportingThisVariable(state, "Environmental Impact Total N2O Emissions Carbon Equivalent Mass") ||
            ReportingThisVariable(state, "Environmental Impact Total CH4 Emissions Carbon Equivalent Mass") ||
            ReportingThisVariable(state, "Environmental Impact Total CO2 Emissions Carbon Equivalent Mass") ||
            ReportingThisVariable(state, "Carbon Equivalent:Facility") ||
            ReportingThisVariable(state, "CarbonEquivalentEmissions:Carbon Equivalent")) {
            ShowWarningError(
                state, "GetPollutionFactorInput: Requested reporting for Carbon Equivalent Pollution, but insufficient information is entered.");
            ShowContinueError(
                state, R"(Both "FuelFactors" and "EnvironmentalImpactFactors" must be entered or the displayed carbon pollution will all be zero.)");
        }
    }
}

void CheckFFSchedule(EnergyPlusData &state,
                     std::string const &currentModuleObject, // the module Object
                     std::string const &resourceType,        // resource type (Natural Gas, etc)
                     std::string const &fieldName,           // Actual field name
                     std::string const &ScheduleName,        // Schedule Name as input
                     int &SchedulePtr,                       // Schedule Index
                     bool &ErrorsFound                       // true if errors found
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   September 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This support routine performs the "obtain schedule pointer" and checks Fuel Factor
    // schedules for validity (values must be >= 0).

    // Using/Aliasing
    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;

    SchedulePtr = GetScheduleIndex(state, ScheduleName);
    if (SchedulePtr == 0) {
        ShowSevereError(state, currentModuleObject + ": " + resourceType + ", invalid " + fieldName + "=\"" + ScheduleName + "\" not found.");
        ErrorsFound = true;
    } else if (!CheckScheduleValueMinMax(state, SchedulePtr, ">=", 0.0)) {
        ShowSevereError(state, currentModuleObject + ": " + resourceType + ", invalid " + fieldName + "=\"" + ScheduleName + "\" invalid values.");
        ShowContinueError(state, "Schedule values must be (>=0.).");
        ErrorsFound = true;
    }
}

// End of Get Input subroutines for the Pollution Module
//******************************************************************************

void CalcPollution(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   1998
    //       MODIFIED       na
    //       RE-ENGINEERED  December 2003 RJL

    // PURPOSE OF THIS SUBROUTINE:
    // CalcPollution - Does the Pollutant Calculation

    // METHODOLOGY EMPLOYED:
    // NA

    // REFERENCES:
    // na

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

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
    Real64 ElecValue;
    Real64 NatGasValue;
    Real64 FuelOil1Value;
    Real64 FuelOil2Value;
    Real64 CoalValue;
    Real64 GasolineValue;
    Real64 PropaneValue;
    Real64 DieselValue;
    Real64 OtherFuel1Value;
    Real64 OtherFuel2Value;

    //       Then the amount of Pollution produced by each fuel type is
    //       calculated in kgs.
    //       Input units for the coefficients is not standard and needs to be converted here.
    //       Most of the units are g/MJ, however water is in L/MJ and low level nuclear water is m3/MJ
    //       so only the energy has to be converted from J to MJ.

    //     For each pollution/fuel type, Schedule values are allowed.  Thus, calculations are bundled.

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;
    auto &Pollution = state.dataPollutionModule->Pollution;
    auto &FuelType = state.dataPollutionModule->FuelType;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.CO2Pollution = 0.0;
        if (Pollution.ElecCoef.CO2Sched == 0) {
            ElecValue = Pollution.ElecCoef.CO2 * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.CO2 * GetCurrentScheduleValue(state, Pollution.ElecCoef.CO2Sched) * 0.001;
        }
        Pollution.ElecComp.CO2Pollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.CO2Pollution = 0.0;
        if (Pollution.NatGasCoef.CO2Sched == 0) {
            NatGasValue = Pollution.NatGasCoef.CO2 * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.CO2 * GetCurrentScheduleValue(state, Pollution.NatGasCoef.CO2Sched) * 0.001;
        }
        Pollution.NatGasComp.CO2Pollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.CO2Pollution = 0.0;
        if (Pollution.FuelOil1Coef.CO2Sched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.CO2 * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.CO2 * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.CO2Sched) * 0.001;
        }
        Pollution.FuelOil1Comp.CO2Pollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.CO2Pollution = 0.0;
        if (Pollution.FuelOil2Coef.CO2Sched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.CO2 * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.CO2 * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.CO2Sched) * 0.001;
        }
        Pollution.FuelOil2Comp.CO2Pollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.CO2Pollution = 0.0;
        if (Pollution.CoalCoef.CO2Sched == 0) {
            CoalValue = Pollution.CoalCoef.CO2 * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.CO2 * GetCurrentScheduleValue(state, Pollution.CoalCoef.CO2Sched) * 0.001;
        }
        Pollution.CoalComp.CO2Pollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.CO2Pollution = 0.0;
        if (Pollution.GasolineCoef.CO2Sched == 0) {
            GasolineValue = Pollution.GasolineCoef.CO2 * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.CO2 * GetCurrentScheduleValue(state, Pollution.GasolineCoef.CO2Sched) * 0.001;
        }
        Pollution.GasolineComp.CO2Pollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.CO2Pollution = 0.0;
        if (Pollution.PropaneCoef.CO2Sched == 0) {
            PropaneValue = Pollution.PropaneCoef.CO2 * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.CO2 * GetCurrentScheduleValue(state, Pollution.PropaneCoef.CO2Sched) * 0.001;
        }
        Pollution.PropaneComp.CO2Pollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.CO2Pollution = 0.0;
        if (Pollution.DieselCoef.CO2Sched == 0) {
            DieselValue = Pollution.DieselCoef.CO2 * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.CO2 * GetCurrentScheduleValue(state, Pollution.DieselCoef.CO2Sched) * 0.001;
        }
        Pollution.DieselComp.CO2Pollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }

    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.CO2Pollution = 0.0;
        if (Pollution.OtherFuel1Coef.CO2Sched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.CO2 * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.CO2 * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.CO2Sched) * 0.001;
        }
        Pollution.OtherFuel1Comp.CO2Pollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }

    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.CO2Pollution = 0.0;
        if (Pollution.OtherFuel2Coef.CO2Sched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.CO2 * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.CO2 * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.CO2Sched) * 0.001;
        }
        Pollution.OtherFuel2Comp.CO2Pollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    Pollution.CO2PollutTotal = Pollution.ElecComp.CO2Pollution + Pollution.NatGasComp.CO2Pollution + Pollution.FuelOil1Comp.CO2Pollution +
                               Pollution.FuelOil2Comp.CO2Pollution + Pollution.CoalComp.CO2Pollution + Pollution.GasolineComp.CO2Pollution +
                               Pollution.PropaneComp.CO2Pollution + Pollution.DieselComp.CO2Pollution + Pollution.OtherFuel1Comp.CO2Pollution +
                               Pollution.OtherFuel2Comp.CO2Pollution;

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.NOxPollution = 0.0;
        if (Pollution.ElecCoef.NOxSched == 0) {
            ElecValue = Pollution.ElecCoef.NOx * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.NOx * GetCurrentScheduleValue(state, Pollution.ElecCoef.NOxSched) * 0.001;
        }
        Pollution.ElecComp.NOxPollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.NOxPollution = 0.0;
        if (Pollution.NatGasCoef.NOxSched == 0) {
            NatGasValue = Pollution.NatGasCoef.NOx * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.NOx * GetCurrentScheduleValue(state, Pollution.NatGasCoef.NOxSched) * 0.001;
        }
        Pollution.NatGasComp.NOxPollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.NOxPollution = 0.0;
        if (Pollution.FuelOil1Coef.NOxSched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.NOx * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.NOx * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.NOxSched) * 0.001;
        }
        Pollution.FuelOil1Comp.NOxPollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.NOxPollution = 0.0;
        if (Pollution.FuelOil2Coef.NOxSched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.NOx * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.NOx * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.NOxSched) * 0.001;
        }
        Pollution.FuelOil2Comp.NOxPollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.NOxPollution = 0.0;
        if (Pollution.CoalCoef.NOxSched == 0) {
            CoalValue = Pollution.CoalCoef.NOx * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.NOx * GetCurrentScheduleValue(state, Pollution.CoalCoef.NOxSched) * 0.001;
        }
        Pollution.CoalComp.NOxPollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.NOxPollution = 0.0;
        if (Pollution.GasolineCoef.NOxSched == 0) {
            GasolineValue = Pollution.GasolineCoef.NOx * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.NOx * GetCurrentScheduleValue(state, Pollution.GasolineCoef.NOxSched) * 0.001;
        }
        Pollution.GasolineComp.NOxPollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.NOxPollution = 0.0;
        if (Pollution.PropaneCoef.NOxSched == 0) {
            PropaneValue = Pollution.PropaneCoef.NOx * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.NOx * GetCurrentScheduleValue(state, Pollution.PropaneCoef.NOxSched) * 0.001;
        }
        Pollution.PropaneComp.NOxPollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.NOxPollution = 0.0;
        if (Pollution.DieselCoef.NOxSched == 0) {
            DieselValue = Pollution.DieselCoef.NOx * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.NOx * GetCurrentScheduleValue(state, Pollution.DieselCoef.NOxSched) * 0.001;
        }
        Pollution.DieselComp.NOxPollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.NOxPollution = 0.0;
        if (Pollution.OtherFuel1Coef.NOxSched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.NOx * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.NOx * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.NOxSched) * 0.001;
        }
        Pollution.OtherFuel1Comp.NOxPollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.NOxPollution = 0.0;
        if (Pollution.OtherFuel2Coef.NOxSched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.NOx * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.NOx * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.NOxSched) * 0.001;
        }
        Pollution.OtherFuel2Comp.NOxPollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.CH4Pollution = 0.0;
        if (Pollution.ElecCoef.CH4Sched == 0) {
            ElecValue = Pollution.ElecCoef.CH4 * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.CH4 * GetCurrentScheduleValue(state, Pollution.ElecCoef.CH4Sched) * 0.001;
        }
        Pollution.ElecComp.CH4Pollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.CH4Pollution = 0.0;
        if (Pollution.NatGasCoef.CH4Sched == 0) {
            NatGasValue = Pollution.NatGasCoef.CH4 * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.CH4 * GetCurrentScheduleValue(state, Pollution.NatGasCoef.CH4Sched) * 0.001;
        }
        Pollution.NatGasComp.CH4Pollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.CH4Pollution = 0.0;
        if (Pollution.FuelOil1Coef.CH4Sched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.CH4 * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.CH4 * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.CH4Sched) * 0.001;
        }
        Pollution.FuelOil1Comp.CH4Pollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.CH4Pollution = 0.0;
        if (Pollution.FuelOil2Coef.CH4Sched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.CH4 * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.CH4 * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.CH4Sched) * 0.001;
        }
        Pollution.FuelOil2Comp.CH4Pollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.CH4Pollution = 0.0;
        if (Pollution.CoalCoef.CH4Sched == 0) {
            CoalValue = Pollution.CoalCoef.CH4 * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.CH4 * GetCurrentScheduleValue(state, Pollution.CoalCoef.CH4Sched) * 0.001;
        }
        Pollution.CoalComp.CH4Pollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.CH4Pollution = 0.0;
        if (Pollution.GasolineCoef.CH4Sched == 0) {
            GasolineValue = Pollution.GasolineCoef.CH4 * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.CH4 * GetCurrentScheduleValue(state, Pollution.GasolineCoef.CH4Sched) * 0.001;
        }
        Pollution.GasolineComp.CH4Pollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.CH4Pollution = 0.0;
        if (Pollution.PropaneCoef.CH4Sched == 0) {
            PropaneValue = Pollution.PropaneCoef.CH4 * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.CH4 * GetCurrentScheduleValue(state, Pollution.PropaneCoef.CH4Sched) * 0.001;
        }
        Pollution.PropaneComp.CH4Pollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.CH4Pollution = 0.0;
        if (Pollution.DieselCoef.CH4Sched == 0) {
            DieselValue = Pollution.DieselCoef.CH4 * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.CH4 * GetCurrentScheduleValue(state, Pollution.DieselCoef.CH4Sched) * 0.001;
        }
        Pollution.DieselComp.CH4Pollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.CH4Pollution = 0.0;
        if (Pollution.OtherFuel1Coef.CH4Sched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.CH4 * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.CH4 * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.CH4Sched) * 0.001;
        }
        Pollution.OtherFuel1Comp.CH4Pollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.CH4Pollution = 0.0;
        if (Pollution.OtherFuel2Coef.CH4Sched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.CH4 * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.CH4 * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.CH4Sched) * 0.001;
        }
        Pollution.OtherFuel2Comp.CH4Pollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    Pollution.CH4PollutTotal = Pollution.ElecComp.CH4Pollution + Pollution.NatGasComp.CH4Pollution + Pollution.FuelOil1Comp.CH4Pollution +
                               Pollution.FuelOil2Comp.CH4Pollution + Pollution.CoalComp.CH4Pollution + Pollution.GasolineComp.CH4Pollution +
                               Pollution.PropaneComp.CH4Pollution + Pollution.DieselComp.CH4Pollution + Pollution.OtherFuel1Comp.CH4Pollution +
                               Pollution.OtherFuel1Comp.CH4Pollution;

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.COPollution = 0.0;
        if (Pollution.ElecCoef.COSched == 0) {
            ElecValue = Pollution.ElecCoef.CO * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.CO * GetCurrentScheduleValue(state, Pollution.ElecCoef.COSched) * 0.001;
        }
        Pollution.ElecComp.COPollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.COPollution = 0.0;
        if (Pollution.NatGasCoef.COSched == 0) {
            NatGasValue = Pollution.NatGasCoef.CO * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.CO * GetCurrentScheduleValue(state, Pollution.NatGasCoef.COSched) * 0.001;
        }
        Pollution.NatGasComp.COPollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.COPollution = 0.0;
        if (Pollution.FuelOil1Coef.COSched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.CO * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.CO * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.COSched) * 0.001;
        }
        Pollution.FuelOil1Comp.COPollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.COPollution = 0.0;
        if (Pollution.FuelOil2Coef.COSched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.CO * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.CO * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.COSched) * 0.001;
        }
        Pollution.FuelOil2Comp.COPollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.COPollution = 0.0;
        if (Pollution.CoalCoef.COSched == 0) {
            CoalValue = Pollution.CoalCoef.CO * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.CO * GetCurrentScheduleValue(state, Pollution.CoalCoef.COSched) * 0.001;
        }
        Pollution.CoalComp.COPollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.COPollution = 0.0;
        if (Pollution.GasolineCoef.COSched == 0) {
            GasolineValue = Pollution.GasolineCoef.CO * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.CO * GetCurrentScheduleValue(state, Pollution.GasolineCoef.COSched) * 0.001;
        }
        Pollution.GasolineComp.COPollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.COPollution = 0.0;
        if (Pollution.PropaneCoef.COSched == 0) {
            PropaneValue = Pollution.PropaneCoef.CO * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.CO * GetCurrentScheduleValue(state, Pollution.PropaneCoef.COSched) * 0.001;
        }
        Pollution.PropaneComp.COPollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.COPollution = 0.0;
        if (Pollution.DieselCoef.COSched == 0) {
            DieselValue = Pollution.DieselCoef.CO * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.CO * GetCurrentScheduleValue(state, Pollution.DieselCoef.COSched) * 0.001;
        }
        Pollution.DieselComp.COPollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.COPollution = 0.0;
        if (Pollution.OtherFuel1Coef.COSched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.CO * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.CO * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.COSched) * 0.001;
        }
        Pollution.OtherFuel1Comp.COPollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.COPollution = 0.0;
        if (Pollution.OtherFuel2Coef.COSched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.CO * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.CO * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.COSched) * 0.001;
        }
        Pollution.OtherFuel2Comp.COPollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.N2OPollution = 0.0;
        if (Pollution.ElecCoef.N2OSched == 0) {
            ElecValue = Pollution.ElecCoef.N2O * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.N2O * GetCurrentScheduleValue(state, Pollution.ElecCoef.N2OSched) * 0.001;
        }
        Pollution.ElecComp.N2OPollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.N2OPollution = 0.0;
        if (Pollution.NatGasCoef.N2OSched == 0) {
            NatGasValue = Pollution.NatGasCoef.N2O * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.N2O * GetCurrentScheduleValue(state, Pollution.NatGasCoef.N2OSched) * 0.001;
        }
        Pollution.NatGasComp.N2OPollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.N2OPollution = 0.0;
        if (Pollution.FuelOil1Coef.N2OSched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.N2O * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.N2O * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.N2OSched) * 0.001;
        }
        Pollution.FuelOil1Comp.N2OPollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.N2OPollution = 0.0;
        if (Pollution.FuelOil2Coef.N2OSched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.N2O * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.N2O * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.N2OSched) * 0.001;
        }
        Pollution.FuelOil2Comp.N2OPollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.N2OPollution = 0.0;
        if (Pollution.CoalCoef.N2OSched == 0) {
            CoalValue = Pollution.CoalCoef.N2O * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.N2O * GetCurrentScheduleValue(state, Pollution.CoalCoef.N2OSched) * 0.001;
        }
        Pollution.CoalComp.N2OPollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.N2OPollution = 0.0;
        if (Pollution.GasolineCoef.N2OSched == 0) {
            GasolineValue = Pollution.GasolineCoef.N2O * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.N2O * GetCurrentScheduleValue(state, Pollution.GasolineCoef.N2OSched) * 0.001;
        }
        Pollution.GasolineComp.N2OPollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.N2OPollution = 0.0;
        if (Pollution.PropaneCoef.N2OSched == 0) {
            PropaneValue = Pollution.PropaneCoef.N2O * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.N2O * GetCurrentScheduleValue(state, Pollution.PropaneCoef.N2OSched) * 0.001;
        }
        Pollution.PropaneComp.N2OPollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.N2OPollution = 0.0;
        if (Pollution.DieselCoef.N2OSched == 0) {
            DieselValue = Pollution.DieselCoef.N2O * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.N2O * GetCurrentScheduleValue(state, Pollution.DieselCoef.N2OSched) * 0.001;
        }
        Pollution.DieselComp.N2OPollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.N2OPollution = 0.0;
        if (Pollution.OtherFuel1Coef.N2OSched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.N2O * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.N2O * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.N2OSched) * 0.001;
        }
        Pollution.OtherFuel1Comp.N2OPollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.N2OPollution = 0.0;
        if (Pollution.OtherFuel2Coef.N2OSched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.N2O * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.N2O * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.N2OSched) * 0.001;
        }
        Pollution.OtherFuel2Comp.N2OPollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    Pollution.N2OPollutTotal = Pollution.ElecComp.N2OPollution + Pollution.NatGasComp.N2OPollution + Pollution.FuelOil1Comp.N2OPollution +
                               Pollution.FuelOil2Comp.N2OPollution + Pollution.CoalComp.N2OPollution + Pollution.GasolineComp.N2OPollution +
                               Pollution.PropaneComp.N2OPollution + Pollution.DieselComp.N2OPollution + Pollution.OtherFuel1Comp.N2OPollution +
                               Pollution.OtherFuel2Comp.N2OPollution;

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.SO2Pollution = 0.0;
        if (Pollution.ElecCoef.SO2Sched == 0) {
            ElecValue = Pollution.ElecCoef.SO2 * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.SO2 * GetCurrentScheduleValue(state, Pollution.ElecCoef.SO2Sched) * 0.001;
        }
        Pollution.ElecComp.SO2Pollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.SO2Pollution = 0.0;
        if (Pollution.NatGasCoef.SO2Sched == 0) {
            NatGasValue = Pollution.NatGasCoef.SO2 * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.SO2 * GetCurrentScheduleValue(state, Pollution.NatGasCoef.SO2Sched) * 0.001;
        }
        Pollution.NatGasComp.SO2Pollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.SO2Pollution = 0.0;
        if (Pollution.FuelOil1Coef.SO2Sched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.SO2 * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.SO2 * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.SO2Sched) * 0.001;
        }
        Pollution.FuelOil1Comp.SO2Pollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.SO2Pollution = 0.0;
        if (Pollution.FuelOil2Coef.SO2Sched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.SO2 * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.SO2 * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.SO2Sched) * 0.001;
        }
        Pollution.FuelOil2Comp.SO2Pollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.SO2Pollution = 0.0;
        if (Pollution.CoalCoef.SO2Sched == 0) {
            CoalValue = Pollution.CoalCoef.SO2 * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.SO2 * GetCurrentScheduleValue(state, Pollution.CoalCoef.SO2Sched) * 0.001;
        }
        Pollution.CoalComp.SO2Pollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.SO2Pollution = 0.0;
        if (Pollution.GasolineCoef.SO2Sched == 0) {
            GasolineValue = Pollution.GasolineCoef.SO2 * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.SO2 * GetCurrentScheduleValue(state, Pollution.GasolineCoef.SO2Sched) * 0.001;
        }
        Pollution.GasolineComp.SO2Pollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.SO2Pollution = 0.0;
        if (Pollution.PropaneCoef.SO2Sched == 0) {
            PropaneValue = Pollution.PropaneCoef.SO2 * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.SO2 * GetCurrentScheduleValue(state, Pollution.PropaneCoef.SO2Sched) * 0.001;
        }
        Pollution.PropaneComp.SO2Pollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.SO2Pollution = 0.0;
        if (Pollution.DieselCoef.SO2Sched == 0) {
            DieselValue = Pollution.DieselCoef.SO2 * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.SO2 * GetCurrentScheduleValue(state, Pollution.DieselCoef.SO2Sched) * 0.001;
        }
        Pollution.DieselComp.SO2Pollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.SO2Pollution = 0.0;
        if (Pollution.OtherFuel1Coef.SO2Sched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.SO2 * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.SO2 * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.SO2Sched) * 0.001;
        }
        Pollution.OtherFuel1Comp.SO2Pollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.SO2Pollution = 0.0;
        if (Pollution.OtherFuel2Coef.SO2Sched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.SO2 * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.SO2 * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.SO2Sched) * 0.001;
        }
        Pollution.OtherFuel2Comp.SO2Pollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.PMPollution = 0.0;
        if (Pollution.ElecCoef.PMSched == 0) {
            ElecValue = Pollution.ElecCoef.PM * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.PM * GetCurrentScheduleValue(state, Pollution.ElecCoef.PMSched) * 0.001;
        }
        Pollution.ElecComp.PMPollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.PMPollution = 0.0;
        if (Pollution.NatGasCoef.PMSched == 0) {
            NatGasValue = Pollution.NatGasCoef.PM * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.PM * GetCurrentScheduleValue(state, Pollution.NatGasCoef.PMSched) * 0.001;
        }
        Pollution.NatGasComp.PMPollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.PMPollution = 0.0;
        if (Pollution.FuelOil1Coef.PMSched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.PM * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.PM * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.PMSched) * 0.001;
        }
        Pollution.FuelOil1Comp.PMPollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.PMPollution = 0.0;
        if (Pollution.FuelOil2Coef.PMSched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.PM * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.PM * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.PMSched) * 0.001;
        }
        Pollution.FuelOil2Comp.PMPollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.PMPollution = 0.0;
        if (Pollution.CoalCoef.PMSched == 0) {
            CoalValue = Pollution.CoalCoef.PM * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.PM * GetCurrentScheduleValue(state, Pollution.CoalCoef.PMSched) * 0.001;
        }
        Pollution.CoalComp.PMPollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.PMPollution = 0.0;
        if (Pollution.GasolineCoef.PMSched == 0) {
            GasolineValue = Pollution.GasolineCoef.PM * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.PM * GetCurrentScheduleValue(state, Pollution.GasolineCoef.PMSched) * 0.001;
        }
        Pollution.GasolineComp.PMPollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.PMPollution = 0.0;
        if (Pollution.PropaneCoef.PMSched == 0) {
            PropaneValue = Pollution.PropaneCoef.PM * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.PM * GetCurrentScheduleValue(state, Pollution.PropaneCoef.PMSched) * 0.001;
        }
        Pollution.PropaneComp.PMPollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.PMPollution = 0.0;
        if (Pollution.DieselCoef.PMSched == 0) {
            DieselValue = Pollution.DieselCoef.PM * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.PM * GetCurrentScheduleValue(state, Pollution.DieselCoef.PMSched) * 0.001;
        }
        Pollution.DieselComp.PMPollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.PMPollution = 0.0;
        if (Pollution.OtherFuel1Coef.PMSched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.PM * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.PM * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.PMSched) * 0.001;
        }
        Pollution.OtherFuel1Comp.PMPollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.PMPollution = 0.0;
        if (Pollution.OtherFuel2Coef.PMSched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.PM * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.PM * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.PMSched) * 0.001;
        }
        Pollution.OtherFuel2Comp.PMPollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.PM10Pollution = 0.0;
        if (Pollution.ElecCoef.PM10Sched == 0) {
            ElecValue = Pollution.ElecCoef.PM10 * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.PM10 * GetCurrentScheduleValue(state, Pollution.ElecCoef.PM10Sched) * 0.001;
        }
        Pollution.ElecComp.PM10Pollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.PM10Pollution = 0.0;
        if (Pollution.NatGasCoef.PM10Sched == 0) {
            NatGasValue = Pollution.NatGasCoef.PM10 * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.PM10 * GetCurrentScheduleValue(state, Pollution.NatGasCoef.PM10Sched) * 0.001;
        }
        Pollution.NatGasComp.PM10Pollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.PM10Pollution = 0.0;
        if (Pollution.FuelOil1Coef.PM10Sched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.PM10 * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.PM10 * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.PM10Sched) * 0.001;
        }
        Pollution.FuelOil1Comp.PM10Pollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.PM10Pollution = 0.0;
        if (Pollution.FuelOil2Coef.PM10Sched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.PM10 * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.PM10 * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.PM10Sched) * 0.001;
        }
        Pollution.FuelOil2Comp.PM10Pollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.PM10Pollution = 0.0;
        if (Pollution.CoalCoef.PM10Sched == 0) {
            CoalValue = Pollution.CoalCoef.PM10 * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.PM10 * GetCurrentScheduleValue(state, Pollution.CoalCoef.PM10Sched) * 0.001;
        }
        Pollution.CoalComp.PM10Pollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.PM10Pollution = 0.0;
        if (Pollution.GasolineCoef.PM10Sched == 0) {
            GasolineValue = Pollution.GasolineCoef.PM10 * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.PM10 * GetCurrentScheduleValue(state, Pollution.GasolineCoef.PM10Sched) * 0.001;
        }
        Pollution.GasolineComp.PM10Pollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.PM10Pollution = 0.0;
        if (Pollution.PropaneCoef.PM10Sched == 0) {
            PropaneValue = Pollution.PropaneCoef.PM10 * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.PM10 * GetCurrentScheduleValue(state, Pollution.PropaneCoef.PM10Sched) * 0.001;
        }
        Pollution.PropaneComp.PM10Pollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.PM10Pollution = 0.0;
        if (Pollution.DieselCoef.PM10Sched == 0) {
            DieselValue = Pollution.DieselCoef.PM10 * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.PM10 * GetCurrentScheduleValue(state, Pollution.DieselCoef.PM10Sched) * 0.001;
        }
        Pollution.DieselComp.PM10Pollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.PM10Pollution = 0.0;
        if (Pollution.OtherFuel1Coef.PM10Sched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.PM10 * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.PM10 * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.PM10Sched) * 0.001;
        }
        Pollution.OtherFuel1Comp.PM10Pollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.PM10Pollution = 0.0;
        if (Pollution.OtherFuel2Coef.PM10Sched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.PM10 * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.PM10 * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.PM10Sched) * 0.001;
        }
        Pollution.OtherFuel2Comp.PM10Pollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.PM25Pollution = 0.0;
        if (Pollution.ElecCoef.PM25Sched == 0) {
            ElecValue = Pollution.ElecCoef.PM25 * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.PM25 * GetCurrentScheduleValue(state, Pollution.ElecCoef.PM25Sched) * 0.001;
        }
        Pollution.ElecComp.PM25Pollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.PM25Pollution = 0.0;
        if (Pollution.NatGasCoef.PM25Sched == 0) {
            NatGasValue = Pollution.NatGasCoef.PM25 * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.PM25 * GetCurrentScheduleValue(state, Pollution.NatGasCoef.PM25Sched) * 0.001;
        }
        Pollution.NatGasComp.PM25Pollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.PM25Pollution = 0.0;
        if (Pollution.FuelOil1Coef.PM25Sched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.PM25 * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.PM25 * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.PM25Sched) * 0.001;
        }
        Pollution.FuelOil1Comp.PM25Pollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.PM25Pollution = 0.0;
        if (Pollution.FuelOil2Coef.PM25Sched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.PM25 * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.PM25 * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.PM25Sched) * 0.001;
        }
        Pollution.FuelOil2Comp.PM25Pollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.PM25Pollution = 0.0;
        if (Pollution.CoalCoef.PM25Sched == 0) {
            CoalValue = Pollution.CoalCoef.PM25 * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.PM25 * GetCurrentScheduleValue(state, Pollution.CoalCoef.PM25Sched) * 0.001;
        }
        Pollution.CoalComp.PM25Pollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.PM25Pollution = 0.0;
        if (Pollution.GasolineCoef.PM25Sched == 0) {
            GasolineValue = Pollution.GasolineCoef.PM25 * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.PM25 * GetCurrentScheduleValue(state, Pollution.GasolineCoef.PM25Sched) * 0.001;
        }
        Pollution.GasolineComp.PM25Pollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.PM25Pollution = 0.0;
        if (Pollution.PropaneCoef.PM25Sched == 0) {
            PropaneValue = Pollution.PropaneCoef.PM25 * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.PM25 * GetCurrentScheduleValue(state, Pollution.PropaneCoef.PM25Sched) * 0.001;
        }
        Pollution.PropaneComp.PM25Pollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.PM25Pollution = 0.0;
        if (Pollution.DieselCoef.PM25Sched == 0) {
            DieselValue = Pollution.DieselCoef.PM25 * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.PM25 * GetCurrentScheduleValue(state, Pollution.DieselCoef.PM25Sched) * 0.001;
        }
        Pollution.DieselComp.PM25Pollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.PM25Pollution = 0.0;
        if (Pollution.OtherFuel1Coef.PM25Sched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.PM25 * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.PM25 * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.PM25Sched) * 0.001;
        }
        Pollution.OtherFuel1Comp.PM25Pollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.PM25Pollution = 0.0;
        if (Pollution.OtherFuel2Coef.PM25Sched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.PM25 * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.PM25 * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.PM25Sched) * 0.001;
        }
        Pollution.OtherFuel2Comp.PM25Pollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.NH3Pollution = 0.0;
        if (Pollution.ElecCoef.NH3Sched == 0) {
            ElecValue = Pollution.ElecCoef.NH3 * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.NH3 * GetCurrentScheduleValue(state, Pollution.ElecCoef.NH3Sched) * 0.001;
        }
        Pollution.ElecComp.NH3Pollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.NH3Pollution = 0.0;
        if (Pollution.NatGasCoef.NH3Sched == 0) {
            NatGasValue = Pollution.NatGasCoef.NH3 * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.NH3 * GetCurrentScheduleValue(state, Pollution.NatGasCoef.NH3Sched) * 0.001;
        }
        Pollution.NatGasComp.NH3Pollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.NH3Pollution = 0.0;
        if (Pollution.FuelOil1Coef.NH3Sched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.NH3 * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.NH3 * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.NH3Sched) * 0.001;
        }
        Pollution.FuelOil1Comp.NH3Pollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.NH3Pollution = 0.0;
        if (Pollution.FuelOil2Coef.NH3Sched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.NH3 * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.NH3 * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.NH3Sched) * 0.001;
        }
        Pollution.FuelOil2Comp.NH3Pollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.NH3Pollution = 0.0;
        if (Pollution.CoalCoef.NH3Sched == 0) {
            CoalValue = Pollution.CoalCoef.NH3 * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.NH3 * GetCurrentScheduleValue(state, Pollution.CoalCoef.NH3Sched) * 0.001;
        }
        Pollution.CoalComp.NH3Pollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.NH3Pollution = 0.0;
        if (Pollution.GasolineCoef.NH3Sched == 0) {
            GasolineValue = Pollution.GasolineCoef.NH3 * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.NH3 * GetCurrentScheduleValue(state, Pollution.GasolineCoef.NH3Sched) * 0.001;
        }
        Pollution.GasolineComp.NH3Pollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.NH3Pollution = 0.0;
        if (Pollution.PropaneCoef.NH3Sched == 0) {
            PropaneValue = Pollution.PropaneCoef.NH3 * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.NH3 * GetCurrentScheduleValue(state, Pollution.PropaneCoef.NH3Sched) * 0.001;
        }
        Pollution.PropaneComp.NH3Pollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.NH3Pollution = 0.0;
        if (Pollution.DieselCoef.NH3Sched == 0) {
            DieselValue = Pollution.DieselCoef.NH3 * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.NH3 * GetCurrentScheduleValue(state, Pollution.DieselCoef.NH3Sched) * 0.001;
        }
        Pollution.DieselComp.NH3Pollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.NH3Pollution = 0.0;
        if (Pollution.OtherFuel1Coef.NH3Sched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.NH3 * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.NH3 * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.NH3Sched) * 0.001;
        }
        Pollution.OtherFuel1Comp.NH3Pollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.NH3Pollution = 0.0;
        if (Pollution.OtherFuel2Coef.NH3Sched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.NH3 * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.NH3 * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.NH3Sched) * 0.001;
        }
        Pollution.OtherFuel2Comp.NH3Pollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.NMVOCPollution = 0.0;
        if (Pollution.ElecCoef.NMVOCSched == 0) {
            ElecValue = Pollution.ElecCoef.NMVOC * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.NMVOC * GetCurrentScheduleValue(state, Pollution.ElecCoef.NMVOCSched) * 0.001;
        }
        Pollution.ElecComp.NMVOCPollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.NMVOCPollution = 0.0;
        if (Pollution.NatGasCoef.NMVOCSched == 0) {
            NatGasValue = Pollution.NatGasCoef.NMVOC * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.NMVOC * GetCurrentScheduleValue(state, Pollution.NatGasCoef.NMVOCSched) * 0.001;
        }
        Pollution.NatGasComp.NMVOCPollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.NMVOCPollution = 0.0;
        if (Pollution.FuelOil1Coef.NMVOCSched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.NMVOC * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.NMVOC * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.NMVOCSched) * 0.001;
        }
        Pollution.FuelOil1Comp.NMVOCPollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.NMVOCPollution = 0.0;
        if (Pollution.FuelOil2Coef.NMVOCSched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.NMVOC * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.NMVOC * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.NMVOCSched) * 0.001;
        }
        Pollution.FuelOil2Comp.NMVOCPollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.NMVOCPollution = 0.0;
        if (Pollution.CoalCoef.NMVOCSched == 0) {
            CoalValue = Pollution.CoalCoef.NMVOC * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.NMVOC * GetCurrentScheduleValue(state, Pollution.CoalCoef.NMVOCSched) * 0.001;
        }
        Pollution.CoalComp.NMVOCPollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.NMVOCPollution = 0.0;
        if (Pollution.GasolineCoef.NMVOCSched == 0) {
            GasolineValue = Pollution.GasolineCoef.NMVOC * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.NMVOC * GetCurrentScheduleValue(state, Pollution.GasolineCoef.NMVOCSched) * 0.001;
        }
        Pollution.GasolineComp.NMVOCPollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.NMVOCPollution = 0.0;
        if (Pollution.PropaneCoef.NMVOCSched == 0) {
            PropaneValue = Pollution.PropaneCoef.NMVOC * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.NMVOC * GetCurrentScheduleValue(state, Pollution.PropaneCoef.NMVOCSched) * 0.001;
        }
        Pollution.PropaneComp.NMVOCPollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.NMVOCPollution = 0.0;
        if (Pollution.DieselCoef.NMVOCSched == 0) {
            DieselValue = Pollution.DieselCoef.NMVOC * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.NMVOC * GetCurrentScheduleValue(state, Pollution.DieselCoef.NMVOCSched) * 0.001;
        }
        Pollution.DieselComp.NMVOCPollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.NMVOCPollution = 0.0;
        if (Pollution.OtherFuel1Coef.NMVOCSched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.NMVOC * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.NMVOC * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.NMVOCSched) * 0.001;
        }
        Pollution.OtherFuel1Comp.NMVOCPollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.NMVOCPollution = 0.0;
        if (Pollution.OtherFuel2Coef.NMVOCSched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.NMVOC * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.NMVOC * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.NMVOCSched) * 0.001;
        }
        Pollution.OtherFuel2Comp.NMVOCPollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.HgPollution = 0.0;
        if (Pollution.ElecCoef.HgSched == 0) {
            ElecValue = Pollution.ElecCoef.Hg * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.Hg * GetCurrentScheduleValue(state, Pollution.ElecCoef.HgSched) * 0.001;
        }
        Pollution.ElecComp.HgPollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.HgPollution = 0.0;
        if (Pollution.NatGasCoef.HgSched == 0) {
            NatGasValue = Pollution.NatGasCoef.Hg * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.Hg * GetCurrentScheduleValue(state, Pollution.NatGasCoef.HgSched) * 0.001;
        }
        Pollution.NatGasComp.HgPollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.HgPollution = 0.0;
        if (Pollution.FuelOil1Coef.HgSched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.Hg * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.Hg * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.HgSched) * 0.001;
        }
        Pollution.FuelOil1Comp.HgPollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.HgPollution = 0.0;
        if (Pollution.FuelOil2Coef.HgSched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.Hg * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.Hg * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.HgSched) * 0.001;
        }
        Pollution.FuelOil2Comp.HgPollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.HgPollution = 0.0;
        if (Pollution.CoalCoef.HgSched == 0) {
            CoalValue = Pollution.CoalCoef.Hg * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.Hg * GetCurrentScheduleValue(state, Pollution.CoalCoef.HgSched) * 0.001;
        }
        Pollution.CoalComp.HgPollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.HgPollution = 0.0;
        if (Pollution.GasolineCoef.HgSched == 0) {
            GasolineValue = Pollution.GasolineCoef.Hg * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.Hg * GetCurrentScheduleValue(state, Pollution.GasolineCoef.HgSched) * 0.001;
        }
        Pollution.GasolineComp.HgPollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.HgPollution = 0.0;
        if (Pollution.PropaneCoef.HgSched == 0) {
            PropaneValue = Pollution.PropaneCoef.Hg * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.Hg * GetCurrentScheduleValue(state, Pollution.PropaneCoef.HgSched) * 0.001;
        }
        Pollution.PropaneComp.HgPollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.HgPollution = 0.0;
        if (Pollution.DieselCoef.HgSched == 0) {
            DieselValue = Pollution.DieselCoef.Hg * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.Hg * GetCurrentScheduleValue(state, Pollution.DieselCoef.HgSched) * 0.001;
        }
        Pollution.DieselComp.HgPollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.HgPollution = 0.0;
        if (Pollution.OtherFuel1Coef.HgSched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.Hg * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.Hg * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.HgSched) * 0.001;
        }
        Pollution.OtherFuel1Comp.HgPollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.HgPollution = 0.0;
        if (Pollution.OtherFuel2Coef.HgSched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.Hg * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.Hg * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.HgSched) * 0.001;
        }
        Pollution.OtherFuel2Comp.HgPollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.PbPollution = 0.0;
        if (Pollution.ElecCoef.PbSched == 0) {
            ElecValue = Pollution.ElecCoef.Pb * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.Pb * GetCurrentScheduleValue(state, Pollution.ElecCoef.PbSched) * 0.001;
        }
        Pollution.ElecComp.PbPollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.PbPollution = 0.0;
        if (Pollution.NatGasCoef.PbSched == 0) {
            NatGasValue = Pollution.NatGasCoef.Pb * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.Pb * GetCurrentScheduleValue(state, Pollution.NatGasCoef.PbSched) * 0.001;
        }
        Pollution.NatGasComp.PbPollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.PbPollution = 0.0;
        if (Pollution.FuelOil1Coef.PbSched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.Pb * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.Pb * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.PbSched) * 0.001;
        }
        Pollution.FuelOil1Comp.PbPollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.PbPollution = 0.0;
        if (Pollution.FuelOil2Coef.PbSched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.Pb * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.Pb * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.PbSched) * 0.001;
        }
        Pollution.FuelOil2Comp.PbPollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.PbPollution = 0.0;
        if (Pollution.CoalCoef.PbSched == 0) {
            CoalValue = Pollution.CoalCoef.Pb * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.Pb * GetCurrentScheduleValue(state, Pollution.CoalCoef.PbSched) * 0.001;
        }
        Pollution.CoalComp.PbPollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.PbPollution = 0.0;
        if (Pollution.GasolineCoef.PbSched == 0) {
            GasolineValue = Pollution.GasolineCoef.Pb * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.Pb * GetCurrentScheduleValue(state, Pollution.GasolineCoef.PbSched) * 0.001;
        }
        Pollution.GasolineComp.PbPollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.PbPollution = 0.0;
        if (Pollution.PropaneCoef.PbSched == 0) {
            PropaneValue = Pollution.PropaneCoef.Pb * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.Pb * GetCurrentScheduleValue(state, Pollution.PropaneCoef.PbSched) * 0.001;
        }
        Pollution.PropaneComp.PbPollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.PbPollution = 0.0;
        if (Pollution.DieselCoef.PbSched == 0) {
            DieselValue = Pollution.DieselCoef.Pb * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.Pb * GetCurrentScheduleValue(state, Pollution.DieselCoef.PbSched) * 0.001;
        }
        Pollution.DieselComp.PbPollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.PbPollution = 0.0;
        if (Pollution.OtherFuel1Coef.PbSched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.Pb * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.Pb * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.PbSched) * 0.001;
        }
        Pollution.OtherFuel1Comp.PbPollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.PbPollution = 0.0;
        if (Pollution.OtherFuel2Coef.PbSched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.Pb * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.Pb * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.PbSched) * 0.001;
        }
        Pollution.OtherFuel2Comp.PbPollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.WaterPollution = 0.0;
        if (Pollution.ElecCoef.WaterSched == 0) {
            ElecValue = Pollution.ElecCoef.Water;
        } else {
            ElecValue = Pollution.ElecCoef.Water * GetCurrentScheduleValue(state, Pollution.ElecCoef.WaterSched);
        }
        Pollution.ElecComp.WaterPollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.WaterPollution = 0.0;
        if (Pollution.NatGasCoef.WaterSched == 0) {
            NatGasValue = Pollution.NatGasCoef.Water;
        } else {
            NatGasValue = Pollution.NatGasCoef.Water * GetCurrentScheduleValue(state, Pollution.NatGasCoef.WaterSched);
        }
        Pollution.NatGasComp.WaterPollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.WaterPollution = 0.0;
        if (Pollution.FuelOil1Coef.WaterSched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.Water;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.Water * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.WaterSched);
        }
        Pollution.FuelOil1Comp.WaterPollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.WaterPollution = 0.0;
        if (Pollution.FuelOil2Coef.WaterSched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.Water;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.Water * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.WaterSched);
        }
        Pollution.FuelOil2Comp.WaterPollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.WaterPollution = 0.0;
        if (Pollution.CoalCoef.WaterSched == 0) {
            CoalValue = Pollution.CoalCoef.Water;
        } else {
            CoalValue = Pollution.CoalCoef.Water * GetCurrentScheduleValue(state, Pollution.CoalCoef.WaterSched);
        }
        Pollution.CoalComp.WaterPollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.WaterPollution = 0.0;
        if (Pollution.GasolineCoef.WaterSched == 0) {
            GasolineValue = Pollution.GasolineCoef.Water;
        } else {
            GasolineValue = Pollution.GasolineCoef.Water * GetCurrentScheduleValue(state, Pollution.GasolineCoef.WaterSched);
        }
        Pollution.GasolineComp.WaterPollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.WaterPollution = 0.0;
        if (Pollution.PropaneCoef.WaterSched == 0) {
            PropaneValue = Pollution.PropaneCoef.Water;
        } else {
            PropaneValue = Pollution.PropaneCoef.Water * GetCurrentScheduleValue(state, Pollution.PropaneCoef.WaterSched);
        }
        Pollution.PropaneComp.WaterPollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.WaterPollution = 0.0;
        if (Pollution.DieselCoef.WaterSched == 0) {
            DieselValue = Pollution.DieselCoef.Water;
        } else {
            DieselValue = Pollution.DieselCoef.Water * GetCurrentScheduleValue(state, Pollution.DieselCoef.WaterSched);
        }
        Pollution.DieselComp.WaterPollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.WaterPollution = 0.0;
        if (Pollution.OtherFuel1Coef.WaterSched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.Water;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.Water * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.WaterSched);
        }
        Pollution.OtherFuel1Comp.WaterPollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.WaterPollution = 0.0;
        if (Pollution.OtherFuel2Coef.WaterSched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.Water;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.Water * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.WaterSched);
        }
        Pollution.OtherFuel2Comp.WaterPollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.NucHiPollution = 0.0;
        if (Pollution.ElecCoef.NucHiSched == 0) {
            ElecValue = Pollution.ElecCoef.NucHi * 0.001;
        } else {
            ElecValue = Pollution.ElecCoef.NucHi * GetCurrentScheduleValue(state, Pollution.ElecCoef.NucHiSched) * 0.001;
        }
        Pollution.ElecComp.NucHiPollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.NucHiPollution = 0.0;
        if (Pollution.NatGasCoef.NucHiSched == 0) {
            NatGasValue = Pollution.NatGasCoef.NucHi * 0.001;
        } else {
            NatGasValue = Pollution.NatGasCoef.NucHi * GetCurrentScheduleValue(state, Pollution.NatGasCoef.NucHiSched) * 0.001;
        }
        Pollution.NatGasComp.NucHiPollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.NucHiPollution = 0.0;
        if (Pollution.FuelOil1Coef.NucHiSched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.NucHi * 0.001;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.NucHi * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.NucHiSched) * 0.001;
        }
        Pollution.FuelOil1Comp.NucHiPollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.NucHiPollution = 0.0;
        if (Pollution.FuelOil2Coef.NucHiSched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.NucHi * 0.001;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.NucHi * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.NucHiSched) * 0.001;
        }
        Pollution.FuelOil2Comp.NucHiPollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.NucHiPollution = 0.0;
        if (Pollution.CoalCoef.NucHiSched == 0) {
            CoalValue = Pollution.CoalCoef.NucHi * 0.001;
        } else {
            CoalValue = Pollution.CoalCoef.NucHi * GetCurrentScheduleValue(state, Pollution.CoalCoef.NucHiSched) * 0.001;
        }
        Pollution.CoalComp.NucHiPollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.NucHiPollution = 0.0;
        if (Pollution.GasolineCoef.NucHiSched == 0) {
            GasolineValue = Pollution.GasolineCoef.NucHi * 0.001;
        } else {
            GasolineValue = Pollution.GasolineCoef.NucHi * GetCurrentScheduleValue(state, Pollution.GasolineCoef.NucHiSched) * 0.001;
        }
        Pollution.GasolineComp.NucHiPollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.NucHiPollution = 0.0;
        if (Pollution.PropaneCoef.NucHiSched == 0) {
            PropaneValue = Pollution.PropaneCoef.NucHi * 0.001;
        } else {
            PropaneValue = Pollution.PropaneCoef.NucHi * GetCurrentScheduleValue(state, Pollution.PropaneCoef.NucHiSched) * 0.001;
        }
        Pollution.PropaneComp.NucHiPollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.NucHiPollution = 0.0;
        if (Pollution.DieselCoef.NucHiSched == 0) {
            DieselValue = Pollution.DieselCoef.NucHi * 0.001;
        } else {
            DieselValue = Pollution.DieselCoef.NucHi * GetCurrentScheduleValue(state, Pollution.DieselCoef.NucHiSched) * 0.001;
        }
        Pollution.DieselComp.NucHiPollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.NucHiPollution = 0.0;
        if (Pollution.OtherFuel1Coef.NucHiSched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.NucHi * 0.001;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.NucHi * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.NucHiSched) * 0.001;
        }
        Pollution.OtherFuel1Comp.NucHiPollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.NucHiPollution = 0.0;
        if (Pollution.OtherFuel2Coef.NucHiSched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.NucHi * 0.001;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.NucHi * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.NucHiSched) * 0.001;
        }
        Pollution.OtherFuel2Comp.NucHiPollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.FuelFactorUsed) {
        Pollution.ElecComp.NucLoPollution = 0.0;
        if (Pollution.ElecCoef.NucLoSched == 0) {
            ElecValue = Pollution.ElecCoef.NucLo;
        } else {
            ElecValue = Pollution.ElecCoef.NucLo * GetCurrentScheduleValue(state, Pollution.ElecCoef.NucLoSched);
        }
        Pollution.ElecComp.NucLoPollution = (FuelType.Elec * 1.0e-6) * ElecValue;
    }
    if (Pollution.NatGasCoef.FuelFactorUsed) {
        Pollution.NatGasComp.NucLoPollution = 0.0;
        if (Pollution.NatGasCoef.NucLoSched == 0) {
            NatGasValue = Pollution.NatGasCoef.NucLo;
        } else {
            NatGasValue = Pollution.NatGasCoef.NucLo * GetCurrentScheduleValue(state, Pollution.NatGasCoef.NucLoSched);
        }
        Pollution.NatGasComp.NucLoPollution = (FuelType.NatGas * 1.0e-6) * NatGasValue;
    }
    if (Pollution.FuelOil1Coef.FuelFactorUsed) {
        Pollution.FuelOil1Comp.NucLoPollution = 0.0;
        if (Pollution.FuelOil1Coef.NucLoSched == 0) {
            FuelOil1Value = Pollution.FuelOil1Coef.NucLo;
        } else {
            FuelOil1Value = Pollution.FuelOil1Coef.NucLo * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.NucLoSched);
        }
        Pollution.FuelOil1Comp.NucLoPollution = (FuelType.FuelOil1 * 1.0e-6) * FuelOil1Value;
    }
    if (Pollution.FuelOil2Coef.FuelFactorUsed) {
        Pollution.FuelOil2Comp.NucLoPollution = 0.0;
        if (Pollution.FuelOil2Coef.NucLoSched == 0) {
            FuelOil2Value = Pollution.FuelOil2Coef.NucLo;
        } else {
            FuelOil2Value = Pollution.FuelOil2Coef.NucLo * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.NucLoSched);
        }
        Pollution.FuelOil2Comp.NucLoPollution = (FuelType.FuelOil2 * 1.0e-6) * FuelOil2Value;
    }
    if (Pollution.CoalCoef.FuelFactorUsed) {
        Pollution.CoalComp.NucLoPollution = 0.0;
        if (Pollution.CoalCoef.NucLoSched == 0) {
            CoalValue = Pollution.CoalCoef.NucLo;
        } else {
            CoalValue = Pollution.CoalCoef.NucLo * GetCurrentScheduleValue(state, Pollution.CoalCoef.NucLoSched);
        }
        Pollution.CoalComp.NucLoPollution = (FuelType.Coal * 1.0e-6) * CoalValue;
    }
    if (Pollution.GasolineCoef.FuelFactorUsed) {
        Pollution.GasolineComp.NucLoPollution = 0.0;
        if (Pollution.GasolineCoef.NucLoSched == 0) {
            GasolineValue = Pollution.GasolineCoef.NucLo;
        } else {
            GasolineValue = Pollution.GasolineCoef.NucLo * GetCurrentScheduleValue(state, Pollution.GasolineCoef.NucLoSched);
        }
        Pollution.GasolineComp.NucLoPollution = (FuelType.Gasoline * 1.0e-6) * GasolineValue;
    }
    if (Pollution.PropaneCoef.FuelFactorUsed) {
        Pollution.PropaneComp.NucLoPollution = 0.0;
        if (Pollution.PropaneCoef.NucLoSched == 0) {
            PropaneValue = Pollution.PropaneCoef.NucLo;
        } else {
            PropaneValue = Pollution.PropaneCoef.NucLo * GetCurrentScheduleValue(state, Pollution.PropaneCoef.NucLoSched);
        }
        Pollution.PropaneComp.NucLoPollution = (FuelType.Propane * 1.0e-6) * PropaneValue;
    }
    if (Pollution.DieselCoef.FuelFactorUsed) {
        Pollution.DieselComp.NucLoPollution = 0.0;
        if (Pollution.DieselCoef.NucLoSched == 0) {
            DieselValue = Pollution.DieselCoef.NucLo;
        } else {
            DieselValue = Pollution.DieselCoef.NucLo * GetCurrentScheduleValue(state, Pollution.DieselCoef.NucLoSched);
        }
        Pollution.DieselComp.NucLoPollution = (FuelType.Diesel * 1.0e-6) * DieselValue;
    }
    if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
        Pollution.OtherFuel1Comp.NucLoPollution = 0.0;
        if (Pollution.OtherFuel1Coef.NucLoSched == 0) {
            OtherFuel1Value = Pollution.OtherFuel1Coef.NucLo;
        } else {
            OtherFuel1Value = Pollution.OtherFuel1Coef.NucLo * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.NucLoSched);
        }
        Pollution.OtherFuel1Comp.NucLoPollution = (FuelType.OtherFuel1 * 1.0e-6) * OtherFuel1Value;
    }
    if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
        Pollution.OtherFuel2Comp.NucLoPollution = 0.0;
        if (Pollution.OtherFuel2Coef.NucLoSched == 0) {
            OtherFuel2Value = Pollution.OtherFuel2Coef.NucLo;
        } else {
            OtherFuel2Value = Pollution.OtherFuel2Coef.NucLo * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.NucLoSched);
        }
        Pollution.OtherFuel2Comp.NucLoPollution = (FuelType.OtherFuel2 * 1.0e-6) * OtherFuel2Value;
    }

    Pollution.TotCarbonEquivFromN2O = Pollution.N2OPollutTotal * Pollution.CarbonEquivN2O;
    Pollution.TotCarbonEquivFromCH4 = Pollution.CH4PollutTotal * Pollution.CarbonEquivCH4;
    Pollution.TotCarbonEquivFromCO2 = Pollution.CO2PollutTotal * Pollution.CarbonEquivCO2;

    ElecValue = 0.0;
    NatGasValue = 0.0;
    FuelOil1Value = 0.0;
    FuelOil2Value = 0.0;
    CoalValue = 0.0;
    GasolineValue = 0.0;
    PropaneValue = 0.0;
    DieselValue = 0.0;
    OtherFuel1Value = 0.0;
    OtherFuel2Value = 0.0;

    if (Pollution.ElecCoef.SourceSched != 0) {
        Pollution.ElecComp.Source = FuelType.Elec * Pollution.ElecCoef.Source * GetCurrentScheduleValue(state, Pollution.ElecCoef.SourceSched);
        Pollution.ElecPurchComp.Source =
            FuelType.ElecPurch * Pollution.ElecCoef.Source * GetCurrentScheduleValue(state, Pollution.ElecCoef.SourceSched);
        Pollution.ElecSurplusSoldComp.Source =
            FuelType.ElecSold * Pollution.ElecCoef.Source * GetCurrentScheduleValue(state, Pollution.ElecCoef.SourceSched);
    } else {
        Pollution.ElecComp.Source = FuelType.Elec * Pollution.ElecCoef.Source;
        Pollution.ElecPurchComp.Source = FuelType.ElecPurch * Pollution.ElecCoef.Source;
        Pollution.ElecSurplusSoldComp.Source = FuelType.ElecSold * Pollution.ElecCoef.Source;
    }
    if (Pollution.NatGasCoef.SourceSched != 0) {
        // does not include district heating or steam
        Pollution.NatGasComp.Source =
            FuelType.NatGasFacility * Pollution.NatGasCoef.Source * GetCurrentScheduleValue(state, Pollution.NatGasCoef.SourceSched);
    } else {
        Pollution.NatGasComp.Source = FuelType.NatGasFacility * Pollution.NatGasCoef.Source;
    }
    if (Pollution.FuelOil1Coef.SourceSched != 0) {
        Pollution.FuelOil1Comp.Source =
            FuelType.FuelOil1 * Pollution.FuelOil1Coef.Source * GetCurrentScheduleValue(state, Pollution.FuelOil1Coef.SourceSched);
    } else {
        Pollution.FuelOil1Comp.Source = FuelType.FuelOil1 * Pollution.FuelOil1Coef.Source;
    }
    if (Pollution.FuelOil2Coef.SourceSched != 0) {
        Pollution.FuelOil2Comp.Source =
            FuelType.FuelOil2 * Pollution.FuelOil2Coef.Source * GetCurrentScheduleValue(state, Pollution.FuelOil2Coef.SourceSched);
    } else {
        Pollution.FuelOil1Comp.Source = FuelType.FuelOil2 * Pollution.FuelOil2Coef.Source;
    }
    if (Pollution.CoalCoef.SourceSched != 0) {
        Pollution.CoalComp.Source = FuelType.Coal * Pollution.CoalCoef.Source * GetCurrentScheduleValue(state, Pollution.CoalCoef.SourceSched);
    } else {
        Pollution.CoalComp.Source = FuelType.Coal * Pollution.CoalCoef.Source;
    }
    if (Pollution.GasolineCoef.SourceSched != 0) {
        Pollution.GasolineComp.Source =
            FuelType.Gasoline * Pollution.GasolineCoef.Source * GetCurrentScheduleValue(state, Pollution.GasolineCoef.SourceSched);
    } else {
        Pollution.GasolineComp.Source = FuelType.Gasoline * Pollution.GasolineCoef.Source;
    }
    if (Pollution.PropaneCoef.SourceSched != 0) {
        Pollution.PropaneComp.Source =
            FuelType.Propane * Pollution.PropaneCoef.Source * GetCurrentScheduleValue(state, Pollution.PropaneCoef.SourceSched);
    } else {
        Pollution.PropaneComp.Source = FuelType.Propane * Pollution.PropaneCoef.Source;
    }
    if (Pollution.DieselCoef.SourceSched != 0) {
        Pollution.DieselComp.Source =
            FuelType.Diesel * Pollution.DieselCoef.Source * GetCurrentScheduleValue(state, Pollution.DieselCoef.SourceSched);
    } else {
        Pollution.DieselComp.Source = FuelType.Diesel * Pollution.DieselCoef.Source;
    }
    if (Pollution.OtherFuel1Coef.SourceSched != 0) {
        Pollution.OtherFuel1Comp.Source =
            FuelType.OtherFuel1 * Pollution.OtherFuel1Coef.Source * GetCurrentScheduleValue(state, Pollution.OtherFuel1Coef.SourceSched);
    } else {
        Pollution.OtherFuel1Comp.Source = FuelType.OtherFuel1 * Pollution.OtherFuel1Coef.Source;
    }
    if (Pollution.OtherFuel2Coef.SourceSched != 0) {
        Pollution.OtherFuel2Comp.Source =
            FuelType.OtherFuel2 * Pollution.OtherFuel2Coef.Source * GetCurrentScheduleValue(state, Pollution.OtherFuel2Coef.SourceSched);
    } else {
        Pollution.OtherFuel2Comp.Source = FuelType.OtherFuel2 * Pollution.OtherFuel2Coef.Source;
    }
}

void ReadEnergyMeters(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   1998
    //       MODIFIED       na
    //       RE-ENGINEERED  December 2003 RJL

    // PURPOSE OF THIS SUBROUTINE:
    //       Read Energy Results from the meters
    // This routine reads the meters for the energy used

    // Using/Aliasing
    auto &FracTimeStepZone = state.dataHVACGlobal->FracTimeStepZone;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    auto &Pollution = state.dataPollutionModule->Pollution;
    auto &FuelType = state.dataPollutionModule->FuelType;

    FuelType.ElecFacility = GetInstantMeterValue(state, FuelType.ElecFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
                            GetInstantMeterValue(state, FuelType.ElecFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.DieselFacility =
        GetInstantMeterValue(state, FuelType.DieselFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.DieselFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.PurchCoolFacility =
        GetInstantMeterValue(state, FuelType.PurchCoolFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.PurchCoolFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.PurchHeatFacility =
        GetInstantMeterValue(state, FuelType.PurchHeatFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.PurchHeatFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.NatGasFacility =
        GetInstantMeterValue(state, FuelType.NatGasFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.NatGasFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.GasolineFacility =
        GetInstantMeterValue(state, FuelType.GasolineFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.GasolineFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.CoalFacility = GetInstantMeterValue(state, FuelType.CoalFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
                            GetInstantMeterValue(state, FuelType.CoalFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.FuelOil1Facility =
        GetInstantMeterValue(state, FuelType.FuelOil1FacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.FuelOil1FacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.FuelOil2Facility =
        GetInstantMeterValue(state, FuelType.FuelOil2FacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.FuelOil2FacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.PropaneFacility =
        GetInstantMeterValue(state, FuelType.PropaneFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.PropaneFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.OtherFuel1Facility =
        GetInstantMeterValue(state, FuelType.OtherFuel1FacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.OtherFuel1FacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.OtherFuel2Facility =
        GetInstantMeterValue(state, FuelType.OtherFuel2FacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.OtherFuel2FacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.ElecProducedFacility =
        GetInstantMeterValue(state, FuelType.ElecProducedFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.ElecProducedFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.SteamFacility =
        GetInstantMeterValue(state, FuelType.SteamFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.SteamFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.ElecPurchasedFacility =
        GetInstantMeterValue(state, FuelType.ElecPurchasedFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.ElecPurchasedFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);
    FuelType.ElecSurplusSoldFacility =
        GetInstantMeterValue(state, FuelType.ElecSurplusSoldFacilityIndex, OutputProcessor::TimeStepType::TimeStepZone) * FracTimeStepZone +
        GetInstantMeterValue(state, FuelType.ElecSurplusSoldFacilityIndex, OutputProcessor::TimeStepType::TimeStepSystem);

    // Now these fuel types have to be sorted and summed into categories that we have pollution factors for.
    // The Off-Site Electricity is the total needed by the facility minus the amount generated on-site.
    // The on-site pollutants will end up being other fuel types used by the generators.
    // If the difference between the 2 electric quantities is <0.0 then it will be zero for that time step.
    // We will also add the District Cooling here with a rough conversion from Energy using the User
    // defined COP.
    FuelType.Elec = FuelType.ElecFacility - FuelType.ElecProducedFacility + FuelType.PurchCoolFacility / Pollution.PurchCoolCOP;
    if (FuelType.Elec <= 0.0) FuelType.Elec = 0.0;

    // The Natural Gas fuel type will be summed from the meters with the District Heating using an efficiency.
    FuelType.NatGas =
        FuelType.NatGasFacility + FuelType.PurchHeatFacility / Pollution.PurchHeatEffic + FuelType.SteamFacility / Pollution.SteamConvEffic;

    // The Distillate Oil or Fuel Oil #1
    FuelType.FuelOil1 = FuelType.FuelOil1Facility;

    // The Residual Oil or Fuel Oil #2
    FuelType.FuelOil2 = FuelType.FuelOil2Facility;

    // The Gasoline fuel type will be summed
    FuelType.Gasoline = FuelType.GasolineFacility;

    // The Natural Gas fuel type will be summed with the Nat gas and Propane fuel types from the meters and the Purchased
    FuelType.Propane = FuelType.PropaneFacility;

    // The Coal fuel type will be assigned Coal
    FuelType.Coal = FuelType.CoalFacility;

    // The Diesel fuel type will be summed
    FuelType.Diesel = FuelType.DieselFacility;

    // The OtherFuel1 fuel type will be summed
    FuelType.OtherFuel1 = FuelType.OtherFuel1Facility;

    // The OtherFuel2 fuel type will be summed
    FuelType.OtherFuel2 = FuelType.OtherFuel2Facility;

    FuelType.ElecPurch = FuelType.ElecPurchasedFacility;

    FuelType.ElecSold = FuelType.ElecSurplusSoldFacility;
}

// *****************************************************************************
// Utility Routines to allow access to data inside this module.
// *****************************************************************************

void GetFuelFactorInfo(EnergyPlusData &state,
                       std::string const &fuelName,  // input fuel name  (standard from Tabular reports)
                       bool &fuelFactorUsed,         // return value true if user has entered this fuel
                       Real64 &fuelSourceFactor,     // if used, the source factor
                       bool &fuelFactorScheduleUsed, // if true, schedules for this fuel are used
                       int &ffScheduleIndex          // if schedules for this fuel are used, return schedule index
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   July 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine allows access to data inside this module from other modules (specifically the
    // output tabular reports.

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
    // na

    if (state.dataPollutionModule->GetInputFlagPollution) {
        GetPollutionFactorInput(state);
        state.dataPollutionModule->GetInputFlagPollution = false;
    }
    auto &Pollution = state.dataPollutionModule->Pollution;

    fuelFactorUsed = false;
    fuelSourceFactor = 0.0;
    fuelFactorScheduleUsed = false;
    ffScheduleIndex = 0;

    {
        auto const &SELECT_CASE_var(fuelName);

        if ((SELECT_CASE_var == "NaturalGas") || (SELECT_CASE_var == "Gas")) {
            if (Pollution.NatGasCoef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.NatGasCoef.Source;
                if (Pollution.NatGasCoef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.NatGasCoef.SourceSched;
                }
            } else {
                fuelSourceFactor = 1.084;
            }

        } else if (SELECT_CASE_var == "Electricity") {
            if (Pollution.ElecCoef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.ElecCoef.Source;
                if (Pollution.ElecCoef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.ElecCoef.SourceSched;
                }
            } else {
                fuelSourceFactor = 3.167;
            }

        } else if (SELECT_CASE_var == "FuelOilNo2") {
            if (Pollution.FuelOil2Coef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.FuelOil2Coef.Source;
                if (Pollution.FuelOil2Coef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.FuelOil2Coef.SourceSched;
                }
            } else {
                fuelSourceFactor = 1.05;
            }

        } else if (SELECT_CASE_var == "FuelOilNo1") {
            if (Pollution.FuelOil1Coef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.FuelOil1Coef.Source;
                if (Pollution.FuelOil1Coef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.FuelOil1Coef.SourceSched;
                }
            } else {
                fuelSourceFactor = 1.05;
            }

        } else if (SELECT_CASE_var == "Coal") {
            if (Pollution.CoalCoef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.CoalCoef.Source;
                if (Pollution.CoalCoef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.CoalCoef.SourceSched;
                }
            } else {
                fuelSourceFactor = 1.05;
            }

        } else if (SELECT_CASE_var == "Gasoline") {
            if (Pollution.GasolineCoef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.GasolineCoef.Source;
                if (Pollution.GasolineCoef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.GasolineCoef.SourceSched;
                }
            } else {
                fuelSourceFactor = 1.05;
            }

        } else if (SELECT_CASE_var == "Propane") {
            if (Pollution.PropaneCoef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.PropaneCoef.Source;
                if (Pollution.PropaneCoef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.PropaneCoef.SourceSched;
                }
            } else {
                fuelSourceFactor = 1.05;
            }

        } else if (SELECT_CASE_var == "Diesel") {
            if (Pollution.DieselCoef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.DieselCoef.Source;
                if (Pollution.DieselCoef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.DieselCoef.SourceSched;
                }
            } else {
                fuelSourceFactor = 1.05;
            }

        } else if (SELECT_CASE_var == "OtherFuel1") {
            if (Pollution.OtherFuel1Coef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.OtherFuel1Coef.Source;
                if (Pollution.OtherFuel1Coef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.OtherFuel1Coef.SourceSched;
                }
            } else {
                fuelSourceFactor = 1.0;
            }

        } else if (SELECT_CASE_var == "OtherFuel2") {
            if (Pollution.OtherFuel2Coef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.OtherFuel2Coef.Source;
                if (Pollution.OtherFuel2Coef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.OtherFuel2Coef.SourceSched;
                }
            } else {
                fuelSourceFactor = 1.0;
            }

        } else if (SELECT_CASE_var == "DistrictHeating") {
            if (Pollution.NatGasCoef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.NatGasCoef.Source / Pollution.PurchHeatEffic;
                if (Pollution.NatGasCoef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.NatGasCoef.SourceSched;
                }
            } else {
                fuelSourceFactor = 1.084 / Pollution.PurchHeatEffic;
            }

        } else if (SELECT_CASE_var == "DistrictCooling") {
            if (Pollution.ElecCoef.FuelFactorUsed) {
                fuelFactorUsed = true;
                fuelSourceFactor = Pollution.ElecCoef.Source / Pollution.PurchCoolCOP;
                if (Pollution.ElecCoef.SourceSched == 0) {
                    fuelFactorScheduleUsed = false;
                } else {
                    fuelFactorScheduleUsed = true;
                    ffScheduleIndex = Pollution.ElecCoef.SourceSched;
                }
            } else {
                fuelSourceFactor = 3.167 / Pollution.PurchCoolCOP;
            }

        } else if (SELECT_CASE_var == "Steam") {
            fuelSourceFactor = 0.3 / Pollution.SteamConvEffic;

        } else {
        }
    }
}

void GetEnvironmentalImpactFactorInfo(EnergyPlusData &state,
                                      Real64 &efficiencyDistrictHeating, // if entered, the efficiency of District Heating
                                      Real64 &efficiencyDistrictCooling, // if entered, the efficiency of District Cooling
                                      Real64 &sourceFactorSteam          // if entered, the source factor for Steam
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   August 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine allows access to data inside this module from other modules (specifically the
    // output tabular reports.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // Each of the arguments must be entered in the EnvironmentalImpactFactors object

    if (state.dataPollutionModule->GetInputFlagPollution) {
        GetPollutionFactorInput(state);
        state.dataPollutionModule->GetInputFlagPollution = false;
    }

    if (state.dataPollutionModule->NumEnvImpactFactors > 0) {
        efficiencyDistrictHeating = state.dataPollutionModule->Pollution.PurchHeatEffic;
        efficiencyDistrictCooling = state.dataPollutionModule->Pollution.PurchCoolCOP;
        sourceFactorSteam = state.dataPollutionModule->Pollution.SteamConvEffic;
    }
}

} // namespace EnergyPlus::PollutionModule
