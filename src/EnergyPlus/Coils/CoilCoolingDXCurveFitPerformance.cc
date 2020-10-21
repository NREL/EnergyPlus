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

#include <EnergyPlus/Coils/CoilCoolingDXCurveFitPerformance.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/TempSolveRoot.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDXCurveFitPerformance::instantiateFromInputSpec(EnergyPlus::EnergyPlusData &state, const CoilCoolingDXCurveFitPerformanceInputSpecification &input_data)
{
    static const std::string routineName("CoilCoolingDXCurveFitOperatingMode::instantiateFromInputSpec: ");
    bool errorsFound(false);
    this->original_input_specs = input_data;
    this->name = input_data.name;
    this->minOutdoorDrybulb = input_data.minimum_outdoor_dry_bulb_temperature_for_compressor_operation;
    this->maxOutdoorDrybulbForBasin = input_data.maximum_outdoor_dry_bulb_temperature_for_crankcase_heater_operation;
    this->crankcaseHeaterCap = input_data.crankcase_heater_capacity;
    this->normalMode = CoilCoolingDXCurveFitOperatingMode(state, input_data.base_operating_mode_name);
    if (UtilityRoutines::SameString(input_data.capacity_control, "CONTINUOUS")) {
        this->capControlMethod = CapControlMethod::CONTINUOUS;
    } else if (UtilityRoutines::SameString(input_data.capacity_control, "DISCRETE")) {
        this->capControlMethod = CapControlMethod::DISCRETE;
    } else {
        ShowSevereError(routineName + this->object_name + "=\"" + this->name + "\", invalid");
        ShowContinueError("...Capacity Control Method=\"" + input_data.capacity_control + "\":");
        ShowContinueError("...must be Discrete or Continuous.");
        errorsFound = true;
    }
    this->evapCondBasinHeatCap = input_data.basin_heater_capacity;
    this->evapCondBasinHeatSetpoint = input_data.basin_heater_setpoint_temperature;
    if (input_data.basin_heater_operating_schedule_name.empty()) {
        this->evapCondBasinHeatSchedulIndex = DataGlobalConstants::ScheduleAlwaysOn();
    } else {
        this->evapCondBasinHeatSchedulIndex = ScheduleManager::GetScheduleIndex(state, input_data.basin_heater_operating_schedule_name);
    }
    if (this->evapCondBasinHeatSchedulIndex == 0) {
        ShowSevereError(routineName + this->object_name + "=\"" + this->name + "\", invalid");
        ShowContinueError("...Evaporative Condenser Basin Heater Operating Schedule Name=\"" + input_data.basin_heater_operating_schedule_name +
                          "\".");
        errorsFound = true;
    }

    if (input_data.unit_internal_static_air_pressure > 0) {
        // if this isn't in the input data then we will just keep it initialized at zero and use that as the flag
        // for whether we are doing static+fan standard ratings or not
        this->unitStatic = input_data.unit_internal_static_air_pressure;
    }

    if (!input_data.alternate_operating_mode_name.empty() && input_data.alternate_operating_mode2_name.empty()) {
        this->hasAlternateMode = DataHVACGlobals::coilEnhancedMode;
        this->alternateMode = CoilCoolingDXCurveFitOperatingMode(state, input_data.alternate_operating_mode_name);
    }
    if (!input_data.alternate_operating_mode2_name.empty() && !input_data.alternate_operating_mode_name.empty()) {
        this->hasAlternateMode = DataHVACGlobals::coilSubcoolReheatMode;
        this->alternateMode = CoilCoolingDXCurveFitOperatingMode(state, input_data.alternate_operating_mode_name);
        this->alternateMode2 = CoilCoolingDXCurveFitOperatingMode(state, input_data.alternate_operating_mode2_name);
        setOperMode(this->normalMode, 1);
        setOperMode(this->alternateMode, 2);
        setOperMode(this->alternateMode2, 3);
    }

    if (errorsFound) {
        ShowFatalError(routineName + "Errors found in getting " + this->object_name + " input. Preceding condition(s) causes termination.");
    }
}

CoilCoolingDXCurveFitPerformance::CoilCoolingDXCurveFitPerformance(EnergyPlus::EnergyPlusData &state, const std::string &name_to_find)
{
    int numPerformances = inputProcessor->getNumObjectsFound(state, CoilCoolingDXCurveFitPerformance::object_name);
    if (numPerformances <= 0) {
        // error
    }
    bool found_it = false;
    for (int perfNum = 1; perfNum <= numPerformances; ++perfNum) {
        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        inputProcessor->getObjectItem(
            state, CoilCoolingDXCurveFitPerformance::object_name, perfNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, lAlphaFieldBlanks);
        if (!UtilityRoutines::SameString(name_to_find, cAlphaArgs(1))) {
            continue;
        }
        found_it = true;

        CoilCoolingDXCurveFitPerformanceInputSpecification input_specs;

        input_specs.name = cAlphaArgs(1);
        input_specs.crankcase_heater_capacity = rNumericArgs(1);
        input_specs.minimum_outdoor_dry_bulb_temperature_for_compressor_operation = rNumericArgs(2);
        input_specs.maximum_outdoor_dry_bulb_temperature_for_crankcase_heater_operation = rNumericArgs(3);
        // TODO: The static pressure no longer has a default, this needs to check for blank
        input_specs.unit_internal_static_air_pressure = rNumericArgs(4);
        input_specs.capacity_control = cAlphaArgs(2);
        input_specs.basin_heater_capacity = rNumericArgs(5);
        input_specs.basin_heater_setpoint_temperature = rNumericArgs(6);
        input_specs.basin_heater_operating_schedule_name = cAlphaArgs(3);
        input_specs.compressor_fuel_type = DataGlobalConstants::AssignResourceTypeNum(cAlphaArgs(4));
        input_specs.base_operating_mode_name = cAlphaArgs(5);
        if (!lAlphaFieldBlanks(6)) {
            input_specs.alternate_operating_mode_name = cAlphaArgs(6);
        }
        if (!lAlphaFieldBlanks(7)) {
            input_specs.alternate_operating_mode2_name = cAlphaArgs(7);
        }

        this->instantiateFromInputSpec(state, input_specs);
        break;
    }

    if (!found_it) {
        ShowFatalError("Could not find Coil:Cooling:DX:Performance object with name: " + name_to_find);
    }
}

void CoilCoolingDXCurveFitPerformance::simulate(EnergyPlus::EnergyPlusData &state, const DataLoopNode::NodeData &inletNode,
                                                DataLoopNode::NodeData &outletNode,
                                                int useAlternateMode,
                                                Real64 &PLR,
                                                int &speedNum,
                                                Real64 &speedRatio,
                                                int &fanOpMode,
                                                DataLoopNode::NodeData &condInletNode,
                                                DataLoopNode::NodeData &condOutletNode,
                                                Real64 LoadSHR)
{
    Real64 reportingConstant = DataHVACGlobals::TimeStepSys * DataGlobalConstants::SecInHour();

    if (useAlternateMode == DataHVACGlobals::coilSubcoolReheatMode) {
        Real64 totalCoolingRate;
        Real64 sensNorRate;
        Real64 sensSubRate;
        Real64 sensRehRate;
        Real64 SysNorSHR;
        Real64 SysSubSHR;
        Real64 SysRehSHR;
        Real64 minAirHumRat;
        Real64 HumRatNorOut;
        Real64 TempNorOut;
        Real64 EnthalpyNorOut;
        Real64 modeRatio;

        this->recoveredEnergyRate = 0.0;
        this->NormalSHR = 0.0;
        this->calculate(state, this->normalMode, inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode, condInletNode, condOutletNode);

        //this->OperatingMode = 1;
        totalCoolingRate = outletNode.MassFlowRate * (inletNode.Enthalpy - outletNode.Enthalpy);
        minAirHumRat = min(inletNode.HumRat, outletNode.HumRat);
        sensNorRate = outletNode.MassFlowRate *
                      (Psychrometrics::PsyHFnTdbW(inletNode.Temp, minAirHumRat) - Psychrometrics::PsyHFnTdbW(outletNode.Temp, minAirHumRat));
        if (totalCoolingRate > 1.0E-10) {
            this->OperatingMode = 1;
            this->NormalSHR = sensNorRate / totalCoolingRate;
        }

        if (PLR == 0.0) return;
        if (LoadSHR == 0.0) return;

        if (totalCoolingRate == 0.0) {
            SysNorSHR = 1.0;
        } else {
            SysNorSHR = sensNorRate / totalCoolingRate;
        }
        HumRatNorOut = outletNode.HumRat;
        TempNorOut = outletNode.Temp;
        EnthalpyNorOut = outletNode.Enthalpy;
        this->recoveredEnergyRate = sensNorRate;

        if (LoadSHR < SysNorSHR) {
            outletNode.MassFlowRate = inletNode.MassFlowRate;
            this->calculate(state, this->alternateMode, inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode, condInletNode, condOutletNode);
            totalCoolingRate = outletNode.MassFlowRate * (inletNode.Enthalpy - outletNode.Enthalpy);
            minAirHumRat = min(inletNode.HumRat, outletNode.HumRat);
            sensSubRate = outletNode.MassFlowRate *
                          (Psychrometrics::PsyHFnTdbW(inletNode.Temp, minAirHumRat) - Psychrometrics::PsyHFnTdbW(outletNode.Temp, minAirHumRat));
            SysSubSHR = sensSubRate / totalCoolingRate;
            if (LoadSHR < SysSubSHR) {
                outletNode.MassFlowRate = inletNode.MassFlowRate;
                this->calculate(state, this->alternateMode2, inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode, condInletNode, condOutletNode);
                totalCoolingRate = outletNode.MassFlowRate * (inletNode.Enthalpy - outletNode.Enthalpy);
                minAirHumRat = min(inletNode.HumRat, outletNode.HumRat);
                sensRehRate = outletNode.MassFlowRate *
                              (Psychrometrics::PsyHFnTdbW(inletNode.Temp, minAirHumRat) - Psychrometrics::PsyHFnTdbW(outletNode.Temp, minAirHumRat));
                SysRehSHR = sensRehRate / totalCoolingRate;
                if (LoadSHR > SysRehSHR) {
                    modeRatio = (LoadSHR - SysNorSHR) / (SysRehSHR - SysNorSHR);
                    this->OperatingMode = 3;
                    outletNode.HumRat = HumRatNorOut * (1.0 - modeRatio) + modeRatio * outletNode.HumRat;
                    outletNode.Enthalpy = EnthalpyNorOut * (1.0 - modeRatio) + modeRatio * outletNode.Enthalpy;
                    outletNode.Temp = Psychrometrics::PsyTdbFnHW(outletNode.Enthalpy, outletNode.HumRat);
                    this->ModeRatio = modeRatio;
                    // update other reporting terms
                    this->powerUse = this->normalMode.OpModePower * (1.0 - modeRatio) + modeRatio * this->alternateMode2.OpModePower;
                    this->RTF = this->normalMode.OpModeRTF * (1.0 - modeRatio) + modeRatio * this->alternateMode2.OpModeRTF;
                    this->electricityConsumption = this->powerUse * reportingConstant;
                    this->wasteHeatRate = this->normalMode.OpModeWasteHeat * (1.0 - modeRatio) + modeRatio * this->alternateMode2.OpModeWasteHeat;
                    this->recoveredEnergyRate = (this->recoveredEnergyRate - sensRehRate) * this->ModeRatio;
                    return;
                } else {
                    this->ModeRatio = 1.0;
                    this->OperatingMode = 3;
                    this->recoveredEnergyRate = (this->recoveredEnergyRate - sensRehRate) * this->ModeRatio;
                    return;
                }
            } else {
                modeRatio = (LoadSHR - SysNorSHR) / (SysSubSHR - SysNorSHR);
                this->OperatingMode = 2;
                // process outlet conditions and total output
                outletNode.HumRat = HumRatNorOut * (1.0 - modeRatio) + modeRatio * outletNode.HumRat;
                outletNode.Enthalpy = EnthalpyNorOut * (1.0 - modeRatio) + modeRatio * outletNode.Enthalpy;
                outletNode.Temp = Psychrometrics::PsyTdbFnHW(outletNode.Enthalpy, outletNode.HumRat);
                this->ModeRatio = modeRatio;
                // update other reporting terms
                this->powerUse = this->normalMode.OpModePower * (1.0 - modeRatio) + modeRatio * this->alternateMode.OpModePower;
                this->RTF = this->normalMode.OpModeRTF * (1.0 - modeRatio) + modeRatio * this->alternateMode.OpModeRTF;
                this->electricityConsumption = this->powerUse * reportingConstant;
                this->wasteHeatRate = this->normalMode.OpModeWasteHeat * (1.0 - modeRatio) + modeRatio * this->alternateMode.OpModeWasteHeat;
                this->recoveredEnergyRate = (this->recoveredEnergyRate - sensSubRate) * this->ModeRatio;
                return;
            }
        } else {
            this->ModeRatio = 0.0;
            this->OperatingMode = 1;
            this->recoveredEnergyRate = 0.0;
            return;
        }

    } else if (useAlternateMode == DataHVACGlobals::coilEnhancedMode) {
        this->calculate(state, this->alternateMode, inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode, condInletNode, condOutletNode);
    } else {
        this->calculate(state, this->normalMode, inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode, condInletNode, condOutletNode);
    }
}

void CoilCoolingDXCurveFitPerformance::size(EnergyPlus::EnergyPlusData &state)
{
    if (!DataGlobals::SysSizingCalc && this->mySizeFlag) {
        this->normalMode.size(state);
        if (this->hasAlternateMode == DataHVACGlobals::coilEnhancedMode) {
            this->alternateMode.size(state);
        }
        if (this->hasAlternateMode == DataHVACGlobals::coilSubcoolReheatMode) {
            this->alternateMode.size(state);
            this->alternateMode2.size(state);
        }
        this->mySizeFlag = false;
    }
}

void CoilCoolingDXCurveFitPerformance::calculate(EnergyPlus::EnergyPlusData &state,
                                                 CoilCoolingDXCurveFitOperatingMode &currentMode,
                                                 const DataLoopNode::NodeData &inletNode,
                                                 DataLoopNode::NodeData &outletNode,
                                                 Real64 &PLR,
                                                 int &speedNum,
                                                 Real64 &speedRatio,
                                                 int &fanOpMode,
                                                 DataLoopNode::NodeData &condInletNode,
                                                 DataLoopNode::NodeData &condOutletNode)
{

    // calculate the performance at this mode/speed
    currentMode.CalcOperatingMode(state, inletNode, outletNode, PLR, speedNum, speedRatio, fanOpMode, condInletNode, condOutletNode);

    // scaling term to get rate into consumptions
    Real64 reportingConstant = DataHVACGlobals::TimeStepSys * DataGlobalConstants::SecInHour();

    // calculate crankcase heater operation
    if (DataEnvironment::OutDryBulbTemp < this->maxOutdoorDrybulbForBasin) {
        this->crankcaseHeaterPower = this->crankcaseHeaterCap;
    } else {
        this->crankcaseHeaterPower = 0.0;
    }
    this->crankcaseHeaterPower = this->crankcaseHeaterPower * (1.0 - this->RTF);
    this->crankcaseHeaterElectricityConsumption = this->crankcaseHeaterPower * reportingConstant;

    // basin heater
    if (this->evapCondBasinHeatSchedulIndex > 0) {
        Real64 currentBasinHeaterAvail = ScheduleManager::GetCurrentScheduleValue(this->evapCondBasinHeatSchedulIndex);
        if (this->evapCondBasinHeatCap > 0.0 && currentBasinHeaterAvail > 0.0) {
            this->basinHeaterPower = max(0.0, this->evapCondBasinHeatCap * (this->evapCondBasinHeatSetpoint - DataEnvironment::OutDryBulbTemp));
        }
    } else {
        // If schedule does not exist, basin heater operates anytime outdoor dry-bulb temp is below setpoint
        if (this->evapCondBasinHeatCap > 0.0) {
            this->basinHeaterPower = max(0.0, this->evapCondBasinHeatCap * (this->evapCondBasinHeatSetpoint - DataEnvironment::OutDryBulbTemp));
        }
    }
    this->basinHeaterPower *= (1.0 - this->RTF);

    // update other reporting terms
    this->powerUse = currentMode.OpModePower;
    this->RTF = currentMode.OpModeRTF;
    this->electricityConsumption = this->powerUse * reportingConstant;
    this->wasteHeatRate = currentMode.OpModeWasteHeat;

}

void CoilCoolingDXCurveFitPerformance::calcStandardRatings(EnergyPlus::EnergyPlusData &state, int supplyFanIndex, int const supplyFanType, std::string const &supplyFanName, int condInletNodeIndex) {

    using TempSolveRoot::SolveRoot;
    // If fan index hasn't been set, we can't do anything
    if (supplyFanIndex == -1) { // didn't find VAV fan, do not rate this coil
        ShowWarningError("CalcTwoSpeedDXCoilStandardRating: Did not find an appropriate fan associated with DX coil named = \"" + this->name +
                         "\". Standard Ratings will not be calculated.");
        return;
    }

    static constexpr auto Format_890(
            "('! <VAV DX Cooling Coil Standard Rating Information>, DX Coil Type, DX Coil Name, Fan Type, Fan Name, "
            "','Standard Net Cooling Capacity {W}, Standard Net Cooling Capacity {Btu/h}, IEER {Btu/W-h}, ','COP 100% "
            "Capacity {W/W}, COP 75% Capacity {W/W}, COP 50% Capacity {W/W}, COP 25% Capacity {W/W}, ','EER 100% Capacity "
            "{Btu/W-h}, EER 75% Capacity {Btu/W-h}, EER 50% Capacity {Btu/W-h}, EER 25% Capacity {Btu/W-h}, ','Supply Air "
            "Flow 100% {kg/s}, Supply Air Flow 75% {kg/s},Supply Air Flow 50% {kg/s},Supply Air Flow 25% {kg/s}')\n");
    print(state.files.eio, Format_890);

    std::string const RoutineName = "CoilCoolingDXCurveFitPerformance::calcStandardRatings";

    // 365 W/1000 scfm or 773.3 W/(m3/s). The AHRI standard
    // specifies a nominal/default fan electric power consumption per rated air
    // volume flow rate to account for indoor fan electric power consumption
    // when the standard tests are conducted on units that do not have an
    // indoor air circulating fan. Used if user doesn't enter a specific value.
    Real64 const DefaultFanPowerPerEvapAirFlowRate(773.3);

    // Calculate the Indoor fan electric power consumption.  The electric power consumption is estimated
    // using either user supplied or AHRI default value for fan power per air volume flow rate
    Real64 const AirMassFlowRatioRated(1.0);                // AHRI test is at the design flow rate
    Real64 const CoolingCoilInletAirWetBulbTempRated(19.4); // 19.44C (67F)
    Real64 const CoolingCoilInletAirDryBulbTempRated(26.7);
    Real64 const OutdoorUnitInletAirDryBulbTempRated(35.0); // 35.00C (95F)
    Real64 NetCoolingCapRated(0.0);
    Real64 ExternalStatic = 0.0;
    Real64 FanStaticPressureRise;
    Real64 fanPowerCorrection = 0.0;
    Real64 fanHeatCorrection;
    Real64 totCapFlowModFac;
    Real64 totCapTempModFac;
    int fanInletNode = 0;
    int fanOutletNode = 0;

    bool errorsFound = false;

    if (this->unitStatic > 0.0) {
        totCapFlowModFac = CurveManager::CurveValue(state, this->normalMode.speeds.back().indexCapFFF, AirMassFlowRatioRated);
        totCapTempModFac = CurveManager::CurveValue(state,
            this->normalMode.speeds.back().indexCapFT, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated);
        for (int Iter = 1; Iter <= 4; ++Iter) { // iterative solution in the event that net capacity is near a threshold for external static
            // Obtain external static pressure from Table 5 in ANSI/AHRI Std. 340/360-2007
            if (NetCoolingCapRated <= 21000.0) {
                ExternalStatic = 50.0;
            } else if (21000.0 < NetCoolingCapRated && NetCoolingCapRated <= 30800.0) {
                ExternalStatic = 60.0;
            } else if (30800.0 < NetCoolingCapRated && NetCoolingCapRated <= 39300.0) {
                ExternalStatic = 70.0;
            } else if (39300.0 < NetCoolingCapRated && NetCoolingCapRated <= 61500.0) {
                ExternalStatic = 90.0;
            } else if (61500.0 < NetCoolingCapRated && NetCoolingCapRated <= 82100.0) {
                ExternalStatic = 100.0;
            } else if (82100.0 < NetCoolingCapRated && NetCoolingCapRated <= 103000.0) {
                ExternalStatic = 110.0;
            } else if (103000.0 < NetCoolingCapRated && NetCoolingCapRated <= 117000.0) {
                ExternalStatic = 140.0;
            } else if (117000.0 < NetCoolingCapRated && NetCoolingCapRated <= 147000.0) {
                ExternalStatic = 160.0;
            } else if (147000.0 < NetCoolingCapRated) {
                ExternalStatic = 190.0;
            }
            FanStaticPressureRise = ExternalStatic + this->unitStatic;
            if (supplyFanType == DataHVACGlobals::FanType_SystemModelObject) {
                fanInletNode = HVACFan::fanObjs[supplyFanIndex]->inletNodeNum;
                fanOutletNode = HVACFan::fanObjs[supplyFanIndex]->outletNodeNum;
            } else { // TODO: Are there other fan types to be considered here??
                fanInletNode = Fans::GetFanInletNode(state, "FAN:VARIABLEVOLUME", supplyFanName, errorsFound);
                fanOutletNode = Fans::GetFanOutletNode(state, "FAN:VARIABLEVOLUME", supplyFanName, errorsFound);
            }

            // set node state variables in preparation for fan model.
            DataLoopNode::Node(fanInletNode).MassFlowRate = this->normalMode.ratedEvapAirFlowRate;
            DataLoopNode::Node(fanOutletNode).MassFlowRate = this->normalMode.ratedEvapAirFlowRate;
            DataLoopNode::Node(fanInletNode).Temp = CoolingCoilInletAirDryBulbTempRated;
            DataLoopNode::Node(fanInletNode).HumRat = Psychrometrics::PsyWFnTdbTwbPb(
                CoolingCoilInletAirDryBulbTempRated, CoolingCoilInletAirWetBulbTempRated, DataEnvironment::OutBaroPress, RoutineName);
            DataLoopNode::Node(fanInletNode).Enthalpy =
                Psychrometrics::PsyHFnTdbW(CoolingCoilInletAirDryBulbTempRated, DataLoopNode::Node(fanInletNode).HumRat);
            if (supplyFanType == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[supplyFanIndex]->simulate(state, _, true, false, FanStaticPressureRise);
                fanPowerCorrection = HVACFan::fanObjs[supplyFanIndex]->fanPower();
            } else {
                Fans::SimulateFanComponents(state, supplyFanName, true, supplyFanIndex, _, true, false, FanStaticPressureRise);
                fanPowerCorrection = Fans::GetFanPower(supplyFanIndex);
            }

            fanHeatCorrection = DataLoopNode::Node(fanOutletNode).Enthalpy - DataLoopNode::Node(fanInletNode).Enthalpy;

            NetCoolingCapRated = this->normalMode.ratedGrossTotalCap * totCapTempModFac * totCapFlowModFac - fanHeatCorrection;
        }

    } else {
        fanPowerCorrection = DefaultFanPowerPerEvapAirFlowRate * this->normalMode.ratedEvapAirFlowRate;
        fanHeatCorrection = DefaultFanPowerPerEvapAirFlowRate * this->normalMode.ratedEvapAirFlowRate;
        totCapFlowModFac = CurveManager::CurveValue(state, this->normalMode.speeds.back().indexCapFFF, AirMassFlowRatioRated);
        totCapTempModFac = CurveManager::CurveValue(state,
            this->normalMode.speeds.back().indexCapFT, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated);
        NetCoolingCapRated = this->normalMode.ratedGrossTotalCap * totCapTempModFac * totCapFlowModFac - fanHeatCorrection;
    }

    std::vector<Real64> EER_TestPoint_SI{0, 0, 0, 0};      // 0 = A, 1 = B, 2 = C, 3 = D
    std::vector<Real64> EER_TestPoint_IP{0, 0, 0, 0};      // 0 = A, 1 = B, 2 = C, 3 = D
    std::vector<Real64> NetCapacity_TestPoint{0, 0, 0, 0}; // 0 = A, 1 = B, 2 = C, 3 = D
    std::vector<Real64> NetPower_TestPoint{0, 0, 0, 0};    // 0 = A, 1 = B, 2 = C, 3 = D
    std::vector<Real64> SupAirMdot_TestPoint{0, 0, 0, 0};  // 0 = A, 1 = B, 2 = C, 3 = D

    SupAirMdot_TestPoint[0] = this->normalMode.ratedEvapAirMassFlowRate;

    // Calculate Energy Efficiency Ratio (EER) at (19.44C WB and 35.0C DB ), ANSI/AHRI Std. 340/360
    Real64 EIRTempModFac =
        CurveManager::CurveValue(state, this->normalMode.speeds.back().indexEIRFT, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated);
    Real64 EIRFlowModFac = CurveManager::CurveValue(state, this->normalMode.speeds.back().indexEIRFFF, AirMassFlowRatioRated);
    Real64 EIR = 0.0;
    if (this->normalMode.speeds.back().ratedCOP > 0.0) {
        // RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
        EIR = EIRTempModFac * EIRFlowModFac / this->normalMode.speeds.back().ratedCOP;
    }
    Real64 TotalElecPowerRated = EIR * (this->normalMode.ratedGrossTotalCap * totCapTempModFac * totCapFlowModFac) + fanPowerCorrection;

    Real64 EER = 0.0;
    if (TotalElecPowerRated > 0.0) {
        EER = NetCoolingCapRated / TotalElecPowerRated;
    }

    // IEER - A point 100 % net capacity
    EER_TestPoint_SI[0] = EER;
    Real64 const ConvFromSIToIP(3.412141633); // Conversion from SI to IP [3.412 Btu/hr-W]
    EER_TestPoint_IP[0] = EER * ConvFromSIToIP;

    Real64 speedRatio = 1.0;
    Real64 CycRatio = 1.0;
    DataLoopNode::NodeData evapInlet;
    DataLoopNode::NodeData evapOutlet;
    DataLoopNode::NodeData condInlet;
    DataLoopNode::NodeData condOutlet;
    // setup point A
    evapInlet.MassFlowRate = this->normalMode.ratedEvapAirMassFlowRate;
    evapInlet.MassFlowRateMax = this->normalMode.ratedEvapAirMassFlowRate;
    evapInlet.Temp = 26.7;
    evapInlet.HumRat = Psychrometrics::PsyWFnTdbTwbPb(26.7, 19.4, DataEnvironment::OutBaroPress, RoutineName);
    evapInlet.Enthalpy = Psychrometrics::PsyHFnTdbW(26.7, evapInlet.HumRat);
    condInlet.Temp = OutdoorUnitInletAirDryBulbTempRated;
    int speedNum = 1;
    int fanOpMode = DataHVACGlobals::CycFanCycCoil;
    this->calculate(state, this->normalMode, evapInlet, evapOutlet, CycRatio, speedNum, speedRatio, fanOpMode, condInlet, condOutlet);
    Real64 TempDryBulb_Leaving_Apoint = evapOutlet.Temp;

    std::vector<Real64> const OutdoorUnitInletAirDryBulbTempPLTestPoint({27.5, 20.0, 18.3});
    std::vector<Real64> const NetCapacityFactorPLTestPoint({0.75, 0.50, 0.25});

    // IEER - part load test points ***************************************************
    for (int PartLoadTestPoint = 1; PartLoadTestPoint <= 3; ++PartLoadTestPoint) {
        // determine minimum unloading capacity fraction at point B conditions.
        Real64 heldOutdoorDB =
            DataEnvironment::OutDryBulbTemp; // TODO: Ugly, shared, potential race condition, blah. Shouldn't we just get from the condInletNode!?
        if (condInletNodeIndex != 0) {
            DataLoopNode::Node(condInletNodeIndex).Temp = OutdoorUnitInletAirDryBulbTempPLTestPoint[PartLoadTestPoint - 1];
        } else {
            DataEnvironment::OutDryBulbTemp = OutdoorUnitInletAirDryBulbTempPLTestPoint[PartLoadTestPoint - 1];
        }

        Real64 TargetNetCapacity = NetCapacityFactorPLTestPoint[PartLoadTestPoint - 1] * NetCoolingCapRated;

        std::vector<Real64> par; // Parameter array passed to solver
        par.push_back(TempDryBulb_Leaving_Apoint);
        par.push_back(TargetNetCapacity);
        par.push_back(OutdoorUnitInletAirDryBulbTempPLTestPoint[PartLoadTestPoint - 1]);
        par.push_back(CoolingCoilInletAirWetBulbTempRated);
        par.push_back(CoolingCoilInletAirDryBulbTempRated);
        if (this->unitStatic > 0.0) {
            par.push_back(0.0);
            par.push_back(double(fanInletNode));
            par.push_back(double(fanOutletNode));
            par.push_back(ExternalStatic);
            par.push_back(double(supplyFanIndex));
            par.push_back(double(supplyFanType));
        } else {
            par.push_back(DefaultFanPowerPerEvapAirFlowRate);
            par.push_back(0.0);
            par.push_back(0.0);
            par.push_back(0.0);
            par.push_back(0.0);
            par.push_back(0.0);
        }

        Real64 LowerBoundMassFlowRate = 0.01 * this->normalMode.ratedEvapAirMassFlowRate;

        int SolverFlag = 0;
        Real64 const AccuracyTolerance(0.2); // tolerance in AHRI 340/360 Table 6 note 1
        int const MaximumIterations(1000);
        Real64 PartLoadAirMassFlowRate = 0.0;
        auto f = std::bind(&CoilCoolingDXCurveFitPerformance::calcIEERResidual, this, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3);
        TempSolveRoot::SolveRoot(state, AccuracyTolerance,
                           MaximumIterations,
                           SolverFlag,
                           PartLoadAirMassFlowRate,
                           f,
                           LowerBoundMassFlowRate,
                           this->normalMode.ratedEvapAirMassFlowRate,
                           par);

        // reset outdoor dry bulb, this is gross
        DataEnvironment::OutDryBulbTemp = heldOutdoorDB;

        if (SolverFlag == -1) {
            ShowWarningError("CalcTwoSpeedDXCoilStandardRating: air flow rate solver failed. Iteration limit exceeded ");
            SupAirMdot_TestPoint[PartLoadTestPoint] = -999.0;
            EER_TestPoint_SI[PartLoadTestPoint] = -999.0;
            EER_TestPoint_IP[PartLoadTestPoint] = -999.0;
            NetCapacity_TestPoint[PartLoadTestPoint] = -999.0;
            NetPower_TestPoint[PartLoadTestPoint] = -999.0;
        } else if (SolverFlag == -2) {
            ShowWarningError("CalcTwoSpeedDXCoilStandardRating: air flow rate solver failed. root not bounded ");
            SupAirMdot_TestPoint[PartLoadTestPoint] = -999.0;
            EER_TestPoint_SI[PartLoadTestPoint] = -999.0;
            EER_TestPoint_IP[PartLoadTestPoint] = -999.0;
            NetCapacity_TestPoint[PartLoadTestPoint] = -999.0;
            NetPower_TestPoint[PartLoadTestPoint] = -999.0;
        } else {
            // now we have the supply air flow rate
            SupAirMdot_TestPoint[PartLoadTestPoint] = PartLoadAirMassFlowRate;
            Real64 AirMassFlowRatio = PartLoadAirMassFlowRate / this->normalMode.ratedEvapAirMassFlowRate;
            Real64 const SupplyAirHumRat = Psychrometrics::PsyWFnTdbTwbPb(
                CoolingCoilInletAirDryBulbTempRated, CoolingCoilInletAirWetBulbTempRated, DataEnvironment::OutBaroPress, RoutineName);

            if (this->unitStatic > 0.0) {
                FanStaticPressureRise = this->unitStatic + (ExternalStatic * pow_2(AirMassFlowRatio));
                DataLoopNode::Node(fanInletNode).MassFlowRate = PartLoadAirMassFlowRate;
                DataLoopNode::Node(fanInletNode).Temp = CoolingCoilInletAirDryBulbTempRated;
                DataLoopNode::Node(fanInletNode).HumRat = SupplyAirHumRat;
                DataLoopNode::Node(fanInletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(CoolingCoilInletAirDryBulbTempRated, SupplyAirHumRat);

                if (supplyFanType == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[supplyFanIndex]->simulate(state, _, true, false, FanStaticPressureRise);
                    fanPowerCorrection = HVACFan::fanObjs[supplyFanIndex]->fanPower();
                } else {
                    Fans::SimulateFanComponents(state, supplyFanName, true, supplyFanIndex, _, true, false, FanStaticPressureRise);
                    fanPowerCorrection = Fans::GetFanPower(supplyFanIndex);
                }

                fanHeatCorrection = DataLoopNode::Node(fanOutletNode).Enthalpy - DataLoopNode::Node(fanInletNode).Enthalpy;

            } else {
                fanPowerCorrection = DefaultFanPowerPerEvapAirFlowRate * PartLoadAirMassFlowRate;
                fanHeatCorrection = DefaultFanPowerPerEvapAirFlowRate * PartLoadAirMassFlowRate;
            }

            totCapFlowModFac = CurveManager::CurveValue(state, this->normalMode.speeds.back().indexCapFFF, AirMassFlowRatio);
            totCapTempModFac = CurveManager::CurveValue(state, this->normalMode.speeds.back().indexCapFT,
                                                        CoolingCoilInletAirWetBulbTempRated,
                                                        OutdoorUnitInletAirDryBulbTempPLTestPoint[PartLoadTestPoint - 1]);
            Real64 HighSpeedTotCoolingCap = this->normalMode.ratedGrossTotalCap * totCapTempModFac * totCapFlowModFac;
            Real64 HighSpeedNetCoolingCap = HighSpeedTotCoolingCap - fanHeatCorrection;

            EIRTempModFac = CurveManager::CurveValue(state, this->normalMode.speeds.back().indexEIRFT,
                                                     CoolingCoilInletAirWetBulbTempRated,
                                                     OutdoorUnitInletAirDryBulbTempPLTestPoint[PartLoadTestPoint - 1]);
            EIRFlowModFac = CurveManager::CurveValue(state, this->normalMode.speeds.back().indexEIRFFF, AirMassFlowRatio);
            Real64 EIR_HighSpeed = 0.0;
            if (this->normalMode.speeds.back().ratedCOP > 0.0) {
                // RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
                EIR_HighSpeed = EIRTempModFac * EIRFlowModFac / this->normalMode.speeds.back().ratedCOP;
            }

            totCapFlowModFac = CurveManager::CurveValue(state, this->normalMode.speeds[0].indexCapFFF, AirMassFlowRatio);
            totCapTempModFac = CurveManager::CurveValue(state, this->normalMode.speeds[0].indexCapFT,
                                                        CoolingCoilInletAirWetBulbTempRated,
                                                        OutdoorUnitInletAirDryBulbTempPLTestPoint[PartLoadTestPoint - 1]);
            Real64 const LowSpeedTotCoolingCap = this->normalMode.speeds[0].rated_total_capacity * totCapTempModFac * totCapFlowModFac;
            Real64 const LowSpeedNetCoolingCap = LowSpeedTotCoolingCap - fanHeatCorrection;

            EIRTempModFac = CurveManager::CurveValue(state, this->normalMode.speeds[0].indexEIRFT,
                                                     CoolingCoilInletAirWetBulbTempRated,
                                                     OutdoorUnitInletAirDryBulbTempPLTestPoint[PartLoadTestPoint - 1]);
            EIRFlowModFac = CurveManager::CurveValue(state, this->normalMode.speeds[0].indexEIRFFF, AirMassFlowRatio);
            Real64 EIR_LowSpeed = 0.0;
            if (this->normalMode.speeds[0].ratedCOP > 0.0) {
                // RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
                EIR_LowSpeed = EIRTempModFac * EIRFlowModFac / this->normalMode.speeds[0].ratedCOP;
            }

            if (LowSpeedNetCoolingCap <= TargetNetCapacity) {
                Real64 SpeedRatio = (TargetNetCapacity - LowSpeedNetCoolingCap) / (HighSpeedNetCoolingCap - LowSpeedNetCoolingCap);
                Real64 const TotCoolingCap = HighSpeedTotCoolingCap * SpeedRatio + LowSpeedTotCoolingCap * (1.0 - SpeedRatio);
                Real64 const NetCoolingCap = TotCoolingCap - fanHeatCorrection;
                EIR = EIR_HighSpeed * SpeedRatio + EIR_LowSpeed * (1.0 - SpeedRatio);
                TotalElecPowerRated = TotCoolingCap * EIR + fanPowerCorrection;
                EER_TestPoint_SI[PartLoadTestPoint] = NetCoolingCap / TotalElecPowerRated;
                EER_TestPoint_IP[PartLoadTestPoint] = EER_TestPoint_SI[PartLoadTestPoint] * ConvFromSIToIP;
                NetCapacity_TestPoint[PartLoadTestPoint] = NetCoolingCap;
                NetPower_TestPoint[PartLoadTestPoint] = TotalElecPowerRated;
            } else { // minimum unloading limit exceeded without cycling, so cycle
                CycRatio = TargetNetCapacity / LowSpeedNetCoolingCap;
                Real64 PLF = CurveManager::CurveValue(state, this->normalMode.speeds.back().indexPLRFPLF, CycRatio);
                if (PLF < 0.7) {
                    PLF = 0.7;
                }
                Real64 RunTimeFraction = CycRatio / PLF;
                RunTimeFraction = min(RunTimeFraction, 1.0);
                Real64 const TotCoolingCap = LowSpeedTotCoolingCap * RunTimeFraction;
                Real64 const NetCoolingCap = TotCoolingCap - fanHeatCorrection;
                TotalElecPowerRated = LowSpeedTotCoolingCap * EIR_LowSpeed * RunTimeFraction + fanPowerCorrection;
                EER_TestPoint_SI[PartLoadTestPoint] = NetCoolingCap / TotalElecPowerRated;
                EER_TestPoint_IP[PartLoadTestPoint] = EER_TestPoint_SI[PartLoadTestPoint] * ConvFromSIToIP;
                NetCapacity_TestPoint[PartLoadTestPoint] = NetCoolingCap;
                NetPower_TestPoint[PartLoadTestPoint] = TotalElecPowerRated;
            }
        }
    } // loop over 3 part load test points

    Real64 const IEER = (0.02 * EER_TestPoint_IP[0]) + (0.617 * EER_TestPoint_IP[1]) + (0.238 * EER_TestPoint_IP[2]) + (0.125 * EER_TestPoint_IP[3]);

    // begin output
    if (this->oneTimeEIOHeaderWrite) {
        print(state.files.eio, Format_890); // TODO: Verify this works
        this->oneTimeEIOHeaderWrite = false;
        OutputReportPredefined::pdstVAVDXCoolCoil =
            OutputReportPredefined::newPreDefSubTable(OutputReportPredefined::pdrEquip, "VAV DX Cooling Standard Rating Details");
        OutputReportPredefined::pdchVAVDXCoolCoilType =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "DX Cooling Coil Type");
        OutputReportPredefined::pdchVAVDXFanName =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "Assocated Fan");
        OutputReportPredefined::pdchVAVDXCoolCoilNetCapSI =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "Net Cooling Capacity [W]");
        OutputReportPredefined::pdchVAVDXCoolCoilCOP =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "COP [W/W]");
        OutputReportPredefined::pdchVAVDXCoolCoilEERIP =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "EER [Btu/W-h]");
        OutputReportPredefined::pdchVAVDXCoolCoilIEERIP =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "IEER [Btu/W-h]");
        OutputReportPredefined::pdchVAVDXCoolCoilMdotA =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "Supply Air Flow 100% [kg/s]");
        OutputReportPredefined::pdchVAVDXCoolCoilCOP_B =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "COP 75% Capacity [W/W]");
        OutputReportPredefined::pdchVAVDXCoolCoilEER_B_IP =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "EER 75% Capacity [Btu/W-h]");
        OutputReportPredefined::pdchVAVDXCoolCoilMdotB =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "Supply Air Flow 75% [kg/s]");
        OutputReportPredefined::pdchVAVDXCoolCoilCOP_C =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "COP 50% Capacity [W/W]");
        OutputReportPredefined::pdchVAVDXCoolCoilEER_C_IP =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "EER 50% Capacity [Btu/W-h]");
        OutputReportPredefined::pdchVAVDXCoolCoilMdotC =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "Supply Air Flow 50% [kg/s]");
        OutputReportPredefined::pdchVAVDXCoolCoilCOP_D =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "COP 25% Capacity [W/W]");
        OutputReportPredefined::pdchVAVDXCoolCoilEER_D_IP =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "EER 25% Capacity [Btu/W-h]");
        OutputReportPredefined::pdchVAVDXCoolCoilMdotD =
            OutputReportPredefined::newPreDefColumn(OutputReportPredefined::pdstVAVDXCoolCoil, "Supply Air Flow 25% [kg/s]");

        // determine footnote content
        // TODO: This may be slightly incorrect if all the parent objects haven't created coils yet
        // Once we move to a factory approach, all the coils should've been interpreted, so this will be "OK"
        // BUT actually, the performance object can't go get info about the coils so I'm not sure what to do here yet
        //        int countStaticInputs = 0;
        //        for (auto & thisCoil : ) {
        //            if (DXCoil(index).RateWithInternalStaticAndFanObject && DXCoil(index).DXCoilType_Num == CoilDX_CoolingTwoSpeed) {
        //                ++countStaticInputs;
        //            }
        //        }
        //
        //        if (countStaticInputs == NumDXMulSpeedCoils) {
        //            OutputReportPredefined::addFootNoteSubTable(OutputReportPredefined::pdstVAVDXCoolCoil, "Packaged VAV unit ratings per ANSI/AHRI
        //            Standard 340/360-2007 with Addenda 1 and 2");
        //        } else if (countStaticInputs == 0) {
        //            OutputReportPredefined::addFootNoteSubTable(OutputReportPredefined::pdstVAVDXCoolCoil,
        //                                "Indoor-coil-only unit ratings per ANSI/AHRI Standard 340/360-2007 with Addenda 1 and 2, with "
        //                                "supply fan specific power at 365 {W/1000cfm} (773.3 {W/(m3/s)})");
        //        } else { // both
        //            OutputReportPredefined::addFootNoteSubTable(OutputReportPredefined::pdstVAVDXCoolCoil,
        //                                "Packaged VAV unit ratings per ANSI/AHRI Standard 340/360-2007 with Addenda 1 and 2, "
        //                                "indoor-coil-only units with supply fan specific power at 365 {W/1000cfm} (773.3 {W/(m3/s)})");
        //        }
    }

    static constexpr auto fmt = " VAV DX Cooling Coil Standard Rating Information, {},{},{},{},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.4R},{:.4R},{:.4R},{:.4R}\n";
    if (this->unitStatic > 0) {
        print(state.files.eio, fmt,"Coil:Cooling:DX", this->name,"Fan:VariableVolume",
              supplyFanName, NetCoolingCapRated,(NetCoolingCapRated * ConvFromSIToIP), IEER,EER_TestPoint_SI[0],EER_TestPoint_SI[1],
              EER_TestPoint_SI[2],EER_TestPoint_SI[3],EER_TestPoint_IP[0],EER_TestPoint_IP[1],EER_TestPoint_IP[2],
              EER_TestPoint_IP[3],SupAirMdot_TestPoint[0],SupAirMdot_TestPoint[1],SupAirMdot_TestPoint[2],SupAirMdot_TestPoint[3]);
    } else {
        print(state.files.eio, fmt,"Coil:Cooling:DX", "N/A","Fan:VariableVolume",
              "N/A", NetCoolingCapRated,(NetCoolingCapRated * ConvFromSIToIP), IEER,EER_TestPoint_SI[0],EER_TestPoint_SI[1],
              EER_TestPoint_SI[2],EER_TestPoint_SI[3],EER_TestPoint_IP[0],EER_TestPoint_IP[1],EER_TestPoint_IP[2],
              EER_TestPoint_IP[3],SupAirMdot_TestPoint[0],SupAirMdot_TestPoint[1],SupAirMdot_TestPoint[2],SupAirMdot_TestPoint[3]);
    }

    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchDXCoolCoilType, this->name, "Coil:Cooling:DX");
    // W to tons
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchDXCoolCoilNetCapSI, this->name, NetCoolingCapRated, 1);
    // These will convert with a factor of 1 which is ok
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchDXCoolCoilCOP, this->name, EER_TestPoint_SI[0], 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchDXCoolCoilEERIP, this->name, EER_TestPoint_IP[0], 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchDXCoolCoilIEERIP, this->name, IEER, 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchDXCoolCoilSEERUserIP, this->name, "N/A");
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchDXCoolCoilSEERStandardIP, this->name, "N/A");
    OutputReportPredefined::addFootNoteSubTable(OutputReportPredefined::pdstDXCoolCoil, "ANSI/AHRI ratings include supply fan");

    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilType, this->name, "Coil:Cooling:DX");
    if (this->unitStatic > 0) {
        OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXFanName, this->name, supplyFanName);
    } else {
        OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXFanName, this->name, "None");
    }
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilNetCapSI, this->name, NetCoolingCapRated, 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilIEERIP, this->name, IEER, 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilEERIP, this->name, EER_TestPoint_IP[0], 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilMdotA, this->name, SupAirMdot_TestPoint[0], 4);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilCOP_B, this->name, EER_TestPoint_SI[1], 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilEER_B_IP, this->name, EER_TestPoint_IP[1], 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilMdotB, this->name, SupAirMdot_TestPoint[1], 4);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilCOP_C, this->name, EER_TestPoint_SI[2], 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilEER_C_IP, this->name, EER_TestPoint_IP[2], 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilMdotC, this->name, SupAirMdot_TestPoint[2], 4);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilCOP_D, this->name, EER_TestPoint_SI[3], 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilEER_D_IP, this->name, EER_TestPoint_IP[3], 2);
    OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchVAVDXCoolCoilMdotD, this->name, SupAirMdot_TestPoint[3], 4);
}

Real64
CoilCoolingDXCurveFitPerformance::calcIEERResidual(EnergyPlus::EnergyPlusData &state,Real64 const SupplyAirMassFlowRate, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                   std::vector<Real64> const &Par)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   July 2012

    // PURPOSE OF THIS FUNCTION:
    // Calculates residual function (desired outlet temp - actual outlet temp)
    // Two Speed DX Coil rating for VAV, output depends on the supply air flow rate which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    // Calls CalcMultiSpeedDXCoil to get outlet temperature at the given supply flow rate and SpeedRatio
    // and calculates the residual as defined above

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    static std::string const RoutineName("CalcTwoSpeedDXCoilIEERResidual");
    Real64 OutletAirTemp; // outlet air temperature [C]
    Real64 TargetCoilLeavingDryBulb;
    Real64 OutdoorUnitInletDryBulb;
    Real64 IndoorUnitInletDryBulb;
    Real64 IndoorUnitInletWetBulb;
    Real64 AirMassFlowRatio;
    Real64 SpeedRatio;
    Real64 CycRatio;
    Real64 TargetNetCapacity;
    Real64 FanPowerPerEvapAirFlowRate;
    int FanInletNodeNum;
    int FanOutletNodeNum;
    Real64 FanExternalStaticFull;
    Real64 SupplyAirVolFlowRate;
    Real64 FanStaticPressureRise;
    Real64 FanHeatCorrection;
    Real64 TotCapFlowModFac;
    Real64 TotCapTempModFac;
    Real64 HighSpeedNetCoolingCap;
    Real64 LowSpeedNetCoolingCap;

    TargetCoilLeavingDryBulb = Par[0];
    TargetNetCapacity = Par[1];
    OutdoorUnitInletDryBulb = Par[2];
    IndoorUnitInletWetBulb = Par[3];
    IndoorUnitInletDryBulb = Par[4];
    FanPowerPerEvapAirFlowRate = Par[5];
    FanInletNodeNum = int(Par[6]);
    FanOutletNodeNum = int(Par[7]);
    FanExternalStaticFull = Par[8];
    int supplyFanIndex = int(Par[9]);
    int supplyFanTypeNum = int(Par[10]);

    if (this->normalMode.ratedEvapAirFlowRate > 0.0) {
        AirMassFlowRatio = SupplyAirMassFlowRate / this->normalMode.ratedEvapAirFlowRate;
    } else {
        AirMassFlowRatio = 0.0;
    }
    Real64 const SupplyAirHumRat =
        Psychrometrics::PsyWFnTdbTwbPb(IndoorUnitInletDryBulb, IndoorUnitInletWetBulb, DataEnvironment::OutBaroPress, RoutineName);
    Real64 const SupplyAirRho =
        Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, IndoorUnitInletDryBulb, SupplyAirHumRat, RoutineName);

    SupplyAirVolFlowRate = SupplyAirMassFlowRate / SupplyAirRho;

    if (this->unitStatic > 0.0) {
        // modify external static per AHRI 340/360, Table 6, note 1.
        FanStaticPressureRise = this->unitStatic + (FanExternalStaticFull * pow_2(AirMassFlowRatio));
        DataLoopNode::Node(FanInletNodeNum).MassFlowRate = SupplyAirMassFlowRate;
        DataLoopNode::Node(FanOutletNodeNum).MassFlowRate = SupplyAirMassFlowRate;
        DataLoopNode::Node(FanInletNodeNum).Temp = IndoorUnitInletDryBulb;
        DataLoopNode::Node(FanInletNodeNum).HumRat =
            Psychrometrics::PsyWFnTdbTwbPb(IndoorUnitInletDryBulb, IndoorUnitInletWetBulb, DataEnvironment::OutBaroPress, RoutineName);
        DataLoopNode::Node(FanInletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(IndoorUnitInletDryBulb, DataLoopNode::Node(FanInletNodeNum).HumRat);
        if (supplyFanTypeNum == DataHVACGlobals::FanType_SystemModelObject) {
            HVACFan::fanObjs[supplyFanIndex]->simulate(state, _, true, false, FanStaticPressureRise);
        } else {
            // TODO: I am hoping we can just pass in the supply fan name
            Fans::SimulateFanComponents(state, "", true, supplyFanIndex, _, true, false, FanStaticPressureRise);
        }

        FanHeatCorrection = DataLoopNode::Node(FanOutletNodeNum).Enthalpy - DataLoopNode::Node(FanInletNodeNum).Enthalpy;

    } else {

        FanHeatCorrection = FanPowerPerEvapAirFlowRate * SupplyAirVolFlowRate;
    }

    TotCapFlowModFac = CurveManager::CurveValue(state, this->normalMode.speeds.back().indexCapFFF, AirMassFlowRatio);
    TotCapTempModFac = CurveManager::CurveValue(state, this->normalMode.speeds.back().indexCapFT, IndoorUnitInletWetBulb, OutdoorUnitInletDryBulb);
    HighSpeedNetCoolingCap = this->normalMode.speeds.back().parentModeRatedGrossTotalCap * TotCapTempModFac * TotCapFlowModFac - FanHeatCorrection;

    TotCapFlowModFac = CurveManager::CurveValue(state, this->normalMode.speeds[0].indexCapFFF, AirMassFlowRatio);
    TotCapTempModFac = CurveManager::CurveValue(state, this->normalMode.speeds[0].indexCapFT, IndoorUnitInletWetBulb, OutdoorUnitInletDryBulb);
    LowSpeedNetCoolingCap = this->normalMode.speeds[0].parentModeRatedGrossTotalCap * TotCapTempModFac * TotCapFlowModFac - FanHeatCorrection;

    if (LowSpeedNetCoolingCap <= TargetNetCapacity) {
        CycRatio = 1.0;
        SpeedRatio = (TargetNetCapacity - LowSpeedNetCoolingCap) / (HighSpeedNetCoolingCap - LowSpeedNetCoolingCap);
    } else { // minimum unloading limit exceeded for no cycling
        SpeedRatio = 0.0;
        CycRatio = TargetNetCapacity / LowSpeedNetCoolingCap;
    }

    DataLoopNode::NodeData evapInlet;
    DataLoopNode::NodeData evapOutlet;
    DataLoopNode::NodeData condInlet;
    DataLoopNode::NodeData condOutlet;
    // setup point A
    evapInlet.MassFlowRate = SupplyAirMassFlowRate;
    evapInlet.MassFlowRateMax = SupplyAirMassFlowRate;
    evapInlet.Temp = 26.7;
    evapInlet.HumRat = Psychrometrics::PsyWFnTdbTwbPb(26.7, 19.4, DataEnvironment::OutBaroPress, RoutineName);
    evapInlet.Enthalpy = Psychrometrics::PsyHFnTdbW(26.7, evapInlet.HumRat);
    int speedNum = (int)this->normalMode.speeds.size();
    int fanOpMode = DataHVACGlobals::CycFanCycCoil;
    this->calculate(state, this->normalMode, evapInlet, evapOutlet, CycRatio, speedNum, SpeedRatio, fanOpMode, condInlet, condOutlet);

    OutletAirTemp = evapOutlet.Temp;
    return TargetCoilLeavingDryBulb - OutletAirTemp;
}

void CoilCoolingDXCurveFitPerformance::setOperMode(CoilCoolingDXCurveFitOperatingMode &currentMode, int const mode)
{
    // set parent mode for each speed
    int numSpeeds;
    bool errorsFound = false;

    numSpeeds = (int)currentMode.speeds.size();
    for (int speedNum = 0; speedNum < numSpeeds; speedNum++) {
        currentMode.speeds[speedNum].parentOperatingMode = mode;
        if (mode == 2) {
            if (currentMode.speeds[speedNum].indexSHRFT == 0) {
                ShowSevereError(currentMode.speeds[speedNum].object_name + "=\"" + currentMode.speeds[speedNum].name + "\", Curve check:");
                ShowContinueError("The input of Sensible Heat Ratio Modifier Function of Temperature Curve Name is required, but not available for "
                                  "SubcoolReheat mode. Please input");
                errorsFound = true;
            }
            if (currentMode.speeds[speedNum].indexSHRFFF == 0) {
                ShowSevereError(currentMode.speeds[speedNum].object_name + "=\"" + currentMode.speeds[speedNum].name + "\", Curve check:");
                ShowContinueError("The input of Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name is required, but not available for "
                                  "SubcoolReheat mode. Please input");
                errorsFound = true;
            }
        }
        if (mode == 3) {
            if (currentMode.speeds[speedNum].indexSHRFT == 0) {
                ShowSevereError(currentMode.speeds[speedNum].object_name + "=\"" + currentMode.speeds[speedNum].name + "\", Curve check:");
                ShowContinueError("The input of Sensible Heat Ratio Modifier Function of Temperature Curve Name is required, but not available for "
                                  "SubcoolReheat mode. Please input");
                errorsFound = true;
            }
            if (currentMode.speeds[speedNum].indexSHRFFF == 0) {
                ShowSevereError(currentMode.speeds[speedNum].object_name + "=\"" + currentMode.speeds[speedNum].name + "\", Curve check:");
                ShowContinueError("The input of Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name is required, but not available for "
                                  "SubcoolReheat mode. Please input");
                errorsFound = true;
            }
        }
    }
    if (errorsFound) {
        ShowFatalError("CoilCoolingDXCurveFitPerformance: Errors found in getting " + this->object_name +
                       " input. Preceding condition(s) causes termination.");
    }
}
