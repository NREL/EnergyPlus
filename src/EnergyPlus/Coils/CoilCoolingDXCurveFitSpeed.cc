#include <Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <Coils/CoilCoolingDXCurveFitSpeed.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <General.hh>
#include <InputProcessing/InputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDXCurveFitSpeed::instantiateFromInputSpec(CoilCoolingDXCurveFitSpeedInputSpecification input_data)
{
    bool errorsFound(false);
    static const std::string routineName("CoilCoolingDXCurveFitSpeed::instantiateFromInputSpec: ");
    this->original_input_specs = input_data;
    this->name = input_data.name;
    this->active_fraction_of_face_coil_area = input_data.active_fraction_of_coil_face_area;
    this->rated_evap_fan_power_per_volume_flow_rate = input_data.rated_evaporator_fan_power_per_volume_flow_rate;
    this->evap_condenser_pump_power_fraction = input_data.rated_evaporative_condenser_pump_power_fraction;
    this->evap_condenser_effectiveness = input_data.evaporative_condenser_effectiveness;
    this->rated_waste_heat_fraction_of_power_input = input_data.rated_waste_heat_fraction_of_power_input;
    errorsFound |= this->processCurve(input_data.total_cooling_capacity_function_of_temperature_curve_name,
                                      this->indexCapFT,
                                      {1, 2},
                                      routineName,
                                      "Total Cooling Capacity Function of Temperature Curve Name",
                                      RatedInletWetBulbTemp,
                                      RatedOutdoorAirTemp);

    errorsFound |= this->processCurve(input_data.total_cooling_capacity_function_of_air_flow_fraction_curve_name,
                                      this->indexCapFFF,
                                      {1},
                                      routineName,
                                      "Total Cooling Capacity Function of Air Flow Fraction Curve Name",
                                      1.0);

    errorsFound |= this->processCurve(input_data.energy_input_ratio_function_of_temperature_curve_name,
                                      this->indexEIRFT,
                                      {1, 2},
                                      routineName,
                                      "Energy Input Ratio Function of Temperature Curve Name",
                                      RatedInletWetBulbTemp,
                                      RatedOutdoorAirTemp);

    errorsFound |= this->processCurve(input_data.energy_input_ratio_function_of_air_flow_fraction_curve_name,
                                      this->indexEIRFFF,
                                      {1},
                                      routineName,
                                      "Energy Input Ratio Function of Air Flow Fraction Curve Name",
                                      1.0);

    errorsFound |= this->processCurve(input_data.sensible_heat_ratio_modifier_function_of_temperature_curve_name,
                                      this->indexSHRFT,
                                      {2},  // Only allow bivariate functions since curve inputs are different from other f(Temp) functions
                                      routineName,
                                      "Sensible Heat Ratio Modifier Function of Temperature Curve Name",
                                      RatedInletWetBulbTemp,
                                      RatedOutdoorAirTemp);

    errorsFound |= this->processCurve(input_data.sensible_heat_ratio_modifier_function_of_flow_fraction_curve_name,
                                      this->indexSHRFFF,
                                      {1},
                                      routineName,
                                      "Sensible Heat Ratio Modifier Function of Air Flow Fraction Curve Name",
                                      1.0);


    // TODO: Warn if only one of SHRFT and SHRFFF are defined (curves won't be used)

    errorsFound |= this->processCurve(input_data.waste_heat_function_of_temperature_curve_name,
                                      this->indexWHFT,
                                      {2},
                                      routineName,
                                      "Waste Heat Modifier Function of Temperature Curve Name",
                                      RatedOutdoorAirTemp,
                                      RatedInletAirTemp);

    if (!errorsFound && !input_data.waste_heat_function_of_temperature_curve_name.empty()) {
        Real64 CurveVal = CurveManager::CurveValue(this->indexWHFT, RatedOutdoorAirTemp, RatedInletAirTemp);
        if (CurveVal > 1.10 || CurveVal < 0.90) {
            ShowWarningError(routineName + this->object_name + "=\"" + this->name + "\", curve values");
            ShowContinueError("Waste Heat Modifier Function of Temperature Curve Name = " + input_data.waste_heat_function_of_temperature_curve_name);
            ShowContinueError("...Waste Heat Modifier Function of Temperature Curve Name output is not equal to 1.0 (+ or - 10%) at rated conditions.");
            ShowContinueError("...Curve output at rated conditions = " + General::TrimSigDigits(CurveVal, 3));
        }
    }

    std::string fieldName("Part Load Fraction Correlation Curve Name");
    std::string curveName(input_data.part_load_fraction_correlation_curve_name);
    errorsFound |= this->processCurve(input_data.part_load_fraction_correlation_curve_name,
                                      this->indexPLRFPLF,
                                      {1},
                                      routineName,
                                      "Part Load Fraction Correlation Curve Name",
                                      1.0);

    if (!errorsFound) {
        //     Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
        Real64 MinCurveVal = 999.0;
        Real64 MaxCurveVal = -999.0;
        Real64 CurveInput = 0.0;
        Real64 MinCurvePLR, MaxCurvePLR;
        while (CurveInput <= 1.0) {
            Real64 CurveVal = CurveManager::CurveValue(this->indexPLRFPLF, CurveInput);
            if (CurveVal < MinCurveVal) {
                MinCurveVal = CurveVal;
                MinCurvePLR = CurveInput;
            }
            if (CurveVal > MaxCurveVal) {
                MaxCurveVal = CurveVal;
                MaxCurvePLR = CurveInput;
            }
            CurveInput += 0.01;
        }
        if (MinCurveVal < 0.7) {
            ShowWarningError(routineName + this->object_name + "=\"" + this->name + "\", invalid");
            ShowContinueError("..." + fieldName + "=\"" + curveName + "\" has out of range values.");
            ShowContinueError("...Curve minimum must be >= 0.7, curve min at PLR = " + General::TrimSigDigits(MinCurvePLR, 2) + " is " +
                General::TrimSigDigits(MinCurveVal, 3));
            ShowContinueError("...Setting curve minimum to 0.7 and simulation continues.");
            CurveManager::SetCurveOutputMinMaxValues(this->indexPLRFPLF, errorsFound, 0.7, _);
        }

        if (MaxCurveVal > 1.0) {
            ShowWarningError(routineName + this->object_name + "=\"" + this->name + "\", invalid");
            ShowContinueError("..." + fieldName + " = " + curveName + " has out of range value.");
            ShowContinueError("...Curve maximum must be <= 1.0, curve max at PLR = " + General::TrimSigDigits(MaxCurvePLR, 2) + " is " +
                General::TrimSigDigits(MaxCurveVal, 3));
            ShowContinueError("...Setting curve maximum to 1.0 and simulation continues.");
            CurveManager::SetCurveOutputMinMaxValues(this->indexPLRFPLF, errorsFound, _, 1.0);
        }
    }

    if (errorsFound) {
        ShowFatalError(routineName + "Errors found in getting " + this->object_name + " input. Preceding condition(s) causes termination.");
    }
}

bool CoilCoolingDXCurveFitSpeed::processCurve(const std::string curveName,
                                              int &curveIndex,
                                              std::vector<int> validDims,
                                              const std::string routineName,
                                              const std::string fieldName,
                                              Real64 const Var1,                   // required 1st independent variable
                                              Optional<Real64 const> Var2,         // 2nd independent variable
                                              Optional<Real64 const> Var3,         // 3rd independent variable
                                              Optional<Real64 const> Var4,         // 4th independent variable
                                              Optional<Real64 const> Var5)          // 5th independent variable
{
    if (curveName.empty()) {
        return false;
    } else {
        curveIndex = CurveManager::GetCurveIndex(curveName);
        if (curveIndex == 0) {
            ShowSevereError(routineName + this->object_name + "=\"" + this->name + "\", invalid");
            ShowContinueError("...not found " + fieldName + "=\"" + curveName + "\".");
            return true;
        } else {
            // Verify Curve Object dimensions
            bool errorFound = CurveManager::CheckCurveDims(curveIndex,        // Curve index
                                                           validDims,         // Valid dimensions
                                                           routineName,       // Routine name
                                                           this->object_name, // Object Type
                                                           this->name,        // Object Name
                                                           fieldName);        // Field Name

            if (!errorFound) {
                CurveManager::checkCurveIsNormalizedToOne(routineName + this->object_name,
                                                          this->name,
                                                          curveIndex,
                                                          fieldName,
                                                          curveName,
                                                          Var1,
                                                          Var2,
                                                          Var3,
                                                          Var4,
                                                          Var5);
            }
            return errorFound;
        }
    }
}

CoilCoolingDXCurveFitSpeed::CoilCoolingDXCurveFitSpeed(std::string name_to_find)
    :

      // model inputs
      indexCapFT(0), indexCapFFF(0), indexEIRFT(0), indexEIRFFF(0), indexPLRFPLF(0), indexWHFT(0), indexWHFFF(0),
      indexSHRFT(0), indexSHRFFF(0),

      // speed class inputs
      PLR(0.0),                  // coil operating part load ratio
      CondInletTemp(0.0),        // condenser inlet node temp or outdoor temp if no condenser node {C}
      ambPressure(0.0),          // outdoor pressure {Pa]
      AirFF(0.0),                // ratio of air mass flow rate to rated air mass flow rate
                                 //	RatedTotCap( 0.0 ), // rated total capacity at speed {W}
      RatedAirMassFlowRate(0.0), // rated air mass flow rate at speed {kg/s}
      RatedSHR(0.0),             // rated sensible heat ratio at speed
      RatedCBF(0.0),             // rated coil bypass factor at speed
      RatedEIR(0.0),             // rated energy input ratio at speed {W/W}
      AirMassFlow(0.0),          // coil inlet air mass flow rate {kg/s}
      FanOpMode(0),              // fan operating mode, constant or cycling fan

      // speed class outputs
      FullLoadPower(0.0), // full load power at speed {W}
      RTF(0.0),           // coil runtime fraction at speed

      // other data members
      rated_total_capacity(0.0), evap_air_flow_rate(0.0), condenser_air_flow_rate(0.0), gross_shr(0.0), active_fraction_of_face_coil_area(0.0),
      rated_evap_fan_power_per_volume_flow_rate(0.0), evap_condenser_pump_power_fraction(0.0), evap_condenser_effectiveness(0.0),
      rated_waste_heat_fraction_of_power_input(0.0),

      // rating data
      RatedInletAirTemp(26.6667),       // 26.6667C or 80F
      RatedInletWetBulbTemp(19.44),     // 19.44 or 67F
      RatedInletAirHumRat(0.01125),     // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
      RatedOutdoorAirTemp(35.0),        // 35 C or 95F
      DryCoilOutletHumRatioMin(0.00001), // dry coil outlet minimum hum ratio kgH2O/kgdry air
      mySizeFlag(true)

{
    int numModes = inputProcessor->getNumObjectsFound(CoilCoolingDXCurveFitSpeed::object_name);
    if (numModes <= 0) {
        // error
    }
    bool found_it = false;
    for (int modeNum = 1; modeNum <= numModes; ++modeNum) {
        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        inputProcessor->getObjectItem(CoilCoolingDXCurveFitSpeed::object_name, modeNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);
        if (!UtilityRoutines::SameString(name_to_find, cAlphaArgs(1))) {
            continue;
        }
        found_it = true;

        CoilCoolingDXCurveFitSpeedInputSpecification input_specs;

        input_specs.name = cAlphaArgs(1);
        input_specs.gross_rated_total_cooling_capacity_ratio_to_nominal = rNumericArgs(1);
        input_specs.evaporator_air_flow_fraction = rNumericArgs(2);
        input_specs.condenser_air_flow_fraction = rNumericArgs(3);
        input_specs.gross_rated_sensible_heat_ratio = rNumericArgs(4);
        input_specs.gross_rated_cooling_COP = rNumericArgs(5);
        input_specs.active_fraction_of_coil_face_area = rNumericArgs(6);
        input_specs.rated_evaporator_fan_power_per_volume_flow_rate = rNumericArgs(7);
        input_specs.rated_evaporative_condenser_pump_power_fraction = rNumericArgs(8);
        input_specs.evaporative_condenser_effectiveness = rNumericArgs(9);
        input_specs.total_cooling_capacity_function_of_temperature_curve_name = cAlphaArgs(2);
        input_specs.total_cooling_capacity_function_of_air_flow_fraction_curve_name = cAlphaArgs(3);
        input_specs.energy_input_ratio_function_of_temperature_curve_name = cAlphaArgs(4);
        input_specs.energy_input_ratio_function_of_air_flow_fraction_curve_name = cAlphaArgs(5);
        input_specs.part_load_fraction_correlation_curve_name = cAlphaArgs(6);
        input_specs.rated_waste_heat_fraction_of_power_input = rNumericArgs(10);
        input_specs.waste_heat_function_of_temperature_curve_name = cAlphaArgs(7);
        input_specs.sensible_heat_ratio_modifier_function_of_temperature_curve_name = cAlphaArgs(8);
        input_specs.sensible_heat_ratio_modifier_function_of_flow_fraction_curve_name = cAlphaArgs(9);

        this->instantiateFromInputSpec(input_specs);
        break;
    }

    if (!found_it) {
        // error
    }
}

void CoilCoolingDXCurveFitSpeed::sizeSpeed()
{

    std::string RoutineName = "sizeSpeed";

    this->rated_total_capacity = this->original_input_specs.gross_rated_total_cooling_capacity_ratio_to_nominal * parentMode->ratedGrossTotalCap;
    this->evap_air_flow_rate = this->original_input_specs.evaporator_air_flow_fraction * parentMode->ratedEvapAirFlowRate;
    this->condenser_air_flow_rate = this->original_input_specs.condenser_air_flow_fraction * parentMode->ratedCondAirFlowRate;
    this->gross_shr = this->original_input_specs.gross_rated_sensible_heat_ratio;

    this->RatedAirMassFlowRate = this->evap_air_flow_rate * Psychrometrics::PsyRhoAirFnPbTdbW(
                                                                DataEnvironment::StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, RoutineName);
    this->RatedCondAirMassFlowRate =
        this->condenser_air_flow_rate *
        Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, RoutineName);

    bool PrintFlag = true;
    int SizingMethod = DataHVACGlobals::CoolingSHRSizing;
    std::string CompType = this->object_name;
    std::string CompName = this->name;
    std::string SizingString = "Gross Sensible Heat Ratio";
    DataSizing::DataFlowUsedForSizing = this->evap_air_flow_rate;
    DataSizing::DataCapacityUsedForSizing = this->rated_total_capacity;
    //  DataSizing::DataEMSOverrideON = DXCoil( DXCoilNum ).RatedSHREMSOverrideOn( Mode );
    //  DataSizing::DataEMSOverride = DXCoil( DXCoilNum ).RatedSHREMSOverrideValue( Mode );
    Real64 TempSize = this->gross_shr;
    ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
    this->RatedSHR = TempSize;
    this->gross_shr = TempSize;
    DataSizing::DataFlowUsedForSizing = 0.0;
    DataSizing::DataCapacityUsedForSizing = 0.0;
    //  DataSizing::DataEMSOverrideON = false;
    //  DataSizing::DataEMSOverride = 0.0;

    // Psychrometrics::PsychState in;
    // in.tdb = RatedInletAirTemp;
    // in.w = RatedInletAirHumRat;
    // in.h = Psychrometrics::PsyHFnTdbW(RatedInletAirTemp, RatedInletAirHumRat);
    // in.p = DataEnvironment::StdPressureSeaLevel;

    this->RatedCBF = CalcBypassFactor(RatedInletAirTemp, RatedInletAirHumRat, Psychrometrics::PsyHFnTdbW(RatedInletAirTemp, RatedInletAirHumRat),
                                      DataEnvironment::StdPressureSeaLevel);
    this->RatedEIR = 1.0 / this->original_input_specs.gross_rated_cooling_COP;
}

void CoilCoolingDXCurveFitSpeed::CalcSpeedOutput(DataLoopNode::NodeData &inletNode, DataLoopNode::NodeData &outletNode, Real64 &PLR, int &fanOpMode)
{

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("CalcSpeedOutput: ");

    if (!DataGlobals::SysSizingCalc && this->mySizeFlag) {
        this->sizeSpeed();
        this->mySizeFlag = false;
    }

    if ((PLR == 0.0) || (AirMassFlow == 0.0)) {
        outletNode.Temp = inletNode.Temp;
        outletNode.HumRat = inletNode.HumRat;
        outletNode.Enthalpy = inletNode.Enthalpy;
        outletNode.Press = inletNode.Press;
        FullLoadPower = 0.0;
        RTF = 0.0;
        return;
    }

    Real64 TotCap = 0.0;
    Real64 SHR = 0.0;
    Real64 hDelta;    // enthalpy difference across cooling coil
    Real64 A0 = 0.0;  // ratio of UA to Cp
    Real64 CBF = 0.0; // adjusted coil bypass factor
    if (RatedCBF > 0.0) {
        A0 = -std::log(RatedCBF) * RatedAirMassFlowRate;
    } else {
        // This is bad - results in CBF = 1.0 which results in divide by zero below: hADP = inletState.h - hDelta / (1.0 - CBF)
        ShowFatalError(RoutineName + "Rated CBF=" + General::RoundSigDigits(RatedCBF, 6) + " is <= 0.0 for "+object_name + "=" + name);
        A0 = 0.0;
    }
    Real64 ADiff = -A0 / AirMassFlow;
    if (ADiff >= DataPrecisionGlobals::EXP_LowerLimit) {
        CBF = std::exp(ADiff);
    } else {
        CBF = 0.0;
    }

    Real64 inletWetBulb = Psychrometrics::PsyTwbFnTdbWPb(inletNode.Temp, inletNode.HumRat, ambPressure);
    Real64 inletw = inletNode.HumRat;

    int Counter = 0;              // iteration counter for dry coil condition
    int const MaxIter(30);        // iteration limit
    Real64 const Tolerance(0.01); // iteration convergence limit
    Real64 RF = 0.4;              // relaxation factor for holding back changes in value during iteration
    while (true) {

        Real64 TotCapTempModFac = 1.0;
        if (indexCapFT > 0) {
            if (CurveManager::PerfCurve(indexCapFT).NumDims == 2) {
                TotCapTempModFac = CurveManager::CurveValue(indexCapFT, inletWetBulb, CondInletTemp);
            } else {
                TotCapTempModFac = CurveManager::CurveValue(indexCapFT, CondInletTemp);
            }
        }
        Real64 TotCapFlowModFac = 1.0;
        if (indexCapFFF > 0) {
            TotCapFlowModFac = CurveManager::CurveValue(indexCapFFF, AirFF);
        }

        TotCap = this->rated_total_capacity * TotCapFlowModFac * TotCapTempModFac;
        hDelta = TotCap / AirMassFlow;

        if (indexSHRFT > 0 && indexSHRFFF > 0) {  // TODO: Do we want to allow either of these curves to default to 1.0?
            Real64 SHRTempModFrac = max(CurveManager::CurveValue(indexSHRFT, inletWetBulb, inletNode.Temp), 0.0);
            Real64 SHRFlowModFrac = max(CurveManager::CurveValue(indexSHRFFF, AirFF), 0.0);
            SHR = this->RatedSHR * SHRTempModFrac * SHRFlowModFrac;
            SHR = max(min(SHR, 1.0), 0.0);
            break;
        } else {
            // Calculate apparatus dew point conditions using TotCap and CBF
            Real64 hADP = inletNode.Enthalpy - hDelta / (1.0 - CBF);
            Real64 tADP = Psychrometrics::PsyTsatFnHPb(hADP, ambPressure, RoutineName);
            Real64 wADP = Psychrometrics::PsyWFnTdbH(tADP, hADP, RoutineName);
            Real64 hTinwADP = Psychrometrics::PsyHFnTdbW(inletNode.Temp, wADP);
            if ((inletNode.Enthalpy - hADP) > 1.e-10) {
                SHR = min((hTinwADP - hADP) / (inletNode.Enthalpy - hADP), 1.0);
            } else {
                SHR = 1.0;
            }
            // Check for dry evaporator conditions (win < wadp)
            if (wADP > inletw || (Counter >= 1 && Counter < MaxIter)) {
                if (inletw == 0.0) inletw = 0.00001;
                Real64 werror = (inletw - wADP) / inletw;
                // Increase InletAirHumRatTemp at constant InletAirTemp to find coil dry-out point. Then use the
                // capacity at the dry-out point to determine exiting conditions from coil. This is required
                // since the TotCapTempModFac doesn't work properly with dry-coil conditions.
                inletw = RF * wADP + (1.0 - RF) * inletw;
                inletWetBulb = Psychrometrics::PsyTwbFnTdbWPb(inletNode.Temp, inletw, ambPressure);
                ++Counter;
                if (std::abs(werror) > Tolerance) continue; // Recalculate with modified inlet conditions
                break;
            } else {
                break;
            }
        }
    }

    Real64 PLF = 1.0; // part load factor as a function of PLR, RTF = PLR / PLF
    if (indexPLRFPLF > 0) {
        PLF = CurveManager::CurveValue(indexPLRFPLF, PLR); // Calculate part-load factor
    }
    if (fanOpMode == DataHVACGlobals::CycFanCycCoil) DataHVACGlobals::OnOffFanPartLoadFraction = PLF;

    Real64 EIRTempModFac = 1.0; // EIR as a function of temperature curve result
    if (indexEIRFT > 0) {
        if (CurveManager::PerfCurve(indexEIRFT).NumDims == 2) {
            EIRTempModFac = CurveManager::CurveValue(indexEIRFT, inletWetBulb, CondInletTemp);
        } else {
            EIRTempModFac = CurveManager::CurveValue(indexEIRFT, CondInletTemp);
        }
    }
    Real64 EIRFlowModFac = 1.0; // EIR as a function of flow fraction curve result
    if (indexEIRFFF > 0) {
        EIRFlowModFac = CurveManager::CurveValue(indexEIRFFF, AirFF);
    }

    Real64 EIR = RatedEIR * EIRFlowModFac * EIRTempModFac;
    RTF = PLR / PLF;
    FullLoadPower = TotCap * EIR;

    outletNode.Enthalpy = inletNode.Enthalpy - hDelta;
    Real64 hTinwout = inletNode.Enthalpy - ((1.0 - SHR) * hDelta);
    outletNode.HumRat = Psychrometrics::PsyWFnTdbH(inletNode.Temp, hTinwout);
    outletNode.Temp = Psychrometrics::PsyTdbFnHW(outletNode.Enthalpy, outletNode.HumRat);

}

Real64 CoilCoolingDXCurveFitSpeed::CalcBypassFactor(Real64 tdb, Real64 w, Real64 h, Real64 p)
{

    static std::string const RoutineName("CalcBypassFactor: ");
    // Bypass factors are calculated at rated conditions at sea level (make sure in.p is Standard Pressure)
    Real64 calcCBF = 0.0;

    Real64 airMassFlowRate = evap_air_flow_rate * Psychrometrics::PsyRhoAirFnPbTdbW(p, tdb, w);
    Real64 deltaH = rated_total_capacity / airMassFlowRate;
    Real64 outp = p;
    Real64 outh = h - deltaH;
    Real64 outw = Psychrometrics::PsyWFnTdbH(tdb, h - (1.0 - this->gross_shr) * deltaH); // enthalpy at Tdb,in and Wout
    Real64 outtdb = Psychrometrics::PsyTdbFnHW(outh, outw);
    Real64 outrh = Psychrometrics::PsyRhFnTdbWPb(outtdb, outw, outp);

    if (outrh >= 1.0) {
        Real64 outletAirTempSat = Psychrometrics::PsyTsatFnHPb(outh, outp, RoutineName);
        if (outtdb < outletAirTempSat) { // Limit to saturated conditions at OutletAirEnthalpy
            outtdb = outletAirTempSat + 0.005;
            outw = Psychrometrics::PsyWFnTdbH(outtdb, outh, RoutineName);
            Real64 adjustedSHR = (Psychrometrics::PsyHFnTdbW(tdb, outw) - outh) / deltaH;
            ShowWarningError(RoutineName + object_name + " \"" + name +
                             "\", SHR adjusted to achieve valid outlet air properties and the simulation continues.");
            ShowContinueError("Initial SHR = " + General::RoundSigDigits(this->gross_shr, 5));
            ShowContinueError("Adjusted SHR = " + General::RoundSigDigits(adjustedSHR, 5));
        }
    }

    // ADP conditions
    Real64 adp_tdb = Psychrometrics::PsyTdpFnWPb(outw, outp);

    std::size_t iter = 0;
    const std::size_t maxIter(50);
    Real64 errorLast = 100.0;
    Real64 deltaADPTemp = 5.0;
    Real64 tolerance = 1.0; // initial conditions for iteration
    Real64 slopeAtConds = 0.0;
    Real64 deltaT = tdb - outtdb;
    Real64 deltaHumRat = w - outw;
    bool cbfErrors = false;

    if (deltaT > 0.0) slopeAtConds = deltaHumRat / deltaT;

    Real64 adp_w = min(outw, Psychrometrics::PsyWFnTdpPb(adp_tdb, DataEnvironment::StdPressureSeaLevel));

    while ((iter <= maxIter) && (tolerance > 0.001)) {

        // Do for IterMax iterations or until the error gets below .1%
        if (iter > 0) adp_tdb += deltaADPTemp;
        ++iter;
        //  Find new slope using guessed Tadp
        adp_w = min(outw, Psychrometrics::PsyWFnTdpPb(adp_tdb, DataEnvironment::StdPressureSeaLevel));
        Real64 slope = (w - adp_w) / max(0.001, (tdb - adp_tdb));
        //  check for convergence (slopes are equal to within error tolerance)
        Real64 error = (slope - slopeAtConds) / slopeAtConds;
        if ((error > 0.0) && (errorLast < 0.0)) {
            deltaADPTemp = -deltaADPTemp / 2.0;
        } else if ((error < 0.0) && (errorLast > 0.0)) {
            deltaADPTemp = -deltaADPTemp / 2.0;
        } else if (abs(error) > abs(errorLast)) {
            deltaADPTemp = -deltaADPTemp / 2.0;
        }
        errorLast = error;
        tolerance = std::abs(error);
    }

    //   Calculate Bypass Factor from Enthalpies
    Real64 adp_h = Psychrometrics::PsyHFnTdbW(adp_tdb, adp_w);
    calcCBF = min(1.0, (outh - adp_h) / (h - adp_h));

    if (iter > maxIter) {
        ShowSevereError(RoutineName + object_name + " \"" + name + "\" -- coil bypass factor calculation did not converge after max iterations.");
        ShowContinueError("The RatedSHR of [" + General::RoundSigDigits(this->gross_shr, 3) +
                          "], entered by the user or autosized (see *.eio file),");
        ShowContinueError("may be causing this. The line defined by the coil rated inlet air conditions");
        ShowContinueError("(26.7C drybulb and 19.4C wetbulb) and the RatedSHR (i.e., slope of the line) must intersect");
        ShowContinueError("the saturation curve of the psychrometric chart. If the RatedSHR is too low, then this");
        ShowContinueError("intersection may not occur and the coil bypass factor calculation will not converge.");
        ShowContinueError("If autosizing the SHR, recheck the design supply air humidity ratio and design supply air");
        ShowContinueError("temperature values in the Sizing:System and Sizing:Zone objects. In general, the temperatures");
        ShowContinueError("and humidity ratios specified in these two objects should be the same for each system");
        ShowContinueError("and the zones that it serves.");
        ShowContinueErrorTimeStamp("");
        cbfErrors = true; // Didn't converge within MaxIter iterations
    }
    if (calcCBF < 0.0) {
        ShowSevereError(RoutineName + object_name + " \"" + name + "\" -- negative coil bypass factor calculated.");
        ShowContinueErrorTimeStamp("");
        cbfErrors = true; // Negative CBF not valid
    }
    // Show fatal error for specific coil that caused a CBF error
    if (cbfErrors) {
        ShowFatalError(RoutineName + object_name + " \"" + name + "\" Errors found in calculating coil bypass factors");
    }
    return calcCBF;
}
