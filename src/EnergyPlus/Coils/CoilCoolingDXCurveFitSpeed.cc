#include <Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <Coils/CoilCoolingDXCurveFitSpeed.hh>
#include <Coils/PsychStruct.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DXCoils.hh>
#include <InputProcessing/InputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDXCurveFitSpeed::instantiateFromInputSpec( CoilCoolingDXCurveFitSpeedInputSpecification input_data ) {
    this->original_input_specs = input_data;
    //this->parentMode = _parentMode;
//	this->mySizeFlag = true; // set up flag for sizing
    //bool errorsFound = false;
    this->name = input_data.name;
//    this->rated_total_capacity = input_data.gross_rated_total_cooling_capacity_ratio_to_nominal * parentMode->ratedGrossTotalCap;
//    this->evap_air_flow_rate = input_data.evaporator_air_flow_fraction * parentMode->ratedEvapAirFlowRate;
//    this->condenser_air_flow_rate = input_data.condenser_air_flow_fraction * parentMode->ratedCondAirFlowRate;
//    this->gross_shr = input_data.gross_rated_sensible_heat_ratio;
    this->active_fraction_of_face_coil_area = input_data.active_fraction_of_coil_face_area;
    this->rated_evap_fan_power_per_volume_flow_rate = input_data.rated_evaporator_fan_power_per_volume_flow_rate;
    this->evap_condenser_pump_power_fraction = input_data.rated_evaporative_condenser_pump_power_fraction;
    this->evap_condenser_effectiveness = input_data.evaporative_condenser_effectiveness;
    this->rated_waste_heat_fraction_of_power_input = input_data.rated_waste_heat_fraction_of_power_input;
	if ( input_data.total_cooling_capacity_function_of_temperature_curve_name != "" ) {
		this->indexCapFT = CurveManager::GetCurveIndex( input_data.total_cooling_capacity_function_of_temperature_curve_name );
		this->typeCapFT = CurveManager::GetCurveTypeNum( this->indexCapFT );
	}
	if ( input_data.total_cooling_capacity_function_of_air_flow_fraction_curve_name != "" ) {
		this->indexCapFFF = CurveManager::GetCurveIndex( input_data.total_cooling_capacity_function_of_air_flow_fraction_curve_name );
	}
	if ( input_data.energy_input_ratio_function_of_temperature_curve_name != "" ) {
		this->indexEIRFT = CurveManager::GetCurveIndex( input_data.energy_input_ratio_function_of_temperature_curve_name );
	}
	if ( input_data.energy_input_ratio_function_of_air_flow_fraction_curve_name != "" ) {
		this->indexEIRFFF = CurveManager::GetCurveIndex( input_data.energy_input_ratio_function_of_air_flow_fraction_curve_name );
	}
	if ( input_data.part_load_fraction_correlation_curve_name != "" ) {
		this->indexPLRFPLF = CurveManager::GetCurveIndex( input_data.part_load_fraction_correlation_curve_name );
	}
    if ( input_data.waste_heat_function_of_temperature_curve_name != "" ) {
        this->indexWHFT = CurveManager::GetCurveIndex( input_data.waste_heat_function_of_temperature_curve_name );
    }

}

CoilCoolingDXCurveFitSpeed::CoilCoolingDXCurveFitSpeed(std::string name_to_find):

	// model inputs
	TotalCapacity( 0.0 ),
	indexCapFT( 0 ),
	typeCapFT( 0 ),
	indexCapFFF( 0 ),
	indexEIRFT( 0 ),
	indexEIRFFF( 0 ),
	indexPLRFPLF( 0 ),
	indexWHFT( 0 ),
	indexWHFFF( 0 ),
	indexSHRFT( 0 ),
	indexSHRFFF( 0 ),

	// speed class inputs
	PLR( 0.0 ), // coil operating part load ratio
	CondInletTemp( 0.0 ), // condenser inlet node temp or outdoor temp if no condenser node {C}
	ambPressure( 0.0 ), // outdoor pressure {Pa]
	AirFF( 0.0 ), // ratio of air mass flow rate to rated air mass flow rate
//	RatedTotCap( 0.0 ), // rated total capacity at speed {W}
	RatedAirMassFlowRate( 0.0 ), // rated air mass flow rate at speed {kg/s}
	RatedSHR( 0.0 ), // rated sensible heat ratio at speed
	RatedCBF( 0.0 ), // rated coil bypass factor at speed
	RatedEIR( 0.0 ), // rated energy input ratio at speed {W/W}
	AirMassFlow( 0.0 ), // coil inlet air mass flow rate {kg/s}
	FanOpMode( 0 ), // fan operating mode, constant or cycling fan

	// speed class outputs
	FullLoadPower( 0.0 ), // full load power at speed {W}
	RTF( 0.0 ), // coil runtime fraction at speed

    // other data members
    rated_total_capacity(0.0),
    evap_air_flow_rate(0.0),
    condenser_air_flow_rate(0.0),
    gross_shr(0.0),
    active_fraction_of_face_coil_area(0.0),
    rated_evap_fan_power_per_volume_flow_rate(0.0),
    evap_condenser_pump_power_fraction(0.0),
    evap_condenser_effectiveness(0.0),
    rated_waste_heat_fraction_of_power_input(0.0),

	// rating data
    RatedInletAirTemp(26.6667), // 26.6667C or 80F
    RatedInletWetBulbTemp(19.44), // 19.44 or 67F
    RatedInletAirHumRat(0.01125), // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
    RatedOutdoorAirTemp(35.0), // 35 C or 95F
    DryCoilOutletHumRatioMin(0.00001) // dry coil outlet minimum hum ratio kgH2O/kgdry air

{
    int numModes = inputProcessor->getNumObjectsFound(CoilCoolingDXCurveFitSpeed::object_name);
    if (numModes <= 0) {
        // error
    }
    bool found_it = false;
    for (int modeNum = 1; modeNum <= numModes; ++modeNum) {
        int NumAlphas; // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        inputProcessor->getObjectItem(CoilCoolingDXCurveFitSpeed::object_name, modeNum, cAlphaArgs, NumAlphas,
                                      rNumericArgs, NumNumbers, IOStatus);
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
    }

    if (!found_it) {
        // error
    }
}

void CoilCoolingDXCurveFitSpeed::sizeSpeedMode() {

    std::string RoutineName = "sizeSpeedMode";

    this->rated_total_capacity = this->original_input_specs.gross_rated_total_cooling_capacity_ratio_to_nominal * parentMode->ratedGrossTotalCap;
    this->evap_air_flow_rate = this->original_input_specs.evaporator_air_flow_fraction * parentMode->ratedEvapAirFlowRate;
    this->condenser_air_flow_rate = this->original_input_specs.condenser_air_flow_fraction * parentMode->ratedCondAirFlowRate;
    this->gross_shr = this->original_input_specs.gross_rated_sensible_heat_ratio;

    this->RatedAirMassFlowRate = this->evap_air_flow_rate * Psychrometrics::PsyRhoAirFnPbTdbW( DataEnvironment::StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, RoutineName );
	this->RatedCondAirMassFlowRate = this->condenser_air_flow_rate * Psychrometrics::PsyRhoAirFnPbTdbW( DataEnvironment::StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, RoutineName );

    bool PrintFlag = true;
    int SizingMethod = DataHVACGlobals::CoolingSHRSizing;
    std::string CompType = this->object_name;
    std::string CompName = this->name;
    std::string SizingString = "Evaporator Air Flow Rate";
    DataSizing::DataFlowUsedForSizing = this->evap_air_flow_rate;
    DataSizing::DataCapacityUsedForSizing = this->rated_total_capacity;
//  DataSizing::DataEMSOverrideON = DXCoil( DXCoilNum ).RatedSHREMSOverrideOn( Mode );
//  DataSizing::DataEMSOverride = DXCoil( DXCoilNum ).RatedSHREMSOverrideValue( Mode );
    Real64 TempSize = this->gross_shr;
    ReportSizingManager::RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
    this->RatedSHR = TempSize;
    this->gross_shr = TempSize;
    DataSizing::DataFlowUsedForSizing = 0.0;
    DataSizing::DataCapacityUsedForSizing = 0.0;
//  DataSizing::DataEMSOverrideON = false;
//  DataSizing::DataEMSOverride = 0.0;

    Psychrometrics::PsychState in;
    in.tdb = RatedInletAirTemp;
    in.w = RatedInletAirHumRat;
    in.h = Psychrometrics::PsyHFnTdbW( RatedInletAirTemp, RatedInletAirHumRat );
    in.p = DataEnvironment::StdPressureSeaLevel;

    this->RatedCBF = CalcBypassFactor( in );
    this->RatedEIR = 1.0 / this->original_input_specs.gross_rated_cooling_COP;

}

Psychrometrics::PsychState CoilCoolingDXCurveFitSpeed::CalcSpeedOutput( Psychrometrics::PsychState & inletState, Real64 & PLR, Real64 & speedRatio, int & fanOpMode ) {

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName( "CalcSpeedOutput: " );

    Psychrometrics::PsychState outletState;

    if ( !DataGlobals::SysSizingCalc && this->mySizeFlag ) {
        sizeSpeedMode();
        this->mySizeFlag = false;
    }

    if ( ( PLR == 0.0 ) || ( AirMassFlow == 0.0 ) ) {
        outletState.tdb = inletState.tdb;
        outletState.w = inletState.w;
        outletState.h = inletState.h;
		outletState.p = inletState.p;
        FullLoadPower = 0.0;
        RTF = 0.0;
        return outletState;
    }

    Real64 TotCap = 0.0;
    Real64 SHR = 0.0;
    Real64 hDelta; // enthalpy difference across cooling coil
    Real64 A0 = 0.0; // ratio of UA to Cp
    Real64 CBF = 0.0; // adjusted coil bypass factor
    if ( RatedCBF > 0.0 ) {
        A0 = -std::log( RatedCBF ) * RatedAirMassFlowRate;
    } else {
        A0 = 0.0;
    }
    Real64 ADiff = -A0 / AirMassFlow;
    if ( ADiff >= DataPrecisionGlobals::EXP_LowerLimit ) {
        CBF = std::exp( ADiff );
    } else {
        CBF = 0.0;
    }

    Real64 inletWetBulb = Psychrometrics::PsyTwbFnTdbWPb( inletState.tdb, inletState.w, ambPressure );
	Real64 inletw = inletState.w;

    int Counter = 0; // iteration counter for dry coil condition
    int const MaxIter( 30 ); // iteration limit
    Real64 const Tolerance( 0.01 ); // iteration convergence limit
    Real64 RF = 0.4; // relaxation factor for holding back changes in value during iteration
    while ( true ) {

        Real64 TotCapTempModFac = 1.0;
        if ( indexCapFT > 0 ) {
            if ( typeCapFT == CurveManager::BiQuadratic ) {
                TotCapTempModFac = CurveManager::CurveValue( indexCapFT, inletWetBulb, CondInletTemp );
            } else {
                TotCapTempModFac = CurveManager::CurveValue( indexCapFT, CondInletTemp );
            }
        }
        Real64 TotCapFlowModFac = 1.0;
        if ( indexCapFFF > 0 ) {
            TotCapFlowModFac = CurveManager::CurveValue( indexCapFFF, AirFF );
        }

        TotCap = this->rated_total_capacity * TotCapFlowModFac * TotCapTempModFac;

        SHR = 0.0;
        if ( indexSHRFT > 0 ) {
            SHR = DXCoils::CalcSHRUserDefinedCurves( inletState.tdb, inletWetBulb, AirFF, indexSHRFT, indexSHRFFF, RatedSHR );
            break;
        } else {
            // Calculate apparatus dew point conditions using TotCap and CBF
            hDelta = TotCap / AirMassFlow;
            Real64 hADP = inletState.h - hDelta / ( 1.0 - CBF );
            Real64 tADP = Psychrometrics::PsyTsatFnHPb( hADP, ambPressure, RoutineName );
            Real64 wADP = Psychrometrics::PsyWFnTdbH( tADP, hADP, RoutineName );
            Real64 hTinwADP = Psychrometrics::PsyHFnTdbW( inletState.tdb, wADP );
            if ( ( inletState.h - hADP ) > 1.e-10 ) {
                SHR = min( ( hTinwADP - hADP ) / ( inletState.h - hADP ), 1.0 );
            } else {
                SHR = 1.0;
            }
            // Check for dry evaporator conditions (win < wadp)
            if ( wADP > inletw || ( Counter >= 1 && Counter < MaxIter ) ) {
            if ( inletw == 0.0 ) inletw = 0.00001;
                Real64 werror = ( inletw - wADP ) / inletw;
                // Increase InletAirHumRatTemp at constant InletAirTemp to find coil dry-out point. Then use the
                // capacity at the dry-out point to determine exiting conditions from coil. This is required
                // since the TotCapTempModFac doesn't work properly with dry-coil conditions.
				inletw = RF * wADP + ( 1.0 - RF ) * inletw;
				inletWetBulb = Psychrometrics::PsyTwbFnTdbWPb( inletState.tdb, inletw, ambPressure );
                ++Counter;
                if ( std::abs( werror ) > Tolerance ) continue; // Recalculate with modified inlet conditions
                break;
            } else {
                break;
            }
        }
    }

    Real64 PLF = 1.0; // part load factor as a function of PLR, RTF = PLR / PLF
    if ( indexPLRFPLF > 0 ) {
        PLF = CurveManager::CurveValue( indexPLRFPLF, PLR ); // Calculate part-load factor
    }
    if ( fanOpMode == DataHVACGlobals::CycFanCycCoil ) DataHVACGlobals::OnOffFanPartLoadFraction = PLF;

    Real64 EIRTempModFac = 1.0; // EIR as a function of temperature curve result
    if ( indexEIRFT > 0 ) {
        EIRTempModFac = CurveManager::CurveValue( indexEIRFT, inletWetBulb, CondInletTemp );
    }
    Real64 EIRFlowModFac = 1.0; // EIR as a function of flow fraction curve result
    if ( indexEIRFFF > 0 ) {
        EIRFlowModFac = CurveManager::CurveValue( indexEIRFFF, AirFF );
    }

    Real64 EIR = RatedEIR * EIRFlowModFac * EIRTempModFac;
    RTF = PLR / PLF;
    FullLoadPower = TotCap * EIR;

    outletState.h = inletState.h - ( TotCap / AirMassFlow );
    Real64 hTinwout = inletState.h - ( ( 1.0 - SHR ) * hDelta );
    outletState.w = Psychrometrics::PsyWFnTdbH( inletState.tdb, hTinwout );
    outletState.tdb = Psychrometrics::PsyTdbFnHW( outletState.h, outletState.w );

    return outletState;

}

Real64 CoilCoolingDXCurveFitSpeed::CalcBypassFactor(Psychrometrics::PsychState & in) {

  // Bypass factors are calculated at rated conditions at sea level (make sure in.p is Standard Pressure)
  Real64 calcCBF = 0.0;

  // Outlet conditions
  Psychrometrics::PsychState out;

  Real64 airMassFlowRate = evap_air_flow_rate * Psychrometrics::PsyRhoAirFnPbTdbW( in.p, in.tdb, in.w);
  Real64 deltaH = rated_total_capacity / airMassFlowRate;
  out.p = in.p;
  out.h = in.h - deltaH;
  out.w = Psychrometrics::PsyWFnTdbH(in.tdb, in.h - (1.0 - this->gross_shr)*deltaH); // enthalpy at Tdb,in and Wout
  out.tdb = Psychrometrics::PsyTdbFnHW(out.h, out.w);
  out.rh = Psychrometrics::PsyRhFnTdbWPb(out.tdb, out.w, out.p);

  // ADP conditions
  Psychrometrics::PsychState adp;
  adp.tdb = Psychrometrics::PsyTdpFnWPb( out.w, out.p );

  Real64 tol = 1.0;
  std::size_t iter = 0;
  const std::size_t maxIter(50);
  Real64 errorLast = 100.0;
  Real64 deltaADPTemp = 5.0;
  Real64 tolerance = 1.0; // initial conditions for iteration
  Real64 ADPHumRat = 0.0;
  Real64 slopeAtConds = 0.0;
  Real64 deltaT = in.tdb - out.tdb;
  Real64 deltaHumRat = in.w - out.w;

  if ( deltaT > 0.0 ) slopeAtConds = deltaHumRat / deltaT;

  while ( ( iter <= maxIter ) && ( tolerance > 0.001 ) ) {

    // Do for IterMax iterations or until the error gets below .1%
    if ( iter > 0 ) adp.tdb += deltaADPTemp;
      ++iter;
      //  Find new slope using guessed Tadp
      ADPHumRat = min( out.w, Psychrometrics::PsyWFnTdpPb( adp.tdb, DataEnvironment::StdPressureSeaLevel ) );
      Real64 slope = ( in.w - ADPHumRat ) / max( 0.001, ( in.tdb - adp.tdb ) );
      //  check for convergence (slopes are equal to within error tolerance)
      Real64 error = ( slope - slopeAtConds ) / slopeAtConds;
      if ( ( error > 0.0 ) && ( errorLast < 0.0 ) ) {
        deltaADPTemp = -deltaADPTemp / 2.0;
      } else if ( ( error < 0.0 ) && ( errorLast > 0.0 ) ) {
        deltaADPTemp = -deltaADPTemp / 2.0;
      } else if ( abs( error ) > abs( errorLast ) ) {
        deltaADPTemp = -deltaADPTemp / 2.0;
      }
      errorLast = error;
      tolerance = std::abs( error );
  }

  //   Calculate Bypass Factor from Enthalpies
  Real64 ADPEnthalpy = Psychrometrics::PsyHFnTdbW( adp.tdb, ADPHumRat );
  calcCBF = min( 1.0, ( out.h - ADPEnthalpy ) / ( in.h - ADPEnthalpy ) );

  return calcCBF;

}
