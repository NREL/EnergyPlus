#include <Coils/CoilCoolingDXCurveFitSpeed.hh>
#include <CurveManager.hh>
#include <DataHVACGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DXCoils.hh>
#include <Psychrometrics.hh>

using namespace EnergyPlus;

CoilCoolingDXCurveFitSpeedInputSpecification::CoilCoolingDXCurveFitSpeedInputSpecification() {}

CoilCoolingDXCurveFitSpeed::CoilCoolingDXCurveFitSpeed() {}

void CoilCoolingDXCurveFitSpeed::CalcSpeedOutput() {

	// SUBROUTINE PARAMETER DEFINITIONS:
	static std::string const RoutineName( "CalcSpeedOutput: " );
	static int const MaxIter( 30 );
	static Real64 const Tolerance( 0.01 );

	// Required information
	Real64 PLR( 0.0 );
	Real64 coilInletT( 0.0 ); // coil inlet temperature {C}
	Real64 coilInletW( 0.0 ); // coil inlet humidity ratio {kg/kg}
	Real64 coilInletWB( 0.0 ); // coil inlet wet-bulb temperature {C}
	Real64 coilInletH( 0.0 ); // coil inlet enthalpy {J/kg}
	Real64 CondInletTemp( 0.0 ); // condenser inlet node temp or outdoor temp if no condenser node {C}
	Real64 OAP( 0.0 ); // outdoor pressure {Pa]
	Real64 AirFF( 1.0 ); // ratio of air mass flow rate to rated air mass flow rate
	Real64 RatedTotCap( 0.0 ); // rated total capacity at speed {W}
	Real64 RatedAirMassFlowRate( 0.0 ); // rated air mass flow rate at speed {kg/s}
	Real64 RatedSHR( 0.0 ); // rated sensible heat ratio at speed
	Real64 RatedCBF( 0.0 ); // rated coil bypass factor at speed
	Real64 RatedEIR( 0.0 ); // rated energy input ratio at speed {W/W}
	Real64 AirMassFlow( 0.0 ); // coil inlet air mass flow rate {kg/s}
	int FanOpMode( 0 ); // fan operating mode, constant or cycling fan

	// local variables
	Real64 FullLoadOutAirTemp; // full load outlet air temperature {C}
	Real64 FullLoadOutAirHumRat; // full load outlet air humidity ratio {kg/kg}
	Real64 FullLoadOutAirEnth; // full load outlet air enthalpy {J/kg}

	// evaluate CapFT curve, can be any curve type, BiQuad, Cubic, etc. How to get inputs to Curve objects?

	// may need to use heat exchanger calculation of bypass factor at off design air mass flow rate here? (i.e., DXCoils line 7452)
	
	if ( PLR == 0.0 ) {
		FullLoadOutAirTemp = coilInletT;
		FullLoadOutAirHumRat = coilInletW;
		FullLoadOutAirEnth = coilInletH;
		return;
	}
	
	Real64 hDelta; // enthalpy difference across cooling coil
	Real64 A0 = 0.0;
	Real64 CBF = 0.0;
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

	int Counter = 0;
	Real64 RF = 0.4; // relaxation factor for holding back changes in value during iteration 
	while ( true ) {

		Real64 TotCapTempModFac = CurveManager::CurveValue( indexCapFT, coilInletT, CondInletTemp );
		Real64 TotCapFlowModFac = CurveManager::CurveValue( indexCapFFF, AirFF );
		Real64 TotCap = RatedTotCap * TotCapFlowModFac * TotCapTempModFac;

		Real64 SHR = 0.0;
		if ( indexSHRFT > 0 ) {
			SHR = DXCoils::CalcSHRUserDefinedCurves( coilInletT, coilInletWB, AirFF, indexSHRFT, indexSHRFFF, RatedSHR );
			break;
		} else {
			// Calculate apparatus dew point conditions using TotCap and CBF
			hDelta = TotCap / AirMassFlow;
			Real64 hADP = coilInletH - hDelta / ( 1.0 - CBF );
			Real64 tADP = Psychrometrics::PsyTsatFnHPb( hADP, OAP, RoutineName );
			//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			//  tADP = PsyTsatFnHPb(hADP,InletAirPressure)
			Real64 wADP = Psychrometrics::PsyWFnTdbH( tADP, hADP, RoutineName );
			Real64 hTinwADP = Psychrometrics::PsyHFnTdbW( coilInletT, wADP );
			if ( ( coilInletH - hADP ) > 1.e-10 ) {
				SHR = min( ( hTinwADP - hADP ) / ( coilInletH - hADP ), 1.0 );
			} else {
				SHR = 1.0;
			}
			// Check for dry evaporator conditions (win < wadp)
			if ( wADP > coilInletW || ( Counter >= 1 && Counter < MaxIter ) ) {
				if ( coilInletW == 0.0 ) coilInletW = 0.00001;
				Real64 werror = ( coilInletW - wADP ) / coilInletW;
				// Increase InletAirHumRatTemp at constant InletAirTemp to find coil dry-out point. Then use the
				// capacity at the dry-out point to determine exiting conditions from coil. This is required
				// since the TotCapTempModFac doesn't work properly with dry-coil conditions.
				coilInletW = RF * wADP + ( 1.0 - RF ) * coilInletW;
				coilInletWB = Psychrometrics::PsyTwbFnTdbWPb( coilInletT, coilInletW, OAP );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  InletAirWetBulbC = PsyTwbFnTdbWPb(InletAirDryBulbTemp,InletAirHumRatTemp,InletAirPressure)
				++Counter;
				if ( std::abs( werror ) > Tolerance ) continue; // Recalculate with modified inlet conditions
				break;
			} else {
				break;
			}
		}

		Real64 PLF = 1.0;
		if ( indexPLRFPLF > 0 ) {
			PLF = CurveManager::CurveValue( indexPLRFPLF, PLR ); // Calculate part-load factor
		}
		if ( FanOpMode == DataHVACGlobals::CycFanCycCoil ) DataHVACGlobals::OnOffFanPartLoadFraction = PLF;

		Real64 EIRTempModFac = 1.0; // EIR as a function of temperature curve result
		if ( indexEIRFT > 0 ) {
			EIRTempModFac = CurveManager::CurveValue( indexEIRFT, coilInletWB, CondInletTemp );
		}
		Real64 EIRFlowModFac = 1.0; // EIR as a function of flow fraction curve result
		if ( indexEIRFFF > 0 ) {
			EIRFlowModFac = CurveManager::CurveValue( indexEIRFFF, AirFF );
		}

		Real64 EIR = RatedEIR * EIRFlowModFac * EIRTempModFac;

		FullLoadOutAirEnth = coilInletH - ( TotCap / AirMassFlow );
		Real64 hTinwout = coilInletH - ( ( 1.0 - SHR ) * hDelta );
		FullLoadOutAirHumRat = Psychrometrics::PsyWFnTdbH( coilInletT, hTinwout );
		FullLoadOutAirTemp = Psychrometrics::PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );

	}

}
