// EnergyPlus::Evaporative Cooler Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/Psychrometrics.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::CurveManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::Psychrometrics;

// This could almost definitely benefit from some improvements such as a fixture to only do init once,
// but I've never used those and am just interested in stubbing this out for now

TEST( EvaporativeCoolers, CalcSecondaryAirOutletCondition )
{

	ShowMessage( "Begin Test: EvaporativeCoolers, CalcSecondaryAirOutletCondition" );

	EvaporativeCoolers::EvapCond.allocate( 1 );
	int const EvapCoolNum( 1 );
	EvaporativeCoolers::EvapCond( EvapCoolNum ).SecInletEnthalpy = 42000.0;

	// set up arguments
	int OperatingMode( EvaporativeCoolers::None );
	Real64 AirMassFlowSec( 0.0 );
	Real64 const EDBTSec( 20.0 );
	Real64 const EWBTSec( 15.0 );
	Real64 const EHumRatSec( 0.0085 );
	Real64 QHXTotal( 0.0 );
	Real64 QHXLatent( 0.0 );

	// make the call for zero secondary air flow rate
	EvaporativeCoolers::CalcSecondaryAirOutletCondition(
		EvapCoolNum,
		OperatingMode,
		AirMassFlowSec,
		EDBTSec,
		EWBTSec,
		EHumRatSec,
		QHXTotal,
		QHXLatent
	);

	// check outputs for evap cooler set off
	EXPECT_DOUBLE_EQ( EvaporativeCoolers::EvapCond( EvapCoolNum ).SecOutletEnthalpy, EvaporativeCoolers::EvapCond( EvapCoolNum ).SecInletEnthalpy );
	EXPECT_DOUBLE_EQ( 0.0, QHXLatent );


	// dry operating mode and non zero secondary air flow rate
	OperatingMode = EvaporativeCoolers::DryFull;
	AirMassFlowSec = 2.0;
	QHXTotal = 10206.410750000941;

	InitializePsychRoutines();

	// make the call for dry operating mode
	EvaporativeCoolers::CalcSecondaryAirOutletCondition(
		EvapCoolNum,
		OperatingMode,
		AirMassFlowSec,
		EDBTSec,
		EWBTSec,
		EHumRatSec,
		QHXTotal,
		QHXLatent
		);

	// check outputs for dry operating condition
	EXPECT_DOUBLE_EQ( 25.0, EvaporativeCoolers::EvapCond( EvapCoolNum ).SecOutletTemp );
	EXPECT_DOUBLE_EQ( 0.0, QHXLatent );


	// wet operating mode and non zero secondary air flow rate
	OperatingMode = EvaporativeCoolers::WetFull;
	AirMassFlowSec = 2.0;
	QHXTotal = 10206.410750000941;

	// make the call for wet operating condition
	EvaporativeCoolers::CalcSecondaryAirOutletCondition(
		EvapCoolNum,
		OperatingMode,
		AirMassFlowSec,
		EDBTSec,
		EWBTSec,
		EHumRatSec,
		QHXTotal,
		QHXLatent
		);

	// check outputs for wet operating condition
	EXPECT_DOUBLE_EQ( 20.0, EvaporativeCoolers::EvapCond( EvapCoolNum ).SecOutletTemp );
	EXPECT_DOUBLE_EQ( 47103.205375000471, EvaporativeCoolers::EvapCond( EvapCoolNum ).SecOutletEnthalpy );
	EXPECT_DOUBLE_EQ( QHXTotal, QHXLatent );

	EvaporativeCoolers::EvapCond.deallocate();

}

TEST( EvaporativeCoolers, CalcIndirectRDDEvapCoolerOutletTemp )
{

	ShowMessage( "Begin Test: EvaporativeCoolers, CalcIndirectRDDEvapCoolerOutletTemp" );

	OutBaroPress = 101325.0;
	EvaporativeCoolers::EvapCond.allocate( 1 );
	int const EvapCoolNum( 1 );
	EvaporativeCoolers::EvapCond( EvapCoolNum ).InletMassFlowRate = 1.0;
	EvaporativeCoolers::EvapCond( EvapCoolNum ).InletTemp = 24.0;
	EvaporativeCoolers::EvapCond( EvapCoolNum ).InletHumRat = 0.013;
	EvaporativeCoolers::EvapCond( EvapCoolNum ).DryCoilMaxEfficiency = 0.8;

	// set up arguments
	int DryOrWetOperatingMode( EvaporativeCoolers::DryFull );
	Real64 const AirMassFlowSec( 1.0 );
	Real64 const EDBTSec( 14.0 );
	Real64 const EWBTSec( 11.0 );
	Real64 const EHumRatSec( 0.0075 );

	// testing full capacity in dry operating mode
	EvaporativeCoolers::CalcIndirectRDDEvapCoolerOutletTemp(
	EvapCoolNum,
	DryOrWetOperatingMode,
	AirMassFlowSec,
	EDBTSec,
	EWBTSec,
	EHumRatSec );

	EXPECT_DOUBLE_EQ( 16.0, EvaporativeCoolers::EvapCond( EvapCoolNum ).OutletTemp );

	// testing full capacity in wet operating mode
	DryOrWetOperatingMode = EvaporativeCoolers::WetFull;
	EvaporativeCoolers::EvapCond( EvapCoolNum ).WetCoilMaxEfficiency = 0.75;

	EvaporativeCoolers::CalcIndirectRDDEvapCoolerOutletTemp(
		EvapCoolNum,
		DryOrWetOperatingMode,
		AirMassFlowSec,
		EDBTSec,
		EWBTSec,
		EHumRatSec );

	EXPECT_DOUBLE_EQ( 14.25, EvaporativeCoolers::EvapCond( EvapCoolNum ).OutletTemp );

	EvaporativeCoolers::EvapCond.deallocate();

}

TEST( EvaporativeCoolers, IndEvapCoolerPower )
{

	ShowMessage( "Begin Test: EvaporativeCoolers, IndEvapCoolerPower" );

	using CurveManager::Quadratic;

	int CurveNum;

	EvaporativeCoolers::EvapCond.allocate( 1 );
	int const EvapCoolNum( 1 );
	EvaporativeCoolers::EvapCond( EvapCoolNum ).IndirectFanPower = 200.0;
	EvaporativeCoolers::EvapCond( EvapCoolNum ).IndirectRecircPumpPower = 100.0;

	// set up arguments
	int DryWetMode( EvaporativeCoolers::DryFull );
	Real64 FlowRatio( 1.0 );

	CurveNum = 1;
	EvaporativeCoolers::EvapCond( EvapCoolNum ).FanPowerModifierCurveIndex = CurveNum;

	NumCurves = 1;
	PerfCurve.allocate( 1 );
	PerfCurve( CurveNum ).CurveType = Quadratic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 0.0;
	PerfCurve( CurveNum ).Coeff2 = 1.0;
	PerfCurve( CurveNum ).Coeff3 = 0.0;
	PerfCurve( CurveNum ).Coeff4 = 0.0;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = 0.0;
	PerfCurve( CurveNum ).Var1Max = 1.0;
	PerfCurve( CurveNum ).Var2Min = 0;
	PerfCurve( CurveNum ).Var2Max = 0;

	// make the call for dry full load operating condition
	EvaporativeCoolers::EvapCond( EvapCoolNum ).EvapCoolerPower = EvaporativeCoolers::IndEvapCoolerPower(
		EvapCoolNum,
		DryWetMode,
		FlowRatio );

	// check outputs for dry full load operating condition
	EXPECT_EQ( 200.0, EvaporativeCoolers::EvapCond( EvapCoolNum ).EvapCoolerPower );

	// set up arguments for wet modulated operating condition
	DryWetMode = EvaporativeCoolers::WetModulated;
	FlowRatio = 0.5;
	EvaporativeCoolers::EvapCond( EvapCoolNum ).PartLoadFract = 0.5;

	// make the call for wet modulated operating condition
	EvaporativeCoolers::EvapCond( EvapCoolNum ).EvapCoolerPower = EvaporativeCoolers::IndEvapCoolerPower(
	EvapCoolNum,
	DryWetMode,
	FlowRatio );

	// check outputs for wet modulated operating condition
	EXPECT_EQ( 150.0, EvaporativeCoolers::EvapCond( EvapCoolNum ).EvapCoolerPower );

	EvaporativeCoolers::EvapCond.deallocate();
	PerfCurve.deallocate();
}
