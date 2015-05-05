// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/ManageElectricPower.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::ManageElectricPower;
using namespace EnergyPlus::CurveManager;
using namespace ObjexxFCL;
using namespace DataGlobals;

TEST( ManageElectricPowerTest, BatteryDischargeTest )
{
	ShowMessage( "Begin Test: ManageElectricPowerTest, BatteryDischargeTest" );

	NumCurves = 1;
	PerfCurve.allocate( NumCurves );
	PerfCurve( 1 ).CurveType = CurveType_RectangularHyperbola1;
	PerfCurve( 1 ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( 1 ).Coeff1 = 0.0899;
	PerfCurve( 1 ).Coeff2 = -98.24;
	PerfCurve( 1 ).Coeff3 = -.0082;
	int CurveNum1 = 1;
	Real64 k = 0.5874;
	Real64 c = 0.37;
	Real64 qmax = 86.1;
	Real64 E0c = 12.6;
	Real64 InternalR = 0.054;

	Real64 I0 = 0.159;
	Real64 T0 = 537.9;
	Real64 Volt = 12.59;
	Real64 Pw = 2.0;
	Real64 q0 = 60.2;

	EXPECT_TRUE( determineCurrentForBatteryDischarge( I0, T0, Volt, Pw, q0, CurveNum1, k, c, qmax, E0c, InternalR ) );

	I0 = -222.7;
	T0 = -0.145;
	Volt = 24.54;
	Pw = 48000;
	q0 = 0;

	EXPECT_FALSE( determineCurrentForBatteryDischarge( I0, T0, Volt, Pw, q0, CurveNum1, k, c, qmax, E0c, InternalR ) );

	PerfCurve.deallocate();
}
