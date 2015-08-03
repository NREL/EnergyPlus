// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataAirflowNetwork.hh>
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataAirflowNetwork;
using namespace EnergyPlus::AirflowNetworkBalanceManager;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataEnvironment;
using DataHeatBalance::Zone;
using DataHeatBalFanSys::MAT;
using DataHeatBalance::MRT;
using namespace CurveManager;
using DataHeatBalance::ZoneIntGain;
using DataHeatBalFanSys::TempControlType;
using DataHeatBalFanSys::ZoneThermostatSetPointLo;
using DataHeatBalFanSys::ZoneThermostatSetPointHi;

TEST( AdvancedAFNTest, Test1 )
{

	ShowMessage( "Begin Test: AdvancedAFNTest, Test1" );

	int AirflowNetworkNumOfOccuVentCtrls;
	Real64 TimeOpenElapsed; 
	Real64 TimeCloseElapsed;
	int OpenStatus;
	int OpenProbStatus;
	int CloseProbStatus;
	int CurveNum;

	AirflowNetworkNumOfOccuVentCtrls = 1;
	OccupantVentilationControl.allocate( AirflowNetworkNumOfOccuVentCtrls );
	OccupantVentilationControl( 1 ).MinOpeningTime = 4;
	OccupantVentilationControl( 1 ).MinClosingTime = 4;
	OccupantVentilationControl( 1 ).MinTimeControlOnly = true;

	TimeOpenElapsed = 3.0;
	TimeCloseElapsed = 0.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 1, OpenStatus );

	TimeOpenElapsed = 5.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 0, OpenStatus );

	TimeOpenElapsed = 0.0;
	TimeCloseElapsed = 3.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 2, OpenStatus );

	TimeOpenElapsed = 0.0;
	TimeCloseElapsed = 5.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 0, OpenStatus );

	OutDryBulbTemp = 15.0;
	Zone.allocate( 1 );
	MAT.allocate( 1 );
	MRT.allocate( 1 );
	MAT( 1 ) = 22.0;
	MRT( 1 ) = 22.0;

	TimeOpenElapsed = 5.0;
	TimeCloseElapsed = 0.0;
	OccupantVentilationControl( 1 ).MinTimeControlOnly = false;
	OccupantVentilationControl( 1 ).ComfortBouPoint = 10.0;
	OccupantVentilationControl( 1 ).ComfortLowTempCurveNum = 1;
	OccupantVentilationControl( 1 ).ComfortHighTempCurveNum = 2;

	NumCurves = 2;
	PerfCurve.allocate( NumCurves );

	CurveNum = 1;
	PerfCurve( CurveNum ).CurveType = Quadratic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 21.2;
	PerfCurve( CurveNum ).Coeff2 = 0.09;
	PerfCurve( CurveNum ).Coeff3 = 0.0;
	PerfCurve( CurveNum ).Coeff4 = 0.0;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = -50.0;
	PerfCurve( CurveNum ).Var1Max = 10.0;
	PerfCurve( CurveNum ).Var2Min = 0.0;
	PerfCurve( CurveNum ).Var2Max = 2.0;

	CurveNum = 2;
	PerfCurve( CurveNum ).CurveType = Quadratic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 18.8;
	PerfCurve( CurveNum ).Coeff2 = 0.33;
	PerfCurve( CurveNum ).Coeff3 = 0.0;
	PerfCurve( CurveNum ).Coeff4 = 0.0;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = 10.0;
	PerfCurve( CurveNum ).Var1Max = 50.0;
	PerfCurve( CurveNum ).Var2Min = 0.0;
	PerfCurve( CurveNum ).Var2Max = 2.0;

	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 0, OpenProbStatus );
	EXPECT_EQ( 1, CloseProbStatus );

	MAT( 1 ) = 26.0;
	MRT( 1 ) = 26.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 2, OpenProbStatus );
	EXPECT_EQ( 0, CloseProbStatus );

	TimeOpenElapsed = 0.0;
	TimeCloseElapsed = 5.0;
	ZoneIntGain.allocate( 1 );
	ZoneIntGain( 1 ).NOFOCC = 0.5;
	TempControlType.allocate( 1 );
	TempControlType( 1 ) = 0;
	ZoneThermostatSetPointLo.allocate( 1 );
	ZoneThermostatSetPointHi.allocate( 1 );

	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 1, OpenProbStatus );
	EXPECT_EQ( 0, CloseProbStatus );

	TempControlType( 1 ) = 4;
	ZoneThermostatSetPointLo( 1 ) = 22.0;
	ZoneThermostatSetPointHi( 1 ) = 28.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 1, OpenProbStatus );
	EXPECT_EQ( 0, CloseProbStatus );

}
