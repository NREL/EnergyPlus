// EnergyPlus::DXCoils unit tests
// DX heating coil defrost capacity with electric resistance

// Google test headers
#include <gtest/gtest.h>

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <CurveManager.hh>
#include <Fans.hh>
#include <FaultsManager.hh>

using namespace EnergyPlus;
using namespace FaultsManager;
using namespace CurveManager;
using namespace Fans;

TEST( FaultFoulingAirFilters, CheckFaultyAirFilterFanCurve )
{
	// PURPOSE OF THIS SUBROUTINE:
	// To check whether the fan curve specified in the FaultModel:Fouling:AirFilter object
	// covers the rated operational point of the corresponding fan 
	// Return true if the curve covers the fan rated operational point
		
	int CurveNum;
	int FanNum;
	bool TestRestult; 
	
	// Allocate
	NumCurves = 1;
	PerfCurve.allocate( NumCurves );
	
	NumFans = 2;
	Fan.allocate( NumFans );
		
	// Inputs: fan curve
	CurveNum = 1;
	PerfCurve( CurveNum ).CurveType = Cubic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Cubic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 1151.1;
	PerfCurve( CurveNum ).Coeff2 = 13.509;
	PerfCurve( CurveNum ).Coeff3 = -0.9105;
	PerfCurve( CurveNum ).Coeff4 = -0.0129;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = 7.0;
	PerfCurve( CurveNum ).Var1Max = 21.0;
	
	// Inputs: fans
	FanNum = 1;
	Fan( FanNum ).FanName = "Fan_1";
	Fan( FanNum ).FanType = "Fan:VariableVolume";
	Fan( FanNum ).MaxAirFlowRate = 18.194;
	Fan( FanNum ).DeltaPress = 1017.59;
	
	FanNum = 2;
	Fan( FanNum ).FanName = "Fan_2";
	Fan( FanNum ).FanType = "Fan:VariableVolume";
	Fan( FanNum ).MaxAirFlowRate = 18.194;
	Fan( FanNum ).DeltaPress = 1017.59 * 1.2;
	
	// Run and Check
	// (1)The rated operational point of Fan_1 falls on the fan curve
	TestRestult = CheckFaultyAirFilterFanCurve( "Fan_1", CurveNum );
	EXPECT_TRUE( TestRestult ); 
	// (2)The rated operational point of Fan_2 does not fall on the fan curve
	TestRestult = CheckFaultyAirFilterFanCurve( "Fan_2", CurveNum );
	EXPECT_FALSE( TestRestult ); 
	
	// Clean up
	PerfCurve.deallocate( );
	Fan.deallocate( ); 

}

TEST( FaultFoulingAirFilters, CalFaultyFanAirFlowReduction )
{
	// PURPOSE OF THIS SUBROUTINE:
	// Calculate the decrease of the fan air flow rate, given the fan curve 
	// and the increase of fan pressure rise due to fouling air filters
		
	int CurveNum;
	int FanNum;
	double FanDesignFlowRateDec;
	double FanFaultyDeltaPressInc = 0.10; // Increase by 10%
	
	// Allocate
	NumCurves = 1;
	PerfCurve.allocate( NumCurves );
	
	NumFans = 1;
	Fan.allocate( NumFans );
		
	// Inputs: fan curve
	CurveNum = 1;
	PerfCurve( CurveNum ).CurveType = Cubic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Cubic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 1151.1;
	PerfCurve( CurveNum ).Coeff2 = 13.509;
	PerfCurve( CurveNum ).Coeff3 = -0.9105;
	PerfCurve( CurveNum ).Coeff4 = -0.0129;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = 7.0;
	PerfCurve( CurveNum ).Var1Max = 21.0;
	
	// Inputs: fans
	FanNum = 1;
	Fan( FanNum ).FanName = "Fan_1";
	Fan( FanNum ).FanType = "Fan:VariableVolume";
	Fan( FanNum ).MaxAirFlowRate = 18.194;
	Fan( FanNum ).DeltaPress = 1017.59;
	
	// Run and Check
    FanDesignFlowRateDec = CalFaultyFanAirFlowReduction( Fan( FanNum ).FanName, Fan( FanNum ).MaxAirFlowRate, Fan( FanNum ).DeltaPress, 
                           FanFaultyDeltaPressInc * Fan( FanNum ).DeltaPress, CurveNum );
							
	EXPECT_NEAR( 3.845, FanDesignFlowRateDec, 0.005 );
	
	// Clean up
	PerfCurve.deallocate( );
	Fan.deallocate( ); 
	
}

