// EnergyPlus::HVACVariableRefrigerantFlow unit tests

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
#include <HVACVariableRefrigerantFlow.hh>

using namespace EnergyPlus;
using namespace HVACVariableRefrigerantFlow;
using namespace CurveManager;

TEST( HVACVariableRefrigerantFlow, VRF_FluidTCtrl_CalcVRF )
{
	// PURPOSE OF THIS SUBROUTINE:
	// 		Subroutine CalcVRF_FluidTCtrl is part of the new VRF model based on physics, appliable for Fluid Temperature Control.
	// 		This subroutine simulates the components making up the VRF indoor terminal unit.

}

TEST( HVACVariableRefrigerantFlow, VRF_FluidTCtrl_CompResidual )
{
	// PURPOSE OF THIS SUBROUTINE:
	//  Calculates residual function ((VRV terminal unit cooling output - Zone sensible cooling load)
		
	using namespace CurveManager;

	int CurveNum = 1;
	int NumPar;
	double Te = -2.796; // Outdoor unit evaporating temperature
	double Tdis = 40.093; 
	double CondHeat = 1864.44;
	double FanDesignFlowRateDec;
	double FanFaultyDeltaPressInc = 0.10; // Increase by 10%
	Array1D< Real64 > Par;
	
	// Allocate
	NumCurves = 1; //CurveManager::NumCurves
	PerfCurve.allocate( NumCurves );
	
	NumPar = 3;
	Par.allocate( NumPar );
		
	// Inputs: curve parameters
	Par( 1 ) = Tdis;
	Par( 2 ) = CondHeat;
	Par( 3 ) = CurveNum;
		
	// Inputs: parameters
	PerfCurve( CurveNum ).CurveType = CurveManager::BiQuadratic;
	PerfCurve( CurveNum ).ObjectType = CurveType_BiQuadratic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 724.71125 ; // Coefficient1 Constant
	PerfCurve( CurveNum ).Coeff2 = -21.867868; // Coefficient2 x
	PerfCurve( CurveNum ).Coeff3 = 0.52480042; // Coefficient3 x**2
	PerfCurve( CurveNum ).Coeff4 = -17.043566; // Coefficient4 y
	PerfCurve( CurveNum ).Coeff5 = -.40346383; // Coefficient5 y**2
	PerfCurve( CurveNum ).Coeff6 = 0.29573589; // Coefficient6 x*y
	PerfCurve( CurveNum ).Var1Min= 15 ; // Minimum Value of x
	PerfCurve( CurveNum ).Var1Max= 65 ; // Maximum Value of x
	PerfCurve( CurveNum ).Var2Min= -30; // Minimum Value of y
	PerfCurve( CurveNum ).Var2Max= 15 ; // Maximum Value of y

	// Run and Check
    double CompResidual = HVACVariableRefrigerantFlow::CompResidual( Te, Par );
	EXPECT_NEAR( 1.652, CompResidual, 0.005 );
	
	// Clean up
	PerfCurve.deallocate( );
	Par.deallocate( ); 
	
}
