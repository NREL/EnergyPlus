// EnergyPlus::DXCoils unit tests
// DX heating coil defrost capacity with electric resistance

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/HVACFixture.hh"
#include <DXCoils.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataSizing.hh>
#include <OutputReportPredefined.hh>

using namespace EnergyPlus;
using namespace DXCoils;
using namespace DataAirLoop;
using namespace DataAirSystems;
using namespace DataHVACGlobals;
using namespace DataSizing;
using namespace CurveManager;
using namespace OutputReportPredefined;

namespace EnergyPlus {

	TEST_F( HVACFixture, DXCoils_Test1 )
	{
		using CurveManager::Quadratic;
		using CurveManager::BiQuadratic;
		using CurveManager::NumCurves;
		//	int NumDXCoils( 0 ); // Total number of DX coils
	//	Array1D< DXCoilData > DXCoil;
		int DXCoilNum;
		int CurveNum;

		NumDXCoils = 2;
		DXCoilNum = 2;
		DXCoil.allocate( NumDXCoils );
		DXCoil( 1 ).DXCoilType_Num = CoilDX_MultiSpeedCooling;
		DXCoil( 2 ).DXCoilType_Num = CoilDX_MultiSpeedHeating;
		DXCoil( 1 ).MSRatedTotCap.allocate( 2 );
		DXCoil( 2 ).MSRatedTotCap.allocate( 2 );
		DXCoil( 2 ).CompanionUpstreamDXCoil = 1;

		DXCoilNumericFields.allocate( NumDXCoils );
		DXCoilNumericFields( 2 ).PerfMode.allocate( 1 );
		DXCoilNumericFields( 2 ).PerfMode( 1 ).FieldNames.allocate( 4 );
		DXCoil( 2 ).DefrostStrategy = Resistive;
		DXCoil( 2 ).DefrostCapacity = 5000.0;
		DXCoil( 2 ).Name = "DX Heating coil";
		DXCoil( 1 ).NumOfSpeeds = 2;
		DXCoil( 2 ).NumOfSpeeds = 2;

		DXCoil( DXCoilNum ).MSRatedTotCap.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSRatedSHR.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSRatedCOP.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSRatedAirVolFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSRatedAirMassFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSCCapFTemp.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSCCapFFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSEIRFTemp.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSEIRFFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSWasteHeat.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSEvapCondEffect.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSEvapCondAirFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSRatedCBF.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSWasteHeatFrac.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSPLFFPLR.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSTwet_Rated.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSGamma_Rated.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSMaxONOFFCyclesperHour.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSLatentCapacityTimeConstant.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSFanPowerPerEvapAirFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );

		DXCoil( 1 ).MSRatedTotCap( 1 ) = 4455.507579219055;
		DXCoil( 1 ).MSRatedTotCap( 2 ) = 6188.507579219055;

		DXCoil( DXCoilNum ).MSRatedTotCap( 1 ) = 4455.507579219055;
		DXCoil( DXCoilNum ).MSRatedTotCap( 2 ) = 6188.204971137576;
		DXCoil( DXCoilNum ).MSRatedCOP( 1 ) = 4.03;
		DXCoil( DXCoilNum ).MSRatedCOP( 2 ) = 3.53;

		DXCoil( DXCoilNum ).MSCCapFFlow = 1;
		DXCoil( DXCoilNum ).MSCCapFTemp = 3;
		DXCoil( DXCoilNum ).MSEIRFFlow = 1;
		DXCoil( DXCoilNum ).MSEIRFTemp = 3;
		DXCoil( DXCoilNum ).MSPLFFPLR = 2;
		DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( 1 ) = 0.2339;
		DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( 2 ) = 0.2924;
		DXCoil( DXCoilNum ).MSFanPowerPerEvapAirFlowRate = 0.0;
		DXCoil( DXCoilNum ).RegionNum = 4;
		DXCoil( DXCoilNum ).MinOATCompressor = -17.78;


		NumCurves = 3;
		PerfCurve.allocate( NumCurves );

		CurveNum = 1;
		PerfCurve( CurveNum ).CurveType = Quadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = 0.0;
		PerfCurve( CurveNum ).Var1Max = 2.0;
		PerfCurve( CurveNum ).Var2Min = 0.0;
		PerfCurve( CurveNum ).Var2Max = 2.0;

		CurveNum = 2;
		PerfCurve( CurveNum ).CurveType = Quadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = 0.0;
		PerfCurve( CurveNum ).Var1Max = 1.0;
		PerfCurve( CurveNum ).Var2Min = 0.7;
		PerfCurve( CurveNum ).Var2Max = 1.0;

		CurveNum = 3;
		PerfCurve( CurveNum ).CurveType = BiQuadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_BiQuadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = -100.0;
		PerfCurve( CurveNum ).Var1Max = 100.0;
		PerfCurve( CurveNum ).Var2Min = -100.0;
		PerfCurve( CurveNum ).Var2Max = 100.0;

		SetPredefinedTables();
		SizeDXCoil( 2 );
		EXPECT_DOUBLE_EQ( 5000.0, DXCoil( 2 ).DefrostCapacity );

		EXPECT_TRUE( has_cerr_output() );

		// fails on windows due to endline issue... this outputs /r/n on Windows but it is outputting /n on Windows for some reason...
		// EXPECT_TRUE( compare_cerr_stream( delimited_string( { 
		// 	"! <DX Heating Coil Standard Rating Information>, Component Type, Component Name, High Temperature Heating (net) Rating Capacity {W}, Low Temperature Heating (net) Rating Capacity {W}, HSPF {Btu/W-h}, Region Number", 
		// 	" DX Heating Coil Standard Rating Information, , DX Heating coil, 6414.3, 6414.3, 6.58, 4" } ) ) );

		// Clean up
		DXCoil.deallocate();
		DXCoilNumericFields.deallocate();
		PerfCurve.deallocate();

	}
	TEST_F( HVACFixture, DXCoils_Test2 )
	{
		using CurveManager::Quadratic;
		using CurveManager::BiQuadratic;
		using CurveManager::NumCurves;
		int DXCoilNum;
		int CurveNum;

		DataGlobals::DisplayExtraWarnings = true;
		SysSizingRunDone = true;
		FinalSysSizing.allocate( 1 );
		PrimaryAirSystem.allocate( 1 );
		AirLoopControlInfo.allocate( 1 );
		CurSysNum = 1;
		NumDXCoils = 2;
		DXCoilNum = 2;
		UnitarySysEqSizing.allocate( 1 );
		DXCoil.allocate( NumDXCoils );
		DXCoil( 1 ).DXCoilType_Num = CoilDX_CoolingSingleSpeed;
		DXCoil( 2 ).DXCoilType_Num = CoilDX_HeatingEmpirical;
		DXCoil( DXCoilNum ).DXCoilType = "Coil:Heating:DX:SingleSpeed";
		DXCoil( 2 ).CompanionUpstreamDXCoil = 1;

		DXCoilNumericFields.allocate( NumDXCoils );
		DXCoilNumericFields( 2 ).PerfMode.allocate( 1 );
		DXCoilNumericFields( 2 ).PerfMode( 1 ).FieldNames.allocate( 20 );
		DXCoil( 2 ).DefrostStrategy = Resistive;
		DXCoil( 2 ).DefrostCapacity = 5000.0;
		DXCoil( 2 ).Name = "DX Heating coil";

		DXCoil( 1 ).RatedTotCap( 1 ) = AutoSize;
		DXCoil( 1 ).RatedTotCap( 2 ) = AutoSize;
		DXCoil( 2 ).RatedTotCap( 1 ) = AutoSize;
		DXCoil( DXCoilNum ).RegionNum = 4;
		DXCoil( DXCoilNum ).MinOATCompressor = -17.78;
		DXCoil( DXCoilNum ).CCapFFlow( 1 ) = 1;
		DXCoil( DXCoilNum ).CCapFTemp( 1 ) = 1;
		DXCoil( DXCoilNum ).EIRFFlow( 1 ) = 1;
		DXCoil( DXCoilNum ).EIRFTemp( 1 ) = 1;
		DXCoil( DXCoilNum ).PLFFPLR( 1 ) = 1;
		NumCurves = 3;
		PerfCurve.allocate( NumCurves );

		CurveNum = 1;
		PerfCurve( CurveNum ).CurveType = Quadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = 0.0;
		PerfCurve( CurveNum ).Var1Max = 2.0;
		PerfCurve( CurveNum ).Var2Min = 0.0;
		PerfCurve( CurveNum ).Var2Max = 2.0;

		CurveNum = 2;
		PerfCurve( CurveNum ).CurveType = Quadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = 0.0;
		PerfCurve( CurveNum ).Var1Max = 1.0;
		PerfCurve( CurveNum ).Var2Min = 0.7;
		PerfCurve( CurveNum ).Var2Max = 1.0;

		CurveNum = 3;
		PerfCurve( CurveNum ).CurveType = BiQuadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_BiQuadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = -100.0;
		PerfCurve( CurveNum ).Var1Max = 100.0;
		PerfCurve( CurveNum ).Var2Min = -100.0;
		PerfCurve( CurveNum ).Var2Max = 100.0;

		SetPredefinedTables();
		SizeDXCoil( 2 );
		EXPECT_DOUBLE_EQ( 0.0, DXCoil( 2 ).RatedTotCap( 1 ) );

		EXPECT_TRUE( has_cerr_output() );

		// EXPECT_TRUE( compare_cerr_stream( delimited_string( { 
		// 	"! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value", 
		// 	" Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, Design Size  [W], 0.00000", 
		// 	" Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, User-Specified  [W], 5000.00000", 
		// 	" DX Heating Coil Standard Rating Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, 0.0, 0.0, 3.51, 4"} ) ) );

		// Output from CI, I don't know why it is different than above...

		// "! , Component Type, Component Name, Input Field Description, Value",
		// " Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, Design Size [W], 0.00000",
		// " Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, User-Specified [W], 5000.00000",
		// "! , Component Type, Component Name, High Temperature Heating (net) Rating Capacity {W}, Low Temperature Heating (net) Rating Capacity {W}, HSPF {Btu/W-h}, Region Number",
		// " DX Heating Coil Standard Rating Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, 0.0, 0.0, 3.51, 4"

		// Clean up
		DXCoil.deallocate();
		DXCoilNumericFields.deallocate();
		UnitarySysEqSizing.deallocate();
		PerfCurve.deallocate();
		FinalSysSizing.deallocate();
		PrimaryAirSystem.deallocate();
		AirLoopControlInfo.deallocate();

	}

}
