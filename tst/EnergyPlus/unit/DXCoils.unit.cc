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
#include <DXCoils.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataBranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataWater.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <StandardRatings.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

using namespace EnergyPlus;
using namespace DXCoils;
using namespace DataAirLoop;
using namespace DataAirSystems;
using namespace DataHVACGlobals;
using namespace DataSizing;
using namespace CurveManager;
using namespace OutputReportPredefined;
using namespace EnergyPlus::Psychrometrics;

TEST( DXCoilsTest, Test1 )
{

	ShowMessage( "Begin Test: DXCoilsTest, Test1" );

	using CurveManager::Quadratic;
	using CurveManager::BiQuadratic;
	using CurveManager::NumCurves;
	//	int NumDXCoils( 0 ); // Total number of DX coils
//	Array1D< DXCoilData > DXCoil;
	int DXCoilNum;
	int CurveNum;

	InitializePsychRoutines();
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

	// Clean up
	DXCoil.deallocate();
	DXCoilNumericFields.deallocate();
	PerfCurve.deallocate();
	cached_Twb.deallocate();
	cached_Psat.deallocate();

}
TEST( DXCoilsTest, Test2 )
{

	using CurveManager::Quadratic;
	using CurveManager::BiQuadratic;
	using CurveManager::NumCurves;
	int DXCoilNum;
	int CurveNum;

	InitializePsychRoutines();
	DisplayExtraWarnings = true;
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

	// Clean up
	DXCoil.deallocate();
	DXCoilNumericFields.deallocate();
	UnitarySysEqSizing.deallocate();
	PerfCurve.deallocate();
	FinalSysSizing.deallocate();
	PrimaryAirSystem.deallocate();
	AirLoopControlInfo.deallocate();
	cached_Twb.deallocate();
	cached_Psat.deallocate();

}

