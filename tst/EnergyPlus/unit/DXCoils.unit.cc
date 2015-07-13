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

TEST( DXCoilsTest, TestMultiSpeedDefrostCOP )
{
	// Test that the COP calculation is correct when the defrost is on. #4973
	
	using CurveManager::Quadratic;
	using CurveManager::BiQuadratic;
	using CurveManager::NumCurves;
	using EnergyPlus::DataEnvironment::OutHumRat;
	using EnergyPlus::DataEnvironment::OutDryBulbTemp;
	using EnergyPlus::DataEnvironment::OutBaroPress;
	using DXCoils::CalcMultiSpeedDXCoilHeating;
	int DXCoilNum;
	
	// Set up heating coil and curves.
	
	InitializePsychRoutines();
	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoilData & Coil = DXCoil( DXCoilNum );
	
	Coil.DXCoilType = "Coil:Heating:DX:MultiSpeed";
	Coil.DXCoilType_Num = CoilDX_MultiSpeedHeating;
	Coil.SchedPtr = ScheduleAlwaysOn;
	
	DXCoilNumericFields.allocate( NumDXCoils );
	EnergyPlus::DataHeatBalance::HeatReclaimDXCoil.allocate( NumDXCoils );
	DXCoilOutletTemp.allocate( NumDXCoils );
	DXCoilOutletHumRat.allocate( NumDXCoils );
	DXCoilFanOpMode.allocate( NumDXCoils );
	DXCoilPartLoadRatio.allocate( NumDXCoils );
	DXCoilNumericFields( DXCoilNum ).PerfMode.allocate( 1 );
	DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate( 4 );
	Coil.DefrostStrategy = Resistive;
	Coil.Name = "DX Heating coil";
	Coil.NumOfSpeeds = 2;
	
	Coil.MSRatedTotCap.allocate( Coil.NumOfSpeeds );
	Coil.MSRatedSHR.allocate( Coil.NumOfSpeeds );
	Coil.MSRatedCOP.allocate( Coil.NumOfSpeeds );
	Coil.MSRatedAirVolFlowRate.allocate( Coil.NumOfSpeeds );
	Coil.MSRatedAirMassFlowRate.allocate( Coil.NumOfSpeeds );
	Coil.MSCCapFTemp.allocate( Coil.NumOfSpeeds );
	Coil.MSCCapFFlow.allocate( Coil.NumOfSpeeds );
	Coil.MSEIRFTemp.allocate( Coil.NumOfSpeeds );
	Coil.MSEIRFFlow.allocate( Coil.NumOfSpeeds );
	Coil.MSWasteHeat.allocate( Coil.NumOfSpeeds );
	Coil.MSEvapCondEffect.allocate( Coil.NumOfSpeeds );
	Coil.MSEvapCondAirFlow.allocate( Coil.NumOfSpeeds );
	Coil.MSEvapCondPumpElecNomPower.allocate( Coil.NumOfSpeeds );
	Coil.MSRatedCBF.allocate( Coil.NumOfSpeeds );
	Coil.MSWasteHeatFrac.allocate( Coil.NumOfSpeeds );
	Coil.MSPLFFPLR.allocate( Coil.NumOfSpeeds );
	Coil.MSTwet_Rated.allocate( Coil.NumOfSpeeds );
	Coil.MSGamma_Rated.allocate( Coil.NumOfSpeeds );
	Coil.MSMaxONOFFCyclesperHour.allocate( Coil.NumOfSpeeds );
	Coil.MSLatentCapacityTimeConstant.allocate( Coil.NumOfSpeeds );
	Coil.MSFanPowerPerEvapAirFlowRate.allocate( Coil.NumOfSpeeds );
	Coil.MSTotCapTempModFacCurveType.allocate( Coil.NumOfSpeeds );
	Coil.MSEIRTempModFacCurveType.allocate( Coil.NumOfSpeeds );
	
	Coil.MinOATCompressor = -73.27777777777779;
	Coil.CrankcaseHeaterCapacity = 0.0;
	Coil.MaxOATDefrost = 0.0;
	Coil.DefrostStrategy = Resistive;
	Coil.DefrostControl = Timed;
	Coil.DefrostTime = 0.058333;
	Coil.DefrostCapacity = 1000;
	Coil.PLRImpact = false;
	Coil.FuelType = FuelTypeElectricity;
	Coil.RegionNum = 4;
	Coil.MSRatedTotCap( 1 ) = 2202.5268975202675;
	Coil.MSRatedCOP( 1 ) = 4.200635910578916;
	Coil.MSRatedAirVolFlowRate( 1 ) = 0.087746133503702;
	Coil.MSFanPowerPerEvapAirFlowRate( 1 ) = 773.3;
	Coil.MSWasteHeatFrac( 1 ) = 0.2;
	Coil.MSRatedTotCap( 2 ) = 11012.634487601337;
	Coil.MSRatedCOP( 2 ) = 4.200635910578916;
	Coil.MSRatedAirVolFlowRate( 2 ) = 0.43873066751851;
	Coil.MSFanPowerPerEvapAirFlowRate( 2 ) = 773.3;
	Coil.MSWasteHeatFrac( 2 ) = 0.2;
	Coil.RatedSHR( 1 ) = 1.0;
	
	for (int mode = 1; mode <= Coil.NumOfSpeeds; ++mode) {
		Coil.MSRatedAirMassFlowRate( mode ) = Coil.MSRatedAirVolFlowRate( mode ) * PsyRhoAirFnPbTdbW(EnergyPlus::DataEnvironment::StdBaroPress, 21.11, 0.00881, "InitDXCoil");
	}
	
	NumCurves = 11;
	PerfCurve.allocate( NumCurves );
	
	PerfomanceCurveData * pCurve;

	int const nCapfT1 = 1;
	pCurve = &PerfCurve( nCapfT1 );
	pCurve->CurveType = BiQuadratic;
	pCurve->Name = "HP_Heat-Cap-fT1";
	pCurve->Coeff1 = 0.95624428;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Coeff4 = 0.005999544;
	pCurve->Coeff5 = -0.0000900072;
	pCurve->Coeff6 = 0;
	pCurve->Var1Min = -100;
	pCurve->Var1Max = 100;
	pCurve->Var2Min = -100;
	pCurve->Var2Max = 100;
	
	Coil.MSCCapFTemp( 1 ) = nCapfT1;
	Coil.MSTotCapTempModFacCurveType( 1 ) = pCurve->CurveType;

	int const nCapfFF1 = 2;
	pCurve = &PerfCurve( nCapfFF1 );
	pCurve->CurveType = Quadratic;
	pCurve->Name = "HP_Heat-Cap-fFF1";
	pCurve->Coeff1 = 1;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Var1Min = 0;
	pCurve->Var1Max = 2;
	pCurve->CurveMin = 0;
	pCurve->CurveMax = 2;
	
	Coil.MSCCapFFlow( 1 ) = nCapfFF1;

	int const nEIRfT1 = 3;
	pCurve = &PerfCurve( nEIRfT1 );
	pCurve->CurveType = BiQuadratic;
	pCurve->Name = "HP_Heat-EIR-fT1";
	pCurve->Coeff1 = 1.065476178;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Coeff4 = -0.0085714308;
	pCurve->Coeff5 = 0.0000857142;
	pCurve->Coeff6 = 0;
	pCurve->Var1Min = -100;
	pCurve->Var1Max = 100;
	pCurve->Var2Min = -100;
	pCurve->Var2Max = 100;
	
	Coil.MSEIRFTemp( 1 ) = nEIRfT1;

	int const nEIRfFF1 = 4;
	pCurve = &PerfCurve( nEIRfFF1 );
	pCurve->CurveType = Quadratic;
	pCurve->Name = "HP_Heat-EIR-fFF1";
	pCurve->Coeff1 = 1;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Var1Min = 0;
	pCurve->Var1Max = 2;
	pCurve->CurveMin = 0;
	pCurve->CurveMax = 2;
	
	Coil.MSEIRFFlow( 1 ) = nEIRfFF1;
	Coil.MSEIRTempModFacCurveType( 1 ) = pCurve->CurveType;
	
	int const nPLFfPLR1 = 5;
	pCurve = &PerfCurve( nPLFfPLR1 );
	pCurve->CurveType = Quadratic;
	pCurve->Name = "HP_Heat-PLF-fPLR1";
	pCurve->Coeff1 = 1;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Var1Min = 0;
	pCurve->Var1Max = 1;
	pCurve->CurveMin = 0.7;
	pCurve->CurveMax = 1;
	
	Coil.MSPLFFPLR( 1 ) = nPLFfPLR1;
	
	int const nConstantBiquadratic = 6;
	pCurve = &PerfCurve( nConstantBiquadratic );
	pCurve->CurveType = BiQuadratic;
	pCurve->Name = "ConstantBiquadratic";
	pCurve->Coeff1 = 1;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Coeff4 = 0;
	pCurve->Coeff5 = 0;
	pCurve->Coeff6 = 0;
	pCurve->Var1Min = -100;
	pCurve->Var1Max = 100;
	pCurve->Var2Min = -100;
	pCurve->Var2Max = 100;
	
	Coil.MSWasteHeat( 1 ) = nConstantBiquadratic;
	Coil.MSWasteHeat( 2 ) = nConstantBiquadratic;

	int const nCapfT2 = 7;
	pCurve = &PerfCurve( nCapfT2 );
	pCurve->CurveType = BiQuadratic;
	pCurve->Name = "HP_Heat-Cap-fT2";
	pCurve->Coeff1 = 0.95624428;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Coeff4 = 0.005999544;
	pCurve->Coeff5 = -0.0000900072;
	pCurve->Coeff6 = 0;
	pCurve->Var1Min = -100;
	pCurve->Var1Max = 100;
	pCurve->Var2Min = -100;
	pCurve->Var2Max = 100;
	
	Coil.MSCCapFTemp( 2 ) = nCapfT2;
	Coil.MSTotCapTempModFacCurveType( 2 ) = pCurve->CurveType;

	int const nCapfFF2 = 8;
	pCurve = &PerfCurve( nCapfFF2 );
	pCurve->CurveType = Quadratic;
	pCurve->Name = "HP_Heat-Cap-fFF2";
	pCurve->Coeff1 = 1;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Var1Min = 0;
	pCurve->Var1Max = 2;
	pCurve->CurveMin = 0;
	pCurve->CurveMax = 2;
	
	Coil.MSCCapFFlow( 2 ) = nCapfFF2;
	
	int const nEIRfT2 = 9;
	pCurve = &PerfCurve( nEIRfT2 );
	pCurve->CurveType = BiQuadratic;
	pCurve->Name = "HP_Heat-EIR-fT2";
	pCurve->Coeff1 = 1.065476178;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Coeff4 = -0.0085714308;
	pCurve->Coeff5 = 0.0000857142;
	pCurve->Coeff6 = 0;
	pCurve->Var1Min = -100;
	pCurve->Var1Max = 100;
	pCurve->Var2Min = -100;
	pCurve->Var2Max = 100;
	
	Coil.MSEIRFTemp( 2 ) = nEIRfT2;
	Coil.MSEIRTempModFacCurveType = pCurve->CurveType;

	int const nEIRfFF2 = 10;
	pCurve = &PerfCurve( nEIRfFF2 );
	pCurve->CurveType = Quadratic;
	pCurve->Name = "HP_Heat-EIR-fFF2";
	pCurve->Coeff1 = 1;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Var1Min = 0;
	pCurve->Var1Max = 2;
	pCurve->CurveMin = 0;
	pCurve->CurveMax = 2;
	
	Coil.MSEIRFFlow( 2 ) = nEIRfFF2;

	int const nPLFfPLR2 = 11;
	pCurve = &PerfCurve( nPLFfPLR2 );
	pCurve->CurveType = Quadratic;
	pCurve->Name = "HP_Heat-PLF-fPLR2";
	pCurve->Coeff1 = 1;
	pCurve->Coeff2 = 0;
	pCurve->Coeff3 = 0;
	pCurve->Var1Min = 0;
	pCurve->Var1Max = 1;
	pCurve->CurveMin = 0.7;
	pCurve->CurveMax = 1;
	
	Coil.MSPLFFPLR( 2 ) = nPLFfPLR2;
	
	for ( int CurveNum = 1; CurveNum <= NumCurves; ++CurveNum) {
		PerfomanceCurveData & rCurve = PerfCurve( CurveNum );
		if ( rCurve.CurveType == BiQuadratic ) {
			rCurve.ObjectType = CurveType_BiQuadratic;
			rCurve.InterpolationType = EvaluateCurveToLimits;
		} else if ( rCurve.CurveType == Quadratic ) {
			rCurve.ObjectType = CurveType_Quadratic;
			rCurve.InterpolationType = EvaluateCurveToLimits;
		}
	}
	
	// Set up inlet air conditions.
	Coil.InletAirMassFlowRate = Coil.MSRatedAirMassFlowRate( 1 );
	MSHPMassFlowRateLow = Coil.MSRatedAirMassFlowRate( 1 );
	MSHPMassFlowRateHigh = Coil.MSRatedAirMassFlowRate( 2 );
	OutHumRat = 0.002;
	OutBaroPress = 101325; // sea level
	Coil.InletAirTemp = 20;
	Coil.InletAirHumRat = 0.008;
	Coil.InletAirEnthalpy = PsyHFnTdbW(Coil.InletAirTemp, Coil.InletAirHumRat);
	
	// Test high speed
	Real64 SpeedRatio = 1.0;
	Real64 CycRatio = 1.0;
	int const FanOpMode = ContFanCycCoil;
	
	for ( int SpeedNum = 2; SpeedNum >= 1; --SpeedNum ) {
		
		// Defroster on
		OutDryBulbTemp = -5.0; // cold
		CalcMultiSpeedDXCoilHeating( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode );
		Real64 COPwoDefrost = Coil.MSRatedCOP( SpeedNum ) / ( CurveValue(nEIRfT2, Coil.InletAirTemp, OutDryBulbTemp) * CurveValue(nEIRfFF2, 1));
		Real64 COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
		EXPECT_LT( COPwDefrost, COPwoDefrost );
		
		// Defroster off
		OutDryBulbTemp = 5.0; // not cold enough for defroster
		CalcMultiSpeedDXCoilHeating( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode );
		COPwoDefrost = Coil.MSRatedCOP( SpeedNum ) / ( CurveValue(nEIRfT2, Coil.InletAirTemp, OutDryBulbTemp) * CurveValue(nEIRfFF2, 1));
		COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
		EXPECT_DOUBLE_EQ( COPwoDefrost, COPwDefrost );

	}
	
	// Clean up
	DXCoil.deallocate();
	DXCoilNumericFields.deallocate();
	EnergyPlus::DataHeatBalance::HeatReclaimDXCoil.deallocate();
	DXCoilOutletTemp.deallocate();
	DXCoilOutletHumRat.deallocate();
	DXCoilFanOpMode.deallocate();
	DXCoilPartLoadRatio.deallocate();
	PerfCurve.deallocate();

}
