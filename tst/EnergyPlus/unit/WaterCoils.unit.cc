// EnergyPlus::WaterCoils Unit Tests
// FUNCTION INFORMATION:
//       AUTHOR         R Raustad
//       DATE WRITTEN   March 2015
//       MODIFIED
//       RE-ENGINEERED  na

// PURPOSE OF THIS FUNCTION:
// This function performs the sizing calculation on a chilled water coil.
// The water coil should size to use sizing data or Data* globals. After a coil is sized, the Data* variables should all be reset.
// Defect file showed a second coil in the input that autosized the same as the first (incorrect based on zone flow rates).


// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <Psychrometrics.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <ObjexxFCL/gio.hh>

using namespace EnergyPlus;
using namespace DataAirLoop;
using namespace DataAirSystems;
using namespace DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::FluidProperties;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::WaterCoils;
using namespace EnergyPlus::Psychrometrics;

class WaterCoilsTest : public testing::Test
{

public:

	WaterCoilsTest() // Setup global state
	{
		CurZoneEqNum = 0;
		CurSysNum = 0;
		CurOASysNum = 0;
		NumWaterCoils = 1;
		WaterCoil.allocate( NumWaterCoils );
		WaterCoilNumericFields.allocate( NumWaterCoils );
		WaterCoilNumericFields(NumWaterCoils).FieldNames.allocate(7); // max N fields for water coil
		TotNumLoops = 1;
		PlantLoop.allocate( TotNumLoops );
		PlantSizData.allocate( 1 );
		ZoneEqSizing.allocate( 1 );
		UnitarySysEqSizing.allocate( 1 );
		OASysEqSizing.allocate( 1 );
		SysSizInput.allocate( 1 );
		SysSizPeakDDNum.allocate( 1 );
		SysSizPeakDDNum( 1 ).TimeStepAtSensCoolPk.allocate( 1 );
		SysSizPeakDDNum( 1 ).TimeStepAtCoolFlowPk.allocate( 1 );
		SysSizPeakDDNum( 1 ).TimeStepAtTotCoolPk.allocate( 1 );
		SysSizPeakDDNum( 1 ).SensCoolPeakDD = 1;
		SysSizPeakDDNum( 1 ).CoolFlowPeakDD = 1;
		SysSizPeakDDNum( 1 ).TotCoolPeakDD = 1;
		FinalSysSizing.allocate( 1 );
		PrimaryAirSystem.allocate( 1 );
		AirLoopControlInfo.allocate( 1 );
		InitializePsychRoutines();
	}

	~WaterCoilsTest() // Reset global state
	{
		NumWaterCoils = 0;
		WaterCoil.clear();
		WaterCoilNumericFields.clear();
		PlantLoop.clear();
		PlantSizData.clear();
		ZoneEqSizing.clear();
		UnitarySysEqSizing.clear();
		OASysEqSizing.clear();
		SysSizInput.clear();
		SysSizPeakDDNum.clear();
		FinalSysSizing.clear();
		SysSizPeakDDNum.clear();
		PrimaryAirSystem.clear();
		AirLoopControlInfo.clear();
		cached_Twb.clear();
		cached_Psat.clear();
	}

};

TEST_F( WaterCoilsTest, WaterCoolingCoilSizing )
{
	InitializePsychRoutines();
	OutBaroPress = 101325.0;
	StdRhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, 20.0, 0.0 );
	ShowMessage( "Begin Test: WaterCoilsTest, WaterCoolingCoilSizing" );
	int write_stat;
	// Open the Initialization Output File (lifted from SimulationManager.cc)
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }

	// set up sizing flags
	SysSizingRunDone = true;

	// set up plant sizing
	NumPltSizInput = 1;
	PlantSizData( 1 ).PlantLoopName = "WaterLoop";

	// set up plant loop
	for ( int l = 1; l <= TotNumLoops; ++l ) {
		auto & loop( PlantLoop( l ) );
		loop.LoopSide.allocate( 2 );
		auto & loopside( PlantLoop( 1 ).LoopSide( 1 ) );
		loopside.TotalBranches = 1;
		loopside.Branch.allocate( 1 );
		auto & loopsidebranch( PlantLoop( 1 ).LoopSide(1).Branch( 1 ) );
		loopsidebranch.TotalComponents = 1;
		loopsidebranch.Comp.allocate( 1 );
	}
	PlantLoop( 1 ).Name = "WaterLoop";
	PlantLoop( 1 ).FluidName = "FluidWaterLoop";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidName = "WATER";
	PlantLoop( 1 ).FluidIndex = 1;

	// set up sizing data
	FinalSysSizing( 1 ).MixTempAtCoolPeak = 20.0;
	FinalSysSizing( 1 ).CoolSupTemp = 10.0;
	FinalSysSizing( 1 ).MixHumRatAtCoolPeak = 0.01;
	FinalSysSizing( 1 ).DesMainVolFlow = 0.00159;
	FinalSysSizing( 1 ).HeatSupTemp = 25.0;
	FinalSysSizing( 1 ).HeatOutTemp = 5.0;
	FinalSysSizing( 1 ).HeatRetTemp = 20.0;

	// set up water coil
	int CoilNum = 1;
	WaterCoil( CoilNum ).Name = "Test Water Cooling Coil";
	WaterCoil( CoilNum ).WaterLoopNum = 1;
	WaterCoil( CoilNum ).WaterCoilType = CoilType_Cooling;
	WaterCoil( CoilNum ).RequestingAutoSize = true;
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

	WaterCoilNumericFields( CoilNum ).FieldNames( 3 ) = "Maximum Flow Rate";
	WaterCoil( CoilNum ).WaterInletNodeNum = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( CoilNum ).WaterInletNodeNum;

	CurZoneEqNum = 0;
	CurSysNum = 1;
	CurOASysNum = 0;
	SysSizInput( 1 ).CoolCapControl = VAV;
	PlantSizData( 1 ).ExitTemp = 5.7;
	PlantSizData( 1 ).DeltaT = 5.0;
	FinalSysSizing( 1 ).MassFlowAtCoolPeak = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;
	DataWaterLoopNum = 1;
	NumOfGlycols = 1;

	SizeWaterCoil( CoilNum );

	EXPECT_DOUBLE_EQ( 0.00159, WaterCoil( CoilNum ).DesAirVolFlowRate );
	// Check that all Data* variables have been reset
	EXPECT_EQ( 0, DataPltSizCoolNum );
	EXPECT_EQ( 0, DataWaterLoopNum );
	EXPECT_DOUBLE_EQ( 0.0, DataConstantUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFractionUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataAirFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataWaterFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataCapacityUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletWaterTemp );

	// set second cooling coil to size at a different air flow, adjust sizing data, and autosize input data
	FinalSysSizing( 1 ).DesMainVolFlow = 0.00259;
	FinalSysSizing( 1 ).MassFlowAtCoolPeak = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;

	WaterCoil( CoilNum ).Name = "Test Water Cooling Coil 2";
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

	SizeWaterCoil( CoilNum );

	EXPECT_DOUBLE_EQ( 0.00259, WaterCoil( CoilNum ).DesAirVolFlowRate );
	EXPECT_EQ( 0, DataPltSizCoolNum );
	EXPECT_EQ( 0, DataWaterLoopNum );
	EXPECT_DOUBLE_EQ( 0.0, DataConstantUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFractionUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataAirFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataWaterFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataCapacityUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletWaterTemp );

	// size heating coil
	CurZoneEqNum = 0;
	CurSysNum = 1;
	CurOASysNum = 0;
	FinalSysSizing( 1 ).DesMainVolFlow = 0.00359;
	FinalSysSizing( 1 ).MassFlowAtCoolPeak = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;
	AirLoopControlInfo( 1 ).UnitarySys = true;

	WaterCoil( CoilNum ).Name = "Test Water Heating Coil";
	WaterCoil( CoilNum ).WaterCoilType = CoilType_Heating;
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

	SizeWaterCoil( CoilNum );

	EXPECT_DOUBLE_EQ( 0.00359, WaterCoil( CoilNum ).DesAirVolFlowRate );
	EXPECT_EQ( 0, DataPltSizCoolNum );
	EXPECT_EQ( 0, DataWaterLoopNum );
	EXPECT_DOUBLE_EQ( 0.0, DataConstantUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFractionUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataAirFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataWaterFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataCapacityUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletWaterTemp );

	// size 2nd heating coil
	FinalSysSizing( 1 ).DesMainVolFlow = 0.00459;
	FinalSysSizing( 1 ).MassFlowAtCoolPeak = FinalSysSizing( 1 ).DesMainVolFlow * StdRhoAir;

	WaterCoil( CoilNum ).Name = "Test Water Heating Coil 2";
	WaterCoil( CoilNum ).WaterCoilType = CoilType_Heating;
	WaterCoil( CoilNum ).DesAirVolFlowRate = AutoSize;
	WaterCoil( CoilNum ).DesInletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletWaterTemp = AutoSize;
	WaterCoil( CoilNum ).DesInletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).DesOutletAirHumRat = AutoSize;
	WaterCoil( CoilNum ).MaxWaterVolFlowRate = AutoSize;

	SizeWaterCoil( CoilNum );

	EXPECT_DOUBLE_EQ( 0.00459, WaterCoil( CoilNum ).DesAirVolFlowRate );
	EXPECT_EQ( 0, DataPltSizCoolNum );
	EXPECT_EQ( 0, DataWaterLoopNum );
	EXPECT_DOUBLE_EQ( 0.0, DataConstantUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFractionUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataAirFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataWaterFlowUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataCapacityUsedForSizing );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirTemp );
	EXPECT_DOUBLE_EQ( 0.0, DataDesOutletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletAirHumRat );
	EXPECT_DOUBLE_EQ( 0.0, DataDesInletWaterTemp );

	// Close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }

}
