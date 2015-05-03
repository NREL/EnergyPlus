// EnergyPlus::Low Temperature Radiant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/LowTempRadiantSystem.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>


using namespace EnergyPlus;
using namespace EnergyPlus::LowTempRadiantSystem;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace DataGlobals;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::FluidProperties;


class LowTempRadiantSystemTest : public ::testing::Test
{

public:

	int RadSysNum;
	int SystemType;
	Real64 ExpectedResult1;
	Real64 ExpectedResult2;
	Real64 ExpectedResult3;
	Real64 const CpWater = 4180.0; // For estimating the expected result
	Real64 const RhoWater = 1000.0; // For estimating the expected result

	// constructor for test fixture class
	LowTempRadiantSystemTest( )
	{
		ElecRadSys.allocate( 1 );
		HydrRadSys.allocate( 1 );
		CFloRadSys.allocate( 1 );
		CalcFinalZoneSizing.allocate( 1 );
		ZoneEqSizing.allocate( 1 );
		Zone.allocate( 1 );
		CurZoneEqNum = 1;
		ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
		ZoneSizingRunDone = true;

		CurSysNum = 0;
		RadSysNum = 1;
		SystemType = ElectricSystem;
		ElecRadSysNumericFields.allocate( 1 );
		ElecRadSysNumericFields( RadSysNum ).FieldNames.allocate( 1 );
		HydronicRadiantSysNumericFields.allocate( 1 );
		HydronicRadiantSysNumericFields( RadSysNum ).FieldNames.allocate( 15 );
		HydrRadSys( RadSysNum ).NumCircuits.allocate( 1 );
		// set up plant loop
		TotNumLoops = 2;
		PlantLoop.allocate( TotNumLoops );
		PlantSizData.allocate( TotNumLoops );
		NumPltSizInput = TotNumLoops;

		for ( int loopindex = 1; loopindex <= TotNumLoops; ++loopindex ) {
			auto & loop( PlantLoop( loopindex ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( PlantLoop( loopindex ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( PlantLoop( loopindex ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}
		PlantLoop( 1 ).Name = "Hot Water Loop";
		PlantLoop( 1 ).FluidName = "WATER";
		PlantLoop( 1 ).FluidIndex = 1;

		PlantLoop( 2 ).Name = "Chilled Water Loop";
		PlantLoop( 2 ).FluidName = "WATER";
		PlantLoop( 2 ).FluidIndex = 1;

		PlantSizData( 1 ).PlantLoopName = "Hot Water Loop";
		PlantSizData( 1 ).ExitTemp = 80.0;
		PlantSizData( 1 ).DeltaT = 10.0;

		PlantSizData( 2 ).PlantLoopName = "Chilled Water Loop";
		PlantSizData( 2 ).ExitTemp = 6.0;
		PlantSizData( 2 ).DeltaT = 5.0;

		ExpectedResult1 = 0.0;
		ExpectedResult2 = 0.0;
		ExpectedResult3 = 0.0;

		int write_stat;
		// Open the Initialization Output File (lifted from SimulationManager.cc)
		OutputFileInits = GetNewUnitNumber( );
		{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios( ); }
	}

	//destructor
	~LowTempRadiantSystemTest( )
	{

		// Reset sizing flags to prevent interactions with other unit tests when run as a group
		DataFracOfAutosizedCoolingAirflow = 1.0; // fraction of design cooling supply air flow rate
		DataFracOfAutosizedHeatingAirflow = 1.0; // fraction of design heating supply air flow rate
		DataFlowPerCoolingCapacity = 0.0; // cooling supply air flow per unit cooling capacity
		DataFlowPerHeatingCapacity = 0.0; // heating supply air flow per unit heating capacity
		DataFracOfAutosizedCoolingCapacity = 1.0; // fraction of autosized cooling capacity
		DataFracOfAutosizedHeatingCapacity = 1.0; // fraction of autosized heating capacit
		DataAutosizedCoolingCapacity = 0.0; // Autosized cooling capacity used for multiplying flow per capacity to get flow rate
		DataAutosizedHeatingCapacity = 0.0; // Autosized heating capacit used for multiplying flow per capacity to get flow rate
		DataConstantUsedForSizing = 0.0; // base value used for sizing inputs that are ratios of other inputs
		DataFractionUsedForSizing = 0.0; // fractional value of base value used for sizing inputs that are ratios of other inputs
		DataScalableSizingON = false; // boolean determines scalable flow sizing is specified
		DataScalableCapSizingON = false; // boolean determines scalable capacity sizing is specified
		DataSysScalableFlowSizingON = false; // boolean determines scalable system flow sizing is specified
		DataSysScalableCapSizingON = false; // boolean determines scalable system capacity sizing is specified

		ElecRadSys.deallocate( );
		HydrRadSys( 1 ).NumCircuits.deallocate( );
		HydrRadSys.deallocate( );
		CFloRadSys.deallocate( );
		ElecRadSysNumericFields( 1 ).FieldNames.deallocate( );
		ElecRadSysNumericFields.deallocate( );
		CalcFinalZoneSizing.deallocate( );
		ZoneEqSizing( 1 ).SizingMethod.deallocate( );
		ZoneEqSizing.deallocate( );
		Zone.deallocate( );
		HydronicRadiantSysNumericFields( 1 ).FieldNames.deallocate( );
		HydronicRadiantSysNumericFields.deallocate( );
		for ( int loopindex = 1; loopindex <= TotNumLoops; ++loopindex ) {
			auto & loopsidebranch( PlantLoop( loopindex ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.Comp.deallocate( );
			auto & loopside( PlantLoop( loopindex ).LoopSide( 1 ) );
			loopside.Branch.deallocate( );
			auto & loop( PlantLoop( loopindex ) );
			loop.LoopSide.deallocate( );
		}
		PlantLoop.deallocate( );
		PlantSizData.deallocate( );

		// Close and delete eio output file
		{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }
	}

};

TEST_F( LowTempRadiantSystemTest, SizeLowTempRadiantElectric )
{
	ShowMessage( "Begin Test: LowTempRadiantSystemTest, SizeLowTempRadiantElectric" );

	SystemType = ElectricSystem;
	ElecRadSys( RadSysNum ).Name = "LowTempElectric 1";
	ElecRadSys( RadSysNum ).ZonePtr = 1;
	ElecRadSysNumericFields( RadSysNum ).FieldNames( 1 ) = "Heating Design Capacity";

	//Electric - HeatingDesignCapacity method
	ElecRadSys( RadSysNum ).MaxElecPower = AutoSize;
	ElecRadSys( RadSysNum ).HeatingCapMethod = HeatingDesignCapacity;
	ElecRadSys( RadSysNum ).ScaledHeatingCapacity = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 1000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.2;
	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( 1200.0, ElecRadSys( RadSysNum ).MaxElecPower, 0.1 );

	//Electric - CapacityPerFloorArea method - hold until scalable sizing issue is resolved
	//ElecRadSys( RadSysNum ).MaxElecPower = AutoSize;
	//ElecRadSys( RadSysNum ).HeatingCapMethod = CapacityPerFloorArea;
	//ElecRadSys( RadSysNum ).ScaledHeatingCapacity = 1.5;
	//Zone( 1 ).FloorArea = 500.0;
	//SizeLowTempRadiantSystem( RadSysNum, SystemType );
	//EXPECT_NEAR( 750.0, ElecRadSys( RadSysNum ).MaxElecPower, 0.1 );

	//Electric - FractionOfAutosizedHeatingCapacity method - hold until scalable sizing issue is resolved
	//ElecRadSys( RadSysNum ).MaxElecPower = AutoSize;
	//ElecRadSys( RadSysNum ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
	//ElecRadSys( RadSysNum ).ScaledHeatingCapacity = 10.0;
	//CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 800.0;
	//CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.1;
	//SizeLowTempRadiantSystem( RadSysNum, SystemType );
	//EXPECT_NEAR( 8800.0, ElecRadSys( RadSysNum ).MaxElecPower, 0.1 );
}

TEST_F( LowTempRadiantSystemTest, SizeLowTempRadiantVariableFlow )
{
	ShowMessage( "Begin Test: LowTempRadiantSystemTest, SizeLowTempRadiantVariableFlow" );

	SystemType = HydronicSystem;
	HydrRadSys( RadSysNum ).Name = "LowTempVarFlow 1";
	HydrRadSys( RadSysNum ).ZonePtr = 1;
	HydronicRadiantSysNumericFields( RadSysNum ).FieldNames( 3 ) = "Heating Design Capacity";
	HydronicRadiantSysNumericFields( RadSysNum ).FieldNames( 8 ) = "Cooling Design Capacity";

	HydrRadSys( RadSysNum ).HotWaterInNode = 1;
	HydrRadSys( RadSysNum ).HotWaterOutNode = 2;
	HydrRadSys( RadSysNum ).HWLoopNum = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = HydrRadSys( RadSysNum ).HotWaterInNode;

	HydrRadSys( RadSysNum ).ColdWaterInNode = 3;
	HydrRadSys( RadSysNum ).ColdWaterOutNode = 4;
	HydrRadSys( RadSysNum ).CWLoopNum = 2;
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = HydrRadSys( RadSysNum ).ColdWaterInNode;

	//Hydronic - HeatingDesignCapacity/CoolingDesignCapacity method
	HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat = AutoSize;
	HydrRadSys( RadSysNum ).HeatingCapMethod = HeatingDesignCapacity;
	HydrRadSys( RadSysNum ).ScaledHeatingCapacity = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 1000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.2;
	ExpectedResult1 = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
	ExpectedResult1 = ExpectedResult1 / ( PlantSizData( 1 ).DeltaT * RhoWater * CpWater );

	HydrRadSys( RadSysNum ).WaterVolFlowMaxCool = AutoSize;
	HydrRadSys( RadSysNum ).CoolingCapMethod = CoolingDesignCapacity;
	HydrRadSys( RadSysNum ).ScaledCoolingCapacity = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad = 2000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor = 1.1;
	ExpectedResult2 = CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor;
	ExpectedResult2 = ExpectedResult2 / ( PlantSizData( 2 ).DeltaT * RhoWater * CpWater );

	HydrRadSys( RadSysNum ).NumCircCalcMethod = 0;
	HydrRadSys( RadSysNum ).NumOfSurfaces = 1;
	HydrRadSys( RadSysNum ).TubeLength = AutoSize;
	HydrRadSys( RadSysNum ).TotalSurfaceArea = 1500.0;
	ExpectedResult3 = HydrRadSys( RadSysNum ).TotalSurfaceArea / 0.15;

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult1, HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat, 0.1 );
	EXPECT_NEAR( ExpectedResult2, HydrRadSys( RadSysNum ).WaterVolFlowMaxCool, 0.1 );
	EXPECT_NEAR( ExpectedResult3, HydrRadSys( RadSysNum ).TubeLength, 0.1 );

	//Hydronic - CapacityPerFloorArea method
	HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat = AutoSize;
	HydrRadSys( RadSysNum ).HeatingCapMethod = CapacityPerFloorArea;
	HydrRadSys( RadSysNum ).ScaledHeatingCapacity = 10.0;
	Zone( 1 ).FloorArea = 500.0;
	ExpectedResult1 = HydrRadSys( RadSysNum ).ScaledHeatingCapacity * Zone( 1 ).FloorArea;
	ExpectedResult1 = ExpectedResult1 / ( PlantSizData( 1 ).DeltaT * RhoWater * CpWater );

	HydrRadSys( RadSysNum ).WaterVolFlowMaxCool = AutoSize;
	HydrRadSys( RadSysNum ).CoolingCapMethod = CapacityPerFloorArea;
	HydrRadSys( RadSysNum ).ScaledCoolingCapacity = 20.0;
	ExpectedResult2 = HydrRadSys( RadSysNum ).ScaledCoolingCapacity * Zone( 1 ).FloorArea;
	ExpectedResult2 = ExpectedResult2 / ( PlantSizData( 2 ).DeltaT * RhoWater * CpWater );

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult1, HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat, 0.1 );
	EXPECT_NEAR( ExpectedResult2, HydrRadSys( RadSysNum ).WaterVolFlowMaxCool, 0.1 );

	//Hydronic - FractionOfAutosizedHeating/CoolingCapacity method
	HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat = AutoSize;
	HydrRadSys( RadSysNum ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
	HydrRadSys( RadSysNum ).ScaledHeatingCapacity = 1.2;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 800.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.1;
	ExpectedResult1 = HydrRadSys( RadSysNum ).ScaledHeatingCapacity * CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
	ExpectedResult1 = ExpectedResult1 / ( PlantSizData( 1 ).DeltaT * RhoWater * CpWater );

	HydrRadSys( RadSysNum ).WaterVolFlowMaxCool = AutoSize;
	HydrRadSys( RadSysNum ).CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
	HydrRadSys( RadSysNum ).ScaledCoolingCapacity = 1.5;
	CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad = 1000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor = 1.2;
	ExpectedResult2 = HydrRadSys( RadSysNum ).ScaledCoolingCapacity * CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor;
	ExpectedResult2 = ExpectedResult2 / ( PlantSizData( 2 ).DeltaT * RhoWater * CpWater );

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult1, HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat, 0.1 );
	EXPECT_NEAR( ExpectedResult2, HydrRadSys( RadSysNum ).WaterVolFlowMaxCool, 0.1 );

}
