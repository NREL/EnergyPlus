// EnergyPlus::HVACVariableRefrigerantFlow unit tests

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <HeatBalanceManager.hh>
#include "OutdoorAirUnit.hh"
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::CurveManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::OutdoorAirUnit;
using namespace OutputReportPredefined;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, OutdoorAirUnit_AutoSize ) {
		
		bool ErrorsFound( false );        // function returns true on error
		bool FirstHVACIteration( true );  // simulate the first pass through HVAC simulation, use false for next iteration
		int OAUnitNum( 1 );               // index to ZoneHVAC:OutdoorAirUnit
		int EquipPtr( 1 );                // index to equipment list
		int CurZoneNum( 1 );              // index to zone
		Real64 SysOutputProvided( 0.0 );  // function returns sensible capacity [W]
		Real64 LatOutputProvided( 0.0 );  // function returns latent capacity [W]
		int ZoneInletNode( 0 );
		
		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			" ",
			"Output:Diagnostics, DisplayExtraWarnings;",
			" ",
			"Zone,",
			"  SPACE1-1,                !- Name",
			"  0,                       !- Direction of Relative North {deg}",
			"  0,                       !- X Origin {m}",
			"  0,                       !- Y Origin {m}",
			"  0,                       !- Z Origin {m}",
			"  1,                       !- Type",
			"  1,                       !- Multiplier",
			"  2.5,                     !- Ceiling Height {m}",
			"  250.0;                   !- Volume {m3}",
			" ",
			"ZoneHVAC:EquipmentConnections,",
			"  SPACE1-1,                !- Zone Name",
			"  SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
			"  Zone Eq Outlet Node,     !- Zone Air Inlet Node or NodeList Name",
			"  Zone Eq Exhaust Node,    !- Zone Air Exhaust Node or NodeList Name",
			"  SPACE1-1 Node,           !- Zone Air Node Name",
			"  SPACE1-1 Out Node;       !- Zone Return Air Node Name", // not used anywhere else in the example file
			" ",
			"ZoneHVAC:EquipmentList,",
			"  SPACE1-1 Eq,             !- Name",
			"  ZoneHVAC:OutdoorAirUnit, !- Zone Equipment 1 Object Type",
			"  Zone1OutAir,             !- Zone Equipment 1 Name",
			"  1,                       !- Zone Equipment 1 Cooling Sequence",
			"  1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
			" ",
			"ZoneHVAC:OutdoorAirUnit,",
			"  Zone1OutAir,             !- Name",
			"  AvailSched,              !- Availability Schedule Name",
			"  SPACE1-1,                !- Zone Name",
			"  autosize,                !- Outdoor Air Flow Rate{ m3 / s }",
			"  AvailSched,              !- Outdoor Air Schedule Name",
			"  Zone1OAUFan,             !- Supply Fan Name",
			"  DrawThrough,             !- Supply Fan Placement",
			"  Zone1OAUextFan,          !- Exhaust Fan Name",
			"  autosize,                !- Exhaust Air Flow Rate{ m3 / s }",
			"  AvailSched,              !- Exhaust Air Schedule Name",
			"  TemperatureControl,      !- Unit Control Type",
			"  OAUHiCtrlTemp,           !- High Air Control Temperature Schedule Name",
			"  OAULoCtrlTemp,           !- Low Air Control Temperature Schedule Name",
			"  Outside Air Inlet Node 1, !- Outdoor Air Node Name",
			"  Zone Eq Outlet Node,     !- AirOutlet Node Name",
			"  Zone Eq Inlet Node,      !- AirInlet Node Name",
			"  Zone Eq Inlet Node,      !- Supply Fan Outlet Node Name",
			"  Zone1OAEQLIST;           !- Outdoor Air Unit List Name",
			" ",
			"Fan:ConstantVolume,",
			"  Zone1OAUFan,             !- Name",
			"   AvailSched,             !- Availability Schedule Name",
			"   0.5,                    !- Fan Total Efficiency",
			"   75.0,                   !- Pressure Rise{ Pa }",
			"   autosize,               !- Maximum Flow Rate{ m3 / s }",
			"   0.9,                    !- Motor Efficiency",
			"   1.0,                    !- Motor In Airstream Fraction",
			"   Heat Coil Outlet Node,  !- Air Inlet Node Name",
			"   Zone Eq Inlet Node;     !- Air Outlet Node Name",
			" ",
			"Fan:ConstantVolume,",
			"   Zone1OAUextFan,         !- Name",
			"   AvailSched,             !- Availability Schedule Name",
			"   0.5,                    !- Fan Total Efficiency",
			"   75.0,                   !- Pressure Rise{ Pa }",
			"   autosize,               !- Maximum Flow Rate{ m3 / s }",
			"   0.9,                    !- Motor Efficiency",
			"   1.0,                    !- Motor In Airstream Fraction",
			"   Zone Eq Exhaust Node,   !- Air Inlet Node Name",
			"   OutAir1;                !- Air Outlet Node Name",
			" ",
			"ZoneHVAC:OutdoorAirUnit:EquipmentList,",
			"  Zone1OAEQLIST,           !- Name",
			"  CoilSystem:Cooling:DX,   !- Component 1 Object Type",
			"  DX Cooling Coil System 1, !- Component 1 Name",
			"  Coil:Heating:Electric,   !- Component 2 Object Type",
			"  Zone1OAUHeatingCoil;     !- Component 2 Name",
			" ",
			"CoilSystem:Cooling:DX,",
			"  DX Cooling Coil System 1, !- Name",
			"  AvailSched,              !- Availability Schedule Name",
			"  Zone Eq Outlet Node,     !- DX Cooling Coil System Inlet Node Name",
			"  Heat Coil Inlet Node,    !- DX Cooling Coil System Outlet Node Name",
			"  Heat Coil Inlet Node,    !- DX Cooling Coil System Sensor Node Name",
			"  Coil:Cooling:DX:SingleSpeed, !- Cooling Coil Object Type",
			"  ACDXCoil 1;              !- Cooling Coil Name",
			" ",
			"Coil:Cooling:DX:SingleSpeed,",
			"  ACDXCoil 1,              !- Name",
			"  AvailSched,              !- Availability Schedule Name",
			"  autosize,                !- Gross Rated Total Cooling Capacity{ W }",
			"  autosize,                !- Gross Rated Sensible Heat Ratio",
			"  3.0,                     !- Gross Rated Cooling COP{ W / W }",
			"  autosize,                !- Rated Air Flow Rate{ m3 / s }",
			"  ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
			"  Outside Air Inlet Node 1, !- Air Inlet Node Name",
			"  Heat Coil Inlet Node,    !- Air Outlet Node Name",
			"  BiQuadCurve,             !- Total Cooling Capacity Function of Temperature Curve Name",
			"  QuadraticCurve,          !- Total Cooling Capacity Function of Flow Fraction Curve Name",
			"  BiQuadCurve,             !- Energy Input Ratio Function of Temperature Curve Name",
			"  QuadraticCurve,          !- Energy Input Ratio Function of Flow Fraction Curve Name",
			"  QuadraticCurve;          !- Part Load Fraction Correlation Curve Name",
			" ",
			"Coil:Heating:Electric,",
			"  Zone1OAUHeatingCoil, !- Name",
			"  AvailSched, !- Availability Schedule Name",
			"  0.99, !- Efficiency",
			"  autosize, !- Nominal Capacity{ W }",
			"  Heat Coil Inlet Node, !- Air Inlet Node Name",
			"  Heat Coil Outlet Node;               !- Air Outlet Node Name",
			" ",
			"OutdoorAir:NodeList,",
			"  OutsideAirInletNodes;     !- Node or NodeList Name 1",
			" ",
			"NodeList,",
			"  OutsideAirInletNodes, !- Name",
			"  Outside Air Inlet Node 1; !- Node 1 Name",
			" ",
			" ",
			"ScheduleTypeLimits,",
			"  Any Number;              !- Name",
			" ",
			"Schedule:Compact,",
			"  AvailSched,           !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 13",
			"  For: AllDays,            !- Field 14",
			"  Until: 24:00,1.0;        !- Field 15",
			" ",
			"Schedule:Compact,",
			"  OAULoCtrlTemp, !- Name",
			"  Any Number, !- Schedule Type Limits Name",
			"  Through: 12/31, !- Field 1",
			"  For: AllDays, !- Field 2",
			"  Until: 24:00, 10;         !- Field 3",
			" ",
			"Schedule:Compact,",
			"  OAUHiCtrlTemp, !- Name",
			"  Any Number, !- Schedule Type Limits Name",
			"  Through: 12/31, !- Field 1",
			"  For: AllDays, !- Field 2",
			"  Until: 24:00, 15;         !- Field 3",
			" ",
			"Curve:Biquadratic,",
			"  BiQuadCurve,             !- Name",
			"  1.0,                     !- Coefficient1 Constant",
			"  0.0,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 y",
			"  0.0,                     !- Coefficient5 y**2",
			"  0.0,                     !- Coefficient6 x*y",
			"  5,                       !- Minimum Value of x",
			"  36,                      !- Maximum Value of x",
			"  5,                       !- Minimum Value of y",
			"  36,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  QuadraticCurve,          !- Name",
			"  1.0,                     !- Coefficient1 Constant",
			"  0.0,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		DataGlobals::BeginEnvrnFlag = true;
		DataSizing::CurZoneEqNum = 1;
		DataEnvironment::OutBaroPress = 101325; // sea level
		DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
		DataEnvironment::StdRhoAir = PsyRhoAirFnPbTdbW( DataEnvironment::OutBaroPress, 20.0, 0.0 );
		ZoneEqSizing.allocate( 1 );
		ZoneSizingRunDone = true;
		ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent = false;
		ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
		ZoneEqSizing( CurZoneEqNum ).SizingMethod( DataHVACGlobals::SystemAirflowSizing ) = DataSizing::SupplyAirFlowRate;

		ZoneSysEnergyDemand.allocate( 1 );

		ProcessScheduleInput(); // read schedules
		GetCurveInput(); // read curves
		GetZoneData( ErrorsFound ); // read zone data
		EXPECT_FALSE( ErrorsFound ); 

		GetZoneEquipmentData(); // read equipment list and connections

		// Test coil sizing

		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired = 0.0; // set load = 0
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP = 0.0;
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP = 0.0;

		FinalZoneSizing.allocate( 1 );
		FinalZoneSizing( CurZoneEqNum ).MinOA = 0.5;
		FinalZoneSizing( CurZoneEqNum ).ZoneRetTempAtCoolPeak = 26.66667;
		FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak = 0.01117049470250416; // AHRI condition at 80 F db / 67 F wb
		FinalZoneSizing( CurZoneEqNum ).CoolDDNum = 1;
		FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax = 1;
		DesDayWeath.allocate( 1 );
		DesDayWeath( 1 ).Temp.allocate( 1 );
		DesDayWeath( FinalZoneSizing( CurZoneEqNum ).CoolDDNum ).Temp( FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax ) = 35.0;

		FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 13.1; // 55.58 F
		FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.009297628698818194; // humrat at 12.77777 C db / 12.6 C wb

		ZoneInletNode = GetOutdoorAirUnitZoneInletNode( OAUnitNum );

		// schedule values will get reset to 0 if initialized before GetInput
		Schedule( 1 ).CurrentValue = 1.0; // enable the VRF condenser
		Schedule( 2 ).CurrentValue = 1.0; // enable the terminal unit
		Schedule( 3 ).CurrentValue = 1.0; // turn on fan

		SetPredefinedTables();
		SimOutdoorAirUnit( "ZONE1OUTAIR", CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

		EXPECT_DOUBLE_EQ( FinalZoneSizing( CurZoneEqNum ).MinOA, OutAirUnit( OAUnitNum ).OutAirVolFlow );
		EXPECT_DOUBLE_EQ( FinalZoneSizing( CurZoneEqNum ).MinOA * StdRhoAir, OutAirUnit( OAUnitNum ).OutAirMassFlow );
		EXPECT_DOUBLE_EQ( FinalZoneSizing( CurZoneEqNum ).MinOA, OutAirUnit( OAUnitNum ).ExtAirVolFlow );
		EXPECT_DOUBLE_EQ( FinalZoneSizing( CurZoneEqNum ).MinOA * StdRhoAir, OutAirUnit( OAUnitNum ).ExtAirMassFlow );

		// clean up
		StdRhoAir = 0.0;
		ZoneEqSizing.deallocate();
		FinalZoneSizing.deallocate();
		ZoneSysEnergyDemand.deallocate();
		DesDayWeath.deallocate();

	}
}
