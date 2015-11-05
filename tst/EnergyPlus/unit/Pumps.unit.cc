// EnergyPlus::DataPlant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include <Pumps.hh>
#include <SizingManager.hh>
#include <DataPlant.hh>

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, HeaderedConstantSpeedPumpSizingPowerTest ) {
		std::string const idf_objects = delimited_string( {
		"Version,8.5;",

		"HeaderedPumps:ConstantSpeed,",
		"Chilled Water Headered Pumps,  !- Name",
		"CW Supply Inlet Node,    !- Inlet Node Name",
		"CW Pumps Outlet Node,    !- Outlet Node Name",
		"0.001,                   !- Total Rated Flow Rate {m3/s}",
		"2,                       !- Number of Pumps in Bank",
		"SEQUENTIAL,              !- Flow Sequencing Control Scheme",
		"100000,                  !- Rated Pump Head {Pa}",
		"autosize,                !- Rated Power Consumption {W}",
		"0.8,                     !- Motor Efficiency",
		"0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"INTERMITTENT,            !- Pump Control Type",
		"CoolingPumpAvailSched,   !- Pump Flow Rate Schedule Name",
		",                        !- Zone Name",
		",                        !- Skin Loss Radiative Fraction",
		"PowerPerFlowPerPressure, !- Design Power Sizing Method",
		",                        !- Design Electric Power per Unit Flow Rate",
		"1.3;                     !- Design Shaft Power per Unit Flow Rate per Unit Head",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		Pumps::GetPumpInput();
		Pumps::SizePump( 1 );
		EXPECT_NEAR(Pumps::PumpEquip( 1 ).NomPowerUse, 162.5, 0.0001 );
	}

	TEST_F( EnergyPlusFixture, HeaderedConstantSpeedPumpSizingPower19W_per_gpm ) {
		std::string const idf_objects = delimited_string( {
		"Version,8.5;",

		"HeaderedPumps:ConstantSpeed,",
		"Chilled Water Headered Pumps,  !- Name",
		"CW Supply Inlet Node,    !- Inlet Node Name",
		"CW Pumps Outlet Node,    !- Outlet Node Name",
		"0.001,                   !- Total Rated Flow Rate {m3/s}",
		"2,                       !- Number of Pumps in Bank",
		"SEQUENTIAL,              !- Flow Sequencing Control Scheme",
		",                        !- Rated Pump Head {Pa}",
		"autosize,                !- Rated Power Consumption {W}",
		",                        !- Motor Efficiency",
		"0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"INTERMITTENT,            !- Pump Control Type",
		"CoolingPumpAvailSched,   !- Pump Flow Rate Schedule Name",
		",                        !- Zone Name",
		",                        !- Skin Loss Radiative Fraction",
		"PowerPerFlow,            !- Design Power Sizing Method",
		"301156.1,                !- Design Electric Power per Unit Flow Rate",
		";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		Pumps::GetPumpInput();
		Pumps::SizePump( 1 );
		EXPECT_NEAR(Pumps::PumpEquip( 1 ).NomPowerUse, 301.1561, 0.0001 );
	}

	TEST_F( EnergyPlusFixture, HeaderedConstantSpeedPumpSizingPowerDefault ) {
		std::string const idf_objects = delimited_string( {
		"Version,8.5;",

		"HeaderedPumps:ConstantSpeed,",
		"Chilled Water Headered Pumps,  !- Name",
		"CW Supply Inlet Node,    !- Inlet Node Name",
		"CW Pumps Outlet Node,    !- Outlet Node Name",
		"0.001,                   !- Total Rated Flow Rate {m3/s}",
		"2,                       !- Number of Pumps in Bank",
		"SEQUENTIAL,              !- Flow Sequencing Control Scheme",
		",                        !- Rated Pump Head {Pa}",
		"autosize,                !- Rated Power Consumption {W}",
		",                        !- Motor Efficiency",
		"0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"INTERMITTENT,            !- Pump Control Type",
		"CoolingPumpAvailSched,   !- Pump Flow Rate Schedule Name",
		",                        !- Zone Name",
		",                        !- Skin Loss Radiative Fraction",
		",                        !- Design Power Sizing Method",
		",                        !- Design Electric Power per Unit Flow Rate",
		";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		Pumps::GetPumpInput();
		Pumps::SizePump( 1 );
		EXPECT_NEAR(Pumps::PumpEquip( 1 ).NomPowerUse, 255.4872, 0.0001 );
	}

	TEST_F( EnergyPlusFixture, VariableSpeedPumpSizingPowerPerPressureTest ) {
		std::string const idf_objects = delimited_string( {
		"Version,8.5;",

		"Pump:VariableSpeed,",
		"CoolSys1 Pump,           !- Name",
		"CoolSys1 Supply Inlet Node,  !- Inlet Node Name",
		"CoolSys1 Pump-CoolSys1 ChillerNodeviaConnector,  !- Outlet Node Name",
		"0.001,                !- Rated Flow Rate {m3/s}",
		"100000,                  !- Rated Pump Head {Pa}",
		"AUTOSIZE,                !- Rated Power Consumption {W}",
		"0.8,                     !- Motor Efficiency",
		"0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"0,                       !- Coefficient 1 of the Part Load Performance Curve",
		"1,                       !- Coefficient 2 of the Part Load Performance Curve",
		"0,                       !- Coefficient 3 of the Part Load Performance Curve",
		"0,                       !- Coefficient 4 of the Part Load Performance Curve",
		"autosize,                !- Minimum Flow Rate {m3/s}",
		"Intermittent,            !- Pump Control Type",
		",                        !- Pump Flow Rate Schedule Name",
		",                        !- Pump Curve Name",
		",                        !- Impeller Diameter",
		",                        !- VFD Control Type",
		",                        !- Pump rpm Schedule Name",
		",                        !- Minimum Pressure Schedule",
		",                        !- Maximum Pressure Schedule",
		",                        !- Minimum RPM Schedule",
		",                        !- Maximum RPM Schedule",
		",                        !- Zone Name",
		",                        !- Skin Loss Radiative Fraction",
		"PowerPerFlowPerPressure, !- Design Power Sizing Method",
		",                        !- Design Electric Power per Unit Flow Rate",
		"1.3,                     !- Design Shaft Power per Unit Flow Rate per Unit Head",
		";                        !- Design Minimum Flow Rate Sizing Factor",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		Pumps::GetPumpInput();
		Pumps::SizePump( 1 );
		EXPECT_NEAR(Pumps::PumpEquip( 1 ).NomPowerUse, 162.5, 0.0001 );
	}

	TEST_F( EnergyPlusFixture, VariableSpeedPumpSizingPowerDefault ) {
		std::string const idf_objects = delimited_string( {
		"Version,8.5;",

		"Pump:VariableSpeed,",
		"CoolSys1 Pump,           !- Name",
		"CoolSys1 Supply Inlet Node,  !- Inlet Node Name",
		"CoolSys1 Pump-CoolSys1 ChillerNodeviaConnector,  !- Outlet Node Name",
		"0.001,                   !- Rated Flow Rate {m3/s}",
		",                        !- Rated Pump Head {Pa}",
		"AUTOSIZE,                !- Rated Power Consumption {W}",
		",                        !- Motor Efficiency",
		"0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"0,                       !- Coefficient 1 of the Part Load Performance Curve",
		"1,                       !- Coefficient 2 of the Part Load Performance Curve",
		"0,                       !- Coefficient 3 of the Part Load Performance Curve",
		"0,                       !- Coefficient 4 of the Part Load Performance Curve",
		"autosize,                !- Minimum Flow Rate {m3/s}",
		"Intermittent,            !- Pump Control Type",
		",                        !- Pump Flow Rate Schedule Name",
		",                        !- Pump Curve Name",
		",                        !- Impeller Diameter",
		",                        !- VFD Control Type",
		",                        !- Pump rpm Schedule Name",
		",                        !- Minimum Pressure Schedule",
		",                        !- Maximum Pressure Schedule",
		",                        !- Minimum RPM Schedule",
		",                        !- Maximum RPM Schedule",
		",                        !- Zone Name",
		",                        !- Skin Loss Radiative Fraction",
		",                        !- Design Power Sizing Method",
		",                        !- Design Electric Power per Unit Flow Rate",
		",                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
		";                        !- Design Minimum Flow Rate Sizing Factor",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		Pumps::GetPumpInput();
		Pumps::SizePump( 1 );
		EXPECT_NEAR(Pumps::PumpEquip( 1 ).NomPowerUse, 255.4872, 0.0001 );
	}

	TEST_F( EnergyPlusFixture, VariableSpeedPumpSizingPower22W_per_GPM ) {
		std::string const idf_objects = delimited_string( {
		"Version,8.5;",

		"Pump:VariableSpeed,",
		"CoolSys1 Pump,           !- Name",
		"CoolSys1 Supply Inlet Node,  !- Inlet Node Name",
		"CoolSys1 Pump-CoolSys1 ChillerNodeviaConnector,  !- Outlet Node Name",
		"0.001,                   !- Rated Flow Rate {m3/s}",
		"179352,                  !- Rated Pump Head {Pa}",
		"AUTOSIZE,                !- Rated Power Consumption {W}",
		"0.9,                     !- Motor Efficiency",
		"0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"0,                       !- Coefficient 1 of the Part Load Performance Curve",
		"1,                       !- Coefficient 2 of the Part Load Performance Curve",
		"0,                       !- Coefficient 3 of the Part Load Performance Curve",
		"0,                       !- Coefficient 4 of the Part Load Performance Curve",
		"autosize,                !- Minimum Flow Rate {m3/s}",
		"Intermittent,            !- Pump Control Type",
		",                        !- Pump Flow Rate Schedule Name",
		",                        !- Pump Curve Name",
		",                        !- Impeller Diameter",
		",                        !- VFD Control Type",
		",                        !- Pump rpm Schedule Name",
		",                        !- Minimum Pressure Schedule",
		",                        !- Maximum Pressure Schedule",
		",                        !- Minimum RPM Schedule",
		",                        !- Maximum RPM Schedule",
		",                        !- Zone Name",
		",                        !- Skin Loss Radiative Fraction",
		"PowerPerFlow,            !- Design Power Sizing Method",
		",                        !- Design Electric Power per Unit Flow Rate",
		",                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
		"0.0;                     !- Design Minimum Flow Rate Sizing Factor",
		} );
		ASSERT_FALSE( process_idf( idf_objects ) );
		Pumps::GetPumpInput();
		Pumps::SizePump( 1 );
		EXPECT_NEAR(Pumps::PumpEquip( 1 ).NomPowerUse, 348.7011, 0.0001 );
	}

	TEST_F( EnergyPlusFixture, ConstantSpeedPumpSizingPower19W_per_gpm ) {
		std::string const idf_objects = delimited_string( {
		"Version,8.5;",
	
		"Pump:ConstantSpeed,",
		"TowerWaterSys Pump,      !- Name",
		"TowerWaterSys Supply Inlet Node,  !- Inlet Node Name",
		"TowerWaterSys Pump-TowerWaterSys CoolTowerNodeviaConnector,  !- Outlet Node Name",
		"0.001,                   !- Rated Flow Rate {m3/s}",
		"179352,                  !- Rated Pump Head {Pa}",
		"AUTOSIZE,                !- Rated Power Consumption {W}",
		"0.87,                    !- Motor Efficiency",
		"0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"Intermittent,            !- Pump Control Type",
		",                        !- Pump Flow Rate Schedule Name",
		",                        !- Pump Curve Name",
		",                        !- Impeller Diameter",
		",                        !- Rotational Speed",
		",                        !- Zone Name",
		",                        !- Skin Loss Radiative Fraction",
		"PowerPerFlow,            !- Design Power Sizing Method",
		"301156.1,                !- Design Electric Power per Unit Flow Rate",
		";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		Pumps::GetPumpInput();
		Pumps::SizePump( 1 );
		EXPECT_NEAR(Pumps::PumpEquip( 1 ).NomPowerUse, 301.1561, 0.0001 );
	}

	TEST_F( EnergyPlusFixture, ConstantSpeedPumpSizingPowerPerPressureTest ) {
		std::string const idf_objects = delimited_string( { 
		"Version,8.5;",
	
		"Pump:ConstantSpeed,",
		"TowerWaterSys Pump,      !- Name",
		"TowerWaterSys Supply Inlet Node,  !- Inlet Node Name",
		"TowerWaterSys Pump-TowerWaterSys CoolTowerNodeviaConnector,  !- Outlet Node Name",
		"0.001,                   !- Rated Flow Rate {m3/s}",
		"100000,                  !- Rated Pump Head {Pa}",
		"AUTOSIZE,                !- Rated Power Consumption {W}",
		"0.8,                     !- Motor Efficiency",
		"0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"Intermittent,            !- Pump Control Type",
		",                        !- Pump Flow Rate Schedule Name",
		",                        !- Pump Curve Name",
		",                        !- Impeller Diameter",
		",                        !- Rotational Speed",
		",                        !- Zone Name",
		",                        !- Skin Loss Radiative Fraction",
		"PowerPerFlowPerPressure, !- Design Power Sizing Method",
		",                        !- Design Electric Power per Unit Flow Rate",
		"1.3;                     !- Design Shaft Power per Unit Flow Rate per Unit Head",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		Pumps::GetPumpInput();
		Pumps::SizePump( 1 );
		EXPECT_NEAR(Pumps::PumpEquip( 1 ).NomPowerUse, 162.5, 0.0001 );
	}

	TEST_F( EnergyPlusFixture, ConstantSpeedPumpSizingPowerDefaults ) {
		std::string const idf_objects = delimited_string( { 
		"Version,8.5;",
	
		"Pump:ConstantSpeed,",
		"TowerWaterSys Pump,      !- Name",
		"TowerWaterSys Supply Inlet Node,  !- Inlet Node Name",
		"TowerWaterSys Pump-TowerWaterSys CoolTowerNodeviaConnector,  !- Outlet Node Name",
		"0.001,                   !- Rated Flow Rate {m3/s}",
		",                        !- Rated Pump Head {Pa}",
		"AUTOSIZE,                !- Rated Power Consumption {W}",
		",                        !- Motor Efficiency",
		"0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream",
		"Intermittent,            !- Pump Control Type",
		",                        !- Pump Flow Rate Schedule Name",
		",                        !- Pump Curve Name",
		",                        !- Impeller Diameter",
		",                        !- Rotational Speed",
		",                        !- Zone Name",
		",                        !- Skin Loss Radiative Fraction",
		",                        !- Design Power Sizing Method",
		",                        !- Design Electric Power per Unit Flow Rate",
		";                        !- Design Shaft Power per Unit Flow Rate per Unit Head",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		Pumps::GetPumpInput();
		Pumps::SizePump( 1 );
		EXPECT_NEAR(Pumps::PumpEquip( 1 ).NomPowerUse, 255.4872, 0.0001 );
	}


}