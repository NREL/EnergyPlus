// EnergyPlus::WaterThermalTank Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <WaterThermalTanks.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include "Fixtures/HVACFixture.hh"

using namespace EnergyPlus;

TEST( HeatPumpWaterHeaterTests, TestQsourceCalcs )
{
	ShowMessage( "Begin Test: HeatPumpWaterHeaterTests, TestQsourceCalcs" );
	Real64 DeltaT = 0.0;
	Real64 const SourceInletTemp = 62.0;
	Real64 const Cp = 4178.; // water, J/(kg * K)
	Real64 const SetPointTemp = 60.0;
	Real64 const SourceMassFlowRateOrig = 0.378529822165; // water, 6 gal/min
	Real64 SourceMassFlowRate = SourceMassFlowRateOrig;
	Real64 Qheatpump = 0.0;
	Real64 Qsource = 0.0;
	
	// Mixed Tank
	
	// Test case without HPWH
	WaterThermalTanks::CalcMixedTankSourceSideHeatTransferRate(DeltaT, SourceInletTemp, Cp, SetPointTemp, SourceMassFlowRate, Qheatpump, Qsource);
	// Qsource is non zero and calculated relative to the tank setpoint.
	EXPECT_DOUBLE_EQ(SourceMassFlowRate * Cp * (SourceInletTemp - SetPointTemp), Qsource);
	// Qheatpump is zero
	EXPECT_DOUBLE_EQ(Qheatpump, 0.0);
	// SourceMassFlowRate is unchanged
	EXPECT_DOUBLE_EQ(SourceMassFlowRateOrig, SourceMassFlowRate);
	
	// Test case with HPWH
	DeltaT = 5.0;
	WaterThermalTanks::CalcMixedTankSourceSideHeatTransferRate(DeltaT, SourceInletTemp, Cp, SetPointTemp, SourceMassFlowRate, Qheatpump, Qsource);
	// Qsource is Qheatpump
	EXPECT_DOUBLE_EQ(Qsource, Qheatpump);
	// Qheatpump is the heat transfer rate from the input DeltaT
	EXPECT_DOUBLE_EQ(SourceMassFlowRateOrig * Cp * DeltaT, Qheatpump);
	// SourceMassFlowRate is zero
	EXPECT_DOUBLE_EQ(SourceMassFlowRate, 0.0);
	
	// Stratified Tank
	SourceMassFlowRate = SourceMassFlowRateOrig;
	Real64 const NodeTemp = 58.0;
	
	WaterThermalTanks::StratifiedNodeData StratNode;
	StratNode.Temp = 58.0;
	StratNode.HPWHWrappedCondenserHeatingFrac = 0.5;
	
	// Test case without HPWH
	Qheatpump = 0.0;
	Qsource = WaterThermalTanks::CalcStratifiedTankSourceSideHeatTransferRate(Qheatpump, SourceInletTemp, Cp, SourceMassFlowRate, StratNode);
	EXPECT_DOUBLE_EQ(Qsource, SourceMassFlowRate * Cp * (SourceInletTemp - NodeTemp));
	
	// Test case with Pumped HPWH
	Qheatpump = 100.0;
	Qsource = WaterThermalTanks::CalcStratifiedTankSourceSideHeatTransferRate(Qheatpump, SourceInletTemp, Cp, SourceMassFlowRate, StratNode);
	EXPECT_DOUBLE_EQ(Qsource, Qheatpump);
	
	// Test case with Wrapped HPWH
	SourceMassFlowRate = 0.0;
	Qsource = WaterThermalTanks::CalcStratifiedTankSourceSideHeatTransferRate(Qheatpump, SourceInletTemp, Cp, SourceMassFlowRate, StratNode);
	EXPECT_DOUBLE_EQ(Qsource, Qheatpump * StratNode.HPWHWrappedCondenserHeatingFrac );
	
}

TEST( WaterThermalTankData, GetDeadBandTemp )
{

	ShowMessage( "Begin Test: WaterThermalTankData, GetDeadBandTemp" );
	WaterThermalTanks::WaterThermalTankData thisTank;
	thisTank.SetPointTemp = 10;
	thisTank.DeadBandDeltaTemp = 1;

	// first the hot water tank
	thisTank.IsChilledWaterTank = false;
	EXPECT_DOUBLE_EQ( 9.0,  thisTank.getDeadBandTemp() );

	// then the chilled water tank
	thisTank.IsChilledWaterTank = true;
	EXPECT_DOUBLE_EQ( 11.0, thisTank.getDeadBandTemp() );

}

TEST_F( HVACFixture, HPWHWrappedDummyNodeConfig ) {

	// Unit test for #5127

	std::vector<std::string> idf_lines({
		"Version, 8.4;",
		"Schedule:Constant,DummySch,,1.0;",
		"Curve:Biquadratic,",
		"    HPWH-Htg-Cap-fT,         !- Name",
		"    0.563,                   !- Coefficient1 Constant",
		"    0.0437,                  !- Coefficient2 x",
		"    0.000039,                !- Coefficient3 x**2",
		"    0.0055,                  !- Coefficient4 y",
		"    -.000148,                !- Coefficient5 y**2",
		"    -.000145,                !- Coefficient6 x*y",
		"    0,                       !- Minimum Value of x",
		"    100,                     !- Maximum Value of x",
		"    0,                       !- Minimum Value of y",
		"    100,                     !- Maximum Value of y",
		"    0;                       !- Minimum Curve Output",
		"Curve:Biquadratic,",
		"    HPWH-Htg-COP-fT,         !- Name",
		"    1.1332,                  !- Coefficient1 Constant",
		"    0.063,                   !- Coefficient2 x",
		"    -.0000979,               !- Coefficient3 x**2",
		"    -.00972,                 !- Coefficient4 y",
		"    -.0000214,               !- Coefficient5 y**2",
		"    -.000686,                !- Coefficient6 x*y",
		"    0,                       !- Minimum Value of x",
		"    100,                     !- Maximum Value of x",
		"    0,                       !- Minimum Value of y",
		"    100;                     !- Maximum Value of y",
		"Curve:Quadratic,",
		"    HPWH-COP-fPLR,           !- Name",
		"    1,                       !- Coefficient1 Constant",
		"    0,                       !- Coefficient2 x",
		"    0,                       !- Coefficient3 x**2",
		"    0,                       !- Minimum Value of x",
		"    1;                       !- Maximum Value of x"
	});
	for ( int i = 1; i <= 2; ++i ) {
		std::string const i_str = std::to_string(i);
		idf_lines.push_back("Coil:WaterHeating:AirToWaterHeatPump:Wrapped,");
		idf_lines.push_back("    HPWH Coil " + i_str + ",               !- Name");
		idf_lines.push_back("    2349.6,                  !- Rated Heating Capacity {W}");
		idf_lines.push_back("    2.4,                     !- Rated COP {W/W}");
		idf_lines.push_back("    0.981,                   !- Rated Sensible Heat Ratio");
		idf_lines.push_back("    19.72,                   !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}");
		idf_lines.push_back("    13.5,                    !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}");
		idf_lines.push_back("    48.89,                   !- Rated Condenser Water Temperature {C}");
		idf_lines.push_back("    0.189,                   !- Rated Evaporator Air Flow Rate {m3/s}");
		idf_lines.push_back("    Yes,                     !- Evaporator Fan Power Included in Rated COP");
		idf_lines.push_back("    HPWH Air Inlet " + i_str + ",          !- Evaporator Air Inlet Node Name");
		idf_lines.push_back("    HPWH Coil Outlet Fan Inlet " + i_str + ",  !- Evaporator Air Outlet Node Name");
		idf_lines.push_back("    0,                       !- Crankcase Heater Capacity {W}");
		idf_lines.push_back("    10,                      !- Maximum Ambient Temperature for Crankcase Heater Operation {C}");
		idf_lines.push_back("    WetBulbTemperature,      !- Evaporator Air Temperature Type for Curve Objects");
		idf_lines.push_back("    HPWH-Htg-Cap-fT,         !- Heating Capacity Function of Temperature Curve Name");
		idf_lines.push_back("    ,                        !- Heating Capacity Function of Air Flow Fraction Curve Name");
		idf_lines.push_back("    HPWH-Htg-COP-fT,         !- Heating COP Function of Temperature Curve Name");
		idf_lines.push_back("    ,                        !- Heating COP Function of Air Flow Fraction Curve Name");
		idf_lines.push_back("    HPWH-COP-fPLR;           !- Part Load Fraction Correlation Curve Name");
		idf_lines.push_back("Fan:OnOff,");
		idf_lines.push_back("    HPWH Fan " + i_str + ",                !- Name");
		idf_lines.push_back("    DummySch,                !- Availability Schedule Name");
		idf_lines.push_back("    0.1722,                   !- Fan Total Efficiency");
		idf_lines.push_back("    65,                     !- Pressure Rise {Pa}");
		idf_lines.push_back("    0.2279,                   !- Maximum Flow Rate {m3/s}");
		idf_lines.push_back("    1,                       !- Motor Efficiency");
		idf_lines.push_back("    0,                       !- Motor In Airstream Fraction");
		idf_lines.push_back("    HPWH Coil Outlet Fan Inlet " + i_str + ",  !- Air Inlet Node Name");
		idf_lines.push_back("    HPWH Air Outlet " + i_str + ",         !- Air Outlet Node Name");
		idf_lines.push_back("    ,                        !- Fan Power Ratio Function of Speed Ratio Curve Name");
		idf_lines.push_back("    ,                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name");
		idf_lines.push_back("    Water Heater;            !- End-Use Subcategory");
		idf_lines.push_back("WaterHeater:Stratified,");
		idf_lines.push_back("    HPWH Tank " + i_str + ",               !- Name");
		idf_lines.push_back("    Water Heater,            !- End-Use Subcategory");
		idf_lines.push_back("    0.287691,                !- Tank Volume {m3}");
		idf_lines.push_back("    1.594,                   !- Tank Height {m}");
		idf_lines.push_back("    VerticalCylinder,        !- Tank Shape");
		idf_lines.push_back("    ,                        !- Tank Perimeter {m}");
		idf_lines.push_back("    100,                     !- Maximum Temperature Limit {C}");
		idf_lines.push_back("    MasterSlave,             !- Heater Priority Control");
		idf_lines.push_back("    DummySch,                !- Heater 1 Setpoint Temperature Schedule Name");
		idf_lines.push_back("    18.5,                    !- Heater 1 Deadband Temperature Difference {deltaC}");
		idf_lines.push_back("    4500,                    !- Heater 1 Capacity {W}");
		idf_lines.push_back("    1.129,                   !- Heater 1 Height {m}");
		idf_lines.push_back("    DummySch,                !- Heater 2 Setpoint Temperature Schedule Name");
		idf_lines.push_back("    18.5,                    !- Heater 2 Deadband Temperature Difference {deltaC}");
		idf_lines.push_back("    0,                       !- Heater 2 Capacity {W}");
		idf_lines.push_back("    0.266,                   !- Heater 2 Height {m}");
		idf_lines.push_back("    Electricity,             !- Heater Fuel Type");
		idf_lines.push_back("    1,                       !- Heater Thermal Efficiency");
		idf_lines.push_back("    8.3,                     !- Off Cycle Parasitic Fuel Consumption Rate {W}");
		idf_lines.push_back("    Electricity,             !- Off Cycle Parasitic Fuel Type");
		idf_lines.push_back("    0,                       !- Off Cycle Parasitic Heat Fraction to Tank");
		idf_lines.push_back("    1,                       !- Off Cycle Parasitic Height {m}");
		idf_lines.push_back("    8.3,                     !- On Cycle Parasitic Fuel Consumption Rate {W}");
		idf_lines.push_back("    Electricity,             !- On Cycle Parasitic Fuel Type");
		idf_lines.push_back("    0,                       !- On Cycle Parasitic Heat Fraction to Tank");
		idf_lines.push_back("    1,                       !- On Cycle Parasitic Height {m}");
		idf_lines.push_back("    Schedule,                !- Ambient Temperature Indicator");
		idf_lines.push_back("    DummySch,                !- Ambient Temperature Schedule Name");
		idf_lines.push_back("    ,                        !- Ambient Temperature Zone Name");
		idf_lines.push_back("    ,                        !- Ambient Temperature Outdoor Air Node Name");
		idf_lines.push_back("    0.7878,                  !- Uniform Skin Loss Coefficient per Unit Area to Ambient Temperature {W/m2-K}");
		idf_lines.push_back("    1,                       !- Skin Loss Fraction to Zone");
		idf_lines.push_back("    ,                        !- Off Cycle Flue Loss Coefficient to Ambient Temperature {W/K}");
		idf_lines.push_back("    1,                       !- Off Cycle Flue Loss Fraction to Zone");
		idf_lines.push_back("    0.001,                   !- Peak Use Flow Rate {m3/s}");
		idf_lines.push_back("    DummySch,                !- Use Flow Rate Fraction Schedule Name");
		idf_lines.push_back("    DummySch,                !- Cold Water Supply Temperature Schedule Name");
		idf_lines.push_back("    ,                        !- Use Side Inlet Node Name");
		idf_lines.push_back("    ,                        !- Use Side Outlet Node Name");
		idf_lines.push_back("    1,                       !- Use Side Effectiveness");
		idf_lines.push_back("    0,                       !- Use Side Inlet Height {m}");
		idf_lines.push_back("    autocalculate,           !- Use Side Outlet Height {m}");
		idf_lines.push_back("    ,                        !- Source Side Inlet Node Name");
		idf_lines.push_back("    ,                        !- Source Side Outlet Node Name");
		idf_lines.push_back("    1,                       !- Source Side Effectiveness");
		idf_lines.push_back("    0.7,                     !- Source Side Inlet Height {m}");
		idf_lines.push_back("    0,                       !- Source Side Outlet Height {m}");
		idf_lines.push_back("    Fixed,                   !- Inlet Mode");
		idf_lines.push_back("    autosize,                !- Use Side Design Flow Rate {m3/s}");
		idf_lines.push_back("    autosize,                !- Source Side Design Flow Rate {m3/s}");
		idf_lines.push_back("    1.5,                     !- Indirect Water Heating Recovery Time {hr}");
		idf_lines.push_back("    12;                      !- Number of Nodes");
		idf_lines.push_back("WaterHeater:HeatPump:WrappedCondenser,");
		idf_lines.push_back("    HPWH " + i_str + ",                    !- Name");
		idf_lines.push_back("    DummySch,                !- Availability Schedule Name");
		idf_lines.push_back("    DummySch,                !- Compressor Setpoint Temperature Schedule Name");
		idf_lines.push_back("    3.89,                    !- Dead Band Temperature Difference {deltaC}");
		idf_lines.push_back("    0.0664166667,            !- Condenser Bottom Location");
		idf_lines.push_back("    0.8634166667,            !- Condenser Top Location");
		idf_lines.push_back("    0.2279,                  !- Evaporator Air Flow Rate {m3/s}");
		idf_lines.push_back("    Schedule,                !- Inlet Air Configuration");
		idf_lines.push_back("    HPWH Air Inlet " + i_str + ",          !- Air Inlet Node Name");
		idf_lines.push_back("    HPWH Air Outlet " + i_str + ",         !- Air Outlet Node Name");
		idf_lines.push_back("    ,                        !- Outdoor Air Node Name");
		idf_lines.push_back("    ,                        !- Exhaust Air Node Name");
		idf_lines.push_back("    DummySch,                !- Inlet Air Temperature Schedule Name");
		idf_lines.push_back("    DummySch,                !- Inlet Air Humidity Schedule Name");
		idf_lines.push_back("    ,                        !- Inlet Air Zone Name");
		idf_lines.push_back("    WaterHeater:Stratified,  !- Tank Object Type");
		idf_lines.push_back("    HPWH Tank " + i_str + ",               !- Tank Name");
		idf_lines.push_back("    ,                        !- Tank Use Side Inlet Node Name");
		idf_lines.push_back("    ,                        !- Tank Use Side Outlet Node Name");
		idf_lines.push_back("    Coil:WaterHeating:AirToWaterHeatPump:Wrapped,   !- DX Coil Object Type");
		idf_lines.push_back("    HPWH Coil " + i_str + ",               !- DX Coil Name");
		idf_lines.push_back("    7.2,                     !- Minimum Inlet Air Temperature for Compressor Operation {C}");
		idf_lines.push_back("    ,                        !- Maximum Inlet Air Temperature for Compressor Operation {C}");
		idf_lines.push_back("    Schedule,                !- Compressor Location");
		idf_lines.push_back("    DummySch,                !- Compressor Ambient Temperature Schedule Name");
		idf_lines.push_back("    Fan:OnOff,               !- Fan Object Type");
		idf_lines.push_back("    HPWH Fan " + i_str + ",                !- Fan Name");
		idf_lines.push_back("    DrawThrough,             !- Fan Placement");
		idf_lines.push_back("    0,                       !- On Cycle Parasitic Electric Load {W}");
		idf_lines.push_back("    0,                       !- Off Cycle Parasitic Electric Load {W}");
		idf_lines.push_back("    Outdoors,                !- Parasitic Heat Rejection Location");
		idf_lines.push_back("    ,                        !- Inlet Air Mixer Node Name");
		idf_lines.push_back("    ,                        !- Outlet Air Splitter Node Name");
		idf_lines.push_back("    ,                        !- Inlet Air Mixer Schedule Name");
		idf_lines.push_back("    MutuallyExclusive,       !- Tank Element Control Logic");
		idf_lines.push_back("    1.262,                   !- Control Sensor 1 Height In Stratified Tank");
		idf_lines.push_back("    0.75,                    !- Control Sensor 1 Weight");
		idf_lines.push_back("    0.464;                   !- Control Sensor 2 Height In Stratified Tank");
	}
	std::string const idf_objects = delimited_string(idf_lines);

	ASSERT_FALSE( process_idf( idf_objects ) );

	WaterThermalTanks::GetWaterThermalTankInput();

	for ( int i = 1; i <= WaterThermalTanks::NumHeatPumpWaterHeater; ++i ) {
		auto const & HPWH = WaterThermalTanks::HPWaterHeater( i );
		auto const & Tank = WaterThermalTanks::WaterThermalTank( HPWH.WaterHeaterTankNum );
		EXPECT_EQ( HPWH.CondWaterInletNode, Tank.SourceOutletNode );
		EXPECT_EQ( HPWH.CondWaterOutletNode, Tank.SourceInletNode );
	}

}
