// EnergyPlus::WaterThermalTank Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <WaterThermalTanks.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
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


TEST_F( HVACFixture, HPWHZoneEquipSeqenceNumberWarning )
{
	std::string const idf_objects = delimited_string({
		"  Schedule:Constant, DummySch, , 1.0;",
		"  WaterHeater:HeatPump:PumpedCondenser,",
		"    Zone4HeatPumpWaterHeater,!- Name",
		"    ,  !- Availability Schedule Name",
		"    DummySch,             !- Compressor Setpoint Temperature Schedule Name",
		"    2.0,                     !- Dead Band Temperature Difference {deltaC}",
		"    Zone4WaterInletNode,     !- Condenser Water Inlet Node Name",
		"    Zone4WaterOutletNode,    !- Condenser Water Outlet Node Name",
		"    autocalculate,           !- Condenser Water Flow Rate {m3/s}",
		"    autocalculate,           !- Evaporator Air Flow Rate {m3/s}",
		"    ZoneAirOnly,             !- Inlet Air Configuration",
		"    Zone4AirOutletNode,      !- Air Inlet Node Name",
		"    Zone4AirInletNode,       !- Air Outlet Node Name",
		"    ,                        !- Outdoor Air Node Name",
		"    ,                        !- Exhaust Air Node Name",
		"    ,                        !- Inlet Air Temperature Schedule Name",
		"    ,                        !- Inlet Air Humidity Schedule Name",
		"    SPACE4-1,                !- Inlet Air Zone Name",
		"    WaterHeater:Mixed,       !- Tank Object Type",
		"    Zone4HPWHTank,           !- Tank Name",
		"    ,                        !- Tank Use Side Inlet Node Name",
		"    ,                        !- Tank Use Side Outlet Node Name",
		"    Coil:WaterHeating:AirToWaterHeatPump:Pumped,  !- DX Coil Object Type",
		"    Zone4HPWHDXCoil,         !- DX Coil Name",
		"    5.0,                     !- Minimum Inlet Air Temperature for Compressor Operation {C}",
		"    ,                        !- Maximum Inlet Air Temperature for Compressor Operation {C}",
		"    Zone,                    !- Compressor Location",
		"    ,                        !- Compressor Ambient Temperature Schedule Name",
		"    Fan:OnOff,               !- Fan Object Type",
		"    Zone4HPWHFan,            !- Fan Name",
		"    DrawThrough,             !- Fan Placement",
		"    15.0,                    !- On Cycle Parasitic Electric Load {W}",
		"    5.0,                     !- Off Cycle Parasitic Electric Load {W}",
		"    Zone;                    !- Parasitic Heat Rejection Location",
		"  WaterHeater:Mixed,",
		"    Zone4HPWHTank,           !- Name",
		"    0.3785,                  !- Tank Volume {m3}",
		"    DummySch,                !- Setpoint Temperature Schedule Name",
		"    2.0,                     !- Deadband Temperature Difference {deltaC}",
		"    82.2222,                 !- Maximum Temperature Limit {C}",
		"    CYCLE,                   !- Heater Control Type",
		"    5000,                    !- Heater Maximum Capacity {W}",
		"    0,                       !- Heater Minimum Capacity {W}",
		"    ,                        !- Heater Ignition Minimum Flow Rate {m3/s}",
		"    ,                        !- Heater Ignition Delay {s}",
		"    ELECTRICITY,             !- Heater Fuel Type",
		"    0.95,                    !- Heater Thermal Efficiency",
		"    ,                        !- Part Load Factor Curve Name",
		"    10,                      !- Off Cycle Parasitic Fuel Consumption Rate {W}",
		"    ELECTRICITY,             !- Off Cycle Parasitic Fuel Type",
		"    0,                       !- Off Cycle Parasitic Heat Fraction to Tank",
		"    30,                      !- On Cycle Parasitic Fuel Consumption Rate {W}",
		"    ELECTRICITY,             !- On Cycle Parasitic Fuel Type",
		"    0,                       !- On Cycle Parasitic Heat Fraction to Tank",
		"    Schedule,                !- Ambient Temperature Indicator",
		"    DummySch,                !- Ambient Temperature Schedule Name",
		"    ,                        !- Ambient Temperature Zone Name",
		"    ,                        !- Ambient Temperature Outdoor Air Node Name",
		"    2.0,                     !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
		"    1.0,                     !- Off Cycle Loss Fraction to Zone",
		"    2.0,                     !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
		"    1.0,                     !- On Cycle Loss Fraction to Zone",
		"    0.00379,                 !- Peak Use Flow Rate {m3/s}",
		"    DummySch,                !- Use Flow Rate Fraction Schedule Name",
		"    ,                        !- Cold Water Supply Temperature Schedule Name",
		"    ,                        !- Use Side Inlet Node Name",
		"    ,                        !- Use Side Outlet Node Name",
		"    ,                        !- Use Side Effectiveness",
		"    Zone4WaterOutletNode,    !- Source Side Inlet Node Name",
		"    Zone4WaterInletNode,     !- Source Side Outlet Node Name",
		"    0.95;                    !- Source Side Effectiveness",
		"  Coil:WaterHeating:AirToWaterHeatPump:Pumped,",
		"    Zone4HPWHDXCoil,         !- Name",
		"    4000.0,                  !- Rated Heating Capacity {W}",
		"    3.2,                     !- Rated COP {W/W}",
		"    0.6956,                  !- Rated Sensible Heat Ratio",
		"    29.44,                   !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}",
		"    22.22,                   !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}",
		"    55.72,                   !- Rated Condenser Inlet Water Temperature {C}",
		"    autocalculate,           !- Rated Evaporator Air Flow Rate {m3/s}",
		"    autocalculate,           !- Rated Condenser Water Flow Rate {m3/s}",
		"    No,                      !- Evaporator Fan Power Included in Rated COP",
		"    No,                      !- Condenser Pump Power Included in Rated COP",
		"    No,                      !- Condenser Pump Heat Included in Rated Heating Capacity and Rated COP",
		"    150.0,                   !- Condenser Water Pump Power {W}",
		"    0.1,                     !- Fraction of Condenser Pump Heat to Water",
		"    Zone4AirOutletNode,      !- Evaporator Air Inlet Node Name",
		"    Zone4DXCoilAirOutletNode,!- Evaporator Air Outlet Node Name",
		"    Zone4WaterInletNode,     !- Condenser Water Inlet Node Name",
		"    Zone4WaterOutletNode,    !- Condenser Water Outlet Node Name",
		"    100.0,                   !- Crankcase Heater Capacity {W}",
		"    5.0,                     !- Maximum Ambient Temperature for Crankcase Heater Operation {C}",
		"    WetBulbTemperature,      !- Evaporator Air Temperature Type for Curve Objects",
		"    HPWHHeatingCapFTemp,     !- Heating Capacity Function of Temperature Curve Name",
		"    ,                        !- Heating Capacity Function of Air Flow Fraction Curve Name",
		"    ,                        !- Heating Capacity Function of Water Flow Fraction Curve Name",
		"    HPWHHeatingCOPFTemp,     !- Heating COP Function of Temperature Curve Name",
		"    ,                        !- Heating COP Function of Air Flow Fraction Curve Name",
		"    ,                        !- Heating COP Function of Water Flow Fraction Curve Name",
		"    HPWHPLFFPLR;             !- Part Load Fraction Correlation Curve Name",
		"  Fan:OnOff,",
		"    Zone4HPWHFan,            !- Name",
		"    ,  !- Availability Schedule Name",
		"    0.7,                     !- Fan Total Efficiency",
		"    100.0,                   !- Pressure Rise {Pa}",
		"    0.2685,                  !- Maximum Flow Rate {m3/s}",
		"    0.9,                     !- Motor Efficiency",
		"    1.0,                     !- Motor In Airstream Fraction",
		"    Zone4DXCoilAirOutletNode,!- Air Inlet Node Name",
		"    Zone4AirInletNode;       !- Air Outlet Node Name",
		"  Curve:Biquadratic,",
		"    HPWHHeatingCapFTemp,     !- Name",
		"    0.369827,                !- Coefficient1 Constant",
		"    0.043341,                !- Coefficient2 x",
		"    -0.00023,                !- Coefficient3 x**2",
		"    0.000466,                !- Coefficient4 y",
		"    0.000026,                !- Coefficient5 y**2",
		"    -0.00027,                !- Coefficient6 x*y",
		"    0.0,                     !- Minimum Value of x",
		"    40.0,                    !- Maximum Value of x",
		"    20.0,                    !- Minimum Value of y",
		"    90.0,                    !- Maximum Value of y",
		"    ,                        !- Minimum Curve Output",
		"    ,                        !- Maximum Curve Output",
		"    Temperature,             !- Input Unit Type for X",
		"    Temperature,             !- Input Unit Type for Y",
		"    Dimensionless;           !- Output Unit Type",
		"  Curve:Biquadratic,",
		"    HPWHHeatingCOPFTemp,     !- Name",
		"    1.19713,                 !- Coefficient1 Constant",
		"    0.077849,                !- Coefficient2 x",
		"    -0.0000016,              !- Coefficient3 x**2",
		"    -0.02675,                !- Coefficient4 y",
		"    0.000296,                !- Coefficient5 y**2",
		"    -0.00112,                !- Coefficient6 x*y",
		"    0.0,                     !- Minimum Value of x",
		"    40.0,                    !- Maximum Value of x",
		"    20.0,                    !- Minimum Value of y",
		"    90.0,                    !- Maximum Value of y",
		"    ,                        !- Minimum Curve Output",
		"    ,                        !- Maximum Curve Output",
		"    Temperature,             !- Input Unit Type for X",
		"    Temperature,             !- Input Unit Type for Y",
		"    Dimensionless;           !- Output Unit Type",
		"  Curve:Quadratic,",
		"    HPWHPLFFPLR,             !- Name",
		"    0.75,                    !- Coefficient1 Constant",
		"    0.25,                    !- Coefficient2 x",
		"    0.0,                     !- Coefficient3 x**2",
		"    0.0,                     !- Minimum Value of x",
		"    1.0;                     !- Maximum Value of x",
		"  Zone,",
		"    SPACE4-1,                !- Name",
		"    0,                       !- Direction of Relative North {deg}",
		"    0,                       !- X Origin {m}",
		"    0,                       !- Y Origin {m}",
		"    0,                       !- Z Origin {m}",
		"    1,                       !- Type",
		"    1,                       !- Multiplier",
		"    2.438400269,             !- Ceiling Height {m}",
		"    103.311355591;           !- Volume {m3}",
		"  ZoneHVAC:IdealLoadsAirSystem,",
		"    SPACE4-1 AirSys, !- Name",
		"    ,                        !- Availability Schedule Name",
		"    IdealLoadInNode,        !- Zone Supply Air Node Name",
		"    IdealLoadOutNode,                        !- Zone Exhaust Air Node Name",
		"    50,                      !- Maximum Heating Supply Air Temperature {C}",
		"    13,                      !- Minimum Cooling Supply Air Temperature {C}",
		"    0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}",
		"    0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}",
		"    NoLimit,                 !- Heating Limit",
		"    autosize,                !- Maximum Heating Air Flow Rate {m3/s}",
		"    ,                        !- Maximum Sensible Heating Capacity {W}",
		"    NoLimit,                 !- Cooling Limit",
		"    autosize,                !- Maximum Cooling Air Flow Rate {m3/s}",
		"    ,                        !- Maximum Total Cooling Capacity {W}",
		"    ,                        !- Heating Availability Schedule Name",
		"    ,                        !- Cooling Availability Schedule Name",
		"    ConstantSupplyHumidityRatio,  !- Dehumidification Control Type",
		"    ,                        !- Cooling Sensible Heat Ratio {dimensionless}",
		"    ConstantSupplyHumidityRatio,  !- Humidification Control Type",
		"    ,                        !- Design Specification Outdoor Air Object Name",
		"    ,                        !- Outdoor Air Inlet Node Name",
		"    ,                        !- Demand Controlled Ventilation Type",
		"    ,                        !- Outdoor Air Economizer Type",
		"    ,                        !- Heat Recovery Type",
		"    ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}",
		"    ;                        !- Latent Heat Recovery Effectiveness {dimensionless}",
		"  ZoneHVAC:EquipmentConnections,",
		"    SPACE4-1,                !- Zone Name",
		"    SPACE4-1 Eq,             !- Zone Conditioning Equipment List Name",
		"    SPACE4-1 In Nodes,       !- Zone Air Inlet Node or NodeList Name",
		"    SPACE4-1 Out Nodes,      !- Zone Air Exhaust Node or NodeList Name",
		"    SPACE4-1 Node,           !- Zone Air Node Name",
		"    SPACE4-1 Out Node;       !- Zone Return Air Node Name",
		"  ZoneHVAC:EquipmentList,",
		"    SPACE4-1 Eq,             !- Name",
		"    ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type",
		"    SPACE4-1 AirSys,            !- Zone Equipment 1 Name",
		"    1,                       !- Zone Equipment 1 Cooling Sequence",
		"    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
		"    WaterHeater:HeatPump:PumpedCondenser,    !- Zone Equipment 2 Object Type",
		"    Zone4HeatPumpWaterHeater,!- Zone Equipment 2 Name",
		"    2,                       !- Zone Equipment 2 Cooling Sequence",
		"    2;                       !- Zone Equipment 2 Heating or No-Load Sequence",
		"  NodeList,",
		"    SPACE4-1 In Nodes,       !- Name",
		"    SPACE4-1 In Node,        !- Node 1 Name",
		"    Zone4AirInletNode,       !- Node 2 Name",
		"    IdealLoadInNode;       !- Node 2 Name",
		"  NodeList,",
		"    SPACE4-1 Out Nodes,      !- Name",
		"    Zone4AirOutletNode,      !- Node 1 Name",
		"    IdealLoadOutNode;      !- Node 1 Name",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );

	bool ErrorsFound = false;
	HeatBalanceManager::GetZoneData( ErrorsFound );
	ASSERT_FALSE( ErrorsFound );
	EXPECT_FALSE( WaterThermalTanks::GetWaterThermalTankInput() );

}
