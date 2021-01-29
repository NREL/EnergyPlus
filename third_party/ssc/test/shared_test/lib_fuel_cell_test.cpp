#include "lib_fuel_cell_test.h"

TEST_F(FuelCellTest, UnitConversions_lib_fuel_cell)
{
	// Test macros defined for unit conversions
	double lhv_btu_per_ft3 = 983;
	EXPECT_NEAR(BTU_TO_MCF(1000000, lhv_btu_per_ft3), 1.017, 0.001);
	EXPECT_NEAR(MCF_TO_BTU(1, lhv_btu_per_ft3), 983000, 0.01 );
	EXPECT_NEAR(MCF_TO_KWH(1, lhv_btu_per_ft3), 288.088, 0.01);
}

TEST_F(FuelCellTest, EfficiencyCurve_lib_fuel_cell)
{
	fuelCell->calculateEfficiencyCurve(.16);
	EXPECT_EQ(fuelCell->getElectricalEfficiency(), 0.21);
	EXPECT_EQ(fuelCell->getHeatRecoveryEfficiency(), .50);
}

TEST_F(FuelCellTest, FuelConsumption_lib_fuel_cell)
{
	fuelCell->calculateEfficiencyCurve(.16);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.251, 0.01);
	fuelCell->calculateEfficiencyCurve(.25);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.330, 0.01);
	fuelCell->calculateEfficiencyCurve(.30);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.341, 0.01);
	fuelCell->calculateEfficiencyCurve(.34);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.351, 0.01);
	fuelCell->calculateEfficiencyCurve(.44);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.393, 0.01);
	fuelCell->calculateEfficiencyCurve(.53);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.417, 0.01);
	fuelCell->calculateEfficiencyCurve(.62);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.436, 0.01);
	fuelCell->calculateEfficiencyCurve(.72);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.476, 0.01);
	fuelCell->calculateEfficiencyCurve(.82);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.521, 0.01);
	fuelCell->calculateEfficiencyCurve(.9);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.572, 0.01);
	fuelCell->calculateEfficiencyCurve(1);
	EXPECT_NEAR(fuelCell->getFuelConsumption(), 0.648, 0.01);
}

TEST_F(FuelCellTest, Initialize_lib_fuel_cell)
{
	// Test if started up
	EXPECT_EQ(fuelCell->isRunning(), false);
}

TEST_F(FuelCellTest, Startup_lib_fuel_cell)
{
	// Run for startup_hours 
	for (size_t h = 0; h < startup_hours; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_EQ(fuelCell->getPower(), 0);
		EXPECT_FALSE(fuelCell->isRunning());
	}

	// Next hour, it's fully started up
	fuelCell->runSingleTimeStep(20);
	EXPECT_EQ(fuelCell->getPower(), 20);

	// Test Min Turndown
	fuelCell->runSingleTimeStep(unitPowerMin_kW - 10);
	EXPECT_EQ(fuelCell->getPower(), unitPowerMin_kW);

	// Test ramp up limit
	fuelCell->runSingleTimeStep(100);
	EXPECT_EQ(fuelCell->getPower(), unitPowerMin_kW + dynamicResponseUp_kWperHour);
	fuelCell->runSingleTimeStep(100);
	fuelCell->runSingleTimeStep(100);
	fuelCell->runSingleTimeStep(100);
	fuelCell->runSingleTimeStep(100);

	// Test Max Limit (is not unitPowerMax_kW due to degradation)
	fuelCell->runSingleTimeStep(unitPowerMax_kW + 10);
	EXPECT_EQ(fuelCell->getPower(), fuelCell->getMaxPower());

	// Test ramp down limit
	fuelCell->runSingleTimeStep(0);
	EXPECT_NEAR(fuelCell->getPower(), fuelCell->getMaxPower() - dynamicResponseDown_kWperHour, 0.1);
}

/// Test case for when fuel cell is already started at beginning of year
TEST_F(FuelCellTest, StartedUp_lib_fuel_cell)
{
	fuelCell->setStartupHours(0, true);
	
	// First hour is fully started up
	fuelCell->runSingleTimeStep(dynamicResponseUp_kWperHour * 2);
	EXPECT_EQ(fuelCell->getPower(), dynamicResponseUp_kWperHour * 2);
}


TEST_F(FuelCellTest, Shutdown_lib_fuel_cell)
{
	fuelCell->setShutdownOption(FuelCell::FC_SHUTDOWN_OPTION::SHUTDOWN);

	// Run for startup_hours
	for (size_t h = 0; h < (size_t)startup_hours; h++) {
		fuelCell->runSingleTimeStep(20);
	}

	// Run for a few hours started up
	for (size_t h = (size_t)startup_hours; h < (size_t)(startup_hours + 5); h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_TRUE(fuelCell->isRunning());
	}

	// Initiate shutdown.  Should produce heat but no electricity for shutdown hours
	for (size_t h = 0; h < (size_t)shutdown_hours; h++) {
		fuelCell->runSingleTimeStep(0);
		EXPECT_EQ(fuelCell->getPower(), 0);
		EXPECT_GT(fuelCell->getPowerThermal(), 0);
		EXPECT_FALSE(fuelCell->isRunning());
	}

	// After one more hour it will be fully shut down
	fuelCell->runSingleTimeStep(0);
	EXPECT_EQ(fuelCell->getPower(), 0);
	EXPECT_EQ(fuelCell->getPowerThermal(), 0);
	EXPECT_FALSE(fuelCell->isRunning());

}

TEST_F(FuelCellTest, Idle_lib_fuel_cell)
{
	fuelCell->setShutdownOption(FuelCell::FC_SHUTDOWN_OPTION::IDLE);

	// Run for startup_hours
	for (size_t h = 0; h < (size_t)startup_hours; h++) {
		fuelCell->runSingleTimeStep(20);
	}

	// Run for a few hours started up
	for (size_t h = (size_t)startup_hours; h < (size_t)(startup_hours + 5); h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_TRUE(fuelCell->isRunning());
	}

	// Initiate shutdown.  Should produce heat and idle at minimum turndown electricity for shutdown hours
	for (size_t h = 0; h < (size_t)shutdown_hours; h++) {
		fuelCell->runSingleTimeStep(0);
		EXPECT_EQ(fuelCell->getPower(), fuelCell->getMinPower());
		EXPECT_GT(fuelCell->getPowerThermal(), 0);
		EXPECT_TRUE(fuelCell->isRunning());
	}
}

TEST_F(FuelCellTest, AvailableFuel_lib_fuel_cell) {

	
	// Run for startup_hours, assume no fuel consumed from available stock during startup
	for (size_t h = 0; h < (size_t)startup_hours; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_EQ(fuelCell->getAvailableFuel(), availableFuel_Mcf);
	}

	// Available fuel should start decreasing
	double availableFuelTrack = availableFuel_Mcf;
	for (size_t h = (size_t)startup_hours; h < (size_t)startup_hours + 10; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_EQ(fuelCell->getAvailableFuel(), availableFuelTrack - fuelCell->getFuelConsumption());
		availableFuelTrack -= fuelCell->getFuelConsumption();
	}

}

// Calculate the heat being generated by the fuel cell
TEST_F(FuelCellTest, HeatCalculation_lib_fuel_cell) {

	// Run for startup_hours, assume no available heat
	for (size_t h = 0; h < (size_t)startup_hours; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_EQ(fuelCell->getPowerThermal(), 0);
	}

	// Heat should start be generated
	for (size_t h = (size_t)startup_hours; h < (size_t)startup_hours + 10; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_EQ(fuelCell->getPowerThermal(), 20 * fuelCell->getHeatRecoveryEfficiency());
	}

}

// Verify that replacements are being handled
TEST_F(FuelCellTest, Replacements_lib_fuel_cell) {

	fuelCell->setStartupHours(1,false);
	fuelCell->setDegradationkWPerHour(40);
	fuelCell->setReplacementOption(FuelCell::FC_REPLACEMENT_OPTION::REPLACE_AT_CAPACITY);
	fuelCell->setReplacementCapacity(50);

	// Run for two hours, should hit replacement
	for (size_t h = 0; h < (size_t)3; h++) {
		fuelCell->runSingleTimeStep(20);
	}

	EXPECT_EQ(fuelCell->getTotalReplacements(), 1);
}

// Verify that scheduled restarts are being handled
TEST_F(FuelCellTest, ScheduleRestarts_lib_fuel_cell) {

	util::matrix_t<size_t> shutdowns;
	shutdowns.resize_fill(1, 2, 4);

	fuelCell->setStartupHours(1,false);
	fuelCell->setDegradationkWPerHour(0);
	fuelCell->setDegradationRestartkW(1);
	fuelCell->setScheduledShutdowns(shutdowns);

	// Run for 4 hours (1 to startup)
	for (size_t h = 0; h < (size_t)4; h++) {
		fuelCell->runSingleTimeStep(20);
	}
	// Next 4 hours should be shutdown by schedule, but takes 8 hours to shutdown
	for (size_t h = 0; h <= (size_t)shutdown_hours; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_EQ(fuelCell->getPower(), 0);
	}
	// Ensure restart degradation applied
	EXPECT_EQ(fuelCell->getMaxPower(), fuelCell->getMaxPowerOriginal() - 1.0);

	// Run for startup hours
	for (size_t h = 0; h < (size_t)1; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_EQ(fuelCell->getPower(), 0);
	}
	// Next hours should be running
	for (size_t h = 0; h < (size_t)4; h++) {
		fuelCell->runSingleTimeStep(20);
		EXPECT_GT(fuelCell->getPower(), 0);
	}
}

/// Test subhourly dispatch
TEST_F(FuelCellTest, DispatchFixedSubhourly_lib_fuel_cell_dispatch)
{
	size_t sh = 1;
	size_t stepsPerHour = (size_t)(1 / dt_subHourly);

	// Set to SOFC properties
	fuelCellSubHourly->setSystemProperties(200, 60, 1, 24, 500, 500);
	fuelCellDispatchSubhourly->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::FIXED);
	fuelCellDispatchSubhourly->setFixedDischargePercentage(95);

	// Allow fuel cell to startup
	size_t year_idx, h;
	year_idx = h = 0;
	for (size_t hour = 0; hour < sh; hour++) {
		for (size_t s = 0; s < stepsPerHour; s++) {
			fuelCellDispatchSubhourly->runSingleTimeStep(h, year_idx);
			year_idx++;
			h++;
		}
		EXPECT_EQ(fuelCellSubHourly->getPower(), 0);
	}

	// Dynamic response limits (500 / 4 = 125)
	fuelCellDispatchSubhourly->runSingleTimeStep(h++, year_idx++);
	EXPECT_EQ(fuelCellSubHourly->getPower(), 125);

	// Next step should reach fixed output (200 * 0.95 = 190)
	fuelCellDispatchSubhourly->runSingleTimeStep(h++, year_idx++);
	EXPECT_EQ(fuelCellSubHourly->getPower(), 190);


}


// Also check multiple fuel cells
TEST_F(FuelCellTest, DispatchFixedMultiple_lib_fuel_cell_dispatch) {

	size_t sh = (size_t)startup_hours;

	// Allow fuel cell to startup
	for (size_t h = 0; h < sh; h++) {
		fuelCellDispatchMultiple->runSingleTimeStep(h, h, 0, 0);
		EXPECT_EQ(fuelCell->getPower(), 0);
	}

	// Unit will take two hours to fully ramp to 40 kW
	fuelCellDispatchMultiple->runSingleTimeStep(sh, 0, 0);
	EXPECT_EQ(fuelCell->getPower(), 20);

	// Run at fixed output, which will go lower than min turndown
	for (size_t h = sh + 1; h < sh + 10; h++) {
		fuelCellDispatchMultiple->runSingleTimeStep(h, h, 20,10);
		EXPECT_EQ(fuelCell->getPower(), unitPowerMax_kW * fixed_percent * 0.01);
		EXPECT_EQ(fuelCellDispatchMultiple->getBatteryPower()->powerFuelCellToLoad, 0);
		EXPECT_EQ(fuelCellDispatchMultiple->getBatteryPower()->powerFuelCellToGrid, n_multipleFuelCells * 40);
		EXPECT_EQ(fuelCellDispatchMultiple->getBatteryPower()->powerSystemToLoad,  10);
		EXPECT_EQ(fuelCellDispatchMultiple->getBatteryPower()->powerSystemToGrid,  10);
	}
}

TEST_F(FuelCellTest, DispatchLoadFollow_lib_fuel_cell_dispatch) {

	size_t sh = (size_t)startup_hours;

	fuelCellDispatch->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::LOAD_FOLLOW);

	// Allow fuel cell to startup
	for (size_t h = 0; h < sh; h++) {
		fuelCellDispatch->runSingleTimeStep(h, h, 0, 20);
		EXPECT_EQ(fuelCell->getPower(), 0);
	}

	// Dispatch fuel cell for net load of 20 kW
	fuelCellDispatch->runSingleTimeStep(sh, sh, 20, 40);
	EXPECT_EQ(fuelCell->getPower(), 20);

	// Dispatch fuel cell for net load of 60 kW, dynamic response should limit to 40 kW
	fuelCellDispatch->runSingleTimeStep(sh + 1, sh + 1, 20, 80);
	EXPECT_EQ(fuelCell->getPower(), 40);

	// Dispatch fuel cell for net load of 60 kW, should be fully ramped by now
	fuelCellDispatch->runSingleTimeStep(sh + 2, sh + 2, 20, 80);
	EXPECT_EQ(fuelCell->getPower(), 60);
}

TEST_F(FuelCellTest, DispatchManual_lib_fuel_cell_dispatch) {

	size_t sh = (size_t)startup_hours;
	size_t stepsPerHour = (size_t)(1 / dt_subHourly);

	fuelCellDispatchSubhourly->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::MANUAL);

	// Allow fuel cell to startup
	size_t year_idx = 0;
	for (size_t h = 0; h < sh; h++) {
		for (size_t s = 0; s < stepsPerHour; s++) {
			fuelCellDispatchSubhourly->runSingleTimeStep(h, year_idx, 0, 20);
			year_idx++;
		}
		EXPECT_EQ(fuelCellSubHourly->getPower(), 0);
	}

	// Dispatch fuel cell at 40% of max output (40 kW, limited by dynamic response, and min turndown)
	for (size_t s = 0; s < stepsPerHour; s++) {
		fuelCellDispatchSubhourly->runSingleTimeStep(sh, year_idx, 20, 40);
		year_idx++;
	}
	EXPECT_EQ(fuelCellSubHourly->getPower(), 35);

	// Dispatch fuel cell at 40% of max output (40 kW)
	for (size_t s = 0; s < stepsPerHour; s++) {
		fuelCellDispatchSubhourly->runSingleTimeStep(sh + 1, year_idx, 20, 80);
		EXPECT_EQ(fuelCellSubHourly->getPower(), 40);
		year_idx++;
	}
}

/// Test dispatching different units per period
TEST_F(FuelCellTest, DispatchManualUnits_lib_fuel_cell_dispatch) {

	size_t sh = (size_t)8;
	discharge_units[0] = 3;

	fuelCellDispatchMultiple->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::MANUAL);
	fuelCellDispatchMultiple->setManualDispatchUnits(discharge_units);
//	fuelCellDispatchMultipleStarted->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::MANUAL);
//	fuelCellDispatchMultipleStarted->setManualDispatchUnits(discharge_units);

	// Allow fuel cell to startup - only failing test
	for (size_t h = 0; h < sh; h++) {
		fuelCellDispatchMultiple->runSingleTimeStep(h, h, 0, 20);
	}
	EXPECT_NEAR(fuelCellDispatchMultiple->getPower(), 0, 0.01);
	

	// Dispatch fuel cells at 40% of max output (40 kW per unit, limited by dynamic response, and min turndown)
	fuelCellDispatchMultiple->runSingleTimeStep(sh, sh);
	fuelCellDispatchMultiple->runSingleTimeStep(sh, sh);
	EXPECT_EQ(fuelCellDispatchMultiple->getPower(), 3 * 40);
}


TEST_F(FuelCellTest, DispatchInput_lib_fuel_cell_dispatch) {

	size_t sh = (size_t)startup_hours;

	// Dispatch input is set to constant 50 kW
	fuelCellDispatch->setDispatchOption(FuelCellDispatch::FC_DISPATCH_OPTION::INPUT);

	// Allow fuel cell to startup
	for (size_t h = 0; h < sh; h++) {
		fuelCellDispatch->runSingleTimeStep(h, h, 0, 20);
		EXPECT_EQ(fuelCellDispatch->getPower(), 0);
	}

	// Dispatch fuel cell at 50% of max output (50 kW, limited by dynamic response)
	fuelCellDispatch->runSingleTimeStep(sh, sh, 0, 0);
	EXPECT_EQ(fuelCellDispatch->getPower(), 20);

	// Dispatch fuel cell at 50% of max output (50 kW, limited by dynamic response)
	fuelCellDispatch->runSingleTimeStep(sh + 1, sh + 1, 0, 0);
	EXPECT_EQ(fuelCellDispatch->getPower(), 40);

	// Dispatch fuel cell at 50% of max output (50 kW)
	for (size_t h = sh + 2; h < 50; h++) {
		fuelCellDispatch->runSingleTimeStep(h, h, 0, 0);
		EXPECT_EQ(fuelCellDispatch->getPower(), 50);
	}
}
